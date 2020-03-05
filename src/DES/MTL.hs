{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DES.MTL where

import Control.Monad.Trans
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State, StateT)
import qualified Control.Monad.Random as Random
import Control.Monad.Identity
import System.Random (mkStdGen)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import System.IO.Unsafe (unsafePerformIO)

type ModelState v = Map String v

class Monad m => ModelActionMonad m v | m -> v where
  currentModelState :: m (ModelState v)
  modifyValue :: String -> (v -> v) -> m ()
  evalCondition :: (ModelState v -> Bool) -> m Bool

-- Model monad
type ModelActionT v m = StateT (ModelState v) m

instance Monad m => ModelActionMonad (ModelActionT v m) v where
  currentModelState = State.get
  modifyValue name f = State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)
  evalCondition cond =
    do modelState <- currentModelState
       return (cond modelState)

-- need UndecidableInstances
instance (MonadTrans t, Monad (t m), ModelActionMonad m v) => ModelActionMonad (t m) v where
  currentModelState = lift currentModelState
  modifyValue name f = lift (modifyValue name f)
  evalCondition cond = lift (evalCondition cond)

runModelAction :: Monad m => ModelActionT v m a -> m a
runModelAction action =
  State.evalStateT action Map.empty

runRandom :: Random a -> a
runRandom action =
  Random.evalRand action (mkStdGen 0)

  
data Model v m = Model {
  modelName :: String,
  startEvent :: Event v m
  }

type Condition v = ModelState v -> Bool

trueCondition :: Condition v
trueCondition = \ _ -> True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value ms =
  let (Just value') = Map.lookup name ms
  in  value' > value

type Random = Random.Rand Random.StdGen
      
type Delay m = m Integer

zeroDelay :: Monad m => Delay m
zeroDelay = return 0
constantDelay :: Monad m => Integer -> Delay m
constantDelay v = return v

exponentialDelay :: Double -> Delay Random
exponentialDelay mean =
  do u <- Random.getRandom
     return (round (-mean * log u))

data Transition v m = Transition { targetEvent :: Event v m,
                                   condition :: Condition v,
                                   delay :: Delay m }

nullTransition:: Monad m => () -> Transition v m
nullTransition () = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

type StateChangeT_ v m = ModelActionT v m ()

setValue :: ModelActionMonad m v => String -> v -> m ()
setValue name value =
  modifyValue name (const value)

incrementValue :: (ModelActionMonad m v, Num v) => String -> v -> m ()
incrementValue name inc =
  modifyValue name (\ v -> v + inc)

data Event v m where
    Event :: ModelActionMonad m v =>
          { name :: String,
            priority :: Int,
            transitions :: [Transition v m],
            stateChanges :: [m ()] }
     -> Event v m

instance Eq (Event v m) where
  e1 == e2 = (name e1) == (name e2)

instance Show (Event v m) where
  show e = "Event { name = " ++ (name e) ++ " }"


unknownEvent :: ModelActionMonad m v => () -> Event v m
unknownEvent () = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel :: (Ord v, Num v, ModelActionMonad m v) => () -> Model v m
minimalModel () = -- () is because we're parameterized over monad
  let queue = "Queue"
      serverCapacity = "ServerCapacity"
      serviceTime = constantDelay 6
      interarrivalTime = constantDelay 5
      event = unknownEvent ()
      runEvent = event { name = "RUN",
                         transitions = [runToEnter],
                         stateChanges = [setValue serverCapacity 1,
                                         setValue queue 0] }
      enterEvent = event { name = "ENTER",
                           transitions = [enterToEnter, enterToStart],
                           stateChanges = [incrementValue queue 1] }
      startEvent = event { name = "START",
                           priority = -1,
                           transitions = [startToLeave],
                           stateChanges = [setValue serverCapacity 0,
                                           incrementValue queue (-1)] }
      leaveEvent = event { name = "LEAVE",
                           transitions = [leaveToStart],
                           stateChanges = [setValue serverCapacity 1] }
      transition = nullTransition ()
      runToEnter = transition { targetEvent = enterEvent }
      enterToEnter = transition { targetEvent = enterEvent,
                                  delay = interarrivalTime }
      enterToStart = transition { targetEvent = startEvent,
                                  condition = largerThanValueCondition serverCapacity 0 }
      startToLeave = transition { targetEvent = leaveEvent,
                                  delay = serviceTime }
      leaveToStart = transition { targetEvent = startEvent,
                                  condition = largerThanValueCondition queue 0 }
      
  in Model "MinimalModel" runEvent

type Time = Integer

data EventInstance v m = EventInstance Time (Event v m)
  deriving (Show, Eq)

instance Ord (EventInstance v m) where
  compare (EventInstance t1 e1) (EventInstance t2 e2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x

newtype Clock = Clock { getCurrentTime :: Time }
  deriving Show

data SimulationState v m = SimulationState {
  sstateClock :: Clock,
  sstateEvents :: MinHeap (EventInstance v m),
  sstateReport :: Report v
}

type Simulation_ v m = StateT (SimulationState v m) (ModelActionT v m)

type SimulationStateMonad v m = State.MonadState (SimulationState v m) m

setCurrentTime :: State.MonadState (SimulationState v m) m => Time -> m ()
setCurrentTime t = State.modify (\ ss -> ss { sstateClock = Clock t })

getNextEvent :: SimulationStateMonad v m => m (EventInstance v m)
getNextEvent =
  do ss <- State.get
     case Heap.view (sstateEvents ss) of
       Just (ev, evs') ->
         do State.put (ss { sstateEvents = evs' })
            return ev
       Nothing -> undefined

updateModelState :: Monad m => EventInstance v m -> m ()
updateModelState (EventInstance _ ev) =
  sequence_ (stateChanges ev)

-- FIXME: Don't update incrementally, instead do everything based on consistent old state.

updateStatisticalCounters :: (SimulationStateMonad v m, ModelActionMonad m v, Ord v) => EventInstance v m -> m ()
updateStatisticalCounters (EventInstance t _) =
  do ss <- State.get
     modelState <- currentModelState
     State.put (ss { sstateReport = updateReport (sstateReport ss) t modelState })

generateEvents :: (SimulationStateMonad v m, ModelActionMonad m v) => EventInstance v m -> m ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ss <- State.get
             condFired <- evalCondition (condition tr)
             if condFired then
               do ss <- State.get
                  d <- delay tr
                  let evi = EventInstance ((getCurrentTime (sstateClock ss)) + d) (targetEvent tr)
                  let evs' = Heap.insert evi (sstateEvents ss)
                  State.put (ss { sstateEvents = evs' })
             else
               return ())
         (transitions ev)

timingRoutine :: SimulationStateMonad v m => m (EventInstance v m)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result

simulation :: (SimulationStateMonad v m, ModelActionMonad m v, Ord v) => Time -> m ()
simulation endTime =
  let loop =
        do ss <- State.get
           if ((getCurrentTime (sstateClock ss)) <= endTime) && not (Heap.null (sstateEvents ss)) then
             do currentEvent <- timingRoutine
                -- seq (unsafePesstateRformIO (putStrLn (show currentEvent))) (updateModelState currentEvent)
                updateModelState currentEvent
                updateStatisticalCounters currentEvent
                generateEvents currentEvent
                loop
           else
             return ()
  in loop

runSimulation :: Monad m => StateT (SimulationState v m) (StateT (ModelState v) m) () -> Model v m -> Report v -> m (Report v)
runSimulation sim model rep =
  let clock = Clock 0
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Heap.singleton initialEvent
      ss = SimulationState {
              sstateClock = clock,
              sstateEvents = eventList,
              sstateReport = rep
      }
  in do ss' <- runModelAction (State.execStateT sim ss)
        return (sstateReport ss')

-- main = writeReport (runRandom (runSimulation (simulation 100) (minimalModel ()) initialReport))

{- FAIL:
main = 
  -- writeReport (runRandom (runSimulation (simulation 100) (minimalModel ()) initialReport))
  let sim :: StateT (SimulationState v m) (StateT (ModelState v) m) ()
      sim = simulation 100
      model = minimalModel ()
  in ()
-}

-- Report generator

data Value v = Value {
  valueTime :: Time,
  valueValue :: Maybe v,
  valueMin :: Maybe v,
  valueMax :: Maybe v
}

defaultValue = Value {
  valueMin = Nothing,
  valueMax = Nothing,
  valueValue = Nothing,
  valueTime = 0
  }
  
valueWithCurrent :: Ord v => Value v -> Time -> v -> Value v
valueWithCurrent value t v =
  value { valueMin = valueNewMin value v,
          valueMax = valueNewMax value v,
          valueTime = t,
          valueValue = Just v }

valueNewMin :: Ord v => Value v -> v -> Maybe v
valueNewMin value current =
  case valueMin value of
    Just m -> Just (min current m)
    Nothing -> Just current

valueNewMax :: Ord v => Value v -> v -> Maybe v
valueNewMax value current =
  max (Just current) (valueMax value)

separator = ";\t"

valueHeader :: String
valueHeader =
  "Time" ++ separator ++ "Value" ++ separator ++ "Min" ++ separator ++ "Max" ++ separator

instance Show v => Show (Value v) where
  show value =
    let sm x = case x of
          Just v -> show v
          Nothing -> "<>"
    in      
      (show (valueTime value)) ++ separator ++
      (sm (valueValue value)) ++ separator ++
      (sm (valueMin value)) ++ separator ++
      (sm (valueMax value)) ++ separator
         
data Report v = Report {
  reportValues :: Map String (Value v),
  reportHistory :: Seq (Map String (Value v))
  } deriving Show

initialReport :: Report v
initialReport = Report { reportValues = Map.empty, reportHistory = Sequence.empty }

updateReport :: Ord v => Report v -> Time -> Map String v -> Report v
updateReport freport t ms =
  let vals =
        Map.foldlWithKey (\ vals k v ->
                            case Map.lookup k ms of
                              Nothing -> vals
                              Just current ->
                                let val = case Map.lookup k vals of
                                            Just value -> value
                                            Nothing -> defaultValue
                                    val' = valueWithCurrent val t current
                                in Map.insert k val' vals)
            (reportValues freport)
            ms
  in freport { reportValues = vals,
           reportHistory = (reportHistory freport) |> vals }
      
writeReport :: Show v => Report v -> IO ()
writeReport freport =
  do putStrLn ("name" ++ separator ++ valueHeader)
     let putValues vs =
             sequence (Map.mapWithKey (\ k v -> putStrLn (k ++ separator ++ (show v))) vs)
     mapM_ putValues (reportHistory freport)


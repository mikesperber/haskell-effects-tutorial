
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, TypeApplications, ConstraintKinds, FlexibleContexts #-}

module DES.FusedEffects where

import Control.Algebra
import Control.Effect.Sum
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Carrier.State.Strict
import qualified Control.Carrier.State.Strict as State
import Data.Kind (Type)
import GHC.Generics (Generic1)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import System.IO.Unsafe (unsafePerformIO)

type ModelState v = Map String v

data ModelAction v (m :: Type -> Type) a where
  CurrentModelState :: ModelAction v m (ModelState v)
  ModifyValue :: String -> (v -> v) -> ModelAction v m ()
  EvalCondition :: (ModelState v -> Bool) -> ModelAction v m Bool

currentModelState :: Has (ModelAction v) sig m => m (ModelState v)
currentModelState = send CurrentModelState

modifyValue :: Has (ModelAction v) sig m => String -> (v -> v) -> m ()
modifyValue name f = send (ModifyValue name f)

evalCondition :: Has (ModelAction v) sig m => (ModelState v -> Bool) -> m Bool
evalCondition cond = send (EvalCondition cond)

newtype ModelActionStateC v m a = ModelActionStateC { runModelActionStateC :: StateC (ModelState v) m a }
  deriving (Applicative, Functor, Monad)

modelStateModify :: String -> (v -> v) -> ModelState v -> ModelState v
modelStateModify name f modelState =
  let m Nothing = Just (f undefined) -- dangerous
      m (Just old) = Just (f old)
  in Map.alter m name modelState

instance (Algebra sig m) => Algebra (ModelAction v :+: sig) (ModelActionStateC v m) where
  alg hdl sig ctx = case sig of
    L CurrentModelState -> 
       (<$ ctx) <$> (ModelActionStateC State.get)
    L (ModifyValue name f) ->
      do modelState <- ModelActionStateC (State.get :: StateC (ModelState v) m (ModelState v))
         let modelState' = modelStateModify name f modelState
         ctx <$ (ModelActionStateC (State.put modelState'))
    L (EvalCondition cond) ->
      (<$ ctx) <$> (cond <$> ModelActionStateC State.get)
    R other -> ModelActionStateC (alg (runModelActionStateC . hdl) (R other) ctx)


runModelAction :: Functor m => ModelActionStateC v m a -> m a
runModelAction action = State.evalState Map.empty (runModelActionStateC action)

{-
bar :: ModelActionStateC Int Identity ()
bar = modifyValue "foo" (\ (foo :: Int) -> foo)

bar' = run (runModelAction bar)
-}

data Model v r = Model {
  modelName :: String,
  startEvent :: Event v r
  }

type Condition v = ModelState v -> Bool

trueCondition :: Condition v
trueCondition = \ _ -> True

largerThanValueCondition :: (Show a, Ord a) => String -> a -> Condition a
largerThanValueCondition name value ms =
  let (Just value') = Map.lookup name ms
  in  value' > value

type Delay m = m Integer

zeroDelay :: Monad m => Delay m
zeroDelay = return 0
constantDelay :: Monad m => Integer -> Delay m
constantDelay v = return v

{-
type Random = Random.Rand Random.StdGen

exponentialDelay :: Member Random r => Double -> Delay r
exponentialDelay mean =
  do u <- random
     return (round (-mean * log u))
-}

data Transition v m = Transition { targetEvent :: Event v m,
                                   condition :: Condition v,
                                   delay :: Delay m }

nullTransition:: Monad m => () -> Transition v m
nullTransition () = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

-- type StateChangeT_ v m = ModelActionT v m ()

setValue :: Has (ModelAction v) sig m => String -> v -> m ()
setValue name value =
  modifyValue name (const value)

incrementValue :: (Has (ModelAction v) sig m, Num v) => String -> v -> m ()
incrementValue name inc =
  modifyValue name (\ v -> v + inc)

data Event v m where
    Event :: Has (ModelAction v) sig m =>
          { name :: String,
            priority :: Int,
            transitions :: [Transition v m],
            stateChanges :: [m ()] }
     -> Event v m

instance Eq (Event v m) where
  e1 == e2 = (name e1) == (name e2)

instance Show (Event v m) where
  show e = "Event { name = " ++ (name e) ++ " }"


unknownEvent :: Has (ModelAction v) sig m => () -> Event v m
unknownEvent () = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel :: Has (ModelAction Integer) sig m => () -> Model Integer m
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

data EventInstance v r = EventInstance Time (Event v r)
  deriving (Show, Eq)

instance Ord (EventInstance v r) where
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


-- type Simulation_ v m = StateT (SimulationState v m) (ModelActionT v m)

-- type SimulationStateEffect v rm rs = (rs ~ (State (SimulationState v rm) ': rm), Member (ModelAction v) rm)

type SimulationStateEffect v mm sig ms = (Member (ModelAction v) sig, Algebra sig mm, ms ~ StateC (SimulationState v mm) mm, Algebra (State (SimulationState v mm) :+: sig) ms)

-- FAILED:
-- type SimulationStateEffect v mm sigm ms sigs = (Has (ModelAction v) sigm mm, Has (State (SimulationState v mm)) sigs ms, Members sigm sigs)

-- NOTE: needs type annotation
setCurrentTime :: forall v mm sig ms . SimulationStateEffect v mm sig ms => Time -> ms ()
setCurrentTime t = State.modify (\ (ss :: SimulationState v mm) -> ss { sstateClock = Clock t })


getNextEvent :: SimulationStateEffect v mm sig ms => ms (EventInstance v mm)
getNextEvent =
  do ss <- State.get
     case Heap.view (sstateEvents ss) of
       Just (ev, evs') ->
         do State.put (ss { sstateEvents = evs' })
            return ev
       Nothing -> undefined

updateModelState :: Monad mm => EventInstance v mm -> mm ()
updateModelState (EventInstance _ ev) =
  sequence_ (stateChanges ev)

updateStatisticalCounters :: forall v mm sig ms . (SimulationStateEffect v mm sig ms, Ord v) => EventInstance v mm -> ms ()
updateStatisticalCounters (EventInstance t _) =
  do ss <- State.get @(SimulationState v mm) -- NOTE: needed
     modelState <- currentModelState
     State.put (ss { sstateReport = updateReport (sstateReport ss) t modelState })

-- raise :: SimulationStateEffect v mm sig ms => mm a -> ms a
-- raise delay = lift delay

raise :: (MonadTrans t, Algebra sig m, Algebra (eff :+: sig) (t m)) => m a -> t m a
raise = lift

generateEvents :: forall v mm sig ms . SimulationStateEffect v mm sig ms => EventInstance v mm -> ms ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr -> 
          do ss <- State.get @(SimulationState v mm) -- NOTE: needed
             condFired <- evalCondition (condition tr)
             if condFired then
               do ss <- State.get
                  d <- raise (delay tr)
                  let evi = EventInstance ((getCurrentTime (sstateClock ss)) + d) (targetEvent tr)
                  let evs' = Heap.insert evi (sstateEvents ss)
                  State.put (ss { sstateEvents = evs' })
             else
               return ())
         (transitions ev)

timingRoutine :: SimulationStateEffect v mm sig ms => ms (EventInstance v mm)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result

simulation :: forall v mm sig ms . (SimulationStateEffect v mm sig ms, Ord v, Show v) => Time -> ms ()
simulation endTime =
  let loop =
        do ss <- State.get @(SimulationState v mm) -- NOTE: needed
           if ((getCurrentTime (sstateClock ss)) <= endTime) && not (Heap.null (sstateEvents ss)) then
             do currentEvent <- timingRoutine
                -- seq (unsafePesstateRformIO (putStrLn (show currentEvent))) (updateModelState currentEvent)
                ms1 <- raise (currentModelState :: mm (ModelState v)) -- type annotation is needed to pin v down            
                raise (updateModelState currentEvent) -- NOTE: which ones to raise ...
                ms2 <- raise (currentModelState :: mm (ModelState v)) -- type annotation is needed to pin v down            
                updateStatisticalCounters currentEvent
                generateEvents currentEvent
                loop
           else
             return ()
  in loop


runSimulation :: (Monad m, mm ~ ModelActionStateC v m) => StateC (SimulationState v mm) mm a -> Model v mm -> Report v -> m (Report v)
runSimulation sim model rep =
  let clock = Clock 0
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Heap.singleton initialEvent
      ss = SimulationState {
              sstateClock = clock,
              sstateEvents = eventList,
              sstateReport = rep
      }
  in do ss' <- runModelAction (State.execState ss sim)
        return (sstateReport ss')

-- but putting this together forces me to replace StateC by StateT in ModelActionStateC
main = 
   writeReport (run (runSimulation (simulation 100) (minimalModel ()) initialReport))

-- Report generator
type Time = Integer

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


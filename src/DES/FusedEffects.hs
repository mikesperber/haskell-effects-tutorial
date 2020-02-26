{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module DES.FusedEffects where

import Polysemy
import Polysemy.Internal (send)
import Polysemy.State
import qualified Polysemy.State as State
import Polysemy.Random
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import System.IO.Unsafe (unsafePerformIO)

type ModelState v = Map String v

data ModelAction v m a where
  CurrentModelState :: ModelAction v m (ModelState v)
  ModifyValue :: String -> (v -> v) -> ModelAction v m ()
  EvalCondition :: (ModelState v -> Bool) -> ModelAction v m Bool

currentModelState :: Member (ModelAction v) r => Sem r (ModelState v)
currentModelState = send CurrentModelState

modifyValue :: Member (ModelAction v) r => String -> (v -> v) -> Sem r ()
modifyValue name f = send (ModifyValue name f)

evalCondition :: Member (ModelAction v) r => (ModelState v -> Bool) -> Sem r Bool
evalCondition cond = send (EvalCondition cond)

runModelAction' :: Sem (ModelAction v ': r) a -> Sem (State (ModelState v) ': r) a
runModelAction' = 
  reinterpret (\case CurrentModelState -> State.get
                     ModifyValue name f -> State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)
                     EvalCondition cond ->
                       do modelState <- State.get
                          return (cond modelState))

runModelAction :: Sem (ModelAction v ': r) a -> Sem r a
runModelAction action = State.evalState Map.empty (runModelAction' action)

data Model v r = Model {
  modelName :: String,
  startEvent :: Event v r
  }

type Condition v = ModelState v -> Bool

trueCondition :: Condition v
trueCondition = \ _ -> True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value ms =
  let (Just value') = Map.lookup name ms
  in  value' > value

type Delay r = Sem r Integer

zeroDelay :: Delay r
zeroDelay = return 0
constantDelay :: Integer -> Delay r
constantDelay v = return v

{-
type Random = Random.Rand Random.StdGen
-}
exponentialDelay :: Member Random r => Double -> Delay r
exponentialDelay mean =
  do u <- random
     return (round (-mean * log u))

data Transition v r = Transition { targetEvent :: Event v r,
                                   condition :: Condition v,
                                   delay :: Delay r }

nullTransition:: () -> Transition v r
nullTransition () = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

-- type StateChangeT_ v m = ModelActionT v m ()

setValue :: Member (ModelAction v) r => String -> v -> Sem r ()
setValue name value =
  modifyValue name (const value)

incrementValue :: (Member (ModelAction v) r, Num v) => String -> v -> Sem r ()
incrementValue name inc =
  modifyValue name (\ v -> v + inc)

data Event v r where
    Event :: Member (ModelAction v) r =>
          { name :: String,
            priority :: Int,
            transitions :: [Transition v r],
            stateChanges :: [Sem r ()] }
     -> Event v r

instance Eq (Event v m) where
  e1 == e2 = (name e1) == (name e2)

instance Show (Event v m) where
  show e = "Event { name = " ++ (name e) ++ " }"


unknownEvent :: Member (ModelAction v) r => () -> Event v r
unknownEvent () = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel :: Member (ModelAction Integer) r => () -> Model Integer r
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

data EventInstance v r = EventInstance Time (Event v r)
  deriving (Show, Eq)

instance Ord (EventInstance v r) where
  compare (EventInstance t1 e1) (EventInstance t2 e2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x

newtype Clock = Clock { getCurrentTime :: Time }
  deriving Show

data SimulationState v r = SimulationState {
  sstateClock :: Clock,
  sstateEvents :: MinHeap (EventInstance v r),
  sstateReport :: Report v
}

-- type Simulation_ v m = StateT (SimulationState v m) (ModelActionT v m)

type SimulationStateEffect v rm rs = (rs ~ (State (SimulationState v rm) ': rm), Member (ModelAction v) rm)

setCurrentTime :: SimulationStateEffect v rm rs => Time -> Sem rs ()
setCurrentTime t = modify (\ ss -> ss { sstateClock = Clock t })

getNextEvent :: SimulationStateEffect v rm rs => Sem rs (EventInstance v rm)
getNextEvent =
  do ss <- State.get
     case Heap.view (sstateEvents ss) of
       Just (ev, evs') ->
         do State.put (ss { sstateEvents = evs' })
            return ev
       Nothing -> undefined

updateModelState :: EventInstance v r -> Sem r ()
updateModelState (EventInstance _ ev) =
  sequence_ (stateChanges ev)

updateStatisticalCounters :: (SimulationStateEffect v rm rs, Ord v) => EventInstance v rm -> Sem rs ()
updateStatisticalCounters (EventInstance t _) =
  do ss <- State.get
     modelState <- currentModelState
     State.put (ss { sstateReport = updateReport (sstateReport ss) t modelState })

generateEvents :: SimulationStateEffect v rm rs => EventInstance v rm -> Sem rs ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ss <- State.get
             condFired <- evalCondition (condition tr)
             if condFired then
               do ss <- State.get
                  d <- raise (delay tr) -- NOTE: try removing this
                  let evi = EventInstance ((getCurrentTime (sstateClock ss)) + d) (targetEvent tr)
                  let evs' = Heap.insert evi (sstateEvents ss)
                  State.put (ss { sstateEvents = evs' })
             else
               return ())
         (transitions ev)

timingRoutine :: SimulationStateEffect v rm rs => Sem rs (EventInstance v rm)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result

simulation :: (SimulationStateEffect v rm rs, Ord v) => Time -> Sem rs ()
simulation endTime =
  let loop =
        do ss <- State.get
           if ((getCurrentTime (sstateClock ss)) <= endTime) && not (Heap.null (sstateEvents ss)) then
             do currentEvent <- timingRoutine
                -- seq (unsafePesstateRformIO (putStrLn (show currentEvent))) (updateModelState currentEvent)
                raise (updateModelState currentEvent) -- NOTE: which ones to raise ...
                updateStatisticalCounters currentEvent
                generateEvents currentEvent
                loop
           else
             return ()
  in loop

runSimulation :: Sem (State (SimulationState v rm) : ModelAction v : r) a -> Model v rm -> Report v -> Sem r (Report v)
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
main = 
  writeReport (run (runSimulation (simulation 100) (minimalModel ()) initialReport))

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


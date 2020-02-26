module DES.MonadTransformer where

import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State, StateT)
import qualified Control.Monad.Random as Random
import System.Random (mkStdGen)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import System.IO.Unsafe (unsafePerformIO)

type ModelState v = Map String v

-- Model monad
type ModelActionT v m = StateT (ModelState v) m

runModelAction :: Monad m => ModelActionT v m a -> m a
runModelAction action =
  State.evalStateT action Map.empty

runRandom :: Random a -> a
runRandom action =
  Random.evalRand action (mkStdGen 0)

getModelState :: Monad m => ModelActionT v m (ModelState v)
getModelState = State.get

getValue :: Monad m => String -> ModelActionT v m v
getValue name =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     return v

modifyValue :: Monad m => String -> (v -> v) -> ModelActionT v m ()
modifyValue name f =
  State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)
  
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
  
evalCondition :: Monad m => (ModelState v -> Bool) -> ModelActionT v m Bool
evalCondition cond =
  do modelState <- getModelState
     return (cond modelState)

data Transition v m = Transition { targetEvent :: Event v m,
                                   condition :: Condition v,
                                   delay :: Delay m }

nullTransition:: Monad m => () -> Transition v m
nullTransition () = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

type StateChangeT v m = ModelActionT v m ()

setValue :: Monad m => String -> v -> StateChangeT v m
setValue name value =
  do ms <- State.get
     State.put (Map.insert name value ms)

incrementValue :: Monad m => Num v => String -> v -> StateChangeT v m
incrementValue name inc =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     setValue name (v + inc)

data Event v m = Event { name :: String,
                         priority :: Int,
                         transitions :: [Transition v m],
                         stateChanges :: [StateChangeT v m] }

instance Eq (Event v m) where
  e1 == e2 = (name e1) == (name e2)

instance Show (Event v m) where
  show e = "Event { name = " ++ (name e) ++ " }"

event = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel () = -- () is because we're parameterized over monad
  let queue = "Queue"
      serverCapacity = "ServerCapacity"
      serviceTime = constantDelay 6
      interarrivalTime = constantDelay 5
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

type Simulation v m = StateT (SimulationState v m) (ModelActionT v m)

setCurrentTime :: Monad m => Time -> Simulation v m () 
setCurrentTime t = State.modify (\ ss -> ss { sstateClock = Clock t })

getNextEvent :: Monad m => Simulation v m (EventInstance v m)
getNextEvent =
  do ss <- State.get
     case Heap.view (sstateEvents ss) of
       Just (ev, evs') ->
         do State.put (ss { sstateEvents = evs' })
            return ev
       Nothing -> undefined

updateModelState :: Monad m => EventInstance v m -> Simulation v m ()
updateModelState (EventInstance _ ev) =
  State.lift (sequence_ (stateChanges ev))

-- FIXME: Don't update incrementally, instead do everything based on consistent old state.

updateStatisticalCounters :: (Ord v, Monad m) => EventInstance v m -> Simulation v m ()
updateStatisticalCounters (EventInstance t _) =
  do ss <- State.get
     modelState <- State.lift State.get
     State.put (ss { sstateReport = updateReport (sstateReport ss) t modelState })

generateEvents :: Monad m => EventInstance v m -> Simulation v m ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ss <- State.get
             condFired <- State.lift (evalCondition (condition tr))
             if condFired then
               do ss <- State.get
                  d <- State.lift (State.lift (delay tr))
                  let evi = EventInstance ((getCurrentTime (sstateClock ss)) + d) (targetEvent tr)
                  let evs' = Heap.insert evi (sstateEvents ss)
                  State.put (ss { sstateEvents = evs' })
             else
               return ())
         (transitions ev)

timingRoutine :: Monad m => Simulation v m (EventInstance v m)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result

simulation :: (Ord v, Monad m) => Time -> Simulation v m ()
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

runSimulation :: Monad m => Simulation v m () -> Model v m -> Report v -> m (Report v)
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

main = 
  writeReport (runRandom (runSimulation (simulation 100) (minimalModel ()) initialReport))

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


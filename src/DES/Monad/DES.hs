{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DES.Monad.DES where

import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
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
type ModelAction v = State (ModelState v)

getModelState :: ModelAction v (ModelState v)
getModelState = State.get

getValue :: String -> ModelAction v v
getValue name =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     return v

modifyValue :: String -> (v -> v) -> ModelAction v ()
modifyValue name f =
  State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)
  
data Model v = Model {
  modelName :: String,
  startEvent :: Event v
  }

type Condition v = ModelState v -> Bool

trueCondition :: Condition v
trueCondition = \ _ -> True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value ms =
  let (Just value') = Map.lookup name ms
  in  value' > value

type Random = Random.Rand Random.StdGen
      
type Delay = Random Integer

zeroDelay :: Delay
zeroDelay = return 0
constantDelay :: Integer -> Delay
constantDelay v = return v

exponentialDelay :: Double -> Delay
exponentialDelay mean =
  do u <- Random.getRandom
     return (round (-mean * log u))
  
data Transition v = Transition { targetEvent :: Event v,
                                 condition :: Condition v,
                                 delay :: Delay }

transition = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

type StateChange v = ModelAction v ()

setValue :: String -> v -> StateChange v
setValue name value =
  do ms <- State.get
     State.put (Map.insert name value ms)

incrementValue :: Num a => String -> a -> StateChange a
incrementValue name inc =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     setValue name (v + inc)

data Event v = Event { name :: String,
                       priority :: Int,
                       transitions :: [Transition v],
                       stateChanges :: [StateChange v] }

instance Eq (Event v) where
  e1 == e2 = (name e1) == (name e2)

instance Show (Event v) where
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

data EventInstance v = EventInstance Time (Event v)
  deriving (Show, Eq)

instance Ord (EventInstance v) where
  compare (EventInstance t1 e1) (EventInstance t2 e2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x

class ReportGenerator r v | r -> v where
  update :: r -> Time -> ModelState v -> r
  writeReport :: r -> IO ()

newtype Clock = Clock { getCurrentTime :: Time }
  deriving Show

data SimulationState r v = SimulationState {
  clock :: Clock,
  events :: MinHeap (EventInstance v),
  reportGenerator :: r,
  modelState :: ModelState v,
  randomGenerator :: Random.StdGen
}

type Simulation r v = State (SimulationState r v)

setCurrentTime :: Time -> Simulation r v ()
setCurrentTime t = State.modify (\ ss -> ss { clock = Clock t })

getNextEvent :: Simulation r v (EventInstance v)
getNextEvent =
  do ss <- State.get
     case Heap.view (events ss) of
       Just (ev, evs') ->
         do State.put (ss { events = evs' })
            return ev
       Nothing -> undefined

updateModelState :: EventInstance v -> Simulation r v ()
updateModelState (EventInstance _ ev) =
  do ss <- State.get
     let ms = State.execState (sequence_ (stateChanges ev)) (modelState ss)
     State.put (ss { modelState = ms })

-- FIXME: Don't update incrementally, instead do everything based on consistent old state.

updateStatisticalCounters :: ReportGenerator r v => EventInstance v -> Simulation r v ()
updateStatisticalCounters (EventInstance t _) =
  do ss <- State.get
     State.put (ss {reportGenerator = update (reportGenerator ss) t (modelState ss) })

generateEvents :: EventInstance v -> Simulation r v ()
generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ss <- State.get
             let ms = modelState ss
             if condition tr ms then
               do ss <- State.get
                  let (d, rg) = Random.runRand (delay tr) (randomGenerator ss)
                  let evi = EventInstance ((getCurrentTime (clock ss)) + d) (targetEvent tr)
                  let evs' = Heap.insert evi (events ss)
                  State.put (ss { events = evs', randomGenerator = rg })
             else
               return ())
         (transitions ev)

timingRoutine :: Simulation r v (EventInstance v)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result

simulation :: ReportGenerator r v => Time -> Simulation r v ()
simulation endTime =
  let loop =
        do ss <- State.get
           if ((getCurrentTime (clock ss)) <= endTime) && not (Heap.null (events ss)) then
             do currentEvent <- timingRoutine
                -- seq (unsafePerformIO (putStrLn (show currentEvent))) (updateModelState currentEvent)
                updateModelState currentEvent
                updateStatisticalCounters currentEvent
                generateEvents currentEvent
                loop
           else
             return ()
  in loop

runSimulation :: ReportGenerator r v => Simulation r v () -> Model v -> Time -> r -> r
runSimulation sim model clock rg =
  let clock = Clock 0
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Heap.singleton initialEvent
      ss = SimulationState {
             clock = clock,
             events = eventList,
             reportGenerator = rg,
             modelState = Map.empty,
             randomGenerator = mkStdGen 0
           }
      ss' = State.execState sim ss
  in reportGenerator ss'


-- Report generator

data Value = Value {
  valueTime :: Time,
  valueValue :: Maybe Integer,
  valueMin :: Maybe Integer,
  valueMax :: Maybe Integer,
  valueAverage :: Double
}

defaultValue = Value {
  valueMin = Nothing,
  valueMax = Nothing,
  valueAverage = 0.0,
  valueValue = Nothing,
  valueTime = 0
  }
  
updateAvg :: Value -> Time -> Integer -> Double
updateAvg value currentTime currentValue =
  case valueValue value of
    Just v ->
      if currentTime /= 0 then
        ((valueAverage value) * (fromInteger (valueTime value))
         + ((fromInteger currentTime) - (fromInteger (valueTime value))) * (fromInteger v))
        / (fromInteger currentTime)
      else
        valueAverage value
    Nothing -> valueAverage value

valueWithCurrent :: Value -> Time -> Integer -> Value
valueWithCurrent value t v =
  value { valueMin = valueNewMin value v,
          valueMax = valueNewMax value v,
          valueAverage = updateAvg value t v,
          valueTime = t,
          valueValue = Just v }

valueNewMin value current =
  case valueMin value of
    Just m -> Just (min current m)
    Nothing -> Just current

valueNewMax value current =
  max (Just current) (valueMax value)

separator = ";\t"

valueHeader :: String
valueHeader =
  "Time" ++ separator ++ "Value" ++ separator ++ "Min" ++ separator ++ "Max" ++ separator ++ "Avg" ++ separator

instance Show Value where
  show value =
    let sm x = case x of
          Just v -> show v
          Nothing -> "<>"
    in      
      (show (valueTime value)) ++ separator ++
      (sm (valueValue value)) ++ separator ++
      (sm (valueMin value)) ++ separator ++
      (sm (valueMax value)) ++ separator ++
      (show (valueAverage value)) ++ separator
         
data FullReportGenerator = FullReportGenerator {
  reportValues :: Map String Value,
  reportHistory :: Seq (Map String Value)
  } deriving Show

fullReportGenerator = FullReportGenerator { reportValues = Map.empty, reportHistory = Sequence.empty }

instance ReportGenerator FullReportGenerator Integer where
  update frg t ms =
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
             (reportValues frg)
             ms
    in frg { reportValues = vals,
             reportHistory = (reportHistory frg) |> vals }
       
  writeReport frg =
    do putStrLn ("name" ++ separator ++ valueHeader)
       let putValues vs =
             sequence (Map.mapWithKey (\ k v -> putStrLn (k ++ separator ++ (show v))) vs)
       
       mapM_ putValues (reportHistory frg)


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

type Time = Integer

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


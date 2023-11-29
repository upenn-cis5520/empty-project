import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Random

-- A simple data structure to represent a database record
data Record = Record
  { recordId :: Int,
    content :: String
  }
  deriving (Show)

type Database = [Record]

initializeDatabase :: IO (IORef Database)
initializeDatabase = newIORef [Record 1 "First record", Record 2 "Second record"]

addRecord :: IORef Database -> Record -> IO ()
addRecord dbRef record = atomicModifyIORef' dbRef (\db -> (record : db, ()))

randomDelay :: IO ()
randomDelay = do
  delay <- randomRIO (100000, 2000000)
  threadDelay delay

main :: IO ()
main = do
  dbRef <- initializeDatabase
  forkIO $ do
    randomDelay
    addRecord dbRef (Record 3 "Third record")
    putStrLn "Added third record"

  forkIO $ do
    randomDelay
    addRecord dbRef (Record 4 "Fourth record")
    putStrLn "Added fourth record"

  threadDelay 3000000

  readIORef dbRef >>= print

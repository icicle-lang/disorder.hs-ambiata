{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.Cont where

import qualified Data.List as L
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Control.Applicative
import           System.IO
import           System.IO.Temp

import           Disorder.FSM

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.State


-- | Environment of the model is just a root temporary directory
newtype Environment = Environment {
    workingDirectory :: FilePath
  }

-- | Model state: simple "file system"
newtype FileSystem = FileSystem {
    fileSystemFiles :: M.Map FilePath File
  }

mkFileSystem :: FileSystem
mkFileSystem = FileSystem M.empty

data File = File {
    -- | Nothing if file is closed
    fileHandle :: Maybe Handle
    -- | Mirrors the actual file content
  , fileContent :: [String]
  } deriving (Show )

instance Show FileSystem where
  show = L.unlines . fmap showFile . M.toAscList . fileSystemFiles
    where
      showFile (fp, f) = fileName fp <> " (" <> showInfo f <> ")"
      fileName = L.reverse . L.takeWhile (/='/') . L.reverse
      showInfo (File (Just _) c) = "opened, " <> showContent c
      showInfo (File Nothing c) = "closed, " <> showContent c
      showContent c = show (L.length c) <> " line(s)"

-- * 'ContT' version of "bracket" system fuctions

withTempFileCont :: (MonadMask m, MonadIO m) => FilePath -> String -> ContT a m (FilePath, Handle)
withTempFileCont d t = ContT $ \fc -> withTempFile d t (curry fc)

-- | FSM 'Transition' state
type FileTransition = ContT Property (ReaderT Environment (StateT FileSystem IO))

-- * Model transitions

-- | Creates new empty file inside 'withTempFile' continuation
--   so the file is deleted after tests
--   file stays open after this step
genCreateFile :: Gen (Transition FileTransition ())
genCreateFile =
  return . mkTransition "create file" $ do
    env <- lift ask
    (fp, h) <- lift $ withTempFileCont (workingDirectory env) "file.txt"
    monitor (counterexample fp)
    lift . lift . modify $ \fs -> fs {
        fileSystemFiles = M.insert fp (File (Just h) []) (fileSystemFiles fs)
      }

runFileT :: FileTransition Property -> IO Property
runFileT x =
  withSystemTempDirectory "prop_successful_chain" $ \fp ->
    flip evalStateT mkFileSystem . flip runReaderT (Environment fp) . flip runContT return $ x

-- | Closes randomly chosen open file
genCloseFile :: Gen (Transition FileTransition ())
genCloseFile =
  -- is executed only if there is at least one open file in the model (and in 'RealWord')
   -- anyOpenFile
  return $ mkTransition "close file" $ do
    fs <- lift . lift $ get
    pre $ anyOpenFile fs
    Just (fp, (File (Just h) _)) <- pick $ pickOpenFile fs
    liftIO $ hClose h
    -- update the state of the model (closes the file)
    lift . lift . modify $ \fs' -> fs' {
        fileSystemFiles = M.adjust (\(File _ c) -> File Nothing c) fp (fileSystemFiles fs)
      }

-- | Writes a line into randomly chosen open file
genWriteFile :: Gen (Transition FileTransition ())
genWriteFile = do
  l <- listOf (choose ('A', 'z'))
  return $ mkTransition "write line" $ do
    fs <- lift . lift $ get
    pre $ anyOpenFile fs
    Just (fp, (File (Just h) _)) <- pick $ pickOpenFile fs
    liftIO $ hPutStrLn h l
    -- update the state of the model (add written line to the end of 'fileContent')
    lift . lift . modify $ \fs' -> fs' {
        fileSystemFiles = M.adjust (\(File h' c) -> File h' (c <> [l])) fp (fileSystemFiles fs)
      }

-- | Reads randomly chosen line from randomly chosen closed file
genReadFile :: Gen (Transition FileTransition ())
genReadFile =
  return $ mkTransition "read line" $ do
    fs <- lift . lift $ get
    pre $ anyClosedFile fs
    Just (fp, (File Nothing ls)) <- pick $ pickClosedFile fs
    -- additional "precondition" check that the model file is not empty
    -- if it is empty the transition is discarded (no failure) see 'pre'
    pre $ ls /= []
    (li, sl) <- pick . elements . L.zip [(0 :: Int)..] $ ls
    -- opens file in continuations so it will be closed at the end of "do" block
    -- skips all lines before 'li'-st one
    fl <- fmap (head . drop li . lines) . liftIO $ readFile fp
    monitor (counterexample (show fl <> " /= " <> show sl))
    assert $ fl == sl

-- | Invalid reading of file which may fail if the file is empty
--   (the situation which is not checked here)
genInvalidReadFile :: Gen (Transition FileTransition ())
genInvalidReadFile =
  return $ mkTransition "invalid read line" $ do
    fs <- lift . lift $ get
    pre $ anyClosedFile fs
    Just (fp, _) <- pick $ pickClosedFile fs
    h <- liftIO $ readFile fp
    assert . not . null $ h

-- | Invalid writing to file which does not cause IO exception
--   but does not update the state of the model (does not add written line to model 'File')
--   which may lead to assert 'fail' in subsequent transitions
genInvalidWriteFile :: Gen (Transition FileTransition ())
genInvalidWriteFile = do
  l <- listOf (choose ('A', 'z'))
  return $ mkTransition "invalid write line" $ do
    fs <- lift . lift $ get
    pre $ anyOpenFile fs
    Just (_, (File (Just h) _)) <- pick $ pickOpenFile fs
    liftIO $ hPutStrLn h l

anyOpenFile :: FileSystem -> Bool
anyOpenFile = anyFile True

anyClosedFile :: FileSystem -> Bool
anyClosedFile = anyFile False

anyFile :: Bool -> FileSystem -> Bool
anyFile opened = L.any ((== opened) . isJust) . fmap fileHandle . M.elems . fileSystemFiles

pickOpenFile :: FileSystem -> Gen (Maybe (FilePath, File))
pickOpenFile = pickFile True

pickClosedFile :: FileSystem -> Gen (Maybe (FilePath, File))
pickClosedFile = pickFile False

pickFile :: Bool -> FileSystem -> Gen (Maybe (FilePath, File))
pickFile open (FileSystem fs) = do
  case [ (fp, f) | (fp, f@(File mh _)) <- M.toList fs, open == isJust mh ] of
    [] -> return Nothing
    rs -> Just <$> elements rs

-- | Valid transition shall never cause test failure
prop_success_chain :: Property
prop_success_chain = monadic (testIO . runFileT) $
  runFSM' . listOf1 . frequency $ [
      (10, genCreateFile)
    , (1, genCloseFile)
    , (10, genWriteFile)
    , (1, genReadFile)
    ]


-- | 'invalid' transitions will eventualy lead to failure
--   either due to assert 'fail' or IO exception thrown due to invalid file operation
prop_failure_chain :: Property
prop_failure_chain = expectFailure . monadic (testIO . runFileT) $
  runFSM' . listOf1 . frequency $ [
      (10, genCreateFile)
    , (1, genCloseFile)
    , (10, genWriteFile)
    , (1, genReadFile)
    -- invalid transitions
    , (5, genInvalidReadFile)
    , (5, genInvalidWriteFile)
    ]


testIO :: Testable a => IO a -> Property
testIO = monadicIO . (=<<) stop . run


return []
tests :: IO Bool
tests = $quickCheckAll

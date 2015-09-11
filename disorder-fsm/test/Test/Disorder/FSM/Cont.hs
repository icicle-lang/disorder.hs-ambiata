{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.Cont where

import qualified Data.List as L
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Trans.Cont
import           System.IO
import           System.IO.Temp

import           Disorder.FSM


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

withSystemTempDirectoryCont :: String -> ContT a IO FilePath
withSystemTempDirectoryCont = ContT . withSystemTempDirectory

withTempDirectoryCont :: FilePath -> String -> ContT a IO FilePath
withTempDirectoryCont d = ContT . withTempDirectory d

withTempFileCont :: FilePath -> String -> ContT a IO (FilePath, Handle)
withTempFileCont d t = ContT $ \fc -> withTempFile d t (curry fc)

withFileCont :: FilePath -> IOMode -> ContT a IO Handle
withFileCont fp = ContT . withFile fp

-- | FSM 'Transition' state
type FileTransition = Transition Environment FileSystem (ContT Property IO)

-- * Model transitions

-- | Creates new empty file inside 'withTempFile' continuation
--   so the file is deleted after tests
--   file stays open after this step
genCreateFile :: Gen FileTransition
genCreateFile = do
  return $ mkTransition "create file" `goto` do
    env <- ask
    (fp, h) <- lift $ withTempFileCont (workingDirectory env) "file.txt"
    modify $ \fs -> fs {
        fileSystemFiles = M.insert fp (File (Just h) []) (fileSystemFiles fs)
      }

-- | Closes randomly chosen open file
genCloseFile :: Gen FileTransition
genCloseFile = do
  -- is executed only if there is at least one open file in the model (and in 'RealWord')
  return $ mkTransition "close file" `goif` anyOpenFile `goto` do
    fs <- get
    Just (fp, (File (Just h) _)) <- liftGen $ pickOpenFile fs
    liftIO $ hClose h
    -- update the state of the model (closes the file)
    modify $ \fs' -> fs' {
        fileSystemFiles = M.adjust (\(File _ c) -> File Nothing c) fp (fileSystemFiles fs)
      }

-- | Writes a line into randomly chosen open file
genWriteFile :: Gen FileTransition
genWriteFile = do
  l <- listOf (choose ('A', 'z'))
  return $ mkTransition "write line" `goif` anyOpenFile `goto` do
    fs <- get
    Just (fp, (File (Just h) _)) <- liftGen $ pickOpenFile fs
    liftIO $ hPutStrLn h l
    -- update the state of the model (add written line to the end of 'fileContent')
    modify $ \fs' -> fs' {
        fileSystemFiles = M.adjust (\(File h' c) -> File h' (c <> [l])) fp (fileSystemFiles fs)
      }

-- | Reads randomly chosen line from randomly chosen closed file
genReadFile :: Gen FileTransition
genReadFile = do
  return $ mkTransition "read line" `goif` anyClosedNonEmptyFile `goto` do
    fs <- get
    Just (fp, (File Nothing ls)) <- liftGen $ pickClosedNonEmptyFile fs
    do
      (li, sl) <- liftGen . elements . L.zip [0..] $ ls
      -- opens file in continuations so it will be closed at the end of "do" block
      fl <- liftIO . evalContT $ do
        h <- withFileCont fp ReadMode
        liftIO $ do
          -- skips all lines before 'li'-st one
          replicateM_ li (hGetLine h)
          hGetLine h
      -- checks that the read line matches corresponding line from the model
      fl === sl

-- | Invalid reading of file which may fail if the file is empty
--   (the situation which is not checked here)
genInvalidReadFile :: Gen FileTransition
genInvalidReadFile = do
  return $ mkTransition "invalid read line" `goif` anyClosedFile `goto` do
    fs <- get
    Just (fp, _) <- liftGen $ pickClosedFile fs
    h <- lift $ withFileCont fp ReadMode
    void . liftIO $ hGetLine h

-- | Invalid writing to file which does not cause IO exception
--   but does not update the state of the model (does not add written line to model 'File')
--   which may lead to assert 'fail' in subsequent transitions
genInvalidWriteFile :: Gen FileTransition
genInvalidWriteFile = do
  l <- listOf (choose ('A', 'z'))
  return $ mkTransition "invalid write line" `goif` anyOpenFile `goto` do
    fs <- get
    Just (_, (File (Just h) _)) <- liftGen $ pickOpenFile fs
    liftIO $ hPutStrLn h l

-- | Valid transition shall never cause test failure
prop_success_chain :: Property
prop_success_chain = monadicCont $ do
  d <- lift $ withSystemTempDirectoryCont "prop_success_chain"
  runFSMCont (Environment d) mkFileSystem . frequency $ [
      (10, genCreateFile)
    , (1, genCloseFile)
    , (10, genWriteFile)
    , (1, genReadFile)
    ]

-- | 'invalid' transitions will eventualy lead to failure
--   either due to assert 'fail' or IO exception thrown due to invalid file operation
prop_failure_chain :: Property
prop_failure_chain = expectFailure . monadicCont $ do
  d <- lift $ withSystemTempDirectoryCont "prop_successful_chain"
  runFSMCont (Environment d) mkFileSystem . frequency $ [
      (10, genCreateFile)
    , (1, genCloseFile)
    , (10, genWriteFile)
    , (1, genReadFile)
    -- invalid transitions
    , (5, genInvalidReadFile)
    , (5, genInvalidWriteFile)
    ]


pickOpenFile :: FileSystem -> Gen (Maybe (FilePath, File))
pickOpenFile = pickFile True

pickClosedFile :: FileSystem -> Gen (Maybe (FilePath, File))
pickClosedFile = pickFile False

pickClosedNonEmptyFile :: FileSystem -> Gen (Maybe (FilePath, File))
pickClosedNonEmptyFile (FileSystem fs) = do
  case [ (fp, f) | (fp, f@(File Nothing (_:__))) <- M.toList fs ] of
    [] -> return Nothing
    rs -> Just <$> elements rs



pickFile :: Bool -> FileSystem -> Gen (Maybe (FilePath, File))
pickFile open (FileSystem fs) = do
  case [ (fp, f) | (fp, f@(File mh _)) <- M.toList fs, open == isJust mh ] of
    [] -> return Nothing
    rs -> Just <$> elements rs

anyOpenFile :: FileSystem -> Bool
anyOpenFile = anyFile True

anyClosedFile :: FileSystem -> Bool
anyClosedFile = anyFile False

anyFile :: Bool -> FileSystem -> Bool
anyFile opened = L.any ((== opened) . isJust) . fmap fileHandle . M.elems . fileSystemFiles

anyClosedNonEmptyFile :: FileSystem -> Bool
anyClosedNonEmptyFile = L.any (\(File mh ls) -> isNothing mh && ls /= []) . M.elems . fileSystemFiles


monadicCont :: PropertyM (ContT Property IO) a -> Property
monadicCont = monadic $ ioProperty . (`runContT`return )


return []
tests :: IO Bool
tests = $quickCheckAll

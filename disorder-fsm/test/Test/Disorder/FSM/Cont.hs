{-# LANGUAGE TemplateHaskell #-}
module Test.Disorder.FSM.Cont where

import qualified Data.List as L
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Cont
import           System.IO
import           System.IO.Temp

import           Disorder.FSM

import           Test.QuickCheck
import           Test.QuickCheck.Monadic


newtype Environment = Environment {
    workingDirectory :: FilePath
  }

newtype FileSystem = FileSystem {
    fileSystemFiles :: M.Map FilePath File
  }

mkFileSystem :: FileSystem
mkFileSystem = FileSystem M.empty

data File = File {
    fileHandle :: Maybe Handle
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

withSystemTempDirectoryCont :: String -> ContT a IO FilePath
withSystemTempDirectoryCont = ContT . withSystemTempDirectory

withTempDirectoryCont :: FilePath -> String -> ContT a IO FilePath
withTempDirectoryCont d = ContT . withTempDirectory d

withTempFileCont :: FilePath -> String -> ContT a IO (FilePath, Handle)
withTempFileCont d t = ContT $ \fc -> withTempFile d t (curry fc)

withFileCont :: FilePath -> IOMode -> ContT a IO Handle
withFileCont fp = ContT . withFile fp


type FileTransition = Transition Environment FileSystem (ContT Property IO)


genCreateFile :: Gen FileTransition
genCreateFile = do
  return $ mkTransition "create file" `goto` do
    env <- ask
    (fp, h) <- lift . lift $ withTempFileCont (workingDirectory env) "file.txt"
    modify $ \fs -> fs {
        fileSystemFiles = M.insert fp (File (Just h) []) (fileSystemFiles fs)
      }

genCloseFile :: Gen FileTransition
genCloseFile = do
  f <- chooseF
  return $ mkTransition "close file" `goif` anyOpenFile `goto` do
    fs <- get
    let Just (fp, (File (Just h) _)) = pickOpenFile f fs
    liftIO $ hClose h
    modify $ \fs' -> fs' {
        fileSystemFiles = M.adjust (\(File _ c) -> File Nothing c) fp (fileSystemFiles fs)
      }

genWriteFile :: Gen FileTransition
genWriteFile = do
  f <- chooseF
  l <- listOf (choose ('A', 'z'))
  return $ mkTransition "write line" `goif` anyOpenFile `goto` do
    fs <- get
    let Just (fp, (File (Just h) _)) = pickOpenFile f fs
    liftIO $ hPutStrLn h l >> hFlush h
    modify $ \fs' -> fs' {
        fileSystemFiles = M.adjust (\(File h' c) -> File h' (c <> [l])) fp (fileSystemFiles fs)
      }

genReadFile :: Gen FileTransition
genReadFile = do
  f <- chooseF
  return $ mkTransition "read line" `goif` anyClosedFile `goto` do
    fs <- get
    let
      Just (fp, (File Nothing ls)) = pickClosedFile f fs
      lc = L.length ls
    lift $ do
      pre $ lc > 0
      let li = f (lc - 1)
      fl <- liftIO . (`runContT`return) $ do
        h <- withFileCont fp ReadMode
        liftIO $ replicateM_ li (hGetLine h)
        liftIO $ hGetLine h
      let sl = ls L.!! li
      unless (fl == sl) $ fail (show fl <> " /= " <> show sl)

genInvalidReadFile :: Gen FileTransition
genInvalidReadFile = do
  f <- chooseF
  return $ mkTransition "invalid read line" `goif` anyClosedFile `goto` do
    fs <- get
    let Just (fp, _) = pickClosedFile f fs
    h <- lift . lift $ withFileCont fp ReadMode
    _ <- liftIO $ hGetLine h
    return ()

genInvalidWriteFile :: Gen FileTransition
genInvalidWriteFile = do
  f <- chooseF
  l <- listOf (choose ('A', 'z'))
  return $ mkTransition "invalid write line" `goif` anyOpenFile `goto` do
    fs <- get
    let Just (_, (File (Just h) _)) = pickOpenFile f fs
    liftIO $ hPutStrLn h l


prop_success_chain :: Property
prop_success_chain = monadicCont $ do
  d <- lift $ withSystemTempDirectoryCont "prop_success_chain"
  runFSMCont (Environment d) mkFileSystem . listOf1 . frequency $ [
      (10, genCreateFile)
    , (1, genCloseFile)
    , (10, genWriteFile)
    , (1, genReadFile)
    ]

prop_exception_chain :: Property
prop_exception_chain = expectFailure . monadicCont $ do
  d <- lift $ withSystemTempDirectoryCont "prop_successful_chain"
  runFSMCont (Environment d) mkFileSystem . listOf1 . frequency $ [
      (10, genCreateFile)
    , (1, genCloseFile)
    , (10, genWriteFile)
    , (1, genReadFile)
    , (5, genInvalidReadFile)
    , (5, genInvalidWriteFile)
    ]


pickOpenFile :: (Int -> Int) -> FileSystem -> Maybe (FilePath, File)
pickOpenFile = pickFile True

pickClosedFile :: (Int -> Int) -> FileSystem -> Maybe (FilePath, File)
pickClosedFile = pickFile False

pickFile :: Bool -> (Int -> Int) -> FileSystem -> Maybe (FilePath, File)
pickFile open sel (FileSystem fs) =
  case [ (fp, f) | (fp, f@(File mh _)) <- M.toList fs, open == isJust mh ] of
    [] -> Nothing
    rs -> Just (rs L.!! sel (L.length rs - 1))

chooseF :: Gen (Int -> Int)
chooseF = do
  i <- arbitrary
  return $ \ s -> i `mod` (s + 1)

anyOpenFile :: FileSystem -> Bool
anyOpenFile = anyFile True

anyClosedFile :: FileSystem -> Bool
anyClosedFile = anyFile False

anyFile :: Bool -> FileSystem -> Bool
anyFile opened = L.any ((== opened) . isJust) . fmap fileHandle . M.elems . fileSystemFiles


monadicCont :: PropertyM (ContT Property IO) a -> Property
monadicCont = monadic $ ioProperty . (`runContT`return )


return []
tests :: IO Bool
tests = $quickCheckAll

import Control.Applicative
import Control.Monad
import Data.List (foldl', intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import qualified Graphics.Exif as E
import System.Directory
import System.Environment
import System.Exit (exitSuccess)
import System.FilePath
import System.FilePath.Find
import System.IO (hFlush, stdout)
import Text.Printf

import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Args

-- | Allow only files with an extension drawn from the given set.
extensionOf :: [String] -> FilterPredicate
extensionOf xs = (`elem` xs) <$> extension

recurse :: Bool -> RecursionPredicate
recurse True = always
recurse False = (< 1) <$> depth

-- | At least on an iPhone 4S with iOS 6, the time stamps are in this
-- format.
parseExifDate :: String -> Maybe UTCTime
parseExifDate = parseTime defaultTimeLocale "%Y:%m:%d %X"

myDateFormat :: UTCTime -> String
myDateFormat = formatTime defaultTimeLocale "%F_%H-%M-%S"

defaultExtensions :: [String]
defaultExtensions = [".jpg", ".JPG"]

-- | Find all EXIF tags used by files with the specified extension
-- starting at the given root directory.
allTags :: [String] -> FilePath -> IO (Set String)
allTags exts root = 
  find always (extensionOf exts) root >>= foldM aux S.empty
  where aux s =  E.fromFile
             >=> E.allTags
             >=> return . foldl' (flip S.insert) s . map fst

-- | Build a name component from a specific EXIF tag. This record
-- includes the EXIF tag to read and a function for building a name
-- component from the value bound to that tag.
data ExifName = ExifName { tagName :: String
                         , tagShow :: String -> Maybe String }

-- | Try to build a name component from an 'E.Exif' record and an
-- 'ExifName' name component builder.
nameComponent :: E.Exif -> ExifName -> ErrorT String IO String
nameComponent e (ExifName t f) = 
  do v <- liftIO (E.getTag e t) >>= 
          maybe (throwError $ "EXIF tag "++t++" not found") return
     maybe (throwError $ "Couldn't parse tag "++t++": "++v) return $ f v

-- | Build a name component from the \"DateTimeOriginal\" EXIF tag
-- using the format @Year-Month-Day_Hour-Minute-Second@.
dateName :: ExifName
dateName = ExifName "DateTimeOriginal" (parseExifDate >=> return . myDateFormat)

-- | Build a name from a list of 'ExifName' component builders and a
-- combining string to insert between name components (e.g. \"_\").
nameBuilder :: String -> [ExifName] -> E.Exif -> ErrorT String IO String
nameBuilder combiner components e = 
  intercalate combiner <$> mapM (nameComponent e) components

-- | Build a new name using the given prefix, name builder function,
-- and the EXIF record extracted from the given file.
fileRenamer :: String
            -> (E.Exif -> ErrorT String IO String)
            -> FilePath -> StateT (Set FilePath) (ErrorT String IO) FilePath
fileRenamer prefix mkName f = 
  do f' <- replaceFileName' f . (prefix++) <$> 
           (liftIO (E.fromFile f) >>= lift . mkName)
     names <- get
     let f'' = freshName names f'
     put (S.insert f'' names) >> return f''

-- | If a file name is already present in the 'Set', append numbers to
-- the name until it is unique. The new name isIf the original name was not present
freshName :: Set FilePath -> FilePath -> FilePath
freshName s f
  | f `S.member` s = go (1::Int)
  | otherwise = f
  where go n = let f' = (printf "%s_%02d" fbase n) <.> e
               in if f' `S.member` s then go (n+1) else f'
        (fbase,e) = splitExtension f
  
-- | Replace a file name, preserving the original extension.
replaceFileName' :: FilePath -> String -> FilePath
replaceFileName' f s = replaceFileName f' s <.> e
  where (f',e) = splitExtension f

-- | Rename files based on a prefix and their EXIF \"DateTime\" values.
defaultRenamer :: String -> FilePath
               -> StateT (Set FilePath) (ErrorT String IO) FilePath
defaultRenamer p = fileRenamer p (nameBuilder "_" [dateName])

main :: IO ()
main =
  do args <- getArgs
     when (args `elem` [["--help"],["-h"],["-help"],["--h"], []])
          (putStrLn help >> exitSuccess)
     Args exts pre rec dir <- either error id . parseArgs <$> getArgs
     let exts' = if null exts then defaultExtensions else exts'
     doesDirectoryExist dir 
       >>= flip when (error $ "Directory "++dir++" does not exist") . not
     imgFiles <- find (recurse rec) (extensionOf exts') dir
     imgFiles' <- fmap (either error id) . runErrorT $
                  flip evalStateT (S.empty) $
                  mapM (defaultRenamer pre) imgFiles
     putStrLn "Ready to rename:"
     mapM_ (\(x,y) -> putStrLn (x++" => "++y)) $ take 3 (zip imgFiles imgFiles')
     putStrLn $ "(A total of "++show (length imgFiles)++" files)"
     putStr "Ready to proceed (Y/n)? " >> hFlush stdout
     ans <- getLine
     if ans `elem` ["y","Y",""]
     then zipWithM_ renameFile imgFiles imgFiles' >> putStrLn "Done!"
     else putStrLn "Okay, no files have been renamed!"

help :: String
help = 
  unlines
  [ "Usage: ExifRenamer [-r] [-e extension]* [-p prefix] [dir]"
  , "-r           : recurse into subdirectories"
  , "-e extension : includes files with the given extension (default [jpg,JPG])"
  , "-p prefix    : prefix output names"
  , "dir          : work here (defaults to current directory)" ]

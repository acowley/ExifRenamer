{-# LANGUAGE FlexibleContexts #-}
module Args (parseArgs, Args(..)) where
import Control.Applicative
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.State.Class
import Control.Monad.Error.Class

data Args = Args { extensions         :: [String]
                 , namePrefix         :: String
                 , recurseDirectories :: Bool
                 , rootDirectory      :: FilePath }

parseArgs :: [String] -> Either String Args
parseArgs args = runStateT m args >>= getDir
  where m = Args <$> extensionArgs <*> prefixArg <*> recursionArg
        getDir (f,args') = case args' of
                             [] -> return (f ".")
                             [d'] -> return (f d')
                             xs -> Left $ "Unused arguments: "++show xs

-- Difference lists
dnil :: a -> a
dnil = id

snoc :: ([a] -> [a]) -> a -> [a] -> [a]
snoc xs x = xs . (x:)

fromDList :: ([a] -> [a]) -> [a]
fromDList = ($ [])
--

-- | Find all extension arguments (i.e. those preceded by \"-e\").
extensionArgs :: StateT [String] (Either String) [String]
extensionArgs = 
  do (exts,unused) <- get >>= go (dnil,dnil)
     put unused
     return exts
  where go (es,us) [] = return (fromDList es, fromDList us)
        go (es,us) ("-e":e:xs) = go (snoc es (cleanUp e), us) xs
        go _ ("-e":[]) = throwError $ "Found -e flag, but no extension given"
        go (es,us) (x:xs) = go (es, snoc us x) xs
        -- Ensure we have a leading dot character
        cleanUp e@('.':_) = e
        cleanUp e = '.':e

-- | Determine if the \"-r\" flag was supplied.
recursionArg :: MonadState [String] m => m Bool
recursionArg = do args <- get
                  if "-r" `elem` args
                  then put (filter (/= "-r") args) >> return True
                  else return False

-- | Extract the supplied prefix, given by the \"-p\" flag.
prefixArg :: (MonadState [String] m, MonadError String m) => m String
prefixArg = do args <- get
               if "-p" `elem` args
               then case span (/= "-p") args of
                      (xs,"-p":y:ys) -> put (xs++ys) >> return y
                      _ -> throwError $ "Found -p flag, but no prefix given"
               else return ""

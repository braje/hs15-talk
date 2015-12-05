{- |
     Pandoc filter for lightweight inline Haskell code blocks
 -}
{-# LANGUAGE OverloadedStrings #-}

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk

import           Control.Monad

import           Data.IORef
import           Data.List (partition)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Exit
import           System.IO
import           System.Process

data ModType = GhcjsCode | ReflexCode deriving (Eq, Show)

collectCodeBlocks :: IORef [(ModType,Text)] -> Block -> IO [Block]
collectCodeBlocks blocks (CodeBlock (ident, classes, attrs) code)
 | "ghcjs" `elem` classes || "reflex" `elem` classes = do
     let ghcjs     = "ghcjs" `elem` classes
         code'     = T.pack code
         (_,c2)    = T.breakOn "\n---" code'
         shownCode = if T.null c2 then code' else T.strip (T.drop 4 c2)
         classes'  = "haskell" : filter (\x -> x/="ghcjs" && x/="reflex") classes
         (attrs', runattrs) = partition ((`elem` ["width", "height"]) . fst) attrs
     bs <- readIORef blocks
     modifyIORef blocks ((if ghcjs then GhcjsCode else ReflexCode,code'):)
     return [ CodeBlock (ident, classes', attrs') (T.unpack shownCode)
            , Div (T.unpack $ divId (length bs + 1), ["runCode", "sourceCode"], runattrs) []
            ]
collectCodeBlocks _ b = return [b]

divId :: Int -> Text
divId n = T.pack ("runhaskell" ++ show n)

modName :: ModType -> Int -> Text
modName GhcjsCode  = modName' "M"
modName ReflexCode = modName' "R"

modName' :: String -> Int -> Text
modName' base n = T.pack (base ++ show n)

writeBlockModule :: Int -> (ModType,Text) -> IO ()
writeBlockModule n (modType,xs) = do
  tmpl <- T.readFile (if modType == GhcjsCode then "module.hs.tmpl" else "reflexmodule.hs.tmpl")
  T.writeFile ("src/" ++ T.unpack mn ++ ".hs") $
    T.replace "$modulename" mn (T.replace "$code" xs tmpl)
  where
    mn = modName modType n

writeMainModule :: [(Int,ModType)] -> IO ()
writeMainModule mods = do
  tmpl <- T.readFile "main.hs.tmpl"
  T.writeFile "src/main.hs" $
    T.replace "$imports" imports $ T.replace "$runinit" runinit tmpl
  where
    runinit = "runInit = [" <> T.intercalate " , " (fmap runWidget mods) <> "]\n"
    imports = mconcat $ fmap (\(i,modType) -> "import qualified " <> modName modType i <> "\n") mods
    runWidget (i,GhcjsCode)  = modName GhcjsCode  i <> ".start \"" <> divId i <> "\""
    runWidget (i,ReflexCode) = "runReflexWidget \"" <> divId i <> "\" " <> modName ReflexCode i <> ".start"
{-
writeMainModule :: Int -> IO ()
writeMainModule n = do
  tmpl <- T.readFile "main.hs.tmpl"
  T.writeFile "src/main.hs" $
    T.replace "$imports" imports $ T.replace "$runinit" runinit tmpl
  where
    runinit = "runInit = [" <> T.intercalate " , " (map runWidget [1..n]) <> "]\n"
    imports = mconcat $ map (\i -> "import qualified " <> modName i <> "\n") [1..n]
    runWidget i = "runReflexWidget \"" <> divId i <> "\" " <> modName i <> ".start"
-}

compileCode :: IO (ExitCode, String, String)
compileCode = readProcessWithExitCode "stack" ["exec", "ghcjs", "--", "-O", "-isrc", "src/main.hs"] ""

main :: IO ()
main = do
  r <- newIORef []
  toJSONFilter (collectCodeBlocks r)
  mods <- readIORef r
  sequence_ (zipWith writeBlockModule [1..] (reverse mods))
  writeMainModule (zip [1..] (reverse (fmap fst mods)))
  (e, out, err) <- compileCode
  when (e /= ExitSuccess) $ do
    putStrLn out
    hPutStrLn stderr err
  exitWith e

{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where
import System.Process.Typed
import qualified Data.ByteString.Char8 as C
import System.IO
import Data.List
import Control.Concurrent
import PgnGame


-- uci_wait :: String -> Handle -> ()
uci_wait s h = uci_wait' s h ""
 
uci_wait' s h pv = do
   l <- hGetLine (getStdout h)  
   if (s `isInfixOf` l) 
     then return (l, pv) 
     else if (" pv " `isInfixOf` l)
       then uci_wait' s h l
       else uci_wait' s h pv

-- uci_send :: String -> Handle -> ()
uci_send s h = do
    hPutStrLn (getStdin h) s 
    hFlush (getStdin h)

uci_init h = do
    uci_wait "uciok" h 
    uci_send "isready" h
    uci_wait "readyok" h 
    
-- someFunc :: [String] -> IO ()
someFunc fenss = do
    let fens = zip fenss [1..]
    let catConfig = setStdin createPipe
                  $ setStdout createPipe
                  $ setStderr closed
                    "engine"

    withProcess_ catConfig $ \p -> do
        hPutStrLn (getStdin p) "uci"
        hFlush (getStdin p)

        uci_init p
        -- in
        
        let ac :: [IO ()] 
            ac = fmap (\(fen, n) -> do
                uci_send "ucinewgame" p 
                uci_send ("position fen " ++ fen) p 
                uci_send "go depth 13" p 
                (bm, pv) <- uci_wait "bestmove" p
                let mv = words bm !! 1
                putStrLn $ pgnBoard n fen mv (C.pack pv)
                ) fens
        mapM_ id ac
        uci_send "quit" p
        hClose (getStdin p)
       

module Main where

import System.Environment(getArgs)
import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

main = getArgs >>= doArgs
    
doArgs (src:dst:_) = readFile src >>= BS.writeFile dst . doChars . doFormat 
doArgs _ = putStrLn "Usage: format <src> <dst>"    

doFormat s = reverse $ go ((s, 0, False), "")
    where
        go ((rest, lev, isNL), res) = case s2 of
            ""      -> res'
            '{':s2' -> startBrace '{' s2'
            '[':s2' -> startBrace '[' s2'
            '}':s2' -> finishBrace '}' s2'
            ']':s2' -> finishBrace ']' s2'
            ',':s2' -> go ((dropWhile isSpace s2', lev, True), ',' : res')
            where                                           
                (s1, s2) = break (`elem` "{}[],") rest
                startBrace ch s2' = go ((s2', lev+1, True), ch:res')
                finishBrace ch s2' = go ((s2', lev-1, False), ch : replicate (lev-1) '\t' ++ "\n" ++ res')
                newLine 
                    | isNL = replicate lev '\t' ++ "\n"
                    | otherwise = ""
                res' = reverse s1 ++ newLine ++ res

doChars = TE.encodeUtf8 . T.pack . reverse . fst . foldl' step ("", ("", False))
    where
        step (s,(buf,b)) c
            | not b && c == '\\'                = (s, ("", True))
            | b && length buf < 3 && isDigit c  = (s, (c:buf, True)) 
            | b && length buf == 3 && isDigit c = (chr (read $ reverse $ c:buf) : s, ("", False))
            | b                                 = ((c : buf) ++ "\\" ++ s, ("", False))
            | otherwise                         = (c : s, ("", False))
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import           Control.Arrow ((***), first)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.HTML.TagSoup

fpref = "mozg95/utf8_" :: String
fTree = "tree.htm" :: String

data State = None | Probably | InTD Int | InRow1 | InRow2 deriving Eq

main 
    = fmap  ( filter (not . null . snd)
            . map (\(~(_:a:_:b:_)) -> (T.unpack a, T.unpack b))
            . filter ((>3) . length . take 4)
            . map (T.splitOn "'")
            . filter (T.isPrefixOf "'4") 
            . T.splitOn "WebFXTreeItem("
            ) 
        (TIO.readFile $ fpref <> fTree)
    >>= fmap unzip 
        . mapM  (\(a,b) -> do
                    -- TIO.readFile (fpref <> b) >>= TIO.writeFile a
                    rss <- getVoc b
                    return (rss, formatWords a rss)-- TIO.writeFile ("Roots" <> a) $ formatRoots rss
                )            
    >>= \(rs, ws) -> do
        TIO.writeFile "Roots" $ formatRoots $ concat rs
        TIO.writeFile "Words" $ T.unlines ws
  where
    -- ищем таблицу (table -> table), в ней <td><table>. 
    -- Выбираем <img src> и преобразуем. 
    -- В той же строке выбираем <td>text, потом <td>(<img src>)+, потом <td>text.
    getVoc fn 
        = fmap 
            ( concatTd
            . parseTags
            )
        $ TIO.readFile (fpref <> fn)
      where
        concatTd xs = go (xs, None, "", [], []) 
          where
            go ([], _, _, _, rss)   
                = reverse rss 
            go (a:as, state, r, rs, rss) = case a of 
                TagOpen  "td" []    | state == None                     -> go (as, Probably, "", [], rss)
                                    | state == InRow1                   -> go (as, InTD 2, "", rs, rss)
                                    | state == InRow2                   -> go (as, InTD 4, "", rs, rss)
                TagClose "td"       | state == InTD 2                   -> go (as, InTD 3, "", r : rs, rss)
                                    | state == InTD 4                   -> go (as, None, "", [], (reverse $ T.replace "\r\n" " " r : rs) : rss)
                TagOpen  "table" _  | state == Probably                 -> go (as, InTD 1, "", [], rss)
                TagClose "table"    | state == InTD 1                   -> go (as, InRow1, "", correction r : rs, rss)
                                    | state == InTD 3                   -> go (as, InRow2, "", correction r : rs, rss)
                TagOpen  "img" ps   | state `elem` [InTD 1, InTD 3]     -> go (as, state, (maybe "" id $ lookup "src" ps >>= flip M.lookup g2s) <> r, rs, rss)
                TagText  txt        | state `elem` [InTD 2, InTD 4]     -> go (as, state, txt <> r, rs, rss)
                _                   | state == Probably                 -> go (as, None, "", [], rss)
                                    | otherwise                         -> go (as, state, r, rs, rss)

    -- последовательность оглас-буква[шин-дот][дагеш] преобразуем в буква[шин-дот][дагеш]-оглас
    -- состояние - Maybe (Ogl, Maybe Letter)
    correction 
        = T.reverse
        . fin
        . T.foldr corr (T.empty,Nothing) 
        . T.reverse
      where
        corr x (xs,mogl) 
            = case mogl of
                Nothing             
                    | isOgl x   -> (xs, Just (x,Nothing))
                    | otherwise -> (T.singleton x <> xs, Nothing)
                Just (o,Nothing) 
                                -> (xs, Just (o, Just $ T.singleton x))
                Just (o, Just b)
                    | isShDot x -> (xs, Just (o, Just $ T.singleton x <> b))
                    | isDag x   -> ((T.singleton o <> T.singleton x <> b <> xs, Nothing)) 
                    | otherwise -> corr x (T.singleton o <>  b <> xs, Nothing)
        fin (res,mo)
            = maybe res 
                (\(o,mb) -> maybe res (\b -> T.singleton o <> b <> res) mb) 
                mo

    formatWords name
        = T.unlines
        . map   (\(a:b:c:d:_) -> a 
                    <> "\t" <> b
                    <> "\t" <> c
                    <> " " <> d
                    <> "\t" <> T.replace "Примеры слов. " "" 
                                (T.pack $ dropWhile (`elem` "1234567890. ") name)
                ) 
    formatRoots
        = T.unlines
        . map (\(a,(ss1,ss2)) -> a 
                                <> "\t" <> T.intercalate "; " (S.toList ss1) 
                                <> "\t" <> T.intercalate "; " (S.toList ss2))
        . M.toList
        . M.fromListWith (\(s1,s2) (s3,s4) -> (s1 `S.union` s3, s2 `S.union` s4))
        . map   (\(a:b:c:d:_) 
            ->  ( c{- <> " " <> T.pack (show c)-}
                , (S.singleton d, S.singleton $ a <> " " {-<> T.pack (show a) <> " " -}<> b)
                ))

gmps    = map T.singleton "bgdhvzxtyklmnspcqrf"    -- + ay,sh,si
letters = map T.singleton "בגדהוזחטיכלמנספצקרת"
g2l     = zip gmps letters 
        <>  [ ("al","א")
            , ("ay","ע")
            , ("sh","ש" <> T.singleton '\1473')   -- 5C1
            , ("si","ש" <> T.singleton '\1474')    -- 5C2
            ]
        <>  [ ("k9", "ך")
            , ("m9", "ם")
            , ("n9", "ן")
            , ("p9", "ף")
            , ("c9", "ץ")
            ]
            

-- дагеш: x -> x0   05BC
-- софит: x -> x9
g2o     =   [ ("w" , T.singleton '\x05B0') -- шва             
            , ("e4", T.singleton '\x05B1') -- хатаф-сеголь    
            , ("a4", T.singleton '\x05B2') -- хатаф-патах     
            , ("o4", T.singleton '\x05B3') -- хатаф-камац     
            , ("i3", T.singleton '\x05B4') -- хирик           
            , ("e2", T.singleton '\x05B5') -- цере
            , ("e3", T.singleton '\x05B6') -- сеголь          
            , ("a3", T.singleton '\x05B7') -- патах           
            , ("a2", T.singleton '\x05B8') -- камац гадоль    
            , ("o3", T.singleton '\x05B8') -- камац катан      (05C7)
            , ("o2", T.singleton '\x05B9') -- неполный холам  
            , ("o1", "ו" <> T.singleton '\x05B9') -- полный холам
            , ("u3", T.singleton '\x05BB') -- кубуц           
            ]

g2s = M.fromList
    $ map (first (<>".gif")) 
    $ g2l 
    <> map ((<>"0") *** (<> T.singleton '\x05BC')) g2l -- дагеш
    <> g2o
    <> [("kw", "\x05B0ך")]
            -- 
ogl = S.map T.head 
    $ S.delete (T.singleton '\x05B9') 
    $ S.delete ("ו" <> T.singleton '\x05B9') 
    $ S.fromList 
    $ map snd g2o
isOgl = (`S.member` ogl)
isDag = (== '\x05BC')
isShDot = (`elem` "\1473\1474")


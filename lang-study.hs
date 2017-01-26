{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Network.HTTP.Conduit       (simpleHttp)
import           Text.HTML.TagSoup (parseTags, Tag(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Monoid((<>))

url = "http://www.languages-study.com/hebrew-3000words"

main = mapM printSite [0..22]

printSite n = do
    {-
    putStrLn ""
    putStrLn $ "-------------------- Урок " ++ show n
    putStrLn ""
    -}
    html <- fmap TLE.decodeUtf8 $ simpleHttp $ url <> (if n == 0 then "" else ("-" <> show n)) <> ".html"
    -- print html
    mapM TLIO.putStrLn
            $ filter (\t -> not $ TL.isInfixOf "значение" t && TL.isInfixOf "чтение" t && TL.isInfixOf "слово" t)
            $ concatTd
            $ dropWhile (not . isZn)
            $ parseTags html
  where
    concatTd xs = reverse $ go (xs, "", [], [])
      where
        go ([], _, rs, rss) = rss
        go (a:as, r, rs, rss) = case a of
            TagOpen "table" _   -> go (as, "", [], rss)
            TagClose "table"    -> go (as, "", rs, rss)
            TagOpen "td" _      -> go (as, "", rs, rss)
            TagClose "td"       -> go (as, "", rs', rss)
            TagText txt         -> go (as, r <> txt, rs, rss)
            TagOpen "tr" _      -> go (as, "", [], rss)
            TagClose "tr"       -> go (as, "", [], row:rss)
            _ -> go (as, r, rs, rss)
          where
            rs' = map correct $ if TL.null r then rs else r:rs
            row = case rs' of
                x1:x2:x3:_ -> TL.concat [x1,"\t",x3,"\t",x2]
                _ -> "-----------"
            correct = TL.replace ";;" ";"
                    . TL.map (\c -> if c == '\n' then ';' else c)
                    . trim . trim
              where
                trim = TL.reverse . TL.dropWhile (`elem` (" \n" :: [Char]))

    isZn (TagText x) = "значение" `TL.isInfixOf` x
    isZn _ = False

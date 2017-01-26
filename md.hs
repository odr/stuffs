{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes, LambdaCase, ScopedTypeVariables #-}
module Main where

import Conduit(toConsumer)
import Control.Monad(forM_)
import Control.Monad.Trans.Resource(runResourceT)
import qualified Data.Map as Map
import Data.Monoid((<>))
import qualified Data.Text as T
import           Data.Text(Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time
import System.Process(callCommand)
import qualified Text.HTML.DOM as HD
import Data.Conduit.Process(sourceCmdWithConsumer)
import Text.XML.Cursor(fromDocument, ($//))
import           Text.HTML.TagSoup (parseTags, Tag(..))
import Database.PostgreSQL.Simple

import Utils
-- url = "http://multidict.co.il/Dictionary/RootDict.aspx"

cnt = 5000

main = do
    getCurrentTime >>= print
    html <- TIO.readFile "res.html"
    let roots = concat  [map snd $ filter ((=="value") . fst) args
                            | (TagOpen "option" args) <- parseTags html
                        ]
    curl <- TIO.readFile "curl.sh"

    conn <- connectPostgreSQL "dbname=dic user=p"
    forM_ (take cnt $ zip [1..] roots) $ \(n,root) -> do
        t1 <- getCurrentTime
        let (pref,suf0) = T.breakOn "ddlRoot" curl
            (_,suf) = T.breakOn "&"suf0
            com = pref <> "ddlRoot=" <> root <> suf
            fn = "res/curl_run.sh"
            rn = "res/res" <> show n <> ".html"
        TIO.writeFile fn com
        callCommand $ "chmod 744 " <> fn
        (_, doc)    <- runResourceT
                    $ sourceCmdWithConsumer fn (toConsumer HD.sinkDoc)
        t2 <- getCurrentTime
        let drs = fromDocument doc $// rowsA
        TLIO.writeFile rn $ tabToHtml drs
        t3 <- getCurrentTime
        withTransaction conn $
            forM_ drs $ \(DicRow r e h) -> do
                [Only (vid::Integer)]
                    <- query_ conn "SELECT nextval('word_id_seq')"
                forM_ [(r,'r'),(e,'e'),(h,'h')] $ \(dw,c') -> do
                    let c = T.singleton c'
                    execute conn
                        "INSERT INTO words (id, lang, word) VALUES (?, ?, ?)"
                        (vid, c, dwText dw)
                    forM_ (Map.toList $ dwNote dw) $ \(key, val) -> do
                        execute conn
                            "INSERT INTO props (word_id, lang, name, val) VALUES (?, ?, ?, ?)"
                            (vid, c, key, val)
        t4 <- getCurrentTime

        TIO.putStrLn $ "get root " <> T.pack (show n)
            <> ". http time: "  <> T.pack (show $ diffUTCTime t2 t1)
            <> ". parse time: " <> T.pack (show $ diffUTCTime t3 t2)
            <> ". db time: "    <> T.pack (show $ diffUTCTime t4 t3)

    close conn
    getCurrentTime >>= print







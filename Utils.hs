{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Utils where

import Debug.Trace

import Control.Arrow((&&&))
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe(isNothing)
import Data.Monoid((<>))
import qualified Data.Text as T
import           Data.Text(Text)
import qualified Text.XML as X
import Text.XML.Cursor

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Text (renderHtml)

data DicWord = DicWord { dwText :: Text, dwNote :: Map.Map Text Text }
    deriving (Show, Read, Eq)

dwToText (DicWord a b) = a <> "\t" <> (noteToText "; " b)
noteToText sep = T.intercalate sep . map (\(x,y) -> x <> "=" <> y) . Map.toList

data DicRow = DicRow { drRu :: DicWord, drEn :: DicWord, drHe :: DicWord }
    deriving (Show, Read, Eq)

drToText (DicRow a b c) = T.unlines [ "Ru: " <> dwToText a
                                    , "En: " <> dwToText b
                                    , "He: " <> dwToText c
                                    , ""
                                    , ""
                                    ]
tabToText = T.unlines . map drToText
tabToHtml drs = renderHtml [shamlet|
<html>
    <head>
        <meta charset="UTF-8">
    <body>
        <table>
            $forall (DicRow ru en he) <- drs
                <tr>
                    #{dwToHtml ru}
                    #{dwToHtml en}
                    #{dwToHtml he}
|]
  where
    dwToHtml (DicWord a b) = [shamlet|
<td>
    #{a}
        <br>
            #{noteToText "; " b}
|]

tabA    =   laxElement "table"
        >=> attributeIs "id" "ctl00_ContentPlaceHolder1_grWords"
rowsA
    = concat . filter (not . null) . map (dr . concat . ($/ rowA))
    . drop 2 . (tabA &/ laxElement "tr" . cut)
rowA = (:[])
    . map ((,) <$> allText <*> (Map.fromList . allId))
    . laxElement "td"
  where
    allId   = ($/ spanAllId)
    allText = filter (not . T.null) . map T.strip . ($/ spanAllText)

dw f (a,b)      = DicWord (T.intercalate "; " $ f a) b

dr (a:b:c:_)    = [DicRow (dw id a) (dw id b) (dw reverse c)]
dr _            = []

contentOrSpan c
    = case node c of
        X.NodeElement (X.Element en as ns)
            | X.nameLocalName en == "span" && isNothing (Map.lookup "id" as)
                            -> concatMap (contentOrSpan . fromNode) ns
        X.NodeContent v     -> [v]
        _                   -> []

spanId
    = map   (   T.drop 40 . T.concat . attribute "id"
            &&& T.intercalate " " . map T.strip . ($/ content)
            )
    . (laxElement "span" >=> hasAttribute "id")

spanNoId
    = map (T.intercalate " " . map T.strip . ($/ contentOrSpan))
    . (laxElement "span" >=> hasNoAttribute "id")

hasNoAttribute :: X.Name -> Axis
hasNoAttribute n c =
    case node c of
        X.NodeElement (X.Element _ as _)
            -> maybe [c] (const []) $ Map.lookup n as
        _   -> []

inSpan = laxElement "span" >=> attributeIs "dir" "rtl"

inSpanNoId
    = filter (not . T.null) . (:[]) . T.concat . map T.strip
    . (inSpan &/ laxElement "font" &/ contentOrSpan)

inSpanId
    = inSpan &// spanId

listText t = if T.null t then [] else [t]
spanAllText = (spanNoId <> inSpanNoId)
spanAllId   = (spanId <> inSpanId)


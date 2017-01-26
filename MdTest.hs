{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module MdTest where

import Data.Text(Text)
import qualified Data.Text.Lazy as TL
import qualified Text.XML as X
import Text.XML.Cursor

import Utils
import Multiline

test :: Text
test = [ml|
    <html xmlns="xx">
        <head id="ctl00_Head1">
            <title>Иврит и Английский</title>
        </head>
        <body scroll="yes">
            <table>
                <tr/>
            </table>
            <table>
                <tr>
                    <table id="ctl00_ContentPlaceHolder1_grWords">
                        <tr></tr>
                        <tr></tr>
                        <tr>
        <td align="left" valign="top" style="color:Black;width:30%;">
            <span style="font-weight:bold">папочка</span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_ChR_R" style="color:Blue;font-size:Medium;font-style:italic;">сущ.</span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_SexR" style="color:Blue;font-size:Medium;font-style:italic;">м.</span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_NumR" style="color:Blue;font-size:Medium;font-style:italic;"></span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_RusRabim" style="font-size:Medium;font-style:italic;"></span>
            <span style="font-style: italic;" ></span>
            <span style="font-style: italic;" ></span>
            <br/>
        </td>
        <td align="left" valign="top" style="color:Black;width:30%;">
            <span style="font-weight: bold">daddy</span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_WordE2" style="color:Blue;"></span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_ChR_E" style="color:Blue;font-size:Small;font-style:italic;">n.</span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_T_Eng" style="color:Black;">['dædi]</span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_Num_E" style="color:Blue;font-size:Small;font-style:italic;"></span>
            <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_EngRabim" style="color:Black;font-size:Small;font-style:italic;"></span>
            <span style="font-style: italic;"></span><br/>
        </td>
        <td align="right" valign="top" style="color:Black;">
            <span dir="rtl">
                <span>
                    <font style='color:blue'></font>
                </span>
                <font style="font-size: x-large;  font-weight: bold">
                    <span style='color:red'>אַ</span>בָּא'לֶה
                </font>
                <!--
                -->
                <sub>
                    <font style="font-size: x-small; font-weight: bold;">
                    </font>
                </sub>
                <span dir="ltr">
                    <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_RemRus" style="color:Blue;font-size:Small;">
                    </span>
                </span>
                <span nowrap="nowrap">
                    <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_Heb2" style="color:Blue;font-size:Small;font-style:italic;"></span>
                </span>
                <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_ChR_H" style="color:Blue;font-size:Medium;">שֵם    </span>
                <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_Binyan" style="color:Blue;font-size:Medium;"></span>
                <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_SexH" style="color:Blue;font-size:Medium;">ז'</span>
                <span dir="ltr">
                    <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_T_Heb" style="font-size:Large;">['aba-le]</span>
                </span>&nbsp;
                <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_RootH" style="color:Red;font-size:Medium;font-weight:bold;">א.ב.</span>
                &nbsp;
                <span id="ctl00_ContentPlaceHolder1_grWords_ctl02_HebRabim" style="font-size:Medium;"></span>
                <span style="font-style: italic;"></span>
            </span>
            <table align="center">
                <tr><td>

                    &nbsp;

                 </td></tr>
            </table>

        </td>
            <!--
            -->
        <td>
            <table>
                <tr>
                    <td>
                        <input type="hidden" name="ctl00$ContentPlaceHolder1$grWords$ctl02$hdnRootH" id="ctl00_ContentPlaceHolder1_grWords_ctl02_hdnRootH" value="אב" />
                        <input type="hidden" name="ctl00$ContentPlaceHolder1$grWords$ctl02$hdnTableN" id="ctl00_ContentPlaceHolder1_grWords_ctl02_hdnTableN" value="0" />
                    </td>
                </tr>
                <tr>
                    <td>
                        &nbsp;</td>
                </tr>
            </table>
        </td>
        <!--
        -->
                                </tr>
                        <tr></tr>
                    </table>
                </tr>
            </table>
        </body>
    </html>
|]

doc = either (const $ error "Invalid document") id
    $ X.parseText X.def { X.psDecodeEntities = X.decodeHtmlEntities }
    $ TL.fromStrict test

docC = fromDocument doc

runTest = docC $// rowsA

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils
    ( readHtml
    , toDate
    , entryDate
    , footer
    ) where

import Import hiding (readFile)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, readFile)
import Text.Blaze (toMarkup)
import Text.Blaze.Html (ToMarkup, preEscapedToHtml)
import Text.Blaze.Internal (ChoiceString (ByteString, PreEscaped)
                           ,MarkupM (Content))
import Yesod.Core.Types (WidgetFor)


import Control.Monad ((<=<))

instance ToMarkup ByteString where
    toMarkup = flip Content () . PreEscaped . ByteString

readHtml :: String -> WidgetFor url ()
readHtml = toWidget <=< liftIO . preEscapedToHtml <$.> readFile
  where
    infixr 9 <$.>
    (<$.>) = (.) . (<$>)

toDate :: Day -> (Year, Month, DayOfM)
toDate date = (y, Month m, DayOfM d)
  where
    (y,m,d) = toGregorian date

entryDate :: Entry -> (Year, Month, DayOfM)
entryDate = toDate . utctDay . entryTimestamp

footer :: (ToWidget site a2, ToWidget site a1)
       => Maybe (Route site, a1)
       -> Maybe (Route site, a2) 
       -> WidgetFor site ()
footer rc1 rc2 = [whamlet|
    $maybe (route, content) <- rc1
        <div style="float: left; max-width: 40%;">
            <a href=@{route}>
                «^{content}
    $maybe (route, content) <- rc2
        <div style="float: right; max-width: 40%;">
            <a href=@{route}>
               ^{content}»
    |]


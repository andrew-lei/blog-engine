{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.BlogEntries where

import Foundation
import Yesod.Core
import Common
import Import

import Data.Time.Calendar (addDays
                          ,addGregorianMonthsClip
                          ,addGregorianYearsClip
                          )
import Text.Blaze.Html (preEscapedToHtml)



listEntries :: Foldable t => t (Entity Entry) -> WidgetFor App ()
listEntries entries = [whamlet|
    $forall Entity _ entry <- entries
        ^{linkToEntry entry}
    |]
  where
    linkToEntry entry = do
        let (y,m,d) = entryDate entry
        [whamlet|
            <a href=@{BlogEntryR y m d (entryFilename entry)}>
                <div style="background-color: #ecf0f1; border: 1px solid grey; border-radius: 1em; margin: 1em 0; padding: 0 1em 1em 1.2em; display: flow-root;">
                    <h4>#{y} #{m} #{d}: #{entryTitle entry}
                    <div>
                        $maybe thumbnail <- entryThumbnail entry
                            <img src=#{thumbnail} style="width:10em; height:10em; float:left; margin: 0 1em 0 0; border-radius: 1em; max-width: 30vw; max-height: 30vw;"/>
                        #{entryDescription entry}
        |]

entryRange :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend) =>
              UTCTime -> UTCTime -> ReaderT backend m [Entity Entry]
entryRange start end = selectList [EntryTimestamp >=. start
                                  ,EntryTimestamp <. end]
                                  [Desc EntryTimestamp]

getBlogDayR :: Year -> Month -> DayOfM -> Handler Html
getBlogDayR y m d = do
    entries <- runDB $ entryRange dayOf nextDay
    defaultLayout $ do
        setTitle . preEscapedToHtml $ "Entries for " ++ prettyDate
        [whamlet|<h1>Entries for #{prettyDate}|]
        listEntries entries
        (footer `on` toLink) prevDay' nextDay'
  where
    dayOf'  = fromGregorian y (month m) (day d)
    dayOf   = UTCTime dayOf' 0
    nextDay' = addDays 1 dayOf'
    nextDay = UTCTime nextDay' 0
    prevDay' = addDays (-1) dayOf'

    prettyDate = show y ++ ' ':prettyMonth m ++ ' ':(unpack . toPathPiece) d
    toLink date = let (y',m',d') = toDate date in
        Just (BlogDayR y' m' d', [whamlet|#{m'} #{d'}|])

getBlogMonthR :: Year -> Month -> Handler Html
getBlogMonthR y m = do
    entries <- runDB $ entryRange monthOf nextMonth
    defaultLayout $ do
        setTitle . preEscapedToHtml $ "Entries for " ++ prettyDate
        [whamlet|<h1>Entries for #{prettyDate}|]
        listEntries entries
        (footer `on` toLink) prevMonth' nextMonth'
  where
    monthOf'   = fromGregorian y (month m) 1
    monthOf    = UTCTime monthOf' 0
    nextMonth' = addGregorianMonthsClip 1 monthOf'
    nextMonth  = UTCTime nextMonth' 0
    prevMonth' = addGregorianMonthsClip (-1) monthOf'

    prettyDate = prettyMonth m ++ ' ':show y
    toLink date = let (y',m',_) = toDate date in
        Just (BlogMonthR y' m', [whamlet|#{m'} #{y'}|])


getBlogYearR :: Year -> Handler Html
getBlogYearR y = do
    entries <- runDB $ entryRange yearOf nextYear
    defaultLayout $ do
        setTitle . preEscapedToHtml $ "Entries for " ++ show y
        [whamlet|<h1>Entries for #{y}|]
        listEntries entries
        (footer `on` toLink) prevYear' nextYear'
  where
    yearOf' = fromGregorian y 1 1
    yearOf = UTCTime yearOf' 0
    nextYear' = addGregorianYearsClip 1 yearOf'
    nextYear = UTCTime nextYear' 0
    prevYear' = addGregorianYearsClip (-1) yearOf'

    toLink date = let (y',_,_) = toDate date in
        Just (BlogYearR y', [whamlet|#{y'}|])

getBlogR :: Handler Html
getBlogR = do
    entries <- runDB getLast50
    defaultLayout $ do
        setTitle "Blog"
        [whamlet|<h1>Blog|]
        listEntries entries
  where
    getLast50 = selectList [] [Desc EntryTimestamp, LimitTo 50]


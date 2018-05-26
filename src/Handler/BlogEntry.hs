{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.BlogEntry where

import Foundation
import Yesod.Core
import Common
import Import

import Text.Blaze.Html (preEscapedToHtml)
import Text.Lucius (luciusFile)
import Data.Time.Calendar (addDays)


data MetaTags = MetaTags
              { metaTitle :: Text
              , metaUrl :: String
              , metaDescription :: Text
              , metaAuthor :: Text
              , metaKeywords :: [Text]
              , metaImage :: Maybe FilePath
              }

metaTagsHtml :: MonadWidget m => MetaTags -> m ()
metaTagsHtml MetaTags{..} = toWidgetHead 
    [hamlet|
    <meta property=og:title content=#{metaTitle}>
    <meta property=og:url content=#{metaUrl}>
    <meta name=description property=og:description content=#{metaDescription}>
    <meta name=author property=og:author content=#{metaAuthor}>
    <meta name=keywords content=#{intercalate "," (map unpack metaKeywords)}>
    $maybe image <- metaImage
         <meta property=og:image content=#{image}>
    |]

getBlogEntryR :: Year -> Month -> DayOfM -> Text -> Handler Html
getBlogEntryR y m d name = do
    maybeEntry <- runDB $ selectFirst [EntryTimestamp >=. dayOf
                                      ,EntryTimestamp <. nextDay
                                      ,EntryFilename ==. name] [Desc EntryId]

    case maybeEntry of
        Nothing -> notFound
        Just entry'@(Entity _ Entry{..}) -> do
            (ePath, aName, tags, prev, next) <- getEntryInfo entry'
            defaultLayout $ do
                setTitle . preEscapedToHtml $ entryTitle
                metaTagsHtml $ entryMeta entry' aName tags
                [whamlet|<h1>#{entryTitle}|]
                readHtml ePath
                (footer `on` toLink) prev next
                toWidget $(luciusFile "templates/pandoc.lucius")
                toWidget $(luciusFile "templates/tabs.lucius")
  where
    date     = fromGregorian y (month m) (day d)
    datePath = formatTime defaultTimeLocale "%Y/%m/%d" date
    dayOf    = UTCTime date 0
    nextDay  = UTCTime (addDays 1 date) 0

    getEntryInfo (Entity key Entry{..}) = do
        prev <- runDB $ selectFirst [EntryTimestamp <. entryTimestamp]
                                    [Desc EntryTimestamp]
        next <- runDB $ selectFirst [EntryTimestamp >. entryTimestamp]
                                    [Asc EntryTimestamp]

        author <- runDB $ get entryAuthor
        let authorName = maybe "Unknown author" userName author
            ePath = "static/blog" </> datePath </> unpack entryFilename <.> "html"

        tags <- runDB $ selectList [TagEntry ==. key] []

        pure (ePath, authorName, map (tagTag . entityVal) tags, prev, next)

    getRoute entry = let (y',m',d') = entryDate entry in
        BlogEntryR y' m' d' (entryFilename entry)

    toLink entry = let entry' = entityVal <$> entry in
        (,) <$> (getRoute <$> entry') <*> (entryTitle <$> entry')

    entryMeta (Entity _ Entry{..}) aName tags = MetaTags
        { metaTitle = entryTitle
        , metaUrl = "blog" </> datePath </> unpack entryFilename
        , metaDescription = entryDescription
        , metaAuthor = aName
        , metaKeywords = tags
        , metaImage = entryThumbnail
        }


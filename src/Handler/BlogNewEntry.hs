{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.BlogNewEntry where

import Foundation
import Yesod.Core
import Common
import Import

import Text.Pandoc (Pandoc(Pandoc)
                   ,PandocIO
                   ,Block(Plain)
                   ,Meta
                   ,MetaValue(MetaBlocks
                             ,MetaInlines
                             ,MetaList)
                   ,ReaderOptions(readerExtensions)
                   ,WriterOptions(writerReferenceLinks)
                   ,readMarkdown
                   ,pandocExtensions
                   ,writeHtml5
                   ,writePlain
                   ,lookupMeta
                   ,nullMeta
                   ,runIO
                   ,docTitle
                   )
import Text.Pandoc.Shared (mapLeft
                          ,stringify)
import Control.Monad.Trans.Except (ExceptT(ExceptT)
                                  ,runExceptT)
import Data.Either.Combinators (maybeToRight)
import Data.Function ((&))

import MultiFile (fileAFormMult)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (dropExtension
                             ,(-<.>)
                             )
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)

import TabFilter (addCodeIdentifiers)
import ImageFilter (fullImagePath)


show' :: PathPiece a => a -> String
show' = unpack . toPathPiece

getBlogNewR :: Handler Html
getBlogNewR = do
    ((_, widget), enctype) <- runFormPost entryForm
    defaultLayout $ do
        setTitle "New Post"
        [whamlet|
            <form method=post action=@{BlogNewR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

entryForm :: Html -> MForm Handler (FormResult (FileInfo, [FileInfo], UTCTime), Widget)
entryForm = renderDivs $ (,,) <$> fileAFormReq "File"
                              <*> fileAFormMult "Assets"
                              <*> lift (liftIO getCurrentTime)

writeToServer :: FilePath -> FileInfo -> Handler FilePath
writeToServer dirpath file = do
    liftIO $ fileMove file path'
    return filename
  where
    filename = unpack $ fileName file
    path' = dirpath </> filename

(|>>) :: Functor f => (a1 -> f a2) -> (a2 -> b) -> a1 -> f b
f |>> g = fmap g . f

mdToHtml5 :: FilePath -> Text -> PandocIO (Html, Meta)
mdToHtml5 fp = readMarkdown def {readerExtensions = pandocExtensions}
           |>> addCodeIdentifiers
           |>> (fullImagePath fp)
           >=> (writeHtml5 def {writerReferenceLinks = True} &&& getMeta)
            .# merge
    where
      getMeta (Pandoc meta _) = return meta
      
      (.#) = flip (.)
      merge (a,b) = (,) <$> a <*> b


postBlogNewR :: Handler Html
postBlogNewR = do
    ((result, _), _) <- runFormPost entryForm
    Entity userId _ <- requireAuth
    filedata <- runExceptT $ do
        (file, images, time) <- ExceptT . return $ toEither result

        let (y,m,d) = (toDate . utctDay) time
            dirpath = "static/blog" </> show y </> show' m </> show' d
            fname' = (pack . dropExtension . unpack . fileName) file
            fname = dirpath </> (unpack . fileName) file -<.> "html"
        
        bytes <- fileSourceByteString file
        eitherHtml <- liftIO . runIO . (mdToHtml5 $ "/" </> dirpath) . decodeUtf8 $ bytes 
        (html, meta) <- ExceptT . return . mapLeft show $ eitherHtml
        
        liftIO $ do createDirectoryIfMissing True dirpath
                    renderMarkupToByteStringIO (writeFile fname) html
                    mapM_ (\x -> fileMove x $ dirpath </> (unpack . fileName) x) images
        let title = stringify . docTitle $ meta
        desc'' <- ExceptT . return . maybeToRight "No description" 
                $ writePlain def <$> Pandoc nullMeta 
              <$> (fromILOrBlock =<< lookupMeta "description" meta)
        desc' <- liftIO $ runIO desc''
        desc <- ExceptT . return $ mapLeft show desc'
        let thumbnail = lookupMeta "thumbnail" meta >>= fromMetaInlines 
                    |>> stringify |>> imgPath dirpath 
        let tags = lookupMeta "tags" meta & fromMetaList 
                 & mapMaybe (fromMetaInlines |>> stringify)
        return (Entry time fname' (pack title) userId desc thumbnail, tags)

    case filedata of
        Left err -> defaultLayout [whamlet|Error: #{err}|]
        Right (entry, tags) -> do
            res <- runDB $ insert entry
            _ <- runDB $ insertMany $ map (flip Tag res . pack) tags
            defaultLayout [whamlet|SUCCESS|]
  where
    toEither (FormSuccess x) = Right x
    toEither (FormMissing)   = Left "FormMissing"
    toEither (FormFailure y) = Left $ "FormFailure: " ++ show y

    fromMetaList (Just (MetaList x)) = x
    fromMetaList _                   = [] 

    fromMetaInlines (MetaInlines x) = Just x
    fromMetaInlines _               = Nothing

    fromILOrBlock (MetaInlines x) = Just [Plain x]
    fromILOrBlock (MetaBlocks x)  = Just x
    fromILOrBlock _               = Nothing

    imgPath dir img
      | '.' `elem` dropExtension img = img
      | otherwise                    = mkRootIfNot dir </> img

    mkRootIfNot p@('/':_) = p
    mkRootIfNot p         = '/':p


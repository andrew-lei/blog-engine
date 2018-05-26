module ImageFilter where

import Text.Pandoc
import Text.Pandoc.Walk (walk)

import System.FilePath.Posix

fullImagePath :: FilePath -> Pandoc -> Pandoc
fullImagePath fp = walk fullImagePath'
  where
    extern = elem '.' . dropExtension

    fullImagePath' img@(Image x y (url,title))
      | extern url   = img
      | otherwise    = Image x y (fp </> url, title)
    fullImagePath' x = x

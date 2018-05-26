{-# LANGUAGE QuasiQuotes #-}
module MultiFile (fileAFormMult) where

import Yesod.Core
import Yesod.Form
import qualified Data.Map as Map
import Data.Text (pack)

incrInts :: Ints -> Ints
incrInts (IntSingle i) = IntSingle $ i + 1
incrInts (IntCons i is) = (i + 1) `IntCons` is

fileAFormMult :: MonadHandler m
             => FieldSettings (HandlerSite m)
             -> AForm m [FileInfo]
fileAFormMult fs = AForm $ \(master, langs) menvs ints -> do
    let (name, ints') =
            case fsName fs of
                Just x -> (x, ints)
                Nothing ->
                    let i' = incrInts ints
                     in (pack $ 'f' : show i', i')
    id' <- maybe newIdent return $ fsId fs
    let (res, errs) =
            case menvs of
                Nothing -> (FormMissing, Nothing)
                Just (_, fenv) ->
                    case Map.lookup name fenv of
                        Just fi -> (FormSuccess fi, Nothing)
                        _ -> (FormSuccess [], Nothing)
    let fv = FieldView
            { fvLabel = toHtml $ renderMessage master langs $ fsLabel fs
            , fvTooltip = fmap (toHtml . renderMessage master langs) $ fsTooltip fs
            , fvId = id'
            , fvInput = [whamlet|
$newline never
<input type=file name=#{name} ##{id'} *{fsAttrs fs} multiple>
|]
            , fvErrors = errs
            , fvRequired = False
            }
    return (res, (fv :), ints', Multipart)


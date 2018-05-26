module TabFilter where

import Control.Monad.State
import Data.Maybe (mapMaybe)

import Text.Pandoc
import Text.Pandoc.Walk (walkM)

import Text.Printf (printf, PrintfArg)

label :: (PrintfArg p1, PrintfArg p2, Num p1, Eq p1) 
      => p2 -> String -> p1 -> Block
label tgid lang cbid =
  Plain
    [RawInline (Format "html") radioButtonStr
    ,RawInline (Format "html") labelStr
    ,Str lang
    ,RawInline (Format "html") "</label>"
    ]
  where
    tabid = printf "tab-group-%d-tab-%d" tgid cbid
    tabgroupid = printf "tab-group-%d" tgid
    checkedOrNot = if cbid == 0 then "\" checked>" else "\">"
    radioButtonStr = "<input type=\"radio\" id=\""
                  ++ tabid
                  ++ "\" name=\""
                  ++ tabgroupid
                  ++ checkedOrNot
    labelStr = "<label for=\"" ++ tabid ++ "\">"

getLabel :: Block -> Maybe String
getLabel (CodeBlock ("",lang:_,_) _) = Just lang
getLabel (CodeBlock (lab,_,_) _)     = Just lab
getLabel _                           = Nothing

codetabs :: PrintfArg p => p -> [Block] -> [Block]
codetabs tgid content = tabButtons ++ [Div ("",[],[]) tabContent]
  where
    tabButtons = flip evalState (-1 ::Int) $
      mapM autolabel . mapMaybe getLabel $ content
    autolabel cb = modify (+1)
                >> get
               >>= pure . (label tgid cb)
    tabContent = concatMap section content
    section block = [Plain [RawInline (Format "html") "<section>"]
                    ,block
                    ,Plain [RawInline (Format "html") "</section>"]
                    ]


addCodeIdentifiers :: Pandoc -> Pandoc
addCodeIdentifiers doc = evalState (walkM addCodeId doc) 0
  where addCodeId :: Block -> State Int Block
        addCodeId (Div prop@(_,classes,_) content)
          | "tabs" `elem` classes = do
          tgid <- get
          put (tgid + 1)
          pure . Div prop . codetabs tgid $ content
        addCodeId x = return x

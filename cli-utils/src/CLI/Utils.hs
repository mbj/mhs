module CLI.Utils where

import MPrelude

import Options.Applicative

wrapHelper :: Parser a -> String -> ParserInfo a
wrapHelper parser desc = info parser (progDesc desc)

mkCommand :: String -> Parser a -> String -> Mod CommandFields a
mkCommand name parser desc = command name (wrapHelper parser desc)

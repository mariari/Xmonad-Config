module Shell where

import qualified XMonad.Prompt.Shell as Shell
import qualified XMonad.Util.Run     as Run
import qualified XMonad.Prompt       as Prompt
import qualified XMonad

safePrompt ::
  Prompt.XPrompt p => p -> FilePath -> Prompt.XPConfig -> XMonad.X ()
safePrompt grave c config =
  Prompt.mkXPrompt grave config (Shell.getShellCompl [c] $ Prompt.searchPredicate config) run
  where
    run = Run.safeSpawn c . return

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

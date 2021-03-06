{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Text.IO (getContents)
import Data.Version (showVersion)
import Development.GitRev (gitDirty, gitHash)
import Options.Generic
import qualified Paths_rokolisp
import Relude
import RokoLisp
import System.Console.Repline

replEval :: Maybe FilePath -> Text -> IO ()
replEval fp input =
  doEval fp input >>= \case
    VIO io -> io
    x -> print x

replParse :: Text -> IO ()
replParse input = print (parse input)

-- REPL
runREPL :: IO ()
runREPL = evalReplOpts (ReplOpts {..})
  where
    banner = const $ pure ">>> "
    command = liftIO . replEval Nothing . toText
    options =
      [ ("parse", liftIO . replParse . toText),
        ("quit", const (seeYou >> abort)),
        ("help", const help)
      ]
    prefix = Just ':'
    multilineCommand = Just "paste"
    tabComplete = Word0 (const $ pure [])
    initialiser = liftIO $ putTextLn "Welcome to the RokoLisp REPL!"
    finaliser = seeYou >> pure Exit
    seeYou = liftIO $ putTextLn "Goodbye."
    help = liftIO $ do
      putTextLn "Type any expression to normalize it or use one of the following commands:"
      putTextLn ":help"
      putTextLn "    Print help text and describe options"
      putTextLn ":parse term"
      putTextLn "    Print the desugared form"
      putTextLn ":quit"
      putTextLn "    Exit the REPL"

-- CLI
data RokoLispOptions w = RokoLispOptions
  { file :: w ::: Maybe FilePath <?> "Read expression from a file instead of standard input",
    repl :: w ::: Bool <?> "Interpret expressions in a REPL",
    version :: w ::: Bool <?> "Display version"
  }
  deriving stock (Generic)

instance ParseRecord (RokoLispOptions Wrapped)

main :: IO ()
main = do
  args <- unwrapRecord "RokoLisp interpreter"
  if
      | version args -> putText versionText
      | repl args -> runREPL
      | otherwise -> maybe getContents readFileText (file args) >>= replEval (file args)
  where
    versionText = packageVersion <> " " <> commit
    packageVersion = toText . showVersion $ Paths_rokolisp.version
    commit = $(gitHash) <> dirty
    dirty = if $(gitDirty) then "-dirty" else ""

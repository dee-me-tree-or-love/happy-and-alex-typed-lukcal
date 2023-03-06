{-# LANGUAGE DeriveDataTypeable #-}
module CLI.Main (main) where


import qualified Language.Main          as LM
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, modes, typFile, (&=))


data LukCal = Exec {eval :: String,checkTypes :: String,inferTypes :: String}
    | File {eval :: String,checkTypes :: String,inferTypes :: String}
     deriving (Show, Data, Typeable)

expressionHint :: String
expressionHint = "Expression to evaluate"

exec :: LukCal
exec = Exec {
        eval = def &= help expressionHint,
        checkTypes = def &= help expressionHint,
        inferTypes = def &= help expressionHint
    }

fileHint :: String
fileHint = "File to evaluate"

file :: LukCal
file = File {
        eval = def &= help fileHint &= typFile,
        checkTypes = def &= help fileHint &= typFile,
        inferTypes = def &= help fileHint &= typFile
    }

-- CLI output features:
-- - [x] Basic output
-- - [ ] Prettier output
-- - [ ] Exit codes

handle :: LukCal -> String
handle Exec {eval = pIn, checkTypes = "", inferTypes = ""} = show $ LM.evalExpression . LM.stringToAst $ pIn
handle Exec {eval = "", checkTypes = pIn, inferTypes = ""} = show $ LM.checkTypes . LM.annotateAst . LM.stringToAst $ pIn
handle Exec {eval = "", checkTypes = "", inferTypes = pIn} = show $ LM.inferType . LM.stringToAst $ pIn
handle Exec {eval = _, checkTypes = _, inferTypes = _} = error "Only one flag is supported at a time."
handle File {eval = _, checkTypes = _, inferTypes = _} = error "File mode is not implemented yet."

main :: IO ()
main = print . handle =<< cmdArgs (modes [exec,file])

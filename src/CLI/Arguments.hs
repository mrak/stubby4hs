module CLI.Arguments where

import Options.Applicative

data Arguments = Arguments
    { admin :: Int
    , stubs :: Int
    , quiet :: Bool
    , datafile :: String
    , watch :: Bool
    }

adminOption :: Parser Int
adminOption = option auto
     ( long "admin"
    <> short 'a'
    <> metavar "PORT"
    <> help "Port for admin portal. Defaults to 8889.")
    <|> pure 8889

stubsOption :: Parser Int
stubsOption = option auto
     ( long "stubs"
    <> short 's'
    <> metavar "PORT"
    <> help "Port for stubs portal. Defaults to 8882.")
    <|> pure 8882

muteFlag :: Parser Bool
muteFlag = switch
     ( long "mute"
    <> short 'm'
    <> help "Prevent stubby from printing to the console.")

datafileOption :: Parser String
datafileOption = strOption
     ( long "data"
    <> short 'd'
    <> metavar "FILE"
    <> help "Data file to pre-load endoints. YAML or JSON format.")
    <|> pure ""

watchFlag :: Parser Bool
watchFlag = switch
     ( long "watch"
    <> short 'w'
    <> help "Auto-reload data file when edits are made."
     )

options :: Parser Arguments
options = Arguments
    <$> adminOption
    <*> stubsOption
    <*> muteFlag
    <*> datafileOption
    <*> watchFlag

opts :: ParserInfo Arguments
opts = info (helper <*> options)
     ( fullDesc
    <> header "stubby - a small web server for stubbing external systems during development" )

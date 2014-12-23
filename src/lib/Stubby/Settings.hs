module Stubby.Settings (
      Settings
    , getAdmin
    , getStubs
    , getQuiet
    , getDatafile
    , getWatch
    , getLocation

    , defaultSettings
    , stubbyArgs
    ) where

import Options.Applicative

data Settings = Settings
    { admin :: Int
    , stubs :: Int
    , quiet :: Bool
    , datafile :: String
    , watch :: Bool
    , location :: String
    }

defaultSettings :: Settings
defaultSettings = Settings
    { admin = 8889
    , stubs = 8882
    , quiet = True
    , datafile = ""
    , watch = False
    , location = "0.0.0.0"
    }

getAdmin :: Settings -> Int
getAdmin = admin

getStubs :: Settings -> Int
getStubs = stubs

getQuiet :: Settings -> Bool
getQuiet = quiet

getDatafile :: Settings -> String
getDatafile = datafile

getWatch :: Settings -> Bool
getWatch = watch

getLocation :: Settings -> String
getLocation = location

adminOption :: Parser Int
adminOption = option auto
     ( long "admin"
    <> short 'a'
    <> metavar "PORT"
    <> help "Port for admin portal. Defaults to 8889.")
   <|> pure (getAdmin defaultSettings)

stubsOption :: Parser Int
stubsOption = option auto
     ( long "stubs"
    <> short 's'
    <> metavar "PORT"
    <> help "Port for stubs portal. Defaults to 8882.")
   <|> pure (getStubs defaultSettings)

quietFlag :: Parser Bool
quietFlag = switch
     ( long "quiet"
    <> short 'q'
    <> help "Prevent stubby from printing to the console.")

datafileOption :: Parser String
datafileOption = strOption
     ( long "data"
    <> short 'd'
    <> metavar "FILE"
    <> help "Data file to pre-load endoints. YAML or JSON format.")
   <|> pure (getDatafile defaultSettings)

watchFlag :: Parser Bool
watchFlag = switch
     ( long "watch"
    <> short 'w'
    <> help "Auto-reload data file when edits are made."
     )

locationOption :: Parser String
locationOption = strOption
     ( long "location"
    <> short 'l'
    <> metavar "ADDRESS"
    <> help "Network address at which to bind stubby.")
   <|> pure (getLocation defaultSettings)

options :: Parser Settings
options = Settings
    <$> adminOption
    <*> stubsOption
    <*> quietFlag
    <*> datafileOption
    <*> watchFlag
    <*> locationOption

stubbyArgs :: IO Settings
stubbyArgs = execParser $ info (helper <*> options)
     ( fullDesc
    <> header "stubby - a small web server for stubbing external systems during development" )

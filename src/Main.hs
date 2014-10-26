{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent
import           Control.Exception   (SomeException, catch, try)
import qualified Data.Text           as T
import           Site
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           System.IO

#ifdef DEVELOPMENT
import Snap.Loader.Dynamic
#else
import Snap.Loader.Static
#endif

import Cron
import Package

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])
    threads <- mapM forkIO [ cron 283 doUpdateHackage
                          , cron 337 doUpdatePackages
                          ]
    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    _ <- mapM killThread threads
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)

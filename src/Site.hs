{-# LANGUAGE OverloadedStrings #-}

module Site where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Acid.Advanced     as A
import           Data.ByteString        (ByteString)
import qualified Data.Text              as T
import           Heist
import qualified Heist.Interpreted      as I
import           Snap.Snaplet
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           System.Process

import Application
import Package

handleIndex :: AppHandler ()
handleIndex = do hackage <- query QueryHackage
                 packages <- query QueryPackages
                 renderWithSplices "base" (packageSplices hackage packages)

packageSplices :: Hackage -> [Package] -> Splices (SnapletISplice App)
packageSplices hackage packages = "packages" ## renderPackages hackage packages

renderPackages :: Hackage -> [Package] -> SnapletISplice App
renderPackages hackage = I.mapSplices $ I.runChildrenWith . packageSplice hackage

packageSplice :: Monad m => Hackage -> Package -> Splices (I.Splice m)
packageSplice hackage p@(Package pn pv) =
  do "packageName" ## I.textSplice pn
     "packageVersion" ## I.textSplice $ T.pack . showVersion $ pv
     "hackageVersion" ## I.textSplice $ T.pack . showVersion $ hv
     "packageStatus" ## I.textSplice . statusColor $ pv
  where hv = hackageVersion hackage p
        statusColor pv = if pv < hv
                            then "#f9bdbb"
                            else "#d0f8ce"

handleUpdateHackage :: AppHandler ()
handleUpdateHackage =
  do a <- getAcidState
     liftIO . void . forkIO . void $ cabalUpdate >> readHackage >>= A.scheduleUpdate a . UpdateHackage
  where cabalUpdate = callCommand "cabal update"

handleUpdatePackages :: AppHandler ()
handleUpdatePackages =
  do a <- getAcidState
     liftIO . void . forkIO . void $ readPackages >>= A.scheduleUpdate a . UpdatePackages

routes :: [ (ByteString, AppHandler ()) ]
routes = [ ("/",               handleIndex)
         , ("/updateHackage",  handleUpdateHackage)
         , ("/updatePackages", handleUpdatePackages)
         , ("/static",         serveDirectory "static")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit initState
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes routes
    return $ App a h

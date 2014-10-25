{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Application where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.SafeCopy
import           Data.Typeable
import qualified Distribution.Hackage.DB as H
import           Snap.Snaplet
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Heist

import Package

data AppState = AppState { hackage  :: Hackage
                         , packages :: [Package]
                         } deriving (Show, Eq, Typeable)

initState :: AppState
initState = AppState H.empty []

$(deriveSafeCopy 0 'base ''H.Version)
$(deriveSafeCopy 0 'base ''Package)
$(deriveSafeCopy 0 'base ''AppState)

queryHackage :: Query AppState Hackage
queryHackage = asks hackage

queryPackages :: Query AppState [Package]
queryPackages = asks packages

updateHackage :: Hackage -> Update AppState ()
updateHackage hackage = do AppState _ packages <- get
                           put $ AppState hackage packages

updatePackages :: [Package] -> Update AppState ()
updatePackages packages = do AppState hackage _ <- get
                             put $ AppState hackage packages

$(makeAcidic ''AppState ['queryHackage, 'queryPackages, 'updateHackage, 'updatePackages])

data App = App
    { _acid  :: Snaplet (Acid AppState)
    , _heist :: Snaplet (Heist App)
    }

$(makeLenses ''App)

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App AppState where
    getAcidStore = view (acid . snapletValue)

type AppHandler = Handler App App

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Package where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text    as P
import qualified Data.List               as L
import qualified Data.Text               as T
import           Data.Typeable
import qualified Distribution.Hackage.DB as H
import qualified Distribution.Version    as V
import           Github.GitData.Trees    (GitTree (..), nestedTree,
                                          treeGitTrees)

type Path = T.Text
type Tree = [T.Text]

type Hackage = H.Map String [V.Version]

getTree :: IO Tree
getTree = do eTree <- nestedTree "gentoo-haskell" "gentoo-haskell" "master"
             tree <- either (fail . show) return eTree
             return $ T.pack . gitTreePath <$> filter validEbuild (treeGitTrees tree)

validEbuild :: GitTree -> Bool
validEbuild g = gitTreeType g == "blob" &&
                ".ebuild" `L.isSuffixOf` gitTreePath g

data Ebuild = Ebuild { ebuildCategory :: T.Text
                     , ebuildPackage  :: T.Text
                     , ebuildVersion  :: T.Text
                     } deriving (Show, Eq)

data Package = Package { packageName    :: T.Text
                       , packageVersion :: V.Version
                       } deriving (Show, Eq, Ord, Typeable)

parseEbuildVersion :: P.Parser T.Text
parseEbuildVersion = do chars <- P.manyTill P.anyChar (void ".ebuild" <|> P.endOfInput)
                        return $ T.pack chars

parseEbuild :: P.Parser Ebuild
parseEbuild = do cat <- P.takeWhile1 (/= '/')
                 _ <- P.char '/'
                 pkg <- P.takeWhile1 (/='/')
                 _ <- P.char '/'
                 _ <- P.string pkg
                 _ <- P.char '-'
                 ver <- parseEbuildVersion
                 return $ Ebuild cat pkg ver

ebuildInPath :: Path -> Either String Ebuild
ebuildInPath = P.parseOnly parseEbuild

ebuildsInTree :: Tree -> Either String [Ebuild]
ebuildsInTree = mapM ebuildInPath

parsePackageVersion :: P.Parser V.Version
parsePackageVersion = do branches <- P.decimal `P.sepBy1` P.char '.'
                         return $ V.Version branches []

packageInEbuild :: Ebuild -> Either String Package
packageInEbuild ebuild = do ver <- P.parseOnly parsePackageVersion (ebuildVersion ebuild)
                            return $ Package (ebuildPackage ebuild) ver

packagesInEbuilds :: [Ebuild] -> Either String [Package]
packagesInEbuilds = mapM packageInEbuild

packagesInTree :: Tree -> Either String [Package]
packagesInTree = (packagesInEbuilds =<<) . ebuildsInTree

notLive :: Package -> Bool
notLive (Package _ (V.Version branches _)) = branches < [9,9,9,9]

latest :: [Package] -> [Package]
latest ps = maximum <$> L.groupBy (\x y -> packageName x == packageName y) ps

data PackageStatus = Old | New | Unknown deriving (Show, Eq, Ord)

hackageVersion :: Hackage -> Package -> V.Version
hackageVersion hackage package =
  case H.lookup (T.unpack . packageName $ package) hackage of
    Just m ->  maximum m
    Nothing -> packageVersion package

packageStatus :: Hackage -> Package -> PackageStatus
packageStatus hackage package =
  case H.lookup (T.unpack . packageName $ package) hackage of
    Just m -> if L.any (packageVersion package <) m
                then Old
                else New
    Nothing -> Unknown

showVersion :: V.Version -> String
showVersion (V.Version vs _) = L.foldl1 (\a b -> a ++ "." ++ b) $ show `fmap` vs

buildHackage :: H.Hackage -> Hackage
buildHackage = H.map H.keys

readHackage :: IO Hackage
readHackage = buildHackage <$> H.readHackage

readPackages :: IO [Package]
readPackages = do tree <- getTree
                  packages <- either fail return $ packagesInTree tree
                  let notLivePackages = filter notLive packages
                      latestPackages = latest notLivePackages
                  return latestPackages

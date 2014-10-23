{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text    as P
import qualified Data.List               as L
import qualified Data.Text               as T
import qualified Distribution.Hackage.DB as H
import qualified Distribution.Version    as V
import           Github.GitData.Trees    (GitTree (..), nestedTree,
                                          treeGitTrees)

type Tree = [T.Text]
type Path = T.Text

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
                       } deriving (Show, Eq)

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

data PackageStatus = Old | New | Unknown deriving (Show, Eq, Ord)

packageStatus :: H.Hackage -> Package -> PackageStatus
packageStatus hackage package =
  case H.lookup (T.unpack . packageName $ package) hackage of
    Just m -> if L.any (packageVersion package <) (H.keys m)
                then Old
                else New
    Nothing -> Unknown

render :: H.Hackage -> Package -> IO ()
render h p@(Package name version) = do putStr . show $ name
                                       putStr " -> "
                                       putStr . showVersion $ version
                                       print $ " -> " ++ show (packageStatus h p)

showVersion :: V.Version -> String
showVersion (V.Version vs _) = L.foldl1 (\a b -> a ++ "." ++ b) $ show `fmap` vs

main :: IO ()
main = do tree <- getTree
          hackage <- H.readHackage
          packages <- either fail return $ packagesInTree tree
          forM_ packages $ render hackage

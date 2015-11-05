{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.List
import Hakyll
import System.FilePath
import System.Exit

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

  match "pages/*" $ do
      route pageRoute
      compile $ getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/layout-default.html" defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "posts/*" $ do
      route postRoute
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"   postCtx
          >>= loadAndApplyTemplate "templates/layout-post.html" postCtx
          >>= loadAndApplyTemplate "templates/layout.html" postCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"

          let indexCtx =
                  listField "posts" postCtx (return posts) <>
                  constField "title" "Home"                <>
                  defaultContext

          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/layout-default.html" indexCtx
              >>= loadAndApplyTemplate "templates/layout.html" indexCtx
              >>= relativizeUrls
              >>= cleanIndexUrls

  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

pageRoute :: Routes
pageRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeBaseName p </> "index.html"
                            where p = toFilePath ident

postRoute :: Routes
postRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

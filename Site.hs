{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.List
import Hakyll
import System.FilePath
import System.Exit

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle = "Alex Mingoia"
  , feedDescription = "Alex Mingoia's blog."
  , feedAuthorName = "Alex Mingoia"
  , feedAuthorEmail = "talk@alexmingoia.com"
  , feedRoot = "https://www.alexmingoia.com" }

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
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "posts/*" $ do
      route cleanRoute
      compile $ pandocCompiler
          >>= saveSnapshot "post"
          >>= loadAndApplyTemplate "templates/post.html" postCtx
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
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= loadAndApplyTemplate "templates/layout.html" indexCtx
              >>= relativizeUrls
              >>= cleanIndexUrls

  create ["index.xml"] $ do
    route (constRoute "feed/index.xml")
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "post"
      renderRss feedConfig feedCtx posts >>= cleanIndexUrls

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

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

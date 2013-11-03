{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do

     match "images/*" $ do
         route idRoute
         compile copyFileCompiler

     match "css/*" $ do
         route idRoute
         compile copyFileCompiler

     match "posts/*" $ do
         route $ setExtension "html"
         compile $ pandocCompiler
             >>= loadAndApplyTemplate "templates/post.html" postCtx
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= relativizeUrls

     match "templates/*" $ compile templateCompiler

------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

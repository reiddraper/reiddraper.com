{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do

     match "images/*" $ do
         route idRoute
         compile copyFileCompiler

     css

     match "posts/*" $ do
         route $ composeRoutes (gsubRoute "posts/" (const "")) (setExtension "html")
         compile $ pandocCompiler
             >>= loadAndApplyTemplate "templates/post.html" postCtx
             >>= loadAndApplyTemplate "templates/default.html" defaultContext
             >>= relativizeUrls

     match "templates/*" $ compile templateCompiler

------------------------------------------------------------------------------

matchCss :: Rules ()
matchCss =  match "css/*" $ compile getResourceBody

buildConcatenatedCss :: Rules ()
buildConcatenatedCss = create ["site.css"] $ do
                         route $ constRoute "css/site.css"
                         compile concatenateAndCompress

concatenateCss :: Compiler (Item [Char])
concatenateCss = do
    items <- loadCss
    makeItem $ concatMap itemBody (items :: [Item String])

concatenateAndCompress :: Compiler (Item String)
concatenateAndCompress = concatenateCss >>= (return . compressCssItem)

compressCssItem :: (Item String) -> (Item String)
compressCssItem = fmap compressCss

-- Explicitly load the css in this order, as it's
-- the order we want to concatenate them in
loadCss :: Compiler [Item String]
loadCss = mapM load ["css/normalize.css",
                      "css/syntax.css",
                      "css/application.css"]

css :: Rules ()
css = matchCss >> buildConcatenatedCss

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

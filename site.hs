{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.String.Utils (replace)
import Hakyll

main :: IO ()
main = hakyll $ do
    images
    index
    posts
    templates
    css
    atom

------------------------------------------------------------------------------

images :: Rules ()
images = match "images/*" $ do
             route idRoute
             compile copyFileCompiler

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

posts :: Rules ()
posts = match "posts/*" $ do
            route $ composeRoutes (composeRoutes (gsubRoute "posts/" (const "")) (gsubRoute ".md" (const "/index.md"))) (setExtension "html")
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= saveSnapshot "postContent"
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

index :: Rules ()
index = create ["index.html"] $ do
            route idRoute
            compile $ do
                blogPosts <- recentFirst =<< loadAll "posts/*"
                let indexContext = listField "posts" postCtx (return blogPosts) `mappend`
                                   defaultContext
                makeItem "" >>= loadAndApplyTemplate "templates/index.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Reid Draper" `mappend` defaultContext)
                >>= (return . removeIndexFromUrls)


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

removeIndexFromUrls :: (Item String) -> (Item String)
removeIndexFromUrls = fmap $ withUrls (replace "index.html" "")

postCtx :: Context String
postCtx =
    dateField "date" "%b %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle         = "Reid Draper's blog"
    , feedDescription   = "Reid Draper's blog"
    , feedAuthorName    = "Reid Draper"
    , feedAuthorEmail   = "reiddraper@gmail.com"
    , feedRoot          = "reiddraper.com"
    }

atom :: Rules ()
atom = create ["atom.xml"] $ do
           route idRoute
           compile $ do
               let feedCtx = postCtx `mappend` bodyField "description"
               blogPosts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*" "postContent"
               renderAtom feedConfiguration feedCtx blogPosts

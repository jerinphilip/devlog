
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

createPage :: String -> Rules()
createPage page = create [ fromFilePath (page++".html") ] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (fromGlob ("posts/"++page++"/*"))
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    let (c:cs) = map (\x-> fromGlob (x ++"/**")) ["images", "js", "font"]
    let toCopyPattern = foldr (.||.) c cs
    match toCopyPattern $ do
        route   idRoute
        compile copyFileCompiler
    
    match "css/**.css" $ do
        route   idRoute
        compile compressCssCompiler


    match "posts/**" $ do
        -- route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            -- >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    let pages = ["index", "web", "android", "system"]
    sequence_ $ map createPage pages

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--byurakn.am
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath.Posix  (  takeBaseName, splitDirectories, (</>)
                                         , addExtension, replaceExtension, dropExtension)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do


   -- copy static assets
    let assets = ["images/*", "images/slide/*", "js/*", 
                  "images/slide/*/*",  
                  "CNAME",
                  "pages/events/2014/images/*"]

    match (foldr1 (.||.) assets) $ do
        route   idRoute
        compile copyFileCompiler
      
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    let dzernarks = ["pages/dzernark/*", "pages/dzernark/*/*"]
    match (foldr1 (.||.) dzernarks) $ do
        route $ customRoute $ (processPagesRoute "dzernark") . toFilePath
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/dzernark.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" baseCtx
            >>= relativizeUrls

    let socailEvents = ["pages/events/*/*"]
    match (foldr1 (.||.) socailEvents) $ do
        route $ customRoute $ (processPagesRoute "pages/events") . toFilePath
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/event.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" baseCtx
            >>= relativizeUrls        

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    baseSliderCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    match ("pages/events/index.html") $ do
        route $ customRoute $  (processPagesRoute "events") .  toFilePath
        compile $ do
            posts2014 <- recentFirst =<< loadAll "pages/events/2014/*"
            let indexCtx = listField "posts2014" postCtx (return posts2014) `mappend`
                           baseCtx  
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

baseCtx :: Context String
baseCtx =
    constField "year" "2014" `mappend`
    defaultContext

baseSliderCtx :: Context String
baseSliderCtx =
    constField "withSlider" "true" `mappend`
    baseCtx

processPagesRoute :: FilePath -> [Char] -> FilePath
processPagesRoute root path =
    root </>  year </> (replaceExtension fileName "html")
         where fileName =  last (splitDirectories path)
               year | length (splitDirectories path) < 4  = ""
                    | otherwise = last $ init (splitDirectories path)
                       
    

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Extensions (githubMarkdownExtensions)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    -- Route and compile static files

    match "templates/*" $ compile templateCompiler

    match "web-content/*.md" $ do
        compile $ pandocCompiler

    match "web-content/robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "web-content/script.js" $ do
        route idRoute
        compile copyFileCompiler

    match "web-content/style.css" $ do
        route idRoute
        compile copyFileCompiler

    -- Route and compile HTML files
    -- match "web-content/index.html" $ do
    --     route $ constRoute "index.html"
    --     compile $ do
    --         snippet <- loadBody "web-content/snippet.md"
    --         let indexContext = constField "snippet" snippet `mappend` defaultContext
    --         getResourceBody
    --             >>= applyAsTemplate indexContext
    --             >>= loadAndApplyTemplate "templates/default.html" indexContext
    --             >>= relativizeUrls
    -- Route and compile HTML files
    match "web-content/**/*.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Special handling for Markdown or text files
    match "web-content/jumprope/*.txt" $ do
        route idRoute
        compile copyFileCompiler

    -- match "images/*" $ do
    --     route   idRoute
    --     compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

    -- Custom Pandoc options for GitHub Flavored Markdown

    -- Configure reader options for GFM specifically
    let readerOptions = defaultHakyllReaderOptions {
        readerExtensions = Text.Pandoc.Extensions.githubMarkdownExtensions
    }

    let writerOptions = defaultHakyllWriterOptions {
        writerExtensions = Text.Pandoc.Extensions.githubMarkdownExtensions
    }

    -- Custom Pandoc compiler that uses the GFM-specific reader options
    let customPandocCompiler = pandocCompilerWith readerOptions writerOptions


    match "posts/*" $ do
        route $ setExtension "html"
        compile $ customPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= relativizeUrls

    -- match "posts/index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let indexCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Posts Archive" <>
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            snippet <- loadBody "web-content/snippet.md"
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    constField "snippet" snippet             `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- match "templates/*" $ compile templateCompiler


    -- create ["posts/index.html"] $ do  -- Changed from "archive.html" to "posts/index.html"
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "author" "Timothy Williams" `mappend`
    defaultContext


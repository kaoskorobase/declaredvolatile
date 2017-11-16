module Main where

import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (Day(..))
import           Data.Time.ISO8601
import           DeclaredVolatile.BlogPost
import           DeclaredVolatile.Html
import           DeclaredVolatile.Layout
import qualified DeclaredVolatile.Pixyll as Pixyll
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Atom.Feed
import           Text.Atom.Feed.Export (xmlFeed)
import           Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Pandoc
import           Text.Pandoc.Error (handleError)
import           Text.XML.Light.Output (ppTopElement)

hsDeps :: Action [FilePath]
hsDeps = do
  hs <- getDirectoryFiles "" ["DeclaredVolatile//*.hs"]
  return $ ["declaredvolatile.hs"] ++ hs

cssDeps :: Action [FilePath]
cssDeps = map ("build" </>) <$> getDirectoryFiles "" ["css/*.css"]

site = Pixyll.Site {
    Pixyll.siteTitle = "declared volatile"
  , Pixyll.siteDescription = "Born human, declared volatile."
  , Pixyll.siteBaseUrl = ""
  , Pixyll.siteAuthor = "Kaos Korobase"
  , Pixyll.siteKeywords = words "stefan kersten kaos korobase sex drugs rock'n roll"
  , Pixyll.siteMenu = [ encodeLink "Email" "mailto:kaoskorobase@gmail.com"
                      -- , H.a H.! H.href (H.toValue "https://twitter.com/kaoskorobase") $ H.toHtml "Twitter"
                      , H.a H.! H.href (H.toValue "https://dolby.com") $ H.toHtml "Work"
                      , H.a H.! H.href (H.toValue "https://github.com/kaoskorobase") $ H.toHtml "Github"
                      , H.a H.! H.href (H.toValue "https://www.linkedin.com/pub/stefan-kersten/19/91b/194") $ H.toHtml "LinkedIn" ]
  }

postToPage :: BlogPost -> Pixyll.Page
postToPage post = Pixyll.Page {
    Pixyll.pageTitle = postTitle post
  , Pixyll.pageDate = postDate post
  , Pixyll.pageSummary = ""
  , Pixyll.pageUrl = postUrl post
  }

copy :: FilePath -> Rules ()
copy pattern = "build" </> pattern %> \out -> copyFile' (dropDirectory1 out) out


main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "build/" } $ do
  let build = (</>) "build"

  phony "clean" $ removeFilesAfter "build" ["//*"]

  copy "LICENSE.txt"

  copy "css/*.css"

  getPost <- newCache $ \path -> do
    pandoc@(Pandoc meta _) <- handleError . readMarkdown def <$> readFile' path
    Stdout gitDate <- cmd "git log -1 --format=%ci --" [path]
    let -- Convert title from Pandoc metadata to string
        title = writeAsciiDoc def (Pandoc nullMeta [Plain (docTitle meta)])
        Just cDate = parseISO8601
                   . (++"T00:00:00Z") -- Append time
                   . intercalate "-" . take 3 . splitOn "-" -- Filename starts with 'year-month-day'
                   . takeFileName . dropExtension
                   $ path
        Just mDate = case lines gitDate of
                      [] -> return cDate -- If not checked in yet
                      (x:_) -> parseISO8601
                            . (\[d,t,z] -> d ++ "T" ++ t ++ z) . words -- Convert to proper ISO8601 date
                            $ x
        url = dropExtension path
    return $ BlogPost title cDate mDate url pandoc

  getPosts <- newCache $ \() -> mapM getPost =<< getDirectoryFiles "" ["blog/*.md"]

  build "blog/*/index.html" %> \out -> do
    hs <- hsDeps
    css <- cssDeps
    need $ hs ++ css
    let src = (<.>".md") . dropDirectory1 . takeDirectory $ out
    post <- getPost src
    writeFile' out
      . renderHtml
      . Pixyll.post site (postToPage post)
      $ postHtml post

  build "atom.xml" %> \out -> do
    posts <- getPosts ()
    let mkEntry p =
          (nullEntry (("tag:"++) . takeFileName . postUrl $ p)
                    (TextString . postTitle $ p)
                    (formatISO8601 . postLastModificationDate $ p))
          {
            entryContent = Just . TextContent . writeAsciiDoc def . postBody $ p
          , entryLinks = [nullLink $ "http://declaredvolatile.org/" ++ postUrl p]
          }
        feed = (nullFeed "tag:declared-volatile.org,2014"
                        (TextString "Declared Volatile")
                        (formatISO8601 . maximum . map postLastModificationDate $ posts))
                {
                  feedEntries = map mkEntry posts
                }
    writeFile' out $ ppTopElement $ xmlFeed feed

  build "index.html" %> \out -> do
    hs <- hsDeps
    css <- cssDeps
    posts <- getPosts ()
    need $    hs
           ++ css
           ++ map (combine "build" . flip combine "index.html" . postUrl) posts
           ++ [build "atom.xml"]
           ++ [build "LICENSE.txt"]
    writeFile' out
      . renderHtml
      . Pixyll.index site
      -- Sort posts latest first
      . map postToPage
      . sortBy (\a b -> compare (Down (postDate a)) (Down (postDate b)))
      $ posts

  let needSite = need [build "index.html"]

  phony "build" needSite

  -- Install warp web server if needed
  let server = ".cabal-sandbox/bin/warp"

  server %> \_ -> do
    command_ [] "cabal" ["sandbox", "init"]
    command_ [] "cabal" ["install", "-j", "wai-app-static"]

  phony "run" $ do
    -- need [server]
    needSite
    command_ [Cwd "build"] server []

  phony "deploy" $ do
    needSite
    command_ [Shell] "./deploy.sh" []

  want ["build"]

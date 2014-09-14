module Main where

import           Control.Applicative hiding ((*>))
import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Time.ISO8601
import           Development.Shake
import           Development.Shake.FilePath
import           Index
import           Text.Atom.Feed
import           Text.Atom.Feed.Export (xmlFeed)
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Pandoc
import           Text.XML.Light.Output (ppTopElement)

hsDeps :: Action [FilePath]
hsDeps = return ["AsciiArt.hs", "Index.hs", "Rot13.hs"]

cssDeps :: Action [FilePath]
cssDeps = map ("build" </>) <$> getDirectoryFiles "" ["css/*.css"]

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "build/" } $ do
  let build = (</>) "build"

  phony "clean" $ removeFilesAfter "build" ["//*"]

  build "css/*.css" *> \out ->
    copyFile' (dropDirectory1 out) out

  getPost <- newCache $ \path -> do
    pandoc@(Pandoc meta _) <- readMarkdown def <$> readFile' path
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

  build "blog/**/index.html" *> \out -> do
    hs <- hsDeps
    css <- cssDeps
    need $ hs ++ css
    let src = (<.>".md") . dropDirectory1 . takeDirectory $ out
    getPost src >>= writeFile' out . renderHtml . post "../.."

  build "atom.xml" *> \out -> do
    posts <- getPosts ()
    let mkEntry p =
          (nullEntry (("tag:"++) . takeFileName . postUrl $ p)
                    (TextString . postTitle $ p)
                    (formatISO8601 . postLastModificationDate $ p))
          {
            entryContent = Just . TextContent . writeAsciiDoc def . postBody $ p
          , entryLinks = [nullLink $ "http://space.k-hornz.de/" ++ postUrl p]
          }
        feed = (nullFeed "tag:declared-volatile.org,2014"
                        (TextString "Declared Volatile Blog")
                        (formatISO8601 . maximum . map postLastModificationDate $ posts))
                {
                  feedEntries = map mkEntry posts
                }
    writeFile' out $ ppTopElement $ xmlFeed feed

  build "index.html" *> \out -> do
    hs <- hsDeps
    css <- cssDeps
    posts <- getPosts ()
    need $    hs
           ++ css
           ++ map (combine "build" . flip combine "index.html" . postUrl) posts
           ++ [build "atom.xml"]
    writeFile' out
      . renderHtml . index ""
      -- Sort posts latest first
      . sortBy (\a b -> compare (Down (postDate a)) (Down (postDate b)))
      $ posts

  let needSite = need [build "index.html"]

  phony "build" needSite

  -- Install warp web server if needed
  let server = ".cabal-sandbox/bin/warp"

  server *> \_ -> do
    command_ [] "cabal" ["sandbox", "init"]
    command_ [] "cabal" ["install", "-j", "wai-app-static"]

  phony "run" $ do
    -- need [server]
    needSite
    command_ [Cwd "build"] server []

  phony "deploy" $ do
    needSite
    command_ [Shell] "./deploy.sh" []

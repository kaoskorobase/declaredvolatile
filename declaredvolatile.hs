module Main where

import           Data.List
import           Data.List.Split
import           Data.Ord
import           Development.Shake
import           Development.Shake.FilePath
import           Index
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Pandoc


markdownToHtml :: String -> Html
markdownToHtml = (writeHtml def { writerReferenceLinks = True
                                , writerHtml5 = True
                                , writerHighlight = True })
               . readMarkdown def

getPost :: FilePath -> Action BlogPost
getPost path = do
  let date = intercalate "-" . take 3 . splitOn "-" . takeFileName . dropExtension $ path
  contents <- lines `fmap` readFile' path
  let title = drop 2 . head $ contents
      url = dropExtension path
  return $ BlogPost title date url (markdownToHtml . unlines . tail $ contents)

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "build/" } $ do
  let build = (</>) "build"

  phony "clean" $ removeFilesAfter "build" ["//*"]

  build "css/*.css" *> \out ->
    copyFile' (dropDirectory1 out) out

  build "blog/**/index.html" *> \out -> do
    css <- getDirectoryFiles "" ["css/*.css"]
    need $ ["Index.hs"] ++ map ("build" </>) css
    let src = (<.>".md") . dropDirectory1 . takeDirectory $ out
    getPost src >>= writeFile' out . renderHtml . post "../.."

  build "blog/feed.xml" *> \out -> do
    writeFile' out "<feed/>\n"

  build "index.html" *> \out -> do
    css <- getDirectoryFiles "" ["css/*.css"]
    posts <- getDirectoryFiles "" ["blog/*.md"]
    need $ ["AsciiArt.hs", "Index.hs", build "blog/feed.xml"]
           ++ map ("build" </>) css
           ++ map (combine "build" . flip combine "index.html" . dropExtension) posts
    -- Sort posts latest first
    mapM getPost (sortBy (\a b -> compare (Down a) (Down b)) posts)
      >>= writeFileChanged out . renderHtml . index ""

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

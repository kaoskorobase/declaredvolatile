% Writing a simple Blog with Shake and Pandoc
%
% 2014-09-14

I've been using the [Shake](https://hackage.haskell.org/package/shake) build framework a lot lately, so instead of learning a new framework, such as [Hakyll](http://jaspervdj.be/hakyll/), I thought I'd write a simple static blog generator using Shake, [Shakespeare](https://hackage.haskell.org/package/shakespeare) templates and the [Pandoc](https://hackage.haskell.org/package/pandoc) Markdown converter.

**TL;DR** If you already know how to use Shake or would like to learn it for other projects, a simple static blog site generator can be written in about 500 lines of code, including HTML and CSS templates.

First, let's start with the definition of a `BlogPost`:

```haskell
data BlogPost = BlogPost {
    postTitle :: String
  , postDate :: UTCTime
  , postLastModificationDate :: UTCTime
  , postUrl :: String
  , postBody :: Pandoc
  }
```

Now we incrementally define the Shake rules needed for building the site, starting from the things we need and then telling Shake how to produce them. First off, we define a `main` function and tell Shake where to locate the build database files:

~~~~haskell
main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "build/" } $ do
  -- Put shake build rules here ...
~~~~

We define a convenience function for specifying outputs in the build directory and add a `clean` target:

~~~~haskell
let build = (</>) "build"

phony "clean" $ removeFilesAfter "build" ["//*"]
~~~~

Now the first interesting definition: We read a `BlogPost` from disk and fill the various metadata fields such as title, creation date and modification date. We store the post body as a Pandoc document for later rendering:

~~~~haskell
getPost <- newCache $ \path -> do
  pandoc@(Pandoc meta _) <- readMarkdown def <$> readFile' path
  Stdout gitDate <- cmd "git log -1 --format=%ci --" [path]
  let -- Convert title from Pandoc metadata to string
      title = writeAsciiDoc
                def
                (Pandoc nullMeta [Plain (docTitle meta)])
      Just cDate = parseISO8601
                 . (++"T00:00:00Z") -- Append time
                 -- Filename starts with 'year-month-day'
                 . intercalate "-" . take 3 . splitOn "-"
                 . takeFileName . dropExtension
                 $ path
      Just mDate = case lines gitDate of
                    [] -> return cDate -- If not checked in yet
                    (x:_) -> parseISO8601
                          -- Convert to proper ISO8601 date
                          . (\[d,t,z] -> d ++ "T" ++ t ++ z) . words
                          $ x
      url = dropExtension path
  return $ BlogPost title cDate mDate url pandoc
~~~~

Here we use the `iso8601-time` library for parsing dates; inputs need to be massaged a little into the correct format. The post creation time is taken from the filename which is assumed to follow the convention `YEAR-MONTH-DAY-post-title-slug.md`. The modification time is taken to be the last git commit time; this will be useful later when generating an Atom feed for our blog.

Note the use of Shake's powerful [newCache](http://hackage.haskell.org/package/shake-0.13.2/docs/Development-Shake.html#g:14) combinator, which ensures that the blog post data is read only once from disk and cached in memory for future accesses.

We define another helper function, a cached list of all processed blog posts residing in the `blog` directory:

~~~~haskell
getPosts <- newCache $ \() ->
  mapM getPost =<< getDirectoryFiles "" ["blog/*.md"]
~~~~

Now we can start to declare the blog's output files. Let's start with `index.html`:  We depend on various haskell modules and the generated CSS files. We also need the individual blog posts HTML files and the blog's Atom feed file. Finally we render the `index` HTML template with a list of posts sorted latest first:

~~~~haskell
let hsDeps = return ["AsciiArt.hs", "Index.hs", "Rot13.hs"]
    cssDeps = map ("build" </>)
                <$> getDirectoryFiles "" ["css/*.css"]

build "index.html" *> \out -> do
  hs <- hsDeps
  css <- cssDeps
  posts <- getPosts ()
  need $    hs
         ++ css
         ++ map ( combine "build"
                . flip combine "index.html"
                . postUrl)
                posts
         ++ [build "atom.xml"]
  writeFile' out
    . renderHtml . index ""
    -- Sort posts latest first
    . sortBy (\a b ->
        compare (Down (postDate a))
                (Down (postDate b)))
    $ posts
~~~~

CSS files in the `css` directory just get copied without modifications:

~~~~haskell
build "css/*.css" *> \out ->
  copyFile' (dropDirectory1 out) out
~~~~

Next, we need to add a rule for building the blog posts referenced from the rule above. We depend again on the same Haskell modules and CSS files and compute the Markdown source file path from the output file path in the build directory. The Markdown source is then rendered using the `post` HTML template:

~~~~haskell
build "blog/**/index.html" *> \out -> do
  hs <- hsDeps
  css <- cssDeps
  need $ hs ++ css
  let src = (<.>".md") . dropDirectory1 . takeDirectory $ out
  getPost src >>= writeFile' out . renderHtml . post "../.."
~~~~

The last missing piece is the Atom feed XML file; in the following rule the [feed](https://hackage.haskell.org/package/feed) library is used to generate an Atom feed based on the list of blog posts and their modification times:

~~~~haskell
build "atom.xml" *> \out -> do
  posts <- getPosts ()
  let mkEntry p =
        (nullEntry
          (("tag:"++) . takeFileName . postUrl $ p)
          (TextString . postTitle $ p)
          (formatISO8601 . postLastModificationDate $ p))
        {
          entryContent =
            Just . TextContent
                 . writeAsciiDoc def
                 . postBody $ p
        , entryLinks =
            [nullLink $ "http://space.k-hornz.de/" ++ postUrl p]
        }
      feed = (nullFeed
                "tag:declared-volatile.org"
                (TextString "Declared Volatile Blog")
                ( formatISO8601
                . maximum
                . map postLastModificationDate $ posts))
              {
                feedEntries = map mkEntry posts
              }
  writeFile' out $ ppTopElement $ xmlFeed feed
~~~~

That's it! Now let's just add a phony target for building the blog and we're done:

~~~~haskell
  phony "build" $ need [build "index.html"]
~~~~

The rest of the source code and the HTML/CSS templates can be found [here](https://github.com/kaoskorobase/declaredvolatile). It includes some more goodies like serving the site locally via [wai-app-static](https://hackage.haskell.org/package/wai-app-static) and code for generating the ASCII logo.

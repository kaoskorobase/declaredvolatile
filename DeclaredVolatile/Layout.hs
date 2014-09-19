{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DeclaredVolatile.Layout (index, post) where

import           Data.Time
import           Data.Time.Format
import           DeclaredVolatile.BlogPost
import           DeclaredVolatile.Html (encodeLink)
import qualified DeclaredVolatile.Logo.AsciiArt as AsciiArt
import           DeclaredVolatile.Rot13 (rot13)
import           Development.Shake.FilePath
import           System.Locale (defaultTimeLocale)
import           Text.Blaze.Html (Html)
import           Text.Hamlet (shamlet)
import           Text.Lucius (Css, lucius, renderCss)

layout :: String -> Html -> Html
layout baseUrl content = [shamlet|
  $doctype 5
  <html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <title>declared volatile
        <link rel="stylesheet" href="#{baseUrl </> "css/reset.css"}" type="text/css" media="screen">
        <link rel="stylesheet" href="#{baseUrl </> "css/basscss.css"}" type="text/css" media="screen">
        <link rel="stylesheet" href="#{baseUrl </> "css/pixyll.css"}" type="text/css" media="screen">
        <link rel="stylesheet" href="#{baseUrl </> "css/styles.css"}" type="text/css" media="screen">
        <style type="text/css">#{renderCss logoStyles}
        <link rel="stylesheet" href=#{baseUrl </> "css/pandoc-solarized.css"} type="text/css" media="screen">
        <meta name="description" content="Born human, declared volatile">
        <meta name="keywords" content="stefan kersten kaos korobase sex drugs rock'n roll">
        <meta name="robots" content="all">
        <link rel="index" title="declared volatile" href="/">
        <link href="#{baseUrl </> "atom.xml"}" type="application/atom+xml" rel="alternate" title="Declared Volatile ATOM Feed" />
  <body>
    <div .container>
      <div .row>
        <div .col .col-1>
          &nbsp;
        <div .col .col-6>
          <header>
            <div .mt2 .wrap>
              <div .measure>
                #{logo baseUrl}
                <div .clearfix>
      <div .row>
        <div .col-center .col-8>
          #{content}
          <!-- <footer> -->
            <div .p1 .wrap>
              <div .measure .center>
                <small>
                  <hr>
                  Powered by <a href="https://github.com/johnotander/pixyll.git">Pixyll</a>
|]

index :: String -> [BlogPost] -> Html
index baseUrl posts = layout baseUrl $ [shamlet|
  <div .home>
    <div .posts>
      $forall post <- posts
        <div .post>
          <p .post-meta>#{formatPostDate post}
          <a href="#{baseUrl </> postUrl post}" .post-link>
            <h3 .h2 .post-title>#{postTitle post}
          <!-- <p .post-summary>post.summary -->
|]

post :: String -> BlogPost -> Html
post baseUrl post = layout baseUrl $ [shamlet|
  <div .post .px2 role="main">
    <div .post-header .mb2>
      <h1>#{postTitle post}
      <span .post-meta>#{formatPostDate post}
    <article .post-content>
      #{postHtml post}
      <p>Comments welcome! <a href="https://twitter.com/kaoskorobase">@kaoskorobase
|]

logo :: FilePath -> Html
logo baseUrl = [shamlet|
  <div #logo>
    <div #logo-header>
      <a href="#{baseUrl}">
        <div #logo-header-1>declared
        <div #logo-header-2>volatile
    <div #logo-drawing>#{AsciiArt.logo}
    <div #logo-menu>
      <!-- <a href="http://kaoskorobase.roughdraft.io/">blog</a> -->
      #{encodeLink "email" "mailto:kaoskorobase@gmail.com"}
      <a href="#{baseUrl </> "files/sk.pub.asc"}">pubkey
      <a href="http://twitter.com/kaoskorobase">twitter</a>
      <a href="https://github.com/kaoskorobase">github
      <a href="http://www.linkedin.com/pub/stefan-kersten/19/91b/194">linked-in
|]

-- backgroundImage :: String
-- backgroundImage = "free-paper-texture-47.jpg"
-- backgroundImage = "free-paper-texture-38.jpg"

  -- body {
  --   background-color: #4f9bff;
  --   /*background-image:url(#{backgroundImage});*/
  --   /*background-repeat: repeat-y;*/
  --   /*background-position: center;*/
  --   /*background-attachment: fixed;*/
  --   /*background-size: 45em;*/
  --   color: #5a5a5a;
  --   font-family: Tahoma, Verdana, Arial, Helvetica, sans-serif;
  --   margin: 0;
  --   padding: 0; }

logoStyles :: Css
logoStyles = [lucius|

  @media screen and (max-width: 43em) {
    #logo {
      font-size: 0.5em;
    }
  }

  @media screen and (min-width: 43em) {
    #logo {
      font-size: 0.75em;
    }
  }

  #logo {
    position: relative;
    /*top: 5em;*/
    width: 43em;
    /*margin: 0 auto;*/
    /*margin-left: -6em;*/
    min-height: 21.5em;
    color: #5a5a5a;
    font-family: Tahoma, Verdana, Arial, Helvetica, sans-serif;
    line-height: 1;
    /*font-size: 0.8rem;*/
    a {
      color: #5a5a5a;
      text-decoration: none;
      img {
        border: none;
        padding: 0;
        margin: 0;
      }
    }
    a:hover {
      /*font-weight: bold;*/
      color: #e87830;
    }
  }

  #logo-header {
    z-index: 0;
    #logo-header-1 {
      position: absolute;
      left: 0.7em;
      font-size: 4em;
      font-weight: bold;
      letter-spacing: -0.05em;
      color: #e87830;
    }
    #logo-header-2 {
      position:absolute;
      font-size: 3em;
      font-weight: bold;
      left: 2.3em;
      top: 0.85em;
    }
  }

  #logo-drawing {
    position: absolute;
    font-family: monospace,fixed;
    /*font-size: 1.1em;*/
    font-size: 2em;
    z-index: -1;
    white-space: pre;
    span.axis {
      color: #4f90f1;
    }
    span.curve {
      color: #CC0000;
      font-weight: bold;
    }
    span.digit {
      color: #e87830;
    }
  }

  #logo-menu {
    position: absolute;
    top: 7em;
    left: 13.5em;
    ul {
      list-style: none;
    }
    font-size: 2em;
    line-height: 1.25em;
  }

  .logo-menu-entry {
  }
|] undefined
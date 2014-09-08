{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text.Lazy.IO as TL
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet (shamlet)
import           Text.Lucius (Css, lucius, renderCss)

import AsciiArt

index :: Html
index = [shamlet|
  $doctype 5
  <html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
        <title>declared volatile
        <link rel="stylesheet" href="reset.css" type="text/css" media="screen">
        <style type="text/css">#{renderCss styles}
        <meta name="description" content="Born human, declared volatile">
        <meta name="keywords" content="stefan kersten kaos korobase sex drugs rock'n roll">
        <meta name="robots" content="all">
        <link rel="index" title="declared volatile" href="/">
  <body>
      <div #wrapper>
        <div #header>
            <div #logo>
                <h1>declared
                <h2>volatile
            <div #ascii-logo>#{logo}
        <div #sidebar>
          <ul>
            <li>
              <script>document.write("<n uers=\"znvygb:fx@x-ubeam.qr\">rznvy</n>".replace(/[a-zA-Z]/g,function(c){return String.fromCharCode((c<="Z"?90:122)>=(c=c.charCodeAt(0)+13)?c:c-26);}));
            <li><a href="files/sk.pub.asc">pubkey
            <li><a href="http://twitter.com/kaoskorobase">twitter
            <li><a href="http://www.linkedin.com/pub/stefan-kersten/19/91b/194">linked-in
            <li><a href="https://plus.google.com/116882574218560793137">google+
            <li><a href="https://github.com/kaoskorobase">github
|]

styles :: Css
styles = [lucius|
  body {
    background: #4f9bff;
    color: #5a5a5a;
    font-family: Tahoma, Verdana, Arial, Helvetica, sans-serif;
    margin: 0;
    padding: 0; }

  a {
    color: #5a5a5a;
    text-decoration: none;
    img {
      border: none;
      padding: 0;
      margin: 0; } }
  a:hover {
    font-weight: bold; }

  #wrapper {
    width: 40em;
    margin-left: auto;
    margin-right: auto;
    margin-top: 5em; }

  #logo {
    z-index: 0;
    h1 {
      font-size: 4em;
      font-weight: bold;
      letter-spacing: -0.1em;
      color: #e87830; }
    h2 {
      font-size: 3em;
      font-weight: bold;
      margin-left: 2.9em;
      margin-top: -0.5em; } }

  #ascii-logo {
    margin-top: -5em;
    margin-left: -0.65em;
    pre {
      font-family: monospace,fixed;
      /*font-size: 1.1em;*/
      font-size: 2em;
      z-index: -1;
      position: relative; }
    span.axis {
      color: #4f90f1; }
    span.curve {
      color: #CC0000;
      font-weight: bold; }
    span.digit {
      color: #e87830; } }

  #sidebar {
    position: relative;
    top: -5em;
    margin-left: 4.5em;
    float: left;
    font-size: 1.3em;
    ul {
      list-style: none; }
      ul a:hover {
        letter-spacing: 2.5pt; } }

  #content {
    width: 620px;
    float: right;
    padding: 0 0 30px 0;
    min-height: 420px; }
|] "/"

main :: IO ()
main = TL.putStrLn $ renderHtml index

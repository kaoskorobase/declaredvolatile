{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DeclaredVolatile.Pixyll (
    Site(..)
  , Page(..)
  , post
  , page
  , index
) where

import Data.Time (UTCTime)
import DeclaredVolatile.Date (date)
import DeclaredVolatile.Logo.AsciiArt
import Text.Hamlet (shamlet)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H

data Site = Site {
    siteTitle :: String
  , siteTagline :: String
  , siteDescription :: String
  , siteUrl :: String
  , siteBaseUrl :: String
  , siteAuthor :: String
  , siteMenu :: [Html]
  }

data Page = Page {
    pageTitle :: String
  , pageDate :: UTCTime
  , pageSummary :: String
  , pageUrl :: String
  } deriving (Show)

_head :: Site -> Maybe Page -> Html
_head site maybePage = [shamlet|
<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">

  <title>
    $maybe page <- maybePage
      #{ pageTitle page } &#8211;
    $nothing
    #{ siteTitle site }
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <!-- <meta name="description" content="{% if page.summary %}{{ page.summary }}{% else %}{{ site.description }}{% endif %}"> -->
  <meta name="author" content="#{ siteAuthor site }">
  <!-- {% if page.categories %}<meta name="keywords" content="{{ page.categories | join: ', ' }}">{% endif %} -->
  <!-- <link rel="canonical" href="{{ page.url | replace:'index.html','' | prepend: site.baseurl | prepend: site.url }}"> -->

  <!-- Custom CSS -->
  <link rel="stylesheet" href="#{ siteBaseUrl site }/css/basscss.css" type="text/css">
  <link rel="stylesheet" href="#{ siteBaseUrl site }/css/pixyll.css" type="text/css">
  <link rel="stylesheet" href="#{ siteBaseUrl site }/css/pandoc-solarized.css" type="text/css">
  <link rel="stylesheet" href="#{ siteBaseUrl site }/css/declaredvolatile.css" type="text/css">

  <!-- Fonts -->
  <link href="http://fonts.googleapis.com/css?family=Merriweather:900,900italic,300,300italic" rel="stylesheet" type="text/css">
  <link href="http://fonts.googleapis.com/css?family=Lato:900,300" rel="stylesheet" type="text/css">
|]

title = fmap H.toHtml $ image [
    "                                    "   
  , "                                    "
  , "declared volatile                   "
  , "                                    "
  , "                                    " ]

headerLogo = crop (sine `below` bits) `below` title

_header :: Site -> Html
_header site = [shamlet|
<header class="site-header px2 px-responsive">
  <div class="mt2 wrap">
    <div class="measure">
      <!-- <a href="#{ siteUrl site }" class="left">#{ siteTitle site }</a> -->
      <a href="#{ siteUrl site }" class="left">
        <div .left .logo>
          #{headerLogo}
      <nav class="site-nav right">
        $forall item <- siteMenu site
          #{item}
      <div class="clearfix">
|]

_footer :: Html
_footer = [shamlet|
<footer class="footer">
  <div class="p2 wrap">
    <div class="measure mt1 center">
      <small>
        <a href="https://github.com/johnotander/pixyll.git">Pixyll Theme</a>
|]

-- _pagination = [shamlet|
-- <div class="pagination clearfix mb1 mt4">
--   <div class="left">
--     {% if paginator.previous_page %}
--       {% if paginator.page == 2 %}
--         <a class="pagination-item" href="{{ site.baseurl }}/">Newer</a>
--       {% else %}
--         <a class="pagination-item" href="{{ site.baseurl }}/page{{paginator.previous_page}}/">Newer</a>
--       {% endif %}
--     {% else %}
--       <span class="pagination-item disabled">Newer</span>
--     {% endif %}
--   </div>
--   <div class="right">
--     {% if paginator.next_page %}
--       <a class="pagination-item" href="{{ site.baseurl }}/page{{paginator.next_page}}/">Older</a>
--     {% else %}
--       <span class="pagination-item disabled">Older</span>
--     {% endif %}
--   </div>
-- </div>
-- |]

defaultLayout :: Site -> Maybe Page -> Html -> Html
defaultLayout site page content = [shamlet|
<!DOCTYPE html>
<html>
  #{_head site page}
  <body>
    <div class="site-wrap">
      #{ _header site }

      <div class="post p2 p-responsive wrap" role="main">
        <div class="measure">
          #{ content }

    #{ _footer }
|]

formatPageDate = date "%b %-d, %Y" . pageDate

post :: Site -> Page -> Html -> Html
post site page content = defaultLayout site (Just page) [shamlet|
<div class="post-header mb2">
  <h1 class="py2">#{ pageTitle page }
  <span class="post-meta">#{ formatPageDate page  }
<article class="post-content">
  #{ content }
|]

page :: Site -> Page -> Html -> Html
page site page content = defaultLayout site (Just page) [shamlet|
<div class="post">
  <header class="post-header">
    <h1 class="h2">{{ page.title }}
  <article class="post-content">
    #{ content }
|]

index :: Site -> [Page] -> Html
index site posts = defaultLayout site Nothing $ [shamlet|
<div class="home">
  <div class="posts">
    $forall post <- posts
      <div class="post">
        <a href="#{ siteBaseUrl site }/#{ pageUrl post }" class="post-link">
          <p class="post-meta">#{ formatPageDate post }
          <h3 class="h2 post-title">#{ pageTitle post }
          <p class="post-summary">#{ pageSummary post }

  <!-- {% include pagination.html %} -->

  <!-- <p class="rss-subscribe">subscribe <a href="/feed.xml">via RSS -->
|]
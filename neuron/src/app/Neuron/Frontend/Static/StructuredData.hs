{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Static.StructuredData
  ( renderStructuredData,
  )
where

import Control.Monad.Except (liftEither, runExcept)
import Data.Some (Some (Some))
import Data.Structured.Breadcrumb (Breadcrumb)
import qualified Data.Structured.Breadcrumb as Breadcrumb
import Data.Structured.OpenGraph
  ( Article (Article),
    OGType (..),
    OpenGraph (..),
  )
import Data.Structured.OpenGraph.Render (renderOpenGraph)
import Data.TagTree (Tag (unTag))
import qualified Data.Text as T
import qualified Network.URI.Encode as E
import Neuron.Config.Type (Config (..), getSiteBaseUrl)
import Neuron.Frontend.Route (Route (..), routeHtmlPath, routeTitle')
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Query.Parser (parseQueryLink)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelQuery (..),
    ZettelT (..),
    sansContent,
  )
import Reflex.Dom.Core (DomBuilder)
import Relude
import Text.Pandoc.Definition (Inline (Image, Link, Str), Pandoc (..))
import Text.Pandoc.Util (getFirstParagraphText, plainify)
import qualified Text.Pandoc.Walk as W
import Text.URI (URI, mkURI)
import qualified Text.URI as URI

renderStructuredData :: DomBuilder t m => Config -> Route a -> (ZettelGraph, a) -> m ()
renderStructuredData config route val = do
  renderOpenGraph $ uncurry (routeOpenGraph config) val route
  Breadcrumb.renderBreadcrumbs $ routeStructuredData config val route

routeStructuredData :: Config -> (ZettelGraph, a) -> Route a -> [Breadcrumb]
routeStructuredData cfg (graph, v) = \case
  Route_Zettel _ ->
    case either fail id $ getSiteBaseUrl cfg of
      Nothing -> []
      Just baseUrl ->
        let mkCrumb :: Zettel -> Breadcrumb.Item
            mkCrumb Zettel {..} =
              Breadcrumb.Item zettelTitle (Just $ routeUri baseUrl $ Route_Zettel zettelSlug)
         in Breadcrumb.fromForest $ fmap mkCrumb <$> G.backlinkForest Folgezettel (sansContent v) graph
  _ ->
    []

routeOpenGraph :: Config -> ZettelGraph -> a -> Route a -> OpenGraph
routeOpenGraph cfg@Config {siteTitle, author} g v r =
  OpenGraph
    { _openGraph_title = routeTitle' v r,
      _openGraph_siteName = siteTitle,
      _openGraph_description = case r of
        (Route_Impulse _mtag) -> Just "Impulse"
        Route_ImpulseStatic -> Just "Impulse (static)"
        Route_Zettel _ -> do
          doc <- getPandocDoc v
          para <- getFirstParagraphText doc
          let paraText = renderPandocAsText g para
          pure $ T.take 300 paraText,
      _openGraph_author = author,
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel _ -> do
          doc <- getPandocDoc v
          image <- URI.mkURI =<< getFirstImg doc
          baseUrl <- either fail id $ getSiteBaseUrl cfg
          URI.relativeTo image baseUrl
        _ -> Nothing,
      _openGraph_url = do
        baseUrl <- either fail id $ getSiteBaseUrl cfg
        pure $ routeUri baseUrl r
    }
  where
    getPandocDoc = either (const Nothing) (Just . zettelContent)
    getFirstImg ::
      Pandoc ->
      -- | Relative URL path to the image
      Maybe Text
    getFirstImg (Pandoc _ bs) = listToMaybe $
      flip W.query bs $ \case
        Image _ _ (url, _) -> [toText url]
        _ -> []

renderPandocAsText :: ZettelGraph -> [Inline] -> Text
renderPandocAsText g =
  plainify
    . W.walk plainifyZQueries
  where
    plainifyZQueries :: Inline -> Inline
    plainifyZQueries = \case
      x@(Link attr inlines (url, title)) ->
        fromMaybe x $ do
          -- REFACTOR: This code should would fit in Query.View (rendering text,
          -- rather than html, variation)
          someQ <- parseQueryLink =<< URI.mkURI url
          readableInlines <- case someQ of
            Some (ZettelQuery_ZettelByID zid _conn) -> do
              if inlines == [Str url]
                then do
                  Zettel {zettelTitle} <- G.getZettel zid g
                  pure [Str zettelTitle]
                else pure inlines
            Some (ZettelQuery_TagZettel (unTag -> tag)) -> do
              pure [Str $ "#" <> tag]
            _ ->
              -- Ideally we should replace these with `[[..]]`
              Nothing
          pure $ Link attr readableInlines (url, title)
      x -> x

data BaseUrlError
  = BaseUrlNotAbsolute
  deriving (Eq, Show)

instance Exception BaseUrlError

-- | Make an absolute URI for a route, given a base URL.
routeUri :: HasCallStack => URI -> Route a -> URI
routeUri baseUrl r = either (error . toText . displayException) id $
  runExcept $ do
    let -- Use `E.encode` to deal with unicode code points, as mkURI will fail on them.
        -- This is necessary to support non-ascii characters in filenames
        relUrl = toText . E.encode . toString $ routeHtmlPath r
    uri <- liftEither $ mkURI relUrl
    case URI.relativeTo uri baseUrl of
      Nothing -> liftEither $ Left $ toException BaseUrlNotAbsolute
      Just x -> pure x

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Frontend.View where

import Clay (Css, em, gray, important, pct, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import Data.Tagged (untag)
import Neuron.Cache.Type (NeuronCache (..))
import qualified Neuron.Cache.Type as NeuronCache
import Neuron.Config.Type (Config (..))
import qualified Neuron.Config.Type as Config
import Neuron.Frontend.Common (neuronCommonStyle, neuronFonts)
import qualified Neuron.Frontend.Impulse as Impulse
import qualified Neuron.Frontend.Query.View as QueryView
import Neuron.Frontend.Route
  ( Impulse,
    NeuronVersion,
    NeuronWebT,
    Route (..),
    routeTitle',
  )
import Neuron.Frontend.Theme (Theme)
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (LoadableData, elLinkGoogleFonts)
import qualified Neuron.Frontend.Widget as W
import qualified Neuron.Frontend.Zettel.CSS as ZettelCSS
import qualified Neuron.Frontend.Zettel.View as ZettelView
import Neuron.Zettelkasten.Zettel (Zettel, ZettelC)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude

headTemplate ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  Route a ->
  Dynamic t (LoadableData (Config, a)) ->
  m ()
headTemplate r vLDyn = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  W.loadingWidget' vLDyn (el "title" $ text "Loading...") (const $ el "title" $ text "Error") $ \vDyn ->
    dyn_ $
      ffor vDyn $ \(cfg, v) ->
        el "title" $ text $ routeTitle r v cfg
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
  elAttr "style" ("type" =: "text/css") $ text $ toText $ C.renderWith C.compact [] style
  elLinkGoogleFonts neuronFonts

-- The `a` is used to find zettel title, and `Config` for site title.
routeTitle :: Route a -> a -> Config -> Text
routeTitle r v Config {..} =
  withSuffix siteTitle . routeTitle' v $ r
  where
    withSuffix suffix x =
      if x == suffix
        then x
        else x <> " - " <> suffix

bodyTemplate ::
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t (Maybe NeuronVersion) ->
  Dynamic t (Maybe Theme) ->
  m () ->
  m ()
bodyTemplate neuronVersionM neuronThemeM w = do
  let attrs = ffor neuronThemeM $ \(fmap (toText . Theme.themeIdentifier) -> mId) ->
        "class" =: "ui fluid container universe"
          <> maybe mempty ("id" =:) mId
  elDynAttr "div" attrs $ do
    w
    renderBrandFooter neuronVersionM

renderRouteImpulse ::
  forall t m js.
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) =>
  Dynamic t (LoadableData (((Theme, NeuronVersion), Maybe Zettel), Impulse)) ->
  NeuronWebT t m ()
renderRouteImpulse dataLDyn = do
  let neuronTheme =
        fmap (fst . fst . fst) . W.getData <$> dataLDyn
      neuronVer =
        fmap (snd . fst . fst) . W.getData <$> dataLDyn
      indexZettel =
        join . fmap (snd . fst) . W.getData <$> dataLDyn
  -- HTML for this route is all handled in JavaScript (compiled from
  -- impulse's sources).
  bodyTemplate neuronVer neuronTheme $ do
    elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
      --  divClass "ui text container" $ do
      let impulseL = fmap snd <$> dataLDyn
      Impulse.renderImpulse neuronTheme indexZettel impulseL

renderRouteZettel ::
  forall t m js.
  (PandocBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) =>
  Dynamic t (LoadableData (NeuronCache, ZettelC)) ->
  NeuronWebT t m ()
renderRouteZettel cacheLDyn = do
  let neuronThemeM =
        fmap (Theme.mkTheme . Config.theme . NeuronCache._neuronCache_config . fst) . W.getData <$> cacheLDyn
      neuronVerM =
        fmap (NeuronCache._neuronCache_neuronVersion . fst) . W.getData <$> cacheLDyn
  bodyTemplate neuronVerM neuronThemeM $ do
    W.loadingWidget cacheLDyn $ \cacheDyn -> do
      dyn_ $
        ffor cacheDyn $ \(NeuronCache {..}, val) -> do
          let theme = Theme.mkTheme $ Config.theme _neuronCache_config
              eu = Config.editUrl _neuronCache_config
          ZettelView.renderZettel theme (_neuronCache_graph, val) eu

renderBrandFooter :: (DomBuilder t m, PostBuild t m) => Dynamic t (Maybe NeuronVersion) -> m ()
renderBrandFooter ver =
  divClass "ui center aligned container footer-version" $ do
    divClass "ui tiny image" $ do
      elAttr "a" ("href" =: "https://neuron.zettel.page") $ do
        let attr = ffor ver $ \mver ->
              "title" =: ("Generated by Neuron " <> maybe "" untag mver)
                <> "src" =: "https://raw.githubusercontent.com/srid/neuron/master/assets/neuron.svg"
                <> "alt" =: "logo"
        elDynAttr "img" attr blank

style :: Css
style = do
  "body" ? do
    neuronCommonStyle
    Impulse.style
    ZettelCSS.zettelCss
    QueryView.style
    footerStyle
  where
    footerStyle = do
      ".footer-version img" ? do
        C.filter $ C.grayscale $ pct 100
      ".footer-version img:hover" ? do
        C.filter $ C.grayscale $ pct 0
      ".footer-version, .footer-version a, .footer-version a:visited" ? do
        C.color gray
      ".footer-version a" ? do
        C.fontWeight C.bold
      ".footer-version" ? do
        important $ C.marginTop $ em 1
        C.fontSize $ em 0.7

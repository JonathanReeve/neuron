{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Type where

import Data.Default
import Data.Tagged
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel (ZettelC)
import Reflex.Dom.Core (DomBuilder, PostBuild)
import Reflex.Dom.Widget (blank)
import Relude
import qualified System.Directory.Contents.Types as DC

type PluginName = Tagged "PluginName" Text

-- | `pluginData` must be unified with `Neuron.Plugin.PluginData.PluginData`
data Plugin pluginData = Plugin
  { _plugin_filterSources :: DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath)),
    -- | Called after zettel files read in memory
    _plugin_afterZettelRead :: forall m. MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m (),
    -- | Called after zettel files are parsed
    _plugin_afterZettelParse :: ZettelC -> ZettelC,
    -- | Render this plugin's content below zettel html
    _plugin_renderPanel :: forall t m. (DomBuilder t m, PostBuild t m) => ZettelGraph -> pluginData -> NeuronWebT t m ()
  }

instance Default (Plugin pluginData) where
  def =
    Plugin
      { _plugin_filterSources = pure . Just,
        _plugin_afterZettelRead = void . pure,
        _plugin_afterZettelParse = id,
        _plugin_renderPanel = const $ const blank
      }
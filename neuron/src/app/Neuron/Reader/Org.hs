{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.Org
  ( parseOrg,
  )
where

import qualified Data.Map as Map
import Data.TagTree (Tag (Tag))
import Data.Tagged
import Data.Text (toLower, isPrefixOf, splitOn)
import qualified Data.Text.IO as TIO
import Data.Time.DateMayTime (DateMayTime, parseDateMayTime)
import Neuron.Reader.Type (ZettelParseError, ZettelReader)
import Neuron.Zettelkasten.Zettel.Meta (Meta (..))
import Relude
import Relude.Extra.Map (lookup)
import Text.Pandoc (def, runPure, Format)
import Text.Pandoc.Definition (Pandoc, Format, Block(RawBlock))
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util (getH1)

-- parseOrg :: ZettelReader
-- parseOrg filePath s = do
--   doc <- first show $ runPure $ readOrg def s
--   meta <- extractMetadata doc
--   pure (meta, doc)

-- -- Extract metadata from the properties that are attached to the first headline
-- extractMetadata :: Pandoc -> Either ZettelParseError (Maybe Meta)
-- extractMetadata doc
--   | Just ((_, _, Map.fromList -> properties), _) <- getH1 doc = do
--     date <- traverse parseDate $ lookup "date" properties
--     -- title is now deprecated
--     let title = Nothing
--         slug = Nothing
--         tags = fmap Tag . words <$> lookup "roam_tags" properties
--         unlisted = parseUnlisted <$> lookup "unlisted" properties
--     pure $ Just Meta {..}
--   | otherwise = pure Nothing
--   where
--     parseDate :: Text -> Either ZettelParseError DateMayTime
--     parseDate date = maybeToRight (Tagged $ "Invalid date format: " <> date) $ parseDateMayTime @Maybe date
--     parseUnlisted :: Text -> Bool
--     parseUnlisted a = toLower a == "true"

-- extractMetadata filePath doc = case doc of
--   Left -> Nothing
--   Right (Pandoc meta blocks)-> Meta {title = title,
--                                      slug = slug,
--                                      tags = tags,
--                                      date = date} where
--     title = lookupMeta "title" meta
--     slug = parseSlug filePath
--     date = parseDate filePath
--     tags = parseTags doc


-- parseDate :: FilePath -> Either ZettelParseError DateMayTime
-- parseDate filePath = ...


parseOrg :: ZettelReader
parseOrg filePath s = do
  doc <- first show $ runPure $ readOrg def s
  meta <- extractMetadata doc filePath
  pure (meta, doc)

extractMetadata :: Pandoc -> Either ZettelParseError (Maybe Meta)
extractMetadata doc filePath = case doc of
  Meta meta [Block block] -> do
    let title = parseTitle meta
        date = parseDate filePath
        slug = parseSlug filePath
        tags = parseTags blocks
        -- TODO?
        unlisted = False
    pure $ Just Meta {..}
  _ -> pure Nothing

-- Org-roam dates are stored in the filename.
-- They have the form: 20210102133406 which is 2021-01-02, 13:34:06
parseDate :: FilePath -> Maybe DateMayTime
parseDate fp = case splitOn "-" fp of
  date : slug | length date == 14 -> fp -- TODO
  _ -> Nothing

parseSlug fp :: FilePath -> Slug
parseSlug fp = fp -- TODO

-- | Tags in org-roam look like: @#+roam_tags: tag1 tag2 "tag three"@
-- I.e., they're space-delimited, and can contain spaces if quoted.
-- These aren't really parsed by Pandoc, but appear as Blocks:
-- > RawBlock (Format "org") "#+roam_tags: tag1 tag2 "tag three"
parseTags :: [Block] -> Maybe [Tag]
parseTags blocks =  viaNonEmpty head $ mapMaybe blockToTag blocks where
  blockToTag :: Block -> Maybe [Tag]
  blockToTag block = case block of
    RawBlock fmt blockText | fmt == "org"
      && Data.Text.isPrefixOf "#+roam_tags" (toLower blockText)
      && length (words blockText) < 2 -> Just $ map Tag (words blockText)
    _ -> Nothing

-- testReader :: IO ()
testReader = do
  let fp = "/home/jon/Code/neuron/guide-org/test.org" :: FilePath
  testFile <- TIO.readFile fp
  let result = parseOrg fp testFile
  return result

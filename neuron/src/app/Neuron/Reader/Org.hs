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

-- | Extract metadata from an org-roam file.
-- The vanilla Org reader reads headline properties, but the standard org-roam
-- way is to have all the metadata at the top of the file. For example:
--
-- > #+title: Niklas Luhmann
-- > #+roam_alias: Luhmann
-- > #+roam_tags: notes "zettelkasten stuff"
-- >
-- > Niklas Luhmann was a German sociologist who wrote over 70 books.
extractMetadata :: Pandoc -> Either ZettelParseError (Maybe Meta)
extractMetadata doc filePath = case doc of
  Meta meta [Block block] -> do
    let title = parseTitle meta
        (date, slug) = parseDate filePath
        tags = parseTags blocks
        -- TODO?
        unlisted = False
    pure $ Just Meta {..}
  _ -> pure Nothing

-- Pandoc understands @#+title@ blocks, so we need only get this from the Pandoc structure.
parseTitle :: Pandoc -> Title
parseTitle doc = -- TODO

-- An org-roam filename can be either: @20210102133406-some-slug.org@ or
-- @some-slug.org@. Let's try to extract the slug and the date from them.
parseFilePath :: FilePath -> (Maybe DateMayTime, Maybe Slug)
parseFilePath fp = case splitOn "-" fp of
  -- Org-roam dates are stored in the filename.
  -- They have the form: 20210102133406 which is 2021-01-02, 13:34:06
  date : slug | length date == 14 && all isDigit date -> do
                  let y = take 4 date
                      m = (take 2 . drop 4) date
                      d = (take 2 . drop 6) date
                      date = iso8601Format (y <> "-" <> m <> "-" <> d)
                  (Just date, Just slug)
  _ -> (Nothing, Just slug)


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

-- -- testReader :: IO ()
-- testReader = do
--   let fp = "/home/jon/Code/neuron/guide-org/test.org" :: FilePath
--   testFile <- TIO.readFile fp
--   let result = parseOrg fp testFile
--   return result

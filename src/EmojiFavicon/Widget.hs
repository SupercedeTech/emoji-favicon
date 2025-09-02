{-# LANGUAGE OverloadedStrings #-}

module EmojiFavicon.Widget
  ( -- * Core
    emojiFaviconDataURI
    -- * HTML
  , HtmlUrl
  , tagUrl
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.URI (urlEncode)
import Text.Blaze.Html qualified as Blaze
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- | Hamlet-compatible HTML URL renderer
type HtmlUrl url = (url -> [(Text, Text)] -> Text) -> Blaze.Html

-- | Produce a @data:@ URI for an SVG favicon containing the given emoji.
emojiFaviconDataURI :: Text -> Text
emojiFaviconDataURI emoji =
  let codePoints   = T.unpack emoji
      numericEnt c = T.pack $ "&#x" <> toHex (fromEnum c) <> ";"
      unicodeHex   = mconcat (map numericEnt codePoints)
      svgTemplate  =
           "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
        <> "<text y='.9em' font-size='90'>"
        <> unicodeHex
        <> "</text></svg>"
      encoded :: BS.ByteString
      encoded = urlEncode False (encodeUtf8 svgTemplate)
  in  "data:image/svg+xml," <> decodeUtf8 encoded

-- | <link rel="icon" ...> as an HtmlUrl value (usable with Hamlet ^{...}).
tagUrl :: Text -> HtmlUrl url
tagUrl e _render =
  H.link
    H.! A.rel  "icon"
    H.! A.href (H.toValue (emojiFaviconDataURI e))

toHex :: Int -> String
toHex n = let h = "0123456789ABCDEF"
              go 0 acc | null acc  = "0"
                       | otherwise = acc
              go x acc = let (q,r) = x `quotRem` 16
                         in go q (h !! r : acc)
          in go n ""

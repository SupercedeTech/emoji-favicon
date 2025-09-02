{-# LANGUAGE TemplateHaskell #-}

module EmojiFavicon where

import EmojiFavicon.TH (makeWidgetsFromEmoji)

-- Generates one HtmlUrl function per emoji from emoji-test.txt
$(makeWidgetsFromEmoji "data/emoji-test.txt")

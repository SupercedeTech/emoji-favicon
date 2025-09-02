{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module EmojiFavicon.TH
  ( makeWidgetsFromEmoji
  ) where

import Control.Monad (forM)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import EmojiFavicon.Widget (tagUrl)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Text.Blaze.Html qualified as Blaze
import Text.ParserCombinators.ReadP

makeWidgetsFromEmoji :: FilePath -> Q [Dec]
makeWidgetsFromEmoji fp = do
  addDependentFile fp
  raw <- runIO (readFile fp)
  let pairs = parseEmojiTest raw
      uniq  = uniquifyNames pairs
  fmap concat $ forM uniq $ \(hsName, glyph) -> do
    let nm = mkName hsName
    urlTyVar <- newName "url"
    ty <- forallT [PlainTV urlTyVar SpecifiedSpec] (pure []) [t|
            ( $(varT urlTyVar) -> [(T.Text, T.Text)] -> T.Text ) -> Blaze.Html
          |]
    sig <- sigD nm (pure ty)
    fun <- funD nm [ clause [] (normalB [| tagUrl (T.pack glyph) |]) [] ]
    pure [sig, fun]

parseEmojiTest :: String -> [(String, String)]
parseEmojiTest contents =
  [ (trim name, glyph)
  | ln <- lines contents
  , let s = dropWhile isSpace ln
  , not (null s)
  , not (isComment s)
  , Just (glyph, name) <- [runP pLine s]
  ]

isComment :: String -> Bool
isComment ('#':_) = True
isComment _       = False

pLine :: ReadP (String, String)
pLine = do
  _ <- manyTill get (char '#')
  skipSpaces
  glyph <- munch1 (not . isSpace)
  skipSpaces
  _ <- char 'E' >> munch1 (\c -> isDigit c || c == '.')
  skipSpaces
  name <- munch1 (const True)
  pure (glyph, name)

runP :: ReadP a -> String -> Maybe a
runP p s = case [x | (x,"") <- readP_to_S (p <* skipSpaces <* eof) s] of
  (x:_) -> Just x
  _     -> Nothing

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

uniquifyNames :: [(String, String)] -> [(String, String)]
uniquifyNames =
  go (M.empty :: M.Map String Int) . map (first sanitise)
  where
    go _ [] = []
    go seen ((nm, gl):xs) =
      let k     = M.findWithDefault 0 nm seen
          nm'   = if k == 0 then nm else nm ++ "_" ++ show k
          seen' = M.insert nm (k + 1) seen
      in (nm', gl) : go seen' xs

sanitise :: String -> String
sanitise = fixLeading . collapseUnderscores . map toSafe
  where
    toSafe c | isAlphaNum c = toLower c
             | otherwise    = '_'

    collapseUnderscores :: String -> String
    collapseUnderscores = foldr step []
      where
        step '_' ('_':xs) = '_' : xs
        step c xs         = c   : xs

    fixLeading []     = "emoji"
    fixLeading (c:cs) = (if isAlpha c || c == '_' then toLower c else '_') : cs

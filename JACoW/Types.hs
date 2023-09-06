{-# LANGUAGE OverloadedStrings #-}

module JACoW.Types where

import Data.Yaml
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Char

import Prelude hiding
       (
         
       )

class LaTeXable a where
    fromtitle :: a -> [Affiliation] -> [LaTeX]
    tolatex   :: a -> [LaTeX]

stringify :: LaTeX -> String

stringify (Raw s) = s
stringify (Sup s) = supTeX s
stringify (Maþ s) = mathTeX s
stringify (Mac c ls) = cs1 c $ foldl1 (<>) $ stringify <$> ls
stringify Spc = " "
stringify NewLine = "\n"
stringify TeXLine = "\\\\"
cs1 :: String -> String -> String
cs1 cs arg1 = "\\" <> cs <> wrap "{" "}" arg1
wrap :: String -> String -> String -> String
wrap l r s = l <> s <> r
supTeX    = cs1 "textsuperscript"
mathTeX   = wrap "$" "$"
-- sep :: String -> [LaTeX] -> String
-- sep s ltxs = foldl1 (<> s <>) (fmap toLaTeX ltxs)
simplify :: [LaTeX] -> [LaTeX]

simplify [] = []
simplify (Mac cs ls:xs) = (Mac cs (simplify ls)):simplify xs
simplify (x:[]) = [x]
simplify (Raw x1:Raw x2:xs) = simplify ((Raw (x1<>x2)):xs)
simplify (Sup x1:Sup x2:xs) = simplify ((Sup (x1<>x2)):xs)
simplify (Maþ x1:Maþ x2:xs) = simplify ((Maþ (x1<>x2)):xs)
simplify (x:xs) = x:simplify xs
authorTeX  = cs1 "author"
unTeXlines = intercalate ("\\\\" <> singleton '\n')
infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
maybePrimary :: Maybe String -> Bool
maybePrimary (Just str) = (== "primary") $ toLower <$> str
maybePrimary _ = False
addrIndex :: Affiliation -> [Affiliation] -> Maybe LaTeX
addrIndex aff affs = ((Sup).show.(+1)) <$> aff `elemIndex` affs

data LaTeX = Raw String
           | Sup String
           | Maþ String
           | Mac String [LaTeX]
           | Spc
           | NewLine
           | TeXLine
           deriving Show
data AuthorTitle = AuthorTitle
                   { title   :: Title
                   , authors :: [Author]
                   } deriving (Show)
emptyAuthorTitle = AuthorTitle (Title "" Nothing) [Author (Name "" "" Nothing) "" Nothing False]
instance FromJSON AuthorTitle where
  parseJSON (Object v) =
    AuthorTitle    <$>
    v .:   "title" <*>
    v .:   "authors"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable AuthorTitle where
  tolatex (AuthorTitle t as)  = intercalate [NewLine] [tolatex t,[Mac "author" (intercalate [TeXLine,NewLine] ([authorlist] <> affiliations ))]]
    where
      authorlist = intercalate [NewLine] authors
      authors = flip fromtitle affs <$> as
      affs = fromMaybe [] (nub <$> (foldl1 (<>) $ addresses <$> as))
      affiliations = toAff <$> (zip [1..] affs)
      toAff (n,aff) = [Sup (show n)] <> fromtitle aff affs
  fromtitle t _ = tolatex t
data Title = Title
             { caption :: String
             , funds   :: Maybe [Fund]
             } deriving (Show)
type Fund = String
instance FromJSON Title where
  parseJSON (Object v) =
    Title            <$>
    v .:   "caption" <*>
    v .:?  "funds"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Title where
  tolatex (Title cap fs)  = [Mac "title" $ [Raw cap] <> intersperse (Sup ",") (fromMaybe [] $ ((Mac "thanks").singleton.(Raw)) <<$>> fs)] --((ref:sup-comma-in-title))
  fromtitle t _ = tolatex t
data Author = Author
              { name       :: Name
              , email      :: Email
              , addresses  :: Maybe [Affiliation]
              , isPrimary  :: Bool
              } deriving (Show)
instance FromJSON Author where
  parseJSON (Object v) =
    Author             <$>
    v .:   "name"      <*>
    v .:   "email"     <*>
    v .:?  "addresses" <*>
    (maybePrimary <$> (v .:? "type" .!= Just "other" :: Parser (Maybe String)))
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Author where
  fromtitle (Author nm em addrs isP) affs = fromtitle nm affs <> intersperse (Sup ",") (thankslst <> catMaybes (fmap (flip addrIndex affs) (fromMaybe [] addrs))) --((ref:sup-comma-in-author))
    where thankslst =
            if isP
            then tolatex em
            else []
  tolatex as = fromtitle as []
type Email = String
instance LaTeXable Email where
  tolatex eml = [Mac "thanks" [Raw eml]]
  fromtitle e _ = tolatex e
data Name = Name
            { firstName :: String
            , lastName  :: String
            , initials  :: Maybe String
            } deriving (Show)
instance FromJSON Name where
  parseJSON (Object v) =
    Name               <$>
    v .:   "firstName" <*>
    v .:   "lastName"  <*>
    v .:?  "initials"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Name where
  tolatex (Name fnm lnm ini) = [Raw (unwords [fnm,lnm])] -- firstname lastname
  fromtitle n _ = tolatex n
data Affiliation = Affiliation
                   { affName  :: String
                   , adress   :: Maybe String
                   , zipcode  :: Maybe String
                   } deriving (Show,Eq)
instance FromJSON Affiliation where
  parseJSON (Object v) =
    Affiliation      <$>
    v .:   "name"    <*>
    v .:?  "address" <*>
    v .:?  "zip"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Affiliation where
  tolatex aff = [Raw (affName aff                                                    -- name
                      <> fromMaybe "" (((<>) ", ") <$> adress aff)                   -- , address
                      <> fromMaybe "" (((<>) ", ") <$> zipcode aff)                  -- , zip
                     )]
  fromtitle aff _ = tolatex aff

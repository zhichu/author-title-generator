{-# LANGUAGE OverloadedStrings #-}

module JACoW.Types where

import Data.Yaml
import Control.Applicative
import Data.Maybe

import Data.Text hiding
       (
         zip,foldl1,
       )

import Data.List hiding
       (
         unlines,
         intercalate,singleton,
         unwords,
       )

import Prelude hiding
       (
         unlines,
         unwords,
       )

class LaTeXable a where
    totitle :: a -> [Affiliation] -> Text

simpletitle :: (LaTeXable a) => a -> Text
simpletitle = flip totitle []
cs1 :: Text -> Text -> Text
cs1 cs arg1 = "\\" <> cs <> "{" <> arg1 <> "}"
authorTeX  = cs1 "author"
unTeXlines = intercalate ("\\\\" <> singleton '\n')
titleTeX  = cs1 "title"
thanksTeX = cs1 "thanks"
supTeX    = cs1 "textsuperscript"
supComma  = supTeX ","
sep       = intercalate supComma
maybePrimary :: Maybe Text -> Bool
maybePrimary (Just str) = (== "primary").toLower $ str
maybePrimary _ = False
addrIndex :: Affiliation -> [Affiliation] -> Maybe Text
addrIndex aff affs = (supTeX.pack.show.(+1)) <$> aff `elemIndex` affs

data AuthorTitle = AuthorTitle
                   { title   :: Title
                   , authors :: [Author]
                   } deriving (Show)
instance FromJSON AuthorTitle where
  parseJSON (Object v) =
    AuthorTitle    <$>
    v .:   "title" <*>
    v .:   "authors"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable AuthorTitle where
  totitle (AuthorTitle t as) _  = unlines [totitle t affs, authorTeX (unTeXlines ([unlines auths] <> fmap toAff (zip [1..] affs)))]
    where
      affs = fromMaybe [] (nub <$> (foldl1 (<>) $ fmap addresses as))
      auths = fmap (flip totitle affs) as
      toAff (n,aff) = supTeX ((pack.show) n) <> totitle aff affs
data Title = Title
             { caption :: Text
             , funds   :: Maybe [Fund]
             } deriving (Show)
type Fund = Text
instance FromJSON Title where
  parseJSON (Object v) =
    Title            <$>
    v .:   "caption" <*>
    v .:?  "funds"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Title where
  totitle (Title cap fs) _  = titleTeX $ cap <> sep (fromMaybe [] $ fmap (fmap thanksTeX) fs)
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
    (maybePrimary <$> (v .:? "type" .!= Just "other" :: Parser (Maybe Text)))
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Author where
  totitle (Author nm em addrs isP) affs = totitle nm affs <> sep (thankslst <> catMaybes (fmap (flip addrIndex affs) (fromMaybe [] addrs)))
    where thankslst =
            if isP
            then [totitle em affs]
            else []
type Email = Text
instance LaTeXable Email where
  totitle eml _ = thanksTeX eml
data Name = Name
            { firstName :: Text
            , lastName  :: Text
            , initials  :: Maybe Text
            } deriving (Show)
instance FromJSON Name where
  parseJSON (Object v) =
    Name               <$>
    v .:   "firstName" <*>
    v .:   "lastName"  <*>
    v .:?  "initials"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Name where
    totitle (Name fnm lnm ini) _ = unwords [fnm,lnm] -- firstname lastname
data Affiliation = Affiliation
                   { affName  :: Text
                   , adress   :: Maybe Text
                   , zipcode  :: Maybe Text
                   } deriving (Show,Eq)
instance FromJSON Affiliation where
  parseJSON (Object v) =
    Affiliation      <$>
    v .:   "name"    <*>
    v .:?  "address" <*>
    v .:?  "zip"
  parseJSON _ = fail "Expected Object for Config value"
instance LaTeXable Affiliation where
  totitle aff _ = affName aff                                                    -- name
                  <> fromMaybe "" (((<>) ", ") <$> adress aff)                   -- , address
                  <> fromMaybe "" (((<>) ", ") <$> zipcode aff)                  -- , zip

{-# LANGUAGE DeriveGeneric #-}

module HistoryItem (
  HistoryItem(..)
) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow

import Database.PostgreSQL.Simple.ToField

import GHC.Generics

import Data.Time.LocalTime

data HistoryItem = HistoryItem {
    datetime :: LocalTime,
    maxValue :: Int,
    minValue :: Int,
    avgValue :: Int
  } deriving (Show, Generic)

instance FromRow HistoryItem where
  fromRow = HistoryItem <$> field <*> field <*> field <*> field
instance ToRow HistoryItem where
  toRow i = [toField $ datetime i, toField $ maxValue i, toField $ avgValue i, toField $ minValue i]
instance ToJSON HistoryItem
instance FromJSON HistoryItem
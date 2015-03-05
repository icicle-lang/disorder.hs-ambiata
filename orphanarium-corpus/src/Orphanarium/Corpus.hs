{-# LANGUAGE OverloadedStrings #-}
module Orphanarium.Corpus (
    cooking
  , muppets
  , southpark
  , simpsons
  , viruses
  , colours
  , weather
  ) where

import           Data.Text

cooking :: [Text]
cooking = [
    "salted"
  , "stewed"
  , "diced"
  , "filleted"
  , "sauteed"
  ]

muppets :: [Text]
muppets = [
    "kermit"
  , "gonzo"
  , "fozzy"
  , "chef"
  , "statler"
  , "waldorf"
  , "beaker"
  , "animal"
  ]

southpark :: [Text]
southpark = [
    "kyle"
  , "stan"
  , "cartman"
  , "timmy"
  , "token"
  , "chef"
  , "garrison"
  ]


simpsons :: [Text]
simpsons = [
    "homer"
  , "marge"
  , "maggie"
  , "lisa"
  , "bart"
  , "flanders"
  , "moe"
  , "barney"
  ]

viruses :: [Text]
viruses = [
    "rotavirus"
  , "smallpox"
  , "norovirus"
  , "chickenpox"
  , "camelpox"
  , "dengue"
  , "echovirus"
  , "equine morbillivirus"
  , "gou virus"
  , "measles"
  , "monkeypox"
  ]

colours :: [Text]
colours = [
    "red"
  , "green"
  , "blue"
  , "yellow"
  , "black"
  , "grey"
  , "purple"
  , "orange"
  , "pink"
  ]

weather :: [Text]
weather = [
    "dry"
  , "raining"
  , "hot"
  , "humid"
  , "snowing"
  , "fresh"
  , "windy"
  , "freezing"
  ]

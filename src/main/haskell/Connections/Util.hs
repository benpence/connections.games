module Connections.Util where

import qualified Data.Text                       as Text
import qualified System.Random                   as Random

import Data.Text (Text)

showT :: (Show a) => a -> Text
showT = Text.pack . show

-- TODO: Rewrite this. Currently O(n^2) and not tail recursive
chooseN :: (Random.RandomGen g) => g -> Int -> [a] -> ([a], g)
chooseN randomGen 0 _     = ([], randomGen)
chooseN randomGen _ []    = ([], randomGen)
chooseN randomGen n items =
  let
    (chosenIndex, newGen) = Random.randomR (0, ((length items) - 1)) randomGen

    newItems              = (take chosenIndex items) ++ (drop (chosenIndex + 1) items)
    (chosenList, lastGen) = chooseN newGen (n - 1) newItems

    chosenItem            = items !! chosenIndex
  in
    (chosenItem : chosenList, lastGen)

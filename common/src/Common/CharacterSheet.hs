{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Common.CharacterSheet where

import           Control.Lens          (from, to, (^.))
import           Control.Lens.TH       (makeLenses)
import           Data.Dependent.Map    (DMap)
import qualified Data.Dependent.Map    as DMap
import           Data.Dependent.Sum    ((==>))
import           Data.Functor.Identity (Identity)

data DeftnessAptitudes a where
  Shootin :: DeftnessAptitudes ()

deriveGEq      ''DeftnessAptitudes
deriveGCompare ''DeftnessAptitudes

data TraitTreeKey a where
  Deftness :: TraitTreeKey (DMap DeftnessAptitudes Identity)

deriveGEq      ''TraitTreeKey
deriveGCompare ''TraitTreeKey

data CharacterSheet = CharacterSheet
  { _chSheetAptitudeTree :: DMap TraitTreeKey Identity
  }

gabriella :: CharacterSheet
gabriella = CharacterSheet
  { _chSheetAptitudeTree = DMap.fromList
    [ Deftness ==> DMap.fromList
      [ Shootin ==> ()
      ]]
  }

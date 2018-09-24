{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

module Common.CharacterSheet where

import           Control.Lens          (from, to, (^.))
import           Control.Lens.TH       (makeLenses)
import           Data.Dependent.Map    (DMap)
import qualified Data.Dependent.Map    as DMap
import           Data.Dependent.Sum    ((==>))
import           Data.Functor.Identity (Identity)
import           Data.GADT.Compare.TH  (deriveGCompare, deriveGEq, runGComparing)

import qualified Data.GADT.Compare
import qualified Data.Type.Equality

type ConcentrationMap k f = DMap k f

data DeftnessAptitudes a where
  Shootin :: DeftnessAptitudes ()

deriveGEq      ''DeftnessAptitudes
deriveGCompare ''DeftnessAptitudes

data TraitTreeKey f a where
  Deftness :: TraitTreeKey f (DMap DeftnessAptitudes f)

-- deriveGEq      ''(TraitTreeKey f)
instance Data.GADT.Compare.GEq (TraitTreeKey f) where
  geq Deftness Deftness
    = do { return Data.Type.Equality.Refl }

-- deriveGCompare ''TraitTreeKey
instance DMap.GCompare (TraitTreeKey f) where
  gcompare Deftness Deftness = (Data.GADT.Compare.TH.runGComparing $ (do { return DMap.GEQ }))
  gcompare (Deftness {}) _ = DMap.GLT
  gcompare _ (Deftness {}) = DMap.GGT

data CharacterSheet = CharacterSheet
  { _chSheetAptitudeTree :: DMap (TraitTreeKey Identity) Identity
  }

gabriella :: CharacterSheet
gabriella = CharacterSheet
  { _chSheetAptitudeTree = DMap.fromList
    [ Deftness ==> DMap.fromList
      [ Shootin ==> ()
      ]]
  }

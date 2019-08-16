{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Plutus.Contract.Rows.Instances() where

import           Data.Aeson                       (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Types                 as Aeson
import           Data.Row
import qualified Data.Row.Records                 as Records
import qualified Data.Row.Variants                as Variants
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

instance Forall ρ ToJSON => ToJSON (Rec ρ) where
  toJSON = Aeson.object . Records.eraseWithLabels @ToJSON @ρ @Text @Aeson.Value Aeson.toJSON

instance (AllUniqueLabels ρ, Forall ρ FromJSON) => FromJSON (Rec ρ) where
  parseJSON vl = Records.fromLabelsA @FromJSON @Aeson.Parser @ρ  (\lbl -> Aeson.withObject "Rec" (\obj -> obj .: (Text.pack $ show lbl) >>= Aeson.parseJSON) vl)

instance (AllUniqueLabels ρ, Forall ρ FromJSON) => FromJSON (Var ρ) where
  parseJSON vl = Variants.fromLabels @FromJSON @ρ @Aeson.Parser (\lbl -> Aeson.withObject "Var" (\obj -> do { tg <- obj .: "tag"; if tg == show lbl then (obj .: "value") >>= Aeson.parseJSON else fail "Wrong label" }) vl)

instance Forall ρ ToJSON => ToJSON (Var ρ) where
  toJSON v = Aeson.object [Variants.eraseWithLabels @ToJSON @ρ @Text @Aeson.Value Aeson.toJSON v]
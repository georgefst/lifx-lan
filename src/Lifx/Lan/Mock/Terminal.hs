-- TODO remove when `OverloadedRecordUpdate` is fully implemented (and simplify some nested updates) - hopefully 9.4
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Rather than interacting with any bulbs, simulate interactions by printing to a terminal.
module Lifx.Lan.Mock.Terminal () where

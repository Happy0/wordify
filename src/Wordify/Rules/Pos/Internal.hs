module Wordify.Rules.Pos.Internal ( Pos(Pos) ) where

	  data Pos = Pos !Int !Int !String deriving (Eq,Show, Ord)

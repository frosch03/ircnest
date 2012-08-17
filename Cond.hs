module Cond (cond) where

cond t f True  = t
cond t f False = f

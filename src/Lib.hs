-- * Example source code file for a Haskell project
module Lib
    ( someDecl
    ) where

-- >>> 1 + 2

-- | This is a definition that is exported from the module
someDecl :: String
someDecl = "Hello CIS 5520" 

-- | This definition is not visible outside of the module
privateDecl :: String
privateDecl = "My password is 12345678"
-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{
module Lex (Token(..), tokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]

tokens :-

    $white+     ;
    $digit+     { NUM . read        }
    contain     { const CONTAIN     }
    bags?       { const BAG         }
    "no other"  { const NOOTHER     }
    $alpha+     { WORD              }
    \,          { const COMMA       }
    \.          { const PERIOD      }

{
data Token = WORD String
           | NUM Int
           | CONTAIN
           | BAG
           | COMMA
           | PERIOD
           | NOOTHER
    deriving (Show, Eq)

tokens :: String -> [Token]
tokens = alexScanTokens
}

-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{
module Lex (tokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]

tokens :-

    $white+     ;
    $digit+     { NUM . read        }
    contains?   { const CONTAINS    }
    bags?       { const BAG         }
    $alpha+     { WORD              }
    \,          { const COMMA       }
    \.          { const PERIOD      }

{
data Token = WORD String
           | NUM Int
           | CONTAINS
           | BAG
           | COMMA
           | PERIOD
    deriving (Show, Eq)

tokens :: String -> [Token]
tokens = alexScanTokens
}

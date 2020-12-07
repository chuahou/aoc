-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{
module Parse (parse) where

import Data.Graph
import Lex
}

%name parse
%tokentype  { Token         }
%error      { parseError    }

%token
    word    { WORD $$   }
    num     { NUM  $$   }
    contain { CONTAIN  }
    bag     { BAG       }
    comma   { COMMA     }
    period  { PERIOD    }
    noother { NOOTHER   }

%%

Rules : Rule Rules                  { $1 : $2       }
      | Rule                        { [$1]          }

Rule : Bag contain NumBags period   { Rule $1 $3    }

Bag : word word bag                 { $1 <> $2      }

NumBags : NumBag comma NumBags      { $1 : $3       }
        | NumBag                    { [$1]          }
        | noother bag               { []            }

NumBag : num Bag                    { ($1, $2)      }

{
data Rule = Rule String [(Int, String)] deriving Show

parseError :: [Token] -> a
parseError = error . show
}

{-# LANGUAGE LambdaCase #-}

module HS2BF.SAM2WS
  ( compileWS
  , pprintWS
  , WS(..)
  ) where

import HS2BF.SAM hiding (compileS)
import Data.List (intercalate)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- | AST Whitespace
data WS
  = Push Int
  | Add
  | Sub
  | Mul
  | Store
  | Load
  | Label String
  | Jump String
  | JumpZero String
  | Call String
  | Return
  | End
  deriving (Show, Eq)

type WSProgram = [WS]

-- | Counter for unique labels
freshLabel :: String -> String
freshLabel base = base ++ "_0"

-- | Compile SAM to WS
compileWS :: SAM -> WSProgram
compileWS (SAM _ [SProc _ [] ss]) = concatMap compileS ss ++ [End]

compileS :: Stmt -> WSProgram
compileS = \case
  Val (Memory _ d) v ->
    [ Push d
    , Load
    , Push v
    , Add
    , Push d
    , Store
    ]
  While (Memory _ d) body ->
    let lStart = "while_start"
        lEnd = "while_end"
    in [ Label lStart
       , Push d
       , Load
       , JumpZero lEnd
       ]
       ++ concatMap compileS body
       ++ [ Jump lStart
          , Label lEnd
          ]
  Move (Memory _ src) dsts ->
    concatMap (\(Memory _ d) -> [Push src, Load, Push d, Store]) dsts
  Input (Memory _ d) -> [Push d, Store]     -- <- tu poprawka
  Output (Memory _ d) -> [Push d, Load]    -- <- tu poprawka
  Clear (Memory _ d) -> [Push d, Push 0, Store]
  _ -> []

-- | Pretty printer: WS AST → Whitespace kod
pprintWS :: WSProgram -> String
pprintWS = concatMap wsInstr

wsInstr :: WS -> String
wsInstr = \case
  Push n     -> "  " ++ encodeInt n
  Add        -> "\t   "
  Sub        -> "\t  "
  Mul        -> "\t \t "
  Store      -> "\t\t "
  Load       -> "\t\t\t"
  Label l    -> "\n " ++ l ++ "\n"
  Jump l     -> "\n\t" ++ l ++ "\n"
  JumpZero l -> "\n\t " ++ l ++ "\n"
  Call l     -> "\n\t\t" ++ l ++ "\n"
  Return     -> "\n\t\n"
  End        -> "\n\n\n"

encodeInt :: Int -> String
encodeInt n = (if n < 0 then "\t" else " ") ++ binary ++ "\n"
  where
    binary = map (\c -> if c == '1' then '\t' else ' ') $
             showIntAtBase 2 intToDigit (abs n) ""
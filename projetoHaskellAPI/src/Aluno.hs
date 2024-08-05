{-# LANGUAGE DeriveGeneric #-}

module Aluno
  ( Aluno(..), -- Export the Aluno data type and its constructors
    CreateAlunoRequest(..) -- Export the CreateAlunoRequest data type and its constructors
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics

data Aluno = Aluno
  { raAluno :: Text,       -- RA attribute
    nomeAluno :: Text,     -- Nome attribute
    p1Aluno :: Double,     -- P1 attribute (assuming it's a floating-point number)
    p2Aluno :: Double,     -- P2 attribute (assuming it's a floating-point number)
    projetoAluno :: Double,-- Projeto attribute (assuming it's a floating-point number)
    l1Aluno :: Double,     -- L1 attribute (assuming it's a floating-point number)
    l2Aluno :: Double,     -- L2 attribute (assuming it's a floating-point number)
    l3Aluno :: Double,     -- L3 attribute (assuming it's a floating-point number)
    l4Aluno :: Double,     -- L4 attribute (assuming it's a floating-point number)
    l5Aluno :: Double      -- L5 attribute (assuming it's a floating-point number)
  }
  deriving (Generic)

-- Data type which describes the request that will be received to create a student
data CreateAlunoRequest = CreateAlunoRequest
  { ra :: Text,
    nome :: Text,
    p1 :: Double,
    p2 :: Double,
    projeto :: Double,
    l1 :: Double,
    l2 :: Double,
    l3 :: Double,
    l4 :: Double,
    l5 :: Double
  }
  deriving (Generic)

-- We define a FromJSON instance for CreateAlunoRequest because we will want
-- to parse it from an HTTP request body (JSON).
instance FromJSON CreateAlunoRequest

-- We also define a ToJSON instance for Aluno to convert it to JSON when responding
instance ToJSON Aluno

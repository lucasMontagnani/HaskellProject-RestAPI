{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DbAluno
  ( Aluno (..),
    getAlunoStore,
    insertAluno,
    deleteAluno,
    mkAlunoDb,
    AlunoStore (..),
    getAllAlunos,
  )
where

import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T


data Aluno = Aluno
  { ra :: Text,       -- RA attribute
    nome :: Text,     -- Nome attribute
    p1 :: Double,     -- P1 attribute (assuming it's a floating-point number)
    p2 :: Double,     -- P2 attribute (assuming it's a floating-point number)
    projeto :: Double,-- Projeto attribute (assuming it's a floating-point number)
    l1 :: Double,     -- L1 attribute (assuming it's a floating-point number)
    l2 :: Double,     -- L2 attribute (assuming it's a floating-point number)
    l3 :: Double,     -- L3 attribute (assuming it's a floating-point number)
    l4 :: Double,     -- L4 attribute (assuming it's a floating-point number)
    l5 :: Double      -- L5 attribute (assuming it's a floating-point number)
  }
  deriving (Show, Generic)

newtype AlunoStore = AlunoStore {unAlunoStore :: IORef (Map Int Aluno)}

-- Creates our initial empty database for students
mkAlunoDb :: IO AlunoStore
mkAlunoDb = do
  ref <- newIORef (Map.empty :: Map Int Aluno)
  pure $ AlunoStore ref

-- Accepts a default value to return in case the list is empty
safeLast :: a -> [a] -> a
safeLast x [] = x
safeLast _ (x : xs) = safeLast x xs

-- Utility to allow us to read the data in our "database" for students
getAlunoStore :: AlunoStore -> IO (Map Int Aluno)
getAlunoStore (AlunoStore store) = readIORef store

-- VERY naive utility to get the next unique student ID
getNextId :: AlunoStore -> IO Int
getNextId x = (+ 1) . safeLast 0 . sort . Map.keys <$> getAlunoStore x

-- insertAluno uses getNextId to get the next ID and then updates our database using
-- modifyIORef from the Data.IORef library. It returns the new ID as a result.
insertAluno :: AlunoStore -> Aluno -> IO Int
insertAluno alunoStore aluno = do
  nextId <- getNextId alunoStore
  modifyIORef (unAlunoStore alunoStore) (Map.insert nextId aluno)
  pure nextId

-- deleteAlunoByRA updates our database by deleting the relevant student data by RA
deleteAluno :: AlunoStore -> T.Text -> IO ()
deleteAluno alunoStore raToDelete = modifyIORef' (unAlunoStore alunoStore) delete
  where
    delete :: Map Int Aluno -> Map Int Aluno
    delete alunoMap = Map.filter (\aluno -> raToDelete /= ra aluno) alunoMap



-- Function to retrieve all students from the database
getAllAlunos :: AlunoStore -> IO (Map Int Aluno)
getAllAlunos alunoStore = do
  alunoMap <- getAlunoStore alunoStore
  return alunoMap

instance ToJSON Aluno where
  toJSON = genericToJSON defaultOptions

instance FromJSON Aluno where
  parseJSON = genericParseJSON defaultOptions

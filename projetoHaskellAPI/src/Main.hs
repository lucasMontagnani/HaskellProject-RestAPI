module Main (main) where

import Control.Monad.IO.Class
import qualified DbAluno as Db
import Aluno (CreateAlunoRequest (..))
import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map

main :: IO ()
main = do
  -- Initialize our fake DB
  db <- Db.mkAlunoDb

  -- Run the scotty web app on port 8080
  scotty 8080 $ do
    
    -- Listen for POST requests on the "/alunos" endpoint
    post (capture "/alunos") $
      do
        -- parse the request body into our CreateAlunoRequest type
        createAlunoReq <- jsonData

        -- Create our new student.
        -- In order for this compile we need to use liftIO here to lift the IO from our
        -- createAluno function. This is because the `post` function from scotty expects an
        -- ActionM action instead of an IO action
        newAlunoId <- liftIO $ createAluno db createAlunoReq

        -- Return the student ID of the new student in the HTTP response
        json newAlunoId

    -- Listen for DELETE requests on the "/alunos/:ra" endpoint
    delete (capture "/alunos/:ra") $ do
      -- Get the value of the RA from the URL
      ra <- param (TL.pack "ra")

      -- Delete the relevant student by RA
      -- You will need to implement a function that deletes by RA in your database module.
      liftIO $ Db.deleteAluno db ra

    -- New route for GET request to retrieve all students
    get (capture "/alunos") $ do
      -- Retrieve all students from the database
      alunosMap <- liftIO $ Db.getAllAlunos db
      -- Convert the Map of students to a list of Aluno
      let alunosList = Map.elems alunosMap
      -- Return the list of students in the HTTP response
      json alunosList

-- Our createAluno function simply deals with constructing an Aluno value and passes it
-- to the Db.insertAluno function
createAluno :: Db.AlunoStore -> CreateAlunoRequest -> IO Int
createAluno db CreateAlunoRequest {ra = ra, nome = nome, p1 = p1, p2 = p2, projeto = projeto, l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5} = 
  Db.insertAluno db aluno
  where
    aluno = Db.Aluno
      { Db.ra = ra,
        Db.nome = nome,
        Db.p1 = p1,
        Db.p2 = p2,
        Db.projeto = projeto,
        Db.l1 = l1,
        Db.l2 = l2,
        Db.l3 = l3,
        Db.l4 = l4,
        Db.l5 = l5
      }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Web.Spock
import Web.Spock.Config
import Data.Aeson
import qualified Data.Text as T
import Data.List (sort)
import Data.Char
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Method (methodDelete)
import Data.IORef

data Aluno = Aluno
  { l1 :: Double,
    l2 :: Double,
    l3 :: Double,
    l4 :: Double,
    l5 :: Double,
    nome :: T.Text,
    p1 :: Double,
    p2 :: Double,
    projeto :: Double,
    ra :: T.Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Aluno
instance ToJSON Aluno

-- Define a function to fetch student data
fetchStudentData :: IO LBS.ByteString
fetchStudentData = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "http://localhost:8080/alunos"
  response <- httpLbs request manager
  let responseBodyStr = responseBody response
  case statusCode (responseStatus response) of
    200 -> do
      putStrLn $ "Resultado da requisição:\n" ++ show responseBodyStr
      return responseBodyStr
    _ -> do
      putStrLn $ "Erro na requisição: " ++ show responseBodyStr
      return responseBodyStr

-- Define the function to fetch student data and update the global state
fetchAndUpdateStudentData :: IORef LBS.ByteString -> IO ()
fetchAndUpdateStudentData studentDataRef = do
  responseBodyStr <- fetchStudentData
  atomicWriteIORef studentDataRef responseBodyStr

main :: IO ()
main = do
  putStrLn "hello world"

  -- Fetch student data
  responseBodyStr <- fetchStudentData

  -- Create an IORef to hold the student data
  studentDataRef <- newIORef responseBodyStr

  -- Configuração do Spock
  spockCfg <- defaultSpockCfg () PCNoDatabase ()

  -- Inicialize o aplicativo Spock
  runSpock 8081 (spock spockCfg (app studentDataRef))

-- Update the type of app to accept IORef LBS.ByteString
app :: IORef LBS.ByteString -> SpockM () () () ()
app studentDataRef = do
  -- Rota para a página inicial
  get root $ do
    -- Fetch the student data from the IORef
    responseBodyStr <- liftIO $ readIORef studentDataRef
    let parsedData = decode responseBodyStr :: Maybe [Aluno]
    case parsedData of
      Just alunos -> do
        html $ mconcat
          [ "<html>"
          , "<head><title>Resultado da Requisição</title></head>"
          , "<body>"
          , "<h1>Resultado da Requisição:</h1>"
          , "<table border='1'>"
          , "<tr>"
          , "<th>Nome</th>"
          , "<th>RA</th>"
          , "<th>L1</th>"
          , "<th>L2</th>"
          , "<th>L3</th>"
          , "<th>L4</th>"
          , "<th>L5</th>"
          , "<th>P1</th>"
          , "<th>P2</th>"
          , "<th>Projeto</th>"
          , "<th>Media</th>"
          , "<th>Conceito</th>"
          , "<th>Deletar</th>"
          , "</tr>"
          , mconcat (map renderAlunoTable alunos)
          , "</table>"

            -- Add a form to submit a new student
          , "<h2>Add New Student</h2>"
          , "<form method='POST' action='/add'>"
          , "  <label for='nome'>Nome:</label>"
          , "  <input type='text' name='nome' required><br>"
          , "  <label for='ra'>RA:</label>"
          , "  <input type='text' name='ra' required><br>"
          , "  <label for='p1'>P1:</label>"
          , "  <input type='number' name='p1' step='0.1' required><br>"
          , "  <label for='p2'>P2:</label>"
          , "  <input type='number' name='p2' step='0.1' required><br>"
          , "  <label for='projeto'>Projeto:</label>"
          , "  <input type='number' name='projeto' step='0.1' required><br>"
          , "  <label for='l1'>L1:</label>"
          , "  <input type='number' name='l1' step='0.1' required><br>"
          , "  <label for='l2'>L2:</label>"
          , "  <input type='number' name='l2' step='0.1' required><br>"
          , "  <label for='l3'>L3:</label>"
          , "  <input type='number' name='l3' step='0.1' required><br>"
          , "  <label for='l4'>L4:</label>"
          , "  <input type='number' name='l4' step='0.1' required><br>"
          , "  <label for='l5'>L5:</label>"
          , "  <input type='number' name='l5' step='0.1' required><br>"
          , "  <button type='submit'>Add Student</button>"
          , "</form>"

          , "</body>"
          , "</html>"
          ]
      Nothing -> html "Erro ao analisar os dados da API."

  -- Route to handle student deletion
  post "delete" $ do
    maybeRaToDelete <- param "ra"
    case maybeRaToDelete of
      Just raToDelete -> do
        liftIO $ deleteAluno raToDelete
        -- Refresh student data after deletion
        liftIO $ fetchAndUpdateStudentData studentDataRef
        redirect "/"
      Nothing -> text "Invalid or missing 'ra' parameter"

  -- Add a new route to handle the submission of a new Aluno
  post "add" $ do
    -- Retrieve data from the submitted form
    maybeNome <- param "nome"
    maybeRa <- param "ra"
    maybeP1 <- param "p1"
    maybeP2 <- param "p2"
    maybeProjeto <- param "projeto"
    maybeL1 <- param "l1"
    maybeL2 <- param "l2"
    maybeL3 <- param "l3"
    maybeL4 <- param "l4"
    maybeL5 <- param "l5"

    -- Check if all required parameters are present
    case (maybeNome, maybeRa, maybeP1, maybeP2, maybeProjeto, maybeL1, maybeL2, maybeL3, maybeL4, maybeL5) of
      (Just nome, Just ra, Just p1, Just p2, Just projeto, Just l1, Just l2, Just l3, Just l4, Just l5) -> do
        -- Create a new Aluno object
        let newAluno = Aluno
              { nome = nome,
                ra = ra,
                p1 = p1,
                p2 = p2,
                projeto = projeto,
                l1 = l1,
                l2 = l2,
                l3 = l3,
                l4 = l4,
                l5 = l5
              }
        -- Send a POST request to the API to create the new Aluno
        liftIO $ createNewAluno newAluno
        -- Refresh student data after adding the new Aluno
        liftIO $ fetchAndUpdateStudentData studentDataRef
        redirect "/"
      _ -> text "Invalid or missing parameters"



-- Função para calcular a média das notas das listas
mediaListas :: Double -> Double -> Double -> Double -> Double -> Double -> Double
mediaListas l1 l2 l3 l4 l5 l6 =
  let notas = take 5 $ reverse $ sort [l1, l2, l3, l4, l5, l6]
  in sum notas / 5

-- Função para calcular a média das provas
calcularNotaProvas :: Double -> Double -> Double
calcularNotaProvas p1 p2 = (2 * p1 + 3 * p2) / 5

-- Função para calcular a média final
calcularMediaFinal :: Double -> Double -> Double -> Double
calcularMediaFinal na np nl = 10 / ((5 / maxNa) + (3 / maxNp) + (2 / maxNl))
  where
    maxNa = max 0.1 na
    maxNp = max 0.1 np
    maxNl = max 0.1 nl

converteNota :: Double -> Char
converteNota nota
  | nota >= 0.0 && nota <= 5.0   = 'F'
  | nota > 5.0 && nota <= 6.0    = 'D'
  | nota > 6.0 && nota <= 7.0    = 'C'
  | nota > 7.0 && nota <= 8.5    = 'B'
  | nota > 8.5 && nota <= 10.0   = 'A'

deleteAluno :: T.Text -> IO ()
deleteAluno ra = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:8080/alunos/" ++ T.unpack ra
  request <- parseRequest url
  let request' = request { method = methodDelete }  -- Set the request method to DELETE
  response <- httpLbs request' manager
  let status = responseStatus response
      responseBodyStr = responseBody response
  putStrLn $ "HTTP Status Code: " ++ show (statusCode status)
  LBS.putStrLn responseBodyStr
  case statusCode status of
    204 -> putStrLn "Student deleted successfully."  -- Assuming your API returns 204 No Content on successful deletion
    _ -> putStrLn "Error deleting student."
  
createNewAluno :: Aluno -> IO ()
createNewAluno aluno = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:8080/alunos"
  let requestBody = RequestBodyLBS (encode aluno)
  request <- parseRequest url
  let request' = request
        { method = "POST",  -- Set the request method to POST
          requestBody = requestBody,
          requestHeaders = [("Content-Type", "application/json")]
        }
  response <- httpLbs request' manager
  let status = responseStatus response
      responseBodyStr = responseBody response
  putStrLn $ "HTTP Status Code: " ++ show (statusCode status)
  LBS.putStrLn responseBodyStr
  case statusCode status of
    201 -> putStrLn "Student created successfully."  -- Assuming your API returns 201 Created on successful creation
    _ -> putStrLn "Error creating student."


-- Função para renderizar um aluno em uma linha de tabela HTML
renderAlunoTable :: Aluno -> T.Text
renderAlunoTable aluno =
  let notaProvas = calcularNotaProvas (p1 aluno) (p2 aluno)
      mediaListas' = mediaListas (l1 aluno) (l2 aluno) (l3 aluno) (l4 aluno) (l5 aluno) (projeto aluno)
      mediaFinal = calcularMediaFinal notaProvas (projeto aluno) mediaListas'
      conceito = converteNota mediaFinal  -- Calculate the "Conceito"
  in mconcat
    [ "<tr>"
    , "<td>" <> nome aluno <> "</td>"
    , "<td>" <> ra aluno <> "</td>"
    , "<td>" <> T.pack (show $ l1 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ l2 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ l3 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ l4 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ l5 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ p1 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ p2 aluno) <> "</td>"
    , "<td>" <> T.pack (show $ projeto aluno) <> "</td>"
    , "<td>" <> T.pack (show mediaFinal) <> "</td>"
    , "<td>" <> T.pack [toUpper conceito] <> "</td>"  -- Display the "Conceito" as an uppercase letter
    , "<td><form method='POST' action='/delete'><input type='hidden' name='ra' value='" <> ra aluno <> "'><button type='submit'>Delete</button></form></td>"
    , "</tr>"
    ]

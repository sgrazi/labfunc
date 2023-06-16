----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'

-- start

joinChecks :: Checked -> Checked -> Checked
joinChecks Ok Ok = Ok
joinChecks (Wrong x) (Wrong y) = Wrong (x ++ y)
joinChecks (Wrong errors) _ = Wrong errors
joinChecks _ (Wrong errors) = Wrong errors

-- primero se chequea que no hayan multiples declaraciones de una misma funcion 
-- y luego a continuacion se chequea que no haya nombres de parametros repetidos 
-- dentro de cada declaracion de funcion
defsRNCheck :: Defs -> Checked
defsRNCheck defs = case joinChecks (checkFuncNames defs) (checkVars defs) of
  Ok -> Ok
  Wrong errors -> Wrong errors
  where
    checkVars [] = Ok
    checkVars (x:xs) = joinChecks (checkVar x) (checkVars xs)
    
    checkFuncNames [] = Ok
    checkFuncNames xs = 
      let names = funcNames xs
          repNames = repetidas names []
      in if null repNames then Ok else Wrong repNames

    checkVar (FunDef (name,_) xs _) =
      let repVars = repetidas xs []
      in if null repVars then Ok else Wrong repVars
    
    funcNames [] = []
    funcNames [(FunDef (name,_) _ _)] = [name]
    funcNames (x:xs) = funcNames x ++ funcNames xs

    repetidas [] _ = []
    repetidas (y:ys) vistas
      | y `elem` vistas = Duplicated y : repetidas ys (y : vistas)
      | otherwise = repetidas ys (y : vistas)


exprRNCheck :: Expr -> Checked
exprRNCheck _ = Ok
repeatedNameCheck :: Program -> Checked
repeatedNameCheck (Program defs expr) = do
  let defsResult = defsRNCheck defs
  let exprResult = exprRNCheck expr
  joinChecks defsResult exprResult

paramCheck :: Program -> Checked
-- paramCheck _ = Wrong [Duplicated "Chau"]
paramCheck _ = Ok
declaredNameCheck :: Program -> Checked
declaredNameCheck _ = Ok
typeCheck :: Program -> Checked
typeCheck _ = Ok

checkProgram :: Program -> Checked
checkProgram p = 
  case repeatedNameCheck p of
    Ok -> 
      case paramCheck p of
        Ok -> 
          case declaredNameCheck p of
            Ok -> 
              case typeCheck p of
                Ok -> Ok
                Wrong errors -> Wrong errors
            Wrong errors -> Wrong errors
        Wrong errors -> Wrong errors
    Wrong errors -> Wrong errors
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
import Debug.Trace
-- CHECKER
type EnvFunc = [(Name, (Type, [Type]))]

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
           | ERROR           Name
            
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
 show (ERROR    e)
   = "EXPLOTO: " ++ show e

-- start

joinChecks :: Checked -> Checked -> Checked
joinChecks Ok Ok = Ok
joinChecks (Wrong x) (Wrong y) = Wrong (x ++ y)
joinChecks (Wrong errors) _ = Wrong errors
joinChecks _ (Wrong errors) = Wrong errors

joinChecksArray :: [Checked] -> Checked
joinChecksArray [] = Ok
joinChecksArray (x:xs) = joinChecks x (joinChecksArray xs)

getFuncNames :: Defs -> [Name]
getFuncNames [] = []
getFuncNames [(FunDef (name,_) _ _)] = [name]
getFuncNames (x:xs) = getFuncNames [x] ++ getFuncNames xs

repeatedNameCheck :: Program -> Checked
repeatedNameCheck (Program defs _) = case joinChecks (checkFuncNames defs) (checkVars defs) of
  Ok -> Ok
  Wrong errors -> Wrong errors
  where
    checkVars [] = Ok
    checkVars (x:xs) = joinChecks (checkVar x) (checkVars xs)
    
    checkFuncNames [] = Ok
    checkFuncNames xs = 
      let names = getFuncNames xs
          repNames = repetidas names []
      in if null repNames then Ok else Wrong repNames

    checkVar (FunDef (name,_) xs _) =
      let repVars = repetidas xs []
      in if null repVars then Ok else Wrong repVars

    repetidas [] _ = []
    repetidas (y:ys) vistas
      | y `elem` vistas = Duplicated y : repetidas ys (y : vistas)
      | otherwise = repetidas ys (y : vistas)

paramCheck :: Program -> Checked
paramCheck (Program defs expr) = case joinChecks (checkParamAmountDefs defs) (checkParamAmountExpr expr) of
  Ok -> Ok
  Wrong errors -> Wrong errors
  where
    checkParamAmountDefs [] = Ok
    checkParamAmountDefs (x:xs) = joinChecks (checkFuncParams x) (checkParamAmountDefs xs)

    checkFuncParams (FunDef (name,sig) xs _) = compareArguments name sig xs
      
    compareArguments functionName (Sig types _) argNames
      | length types == length argNames = Ok
      | otherwise = Wrong [ArgNumDef functionName (length types) (length argNames)]
    
    checkParamAmountExpr _ = Ok -- TODO

getNames :: Expr -> [Name]
getNames expr = nub $ case expr of
  Var name -> [name]
  IntLit _ -> []
  BoolLit _ -> []
  Infix _ x y -> getNames x ++ getNames y
  If x y z -> getNames x ++ getNames y ++ getNames z
  Let (name, _) x y -> name : getNames x ++ getNames y
  App name x -> name : concatMap getNames x

findUndefinedNames :: Expr -> [Name] -> [Name]
findUndefinedNames expr inputNames = nub $ findMissingNames expr inputNames []
  where
    findMissingNames :: Expr -> [Name] -> [Name] -> [Name]
    findMissingNames expr names acc = case expr of
      Var name -> if name `elem` acc then acc else name : acc
      IntLit _ -> acc
      BoolLit _ -> acc
      Infix _ x y -> findMissingNames x names (findMissingNames y names acc)
      If x y z -> findMissingNames x names (findMissingNames y names (findMissingNames z names acc))
      Let (name, _) x y -> findMissingNames x names (name : acc) ++ findMissingNames y names (name : acc)
      -- TODO: ver si es necesario que se revise si name esta en la lista de variables no definidas (acc)
      -- App name args -> if name `elem` acc || name `elem` names then acc else name : acc ++ concatMap (`findMissingNames'` names) args
      App name args -> if name `elem` names then acc else name : concatMap (\arg -> findMissingNames arg names []) args ++ acc

declaredNameCheck :: Program -> Checked
declaredNameCheck (Program defs expr) = case joinChecks (checkDefs defs) (checkExpr expr defs) of
  Ok -> Ok
  Wrong errors -> Wrong errors
  where
    checkDefs [] = Ok
    checkDefs (x:xs) = joinChecks (checkDeclDefs x) (checkDefs xs)
      
    checkDeclDefs (FunDef (name,_) xs expr) = 
      let undefs = (getNames expr) \\ (name:xs)
      in if null undefs then Ok else trace ("es en las funciones") $ Wrong (map (\name -> Undefined name) undefs)

    checkExpr expr defs = 
      let missingNames = findUndefinedNames expr (getFuncNames defs)
      in if null missingNames then Ok else trace ("es en las expr") $ Wrong (map (\name -> Undefined name) missingNames)

compareTypes :: Type -> Type -> Checked
compareTypes TyBool TyInt = Wrong [Expected TyBool TyInt]
compareTypes TyInt TyBool = Wrong [Expected TyInt TyBool]
compareTypes _ _ = Ok

getType :: Expr -> Env -> EnvFunc -> Type
getType expr env envFunc = case expr of
  Var name -> case lookup name env of
    Just varType -> varType
    Nothing -> trace ("Error " ++ show name) $ TyInt -- TODO BORRAR
  IntLit _ -> TyInt
  BoolLit _ -> TyBool
  Infix op x y -> case op of
      Add -> TyInt
      Sub -> TyInt
      Mult -> TyInt
      Div -> TyInt
      _ -> TyBool
  If x y z -> (getType x env envFunc)
  Let (name, varType) x y -> getType y (env ++ [(name, varType)]) envFunc
  App name exprArr -> case lookup name envFunc of
    Just (returnType, _) -> returnType
    Nothing -> trace ("Error " ++ show name) $ TyInt -- TODO BORRAR

checkExprType :: Expr -> Type -> Env -> EnvFunc -> Checked
checkExprType expr rtrn env envFunc = case expr of
  Var name -> case lookup name env of
    Just varType -> compareTypes rtrn varType
    Nothing -> trace ("Error " ++ show name) $ Wrong [ERROR ("No encontro name de var " ++ name)] -- TODO BORRRAR
  IntLit int -> compareTypes rtrn TyInt
  BoolLit bool -> compareTypes rtrn TyBool
  Infix op x y -> 
    let opAritmetico = joinChecks (compareTypes rtrn TyInt) (joinChecks (checkExprType x TyInt env envFunc) (checkExprType y TyInt env envFunc))
    in let opEq = joinChecks (compareTypes rtrn TyBool) (joinChecks (checkExprType x (getType x env envFunc) env envFunc) (checkExprType y (getType x env envFunc) env envFunc))
    in let opEqAritmetico = joinChecks (compareTypes rtrn TyBool) (joinChecks (checkExprType x TyInt env envFunc) (checkExprType y TyInt env envFunc))
    in  case op of
      Add  -> opAritmetico
      Sub  -> opAritmetico
      Mult -> opAritmetico
      Div  -> opAritmetico
      Eq -> opEq
      NEq -> opEq
      GTh -> opEqAritmetico
      LTh -> opEqAritmetico
      GEq -> opEqAritmetico
      LEq -> opEqAritmetico
  If x y z -> joinChecks (checkExprType x TyBool env envFunc) (joinChecks (checkExprType y (getType y env envFunc) env envFunc) (checkExprType z (getType y env envFunc) env envFunc))
  Let (name, varType) x y -> joinChecks (checkExprType x varType env envFunc) (checkExprType y rtrn (env ++ [(name, varType)]) envFunc)
  App name exprArr -> case lookup name envFunc of
    Just (returnType, typeArr) -> case returnType of
       expectedType | expectedType == rtrn -> joinChecksArray (map (\(expr, exprType) -> checkExprType expr exprType env envFunc) (zip exprArr typeArr))
                    | otherwise -> joinChecks (Wrong [Expected rtrn returnType]) (joinChecksArray (map (\(expr, exprType) -> checkExprType expr exprType env envFunc) (zip exprArr typeArr)))
    Nothing -> trace ("Error " ++ show name) $ Wrong [ERROR ("No encontro name de func " ++ name)] -- TODO BORRRAR

checkFuncTypes :: FunDef  -> Checked
checkFuncTypes (FunDef (name, (Sig types returnType)) names expr) = case checkExprType expr returnType (zip names types) [(name, (returnType, types))] of
  Ok -> Ok
  Wrong errors -> Wrong errors

typeCheck :: Program -> Checked
typeCheck (Program defs expr) = case joinChecks (checkDefs defs) (checkExpr defs expr []) of
  Ok -> Ok
  Wrong errors -> Wrong errors
  where
    checkDefs [] = Ok
    checkDefs (x:xs) = joinChecks (checkFuncTypes x) (checkDefs xs)
    
    checkExpr defs expr env = case expr of
      Var _ -> Ok
      IntLit _ -> Ok
      BoolLit _ -> Ok
      Infix op _ _ -> case op of
          Add  -> checkExprType expr TyInt env (genEnvFunc defs)
          Sub  -> checkExprType expr TyInt env (genEnvFunc defs)
          Mult -> checkExprType expr TyInt env (genEnvFunc defs)
          Div  -> checkExprType expr TyInt env (genEnvFunc defs)
          _ -> checkExprType expr TyBool env (genEnvFunc defs)
      If x y z -> joinChecks (checkExprType x TyBool env (genEnvFunc defs)) (joinChecks (checkExprType y (getType y env (genEnvFunc defs)) env (genEnvFunc defs)) (checkExprType z (getType y env (genEnvFunc defs)) env (genEnvFunc defs)))
      Let (name, varType) x y -> joinChecks (checkExprType x varType env (genEnvFunc defs)) (checkExpr defs y (env ++ [(name, varType)]))
      App name exprArr -> case lookup name (genEnvFunc defs) of
        Just (_, typeArr) -> joinChecksArray (map (\(expr, exprType) -> checkExprType expr exprType env (genEnvFunc defs)) (zip exprArr typeArr))
        Nothing -> trace ("Error " ++ show name) $ Wrong [ERROR ("No encontro name de func " ++ name)] -- TODO BORRRAR
    
    genEnvFunc [] = []
    genEnvFunc (x:xs) = addFunc x ++ genEnvFunc xs
    
    addFunc (FunDef (name, (Sig types returnType)) _ _) = [(name, (returnType, types))]

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
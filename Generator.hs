----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List
import Debug.Trace

type LetEnv = [(Name, Integer)]

-- CODE GENERATOR

genProgram :: Program -> String
genProgram (Program defs expr) = "#include <stdio.h>\n" ++ genFunctions defs ++ genMain expr

genFunctions :: [FunDef] -> String
genFunctions [] = ""
genFunctions (x:xs) = genFunction x ++ genFunctions xs

genFunction :: FunDef -> String
genFunction (FunDef (funcName, (Sig paramTypes rtrnType)) paramNames expr) = 
    let (_, s, e) = genLetFuncs expr 0 [] in
        "int _" ++ funcName ++ "(" ++ (genParams paramNames) ++ "){\n" ++ s ++ "return (" ++ (genExpr expr e) ++ "); };\n"

genParams :: [Name] -> String
genParams names = intercalate "," (map genParam names)
  where
    genParam :: Name -> String
    genParam name = "int _" ++ name

parentesis :: Expr -> LetEnv -> String
parentesis (Var x) e = genExpr (Var x) e
parentesis (IntLit x) e = genExpr (IntLit x) e
parentesis (BoolLit x) e = genExpr (BoolLit x) e
parentesis (App name exprArr) e = genExpr (App name exprArr) e
parentesis (Let (n, v) x y) e = genExpr (Let (n, v) x y) e
parentesis expr e = "(" ++ genExpr expr e ++ ")"

genExpr :: Expr -> LetEnv -> String
genExpr expr e = case expr of
    Var x -> "_" ++ x
    IntLit x -> show x
    BoolLit x -> case x of
        False -> "0"
        True -> "1"
    Infix op x y -> case op of
        Add  -> parentesis x e ++ " + " ++ parentesis y e
        Sub  -> parentesis x e ++ " - " ++ parentesis y e
        Mult -> parentesis x e ++ " * " ++ parentesis y e
        Div  -> parentesis x e ++ " \\ " ++ parentesis y e
        Eq -> parentesis x e ++ " == " ++ parentesis y e
        NEq -> parentesis x e ++ " != " ++ parentesis y e
        GTh -> parentesis x e ++ " > " ++ parentesis y e
        LTh -> parentesis x e ++ " < " ++ parentesis y e
        GEq -> parentesis x e ++ " >= " ++ parentesis y e
        LEq -> parentesis x e ++ " <= " ++ parentesis y e
    If x y z -> parentesis x e ++ "?" ++ parentesis y e ++ ":" ++ parentesis z e
    Let (name, _) x _ -> case lookup name e of
        Just i -> "_let" ++ show i ++ "(" ++ genExpr x (remove e (name, i)) ++ ")"
        Nothing -> trace ("No encontro el let " ++ name ++ " en " ++ show e) $ " ERROR "
    App name exprArr -> "_" ++ name ++ "(" ++ intercalate ", " (map (\expr -> parentesis expr e) exprArr) ++ ")"

remove :: LetEnv -> (String, Integer) -> LetEnv
remove [] _  = []
remove ((key, val):other) (name, i) = if (key == name && val == i) then other else (key, val):(remove other (name, i))

genLetFuncs :: Expr -> Integer -> LetEnv -> (Integer, String, LetEnv)
genLetFuncs (Infix op x y) n e =
    let (i, s, e1) = genLetFuncs x n e in
        let (i2, s2, e2) = genLetFuncs y (i) e1 in
            (i2, s ++ s2, e2)
genLetFuncs (If x y z) n e =
    let (i, s, e1) = genLetFuncs x n e in
        let (i2, s2, e2) = genLetFuncs y (i) e1 in 
            let (i3, s3, e3) = genLetFuncs z (i2) e2 in
                (i3, s ++ s2 ++ s3, e3)
genLetFuncs (Let (name, _) x y) n e = 
    let (i, s, e1) = genLetFuncs x n e in
        let (i2, s2, e2) = genLetFuncs y i e in
            (i2+1, s ++ "int _let" ++ show i2 ++ "(int _" ++ name ++"){\n" ++ s2 ++ "return (" ++ (parentesis y (e1 ++ e2)) ++ "); };\n", ([(name,i2)] ++ e1 ++ e2))
genLetFuncs _ n e = (n, "", e)

genMain :: Expr -> String
genMain expr = 
    let (_, s, e) = genLetFuncs expr 0 [] in 
        "int main() {\n" ++ s ++ "printf(\"%d\\n\"," ++ parentesis expr e ++ "); }\n"

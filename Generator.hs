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

type LetEnv = [(Name, Integer)]

-- CODE GENERATOR

genProgram :: Program -> String
genProgram (Program defs expr) = "#include <stdio.h>\n" ++ genFunctions defs ++ genMain expr

genFunctions :: [FunDef] -> String
genFunctions [] = ""
genFunctions (x:xs) = genFunction x ++ genFunctions xs

genFunction :: FunDef -> String
genFunction (FunDef (funcName, (Sig paramTypes rtrnType)) paramNames expr) = 
    let (_, s) = genLetFuncs expr 0 in
        "int _" ++ funcName ++ "(" ++ (genParams paramNames) ++ "){\n" ++ s ++ "return (" ++ (genExpr expr) ++ "); };\n"

genParams :: [Name] -> String
genParams names = intercalate "," (map genParam names)
  where
    genParam :: Name -> String
    genParam name = "int _" ++ name

parentesis :: Expr -> String
parentesis (Var x) = genExpr (Var x)
parentesis (IntLit x) = genExpr (IntLit x)
parentesis (BoolLit x) = genExpr (BoolLit x)
parentesis (App name exprArr) = genExpr (App name exprArr)
parentesis (Let (n, v) x y) = genExpr (Let (n, v) x y)
parentesis expr = "(" ++ genExpr expr ++ ")"

genExpr :: Expr -> String
genExpr expr = case expr of
    Var x -> "_" ++ x
    IntLit x -> show x
    BoolLit x -> case x of
        False -> "0"
        True -> "1"
    Infix op x y -> case op of
        Add  -> parentesis x ++ " + " ++ parentesis y
        Sub  -> parentesis x ++ " - " ++ parentesis y
        Mult -> parentesis x ++ " * " ++ parentesis y
        Div  -> parentesis x ++ " \\ " ++ parentesis y
        Eq -> parentesis x ++ " == " ++ parentesis y
        NEq -> parentesis x ++ " != " ++ parentesis y
        GTh -> parentesis x ++ " > " ++ parentesis y
        LTh -> parentesis x ++ " < " ++ parentesis y
        GEq -> parentesis x ++ " >= " ++ parentesis y
        LEq -> parentesis x ++ " <= " ++ parentesis y
    If x y z -> parentesis x ++ "?" ++ parentesis y ++ ":" ++ parentesis z
    Let (name, _) x _ -> "_letX(" ++ (genExpr x) ++ ")"
    App name exprArr -> "_" ++ name ++ "(" ++ (intercalate ", " (map parentesis exprArr)) ++")"

genLetFuncs :: Expr -> Integer -> (Integer, String)
genLetFuncs (Infix op x y) n =
    let (i, s) = genLetFuncs x n in
        let (i2, s2) = genLetFuncs y (i) in
            (i2, s ++ s2)
genLetFuncs (If x y z) n =
    let (i, s) = genLetFuncs x n in
        let (i2, s2) = genLetFuncs y (i) in 
            let (i3, s3) = genLetFuncs z (i2) in
                (i3, s ++ s2 ++ s3)
genLetFuncs (Let (name, _) x y) n = 
    let (i, s) = genLetFuncs y n in
        (i+1, "int _let" ++ show i ++ "(int _" ++ name ++"){\n" ++ s ++ "return (" ++ (genExpr y) ++ "); };\n")
genLetFuncs _ n = (n,"")

genMain :: Expr -> String
genMain expr = "int main() {\nprintf(\"%d\\n\"," ++ genExpr expr ++ "); }"

----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es 
-- un literal entero o booleano. En ese caso se 
-- sustituyen las ocurrencias de x en e2 por e1, 
-- o sea, e2[e1/x]. 
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List

-- ELIMINACION DE LETs

letElimP :: Program -> Program 
letElimP (Program funcs expr) = Program (letElimFunc funcs) (letElimExpr expr)

letElimFunc :: Defs -> Defs
letElimFunc [] = []
letElimFunc (FunDef def names funcExpr : xs) =
    FunDef def names (letElimExpr funcExpr) : letElimFunc xs

letElimExprArr :: [Expr] -> [Expr]
letElimExprArr [] = []
letElimExprArr (x:xs) = letElimExpr x : letElimExprArr xs

letElimExpr :: Expr -> Expr
letElimExpr (Infix op x y) = Infix op (letElimExpr x) (letElimExpr y)
letElimExpr (If x y z) = If (letElimExpr x) (letElimExpr y) (letElimExpr z)
letElimExpr (Let (name, varType) x y) = case x of
    (IntLit n) -> 
        let expr = letElimExpr y in
            subst name x expr
    (BoolLit b) -> 
        let expr = letElimExpr y in
            subst name x expr
    (Let (n, v) i j) ->
        let expr = letElimExpr i in
            let expr2 = subst n expr j in
                subst n expr2 y
    otherwise -> (Let (name, varType) x (letElimExpr y))
letElimExpr (App name exprArr) = App name (letElimExprArr exprArr)
letElimExpr x = x

subst :: Name -> Expr -> Expr -> Expr 
subst n e1 (Var x) = if x == n then e1 else Var x
subst n e1 (Infix op x y) = (Infix op (subst n e1 x) (subst n e1 y)) 
subst n e1 (If x y z) = (If (subst n e1 x) (subst n e1 y) (subst n e1 z))
subst n e1 (Let (name, varType) x y) = (Let (name, varType) (subst n e1 x) (subst n e1 y))
subst n e1 (App name exprArr) = (App name (map (\expr -> subst n e1 expr) exprArr))
subst _ _ e2 = e2

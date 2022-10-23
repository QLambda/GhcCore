module CoreDump.UntypedCoreExtract where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import CoreDump.SimpleCore
import CoreDump.TypeToStr


{-
 Module extraction section
-}

-- sExprToUExp (Var id) = SVar n ""
--                             where
--                                 (n, t)=extractVar id
-- sExprToUExp (Lit literal) = SLit $ showOuputable literal
-- sExprToUExp (App expr arg) = if isType arg then expr1 else SApp expr1 utarg
--                                         where
--                                             expr1 = sExprToUExp expr
--                                             utarg = sExprToUExp arg

-- sExprToUExp (Lam name expr) = if isKindVar name then SExpr else SLam n "" SExpr
--                                         where
--                                             SExpr = sExprToUExp expr
--                                             (n, t)=extractVar name

-- sExprToUExp (Let binder expr) = UTLet (getUTBinder binder) (sExprToUExp expr)
-- sExprToUExp (Cast expr coersion) = Skip $ "SCast("++  show (sExprToUExp expr) ++ "~" ++ showOuputable coersion ++ ")"
-- sExprToUExp (STick _ expr) = Skip $ "STick("++  show (sExprToUExp expr) ++ ")"
-- sExprToUExp (Type t) = Skip $ "@" ++ showPprUnsafe t
-- sExprToUExp (SCoercion c) = Skip $ "~ " ++ showPprUnsafe c
-- sExprToUExp (Case exp1 b t alts) = Skip $ "Case  (" ++ show (sExprToUExp exp1) ++ ") of " ++ (show $ map showOuputable  alts)




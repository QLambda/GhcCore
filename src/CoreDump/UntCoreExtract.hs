module CoreDump.UntCoreExtract where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import CoreDump.SimpleCore
import CoreDump.TypeToStr

import CoreDump.UntCore

import CoreDump.SimpleCoreExtract (exprTopSExpr, getModuleName)


{-
 Module extraction section
 data SExpr
  = SVar   String String          -- SVar Name Type
  | SLit   String                 -- Literal String_Representation
  | SApp   SExpr  SExpr           -- SApp e1 e2
  | SLam   String String SExpr    -- SLam variable type exp
  | SLet   UTBinder SExpr        -- ULet Binder e
  | SCase  SExpr [SAlt]           -- SCase e [Alternatives]
  | Skip String                   -- Skip is for string representation of not done yet expresions
-}

sExprToUExp (SVar id t cat) = UVar id
sExprToUExp (SLit literal) = ULit literal
sExprToUExp (SApp expr arg) = UApp (sExprToUExp expr) (sExprToUExp arg)

sExprToUExp (SLam var t expr) = ULam varName (sExprToUExp expr)
                                where
                                  varName = show (sExprToUExp var)
sExprToUExp (Skip str) = USkip str


-- sExprToUExp (Let binder expr) = UTLet (getUTBinder binder) (sExprToUExp expr)
-- sExprToUExp (Cast expr coersion) = Skip $ "SCast("++  show (sExprToUExp expr) ++ "~" ++ showOuputable coersion ++ ")"
-- sExprToUExp (STick _ expr) = Skip $ "STick("++  show (sExprToUExp expr) ++ ")"
-- sExprToUExp (Type t) = Skip $ "@" ++ showPprUnsafe t
-- sExprToUExp (SCoercion c) = Skip $ "~ " ++ showPprUnsafe c
-- sExprToUExp (Case exp1 b t alts) = Skip $ "Case  (" ++ show (sExprToUExp exp1) ++ ") of " ++ (show $ map showOuputable  alts)




getUTBinder::CoreBind -> UBinder
getUTBinder (NonRec name expr) = UNonRec (getOccString name) $ (sExprToUExp.exprTopSExpr)  expr 
getUTBinder (Rec exprs) =  URec (unpack exprs)
                            where
                                unpack [] = []
                                unpack ((name, expr):es) = (getOccString name, (sExprToUExp.exprTopSExpr)  expr) : unpack es


getUBinders::CoreProgram -> UBinders
getUBinders [] = UBinders []
getUBinders (cb:cps) = UBinders $ getUTBinder cb:gBinders (getUBinders cps)


getUTBinders::CoreProgram -> UBinders
getUTBinders [] = UBinders []
getUTBinders (cb:cps) = UBinders $ getUTBinder cb:gBinders  (getUTBinders cps)


coreToUCProgram::ModGuts -> UntypedCoreModule
coreToUCProgram mod = UntypedCoreModule (getModuleName $ mg_module mod) (getUBinders $ mg_binds mod)   --TODO process the binders



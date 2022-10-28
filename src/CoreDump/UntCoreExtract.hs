{-# LANGUAGE BlockArguments #-}
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

sExprToUExp (SVar id t cat) = UVar id                                    -- erase (var) = var
sExprToUExp (SLit literal) = ULit literal                                -- erase (lit) = lit
sExprToUExp (SApp expr arg) = case arg of 
                                SVar id t ID  -> UApp (sExprToUExp expr) (sExprToUExp arg)       -- erase (t1 var) = (erase t1) (erase var)
                                SVar id t typ -> sExprToUExp expr                                -- erase (t1 Type) = (erase t1) 
                                SType _       -> sExprToUExp expr                                -- erase (t1 Type) = (erase t1) 
                                ex            ->  UApp (sExprToUExp expr) (sExprToUExp arg)      -- erase (t1 t2) = (erase t1) (erase t2)
sExprToUExp (SLam var t expr) = case var of
                                   (SVar id t cat) ->  case cat of
                                                          TyVar -> sExprToUExp expr              -- erase (\x:* -> exp) = erase exp
                                                          TcTyVar -> sExprToUExp expr            -- erase (\x:T a -> exp) = erase exp
                                                          ID -> ULam id (sExprToUExp expr)       -- erase (\x -> exp) =  \x ->  erase exp

sExprToUExp (SLet binder expr) = ULet ubinder $ sExprToUExp expr  
                                  where 
                                    ubinder = binderToUbinder binder
                                    binderToUbinder (SNonRec var expr) =  UNonRec var (sExprToUExp expr)
                                    binderToUbinder (SRec exprs) =  URec $ map (\(s,e) -> (s, sExprToUExp e)) exprs


sExprToUExp (SCase expr expr2 alts) = UCase (sExprToUExp expr) (sExprToUExp expr2) ualts
                                    where
                                            ualts = map (\(SAlt altcon bs expr) -> UAlt (mapAlts altcon) (bssT bs) $ sExprToUExp expr) alts
                                            mapAlts (SDataAlt const) = UDataAlt const
                                            mapAlts (SLitAlt  lit) = ULitAlt lit
                                            mapAlts SDEFAULT = UDEFAULT 
                                            bssT = map sExprToUExp 


sExprToUExp (Skip str) = USkip (str++"SSSS!!!!")
sExprToUExp a = USkip $ show a ++ "!!!!!!"


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



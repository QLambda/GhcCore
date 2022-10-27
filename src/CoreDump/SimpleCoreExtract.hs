module CoreDump.SimpleCoreExtract where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import CoreDump.SimpleCore
import CoreDump.TypeToStr



-- Get actual name from Var and others that have OCC names
-- getOccString = occNameString . nameOccName . getName 

{-
 Module extraction section
-}
getModuleName::Module -> (String, ModuleName)
getModuleName m = (show $ moduleUnit m, moduleName m)


showOuputable out = renderWithContext defaultSDocContext (ppr out)


-- typeToStr::Type -> String
-- typeToStr  = showOuputable 

extractVar::Var->(String, String, VarCategory) -- returns (variableName, type)
extractVar v | isTyVar v = (getOccString v, "*", TyVar)
             | isTcTyVar v  = (getOccString v, typeToStr ( varType  v), TcTyVar)
             | isId  v   = (getOccString v, typeToStr ( varType v), ID)




------------------------------
exprTopSExpr :: Expr Var -> SExpr
exprTopSExpr (Var id) = SVar n t cat
                            where
                                (n, t, cat)=extractVar id
exprTopSExpr (Lit literal) = SLit $ showOuputable literal
exprTopSExpr (App expr args) = SApp (exprTopSExpr expr) (exprTopSExpr args)
exprTopSExpr (Lam name expr) = SLam (SVar n t cat) t (exprTopSExpr expr)
                                    where
                                         (n, t, cat)=extractVar name
exprTopSExpr (Let binder expr) = SLet (getSBinder binder) (exprTopSExpr expr)
exprTopSExpr (Cast expr coersion) = Skip $ "SCast("++  show (exprTopSExpr expr) ++ "~" ++ showOuputable coersion ++ ")"
exprTopSExpr (Tick _ expr) = Skip $ "STick("++  show (exprTopSExpr expr) ++ ")"
exprTopSExpr (Type t) = SType $ showPprUnsafe t
exprTopSExpr (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
exprTopSExpr (Case exp1 b t alts) = Skip $ "Case  (" ++ show (exprTopSExpr exp1) ++ ") of " ++ show ( map showOuputable  alts)
--exprTopSExpr expr = Skip "TODO"
---------------------------


getSBinder::CoreBind -> SBinder
getSBinder (NonRec name expr) = SNonRec (getOccString name) (exprTopSExpr  expr) 
getSBinder (Rec exprs) =  SRec (unpack exprs)
                            where
                                unpack [] = []
                                unpack ((name, expr):es) = (getOccString name, exprTopSExpr  expr) : unpack es


getSBinders::CoreProgram -> SBinders
getSBinders [] = SBinders []
getSBinders (cb:cps) = SBinders $ getSBinder cb:getBinders (getSBinders cps)





coreToCProgram::ModGuts -> SimpleCoreModule
coreToCProgram mod = SimpleCoreModule (getModuleName $ mg_module mod) (getSBinders $ mg_binds mod)   --TODO process the binders



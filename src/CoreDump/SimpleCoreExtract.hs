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

extractVar::Var->(String, String) -- returns (variableName, type)
extractVar v | isTyVar v = (getOccString v , "*")
             | isTcTyVar v  = (getOccString v , "Tc(" ++ typeToStr ( varType  v)++")")
             | isId  v   = (getOccString v , typeToStr ( varType v))

isKindVar::Var -> Bool
isKindVar v =  isTyVar v || isTcTyVar v

isType::Expr b -> Bool
isType (Var id) = isTyVar id || isTcTyVar id
isType (Type t) = True
isType  x = False



------------------------------
exprTopSExpr :: Expr Var -> SExpr
exprTopSExpr (Var id) = SVar n t
                            where
                                (n, t)=extractVar id
exprTopSExpr (Lit literal) = SLit $ showOuputable literal
exprTopSExpr (App expr args) = SApp (exprTopSExpr expr) (exprTopSExpr args)
exprTopSExpr (Lam name expr) = SLam n t (exprTopSExpr expr)
                                    where
                                         (n, t)=extractVar name
exprTopSExpr (Let binder expr) = SLet (getUTBinder binder) (exprTopSExpr expr)
exprTopSExpr (Cast expr coersion) = Skip $ "SCast("++  show (exprTopSExpr expr) ++ "~" ++ showOuputable coersion ++ ")"
exprTopSExpr (Tick _ expr) = Skip $ "STick("++  show (exprTopSExpr expr) ++ ")"
exprTopSExpr (Type t) = Skip $ "@" ++ showPprUnsafe t
exprTopSExpr (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
exprTopSExpr (Case exp1 b t alts) = Skip $ "Case  (" ++ show (exprTopSExpr exp1) ++ ") of " ++ show ( map showOuputable  alts)
--exprTopSExpr expr = Skip "TODO"
---------------------------

-- exprTopSExprNoTyped (Var id) = SVar n ""
--                             where
--                                 (n, t)=extractVar id
-- exprTopSExprNoTyped (Lit literal) = SLit $ showOuputable literal
-- exprTopSExprNoTyped (App expr arg) = if isType arg then expr1 else SApp expr1 utarg
--                                         where
--                                             expr1 = exprTopSExprNoTyped expr
--                                             utarg = exprTopSExprNoTyped arg

-- exprTopSExprNoTyped (Lam name expr) = if isKindVar name then SExpr else SLam n "" SExpr
--                                         where
--                                             SExpr = exprTopSExprNoTyped expr
--                                             (n, t)=extractVar name

-- exprTopSExprNoTyped (Let binder expr) = UTLet (getUTBinder binder) (exprTopSExprNoTyped expr)
-- exprTopSExprNoTyped (Cast expr coersion) = Skip $ "SCast("++  show (exprTopSExprNoTyped expr) ++ "~" ++ showOuputable coersion ++ ")"
-- exprTopSExprNoTyped (STick _ expr) = Skip $ "STick("++  show (exprTopSExprNoTyped expr) ++ ")"
-- exprTopSExprNoTyped (Type t) = Skip $ "@" ++ showPprUnsafe t
-- exprTopSExprNoTyped (SCoercion c) = Skip $ "~ " ++ showPprUnsafe c
-- exprTopSExprNoTyped (Case exp1 b t alts) = Skip $ "Case  (" ++ show (exprTopSExprNoTyped exp1) ++ ") of " ++ (show $ map showOuputable  alts)



getUTBinder::CoreBind -> UTBinder
getUTBinder (NonRec name expr) = SNonRec (getOccString name) (exprTopSExpr  expr) 
getUTBinder (Rec exprs) =  SRec (unpack exprs)
                            where
                                unpack [] = []
                                unpack ((name, expr):es) = (getOccString name, exprTopSExpr  expr) : unpack es


getSBinders::CoreProgram -> SBinders
getSBinders [] = SBinders []
getSBinders (cb:cps) = SBinders $ getUTBinder cb:getBinders (getSBinders cps)





coreToCProgram::ModGuts -> SimpleCoreModule
coreToCProgram mod = SimpleCoreModule (getModuleName $ mg_module mod) (getSBinders $ mg_binds mod)   --TODO process the binders



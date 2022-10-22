module CoreDump.CoreExtract where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import CoreDump.UntypedCore
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
extractVar v | isTyVar v = (getOccString v , "::*")
             | isTcTyVar v  = (getOccString v , "::Tc(" ++ typeToStr ( varType  v)++")")
             | isId  v   = (getOccString v , "::" ++ typeToStr ( varType v)++")")

isKindVar::Var -> Bool
isKindVar v =  isTyVar v || isTcTyVar v

isType::Expr b -> Bool
isType (Var id) = isTyVar id || isTcTyVar id
isType (Type t) = True
isType  x = False



------------------------------
exprTopUTExpr (Var id) = UTVar n t
                            where
                                (n, t)=extractVar id
exprTopUTExpr (Lit literal) = UTLit $ showOuputable literal
exprTopUTExpr (App expr args) = UTApp (exprTopUTExpr expr) (exprTopUTExpr args)
exprTopUTExpr (Lam name expr) = UTLam n t (exprTopUTExpr expr)
                                    where
                                         (n, t)=extractVar name
exprTopUTExpr (Let binder expr) = UTLet (getUTBinder binder) (exprTopUTExpr expr)
exprTopUTExpr (Cast expr coersion) = Skip $ "UTCast("++  show (exprTopUTExpr expr) ++ "~" ++ showOuputable coersion ++ ")"
exprTopUTExpr (Tick _ expr) = Skip $ "Tick("++  show (exprTopUTExpr expr) ++ ")"
exprTopUTExpr (Type t) = Skip $ "@" ++ showPprUnsafe t
exprTopUTExpr (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
exprTopUTExpr (Case exp1 b t alts) = Skip $ "Case  (" ++ show (exprTopUTExpr exp1) ++ ") of " ++ show ( map showOuputable  alts)
--exprTopUTExpr expr = Skip "TODO"
---------------------------

-- exprTopUTExprNoTyped (Var id) = UTVar n ""
--                             where
--                                 (n, t)=extractVar id
-- exprTopUTExprNoTyped (Lit literal) = UTLit $ showOuputable literal
-- exprTopUTExprNoTyped (App expr arg) = if isType arg then expr1 else UTApp expr1 utarg
--                                         where
--                                             expr1 = exprTopUTExprNoTyped expr
--                                             utarg = exprTopUTExprNoTyped arg

-- exprTopUTExprNoTyped (Lam name expr) = if isKindVar name then utexpr else UTLam n "" utexpr
--                                         where
--                                             utexpr = exprTopUTExprNoTyped expr
--                                             (n, t)=extractVar name

-- exprTopUTExprNoTyped (Let binder expr) = UTLet (getUTBinder binder) (exprTopUTExprNoTyped expr)
-- exprTopUTExprNoTyped (Cast expr coersion) = Skip $ "UTCast("++  show (exprTopUTExprNoTyped expr) ++ "~" ++ showOuputable coersion ++ ")"
-- exprTopUTExprNoTyped (Tick _ expr) = Skip $ "Tick("++  show (exprTopUTExprNoTyped expr) ++ ")"
-- exprTopUTExprNoTyped (Type t) = Skip $ "@" ++ showPprUnsafe t
-- exprTopUTExprNoTyped (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
-- exprTopUTExprNoTyped (Case exp1 b t alts) = Skip $ "Case  (" ++ show (exprTopUTExprNoTyped exp1) ++ ") of " ++ (show $ map showOuputable  alts)



getUTBinder::CoreBind -> UTBinder
getUTBinder (NonRec name expr) = UTNonRec (getOccString name) (exprTopUTExpr  expr) 
getUTBinder (Rec exprs) =  UTRec (unpack exprs)
                            where
                                unpack [] = []
                                unpack ((name, expr):es) = (getOccString name, exprTopUTExpr  expr) : unpack es


getUTBinders::CoreProgram -> UTBinders
getUTBinders [] = UTBinders []
getUTBinders (cb:cps) = UTBinders $ getUTBinder cb:getBinders (getUTBinders cps)





coreToCProgram::ModGuts -> UntypedCoreModule
coreToCProgram mod = UntypedCoreModule (getModuleName $ mg_module mod) (getUTBinders $ mg_binds mod)   --TODO process the binders



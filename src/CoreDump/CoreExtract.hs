module CoreDump.CoreExtract where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import CoreDump.UntypedCore
-- import CoreDump.TypeToStr



-- Get actual name from Var and others that have OCC names
-- getOccString = occNameString . nameOccName . getName 

{-
 Module extraction section
-}
getModuleName::Module -> (String, ModuleName)
getModuleName m = (show $ moduleUnit m, moduleName m)


showOuputable out = renderWithContext defaultSDocContext (ppr out)

isType::Expr b -> Bool
isType (Type _) = True
isType (Var id) = not $ isId id--isTyVar id
isType e = False



typeErasure::Expr b -> Expr b
typeErasure (Var id) = Var id
typeErasure (Lit literal) = Lit literal
typeErasure (Lam name expr) = Lam name $ typeErasure expr
typeErasure (App expr arg) = if isType arg then  typeErasure expr else App (typeErasure expr) (typeErasure arg) 
typeErasure (Cast expr coersion) = Cast (typeErasure expr) coersion
typeErasure (Tick t expr) = Tick t $ typeErasure expr
typeErasure (Coercion c) = Coercion c


-- Lam b (Expr b)	 
-- Let (Bind b) (Expr b)	 
-- Case (Expr b) b Type [Alt b]	 
-- Cast (Expr b) CoercionR	 
-- Tick CoreTickish (Expr b)	 
-- Type Type	 
-- Coercion Coercion

typeToStr::Type -> String
typeToStr  = showOuputable 

extractVar::Var->String
extractVar v | isTyVar v = getOccString v ++ "::kind{" ++ typeToStr ( varType v)++"}"
             | isTcTyVar  v  = getOccString v ++ "::kind{" ++ typeToStr ( varType  v)++"}"
             | isId  v   = getOccString v ++ "::type{" ++ typeToStr ( varType v)++"}"




exprTopUTExpr (Var id) = UTVar (getOccString id)  ""
exprTopUTExpr (Lit literal) = UTLit $ showOuputable literal
exprTopUTExpr (App expr args) = UTApp (exprTopUTExpr expr) (exprTopUTExpr args)
exprTopUTExpr (Lam name expr) = UTLam (extractVar name) (exprTopUTExpr expr)
exprTopUTExpr (Let binder expr) = UTLet (getUTBinder binder) (exprTopUTExpr expr)
exprTopUTExpr (Cast expr coersion) = Skip $ "UTCast("++  show (exprTopUTExpr expr) ++ "~" ++ showOuputable coersion ++ ")"
exprTopUTExpr (Tick _ expr) = Skip $ "Tick("++  show (exprTopUTExpr expr) ++ ")"
exprTopUTExpr (Type t) = Skip $ "@" ++ showPprUnsafe t
exprTopUTExpr (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
exprTopUTExpr (Case exp1 b t alts) = Skip $ "Case  (" ++ show (exprTopUTExpr exp1) ++ ") of  " ++ (show $ map showOuputable  alts)
--exprTopUTExpr expr = Skip "TODO"


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



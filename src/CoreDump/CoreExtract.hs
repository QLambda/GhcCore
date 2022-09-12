module CoreDump.CoreExtract where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import CoreDump.UntypedCore



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


exprTopUTExpr (Var id) = UTVar (getOccString id)  (showPprUnsafe id)
exprTopUTExpr (Lit literal) = UTLit $ showOuputable literal
exprTopUTExpr (Lam name expr) = UTLam (getOccString name) (exprTopUTExpr expr)
exprTopUTExpr (App expr args) = UTApp (exprTopUTExpr expr) (exprTopUTExpr args)
exprTopUTExpr (Cast expr coersion) = Skip $ "UTCast("++  show (exprTopUTExpr expr) ++ ")"
exprTopUTExpr (Tick _ expr) = Skip $ "Tick("++  show (exprTopUTExpr expr) ++ ")"
exprTopUTExpr (Type t) = Skip $ "::" ++ showPprUnsafe t
exprTopUTExpr (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
exprTopUTExpr expr = Skip "TODO"

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



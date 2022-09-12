module CoreDump.CoreExtract where
import GHC.Plugins
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

-- data Bind b = 
--             | Rec [(b, (Expr b))]
--   deriving Data


exprTopUTExpr (Var id) = UTVar $ getOccString id
exprTopUTExpr (Lit literal) = UTLit $ showOuputable literal
exprTopUTExpr (Lam name expr) = UTLam (getOccString name) (exprTopUTExpr expr)
exprTopUTExpr (App expr args) = UTApp (exprTopUTExpr expr) (exprTopUTExpr args)



exprTopUTExpr expr = Skip --TODO

getUTBinder::CoreBind -> UTBinder
getUTBinder (NonRec name expr) = UTNonRec (getOccString name) (exprTopUTExpr expr) 
getUTBinder (Rec exprs) =  UTRec (unpack exprs)
                            where
                                unpack [] = []
                                unpack ((name, expr):es) = (getOccString name, exprTopUTExpr expr) : (unpack es)


getUTBinders::CoreProgram -> UTBinders
getUTBinders [] = UTBinders []
getUTBinders (cb:cps) = UTBinders $ (getUTBinder cb):(getBinders (getUTBinders cps))





coreToCProgram::ModGuts -> UntypedCoreModule
coreToCProgram mod = UntypedCoreModule (getModuleName $ mg_module mod) (getUTBinders $ mg_binds mod)   --TODO process the binders



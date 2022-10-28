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

extractVar::Var->SExpr-- returns (variableName, type, category)
extractVar v | isTyVar v = SVar (getOccString v)  "*"  TyVar
             | isTcTyVar v  = SVar (getOccString v) (typeToStr ( varType  v)) TcTyVar
             | isId  v   = SVar (getOccString v) (typeToStr ( varType v)) ID




------------------------------
exprTopSExpr :: Expr Var -> SExpr
exprTopSExpr (Var id) = extractVar id
exprTopSExpr (Lit literal) = SLit $ showOuputable literal
exprTopSExpr (App expr args) = SApp (exprTopSExpr expr) (exprTopSExpr args)
exprTopSExpr (Lam name expr) = SLam var (typ var) (exprTopSExpr expr)
                                    where
                                         var=extractVar name

exprTopSExpr (Let binder expr) = SLet (getSBinder binder) (exprTopSExpr expr)
exprTopSExpr (Cast expr coersion) = Skip $ "SCast("++  show (exprTopSExpr expr) ++ "~" ++ showOuputable coersion ++ ")"
exprTopSExpr (Tick _ expr) = Skip $ "STick("++  show (exprTopSExpr expr) ++ ")"
exprTopSExpr (Type t) = SType $ showPprUnsafe t
exprTopSExpr (Coercion c) = Skip $ "~ " ++ showPprUnsafe c
exprTopSExpr (Case exp1 b t alts) = SCase (exprTopSExpr exp1) bs salts
                                        where
                                            salts = map (\(Alt altcon bss expr) -> SAlt (mapAlts altcon) (args bss) $ exprTopSExpr expr) alts
                                            mapAlts (DataAlt const) = SDataAlt $ getOccString const
                                            mapAlts (LitAlt  lit) = SLitAlt $ showOuputable lit
                                            mapAlts DEFAULT = SDEFAULT 
                                            args = map extractVar
                                            bs = extractVar b
                        

-- data SAlt = SAlt SAltCon [String] SExpr
--                     deriving Show

-- data SAltCon
--   = SDataAlt String   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
--                        -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@
--                       -- Invariant: always an *unlifted* literal
--                       -- See Note [Literal alternatives]
--   | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
--    deriving (Eq, Show)
    
 --   Skip $ "Case  (" ++ show (exprTopSExpr exp1) ++ ") of " ++ show ( map showOuputable  alts)
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



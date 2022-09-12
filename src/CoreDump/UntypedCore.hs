module CoreDump.UntypedCore where
import GHC.Plugins
import GHC.Unit.Types
import GHC.Unit.Module.Name



-- UntypedCoreProgram is the representation of a module in untyped core language
data UntypedCoreModule = UntypedCoreModule { 
                                mod :: (String, ModuleName),   -- Module unit,name
                                binders :: UTBinders            -- Binders
                          }      

data UTBinders = UTBinders {getBinders::[UTBinder]}
data UTBinder = UTNonRec String UTExpr | UTRec [(String, UTExpr)]



data UTExpr
  = UTVar   String
  | UTLit   String
  | UTApp   UTExpr  UTExpr
  | UTLam   String UTExpr 
  | UTLet   UTBinder UTExpr
  | UTCase  UTExpr [UTAlt]  
  | Skip
  -- | UTCast  UTExpr UTCoercionR 
  -- | Tick  CoreTickish (Expr b)
  -- | Type  Type
  -- | Coercion UTCoercion
  deriving Show

data UTAlt = UTAlt UTAltCon [String] (UTExpr)
                    deriving Show


data UTAltCon
  = UTDataAlt String   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@
  | UTLitAlt  String  -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]
  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq, Show)



{-
    Show Section
-}                        

instance Show UTBinder where
    show (UTNonRec name expr) = name ++"="++ show expr
    show (UTRec binders) = "\nREC {\n" ++ shollAll binders ++ "\n}\n\n"
                            where 
                                shollAll [] = ""
                                shollAll ((name, expr):bs) = "name=" ++ show expr ++ "\n" ++ shollAll bs

instance Show UTBinders where
    show (UTBinders [])    = ""
    show (UTBinders (b:bs)) = show b ++ "\n-----\n" ++ show bs


instance Show UntypedCoreModule where
    show (UntypedCoreModule (unit, mname) binders) = "module "++  (moduleNameString mname) ++ " (Unit:" ++ unit ++ ")" ++ "\n\n" 
                                                     ++ (show binders)





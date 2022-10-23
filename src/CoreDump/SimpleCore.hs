module CoreDump.SimpleCore where
import GHC.Plugins
import GHC.Unit.Types
import GHC.Unit.Module.Name



-- UntypedCoreProgram is the representation of a module in untyped core language
data UntypedCoreModule = UntypedCoreModule { 
                                mod :: (String, ModuleName),   -- Module unit,name
                                binders :: UTBinders            -- Binders
                          }      

newtype UTBinders = UTBinders {getBinders::[UTBinder]}
data UTBinder = UTNonRec String SExpr | UTRec [(String, SExpr)]



data SExpr
  = SVar   String String          -- varName type
  | SLit   String
  | SApp   SExpr  SExpr
  | SLam   String String SExpr   -- varName type exp
  | UTLet   UTBinder SExpr
  | SCase  SExpr [SAlt]  
  | Skip String
  -- | SCast  SExpr UTSCoercionR 
  -- | STick  CoreSTickish (Expr b)
  -- | Type  Type
  -- | SCoercion UTSCoercion
  

data SAlt = SAlt SAltCon [String] SExpr
                    deriving Show



data SAltCon
  = UTDataAlt String   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@
  | SLitAlt  String  -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]
  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq, Show)


instance Show SExpr where
    show (SVar var t)   =  var++t
    show (SLit literal) = "Literal("++ literal ++ ")"
    show (SApp e1 e2)   =  "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (SLam var t  e)   = "{\\" ++ var ++ t ++ " -> " ++ show e ++ "}"
    show (UTLet  b e2)   = "let  ("++ show b ++ ") in" ++ show e2
    show (SCase e alts) = "case" ++ show e ++ " of \n     " ++ show alts
    show (Skip s)        = s



{-
    Show Section
-}                        

instance Show UTBinder where
    show (UTNonRec name expr) = name ++"="++ show expr
    show (UTRec binders) = "\nREC {\n" ++ shollAll binders ++ "\n}\n\n"
                            where 
                                shollAll [] = ""
                                shollAll ((name, expr):bs) = name ++ " = " ++ show expr ++ "\n" ++ shollAll bs

instance Show UTBinders where
    show = showB
        where
            showB (UTBinders [])    = ""
            showB (UTBinders (b:bs)) = show b ++ "\n" ++ showB (UTBinders bs)


instance Show UntypedCoreModule where
    show (UntypedCoreModule (unit, mname) binders) = "module "++  moduleNameString mname ++ " (Unit:" ++ unit ++ ")" ++ "\n\n" 
                                                     ++ show binders





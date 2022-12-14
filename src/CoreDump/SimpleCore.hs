module CoreDump.SimpleCore where
import GHC.Plugins
import GHC.Unit.Types
import GHC.Unit.Module.Name



-- UntypedCoreProgram is the representation of a module in untyped core language
data SimpleCoreModule = SimpleCoreModule { 
                                mod :: (String, ModuleName),   -- Module unit,name
                                binders :: SBinders            -- Binders
                          }      

newtype SBinders = SBinders {getBinders::[SBinder]}
data SBinder = SNonRec String SExpr | SRec [(String, SExpr)]

data VarCategory = TyVar | TcTyVar | ID deriving Show

type VarDescription = (String, String, VarCategory)

data SExpr
  = SVar   {name::String, typ::String, cat::VarCategory}         -- SVar Name Type VariableCategory
  | SLit   String                 -- Literal String_Representation
  | SApp   SExpr  SExpr           -- SApp e1 e2
  | SLam   SExpr String SExpr    -- SLam variable type exp
  | SLet   SBinder SExpr          -- ULet Binder e
  | SCase  SExpr SExpr [SAlt]           -- SCase e b [Alternatives]
  | Skip   String                   -- Skip is for string representation of not done yet expresions
  -- | SCast  SExpr UTSCoercionR 
  -- | STick  CoreSTickish (Expr b)
  | SType  String
  -- | SCoercion UTSCoercion
  

data SAlt = SAlt SAltCon [SExpr] SExpr    -- [SExpr] are array of Variables
                    deriving Show



data SAltCon
  = SLitAlt String
  | SDataAlt String   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                       -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]
  | SDEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq, Show)


instance Show SExpr where
    show (SVar var t cat)  =  show cat ++ "<" ++ var++"::" ++t ++">"
    show (SLit literal)    = "Literal("++ literal ++ ")"
    show (SApp e1 e2)      =  "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (SLam var t  e)   = "(\\" ++ show var ++ " -> " ++ show e ++ ")"
    show (SLet  b e2)    = "let  ("++ show b ++ ") in" ++ show e2
    show (SCase e s alts)  = "case " ++ show e ++ "  (" ++ show s ++ ") of \n     " ++ show alts
    show (SType s)       = s
    show (Skip s)        = "##" ++ s ++ "##"



{-
    Show Section
-}                        
instance Show SBinder where
    show (SNonRec name expr) = name ++"="++ show expr
    show (SRec binders) = "\nREC {\n" ++ shollAll binders ++ "\n}\n\n"
                            where 
                                shollAll [] = ""
                                shollAll ((name, expr):bs) = name ++ " = " ++ show expr ++ "\n" ++ shollAll bs

instance Show SBinders where
    show = showB
        where
            showB (SBinders [])    = ""
            showB (SBinders (b:bs)) = show b ++ "\n" ++ showB (SBinders bs)


instance Show SimpleCoreModule where
    show (SimpleCoreModule (unit, mname) binders) = "module "++  moduleNameString mname ++ " (Unit:" ++ unit ++ ")" ++ "\n\n" 
                                                     ++ show binders





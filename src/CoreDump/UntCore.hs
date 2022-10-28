module CoreDump.UntCore where
import GHC.Plugins
import GHC.Unit.Types
import GHC.Unit.Module.Name



-- UntypedCoreProgram is the representation of a module in untyped core language
data UntypedCoreModule = UntypedCoreModule { 
                                mod :: (String, ModuleName),   -- Module unit,name
                                binders :: UBinders            -- Binders
                          }      

newtype UBinders = UBinders {gBinders::[UBinder]}
data UBinder = UNonRec String UExpr | URec [(String, UExpr)]



data UExpr
  = UVar   String               -- SVar Name Type
  | ULit   String               -- Literal String_Representation
  | UApp   UExpr  UExpr         -- SApp e1 e2
  | ULam   String UExpr         -- SLam variable  exp
  | ULet   UBinder UExpr        -- ULet Binder e
  | UCase  UExpr UExpr [UAlt]         -- SCase e [Alternatives]
  | USkip String                -- Skip is for string representation of not done yet expresions
  -- | SCast  SExpr UTSCoercionR 
  -- | STick  CoreSTickish (Expr b)
  -- | Type  Type
  -- | SCoercion UTSCoercion
  

data UAlt = UAlt UAltCon [UExpr] UExpr



data UAltCon
  = ULitAlt String
  | UDataAlt String   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                       -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]
  | UDEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq)


instance Show UExpr where
    show (UVar var)   =  var
    show (ULit literal) = "Literal("++ literal ++ ")"
    show (UApp e1 e2)   =  "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (ULam var   e)   = "(\\" ++ var ++ " -> " ++ show e ++ ")"
    show (ULet  b e2)   = "let "++ show b ++ " in " ++ show e2
    show (UCase e e2 alts) = "case " ++ show e ++ " ("++ show e2++")  of \n     " ++ show alts
    show (USkip s)        =  "##" ++ s ++ "##"



{-
    Show Section
-}                        

instance Show UAlt where
    show (UAlt con ss uexp) = show con ++ " " ++ unwords (map show ss) ++"  -> "++ show uexp

instance Show UAltCon where
    show (ULitAlt s)  = s
    show (UDataAlt s) = s
    show UDEFAULT     = "_"

    

instance Show UBinder where
    show (UNonRec name expr) = name ++" = "++ show expr
    show (URec binders) = "REC {" ++ shollAll binders ++ "\n}"
                            where 
                                shollAll [] = ""
                                shollAll ((name, expr):bs) = "\n"++name ++ " = " ++ show expr ++ shollAll bs

instance Show UBinders where
    show = showB
        where
            showB (UBinders [])    = ""
            showB (UBinders (b:bs)) = show b ++ "\n\n" ++ showB (UBinders bs)


instance Show UntypedCoreModule where
    show (UntypedCoreModule (unit, mname) binders) = "module "++  moduleNameString mname ++ " (Unit:" ++ unit ++ ")" ++ "\n\n" 
                                                     ++ show binders





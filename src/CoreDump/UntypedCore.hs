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


data UTExpr = Skip
                    deriving Show


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





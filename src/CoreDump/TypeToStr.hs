module CoreDump.TypeToStr where
import GHC.Plugins
import GHC.Types.Var (Var(..))
import GHC.Unit.Types
import GHC.Unit.Module.Name
import GHC.Core.TyCo.Rep
import GHC.Utils.Outputable (renderWithContext)



_showOuputable out = renderWithContext defaultSDocContext (ppr out)


typeToStr::Type->String
typeToStr (TyVarTy v) = _showOuputable v
typeToStr (AppTy t1 t2) = typeToStr t1 ++ " " ++ typeToStr t2
typeToStr (TyConApp tcon apps) = _showOuputable tcon ++ show (map typeToStr apps)
typeToStr (ForAllTy b t) = "(forall "++ _showOuputable b ++ "." ++ typeToStr t ++ ")"
typeToStr x = "|TODO:"++ _showOuputable x ++"|"



-- data Type
--   -- See Note [Non-trivial definitional equality]
--   = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a SCoercion variable)

--   | AppTy
--         Type
--         Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
--                         --
--                         --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
--                         --     must be another 'AppTy', or 'TyVarTy'
--                         --     See Note [Respecting definitional equality] \(EQ1) about the
--                         --     no 'CastTy' requirement
--                         --
--                         --  2) Argument type

--   | TyConApp
--         TyCon
--         [KindOrType]    -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
--                         -- Invariant: saturated applications of 'FunTyCon' must
--                         -- use 'FunTy' and saturated synonyms must use their own
--                         -- constructors. However, /unsaturated/ 'FunTyCon's
--                         -- do appear as 'TyConApp's.
--                         -- Parameters:
--                         --
--                         -- 1) Type constructor being applied to.
--                         --
--                         -- 2) Type arguments. Might not have enough type arguments
--                         --    here to saturate the constructor.
--                         --    Even type synonyms are not necessarily saturated;
--                         --    for example unsaturated type synonyms
--                         --    can appear as the right hand side of a type synonym.

--   | ForAllTy
--         {-# UNPACK #-} !TyCoVarBinder
--         Type            -- ^ A Π type.
--              -- Note [When we quantify over a SCoercion variable]
--              -- INVARIANT: If the binder is a SCoercion variable, it must
--              -- be mentioned in the Type. See
--              -- Note [Unused SCoercion variable in ForAllTy]

--   | FunTy      -- ^ FUN m t1 t2   Very common, so an important special case
--                 -- See Note [Function types]
--      { ft_af  :: AnonArgFlag    -- Is this (->) or (=>)?
--      , ft_mult :: Mult          -- Multiplicity
--      , ft_arg :: Type           -- Argument type
--      , ft_res :: Type }         -- Result type

--   | LitTy TyLit     -- ^ Type literals are similar to type constructors.

--   | CastTy
--         Type
--         KindSCoercion  -- ^ A kind cast. The SCoercion is always nominal.
--                       -- INVARIANT: The cast is never reflexive \(EQ2)
--                       -- INVARIANT: The Type is not a CastTy (use TransCo instead) \(EQ3)
--                       -- INVARIANT: The Type is not a ForAllTy over a tyvar \(EQ4)
--                       -- See Note [Respecting definitional equality]

--   | SCoercionTy
--         SCoercion    -- ^ Injection of a SCoercion into a type
--                     -- This should only ever be used in the RHS of an AppTy,
--                     -- in the list of a TyConApp, when applying a promoted
--                     -- GADT data constructor

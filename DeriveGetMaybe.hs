module DeriveGetMaybe (makeGet) where

import Language.Haskell
import Data.Derive.Internal.Derivation


makeGet :: Derivation
makeGet = derivationCustom "Get" $ \(_,d) -> Right $ concatMap (makeGetCtor d) $ dataDeclCtors d


makeGetCtor :: DataDecl -> CtorDecl -> [Decl]
makeGetCtor d c = [ TypeSig sl [name from] typ,
                    FunBind $ match : [defMatch | length (dataDeclCtors d) > 1]]
    where
        n = ctorDeclName c
        from = "get" ++ n

        typ = TyFun (dataDeclType d) $
           TyApp (TyCon (UnQual (Ident "Maybe")))
           -- fromBangType safe? likely strips strictness annotations
           -- (tyTuple $ map (fromBangType . snd) $ ctorDeclFields c)
           (tyTuple $ map snd $ ctorDeclFields c)

        match = Match sl (name from) [pat] Nothing (UnGuardedRhs rhs) (BDecls [])
        pat = (length vars == 0 ? id $ PParen) $ PApp (qname n) (map pVar vars)
        vars = take (length $ ctorDeclFields c) $ map ((:) 'x' . show) [1..]
        rhs = App (Con (UnQual (Ident "Just"))) (valTuple $ map var vars)

        defMatch = Match sl (name from) [PWildCard] Nothing (UnGuardedRhs err) (BDecls [])
        err = Con $ UnQual $ Ident "Nothing"


tyTuple [] = TyCon $ Special UnitCon
tyTuple [x] = x
tyTuple xs = TyTuple Boxed xs


valTuple [] = Con $ Special UnitCon
valTuple [x] = x
valTuple xs = Tuple Boxed xs

---xl69, tianhui2
module Infer where

import Common

import Control.Monad.Writer (listen)
import Control.Monad.Except (throwError)
import Data.Map.Strict as H (Map, insert, lookup, empty, fromList, singleton)

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> Infer MonoTy
freshInst (Forall [] tau) = return tau
freshInst (Forall _ (TyVar tau)) = return (TyVar tau)
freshInst (Forall (x : xs) tau) = do t <- freshTau
                                     freshInst $ Forall xs (apply (substInit x t) tau)

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = case tau of TyConst _ [] -> False
                           TyConst arg (x:xs) ->
                             let const = TyConst arg xs
                             in let first = occurs i x
                                    second = occurs i const
                                in first || second
                           TyVar var -> i == var

  {- question 3: unification -}

unify :: [Constraint] -> Infer Substitution
unify [] = return substEmpty
unify ((s :~: t):xs) = aux s t
  where aux s t | s == t = unify xs
        aux s@(TyConst _ _) t@(TyVar _) = unify ((t :~: s):xs)
        aux s@(TyConst const1 sn) t@(TyConst const2 tn) = case const1 == const2 of 
                                                            True -> let ziplis = zipWith (:~:) sn tn
                                                                    in let union = xs ++ ziplis
                                                                        in unify union
                                                            False -> let nomatch = Can'tMatch s t
                                                                     in throwError nomatch
        aux s@(TyVar i) t | not (occurs i t) =  do let sub = substInit i t
                                                   let appsub = apply sub
                                                   let appmap = map appsub xs
                                                   sigma <- unify appmap
                                                   let appsig = apply sigma t
                                                   let substt = substInit i appsig
                                                   let compos = substCompose substt sigma
                                                   return compos
                          | otherwise = 
                              let infinit = InfiniteType i t
                              in throwError infinit 


  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> Infer MonoTy
infer env exp = case exp of 
  VarExp var -> case H.lookup var env of
    Just something -> freshInst something
    Nothing -> throwError $ LookupError var
  ConstExp const -> freshInst $ constTySig const
  LetExp arg env1 env2 -> do (tau, con) <- listen $ infer env env1
                             sub <- unify con
                             let appenv = apply sub env
                             let apptau = apply sub tau
                             let newtau = gen appenv apptau
                             let newenv = H.insert arg newtau appenv
                             infer newenv env2
  BinOpExp op env1 env2 -> do tau <- freshTau
                              let firinf = infer env env1
                              (atau, phi) <- listen firinf
                              sub <- unify phi
                              let secinf = infer env env2
                              (newtau, newphi) <- listen secinf
                              newsub <- unify newphi
                              let binop = binopTySig op
                              ins <- freshInst binop
                              let inner = funTy newtau tau
                              let outer = funTy atau inner
                              constrain outer ins
                              let comp = substCompose newsub sub
                              let answer = apply comp tau
                              return answer
  FunExp fun funenv -> do tau <- freshTau
                          newtau <- freshTau
                          let quant = quantifyMonoTy newtau
                          let ins = H.insert fun quant env
                          let inf = infer ins funenv
                          (tau1, phi) <- listen inf
                          sub1 <- unify phi
                          let func = funTy newtau tau1
                          constrain tau func
                          let answer = apply sub1 tau
                          return answer
  AppExp func exp -> do tau <- freshTau
                        let inffun  = infer env func
                        (tau1, phi1) <- listen inffun
                        sub1 <- unify phi1
                        let infexp = infer env exp
                        (tau2, phi2) <- listen infexp
                        sub2 <- unify phi2
                        let fun = funTy tau2 tau
                        constrain tau1 fun
                        let comp = substCompose sub2 sub1
                        let answer = apply comp tau 
                        return answer
  IfExp env1 env2 env3 -> do let firinf = infer env env1
                             let secinf = infer env env2
                             let thrinf = infer env env3
                             (tau1, phi1) <- listen firinf
                             sub1 <- unify phi1
                             (tau2, phi2) <- listen secinf
                             sub2 <- unify phi2
                             (tau3, phi3) <- listen thrinf
                             sub3 <- unify phi3
                             constrain tau1 boolTy
                             constrain tau2 tau3
                             let comp = substCompose sub3 sub2
                             let ncmp = substCompose comp sub1
                             let answer = apply ncmp tau2
                             return answer
  MonOpExp op menv -> do tau <- freshTau
                         let minf = infer env menv
                         (tau1, phi1) <- listen minf
                         sub <- unify phi1
                         let mono = monopTySig op
                         inst <- freshInst mono
                         let func = funTy tau1 tau
                         constrain func inst
                         let answer = apply sub tau
                         return answer
  LetRecExp fun x lenv exp -> do tau <- freshTau
                                 tau1 <- freshTau
                                 tau2 <- freshTau
                                 let ftype = funTy tau1 tau2
                                 let polytype = polyTyOfMonoTy ftype
                                 let polyenv = H.insert fun polytype env
                                 let mtype = polyTyOfMonoTy tau1
                                 let firinf = H.insert x mtype polyenv
                                 let ear = infer firinf lenv
                                 (tau3, phi1) <- listen ear
                                 let union = [tau2 :~: tau3] ++ phi1
                                 sub1 <- unify union
                                 let appenv = apply sub1 env
                                 let ftype2 = funTy tau1 tau2
                                 let apparg = apply sub1 ftype2
                                 let genEnv = gen appenv apparg
                                 let funenv = H.insert fun genEnv env
                                 let expear = infer funenv exp
                                 (tau, phi2) <- listen expear
                                 sub2 <- unify phi2
                                 let comp = substCompose sub2 sub1
                                 let answer = apply comp tau 
                                 return answer
                         
                                                            
inferInit :: TypeEnv -> Exp -> Infer PolyTy
inferInit env e = do
  (tau, constraints) <- listen $ infer env e
  substitution <- unify constraints
  return $ quantifyMonoTy $ apply substitution tau

inferDec :: TypeEnv -> Dec -> Infer (TypeEnv, PolyTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x tau env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f tau env, tau)
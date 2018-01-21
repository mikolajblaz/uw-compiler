module Llvm.Generator where

import Control.Monad ( unless )
import Control.Monad.Trans.State.Lazy
import Data.List ( partition )
import qualified Data.Map as Map

import AbsLatte

import Llvm.Core
import Llvm.State
import qualified Llvm.Emitter as Emitter
import qualified Llvm.StdLib as Lib


processProgram :: Program Pos -> GenM String
processProgram (Program _ topDefs) = do
  let (fnDefs, clsDefs) = partition isFnDef topDefs
  buildTopEnv (Lib.libraryTopDefs ++ topDefs)
  buildClassEnv clsDefs
  let libraryOutput = Lib.printLibraryDeclarations
  -- NOTE: we don't process libraryTopDefs, we just put it to topEnv
  classOuts <- mapM processClassDef clsDefs
  funOuts <- mapM processFnDef fnDefs

  constants <- gets sConsts >>= (return . Emitter.outputStringConstants)
  return $ unlines $ libraryOutput ++ constants ++ (concat (classOuts ++ funOuts))


processClassDef :: TopDef Pos -> GenM [Instr]
processClassDef (ClsDef _ (Ident name) attrs) = return $
  [Emitter.outputTypeDef name (map (\(AttrDef _ ty _) -> plainType ty) attrs)]

processClassDef _ = return []

-- | Return instructions in proper order
processFnDef :: TopDef Pos -> GenM [Instr]
processFnDef (FnDef _ ty ident args block) = do
  startNewFun ident ty
  -- outer env is now set

  -- from now, everything is emitted directly to state
  entry <- freshLabel
  setCurrentBlock entry
  argsAddrs <- processArgs args
  processBlock block

  -- by calling finishFunction, we gather emitted instructions from the state
  funBody <- finishFunction

  let funOut = Emitter.outputFunction (plainType ty) ident argsAddrs funBody
  return funOut

processFnDef _ = return []

processArgs :: [Arg Pos] -> GenM [Addr]
processArgs [] = return []
processArgs args = do
  Emitter.emitComment "copy argument values:"
  argAddrs <- mapM processArg args
  Emitter.emitEmptyLine
  return argAddrs

-- | Update environment
-- | Copy value from function argument
-- | I.e. store i32, i32 %arg_n, i32* %loc_n
processArg :: Arg Pos -> GenM Addr
processArg (Arg _ ty ident) = do
  locAddr@(ALoc ui ty) <- insertLocalDecl ident ty
  Emitter.emitAlloc locAddr

  let argAddr = AArg ui ty
  Emitter.emitStore argAddr locAddr
  return argAddr


-- return a flag indicating presence of a return instruction
processBlock :: Block Pos -> GenM Bool
processBlock (Block _ []) = return False
processBlock (Block _ stmts) = do
  oldBlockEnv <- gets blockEnv
  oldOuterEnv <- gets outerEnv
  let newOuterEnv = blockToOuterEnv oldBlockEnv oldOuterEnv
  setNewEnvs emptyEnv newOuterEnv
  -- Now, process block in an empty block environment
  endsRets <- mapM genStmtCommentWrapper stmts
  -- Set old environment back, discarding everything that was inside the block
  setNewEnvs oldBlockEnv oldOuterEnv

  -- if any instruction ends with ret, the block ends with ret also
  let endsRet = or endsRets
  return endsRet


finishFunction :: GenM [Instr]
finishFunction = do
  labelCnt <- gets labelCnt
  let funLabels = [0 .. labelCnt - 1]
  blocks <- gets simpleBlocks
  let funIstrs = map (Emitter.outputBlock blocks) funLabels
  return $ concat funIstrs

------------------------------ Generator part ----------------------------------
-- NOTE:
-- generator Invariant #1
-- when any 'gen*' function is called, there is some block started already
-- (stored in state in currentBlock)

-- NOTE:
-- generator Invariant #2
-- a function calling
--      l <- freshLabel
-- will eventually call
--      setCurrentBlock l

genStmtCommentWrapper :: Stmt Pos -> GenM Bool
genStmtCommentWrapper b@(BStmt _ _) = genStmt b
genStmtCommentWrapper stmt = do
  Emitter.emitCommentStmt stmt
  genStmt stmt

-- return a flag indicating presence of a return instruction
genStmt :: Stmt Pos -> GenM Bool
genStmt (BStmt _ b) = processBlock b

genStmt (Empty _) = return False

genStmt (SExp _ expr) = do
  _ <- genRhs expr
  return False

genStmt (Ass _ lhsExpr expr) = do
  rhsAddr <- genRhs expr
  lhsAddr <- genLhs lhsExpr
  Emitter.emitStore rhsAddr lhsAddr
  return False

genStmt (Cond _ cond thenStmt) = do
  thenLabel <- freshLabel
  afterLabel <- freshLabel
  genCond cond thenLabel afterLabel

  setCurrentBlock thenLabel
  endsRet <- genStmt thenStmt
  unless endsRet $ genJmp afterLabel

  setCurrentBlock afterLabel
  return False

genStmt (CondElse _ cond thenStmt elseStmt) = do
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  afterLabel <- freshLabel
  genCond cond thenLabel elseLabel

  setCurrentBlock thenLabel
  endsRetThen <- genStmt thenStmt
  unless endsRetThen $ genJmp afterLabel

  setCurrentBlock elseLabel
  endsRetElse <- genStmt elseStmt
  unless endsRetElse $ genJmp afterLabel

  let endsRet = endsRetThen && endsRetElse
  unless endsRet $ setCurrentBlock afterLabel
  return endsRet

genStmt (While _ cond bodyStmt) = do
  bodyLabel <- freshLabel
  condLabel <- freshLabel

  genJmp condLabel

  -- In Llvm it doesn't matter, but the order is as in an efficient assembler:
  -- First body, then condition
  setCurrentBlock bodyLabel
  endsRet <- genStmt bodyStmt
  unless endsRet $ genJmp condLabel

  setCurrentBlock condLabel
  case (cond, endsRet) of
    (ELitTrue _, True) -> genJmp bodyLabel >> return True
    (ELitTrue _, False) -> do
                            afterLabel <- freshLabel
                            genJmp bodyLabel
                            setCurrentBlock afterLabel
                            return False
    _ -> do
          afterLabel <- freshLabel
          genCond cond bodyLabel afterLabel
          setCurrentBlock afterLabel
          return False


genStmt (Ret _ expr) = do
  addr <- genRhs expr
  genRet addr
  return True

genStmt (VRet _) = genVRet >> return True

-- declarations
-- process single declaration
genStmt (Decl pos ty [NoInit pos2 ident]) = do
  genStmt (Decl pos ty [Init pos2 ident (defaultInit ty)])

genStmt (Decl _ ty [Init _ ident expr]) = do
  rhsAddr <- genRhs expr
  -- NOTE: here environment changes
  declAddr <- insertLocalDecl ident ty
  Emitter.emitAlloc declAddr
  lhsAddr <- genLhs (EVar Nothing ident)
  Emitter.emitStore rhsAddr lhsAddr
  return False

-- multiple declarations
genStmt (Decl pos ty items) = do
  let singleDecls = map (\item -> Decl pos ty [item]) items
  mapM_ genStmt singleDecls
  return False

-- these shouldn't happen after context analysis
genStmt (For pos _ _ _ _) = failPos pos $ "Compiler error"
genStmt (Incr pos _) = failPos pos $ "Compiler error"
genStmt (Decr pos _) = failPos pos $ "Compiler error"

------------------------- Expressions ---------------------------------------
genRhs :: Expr Pos -> GenM Addr
genRhs (EVar pos ident) = do
  (ty, _, addr) <- getIdentVal pos ident
  r <- freshRegister (plainType ty)
  Emitter.emitLoad addr r
  return r

genRhs (ELitInt _ int) = return $ AImm int TInt
genRhs (ELitTrue _) = return $ AImm 1 TBool
genRhs (ELitFalse _) = return $ AImm 0 TBool

genRhs (EApp pos ident exprs) = do
  addrs <- mapM genRhs exprs
  (_, _, funAddr) <- getIdentVal pos ident
  let funTy = getAddrType funAddr
  case funTy of
    TFun TVoid _ -> do
                      Emitter.emitVoidCall funAddr addrs
                      return undefined -- it will not be used anyway
    TFun retTy _ -> do
                      r <- freshRegister retTy
                      Emitter.emitCall r funAddr addrs
                      return r
    _ -> failPos pos $ "Compiler error" -- this shouldn't happen

genRhs (EString _ str) = do
  addr <- createStringConstant str
  r <- freshRegister TStr
  Emitter.emitConstToString r addr
  return r

genRhs (ENewArray pos ty sizeExpr) = do
  sAddr <- genRhs sizeExpr
  r <- freshRegister $ TPtr (plainType ty)
  Emitter.emitArrAlloc r sAddr
  return r

genRhs e@(EArrayAcc pos arrExpr iExpr) = do
  elemPtrAddr <- genLhs e
  let TPtr elemTy = getAddrType elemPtrAddr
  elemAddr <- freshRegister elemTy
  Emitter.emitArrLoad elemPtrAddr elemAddr
  return elemAddr

genRhs (EFieldAcc pos expr (EVar _ (Ident "length"))) = do
  return $ AImm 10 TInt -- TODO

genRhs (ENull pos ty) = return $ ANul (plainType ty)

genRhs (Neg p expr) = genBinOp "sub" (ELitInt p 0) expr
-- logical not is a substraction from True, when operating on 'i1' type
genRhs (Not p expr) = genBinOp "sub" (ELitTrue p) expr

genRhs (EMul _ expr op expr2) = genBinOp opName expr expr2
  where
    opName = case op of
      Times _ -> "mul"
      Div _ -> "sdiv"
      Mod _ -> "srem"

-- HEAD
genRhs (EAdd _ expr (Minus _) expr2) = genBinOp "sub" expr expr2

genRhs (EAdd pos expr (Plus _) expr2) = do
  addr <- genRhs expr
  addr2 <- genRhs expr2
  let ty = getAddrType addr
  r <- freshRegister ty;
  case getAddrType addr of
    TInt -> Emitter.emitBinOp "add" r addr addr2
    TStr -> do
              (_, _, funAddr) <- getIdentVal Nothing (Ident "concatStrings")
              Emitter.emitCall r funAddr [addr, addr2]
    _ -> failPos pos $ "Compiler error" -- this shouldn't happen after type check
  return r

genRhs (ERel pos expr rel expr2) = do
  addr <- genRhs expr
  addr2 <- genRhs expr2
  r <- freshRegister TBool
  let ty = getAddrType addr -- == getAddrType addr2 (thanks to type check)
  case ty of
    TInt -> Emitter.emitCmp r rel 's' addr addr2  >> return r
    TBool -> Emitter.emitCmp r rel 'u' addr addr2 >> return r
    TStr -> do
              (_, _, funAddr) <- getIdentVal Nothing (Ident "compareStrings")
              Emitter.emitCall r funAddr [addr, addr2]
              case rel of
                EQU _ -> return r
                NE _ -> do  -- negate the result
                  r2 <- freshRegister TBool
                  Emitter.emitBinOp "sub" r2 (AImm 1 TBool) r
                  return r2
                _ -> failPos pos $ "Compiler error" -- this shouldn't happen after type check
    _ -> failPos pos $ "Compiler error" -- this shouldn't happen after type check

-- remaining exps are EAnd and EOr, both handled the same way:
genRhs e@(EAnd _ _ _) = genAndOr e
genRhs e@(EOr _ _ _) = genAndOr e

genRhs _ = fail "Compiler error" -- this shouldn't happen

genAndOr :: Expr Pos -> GenM Addr
genAndOr e = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  genCond e trueLabel falseLabel

  afterLabel <- freshLabel
  setCurrentBlock trueLabel
  Emitter.emitComment $ "empty branch used only by phi in block " ++ printAddr (ALab afterLabel)
  genJmp afterLabel
  setCurrentBlock falseLabel
  Emitter.emitComment $ "empty branch used only by phi in block " ++ printAddr (ALab afterLabel)
  genJmp afterLabel

  setCurrentBlock afterLabel
  r <- freshRegister TBool
  Emitter.emitPhi r [(AImm 1 TBool, ALab trueLabel), (AImm 0 TBool, ALab falseLabel)]
  return r


genBinOp :: String -> Expr Pos -> Expr Pos -> GenM Addr
genBinOp opName expr expr2 = do
  addr <- genRhs expr
  addr2 <- genRhs expr2
  r <- freshRegister (getAddrType addr)
  Emitter.emitBinOp opName r addr addr2
  return r


genLhs :: Expr Pos -> GenM Addr
genLhs (EVar _ ident) = do
  (_, _, addr) <- getIdentVal Nothing ident
  return addr

genLhs (EArrayAcc pos arrExpr iExpr) = do
  indexAddr <- genRhs iExpr
  arrAddr <- genRhs arrExpr
  let ptrTy@(TPtr ty) = getAddrType arrAddr
  r <- genGetElem ptrTy ty arrAddr indexAddr
  return r

genLhs e = fail "Compiler error" -- this shouldn't happen after typecheck


-- getlementptr generator
genGetElem :: TType -> TType -> Addr -> Addr -> GenM Addr
genGetElem retTy baseTy ptrAddr elemAddr = do
  r <- freshRegister retTy
  Emitter.emitGetElement baseTy r ptrAddr [elemAddr] -- TODO
  return r


------------------------- Conditions ----------------------------------------
-- | Lazy condition checking by a "jumping code"
genCond :: Expr Pos -> Label -> Label -> GenM ()
genCond (EAnd _ expr expr2) trueLabel falseLabel = do
  midLabel <- freshLabel
  genCond expr midLabel falseLabel

  setCurrentBlock midLabel
  genCond expr2 trueLabel falseLabel

genCond (EOr _ expr expr2) trueLabel falseLabel = do
  midLabel <- freshLabel
  genCond expr trueLabel midLabel

  setCurrentBlock midLabel
  genCond expr2 trueLabel falseLabel

genCond expr trueLabel falseLabel = do
  addr <- genRhs expr
  genBr addr trueLabel falseLabel

------------------------- Terminators ---------------------------------------
-- NOTE: all those (and only those) finish a block
genJmp :: Label -> GenM ()
genJmp label = do
  Emitter.emitJmp (ALab label)
  finishBlock

genBr :: Addr -> Label -> Label -> GenM ()
genBr flag trueLabel falseLabel = do
  Emitter.emitBr flag (ALab trueLabel) (ALab falseLabel)
  finishBlock

genRet :: Addr -> GenM ()
genRet a = do
  Emitter.emitRet a
  finishBlock

genVRet :: GenM ()
genVRet = do
  Emitter.emitVRet
  finishBlock

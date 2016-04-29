#include <llvm/Pass.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Module.h>
#include <llvm/InitializePasses.h>
#include <llvm/Transforms/UPC.h>
#include <llvm/Config/config.h> // for UPC_IR_RP_ADDRSPACE

using namespace llvm;

namespace {

static const int UPC_PTS_ADDR_SPACE = UPC_IR_RP_ADDRSPACE;

struct LowerUPCPointers : FunctionPass {
  static char ID;
  LowerUPCPointers() : FunctionPass(ID) {
    initializeLowerUPCPointersPass(*PassRegistry::getPassRegistry());
  }
  bool runOnFunction(Function &F) {
    bool result = doInitialization(F);
    for(Function::iterator iter = F.begin(), end = F.end(); iter != end; ++iter) {
      if(runOnBasicBlock(*iter)) {
        result = true;
      }
    }
    if(doFinalization(F)) {
      result = true;
    }
    return result;
  }
  bool runOnBasicBlock(BasicBlock &BB) {
    bool result = false;
    for(BasicBlock::iterator iter = BB.begin(), end = BB.end(); iter != end;) {
      BasicBlock::iterator tmp = iter;
      ++tmp;
      if(handleInstruction(*iter)) {
        result = true;
      }
      iter = tmp;
    }
    return result;
  }
  Value *getAddr(Value *UPCPtr, Instruction *I) {
    Value *Shift = ConstantInt::get(UPCPtr->getType(), UPC_IR_RP_THREAD);
    return BinaryOperator::CreateLShr(UPCPtr, Shift, "addr", I);
  }
  Value *getThread(Value *UPCPtr, Instruction *I) {
    Value *Mask = ConstantInt::get(UPCPtr->getType(), APInt::getLowBitsSet(64, UPC_IR_RP_THREAD));
    return BinaryOperator::CreateAnd(UPCPtr, Mask, "thread", I);
  }
  enum FnID {
    GenericFn,
    FloatFn,
    DoubleFn,
    I8Fn,
    I16Fn,
    I32Fn,
    I64Fn,
    I128Fn,
    NumFns
  };
  enum { Relaxed, Strict };
  FnID ChooseFn(Type *Ty) {
    if(Ty->isFloatTy()) {
      return FloatFn;
    } else if(Ty->isDoubleTy()) {
      return DoubleFn;
    } else if(Ty->isIntegerTy() || Ty->isPointerTy()) {
      switch(Layout->getTypeStoreSize(Ty)) {
      case 1:
        return I8Fn;
      case 2:
        return I16Fn;
      case 4:
        return I32Fn;
      case 8:
        return I64Fn;
      case 16:
        return I128Fn;
      }
    }
    return GenericFn;
  }
  bool handleInstruction(Instruction &I) {
    Type * Int64Ty = Type::getInt64Ty(*Ctx);
    Type * Int8PtrTy = Type::getInt8PtrTy(*Ctx);
    if(LoadInst * LI = dyn_cast<LoadInst>(&I)) {
      if(LI->getPointerAddressSpace() == UPC_PTS_ADDR_SPACE){
        Value * Ptr = LI->getPointerOperand();
        Type * Ty = Ptr->getType()->getPointerElementType();
        int IsStrict = LI->getOrdering() == SequentiallyConsistent?
          Strict : Relaxed;
        FnID Fn = ChooseFn(Ty);
        Value * PtrRep = CastInst::Create(Instruction::PtrToInt, Ptr, Int64Ty, "", &I);
        Value * Thread = getThread(PtrRep, &I);
        Value * Addr = getAddr(PtrRep, &I);
        Value * Result;
        if(Fn != GenericFn) {
          Value *args[] = { Thread, Addr };
          Result = CallInst::Create(LoadFns[Fn][IsStrict], args, "", &I);
          // This can only happen if Ty is a pointer type.
          if(Result->getType() != Ty) {
            Result = CastInst::Create(Instruction::IntToPtr, Result, Ty, "", &I);
          }
        } else {
          Value * Tmp = new AllocaInst(Ty, "ptsload", AllocaInsertPoint);
          Value * Arg = CastInst::Create(Instruction::BitCast, Tmp, Int8PtrTy, "", &I);
        
          Value * Size = ConstantInt::get(Int64Ty, Layout->getTypeStoreSize(Ty));
          Value * args[] = { Thread, Addr, Arg, Size };
          CallInst::Create(LoadFns[GenericFn][IsStrict], args, "", &I);
          Result = new LoadInst(Tmp, "", &I);
        }
        I.replaceAllUsesWith(Result);
        I.eraseFromParent();
        return true;
      }
    } else if(StoreInst * SI = dyn_cast<StoreInst>(&I)) {
      if(SI->getPointerAddressSpace() == UPC_PTS_ADDR_SPACE) {
        Value * Val = SI->getValueOperand();
        Value * Ptr = SI->getPointerOperand();
        Type * Ty = Val->getType();
        int IsStrict = SI->getOrdering() == SequentiallyConsistent?
          Strict : Relaxed;
        FnID Fn = ChooseFn(Ty);
        Value * PtrRep = CastInst::Create(Instruction::PtrToInt, Ptr, Int64Ty, "", &I);
        Value * Thread = getThread(PtrRep, &I);
        Value * Addr = getAddr(PtrRep, &I);
        Instruction *Result;
        if(Fn != GenericFn) {
          if(Val->getType()->isPointerTy()) {
            Val = CastInst::Create(Instruction::PtrToInt, Val, Int64Ty, "", &I);
          }
          Value * args[] = { Thread, Addr, Val };
          Result = CallInst::Create(StoreFns[Fn][IsStrict], args, "", &I);
        } else {
          Value * Tmp = new AllocaInst(Ty, "ptsstore", AllocaInsertPoint);
          Value * Arg = CastInst::Create(Instruction::BitCast, Tmp, Int8PtrTy, "", &I);
          Value * Size = ConstantInt::get(Int64Ty, Layout->getTypeStoreSize(Ty));
          new StoreInst(Val, Tmp, false, &I);
          Value * args[] = { Arg, Thread, Addr, Size };
          Result = CallInst::Create(StoreFns[Fn][IsStrict], args, "", &I);
        }
        I.replaceAllUsesWith(Result);
        I.eraseFromParent();
        return true;
      }
    }
    return false;
  }
  
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesCFG();
  }

  bool doInitialization(Function &F) {
    Ctx = &F.getContext();

    BasicBlock &Entry = F.getEntryBlock();
    BasicBlock::iterator iter = Entry.begin();
    while(isa<AllocaInst>(*iter)) ++iter;


    AllocaInsertPoint =
      new BitCastInst(Constant::getNullValue(Type::getInt32Ty(F.getContext())),
                      Type::getInt32Ty(F.getContext()),
                      "lowerpointers alloca point", &*iter);
    return false;
  }
  bool doFinalization(Function &F) {
    AllocaInsertPoint->eraseFromParent();
    return false;
  }
  bool doInitialization(Module &M) {
    Ctx = &M.getContext();
    Layout = &M.getDataLayout();
    Type *VoidTy = Type::getVoidTy(*Ctx);
    Type *Int8Ty = Type::getInt8Ty(*Ctx);
    Type *Int16Ty = Type::getInt16Ty(*Ctx);
    Type *Int32Ty = Type::getInt32Ty(*Ctx);
    Type *Int64Ty = Type::getInt64Ty(*Ctx);
    Type *Int128Ty = Type::getIntNTy(*Ctx, 128);
    Type *FloatTy = Type::getFloatTy(*Ctx);
    Type *DoubleTy = Type::getDoubleTy(*Ctx);
    Type *Int8PtrTy = Type::getInt8PtrTy(*Ctx);
    // void upcr_llvm_getn(long thread, long addr, void* dst, long sz);
    LoadFns[GenericFn][Relaxed] =
      M.getOrInsertFunction("__getblk4",
                            Type::getVoidTy(*Ctx),
                            Int64Ty, Int64Ty, Int8PtrTy, Int64Ty, (Type*)0);
    // void upcr_llvm_getns(long thread, long addr, void* dst, long sz);
    LoadFns[GenericFn][Strict] =
      M.getOrInsertFunction("__getsblk4",
                            Type::getVoidTy(*Ctx),
                            Int64Ty, Int64Ty, Int8PtrTy, Int64Ty, (Type*)0);
    // T upcr_llvm_get_T(long thread, long addr)
#define DEF_LOAD_FN(var, name, type)                            \
    var = M.getOrInsertFunction(name,                           \
                                type,                           \
                                Int64Ty, Int64Ty, (Type*)0)
#define DEF_LOAD_FN2(Fn, name, type)                    \
    DEF_LOAD_FN(LoadFns[Fn][Relaxed], "__get" name "3", type);      \
    DEF_LOAD_FN(LoadFns[Fn][Strict], "__gets" name "3", type)

    DEF_LOAD_FN2(I8Fn, "qi", Int8Ty);
    DEF_LOAD_FN2(I16Fn, "hi", Int16Ty);
    DEF_LOAD_FN2(I32Fn, "si", Int32Ty);
    DEF_LOAD_FN2(I64Fn, "di", Int64Ty);
    DEF_LOAD_FN2(I128Fn, "ti", Int128Ty);
    DEF_LOAD_FN2(FloatFn, "sf", FloatTy);
    DEF_LOAD_FN2(DoubleFn, "df", DoubleTy);
    // void upcr_llvm_putn(void* src, long thread, long addr, long sz);
    StoreFns[GenericFn][Relaxed] =
      M.getOrInsertFunction("__putblk4",
                            Type::getVoidTy(*Ctx),
                            Int8PtrTy, Int64Ty, Int64Ty, Int64Ty, (Type*)0);
    // void upcr_llvm_putns(void* src, long thread, long addr, long sz);
    StoreFns[GenericFn][Strict] =
      M.getOrInsertFunction("__putsblk4",
                            Type::getVoidTy(*Ctx),
                            Int8PtrTy, Int64Ty, Int64Ty, Int64Ty, (Type*)0);
#define DEF_STORE_FN(var, name, type)                   \
    var = M.getOrInsertFunction(name, VoidTy,           \
                                Int64Ty, Int64Ty, type, \
                                (Type*)0)
#define DEF_STORE_FN2(Fn, name, type)                           \
    DEF_STORE_FN(StoreFns[Fn][Relaxed], "__put" name "3", type);            \
    DEF_STORE_FN(StoreFns[Fn][Strict], "__puts" name "3", type)

    DEF_STORE_FN2(I8Fn, "qi", Int8Ty);
    DEF_STORE_FN2(I16Fn, "hi", Int16Ty);
    DEF_STORE_FN2(I32Fn, "si", Int32Ty);
    DEF_STORE_FN2(I64Fn, "di", Int64Ty);
    DEF_STORE_FN2(I128Fn, "ti", Int128Ty);
    DEF_STORE_FN2(FloatFn, "sf", FloatTy);
    DEF_STORE_FN2(DoubleFn, "df", DoubleTy);

    return true;
  }
  LLVMContext *Ctx;
  const DataLayout *Layout;
  Instruction *AllocaInsertPoint;
  Constant *LoadFns[NumFns][2];
  Constant *StoreFns[NumFns][2];
  Constant *StoreStrictFn;
  Constant *StoreRelaxedFn;
};

}

char LowerUPCPointers::ID = 0;

INITIALIZE_PASS(LowerUPCPointers, "lower-upc-pointers",
                "Pass for lowering UPC pointer accesses",
                false, false)

FunctionPass *llvm::createLowerUPCPointersPass() {
  return new LowerUPCPointers();
}

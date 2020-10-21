add_llvm_library(LLVMupc
  LowerPointers.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms

  DEPENDS
  LLVMupc
  intrinsics_gen
  )

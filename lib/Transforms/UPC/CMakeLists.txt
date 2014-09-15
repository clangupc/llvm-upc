add_llvm_library(LLVMupc
  LowerPointers.cpp
  )

add_dependencies(LLVMupc intrinsics_gen)

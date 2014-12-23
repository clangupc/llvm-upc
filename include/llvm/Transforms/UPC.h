//===-- UPC.h - UPC Transformations -----------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This header file defines prototypes for accessor functions that expose passes
// in the UPC transformations library.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UPC_H
#define LLVM_TRANSFORMS_UPC_H

namespace llvm {

class FunctionPass;

FunctionPass *createLowerUPCPointersPass();

} // End llvm namespace

#endif

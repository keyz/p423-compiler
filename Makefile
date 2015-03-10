#----------------------------------------------------------------------
# File Makefile	
# Written by Chris Frisz	
# 	
# Created 10 Jan 2012	
# 	
# This Makefile is intended for use with CSCI-P423 and runs the the
# load_and_test.ss file. It may be extended to do other things as you
# may need.
#----------------------------------------------------------------------

#-- Variables --#
SC=$(shell which scheme 2> /dev/null)
ifeq ($(SC),)
  SC=petite
endif
HS=ghc

# HS_FLAGS=-v0
HS_FLAGS=

SCRIPT_DIR=scripts

SC_FILE=load_and_test.ss
HS_FILE=LoadAndTest.hs
CG_FILE=compile_grammars.ss

SRC_GRAMMAR=source-grammar.ss

HS_EXE=$(HS_FILE:.hs=.exe)

#-- Rules --#

# The main point of this file is to run the tests
all : grammars

grammars : Framework/GenGrammars
Framework/GenGrammars: $(SRC_GRAMMAR) GrammarCompiler
	@mkdir -p Framework/GenGrammars
	@mkdir -p FrameworkHs/GenGrammars
	$(SC) --script $(SCRIPT_DIR)/$(CG_FILE) "$(SRC_GRAMMAR)"

scheme : grammars
	$(SC) $(SCRIPT_DIR)/$(SC_FILE)

scheme-test:
	echo '(import (Framework testing)) (exit (if (test-all) 0 1))' | scheme

scheme-xml:
	@echo '(begin (import (Framework testing)) (exit (if (test-all-xml) 0 1)))' | $(SC) -q

# Run the tests straight away:
haskell: haskell-test
haskell-test: grammars build-haskell
	./$(HS_EXE)

build-haskell: 
	$(HS) --make -o $(HS_EXE) $(HS_FLAGS) $(SCRIPT_DIR)/$(HS_FILE)

# Load up the compiler interactively so as to run the tests:
haskell-interactive : grammars
	$(HS) --interactive $(HS_FLAGS) $(SCRIPT_DIR)/$(HS_FILE)

# Test both backends:
test: 
	$(MAKE) clean
	$(MAKE) grammars
# RRN: This cannot be interactive and needs to get the exit code right:
	$(MAKE) scheme-test
# RRN: It can be faster to run interpreted rather than compile:
#      (But I'm having problems with that on SOIC machines.)
#	runghc $(SCRIPT_DIR)/$(HS_FILE)
	$(MAKE) haskell

clean :
	rm -f t.s t $(HS_EXE)
	rm -rf Framework{,Hs}/GenGrammars
	find FrameworkHs -name "*.o" -exec rm -f {} \;
	find FrameworkHs -name "*.hi" -exec rm -f {} \;
	find CompilerHs  -name "*.o" -exec rm -f {} \;
	find CompilerHs  -name "*.hi" -exec rm -f {} \;

.PHONY: scheme haskell grammars clean test test-scheme test-haskell

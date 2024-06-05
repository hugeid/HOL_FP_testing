
HOMEDIR=$(HOME)
HOL=$(HOMEDIR)/HOL/bin/hol
TESTDIR=$(HOMEDIR)/HOL_FP_testing/testfiles
RESULTDIR=$(HOMEDIR)/HOL_FP_testing/results
HOLMAKE=$(HOMEDIR)/HOL/bin/Holmake
FP_TEST=$(HOMEDIR)/HOL_FP_testing/fp_test

all:
	echo "Usage: make build | make <testname> | make <testname>_save | make split_save"

build:
	$(HOLMAKE) FP_test_parser.uo
	$(HOLMAKE) FP_test_base.uo

split: $(TESTDIR)/$(op)/tests_$(op)_$(n).txt
	$(HOL) --extra "--testfile $^ --save $(save)$(s) --verbose $(verbose)$(v) --silent $(silent) --inverted $(inverted) --flags $(flags)$(f) --progress $(progress) 100" $(FP_TEST)/runtests.sml

split_save: $(TESTDIR)/$(op)/tests_$(op)_$(n).txt
	$(HOL) --extra "--testfile $^ --save true --progress 500 --verbose $(verbose)$(v) --silent $(silent) --inverted $(inverted) --flags $(flags)$(f)" $(FP_TEST)/runtests.sml > $(RESULTDIR)/$(op)/$(op)_$(n)_result.txt

%_save: $(TESTDIR)/tests_%.txt
	$(HOL) --extra "--testfile $^ --save true --progress 5000 --verbose $(verbose)$(v) --silent $(silent) --inverted $(inverted) --flags $(flags)$(f)" $(FP_TEST)/runtests.sml > $(RESULTDIR)/$*_result.txt

%: $(TESTDIR)/tests_%.txt
	$(HOL) --extra "--testfile $^ --save $(save)$(s) --verbose $(verbose)$(v) --silent $(silent) --inverted $(inverted) --flags $(flags)$(f) --progress $(progress) 100" $(FP_TEST)/runtests.sml
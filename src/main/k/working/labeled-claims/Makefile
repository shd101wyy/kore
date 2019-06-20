DEFINITION_NAME := labeled-claims

include $(CURDIR)/../include.mk

prove_tests := \
	passing-spec \

test: test-k

test-k: test-prove

test-prove: $(foreach test, $(prove_tests), $(test).kprove)

golden: $(foreach test, $(all_tests), tests/$(test).output.golden)

clean:
	rm -rf $(KOMPILED) tests/*.output

.PHONY: test-k test golden clean %.test %.krun

include $(CURDIR)/../include.mk

KOMPILED := set-access-kompiled
DEFINITION := $(KOMPILED)/definition.kore

$(DEFINITION): set-access.k
	$(KOMPILE) $(KOMPILE_OPTS) $< --syntax-module IMP

%.output: %.set-access $(DEFINITION) $(KORE_EXEC)
	$(KRUN) $(KRUN_OPTS) $< --output-file $@

%.test: %.output
	diff -u $<.golden $<

%.output.golden: %.output
	mv $< $<.golden

k_tests := abstract1
all_tests := $(k_tests)

test: $(foreach test, $(all_tests), tests/$(test).test)

test-k: $(foreach test, $(k_tests), tests/$(test).test)

golden: $(foreach test, $(all_tests), tests/$(test).output.golden)

clean:
	rm -rf $(KOMPILED) tests/*.output

.PHONY: test-k test golden clean %.test %.krun

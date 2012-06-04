BENCHMARKS = primes queens nofib tak ntak integrate_i church donut
HBCFILES = $(patsubst %,%.hbc,$(BENCHMARKS))
HBIFILES = $(patsubst %,%.hbi,$(BENCHMARKS))

.PHONY: all hbc hbi clean

all: hbc hbi clean
hbc: $(HBCFILES)
hbi: $(HBIFILES)

%.exec: %.hs
	@hbc -o $@ $^

%.hbc: %.exec
	@echo "Running $<, compiled with HBC"
	@./$^

%.script: %.hs
	@echo "Running $<, interpreted by HBI"
	@echo 'load "$^";' > $@
	@echo 'main;' >> $@
	@echo 'quit;' >> $@

%.hbi: %.script
	@hbi < $^

clean:
	$(RM) *.hi *.exec *.script

BENCHMARKS = primes queens nofib tak ntak integrate_i church donut
LMLCFILES = $(patsubst %,%.lmlc,$(BENCHMARKS))
LMLIFILES = $(patsubst %,%.lmli,$(BENCHMARKS))

.PHONY: all lmlc lmli clean

all: lmlc lmli clean
lmlc: $(LMLCFILES)
lmli: $(LMLIFILES)

%.exec: %.m
	@lmlc -o $@ $^

%.lmlc: %.exec
	@echo "Running $<, compiled with LMLC"
	@./$^

%.script: %.m
	@echo "Running $<, interpreted by LMLI"
	@echo 'load "$^";' > $@
	@echo 'main;' >> $@
	@echo 'quit;' >> $@

%.lmli: %.script
	@lmli < $^

clean:
	$(RM) *.t *.exec *.script

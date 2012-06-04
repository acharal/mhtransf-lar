BENCHMARKS = primes queens nofib tak ntak integrate_i church donut

.PHONY: all clean

all: $(BENCHMARKS)

%: %.m
	@mmake $@.depend
	@mmake $@

clean:
	mmake clean
	$(RM) $(BENCHMARKS) *.d *.dep *.dv *.mh

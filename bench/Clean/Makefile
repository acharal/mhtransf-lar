BENCHMARKS = primes queens nofib tak ntak integrate_i church donut ack

.PHONY: all clean

all: $(BENCHMARKS)

%: %.icl
	clm -nsa $@ -o $@

clean:
	$(RM) $(BENCHMARKS)
	$(RM) -fr "Clean System Files"

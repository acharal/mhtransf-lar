HSFILES   = exmh1.hs exmh2.hs exmh20.hs exmh21.hs exmh3.hs exmh31.hs \
            exmh88.hs exmh9.hs exmh100.hs exmh101.hs exmh91.hs \
            exmh90.hs 
RUNFILES  = $(patsubst %.hs,%.run,$(HSFILES))
EXECFILES = $(patsubst %.hs,%.exec,$(HSFILES))

run: $(RUNFILES)
exec: $(EXECFILES)

%.run: %.ghc
	@echo -n $(patsubst %.run,%.hs,$@) ": "
	@./$^

%.ghc: %.hs
	@echo "main =" > $@.hs
	@echo "  let {" >> $@.hs
	@sed 's/^/    /' $^ >> $@.hs
	@echo "  }" >> $@.hs
	@echo "  in print result" >> $@.hs
	@ghc -o $@ $@.hs
	@$(RM) $@.{hs,hi,o}

%.hugs: %.hs
	@echo "main =" > $@.hs
	@echo "  let {" >> $@.hs
	@sed 's/^/    /' $^ >> $@.hs
	@echo "  }" >> $@.hs
	@echo "  in print result" >> $@.hs
	@runhugs $@.hs
	@$(RM) $@.hs

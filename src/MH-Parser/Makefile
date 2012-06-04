SRCFILES = mhparse.l mhparse.y ast.h ast.c

mhparse: mhparse.l mhparse.y ast.h ast.c
	bison -dt mhparse.y
	flex -s mhparse.l
	gcc -g -o mhparse lex.yy.c mhparse.tab.c ast.c -lfl

clean:
	$(RM) mhparse.tab.c mhparse.tab.h mhparse.output lex.yy.c

distclean: clean
	$(RM) mhparse

touch:
	touch-dates -a -l -v -p '\(Revised[ \t]*: \).*$$' -rp '\1' -rs '' \
             Makefile ${SRCFILES}

checkin: touch
	ci -l Makefile $(SRCFILES)

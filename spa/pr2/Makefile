TVLA = ../tvla-3.0alpha/bin/tvla
DIR = pr2

all: search.tvp.ps create.tvp.ps add.tvp.ps

%.tvp.ps: %.tvp %.tvs
	$(TVLA) $+ 2>&1 | tee /tmp/tvla.out
	@echo
	@grep -iE '(some|all).*inconsistent|The analysis has stopped since a constraint was breached|warning' --color=yes /tmp/tvla.out || true

clean:
	rm -rf *.tvp.ps *.tvp.dt *.tvp.out.tvs

dist: clean
	tar --exclude-vcs -czvf ../$(DIR).tar.gz -C ../ $(DIR)

.PHONY: all clean dist

%.tvp:
	@touch $@

search.tvp: actions.tvp predicates.tvp
create.tvp: actions.tvp predicates.tvp
add.tvp: actions.tvp predicates.tvp

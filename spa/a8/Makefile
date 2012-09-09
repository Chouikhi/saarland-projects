TVLA = ../tvla-3.0alpha/bin/tvla
DIR = a8

all: swap.tvp.ps delete.tvp.ps

%.tvp.ps: %.tvp %.tvs
	$(TVLA) $+ 2>&1 | tee /tmp/tvla.out
	@echo
	@grep -iE '(some|all).*inconsistent' --color=yes /tmp/tvla.out || true

clean:
	rm -rf *.tvp.ps *.tvp.dt *.tvp.out.tvs

dist: clean
	tar --exclude-vcs -czvf ../$(DIR).tar.gz -C ../ $(DIR)

.PHONY: all clean dist

%.tvp:
	@touch $@

swap.tvp: swap_assert.tvp actions.tvp predicates.tvp
delete.tvp: delete_predicates.tvp delete_actions.tvp actions.tvp predicates.tvp

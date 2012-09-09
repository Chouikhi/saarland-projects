TVLA = ../tvla-3.0alpha/bin/tvla

all: access.tvp.ps access_all.tvp.ps free_list.tvp.ps

%.tvp.ps: %.tvp %.tvs
	$(TVLA) $+

clean:
	rm -rf *.tvp.ps *.tvp.dt *.tvp.out.tvs

dist: clean
	tar --exclude-vcs -czvf ../a7.tar.gz -C ../ a7

.PHONY: all clean dist

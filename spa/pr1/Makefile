all: Main Tests

Main: Main.o Parser.o RoundRobin.o TrulyLiveVariables.o Program.o
	ghc -o $@ $^

Tests: Tests.hs Parser.hs RoundRobin.hs TrulyLiveVariables.hs \
  Program.hs AvailableExpressions.hs
	ghc --make -o $@ -main-is Tests $^

Parser.hs: parser.y
	happy parser.y -o $@

%.o: %.hs
	ghc --make $^

clean:
	rm -rf *.o *.hi

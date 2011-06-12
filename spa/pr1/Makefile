all: Main Tests

Main: Main.o Parser.o RoundRobin.o TrulyLiveVariables.o Program.o \
  AvailableExpressions.o AnalysisBase.o FixPointAlgorithmBase.o \
  Worklist.o Recursive.o IntervalAnalysis.o Util.o
	ghc -o $@ $^

Tests: Tests.hs Parser.hs RoundRobin.hs TrulyLiveVariables.hs \
  Program.hs AvailableExpressions.hs AnalysisBase.hs FixPointAlgorithmBase.hs \
  Worklist.hs Recursive.hs IntervalAnalysis.hs Util.hs
	ghc --make -o $@ -main-is Tests $^

Parser.hs: parser.y
	happy parser.y -o $@

%.o: %.hs
	ghc --make $^

clean:
	rm -rf *.o *.hi Main Tests

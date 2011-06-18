HC      = ghc
HC_OPTS = -cpp $(EXTRA_HC_OPTS)

SRCS = Main.hs Tests.hs Util.hs Parser.hs \
       AnalysisBase.hs FixPointAlgorithmBase.hs Program.hs \
       AvailableExpressions.hs IntervalAnalysis.hs TrulyLiveVariables.hs \
       RoundRobin.hs Worklist.hs Recursive.hs

OBJS = Util.o Parser.o \
       AnalysisBase.o FixPointAlgorithmBase.o Program.o \
       AvailableExpressions.o IntervalAnalysis.o TrulyLiveVariables.o \
       RoundRobin.o Worklist.o Recursive.o

GEN_SRCS = Parser.hs

EXECS = Main Tests

.SUFFIXES : .o .hs .hi .lhs .hc .s

all : $(EXECS)

Main: Main.o $(OBJS)
	rm -f $@
	$(HC) -o $@ $(HC_OPTS) $^

Tests: Tests.o $(OBJS)
	rm -f $@
	$(HC) -o $@ -package HUnit $(HC_OPTS) $^
	
# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot:
	@:

.lhs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

.hs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

Parser.hs: parser.y
	happy parser.y -o $@

Tests.o: Tests.hs
	$(HC) -main-is Tests.main -c $< $(HC_OPTS)

clean:
	rm -rf *.o *.hi $(GEN_SRCS) $(EXECS)

dist: clean
	tar -C ../ pr1 --exclude-vcs -czvf ../pr1.tar.gz

depend : $(GEN_SRCS)
	ghc -M $(HC_OPTS) $(SRCS)

# DO NOT DELETE: Beginning of Haskell dependencies
Util.o : Util.hs
Program.o : Program.hs
AnalysisBase.o : AnalysisBase.hs
AnalysisBase.o : Util.hi
AnalysisBase.o : Program.hi
FixPointAlgorithmBase.o : FixPointAlgorithmBase.hs
FixPointAlgorithmBase.o : AnalysisBase.hi
FixPointAlgorithmBase.o : Program.hi
RoundRobin.o : RoundRobin.hs
RoundRobin.o : Program.hi
RoundRobin.o : AnalysisBase.hi
Worklist.o : Worklist.hs
Worklist.o : Program.hi
Worklist.o : AnalysisBase.hi
Recursive.o : Recursive.hs
Recursive.o : Util.hi
Recursive.o : Program.hi
Recursive.o : AnalysisBase.hi
TrulyLiveVariables.o : TrulyLiveVariables.hs
TrulyLiveVariables.o : Util.hi
TrulyLiveVariables.o : Program.hi
TrulyLiveVariables.o : FixPointAlgorithmBase.hi
TrulyLiveVariables.o : AnalysisBase.hi
AvailableExpressions.o : AvailableExpressions.hs
AvailableExpressions.o : Util.hi
AvailableExpressions.o : AnalysisBase.hi
AvailableExpressions.o : FixPointAlgorithmBase.hi
AvailableExpressions.o : Program.hi
IntervalAnalysis.o : IntervalAnalysis.hs
IntervalAnalysis.o : Util.hi
IntervalAnalysis.o : AnalysisBase.hi
IntervalAnalysis.o : FixPointAlgorithmBase.hi
IntervalAnalysis.o : Program.hi
Parser.o : Parser.hs
Parser.o : Program.hi
Tests.o : Tests.hs
Tests.o : Parser.hi
Tests.o : Program.hi
Tests.o : RoundRobin.hi
Tests.o : AvailableExpressions.hi
Tests.o : IntervalAnalysis.hi
Tests.o : AnalysisBase.hi
Tests.o : TrulyLiveVariables.hi
Main.o : Main.hs
Main.o : IntervalAnalysis.hi
Main.o : AvailableExpressions.hi
Main.o : TrulyLiveVariables.hi
Main.o : Recursive.hi
Main.o : Worklist.hi
Main.o : RoundRobin.hi
Main.o : Util.hi
Main.o : AnalysisBase.hi
Main.o : FixPointAlgorithmBase.hi
Main.o : Program.hi
Main.o : Parser.hi
# DO NOT DELETE: End of Haskell dependencies

include ../include.mk

OBJ = CApply.o CBinary.o CExpr.o CFunctionStorage.o CLiteral.o CPrettyPrinter.o ParserError.o CArgument.o CEvaluator.o CFunction.o CIfThenElse.o COptimizer.o EvaluationError.o FL.tab.o lex.yy.o
CFLAGS = -O3 

all: TestIO TestEvaluator TestOptimizer

doc: 
	doxygen

TestIO: $(OBJ) TestIO.cpp
	g++ $(CFLAGS) -o TestIO $(OBJ) TestIO.cpp -lfl

TestEvaluator: $(OBJ) TestEvaluator.cpp
	g++ $(CFLAGS) -o TestEvaluator $(OBJ) TestEvaluator.cpp -lfl

TestOptimizer: $(OBJ) TestOptimizer.cpp
	g++ $(CFLAGS) -o TestOptimizer $(OBJ) TestOptimizer.cpp -lfl


FL.tab.o: FL.tab.c
	g++ $(CFLAGS) -g -c FL.tab.c
FL.tab.c: FL.y
	bison -dv FL.y
FL.tab.h: FL.y
	bison -dv FL.y

lex.yy.o: lex.yy.c
	g++ $(CFLAGS) -g -c lex.yy.c

lex.yy.c: FL.lex
	flex FL.lex

%.o: %.cpp FL.tab.h FL.tab.c 
	g++ $(CFLAGS) -g -c $<

test:	all
	@echo Testing Implementation `pwd`
	@echo Testing IO...
	./TestIO ${factorial} ${temp}
	diff ${factorial} ${temp}
	rm -f ${temp}
	@echo Testing Evaluator...
	./TestEvaluator ${factorial} ${apply} 120
	@echo Testing Optimizer...
	./TestOptimizer < ${unoptimized} > ${temp}
	diff ${optimized} ${temp}
	rm -f ${temp}
	@echo done

clean:
	rm -f *.o *~ TestIO TestEvaluator TestOptimizer FL.tab.c FL.tab.h lex.yy.c FL.output

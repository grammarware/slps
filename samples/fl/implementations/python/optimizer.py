import fltypes as t
import operator

__metaclass__ = t.buildMixinMeta(t, "Optimize", "_OPTIMIZE")

class PatternMatcher:
    class SameMatch:
        def __init__(self):
            self.__initialized = False

        def __call__(self, obj=None):
            if not self.__initialized:
                self.__o = obj
                self.__initialized = True
                return True
            self.__initialized = False
            return self.__o == obj

    def _match(self, objs, patterns):
        for obj, pattern in zip(objs, patterns):
            if not self.__match_objs(obj, pattern):
                return False
        return True

    def __match_objs(self, obj, pattern):
        if isinstance(pattern, self.SameMatch):
            return pattern(obj)
        if isinstance(pattern, tuple):
            return self.__match_objs(obj, pattern[0]) and obj.match(pattern[1])
        return (
                (isinstance(pattern, type) and isinstance(obj, pattern)) or
                obj == pattern
                )
_ = object

def __land__(l, r):
    return l and r

class OptimizeLiteral(PatternMatcher):
    def match(self, *pattern):
        return False

    def optimize(self):
        return self

class OptimizeArgument(OptimizeLiteral):
    def __eq__(self, other):
        return self is other

class OptimizeOperator(PatternMatcher):
    def __eq__(self, other):
        return (type(self) == type(other)
                and self.Operator == other.Operator
                and (
                    (self.L == other.L and self.R == other.R)
                    or (self.L == other.R and self.R == other.L)
                    )
                )
    def match(self, pattern):
        return self._match((self.Operator,)+tuple(self.Operands), pattern)

    def optimize(self):
        ops = tuple(i.optimize() for i in self.Operands)
        x = self.SameMatch()
        _op_map = { "+": operator.__add__, "-": operator.__sub__, "==": operator.__eq__}
        for pattern, action in [
                #Literal(x) + Literal(y) -> Literal(x+y)
                ((str, t.Literal, t.Literal),
                    lambda: t.Literal(_op_map[self.Operator](*ops))),
                (("-", _, t.Literal),
                    lambda: t.Operator(t.Literal(-ops[1]), "+", ops[0])),
                #0 + x -> x
                (("+", 0, _),
                    lambda: ops[1]),
                #((Literal(x) + n) + Literal(y)) -> (Literal(x+y) + n)
                (("+", (t.Operator, ("+", t.Literal, _)), t.Literal),
                    lambda: t.Operator(t.Literal(ops[0].L+ops[1]), "+", ops[0].R)),
                (("+", t.Literal, (t.Operator, ("+", t.Literal, _))),
                    lambda: t.Operator(ops[0]+ops[1].L, ops[1].R)),
                #((n + m) + Literal(x)) -> ((Literal(x) + n) + m)
                (("+", (t.Operator, ("+", _, _)), t.Literal),
                    lambda: t.Operator(t.Operator(ops[1], "+", ops[0].L), "+", ops[0].R)),
                #(n + Literal(x)) -> (Literal(x) + n)
                (("+", _, t.Literal),
                    lambda: t.Operator(ops[1], "+", ops[0])),
                (("+", (t.Operator, ("-", t.Literal, _)), t.Literal),
                    lambda: t.Operator(t.Literal(ops[1] + ops[0].L), "-", ops[0].R)),
                (("+", t.Literal, (t.Operator, ("+", _, _))),
                    lambda: t.Operator(ops[1], "+", ops[0])),
                (("+", t.Argument, (t.Operator, ("+", _, _))),
                    lambda: t.Operator(ops[1], "+", ops[0])),
                (("-", _, 0),
                    lambda: ops[0]),
                (("-", _, t.Literal),
                    lambda: t.Operator(ops[0], "+", t.Literal(-ops[1]))),
                (("-", t.Literal, (t.Operator, ("-", t.Literal, _))),
                    lambda: t.Operator(t.Literal(ops[0]-ops[1].L), "+", ops[1].R)),
                (("-", t.Literal, (t.Operator, ("+", t.Literal, _))),
                    lambda: t.Operator(t.Literal(ops[0]-ops[1].L), "+", ops[1].R)),
                (("-", x, x),
                    lambda: t.Literal(0)),
                (("==", x, x),
                    lambda: t.Literal(ops[0] == ops[1])),
                (("-", (t.Operator, ("+", _, x)), x),
                    lambda: ops[0].L),
            ]:
            if self._match((self.Operator,)+ops, pattern):
                result = action().optimize()
                return result
        return t.Operator(ops[0], self.Operator, ops[1])

class OptimizeIfThenElse(PatternMatcher):
    def __eq__(self, other):
        return (
                type(self) == type(other)
                and self.Condition == other.Condition
                and self.Then == other.Then
                and self.Else == other.Else
                )

    def match(self, pattern):
        return self._match((self.Condition, self.Then, self.Else), pattern)

    def optimize(self):
        cond = self.Condition.optimize()
        then = self.Then.optimize()
        _else= self.Else.optimize()
        x = self.SameMatch()
        for pattern, action in [
                (((t.Operator, ("==", 0, t.Argument)), _, _),
                    lambda: t.IfThenElse(cond.R, _else, then)),
                (((t.Operator, ("==", t.Argument, 0)), _, _),
                    lambda: t.IfThenElse(cond.L, _else, then)),
                ((0, _, _),
                    lambda: _else),
                ((1, _, _),
                    lambda: then),
                ((_, x, x),
                    lambda: then)
            ]:
            if self._match((cond, then, _else), pattern):
                return action().optimize()
        return t.IfThenElse(cond, then, _else)

class OptimizeApply:
    def __eq__(self, other):
        if type(self) != type(other) or self.Function != other.Function or len(self.Arguments) != len(other.Arguments):
            return False
        for s, o in zip(self.Arguments, other.Arguments):
            if not s == o:
                return False
        return True

    def optimize(self):
        return t.Apply(self.Function, *(i.optimize() for i in self.Arguments))

class OptimizeFunction:
    def optimize(self):
        return t.Function(self.Name, tuple(i.optimize() for i in self.Arguments), self.Body.optimize(), self.Tainted)

class OptimizeProgram:
    def optimize(self):
        return t.Program(list(i.optimize() for i in self.Functions))

__all__ = []

import fltypes as parsing

class PPMeta(parsing.buildMixinMeta(parsing, "PP")):
    def __setParens(self, val=True):
        self.__parens = val
    def __getParens(self):
        try:
            return self.__parens
        except AttributeError:
            self.__parens = False
            return False

    Parens = property(__getParens, __setParens)

    @classmethod
    def parens(cls, format):
        if cls.Parens:
            return "(%s)"%format
        return format

__metaclass__ = PPMeta

class PPArgument:
    def __str__(self):
        if self.Tainted:
            return "<%s>"%self.Name
        return self.Name

class PPOperator:
    def __str__(self):
        format = PPMeta.parens("%s %s %s")
        PPMeta.Parens = True
        return format%(self.Operands[0], self.Operator, self.Operands[1])

class PPIfThenElse:
    def __str__(self):
        format = PPMeta.parens("if%s then %s else %s")
        PPMeta.Parens = True
        cond = str(self.Condition)
        if not cond.startswith("("):
            cond = " "+cond
        PPMeta.Parens = False
        then = str(self.Then)
        PPMeta.Parens = False
        _else= str(self.Else)
        return format%(cond, then, _else)

class PPApply:
    def __str__(self):
        PPMeta.Parens = True
        return "(%s)"%(" ".join(str(i) for i in (self.Function,)+self.Arguments))

class PPFunction:
    def __str__(self):
        PPMeta.Parens = False
        f = "%s %s = %s"%(self.Name, " ".join(str(a) for a in self.Arguments), self.Body)
        if self.Tainted:
            return "!" + f
        return f

class PPProgram:
    def __str__(self):
        return "\n".join(str(f) for f in self.Functions)

__all__ = []

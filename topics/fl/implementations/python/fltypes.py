class AutoPropertyMeta(type):
    """
    Create properties from methods:
    class.__p_<name> -> class().Name
    """
    def __init__(cls, name, bases, dict_):
        super(AutoPropertyMeta, cls).__init__(name, bases, dict_)
        for i in dict_:
            tmp = "_%s__p_"%name
            if i.startswith(tmp):
                setattr(cls, i[len(tmp):].capitalize(), property(dict_[i]))


class Expr(object):
    """Base class for all FL-expressions"""
    __metaclass__ = AutoPropertyMeta

class Literal(Expr, int):
    pass

class Argument(Expr):
    def __init__(self, name, tainted=False):
        self.__name = name
        self.__tainted = tainted

    def __p_name(self): return self.__name
    def __p_tainted(self): return self.__tainted


class Operator(Expr):
    def __init__(self, l, op, r):
        self.__op = op
        self.__ops = l, r

    def __p_operator(self): return self.__op
    def __p_operands(self): return self.__ops
    def __p_l(self): return self.__ops[0]
    def __p_r(self): return self.__ops[1]

class IfThenElse(Expr):
    def __init__(self, *exprs):
        self.__cond, self.__then, self.__else = exprs

    def __p_condition(self): return self.__cond
    def __p_then(self): return self.__then
    def __p_else(self): return self.__else

class Apply(Expr):
    def __init__(self, func, *args):
        self.__func = func
        self.__args = args

    def __p_function(self): return self.__func
    def __p_arguments(self): return self.__args

class Function(Expr):
    def __init__(self, name, args, body, tainted=False):
        self.__name = name
        self.__args = args
        self.__body = body
        self.__tainted = tainted

    def __p_name(self): return self.__name
    def __p_body(self): return self.__body
    def __p_arguments(self): return self.__args
    def __p_tainted(self): return self.__tainted

class Program(Expr, dict):
    def __init__(self, funcs):
        #Functions are stored in a list to preserve order
        self.__funcs = funcs
        for func in funcs:
            self[func.Name] = func

    def __p_functions(self): return self.__funcs


def buildMixinMeta(namespace, prefix, key=""):
    """Automagically create (and apply) Mixins"""
    class MixinMeta(type):
        def __init__(cls, name, bases, dict_):
            super(MixinMeta, cls).__init__(name, bases, dict_)
            if name.startswith(prefix):
                for i in dict_.get(key, list()):
                    cls._mixin(i)
                else:
                    cls._mixin(getattr(namespace, name[len(prefix):]))

        def _mixin(cls, base):
            base.__bases__ = (cls,)+tuple(i for i in base.__bases__ if i not in cls.__bases__)
    return MixinMeta


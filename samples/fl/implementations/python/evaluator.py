# vim: fileencoding=utf-8 :
from __future__ import with_statement
import contextlib
import fltypes
from operator import __eq__, __add__, __sub__

__metaclass__ = fltypes.buildMixinMeta(fltypes, "Eval")

class NoSuchFunction(Exception): pass
class UnboundArgument(Exception): pass
class UknownArgument(Exception): pass

class EvalLiteral:
    def eval(self, **kwargs):
        return self

class EvalArgument:
    def eval(self, init=False, val=None, no_exception=False, **kwargs):
        if init:
            self.__val = val
        else:
            try:
                return self.__val
            except AttributeError:
                raise UnboundArgument("Argument '%s' not passed to function call"%self.Name)

class EvalFunction:
    @contextlib.contextmanager
    def managed_stack(self, **kwargs):
        try:
            saved = list()
            for i in self.Arguments:
                saved.append(i.eval(**kwargs))
            yield self
            for i in zip(saved, self.Arguments):
                i[1].eval(init=True, val=i[0], **kwargs)
        except UnboundArgument:
            yield self

    def __call__(self, *args, **kwargs):
        return self.eval(args, funcs=getattr(self, "_funcs", dict()), **kwargs)

    def eval(self, args=[], **kwargs):
        if self.Tainted:
            raise UnknownArgument("Function contains reference to unknown argument")
        with self.managed_stack(**kwargs):
            for i in zip(args, self.Arguments):
                i[1].eval(init=True, val=i[0], **kwargs)
            return self.Body.eval(**kwargs)

class EvalIfThenElse:
    def eval(self, **kwargs):
        if self.Condition.eval(**kwargs):
            return self.Then.eval(**kwargs)
        return self.Else.eval(**kwargs)

class EvalOperator:
    ops = {
            "+": __add__,
            "-": __sub__,
            "==": __eq__
            }
    def eval(self, **kwargs):
        return reduce(self.ops[self.Operator], tuple(i.eval(**kwargs) for i in self.Operands))

class EvalApply:
    def eval(self, funcs={}, **kwargs):
        return funcs[self.Function](*tuple(a.eval(funcs=funcs, **kwargs) for a in self.Arguments), **kwargs)

class EvalProgram:
    def __getitem__(self, key):
        try:
            item = super(EvalProgram, self).__getitem__(key)
        except KeyError:
            raise NoSuchFunction("There is no function '%s'"%key)
        item._funcs = self
        return item

    def eval(self, func="", args=[]):
        return self[func](*args)

__all__ = []

import fltypes, evaluator, types

__metaclass__ = fltypes.buildMixinMeta(fltypes, "Caching")

class StackException(Exception):
    def __init__(self, f):
        self.f = f

class CachingFunction:
    def eval(self, args=[], _start_flatting=False, **kwargs):
        try:
            i = self.__cache
        except AttributeError:
            self.__cache = dict()
        if not args in self.__cache:
            def _anon_(flat=True):
                self.__cache[args] = super(CachingFunction, self).eval(args, _start_flatting=flat, **kwargs)
            if not _start_flatting:
                funcs = list()
                funcs.append(_anon_)
                while True:
                    try:
                        funcs[-1]()
                        break
                    except StackException, e:
                        funcs.append(e.f)
                for i in reversed(funcs):
                    i(False)
            else:
                raise StackException(_anon_)
        return self.__cache[args]

__all__ = []

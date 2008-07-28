#!/usr/bin/env python
import parser, evaluator, prettyprinter, optimizer

p = parser.parseProgram("""
        mult n m = if n == 0 then 0 else m+(mult (n-1) m)
        fac n = if n == 0 then 1 else mult n (fac (n-1))
        test n = if n == 1 then 1 else mult (n+1) (fac (n-1)) (if n then 0 else m)



        drei n = if n==n then 1+1+1 else 0+7-0
        sn n = n-1+n-n+1+n+5-n+2-n""")

def main():
    print p
    print
    optimized = p.optimize()
    print p.optimize()
    print 
    i = 1
    try:
        while True:
            print "fac %d ="%i,
            print optimized['fac'](i)
            i *= 2
    except RuntimeError, e:
        print
        print e
    print
    import caching_evaluator
    print "Retrying with cache:"
    j = 1
    while j <= i:
        print "fac %d = %d"%(j, optimized['fac'](j))
        j *= 2
    print "fac %d = %d"%(j/2+1, optimized['fac'](j/2+1))

if __name__ == "__main__":
    main()

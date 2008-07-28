#!/usr/bin/env/python
import unittest
import pyparsing
import parser, evaluator, prettyprinter, optimizer

class TestPyFL(unittest.TestCase):
    p = parser.parseProgram("""
            mult n m = if n == 0 then 0 else m+(mult (n-1) m)
            fac n = if n == 0 then 1 else mult n (fac (n-1))
            test n = if n == 1 then 1 else mult (n+1) (fac (n-1)) (if n then 0 else m)



            drei n = if n==n then 1+1+1 else 0+7-0
            sn n = n-1+n-n+1+n+5-n+2-n""")

    def testPrettyPrinter(self):
        self.assertEqual(str(self.p['mult']), "mult n m = if(n == 0) then 0 else m + (mult (n - 1) m)")
        self.assertEqual(str(self.p['fac']), "fac n = if(n == 0) then 1 else (mult n (fac (n - 1)))")

    def testEvaluator(self):
        self.assertEqual(self.p.eval("fac", (6,)), 720)
        self.assertEqual(self.p.optimize().eval("fac", (6,)), 720)
        self.assertEqual(parser.parseApply("fac 6").eval(funcs=self.p), 720)

    def testEvaluatorCached(self):
        import caching_evaluator
        self.testEvaluator()

    def testOptimizer(self):
        self.assertEqual(str(self.p.optimize()['sn']), "sn n = 7")
        self.assertEqual(str(self.p.optimize()['fac']), "fac n = if n then (mult n (fac (-1 + n))) else 1")
        self.assertEqual(self.p.optimize()['fac'](6), 720)

    def testParser(self):
        self.assertRaises(pyparsing.ParseException, parser.parseProgram, "n")
        self.assertRaises(pyparsing.ParseException, parser.parseProgram, "fac n = if n == 0 then 1 else mult n (fac (n-1))\nfoo")

def main():
    suite = unittest.TestLoader().loadTestsFromTestCase(TestPyFL)
    unittest.TextTestRunner(verbosity=2).run(suite)

if __name__ == '__main__':
    main()

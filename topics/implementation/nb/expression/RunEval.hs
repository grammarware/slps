module RunEval where
import Evaluation
import Syntax
import Data.Maybe


main = do
	print $ eval   TrueB
	print $ eval   FalseB
	print $ eval $ IsZeroB (PredN (PredN (SuccN (SuccN (PredN (SuccN ZeroN))))))
	print $ eval $ SuccN (SuccN (PredN (SuccN ZeroN)))
	print $ eval $ PredN (SuccN ZeroN)
	print $ eval $ IfNB TrueB (SuccN ZeroN) (PredN ZeroN)
	print $ eval $ IfNB TrueB TrueB FalseB
	print $ eval $ IfNB (IsZeroB ZeroN) (SuccN (SuccN ZeroN)) (PredN ZeroN)
	print $ eval $ IfNB ZeroN ZeroN ZeroN

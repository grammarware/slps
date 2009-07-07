% Calculator.Txl - simple numerical expression evaluator

% Part 1: Lexical conventions
#pragma -newline
compounds
		==
end compounds

% Part 2: Syntax specification
define program
		[fun+]
end define

define fun
		[id] [id+] = [expression] [newline]
end define

define expression
		[expression] [op] [expression]
	|	[id] [expression+]
	|	if [expression] then [expression] else [expression]
	|	( [expression] )
	|	[id]    
	|	[number]
end define

define op
		+
	|	-
	|	==
end define

% Part 2.  Transformation rules
rule main
	replace [fun]
		name [id] pars [id+] = E [expression] nl [newline]
	construct NewE [expression]
		E [resolveAddition]
		  [resolveSubtraction]
		  [resolveComparisonEq]
		  [resolveComparisonNe]
		  [resolveChoiceFalse]
		  [resolveChoiceTrue]
		  [resolveNoChoice]
		  [resolveComp1]
		  [resolveComp2]
		  [resolveBracketedExpressions]
	where not
		NewE [= E]
	by
		name pars = NewE nl
end rule

rule resolveComparisonEq
	replace [expression]
		E1 [number] == E2 [number]
	where
		E1 [= E2]
	by
		1
end rule

rule resolveComparisonNe
	replace [expression]
		E1 [number] == E2 [number]
	where not
		E1 [= E2]
	by
		0
end rule

rule resolveChoiceFalse
	replace [expression]
		if 0 then E2 [expression] else E3 [expression]
	by
		E3
end rule

rule resolveChoiceTrue
	replace [expression]
		if 1 then E2 [expression] else E3 [expression]
	by
		E2
end rule

rule resolveNoChoice
	replace [expression]
		if E1 [expression] then E2 [expression] else E3 [expression]
	where
		E2 [= E3]
	by
		E2
end rule

rule resolveComp1
	replace [expression]
		if E1 [expression] then E2 [expression] else E3 [expression]
	deconstruct E1
		0 == E4 [expression]
	by
		if E4 then E3 else E2
end rule

rule resolveComp2
	replace [expression]
		if E1 [expression] then E2 [expression] else E3 [expression]
	deconstruct E1
		E4 [expression] == 0
	by
		if E4 then E3 else E2
end rule

rule resolveAddition
    replace [expression]
        N1 [number] + N2 [number]
    by
        N1 [+ N2]
end rule

rule resolveSubtraction
    replace [expression]
        N1 [number] - N2 [number]
    by
        N1 [- N2]
end rule 

rule resolveBracketedExpressions
    replace [expression]
        ( E [expression] )
    by
        E
end rule


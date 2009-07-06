% Calculator.Txl - simple numerical expression evaluator

% Part I.  Syntax specification
define program
        [fun+]
end define

define fun
        [id] [id+] = [expression] [newline]
end define

define expression
        [expression] [op] [expression]
    |   [id] [expression+]
    |   if [expression] then [expression] else [expression]
    |       [number]
    |   [id]
    |   ( [expression] )
    
end define

define op
        +
    |   -
    |   ==
end define

% Part 2.  Transformation rules
rule main
    replace [expression]
        E [expression]
    construct NewE [expression]
        E [resolveAddition]
          [resolveSubtraction]
    where not
        NewE [= E]
    by
        NewE
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


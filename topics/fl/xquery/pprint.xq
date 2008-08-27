declare default element namespace "fl";
declare variable $input external;

declare function local:convert($name)
{
let $n := $name/text()
	return 
		if ($n = 'Equal')
		then "=="
		else if ($n = 'Plus')
			then "+"
			else if ($n = 'Minus')
			then "-"
			else "?"
};

declare function local:pprint($fun)
{
 let $type := $fun[1]/@xsi:type
 return 
  if ($type = "Argument")
  then data($fun/name) 				(: the use of /text() gives no whitespace :)	
  else if ($type = "Literal")
  then data($fun/info)
  else if ($type = "Binary")
  then ("(",
		local:pprint($fun/left),
        local:convert($fun/ops),
		local:pprint($fun/right),
		")")
  else if ($type = "IfThenElse")
  then ("if",
		local:pprint($fun/ifExpr),
		"then",
        local:pprint($fun/thenExpr),
		"else",
        local:pprint($fun/elseExpr)
		)
  else if ($type = "Apply")
  then ("(",
        data($fun/name),
        for $arg in $fun/arg
		return local:pprint($arg),
		")"
       )
  else ("NICHT",data($type))
};

let $prg := $input
return
  ("
",for $f in $prg//function
  return ($f/name/text(),
          for $arg in $f/arg
          return data($arg),
		  "=",
		  local:pprint($f/rhs),"
"))

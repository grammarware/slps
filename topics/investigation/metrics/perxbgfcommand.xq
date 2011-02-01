declare default element namespace "xsd";
declare variable $xsd  external;
declare variable $xbgf external;

declare function local:report($name,$nodes)
{
 let $counter := count($nodes)
 return
	if ($counter != 0)
	then ("
",data($name),data($counter))
	else ""
};

let $schema := $xsd/*
for $command in $schema/*/@name
return
	local:report($command,$xbgf/*[local-name()='sequence']/*[local-name()=$command]),"
"

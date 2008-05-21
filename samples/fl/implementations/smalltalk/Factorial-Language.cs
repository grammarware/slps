SystemOrganization addCategory: #'Factorial-Language'!

Object subclass: #FLFactorialExample
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Factorial-Language'!

PPCompositeParser subclass: #FLFactorialParser
	instanceVariableNames: 'apply binary condition expression function literal operation variable add close cmp else equal id if num open sub then'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Factorial-Language'!

FLFactorialParser subclass: #FLFactorialCompiler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Factorial-Language'!

FLFactorialParser subclass: #FLFactorialPrinter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Factorial-Language'!

FLFactorialExample class
	instanceVariableNames: ''!

FLFactorialParser class
	instanceVariableNames: ''!

FLFactorialCompiler class
	instanceVariableNames: ''!

FLFactorialPrinter class
	instanceVariableNames: ''!


!FLFactorialCompiler commentStamp: 'lr 5/21/2008 20:10' prior: 0!
I define productions to create a Smalltalk AST from the FL source. The Smalltalk AST can be trivially transformed to Smalltalk bytecodes and executed using the infrastructure of the development environment.!

!FLFactorialExample commentStamp: 'lr 5/21/2008 20:15' prior: 0!
I implement the example given in "factorial.txt". The code can be edited directly in the Smalltalk code browser and is automatically parsed, transformed and eventually compiled down to Smalltalk bytecodes.!

!FLFactorialParser commentStamp: 'lr 5/21/2008 20:07' prior: 0!
I define the scanner and parser for the FL programming language using the parser combinator framework PetitParser.!

!FLFactorialPrinter commentStamp: 'lr 5/21/2008 20:09' prior: 0!
I implement the pretty printer of the FL language. !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
add
	^ $+ asParser flatten! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:47'!
apply
	^ super apply ==> [ :node |
		RBMessageNode
			receiver: (RBVariableNode named: 'self')
			selector: (self
				selector: node second
				count: node third size) 
			arguments: node third ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:43'!
apply
	^ open , id , expression star , close! !

!FLFactorialPrinter methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:45'!
apply
	^ super apply ==> [ :node | '(' , node second , ' ' , (node third fold: [ :a :b | a , ' ' , b ]) , ')' ]! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:48'!
binary
	^ super binary ==> [ :node | 
		RBMessageNode
			receiver: node second
			selector: node third asSymbol
			arguments: (Array with: node fourth) ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:43'!
binary
	^ open , expression , operation , expression , close! !

!FLFactorialPrinter methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:45'!
binary
	^ super binary ==> [ :node | '(' , node second , ' ' , node third , ' ' , node fourth , ')' ]! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
close
	^ $) asParser flatten! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
cmp
	^ '==' asParser flatten! !

!FLFactorialExample class methodsFor: 'accessing' stamp: 'lr 5/21/2008 21:11'!
compilerClass
	^ PPCompilerAdapter on: FLFactorialCompiler! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:49'!
condition
	^ super condition ==> [ :node | 
		RBMessageNode 
			receiver: node second
			selector: #ifTrue:ifFalse:
			arguments: (Array 
				with: (RBBlockNode 
					arguments: #()
					body: (RBSequenceNode 
						statements: (Array with: node fourth)))
				with: (RBBlockNode 
					arguments: #()
					body: (RBSequenceNode 
						statements: (Array with: node sixth)))) ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:43'!
condition
	^ if , expression , then , expression , else , expression! !

!FLFactorialPrinter methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:46'!
condition
	^ super condition ==> [ :node | 'if ' , node second , ' then ' , node fourth , ' else ' , node sixth ]! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
else
	^ 'else' asParser flatten! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
equal
	^ $= asParser flatten! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:43'!
expression
	^ apply | condition | binary | variable | literal! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:45'!
function
	^ super function ==> [ :node |
		RBMethodNode
			selector: (self
				selector: node first
				count: node second size)
			arguments: node second
			body: ((RBSequenceNode statements: (Array
				with: node fourth))
				addReturn;
				yourself) ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:43'!
function
	^ id , variable star , equal , expression! !

!FLFactorialPrinter methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:46'!
function
	^ super function ==> [ :node | node first , ' ' , (node second fold: [ :a :b | a , ' ' , b ]) , ' = ' , node fourth ]! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
id
	^ #letter asParser plus flatten! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
if
	^ 'if' asParser flatten! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:49'!
literal
	^ super literal ==> [ :node | RBLiteralNode value: node asNumber ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:44'!
literal
	^ num! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
num
	^ #digit asParser plus flatten! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:44'!
open
	^ $( asParser flatten! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/21/2008 00:56'!
operation
	^ super operation ==> [ :node | node = '==' ifTrue: [ #= ] ifFalse: [ node asSymbol ] ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:44'!
operation
	^ cmp | add | sub! !

!FLFactorialCompiler methodsFor: 'private' stamp: 'lr 5/16/2008 21:58'!
selector: aString count: anInteger
	| stream |
	stream := WriteStream on: String new.
	stream nextPutAll: aString.
	1 to: anInteger do: [ :index |
		index > 1
			ifTrue: [ stream nextPutAll: 'with' ].
		stream nextPut: $: ].
	^ stream contents asSymbol! !

!FLFactorialExample class methodsFor: 'accessing' stamp: 'lr 5/21/2008 16:45'!
sourceCodeAt: aSelector
	^ self sourceCodeAt: aSelector ifAbsent: [ nil ]! !

!FLFactorialExample class methodsFor: 'accessing' stamp: 'lr 5/19/2008 16:30'!
sourceCodeAt: aSelector ifAbsent: aBlock
	^ (self methodDictionary at: aSelector ifAbsent: [ ^ aBlock value ])
		getSourceFromFile! !

!FLFactorialParser methodsFor: 'accessing' stamp: 'lr 5/19/2008 11:43'!
start
	^ function end! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:59'!
sub
	^ $- asParser flatten! !

!FLFactorialParser methodsFor: 'token' stamp: 'lr 5/19/2008 11:45'!
then
	^ 'then' asParser flatten! !

!FLFactorialCompiler methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:45'!
variable
	^ super variable ==> [ :node | RBVariableNode named: node ]! !

!FLFactorialParser methodsFor: 'grammar' stamp: 'lr 5/19/2008 11:51'!
variable
	^ id! !
	
!FLFactorialExample methodsFor: 'other' stamp: 'lr 5/21/2008 20:01'!
ack m n = if (m == 0) then (n + 1) else if (n == 0) then (ack (m - 1) 1) else (ack (m - 1) (ack m (n - 1)))! !

!FLFactorialExample methodsFor: 'factorial' stamp: 'lr 5/21/2008 20:01'!
fac n = if (n==0) then 1 else (mult n (fac (n - 1)))! !

!FLFactorialExample methodsFor: 'other' stamp: 'lr 5/21/2008 20:01'!
fib n = if (n == 0) then 0 else if (n == 1) then 1 else ((fib (n - 1)) + (fib (n - 2)))! !

!FLFactorialExample methodsFor: 'factorial' stamp: 'lr 5/21/2008 20:01'!
mult n m = if (n==0) then 0 else (m + (mult (n - 1) m))! !

Browser
    fullOnClass: FLFactorialExample
    selector: #fac: !

Workspace new
    contents: 'FLFactorialExample new fac: 6.

FLFactorialExample new fib: 10.
  
FLFactorialExample new ack: 2 with: 3.'; 
    openLabel: 'Factorial Language Examples' !
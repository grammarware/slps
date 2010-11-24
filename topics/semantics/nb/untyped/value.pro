value(true).
value(false).
value(NV) :- nvalue(NV).

nvalue(zero).
nvalue(succ(NV)) :- nvalue(NV).


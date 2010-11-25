letrec(
  add,
  fun(nat,fun(nat,nat)),
  lam(x,nat,lam(y,nat,
    if(iszero(var(x)),
      var(y),
      succ(app(app(var(add),pred(var(x))),var(y)))))),
  app(app(var(add),succ(zero)),succ(zero))
).


letrec(
  add,
  fun(nat,fun(nat,nat)),
  lam(x,nat,lam(y,nat,
    if(iszero(var(x)),
      var(y),
      succ(app(app(var(add),pred(var(x))),var(y)))))),
  letrec(
    mult,
    fun(nat,fun(nat,nat)),
    lam(x,nat,lam(y,nat,
      if(iszero(var(x)),
        zero,
        app(app(var(add),var(y)),app(app(var(mult),pred(var(x))),var(y)))))),
    app(app(var(mult),succ(succ(zero))),succ(succ(zero)))
)).

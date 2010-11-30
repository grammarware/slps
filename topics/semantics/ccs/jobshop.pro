/*

This is a very simple JobShop system.
We have one jobber.
We have one hammer.
All jobs require a jammer.

*/

jobshop(As)
 :-
    S =
       [
         (jobshop, restrict((var(hammer)|var(jobber)),[geth,puth]))
       , (hammer, name(geth):name(puth):var(hammer))
       , (jobber, name(job):coname(geth):coname(puth):coname(done):var(jobber)) 
       ],
    tstar(var(jobshop),_,As,S).

/*

(C) 2010 Ralf Laemmel

A JobShop system

The spec has been extracted from this resource:

W. Schreiner: Concurrency lectures, Kepler University, Linz

The spec has been adapted to work without variable binding.

*/


% Take the "environment" (the client) as an argument 

jobshop(Env,As)
 :-
    S = [

/*
A JobShop consists of these components: 
- 2 jobbers
- 1 hammer
- 1 mallet
*/
         (jobshop, restrict(
                     ( var(jobber)
                     | var(jobber)
                     | var(hammer)
                     | var(mallet)
                     ),[geth,puth,getm,putm]))

/* 
Tools are derived from a reusable agent for semaphores.
We apply relabeling to derive hammer and mallet.
*/
       , (sem, name(get):name(put):var(sem))
       , (hammer, relabel(var(sem),[(get,geth),(put,puth)]))
       , (mallet, relabel(var(sem),[(get,getm),(put,putm)]))

/*
A jobber can do different kinds of jobs.
An easy job does not require any tool.
A hard job requires a hammer.
A tool job requires some tool.
*/
       , (jobber, (var(easyJob)+var(hardJob)+var(toolJob)))
       , (easyJob, (name(easyJob):var(finish)))
       , (hardJob, (name(hardJob):var(usehammer)))
       , (toolJob, (name(toolJob):(var(usehammer)|var(usemallet))))
       , (usehammer, coname(geth):coname(puth):var(finish))
       , (usemallet, coname(getm):coname(putm):var(finish))
       , (finish, coname(done):var(jobber))

% We compose the jobshop and the environment.
       , (main, restrict((Env|var(jobshop)),[easyJob,hardJob,toolJob]))

       ],


% Invoke transitive closure

    tstar(var(main),_,As,S).


/*
Execute a job list.
Use jobs of different kinds (easy, hard, and tool jobs).
For traceability purposes, we have a "do" action.
*/

main
 :-
    jobshop(
      ( coname(easyJob)
      : coname(do)
      : coname(hardJob)
      : coname(do)
      : coname(toolJob)
      : coname(do)
      : zero
      ),
      As),
    done(3,As).


/*
Constraint traces:
- do a number jobs
- finish all started jobs
- end with action done
*/

done(N,As) :- done(N,N,As).

done(0,0,[]).

done(X1,Y,[A|Rest])
 :- 
    X1 > 0,
    A = coname(do),
    X2 is X1 - 1,
    done(X2,Y,Rest).

done(X,Y1,[A|Rest])
 :- 
    Y1 > 0,
    A = coname(done),
    Y2 is Y1 - 1,
    done(X,Y2,Rest).

done(X,Y,[A|Rest])
 :- 
    \+ A = coname(do),
    \+ A = coname(done),
    done(X,Y,Rest).


/*
Find a possible trace for the list of jobs above.
*/

:- 
   main,
   halt.

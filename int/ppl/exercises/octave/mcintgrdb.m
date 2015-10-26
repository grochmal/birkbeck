% Use the dbugger to find 2 bugs here
% dbstop(fun, linenum)
% dbnext [n]
% dbcont
% dbquit
% dbclear(fun)

function I = mcintgrdb(fun, a, b, mcloops)
  x = linspace(a,b);
  f = feval(fun,x);
  maxf = max(f);
  r1 = rand(mcloops,1);
  r2 = rand(mcloops,1);
  l = a - b;
  x = a + l.*r1;
  y = maxf.*r2;
  fx = feval(fun, x);
  counter = length(find(y>fx));
  I = counter/mcloops*maxf*l;
end


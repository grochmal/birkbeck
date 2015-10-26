#!/usr/bin/octave -qf
% selkov model bifurcation
1;

function truth = samesign(x, y)
  if 0 > x && 0 > y
    truth = 1;
  elseif 0 <= x && 0 <= y
    truth = 1;
  else
    truth = 0;
  end
end

clear;
global global_a;
global global_b;
global_a = 0.01;
global_b = 0.6;
opt = optimset("Jacobian", "on");
root = fsolve("selkov", [1 1], opt);
[f, J] = selkov(root);
[Vec, Val] = eig(J);
BaseEig = [real(Val(1,1)), real(Val(2,2))];
NewEig = BaseEig
while samesign(BaseEig(1), NewEig(1)) && samesign(BaseEig(1), NewEig(1))
  global_a += 0.01;
  [func, Jacobian] = selkov(root);
  [NewVec, NewVal] = eig(Jacobian);
  NewEig = [real(NewVal(1,1)), real(NewVal(2,2))];
end  % never finishes J is not dependent on b!
BaseEig
NewEig
printf("Bifurcation at value %f\n", global_a);


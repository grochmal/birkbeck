
function [f, J] = selkov(x)
  global global_a;
  global global_b;
  f(1) =    -x(1) + global_a*x(2) + x(1).^2 * x(2);
  f(2) = global_b - global_a*x(2) - x(1).^2 * x(2);
  J(1,1) = -1   + 2*x(1)*x(2);
  J(1,2) =  global_a + x(1).^2;
  J(2,1) = -2*x(1)*x(2);
  J(2,2) = -global_a - 2*x(1)*x(2);
end


a = []; for i = [1:10]; a = [a 0.1*i];         end;
b = []; for i = [1:10]; b = [b i*2-0.1*i-3*i]; end;
c = []; for i = [2:20]; c = [c 1/i];           end;
d = []; for i = [1:20]; d = [d 1/2^i];         end;
s = sum(d);
s

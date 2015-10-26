#!/usr/bin/octave -qf

try
  load primes_1000.mat;
catch
  disp("Needs the file primes_1000.mat, containing the 1st 1000 primes");
end
prime_diff = diff(prime_seq);
p100_diff = prime_diff(1:100);
plot(p100_diff);


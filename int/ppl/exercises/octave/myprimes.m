#!/usr/bin/octave -qf
% Octave myprimes function
% returns a list of nprimes prime numbers
%
% Usage:
%   [prime_seq] = myprimes(nprimes)

function [prime_seq] = myprimes(nprimes)
  fflush(stdout);
  if 1 != nargin || 1 < nargout
    usage("[prime_seq] = myprimes(nprimes)");
  elseif 0 >= nprimes
    usage("The argument must be a positive number");
  end

  prime_counter = 1;
  prime_seq(prime_counter) = 2;
  x = 3;
  while prime_counter < nprimes
    is_x_prime = 1;
    for y = 3:2:(x-1)/2
      if 0 == rem(x, y)
        is_x_prime = 0;
        break;
      end
    end
    if is_x_prime
      prime_counter++;
      prime_seq(prime_counter) = x;
      printf("Got prime number %d (%d)\n", prime_counter, x);
      fflush(stdout);
    end
    x += 2;
  end
end


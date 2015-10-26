#!/usr/bin/octave -qf

fflush(stdout);
try
  nprimes = input("Enter number of primes (>0): ");
catch
  usage("Must be a number");
end
if 0 > nprimes
  usage("Must be a positive number");
end

clear prime_seq;
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
prime_seq


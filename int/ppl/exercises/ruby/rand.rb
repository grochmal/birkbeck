#!/usr/bin/env ruby
# random guess

num = rand(10)
guess = nil
puts 'Please guess a number between 0 and 9 (type "quit" to exit)'
until guess == num or 'quit' == guess
  print 'Your choice: '
  guess = gets.to_i
  if num < guess
    puts 'Try a smaller number, please try again'
  elsif num > guess
    puts 'Try a bigger number, please try again'
  else
    puts 'It is the correct number!  Thanks'
  end
end


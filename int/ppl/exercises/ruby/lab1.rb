#!/usr/bin/env ruby
# lab 1 exercises

ar = [5, 4, 7, 8, 3]
ar.each_index do |idx|
  puts ar[idx] if 1 == idx % 2
end

def odd_idxs(ar=[], ret=[])
  ar.each_index {|idx| ret << ar[idx] if 1 == idx % 2}
  ret
end

odds = odd_idxs ar
puts 'Odd indexes'
puts odds

File.open 'lab1.rb' do |file|
  file.each {|line| print "#{file.lineno}: #{line}"}
end


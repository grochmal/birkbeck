#!/usr/bin/env ruby
# grep implementation

pattern = ARGV.shift
file = ARGV.shift
File.open file, 'r' do |file|
  file.each do |line|
    puts "#{file.lineno} - #{line}" if /#{pattern}/ =~ line
  end
end


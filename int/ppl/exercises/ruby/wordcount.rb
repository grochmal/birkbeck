#!/usr/bin/env ruby

wordcount = {}
File.open 'words' do |file|
  file.each do |line|
    words = line.split
    words.each do |word|
      same = words.select {|x| x == word}
      wordcount[word] = same.length
    end
  end
end

wordcount.each {|k, p| puts "#{p}: #{k}" if p > 1}


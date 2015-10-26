#!/usr/bin/env ruby

File.open 'words' do |file|
  file.each do |line|
    words = line.split
    word1 = words.pop
    while word1
      word2 = words.pop
      puts "#{file.lineno}: #{word1}" if word1 == word2
      word1 = word2
    end
  end
end


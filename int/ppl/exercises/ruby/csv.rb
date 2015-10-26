#!/usr/bin/env ruby
# read csv files

class CsvRow
  attr_accessor :headers, :csv_contents
  def initialize(headers, csv_contents)
    @headers = headers
    @csv_contents = csv_contents
  end

  def method_missing name, *args
    header = name.to_s
    idx = @headers[header]
    @csv_contents[idx] if idx
  end
end

module ActAsCsv
  def self.included(base)
    base.extend ClassMethods
  end

  module ClassMethods
    def act_as_csv
      include InstanceMethods
    end
  end

  module InstanceMethods
    def read
      @csv_rows = []
      file = File.new(self.class.to_s.downcase + '.txt')
      headers = {}
      @head_arr = file.gets.chomp.split /\s*,\s*/
      @head_arr.each_index {|idx| headers[head_arr[idx]] = idx}
      file.each do |row|
        csv_contents = row.chomp.split(/\s*,\s*/)
        @csv_rows << CsvRow.new(headers, csv_contents)
      end
    end

    def each
      @csv_rows.each {|i| yield i}
    end

    attr_accessor :head_arr, :csv_rows
    def initialize
      read
    end
  end
end

class RubyCsv  # no inheritance!  You can mix it in
  include ActAsCsv
  act_as_csv
end

csv = RubyCsv.new
csv.each {|row| puts row.one}


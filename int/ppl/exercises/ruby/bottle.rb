#!/usr/bin/env ruby

class Bottle
  attr_accessor :content
  def initialize(content)
    @content = Content.new content
  end

  def serve
    content.say
  end
end

class Content
  attr :data
  def initialize(data)
    @data = data
    @info = {'water' => 'pour',
             'wine'  => 'decant'}
  end

  def say
    if action = @info[@data]
      puts "#{action} #{@data}"
    else
      puts 'no idea what I am'
    end
  end
end

water = Bottle.new 'water'
wine = Bottle.new 'wine'
cheese = Bottle.new 'cheese'
water.serve
wine.serve
cheese.serve


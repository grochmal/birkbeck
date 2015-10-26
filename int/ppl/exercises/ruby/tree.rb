#!/usr/bin/env ruby
# hash to tree

class Tree
  attr_accessor :children, :node_name

  def initialize(construct={})
    construct.each do |pkey, pval|
      @node_name = pkey
      @children = []
      pval.each_pair {|ckey, cval| @children.push Tree.new(ckey => cval)}
    end
  end

  def visit_all(&block)
    visit &block
    self.children.each {|c| c.visit_all &block}
  end

  def visit(&block)
    block.call self
  end
end

tree = { 'grandpa' => { 'dad' => { 'child1' => {}    \
                                 , 'child2' => {}    \
                                 }                   \
                      , 'uncle' => { 'child3' => {}  \
                                   , 'child4' => {}  \
                                   }
                      }
       }

ruby_tree = Tree.new tree

puts 'Visiting a node'
ruby_tree.visit {|node| puts node.node_name}

puts 'Visiting the entire tree'
ruby_tree.visit_all {|node| puts node.node_name}


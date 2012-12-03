
require 'erb'

t = ERB.new(File.read("templates/ruby.erb"))
hello = 'world'
puts t.result

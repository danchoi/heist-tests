require 'sequel'
require 'erb'
DB = Sequel.connect "postgres:///netflix"
xs = DB["select netflix_id, title, synopsis, year from titles_instant limit 20"]

templ = File.read("templates/title.erb")
hello = 'world'
titles = xs.to_a 
puts ERB.new(templ).result(binding)




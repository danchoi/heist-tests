require 'sequel'
require 'erb'
DB = Sequel.connect "postgres:///netflix"
xs = DB["select netflix_id, title, synopsis, year from titles_instant limit 20"]

templ = "<h1><%= x[:title] %></h1>\n<div><%= x[:synopsis] %></div>"

xs.to_a.map {|x|
  puts ERB.new(templ).result
}



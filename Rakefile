require 'cucumber'
require 'cucumber/rake/task'
require 'fileutils'

Cucumber::Rake::Task.new :features do |t|
  t.cucumber_opts = 'features --format pretty'
end

desc 'Build this project'
task :build do
  sh 'stack build'
  FileUtils.mkdir 'bin' unless File.exist? 'bin'
  FileUtils.cp `stack exec which dl-momonga-images-from-pinterest-exe`.strip, 'bin/dl-momonga-images-from-pinterest', preserve: true
end

desc 'Rus tests'
task test: [:features] do
  sh 'stack test'
  sh 'stack exec hlint -- . -c'
end

desc 'Install bin'
task :install do
  sh 'stack install'
end

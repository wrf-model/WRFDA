#!/usr/bin/env ruby


# == Synopsis
#
# A bit of hackery to run count_field_diffs.rb for all files in 
# subdirectories 1taskA and 16taskA.  
# Runs "diff" first...  
#
# == Usage
#
#   ruby count_all.rb dir1 dir2
# 
# == Author
# Tom Henderson, NCAR/MMM

require 'find'

# Do all the work...  
class CountAll

  # CountAll.new
  def initialize( dir1, dir2 )
    @dirname1 = dir1
    @dirname2 = dir2
    do_it  # yes!  the best name ever!  
  end

  # read input files and find differences
  # and find any arrays that match and contain only zeros
  def do_it
    @fieldfiles1 = []
    @fieldfiles2 = []
    Find.find(@dirname1){|f| @fieldfiles1 << File.basename(f) if (f=~/field\./)}
    Find.find(@dirname2){|f| @fieldfiles2 << File.basename(f) if (f=~/field\./)}
    @fieldfiles1.sort!
    @fieldfiles2.sort!
    raise "mismatched dirs" unless (@fieldfiles1 == @fieldfiles2)
    @fieldfiles1.each do |f|
      file1 = "#{@dirname1}/#{f}"
      file2 = "#{@dirname2}/#{f}"
      cmd = "diff #{file1} #{file2} | wc"
      puts "#{cmd}"
      puts `#{cmd}`
    end
  end

end  # class CountAll


if $0 == __FILE__

unless (ARGV.length == 2) then
  puts "USAGE:  ruby count_all.rb dir1 dir2"
  raise "incorrect number of arguments"
end
CountAll.new(ARGV[0], ARGV[1])

end



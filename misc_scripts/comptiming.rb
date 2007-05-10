#!/usr/bin/env ruby

# Quick hack to grab info from DBENCH output of solve_em_ad.F
# and print a few things.  
# Stuff this into Excel later...  

# Usage:  comptiming.rb wrf_ad.error

# require 'csv'

# Responsible for:  
#
# * Parsing output from DBENCH and extracting timing data for 
#   various occurrences of "comp*" and a few others.  
#
# * Writing an ASCII report.  
#

class CompTiming

  # CompTiming.new( aString )
  # Argument is file name.  
  def initialize( fname )
    @filename = fname
    @comp_timing_results = Hash.new
    @solve_tim = 0
    parse_timing
    print_report
  end

  def parse_timing
    # read file
    lines = IO.readlines(@filename)
    lines.each do |line|
      if (line =~ /comp(\w*)\s*=\s*/) then
        new_key = "comp" + $1.dup
        @comp_timing_results[new_key] = 0 unless (@comp_timing_results.has_key?(new_key))
        @comp_timing_results[new_key] += $'.to_i
      elsif (line =~ /solve_tim\s*=\s*/) then
        @solve_tim += $'.to_i
      end
    end
  end

  def print_report
    total_time = 0
    @comp_timing_results.each_value { |val| total_time += val }
    sorted_results = @comp_timing_results.sort {|a,b| b[1]<=>a[1]}
    sorted_results.each do |timer|
      fraction = timer[1].to_f / total_time.to_f
      # TODO:  use a better way to format printed Float 
      ifraction = ( 10000.0 * fraction ).round
      percent = ifraction.to_f / 100.0
      puts "Timer #{timer[0]}:  #{timer[1]}  #{percent}"
    end
    puts "Sum of all \"comp\" times = #{total_time}"
    puts "Sum of all \"solve_tim\" times = #{@solve_tim}"
  end

end  # class CompTiming


if $0 == __FILE__

CompTiming.new(ARGV[0])

end



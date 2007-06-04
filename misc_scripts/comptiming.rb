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

class DBENCHTiming

  # DBENCHTiming.new( aString )
  # Argument is file name.  
  def initialize( fname )
    @filename = fname
    @compexch_timing_results = Hash.new
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
        @compexch_timing_results[new_key] = 0 unless (@compexch_timing_results.has_key?(new_key))
        @compexch_timing_results[new_key] += $'.to_i
      elsif (line =~ /exch(\w*)\s*=\s*/) then
        # refactor to remove duplication
        new_key = "exch" + $1.dup
        @compexch_timing_results[new_key] = 0 unless (@compexch_timing_results.has_key?(new_key))
        @compexch_timing_results[new_key] += $'.to_i
      elsif (line =~ /solve_tim\s*=\s*/) then
        @solve_tim += $'.to_i
      end
    end
  end

  def print_report
    total_time = 0
    total_comptime = 0
    total_exchtime = 0
    @compexch_timing_results.each do |key,val|
      total_time += val
      total_comptime += val if (key =~ /^comp/)
      total_exchtime += val if (key =~ /^exch/)
    end
    sorted_results = @compexch_timing_results.sort {|a,b| b[1]<=>a[1]}
    sorted_results.each do |timer|
      fraction = timer[1].to_f / total_time.to_f
      # TODO:  use a better way to format printed Float 
      ifraction = ( 10000.0 * fraction ).round
      percent = ifraction.to_f / 100.0
      puts "Timer #{timer[0]}:  #{timer[1]}  #{percent}"
    end
    puts "Sum of all \"comp\" times = #{total_comptime}"
    puts "Sum of all \"exch\" times = #{total_exchtime}"
    puts "Sum of all \"comp\" and \"exch\" times = #{total_time}"
    puts "Sum of all \"solve_tim\" times = #{@solve_tim}"
  end

end  # class DBENCHTiming


if $0 == __FILE__

DBENCHTiming.new(ARGV[0])

end



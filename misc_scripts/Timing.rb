#!/usr/bin/env ruby

# Quick hack to grab info from WRF "Timing" output 
# and print a few things.  
#
# Stuff this into Excel later...  

# Usage:  Timing.rb filename
#

# Responsible for:  
#
# * Grabbing filename from the command-line.  
#
# * Parsing output from "Timing" messages and extracting timing data.  
#
# * Writing an ASCII report.  
#

class WRFTiming

  # WRFTiming.new
  def initialize
    @filename = find_file
    @timings = parse_timing
    print_report
  end

  def find_file
    raise "must supply input file name as single argument" unless ARGV.length==1
    ARGV[0]
  end

  def parse_timing
    # read files and sum parsed times
    times = Hash.new
    IO.readlines(@filename).each do |line|
      if (line =~ /Timing for ([\w\s]+):/) then
        new_key = $1.dup
        if (line =~ /:    ([\d\.]+) elapsed seconds/) then
          new_time = $1.to_f
          times[new_key] = 0.0 unless (times.has_key?(new_key))
          times[new_key] += new_time
        end
      end
    end
    times
  end

  # TODO:  improve formatting
  def print_report
    @timings.each { |key,val| puts "Sum of times for \"#{key}\" = #{val}" }
    non_main_time = 0
    @timings.each { |key,val| non_main_time += val unless (key == "main") }
    puts "Total non-main time = #{non_main_time}"
  end

end  # class WRFTiming


if $0 == __FILE__

WRFTiming.new

end



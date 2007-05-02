#!/usr/bin/env ruby

# Quick hack to grab info from DBENCH output of solve_em_ad.F
# and print a few things.  
# Stuff this into Excel later...  

# Usage:  solve_tim.rb baseline_filename [ filename2 filename3 ... ]
#
# "baseline_filename" is the name of the file with which all others will be 
# compared.  
#
# NOTE:  Each file must live in a different directory

# Responsible for:  
#
# * Grabbing file names from the command-line.  
#
# * Parsing output from DBENCH and extracting timing data for 
#   all occurrences of "solve_tim".  
#
# * Writing an ASCII report.  
#

class SolveTiming

#  require 'find'

  # SolveTiming.new
  def initialize
    @filenames = []
    @solve_tims = Hash.new
    find_files
    parse_timing
    check_timing
    extract_times
    compute_relative_times
    print_report
  end

  def find_files
#    Find.find(".") { |f| @filenames << f if ( f =~ /wrf_ad.*error/ ) }
    ARGV.each { |f| @filenames << f }
  end

  def parse_timing
    # read files
    @filenames.each do |fname|
      @solve_tims[fname] = []
      IO.readlines(fname).each do |line|
        @solve_tims[fname] << $'.to_i if (line =~ /solve_tim\s*=\s*/)
      end
    end
  end

  def check_timing
    # verify that all files contain the same number of measurements
    @solve_tims.each do |key, val|
      firstkey ||= key
      thislength = val.length
      numtimes ||= thislength
      unless ( numtimes == thislength ) then
        raise "ERROR:  number of times do not match in files #{firstkey} (#{numtimes}) and #{key} (#{thislength})"
      end
    end
  end

  def extract_times
    @time_rows = []
    @filenames.each do |fname|
      @solve_tims[fname].each_with_index do |tim,indx|
        @time_rows[indx] ||= []
        @time_rows[indx] << tim
      end
    end
  end

  def compute_relative_times
    @relative_times = []
    @time_rows.each_with_index do |row,indx|
#      avg = 0; row.each { |tim| avg += tim }
#      avg /= row.length
      @relative_times[indx] ||= []
      row.each { |tim| @relative_times[indx] << Float(tim) / Float(row.first) }
    end
  end

  # TODO:  improve formatting
  def print_report
    header = ""
    @filenames.each { |f| header << " #{File.dirname(f)} " }
    puts "========="
    puts "RAW TIMES"
    puts "========="
    puts "#{header}"
    @time_rows.each { |row| puts row.join("  ") }
    puts "========================="
    puts "TIMES RELATIVE TO #{@filenames.first}"
    puts "========================="
    puts "#{header}"
    @relative_times.each { |relative_time| puts relative_time.join("  ") }
  end

end  # class SolveTiming


if $0 == __FILE__

SolveTiming.new

end



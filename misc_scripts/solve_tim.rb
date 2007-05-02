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
    @filenames = find_files
    @solve_tims = parse_timing
    check_timing
    @time_rows = extract_times
    @relative_times = compute_relative_times
    @relative_times_min = compute_relative_minimum_times
    print_report
  end

  def find_files
#    fnames = []
#    Find.find(".") { |f| fnames << f if ( f =~ /wrf_ad.*error/ ) }
#    fnames
    ARGV
  end

  def parse_timing
    # read files
    solve_times = Hash.new
    @filenames.each do |fname|
      solve_times[fname] = []
      IO.readlines(fname).each do |line|
        solve_times[fname] << $'.to_i if (line =~ /solve_tim\s*=\s*/)
      end
    end
    solve_times
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
    trows = []
    @filenames.each do |fname|
      @solve_tims[fname].each_with_index do |tim,indx|
        trows[indx] ||= []
        trows[indx] << tim
      end
    end
    trows
  end

  def compute_relative_times
    time_ratios = []
    @time_rows.each_with_index do |row,indx|
#      avg = 0; row.each { |tim| avg += tim }
#      avg /= row.length
      time_ratios[indx] ||= []
      row.each { |tim| time_ratios[indx] << Float(tim) / Float(row.first) }
    end
    time_ratios
  end

  # Find minimum solve_tim from each file and compute ratios relative to 
  # baseline.  
  def compute_relative_minimum_times
    times_min = nil
    @time_rows.each do |row|
      times_min ||= row.collect {|tim| tim}  # "dup"
      row.each_with_index do |tim,indx|
        times_min[indx] = tim if (tim < times_min[indx])
      end
    end
    times_min.collect { |tim| Float(tim) / Float(times_min.first) }
  end

  # TODO:  improve formatting
  def print_report
    header = ""
    @filenames.each { |f| header << " #{File.dirname(f)} " }
    if (@time_rows) then
      puts "========="
      puts "RAW TIMES"
      puts "========="
      puts "#{header}"
      @time_rows.each { |row| puts row.join("  ") }
    end
    if (@relative_times) then
      puts "========================="
      puts "TIMES RELATIVE TO #{@filenames.first}"
      puts "========================="
      puts "#{header}"
      @relative_times.each { |tim| puts tim.join("  ") }
    end
    if (@relative_times_min) then
      puts "========================="
      puts "MINIMUM TIMES RELATIVE TO #{@filenames.first}"
      puts "========================="
      puts "#{header}"
      puts @relative_times_min.join("  ")
    end
  end

end  # class SolveTiming


if $0 == __FILE__

SolveTiming.new

end



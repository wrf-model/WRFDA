#!/usr/bin/env ruby

# Quick hack to grab info from DBENCH output of solve_em_ad.F
# and print a few things.  
# Stuff this into Excel later...  

# Usage:  detailtiming.rb wrf_ad.error

# require 'csv'

# Responsible for:  
#
# * Parsing output from DBENCH and extracting timing data for 
#   various occurrences of repeated bits of code.  
#
# * Writing an ASCII.  
#

class DetailTiming

  # DetailTiming.new( aString )
  # Argument is file name.  
  def initialize( fname )
    @filename = fname
    @result_order = []
    @timing_tags = Hash.new
    @timing_results = Hash.new
    parse_small_step_timing
    print_report
  end

  def parse_small_step_timing
    # set up hard-coded data
    # TODO:  deduce result_order from order of "comp" tags...  
    @result_order = %w{ before_small_stepu before_small_stepzt before_small_stepzq before_small_stepzw before_small_stepzz before_small_stepzr before_a_calc_coef_w before_a_spec_bdy_dry small_stepu small_stepzt small_stepzq small_stepzw small_stepzz small_stepzr }
    @timing_tags["before_small_stepu"]  = %w{ comp7 }
    @timing_tags["small_stepu"]  = %w{ comp8  comp9  comp10 }
    @timing_tags["before_small_stepzt"]  = %w{ comp20 }
    @timing_tags["small_stepzt"] = %w{ comp21 comp22 comp23 }
    @timing_tags["before_small_stepzq"]  = %w{ comp32 }
    @timing_tags["small_stepzq"] = %w{ comp33 comp34 comp35 }
    @timing_tags["before_small_stepzw"]  = %w{ comp43 }
    @timing_tags["small_stepzw"] = %w{ comp44 comp44b comp45 }
    @timing_tags["before_small_stepzz"]  = %w{ comp57 }
    @timing_tags["small_stepzz"] = %w{ comp58 comp59 comp60 }
    @timing_tags["before_small_stepzr"]  = %w{ comp70 }
    @timing_tags["small_stepzr"] = %w{ comp71 comp72 comp73 }
    @timing_tags["before_a_calc_coef_w"]  = %w{ comp93 }
    @timing_tags["before_a_spec_bdy_dry"] = %w{ comp103 }
    # initialize results
    @timing_tags.each_value do |tags|
      tags.each { |tag| @timing_results[tag] = 0 }
    end
    # read file
    lines = IO.readlines(@filename)
    lines.each do |line|
      @timing_tags.each_value do |tags|
        tags.each do |tag|
          if (line =~ /#{tag}\s*=\s*/) then
            @timing_results[tag] += $'.to_i
          end
        end
      end
    end
  end

  def print_report
    @result_order.each do |loop_name|
      s = "Times for loop #{loop_name}:"
      tags = @timing_tags[loop_name]
      tags.each { |tag| s << " #{@timing_results[tag]}" }
      if (tags.length > 1) then
        total = 0
        tags.each { |tag| total += @timing_results[tag] }
        s << "  TOTAL = #{total}"
      end
      puts s
    end
  end

  def to_s
    s = ""
    @result_order.each do |loop_name|
      s << "loop_name = #{loop_name}, tags = "
      s << @timing_tags[loop_name].join(" ") << "\n"
    end
    s
  end

end  # class DetailTiming

if $0 == __FILE__

DetailTiming.new(ARGV[0])

end


#!/usr/bin/env ruby

# quick hack to grab info from hand-analyzed files
# replace with automatic analysis later

require 'csv'

# Responsible for:  
#
# * Reading a source file fragment containing analysis information in the 
#   form of lines beginning with "! TBH:  OVERWRITE" followed by a 
#   comma-separated list of variable names.  
#
# * Writing a custom report.  
#
# Much is hard-coded for now.  Integrate with an automated dependence 
# analyzer later...  

class OverwriteReader

  # OverwriteReader.new
  def initialize
    # default settings
    @srcfile = "solve_em_ad_anal.F"
    @varlist = []
    @small_step_varlist = []
    @before_small_step_varlist = []
    @after_small_step_varlist = []
    @all_report_file = "ALL"
    @small_step_report_file = "IN_small_step_LOOP"
    @before_small_step_report_file = "BEFORE_small_step_LOOP"
    @after_small_step_report_file = "AFTER_small_step_LOOP"
    parse_overwrite_info
    print_report
  end

  def parse_overwrite_info
    src = IO.readlines(@srcfile)
    before_small_step_loop = true
    in_small_step_loop = false
    after_small_step_loop = false
    src.each do |line|
      if (line =~ /OVERWRITE\s*/) then
        varstr = $'.dup
        if (varstr =~ /BEGIN small_step/) then
          before_small_step_loop = false
          in_small_step_loop = true
          after_small_step_loop = false
        elsif (varstr =~ /END small_step/) then
          before_small_step_loop = false
          in_small_step_loop = false
          after_small_step_loop = true
        else
          newlist = CSV.parse_line(varstr)
          @varlist += newlist
          @small_step_varlist += newlist if (in_small_step_loop)
          @before_small_step_varlist += newlist if (before_small_step_loop)
          @after_small_step_varlist += newlist if (after_small_step_loop)
        end
      end
    end
    @varlist.sort!.uniq!
    @small_step_varlist.sort!.uniq!
    @before_small_step_varlist.sort!.uniq!
    @after_small_step_varlist.sort!.uniq!
  end

  def print_report
    all_report = "All variables set = \n#{@varlist.join("\n")}\n"
    small_step_report = "Variables set in small_step loop = \n#{@small_step_varlist.join("\n")}\n"
    before_small_step_report = "Variables set before small_step loop = \n#{@before_small_step_varlist.join("\n")}\n"
    after_small_step_report = "Variables set after small_step loop = \n#{@after_small_step_varlist.join("\n")}\n"
    File.open("#{@all_report_file}", "w") { |aFile| aFile.puts(all_report) }
    File.open("#{@small_step_report_file}", "w") { |aFile| aFile.puts(small_step_report) }
    File.open("#{@before_small_step_report_file}", "w") { |aFile| aFile.puts(before_small_step_report) }
    File.open("#{@after_small_step_report_file}", "w") { |aFile| aFile.puts(after_small_step_report) }
  end

end  # class OverwriteReader


if $0 == __FILE__

OverwriteReader.new

end


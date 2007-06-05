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
# Usage:  
#   readOverwrite.rb filename
#
# Much is hard-coded for now.  Integrate with an automated dependence 
# analyzer later...  

class OverwriteReader

  # OverwriteReader.new
  def initialize(infile)
    # default settings
    @srcfile = infile
    @varlist = []
    @report_file = "#{infile}.report"
    parse_overwrite_info
    print_report
  end

  def parse_overwrite_info
    src = IO.readlines(@srcfile)
    src.each do |line|
      if (line =~ /OVERWRITE\s*/) then
        varstr = $'.dup
        newlist = CSV.parse_line(varstr)
        @varlist += newlist
      end
    end
    @varlist.sort!.uniq!
  end

  def print_report
    all_report = "All variables set = \n#{@varlist.join("\n")}\n"
    File.open("#{@report_file}", "w") { |aFile| aFile.puts(all_report) }
  end

end  # class OverwriteReader


if $0 == __FILE__

OverwriteReader.new(ARGV[0])

end


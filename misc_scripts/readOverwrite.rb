#!/usr/bin/env ruby

# quick hack to grab info from hand-analyzed files
# replace with automatic analysis later

# load path for FortranTools
$: << "/loquat2/hender/Ruby/FortranTools"

require 'csv'
require 'fortranStatements'
require 'subroutineDefinition'
require 'dummyArgument'


# Responsible for:  
#
# * Reading a source file fragment containing analysis information in the 
#   form of lines beginning with "! TBH:  OVERWRITE" followed by a 
#   comma-separated list of variable names.  
#
# * Grabbing subroutine definition and call statements from specified 
#   files and extracting the parameter lists.  If the file from which the 
#   specified subroutine is called contains more than one call to the 
#   subroutine, then only the first call is used.  
#
# * Writing a custom report in which dummy arguments are translated 
#   into the actual arguments used in the caller.  
#
# Usage:  
#   readOverwrite.rb sourcefile subname subfile callfile
#
# Much is hard-coded for now.  Integrate with an automated dependence 
# analyzer later...  

class OverwriteReader

  # OverwriteReader.new
  def initialize(sourcefile, subname, subfile, callfile)
    # default settings
    @srcfile = sourcefile
    @subname = subname
    @subfile = subfile
    @callfile = callfile
    @varlist = []
    @report_file = "#{@srcfile}.report"
    parse_overwrite_info
    @xlate_list = translate_dummy_args
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

  def translate_dummy_args
    dummyargs = findargs(:subroutine, @subfile)
    actualargs = findargs(:call, @callfile)
    raise "argument length mismatch" if (dummyargs.length != actualargs.length)
    xlate = Hash.new
    dummyargs.each_with_index do |darg,indx|
      xlate[darg.downcase] = actualargs[indx].downcase
    end
    @varlist.collect { |var| xlate[var.downcase] }
  end

  def findargs(matchtype, filename)
    statements = FortranStatements.new(filename)
    ret = nil
    if ( matchtype == :subroutine) then
      mysub = SubroutineDefinition.new(statements, @subname)
      mysub_args = mysub.getDummyArguments   # aArray of aDummyArgument
      ret = mysub_args.collect{ |arg| arg.argname }
    elsif ( matchtype == :call) then
      # TODO:  push this down into FortranStatements and friends
      # TODO:  return aArray of aDummyArgument
      statements.each do |statement|
        if (statement.subroutine_call_name =~ /#{@subname}/i) then
          ret = statement.subroutine_call_actual_arguments
          break
        end
      end
    else
      raise "findargs only finds calls and subroutine defintions, so far"
    end
    raise "could not find \"#{matchtype} #{@subname}\" in file \"#{filename}\"" unless ret
    ret
  end

  def print_report
    all_report = "! TBH:  OVERWRITE #{@xlate_list.join(",")}\n"
    File.open("#{@report_file}", "w") { |aFile| aFile.puts(all_report) }
  end

end  # class OverwriteReader


if $0 == __FILE__

unless (ARGV.length == 4) then
  puts "USAGE:  ruby readOverwrite.rb sourcefile subname subfile callfile"
  raise "incorrect number of arguments"
end
OverwriteReader.new(ARGV[0], ARGV[1], ARGV[2], ARGV[3])


end


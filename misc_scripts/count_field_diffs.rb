#!/usr/bin/env ruby


# == Synopsis
#
# A bit of hackery to count differences in fields stored in simple ASCII 
# files.  File format is:  
#   a_mu_1( 29 , 24 ) =  0.000000000000000000E+00
#   a_mu_1( 30 , 24 ) =  0.000000000000000000E+00
#   a_mu_2( 1 , 1 ) =  0.237405503262783543E-05
#   a_mu_2( 2 , 1 ) =  -0.200366413057258622E-01
#   ...
#   u_2( 30 , 16 , 24 ) =  5.10168933868408203
#   u_2( 31 , 16 , 24 ) =  4.33041763305664062
#   a_a( 1 , 1 , 1 ) =  0.000000000000000000E+00
#   a_a( 2 , 1 , 1 ) =  0.000000000000000000E+00
#
# == Usage
#
#   ruby count_field_diffs.rb input_file1 input_file2
# 
# == Author
# Tom Henderson, NCAR/MMM


# Do all the work...  
class CountFieldDiffs

  # CountFieldDiffs.new
  def initialize( fname1, fname2 )
    @filename1 = fname1
    @filename2 = fname2
    find_differences
    print_differences
  end

  # read input files and find differences
  # and find any arrays that match and contain only zeros
  def find_differences
    @allvarnames = {}
    @diffcounts = {}
    @diffindxs = {}
    @allzeros = {}
    @lines1 = IO.readlines(@filename1)
    @lines2 = IO.readlines(@filename2)
    @lines1.each_with_index do |line, indx|
      if (line =~ /\s*(\w+)\s*\(/) then
        varname = $1.dup
        @allvarnames[varname] = nil
        if ( line == @lines2[indx] ) then
          # if lines match, check for zero value
          if (line =~ /\)\s*=\s*/) then
            begin
              value = Float($'.dup.strip)
            rescue
              raise "failed to parse value in line \"#{line}\""
            end
            @allzeros[varname] ||= true
            @allzeros[varname] = false if (value != 0.0)
          else
            raise "bad value in line \"#{line}\""
          end
        else
          @diffcounts[varname] ||= 0
          @diffcounts[varname] += 1
          @diffindxs[varname] ||= []
          @diffindxs[varname] << indx
        end
      else
        raise "bad variable name in line \"#{line}\""
      end
    end
  end

  # print information about differences to stdout
  def print_differences
    different_varnames = @diffcounts.reject{|key, value| value == 0}.keys.sort
    match_varnames = (@allvarnames.keys - different_varnames).sort
    allzero_varnames = @allzeros.reject{|key, value| not value}.keys.sort
    puts "==============================================="
    puts "FIELDS THAT DIFFER:"
    puts "==============================================="
    different_varnames.each do |vname|
      puts "#{vname}\t\tNumber of differences = #{@diffcounts[vname]}"
    end
    puts "==============================================="
    puts "FIELDS THAT MATCH AND CONTAIN NON-ZERO VALUES:"
    puts "==============================================="
    puts (match_varnames - allzero_varnames).sort.join("\n")
    puts "==============================================="
    puts "FIELDS THAT MATCH AND CONTAIN ONLY ZERO VALUES:"
    puts "==============================================="
    puts allzero_varnames.sort.join("\n")
  end

end  # class CountFieldDiffs


if $0 == __FILE__

unless (ARGV.length == 2) then
  puts "USAGE:  ruby count_field_diffs.rb input_file1 input_file2"
  raise "incorrect number of arguments"
end
CountFieldDiffs.new(ARGV[0], ARGV[1])

end



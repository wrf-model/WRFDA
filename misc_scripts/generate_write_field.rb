#!/usr/bin/env ruby


# == Synopsis
#
# A bit of hackery to generate calls to write_field_global[23]d 
# given calls of the form:  
#
#   call foo( var , n[23]d_elem )
#
# Where "foo" can be the name of any Fortran subroutine and 
# "var" can be the name of any Fortran variable.  
#
# All calls live in a file whose name is specified on the command line.  
#
# == Usage
#
#   ruby generate_write_field.rb input_file
# 
# == Author
# Tom Henderson, NCAR/MMM


# Do all the work...  
class GenerateWriteField

  # GenerateWriteField.new
  def initialize( fname )
    @filename = fname
    translate_code
  end

  # read input, translate, and write to stdout
  def translate_code
    IO.readlines(@filename).each do |line|
      if (line =~ /call\s+(\w+)\s*\(\s*(\w+)\s*,\s*n(\d)d_elem\s*\)/i) then
        sub = $1.dup
        var = $2.dup
        dim = $3.dup
        newsub = "write_field_global#{dim}d"
        puts "CALL #{newsub} ( #{var}, &"
        puts "                          \'#{var}\', filecount, \'m\', &"
        puts "                          ids, ide, jds, jde, kds, kde, &"
        puts "                          ims, ime, jms, jme, kms, kme, &"
        puts "                          ips, ipe, jps, jpe, kps, kpe )"
      else
        # leave untranslated lines as-is
        puts line
      end
    end
  end

end  # class GenerateWriteField


if $0 == __FILE__

GenerateWriteField.new(ARGV[0])

end



#!/usr/bin/env ruby

outfile = "solve_tim_tst.out"
outfileOK = "#{outfile}_OK"

File.delete(outfile) if (FileTest.file?(outfile))

` ../solve_tim.rb tst0/wrf_ad.error tst12/wrf_ad.error tst123/wrf_ad.error > #{outfile} `

` xxdiff #{outfileOK} #{outfile} `



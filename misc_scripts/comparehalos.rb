#!/usr/bin/env ruby

# quick hack to grab info from hand-analyzed files
# TODO:  convert this hackery into a Class

halonames = []
order = []
halos = []

halonames.push("SECOND HALO")
order.push(%w{ small_stepu_0747 small_stepzt_1146 small_stepzq_1518 small_stepzw_1889 small_stepzz_2397 small_stepzr_2715 })

halos.push(Hash.new)
halos[0][order[0][0]] = "al,alt,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,php,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww".split(",")
halos[0][order[0][1]] = "al,alt,cosa,e,f,moist_1,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,phb,php,t_1,t_2,t_init,u_1,u_2,v_1,v_2,w_1,w_2,ww".split(",")
halos[0][order[0][2]] = "al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww".split(",")
halos[0][order[0][3]] = "al,alt,moist_2,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww".split(",")
halos[0][order[0][4]] = "al,alt,moist_1,moist_2,msft,msfu,msfv,mu_1,mu_2,p,pb,ph_2,php,t_1,t_2,u_2,v_2,w_2,ww".split(",")
halos[0][order[0][5]] = "al,alt,moist_1,moist_2,mu_1,mu_2,p,pb,ph_1,ph_2,php,t_1,t_2,u_1,u_2,v_1,v_2,w_1,w_2,ww".split(",")


halonames.push("THIRD HALO")
order.push(%w{ small_stepu_0790 small_stepzt_1189 small_stepzq_1560 small_stepzw_1930 small_stepzz_2441 small_stepzr_2762 })

halos.push(Hash.new)
halos[1][order[1][0]] = "al,mu_2,mudf,muts,muus,muvs,p,ph_2".split(",")
halos[1][order[1][1]] = "al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save".split(",")
halos[1][order[1][2]] = "al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save".split(",")
halos[1][order[1][3]] = "al,alt,mu_1,mu_2,mudf,muts,p,pb,ph_2,php,t_1".split(",")
halos[1][order[1][4]] = "al,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1,t_save,u_save,v_save,ru_m,rv_m,w_save,a_mu_2,cqu,cqv,t_2,u_2,v_2,w_2".split(",")
halos[1][order[1][5]] = "al,alt,mu_1,mu_2,mudf,muts,muus,muvs,p,pb,ph_2,php,t_1".split(",")

halos_1_larger_stencil_4 = "al,mu_2,muts,muus,muvs,p,pb,ph_2,php,t_save,u_save,v_save,w_save,a_mu_2,cqu,cqv,t_2,u_2,v_2,w_2"

puts "COMPARING HALO UPDATES IN small_step LOOPS..."
common = []
common.push(halos[0][order[0][0]])
common.push(halos[1][order[1][0]])
worst = []
worst.push(halos[0][order[0][0]])
worst.push(halos[1][order[1][0]])
halos.each_index do |indx|
  halos[indx].each do |key,val|
  #puts "val = #{val.join(",")}"
    common[indx] = common[indx] & val
    worst[indx] = worst[indx] | val
  end
  puts "-- #{halonames[indx]} ----------------------------------------"
  puts "  #{order[indx].join("\n  ")}"
  puts "#{common[indx].length} ARRAYS FOUND IN EVERY HALO UPDATE:\n  #{common[indx].join(",")}"
  puts "#{worst[indx].length} ARRAYS FOUND IN AT LEAST ONE HALO UPDATE:\n  #{worst[indx].join(",")}"
end

puts "NOTE:  The following arrays have a much larger stencil for #{order[1][4]}:"
puts "  #{halos_1_larger_stencil_4}"


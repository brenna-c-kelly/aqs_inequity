



mon_crds <- st_coordinates(mons$co)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_co <- rescale(monitors.ppp, 1000, "km")

co.lest.mc <- envelope(monitors.ppp_co, 
                       fun = 'Lest', 
                       nsim = 99, 
                       verbose = FALSE, 
                       global = TRUE)
plot(co.lest.mc, shade = c("hi", "lo"))

mon_crds <- st_coordinates(mons$pm)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_pm <- rescale(monitors.ppp, 1000, "km")

pm.lest.mc <- envelope(monitors.ppp_pm, 
                       fun = 'Lest', 
                       #nsim = 2, 
                       verbose = FALSE, 
                       global = TRUE)
plot(pm.lest.mc, shade = c("hi", "lo"))

mon_crds <- st_coordinates(mons$no2)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_no2 <- rescale(monitors.ppp, 1000, "km")

no2.lest.mc <- envelope(monitors.ppp_no2, 
                       fun = 'Lest', 
                       #nsim = 2, 
                       verbose = FALSE, 
                       global = TRUE)
plot(no2.lest.mc, shade = c("hi", "lo"))

mon_crds <- st_coordinates(mons$o3)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_o3 <- rescale(monitors.ppp, 1000, "km")

o3.lest.mc <- envelope(monitors.ppp_o3, 
                       fun = 'Lest', 
                       #nsim = 2, 
                       verbose = FALSE, 
                       global = TRUE)
plot(pm.lest.mc, shade = c("hi", "lo"))

mon_crds <- st_coordinates(mons$so2)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_so2 <- rescale(monitors.ppp, 1000, "km")

so2.lest.mc <- envelope(monitors.ppp_so2, 
                       fun = 'Lest', 
                       #nsim = 2, 
                       verbose = FALSE, 
                       global = TRUE)
plot(pm.lest.mc, shade = c("hi", "lo"))

mon_crds <- st_coordinates(mons$pb)#monitors_criteria)#test)#monitors_sf)
monitors.ppp <- ppp(x = mon_crds[, 1], y = mon_crds[, 2], 
                    window = states_shp.win)#, marks = mons$criteria) #us.proj.win)
monitors.ppp_pb <- rescale(monitors.ppp, 1000, "km")

pb.lest.mc <- envelope(monitors.ppp_pb, 
                       fun = 'Lest', 
                       #nsim = 2, 
                       verbose = FALSE, 
                       global = TRUE)
plot(pb.lest.mc, shade = c("hi", "lo"))

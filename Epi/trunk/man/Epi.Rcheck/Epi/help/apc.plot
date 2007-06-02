apc.plot                 package:Epi                 R Documentation

_P_l_o_t _t_h_e _e_s_t_i_m_a_t_e_s _f_r_o_m _a _f_i_t_t_e_d _A_g_e-_P_e_r_i_o_d-_C_o_h_o_r_t _m_o_d_e_l

_D_e_s_c_r_i_p_t_i_o_n:

     This function plots the estimates created by 'apc.fit' in a single
      graph. It just calls 'apc.frame' after computing some sensible 
     values of the parameters, and subsequently plots the estimates
     using  'apc.lines'.

_U_s_a_g_e:

     apc.plot(obj, r.txt = "Rate", ...)

_A_r_g_u_m_e_n_t_s:

     obj: An object of class 'apc'. 

   r.txt: The text to put on the vertical rate axis. 

     ...: Additional arguments passed on to 'apc.lines'. 

_V_a_l_u_e:

     A numerical vector of length two, with names
     'c("cp.offset","RR.fac")'. The first is the offset for the cohort
     period-axis, the second the multiplication factor for the
     rate-ratio scale. Therefore, if you want to plot at '(x,y)' in the
     right panel, use
     '(x-res["cp.offset"],y/res["RR.fac"])=(x-res[1],y/res[2])'. This
     vector should be supplied for the parameter 'frame.par' to
     'apc.lines' if more sets of estimates is plotted in the same
     graph.

_A_u_t_h_o_r(_s):

     Bendix Carstensen, Steno Diabetes Center, <URL:
     http://www.pubhealth.ku.dk/~bxc>

_S_e_e _A_l_s_o:

     'apc.lines,apc.frame,apc.fit'

_E_x_a_m_p_l_e_s:

     data( lungDK )
     attach( lungDK )
     apc1 <- apc.fit( A=Ax, P=Px, D=D, Y=Y/10^5 )
     fp <- apc.plot( apc1 )
     apc.lines( apc1, frame.par=fp, drift=1.01, col="red" )
     for( i in 1:11 )
       apc.lines( apc1, frame.par=fp, drift=1+(i-6)/100, col=rainbow(12)[i] )


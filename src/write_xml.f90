module write_xml

contains

subroutine Wxml_cpfic()
    use sys_utils
    use param_data
    implicit none

    integer lcmd, ls
    character (len=100) f_result
    logical(4) resultat
    integer(4) resultatI  
    
    ls=len_trim(param%nomficout)

    if (param%systeme=='LINUX') then
       f_result(1:6+ls) = param%nomficout(1:ls)//'.xml .'
       !resultatI = copy(f_result)
    else
       f_result(1:5+ls) = param%nomficout(1:ls)//'.xml'
       !resultatI = copy(f_result)
    endif
    
    !write(0,*)f_result

end subroutine Wxml_cpfic


subroutine Wxml_header(num_vers)
    implicit none
    
    character (len=8) date
    character (len=10) heure
    character (len=40) num_vers
    !call W_ligne()
    call date_and_time(date,heure)
    
    write(75,'(A)')'<?xml version="1.0" encoding="UTF-8"?>'
    write(75,*)'<?xml-stylesheet href="mcgravi_results.xsl" type="text/xsl"?>'
    write(75,*)'<mcgravi_result>'
    
    write(75,*)'	<process_info>'
    write(75,*)'		<program_name>mcgravi</program_name>'
    write(75,*)'		<version>',num_vers(1:len_trim(num_vers)),'</version>'
    write(75,*)'		<title>Absolute and relative gravimetric data adjustement</title>'
    write(75,*)'		<author>Beilin Jacques</author>'
    write(75,*)'		<agency>IPGP / IGN</agency>'
    write(75,*)'		<start_date>',date(7:8),'-',date(5:6),'-',date(1:4),'</start_date>'
    write(75,*)'		<start_time>',heure(1:2),':',heure(3:4),':',heure(5:6),'</start_time>'
    write(75,*)'	</process_info>'
    
end subroutine Wxml_header

subroutine Wxml_footer()
    implicit none
    character (len=8) date
    character (len=10) heure
     
    call date_and_time(date,heure)   
    write(75,*)'	<process_info2>'
    write(75,*)'		<end_date>',date(7:8),'-',date(5:6),'-',date(1:4),'</end_date>'
    write(75,*)'		<end_time>',heure(1:2),':',heure(3:4),':',heure(5:6),'</end_time>'
    write(75,*)'	</process_info2>'
    write(75,*)'</mcgravi_result>'
    
end subroutine Wxml_footer

subroutine Wxml_cal_gravi()
    use param_data
    use raw_data
    implicit none
    integer k
    ! Ecriture des observations dans le fichier résultat
    
    write(75,*)'	<initial_calibration>'
    do k=1,ngravimeter
        if (TabGravi(k)%Estimate) then
    		!write(75,*)'		<gravimeter>'
    		!write(75,*)'			<serial>',TabGravi(k)%Serial,'</serial>'
    		!write(75,*)'			<n>',TabGravi(k)%N,'</n>'
    		!write(75,*)'			<factor>',TabGravi(k)%Cf,'</factor>'
    		!write(75,*)'			<estimated>Y</estimated>'
			!write(75,*)'		</gravimeter>'
						
			write(75,'(A,A,A,A,A,F16.8,A)')'		<gravimeter serial="',&
			&TabGravi(k)%serial,&
			&'" n="',TabGravi(k)%N,&
			&'" factor="',TabGravi(k)%Cf,&
			&'" estimated="Y" />'  
        else

    		!write(75,*)'		<gravimeter>'
    		!write(75,*)'			<serial>',TabGravi(k)%Serial,'</serial>'
    		!write(75,*)'			<n>',TabGravi(k)%N,'</n>'
    		!write(75,*)'			<factor>',TabGravi(k)%Cf,'</factor>'
    		!write(75,*)'			<estimated>N</estimated>'
			!write(75,*)'		</gravimeter>'
			
			write(75,'(A,A,A,A,A,F16.8,A)')'		<gravimeter serial="',&
			&TabGravi(k)%serial,&
			&'" n="',TabGravi(k)%N,&
			&'" factor="',TabGravi(k)%Cf,&
			&'" estimated="N" />'  
        end if
    end do

    write(75,*)'	</initial_calibration>'

end subroutine Wxml_cal_gravi


subroutine Wxml_sigma_coeff()
    use param_data
    implicit none
    write(75,*)'	<sigma_factor>'
    write(75,*)'			<constant>',param%sigma_add,'</constant>'
    write(75,*)'			<factor>',param%sigma_factor,'</factor>'
    write(75,*)'	</sigma_factor>'
end subroutine Wxml_sigma_coeff

subroutine Wxml_relf_list()
    use param_data
    implicit none
    integer k,LENrep, LENnomfic,w,z
    type (TDataFic) Dfic
    character (len=100) :: CHFMT
    character (len=60) nom_complet,rep,nomfic
    
    write(75,*)'	<relf_list>'
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 0) then
        
            rep = Dfic%rep       
            LENrep = len_trim(rep)
            nomfic = Dfic%nom
            LENnomfic = len_trim(nomfic)
            nom_complet = ''        
            nom_complet(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic(1:LENnomfic)
			!write(75,*)'		<relf>'
            !write(75,*)'			<num>',k,'</num>'
            !write(75,*)'			<name>',nom_complet(1:len_trim(nom_complet)),'</name>'
            !write(75,*)'			<constant>',Dfic%sigma_a,'</constant>'
            !write(75,*)'			<factor>',Dfic%Sigma_f,'</factor>'
            !write(75,*)'		</relf>'
            write(75,*)'		<relf num="',&
            &k,'" name="',&
            &nom_complet(1:len_trim(nom_complet)),&
            &'" constant="',&
            &Dfic%sigma_a,&
            &'" factor="',&
            &Dfic%Sigma_f,'" />'
                       
        end if
    end do  
	write(75,*)'	</relf_list>'
end subroutine Wxml_relf_list



subroutine Wxml_absf_list()
    use param_data
    implicit none
    integer k,w
    type (TDataFic) Dfic
    character (len=100) :: CHFMT
    logical trouve
    
    trouve = .false.
    do k=1,param%nDataFic
       Dfic = param%TabDataFic(k)
       if (Dfic%typ == 1) trouve=.true.
    end do
    
    if (.not. trouve) return
    
    write(75,*)'	<absf_list>'
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)     
        if (Dfic%typ == 1) then
			!write(75,*)'		<absf>'
			!write(75,*)'			<name>',Dfic%nom(1:len_trim(Dfic%nom)),'</name>'
			!write(75,*)'			<constant>',Dfic%sigma_a,'</constant>'
            !write(75,*)'			<factor>',Dfic%Sigma_f,'</factor>'
			!write(75,*)'		</absf>'   
			write(75,*)'		<abs num="',&
            &k,'" name="',&
            &Dfic%nom(1:len_trim(Dfic%nom)),&
            &'" constant="',&
            &Dfic%sigma_a,&
            &'" factor="',&
            &Dfic%Sigma_f,'" />'  
        end if    
    end do
    write(75,*)'	</absf_list>'
end subroutine Wxml_absf_list

subroutine Wxml_a10f_list()
    use param_data
    implicit none
    integer k,w
    type (TDataFic) Dfic
    character (len=100) :: CHFMT
    logical trouve
    
    trouve = .false.
    do k=1,param%nDataFic
       Dfic = param%TabDataFic(k)
       if (Dfic%typ == 2) trouve=.true.
    end do
    
    if (.not. trouve) return
    
    write(75,*)'  <absf_list>'
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)     
        if (Dfic%typ == 2) then
			!write(75,*)'    <a10f>'
			!write(75,*)'      <name>',Dfic%nom(1:len_trim(Dfic%nom)),'</name>'
			!write(75,*)'      <constant>',Dfic%sigma_a,'</constant>'
            !write(75,*)'      <factor>',Dfic%Sigma_f,'</factor>'
			!write(75,*)'    </a10f>'
		    write(75,*)'		<a10 num="',&
            &k,'" name="',&
            &Dfic%nom(1:len_trim(Dfic%nom)),&
            &'" constant="',&
            &Dfic%sigma_a,&
            &'" factor="',&
            &Dfic%Sigma_f,'" />'     
        end if    
    end do
    write(75,*)'  </absf_list>'
end subroutine Wxml_a10f_list

subroutine Wxml_station_list()
    use param_data
    use raw_data
    implicit none
    integer k
   
    write(75,*)'  <station_list>'

    do k=1,NTabStation
        write(75,'(A,A,A,F14.8,A,f14.8,A,I5,A)')&
        &'      <station name="',&
        &TabStation(k)%nomsta(1:len_trim(TabStation(k)%nomsta)),&
        &'" lon="',TabStation(k)%lon,&
        &'" lat="',TabStation(k)%lat,&
        &'" num="',TabStation(k)%numsta,&
        &'"/>' 
    end do
    write(75,*)'  </station_list>'
end subroutine Wxml_station_list

subroutine Wxml_rel_observation()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil,w
    character (len=80) :: chfmt
    
    ! Ecriture des observations dans le fichier résultat
    write(75,*)'  <rel_observations>'    
    nprofil = 0
    do k=1,nTabObs
        obs=TabObs(k)
        !write(0,*)'Profil (XML) =  ',obs%profil
        
        if (obs%profil/=nprofil) then
            if (obs%profil .gt. 1) write(75,*)'    </profile>'
            w = len_trim(tabprofil(obs%profil)%nomfic)

            if (k>0)  write(75,*)'    <profile num="',obs%profil,&
            &'" file="',tabprofil(obs%profil)%nomfic(1:w),&
            &'" cal_factor="',obs%Cf,&
            &'" tide="',&
            &tabprofil(obs%profil)%Tide(1:len_trim(tabprofil(obs%profil)%Tide)),&
            &'" >'

            nprofil = obs%profil
        end if 
        
        write(75,'(A,A,A,F9.3,A,f9.4,A,f9.3,A,F5.2,A,I6.6,A,I6.6,A)')&
        &'      <obs_rel sta="',&
        &obs%nomsta(1:len_trim(obs%nomsta)),&
        &'" grav="',obs%grav,&
        &'" grad="',obs%grad,&
        &'" sd="',obs%sd,&
        &'" temp="',obs%tempK,&
        &'" date="',obs%date,&
        &'" time="',obs%heure,&
        &'"/>'         
    end do
    write(75,*)'    </profile>'
    write(75,*)'  </rel_observations>'
    
end subroutine Wxml_rel_observation

subroutine Wxml_gravity_difference()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil,w
    character(len=80) :: chfmt
    real*8 sigma
    nprofil = 0
    
    write(75,*)'  <gravity_differences>'
  
    do k=1,param%Nb_obsRel

        Obs_Rel=TabObsRel(k)
        
        if (obs_Rel%profil/=nprofil) then
            if (k>1)  write(75,*)'    </profile>'
            
            w = len_trim(tabprofil(obs%profil)%nomfic)
            write(75,'(A,I4,A,A,A,F10.6,A)')&
            &'    <profile num="',&
            &obs_rel%profil,&
            &'" file="',&
            &tabprofil(obs_rel%profil)%nomfic(1:w),&
            &'" factor="',&
            &obs_rel%Cf,&
            &'">'
            
            nprofil = obs_Rel%profil
        end if 
        
        sigma = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)                      
        write(75,'(A,A,A,A,A,F9.3,A,F9.3,A)')&
        &'      <grav_diff AR="',&
        &obs_rel%nomsta_AR(1:len_trim(obs_rel%nomsta_AR)),&
        &'" AV="',obs_rel%nomsta_AV(1:len_trim(obs_rel%nomsta_AV)),&
        &'" dgrav="',obs_rel%grav_AR-obs_rel%grav_AV,&
        &'" sd="',sigma,&
        &'"/>'
        
    end do
    write(75,*)'    </profile>'
    write(75,*)'  </gravity_differences>'

end subroutine Wxml_gravity_difference

subroutine Wxml_abs_observation()
    use param_data
    use raw_data
    implicit none
    integer k,w,nf
    
    write(75,*)'  <absolute_observations>'
	do k=1,param%Nb_obsAbs
	    nf = TabObsAbs(k)%num_abs_file
	    w = len_trim(param%TabDataFic(nf)%nom)
	    
	    write(75,'(A,A,A,F11.3,A,F6.3,A,A,A)')&
	    &'    <abs_obs sta="',&
	    &TabObsAbs(k)%nomsta(1:len_trim(TabObsAbs(k)%nomsta)),&
	    &'" grav="',TabObsAbs(k)%grav,&
	    &'" sd="',TabObsAbs(k)%sd,&
	    &'" file="',param%TabDataFic(nf)%nom(1:w),&
	    &'"/>'
	end do
    write(75,*)'  </absolute_observations>'
end subroutine Wxml_abs_observation

subroutine Wxml_free_solution_header()
    implicit none
    write(75,*)'  <free_solution>' 
end subroutine Wxml_free_solution_header

subroutine Wxml_free_solution_footer()
    implicit none
    write(75,*)'  </free_solution>' 
end subroutine Wxml_free_solution_footer

subroutine Wxml_constrained_solution_header()
    implicit none
    write(75,*)'  <constrained_solution>' 
end subroutine Wxml_constrained_solution_header

subroutine Wxml_constrained_solution_footer()
    implicit none
    write(75,*)'  </constrained_solution>' 
end subroutine Wxml_constrained_solution_footer

subroutine Wxml_calib_solution_header()
    implicit none
    write(75,*)'  <calib_solution>' 
end subroutine Wxml_calib_solution_header

subroutine Wxml_calib_solution_footer()
    implicit none
    write(75,*)'  </calib_solution>' 
end subroutine Wxml_calib_solution_footer

subroutine Wxml_mc_param(MC)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(in):: MC 
    write(75,*)'    <mc_param>'
    write(75,*)'      <nb_sta>',MC%Nb_sta,'</nb_sta>'
    write(75,*)'      <nb_profile>',MC%Nb_profil,'</nb_profile>'
    write(75,*)'      <degre_t>',MC%degre_t,'</degre_t>'
    write(75,*)'      <degre_k>',MC%degre_k,'</degre_k>'
    write(75,*)'      <nb_inc>',MC%Nb_inc,'</nb_inc>'
    write(75,*)'      <nb_obsrel>',MC%Nb_obsRel,'</nb_obsrel>'
    write(75,*)'      <nb_obsabs>',MC%Nb_obsAbs,'</nb_obsabs>'
    write(75,*)'      <dof>',Mc%dof,'</dof>'  
    write(75,*)'    </mc_param>'   
end subroutine Wxml_mc_param

subroutine Wxml_gravity(MC)
    use str_const
    use param_data

    use MC_data
    implicit none
    
    type (Tmc), intent(in):: MC
    integer i
    
    write(75,*)'    <gravity>'
    DO I=1,MC%Nb_sta
        write(75,'(A,A,A,F16.3,A,F16.3,A)')&
        &'      <sta name="',&
        &MC%stat(i)(1:len_trim(MC%stat(i))),&
        &'" grav="',&
        &MC%X(i),&
        &'" sd="',&
        &MC%Sig(i),'" />'
    END DO
    write(75,*)'    </gravity>'    
end subroutine Wxml_gravity

subroutine Wxml_delta_gravity(MC)
    use str_const
    use param_data

    use MC_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i
   
	write(75,*)'    <delta_gravity>'
    DO I=1,MC%Nb_sta
        write(75,'(A,A,A,F16.3,A,F16.3,A,F16.3,A)')&
        &'      <sta name="',&
        &MC%stat(i)(1:len_trim(MC%stat(i))),&
        &'" grav="',&
        &MC%X(i),&
        &'" delta_grav="',&
        &MC%sol_sans_calib(i)-MC%X(i),&
        &'" sd="',&
        &MC%Sig(i),'" />'
    END DO
    write(75,*)'    </delta_gravity>'
end subroutine Wxml_delta_gravity

subroutine Wxml_calibration(MC)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,k,row,n
    
    
    row = MC%Nb_sta + (MC%Nb_profil) * (MC%degre_k + MC%degre_t)
    
    if (MC%ngravi_Cal==0) then 
        return
    end if
    
    write(75,*)'    <final_calibration>'
    do k=1,ngravimeter    
        if (TabGravi(k)%Estimate) then
            row = row + 1
            !write(75,*)'		<gravimeter>'
    		!write(75,*)'			<serial>',TabGravi(k)%Serial,'</serial>'
    		!write(75,*)'			<n>',TabGravi(k)%N,'</n>'
    		!write(75,*)'			<factor>',TabGravi(k)%Cf,'</factor>'
    		!write(75,*)'			<estimated>Y</estimated>'
			!write(75,*)'		</gravimeter>'
			
			write(75,'(A,A,A,A,A,F16.8,A,F16.8,A)')'		<gravimeter serial="',&
			&TabGravi(k)%serial,&
			&'" n="',TabGravi(k)%N,&
			&'" factor="',MC%X0(row),&
			&'" sd="',MC%Sig(row),'" />'            
        end if
    end do
    write(75,*)'    </final_calibration>'  
end subroutine Wxml_calibration

subroutine Wxml_20_biggest_residual(MC,mode)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(inout):: MC
    integer mode
    real*8 resNorm,resnorm2
    integer i,nb_oa,w,k,nobs
    character (len=40) profile
    character (len=40) chfmt
    character (len=255) profile_abs
    
    if (mode==1) then
        nb_oa = 0;
    else
        nb_oa = MC%nb_obsAbs
    end if
    
    write(75,*)'    <biggest_std_residuals>'  
       
    nobs = MC%Nb_obsRel+Nb_oa
    if (nobs >= 20) then
        nobs = 19
    end if 
    
    do i=MC%Nb_obsRel+Nb_oa,MC%Nb_obsRel+Nb_oa-nobs,-1
        if (param%type_resid) then
            resNorm = TabRes_std_sort(i)%std_res
        else
            resNorm = TabRes_std_sort(i)%Norm_res
        end if
        if (TabRes_std_sort(i)%abs) then
                 
            write(75,*)&
            &'      <residual AR="',&
            &TabRes_std_sort(i)%ini(1:len_trim(TabRes_std_sort(i)%ini)),&
            '" AV="" grav="',&
            &TabRes_std_sort(i)%obs,&
            &'" resid="',TabRes_std_sort(i)%resid,&
            &'" resid_std="',resNorm,&
            &'" file="',&
            &param%TabDataFic(TabRes_std_sort(i)%profil)%nom,'" />'  
            
        else
        
            k = TabRes_std_sort(i)%profil
            profile = TabProfil(k)%nomfic
            
            write(75,*)&
            &'      <residual AR="',&
            &TabRes_std_sort(i)%ini(1:len_trim(TabRes_std_sort(i)%ini)),&
            '" AV="',&
            &TabRes_std_sort(i)%fin(1:len_trim(TabRes_std_sort(i)%fin)),&
            &'" grav="',&
            &TabRes_std_sort(i)%obs,&
            &'" resid="',TabRes_std_sort(i)%resid,&
            &'" resid_std="',resNorm,&
            &'" file="',&
            &param%TabDataFic(TabRes_std_sort(i)%profil)%nom,'" />'  
            
        end if
    
    end do
    
 
    
    write(75,*)'    </biggest_std_residuals>' 
    write(75,*)'    <biggest_raw_residuals>'   

    nobs = MC%Nb_obsRel+Nb_oa
    if (nobs >= 20) then
        nobs = 19
    end if 
    
    do i=MC%Nb_obsRel+Nb_oa,MC%Nb_obsRel+Nb_oa-nobs,-1
        if (param%type_resid) then
            resNorm = TabRes_raw_sort(i)%std_res
        else
            resNorm = TabRes_raw_sort(i)%Norm_res
        end if
        if (TabRes_raw_sort(i)%abs) then
            
            write(75,*)&
            &'      <residual AR="',&
            &TabRes_raw_sort(i)%ini(1:len_trim(TabRes_raw_sort(i)%ini)),&
            '" AV="" grav="',&
            &TabRes_raw_sort(i)%obs,&
            &'" resid="',TabRes_raw_sort(i)%resid,&
            &'" resid_std="',resNorm,&
            &'" file="',&
            &param%TabDataFic(TabRes_raw_sort(i)%profil)%nom,'" />'  
            
        else
        
            k = TabRes_raw_sort(i)%profil
            profile = TabProfil(k)%nomfic
                    
            write(75,*)&
            &'      <residual AR="',&
            &TabRes_raw_sort(i)%ini(1:len_trim(TabRes_raw_sort(i)%ini)),&
            '" AV="',&
            &TabRes_raw_sort(i)%fin(1:len_trim(TabRes_raw_sort(i)%fin)),&
            &'" grav="',&
            &TabRes_std_sort(i)%obs,&
            &'" resid="',TabRes_raw_sort(i)%resid,&
            &'" resid_std="',resNorm,&
            &'" file="',&
            &param%TabDataFic(TabRes_raw_sort(i)%profil)%nom,'" />' 
           
        end if
    
    end do
    
    write(75,*)'    </biggest_raw_residuals>'  
end subroutine Wxml_20_biggest_residual

end module write_xml



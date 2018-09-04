module ecriture
! Ecriture dans le fichier résultat
contains

! Procedure qui permet de creer un dossier pour stocker les résultats du calcul
logical function Cree_dossier_resultat()
    use Portability_routines
    use param_data
	use sys_utils

    implicit none
    
    logical resultat
    integer(4) err
	integer(4) resultatI
    integer LENficout, len_s, len_dossier

    character (len=10) date_ , time_ , zone
    character (len=20) s 
	CHARACTER (len=80) CHFMT
    INTEGER DATE_TIME (8)
    
    CALL DATE_AND_TIME ( date_ , time_ , zone , DATE_TIME )
    write(s,'(A8,A1,A6)')date_,"_",time_
    len_s = len_trim(s)
    lenficout = len_trim(param%nomficout)
    len_dossier = len_s+1+LENficout

    param%dossier(1:len_dossier) = param%nomficout(1:LENficout)//"_"//s(1:len_s)
    
   	resultatI = mkdir(param%dossier(1:len_trim(param%dossier)))
    
    if (resultatI .eq. 0) then
        WRITE (CHFMT,500)'(A11,1x,A',len_dossier,',1x,A8)'
        500  FORMAT (A,I2,A) 
        if (param%lg=='F') then
            write(0,FMT=CHFMT)'Repertoire ',param%dossier,' cree'
        else
            write(0,FMT=CHFMT)'Directory ',param%dossier,' created'
        end if
    end if
    
    if (param%systeme == 'LINUX') then
       param%nomficout(1:2+len_s+1+LENficout+LENficout+1) = &
	    &'./'//param%nomficout(1:LENficout)//"_"//s(1:len_s)//param%dsep//param%nomficout(1:LENficout)
    else
       param%nomficout(1:len_s+1+LENficout+LENficout+1) = &
	    &param%nomficout(1:LENficout)//"_"//s(1:len_s)//param%dsep//param%nomficout(1:LENficout)	
    endif

    Cree_dossier_resultat = resultat
    return
end function Cree_dossier_resultat

subroutine W_calcul
use param_data
	implicit none
	call W_ligne()
	if (param%sparse==0) then
		write(67,*)'Calcul avec des matrices pleines'
	else if (param%sparse==1) then
		write(67,*)"Calcul à l'aide des matrices creuses"
	end if
end subroutine W_calcul

subroutine W_error
    write(0,*)'Usage: mcgravi config_file'
    return
end subroutine W_error

subroutine W_entete(num_vers)
    use param_data
    implicit none
  
    character (len=8) date
    character (len=10) heure
    character (len=40) num_vers
    call W_ligne()
    write(67,*)num_vers

    call date_and_time(date,heure)

    if (param%lg=='F') then
        write(67,*)''
        write(67,*)'Compensation des observations de gravimétrie absolue et relative'
        write(67,*)'Méthode des moindres carrés'
        write(67,*)'Beilin Jacques - 2004-08'
        write(67,*)'IGN - IPGP'
        call W_ligne()
        write(67,*)'Calcul effectué le ',date(7:8),'/',date(5:6),'/',date(1:4),' à ',heure(1:2),':',heure(3:4),':',heure(5:6)
    else
        write(67,*)''
        write(67,*)'Least squares software for absolute and relative gravity measurements'
        write(67,*)'Beilin Jacques - 2004-08'
        write(67,*)'IGN - IPGP'
        call W_ligne()
        write(67,*)'Process date : ',date(7:8),'/',date(5:6),'/',date(1:4),' à ',heure(1:2),':',heure(3:4),':',heure(5:6)
    end if

end subroutine W_entete

subroutine W_code_erreur(c)
    use param_data
    implicit none
    integer c

    if (param%lg=='F') then
        call W_ligne()
        write(67,*)'Calcul terminé avec erreur'
        write(67,'(a7,I3)')'Code : ',c
    else
        call W_ligne()
        write(67,*)'Abnormal termination'
        write(67,'(a7,I3)')'Code : ',c
    end if
end subroutine W_code_erreur

subroutine W_ligne()
    write(67,*)''
    write(67,*)'=============================================================================='
    write(67,*)''
end subroutine W_ligne

subroutine W_sigma_coeff()
    use param_data
    implicit none
    integer k
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Modification des écarts-types a priori sur les fichiers relatifs'
        write(67,'(1x,a10,f6.3)')'Facteur = ',param%sigma_factor
        write(67,'(1x,a16,f6.4)')'Terme additif = ',param%sigma_add 
    else
        write(67,*)'Constant and scale factor applied to SD or relative observations'
        write(67,'(1x,a15,f6.3)')'Scale factor = ',param%sigma_factor
        write(67,'(1x,a15,f6.4)')'Constant     = ',param%sigma_add 
    end if

end subroutine W_sigma_coeff

subroutine W_cal_gravi()
    use param_data
    use raw_data
    implicit none
    integer k
    ! Ecriture des observations dans le fichier résultat
    call W_ligne()

    if (param%lg=='F') then
        write(67,*)'Coefficients d''étalonnage des gravimètres '
        write(67,*)'Valeurs initiales '
        write(67,*)'Numero de série, numero dans CG3TOOL.init, Coefficient d''étalonnage'
        do k=1,ngravimeter
            if (TabGravi(k)%Estimate) then
                write(67,'(1x,A8,1x,A3,2x,F12.10,1x,A6)')TabGravi(k)%Serial, TabGravi(k)%N, TabGravi(k)%Cf,'estimé'
            else
                write(67,'(1x,A8,1x,A3,2x,F12.10,1x,A5)')TabGravi(k)%Serial, TabGravi(k)%N, TabGravi(k)%Cf,'connu'
            end if
        end do
    else
        write(67,*)'Relative gravimeters calibration factor '
        write(67,*)'Initial values '
        write(67,*)'Serial #, CG3TOOL.init #, Calibration factor'
        do k=1,ngravimeter
            if (TabGravi(k)%Estimate) then
                write(67,'(1x,A8,1x,A3,2x,F12.10,1x,A9)')TabGravi(k)%Serial, TabGravi(k)%N, TabGravi(k)%Cf,'Estimated'
            else
                write(67,'(1x,A8,1x,A3,2x,F12.10,1x,A5)')TabGravi(k)%Serial, TabGravi(k)%N, TabGravi(k)%Cf,'Known'
            end if
        end do
    end if
end subroutine W_cal_gravi

subroutine W_liste_nomficrel()
    use param_data
    implicit none
    integer k,LENrep, LENnomfic,w,z
    type (TDataFic) Dfic
    character (len=60) nom_complet,rep,nomfic
    character (len=35) ch
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Liste des fichiers d''observations relatives  '
        write(67,*)"Fichier, facteur, terme additif"
    else
        write(67,*)'Relative observation files'
        write(67,*)"File, Calibration factor, Additive constant"
    end if
    
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 0) then
        
            rep = Dfic%rep       
            LENrep = len_trim(rep)
            nomfic = Dfic%nom
            LENnomfic = len_trim(nomfic)
            nom_complet = ''        
            nom_complet(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic(1:LENnomfic)
            w = len_trim(nom_complet)
            z = 50 - w
            
            !WRITE (CHFMT,500) '(1X,',N,'F5.2)' 

			WRITE (CH,500) '(1x,I3,1x,A',w,',',z,'x,f6.1,1x,f6.3)'
			!write(0,*)CH 
            500  FORMAT (A,I2,A,I2,A) 
            WRITE (67,FMT=CH)k,nom_complet,Dfic%sigma_f,Dfic%sigma_a
            !write(67,55)nom_complet,Dfic%sigma_f,Dfic%sigma_a
            !55 format (1x,A<w>,<z>x,f6.1,1x,f6.3)
            
        end if
    end do
    write(67,*)' '
end subroutine W_liste_nomficrel

subroutine W_liste_nomficabs()
    use param_data
    implicit none
    integer k,w,z
    character (len=60) ch
    type (TDataFic) Dfic
    integer n_abs
    n_abs = 0
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)
        if (Dfic%typ == 1) then
            n_abs = n_abs + 1        
        end if
    end do
    if (n_abs == 0) then
        return
    endif

    if (param%lg=='F') then
        write(67,*)'Liste des fichiers d''observations absolues'
        write(67,*)"Fichier, facteur, terme additif"
    else
        write(67,*)'Absolute observation files'
        write(67,*)"File, Calibration factor, Additive constant"
    end if
    
    do k=1,param%nDataFic
    
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 1) then
            !write(67,'(x,A80)')Dfic%nom
            w = len_trim(Dfic%nom)
            z = 50 - w
            WRITE (CH,500) '(1x,A',w,',',z,'x,f6.1,1x,f6.3)' 
            500  FORMAT (A,I2,A,I2,A) 
            WRITE (67,FMT=CH)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a

            
            !write(67,55)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a
            !55 format (1x,A<w>,<z>x,f6.1,1x,f6.3)
            
        end if
        
    end do
    write(67,*)' '
end subroutine W_liste_nomficabs

subroutine W_liste_nomficA10()
    use param_data
    implicit none
    integer k,w,z
    type (TDataFic) Dfic
    character (len=60) ch
    integer n_A10
    n_A10 = 0
    do k=1,param%nDataFic
        Dfic = param%TabDataFic(k)
        if (Dfic%typ == 2) then
            n_A10 = n_A10 + 1        
        end if
    end do
    if (n_A10 == 0) then
        return
    endif
    if (param%lg=='F') then
        write(67,*)'Liste des fichiers d''observations de gravimètre A10'
        write(67,*)"Fichier, facteur, terme additif"
    else
        write(67,*)'MicroG-Lacoste A10 observation files'
        write(67,*)"File, Calibration factor, Additive constant"
    end if

    do k=1,param%nDataFic
    
        Dfic = param%TabDataFic(k)
        
        if (Dfic%typ == 2) then
            w = len_trim(Dfic%nom)
            z = 50 - w
            WRITE (CH,500) '(1x,A',w,',',z,'x,f6.1,1x,f6.3)' 
            500  FORMAT (A,I2,A,I2,A) 
            WRITE (67,FMT=CH)Dfic%nom,Dfic%sigma_f,Dfic%sigma_a
            
        end if
        
    end do
    write(67,*)' '
end subroutine W_liste_nomficA10

subroutine W_observation_rel()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil
    character (len=8) str_profile
    character (len=5) str_tide

    ! Ecriture des observations dans le fichier résultat
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Observations Relatives'
        write(67,*)'Nom, Pesanteur(mGal), SD(mGal), Temp(mK), t (jour decimal), Date, Heure'
        str_profile = 'Profil'
        str_tide = 'Marée'
    else
        write(67,*)'Relative observations'
        write(67,*)'Name, Gravity(mGal), SD(mGal), Temp(mK), t (decimal day), Date, Time'
        str_profile = 'Profile'
        str_tide = ' Tide'
    end if

    nprofil = 0
    do k=1,nTabObs
        obs=TabObs(k)
        !write(0,*)'Profil =  ',obs%profil
        if (obs%profil/=nprofil) then
            write(67,*)' '

            write(67,'(1x,A8,1x,I2,1x,A20,1x,A8,F10.6,A5,A3,A8)')str_profile,&
					&obs%profil,tabprofil(obs%profil)%nomfic,&
            	&' - Cf = ',obs%Cf,&
            	&str_tide,' : ',tabprofil(obs%profil)%Tide

            nprofil = obs%profil
        end if 
        
        write(67,'(1x,A8,1x,f9.3,1x,f6.3,1x,f9.4,1x,f5.2,1x,f9.5,1x,I6,1x,I6)')&
            &obs%nomsta,obs%grav,obs%sd,obs%grad,obs%tempK,obs%mjd,obs%date,obs%heure
    end do

end subroutine W_observation_rel

subroutine W_liste_station()
    use param_data
    use raw_data
    implicit none
    integer k

    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Liste des stations observées'
    else
        write(67,*)'Station list'
    end if
    
    do k=1,param%Nb_sta
        write(67,'(1x,A8)')stat(k)
    end do
end subroutine W_liste_station

subroutine W_liste_station2()
    use param_data
    use raw_data
    implicit none
    integer k

    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Liste des stations observées'
        write(67,*)'Nom, longitude, latitude, numero de l''inconnue'
    else
        write(67,*)'Station list'
        write(67,*)'Name, Longitude, Latitude, Unknown #'
    end if
    
    do k=1,NTabStation
        !write(67,'(x,A8,x,f14.8,x,f14.8)')TabStation(k).nomsta,TabStation(k).lon,TabStation(k).lat
        write(67,'(1x,A8,1x,f14.8,1x,f14.8,1x,I12)')TabStation(k)%nomsta,TabStation(k)%lon,TabStation(k)%lat,TabStation(k)%numsta

    end do
end subroutine W_liste_station2

subroutine W_difference_gravite()
    use param_data
    use raw_data
    implicit none
    integer k,nprofil
    real*8 sigma
    character (len=8) str_profile
    !Fichier log
    call W_ligne()
    nprofil = 0
    if (param%lg=='F') then
        write(67,*)'Différences de pesanteur'
        str_profile = 'Profil'
    else
        write(67,*)'Gravity différences'
        str_profile = 'Profile'
    end if

    do k=1,param%Nb_obsRel

        Obs_Rel=TabObsRel(k)
        if (obs_Rel%profil/=nprofil) then
            write(67,*)' '
            write(67,'(1x,A7,1x,I2,1x,A12,2x,A5,F10.8,2x,A5,F9.5,2x,A5,F5.2)')&
                &str_profile,obs_rel%profil,&
                &tabprofil(obs_rel%profil)%nomfic,&
                &'Cf = ',obs_rel%Cf,&
                &'t0 = ',obs_rel%mjd0,&
                &'K0 = ',obs_rel%temp_K0
            nprofil = obs_Rel%profil
        end if 
        
        sigma = SQRT(obs_rel%SD_AR**2+obs_rel%SD_AV**2)
        write(67,'(1x,A8,1x,A8,1x,f9.3,1x,f9.3,1x,f9.3,1x,f6.3,1x,F5.2,1x,f5.2,1x,F9.5,1x,F9.5)')&
                        &obs_rel%nomsta_AR,&
                        &obs_rel%nomsta_AV,&
                        &obs_rel%grav_AR-obs_rel%grav_AV,&
                        &obs_rel%grav_AR,&
                        &obs_rel%grav_AV,&
                        &sigma,&
                        &obs_rel%tempK_AR,&
                        &obs_rel%tempK_AV,&
                        &obs_rel%mjd_Ar,&
                        &obs_rel%mjd_AV
        
    end do

end subroutine W_difference_gravite

subroutine W_gravi_abs()
    use param_data
    use raw_data
    implicit none
    integer k

    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Observations absolues'
        write(67,*)'Nom, Pesanteur (mgal), SD (mgal)'
    else
        write(67,*)'Absolute observations'
        write(67,*)'Name, Gravity (mgal), SD (mgal)'
    end if
    
	do k=1,param%Nb_obsAbs
	    !write(67,'(1x,A8,1x,f11.3,1x,F6.3)')fixstn(k),fixgra(k),stdx(k)
	    write(67,'(1x,A8,1x,f11.3,1x,F6.3)')&
	    &TabObsAbs(k)%nomsta,&
	    &TabObsAbs(k)%grav,&
	    &TabObsAbs(k)%sd
	end do

end subroutine W_gravi_abs

subroutine W_sol_contrainte()
    use str_const
    use param_data
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)constraintF
    else
        write(67,*)constraintA
    end if

    !call W_ligne()
    if (param%lg=='F') then ; write(0,*)constraintF ; else ; write(0,*)constraintA ; end if
end subroutine W_sol_contrainte

subroutine W_simul()
    use str_const
    use param_data
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)simulF
    else
        write(67,*)simulA
    end if
    !call W_ligne()
    if (param%lg=='F') then ; write(0,*)simulF ; else ; write(0,*)simulA ; end if
end subroutine W_simul

subroutine W_sol_libre()
    use str_const
    use param_data
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)datumfreeF
    else
        write(67,*)datumfreeA
    end if
    call W_ligne()
    if (param%lg=='F') then ; write(0,*)datumfreeF ; else ; write(0,*)datumfreeA ; end if
    WRITE(67,*)'Information : le calcul en solution libre cale les gravités'
    WRITE(67,*)'sur le premier point du premier fichier d''observations absolues'
    !write(67,*)''
end subroutine W_sol_libre

subroutine W_sol_calib()
    use str_const
    use param_data
    call W_ligne()
    if (param%lg=='F') then
        WRITE(67,*)'Calcul avec estimation des calibrations'
    else
        WRITE(67,*)'Process with calibration estimation'
    end if
    
    write(67,*)''
end subroutine W_sol_calib

subroutine W_dof_null()
    use str_const
    use param_data
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)DOF_nullF
    else
        write(67,*)DOF_nullA
    end if
    call W_ligne()
    !if (param%lg=='F') then ; write(0,*)DOF_nullF ; else ; write(0,*)DOF_nullA ; end if
end subroutine W_dof_null

subroutine W_param(MC)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i
    
    call W_ligne()

    if (param%lg=='F') then
        write(67,103)'Nombre de stations                                : ',MC%Nb_sta
        write(67,103)'Nombre de profils                                 : ',MC%Nb_profil
        write(67,103)'Degre du polynome de derive liee au temps         : ',MC%degre_t
        write(67,103)'Degre du polynome de derive liee à la temperature : ',MC%degre_k
        write(67,*)''
        write(67,103)'Nombre d''inconnues                                : ',MC%Nb_inc
        write(67,*)''
        write(67,103)'Nombre d''observations relatives                   : ',MC%Nb_obsRel
        if (param%lfix) then 
            write(67,103)'Nombre d''observations absolues                    : ',MC%Nb_obsAbs
        end if 
        write(67,*)''                      
        write(67,103)'Degré de liberté du système                       : ',Mc%dof
        write(67,*)''
    else
        write(67,103)'Station #                                          : ',MC%Nb_sta
        write(67,103)'Profile #                                          : ',MC%Nb_profil
        write(67,103)'Time drift polynomial degree                       : ',MC%degre_t
        write(67,103)'Temperature drift polynomial degree                : ',MC%degre_k
        write(67,*)''
        write(67,103)'Unknown #                                          : ',MC%Nb_inc
        write(67,*)''
        write(67,103)'Relative observation #                             : ',MC%Nb_obsRel
        if (param%lfix) then 
        write(67,103)'Absolute observation #                             : ',MC%Nb_obsAbs
        end if 
        write(67,*)''                      
        write(67,103)'Degree Of Freedom                                  : ',Mc%dof
        write(67,*)''
    end if

    103 format(1x,a52,I4)
    
end subroutine W_param

subroutine W_gravity(MC,unite)
    use str_const
    use param_data
    use util_str
    use MC_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,unite

    write(67,*)''
    

    if (unite==67) then 
        if (param%lg=='F') then
            WRITE(unite,*)WgravF
            WRITE(unite,*)'Nom, Pesanteur compensee (mgal), SD (mgal)' 
        else
            WRITE(unite,*)WgravA
            WRITE(unite,*)'Name, Gravity (mgal), SD (mgal)' 
        end if
        if (param%lg=='F') then ; write(0,*)WgravF ; else ; write(0,*)WgravA ; end if
    end if
    !write(0,*)MC%sigma0
    DO I=1,MC%Nb_sta
        WRITE(unite,'(1x,a8,3X,F12.3,3x,f12.3)')MC%stat(i),MC%X(i),MC%Sig(i)
    END DO
    
    write(67,*)''
end subroutine W_gravity

subroutine W_Ecart(MC,unite)
    use str_const
    use param_data

    use MC_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,unite
    
    write(67,*)''
    if (unite==67) then 
        if (param%lg=='F') then
            WRITE(0,*)WgravF
            WRITE(unite,*)'Nom, Gravite compensee (mGal), Ecart avec la solution sans calibration (mGal), SD (mGal)' 
        else
            WRITE(0,*)WgravA
            WRITE(unite,*)'Name, Gravity (mgal), difference with fixed calibration solution (mGal), SD (mgal)' 
        end if
        
    end if
    !write(0,*)MC%sigma0
    DO I=1,MC%Nb_sta
        !WRITE(unite,'(1x,a8,3X,F12.3,3x,f9.3)')MC.stat(i),MC.X(i),DSQRT(MC.SX(i,i))*MC.sigma0
        WRITE(unite,'(1x,a8,3X,F12.3,3x,F12.3,3x,f9.3)')MC%stat(i),MC%X(i),MC%sol_sans_calib(i)-MC%X(i),MC%Sig(i)
    END DO
    
    write(67,*)''
end subroutine W_Ecart


subroutine W_drift(MC)
    use MC_data
    use param_data
    use raw_data
    implicit none
    type (Tmc), intent(in):: MC
    integer i,k,row
    character (len=7) :: str_profile
    character (len=7) :: str_degree

    if (param%lg=='F') then
        str_profile = 'Profil'
        str_degree = 'Degré'
    else
        str_profile = 'Profile'
        str_degree = 'Degree'
    end if
    
    if (param%drift_t==0 .and. param%drift_k==0) then 
        return
    end if
    ! Write drift parameters
    do k=1,MC%Nb_profil
        write(67,'(1x,A7,1x,I3,1x,A7,1x,A8)')str_profile,k,'gravi ',tabprofil(k)%serial

        ! dérive liée au temps
        if(param%drift_t.gt.0) then
            if (param%lg=='F') then
                write(67,*)'Temps'
            else
                write(67,*)'Time'
            end if

            row=MC%Nb_sta+(k-1)*(param%drift_t+param%drift_k)
            do i=1,param%drift_t
                row=row+1
                write(67,'(1x,A7,1x,I1,1x,2f7.3)')str_degree,i,MC%X(row),MC%Sig(row)
            end do
        end if

        ! dérive liée à la température
        if(param%drift_k.gt.0) then
            if (param%lg=='F') then
                write(67,*)'Température'
            else
                write(67,*)'Temperature'
            end if
            
            row=MC%Nb_sta+(k-1)*(param%drift_t+param%drift_k) + param%drift_t
            do i=1,param%drift_k
                row=row+1
                write(67,'(1x,A7,1x,I1,1x,2f7.3)')str_degree,i,MC%X(row),MC%Sig(row)
            end do
        end if

        write(67,*)' '
    end do

    write(67,*)''
    
end subroutine W_drift

subroutine W_calibration(MC)
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

    if (param%lg=='F') then
        write(67,*)'Coefficients de calibration'
    else
        write(67,*)'Calibration factors'
    end if
   
    do k=1,ngravimeter
    
        if (TabGravi(k)%Estimate) then
            row = row + 1
            write(67,'(A8,1x,A1,1x,f20.6,1x,f20.6)')TabGravi(k)%serial,TabGravi(k)%N,MC%X0(row),MC%Sig(row)
        end if

    end do

    write(67,*)''
    
end subroutine W_calibration

subroutine W_20_plus_gros_resid(MC,mode)
    use MC_data
    use param_data
    implicit none
    type (Tmc), intent(inout):: MC
    integer mode
    real*8 resNorm,resnorm2
    integer i,nb_oa
    
    if (mode==1) then
        nb_oa = 0;
    else
        nb_oa = MC%nb_obsAbs
    end if

    write(67,*)' '
    if (param%lg=='F') then
        if (param%type_resid) then
            write(67,*)'20 plus gros résidus standards' 
            write(67,*)'Point initial, Point final, observation (mgal), résidu (mgal), résidu standard' 
        else 
            write(67,*)'20 plus gros résidus normalisés'  
            write(67,*)'Point initial, Point final, observation (mgal), résidu (mgal), résidu normalisé' 
        end if
    else
        if (param%type_resid) then
            write(67,*)'20 biggest standard residuals' 
            write(67,*)'Station 1, Station 2, Observation (mgal), Residual (mgal), Standard residual' 
        else 
            write(67,*)'20 biggest standard residuals' 
            write(67,*)'Station 1, Station 2, Observation (mgal), Residual (mgal), Standard residual' 
        end if
    end if

    
    do i=MC%Nb_obsRel+Nb_oa,MC%Nb_obsRel+Nb_oa-20,-1
        if (param%type_resid) then
            resNorm = TabRes_std_sort(i)%std_res
        else
            resNorm = TabRes_std_sort(i)%Norm_res
        end if
        if (TabRes_std_sort(i)%abs) then
            write(67,'(1x,A8,10x,f16.3,1x,f8.3,1x,f8.3)')&
			&TabRes_std_sort(i)%ini,TabRes_std_sort(i)%obs,TabRes_std_sort(i)%resid, resNorm
        else
            write(67,'(1x,A8,1x,A8,1x,f16.3,1x,f8.3,1x,f8.3)')&
			&TabRes_std_sort(i)%ini,TabRes_std_sort(i)%fin,TabRes_std_sort(i)%obs,TabRes_std_sort(i)%resid, resNorm
        end if
    
    end do
        
end subroutine W_20_plus_gros_resid
    
subroutine W_comparaison()
    use MC_data
    use param_data
    implicit none
    integer i    
    call W_ligne()
    if (param%lg=='F') then
        write(67,*)'Comparaison des solutions contraintes et libres'
        call W_ligne()     
        WRITE(67,*)'C : solution contrainte (mgal)'
        WRITE(67,*)'L : solution libre (mgal)'
        WRITE(67,*)'C-L : solution contrainte - solution libre (mgal)'
        WRITE(67,*)
        WRITE(67,*)' Num  Nom           L           C           C-L ' 
    else
        write(67,*)'Free and constrained solutions comparison'
        call W_ligne()
        WRITE(67,*)'C : constrained solution (mgal)'
        WRITE(67,*)'L : free solution (mgal)'
        WRITE(67,*)'C-L : constrained solution - free solution (mgal)'
        WRITE(67,*)
        WRITE(67,*)' #    Name          L           C           C-L ' 
    end if

    WRITE(67,*)
    do I=1,Mcc%Nb_sta
        WRITE(67,'(I3,3x,a8,3X,F9.3,3x,f9.3,3x,f9.3)')I,MCc%stat(i),Mcf%X(i),Mcc%X(i),Mcc%X(i)-Mcf%X(i)
    end do
end subroutine W_comparaison

subroutine W_time(ch)

    use Portability_routines
    implicit none
    
    CHARACTER*8 HOUR
    character (len=100) :: ch, CHFMT
    integer width
    width = len_trim(ch)
    width = 8 + 3 + width
    WRITE (CHFMT,500) '(1x,A',width,')' 
    500  FORMAT (A,I2,A) 
    HOUR = heure()
    ch = hour // " : "//ch
    WRITE (0,FMT=CHFMT)ch
    
end subroutine W_time

! Module d'ecriture de fichiers r artificiels
subroutine creer_fic_r (MC)

    use param_data
    use MC_data
    use raw_data
    implicit none
    integer i,j,k,npts
    character (len=40) nom_fic_c_courant,nom_fic_r_courant
    type (Tmc), intent(in):: MC
    integer, dimension (20) :: tab_temp ! tableau temporaire servant à éviter d'écrire plusieurs fois le même point
    logical ok
    
    do i=1,nTabObs
        if (i==1) then
            nom_fic_c_courant = tabprofil(TabObs(i)%profil)%nomfic
            nom_fic_r_courant = nom_fic_c_courant(1:5)//"r"//nom_fic_c_courant(7:12)
            open (21,file=nom_fic_r_courant,action='write',status='replace',position='rewind')
            if (param%lg=='F') then
                write (21,*)'# Fichier de mesure r artificiellement cree par MCGravi'
            else
                write (21,*)'# r observation file created by MCGravi'
            end if
            
            npts=0
            tab_temp=0
        end if
        if (i>1) then
            if (TabObs(i)%profil /= TabObs(i-1)%profil) then
                do k=1,npts
                    write(21,'(1x,A8,3x,f15.4,3x,f15.4,3x,i1,3x,i1)') &
                       & MC%stat(tab_temp(k)),MC%X(tab_temp(k))-980000.0D0,MC%sig(tab_temp(k)),0,0
                end do
                close (21)
                nom_fic_c_courant = tabprofil(TabObs(i)%profil)%nomfic
                nom_fic_r_courant = nom_fic_c_courant(1:5)//"r"//nom_fic_c_courant(7:12)
                open (21,file=nom_fic_r_courant,action='write',status='replace',position='rewind')
                if (param%lg=='F') then
                    write (21,*)'# Fichier de mesure r artificiellement cree par MCGravi'
                else
                    write (21,*)'# r observation file created by MCGravi'
                end if
                npts=0
                tab_temp=0
            end if
        end if
        do j=1,MC%Nb_sta
            if (TabObs(i)%nomsta==MC%stat(j)) then
                if (npts==0) then
                    npts=npts+1
                    tab_temp(npts)=j
                end if
                ok=.true.
                do k=1,npts
                    if (j==tab_temp(k)) then
                        ok=.false.
                        exit
                    end if
                end do
                if (ok) then
                    npts=npts+1
                    tab_temp(npts)=j
                end if
            end if
        end do
        if (i==nTabObs) then
            do k=1,npts
                write(21,'(1x,A8,3x,f15.4,3x,f15.4,3x,i1,3x,i1)')&
                   & MC%stat(tab_temp(k)),MC%X(tab_temp(k))-980000.0D0,MC%sig(tab_temp(k)),0,0
            end do
        end if
    end do
    
    

end subroutine creer_fic_r

end module ecriture

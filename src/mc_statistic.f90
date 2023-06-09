module MC_statistic
! module de calcul des estimateurs statistiques 

implicit none

contains

! Procedure qui calcule les r�sidus standards ou normalis�s (au choix de l'utilisateur)
! et remplit le tableau des r�sidus 
integer function Calc_resid(MC,mode)

    use param_data
    use raw_data
    use MC_data
    use str_const
    use ecriture
    use MC_inversion
    use write_matr
    
    implicit none
    
    type (Tmc), intent(inout):: MC
    type (Tresid) tmp_resid
    integer mode, nobsa, nobsr, nobs, ninc, NvarV, ALLOC_ERR, i, j, k, l, code, hh,m
    
    real*8,allocatable,dimension(:) :: varV, C, Resid,sigmV
    real*8,allocatable,dimension(:,:)::mtmp
    
    real*8,allocatable,dimension(:,:)::Qll
    real*8,allocatable,dimension(:,:) :: correl ! vecteur des correlations calibration-derive
    character (len=100) :: ch
    real*8 varX, varY, covXY, CorrXY
    integer ncalib, abs_obs_index

    !ch = 'Debut Calc_resid'
    !call W_time(ch)
    
    ! affectation des nombres d'observations
    nobsr = MC%Nb_obsRel
    if (mode==1) then
        nobsa = 1;
    else
        nobsa = MC%nb_obsAbs
    end if

    nobs = nobsa + nobsr
    ninc = MC%Nb_inc
    
    if (allocated(tabresid)) deallocate(tabresid) 
    allocate (Tabresid(nobs))
    
    ! recherche des correlations calibration/derive
    if (allocated(correl)) deallocate(correl) 
    allocate (correl(ninc-MC%Nb_sta-MC%Nb_profil,MC%Nb_profil))
    l =0
    do i=MC%Nb_sta+MC%Nb_profil+1, ninc
            l = l+1
            k = 0
            do j=MC%Nb_sta+1,MC%Nb_sta+MC%Nb_profil
                k = k+1
                varX = MC%SX(j,j)
                varY = MC%SX(i,i)
                covXY = MC%SX(i,j)
                
                corrXY = covXY / (sqrt(varX * varY))
                !write(0,*)i, j, varX, VarY, covXY, corrXY 
                correl(l,k) = corrXY
            end do
    end do
    if (param%writemat) THEN 
        call write_oct_mat(correl,l,k,'corr','correlation.m')   
    END IF     
    if (allocated(correl)) deallocate(correl) 

    
    if (param%Type_resid) then 
    ! le calcul des r�sidus std est tr�s gourmand.
    ! On ne le fait que si c'est demand�
    ! Peut-�tre ajouter un test sur la taille de la matrice � inverser (nobx*nobs !!!)
        
        allocate (Qll(Nobs,Nobs) , STAT = ALLOC_ERR)
        allocate (mtmp(Nobs,Ninc) , STAT = ALLOC_ERR)
        allocate (sigmv(Nobs) , STAT = ALLOC_ERR)

        if (allocated(varV)) deallocate(varV)
        NvarV = (Nobs * (Nobs + 1)) / 2
        allocate (varV(NvarV), STAT = ALLOC_ERR)

        if (param%lg=='F') then
            ch = 'Calcul VCV sur les residus'
        else
            ch = 'Calculating residual VCV matrix'
        end if
    
        
        call W_time(ch)
    
        ch = 'matmul A*(AtPA)^-1'
        call W_time(ch)
    
        do i=1,nobs
            do j=1,Ninc
                mtmp(i,j) = 0.0D0
                do k=1,Ninc
                    mtmp(i,j) = mtmp(i,j) + MC%A(i,k) * MC%SX(k,j) 
                end do
            end do
        end do
    
        ch = 'matmul (A*(AtPA)^-1)*At'
        call W_time(ch)
    
        do i=1,nobs
            do j=1,nobs
                l = ((i * (i-1)) / 2) + j
                varV(l) = 0.0D0
                Qll(i,j) = 0.0D0
                do k=1,Ninc 
                    varV(l) = varV(l) + mtmp(i,k) * MC%A(j,k) 
                    Qll(i,j) = Qll(i,j) + mtmp(i,k) * MC%A(j,k)  
                end do
            end do
        end do
        
        do k=1,nobs 
            sigmv(k)= SQRT(Qll(k,k))
        end do
        
        hh=0
        do k=1,nobs
            do j=1,k
                hh=hh+1
                varV(hh)=Qll(k,j)
            end do
        end do
        
        if (param%writemat) call write_mat(Qll,Nobs,Nobs,'Matrice_QLL.txt')
        
        
        if (allocated(Qll)) deallocate(Qll)
        if (allocated(mtmp)) deallocate (mtmp)    
        if (allocated(resid)) deallocate(resid)        
         
        allocate (Resid(nobs)) 
    
        do i=1,nobs
            Resid(i) = MC%V(i)
        end do
    
        !ch = 'Calcul des residus std par Cholesky'
        !call W_time(ch)
    
        !if (allocated(C)) deallocate(C) 
        !allocate (C((Ninc+1)*Ninc/2), STAT = ALLOC_ERR)

        
        if (param%lg=='F') then
            ch = 'Impossible d''allouer la memoire pour la matrice C'
        else
            ch = 'C matrix : Unable to allocate memory'
        end if
    
        if (ALLOC_ERR>0) write(0,'(I4,1x,A40)')ALLOC_ERR,ch 
     
        !code =  solution2(VarV,Resid,Nobs,0,C)
        if (code == 110) then
            Calc_resid = code 
            return
        end if
        
        if (allocated(C)) deallocate(C)
        if (param%lg=='F') then
            ch = 'Attention, ecart-type trop petit'
        else
            ch = 'Warning : SD too small'
        end if

        do i=1,nobs
            Tabresid(i)%Sd_fin = MC%sigma0 * sigmv(i)
            if (Tabresid(i)%Sd_fin<1.0D-8) then
                write(0,*)ch,i
                Tabresid(i)%Sd_fin = 1.0D-6
                Calc_resid = 111
                return
            end if  
        end do 
        
    end if
    
    if (allocated(resid)) deallocate(resid) 
    
    !ch = 'Remplissage du tableau des residus'
    !call W_time(ch)

    do i=1,MC%Nb_obsRel

        Tabresid(i)%ini = MC%stat(MC%stp(i))
        Tabresid(i)%fin = MC%stat(MC%enp(i))
        Tabresid(i)%abs = .FALSE.
        Tabresid(i)%resid = MC%V(i)
        Tabresid(i)%profil = TabObsRel(i)%profil
        Tabresid(i)%obs = TabObsRel(i)%grav_AR - TabObsRel(i)%grav_AV
        Tabresid(i)%Sd_ini = 1.0D0 / SQRT(MC%P(i))
        if (Tabresid(i)%Sd_ini<1.0D-8) then
            Tabresid(i)%Sd_ini = 1.0D-6
            Calc_resid = 116
            return
        end if
        if (param%Type_resid) then
            Tabresid(i)%std_res = Tabresid(i)%resid / Tabresid(i)%Sd_fin
        else 
            Tabresid(i)%Norm_res = Tabresid(i)%resid / Tabresid(i)%Sd_ini ! la matrice de poids est diagonale ; on peut donc l'inverser simplement
        end if
    end do

    ! Test JBL pour verifier le comportement du r�sidu sur l'unique obs absolue dans le cas d'une solution libre
    !Tabresid(nobs)%resid = MC%V(nobs)
    !write(*,*)Tabresid(nobs)%resid 
    !    if (param%Type_resid) then
    !        Tabresid(nobs)%std_res = Tabresid(nobs)%resid / Tabresid(nobs)%Sd_fin
    !   else 
    !        Tabresid(nobs)%Norm_res = Tabresid(nobs)%resid / Tabresid(nobs)%Sd_ini ! la matrice de poids est diagonale ; on peut donc l'inverser simplement
    !    end if

    !ch = 'Fin mode=1'
    !call W_time(ch)
    
    if (param%mode>1) then
    do i=nobsr+1,nobs
    
        abs_obs_index = i - nobsr
        Tabresid(i)%ini = MC%STAT(MC%pos(i-MC%nb_obsrel))
        Tabresid(i)%fin = MC%STAT(MC%pos(i-MC%nb_obsrel))
        Tabresid(i)%abs = .TRUE.
        Tabresid(i)%resid = MC%V(i)
        !Tabresid(i)%profil = abs_obs_index
        Tabresid(i)%profil = TabObsAbs(abs_obs_index)%num_abs_file
        ! essai en vue de ramener le nom du fichier avec les observations
        !m = i-MC%nb_obsrel
        !Tabresid(i)%profil = m
        !Tabresid(i)%profil = fichier(i-MC%nb_obsrel)
        !write(0,*)"fichier ",Tabresid(i)%profil,m
        !Tabresid(i)%obs = MC%B(i)
        
        !Tabresid(i)%obs = fixgra(i-MC%nb_obsrel)
        Tabresid(i)%obs = TabObsAbs(i-MC%nb_obsrel)%grav
        
        Tabresid(i)%Sd_ini = 1.0D0 / SQRT(MC%P(i))
        if (Tabresid(i)%Sd_ini<1.0D-8) then
            Tabresid(i)%Sd_ini = 1.0D-6
            Calc_resid = 116
            return
        end if

        IF (param%Type_resid) THEN
            Tabresid(i)%std_res = MC%sigma0 * Tabresid(i)%resid / Tabresid(i)%Sd_fin
        ELSE
            Tabresid(i)%Norm_res = Tabresid(i)%resid / Tabresid(i)%Sd_ini
        ENDIF
        
    end do
    end if 

    !ch = 'Fin Calc_resid'
    !call W_time(ch)
    

    
end function Calc_resid



integer function statistique(MC,mode)

    use param_data
    use MC_data
    use str_const
    use ecriture
    use write_HTML
    
    implicit none

    type (Tmc), intent(inout):: MC
   
    real*8,allocatable,dimension(:,:)::mtmp,sigtmp
    real*8,allocatable,dimension(:)::sigmv 
    real*8,allocatable,dimension(:,:)::tA
    real*8 ski2,ski2c,tau
    real*8::ResNorm ,resnorm2
    real*8::inter,rnbar,buf
    real*8 resid_N_i, resid_N_j
    integer,dimension(18)::hist
    integer::mode,i,j,kk,L,k,nbar,classe,nb_oa, ALLOC_ERR, code
    character (len=100) :: ch

    character*6 test,result*15

    type (Tresid) tmp_resid
    
    !ch = 'Entree stat - apres declaration'
    !call W_time(ch)
    
    if (mode==1) then
        nb_oa = 0; 
        ! on force � 0 (il y en a une en fait) car dans le cas d'une solution libre 
        ! l'unique obs absolue sert � rendre le syst�me inversible
        ! par contre �a n'a pas vraiment de sens d'en tenir compte 
        ! d'un point de vue statistique
    else
        nb_oa = MC%nb_obsAbs
    end if
    
    statistique = 0
        
    ! Calcul des elements statistiques ********************************************

    code = Calc_resid(MC,mode)

    ! Test du khi2 ****************************************************************
    
    !ch = 'Test du khi2'
    !call W_time(ch)
    
    ski2 = DBLE(MC%dof) * MC%sigma0**2
    ! Perform global test, a priori variance of unit weight = 1
    !WRITE(*,*)'Entree critical_chi2 - avant declaration'

    call critical_chi2(param%siglevel,MC%dof,ski2c)
    if (ski2>ski2c)then
        test='REJECT'
    else
        test='ACCEPT'
    end if
    
    MC%crit_khi2 = ski2c 

    if (param%lg=='F') then
        write(67,'(A65,1x,f10.3)')'Racine carr�e de l''estimateur du facteur unitaire de variance : ',MC%sigma0
        WRITE(67,'(1x,A7,f10.2)')'Test : ',ski2
        WRITE(67,'(1x,A35,f10.2)')'Valeur critique du test du khi-2 : ',ski2c
        WRITE(67,*)'R�sultat du test : ',test
                
        WRITE(0,'(1x,A7,f10.2)')'Test : ',ski2
        WRITE(0,'(1x,A35,f10.2)')'Valeur critique du test du khi-2 : ',ski2c
        WRITE(0,*)'Resultat du test : ',test
    else
        write(67,'(1x,A23,f10.3)')'Sigma0               : ',MC%sigma0
        write(67,'(1x,A23,f10.2)')'khi-2 test           : ',ski2
        write(67,'(1x,A23,f10.2)')'khi-2 critical value : ',ski2c
        write(67,*)'Test result : ',test
                
        write(0,'(1x,A23,f10.2)') 'khi-2 test           : ',ski2
        write(0,'(1x,A23,f10.2)') 'khi-2 critical value : ',ski2c
        write(0,*)'Test result : ',test
    end if

    if (param%write_resid) then
        ! Detection of outliers (tau-test)
        ch = 'Tau-test'
        call W_time(ch)


        IF ( param%lfix .AND. param%mode .GT. 1 ) THEN
            CALL  TAURE(MC%Nb_obsRel+MC%Nb_obsAbs,MC%NB_obsRel+MC%Nb_obsAbs-MC%Nb_inc,param%siglevel,MC%crit_tau)
        ELSE
            CALL  TAURE(MC%Nb_obsRel,MC%Nb_obsRel-MC%Nb_inc+1,param%siglevel,MC%crit_tau)
        ENDIF

        ch = 'apres Tau-test'
        call W_time(ch)

        write(67,*)''
        write(67,*)''
        if (param%lg=='F') then
            write(67,*)'R�sultat du tau-test sur les observations relatives'
            write(67,'(1x,A18,f6.2)')'Valeur critique : ',MC%crit_tau
            write(67,*)' '
            write(67,*)'D�but, Fin, Observation (mgal), R�sidu (mgal), Test statistique, Statut'
        else
            write(67,*)'Relative observation tau-test'
            write(67,'(1x,A18,f6.2)')'Critical value : ',MC%crit_tau
            write(67,*)' '
            write(67,*)'Station 1, Station 2, Observation (mgal), Residual (mgal), Test, Status' 
        end if

        do k=1,MC%Nb_obsRel
            if (param%Type_resid) then 
                    ! Si on a choisi residus normalis�s, on fait le tau-test dessus, faute de mieux
                    tau = abs(Tabresid(k)%std_res) 
                else
                    tau = abs(Tabresid(k)%norm_res) 
            end if 
            if (tau>MC%crit_tau)then
                Tabresid(k)%tautest = .false.
                test='reject'
            else
                Tabresid(k)%tautest = .true.
                test='accept'
            end if
            if(tau .GT. 2.0D0) then
                result=test//' ******'
            else
                result=test
            end if
            
            if (param%write_only_failed_tau_test) then
                if (tau>MC%crit_tau) then
                     WRITE(67,'(1x,A8,1x,A8,1x,f9.3,1x,f9.3,1x,f9.3,1x,a15)')&
                     &Tabresid(k)%ini,Tabresid(k)%fin,Tabresid(k)%obs,&
                     &Tabresid(k)%resid,tau,result
                endif
            else
                 WRITE(67,'(1x,A8,1x,A8,1x,f9.3,1x,f9.3,1x,f9.3,1x,a15)')&
                 &Tabresid(k)%ini,Tabresid(k)%fin,Tabresid(k)%obs,&
                 &Tabresid(k)%resid,tau,result
            endif      
           
        end do
        write(67,*)' '

        if(param%lfix .and. param%mode>1 ) then
            if (param%lg=='F') then
                write(67,*)'R�sultat du tau-test sur les observations absolues'
                write(67,*)'Nom, Observation (mgal), R�sidu (mgal), Test statistique, Statut'
            else
                write(67,*)'Absolute observation tau-test'
                write(67,*)'Station, Observation (mgal), Residual (mgal), Test, Status'
            end if


            do k=MC%Nb_obsRel+1,MC%Nb_obsRel+Nb_oa
                !tau=abs(MC%V(k))/sqrt(sigmv(k))/MC%sigma0
                if (param%Type_resid) then 
                    tau = abs(Tabresid(k)%std_res) 
                else
                    tau = abs(Tabresid(k)%norm_res) 
                end if 
                if (tau>MC%crit_tau)then
                    Tabresid(k)%tautest = .false.
                    test='reject'
                else
                    Tabresid(k)%tautest = .true.  
                    test='accept'
                end if
                
                if (param%write_only_failed_tau_test) then
                    if (tau>MC%crit_tau) then
                        WRITE(67,'(1x,A8,1x,f12.3,1x,f9.3,1x,f9.3,1x,a6)')Tabresid(k)%ini,&
                        &Tabresid(k)%obs,Tabresid(k)%resid,tau,test
                    endif
                else
                    WRITE(67,'(1x,A8,1x,f12.3,1x,f9.3,1x,f9.3,1x,a6)')Tabresid(k)%ini,&
                    &Tabresid(k)%obs,Tabresid(k)%resid,tau,test
                endif      
             
            end do
        

        end if
    end if
   
    write(67,*)''
    
    ! Calcul de l'histogramme des r�sidus normalis�s **************************
    
    if (param%lg=='F') then
            write(67,'(1x,A25,I4,A9)')'Histogramme des r�sidus (',&
            &MC%Nb_obsRel+MC%Nb_obsabs,' r�sidus)'
    else
            write(67,'(1x,A11,I4,A11)')'Histogram (',&
            &MC%Nb_obsRel+MC%Nb_obsabs,' residuals)'
    end if
    
    !ch = 'Calcul des l''histogramme des residus'
    !call W_time(ch)

    do i=1,18
        MC%histo(i)=0.0D0
    end do

    do i=1,MC%Nb_obsRel+Nb_oa  
        !if (i .GT. 121) then
        !    write(*,*)i 
        !    if (param%type_resid) then 
        !       write(*,*)Tabresid(i)%std_res
        !    else
        !        write(*,*)Tabresid(i)%norm_res
        !    end if 
        !    write(*,*)i 
        !end if
        if (param%type_resid) then 
            ResNorm = Tabresid(i)%std_res
        else
            ResNorm = Tabresid(i)%norm_res
        end if
       
        classe = CEILING( 2.0D0 * ResNorm + 9.0D0 ) ! recherche de la classe

        if (classe < 1) classe = 1 ! on met dans la premi�re classe tout ce qui est <4*sigma
        if (classe > 18) classe = 18 ! on met dans la classe 18 tout ce qui est >4*sigma
        MC%histo(classe)=MC%histo(classe)+1.0D0  
    end do

    !ch = 'fin du remplissage de l''histogramme'
    !call W_time(ch)

    do i=-9,8 ! Conversion des valeurs de l'histogramme en valeurs dessinables
        !write(67,'(I3,x,I3,x,f5%0,x,f5%2)')i,i+10,histo(i+10),histo(i+10)/L  
        MC%histo(i+10)=MC%histo(i+10)/DBLE(MC%Nb_obsrel+Nb_oa)
        hist(i+10)=FLOOR(MC%histo(i+10)*160.0D0)
        if (hist(i+10)>47) hist(i+10)=47 ! on tronque le dessin � 47
    end do

    call draw_histo(hist)
    if (allocated(TabRes_std_sort)) deallocate(TabRes_std_sort) 

    allocate (TabRes_std_sort(MC%Nb_obsRel+Nb_oa))
    do i=1,MC%Nb_obsRel+Nb_oa
        TabRes_std_sort(i) = tabResid(i)
    end do

    
        
    ! Sorting standard residuals
    ! --------------------------   
    ch = 'Tri des residus std'
    !call W_time(ch)
    do i=1,MC%Nb_obsRel+Nb_oa

        do j = i+1,MC%Nb_obsRel+Nb_oa
        
            if (param%type_resid) then
                resid_N_i = abs(TabRes_std_sort(i)%std_res)
            else 
                resid_N_i = abs(TabRes_std_sort(i)%Norm_res)          
            end if
        
            if (param%type_resid) then
                resid_N_j = abs(TabRes_std_sort(j)%std_res)
            else 
                resid_N_j = abs(TabRes_std_sort(j)%Norm_res)          
            end if

            if (resid_N_i.GT.resid_N_j) then     
                tmp_resid = TabRes_std_sort(i)
                TabRes_std_sort(i) = TabRes_std_sort(j)
                TabRes_std_sort(j) = tmp_resid
            end if
        end do 
    end do 
          
    if (MC%Nb_obsRel+Nb_oa>20) then
        call W_20_plus_gros_resid(MC,mode)
        !call WHTML_20_plus_gros_resid(MC,mode)
    end if

    ! Sorting raw residuals
    ! ---------------------
    ch = 'Tri des residus bruts'
    !call W_time(ch)
    if (allocated(TabRes_raw_sort)) deallocate(TabRes_raw_sort) 

    allocate (TabRes_raw_sort(MC%Nb_obsRel+Nb_oa))
    do i=1,MC%Nb_obsRel+Nb_oa
        TabRes_raw_sort(i) = tabResid(i)
    end do
   
    do i=1,MC%Nb_obsRel+Nb_oa

        do j = i+1,MC%Nb_obsRel+Nb_oa
            resid_N_i = abs(TabRes_raw_sort(i)%resid)
            resid_N_j = abs(TabRes_raw_sort(j)%resid)

            if (resid_N_i.GT.resid_N_j) then     
                tmp_resid = TabRes_raw_sort(i)
                TabRes_raw_sort(i) = TabRes_raw_sort(j)
                TabRes_raw_sort(j) = tmp_resid
            end if
        end do 
    end do 
          
    if (MC%Nb_obsRel+Nb_oa>20) then
        !call W_20_plus_gros_resid(MC,mode)
        !call WHTML_20_plus_gros_resid(MC,mode)
    end if
    
    !deallocate (TabRes_std_sort)
    
    ch = 'Synthese des elements statistiques'
    !call W_time(ch)

        
    ! Synthese des �l�ments statistiques **************************************
    
    MC%vmax = tabresid(1)%resid
    MC%vmin = tabresid(1)%resid
    do j=1,MC%Nb_obsrel+Nb_oa
        if (abs(tabresid(j)%resid) > abs(MC%vmax)) MC%vmax =  tabresid(j)%resid  
        if (abs(tabresid(j)%resid) < abs(MC%vmin)) MC%vmin =  tabresid(j)%resid  
    end do
    
    MC%vmean = 0.0D0
    MC%vrms = 0.0D0
    do j=1,MC%Nb_obsrel+Nb_oa ! aller jusqu'aux obs absolues
        MC%vmean = MC%vmean + tabresid(j)%resid
        MC%vrms = MC%vrms + tabresid(j)%resid**2
    end do
    MC%vmean = MC%vmean / (DBLE(MC%Nb_obsrel)+DBLE(Nb_oa))
    MC%vrms = DSQRT(MC%vrms / (DBLE(MC%Nb_obsrel)+DBLE(Nb_oa)))
    
    WRITE(67,*)
    if (param%lg=='F') then
        write(67,101)'R�sidu maximum  :',MC%vmax
        write(67,101)'R�sidu minimum  :',MC%vmin
        write(67,101)'R�sidu moyen    :',MC%vmean
        write(67,101)'RMS des r�sidus :',MC%vrms
    else
        write(67,101)'Maximum residual :',MC%vmax
        write(67,101)'Minimum residual :',MC%vmin
        write(67,101)'Mean residual    :',MC%vmean
        write(67,101)'RMS of residuals :',MC%vrms
    end if


    MC%sgmax=MC%sig(1)
    MC%sgmin=MC%sig(1)
    do k=1,MC%Nb_sta
        if (MC%sig(k)<MC%sgmin) MC%sgmin = MC%sig(k) 
        if (MC%sig(k)>MC%sgmax) MC%sgmax = MC%sig(k)       
    end do

    MC%sgmean=0.0D0
    MC%sgrms=0.0D0
    do j=1,MC%Nb_sta
        MC%sgmean = MC%sgmean + MC%sig(j)
        MC%sgrms = MC%sgrms + MC%sig(j)**2
    end do
    MC%sgmean = MC%sgmean / DBLE(MC%Nb_sta)
    MC%sgrms = DSQRT(MC%sgrms / DBLE(MC%Nb_sta))
    

    WRITE(67,*)
    if (param%lg=='F') then
        write(67,*)'Inconnues de pesanteur'
        write(67,102)'Ecart-type maximum sur les inconnues   :',MC%sgmax
        write(67,102)'Ecart-type minimum sur les inconnues   :',MC%sgmin
        write(67,102)'Ecart-type moyen sur les inconnues     :',MC%sgmean
        write(67,102)'RMS de l''�cart-type sur les inconnues :',MC%sgrms
    else
        write(67,*)'Gravity unknowns'
        write(67,102)'Maximum SD on the unknowns             :',MC%sgmax
        write(67,102)'Minimum SD on the unknowns             :',MC%sgmin
        write(67,102)'Mean SD on the unknowns                :',MC%sgmean
        write(67,102)'RMS of SD on the unknowns              :',MC%sgrms
    end if

    if (MC%degre_t>0 .or. MC%degre_t>0) then
    
        MC%sdmax=MC%sig(MC%Nb_sta+1)
        MC%sdmin=MC%sig(MC%Nb_sta+1)
        do k=MC%Nb_sta+1, MC%NB_inc
            if (MC%sig(k)<MC%sdmin) MC%sdmin = MC%sig(k) 
            if (MC%sig(k)>MC%sdmax) MC%sdmax = MC%sig(k)       
        end do

        MC%sdmean=0.0D0
        MC%sdrms=0.0D0
        do j=MC%Nb_sta+1, MC%NB_inc
            MC%sdmean = MC%sdmean + MC%sig(j)
            MC%sdrms = MC%sdrms + MC%sig(j)**2
        end do
        MC%sdmean=MC%sdmean / ( DBLE(MC%Nb_inc) - DBLE(MC%Nb_sta))
        MC%sdrms=DSQRT(MC%sdrms / ( DBLE(MC%Nb_inc) - DBLE(MC%Nb_sta)))
        

        WRITE(67,*)
        if (param%lg=='F') then
            write(67,*)'Inconnues de d�rives'
            write(67,102)'Ecart-type maximum sur les inconnues   :',MC%sdmax
            write(67,102)'Ecart-type minimum sur les inconnues   :',MC%sdmin
            write(67,102)'Ecart-type moyen sur les inconnues     :',MC%sdmean
            write(67,102)'RMS de l''�cart-type sur les inconnues  :',MC%sdrms
        else
            write(67,*)'Drift unknowns'
            write(67,102)'Maximum SD on the unknowns             :',MC%sdmax
            write(67,102)'Minimum SD on the unknowns             :',MC%sdmin
            write(67,102)'Mean SD on the unknowns                :',MC%sdmean
            write(67,102)'RMS of SD on the unknowns              :',MC%sdrms
        end if

        
    end if
    101 format(1x,a18,f8.3)
    102 format(1x,a40,f8.3)
    
    !deallocate (tabresid)
    return

end function statistique


subroutine statistique_simul(MC)
! procedure de calcul des elements statistiques dans le cas d'une simulation

    use param_data
    use MC_data
    use str_const
    use ecriture
    
    implicit none

    type (Tmc), intent(inout):: MC
    integer::i,j,kk,L,k
    real*8::vmax,vmin,comax,comin,ResNorm ,resnorm2
    real*8::xrms,vrms,xbar,vbar,inter,rnbar
 
       
    if (param%lg=='F') then ; write(0,*)stat_simulF ; else ; write(0,*)stat_simulA ; end if
    ! Calcul des elements statistiques ********************************************
    
    
    
    allocate (MC%sig(MC%Nb_inc))
          
    ! Compute variance of estimated gravities
    do i=1,MC%Nb_inc
        MC%sig(i) = MC%sx(i,i) !* MC%sigma0**2
        !if (MC%sig(i)<0) write(0,*)'Negative variance : ',i
        MC%sig(i)=sqrt(MC%sig(i))
    end do
    
    DO I=1,MC%Nb_sta
    WRITE(67,'(1x,a8,3X,f9.3)')MC%stat(i),DSQRT(MC%SX(i,i))
    END DO

    comax=MC%sig(1)
    comin=MC%sig(1)
    do k=1,MC%Nb_sta
        if (MC%sig(k)<comin) comin = MC%sig(k) 
        if (MC%sig(k)>comax) comax = MC%sig(k)       
    end do

    xbar=0.0D0
    xrms=0.0D0
    do j=1,MC%Nb_sta
        xbar = xbar + MC%sig(j)
        xrms = xrms + MC%sig(j)**2
    end do
    xbar=xbar / DBLE(MC%Nb_sta)
    xrms=DSQRT(xrms / DBLE(MC%Nb_sta))
    
    WRITE(67,*)
    if (param%lg=='F') then
        write(67,*)'Inconnues de pesanteur'
        write(67,102)'Ecart-type maximum sur les inconnues   :',comax
        write(67,102)'Ecart-type minimum sur les inconnues   :',comin
        write(67,102)'Ecart-type moyen sur les inconnues     :',xbar
        write(67,102)'RMS de l''�cart-type sur les inconnues :',xrms
    else
        write(67,*)'Gravity unknowns'
        write(67,102)'Maximum SD on the unknowns             :',comax
        write(67,102)'Minimum SD on the unknowns             :',comin
        write(67,102)'Mean SD on the unknowns                :',xbar
        write(67,102)'RMS of SD on the unknowns              :',xrms
    end if
   
    if (MC%degre_t>0 .or. MC%degre_t>0) then
    
        comax=MC%sig(MC%Nb_sta+1)
        comin=MC%sig(MC%Nb_sta+1)
        do k=MC%Nb_sta+1, MC%NB_inc
            if (MC%sig(k)<comin) comin = MC%sig(k) 
            if (MC%sig(k)>comax) comax = MC%sig(k)       
        end do

        xbar=0.0D0
        xrms=0.0D0
        do j=MC%Nb_sta+1, MC%Nb_profil * (MC%degre_k + MC%degre_t)
            xbar = xbar + MC%sig(j)
            xrms = xrms + MC%sig(j)**2
        end do
        xbar=xbar / ( DBLE(MC%Nb_inc) - DBLE(MC%Nb_sta))
        xrms=DSQRT(xrms / ( DBLE(MC%Nb_inc) - DBLE(MC%Nb_sta)))
        
    WRITE(67,*)
    if (param%lg=='F') then
        write(67,*)'Inconnues de pesanteur'
        write(67,102)'Ecart-type maximum sur les inconnues   :',comax
        write(67,102)'Ecart-type minimum sur les inconnues   :',comin
        write(67,102)'Ecart-type moyen sur les inconnues     :',xbar
        write(67,102)'RMS de l''�cart-type sur les inconnues :',xrms
    else
        write(67,*)'Drift unknowns'
        write(67,102)'Maximum SD on the unknowns             :',comax
        write(67,102)'Minimum SD on the unknowns             :',comin
        write(67,102)'Mean SD on the unknowns                :',xbar
        write(67,102)'RMS of SD on the unknowns              :',xrms
    end if
        
    end if
    101 format(1x,a17,f8.3)
    102 format(1x,a40,f8.3)
    
    return

end subroutine statistique_simul

SUBROUTINE TAURE(NT,NU,ALPH,CRTAU)
! Subroutine to compute crtical tau value based on Pope (1976)
!Calcul de la valeur critique du Tau-test
!Algorithme de Pope 1976
!Code traduit � partir d'un code Fortran 77 �crit par C. Hwang  pour le programme Gravnet
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        integer NT,NU
        integer I,M,N
        
        !WRITE(67,*)'Entree TAURE - apres declaration'
        PI=4.D0*DATAN(1.D0)
        PD=2.D0/PI
        S=1.D0
        WNU=DBLE(NU)
        U=WNU-1.D0
        IF (U.EQ.0.D0) GOTO 72
        IF (ALPH.EQ.0.D0) GOTO 72
        IF (ALPH.LT.1.D0) GOTO 10
        CRTAU=0.D0
        RETURN
10      Q=DBLE(NT)
        IF (ALPH.GT.0.5D0) GOTO 19
        AA=ALPH/Q
        DELT=AA
        DO 18 I=1,100
        XI=DBLE(I)
        DELT=DELT*ALPH*((XI*Q-1.D0)/((XI+1.D0)*Q))
        IF (DELT.LT.1.D-14) GOTO 20
18      AA=AA+DELT
19      AA=1.D0-(1.D0-ALPH)**(1.D0/Q)
20      P=1.D0-AA
        IF (U.EQ.1.D0) GOTO 71
        F=1.3862943611199D0-2.D0*DLOG(AA)
        G=DSQRT(F)
        X=G-(2.515517D0+0.802853D0*G+0.010328D0*F)/(1.0D0+1.432788D0*G+F*(.189269D0+0.001308D0*G))
        Y=X**2
        A=X*(1.D0+Y)/4.D0
        B=X*(3.D0+Y*(16.D0+5.D0*Y))/96.D0
        C=X*(-15.D0+Y*(17.D0+Y*(19.D0+3.D0*Y)))/384.D0
        E=X*(-945.D0+Y*(-1920.D0+Y*(1482.D0+Y*(776.D0+79.D0*Y))))/92160.D0
        V=1.D0/U
        T=X+V*(A+V*(B+V*(C+E*V)))
        S=T/DSQRT(U+T**2)
        UM=U-1.D0
        DELL=1.D0
        DO 70 M=1,50
        H=1.D0-S**2
        R=0.D0
        IF (DMOD(U,2.D0).EQ.0.D0) GOTO 49
        DD=DSQRT(H)
        N=NINT(.5D0*UM) ! conversions via function INT ajout JBL 20100311
        DO 45 I=1,N
        Z=2.0D0*DBLE(I)
        R=R+DD
        D=DD
45      DD=DD*H*(Z/(Z+1.D0))
        R=PD*(R*S+DASIN(S))
        D=PD*D*UM
        GOTO 61
49      DD=1.D0
        N=NINT(.5D0*U)  ! conversions via function INT ajout JBL 20100311
        DO 55 I=1,N
        Z=2.0D0*DBLE(I)
        R=R+DD
        D=DD
55      DD=DD*H*((Z-1.D0)/Z)
        R=R*S
        D=D*UM
61      CONTINUE
        DEL=(P-R)/D
        IF (DABS(DEL).LT.1.D-8) GOTO 72
70      CONTINUE
        GOTO 72
71      S=DSIN(P/PD)
72      CRTAU=S*DSQRT(WNU)
        RETURN
END SUBROUTINE TAURE

subroutine critical_chi2(siglevel,n,chi2c)
! Compute crtical chi square value for global model test
! siglevel: significance level
! n: degree of freedom
! chi2c: critical chi square value
    implicit none

    integer::i,j,n
    real*8::siglevel,chi2c,c(3),d(3)

    double precision t,ct,dt,xc
    data c/2.515517D0,0.802853D0,0.010328D0/
    data d/1.432788D0,0.189269D0,0.001308D0/

    !WRITE(67,*)'Entree critical_chi2 - apres declaration'

    !t=Dsqrt(2*dlog((1/siglevel**2)))
    t=DSQRT(2.0D0 * DLOG((1.0D0/siglevel)))
     ct=c(1)+c(2)*t+c(3)*t**2
    dt=1.0D0+d(1)*t+d(2)*t**2+d(3)*t**3
    xc=(t-ct/dt)

     chi2c=DBLE(n) * (xc*DSQRT(2.d0/(9.d0*DBLE(n)))+1.0D0 - 2.d0/(9.d0*DBLE(n)))**3

    return
end subroutine critical_chi2

subroutine draw_histo(hist)
! dessin d'un histogramme des r�sidus
    use param_data
    use util_str
    
    implicit none
    integer,dimension(18)::hist
    character, dimension(50,75) :: dessin
    character (len=80) ligne,ch
    integer i,j,valeur,position
    real*8 pi,ex,x,eex,sqpi
    
    !WRITE(67,*)'Entree draw_histo - apres declaration'

    
    pi=4.0D0*datan(1.0D0)
    
    ! remplissage de l'histogramme
    
    dessin =' '
    do i=3,75
        if (mod(i+1,4)==0) then
            dessin(3,i)='+'
        else 
            dessin(3,i)='-'
        end if
    end do
    
    do i=4,47
        dessin(i,39)='|'
    end do
    
    do i=1,18
        do j=1,hist(i)-1
            dessin(j+3,3+(i-1)*4)='|'
            dessin(j+3,3+(i)*4)='|'
            dessin(j+3,4+(i-1)*4)=':'
            dessin(j+3,5+(i-1)*4)=':' 
            dessin(j+3,6+(i-1)*4)=':'        
        end do 
        dessin(3+hist(i),4+(i-1)*4)='-' 
        dessin(3+hist(i),5+(i-1)*4)='-' 
        dessin(3+hist(i),6+(i-1)*4)='-'  
        
        
        if (mod(i,2)==0) then
	        valeur = i/2 -5
	        position = 3 + (i-1)*4
	        dessin(2,position) = int2char(abs(valeur))
	        if (i<9) dessin(2,position-1) = '-'
        end if     
    end do   
        
    do i=1,5   
	!    dessin(3+8*i,3) = int2char(i)
	!    dessin(3+8*i,4) = '0'
	!    dessin(3+8*i,5) = '%'
	!    dessin(3+8*i,6) = ' '
	    dessin(3+8*i,7) = '-'
    end do

    if (param%lg=='F') then ; ch = 'fr�quence relative' ;  else ; ch = 'relative frequency' ; end if
    do i = 1,18
	    dessin(29-i,1) = ch(i:i)
    end do
    
    if (param%type_resid) then
        if (param%lg=='F') then ; ch = 'r�sidus standards ' ;  else  
                                  ch = 'Standard residuals' ; end if

        do i = 1,18
	        dessin(1,i+30) = ch(i:i)
        end do    
    else 
        if (param%lg=='F') then ; ch = 'r�sidus normalis�s' ;  else  
                                  ch = 'Standard residuals' ; end if
        do i = 1,18
	        dessin(1,i+30) = ch(i:i)
        end do
    end if
    
    do i = 18,60
        x = (DBLE(i)-39.0D0) / 8.0D0
        ex = -0.5D0 * (x**2)
        eex = dexp(ex)
        sqpi = DSQRT(2.0D0*pi)
	    valeur = int((eex / sqpi) * 80.0D0)
	    dessin(3+valeur,i)='*'
    end do

    
    ! dessin de l'histogramme
    do i=50,1,-1
        ligne=' '
        do j=1,75
            ligne(1:j)=ligne(1:j-1)//dessin(i,j)(1:1)
        end do
        write(67,'(2x,A75)')ligne(1:75)
    end do

end subroutine draw_histo

integer function solution2(A,X,N,mode,C)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C PROGRAM TO SOLVE FOR A LINEAR SYSTEM USING CHOLESKY DECOMPOSTION%
!C
!C    A %%% A psd matrix, or the normal matrix% On output, it
!c          becomes a lower triangular matrix from the Cholesky
!c          decomposition or its inverse, C
!C    X %%% on input it is the observation vector; on output
!c          it is the solution vector
!c    C %%% inverse of A
!c    work %% a work array with dim=N
!C    N %%% order of A
!c    mode %% 0, solve for x only
!c            1, invert A (the inverse is C) and solve for x
!c            2, invert A only% X is not used in this mode
!C
!C              CHEINWAY HWANG
!c 	            Feb 20, 1999
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      use param_data
      use str_const
      
      integer N
      real*8  A(*),SUM
      real*8, intent(out):: C(N*(N+1)/2)
      real*8, intent(inout):: X(N)
      integer order,nsing,mode,K,I,J,KK,II,IK,II1
      ! CHOLESKY DECOMPOSITION of A
      
      real*8,allocatable,dimension(:):: work
      if (allocated(work)) deallocate(work)
      allocate(work(N))
      
      nsing=0
      if(mode .LT. 0 .or. mode .GT. 3) stop 'incorrect mode'
      
      !write(0,*)'debut Cholesky'
      
      DO 1 K=1,N
        kk=k*(k-1)/2
        DO 2 I=1,K-1
            II=I*(I-1)/2
            SUM=0.0D0
            DO 3 J=1,I-1
            3    SUM=SUM+A(II+J)*A(KK+J)
            II1=KK+I
        2    A(II1)=(A(II1)-SUM)/A(II+I)
        SUM=0.0D0
        DO 4 J=1,K-1
        IK=KK+J
    4    SUM=SUM+A(IK)*A(IK)
        IK=KK+K
        A(IK)=A(IK)-SUM
        IF(A(IK) .LE. 0.0D0) THEN
            if (param%lg=='F') then 
                write(0,*)neg_diag_elemF,k,a(ik)
                write(67,*)neg_diag_elemF,k,a(ik) 
            else 
                write(0,*)neg_diag_elemA,k,a(ik) 
                write(67,*)neg_diag_elemA,k,a(ik)
            end if
            if (param%lg=='F') then 
                write(0,*)matrix_not_psdF
                write(67,*)matrix_not_psdF 
            else
                write(0,*)matrix_not_psdA
                write(67,*)matrix_not_psdA
            end if
            solution2 = 110
            return
        END IF
1     A(IK)=DSQRT(A(IK))
      if(mode.eq.0 .or.(mode.eq.1)) then
        call LTRISOL2(A,X,N)
        call UTRISOL2(A,X,N)
      end if
!c Invert A
      if((mode.eq.1) .or. (mode.eq.2)) then
        do order=1,n
            do i=1,n
                work(i)=0.0D0
            end do
            work(order)=1.0D0
            call LTRISOL2(A,work,N)
            Call UTRISOL2(A,work,N)
            do i=order,n
                ii=i*(i-1)/2
                c(ii+order)=work(i)
            end do
        end do
        do i=1,n*(n+1)/2
        a(i)=c(i)
        end do
      end if
!c      write(0,*)'Nsing=',nsing
      if (allocated(work)) deallocate(work)
      RETURN
END function solution2
        

SUBROUTINE LTRISOL2(A,X,N)
!C SUBROUTINE TO SOLVE A LINEAR SYSTEM% A IS A LOWER TRIANGULAR MATRIX%
!C INPUT :
!C        A ----- A NONSINGULAR LOWER TRIANGULAR MATRIX
!C        N ----- ORDER OF T
!C        X  ---- OBSERVATION VECTOR
!C OUTPUT :
!C        X ----- SOLUTION VECTOR
!C
       REAL*8 A(*),X(*)
       REAL*8 SUM
       integer N,K,I,II,J
       DO 1 I=1,N
            II=I*(I-1)/2
            SUM=0.D0
            DO 2 J=1,I-1
 2          SUM=SUM+A(II+J)*X(J)
1           X(I)=(X(I)-SUM)/A(II+I)
       RETURN
END SUBROUTINE LTRISOL2

SUBROUTINE UTRISOL2(A,X,N)
!C SUBROUTINE TO SOLVE A LINEAR SYSTEM% A IS AN UPPER
!C TRIANGULAR MATRIX FROM THE CHOLESKY DECOMPOSITION%
!C INPUT :
!C        A ----- A NONSINGULAR LOWER TRIANGULAR MATRIX
!C        N ----- ORDER OF A
!C        X  ---- OBSERVATION VECTOR
!C OUTPUT :
!C        X ----- SOLUTION VECTOR
!C
        integer N,K,I,J,II,nsta
       REAL*8 A(*),X(*)
       REAL*8 SUM
!C BACKWARD SUBSTITUTION
      DO J=N,1,-1
        SUM=0.D0
        DO I=J+1,N
            II=I*(I-1)/2
            SUM=SUM+A(II+J)*X(I)
        END DO
        nsta=J*(J+1)/2
        X(J)=(X(J)-SUM)/A(nsta)
      END DO
      RETURN
END SUBROUTINE UTRISOL2



end module MC_statistic

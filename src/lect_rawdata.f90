module lect_rawdata

implicit none

contains


integer function lect_ficCal_gravi(nomfic)
    
    use Raw_data
    use util_str
    use param_data
    
    implicit none
    character (len=255) nomfic
    integer nc,i,k,num
    character (len=255) line 
    character (len=255) tabline(20)
    logical valid
    real*8 Ck
    character (len=100) :: CHFMT
   
    valid = .true.
    ! ouverture du fichier de calibration des gravis
    WRITE (CHFMT,501) '(A9,A',len_trim(nomfic),')' 
    501 FORMAT (A,I2,A)
    if (param%verbose) write(0,FMT=CHFMT)' Loading ',nomfic

    !if (param%verbose) write(0,*)nomfic
    ! Modif slavoue 07/2017 : augmentation du nombre de gravimètres possibles
    ! on commence par compter le nombre de gravimètres pour allouer TabGravi avec le bon nombre
    open(14,file=nomfic)
    ngravimeter=0
    do while(.true.)
        read(14,'(A80)',end=669)line
        call decoupe_ligne(line,tabline,nc)
        if (nc==3) then ! on garde ce bloc pour conserver la compatibilité avec la version 1
            if  (strisnumber(tabline(3))) then
                ngravimeter=ngravimeter+1
            end if
        end if
        if (nc==4) then
            if  (strisnumber(tabline(3))) then
                ngravimeter=ngravimeter+1
            end if
        end if
    end do 
    669 continue
    close(14)
    write(0,*)'Number of gravimeters in calibration file :',ngravimeter
    
    open(14,file=nomfic)
    
    !501 write(0,'(A19,x,A12)')'file open failed : ',nomfic;stop
    
    
    
    ! on alloue avec le nombre de gravi comptés
    if (allocated(tabgravi)) deallocate(tabgravi)
    allocate (TabGravi(ngravimeter))
    
    ngravimeter=0
    
    do while(.true.)
        read(14,'(A80)',end=668)line

        call decoupe_ligne(line,tabline,nc)
        if (nc==3) then ! on garde ce bloc pour conserver la compatibilité avec la version 1
            if  (strisnumber(tabline(3))) then
                ngravimeter=ngravimeter+1
                TabGravi(ngravimeter)%Serial=tabline(1)
                if (strisnumber(tabline(2))) then
                    read(tabline(2),'(I8)')TabGravi(ngravimeter)%Num
                else
                    TabGravi(ngravimeter)%Num = ngravimeter
                end if
                read(tabline(2),'(A3)')TabGravi(ngravimeter)%N
                TabGravi(ngravimeter)%Cf = str2double(tabline(3))
                TabGravi(ngravimeter)%estimate = .false.   
            end if
        end if
        if (nc==4) then
            if  (strisnumber(tabline(3))) then
                ngravimeter=ngravimeter+1
                TabGravi(ngravimeter)%Serial=tabline(1)
                if (strisnumber(tabline(2))) then 
                    read(tabline(2),'(I8)')TabGravi(ngravimeter)%Num
                else
                    TabGravi(ngravimeter)%Num = ngravimeter
                end if
                read(tabline(2),'(A3)')TabGravi(ngravimeter)%N
                TabGravi(ngravimeter)%Cf = str2double(tabline(3))
                TabGravi(ngravimeter)%pos = 0
                if (tabline(4)=='Y') then
                    TabGravi(ngravimeter)%estimate = .true. 
                    param%ngravi_Cal = param%ngravi_Cal + 1
                else
                    TabGravi(ngravimeter)%estimate = .false.
                end if  
            end if
        end if
    end do
    668 continue
    
    close(14)

    if (ngravimeter==0) valid=.false.
    
    if (valid) then
        lect_ficCal_gravi=0 
    else 
        lect_ficCal_gravi=102
    end if
    !do k=1,ngravimeter
    !    write(0,*)TabGravi(k)%serial,TabGravi(k)%N,TabGravi(k)%Cf,TabGravi(k)%Estimate
    !end do
end function lect_ficCal_gravi


integer function lect_obsAbs()
    use Raw_data
    use util_str
    use param_data
    implicit none
    character (len=255) nomfic
    integer nc,i,k
    character (len=255) line
    character (len=255) tabline(20)
    logical valid
    integer Nb_obsAbs2
    real*8 sig_f,sig_a,sig, grav
    type (TDataFic) Dfic   
    character (len=100) :: CHFMT
 
    valid = .true.
    ! ouverture du fichier de calibration des gravis
    
    if (allocated(TabObsAbs)) deallocate(TabObsAbs)
    allocate (TabObsAbs(datablock))
    
    do k=1,param%nDatafic
    
       Dfic = param%TabDataFic(k)
       
        if (Dfic%typ == 1) then ! typ==1 fichier absolu
        sig_f = Dfic%sigma_f
        sig_a = Dfic%sigma_a
        nomfic = Dfic%rep(1:len_trim(Dfic%rep))//Dfic%nom(1:len_trim(Dfic%nom))
        WRITE (CHFMT,501) '(A9,A',len_trim(nomfic),')' 
        501 FORMAT (A,I2,A)
        if (param%verbose) write(0,FMT=CHFMT)' Loading ',nomfic
        if (param%verbose) write(0,*)nomfic
        open(11,file=nomfic)
        !504 write(0,'(A19,x,A12)')'file open failed : ',absfilename;stop
        Nb_obsAbs2=0
        do while(.true.)
            read(11,'(A80)',end=101)line
            call decoupe_ligne(line,tabline,nc)
            if (nc==3) then
                if (strisnumber(tabline(2)).and. strisnumber(tabline(3))) then

                    if (mod(param%Nb_obsAbs,datablock)==0) then
                        ! on atteint la fin du datablock, il faut réallouer de la place
                        
                        allocate (TabObsAbs2(param%Nb_obsAbs))
                        do i=1,param%Nb_obsAbs
                            TabObsAbs2(i)=TabObsAbs(i)
                        end do
                        deallocate (TabObsAbs)
                        allocate (TabObsAbs(param%Nb_obsAbs+datablock))
                        do i=1,param%Nb_obsAbs
                    		TabObsAbs(i)=TabObsAbs2(i)
                    	end do
                        deallocate (TabObsAbs2)
                        
                    end if
                   
                                           
                    param%Nb_obsAbs = param%Nb_obsAbs +1
                    Nb_obsAbs2 = Nb_obsAbs2 +1

                    ! Station name, gravity value and standard deviation
	                read(tabline(2),'(f20.8)')grav
	                read(tabline(3),'(f20.8)')sig
	                ! On n'applique que la repondération par fichier
	                sig = sig_f * sig
	                sig = sqrt(sig**2 + sig_a**2)
	                if(sig==0.0D0) sig=1.0D-20
	                
	                TabObsAbs(param%Nb_obsAbs)%nomsta = tabline(1)
					TabObsAbs(param%Nb_obsAbs)%numsta = 0
					TabObsAbs(param%Nb_obsAbs)%num_abs_file = k
					TabObsAbs(param%Nb_obsAbs)%grav = grav
					TabObsAbs(param%Nb_obsAbs)%sd = sig
	               
                end if
            end if
        end do
        101 continue

        close(11)
       end if
    end do

    if (allocated(pos)) deallocate(pos)
    allocate (pos(param%Nb_obsAbs)) ! on connait desormais la taille que devra avoir pos

    if (valid) then
        lect_obsAbs=0
    else 
        lect_obsAbs=103
    end if
    
end function  lect_obsAbs

integer function lect_obsA10()
    use Raw_data
    use util_str
    use param_data
    implicit none
    character (len=255) nomfic
    integer nc,i,k
    character (len=255) line
    character (len=255) tabline(20)
    logical valid
    integer Nb_obsAbs2
    character (len=100) :: CHFMT
    
    character (len=8) nomsta
    real(8) grav,sd,sig_f,sig_a
    type (TDataFic) Dfic
    
    valid = .true.
          
    do k=1,param%NDataFic
        
      Dfic = param%TabDataFic(k) 
      if (Dfic%typ==2) then 
        nomfic = Dfic%rep(1:len_trim(Dfic%rep))//Dfic%nom(1:len_trim(Dfic%nom))

        WRITE (CHFMT,501) '(A9,A',len_trim(nomfic),')' 
        501 FORMAT (A,I2,A)
        if (param%verbose) write(0,FMT=CHFMT)' Loading ',nomfic

        !nomfic = Dfic%nom
        !write(0,*)nomfic
        sig_f = Dfic%sigma_f
        sig_a = Dfic%sigma_a 
        
        open(11,file=nomfic)
        
        !504 write(0,'(A19,x,A12)')'file open failed : ',absfilename;stop
        Nb_obsAbs2=0
        
        nomsta = ''
        grav = 0.0D0
        sd = 0.0D0
        
        read(11,'(A255)',end=102)line
        line = TRIM(line)
        if (line/='Micro-g Solutions g Processing Report'  .AND. line/='Micro-g LaCoste g Processing Report') then
            write(0,*)'A10 file : wrong header format in file ',nomfic 
            lect_obsA10=103
            return
        else
        
            do while(.true.)
                read(11,'(A255)',end=101)line

                IF (len_trim(line)>254) THEN
                    line = line(1:254)
                    !WRITE(0,*) line
                ENDIF
                
                IF (len(line) > 255) then
                    CYCLE
                endif
                
                call decoupe_ligne(line,tabline,nc)
                 
                if (nc>=2) then
                     !write(0,'(A79)')line,tabline(2)
                    !On cherche : 'Name: 1709'
                    if (nc==2 .and. tabline(1)=='Name:') then
                        nomsta = tabline(2)
                        !write(0,'(A30)')nomsta
                    end if      
                     
                    ! on cherche : 'Gravity:   980702580.54 uGal ou µGal'
                    if (nc==3 .and. tabline(1)=='Gravity:' .AND. strisnumber(tabline(2)) &
                        & .AND. ((tabline(3)=='uGal') .OR. (tabline(3)=='µGal')) ) then
                        
                        read(tabline(2),'(f20.8)')grav
                        grav = grav / 1000.0D0
     
                    end if
                        
                    ! on cherche : 'Set Scatter:  1.98 uGal'
                    if (nc==4 .and. tabline(1)=='Set' .and. tabline(2)=='Scatter:' &
                        &.and. strisnumber(tabline(3)) &
                        & .AND. ((tabline(4)=='uGal') .OR. (tabline(4)=='µGal')) ) then
                        read(tabline(3),'(f16.8)')sd
                        sd = sd / 1000.0D0
                        sd = sd * sig_f
                        sd = sqrt(sd**2 + sig_a**2)
                        !write(0,*)tabline(3)
                    end if
                    
                end if 
            end do
        
            101 continue
            if (nomsta/='' .and. grav>0.001D0 .and. sd>0.0000001D0) then
                    
                if (mod(param%Nb_obsAbs,datablock)==0) then
                    ! on atteint la fin du datablock, il faut réallouer de la place
                    
                    allocate (TabObsAbs2(param%Nb_obsAbs))
                    do i=1,param%Nb_obsAbs
                    	TabObsAbs2(i)=TabObsAbs(i)
                    end do
                    deallocate (TabObsAbs)
                    allocate (TabObsAbs(param%Nb_obsAbs+datablock))
                    do i=1,param%Nb_obsAbs
                    	TabObsAbs(i)=TabObsAbs2(i)
                    end do
                    deallocate (TabObsAbs2)
                    
                end if
                param%Nb_obsAbs = param%Nb_obsAbs +1
                Nb_obsAbs2 = Nb_obsAbs2 +1
                        
                ! Station name, gravity value and standard deviation
    	        if(sd==0.0D0) sd=1.0D-20
	                 
				TabObsAbs(param%Nb_obsAbs)%nomsta = nomsta
				TabObsAbs(param%Nb_obsAbs)%numsta = 0
				TabObsAbs(param%Nb_obsAbs)%num_abs_file = k
				TabObsAbs(param%Nb_obsAbs)%grav = grav
				TabObsAbs(param%Nb_obsAbs)%sd = sd
				
    	    else
    	        write(0,*)nomsta, grav,sd 
	        end if
        end if
        
        102 continue
        close(11)
        
       end if
    end do

    if (allocated(pos)) deallocate(pos)
    allocate (pos(param%Nb_obsAbs)) ! on connait desormais la taille que devra avoir pos

    if (valid) then
        lect_obsA10=0
    else 
        lect_obsA10=103
    end if
    
end function  lect_obsA10

integer function Lect_Coord()
    use Raw_data
    use util_str
    use param_data
    implicit none
    character (len=255) nomfic
    integer nc,i,k
    character (len=255) line
    character (len=255) tabline(20)
    logical valid,trouve
    integer Nb_coord2
    real*8 lon, lat
    character (len=100) :: CHFMT 
    
    character (len=8) nomsta
    type (TCoordFic) Cfic
    
    valid = .true.
          
    do k=1,param%NCoordFic
        
       Cfic = param%TabCoordFic(k) 
       nomfic = Cfic%rep(1:len_trim(Cfic%rep))//Cfic%nom(1:len_trim(Cfic%nom))
       WRITE (CHFMT,501) '(A9,A',len_trim(nomfic),')' 
       501 FORMAT (A,I2,A)
       if (param%verbose) write(0,FMT=CHFMT)' Loading ',nomfic
        
       open(11,file=nomfic)
       Nb_coord2=0
                      
       do while(.true.)
            read(11,'(A255)',end=101)line
            call decoupe_ligne(line,tabline,nc)!______________________ATTENTION, Ces variables (line, tabline, nc) ne sont
            !_________________________________________________________pas réinitialisées et les lignes blanches risquent de
            !_________________________________________________________poser problème!
            nomsta = ''
            lon = 0.0D0
            lat = 0.0D0
            if (nc .GT. 2) then
            
                valid = .false.
                if (strIsNumber(tabline(2)) .and. strIsNumber(tabline(3))) then
                    valid = .true.    
                end if 
                
                if (valid) then
                
                    Nomsta = tabline(1)
                    !read(tabline(2),'(f20.8)')lon
                    !lon = DNUM(tabline(2))
                    lon = str2double(tabline(2))
                    !read(tabline(3),'(f20.8)')lat
                    !lat = DNUM(tabline(3))
                    lat = str2double(tabline(3))
                    
                    trouve = .false.
                    do i=1,NTabStation
                        if (nomsta==TabStation(i)%Nomsta) trouve = .true. 
                        exit
                    end do
                    
                    if (.not. trouve) then
                    
                        if (mod(NTabStation,datablock)==0) then
                            allocate (TabStation2(NTabStation))
                    
                            do i=1,NTabStation
                                TabStation2(i)=TabStation(i)
                            end do 
                    
                            if (allocated(TabStation)) deallocate (TabStation)
                            allocate (TabStation(NTabStation+datablock))
                    
                            do i=1,NTabStation
                                TabStation(i)=TabStation2(i)
                            end do    
                    
                            if (allocated(TabStation2)) deallocate (TabStation2)  
                        end if
                        
                        NTabStation = NTabStation + 1
                        TabStation(NTabStation)%nomsta = nomsta
                        TabStation(NTabStation)%lon = lon
                        TabStation(NTabStation)%lat = lat
                   
                    end if
                     
                end if    
            end if 
        end do
        
        101 continue
    end do
        
    close(11)    
  

    if (valid) then
        Lect_Coord=0
    else 
        Lect_Coord=103
    end if
    
end function  Lect_Coord



! Lecture des fichiers d'observations relatives
integer function lect_obsrel()
    use Raw_data
    use util_str
    use param_data
    implicit none
    character (len=255) repficobsrel, nomficobsrel, nom_complet
    integer LENrepficobsrel, LENnomficobsrel
    
    integer nc,i,k,l,nstn2,j
    character (len=255) line
    character (len=255) tabline(20)
    logical valid,trouve,valid_format
    character (len=1) typefic ! fichier "c" ou fichier "o"
    real*8 Cf, sig_f, sig_a
    character (len=100) :: CHFMT 
    
    integer code
    type (TDataFic) Dfic
    
    valid = .true.
    code = 0
   
    SD_too_Small=.false.
    if (allocated(tabobs)) deallocate(tabobs) 
    allocate (TabObs(datablock))
    if (allocated(tabprofil)) deallocate(tabprofil) 
    allocate (Tabprofil(datablock))
    Nb_profil = 0
    
    lect_obsrel=0
    do l=1,param%NDataFic
        
       Dfic = param%TabDataFic(l)
       if (Dfic%typ==0) then ! typ==0 , fichier relatif
                
        nomficobsrel = Dfic%nom
        repficobsrel = Dfic%rep
        sig_f = Dfic%sigma_f
        sig_a = Dfic%sigma_a
        
       ! write(0,*)'>',repficobsrel(1:len_trim(repficobsrel)),'< >',nomficobsrel(1:len_trim(nomficobsrel)),'<'
        
        typefic = nomficobsrel(6:6)
        ! Lecture de fichier "c" (fichiers intermédiaires CGxTool) 
        ! On suppose des lors qu'on va trouver au même endroit un fichier "s" avec des hauteurs
        ! Cette vérification doit être effectuée par le programme appelant.
        if (typefic=='c' .or. typefic=='C') then
            code = lect_fic_c(nomficobsrel,repficobsrel,sig_f,sig_a)
            if (code /= 0) then
                write(0,*)'Invalid "c" observation file, file number ',l
                lect_obsrel=code
            end if
        
            ! Lecture fichiers o (brut CG3)
        else if (typefic=='o' .or. typefic=='O') then
            code = lect_fic_o(nomficobsrel,repficobsrel,sig_f,sig_a)
            if (code /= 0) then
                write(0,*)'Invalid "o" observation file, file number ',l
                lect_obsrel=code
            end if

        ! Lecture fichiers r (fichiers finaux CGxTool)
        else if (typefic=='r' .or. typefic=='R') then
            code = lect_fic_r(nomficobsrel,repficobsrel,sig_f,sig_a)
            if (code /= 0) then
                write(0,*)'Invalid "r" observation file, file number ',l
                lect_obsrel=code
                !return
            end if
        !___________________________
        else 
            write (0,*)nomficobsrel, 'Undefined file type'
              lect_obsrel=104
            return    
        end if
       end if
    end do
    
    call reduit_tabobs ()
    
    
end function  lect_obsRel


! Fonction de lecture des fichiers 'r'
integer function lect_fic_r(nomfic,rep,sf,sa)

    use Raw_data
    use util_str
    use param_data
    use Str_const

    implicit none
    character (len=255) ,intent(in) :: nomfic,rep
    character (len=255) nom_complet_r
    integer nc,i,k,l,nlect, LENrep, LENnomfic
    
    character (len=255) line,line2
    character (len=80) linelog
    character (len=8) tide
    character (len=255) tabline(20)
    logical valid,trouve,valid_format,exists,valid_format_r
    integer numgravi,code,ok
    character (len=3) Ngravi
    real*8 Cf,heure,site_corr,sf,sa,day,month, hour,gravi, maree
    type (Tprofil) , allocatable, dimension(:):: tabprofil2
    character (len=8) serial
    character (len=100) :: CHFMT 

    LENrep = len_trim(rep)
    LENnomfic = len_trim(nomfic)
    nom_complet_r = ''    
    nom_complet_r(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic(1:LENnomfic)
    valid = .true.
    SD_too_Small=.false.

    tide =  'cg3'

    WRITE (CHFMT,501) '(A9,A',len_trim(nom_complet_r),')' 
    501 FORMAT (A,I2,A)
   if (param%verbose) write(0,FMT=CHFMT)' Loading ',nom_complet_r    

    open(10,file=nom_complet_r, iostat=ok)
    if (ok/=0) then
        write(0,*)"probleme a l'ouverture de ",nom_complet_r
        stop
    end if
    
    if (param%calf) then
        ! on cherche le gravimetre et sa coNb_stante Cf
        !numgravi=char2int(nomfic(5:5))
        Ngravi = nomfic(5:5)//nomfic(7:8)
        trouve=.false.
        do k=1,ngravimeter
            !if (numgravi==TabGravi(k)%Num) then
            
            if (Ngravi .EQ. TabGravi(k)%N) then
                trouve=.true.
                Cf=TabGravi(k)%Cf
                serial = TabGravi(k)%serial
            end if
        end do
        if (.not. trouve) then
            Cf=1.0D0
            serial = 'inconnu'
            write(0,'(A5,A12,A14,A3,A31)')'file ',nomfic,' : gravimeter ',Ngravi,' not found. Cf set to 1.0000000'
        end if
    else
        Cf=1.0D0
    end if

    ! format d'une ligne du fichier c de CG3TOOL
    ! 700    449.523 0.003 100   0  -0.2   0.9 -1.60  0.072 167  473.3333 150604 075320   0  0.000     -17.9884
    
    ! format d'une ligne du fichier r de CG3TOOL
    ! STATION / VALUE (mGal) / ERROR (mGal) / REITERATION / REOCCUPATION
    ! 7108       0.0000          0.0141              6          1

    nlect=0
    
    do while(.true.)
        read(10,'(A255)',end=199)line

        if (line(1:1) .EQ. '#') then
            line2 = line(2:len_trim(line))
            !write(0,*)line2
            call decoupe_ligne(line2,tabline,nc)  
            if (nc>6) then
                if (tabline(1) .eq. 'CORRECTION') then
                    tide = tabline(6)
                    !write(0,*)tide
                end if
            end if       
        else 
        
            call decoupe_ligne(line,tabline,nc)
            
            
            if (nc==5) then
                valid_format=.true.
                do i=2,5
                    if (.not. strisnumber(tabline(i))) then
                        valid_format=.false.
                        write(0,*)tabline(i),"n'est pas un champ numerique",nom_complet_r
                        ! certains champs ne sont pas numériques !
                    end if
                end do 
                                
                if (valid_format) then
                    
                    ! on vérifie que l'on n'est pas arrivé à la fin du tableau tabobs et si c'est le cas on realloue un tableau avec datablock nouvelles lignes
                    if (mod(nTabObs,datablock)==0) then
                        allocate (TabObs2(nTabObs))
                        
                        do i=1,nTabObs
                            TabObs2(i)=TabObs(i)
                        end do 
                        
                        deallocate (TabObs)
                        allocate (TabObs(nTabObs+datablock))
                        
                        do i=1,nTabObs
                            TabObs(i)=TabObs2(i)
                        end do    
                        
                        deallocate (TabObs2)  
                    end if
                        
                    nTabObs = nTabObs + 1      
                    nlect = nlect +1

    	            obs%nomsta=tabline(1)
    	       
    	            ! Gestion de la reponderation globale et par fichier
    	            ! on commence par les termes multiplicatifs 
    	            ! en on additionne quadratiquement ensuite
    	            read(tabline(3),'(f20.5)')obs%sd
    	            obs%sd = obs%sd * param%sigma_factor * sf
    	            obs%sd = sqrt(obs%sd**2 + param%sigma_add**2 + sa**2)
                    
    	            if (obs%sd<SD_min) then
    	                write(0,'(1x,A11,1x,I4,1x,A12)')'Observation ',nTabObs,'SD too small' 
    	                SD_too_Small=.true.
    	            end if
    	            obs%tempK=0.0D0
    	            
    	            day=0.0D0
    	            hour=0.0D0
    	            obs%mjd=0.0D0
    	            obs%date=0
    	            obs%heure=0
    	            
    	            read(tabline(2),'(f20.8)')obs%grav
    	            maree=0.0D0
    	            obs%profil=Nb_profil+1
    	            obs%Cf=Cf
    	            obs%h=0.0D0
    	            

                    TabObs(nTabObs)=obs
                    end if
                end if
            end if
        end do
        199 continue

    close(10)
    
    if (nlect==0) then
        valid=.false.
        write(0,*)"Wrong file format"
    end if
    
    if (sd_too_small) then
        valid = .false.
        write(0,*)"SD too small"
    end if
    
    if (valid) then
        lect_fic_r = 0
        if (mod(Nb_profil,datablock)==0) then
            allocate (tabprofil2(Nb_profil)) 
            do i=1,Nb_profil
                tabprofil2(i)=tabprofil(i)
            end do 
            deallocate (tabprofil)
            allocate (tabprofil(Nb_profil+datablock))
            do i=1,Nb_profil
                tabprofil(i)=tabprofil2(i)
            end do    
            deallocate (tabprofil2)  
        end if
        
        Nb_profil = Nb_profil + 1
        tabprofil(Nb_profil)%num = Nb_profil
        tabprofil(Nb_profil)%nomfic = nomfic
        tabprofil(Nb_profil)%serial = serial
        tabprofil(Nb_profil)%Ngravi = Ngravi
        tabprofil(Nb_profil)%Tide = tide
        code = nomfic2jour(nomfic)
        tabprofil(Nb_profil)%jour = code
    else
        lect_fic_r = 104
    end if


end function lect_fic_r

!__________________________________________________________________________________________________________________________________
!__________________________________________________________________________________________________________________________________




integer function lect_fic_c(nomfic,rep,sf,sa)

! lecture du fichier c. On lit le fichier s en même temps 
    use Raw_data
    use util_str
    use param_data
    use Str_const

    implicit none
    character (len=255) ,intent(in) :: nomfic,rep
    character (len=255) nomfic_s, nom_complet_c, nom_complet_s
    integer nc,i,k,l,nlect, LENrep, LENnomfic
    
    character (len=255) line,line2
    character (len=5) s_heure
    character (len=8) tide
    character (len=80) linelog
    character (len=255) tabline(20)
    character (len=100) :: CHFMT 


    logical valid,trouve,valid_format,exists,valid_format_s
    !,sd_too_small
    logical fic_s_exist, hauteur_trouvee
    integer numgravi,code,hh
    character (len=3) Ngravi
    !character (len=1) typefic ! fichier "c" ou fichier "o"
    real*8 Cf,heure,site_corr,sf,sa,day,month, hour,gravi, maree, grad
    type (Tprofil) , allocatable, dimension(:):: tabprofil2
    character (len=8) serial

    LENrep = len_trim(rep)
    LENnomfic = len_trim(nomfic)
    nom_complet_c = ''    
    nom_complet_c(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic(1:LENnomfic)

    WRITE (CHFMT,501) '(A9,A',len_trim(nom_complet_c),')' 
    501 FORMAT (A,I2,A)
    if (param%verbose) write(0,FMT=CHFMT)' Loading ',nom_complet_c

    valid = .true.
    SD_too_Small=.false.
    fic_s_exist = .true.
    
    nomfic_s = nomfic
    nomfic_s(6:6)='s'
    nom_complet_s = ''    
    nom_complet_s(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic_s(1:LENnomfic)
    
    ! on cherche si le fichier "s" est présent
    ! S'il est manquant on signale mais on continue sans réduire
    INQUIRE (FILE = nom_complet_s, EXIST = exists)
    IF (.NOT. exists) THEN
        write(0,101)"Unable to find site file : ",nom_complet_s 
        101 format (1x,A27,1x,A30)
        fic_s_exist = .false.
        
        !if (lg=='F') then ; write(0,*)invalidsitefF ,nom_complet_s; else ; write(0,*)invalidsitefA ,nom_complet_s; end if
        !lect_fic_c = 109
        !return
    END IF

    open(10,file=nom_complet_c)
    if (fic_s_exist) open(17,file=nom_complet_s)
    tide = 'cg3'
    
    !505 write(0,'(A19,x,A12)')'file open failed : ',file1;stop
    if (param%calf) then
        ! on cherche le gravimetre et sa coNb_stante Cf
        Ngravi = nomfic(5:5)//nomfic(7:8)
        trouve=.false.
        do k=1,ngravimeter 
            if (Ngravi .EQ. TabGravi(k)%N) then
                trouve=.true.
                Cf=TabGravi(k)%Cf
                serial = TabGravi(k)%serial
                !write(0,*)'Gravi trouve : ', TabGravi(k)%serial,TabGravi(k)%N,TabGravi(k)%Cf,TabGravi(k)%Estimate
            end if
        end do
        if (.not. trouve) then
            Cf=1.0D0
            serial = 'inconnu'
            write(0,'(A5,A12,A14,A3,A31)')'file ',nomfic,' : gravimeter ',Ngravi,' not found. Cf set to 1.0000000'
        end if
    else
        Cf=1.0D0
    end if

    ! format d'une ligne du fichier c de CG3TOOL
    ! 700    449.523 0.003 100   0  -0.2   0.9 -1.60  0.072 167  473.3333 150604 075320   0  0.000     -17.9884
    nlect=0
    
    do while(.true.)
        read(10,'(A255)',end=199)line
        ! write(0,*)line(1:1)
        if (line(1:1) .EQ. '#') then
            line2 = line(2:len_trim(line))
            !write(0,*)line2
            call decoupe_ligne(line2,tabline,nc)  
            if (nc>6) then
                if (tabline(1) .eq. 'CORRECTION') then
                    tide = tabline(6)
                    !write(0,*)tide
                end if
            end if       
        else 
            call decoupe_ligne(line,tabline,nc)   
            if (nc==16) then
                valid_format=.true.
                do i=2,16
                    if (.not. strisnumber(tabline(i))) then
                        valid_format=.false.
                        ! certains champs ne sont pas numériques !
                    end if
                end do 
                                
                if (valid_format) then
                    
                    if (mod(nTabObs,datablock)==0) then
                        allocate (TabObs2(nTabObs))
                        
                        do i=1,nTabObs
                            TabObs2(i)=TabObs(i)
                        end do 
                        
                        deallocate (TabObs)
                        allocate (TabObs(nTabObs+datablock))
                        
                        do i=1,nTabObs
                            TabObs(i)=TabObs2(i)
                        end do    
                        
                        deallocate (TabObs2)  
                    end if
                        
                    nTabObs = nTabObs + 1      
                    nlect = nlect +1

    	            obs%nomsta=tabline(1)
    	       
    	            ! Gestion de la reponderation globale et par fichier
    	            ! on commence par les termes multiplicatifs 
    	            ! en on additionne quadratiquement ensuite
    	            read(tabline(3),'(f20.5)')obs%sd
    	            obs%sd = obs%sd * param%sigma_factor * sf
    	            obs%sd = sqrt(obs%sd**2 + param%sigma_add**2 + sa**2)  	         
    	            
    	            if (obs%sd<SD_min) then
    	                write(0,'(1x,A11,1x,I4,1x,A12)')'Observation ',nTabObs,'SD too small' 
    	                SD_too_Small=.true.
    	            end if
    	            read(tabline(8),'(f20.5)')obs%tempK
    	            
    	            !read(tabline(10),'(f20.5)')day ! modif C.champollion
                    read(tabline(10),'(f3.0)')day
    	            read(tabline(11),'(f20.5)')hour
    	            obs%mjd=day + hour / 1440.0D0
    	            read(tabline(12),'(I8)')obs%date
    	            read(tabline(13),'(I8)')obs%heure
    	            
    	            read(tabline(2),'(f20.8)')gravi
    	            read(tabline(9),'(f20.8)')maree
    	            obs%grav=gravi + maree ! on ajoute la marée calculée
                    ! il s'agit de la gravi corrigée de la dérive
    	            obs%profil=Nb_profil+1
    	            obs%Cf=Cf
    	            obs%h=0.0D0
						obs%grad=param%gradstd
    	            
    	            if (fic_s_exist) then
    	                !write(0,*)obs%grav
    	                valid_format_s = .true.
    	                hauteur_trouvee = .false.
    	                do while(.true.)
                      	read(17,'(A255)',end=198)line
                        ! 6902 16:24 0.196 9999.000 9999.000 9999.000
                        call decoupe_ligne(line,tabline,nc)
            
                        if (nc==6) then
                        
                        	do i=3,6
                        		if (.not. strisnumber(tabline(i))) then
                        			valid_format_s=.false.
                                 ! certains champs ne sont pas numériques !
                              end if
                           end do
                           s_heure = tabline(2)
                           s_heure = s_heure(1:2) // s_heure(4:5)
                        
                           if (.not. strisnumber(s_heure)) valid_format_s=.false.
                            
                           if (valid_format_s .and. obs%nomsta == tabline(1)) then
                           	!read(s_heure,'(f20.8)')heure
                              read(s_heure,'(I8)')hh
                           	                                    
                             	site_corr = 0.0D0
                              if (abs((obs%heure/100)-hh)<1) then
                              	read(tabline(3),'(f20.8)')obs%h
                               
                                 IF (obs%h .GE. 9999.0D0) THEN
                                 	obs%h = 0.0D0
                                 	write(0,'(1x,A30,1x,A80)')"Instrument height set to 0.0D0 : ",line
                                 ENDIF

                                 read(tabline(6),'(f20.8)')grad
											IF (grad .GE. 9998.0D0) THEN
                                 	site_corr = param%gradstd * obs%h 
												obs%grad=param%gradstd 
											ELSE
										   	site_corr = grad * obs%h
												obs%grad=grad 
											ENDIF


 
                              	hauteur_trouvee = .true.                                   
                              	obs%grav = obs%grav - site_corr ! réduction au niveau du repère       
                            	end if
                            end if      
                          end if
                        end do
                        198 continue

                        if (.not. hauteur_trouvee) then
                            write(0,'(1x,A30,1x,A8)')"Missing gravimeter height : ",obs%nomsta
                        end if
                        !write(0,*)obs%grav
                        rewind 17
        	        
    	                               
                    endif
                    TabObs(nTabObs)=obs
						  !   write(0,*)TabObs(nTabObs)%nomsta, TabObs(nTabObs)%grad
                    end if
                end if
            end if
        end do
        199 continue

    !write(0,'(A32,2x,A12,A3,I3)')'Number of observations in file ',file1,' : ',nstn2
    !Ntabobs = Ntabobs + nstn2

    close(10)
    if (fic_s_exist) close(17)
    
    if (nlect==0) then
        valid=.false. 
        write(0,*)nom_complet_c, 'Empty file'
    endif
    
    if (sd_too_small) then
        valid = .false.
        write(0,*)nom_complet_c, SD_too_small
    endif

    if (valid) then
        lect_fic_c = 0
        !  write(0,*)nom_complet_c, 'code = 0'
        if (mod(Nb_profil,datablock)==0) then
            allocate (tabprofil2(Nb_profil)) 
            do i=1,Nb_profil
                tabprofil2(i)=tabprofil(i)
            end do 
            deallocate (tabprofil)
            allocate (tabprofil(Nb_profil+datablock))
            do i=1,Nb_profil
                tabprofil(i)=tabprofil2(i)
            end do    
            deallocate (tabprofil2)  
        end if
        
        Nb_profil = Nb_profil + 1
        tabprofil(Nb_profil)%num = Nb_profil
        tabprofil(Nb_profil)%nomfic = nomfic
        tabprofil(Nb_profil)%serial = serial
        tabprofil(Nb_profil)%Ngravi = Ngravi
        tabprofil(Nb_profil)%Tide   = tide
        code = nomfic2jour(nomfic)
        tabprofil(Nb_profil)%jour = code

        !write(0,*)'Profil : ',Nb_profil
    else
        write(0,*)nom_complet_c, 'code = 104'
          lect_fic_c = 104
    end if

end function lect_fic_c

!Pour réduire les séries d'observations successives sur un même point
!Cette procédure fait une moyenne des valeurs de g pondérées par les écarts types
!Le champ mjd est aussi une moyenne pondérée mais le champ heure (hhmmss)reprend simplement la  première valeur de la série
subroutine reduit_tabobs ()
    use Raw_data
    use util_str
    use param_data
    implicit none
    integer i,ntabobsred,cpt
    real*8 somme_ponderee_grav,somme_sd,somme_sd_carre,somme_mjd,somme_tempK
    integer npts_consecutifs
    
    allocate (TabObs2(nTabObs))

    write(0,'(A,I2,A)')' Maximum duration for point occupation : ', FLOOR(param%delai_max),' min'
    
    somme_ponderee_grav=0.0D0
    somme_sd=0.0D0
    somme_sd_carre=0.0D0
    somme_mjd=0.0D0
    somme_tempK=0.0D0
    npts_consecutifs=0
   ! cpt=1
    
    ntabobsred=0
    do i=1,nTabObs
        !write (0,*) tabobs(i)%numsta
        
        ! à chaque tour on empile une observation de plus sauf si ...

        somme_ponderee_grav = somme_ponderee_grav + tabobs(i)%grav/tabobs(i)%sd
        somme_sd = somme_sd + 1.0D0/tabobs(i)%sd
        somme_mjd = somme_mjd + tabobs(i)%mjd/tabobs(i)%sd 
        somme_tempK = somme_tempK + tabobs(i)%tempK/tabobs(i)%sd
        npts_consecutifs = npts_consecutifs +1

        if (trim(tabobs(i)%nomsta) .NE. trim(tabobs(i+1)%nomsta)  & 
            & .OR. tabobs(i)%profil .NE. tabobs(i+1)%profil &
            & .OR. ( (tabobs(i+1)%mjd - tabobs(i)%mjd)*1440.0D0 .GT. param%delai_max ) &
            & .AND. trim(tabobs(i)%nomsta) .EQ. trim(tabobs(i+1)%nomsta)) then

        !IF (trim(tabobs(i)%nomsta) .NE. trim(tabobs(i+1)%nomsta) &
        !   & .OR. (tabobs(i)%profil .NE. tabobs(i+1)%profil) &
        !   & .OR. ( abs(tabobs(i)%mjd - tabobs(cpt)%mjd)*1440.D0 .GT. param%delai_max &
        !   & .AND. trim(tabobs(cpt)%nomsta) .EQ. trim(tabobs(i)%nomsta) ) )  THEN
            
            ! ... sauf si :
            ! - on change de profil
            ! - on change de point
            ! - delai_max est dépassé

            ! si ces conditions sont remplie, on ajoute un obs à TabObs2 ...
 
            ntabobsred=ntabobsred+1
            
            tabobs2(ntabobsred)%nomsta=tabobs(i)%nomsta
            tabobs2(ntabobsred)%grav=somme_ponderee_grav/somme_sd
            tabobs2(ntabobsred)%sd=(DBLE(npts_consecutifs)**0.5D0)/somme_sd 
            tabobs2(ntabobsred)%mjd=somme_mjd/somme_sd
            tabobs2(ntabobsred)%tempK=somme_tempK/somme_sd
            tabobs2(ntabobsred)%Cf=tabobs(i)%Cf
            tabobs2(ntabobsred)%date=tabobs(i)%date
            tabobs2(ntabobsred)%heure=tabobs(i)%heure
            tabobs2(ntabobsred)%profil=tabobs(i)%profil
            tabobs2(ntabobsred)%grad=tabobs(i)%grad
            tabobs2(ntabobsred)%h=tabobs(i)%h

            ! et on réinitialise la pile
            
            somme_ponderee_grav=0.0D0
            somme_sd=0.0D0
            somme_sd_carre=0.0D0
            somme_mjd=0.0D0
            somme_tempK=0.0D0
            npts_consecutifs=0
         !   cpt=i
            
        end if
    end do
     
    ! Transfert de TabObs2 dans TabObs 
    deallocate (TabObs)
    nTabObs=ntabobsred
    allocate (TabObs(ntabobsred))
    
    do i=1,nTabObs
        tabobs(i)%nomsta=tabobs2(i)%nomsta
        tabobs(i)%grav=tabobs2(i)%grav
        tabobs(i)%sd=tabobs2(i)%sd
        tabobs(i)%mjd=tabobs2(i)%mjd
        tabobs(i)%tempK=tabobs2(i)%tempK
        tabobs(i)%Cf=tabobs2(i)%Cf
        tabobs(i)%date=tabobs2(i)%date
        tabobs(i)%heure=tabobs2(i)%heure
        tabobs(i)%profil=tabobs2(i)%profil
        tabobs(i)%grad=tabobs2(i)%grad
        tabobs(i)%h=tabobs2(i)%h
    end do
    
    deallocate (TabObs2)
    
end subroutine reduit_tabobs

!_______________________________________________________________________________________________________

integer function lect_fic_o(nomfic,rep,sf,sa)
    use Raw_data
    use util_str
    use param_data
    implicit none
    character (len=255) nomfic,rep,nom_complet
    integer nc,i,k,l,long,j,jour,year,mja,lenrep,lennomfic
    character (len=255) line
    character (len=255) tabline(20)
    character (len=80) heure
    logical valid,trouve,valid_format !,sd_too_small
    integer numgravi
    character (len=3) Ngravi ! caractère destiné a faire le lien avec le fichier de calibration (destiné
    ! à remplacer numgravi
    character (len=1) typefic ! fichier "c" ou fichier "o"
    real*8 Cf,sf,sa,Ddum,Dmm, Dss, Dhh
    type (Tprofil) , allocatable, dimension(:):: tabprofil2
    character (len=8) serial
    integer pos1,pos2,Idum, Imm, Iss, Ihh
    character (len=2) hh,mm,ss
    character (len=100) :: CHFMT 

    
    type (Tobs) ,allocatable,dimension(:)::tabObs_N_trie,tabObs_N_trie2
    integer nTabObs_N_trie
    type (Tobs) Tmp_obs
    ! les observations des fichiers scintrex ne sont pas triées on les lit d'abord dans l'ordre et on trie après


    LENrep = len_trim(rep)
    LENnomfic = len_trim(nomfic)
    !write(0,*)rep,LENrep
    !write(0,*)nomfic,LENnomfic
        
    !write(0,*)repficobsrel, nomficobsrel ,LENrepficobsrel, LENnomficobsrel
    nom_complet(1:LENnomfic+LENrep) = rep(1:LENrep)//nomfic(1:LENnomfic)

    WRITE (CHFMT,501) '(A9,A',len_trim(nom_complet),')' 
    501 FORMAT (A,I2,A)
    if (param%verbose) write(0,FMT=CHFMT)' Loading ',nom_complet

    SD_too_Small=.false.
    valid = .true.
    
    if (strisnumber(nomfic(10:12))) then
        read(nomfic(10:12),'(I8)')jour
        !jour = JNUM(nomfic(10:12))
    else 
        jour = 0
    end if
    
    if (strisnumber(nomfic(7:8))) then
        read(nomfic(7:8),'(I8)')year
        !year = JNUM(nomfic(7:8))
    else 
        year = 0
    end if
    mja = 0
    if (jour/=0 .and. year/=0) then
        mja = mjd2mja(jour,year)
    end if
    open(10,file=nom_complet)
    !505 write(0,'(A19,x,A12)')'file open failed : ',file1;stop
    if (param%calf) then
        ! on cherche le gravimetre et sa coNb_stante Cf
        !numgravi=char2int(nomfic(5:5))
        Ngravi=nomfic(5:5)//nomfic(7:8)
        !write(0,*)numgravi
        trouve=.false.
        do k=1,ngravimeter
            !if (numgravi==TabGravi(k)%Num) then
            if (Ngravi .EQ. TabGravi(k)%N) then
                trouve=.true.
                Cf=TabGravi(k)%Cf
                serial = TabGravi(k)%serial
            end if
        end do
        if (.not. trouve) then
            Cf=1.0D0
            serial = 'inconnu'
            write(0,'(A5,A12,A14,A3,A31)')'file ',nomfic,' : gravimeter ',Ngravi,' not found. Cf set to 1.0000000'
        end if
    else
        Cf=1.0D0
    end if
    
    
!-------------------------------------------------------------------------------
!SCINTREX V7.2       AUTOGRAV / Field Mode            R7.21 REMOTE/Hires
!                                                             Ser No:       193.
!Line:      1.  Grid:      1.   Job:      1.  Date: 04/06/09  Operator:       1.
!
!GREF.:                   -6000. mGals           Tilt x sensit.:           ?5.47
!GCAL.1:                6015.752                 Tilt y sensit.:           ?0.81
!GCAL.2:                     0.0                 Deg.Lat.:                 43.94
!TEMPCO.:                 -0.1253mGal/mK         Deg.Long.:               -5.484
!Drift const.:                 0.                GMT Difference:            0.hr
!Drift Correction Start  Time: 07:09:35          Cal.after x samples:         12
!                        Date: 04/06/08          On-Line Tilt Corrected = "*"
!-------------------------------------------------------------------------------
!Station  Grav.     SD.   Tilt x  Tilt y    Temp.    E.T.C.  Dur  # Rej     Time
!   200. 466.0670* 0.015     0.3    -1.6    -0.80  -0.022    100     0  06:39:29
!   200. 466.0668* 0.014     0.9    -1.8    -0.84  -0.022    100     0  06:43:16
!   200. 466.0668* 0.013     1.2    -1.8    -0.85  -0.022    100     1  06:46:03

    Ntabobs_N_trie=0
    allocate (Tabobs_N_trie(datablock))
    
    do while(.true.)
        read(10,'(A255)',end=199)line
        
        call decoupe_ligne(line,tabline,nc)
        
        if (nc==10) then
            valid_format=.true.
            do i=2,9
                if (.not. strisnumber(tabline(i))) then
                    valid_format=.false.
                    
                    ! certains champs ne sont pas numériques !
                end if
                
            end do 
            !if (valid_format==.false.) write(0,*)line
                            
            if (valid_format) then
                
                if (mod(nTabObs_N_trie,datablock)==0) then
                    allocate (TabObs_N_trie2(nTabObs_N_trie))
                    
                    do i=1,nTabObs_N_trie
                        TabObs_N_trie2(i)=TabObs_N_trie(i)
                    end do 
                    
                    deallocate (TabObs_N_trie)
                    allocate (TabObs_N_trie(nTabObs_N_trie+datablock))
                    
                    do i=1,nTabObs_N_trie
                        TabObs_N_trie(i)=TabObs_N_trie2(i)
                    end do    
                    
                    deallocate (TabObs_N_trie2)  
                end if
                    
                nTabObs_N_trie = nTabObs_N_trie + 1      
                !nstn2 = nstn2 +1
                !   200. 466.0668* 0.013     1.2    -1.8    -0.85  -0.022    100     1  06:46:03
    
	            obs%nomsta=tabline(1)
	            long = len_trim(obs%nomsta)
	            if (obs%nomsta(long:long)=='.') then
	                obs%nomsta = obs%nomsta(1:long-1)
	            end if 
	            obs%date = mja
	            read(tabline(2),'(f20.5)')obs%grav   
	            
	            ! Gestion de la reponderation globale et par fichier
	            ! on commence par les termes multiplicatifs 
	            ! en on additionne quadratiquement ensuite
	            read(tabline(3),'(f20.5)')Ddum
	            read(tabline(8),'(I8)')Idum
	            obs%sd = DBLE(NINT(100.0D0*Ddum)) / 100.0D0 / SQRT(DBLE(IDum)) 
	            obs%sd = obs%sd * param%sigma_factor * sf
	            obs%sd = sqrt(obs%sd**2 + param%sigma_add**2 + sa**2)              
	         
	            if (obs%sd<SD_min) then
	                write(0,'(1x,A11,1x,I4,1x,A12)')'Observation ',nTabObs,'SD too small' 
	                SD_too_Small=.true.
	            end if
	            
	            read(tabline(6),'(f20.5)')obs%tempK
	            
	            heure = tabline(10)
	            
	            if (strisnumber(heure)) then
	                
	            else
	            
	            pos1 = scan(heure,':')
	            pos2 = scan(heure,':',back=.true.)
	            
	            if (pos1>0 .and. pos2>pos1)  then
	                hh = heure(1:pos1-1)
	                mm = heure(pos1+1:pos2-1)
	                ss = heure(pos2+1:len_trim(heure))
	                
	                if (strisnumber(hh) .and. strisnumber(mm) .and. strisnumber(ss)) then
	                    !heure = hh//mm//ss
	                    !write(67,*)heure
	                    read(ss,'(I8)')Iss
	                    read(mm,'(I8)')Imm
	                    read(hh,'(I8)')Ihh
	                    obs%heure= Iss + Imm * 100  + Ihh * 10000
                        ! Modif Cedric Champollion 20100311
	                    !read(ss,'(f20.8)')Dss
	                    !read(mm,'(f20.8)')Dmm
	                    !read(hh,'(f20.8)')Dhh
	                    read(ss,'(f2.0)')Dss
	                    read(mm,'(f2.0)')Dmm
	                    read(hh,'(f2.0)')Dhh
	                    obs%mjd = DBLE(jour) + ( 60.0D0*Dhh + Dmm + Dss / 60.0D0 ) / 1440.0D0
	                else
	                    valid_format = .false.    
	                end if 
	            else
	                valid_format = .false.
	            end if
	            
	            
	                valid_format = .false.
	            end if
	            
	            obs%profil=Nb_profil+1
	            obs%Cf=Cf
	            obs%h = 0.0D0 ! Pour l'instant on ne gère pas les hauteurs dans les fichiers "o". 
	            ! par la suite il sera préférable de les gérer sous forme d'info types "A: 0.168"
	            ! norme de saisie IGN (Gatta)
    	        
	            TabObs_N_trie(nTabObs_N_trie)=obs               
                
            end if
        end if
    end do
    199 continue
    close(10)
    
    if (nTabObs_N_trie==0) then
        valid=.false.
        write(0,*)'Empty file' 
    end if
    !write(0,'(A32,2x,A12,A3,I3)')'Number of observations in file ',file1,' : ',nstn2
    !nstn = nstn + nstn2
    
    ! Reste à trier les observations et à nettoyer les noms des stations
    do i=1,Ntabobs_N_trie 
        do j=i+1,Ntabobs_N_trie
            if (Tabobs_N_trie(i)%mjd>Tabobs_N_trie(j)%mjd) then
                tmp_obs = Tabobs_N_trie(i)
                Tabobs_N_trie(i) = Tabobs_N_trie(j)
                Tabobs_N_trie(j) = tmp_obs
            end if
        end do
    end do
    
    do i=1,Ntabobs_N_trie 
    
        if (mod(nTabObs,datablock)==0) then
            allocate (TabObs2(nTabObs))
            
            do j=1,nTabObs
                TabObs2(j)=TabObs(j)
            end do 
            
            deallocate (TabObs)
            allocate (TabObs(nTabObs+datablock))
            
            do j=1,nTabObs
                TabObs(j)=TabObs2(j)
            end do    
            
            deallocate (TabObs2)  
        end if
        
        nTabObs = nTabObs + 1    
        Tabobs(nTabObs) = Tabobs_N_trie(i)
    
    end do
    
    if (sd_too_small) then
        valid = .false.
        write(0,*)'SD too small'    
    end if
    
    if (valid) then
        lect_fic_o = 0
        if (mod(Nb_profil,datablock)==0) then
            allocate (tabprofil2(Nb_profil)) 
            do i=1,Nb_profil
                tabprofil2(i)=tabprofil(i)
            end do 
            deallocate (tabprofil)
            allocate (tabprofil(Nb_profil+datablock))
            do i=1,Nb_profil
                tabprofil(i)=tabprofil2(i)
            end do    
            deallocate (tabprofil2)  
        end if
        
        Nb_profil = Nb_profil + 1
        tabprofil(Nb_profil)%num    = Nb_profil
        tabprofil(Nb_profil)%nomfic = nomfic
        tabprofil(Nb_profil)%serial = serial
        tabprofil(Nb_profil)%Ngravi = Ngravi
        tabprofil(Nb_profil)%Tide   = 'cg3'
    else 
        lect_fic_o = 104
    end if

end function lect_fic_o



integer function cherche_station()
    use raw_data
    use param_data
    implicit none
    integer k,l
    logical trouve,valid

    valid=.true.
    if (allocated(sta)) deallocate(sta)
    allocate(sta(ntabobs)) ! initialise la liste des noms de stations (cette liste ne compte qu'une fois chaque point)
    Param%Nb_sta=1
    sta(1)=TabObs(1)%nomsta

    do k=2,ntabobs
        trouve=.false.

        do l=1,Param%Nb_sta
            if (TabObs(k)%nomsta==sta(l)) then
                trouve=.true.
            end if
        end do

        if (.not. trouve) then
            Param%Nb_sta=Param%Nb_sta+1
            sta(Param%Nb_sta)=TabObs(k)%nomsta
        end if
    end do
    if (allocated(stat)) deallocate(stat)
    allocate(stat(Param%Nb_sta))
    do l=1,Param%Nb_sta
        stat(l)=sta(l)
    end do

    deallocate(sta)
    
    if (valid) then
        cherche_station = 0
    else
        cherche_station = 105
    end if

end function cherche_station

! Cette procedure génère les différences de gravité entre observations sur un meme profil
integer function cree_difference_gravite()
    use raw_data
    use param_data
    implicit none
    integer k,l,i
    logical trouve,valid
    real*8 mjd0,temp_K0

    valid=.true.

    ! On prend les observations relatives par paire dans un même profil
    if (allocated(tabobsrel)) deallocate(tabobsrel)
    allocate (TabObsRel(datablock))
    nTabObsRel=0
    param%Nb_obsRel=0
    Obs_AR=TabObs(1)

    ! mjdo et temp_K0 servent de reference pour la modélisation de la dérive (géré par profil)
    mjd0 = obs_AR%mjd 
    temp_K0 = obs_AR%tempK

    do k=2,ntabobs

        ! Constitution des paires d'obs
        Obs_AV=TabObs(k)

        if (obs_AR%profil==obs_AV%profil) then !on verifie que les 2 obs sont sur le même profil
             
            if (mod(nTabObsRel,datablock)==0) then
                allocate (TabObsRel2(nTabObsRel))
                
                do i=1,nTabObsRel
                    TabObsRel2(i)=TabObsRel(i)
                end do 
                
                deallocate (TabObsRel)
                allocate (TabObsRel(nTabObsRel+datablock))
                
                do i=1,nTabObsRel
                    TabObsRel(i)=TabObsRel2(i)
                end do    
                
                deallocate (TabObsRel2)  
            end if
            
            nTabObsRel = nTabObsRel + 1

            
            obs_rel%nomsta_AR=obs_AR%nomsta
            obs_rel%numsta_AR=obs_AR%numsta
            obs_rel%nomsta_AV=obs_AV%nomsta
            obs_rel%numsta_AV=obs_AV%numsta
            obs_rel%grav_AR=obs_AR%grav
            obs_rel%grav_AV=obs_AV%grav
            obs_rel%sd_AR=obs_AR%sd
            obs_rel%sd_AV=obs_AV%sd
            obs_rel%tempK_AR=obs_AR%tempK
            obs_rel%tempK_AV=obs_AV%tempK
            obs_rel%mjd_AR=obs_AR%mjd
            obs_rel%mjd_AV=obs_AV%mjd
            obs_rel%profil=obs_AV%profil
            obs_rel%Cf=obs_AV%Cf
            obs_rel%mjd0=mjd0
            obs_rel%temp_K0=temp_K0
            
            TabObsRel(nTabObsRel)=Obs_Rel

        else
            ! date et température du début du profil
            ! utilisé pour le calcul des polynomes de dérive
            mjd0 = obs_AV%mjd
            temp_K0 = obs_AV%tempK

        end if

        obs_AR=obs_AV

    end do
    
    param%Nb_obsRel=nTabObsRel

    if (valid) then
        cree_difference_gravite = 0
    else
        cree_difference_gravite = 108
    end if

end function cree_difference_gravite

integer function libere_rawdata()
    use raw_data
    implicit none
   
    if (allocated(TabGravi)) deallocate (TabGravi)
    !if (allocated(fixstn)) deallocate (fixstn)
    !if (allocated(fixgra)) deallocate (fixgra)
    !if (allocated(stdx)) deallocate (stdx)
    if (allocated(pos)) deallocate (pos)
    !if (allocated(fichier)) deallocate (fichier)
    if (allocated(TabObsAbs)) deallocate (TabObsAbs)
    if (allocated(TabObsA10)) deallocate (TabObsA10)
    if (allocated(TabObs)) deallocate (TabObs)
    if (allocated(TabObsRel)) deallocate (TabObsRel)
    if (allocated(TabProfil)) deallocate (TabProfil)
    if (allocated(stp)) deallocate (stp)
    if (allocated(enp)) deallocate (enp)
    if (allocated(TabStation)) deallocate (TabStation)
    if (allocated(stat)) deallocate (stat)
    if (allocated(sta)) deallocate (sta)


    libere_rawdata = 0

end function libere_rawdata


integer function controle_num_point()
    use raw_data
    use param_data
    use util_str
    implicit none
    integer k,l,i,j
    logical trouve,valid
    character (len=80) c
   
    valid=.true.

    do i=1,param%Nb_obsAbs
        k=0
        do j=1,Param%Nb_sta
            if (TabObsAbs(i)%nomsta==stat(j)) then
                pos(i)=j
                k=k+1
            end if
        end do

        if(k==0) then
            write(0,*)'Station ',TabObsAbs(i)%nomsta,&
            &' (Absolute observation ',char(35),i,') ',&
            &' is not in the network' 
            controle_num_point = 112
        end if
        if (k.gt.1) then
            write(0,*)'Station name ',TabObsAbs(i)%nomsta,&
            &' is repeated ',k,' times'
            
            controle_num_point = 112
        end if
    end do
    
    if (controle_num_point == 112) return

    if (valid) then
        controle_num_point = 0
    end if

end function controle_num_point

integer function rempli_numpoint()

! Recherche du numéro du point dans le tableau stat ***************************
! Cette numérotation sert à retrouver les inconnues lors de l'inversion
    use raw_data
    use param_data
    implicit none
    integer k,l,m
    logical trouve,valid,mq_pts

    if (param%verbose) write(0,'(A24)')'Setting station numbers'
    
    valid = .true.

    do k=1,nTabObs
        obs=TabObs(k)
        do l=1,Param%Nb_sta
            if (stat(l)==TabObs(k)%nomsta) then
                TabObs(k)%numsta=l
            end if
        end do
    end do
    
    mq_pts = .false.
    
    allocate(TabStation2(Param%Nb_sta))
    do k=1,Param%Nb_sta
        trouve = .false.
        do l = 1,NTabStation
            if (stat(k)==TabStation(l)%nomsta) then
                TabStation2(k)%nomsta =TabStation(l)%nomsta
                TabStation2(k)%lon =TabStation(l)%lon
                TabStation2(k)%lat =TabStation(l)%lat 
                TabStation2(k)%numsta = k 
                trouve = .true.
                exit
            end if 
        end do
        if (.not. trouve) then
            m = len_trim(stat(k)) 
            write(0,'(1x,A23,1x,A8)')"Unable to find station ",stat(k)
            !write(0,'(1x,A5,1x,A<m>,1x,A10)')"Point",stat(k),"non trouve"
            mq_pts = .true.
        end if
    end do
    
    if (allocated(TabStation)) deallocate(TabStation)
    allocate(TabStation(Param%Nb_sta))
    do k=1,Param%Nb_sta
        TabStation(k)%nomsta =TabStation2(k)%nomsta
        TabStation(k)%lon =TabStation2(k)%lon
        TabStation(k)%lat =TabStation2(k)%lat 
        TabStation(k)%numsta =TabStation2(k)%numsta   
    end do
    deallocate(TabStation2)
    NTabStation = Param%Nb_sta
    
    rempli_numpoint = 0
    if (mq_pts) rempli_numpoint = 125
     
end function rempli_numpoint

integer function Rempli_pos_Inc_Calib()

! Affecte du numéro des inconnues de calibration *****************************
! Cette numérotation sert à retrouver les inconnues lors de l'inversion

! la position des inconnues de calibration est dupliquée dans les profils et dans les gravi pour 
! des raisons de rapidité d'accès à l'info
    use raw_data
    use param_data
    implicit none
    integer i,j,nb,ncal
    logical utilise

    if (param%verbose) write(0,'(A24)')'Setting unknown numbers'
    
    ! on se debarrasse des inconnues de calibration pour les gravimètre non utilisés
    do i=1,nGravimeter
        utilise = .false.
        do j=1,Nb_profil
            if (TabProfil(j)%serial==TabGravi(i)%Serial) then
                    utilise = .true.
            end if
        end do 
        
        if (.not. utilise) then
            TabGravi(i)%Estimate=.false.    
        end if       
    end do
    
    ! comptage des inconnues de calibration
    param%ngravi_Cal = 0
    do j=1,nGravimeter
        if (TabGravi(j)%Estimate) param%ngravi_Cal = param%ngravi_Cal + 1     
    end do
    write(0,*)'Nombre inconnues calib : ', param%ngravi_Cal
    ! position des inconnues de calibration
    nb = param%Nb_sta + Nb_profil * (param%drift_k + param%drift_t) 
    ncal = 0 
    do i=1,nGravimeter
        if (TabGravi(i)%Estimate) then 
            ncal = ncal + 1
        end if 
        do j=1,Nb_profil

            if ((TabGravi(i)%serial == TabProfil(j)%serial) .and. (TabGravi(i)%N .EQ. TabProfil(j)%Ngravi)) then
            !if (TabProfil(j)%serial==TabGravi(i)%Serial) then ! modif 2.3.18
                if (TabGravi(i)%Estimate) then 
                    TabProfil(j)%posInc = ncal + nb
                    TabGravi(i)%pos = TabProfil(j)%posInc       
                    !write(0,*)'tabgravi inconnu pos : ',TabGravi(i)%pos
                else
                    TabProfil(j)%posInc = 0
                end if
            end if
        end do
        
        
    end do
    
 
    Rempli_pos_Inc_Calib = 0
    
end function Rempli_pos_Inc_Calib


integer function mjd2mja(mjd,y)

    implicit none   
    integer , intent(in) :: mjd,y
    integer m, i, jour, jj, yy
    
    integer , dimension(12) :: mois 
    mois(1) = 31 ; mois(3) = 31 ; mois(5) = 31 ; mois(7) = 31 ; mois(8) = 31 ; mois(10) = 31 ; mois(12) = 31 
    mois(2) = 28 ; mois(4) = 30 ; mois(6) = 30 ; mois(9) = 30 ; mois(11) = 30
    
    yy = y
    if (y>70) then
        yy = 1900 + yy
    else 
        yy = yy + 2000
    end if
    if (mod(yy,100)==0 .and. mod(yy,400)/=0) mois(2) = 29
    jj = mjd
    do i=1,12
        jj = jj - mois(i)
        if (jj<0) then
            m = i
            jour = jj + mois(i)
            exit
        end if
    end do

    mjd2mja = jour * 10000 + m * 100  + y 
    
end function mjd2mja


end module lect_rawdata

module Lect_param
! module de lecture de paramètres de calcul dans un fichier 

    use util_str
    use Str_const
    implicit none
    
contains

INTEGER FUNCTION Write_config_file(nomfic,p) 

    USE param_data

    IMPLICIT NONE 
    CHARACTER (LEN=*) nomfic 
    TYPE (TParam), intent(inout) :: p
    INTEGER j

    OPEN(76,FILE=nomfic) ! Output file
    DO j=1,p%NTabParam
        write(76,*)p%TabParam(j)(1:len_trim(p%TabParam(j)))    
    ENDDO
    CLOSE(76)
    Write_config_file = 0

END FUNCTION Write_config_file    

integer function Read_config_file(nomfic,p) 

    use param_data
    use Raw_data
    use util_str
    use portability_routines

    implicit none                 
      
    character (len=*) nomfic 
    type (TParam) p
    integer nc,i,j,nfic_grav_abs,k, unit_nb, code
    character (len=255) line
    character (len=255) nom,rep
    character (len=255) tabline(20)
    logical valid, exists
    type (TDataFic), pointer , dimension(:) :: TabDataFic2
    type (TDataFic) Dfic
  
    ! definition des formats de lecture de nombres
    101 format (f16.8) 
    102 format (I8)
    103 format (G16.8)
      
    valid = .true.
    Read_config_file = 0

    INQUIRE (FILE = nomfic, EXIST = exists)
    IF (.NOT. exists) THEN
        WRITE(0,*) 'Unable to find ',nomfic    
        stop
    END IF

    call Init_param(p)
    
    p%systeme = OS_NAME_POSIX()

    if (p%systeme == 'LINUX') then
      p%ch_mkdir = 'mkdir   '
      p%ch_copy  = 'cp -f   '
      p%ch_move  = 'mv -f   '
      p%ch_rm    = 'rm -f   '
      p%dsep     = '/'
    else
      p%ch_mkdir = 'md      '
      p%ch_copy  = 'copy /Y '
      p%ch_move  = 'move /Y '
      p%ch_rm    = 'del /Q  '
      p%dsep     = '\'
    endif

    allocate(p%tabDataFic(datablock))
    allocate(p%tabCoordFic(datablock))
    
    allocate(p%TabParam(5000))

    write(0,*)'Reading ', nomfic(1:len_trim(nomfic))
    open(20,file=nomfic)     
    unit_nb = 500
    do while(.true.)
    
        read(20,'(A255)',end=668)line
        !write(0,*)line
        call decoupe_ligne(line,tabline,nc) 
        if (nc .GT. 1) then 
            if (tabline(1) .EQ. '@include') then
                INQUIRE (FILE = tabline(2), EXIST = exists)
                IF (exists) THEN
                    !write (0,*) 'WARNING INCLUDE',tabline(2)
                    unit_nb = unit_nb + 1
                    open(unit_nb,file=tabline(2))
                    do while(.true.)
                        read(unit_nb,'(A255)',end=669)line
                        !write(0,*)line
                        p%NTabParam = p%NTabParam + 1
                        p%TabParam(p%NTabParam) = line
                    end do
                    669 continue 
                    close(unit_nb)
                ELSE 
                    WRITE(0,*) 'Unable to open ',tabline(2)(1:len_trim(tabline(2)))
                END IF
                
                !write (0,*) 'WARNING INCLUDE',tabline(2)
            else
                p%NTabParam = p%NTabParam + 1
                p%TabParam(p%NTabParam) = line
            end if
        end if
    end do
    668 continue    
    close(20)  

    code = Parse_config_file(p)
                                         
    return                               
                                      
end function Read_config_file

integer function Parse_config_file(p)
    use param_data
    use Raw_data
    use util_str
    use portability_routines

    implicit none                 
      
    type (TParam) p
    integer nc,i,j,nfic_grav_abs,k
    character (len=255) line
    character (len=255) nom,rep
    character (len=255) tabline(20)
    logical valid, exists
    type (TDataFic), pointer , dimension(:) :: TabDataFic2
    type (TDataFic) Dfic
    Parse_config_file = 0

    ! definition des formats de lecture de nombres
    101 format (f16.8) 
    102 format (I8)
    103 format (G16.8)

    do j=1,p%NTabParam
        !write(0,*)p%TabParam(j)(1:len_trim(p%TabParam(j)))
        line = p%TabParam(j)
        call decoupe_ligne(line,tabline,nc)

        if (nc==2) then
        
            if ((tabline(1)=='DRIFT_T').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%drift_t
                !p%drift_t = JNUM(tabline(2))
                if (p%drift_t<0) then
                    write(0,*)'Time related drift. Polynomial degree must be >=0. But requested degree is:',p%drift_t
                    valid=.false.
                end if
                
            else if ((tabline(1)=='DRIFT_K').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%drift_k
                !p%drift_k = JNUM(tabline(2))
                if (p%drift_k<0) then
                    write(0,*)'Temperature related drift. Polynomial degree must be >=0. But requested degree is:',p%drift_k
                    valid=.false.
                end if
                
            else if ((tabline(1)=='SIGMA_FACTOR').and.StrIsnumber(tabline(2))) then
                !read(tabline(2),101)p%sigma_factor
                !p%sigma_factor = DNUM(tabline(2))
                p%sigma_factor = str2double(tabline(2))
                
                if (p%sigma_factor<0.0D0) then
                    p%sigma_factor = 1.0D0
                end if
                
            else if ((tabline(1)=='CONV').and.StrIsnumber(tabline(2))) then
                !read(tabline(2),101)p%critere_convergence
                !p%critere_convergence = DNUM(tabline(2))
                p%critere_convergence = str2double(tabline(2))
                if (p%critere_convergence<0.0D0) then
                    p%critere_convergence = 0.000001D0
                end if
                
            else if ((tabline(1)=='SIGMA_ADD').and.StrIsnumber(tabline(2))) then
                !read(tabline(2),101)p%sigma_add
                !p%sigma_add = DNUM(tabline(2))
                p%sigma_add = str2double(tabline(2))
                if (p%sigma_add<0.0D0) then
                    p%sigma_add = 0.0D0
                end if
                
            else if ((tabline(1)=='MODE').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%mode
                !p%mode = JNUM(tabline(2))
                if (P%mode<0 .or. p%mode>4) then
                    write(0,*)'No such adjustment model. Requested model is:', p%mode
                    valid=.false.
                end if
            
            else if ((tabline(1)=='SPARSE').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)p%sparse
                !p%mode = JNUM(tabline(2))
                if (P%sparse<0 .or. p%sparse>1) then
                    write(0,*)'No such adjustment model. Requested sparse is:', p%sparse !!!!!!!!!!!!
                    valid=.false.
                end if
                 
            else if ((tabline(1)=='TYPE_HISTO').and.StrIsnumber(tabline(2))) then
                read(tabline(2),102)k
                if (k==1) then
                    P%type_resid=.true.
                    !write(0,*)line
                else 
                    P%type_resid=.false.
                end if
                
            else if (tabline(1)=='WRITE_OBS') then
                if (tabline(2)=='N') then
                    P%print_obs = .false.
                else if (tabline(2)=='Y') then
                    P%print_obs = .true.   
                end if

            else if (tabline(1)=='VERBOSE') then
                if (tabline(2)=='N') then
                    P%verbose = .false.
                else if (tabline(2)=='Y') then
                    P%verbose = .true.   
                end if

            ! Arret apres le chargement des données
            ! (Utile en cas de données douteuses)
            else if (tabline(1)=='STOP_AFTER_LOAD') then
                if (tabline(2)=='N') then
                    P%stop_after_load = .false.
                else if (tabline(2)=='Y') then
                    P%stop_after_load = .true.   
                end if
   
            else if (tabline(1)=='OUTF') then
                p%nomficout=tabline(2)
                
            else if (tabline(1)=='CALF') then
                call update_separator(tabline(2),p)
                p%nomficcal=tabline(2)
                call verif_nomfic(p%nomficcal)
                
            else if (tabline(1)=='FCOR') then
                p%NCoordFic = p%NCoordFic + 1
                nom = tabline(2)
                call update_separator(nom,p)
                call get_rep(nom,rep)
                !write(0,*) nom
                p%tabCoordFic(p%NCoordFic)%nom = nom
                p%tabCoordFic(p%NCoordFic)%rep = rep 
                
            else if (tabline(1)=='WRITE_MAT' ) then  
                if (tabline(2)=='N') then
                    p%writemat=.false.
                else if (tabline(2)=='Y') then
                    p%writemat=.true.
                end if  
                
            else if (tabline(1)=='WRITE_RESID' ) then  
                if (tabline(2)=='N') then
                    p%write_resid=.false.
                else
                    p%write_resid=.true. 
                end if 
                
            else if (tabline(1)=='WRITE_TAU' ) then  
                if (tabline(2)=='Y') then
                    p%write_only_failed_tau_test=.true.
                else 
                    p%write_only_failed_tau_test=.false.
                end if  
                
            else if (tabline(1)=='WRITE_GRAV' ) then  
                if (tabline(2)=='N') then
                    p%write_gravity=.false.
                end if 
                
            else if (tabline(1)=='WRITE_DRIFT' ) then  
                if (tabline(2)=='N') then
                    p%write_drift=.false.
                end if  
                
            ! Ecriture dans les rapports de la liste des fichiers
            else if (tabline(1)=='WRITE_LIST_FIC' ) then  
                if (tabline(2)=='N') then
                    p%write_list_fic=.false.
                end if  
                
            ! Ecriture dans les rapports de la liste des stations
            else if (tabline(1)=='WRITE_LIST_STATION' ) then  
                if (tabline(2)=='N') then
                    p%write_list_station=.false.
                end if

            ! Contrainte de temps pour la réduction des fichiers c    
            else if ( (tabline(1)=='DELAY_MAX' .or. tabline(1)=='DELAI_MAX') .and. StrIsnumber(tabline(2))) then 
                p%delai_max = str2double(tabline(2))   
 
            ! Création ou non de fichiers r ?  
            else if (tabline(1)=='CREATE_R') then
                if (tabline(2)=='Y') P%create_r = .true.
       
            end if
        end if

        if (nc==5) then
             if  (tabline(1)=='REGION') then
                if (strisnumber(tabline(2)) .and. strisnumber(tabline(3))&
                    &.and. strisnumber(tabline(4)) .and. strisnumber(tabline(5)) ) then       
                    p%lon1 = str2double(tabline(2))
                    p%lon2 = str2double(tabline(4))
                    p%lat1 = str2double(tabline(3))
                    p%lat2 = str2double(tabline(5))              
                end if
                
                !WRITE(0,*)p%lon1,p%lon2,p%lat1,p%lat2
             end if
        end if
        
        if (nc>=2 .and. nc<=4) then
        
            if  (tabline(1)=='RELF' .or. tabline(1)=='ABSF' .or. tabline(1)=='A10F') then  
            
                if (mod(p%NDataFic,datablock)==0) then
                        ! on atteint la fin du datablock, il faut réallouer de la place
                        allocate (TabDataFic2(p%NDataFic))
                        do i=1,p%NDataFic
                            TabDataFic2(i) = p%TabDataFic(i)
                        end do
                        deallocate (p%TabDataFic)
                        allocate (p%TabDataFic(p%NDataFic+datablock))
                        do i=1,p%NDataFic
                            p%TabDataFic(i) = TabDataFic2(i)
                        end do
                        deallocate (TabDataFic2)
                end if
            
          !  write(0,*)p%NDataFic
                p%NDataFic = p%NDataFic + 1
                nom = tabline(2)
                call update_separator(nom,p)
                call get_rep(nom,rep)
                p%TabDataFic(p%NDataFic)%nom = nom
                p%TabDataFic(p%NDataFic)%rep = rep
                
                p%TabDataFic(p%NDataFic)%sigma_f = 1.0D0
                p%TabDataFic(p%NDataFic)%sigma_a = 0.0D0
            
                if (StrIsnumber(tabline(3))) then
                    p%TabDataFic(p%NDataFic)%sigma_f = str2double(tabline(3)) 
                end if
            
                if (StrIsnumber(tabline(4))) then
                    p%TabDataFic(p%NDataFic)%sigma_a = str2double(tabline(4)) 
                end if
            
                if (tabline(1) == 'RELF') then
                    p%ntabnomficrel = p%ntabnomficrel + 1
                    p%TabDataFic(p%NDataFic)%typ = 0      
                else if (tabline(1) == 'ABSF') then  
                    p%ntabnomficabs = p%ntabnomficabs + 1  
                    p%TabDataFic(p%NDataFic)%typ = 1
                else
                    p%ntabnomficA10 = p%ntabnomficA10 + 1 
                    p%TabDataFic(p%NDataFic)%typ = 2
                end if   
            end if           
        end if
    end do

    nfic_grav_abs = p%ntabnomficabs + p%ntabnomficA10
    
    if (P%mode>=2 .and. nfic_grav_abs<=0) then
        write(0,*)'Must provide fixed stations if weighted constraint solution is wanted'
        valid = .false.
    end if
    
    if (p%ntabnomficabs>0 .OR. p%ntabnomficA10>0) then
        p%lfix=.true.
    end if

    if (p%nomficcal/='') p%calf=.true.
    
    if  ((p%ntabnomficrel>0) .and. (p%nomficout/='') .and. (nfic_grav_abs>0) .and. valid) then
        Parse_config_file = 0
    else 
        Parse_config_file = 101        
    end if
    
 !   if (Associated(p%TabParam)) deallocate(p%TabParam)
 
    return

end function Parse_config_file

! Update directory separators 
subroutine update_separator(line,p)
    use param_data
    implicit none                       
    character (len=*) line
    type (TParam) p
    integer i

    if (p%systeme == 'LINUX') then
        do i=1,len_trim(line)  
            if (line(i:i)=='\') then
                line(i:i)='/'
            end if
        end do
    else
        do i=1,len_trim(line)  
            if (line(i:i)=='/') then
                line(i:i)='\'
            end if
        end do
    endif
end subroutine update_separator

integer function libere_param(p) 

    use param_data
    implicit none     
    type (TParam) , intent(inout):: p            
            
    if (Associated(p%TabDataFic)) deallocate(p%TabDataFic)
    if (Associated(p%TabCoordFic)) deallocate(p%TabCoordFic)

    
    libere_param = 0
    
end function libere_param

end module Lect_param

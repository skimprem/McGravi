module GMT6
! Numbers of files
! 80 : script
! 81 : relative points
! 82 : absolute points
! 83 : error bars
! 84 : profils
! 85 : relative and absolute residuals
! 86 : Gaussian curve
! 87 : footprint calculation script

type TGMT6
    character (len=255) gmtbin ! GMTBIN directory
    character (len=16)  FORMAT_GEO_MAP
    character (len=16)  PS_MEDIA
    character (len=50)  FONT_ANNOT_PRIMARY
    character (len=50)  FONT_HEADING
    character (len=50)  FONT_LABEL
    character (len=16)  PS_CHAR_ENCODING
    character (len=16)  MAP_FRAME_PEN
    character (len=16)  PROJ_LENGTH_UNIT
    character (len=16)  PS_PAGE_ORIENTATION
    character (len=46)  REGION
    character (len=10)  PROJ
    character (len=16)  X_SHIFT, Y_SHIFT
    integer             POINT_NAME_FONT_SIZE

    real(8)  :: W, E, N, S ! lon/lat drawing limits
    real(8)  :: XW, XE, YN, YS, DX, DY ! East/North drawing limits
    real(8)  :: PAGE_WIDTH, PAGE_HEIGHT, PAGE_RATIO
    real(8)  :: MAP_WIDTH, MAP_HEIGHT, MAP_RATIO
       
end type TGMT6

type (TGMT6) gmt_map ! GMT drawing parameters

contains

subroutine Init_gmt(g)

use param_data

implicit none

type (TGMT6) :: g
logical :: DEBUG
real(8) :: DXX, DYY, A, B, OFFSET_X, OFFSET_Y, OFFSET_TITLE
character(300) :: temp1, temp2, temp3, temp4

DEBUG = .false.

g%GMTBIN = ''

g%FORMAT_GEO_MAP = "ddd:mm"
g%PS_MEDIA = "a3"
g%FONT_ANNOT_PRIMARY = "auto,Helvetica,black"
g%FONT_HEADING = "auto,Helvetica-Bold,black"
g%FONT_LABEL = "auto,Helvetica,black"
g%PS_CHAR_ENCODING = "Standard+"
g%MAP_FRAME_PEN = "auto"
g%PROJ_LENGTH_UNIT = "c"
g%PS_PAGE_ORIENTATION = ""
g%POINT_NAME_FONT_SIZE = 6

if (g%PS_MEDIA .eq. "a4") then
    A = 0.297D2
    B = 0.210D2
elseif (g%PS_MEDIA .eq. "a3") then
    A = 0.420D2
    B = 0.297D2
elseif (g%PS_MEDIA .eq. "a2") then
    A = 0.594D2
    B = 0.420D2
end if

g%N = param%lat2
g%S = param%lat1
g%W = param%lon1
g%E = param%lon2

write(temp1, '(f10.6)') param%lon1
write(temp2, '(f10.6)') param%lat1
write(temp3, '(f10.6)') param%lon2
write(temp4, '(f10.6)') param%lat2

g%REGION = '-R'//trim(adjustl(temp1))//'/'//trim(adjustl(temp2))//'/'//trim(adjustl(temp3))//'/'//trim(adjustl(temp4))//'r'

if (DEBUG) then
    write(0, *)'W : ', g%W
    write(0, *)'E : ', g%E
    write(0, *)'S : ', g%S
    write(0, *)'N : ', g%N
    write(0, *)'REGION : ', g%REGION
endif

! The footprint calculation is used to determine the height/width ratio to
! choose the direction of the sheet and the final scale CALL CALC_EMPRISE(g)
call calc_emprise(g)
g%MAP_RATIO = g%DY / g%DX

if ((g%MAP_RATIO) .gt. 1.0d0) then
    ! protrait
    g%PS_PAGE_ORIENTATION = "portrait"
    g%PAGE_WIDTH = min(A, B)
    g%PAGE_HEIGHT = max(A, B)
else
    ! landscape
    g%PS_PAGE_ORIENTATION = "landscape"
    g%PAGE_WIDTH = max(A, B)
    g%PAGE_HEIGHT = min(A, B)
endif 
g%PAGE_HEIGHT = g%PAGE_HEIGHT - 1.0d0 
g%PAGE_RATIO = g%PAGE_HEIGHT / g%PAGE_WIDTH

OFFSET_TITLE = 1.0d0

if ((g%MAP_RATIO) .lt. g%PAGE_RATIO) then
    if (DEBUG) write(0, *)'CAS MAP_RATIO < PAGE_RATIO'
    OFFSET_X = 1.0d0
    g%MAP_WIDTH = g%PAGE_WIDTH - 2.0d0 * OFFSET_X
    g%MAP_HEIGHT = g%MAP_WIDTH * g%MAP_RATIO
    OFFSET_Y = 0.5d0 * (g%PAGE_HEIGHT - g%MAP_HEIGHT)  
else
    if (DEBUG) write(0, *)'CAS MAP_RATIO > PAGE_RATIO'
    OFFSET_Y = 1.0d0
    g%MAP_HEIGHT = g%PAGE_HEIGHT - 2.0d0 * OFFSET_Y
    g%MAP_WIDTH = g%MAP_HEIGHT / g%MAP_RATIO 
    OFFSET_X = 0.5d0 * (g%PAGE_WIDTH - g%MAP_WIDTH)      
endif 

write(g%X_SHIFT,'(A3,E8.2,A2)') ' -X', OFFSET_X, 'c ' 
write(g%Y_SHIFT,'(A3,E8.2,A2)') ' -Y', OFFSET_Y, 'c '
write(g%PROJ,'(A4,F4.1,A2)') ' -JM', g%MAP_WIDTH, 'c '
    
if (DEBUG) then
    write(0, *) 'Easting W : ',g%XW
    write(0, *) 'Easting E : ',g%XE
    write(0, *) 'Northing S : ',g%YS
    write(0, *) 'Northing N : ',g%YN
    write(0, *) 'DX : ',g%DX
    write(0, *) 'DY : ',g%DY
    write(0, *) 'MAP_RATIO (DY/DX) : ',g%MAP_RATIO
    write(0, *) 'PAGE_RATIO (H/W) : ',g%PAGE_RATIO
    write(0, *) 'PS__MEDIA : ',g%PS_MEDIA
    write(0, *) 'PS_PAGE_ORIENTATION : ',g%PS_PAGE_ORIENTATION
    write(0, *) 'PROJ : ',g%PROJ
    write(0, *) 'PAGE_WIDTH : ',g%PAGE_WIDTH
    write(0, *) 'PAGE_HEIGHT : ',g%PAGE_HEIGHT
    write(0, *) 'MAP_WIDTH : ',g%MAP_WIDTH
    write(0, *) 'MAP_HEIGHT : ',g%MAP_HEIGHT
    write(0, *) 'X_SHIFT : ',g%X_SHIFT
    write(0, *) 'Y_SHIFT : ',g%Y_SHIFT
endif

end SUBROUTINE Init_gmt

! Map coverage calculation (E,N coordinates)
! using GMT's mapproject function
! JBL - 2010-05-19
! input:
! (TGMT) g: the W,E,N,S fields must be initialized beforehand
! The SUBROUTINE affects the fields XW,XE,YN,YS,DX,DY
! UPD: SRA - 2023-04-19

SUBROUTINE CALC_EMPRISE(gmt_map)
    use Portability_routines
    use util_str
    implicit none
    type (TGMT6) gmt_map
    logical DEBUG  
    character(350) s
    logical(4) resultat
    character (len=255) line 
    character (len=255) tabline(20)
    integer nc
    
    DEBUG = .true.

    ! creating the script
    open(87,file = 'EN_minmax.sh')
    
    call Wfic(87, 'gmtset="gmt set"')
    call Wfic(87, 'mapproject="gmt mapproject"')
    write(s, *) '$gmtset PROJ_LENGTH_UNIT ', gmt_map%PROJ_LENGTH_UNIT
    call Wfic(87, s)
    
    write(s, *) 'region="', gmt_map%REGION, '"'
    call Wfic(87, s)
    ! For the calculation of the LxH ratio we take an arbitrary scale 1:1 
    call Wfic(87, 'proj=" -Jm1"')

    ! creation of the input points file
    call Wfic(87, '# Creating input points file')
    write(s, *) 'echo "',gmt_map%W,' ',gmt_map%N,'" > EN_minmax.in'
    call Wfic(87, s)
    write(s, *) 'echo "',gmt_map%E,' ',gmt_map%S,'" >> EN_minmax.in'
    call Wfic(87, s)
    
    call Wfic(87, "$mapproject EN_minmax.in $region $proj -C > EN_minmax.out")
    
    close(87)

    resultat = run_system2('bash EN_minmax.sh')

    open(87, file = 'EN_minmax.out')

    read(87,'(A80)') line
    call decoupe_ligne(line, tabline, nc)
    if (nc==2) then 
        if  (strisnumber(tabline(1)) .and. strisnumber(tabline(2))) then 
            gmt_map%XW = str2double(tabline(1))
            gmt_map%YN = str2double(tabline(2))
        end if
    end if

    read(87, '(A80)') line
    call decoupe_ligne(line, tabline, nc)
    if (nc==2) then 
        if  (strisnumber(tabline(1)) .and. strisnumber(tabline(2))) then 
            gmt_map%XE = str2double(tabline(1))
            gmt_map%YS = str2double(tabline(2))
        end if
    end if

    gmt_map%DX = gmt_map%XE - gmt_map%XW;
    gmt_map%DY = gmt_map%YN - gmt_map%YS;

    close(87)

end subroutine calc_emprise

subroutine mercator(l, p, E, N)
    implicit none
    real(8), intent(inout) :: p, l, E, N
    real(8) :: pi, dd2rad, Latiso, k0
    
    k0 = 1.0d0
    pi = 4.0d0 * atan(1.0d0)
    dd2rad = pi / 180.0d0
    Latiso = lat2latiso(p)
    E = k0 * dd2rad * l
    N = k0 * Latiso
end subroutine mercator

! isometric latitude calculation (spherical version)
real(8) function lat2latiso(phi)
    implicit none
    real(8) pi, phi, dd2rad
    pi = 4.0d0 * atan(1.0d0)
    dd2rad = pi / 180.0d0	
    lat2latiso = log (tan(pi / 4.0d0 + dd2rad * phi / 2.0d0))
    return 
end function 

! creation and launch of the GMT profile drawing script 
subroutine dessine_reseau(MC, run)
    use MC_data 
    use param_data
    use Portability_routines
    use sys_utils
    implicit none
    type (Tmc), intent(inout) :: MC
    logical(4) :: resultat
    logical :: run
    integer(4) :: resultatI

    call WGMT_bat_profils('profils.sh')
    call WGMT_pts_rel("pts_rel.txt")
    call WGMT_pts_abs("pts_abs.txt")
    call WGMT_profil("profils.txt", MC)

    if (run) then
        resultatI = rm('.gmtdefaults')
        resultatI = rm('gmt.conf')
        resultatI = rm('gmt.history')
        ! resultatI = rm('.gmtdefaults4')
        resultat = run_system2('bash  profils.sh')
    end if

end subroutine dessine_reseau

! creation and launch of the GMT script
subroutine run_GMT(MC, run)
    use MC_data
    use param_data
    use Portability_routines
    use sys_utils
    implicit none
    type (Tmc), intent(inout) :: MC
    character (len=80) :: cmd
    integer(4) :: resultatI
    logical(4) :: resultat
    character(5) :: systeme
    logical :: run
	
    call WGMT_bat('gmt.sh')
    call WGMT_sigma("error_bars.txt", MC)
	
    call WGMT_resid_rel("residus_std_rel.txt", MC, 'NORM', 'ALL')
    call WGMT_resid_rel("residus_brut_rel.txt", MC, 'BRUT', 'ALL')
    call WGMT_resid_abs("residus_std_abs.txt", MC, 'NORM', 'ALL')
	! error :
    call WGMT_resid_abs("residus_brut_abs.txt", MC, 'BRUT', 'ALL')
    call WGMT_resid_rel("residus_tau_rel.txt", MC, 'NORM', 'TAU')
    call WGMT_resid_abs("residus_tau_abs.txt", MC, 'NORM', 'TAU')
    
    CALL WGMT_leg_resid("legende_resid_std.txt",'NORM')
    CALL WGMT_leg_resid("legende_resid_brut.txt",'BRUT')
    CALL WGMT_leg_sigma("legende_sigma.txt")
    
    if (run) then
        resultatI = run_system('bash','gmt.sh')
        resultatI = move('gmt.sh', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('profils.sh', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('pts_rel.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('pts_abs.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('error_bars.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('profils.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('residus_std_rel.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('residus_std_abs.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('residus_tau_rel.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('residus_tau_abs.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('residus_brut_rel.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('residus_brut_abs.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('legende_resid_brut.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('legende_resid_std.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = move('legende_sigma.txt', param%dossier(1:len_trim(param%dossier)))
        resultatI = copy('Profils.pdf', param%dossier(1:len_trim(param%dossier)))
        resultatI = copy('Error_bars.pdf', param%dossier(1:len_trim(param%dossier)))
        resultatI = copy('Residus_norm.pdf', param%dossier(1:len_trim(param%dossier)))
        resultatI = copy('Residus_bruts.pdf', param%dossier(1:len_trim(param%dossier)))
        resultatI = copy('Residus_tautest.pdf', param%dossier(1:len_trim(param%dossier)))
        resultatI = copy(param%nomficcal, param%dossier(1:len_trim(param%dossier)))
    end if
	
end subroutine run_GMT

! Writing the GMT script
subroutine WGMT_bat_profils(nomfic)
    use param_data
    implicit none
    character (len=*) :: nomfic
    character(350) :: s
    character :: string*(20)

    !write(0, *) "Writing GMT script"
    open(80, file = nomfic)
    
    call Wfic(80, 'gmtset="gmt set"')
    call Wfic(80, 'coast="gmt coast"')
    call Wfic(80, 'plot="gmt plot"')
    call Wfic(80, 'text="gmt text"')
     
    call Wfic(80, '$gmtset PS_MEDIA '//trim(adjustl(gmt_map%PS_MEDIA)))
    call Wfic(80, '$gmtset PS_PAGE_ORIENTATION '//trim(adjustl(gmt_map%PS_PAGE_ORIENTATION)))
    call Wfic(80, '$gmtset PS_CHAR_ENCODING '//trim(adjustl(gmt_map%PS_CHAR_ENCODING)))
    call Wfic(80, '$gmtset PROJ_LENGTH_UNIT '//trim(adjustl(gmt_map%PROJ_LENGTH_UNIT)))
    call Wfic(80, '$gmtset FORMAT_GEO_MAP '//trim(adjustl(gmt_map%FORMAT_GEO_MAP)))
    call Wfic(80, '$gmtset FONT_ANNOT_PRIMARY '//trim(adjustl(gmt_map%FONT_ANNOT_PRIMARY)))
    call Wfic(80, '$gmtset FONT_HEADING '//trim(adjustl(gmt_map%FONT_HEADING)))
    call Wfic(80, '$gmtset FONT_LABEL '//trim(adjustl(gmt_map%FONT_LABEL)))
    call Wfic(80, '$gmtset MAP_FRAME_PEN '//trim(adjustl(gmt_map%MAP_FRAME_PEN)))
    
    call Wfic(80, 'region="'//trim(adjustl(gmt_map%REGION))//'"')
    call Wfic(80, 'proj="'//trim(adjustl(gmt_map%PROJ))//'"')
    call Wfic(80, 'x_shift="'//trim(adjustl(gmt_map%X_SHIFT))//'"')
    call Wfic(80, 'y_shift="'//trim(adjustl(gmt_map%Y_SHIFT))//'"')

    call Wfic(80, "gmt begin Profils") 
    call Wfic(80, &
        '    $coast $region $proj -BWESN+t"Observations relatives et absolue" -B1g1f -Df -A0/1 -N1 &
        -W1 -G255/220/150 -Scyan -I1/0.5p,blue &
        -I2/0.5p,blue -I3/0.5p,blue $x_shift $y_shift ')
    call Wfic(80, "    $plot -W1.5p,gray profils.txt")
    call Wfic(80, "    $plot -Sc0.075 -Gblack pts_rel.txt")
    call Wfic(80, "    $text -S5/255/220/150 -G94/151/106 -Dj0.05 pts_rel.txt")
    call Wfic(80, "    $plot -St0.25 -Gred pts_abs.txt")
    call Wfic(80, "    $text -S5/255/220/150 -G0 -Dj0.05 pts_abs.txt")
    call Wfic(80, "gmt end")
    
    close(80)
    
end subroutine WGMT_bat_profils

! Writing the GMT script
subroutine WGMT_bat_histo(nomfic, nomfic_histo, nomfic_pdf, nomfic_png)
    use param_data
    use Portability_routines
    use sys_utils
    implicit none
    character (len=*) :: nomfic, nomfic_pdf, nomfic_histo, nomfic_png
    character(255) :: s
    character :: string*(20)
    integer(4) :: resultatI
    logical(4) :: resultat

    !write(0, *) "Writing GMT script"
    open(80, file = nomfic)
    
    call Wfic(80, 'gmtset="gmt set"')
    call Wfic(80, 'basemap="gmt basemap"')
    call Wfic(80, 'plot="gmt plot"')
    
    call Wfic(80, "$gmtset PS_PAGE_ORIENTATION portrait")
    call Wfic(80, "$gmtset PS_MEDIA a4")
    call Wfic(80, "$gmtset FONT_ANNOT_PRIMARY 12p,Helvetica,black")
    call Wfic(80, "$gmtset FONT_HEADING 10p,Helvetica,black")
    call Wfic(80, "$gmtset FONT_LABEL 8p,Helvetica,black")
    call Wfic(80, "$gmtset PS_CHAR_ENCODING  Standard+")
    call Wfic(80, "$gmtset MAP_FRAME_PEN thicker,black")
    call Wfic(80, "$gmtset PROJ_LENGTH_UNIT c")
    
    call Wfic(80, 'gmt begin '//nomfic_pdf//' pdf,png')
    if (param%Type_resid) then         
        call Wfic(80, '$basemap -R-4.5/+4.5/0/60 -JX16c/20c -Bxf0.5a0.5 -Byf10a10 &
        -By+L"\045" -B+t"Histogram of standard residuals"')
    else
        call Wfic(80, '$basemap -R-4.5/+4.5/0/60 -JX16c/20c -Bxf0.5a0.5 -Byf10a10 &
        -By+L"\045" -B+t"Histogram of normalized residuals"')
    end if
    s = "$plot -Gblue -W0.5p -Sb0.9c -N " // nomfic_histo
    call Wfic(80, s) 
    call Wfic(80, '$plot -W0.8p,red gauss.txt -N')
    call Wfic(80, 'gmt end')
   
    close(80)
    
    resultatI = run_system('bash', nomfic)

    resultatI = move(nomfic_histo(1:len_trim(nomfic_histo)), param%dossier(1:len_trim(param%dossier)))
    resultatI = move(nomfic, param%dossier(1:len_trim(param%dossier)))
    resultatI = move(nomfic_pdf, param%dossier(1:len_trim(param%dossier)))
    resultatI = move('gauss.txt', param%dossier(1:len_trim(param%dossier)))
    resultatI = copy(nomfic_png, param%dossier(1:len_trim(param%dossier)))
       
end subroutine WGMT_bat_histo

! Writing the GMT script
subroutine WGMT_bat(nomfic)
    use param_data
    implicit none
    character (len=*) :: nomfic   
    character(300) :: s
    character :: string*(20)

    17 format(A3, SP, D9.3, A1, SP, D9.3, A1, SP, D9.3, A1, SP, D9.3, A1)
    18 format(A3, SP, D9.3, A1, SP, D9.3, A1, SP, D9.3, A1, SP, D9.3, A1)

    write(0, *) "Writing GMT script"
    open(80, file = nomfic)

    call Wfic(80, 'gmtset="gmt set"')
    call Wfic(80, 'coast="gmt coast"')
    call Wfic(80, 'plot="gmt plot"')
    call Wfic(80, 'text="gmt text"')
     
    call Wfic(80, '$gmtset PS_MEDIA '//gmt_map%PS_MEDIA)
    call Wfic(80, '$gmtset PS_PAGE_ORIENTATION '//gmt_map%PS_PAGE_ORIENTATION)
    call Wfic(80, '$gmtset PS_CHAR_ENCODING '//gmt_map%PS_CHAR_ENCODING)
    call Wfic(80, '$gmtset PROJ_LENGTH_UNIT '//gmt_map%PROJ_LENGTH_UNIT)
    call Wfic(80, '$gmtset FORMAT_GEO_MAP '//gmt_map%FORMAT_GEO_MAP)
    call Wfic(80, '$gmtset FONT_ANNOT_PRIMARY '//gmt_map%FONT_ANNOT_PRIMARY)
    call Wfic(80, '$gmtset FONT_HEADING '//gmt_map%FONT_HEADING)
    call Wfic(80, '$gmtset FONT_LABEL '//gmt_map%FONT_LABEL)
    call Wfic(80, '$gmtset MAP_FRAME_PEN '//gmt_map%MAP_FRAME_PEN)

    call Wfic(80, 'region="'//gmt_map%REGION//'"')
    call Wfic(80, 'proj="'//gmt_map%PROJ//'"')
    call Wfic(80, 'x_shift="'//gmt_map%X_SHIFT//'"')
    call Wfic(80, 'y_shift="'//gmt_map%Y_SHIFT//'"')
 
    ! map of standard deviations on gravity
    call Wfic(80, 'echo -n "    begin Error_bars plotting ... "')
    call Wfic(80, "gmt begin Error_bars")
    call Wfic(80, &
    '$coast $region $proj -B1g1f1 -B+t"Standard deviations on the blue estimated gravities" &
    -Df -A0/1 -N1 -W1 -G255/220/150 -Scyan -I1/0.5p,blue &
    -I2/0.5p,blue -I3/0.5p,blue  $x_shift $y_shift')
    call Wfic(80, "$plot error_bars.txt -Wthick,red -Gred -Sc0.001c -Ey+p0.3c,red")
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 legende_sigma.txt")
    call Wfic(80, "$plot -Sc0.075 -Gblack pts_rel.txt")
    call Wfic(80, "$text -S5/255/220/150 -G94/151/106 -Dj0.05 pts_rel.txt")
    call Wfic(80, "$plot -St0.25 -Gred pts_abs.txt")
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 pts_abs.txt")
    call Wfic(80, "gmt end")
    call Wfic(80, 'echo "done!"')
    !-Lf6.5/42.5/45/50k       
    ! call Wfic(80, s)

   ! map of normalized residuals
    call Wfic(80, 'echo -n "    begin Residus_norm plotting ... "')
    call Wfic(80, "gmt begin Residus_norm")
    call Wfic(80, &
    '$coast $region $proj -B1g1f1 -B+t"Normalized residuals" -Df -A0/1 -N1 &
    -W1  -G255/220/150 -Scyan -I1/0.5p,blue -I2/0.5p,blue -I3/0.5p,blue $x_shift $y_shift')
    call Wfic(80, "$plot -W1.5p,gray profils.txt")
    call Wfic(80, "$plot -Wthick,green -Ggreen -Sv0.02c/0.04c/0.04c residus_std_rel.txt")
    !-Lf6.5/42.5/45/50k    
    if (param%mode .eq. 2) then
        call Wfic(80, "$plot -Wthick,magenta -Gmagenta -Sv0.02c/0.04c/0.04c residus_std_abs.txt")   
    endif
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 legende_resid_std.txt")  
    call Wfic(80, "$plot -Sc0.075 -Gblack pts_rel.txt")
    call Wfic(80, "$text -S5/255/220/150 -G94/151/106 -Dj0.05  pts_rel.txt")     
    call Wfic(80, "$plot -St0.25 -Gred pts_abs.txt")
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 pts_abs.txt")
    call Wfic(80, "gmt end")
    call Wfic(80, 'echo "done!"')

    ! map of normalized residues not passing the tau test
    call Wfic(80, 'echo -n "    begin Residus_tautest plotting ... "')
    call Wfic(80, "gmt begin Residus_tautest")
    call Wfic(80, &
    '$coast $region $proj -B1g1f1 -B+t"Normalized residuals failing the tau test" -Df -A0/1 -N1 &
    -W1 -G255/220/150 -Scyan -I1/0.5p,blue -I2/0.5p,blue -I3/0.5p,blue $x_shift $y_shift')
    !-Lf6.5/42.5/45/50k    
    call Wfic(80, "$plot -W1.5p,gray profils.txt")
    call Wfic(80, "$plot -Wthick,green -Ggreen -Sv0.02c/0.04c/0.04c residus_tau_rel.txt")
    call Wfic(80, "$plot -Wthick,magenta -Gmagenta -Sv0.02c/0.04c/0.04c residus_tau_abs.txt")   
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 legende_resid_std.txt")  
    call Wfic(80, "$plot -Sc0.075 -Gblack pts_rel.txt")
    call Wfic(80, "$text -S5/255/220/150 -G94/151/106 -Dj0.05 pts_rel.txt")     
    call Wfic(80, "$plot -St0.25 -Gred pts_abs.txt")
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 pts_abs.txt")
    call Wfic(80, "gmt end")
    call Wfic(80, 'echo "done!"')

    ! crude tailings map
    call Wfic(80, 'echo -n "    begin Residus_bruts ... "')
    call Wfic(80, "gmt begin Residus_bruts")
    call Wfic(80, &
    '$coast $region $proj -B1g1f1 -B+t"Residuals" -Df -A0/1 -N1 -W1 -G255/220/150 -Scyan -I1/0.5p,blue &
    -I2/0.5p,blue -I3/0.5p,blue $x_shift $y_shift')
    !-Lf6.5/42.5/45/50k   
    call Wfic(80, "$plot -W1.5p,gray profils.txt")
    call Wfic(80, "$plot -Wthick,green -Ggreen -Sv0.02c/0.04c/0.04c residus_brut_rel.txt")
    if (param%mode .eq. 2) then
        call Wfic(80, "$plot -Wthick,magenta -Gmagenta -Sv0.02c/0.04c/0.04c residus_brut_abs.txt")  
    end if
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 legende_resid_brut.txt") 
    call Wfic(80, "$plot -Sc0.075 -Gblack pts_rel.txt")
    call Wfic(80, "$text -S5/255/220/150 -G94/151/106 -Dj0.05 pts_rel.txt")     
    call Wfic(80, "$plot -St0.25 -Gred pts_abs.txt")
    call Wfic(80, "$text -S5/255/220/150 -G0 -Dj0.05 pts_abs.txt")
    call Wfic(80, "gmt end")
    call Wfic(80, 'echo "done!"')

    close(80)

end subroutine WGMT_bat

subroutine WGMT_pts_rel(nomfic)
    use Raw_data
    use param_data
    implicit none
    character (len=*) :: nomfic
    character(255) :: s
    integer :: i
    open(81, file = nomfic)

    do i = 1, NTabStation
        write(s, '(f12.6,1x,f12.6,1x,I2,1x,A7,1x,A8)') TabStation(i)%lon,&
		&TabStation(i)%lat, gmt_map%POINT_NAME_FONT_SIZE, " 0 1 0", TabStation(i)%nomsta
        call Wfic(81, s)
    end do
    
    close(81)
end subroutine WGMT_pts_rel

subroutine WGMT_leg_sigma(nomfic)
    use Raw_data
    use param_data
    implicit none
    character (len=*) :: nomfic
    character(255) :: s
    real(8) :: dlat, dlon, sigma, lat, lon
    integer :: i
    open(81, file = nomfic)

    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10.0d0
    lon = param%lon1 + dlon / 10.0d0 + 0.1d0
    write(s, '(f12.6,1x,f12.6,1x,A7,1x,A20)') lon, lat, "8 0 1 0", "25 \225Gal"
    call Wfic(81, s)
    
    close(81)
end subroutine WGMT_leg_sigma

subroutine WGMT_leg_resid(nomfic, BRUTouNORM)
    use Raw_data
    use param_data
    implicit none
    character (len=*) :: nomfic
    character (len=4) :: BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    character(255) :: s
    real(8) :: dlat, dlon, sigma, lat, lon
   
    open(81, file = nomfic)

    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10.0d0
    lon = param%lon1 + dlon / 10.0d0 + 0.1d0
    if (BRUTouNORM =='BRUT') then
      write(s, '(f12.6,1x,f12.6,1x,A7,1x,A10)') lon, lat, "8 0 1 0", "10 \225Gal"
    else
	   write(s, '(f12.6,1x,f12.6,1x,A7,1x,A3)') lon, lat, "8 0 1 0", "1"
    endif
   
    call Wfic(81,s)
    
    close(81)
end subroutine WGMT_leg_resid

subroutine WGMT_pts_abs(nomfic)
    use Raw_data
    use param_data
    implicit none
    character (len=*) :: nomfic
    character(255) :: s
    integer :: i, j
    open(82, file = nomfic)

    do i = 1, NTabStation
        do j = 1, param%Nb_obsAbs
            if (TabStation(i)%nomsta == TabObsAbs(j)%nomsta) then
            !if (TabStation(i)%nomsta == fixstn(j)) then
                write(s, '(f12.6,1x,f12.6,1x,I2,1x,A7,1x,A8)') TabStation(i)%lon, &
				&TabStation(i)%lat, gmt_map%POINT_NAME_FONT_SIZE, " 0 1 0", TabStation(i)%nomsta
                call Wfic(82, s)
            end if
        end do
    end do
    
    close(82)
end subroutine WGMT_pts_abs

subroutine WGMT_sigma(nomfic,MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC

    real(8) :: dlat, dlon, sigma, lat, lon
    character (len=*) :: nomfic
    character(255) :: s
    integer :: i, j
    open(83, file = nomfic)
    
    do i = 1, NTabStation
        write(s, '(f12.6,1x,f12.6,1x,f12.6,1x,A8)') TabStation(i)%lon, TabStation(i)%Lat, MC%sig(TabStation(i)%numsta), "0 0 0 0"
        call Wfic(83, s)
    end do
    
    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10.0d0
    lon = param%lon1 + dlon / 10.0d0
    sigma = 0.025d0
    write(s, '(f12.6,1x,f12.6,1x,f12.4,1x,A8)') lon, lat, sigma, "0 0 0 0"
    call Wfic(83, s)
    
    close(83)
end subroutine WGMT_sigma

! Dessin des profils pour detecter les zones de faiblesse du r�seau
subroutine WGMT_profil3(nomfic, MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC

    character (len=*) :: nomfic
    character(255) :: s
    integer :: i, j, nprofil
	
    open(84, file = nomfic)
    nprofil = 0
    do i = 1, nTabObsRel
		obs_rel = TabObsRel(i)
        if (Obs_Rel%profil /= nprofil) then
            call Wfic(84, ">")
			nprofil = obs_Rel%profil
		end if
		
		write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8,1x,A20)') TabStation(Obs_Rel%numsta_AR)%lon,&
            TabStation(Obs_Rel%numsta_AR)%lat, "8 0 1 0", TabStation(Obs_Rel%numsta_AR)%nomsta,Obs_Rel%profil
		call Wfic(84, s)
		
        write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8)') TabStation(Obs_Rel%numsta_AV)%lon,&
            TabStation(Obs_Rel%numsta_AV)%lat, "8 0 1 0", TabStation(Obs_Rel%numsta_AV)%nomsta
        call Wfic(84, s)
    end do
    
    close(84)
end subroutine WGMT_profil3

! Drawing profiles to detect weak areas of the network 
subroutine WGMT_profil(nomfic, MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC

    character (len=*) :: nomfic
    character(255) :: s
    integer :: i, j
	
    open(84, file = nomfic)
    obs_rel = TabObsRel(1)
    call Wfic(84,">")
    write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8,1x,I4,1x,A20)') TabStation(obs_rel%numsta_AR)%lon,&
    TabStation(obs_rel%numsta_AR)%lat, "8 0 1 0", TabStation(obs_rel%numsta_AR)%nomsta,&
	TabObsRel(1)%profil, param%TabDataFic(TabObsRel(1)%profil)%nom
    !write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8)') TabStation(obs_rel%numsta_AR)%lon,&
    !TabStation(obs_rel%numsta_AR)%lat, "8 0 1 0", TabStation(obs_rel%numsta_AR)%nomsta
    call Wfic(84,s)
	write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8)') TabStation(obs_rel%numsta_AV)%lon,&
    TabStation(obs_rel%numsta_AV)%lat,"8 0 1 0", TabStation(obs_rel%numsta_AV)%nomsta
    call Wfic(84, s)
    do i = 2, nTabObsRel
        if (TabObsRel(i)%profil /= obs_rel%profil) then
            call Wfic(84, ">")
            write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8,1x,I4,1x,A20)') TabStation(TabObsRel(i)%numsta_AR)%lon,&
            TabStation(TabObsRel(i)%numsta_AR)%lat, "8 0 1 0", TabStation(TabObsRel(i)%numsta_AR)%nomsta,&
			TabObsRel(i)%profil, param%TabDataFic(TabObsRel(i)%profil)%nom
            call Wfic(84, s)
			write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)') TabStation(TabObsRel(i)%numsta_AV)%lon,&
            TabStation(TabObsRel(i)%numsta_AV)%lat, "8 0 1 0", TabStation(TabObsRel(i)%numsta_AV)%nomsta
            call Wfic(84, s)
        else
            if (TabObsRel(i)%numsta_AV /= TabObsRel(i)%numsta_AR) then
                write(s, '(f12.6,1x,f12.6,1x,A7,1x,A8)') TabStation(TabObsRel(i)%numsta_AV)%lon,&
                TabStation(TabObsRel(i)%numsta_AV)%lat, "8 0 1 0", TabStation(TabObsRel(i)%numsta_AV)%nomsta
                call Wfic(84,s)
            end if
        end if 
        obs_rel = TabObsRel(i)
    end do
    
    close(84)
end subroutine WGMT_profil

!  Drawing of residues
subroutine WGMT_resid_rel(nomfic, MC, BRUTouNORM, ONLYTAU)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC
    real(8) :: lon, lat, v, moy, dlat, dlon
    character (len=*) :: nomfic
    character (len=4) :: BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    character (len=3) :: ONLYTAU ! TAU ou ALL ; on appelle la procedure 2 fois
    character (len=3) :: Az
    character(255) ::s
    integer :: i, j
    
    open(85, file = nomfic)
    moy = 0.0d0
    do i = 1, nTabObsRel
    
        ! middle of the link 
        lon = 0.5d0 * (TabStation(TabObsRel(i)%numsta_AR)%lon + TabStation(TabObsRel(i)%numsta_AV)%lon)
        lat = 0.5d0 * (TabStation(TabObsRel(i)%numsta_AR)%lat + TabStation(TabObsRel(i)%numsta_AV)%lat)
        if (BRUTouNORM=='BRUT') then
            v = 100.0d0 * TabResid(i)%resid
        else
            if (param%Type_resid) then
               v = TabResid(i)%std_res
            else
               v = TabResid(i)%Norm_res
            endif
        endif
        !3.11056    45.76361  90 3.59 0 0 0
        if (v > 0.0d0) then
            Az = '90'
        else
            Az = '-90'
        end if
        v = abs(v)
        write(s, '(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5,A10,A10)') lon, lat, Az, v, "0 0 0", TabResid(i)%ini,TabResid(i)%fin

        if (ONLYTAU=='TAU') then
!		    if ( TabResid(i)%tautest==.false.) then
		    if (.not. TabResid(i)%tautest) then
				call Wfic(85, s)	
          end if 
        else 
			 call Wfic(85, s)
        endif
        !write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5)')lon,lat,Az,v,"0 0 0"
    end do
    
    !moy = moy /nTabObsRel
    moy = 1.0d0
    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    write(s, '(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5)') param%lon1+dlon / 10.0d0, param%lat1+dlat / 10.0d0, "90", moy, "0 0 0"
    call Wfic(85, s)
      
    close(85)

end subroutine WGMT_resid_rel

! Dessin des residus 
subroutine WGMT_resid_abs(nomfic, MC, BRUTouNORM, ONLYTAU)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC
    real(8) :: lon, lat, v, moy
    character (len=*) :: nomfic
    character (len=4) :: BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    character (len=3) :: ONLYTAU ! TAU ou ALL ; on appelle la procedure 2 fois
    character (len=3) :: Az
    character (255) :: s
    integer :: i, j, num_pt
    
    open(85, file = nomfic)
    moy = 0.0d0
    
    !WRITE(0,*)nTabObsRel,MC%Nb_obsAbs
    do i = nTabObsRel + 1, nTabObsRel + MC%Nb_obsAbs
        
        num_pt = pos(i - nTabObsRel)

        lon = TabStation(num_pt)%lon
        lat = TabStation(num_pt)%lat
        
        ! error :
        if (BRUTouNORM == 'BRUT') then
			! write(0,*)TabResid(i)%resid
            v = 100.0d0 * TabResid(i)%resid
        else
            if (param%Type_resid) then
               v = TabResid(i)%std_res
            else
               v = TabResid(i)%Norm_res
            end if
        end if
		
		
        if (v > 0.0d0) then
            Az = '90'
        else
            Az = '-90'
        end if
        
        v = abs(v) 
        !3.11056    45.76361  90 3.59 0 0 0
        write(s, '(f12.6,1x,f12.6,1x,A3,1x,f12.6,1x,A5,A10)') lon, lat, Az, v, "0 0 0", TabResid(i)%ini
        if (ONLYTAU == 'TAU') then
!		    if (TabResid(i)%tautest==.false.) then
		    if (.not. TabResid(i)%tautest) then
				 call Wfic(85, s)	
          end if 
        else 
			  call Wfic(85, s)
        end if
		
        !write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.6,1x,A5)')lon,lat,Az,v,"0 0 0"
            
    end do
    
    close(85)
end subroutine WGMT_resid_abs

! Drawing Linear Drifts
! For the moment we fill in the table
! for the drawing we will see later
subroutine WGMT_drift(nomfic, MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC
    real(8) :: lon, lat, v
    character (len=*) :: nomfic
    character(255) :: s
    integer :: i, j, dt, dk, np, ns, ng, row, g
    real(8) :: mini, maxi
    real(8), allocatable, dimension(:,:,:) :: Tabdrift
    real(8), allocatable, dimension(:) :: TabNdrift
    
    dt = MC%degre_t
    dk = MC%degre_k
    np = MC%Nb_profil
    ns = MC%Nb_sta
    ng = ngravimeter
    row = ns + np * (dt + dk)
    
    if (allocated(Tabdrift)) deallocate(Tabdrift)
    if (allocated(TabNdrift)) deallocate(TabNdrift)
    allocate (Tabdrift(ng,np,(dt + dk)))
    allocate (TabNdrift(ng))
    
    TabNdrift = 0.0d0
    Tabdrift = 0.0d0
    
    mini = -1.0d20
    maxi = 1.0d20
    
    if (dt > 0 .or. dk > 0) then
        do i = 1, np
            !WRITE(0,*)i
            do j = 1, ng
                if (TabProfil(i)%serial == TabGravi(j)%serial) then
                    g = j
                    exit
                end if
            end do 
            
            TabNdrift(g) = TabNdrift(g) + 1.0d0
            
            if (dt > 0) then
                row = ns + (i - 1) * (dt + dk) 
                do j = 1, dt
                    row = row + 1
                    Tabdrift(g, int(TabNdrift(g)), j) = MC%X(row)
                    if (j == 1) then
                        if (MC%X(row) < mini) mini = MC%X(row)
                        if (MC%X(row) > maxi) maxi = MC%X(row)
                    end if
                end do 
            end if
            
            if (dk > 0) then
                row = ns + (i - 1) * (dt + dk) + dt
                do j = 1, dk
                    Tabdrift(g, int(TabNdrift(g)), j + dt) = MC%X(row)
                end do
            end if         
        end do
    end if
    
    open(85,file = nomfic)
    
    do i = 1, nTabObsRel
        ! middle of the link
        lon = 0.5d0 * (TabStation(TabObsRel(i)%numsta_AR)%lon + TabStation(TabObsRel(i)%numsta_AV)%lon)
        lat = 0.5d0 * (TabStation(TabObsRel(i)%numsta_AR)%lat + TabStation(TabObsRel(i)%numsta_AV)%lat)
        v = 10.0d0 * MC%V(i)
        !3.11056    45.76361  90 3.59 0 0 0
        write(s, '(f12.6,1x,f12.6,1x,A2,1x,f12.6,1x,A5)') lon, lat, "90", v, "0 0 0"
        call Wfic(85, s)
    end do
    
    close(85)
    
    if (allocated(Tabdrift)) deallocate(Tabdrift)
    if (allocated(TabNdrift)) deallocate(TabNdrift)
    
end subroutine WGMT_drift

! Dessin des residus 
subroutine WGMT_fic_histo(nomfic, MC)
    use Raw_data
    use param_data
    use MC_data 
    
    implicit none
    type (Tmc), intent(inout) :: MC
    real(8) :: valeur, gauss, pi1 ,Norm
    character (len=*) :: nomfic
    real(8), dimension(18) :: hist
    character(255) :: s
    integer :: i
    
    pi1 = 4.0d0 * DATAN(1.0d0)
    
    do i = 1, 18
	    hist(i) = 2.0d0 * 100.0d0 * dble(MC%histo(i)) 
	    if (hist(i) > 55.0d0) hist(i) = 55.0d0
    end do
    
   
    open(85, file = nomfic)
    do i = 1, 18
	    valeur = dble(i) / 2.0d0 - 5.0d0
	    valeur = valeur + 0.25d0     
        write(s, '(f12.6,1x,f12.6)') valeur, hist(i)
        call Wfic(85, s)           
    end do
    close(85)
    
    open(86, file = 'gauss.txt')
    do i = -45, 45
      Norm = dble(i) / 10.0d0;
      gauss = 100d0 * (dexp(-0.5D0 *(Norm**2)))   / (dsqrt(2.0d0 * pi1));
      write(86, '(f12.6,1x,f12.6)') Norm, gauss
    end do 
           
    close(86)
end subroutine WGMT_fic_histo

! Write to file
! Avoid line length problems
! num is the number of the file where we want to write
subroutine Wfic(num, line)
    implicit none
    integer :: l, num
    character (len=*) :: line
    character (len=40) :: chfmt 
    l = len_trim(line)
    write (chfmt, 500) '(A',l,')'
    500  format (A, I4, A) 
    write(num, fmt=chfmt) line
    !write(num, '(A<l>)') line
end subroutine Wfic

end module GMT6
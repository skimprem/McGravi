module GMT
! Creation des fichiers pour la création des dessins sous GMT
 
! Numeros des fichiers 
! 80 : script
! 81 : points relatifs
! 82 : points absolus
! 83 : barres d'erreur sur les gravités
! 84 : profils
! 85 : residus relatifs et absolus
! 86 : courbe de Gauss
! 87 : script de calcul de l'emprise


type TGMT
    CHARACTER (len=255) GMTBIN ! GMTBIN directory
    CHARACTER (len=16)  PLOT_DEGREE_FORMAT
    CHARACTER (len=16)  PAPER_MEDIA
    CHARACTER (len=16)  ANNOT_FONT_SIZE_PRIMARY
    CHARACTER (len=16)  HEADER_FONT_SIZE
    CHARACTER (len=16)  LABEL_FONT_SIZE
    CHARACTER (len=16)  CHAR_ENCODING
    CHARACTER (len=16)  FRAME_WIDTH
    CHARACTER (len=16)  MEASURE_UNIT
    CHARACTER (len=16)  PAGE_ORIENTATION
    CHARACTER (len=46)  REGION
    CHARACTER (len=10)  PROJ
    CHARACTER (len=16)  X_SHIFT, Y_SHIFT
    INTEGER             POINT_NAME_FONT_SIZE

    REAL*8  :: W,E,N,S ! limites du dessin lon/lat
    REAL*8  :: XW,XE,YN,YS,DX,DY ! limites du dessin Easting/Northing
    REAL*8  :: PAGE_WIDTH,PAGE_HEIGHT,PAGE_RATIO
    REAL*8  :: MAP_WIDTH,MAP_HEIGHT,MAP_RATIO
end type TGMT

TYPE (TGMT) gmt_map ! GMT drawing parameters

contains

SUBROUTINE Init_gmt(g)
    USE param_data
    IMPLICIT NONE
    type (TGMT) g
    LOGICAL DEBUG
    REAL*8  :: DXX,DYY,a,b,OFFSET_X,OFFSET_Y, OFFSET_TITLE
    
    DEBUG = .FALSE.
    
    g%GMTBIN = ''

    g%PLOT_DEGREE_FORMAT = "ddd:mm"
    g%PAPER_MEDIA = "a3"
    g%ANNOT_FONT_SIZE_PRIMARY = "10"
    g%HEADER_FONT_SIZE = "14"
    g%LABEL_FONT_SIZE = "8"
    g%CHAR_ENCODING = "Standard+"
    g%FRAME_WIDTH = "0.1c"
    g%MEASURE_UNIT = "cm"
    g%PAGE_ORIENTATION = ""
    g%POINT_NAME_FONT_SIZE = 6
    
    IF (g%PAPER_MEDIA .EQ. "a4") THEN
        a = 0.297D2
        b = 0.210D2
    ELSEIF (g%PAPER_MEDIA .EQ. "a3") THEN
        a = 0.420D2
        b = 0.297D2
    ELSEIF (g%PAPER_MEDIA .EQ. "a2") THEN
        a = 0.594D2
        b = 0.420D2
    ENDIF
    
    g%N = param%lat2
    g%S = param%lat1
    g%W = param%lon1
    g%E = param%lon2
    
    write(g%REGION,17)' -R',param%lon1,'/',param%lat1,'/',param%lon2,'/',param%lat2,"r "
    17 format(A3,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A2)
    
    IF (DEBUG) THEN   
        WRITE(0,*)'W : ',g%W
        WRITE(0,*)'E : ',g%E
        WRITE(0,*)'S : ',g%S
        WRITE(0,*)'N : ',g%N
        WRITE(0,*)'REGION : ',g%REGION
    ENDIF

    ! Le calcul de l'emprise sert à déterminer le ration hauteur/largeur 
    ! pour choisir le sens de la feuille et l'échelle finale 
    CALL CALC_EMPRISE(g)
    g%MAP_RATIO = g%DY / g%DX

    IF ((g%MAP_RATIO) .GT. 1.0D0) THEN
        ! PORTRAIT
        g%PAGE_ORIENTATION = "portrait"
        g%PAGE_WIDTH = MIN(a,b)
        g%PAGE_HEIGHT = MAX(a,b)
    ELSE
        ! LANDSCAPE
        g%PAGE_ORIENTATION = "landscape"
        g%PAGE_WIDTH = MAX(a,b)
        g%PAGE_HEIGHT = MIN(a,b)
    ENDIF 
    g%PAGE_HEIGHT = g%PAGE_HEIGHT - 1.0D0 
    g%PAGE_RATIO = g%PAGE_HEIGHT / g%PAGE_WIDTH

    OFFSET_TITLE = 1.0D0

    IF ((g%MAP_RATIO) .LT. g%PAGE_RATIO) THEN
        IF (DEBUG) WRITE(0,*)'CAS MAP_RATIO < PAGE_RATIO'
        OFFSET_X = 1.0D0
        g%MAP_WIDTH = g%PAGE_WIDTH - 2.0D0 * OFFSET_X
        g%MAP_HEIGHT = g%MAP_WIDTH * g%MAP_RATIO
        OFFSET_Y = 0.5D0 * (g%PAGE_HEIGHT - g%MAP_HEIGHT)  
    ELSE
        IF (DEBUG) WRITE(0,*)'CAS MAP_RATIO > PAGE_RATIO'
        OFFSET_Y = 1.0D0
        g%MAP_HEIGHT = g%PAGE_HEIGHT - 2.0D0 * OFFSET_Y
        g%MAP_WIDTH = g%MAP_HEIGHT / g%MAP_RATIO 
        OFFSET_X = 0.5D0 * (g%PAGE_WIDTH - g%MAP_WIDTH)      
    ENDIF 

    WRITE(g%X_SHIFT,'(A3,E8.2,A2)')' -X',OFFSET_X,'c ' 
    WRITE(g%Y_SHIFT,'(A3,E8.2,A2)')' -Y',OFFSET_Y,'c '
    WRITE(g%PROJ,'(A4,F4.1,A2)')' -JM',g%MAP_WIDTH,'c '
       
    IF (DEBUG) THEN
        WRITE(0,*)'Easting W : ',g%XW
        WRITE(0,*)'Easting E : ',g%XE
        WRITE(0,*)'Northing S : ',g%YS
        WRITE(0,*)'Northing N : ',g%YN
        WRITE(0,*)'DX : ',g%DX
        WRITE(0,*)'DY : ',g%DY
        WRITE(0,*)'MAP_RATIO (DY/DX) : ',g%MAP_RATIO
        WRITE(0,*)'PAGE_RATIO (H/W) : ',g%PAGE_RATIO
        WRITE(0,*)'PAPER_MEDIA : ',g%PAPER_MEDIA
        WRITE(0,*)'PAGE_ORIENTATION : ',g%PAGE_ORIENTATION
	    WRITE(0,*)'PROJ : ',g%PROJ
        WRITE(0,*)'PAGE_WIDTH : ',g%PAGE_WIDTH
	    WRITE(0,*)'PAGE_HEIGHT : ',g%PAGE_HEIGHT
	    WRITE(0,*)'MAP_WIDTH : ',g%MAP_WIDTH
	    WRITE(0,*)'MAP_HEIGHT : ',g%MAP_HEIGHT
        WRITE(0,*)'X_SHIFT : ',g%X_SHIFT
	    WRITE(0,*)'Y_SHIFT : ',g%Y_SHIFT
    ENDIF
    

end SUBROUTINE Init_gmt

! Calcul de l'emprise des cartes (coordonnées E,N) 
! en utilisant la fonction mapproject de GMT
! JBL - 2010-05-19
! input :
! (TGMT) g : les champs W,E,N,S doivent être initialisés au préalable
! La SUBROUTINE affecte les champs XW,XE,YN,YS,DX,DY
SUBROUTINE CALC_EMPRISE(gmt_map)
    USE Portability_routines
    USE util_str
    IMPLICIT NONE
    TYPE (TGMT) gmt_map
    LOGICAL DEBUG  
    CHARACTER(350) s
    LOGICAL(4) resultat
    character (len=255) line 
    character (len=255) tabline(20)
    integer nc
    
    DEBUG = .TRUE.

    ! creation du script 
    open(87,file = 'EN_minmax.pl')
    
    WRITE(s,*)'$gmtset = "gmtset";'
    CALL Wfic(87,s)
    WRITE(s,*)'$mapproject = "mapproject";'
    CALL Wfic(87,s)
    WRITE(s,*)'`$gmtset MEASURE_UNIT ',gmt_map%MEASURE_UNIT,'`;'
    CALL Wfic(87,s)
    
    WRITE(s,*)'$region = "',gmt_map%REGION,'";'
    CALL Wfic(87,s)
    ! Pour le calcul du ratio LxH on prend une échelle arbitraire 1:1
    WRITE(s,*)'$proj = "',' -Jm1 ','";'
    CALL Wfic(87,s)

    ! creation du fichier de points en entree
    WRITE(s,*)'# Creating input points file'
    CALL Wfic(87,s)
    WRITE(s,*)'open (F,"> EN_minmax.in");'
    CALL Wfic(87,s)
    WRITE(s,*)'print F "',gmt_map%W,' ',gmt_map%N,'\n";'
    CALL Wfic(87,s)
    WRITE(s,*)'print F "',gmt_map%E,' ',gmt_map%S,'\n";'
    CALL Wfic(87,s)
    WRITE(s,*)'close(F);'
    CALL Wfic(87,s)
    
    s= "`$mapproject EN_minmax.in $region $proj -C > EN_minmax.out`;"
    CALL Wfic(87,s)
    
    CLOSE(87)

    resultat = run_system2('perl EN_minmax.pl')

    OPEN(87,FILE = 'EN_minmax.out')

    READ(87,'(A80)')line
    CALL decoupe_ligne(line,tabline,nc)
    IF (nc==2) THEN 
        IF  (strisnumber(tabline(1)) .AND. strisnumber(tabline(2))) THEN 
            gmt_map%XW = str2double(tabline(1))
            gmt_map%YN = str2double(tabline(2))
        END IF
    END IF

    READ(87,'(A80)')line
    CALL decoupe_ligne(line,tabline,nc)
    IF (nc==2) THEN 
        IF  (strisnumber(tabline(1)) .AND. strisnumber(tabline(2))) THEN 
            gmt_map%XE = str2double(tabline(1))
            gmt_map%YS = str2double(tabline(2))
        END IF
    END IF

    gmt_map%DX = gmt_map%XE - gmt_map%XW;
    gmt_map%DY = gmt_map%YN - gmt_map%YS;

    CLOSE(87)

END SUBROUTINE CALC_EMPRISE

SUBROUTINE MERCATOR(l,p,E,N)
    IMPLICIT NONE
    REAL*8, intent(inout):: p,l,E,N
    real*8 pi,dd2rad,Latiso,k0
    
    k0 = 1.0D0
    pi = 4.0D0 * atan(1.0D0)
    dd2rad = pi / 180.0D0
    Latiso = lat2latiso(p)
    E = k0 * dd2rad * l
    N = k0 * Latiso
end SUBROUTINE MERCATOR

! calcul de la latitude isometrique (version sphérique)
real*8 function lat2latiso(phi)
    IMPLICIT NONE
    real*8 pi, phi, dd2rad

    pi = 4.0D0 * atan(1.0D0)
    dd2rad = pi / 180.0D0	
    lat2latiso = log (tan(pi / 4.0D0 + dd2rad * phi / 2.0D0))
    RETURN 
END FUNCTION 



! creation et lancement du script GMT de dessin des profils
SUBROUTINE dessine_reseau(MC,run)
    USE MC_data 
    USE param_data
    USE Portability_routines
    USE sys_utils
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC
    logical(4) resultat
    logical run
    integer(4) resultatI

    CALL WGMT_bat_profils('profils.pl')
    CALL WGMT_pts_rel("pts_rel.txt")
    CALL WGMT_pts_abs("pts_abs.txt")
    CALL WGMT_profil("profils.txt",MC)

    if (run) then
	resultatI = rm('.gmtdefaults')
	 resultatI = rm('.gmtdefaults4')
        resultat = run_system2('perl  profils.pl')
    end if

end SUBROUTINE dessine_reseau


! creation et lancement du script GMT
SUBROUTINE run_GMT(MC,run)
    USE MC_data
    USE param_data
    USE Portability_routines
    USE sys_utils
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC
    CHARACTER (len=80) :: cmd
    integer(4) resultatI
    logical(4) resultat
    CHARACTER(5) systeme
    logical run
	
	
	
    CALL WGMT_bat('gmt.pl')
    CALL WGMT_sigma("error_bars.txt",MC)
	
	
	
    CALL WGMT_resid_rel("residus_std_rel.txt",MC,'NORM','ALL')
    CALL WGMT_resid_rel("residus_brut_rel.txt",MC,'BRUT','ALL')
    CALL WGMT_resid_abs("residus_std_abs.txt",MC,'NORM','ALL')
	! error :
    CALL WGMT_resid_abs("residus_brut_abs.txt",MC,'BRUT','ALL')
    CALL WGMT_resid_rel("residus_tau_rel.txt",MC,'NORM','TAU')
    CALL WGMT_resid_abs("residus_tau_abs.txt",MC,'NORM','TAU')
    
    
    
    CALL WGMT_leg_resid("legende_resid_std.txt",'NORM')
    CALL WGMT_leg_resid("legende_resid_brut.txt",'BRUT')
    CALL WGMT_leg_sigma("legende_sigma.txt")
    
    
    
    if (run) then
    
         resultatI = run_system('perl','gmt.pl')

         resultatI = move('gmt.pl',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('profils.pl',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('pts_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('pts_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('error_bars.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('profils.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_std_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_std_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_tau_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_tau_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_brut_rel.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('residus_brut_abs.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('legende_resid_brut.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('legende_resid_std.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = move('legende_sigma.txt',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Profils.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Error_bars.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Residus_norm.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Residus_bruts.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy('Residus_tautest.ps',param%dossier(1:len_trim(param%dossier)))
         resultatI = copy(param%nomficcal,param%dossier(1:len_trim(param%dossier)))
      
      
    end if
	
end SUBROUTINE run_GMT

! Ecriture du script GMT
SUBROUTINE WGMT_bat_profils(nomfic)
    USE param_data
    IMPLICIT NONE
    CHARACTER (len=*) nomfic
    CHARACTER(350) s
    CHARACTER string*(20)

    !WRITE(0,*)"Writing GMT script"
    open(80,file = nomfic)
    
    WRITE(s,*)'$gmtset = "gmtset";'
    CALL Wfic(80,s)
    WRITE(s,*)'$pscoast = "pscoast";'
    CALL Wfic(80,s)
    WRITE(s,*)'$psxy = "psxy";'
    CALL Wfic(80,s)
    WRITE(s,*)'$pstext = "pstext";'
    CALL Wfic(80,s)
     
    WRITE(s,*)'`$gmtset PAPER_MEDIA ',gmt_map%PAPER_MEDIA,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset PAGE_ORIENTATION ',gmt_map%PAGE_ORIENTATION,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset CHAR_ENCODING ',gmt_map%CHAR_ENCODING,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset MEASURE_UNIT ',gmt_map%MEASURE_UNIT,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset PLOT_DEGREE_FORMAT ',gmt_map%PLOT_DEGREE_FORMAT,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset ANNOT_FONT_SIZE_PRIMARY ',gmt_map%ANNOT_FONT_SIZE_PRIMARY,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset HEADER_FONT_SIZE ',gmt_map%HEADER_FONT_SIZE,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset LABEL_FONT_SIZE ',gmt_map%LABEL_FONT_SIZE,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset FRAME_WIDTH ',gmt_map%FRAME_WIDTH,'`;'
    CALL Wfic(80,s)
    
    WRITE(s,*)'$region = "',gmt_map%REGION,'";'
    CALL Wfic(80,s)
    WRITE(s,*)'$proj = "',gmt_map%PROJ,'";'
    CALL Wfic(80,s)
    WRITE(s,*)'$x_shift = "',gmt_map%X_SHIFT,'";'
    CALL Wfic(80,s)
    WRITE(s,*)'$y_shift = "',gmt_map%Y_SHIFT,'";'
    CALL Wfic(80,s)
    
    s= "`$pscoast $region $proj &
    &-B1.00g1.00f1.00:.""Observations relatives et absolues"":&
    & -Df -K -A0/1 -N1 -S210 &
    & -W1 -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 $x_shift $y_shift > Profils.ps`;"
    !-Lf6.5/42.5/45/50k
   
    CALL Wfic(80,s)
    CALL Wfic(80,"`$psxy $region $proj -M -W1.5/120/120/120 -K -O profils.txt >> Profils.ps`;")
    
    CALL Wfic(80,"`$psxy $region $proj -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Profils.ps`;")
    CALL Wfic(80,"`$pstext $region $proj -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Profils.ps`;")
        
    CALL Wfic(80,"`$psxy $region $proj -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Profils.ps`;")
    CALL Wfic(80,"`$pstext $region $proj -S5/255/220/150 -O  -G0 -Dj0.05 pts_abs.txt >> Profils.ps`;")
    
    close(80)
    
end SUBROUTINE WGMT_bat_profils

! Ecriture du script GMT
SUBROUTINE WGMT_bat_histo(nomfic,nomfic_histo,nomfic_ps,nomfic_png)
    USE param_data
    USE Portability_routines
    USE sys_utils
    IMPLICIT NONE
    CHARACTER (len=*) nomfic,nomfic_ps,nomfic_histo,nomfic_png
    CHARACTER(255) s
    CHARACTER string*(20)
    integer(4) resultatI
    logical(4) resultat

    !WRITE(0,*)"Writing GMT script"
    open(80,file = nomfic)
    
    WRITE(s,*)'$gmtset = "gmtset";'
    CALL Wfic(80,s)
    WRITE(s,*)'$psbasemap = "psbasemap";'
    CALL Wfic(80,s)
    WRITE(s,*)'$psxy = "psxy";'
    CALL Wfic(80,s)
    
    CALL Wfic(80,"`$gmtset PAGE_ORIENTATION  portrait`;")
    CALL Wfic(80,"`$gmtset PAPER_MEDIA a4`;")
    CALL Wfic(80,"`$gmtset ANNOT_FONT_SIZE_PRIMARY 12`;")
    CALL Wfic(80,"`$gmtset HEADER_FONT_SIZE 10p`;")
    CALL Wfic(80,"`$gmtset LABEL_FONT_SIZE 8`;")
    CALL Wfic(80,"`$gmtset CHAR_ENCODING  Standard+`;")
    CALL Wfic(80,"`$gmtset FRAME_WIDTH 0.1c`;")
    CALL Wfic(80,"`$gmtset MEASURE_UNIT cm`;")
    
    if (param%Type_resid) then         
        CALL Wfic(80,'`$psbasemap -R-4.5/+4.5/0/60 -JX16c/20c -Bf0.5a0.5:""&
        &:/f10a10:"\045":WS:."Histogramme des r\345sidus standards": -K > '//nomfic_ps//'`;')
    else
        CALL Wfic(80,'`$psbasemap -R-4.5/+4.5/0/60 -JX16c/20c -Bf0.5a0.5:""&
        &:/f10a10:"\045":WS:."Histogramme des r\345sidus normalis\345s": -K > '//nomfic_ps//'`;')
    end if
    s = "`$psxy   -R  -JX -G0/0/255 -W0.5p  -Sb0.9c -N -O -K " // nomfic_histo //' >> '//nomfic_ps//'`;'
    CALL Wfic(80,s) 
    CALL Wfic(80,'`$psxy   -R  -JX -W0.8p/255/0/0  gauss.txt -N -O >> '//nomfic_ps//'`;')
   
    close(80)
    
    resultatI = run_system('perl',nomfic)
    resultatI = run_system ('convert',nomfic_ps//' '//nomfic_png)

    resultatI = move(nomfic_histo(1:len_trim(nomfic_histo)),param%dossier(1:len_trim(param%dossier)))
    resultatI = move(nomfic,param%dossier(1:len_trim(param%dossier)))
    resultatI = move(nomfic_ps,param%dossier(1:len_trim(param%dossier)))
    resultatI = move('gauss.txt',param%dossier(1:len_trim(param%dossier)))
    resultatI = copy(nomfic_png,param%dossier(1:len_trim(param%dossier)))
       
end SUBROUTINE WGMT_bat_histo


! Ecriture du script GMT
SUBROUTINE WGMT_bat(nomfic)
    USE param_data
    IMPLICIT NONE
    CHARACTER (len=*) nomfic   
    CHARACTER(300) s
    CHARACTER string*(20)

    17 format(A3,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1)
    18 format(A3,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1,SP,D9.3,A1)

    WRITE(0,*)"Writing GMT script"
    open(80,file = nomfic)

    WRITE(s,*)'$gmtset = "gmtset";'
    CALL Wfic(80,s)
    WRITE(s,*)'$pscoast = "pscoast";'
    CALL Wfic(80,s)
    WRITE(s,*)'$psxy = "psxy";'
    CALL Wfic(80,s)
    WRITE(s,*)'$pstext = "pstext";'
    CALL Wfic(80,s)
     
    WRITE(s,*)'`$gmtset PAPER_MEDIA ',gmt_map%PAPER_MEDIA,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset PAGE_ORIENTATION ',gmt_map%PAGE_ORIENTATION,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset CHAR_ENCODING ',gmt_map%CHAR_ENCODING,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset MEASURE_UNIT ',gmt_map%MEASURE_UNIT,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset PLOT_DEGREE_FORMAT ',gmt_map%PLOT_DEGREE_FORMAT,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset ANNOT_FONT_SIZE_PRIMARY ',gmt_map%ANNOT_FONT_SIZE_PRIMARY,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset HEADER_FONT_SIZE ',gmt_map%HEADER_FONT_SIZE,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset LABEL_FONT_SIZE ',gmt_map%LABEL_FONT_SIZE,'`;'
    CALL Wfic(80,s)
    WRITE(s,*)'`$gmtset FRAME_WIDTH ',gmt_map%FRAME_WIDTH,'`;'
    CALL Wfic(80,s)
    
    WRITE(s,*)'$region = "',gmt_map%REGION,'";'
    CALL Wfic(80,s)
    WRITE(s,*)'$proj = "',gmt_map%PROJ,'";'
    CALL Wfic(80,s)
    WRITE(s,*)'$x_shift = "',gmt_map%X_SHIFT,'";'
    CALL Wfic(80,s)
    WRITE(s,*)'$y_shift = "',gmt_map%Y_SHIFT,'";'
    CALL Wfic(80,s)
 
    ! carte des ecarts-types sur les pesanteurs 
    s= "`$pscoast $region $proj -B1.00g1.00f1.00:.""Ecarts-types sur les pesanteurs estim\345es"": &
    & -Df -K -A0/1 -N1 -S210 &
    & -W1 -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255  $x_shift $y_shift > Error_bars.ps`;" 
    !-Lf6.5/42.5/45/50k       
    CALL Wfic(80,s)
    CALL Wfic(80,"`$psxy error_bars.txt -R -JM  -W1/255/0/0  -G255/0/0 &
    & -Sc0.001c -Ey0.3c/255/0/0 -O -K >> Error_bars.ps`;")  
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O  -K  -G0 -Dj0.05   legende_sigma.txt >> Error_bars.ps`;")    
    CALL Wfic(80,"`$psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Error_bars.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150  -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Error_bars.ps`;")      
    CALL Wfic(80,"`$psxy -R   -JM  -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Error_bars.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O  -G0 -Dj0.05 pts_abs.txt >> Error_bars.ps`;")
  
    ! carte des residus normalises
    s= "`$pscoast $region $proj -B1.00g1.00f1.00:.""R\345sidus normalis\345s"": -Df -K -A0/1 -N1 -S210 &
    & -W1  -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 $x_shift $y_shift > Residus_norm.ps`;"  
    !-Lf6.5/42.5/45/50k    
    CALL Wfic(80,s)   
    CALL Wfic(80,"`$psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Residus_norm.ps`;")
    CALL Wfic(80,"`$psxy  -R -JM  -W1/0/255/0 -G0/255/0 -Sv0.02c/0.04c/0.04c &
    & -O -K residus_std_rel.txt >> Residus_norm.ps`;")
    
    IF (param%mode .EQ. 2) THEN
	CALL Wfic(80,"`$psxy  -R -JM  -W1/255/0/255  -G255/0/255 -Sv0.02c/0.04c/0.04c &
	& -O -K residus_std_abs.txt >> Residus_norm.ps`;")   
    ENDIF
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O -K -G0 -Dj0.05   legende_resid_std.txt >> Residus_norm.ps`;")  
    CALL Wfic(80,"`$psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Residus_norm.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Residus_norm.ps`;")     
    CALL Wfic(80,"`$psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Residus_norm.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150  -O  -G0 -Dj0.05 pts_abs.txt >> Residus_norm.ps`;")


    ! carte des residus normalises ne passant pas le tau-test
    s= "`$pscoast $region $proj -B1.00g1.00f1.00:.""R\345sidus normalis\345s \345chouant au tau-test"": &
    &-Df -K -A0/1 -N1 -S210 &
    & -W1 -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 $x_shift $y_shift > Residus_tautest.ps`;"   
    !-Lf6.5/42.5/45/50k    
    CALL Wfic(80,s)   
    CALL Wfic(80,"`$psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Residus_tautest.ps`;")
    CALL Wfic(80,"`$psxy  -R -JM  -W1/0/255/0 -G0/255/0 -Sv0.02c/0.04c/0.04c  -O -K &
    & residus_tau_rel.txt >> Residus_tautest.ps`;")
    CALL Wfic(80,"`$psxy  -R -JM  -W1/255/0/255  -G255/0/255 -Sv0.02c/0.04c/0.04c  -O -K &
    & residus_tau_abs.txt >> Residus_tautest.ps`;")   
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O -K -G0 -Dj0.05  &
    & legende_resid_std.txt >> Residus_tautest.ps`;")  
    CALL Wfic(80,"`$psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Residus_tautest.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Residus_tautest.ps`;")     
    CALL Wfic(80,"`$psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Residus_tautest.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150  -O  -G0 -Dj0.05 pts_abs.txt >> Residus_tautest.ps`;")

    ! carte des residus bruts
    s= "`$pscoast $region $proj -B1.00g1.00f1.00:.""R\345sidus"": -Df -K -A0/1 -N1 -S210 &
    & -W1 -G255/220/150 -S150/255/255 -I1/0.5p/0/0/255 &
    & -I2/0.5p/0/0/255 -I3/0.5p/0/0/255 $x_shift $y_shift > Residus_bruts.ps`;" 
    !-Lf6.5/42.5/45/50k   
    CALL Wfic(80,s)   
    CALL Wfic(80,"`$psxy -R  -JM -M -W1.5/120/120/120 -K -O profils.txt >> Residus_bruts.ps`;")
    CALL Wfic(80,"`$psxy  -R -JM  -W1/0/255/0 -G0/255/0 -Sv0.02c/0.04c/0.04c &
    & -O -K residus_brut_rel.txt >> Residus_bruts.ps`;")
    IF (param%mode .EQ. 2) THEN
        CALL Wfic(80,"`$psxy  -R -JM  -W1/255/0/255  -G255/0/255 -Sv0.02c/0.04c/0.04c &
        & -O -K residus_brut_abs.txt >> Residus_bruts.ps`;")  
    ENDIF
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O -K -G0 -Dj0.05 &
    &  legende_resid_brut.txt >> Residus_bruts.ps`;") 
    CALL Wfic(80,"`$psxy -R  -JM -Sc0.075 -G0/0/0 -K -O pts_rel.txt >> Residus_bruts.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150 -O -K -G94/151/106 -Dj0.05  pts_rel.txt >> Residus_bruts.ps`;")     
    CALL Wfic(80,"`$psxy -R   -JM -St0.25 -G255/0/0 -K -O  pts_abs.txt >> Residus_bruts.ps`;")
    CALL Wfic(80,"`$pstext -R -JM -S5/255/220/150  -O  -G0 -Dj0.05 pts_abs.txt >> Residus_bruts.ps`;")

    close(80)

end SUBROUTINE WGMT_bat

SUBROUTINE WGMT_pts_rel(nomfic)
    USE Raw_data
    USE param_data
    IMPLICIT NONE
    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    integer i
    open(81,file = nomfic)

    do i=1,NTabStation
        write(s,'(f12.6,1x,f12.6,1x,I2,1x,A7,1x,A8)')TabStation(i)%lon,&
		&TabStation(i)%lat,gmt_map%POINT_NAME_FONT_SIZE," 0 1 0",TabStation(i)%nomsta
        CALL Wfic(81,s)
    end do
    
    close(81)
end SUBROUTINE WGMT_pts_rel

SUBROUTINE WGMT_leg_sigma(nomfic)
    USE Raw_data
    USE param_data
    IMPLICIT NONE
    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    real*8 dlat,dlon,sigma,lat, lon
    integer i
    open(81,file = nomfic)

    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10.0D0
    lon = param%lon1 + dlon / 10.0D0 + 0.1D0
    write(s,'(f12.6,1x,f12.6,1x,A7,1x,A20)')lon,lat,"8 0 1 0","25 \225Gal"
    CALL Wfic(81,s)
    
    close(81)
end SUBROUTINE WGMT_leg_sigma

SUBROUTINE WGMT_leg_resid(nomfic,BRUTouNORM)
    USE Raw_data
    USE param_data
    IMPLICIT NONE
    CHARACTER (len=*) nomfic
    CHARACTER (len=4) BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    CHARACTER(255) s
    real*8 dlat,dlon,sigma,lat, lon
   
    open(81,file = nomfic)

    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10.0D0
    lon = param%lon1 + dlon / 10.0D0 + 0.1D0
    if (BRUTouNORM=='BRUT') then
      write(s,'(f12.6,1x,f12.6,1x,A7,1x,A10)')lon,lat,"8 0 1 0","10 \225Gal"
    else
	   write(s,'(f12.6,1x,f12.6,1x,A7,1x,A3)')lon,lat,"8 0 1 0","1"
    endif
   
    CALL Wfic(81,s)
    
    close(81)
end SUBROUTINE WGMT_leg_resid

SUBROUTINE WGMT_pts_abs(nomfic)
    USE Raw_data
    USE param_data
    IMPLICIT NONE
    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    integer i,j
    open(82,file = nomfic)

    do i=1,NTabStation
        do j=1,param%Nb_obsAbs
            if (TabStation(i)%nomsta == TabObsAbs(j)%nomsta) then
            !if (TabStation(i)%nomsta == fixstn(j)) then
                write(s,'(f12.6,1x,f12.6,1x,I2,1x,A7,1x,A8)')TabStation(i)%lon,&
				&TabStation(i)%lat,gmt_map%POINT_NAME_FONT_SIZE," 0 1 0",TabStation(i)%nomsta
                CALL Wfic(82,s)
            end if
        
        end do
    end do
    
    close(82)
end SUBROUTINE WGMT_pts_abs

SUBROUTINE WGMT_sigma(nomfic,MC)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC

    real*8 dlat,dlon,sigma,lat, lon
    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    integer i,j
    open(83,file = nomfic)
    
    do i=1,NTabStation
        write(s,'(f12.6,1x,f12.6,1x,f12.6,1x,A8)')TabStation(i)%lon,TabStation(i)%Lat,MC%sig(TabStation(i)%numsta),"0 0 0 0"
        CALL Wfic(83,s)
    end do
    
    
    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    lat = param%lat1 + dlat / 10.0D0
    lon = param%lon1 + dlon / 10.0D0
    sigma = 0.025D0
    write(s,'(f12.6,1x,f12.6,1x,f12.4,1x,A8)')lon,lat,sigma,"0 0 0 0"
    CALL Wfic(83,s)

    
    close(83)
end SUBROUTINE WGMT_sigma

! Dessin des profils pour detecter les zones de faiblesse du réseau
SUBROUTINE WGMT_profil3(nomfic,MC)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC

    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    integer i,j, nprofil
	
    open(84,file = nomfic)
    nprofil = 0
    do i=1,nTabObsRel
		obs_rel = TabObsRel(i)
        if (Obs_Rel%profil/=nprofil) then
            CALL Wfic(84,">")
			nprofil = obs_Rel%profil
		ENDIF
		
		write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8,1x,A20)')TabStation(Obs_Rel%numsta_AR)%lon,&
            &TabStation(Obs_Rel%numsta_AR)%lat,"8 0 1 0",TabStation(Obs_Rel%numsta_AR)%nomsta,Obs_Rel%profil
		CALL Wfic(84,s)
		
        write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(Obs_Rel%numsta_AV)%lon,&
            &TabStation(Obs_Rel%numsta_AV)%lat,"8 0 1 0",TabStation(Obs_Rel%numsta_AV)%nomsta
        CALL Wfic(84,s)
    end do
    
    close(84)
end SUBROUTINE WGMT_profil3

! Dessin des profils pour detecter les zones de faiblesse du réseau
SUBROUTINE WGMT_profil(nomfic,MC)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC

    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    integer i,j
	
    open(84,file = nomfic)
    obs_rel = TabObsRel(1)
    CALL Wfic(84,">")
    write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8,1x,I4,1x,A20)')TabStation(obs_rel%numsta_AR)%lon,&
    &TabStation(obs_rel%numsta_AR)%lat,"8 0 1 0",TabStation(obs_rel%numsta_AR)%nomsta,&
	&TabObsRel(1)%profil,param%TabDataFic(TabObsRel(1)%profil)%nom
    !write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(obs_rel%numsta_AR)%lon,&
    !&TabStation(obs_rel%numsta_AR)%lat,"8 0 1 0",TabStation(obs_rel%numsta_AR)%nomsta
    CALL Wfic(84,s)
	write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(obs_rel%numsta_AV)%lon,&
    &TabStation(obs_rel%numsta_AV)%lat,"8 0 1 0",TabStation(obs_rel%numsta_AV)%nomsta
    CALL Wfic(84,s)
    do i=2,nTabObsRel
        if (TabObsRel(i)%profil/=obs_rel%profil) then
            CALL Wfic(84,">")
            write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8,1x,I4,1x,A20)')TabStation(TabObsRel(i)%numsta_AR)%lon,&
            &TabStation(TabObsRel(i)%numsta_AR)%lat,"8 0 1 0",TabStation(TabObsRel(i)%numsta_AR)%nomsta,&
			&TabObsRel(i)%profil,param%TabDataFic(TabObsRel(i)%profil)%nom
            CALL Wfic(84,s)
			write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(TabObsRel(i)%numsta_AV)%lon,&
            &TabStation(TabObsRel(i)%numsta_AV)%lat,"8 0 1 0",TabStation(TabObsRel(i)%numsta_AV)%nomsta
            CALL Wfic(84,s)
        else
            if (TabObsRel(i)%numsta_AV/=TabObsRel(i)%numsta_AR) then
                write(s,'(f12.6,1x,f12.6,1x,A7,1x,A8)')TabStation(TabObsRel(i)%numsta_AV)%lon,&
                &TabStation(TabObsRel(i)%numsta_AV)%lat,"8 0 1 0",TabStation(TabObsRel(i)%numsta_AV)%nomsta
                CALL Wfic(84,s)
	end if
        end if 
        obs_rel = TabObsRel(i)
    end do
    
    close(84)
end SUBROUTINE WGMT_profil

! Dessin des residus 
SUBROUTINE WGMT_resid_rel(nomfic,MC,BRUTouNORM,ONLYTAU)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC
    real*8 lon, lat, v, moy, dlat, dlon
    CHARACTER (len=*) nomfic
    CHARACTER (len=4) BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    CHARACTER (len=3) ONLYTAU ! TAU ou ALL ; on appelle la procedure 2 fois
    CHARACTER (len=3) Az
    CHARACTER(255) s
    integer i,j
    
    open(85,file = nomfic)
    moy = 0.0D0
    do i=1,nTabObsRel
    
        ! milieu de la liaison
        lon = 0.5D0 * (TabStation(TabObsRel(i)%numsta_AR)%lon + TabStation(TabObsRel(i)%numsta_AV)%lon)
        lat = 0.5D0 * (TabStation(TabObsRel(i)%numsta_AR)%lat + TabStation(TabObsRel(i)%numsta_AV)%lat)
        if (BRUTouNORM=='BRUT') then
            v = 100.0D0 * TabResid(i)%resid
        else
            if (param%Type_resid) then
               v = TabResid(i)%std_res
            else
               v = TabResid(i)%Norm_res
            endif
        endif
        !3.11056    45.76361  90 3.59 0 0 0
        if (v>0.0D0) then
            Az = '90'
        else
            Az = '-90'
        end if
        v = abs(v)
        write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5,A10,A10)')lon,lat,Az,v,"0 0 0",TabResid(i)%ini,TabResid(i)%fin

        if (ONLYTAU=='TAU') then
!		    if ( TabResid(i)%tautest==.false.) then
		    if (.not. TabResid(i)%tautest) then
				CALL Wfic(85,s)	
          endif 
        else 
			 CALL Wfic(85,s)
        endif
        
        !write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5)')lon,lat,Az,v,"0 0 0"
        
            
    end do
    
    !moy = moy /nTabObsRel
    moy = 1.0D0
    dlat = param%lat2 - param%lat1
    dlon = param%lon2 - param%lon1
    write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.4,1x,A5)')param%lon1+dlon/10.0D0,param%lat1+dlat/10.0D0,"90",moy,"0 0 0"
    CALL Wfic(85,s)
      
    close(85)

end SUBROUTINE WGMT_resid_rel

! Dessin des residus 
SUBROUTINE WGMT_resid_abs(nomfic,MC,BRUTouNORM,ONLYTAU)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC
    real*8 lon, lat, v, moy
    CHARACTER (len=*) nomfic
    CHARACTER (len=4) BRUTouNORM ! BRUT ou NORM ; on appelle la procedure 2 fois
    CHARACTER (len=3) ONLYTAU ! TAU ou ALL ; on appelle la procedure 2 fois
    CHARACTER (len=3) Az
    CHARACTER(255) s
    integer i,j,num_pt
    
    open(85,file = nomfic)
    moy = 0.0D0
    
    !WRITE(0,*)nTabObsRel,MC%Nb_obsAbs
    do i=nTabObsRel+1,nTabObsRel+MC%Nb_obsAbs
        
        num_pt = pos(i-nTabObsRel)

        lon = TabStation(num_pt)%lon
        lat = TabStation(num_pt)%lat
        
        ! error :
        if (BRUTouNORM=='BRUT') then
			! write(0,*)TabResid(i)%resid
            v = 100.0D0 * TabResid(i)%resid
        else
            if (param%Type_resid) then
               v = TabResid(i)%std_res
            else
               v = TabResid(i)%Norm_res
            endif
        endif
		
		
        if (v>0.0D0) then
            Az = '90'
        else
            Az = '-90'
        end if
        
        v = abs(v) 
        !3.11056    45.76361  90 3.59 0 0 0
        write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.6,1x,A5,A10)')lon,lat,Az,v,"0 0 0",TabResid(i)%ini
        if (ONLYTAU=='TAU') then
!		    if (TabResid(i)%tautest==.false.) then
		    if (.not. TabResid(i)%tautest) then
				 CALL Wfic(85,s)	
          endif 
        else 
			  CALL Wfic(85,s)
        endif
		
        !write(s,'(f12.6,1x,f12.6,1x,A3,1x,f12.6,1x,A5)')lon,lat,Az,v,"0 0 0"
       
            
    end do
    
    close(85)
end SUBROUTINE WGMT_resid_abs

! Dessin des drifts linéaires
! Pour l'instant on remplit le tableau
! pour le dessin on verra plus tard 
SUBROUTINE WGMT_drift(nomfic,MC)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC
    real*8 lon, lat, v
    CHARACTER (len=*) nomfic
    CHARACTER(255) s
    integer i,j,dt,dk,np,ns,ng,row,g
    real*8 mini,maxi
    real*8,allocatable,dimension(:,:,:)::Tabdrift
    real*8,allocatable,dimension(:)::TabNdrift
    
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
    
    TabNdrift = 0.0D0
    Tabdrift = 0.0D0
    
    mini = -1.0D20
    maxi = 1.0D20
    
    if (dt>0 .or. dk>0) then
        do i=1,np
            !WRITE(0,*)i
            do j=1,ng
                if (TabProfil(i)%serial==TabGravi(j)%serial) then
                    g = j
                    exit
                end if
            end do 
            
            TabNdrift(g) = TabNdrift(g) + 1.0D0
            
            if (dt>0) then
                row = ns + ( i - 1 )*( dt + dk ) 
                do j=1,dt
                    row = row + 1
                    Tabdrift(g,INT(TabNdrift(g)),j) = MC%X(row)
                    if (j==1) then
                        if (MC%X(row)<mini) mini = MC%X(row)
                        if (MC%X(row)>maxi) maxi = MC%X(row)
                    end if
                end do 
            end if
            
            if (dk>0) then
                row = ns + ( i - 1 )*( dt + dk ) + dt
                do j=1,dk
                    Tabdrift(g,INT(TabNdrift(g)),j+dt) = MC%X(row)
                end do
            end if         
    
        end do
        
    end if
    
    
    open(85,file = nomfic)
    
    do i=1,nTabObsRel
    
        ! milieu de la liaison
        lon = 0.5D0 * (TabStation(TabObsRel(i)%numsta_AR)%lon + TabStation(TabObsRel(i)%numsta_AV)%lon)
        lat = 0.5D0 * (TabStation(TabObsRel(i)%numsta_AR)%lat + TabStation(TabObsRel(i)%numsta_AV)%lat)
        v = 10.0D0 * MC%V(i)
        !3.11056    45.76361  90 3.59 0 0 0
        write(s,'(f12.6,1x,f12.6,1x,A2,1x,f12.6,1x,A5)')lon,lat,"90",v,"0 0 0"
        CALL Wfic(85,s)
            
    end do
   
    
    close(85)
    
    if (allocated(Tabdrift)) deallocate(Tabdrift)
    if (allocated(TabNdrift)) deallocate(TabNdrift)
    
end SUBROUTINE WGMT_drift

! Dessin des residus 
SUBROUTINE WGMT_fic_histo(nomfic,MC)
    USE Raw_data
    USE param_data
    USE MC_data 
    
    IMPLICIT NONE
    type (Tmc), intent(inout):: MC
    real*8 valeur, gauss, pi1 ,Norm
    CHARACTER (len=*) nomfic
    real*8,dimension(18)::hist
    CHARACTER(255) s
    integer i
    
    pi1 = 4.0D0 * DATAN(1.0D0)
    
    do i=1,18
	    hist(i) = 2.0D0 * 100.0D0 * DBLE(MC%histo(i)) 
	    if (hist(i)>55.0D0) hist(i)=55.0D0
    end do
    
   
    open(85,file = nomfic)
    do i=1,18
	    valeur = DBLE(i) / 2.0D0 -5.0D0
	    valeur = valeur + 0.25D0     
        write(s,'(f12.6,1x,f12.6)')valeur,hist(i)
        CALL Wfic(85,s)           
    end do
    close(85)
    
    open(86,file = 'gauss.txt')
    do i = -45,45
      Norm = DBLE(i) / 10.0D0;
      gauss = 100d0 * (DEXP(-0.5D0 *(Norm**2)))   / (DSQRT(2.0D0*pi1));
      write(86,'(f12.6,1x,f12.6)')Norm,gauss
    end do 
           
    close(86)
end SUBROUTINE WGMT_fic_histo

! Ecrit dans un fichier 
! Permet d'eviter les problèmes de longueur de ligne
! num est le numéro du fichier ou on veut ecrire
SUBROUTINE Wfic(num,line)
    IMPLICIT NONE
    integer l,num
    CHARACTER (len=*) :: line
    CHARACTER (len=40) :: CHFMT 
    l = len_trim(line)
    WRITE (CHFMT,500)'(A',l,')'
    500  FORMAT (A,I4,A) 
    write(num,FMT=CHFMT)line
    !write(num,'(A<l>)')line
end SUBROUTINE Wfic

end module GMT

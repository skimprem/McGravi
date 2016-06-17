module write_matr
! Export texte des matrices du calcul (déboggage)

implicit none

contains

subroutine write_mat(A,nl,nc,nomfic)
    ! Procedure d'export des matrices de calcul vers des fichiers textes
    use param_data

    implicit none

    INTEGER:: i, j, nl, nc, len_nomfic,len_dossier
    CHARACTER (len=*) nomfic

    CHARACTER (len=255) s
    real*8::A(nl,nc)
    real*8::val

    LOGICAL DEBUG
    DEBUG = .FALSE.

    IF (DEBUG) WRITE(0,*)'WRITE_MAT : START'
    !write(0,*)'Writing >',nomfic,'<'
    !nomfic=trim(nomfic)
    open (150,file=nomfic)

    write(150,*)nl,' lignes ',nc,' colonnes'

    do i=1,nl
        write(150,*)'ligne = ',i
        do j=1,nc
            val = A(i,j)
            write(150,*)val
        end do

    end do
    close(150)
    
    IF (DEBUG) WRITE(0,*)'WRITE_MAT : END'
    return
end subroutine write_mat

! subroutine write_oct_mat(A,nl,nc,nomfic)
! Procedure d'export des matrices de calcul vers des fichiers *.m 
! format MatLab ou GNU Octave
! jbl - 2010-05-20
! export element par element pour eviter les problèmes de longueur de ligne
SUBROUTINE write_oct_mat(A,nl,nc,nom,nomfic)

    USE param_data

    IMPLICIT NONE

    INTEGER:: i, j, nl, nc, len_nomfic,len_dossier
    CHARACTER (len=*) nomfic ! nom du fichier
    CHARACTER (len=*) nom ! nom de la matrice
    REAL*8::A(nl,nc)
    REAL*8::val

    LOGICAL DEBUG
    DEBUG = .FALSE.
    
    !nomfic=trim(nomfic)
    WRITE(0,*)'Writing ',nomfic
    OPEN (150,FILE='debug'//param%dsep//nomfic)
    WRITE(150,*)'% Matrice ',nom
    WRITE(150,*)'% ',nl,' lignes ',nc,' colonnes'
    WRITE (150,*)nom,' = zeros(',nl,',',nc,');'

    DO i=1,nl
        DO j=1,nc
            write(150,*)nom,'(',i,',',j,') = ',A(i,j),';'   
        END DO
    END DO

    CLOSE(150)
    
    RETURN
END SUBROUTINE write_oct_mat

subroutine write_vec(A,nl,nomfic)
    ! Procedure d'export des matrices de calcul vers des fichiers textes
    use param_data

    implicit none

    INTEGER:: i, j, nl, len_nomfic,len_dossier
    CHARACTER (len=*) nomfic

    CHARACTER (len=255) s
    real*8::A(nl)
    real*8::val
    
    nomfic=trim(nomfic)
    write(0,*)'Writing ',nomfic
    open (150,file=nomfic)
    write(150,*)nl,' lignes '

    do i=1,nl
        val = A(i)
        write(150,*)'ligne = ',i, val
    end do
    
    close(150)
    
    return
end subroutine write_vec

subroutine write_mat_Scilab(A,nl,nc,nomfic)
    ! Procedure d'export des matrices de calcul vers des fichiers textes

    implicit none
    INTEGER::i,j,nl,nc
    CHARACTER (len=*) nomfic
    CHARACTER (len=255) :: ligne
    real*8::A(nl,nc)
    real*8::val
    open (150,file=nomfic)

    nomfic=trim(nomfic)
    write(0,*)'Writing ',nomfic

    write(150,*)'//',nl,' lignes ',nc,' colonnes'
    
    write(150,*)nomfic,'=['
    do i=1,nl 
        write(150,*)'ligne = ',i
        do j=1,nc
            !val = int(1000*A(i,j))/1000
            val = A(i,j)
            !ligne  = ligne//val
            write(150,*)val
        end do

    end do
    close(150)
    
    return
end subroutine write_mat_Scilab

end module

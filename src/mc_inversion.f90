Module MC_inversion
! Module d'inversion du système des moindres carrés
implicit none

contains

integer function calcul(MC,mode)
    
    use write_matr
    use param_data
    use str_const
    use MC_data
    
    implicit none

    ! Codes d'erreur
    ! 110 : matrice normale non PSD
    ! 111 : degré de liberte nul

	! Parameters
    type (Tmc), intent(inout):: MC
    integer mode

	! Local variables
    integer i,j,N,k,ij,il,code 
    real*8 sigma0,vtpv,sigma0_2,vtmp
   
    ! Automatic arrays, but by allocation to avoid stack overflow, make sure
    ! to deallocate them
    real*8,allocatable,dimension(:)::c,uvec
    

    LOGICAL DEBUG
    DEBUG = .FALSE.

    IF (DEBUG) WRITE(0,*)'CALCUL : START'

    IF (DEBUG) WRITE(0,*)'CALCUL : tmp arrays allocation'

    if (allocated(c)) deallocate(c)
    if (allocated(uvec)) deallocate(uvec)

	WRITE(0,*)'Unknown number : ',MC%Nb_inc
    
	! allocation de la taille des matrices 
    allocate (c((MC%Nb_inc+1)*MC%Nb_inc/2))
    allocate (uvec(MC%Nb_inc))
    
    code = 0
   
    if (param%writemat) call write_mat(MC%atpa,MC%Nb_inc,MC%Nb_inc,'Matrice_AtPA.txt')
	
	! Calcul 
	if (mode==0) then ! classic calcul
		code = CALCUL_CLASSIC(MC,uvec,c,1)
	else if (mode==1) then ! calcul with sparse matrix
		code = CALCUL_SPARSE(MC,uvec,c,1)
	endif
	
	
    if (code == 110) then
        calcul = code 
        return
    end if
 
	
    IF (DEBUG) WRITE(0,*)'CALCUL : calculating VtPV'

    VTPV=0.0D0
    do k=1,MC%Nb_obsRel+MC%Nb_obsAbs
        Vtmp=0.0D0
        DO I=1,MC%Nb_inc
            Vtmp=Vtmp+MC%A(k,i)*uvec(i)
        ENDDO
        MC%V(k)=Vtmp-MC%B(k)
        VTPV=VTPV+MC%V(k)**2*MC%P(k)
    end do
    

    IF (DEBUG) WRITE(0,*)'CALCUL : calculating Sigma0'
    IF (DBLE(MC%dof) .EQ. 0.0D0 ) THEN 
        deallocate (c)
        write(0,*)'Degree Of Freedom = 0, aborting'
        calcul = 111
        return
    ELSE
        sigma0_2 = VTPV / DBLE(MC%dof)
    END IF


    MC%sigma0=DSQRT(sigma0_2)


    ! conversion de C (colonne) en SX = (AtPA)^-1 carrée
    IF (DEBUG) WRITE(0,*)'CALCUL : converting SX (vector) to  (AtPA)^-1 (matrix)'
    do I=1,MC%Nb_inc
        IL=(I-1)*I/2
            do J=1,I
                IJ=IL+J
                MC%SX(I,J)=c(IJ)
                MC%SX(J,I)=MC%SX(I,J)
            end do
    end do
    
    
    !write(0,*)MC%sigma0
    IF (DEBUG) WRITE(0,*)'CALCUL : Calculating parameter variances'
    
    do i=1,MC%Nb_inc
        if (MC%SX(i,i)<0.0D0) write(0,*)'Negative variance : ',i
        MC%sig(i) = DSQRT(MC%SX(i,i) * MC%sigma0**2)
    end do

    if (param%writemat) THEN
        !call write_mat(MC%SX,MC%Nb_inc,MC%Nb_inc,'Matrice_invAtPA.txt')
        call write_oct_mat(MC%SX,MC%Nb_inc,MC%Nb_inc,'invAtPA','invAtPA.m')
    END IF


    deallocate (c)


    IF (DEBUG) WRITE(0,*)'CALCUL : END'
    return
    
end function CALCUL


integer function SIMUL(MC,mode)
    ! Minimum constraint or weighted constraint solution
    use write_matr
    use param_data
    use str_const
    use MC_data
    
    implicit none
    
	! Parameters
    type (Tmc), intent(inout):: MC
    integer mode

	! Local variables
    integer i,j,hh,k,ij,il
    integer code
    real*8 sigma0,vtpv,sigma0_2,vtmp
    real*8,allocatable,dimension(:)::c,uvec
    if (allocated(c)) deallocate(c)
    if (allocated(uvec)) deallocate(uvec)
    allocate (c((MC%Nb_inc+1)*MC%Nb_inc/2))
    allocate (uvec(MC%Nb_inc))
    
    
    
    code = 0
   
    if (param%writemat) THEN
        !call write_mat(MC%atpa,MC%Nb_inc,MC%Nb_inc,'Matrice_AtPA.txt')
        call write_oct_mat(MC%atpa,MC%Nb_inc,MC%Nb_inc,'AtPA','AtPA.m')
    ENDIF


	if (mode==0) then ! classic calcul
		code = calcul_classic(MC,uvec,c,2)
	else if (mode==1) then ! calcul with sparse matrix
		code = calcul_sparse(MC,uvec,c,2)
	endif

	if (code == 110) then
        simul = code 
        return
    end if

    MC%X = uvec

    vtpv=0d0
    do k=1,MC%Nb_obsRel+MC%Nb_obsAbs
        Vtmp=0.0D0
        DO I=1,MC%Nb_inc
            Vtmp=Vtmp+MC%A(k,i)*uvec(i)
        ENDDO
        MC%V(k)=Vtmp-MC%B(k)
        vtpv=vtpv+MC%V(k)**2*MC%P(k)
    end do
    sigma0_2=VTPV / DBLE(MC%dof)
    MC%sigma0=DSQRT(sigma0_2)

    ! conversion de C (colonne) en SX = (AtPA)^-1 carrée

    DO I=1,MC%Nb_inc
        IL=(I-1)*I/2
            DO J=1,I
                IJ=IL+J
                MC%SX(I,J)=c(IJ)
                MC%SX(J,I)=MC%SX(I,J)
            ENDDO
    ENDDO
    
    ! Calcul de la variance sur les parametres ********************************    
    do i=1,MC%Nb_inc
        MC%sig(i) = MC%SX(i,i) * MC%sigma0**2
    end do
    
    IF (param%writemat) THEN
        !call write_mat(MC%SX,MC%Nb_inc,MC%Nb_inc,'Matrice_invAtPA.txt')
        call write_oct_mat(MC%SX,MC%Nb_inc,MC%Nb_inc,'invAtPA','invAtPA.txt')
    END IF
    
    
    
    deallocate (c)
    deallocate (MC%atpB,MC%atpa)
    simul = code
    return
    
end function SIMUL



integer function CALCUL_SPARSE(MC,uvec,c,mode)
	use MC_data
    use Sparse_cholesky
    use param_data
    use str_const
    
    ! Parameters
    type (Tmc), intent(inout):: MC
    integer mode
	real*8,dimension(:)::c,uvec
   
		
	! Local variables
	integer*4 	k,j,code,N
	real*8,allocatable,dimension(:,:)::mm
    if (allocated(mm)) deallocate(mm)
    allocate (mm(MC%Nb_inc,MC%Nb_inc))
    
	!IF (DEBUG) WRITE(0,*)'CALCUL : filling tmp arrays'
	
    N = MC%Nb_inc
    do k=1,N
        do j=1,N
            mm(k,j)=MC%AtPA(k,j)
        end do
    end do
    uvec=MC%AtPB
    
    if (param%lg=='F') then 
        write(0,*)InversionF
    else 
        write(0,*)InversionA 
    end if


	!IF (DEBUG) WRITE(0,*)'CALCUL : cholesky on sparse matix '
	calcul_sparse = CHOLESKY(mm,N,uvec,mode,c)
 
    MC%X = uvec
	
	deallocate(mm)
	
end function CALCUL_SPARSE

integer function CALCUL_CLASSIC(MC,uvec,c,mode)
	use MC_data
	use param_data
	use str_const
    
    ! Parameters
    type (Tmc), intent(inout):: MC
    integer mode
	real*8,dimension(:)::c,uvec
   
	! Local variables
	integer*4 	k,i,j
	real*8,allocatable,dimension(:)::nn
	if (allocated(nn)) deallocate(nn)
    allocate (nn((MC%Nb_inc+1)*MC%Nb_inc/2))
	
	
	
	!IF (DEBUG) WRITE(0,*)'CALCUL : filling tmp arrays'
    i=0
    do k=1,MC%Nb_inc
        do j=1,k
            i=i+1
            nn(i)=MC%AtPA(k,j)
        end do
    end do
    uvec=MC%AtPB
        
    if (param%lg=='F') then ; write(0,*)InversionF ; else ; write(0,*)InversionA ; end if

    calcul_classic = solution(nn,uvec,MC%Nb_inc,mode,c)
         
    MC%X = uvec
	
	deallocate(nn)

end function CALCUL_CLASSIC




integer function SOLUTION(A,X,N,mode,C)
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
      real*8  A(*),SUM !,work(N)
      real*8, intent(out):: C(N*(N+1)/2)
      real*8, intent(inout):: X(N)
      integer order,nsing,mode,K,I,J,KK,II,IK,II1
      ! CHOLESKY DECOMPOSITION of A
      
      real*8,allocatable,dimension(:):: work
      if (allocated(work)) deallocate(work)
      allocate(work(N))
      
      nsing=0
      if(mode.lt.0 .or. mode.gt.3) stop'incorrect mode'
      
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
            
            solution = 110
            return
        END IF
1     A(IK)=DSQRT(A(IK))

      if(mode.eq.0 .or.(mode.eq.1)) then
        call LTRISOL(A,X,N)
        call UTRISOL(A,X,N)
      end if
      
!c Invert A
      if((mode.eq.1) .or. (mode.eq.2)) then
        do order=1,n  
            do i=1,n
                work(i)=0.0D0
            end do
            work(order)=1.0D0
            call LTRISOL(A,work,N)
            Call UTRISOL(A,work,N)
            do i=order,n
                ii=i*(i-1)/2
                c(ii+order)=work(i)
            end do
        end do
        do i=1,n*(n+1)/2 
        a(i)=c(i)
        end do
      end if
!      write(0,*)'Nsing=',nsing
      if (allocated(work)) deallocate(work)

      RETURN

END function SOLUTION



SUBROUTINE LTRISOL(A,X,N)
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
            SUM=0.0D0
            DO 2 J=1,I-1
 2          SUM=SUM+A(II+J)*X(J)
1           X(I)=(X(I)-SUM)/A(II+I)
       RETURN
END SUBROUTINE LTRISOL

SUBROUTINE UTRISOL(A,X,N)
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
        SUM=0.0D0
        DO I=J+1,N
            II=I*(I-1)/2
            SUM=SUM+A(II+J)*X(I)
        END DO
        nsta=J*(J+1)/2
        X(J)=(X(J)-SUM)/A(nsta)
      END DO
      RETURN
END SUBROUTINE UTRISOL



end module MC_inversion

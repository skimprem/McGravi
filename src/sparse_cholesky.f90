Module Sparse_cholesky
! module de calcul de l'inversion avec Cholesky adapté aux matrices creuses :
! utilisation de la bibliothèque BLKFCLT

implicit none

contains

integer function CHOLESKY(M,neqns,b,mode,c)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C FUNCTION TO SOLVE A SYSTEM WITH CHOLESKY USING SPARSE MATRIX
!C
!C    	M    %%%%% in: psd matrix
!C		N    %%%%% in : order of M
!C		b    %%%%% in : observation vector, out : solution of the system
!C		c	 %%%%% out : inverse of M in a vector
!C      mode %%%%% in : 0, solve for X only 							
!C            		    1, invert A (the inverse is C) and solve for X  
!C           		    2, invert A only% X is not used in this mode	
!C
!C              PREVOST PAOLINE
!C 	            Jan, 2015
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	
	! Parameters
	integer*4 max,nmax
	parameter     (nmax=100000,max=6000000)
    integer*4 mode,neqns
    real*8,dimension(max)::b,c
    real*8,dimension(:,:)::M
    
	! Local variables
	integer*4 i,ij,k,order,nnza,iwsiz,nofsub,iflag,&
			nnzl,nsub,nsuper,tmpsiz,cachsz,nsup
	real*8,dimension(max)::a,d,e,af,lnz,sol,diag
	real*8,dimension(nmax)::tmpvec
	integer*4,dimension(max)::adjncy,adjf,temp,offset
	integer*4,dimension(nmax)::xadj,xadjf,perm,invp,xlindx,lindx,xlnz,snode,split,colcnt
	integer*4,dimension(nmax+1)::xsuper
	integer*4,dimension(nmax*7)::iwork
	
    
    external mmpym,smxpym
    
    if(mode.lt.0 .or. mode.gt.3) stop 'incorrect mode'
    
	! reorganization as sparse matrix
	call reorganize_matrix(M,a,adjncy,xadj,neqns,nnza)
	
	! Minimum degree reordering
	! Save the adjncy vector before the minimum degree reordering
	do i=1,nnza
		temp(i)=adjncy(i)
    end do
	
	iwsiz = 4*neqns
	
	call ordmmd(neqns,xadj,temp,invp,perm,iwsiz,iwork,nofsub,iflag)


	! Determination of the supernodes
	iwsiz = 7*neqns + 3
	call sfinit(neqns,nnza,xadj,adjncy,perm,invp,colcnt,nnzl,nsub,&
				nsuper,snode,xsuper,iwsiz,iwork,iflag)


	! Supernodal symbolic factorization
	call symfct(neqns,nnza,xadj,adjncy,perm,invp,colcnt,nsuper,xsuper,&
				snode,nsub,xlindx,lindx,xlnz,iwsiz,iwork,iflag)
			
				
	! Initialization for block factorization
	cachsz=16   ! cache size in Kb
    call bfinit(neqns,nsuper,xsuper,snode,xlindx,lindx,cachsz,tmpsiz,split)
    
    
	!Insert the numerical values in the data structures
    do i=1,neqns
		xadjf(i)=xadj(i)+i-1
        adjf(xadjf(i))=i
		 do ij=xadj(i),xadj(i+1)-1
			adjf(ij+i)=adjncy(ij)
         enddo
    enddo
    xadjf(neqns+1)=xadj(neqns+1)+neqns
   
    do i=1,neqns
		diag(i)=M(i,i)
    enddo
    
    do i=1,neqns
		af(xadjf(i))=diag(i)
		do ij=xadj(i),xadj(i+1)-1
			af(ij+i)=a(ij)
		enddo
	enddo
	
	call inpnv(neqns,xadjf,adjf,af,perm,invp,nsuper,xsuper,xlindx,&
			lindx,xlnz,lnz,offset)
	
	
	! Block factorization 	
	nsup = neqns ! number of rows of the positive definite part
	call blkfct(neqns,nsuper,xsuper,snode,split,xlindx,lindx,xlnz,lnz,&
			iwsiz,iwork,tmpsiz,tmpvec,iflag,mmpym,smxpym,perm,nsup)
	
	
	! System solution
	do i=1,neqns
		sol(i) = b(perm(i))
	enddo
	
	
    ! Calcul of sol
    if(mode.eq.0 .or.(mode.eq.1)) then
		call blkslv(nsuper,xsuper,xlindx,lindx,xlnz,lnz,sol,perm,nsup)
		do i=1,neqns
			b(perm(i))=sol(i)
		enddo
    end if
    
    
    ! Inverse a -> c
	if((mode.eq.1) .or. (mode.eq.2)) then
		do order=1,neqns
            do i=1,neqns
                sol(i)=0
            end do
            sol(order)=1
            call blkslv(nsuper,xsuper,xlindx,lindx,xlnz,lnz,sol,perm,nsup)
            
            do i=1,neqns
				e(perm(i))=sol(i)
			enddo
			
            do i=1,neqns
                d(i+(perm(order)-1)*neqns)=e(i)
            end do
        end do	
        
        k=1
        do i=1,neqns
			do ij=1,i
				c(k) = d((i-1)*neqns+ij)
				k=k+1
			end do
        end do	
         
	end if
	
end function CHOLESKY


subroutine REORGANIZE_MATRIX(M,a,adjncy,xadj,neqns,nnza)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C FUNCTION TO STORE A MATRIX IN THE COMPRESSED COLUMN STORAGE
!C
!C    	M  %%%%% in: psd matrix
!C		neqns %% in: order of M = number of equations
!C		a  %%%%% out: nonzero entries of M, without the diagonal elements
!C		adjncy % out: the row indices of a
!C		xadj %%% out: the column pointers of a
!C		nnza %%% out: number of nonzero entries
!C
!C              PREVOST PAOLINE
!C 	            Jan, 2015
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	! Parameters
	integer*4       nmax,max
    parameter     (nmax=100000,max=6000000)
	integer*4      neqns,nnza   ! Number of equations and number of nonzero entries of af
    real*8,dimension(neqns,neqns)::M
	integer*4      adjncy(max), xadj(nmax) ! row indices and column pointers of the lower triangular part of the matrix
	real*8         a(max)                  !nonzero entries of the lower triangular part of the matrix af

	! Local variables
	integer i,j,row

	row = 1	
	DO 1 j=1,neqns ! col
		xadj(j) = row
		DO 2 i=1,neqns ! row
			IF (i/=j) THEN
				IF (M(i,j)/=0) THEN
					a(row) = M(i,j)
					adjncy(row) = i
					row = row+1
				ENDIF
			ENDIF
		2	continue
	1	continue
	
	a(row) = 0
	xadj(neqns+1) = row

	nnza = row - 1
	
end subroutine REORGANIZE_MATRIX

end module sparse_cholesky

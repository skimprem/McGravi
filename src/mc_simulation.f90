module MC_simulation


implicit none

contains

subroutine create_obs_rel
end subroutine create_obs_rel

REAL*8 FUNCTION BruitG(iter,sd)
! Generation d'un bruit gaussien  
! entrees : 
!     iter, nombre d'iterations
!     sd,  ecart-type                        
! Sortie  : variable aleatoire gaussienne centree                             
    IMPLICIT none
    INTEGER iter,k
    REAL*8 sd,r,theta,z1,z2,x,y,RAND,pi


    k=0
    pi=4.0D0*ATAN(1.0D0 )
    DO WHILE(k.LE.iter)

        k=k+1
        z1=DBLE(RAND(0))
        z2=DBLE(RAND(0))
        theta=2.0D0*pi*z2
        r=SQRT(-2.0D0*(sd**2)*DLOG(1.0D0-z1))
        x=r*COS(theta)
        y=r*SIN(theta)
        
    ENDDO

    BruitG=y

    RETURN
END FUNCTION BruitG



end module MC_simulation

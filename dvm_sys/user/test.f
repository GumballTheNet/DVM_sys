	program test
	integer:: A(100000)
	integer*8::S = 0

!DVM$ DISTRIBUTE ( BLOCK) :: A 
!DVM$ PARALLEL (I) ON A(I)
	do I = 1, 100000
	     A(I) = I	
	end do

!DVM$ PARALLEL (I) ON A(I) , REDUCTION (SUM(S))
	do I = 1, 100000
	    S = S + A(I)
	end do

	print *,S
	end

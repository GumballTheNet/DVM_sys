
        subroutine    JAC
	PARAMETER    (L=8,  ITMAX=20)
        REAL     A(L,L)
	INTEGER, DIMENSION(:), ALLOCATABLE :: ss
        if (l > 0) THEN
		 RETURN
	 END IF
	ALLOCATE( ss(100) )
	Select case (L)
		case (0)
		    L = L + 1
		case (1)
			return
		case(2)
			RETURN
		case default
			L = L + 1
	end Select
	DEALLOCATE(ss)
	END


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine domain

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------


      nx = nx0
      ny = ny0
      nz = nz0

c---------------------------------------------------------------------
c   check the sub-domain size
c---------------------------------------------------------------------
      if ( ( nx .lt. 4 ) .or.
     >     ( ny .lt. 4 ) .or.
     >     ( nz .lt. 4 ) ) then
         write (*,2001) nx, ny, nz
 2001    format (5x,'SUBDOMAIN SIZE IS TOO SMALL - ',
     >        /5x,'ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS',
     >        /5x,'SO THAT NX, NY AND NZ ARE GREATER THAN OR EQUAL',
     >        /5x,'TO 4 THEY ARE CURRENTLY', 3I3)
         stop
      end if

      if ( ( nx .gt. isiz1 ) .or.
     >     ( ny .gt. isiz2 ) .or.
     >     ( nz .gt. isiz3 ) ) then
         write (*,2002) nx, ny, nz
 2002    format (5x,'SUBDOMAIN SIZE IS TOO LARGE - ',
     >        /5x,'ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS',
     >        /5x,'SO THAT NX, NY AND NZ ARE LESS THAN OR EQUAL TO ',
     >        /5x,'ISIZ1, ISIZ2 AND ISIZ3 RESPECTIVELY.  THEY ARE',
     >        /5x,'CURRENTLY', 3I4)
         stop
      end if

c---------------------------------------------------------------------
c   set up the start and end in i and j extents for all processors
c---------------------------------------------------------------------
      ist = 2
      iend = nx - 1

      jst = 2
      jend = ny - 1


      return
      end



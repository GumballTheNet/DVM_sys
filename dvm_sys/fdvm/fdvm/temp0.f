         subroutine bad_fun ()
         print *, 'MOYA OBORONA'
         end

         module bigarrays
         integer  this_stat6
         integer  this_stat5
         integer  this_stat4
         integer  this_stat3
         integer  this_stat2
         integer  this_stat1
         integer  this_stat0
         double complex  u0(:)
         double complex  pad1(:)
         double complex  u1(:)
         double complex  pad2(:)
         double complex  u2(:)
         double complex  pad3(:)
         integer  indexmap(:)
         subroutine func0 ()
         allocate(u0(ntotal), this_stat0)
         if (this_stat0 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func1 ()
         deallocate(u0, this_stat0)
         if (this_stat0 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func2 ()
         allocate(pad1(3), this_stat1)
         if (this_stat1 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func3 ()
         deallocate(pad1, this_stat1)
         if (this_stat1 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func4 ()
         allocate(u1(ntotal), this_stat2)
         if (this_stat2 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func5 ()
         deallocate(u1, this_stat2)
         if (this_stat2 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func6 ()
         allocate(pad2(3), this_stat3)
         if (this_stat3 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func7 ()
         deallocate(pad2, this_stat3)
         if (this_stat3 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func8 ()
         allocate(u2(ntotal), this_stat4)
         if (this_stat4 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func9 ()
         deallocate(u2, this_stat4)
         if (this_stat4 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func10 ()
         allocate(pad3(3), this_stat5)
         if (this_stat5 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func11 ()
         deallocate(pad3, this_stat5)
         if (this_stat5 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func12 ()
         allocate(indexmap(ntotal), this_stat6)
         if (this_stat6 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func13 ()
         deallocate(indexmap, this_stat6)
         if (this_stat6 .ne. 0)  call bad_fun()
         end subroutine  
         end module 

         module iter
         integer  niter
         end module 

         module sumcomm
         integer  this_stat7
         double complex  sums(:)
         subroutine func14 ()
         allocate(sums(0:niter_default), this_stat7)
         if (this_stat7 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func15 ()
         deallocate(sums, this_stat7)
         if (this_stat7 .ne. 0)  call bad_fun()
         end subroutine  
         end module 

         module ucomm
         integer  this_stat8
         double complex  u(:)
         subroutine func16 ()
         allocate(u(nx), this_stat8)
         if (this_stat8 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func17 ()
         deallocate(u, this_stat8)
         if (this_stat8 .ne. 0)  call bad_fun()
         end subroutine  
         end module 

         module excomm
         integer  this_stat9
         double precision  ex(:)
         subroutine func18 ()
         allocate(ex(0:expmax), this_stat9)
         if (this_stat9 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func19 ()
         deallocate(ex, this_stat9)
         if (this_stat9 .ne. 0)  call bad_fun()
         end subroutine  
         end module 

         module dbg
         logical  debugsynch
         logical  debug
         end module 

         module layout
         integer  this_stat16
         integer  this_stat15
         integer  this_stat14
         integer  this_stat13
         integer  this_stat12
         integer  this_stat11
         integer  this_stat10
         integer  dims(:,:)
         integer  xstart(:)
         integer  ystart(:)
         integer  zstart(:)
         integer  xend(:)
         integer  yend(:)
         integer  zend(:)
         subroutine func20 ()
         allocate(dims(3,3), this_stat10)
         if (this_stat10 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func21 ()
         deallocate(dims, this_stat10)
         if (this_stat10 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func22 ()
         allocate(xstart(3), this_stat11)
         if (this_stat11 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func23 ()
         deallocate(xstart, this_stat11)
         if (this_stat11 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func24 ()
         allocate(ystart(3), this_stat12)
         if (this_stat12 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func25 ()
         deallocate(ystart, this_stat12)
         if (this_stat12 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func26 ()
         allocate(zstart(3), this_stat13)
         if (this_stat13 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func27 ()
         deallocate(zstart, this_stat13)
         if (this_stat13 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func28 ()
         allocate(xend(3), this_stat14)
         if (this_stat14 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func29 ()
         deallocate(xend, this_stat14)
         if (this_stat14 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func30 ()
         allocate(yend(3), this_stat15)
         if (this_stat15 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func31 ()
         deallocate(yend, this_stat15)
         if (this_stat15 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func32 ()
         allocate(zend(3), this_stat16)
         if (this_stat16 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func33 ()
         deallocate(zend, this_stat16)
         if (this_stat16 .ne. 0)  call bad_fun()
         end subroutine  
         end module 

         module blockinfo
         integer  fftblockpad
         integer  fftblock
         end module 


!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  2.3         !
!                                                                         !
!                     S E R I A L     V E R S I O N S                     !
!                                                                         !
!                                   F T                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is a serial version of the NPB FT code.               !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 2.3. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 2.3, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/NAS/NPB/                              !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!    Send bug reports to              npb-bugs@nas.nasa.gov               !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (415) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!
!---------------------------------------------------------------------
!     
! Authors: D. Bailey
!          W. Saphir
!     
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! FT benchmark
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
         program ft

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  i,ierr
         integer  iter
         double precision  total_time,mflops
         logical  verified
         character  class
         use blockinfo
         use layout
         call func32()
         call func30()
         call func28()
         call func26()
         call func24()
         call func22()
         call func20()
         use dbg
         use excomm
         call func18()
         use ucomm
         call func16()
         use sumcomm
         call func14()
         use iter
         use bigarrays
         call func12()
         call func10()
         call func8()
         call func6()
         call func4()
         call func2()
         call func0()

!---------------------------------------------------------------------
! Run the entire problem once to make sure all data is touched. 
! This reduces variable startup costs, which is important for such a 
! short benchmark. The other NPB 2 implementations are similar. 
!---------------------------------------------------------------------
         do  i = 1,t_max
            call timer_clear(i)
         enddo  
         call setup()
         call compute_indexmap(indexmap,dims(1,3))
         call compute_initial_conditions(u1,dims(1,1))
         call fft_init(dims(1,1))
         call fft(1,u1,u0)

!---------------------------------------------------------------------
! Start over from the beginning. Note that all operations must
! be timed, in contrast to other benchmarks. 
!---------------------------------------------------------------------
         do  i = 1,t_max
            call timer_clear(i)
         enddo  
         call timer_start(t_total)
         if (timers_enabled)  call timer_start(t_setup)
         call compute_indexmap(indexmap,dims(1,3))
         call compute_initial_conditions(u1,dims(1,1))
         call fft_init(dims(1,1))
         if (timers_enabled)  call timer_stop(t_setup)
         if (timers_enabled)  call timer_start(t_fft)
         call fft(1,u1,u0)
         if (timers_enabled)  call timer_stop(t_fft)
         do  iter = 1,niter
            if (timers_enabled)  call timer_start(t_evolve)
            call evolve(u0,u1,iter,indexmap,dims(1,1))
            if (timers_enabled)  call timer_stop(t_evolve)
            if (timers_enabled)  call timer_start(t_fft)
            call fft((-(1)),u1,u2)
            if (timers_enabled)  call timer_stop(t_fft)
            if (timers_enabled)  call timer_start(t_checksum)
            call checksum(iter,u2,dims(1,1))
            if (timers_enabled)  call timer_stop(t_checksum)
         enddo  
         call verify(nx,ny,nz,niter,verified,class)
         call timer_stop(t_total)
         total_time = timer_read (t_total)
         if (total_time .ne. 0.) then
            mflops = 1.0d-6 * float (ntotal) * (14.8157 + 7.19641 * log 
     &(float (ntotal)) + (5.23518 + 7.21113 * log (float (ntotal))) * ni
     &ter) / total_time
         else  
            mflops = 0.0
         endif  
         call print_results('FT',class,nx,ny,nz,niter,total_time,mflops,
     &'          floating point',verified,npbversion,compiletime,cs1,cs2
     &,cs3,cs4,cs5,cs6,cs7)
         if (timers_enabled)  call print_timers()
call func1()
call func3()
call func5()
call func7()
call func9()
call func11()
call func13()
call func15()
call func17()
call func19()
call func21()
call func23()
call func25()
call func27()
call func29()
call func31()
call func33()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine evolve (u0, u1, t, indexmap, d)
         integer  this_stat20
         integer  this_stat19
         integer  this_stat18
         integer  this_stat17

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! evolve u0 -> u1 (t time steps) in fourier space
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  d(:)
         double complex  u0(:,:,:)
         double complex  u1(:,:,:)
         integer  indexmap(:,:,:)
         integer  t
         integer  i,j,k
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(indexmap(d(1),d(2),d(3)), this_stat20)
         if (this_stat20 .ne. 0)  call bad_fun()
         allocate(u1(d(1),d(2),d(3)), this_stat19)
         if (this_stat19 .ne. 0)  call bad_fun()
         allocate(u0(d(1),d(2),d(3)), this_stat18)
         if (this_stat18 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat17)
         if (this_stat17 .ne. 0)  call bad_fun()
         do  k = 1,d(3)
            do  j = 1,d(2)
               do  i = 1,d(1)
                  u1(i,j,k) = u0(i,j,k) * ex(t * indexmap(i,j,k))
               enddo  
            enddo  
         enddo  
         deallocate(d, this_stat17)
         if (this_stat17 .ne. 0)  call bad_fun()
         deallocate(u0, this_stat18)
         if (this_stat18 .ne. 0)  call bad_fun()
         deallocate(u1, this_stat19)
         if (this_stat19 .ne. 0)  call bad_fun()
         deallocate(indexmap, this_stat20)
         if (this_stat20 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat17)
         if (this_stat17 .ne. 0)  call bad_fun()
         deallocate(u0, this_stat18)
         if (this_stat18 .ne. 0)  call bad_fun()
         deallocate(u1, this_stat19)
         if (this_stat19 .ne. 0)  call bad_fun()
         deallocate(indexmap, this_stat20)
         if (this_stat20 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine compute_initial_conditions (u0, d)
         integer  this_stat22
         integer  this_stat21

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! Fill in array u0 with initial conditions from 
! random number generator 
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  d(:)
         double complex  u0(:,:,:)
         integer  k
         double precision  x0,start,an,dummy
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(u0(d(1),d(2),d(3)), this_stat22)
         if (this_stat22 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat21)
         if (this_stat21 .ne. 0)  call bad_fun()
         start = seed

!---------------------------------------------------------------------
! Jump to the starting element for our first plane.
!---------------------------------------------------------------------
         call ipow46(a,(zstart(1) - 1) * 2 * nx * ny + (ystart(1) - 1) *
     & 2 * nx,an)
         dummy = randlc (start,an)
         call ipow46(a,2 * nx * ny,an)

!---------------------------------------------------------------------
! Go through by z planes filling in one square at a time.
!---------------------------------------------------------------------
         do  k = 1,dims(3,1)
            x0 = start
            call vranlc(2 * nx * dims(2,1),x0,a,u0(1,1,k))
            if (k .ne. dims(3,1))  dummy = randlc (start,an)
         enddo  
         deallocate(d, this_stat21)
         if (this_stat21 .ne. 0)  call bad_fun()
         deallocate(u0, this_stat22)
         if (this_stat22 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat21)
         if (this_stat21 .ne. 0)  call bad_fun()
         deallocate(u0, this_stat22)
         if (this_stat22 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine ipow46 (a, exponent, result)

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! compute a^exponent mod 2^46
!---------------------------------------------------------------------
         implicit none
         double precision  a,result,dummy,q,r
         integer  exponent,n,n2
         external randlc
         double precision  randlc

!---------------------------------------------------------------------
! Use 
!   a^n = a^(n/2)*a^(n/2) if n even else
!   a^n = a*a^(n-1)       if n odd
!---------------------------------------------------------------------
         result = 1
         if (exponent .eq. 0)  return 
         q = a
         r = 1
         n = exponent
         do while (n .gt. 1)
            n2 = n / 2
            if (n2 * 2 .eq. n) then
               dummy = randlc (q,q)
               n = n2
            else  
               dummy = randlc (r,q)
               n = n - 1
            endif  
         enddo  
         dummy = randlc (r,q)
         result = r
         return 
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine setup ()

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  ierr,i,j,fstatus
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         debug = .FALSE.
         write (unit = *,fmt = 1000) 
         niter = niter_default
         write (unit = *,fmt = 1001) nx,ny,nz
         write (unit = *,fmt = 1002) niter
1000     format(//,' NAS Parallel Benchmarks 2.3-serial version',       
     &   ' - FT Benchmark', /)
1001     format(' Size                : ', i3, 'x', i3, 'x', i3)
1002     format(' Iterations          :     ', i7)
1004     format(' Number of processes :     ', i7)
1005     format(' Processor array     :     ', i3, 'x', i3)
1006     format(' WARNING: compiled for ', i5, ' processes. ',       ' W
     &ill not verify. ')
         do  i = 1,3
            dims(1,i) = nx
            dims(2,i) = ny
            dims(3,i) = nz
         enddo  
         do  i = 1,3
            xstart(i) = 1
            xend(i) = nx
            ystart(i) = 1
            yend(i) = ny
            zstart(i) = 1
            zend(i) = nz
         enddo  

!---------------------------------------------------------------------
! Set up info for blocking of ffts and transposes.  This improves
! performance on cache-based systems. Blocking involves
! working on a chunk of the problem at a time, taking chunks
! along the first, second, or third dimension. 
!     
! - In cffts1 blocking is on 2nd dimension (with fft on 1st dim)
! - In cffts2/3 blocking is on 1st dimension (with fft on 2nd and 3rd dims)
! Since 1st dim is always in processor, we'll assume it's long enough 
! (default blocking factor is 16 so min size for 1st dim is 16)
! The only case we have to worry about is cffts1 in a 2d decomposition. 
! so the blocking factor should not be larger than the 2nd dimension. 
!---------------------------------------------------------------------
         fftblock = fftblock_default
         fftblockpad = fftblockpad_default
         if (fftblock .ne. fftblock_default)  fftblockpad = fftblock + 3
         return 
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine compute_indexmap (indexmap, d)
         integer  this_stat24
         integer  this_stat23

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! compute function from local (i,j,k) to ibar^2+jbar^2+kbar^2 
! for time evolution exponent. 
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  d(:)
         integer  indexmap(:,:,:)
         integer  i,j,k,ii,ii2,jj,ij2,kk
         double precision  ap
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(indexmap(d(1),d(2),d(3)), this_stat24)
         if (this_stat24 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat23)
         if (this_stat23 .ne. 0)  call bad_fun()

!---------------------------------------------------------------------
! basically we want to convert the fortran indices 
!   1 2 3 4 5 6 7 8 
! to  
!   0 1 2 3 -4 -3 -2 -1
! The following magic formula does the trick:
! mod(i-1+n/2, n) - n/2
!---------------------------------------------------------------------
         do  i = 1,dims(1,3)
            ii = mod (i + xstart(3) - 2 + nx / 2,nx) - nx / 2
            ii2 = ii * ii
            do  j = 1,dims(2,3)
               jj = mod (j + ystart(3) - 2 + ny / 2,ny) - ny / 2
               ij2 = jj * jj + ii2
               do  k = 1,dims(3,3)
                  kk = mod (k + zstart(3) - 2 + nz / 2,nz) - nz / 2
                  indexmap(i,j,k) = kk * kk + ij2
               enddo  
            enddo  
         enddo  

!---------------------------------------------------------------------
! compute array of exponentials for time evolution. 
!---------------------------------------------------------------------
         ap = (-(4.d0)) * alpha * pi * pi
         ex(0) = 1.0d0
         ex(1) = exp (ap)
         do  i = 2,expmax
            ex(i) = ex(i - 1) * ex(1)
         enddo  
         deallocate(d, this_stat23)
         if (this_stat23 .ne. 0)  call bad_fun()
         deallocate(indexmap, this_stat24)
         if (this_stat24 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat23)
         if (this_stat23 .ne. 0)  call bad_fun()
         deallocate(indexmap, this_stat24)
         if (this_stat24 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine print_timers ()
         integer  this_stat25

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none
         integer  i

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         character*25  tstrings(:)
         data tstrings / '          total ',                 '          
     &setup ',                 '            fft ',                 '    
     &     evolve ',                 '       checksum ',                
     & '         fftlow ',                 '        fftcopy ' /
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(tstrings(t_max), this_stat25)
         if (this_stat25 .ne. 0)  call bad_fun()
         do  i = 1,t_max
            if (timer_read (i) .ne. 0.0d0) then
               write (unit = *,fmt = 100) i,tstrings(i),timer_read (i)
            endif  
         enddo  
100      format(' timer ', i2, '(', A16,  ') :', F10.6)
         deallocate(tstrings, this_stat25)
         if (this_stat25 .ne. 0)  call bad_fun()
         return 
         deallocate(tstrings, this_stat25)
         if (this_stat25 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine fft (dir, x1, x2)
         integer  this_stat28
         integer  this_stat27
         integer  this_stat26

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  dir
         double complex  x1(:),x2(:)
         double complex  scratch(:)
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(scratch(fftblockpad_default * maxdim * 2), this_stat28
     &)
         if (this_stat28 .ne. 0)  call bad_fun()
         allocate(x2(ntotal), this_stat27)
         if (this_stat27 .ne. 0)  call bad_fun()
         allocate(x1(ntotal), this_stat26)
         if (this_stat26 .ne. 0)  call bad_fun()

!---------------------------------------------------------------------
! note: args x1, x2 must be different arrays
! note: args for cfftsx are (direction, layout, xin, xout, scratch)
!       xin/xout may be the same and it can be somewhat faster
!       if they are
!---------------------------------------------------------------------
         if (dir .eq. 1) then
            call cffts1(1,dims(1,1),x1,x1,scratch)
            call cffts2(1,dims(1,2),x1,x1,scratch)
            call cffts3(1,dims(1,3),x1,x2,scratch)
         else  
            call cffts3((-(1)),dims(1,3),x1,x1,scratch)
            call cffts2((-(1)),dims(1,2),x1,x1,scratch)
            call cffts1((-(1)),dims(1,1),x1,x2,scratch)
         endif  
         deallocate(x1, this_stat26)
         if (this_stat26 .ne. 0)  call bad_fun()
         deallocate(x2, this_stat27)
         if (this_stat27 .ne. 0)  call bad_fun()
         deallocate(scratch, this_stat28)
         if (this_stat28 .ne. 0)  call bad_fun()
         return 
         deallocate(x1, this_stat26)
         if (this_stat26 .ne. 0)  call bad_fun()
         deallocate(x2, this_stat27)
         if (this_stat27 .ne. 0)  call bad_fun()
         deallocate(scratch, this_stat28)
         if (this_stat28 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine cffts1 (is, d, x, xout, y)
         integer  this_stat33
         integer  this_stat32
         integer  this_stat31
         integer  this_stat30
         integer  this_stat29

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  is,d(:),logd(:)
         double complex  x(:,:,:)
         double complex  xout(:,:,:)
         double complex  y(:,:,:)
         integer  i,j,k,jj
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(y(fftblockpad,d(1),2), this_stat33)
         if (this_stat33 .ne. 0)  call bad_fun()
         allocate(xout(d(1),d(2),d(3)), this_stat32)
         if (this_stat32 .ne. 0)  call bad_fun()
         allocate(x(d(1),d(2),d(3)), this_stat31)
         if (this_stat31 .ne. 0)  call bad_fun()
         allocate(logd(3), this_stat30)
         if (this_stat30 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat29)
         if (this_stat29 .ne. 0)  call bad_fun()
         do  i = 1,3
            logd(i) = ilog2 (d(i))
         enddo  
         do  k = 1,d(3)
            do  jj = 0,d(2) - fftblock,fftblock
               if (timers_enabled)  call timer_start(t_fftcopy)
               do  j = 1,fftblock
                  do  i = 1,d(1)
                     y(j,i,1) = x(i,j + jj,k)
                  enddo  
               enddo  
               if (timers_enabled)  call timer_stop(t_fftcopy)
               if (timers_enabled)  call timer_start(t_fftlow)
               call cfftz(is,logd(1),d(1),y,y(1,1,2))
               if (timers_enabled)  call timer_stop(t_fftlow)
               if (timers_enabled)  call timer_start(t_fftcopy)
               do  j = 1,fftblock
                  do  i = 1,d(1)
                     xout(i,j + jj,k) = y(j,i,1)
                  enddo  
               enddo  
               if (timers_enabled)  call timer_stop(t_fftcopy)
            enddo  
         enddo  
         deallocate(d, this_stat29)
         if (this_stat29 .ne. 0)  call bad_fun()
         deallocate(logd, this_stat30)
         if (this_stat30 .ne. 0)  call bad_fun()
         deallocate(x, this_stat31)
         if (this_stat31 .ne. 0)  call bad_fun()
         deallocate(xout, this_stat32)
         if (this_stat32 .ne. 0)  call bad_fun()
         deallocate(y, this_stat33)
         if (this_stat33 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat29)
         if (this_stat29 .ne. 0)  call bad_fun()
         deallocate(logd, this_stat30)
         if (this_stat30 .ne. 0)  call bad_fun()
         deallocate(x, this_stat31)
         if (this_stat31 .ne. 0)  call bad_fun()
         deallocate(xout, this_stat32)
         if (this_stat32 .ne. 0)  call bad_fun()
         deallocate(y, this_stat33)
         if (this_stat33 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine cffts2 (is, d, x, xout, y)
         integer  this_stat38
         integer  this_stat37
         integer  this_stat36
         integer  this_stat35
         integer  this_stat34

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  is,d(:),logd(:)
         double complex  x(:,:,:)
         double complex  xout(:,:,:)
         double complex  y(:,:,:)
         integer  i,j,k,ii
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(y(fftblockpad,d(2),2), this_stat38)
         if (this_stat38 .ne. 0)  call bad_fun()
         allocate(xout(d(1),d(2),d(3)), this_stat37)
         if (this_stat37 .ne. 0)  call bad_fun()
         allocate(x(d(1),d(2),d(3)), this_stat36)
         if (this_stat36 .ne. 0)  call bad_fun()
         allocate(logd(3), this_stat35)
         if (this_stat35 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat34)
         if (this_stat34 .ne. 0)  call bad_fun()
         do  i = 1,3
            logd(i) = ilog2 (d(i))
         enddo  
         do  k = 1,d(3)
            do  ii = 0,d(1) - fftblock,fftblock
               if (timers_enabled)  call timer_start(t_fftcopy)
               do  j = 1,d(2)
                  do  i = 1,fftblock
                     y(i,j,1) = x(i + ii,j,k)
                  enddo  
               enddo  
               if (timers_enabled)  call timer_stop(t_fftcopy)
               if (timers_enabled)  call timer_start(t_fftlow)
               call cfftz(is,logd(2),d(2),y,y(1,1,2))
               if (timers_enabled)  call timer_stop(t_fftlow)
               if (timers_enabled)  call timer_start(t_fftcopy)
               do  j = 1,d(2)
                  do  i = 1,fftblock
                     xout(i + ii,j,k) = y(i,j,1)
                  enddo  
               enddo  
               if (timers_enabled)  call timer_stop(t_fftcopy)
            enddo  
         enddo  
         deallocate(d, this_stat34)
         if (this_stat34 .ne. 0)  call bad_fun()
         deallocate(logd, this_stat35)
         if (this_stat35 .ne. 0)  call bad_fun()
         deallocate(x, this_stat36)
         if (this_stat36 .ne. 0)  call bad_fun()
         deallocate(xout, this_stat37)
         if (this_stat37 .ne. 0)  call bad_fun()
         deallocate(y, this_stat38)
         if (this_stat38 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat34)
         if (this_stat34 .ne. 0)  call bad_fun()
         deallocate(logd, this_stat35)
         if (this_stat35 .ne. 0)  call bad_fun()
         deallocate(x, this_stat36)
         if (this_stat36 .ne. 0)  call bad_fun()
         deallocate(xout, this_stat37)
         if (this_stat37 .ne. 0)  call bad_fun()
         deallocate(y, this_stat38)
         if (this_stat38 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine cffts3 (is, d, x, xout, y)
         integer  this_stat43
         integer  this_stat42
         integer  this_stat41
         integer  this_stat40
         integer  this_stat39

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  is,d(:),logd(:)
         double complex  x(:,:,:)
         double complex  xout(:,:,:)
         double complex  y(:,:,:)
         integer  i,j,k,ii
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(y(fftblockpad,d(3),2), this_stat43)
         if (this_stat43 .ne. 0)  call bad_fun()
         allocate(xout(d(1),d(2),d(3)), this_stat42)
         if (this_stat42 .ne. 0)  call bad_fun()
         allocate(x(d(1),d(2),d(3)), this_stat41)
         if (this_stat41 .ne. 0)  call bad_fun()
         allocate(logd(3), this_stat40)
         if (this_stat40 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat39)
         if (this_stat39 .ne. 0)  call bad_fun()
         do  i = 1,3
            logd(i) = ilog2 (d(i))
         enddo  
         do  j = 1,d(2)
            do  ii = 0,d(1) - fftblock,fftblock
               if (timers_enabled)  call timer_start(t_fftcopy)
               do  k = 1,d(3)
                  do  i = 1,fftblock
                     y(i,k,1) = x(i + ii,j,k)
                  enddo  
               enddo  
               if (timers_enabled)  call timer_stop(t_fftcopy)
               if (timers_enabled)  call timer_start(t_fftlow)
               call cfftz(is,logd(3),d(3),y,y(1,1,2))
               if (timers_enabled)  call timer_stop(t_fftlow)
               if (timers_enabled)  call timer_start(t_fftcopy)
               do  k = 1,d(3)
                  do  i = 1,fftblock
                     xout(i + ii,j,k) = y(i,k,1)
                  enddo  
               enddo  
               if (timers_enabled)  call timer_stop(t_fftcopy)
            enddo  
         enddo  
         deallocate(d, this_stat39)
         if (this_stat39 .ne. 0)  call bad_fun()
         deallocate(logd, this_stat40)
         if (this_stat40 .ne. 0)  call bad_fun()
         deallocate(x, this_stat41)
         if (this_stat41 .ne. 0)  call bad_fun()
         deallocate(xout, this_stat42)
         if (this_stat42 .ne. 0)  call bad_fun()
         deallocate(y, this_stat43)
         if (this_stat43 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat39)
         if (this_stat39 .ne. 0)  call bad_fun()
         deallocate(logd, this_stat40)
         if (this_stat40 .ne. 0)  call bad_fun()
         deallocate(x, this_stat41)
         if (this_stat41 .ne. 0)  call bad_fun()
         deallocate(xout, this_stat42)
         if (this_stat42 .ne. 0)  call bad_fun()
         deallocate(y, this_stat43)
         if (this_stat43 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine fft_init (n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! compute the roots-of-unity array that will be used for subsequent FFTs. 
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  m,n,nu,ku,i,j,ln
         double precision  t,ti
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter

!---------------------------------------------------------------------
!   Initialize the U array with sines and cosines in a manner that permits
!   stride one access at each FFT iteration.
!---------------------------------------------------------------------
         nu = n
         m = ilog2 (n)
         u(1) = m
         ku = 2
         ln = 1
         do  j = 1,m
            t = pi / ln
            do  i = 0,ln - 1
               ti = i * t
               u(i + ku) = dcmplx (cos (ti),sin (ti))
            enddo  
            ku = ku + ln
            ln = 2 * ln
         enddo  
         return 
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine cfftz (is, m, n, x, y)

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   Computes NY N-point complex-to-complex FFTs of X using an algorithm due
!   to Swarztrauber.  X is both the input and the output array, while Y is a 
!   scratch array.  It is assumed that N = 2^M.  Before calling CFFTZ to 
!   perform FFTs, the array U must be initialized by calling CFFTZ with IS 
!   set to 0 and M set to MX, where MX is the maximum value of M for any 
!   subsequent call.
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  is,m,n,i,j,l,mx
         double complex  x,y
         dimension x(fftblockpad,n),y(fftblockpad,n)
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter

!---------------------------------------------------------------------
!   Check if input parameters are invalid.
!---------------------------------------------------------------------
         mx = u(1)
         if (is .ne. 1 .and. is .ne. (-(1)) .or. m .lt. 1 .or. m .gt. mx
     &) then
            write (unit = *,fmt = 1) is,m,mx
1             format ('CFFTZ: Either U has not been initialized, or else
     &'/        'one of the input parameters is invalid', 3I5)
            stop
         endif  

!---------------------------------------------------------------------
!   Perform one variant of the Stockham FFT.
!---------------------------------------------------------------------
         do  l = 1,m,2
            call fftz2(is,l,m,n,fftblock,fftblockpad,u,x,y)
            if (l .eq. m)  goto 160
            call fftz2(is,l + 1,m,n,fftblock,fftblockpad,u,y,x)
         enddo  
         goto 180

!---------------------------------------------------------------------
!   Copy Y to X.
!---------------------------------------------------------------------
160      do  j = 1,n
            do  i = 1,fftblock
               x(i,j) = y(i,j)
            enddo  
         enddo  
180      continue
         return 
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine fftz2 (is, l, m, n, ny, ny1, u, x, y)

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   Performs the L-th iteration of the second variant of the Stockham FFT.
!---------------------------------------------------------------------
         implicit none
         integer  is,k,l,m,n,ny,ny1,n1,li,lj,lk,ku,i,j,i11,i12,i21,i22
         double complex  u,x,y,u1,x11,x21
         dimension u(n),x(ny1,n),y(ny1,n)

!---------------------------------------------------------------------
!   Set initial parameters.
!---------------------------------------------------------------------
         n1 = n / 2
         lk = 2** (l - 1)
         li = 2** (m - l)
         lj = 2 * lk
         ku = li + 1
         do  i = 0,li - 1
            i11 = i * lk + 1
            i12 = i11 + n1
            i21 = i * lj + 1
            i22 = i21 + lk
            if (is .ge. 1) then
               u1 = u(ku + i)
            else  
               u1 = dconjg (u(ku + i))
            endif  

!---------------------------------------------------------------------
!   This loop is vectorizable.
!---------------------------------------------------------------------
            do  k = 0,lk - 1
               do  j = 1,ny
                  x11 = x(j,i11 + k)
                  x21 = x(j,i12 + k)
                  y(j,i21 + k) = x11 + x21
                  y(j,i22 + k) = u1 * (x11 - x21)
               enddo  
            enddo  
         enddo  
         return 
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
         integer  function ilog2 (n) 

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none
         integer  n,nn,lg
         if (n .eq. 1) then
            ilog2 = 0
            return 
         endif  
         lg = 1
         nn = 2
         do while (nn .lt. n)
            nn = nn * 2
            lg = lg + 1
         enddo  
         ilog2 = lg
         return 
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine checksum (i, u1, d)
         integer  this_stat45
         integer  this_stat44

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  i,d(:)
         double complex  u1(:,:,:)
         integer  j,q,r,s,ierr
         double complex  chk,allchk
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(u1(d(1),d(2),d(3)), this_stat45)
         if (this_stat45 .ne. 0)  call bad_fun()
         allocate(d(3), this_stat44)
         if (this_stat44 .ne. 0)  call bad_fun()
         chk = (0.0, 0.0)
         do  j = 1,1024
            q = mod (j,nx) + 1
            if (q .ge. xstart(1) .and. q .le. xend(1)) then
               r = mod (3 * j,ny) + 1
               if (r .ge. ystart(1) .and. r .le. yend(1)) then
                  s = mod (5 * j,nz) + 1
                  if (s .ge. zstart(1) .and. s .le. zend(1)) then
                     chk = chk + u1(q - xstart(1) + 1,r - ystart(1) + 1,
     &s - zstart(1) + 1)
                  endif  
               endif  
            endif  
         enddo  
         chk = chk / dble (ntotal)
         write (unit = *,fmt = 30) i,chk
30       format (' T =',I5,5X,'Checksum =',1P2D22.12)
         sums(i) = chk
         deallocate(d, this_stat44)
         if (this_stat44 .ne. 0)  call bad_fun()
         deallocate(u1, this_stat45)
         if (this_stat45 .ne. 0)  call bad_fun()
         return 
         deallocate(d, this_stat44)
         if (this_stat44 .ne. 0)  call bad_fun()
         deallocate(u1, this_stat45)
         if (this_stat45 .ne. 0)  call bad_fun()
         end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
         subroutine verify (d1, d2, d3, nt, verified, class)
         integer  this_stat55
         integer  this_stat54
         integer  this_stat53
         integer  this_stat52
         integer  this_stat51
         integer  this_stat50
         integer  this_stat49
         integer  this_stat48
         integer  this_stat47
         integer  this_stat46

!---------------------------------------------------------------------
!---------------------------------------------------------------------
         implicit none

! If processor array is 1x1 -> 0D grid decomposition
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! CLASS = B
!     
!     
!  This file is generated automatically by the setparams utility.
!  It sets the number of processors and the class of the NPB
!  in this directory. Do not modify it by hand.
!     
         integer  problem_size,niter_default
         parameter (problem_size = 102,niter_default = 400)
         double precision  dt_default
         parameter (dt_default = 0.001d0)
         logical  convertdouble
         parameter (convertdouble = .FALSE.)
         character  compiletime*11
         parameter (compiletime = '14 Dec 2011')
         character  npbversion*5
         parameter (npbversion = '3.3.1')
         character  cs1*5
         parameter (cs1 = 'pgf90')
         character  cs2*6
         parameter (cs2 = '$(F77)')
         character  cs3*6
         parameter (cs3 = '(none)')
         character  cs4*14
         parameter (cs4 = '-YI,${INCLUDE}')
         character  cs5*28
         parameter (cs5 = '-O2 -ta=nvidia -Minfo -Mcuda')
         character  cs6*46
         parameter (cs6 = '-O2 -ta=nvidia -Minfo -Mcuda -YL,${LD_LIBRA..
     &.')
         character  cs7*6
         parameter (cs7 = 'randi8')
         integer  fftblock_default,fftblockpad_default
         parameter (fftblock_default = 16,fftblockpad_default = 18)
         integer  t_total,t_setup,t_fft,t_evolve,t_checksum,t_fftlow,t_f
     &ftcopy,t_max
         parameter (t_total = 1,t_setup = 2,t_fft = 3,t_evolve = 4,t_che
     &cksum = 5,t_fftlow = 6,t_fftcopy = 7,t_max = 7)
         logical  timers_enabled
         parameter (timers_enabled = .FALSE.)
         external timer_read
         double precision  timer_read
         external ilog2
         integer  ilog2
         external randlc
         double precision  randlc
         double precision  seed,a,pi,alpha
         parameter (seed = 314159265.d0,a = 1220703125.d0,pi = 3.1415926
     &53589793238d0,alpha = 1.0d-6)
         integer  expmax
         parameter (expmax = niter_default * (nx * nx / 4 + ny * ny / 4 
     &+ nz * nz / 4))
         integer  d1,d2,d3,nt
         character  class
         logical  verified
         integer  ierr,size,i
         double precision  err,epsilon

!---------------------------------------------------------------------
!   Sample size reference checksums
!---------------------------------------------------------------------
         double precision  vdata_real_s(:)
         double precision  vdata_imag_s(:)
         double precision  vdata_real_w(:)
         double precision  vdata_imag_w(:)
         double precision  vdata_real_a(:)
         double precision  vdata_imag_a(:)
         double precision  vdata_real_b(:)
         double precision  vdata_imag_b(:)
         double precision  vdata_real_c(:)
         double precision  vdata_imag_c(:)
         data vdata_real_s / 5.546087004964D+02,                5.546385
     &409189D+02,                5.546148406171D+02,                5.54
     &5423607415D+02,                5.544255039624D+02,                
     &5.542683411902D+02 /
         data vdata_imag_s / 4.845363331978D+02,                4.865304
     &269511D+02,                4.883910722336D+02,                4.90
     &1273169046D+02,                4.917475857993D+02,                
     &4.932597244941D+02 /

!---------------------------------------------------------------------
!   Class W size reference checksums
!---------------------------------------------------------------------
          data vdata_real_w /                5.673612178944D+02,        
     &        5.631436885271D+02,                5.594024089970D+02,    
     &            5.560698047020D+02,                5.530898991250D+02,
     &                5.504159734538D+02/
          data vdata_imag_w /               5.293246849175D+02,         
     &      5.282149986629D+02,               5.270996558037D+02,       
     &         5.260027904925D+02,                5.249400845633D+02,   
     &            5.239212247086D+02/

!---------------------------------------------------------------------
!   Class A size reference checksums
!---------------------------------------------------------------------
         data vdata_real_a / 5.046735008193D+02,                5.059412
     &319734D+02,                5.069376896287D+02,                5.07
     &7892868474D+02,                5.085233095391D+02,                
     &5.091487099959D+02 /
         data vdata_imag_a / 5.114047905510D+02,                5.098809
     &666433D+02,                5.098144042213D+02,                5.10
     &1336130759D+02,                5.104914655194D+02,                
     &5.107917842803D+02 /

!---------------------------------------------------------------------
!   Class B size reference checksums
!---------------------------------------------------------------------
         data vdata_real_b / 5.177643571579D+02,                5.154521
     &291263D+02,                5.146409228649D+02,                5.14
     &2378756213D+02,                5.139626667737D+02,                
     &5.137423460082D+02,                5.135547056878D+02,            
     &    5.133910925466D+02,                5.132470705390D+02,        
     &        5.131197729984D+02,                5.130070319283D+02,    
     &            5.129070537032D+02,                5.128182883502D+02,
     &                5.127393733383D+02,                5.126691062020D
     &+02,                5.126064276004D+02,                5.125504076
     &570D+02,                5.125002331720D+02,                5.12455
     &1951846D+02,                5.124146770029D+02 /
         data vdata_imag_b / 5.077803458597D+02,                5.088249
     &431599D+02,                                  5.096208912659D+02,  
     &                                   5.101023387619D+02,            
     &                      5.103976610617D+02,                         
     &         5.105948019802D+02,                                  5.10
     &7404165783D+02,                                  5.108576573661D+0
     &2,                                  5.109577278523D+02,           
     &                       5.110460304483D+02,                        
     &          5.111252433800D+02,                                  5.1
     &11968077718D+02,                                  5.112616233064D+
     &02,                                  5.113203605551D+02,          
     &                        5.113735928093D+02,                       
     &           5.114218460548D+02,                5.114656139760D+02, 
     &               5.115053595966D+02,                5.115415130407D+
     &02,                5.115744692211D+02 /

!---------------------------------------------------------------------
!   Class C size reference checksums
!---------------------------------------------------------------------
         data vdata_real_c / 5.195078707457D+02,                5.155422
     &171134D+02,                5.144678022222D+02,                5.14
     &0150594328D+02,                5.137550426810D+02,                
     &5.135811056728D+02,                5.134569343165D+02,            
     &    5.133651975661D+02,                5.132955192805D+02,        
     &        5.132410471738D+02,                5.131971141679D+02,    
     &            5.131605205716D+02,                5.131290734194D+02,
     &                5.131012720314D+02,                5.130760908195D
     &+02,                5.130528295923D+02,                5.130310107
     &773D+02,                5.130103090133D+02,                5.12990
     &5029333D+02,                5.129714421109D+02 /
         data vdata_imag_c / 5.149019699238D+02,                5.127578
     &201997D+02,                5.122251847514D+02,                5.12
     &1090289018D+02,                5.121143685824D+02,                
     &5.121496764568D+02,                5.121870921893D+02,            
     &    5.122193250322D+02,                5.122454735794D+02,        
     &        5.122663649603D+02,                5.122830879827D+02,    
     &            5.122965869718D+02,                5.123075927445D+02,
     &                5.123166486553D+02,                5.123241541685D
     &+02,                5.123304037599D+02,                5.123356167
     &976D+02,                5.123399592211D+02,                5.12343
     &5588985D+02,                5.123465164008D+02 /
         use blockinfo, fftblock=>fftblock,fftblockpad=>fftblockpad
         use layout, dims=>dims,xstart=>xstart,ystart=>ystart,zstart=>zs
     &tart,xend=>xend,yend=>yend,zend=>zend
         use dbg, debug=>debug,debugsynch=>debugsynch
         use excomm, ex=>ex
         use ucomm, u=>u
         use sumcomm, sums=>sums
         use iter, niter=>niter
         allocate(vdata_imag_c(20), this_stat55)
         if (this_stat55 .ne. 0)  call bad_fun()
         allocate(vdata_real_c(20), this_stat54)
         if (this_stat54 .ne. 0)  call bad_fun()
         allocate(vdata_imag_b(20), this_stat53)
         if (this_stat53 .ne. 0)  call bad_fun()
         allocate(vdata_real_b(20), this_stat52)
         if (this_stat52 .ne. 0)  call bad_fun()
         allocate(vdata_imag_a(6), this_stat51)
         if (this_stat51 .ne. 0)  call bad_fun()
         allocate(vdata_real_a(6), this_stat50)
         if (this_stat50 .ne. 0)  call bad_fun()
         allocate(vdata_imag_w(6), this_stat49)
         if (this_stat49 .ne. 0)  call bad_fun()
         allocate(vdata_real_w(6), this_stat48)
         if (this_stat48 .ne. 0)  call bad_fun()
         allocate(vdata_imag_s(6), this_stat47)
         if (this_stat47 .ne. 0)  call bad_fun()
         allocate(vdata_real_s(6), this_stat46)
         if (this_stat46 .ne. 0)  call bad_fun()
         epsilon = 1.0d-12
         verified = .FALSE.
         class = 'U'
         if (d1 .eq. 64 .and. d2 .eq. 64 .and. d3 .eq. 64 .and. nt .eq. 
     &6) then
            class = 'S'
            do  i = 1,nt
               err = (dble (sums(i)) - vdata_real_s(i)) / vdata_real_s(i
     &)
               if (abs (err) .gt. epsilon)  goto 100
               err = (dimag (sums(i)) - vdata_imag_s(i)) / vdata_imag_s(
     &i)

! If you have a machine where the above does not compile, let 
! us know and use the following
!            err = (aimag(sums(i)) - vdata_imag_s(i)) / vdata_imag_s(i)
               if (abs (err) .gt. epsilon)  goto 100
            enddo  
            verified = .TRUE.
100         continue
         else if (d1 .eq. 128 .and. d2 .eq. 128 .and. d3 .eq. 32 .and. n
     &t .eq. 6) then  
            class = 'W'
            do  i = 1,nt
               err = (dble (sums(i)) - vdata_real_w(i)) / vdata_real_w(i
     &)
               if (abs (err) .gt. epsilon)  goto 105
               err = (dimag (sums(i)) - vdata_imag_w(i)) / vdata_imag_w(
     &i)

! If you have a machine where the above does not compile, let 
! us know and use the following
!            err = (aimag(sums(i)) - vdata_imag_w(i)) / vdata_imag_w(i)
               if (abs (err) .gt. epsilon)  goto 105
            enddo  
            verified = .TRUE.
105         continue
         else if (d1 .eq. 256 .and. d2 .eq. 256 .and. d3 .eq. 128 .and. 
     &nt .eq. 6) then  
            class = 'A'
            do  i = 1,nt
               err = (dble (sums(i)) - vdata_real_a(i)) / vdata_real_a(i
     &)
               if (abs (err) .gt. epsilon)  goto 110
               err = (dimag (sums(i)) - vdata_imag_a(i)) / vdata_imag_a(
     &i)

! If you have a machine where the above does not compile, let 
! us know and use the following
!            err = (aimag(sums(i)) - vdata_imag_a(i)) / vdata_imag_a(i)
               if (abs (err) .gt. epsilon)  goto 110
            enddo  
            verified = .TRUE.
110         continue
         else if (d1 .eq. 512 .and. d2 .eq. 256 .and. d3 .eq. 256 .and. 
     &nt .eq. 20) then  
            class = 'B'
            do  i = 1,nt
               err = (dble (sums(i)) - vdata_real_b(i)) / vdata_real_b(i
     &)
               if (abs (err) .gt. epsilon)  goto 120
               err = (dimag (sums(i)) - vdata_imag_b(i)) / vdata_imag_b(
     &i)

! If you have a machine where the above does not compile, let 
! us know and use the following
!            err = (aimag(sums(i)) - vdata_imag_b(i)) / vdata_imag_b(i)
               if (abs (err) .gt. epsilon)  goto 120
            enddo  
            verified = .TRUE.
120         continue
         else if (d1 .eq. 512 .and. d2 .eq. 512 .and. d3 .eq. 512 .and. 
     &nt .eq. 20) then  
            class = 'C'
            do  i = 1,nt
               err = (dble (sums(i)) - vdata_real_c(i)) / vdata_real_c(i
     &)
               if (abs (err) .gt. epsilon)  goto 130
               err = (dimag (sums(i)) - vdata_imag_c(i)) / vdata_imag_c(
     &i)

!            err = (aimag(sums(i)) - vdata_imag_c(i)) / vdata_imag_c(i)
               if (abs (err) .gt. epsilon)  goto 130
            enddo  
            verified = .TRUE.
130         continue
         endif  
         if (class .ne. 'U') then
            if (verified) then
               write (unit = *,fmt = 2000) 
2000                 format(' Result verification successful')
            else  
               write (unit = *,fmt = 2001) 
2001                 format(' Result verification failed')
            endif  
         endif  
         print *, 'class = ',class
         deallocate(vdata_real_s, this_stat46)
         if (this_stat46 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_s, this_stat47)
         if (this_stat47 .ne. 0)  call bad_fun()
         deallocate(vdata_real_w, this_stat48)
         if (this_stat48 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_w, this_stat49)
         if (this_stat49 .ne. 0)  call bad_fun()
         deallocate(vdata_real_a, this_stat50)
         if (this_stat50 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_a, this_stat51)
         if (this_stat51 .ne. 0)  call bad_fun()
         deallocate(vdata_real_b, this_stat52)
         if (this_stat52 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_b, this_stat53)
         if (this_stat53 .ne. 0)  call bad_fun()
         deallocate(vdata_real_c, this_stat54)
         if (this_stat54 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_c, this_stat55)
         if (this_stat55 .ne. 0)  call bad_fun()
         return 
         deallocate(vdata_real_s, this_stat46)
         if (this_stat46 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_s, this_stat47)
         if (this_stat47 .ne. 0)  call bad_fun()
         deallocate(vdata_real_w, this_stat48)
         if (this_stat48 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_w, this_stat49)
         if (this_stat49 .ne. 0)  call bad_fun()
         deallocate(vdata_real_a, this_stat50)
         if (this_stat50 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_a, this_stat51)
         if (this_stat51 .ne. 0)  call bad_fun()
         deallocate(vdata_real_b, this_stat52)
         if (this_stat52 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_b, this_stat53)
         if (this_stat53 .ne. 0)  call bad_fun()
         deallocate(vdata_real_c, this_stat54)
         if (this_stat54 .ne. 0)  call bad_fun()
         deallocate(vdata_imag_c, this_stat55)
         if (this_stat55 .ne. 0)  call bad_fun()
         end


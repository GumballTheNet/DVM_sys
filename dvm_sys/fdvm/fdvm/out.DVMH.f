      function adder (a, b, c, d) 
      real  a(10,10)
      real  b,c,d
      real  adder
      real  q,w,e(8,8),t(8,8)
      character  r
      integer ,dimension((-(100)):100):: y
      common /cm/q,w,e,r,t,y
      return 3
      adder = a + b + c + d
      end

      module ooo
      character  se
      integer  ut
      real  r(ut,ut)
      end module 

      function tester (a) 
      real ,intent(in),optional:: a
      real  tester
      end

      module ttt
      logical  gg
      integer  h
      end module 

      program jach
      parameter (l = 8,itmax = 20)
      real ::  eps=1,maxeps,uuu
      integer ,dimension(:),allocatable:: ss
      integer ,dimension((-(100)):100):: z
      character  iii
      integer ,pointer:: p1
      real ,pointer,dimension(itmax,itmax):: p2
      common /cm/uuu,eps,b,iii,a,z

! DVMH declarations 
      integer*8 ,parameter:: HANDLER_TYPE_PARALLEL = 1,HANDLER_TYPE_MAST
     &ER = 2
      integer*8 ,parameter:: DEVICE_TYPE_HOST = 1,DEVICE_TYPE_CUDA = 2
      integer*8 ,parameter:: INTENT_INOUT = 3
      external loop_test2_49_cuda,loop_test2_49_host,loop_test2_65_cuda,
     &loop_test2_65_host,loop_test2_73_cuda,loop_test2_73_host
      external dvmh_line,dvmh_scope_end,dvmh_scope_start,dvmh_delete_obj
     &ect,dvmh_actual_variable,dvmh_get_actual_variable,dvmh_get_actual_
     &array,loop_insred,loop_perform,loop_register_handler,dvmh_shadow_r
     &enew,region_set_name_array,region_register_array,region_execute_on
     &_targets,region_end,dvmh_finish,dvmh_init2,ftcntr,endpl,mappl,wait
     &sh,strtsh,inssh,waitrd,strtrd,insred,clfdvm
      integer*8  dvmh_string,loop_create,region_create,arrcpy,tstio,geta
     &ch,getad,getaf,getal,getai,crtpl,crtshg,crtrg,crtrdf,align,crtda,d
     &istr,crtamv,lexit
      integer*8  dvm000(41)
      integer*8  a(38),b(38)
      character*8 ::  filenm001='test2.f'//char (0)
      integer*8  a0004,a0002,b0004,b0002
      integer*8  dvm0c9,dvm0c8,dvm0c7,dvm0c6,dvm0c5,dvm0c4,dvm0c3,dvm0c2
     &,dvm0c1,dvm0c0
      parameter (dvm0c9 = 9,dvm0c8 = 8,dvm0c7 = 7,dvm0c6 = 6,dvm0c5 = 5,
     &dvm0c4 = 4,dvm0c3 = 3,dvm0c2 = 2,dvm0c1 = 1,dvm0c0 = 0)
      integer  i000io(262144)
      real  r000io(262144)
      equivalence (r000io,i000io)
      character  ch000m(0:64)
      logical  l0000m(0:64)
      double precision  d0000m(0:64)
      real  r0000m(0:64)
      integer  i0000m(0:64)
      equivalence (l0000m,d0000m,r0000m,i0000m)
      common /mem000/i0000m
      integer  idvm00
      save 
      call dvmh_line(43_8,dvmh_string (filenm001))
      dvm000(1) = dvm0c6
      dvm000(2) = getai (dvm000(1))
      dvm000(3) = getai (i0000m(0))
      dvm000(4) = getal (l0000m(0))
      dvm000(5) = getaf (r0000m(0))
      dvm000(6) = getad (d0000m(0))
      dvm000(7) = getach (ch000m(0))
      dvm000(8) = getai (dvm000(2))
      dvm000(9) = getai (i0000m(1))
      dvm000(10) = getal (l0000m(1))
      dvm000(11) = getaf (r0000m(1))
      dvm000(12) = getad (d0000m(1))
      dvm000(13) = getach (ch000m(1))
      i0000m(0) = 8
      i0000m(1) = 4
      i0000m(2) = 4
      i0000m(3) = 4
      i0000m(4) = 8
      i0000m(5) = 1
      i0000m(10) = 2
      i0000m(11) = 1
      i0000m(12) = 1
      i0000m(13) = 3
      i0000m(14) = 4
      i0000m(15) = 5
      call ftcntr(6,dvm000(2),dvm000(8),i0000m(0),i0000m(10))
      dvm000(1) = 1

!$    dvm000(1) = dvm000(1) + 8 
      call dvmh_init2(dvm000(1))
      call dvmh_scope_start()
      dvm000(4) = 2
      dvm000(5) = 1
      dvm000(6) = 0
      dvm000(7) = 0
      call dvmh_line(39_8,dvmh_string (filenm001))
      dvm000(8) = 8
      dvm000(9) = 8
      dvm000(10) = crtamv (dvm0c0,dvm0c2,dvm000(8),dvm0c0)
      dvm000(11) = distr (dvm000(10),dvm0c0,dvm0c2,dvm000(4),dvm000(6))
      dvm000(12) = 1
      dvm000(13) = 1
      a(5) = 1
      a(6) = 1
      a(7) = 8
      dvm000(14) = crtda (a(1),dvm0c1,i0000m,dvm0c2,dvm0c4,dvm000(8),dvm
     &0c0,dvm0c0,dvm000(12),dvm000(12))
      dvm000(15) = 1
      dvm000(16) = 2
      dvm000(17) = 1
      dvm000(18) = 1
      dvm000(19) = 0
      dvm000(20) = 0
      dvm000(21) = align (a(1),dvm000(10),dvm000(15),dvm000(17),dvm000(1
     &9))
      dvm000(12) = 1
      dvm000(13) = 2
      dvm000(14) = 1
      dvm000(15) = 1
      dvm000(16) = 0
      dvm000(17) = 0
      call dvmh_line(40_8,dvmh_string (filenm001))
      dvm000(18) = 8
      dvm000(19) = 8
      dvm000(20) = 1
      dvm000(21) = 1
      b(5) = 1
      b(6) = 1
      b(7) = 8
      dvm000(22) = crtda (b(1),dvm0c1,i0000m,dvm0c2,dvm0c4,dvm000(18),dv
     &m0c0,dvm0c0,dvm000(20),dvm000(20))
      dvm000(23) = align (b(1),a(1),dvm000(12),dvm000(14),dvm000(16))
      call dvmh_line(43_8,dvmh_string (filenm001))

!		 arrays A and B  with block distribution 
      if (tstio () .ne. 0)  print *, '**********  TEST_JACOBI   ********
     &**'
      maxeps = 0.5e-7
      call dvmh_line(45_8,dvmh_string (filenm001))

! Start region (line 45)
      dvm000(11) = region_create (dvm0c0)
      call region_register_array(dvm000(11),INTENT_INOUT,b(1),3_8)
      call region_set_name_array(dvm000(11),b(1),'b')
      call region_register_array(dvm000(11),INTENT_INOUT,a(1),3_8)
      call region_set_name_array(dvm000(11),a(1),'a')
      call region_execute_on_targets(dvm000(11),ior (DEVICE_TYPE_HOST,DE
     &VICE_TYPE_CUDA))
      call dvmh_line(46_8,dvmh_string (filenm001))
      dvm000(12) = crtpl (dvm0c2)
      dvm000(19) = getai (j)
      dvm000(20) = getai (i)
      dvm000(21) = 1
      dvm000(22) = 1
      dvm000(23) = 1
      dvm000(24) = 1
      dvm000(25) = l
      dvm000(26) = l
      dvm000(27) = 1
      dvm000(28) = 1
      dvm000(29) = 1
      dvm000(30) = 2
      dvm000(31) = 1
      dvm000(32) = 1
      dvm000(33) = (-1)
      dvm000(34) = (-1)
      call mappl(dvm000(12),a(1),dvm000(29),dvm000(31),dvm000(33),dvm000
     &(19),dvm000(21),dvm000(23),dvm000(25),dvm000(27),dvm000(13),dvm000
     &(15),dvm000(17))

! Parallel loop (line 49)
      call dvmh_line(49_8,dvmh_string (filenm001))
      dvm000(35) = loop_create (dvm000(11),dvm000(12))
      call loop_register_handler(dvm000(35),DEVICE_TYPE_CUDA,dvm0c0,loop
     &_test2_49_cuda,dvm0c0,dvm0c3,b,a,l)
      dvm000(36) = 0

!$    dvm000(36) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
      call loop_register_handler(dvm000(35),DEVICE_TYPE_HOST,dvm000(36),
     &loop_test2_49_host,dvm0c0,dvm0c4,b,a,r0000m,l)

! Loop execution
      call loop_perform(dvm000(35))
      call dvmh_line(57_8,dvmh_string (filenm001))
      call endpl(dvm000(12))

! Region end (line 45)
      call dvmh_line(58_8,dvmh_string (filenm001))
      call region_end(dvm000(11))
      do 2 it = 1,itmax
         eps = 0.
         call dvmh_line(61_8,dvmh_string (filenm001))
         call dvmh_actual_variable(eps)
         call dvmh_line(62_8,dvmh_string (filenm001))

! Start region (line 62)
         dvm000(11) = region_create (dvm0c0)
         call region_register_array(dvm000(11),INTENT_INOUT,a(1),3_8)
         call region_set_name_array(dvm000(11),a(1),'a')
         call region_register_array(dvm000(11),INTENT_INOUT,b(1),3_8)
         call region_set_name_array(dvm000(11),b(1),'b')
         call region_execute_on_targets(dvm000(11),ior (DEVICE_TYPE_HOST
     &,DEVICE_TYPE_CUDA))
         call dvmh_line(63_8,dvmh_string (filenm001))
         dvm000(19) = crtrg (dvm0c1,dvm0c1)
         dvm000(36) = 1
         dvm000(37) = 0
         call dvmh_get_actual_variable(eps)
         dvm000(38) = crtrdf (dvm0c3,getaf (eps),dvm0c3,dvm000(36),dvm0c
     &0,dvm000(37),dvm0c1)
         dvm000(12) = crtpl (dvm0c2)
         dvm000(20) = getai (j)
         dvm000(21) = getai (i)
         dvm000(22) = 1
         dvm000(23) = 1
         dvm000(24) = 2
         dvm000(25) = 2
         dvm000(26) = l - 1
         dvm000(27) = l - 1
         dvm000(28) = 1
         dvm000(29) = 1
         dvm000(30) = 1
         dvm000(31) = 2
         dvm000(32) = 1
         dvm000(33) = 1
         dvm000(34) = (-1)
         dvm000(35) = (-1)
         call mappl(dvm000(12),a(1),dvm000(30),dvm000(32),dvm000(34),dvm
     &000(20),dvm000(22),dvm000(24),dvm000(26),dvm000(28),dvm000(13),dvm
     &000(15),dvm000(17))
         call insred(dvm000(19),dvm000(38),dvm000(12),dvm0c0)

! Parallel loop (line 65)
         call dvmh_line(65_8,dvmh_string (filenm001))
         dvm000(39) = loop_create (dvm000(11),dvm000(12))
         call loop_insred(dvm000(39),dvm000(38))
         call loop_register_handler(dvm000(39),DEVICE_TYPE_CUDA,dvm0c0,l
     &oop_test2_65_cuda,dvm0c0,dvm0c2,a,b)
         dvm000(40) = 0

!$    dvm000(40) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(39),DEVICE_TYPE_HOST,dvm000(4
     &0),loop_test2_65_host,dvm0c0,dvm0c3,a,b,r0000m)

! Loop execution
         call loop_perform(dvm000(39))
         call dvmh_line(69_8,dvmh_string (filenm001))
         call endpl(dvm000(12))
         dvm000(41) = getaf (eps)
         call strtrd(dvm000(19))
         call waitrd(dvm000(19))
         call dvmh_actual_variable(eps)
         call dvmh_delete_object(dvm000(19))
         call dvmh_line(70_8,dvmh_string (filenm001))
         dvm000(12) = crtpl (dvm0c2)
         dvm000(19) = crtshg (dvm0c0)
         dvm000(20) = (-1)
         dvm000(21) = (-1)
         call inssh(dvm000(19),a(1),dvm000(20),dvm000(20),dvm0c0)
         call dvmh_shadow_renew(dvm000(19))
         call strtsh(dvm000(19))
         dvm000(22) = getai (j)
         dvm000(23) = getai (i)
         dvm000(24) = 1
         dvm000(25) = 1
         dvm000(26) = 2
         dvm000(27) = 2
         dvm000(28) = l - 1
         dvm000(29) = l - 1
         dvm000(30) = 1
         dvm000(31) = 1
         dvm000(32) = 1
         dvm000(33) = 2
         dvm000(34) = 1
         dvm000(35) = 1
         dvm000(36) = (-1)
         dvm000(37) = (-1)
         call waitsh(dvm000(19))
         call mappl(dvm000(12),b(1),dvm000(32),dvm000(34),dvm000(36),dvm
     &000(22),dvm000(24),dvm000(26),dvm000(28),dvm000(30),dvm000(13),dvm
     &000(15),dvm000(17))

! Parallel loop (line 73)
         call dvmh_line(73_8,dvmh_string (filenm001))
         dvm000(38) = loop_create (dvm000(11),dvm000(12))
         call loop_register_handler(dvm000(38),DEVICE_TYPE_CUDA,dvm0c0,l
     &oop_test2_73_cuda,dvm0c0,dvm0c2,b,a)
         dvm000(39) = 0

!$    dvm000(39) = ior(HANDLER_TYPE_MASTER,HANDLER_TYPE_PARALLEL) 
         call loop_register_handler(dvm000(38),DEVICE_TYPE_HOST,dvm000(3
     &9),loop_test2_73_host,dvm0c0,dvm0c3,b,a,r0000m)

! Loop execution
         call loop_perform(dvm000(38))
         call dvmh_line(77_8,dvmh_string (filenm001))
         call endpl(dvm000(12))

! Region end (line 62)
         call dvmh_line(78_8,dvmh_string (filenm001))
         call region_end(dvm000(11))
         call dvmh_line(79_8,dvmh_string (filenm001))
         call dvmh_get_actual_variable(eps)
         call dvmh_line(80_8,dvmh_string (filenm001))
         if (tstio () .ne. 0)  print 200, it,eps
200                  FORMAT(' IT = ',I4, '   EPS = ', E14.7)
         if (eps .lt. maxeps)  goto 3
2        continue
3     continue
      call dvmh_line(85_8,dvmh_string (filenm001))
      call dvmh_get_actual_array(b(1))
      call dvmh_line(86_8,dvmh_string (filenm001))
      if (tstio () .ne. 0)  open (unit = 3,file = 'JAC.DAT',form = 'FORM
     &ATTED',status = 'UNKNOWN')
      call dvmh_line(87_8,dvmh_string (filenm001))
      dvm000(11) = (-1)
      dvm000(12) = (-1)
      dvm000(17) = arrcpy (b(1),dvm000(11),dvm000(13),dvm000(15),i000io(
     &1),dvm000(11),dvm000(13),dvm000(15),dvm0c2)
      if (tstio () .ne. 0)  write (unit = 3,fmt = *) (r000io(idvm00), id
     &vm00 = 1,64)
      call dvmh_line(88_8,dvmh_string (filenm001))
      if (tstio () .ne. 0)  close (unit = 3)
      call dvmh_line(89_8,dvmh_string (filenm001))
      call dvmh_scope_end()
      call dvmh_line(89_8,dvmh_string (filenm001))
      call clfdvm()
      call dvmh_finish()
      dvm000(11) = lexit (dvm0c0)
      end


!-----------------------------------------------------------------------


!     Host handler for loop on line 49 

      recursive subroutine loop_test2_49_host (loop_ref,b,a,r0000m,l)
      implicit none
      integer*8  loop_ref,b(6),a(6)
      integer  l
      real  r0000m(0:*)
      integer  i
      integer  j
      integer*8  a0004,a0002,b0004,b0002
      integer*8  boundsLow(2),boundsHigh(2),stepsIgnore(2)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      b0002 = b(2)
      b0004 = b(4)
      a0002 = a(2)
      a0004 = a(4)
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),stepsIgn
     &ore(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (j,i)
!$OMP    DO  SCHEDULE (runtime)

!		nest of two parallel loops, iteration (i,j) will be executed on 
!		processor, which is owner of element A(i,j) 
         do 1 j = boundsLow(1),boundsHigh(1)
            do 1 i = boundsLow(2),boundsHigh(2)
               r0000m(a0004 + i + a0002 * j) = 0.
               if (i .eq. 1 .or. j .eq. 1 .or. i .eq. l .or. j .eq. l) t
     &hen
                  r0000m(b0004 + i + b0002 * j) = 0.
               else  
                  r0000m(b0004 + i + b0002 * j) = 1. + i + j
               endif  
1              continue
!$OMP       END DO  NOWAIT
!$OMP    END PARALLEL 
      end subroutine



!     Host handler for loop on line 65 

      recursive subroutine loop_test2_65_host (loop_ref,a,b,r0000m)
      implicit none
      integer*8  loop_ref,a(6),b(6)
      real  r0000m(0:*)
      integer  i
      integer  j
      integer*8  b0004,b0002,a0004,a0002
      integer*8  boundsLow(2),boundsHigh(2),stepsIgnore(2)
      real  eps
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_red_post,loop_red_init,loop_fill_bounds
      a0002 = a(2)
      a0004 = a(4)
      b0002 = b(2)
      b0004 = b(4)
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),stepsIgn
     &ore(1))
      call loop_red_init(loop_ref,1_8,eps,0_8)
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),REDUCTION (max:eps),PRIVATE (j,i)
!$OMP    DO  SCHEDULE (runtime)

!		variable EPS is used for calculation of maximum value
         do 21 j = boundsLow(1),boundsHigh(1)
            do 21 i = boundsLow(2),boundsHigh(2)
               eps = max (eps,abs (r0000m(b0004 + i + b0002 * j) - r0000
     &m(a0004 + i + a0002 * j)))
               r0000m(a0004 + i + a0002 * j) = r0000m(b0004 + i + b0002 
     &* j)
21             continue
!$OMP       END DO  NOWAIT
!$OMP    END PARALLEL 
      call loop_red_post(loop_ref,1_8,eps,0_8)
      end subroutine



!     Host handler for loop on line 73 

      recursive subroutine loop_test2_73_host (loop_ref,b,a,r0000m)
      implicit none
      integer*8  loop_ref,b(6),a(6)
      real  r0000m(0:*)
      integer  i
      integer  j
      integer*8  a0004,a0002,b0004,b0002
      integer*8  boundsLow(2),boundsHigh(2),stepsIgnore(2)
      integer*8  lgsc
      integer*8  loop_get_slot_count
      external loop_fill_bounds
      a0002 = a(2)
      a0004 = a(4)
      b0002 = b(2)
      b0004 = b(4)
      call loop_fill_bounds(loop_ref,boundsLow(1),boundsHigh(1),stepsIgn
     &ore(1))
      lgsc = loop_get_slot_count (loop_ref)
!$OMP PARALLEL  NUM_THREADS (lgsc),PRIVATE (j,i)
!$OMP    DO  SCHEDULE (runtime)

!		Copying shadow elements of array A from 
!		neighbouring processors before loop execution
         do 22 j = boundsLow(1),boundsHigh(1)
            do 22 i = boundsLow(2),boundsHigh(2)
               r0000m(b0004 + i + b0002 * j) = (r0000m(a0004 + (i - 1) +
     & a0002 * j) + r0000m(a0004 + i + a0002 * (j - 1)) + r0000m(a0004 +
     & (i + 1) + a0002 * j) + r0000m(a0004 + i + a0002 * (j + 1))) / 4
22             continue
!$OMP       END DO  NOWAIT
!$OMP    END PARALLEL 
      end subroutine



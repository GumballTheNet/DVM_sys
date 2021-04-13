         subroutine jac ()
         integer  this_stat11
         parameter (l = 8,itmax = 20)
         real  a(:,:)
         integer ,dimension(:),allocatable:: ss
         allocate(a(l,l), this_stat11)
         if (this_stat11 .ne. 0)  call bad_fun()
         if (l .gt. 0) then
            deallocate(a, this_stat11)
            if (this_stat11 .ne. 0)  call bad_fun()
            return 
         endif  
         allocate(ss(100))
         select case (l)
         case (0) 
            l = l + 1
         case (1) 
            deallocate(a, this_stat11)
            if (this_stat11 .ne. 0)  call bad_fun()
            return 
         case (2) 
            deallocate(a, this_stat11)
            if (this_stat11 .ne. 0)  call bad_fun()
            return 
         case default 
            l = l + 1
         end select  
         deallocate(ss)
         deallocate(a, this_stat11)
         if (this_stat11 .ne. 0)  call bad_fun()
         end


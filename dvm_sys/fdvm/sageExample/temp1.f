         subroutine test1 ()
         integer  this_stat9
         integer  this_stat8
         integer  this_stat7
         integer  this_stat6
         integer  this_stat5
         real ,dimension(:,:):: x,y
         real ,dimension(:,:):: z
         real ,dimension(:):: b
         real ,dimension(:,:):: c
         allocate(c(2,3), this_stat9)
         if (this_stat9 .ne. 0)  call bad_fun()
         allocate(b(6), this_stat8)
         if (this_stat8 .ne. 0)  call bad_fun()
         allocate(z((-(2)):2,20), this_stat7)
         if (this_stat7 .ne. 0)  call bad_fun()
         allocate(y(5,20), this_stat6)
         if (this_stat6 .ne. 0)  call bad_fun()
         allocate(x(5,20), this_stat5)
         if (this_stat5 .ne. 0)  call bad_fun()
         b = (/1,1,2,3,5,8/)
         c = reshape (b,(/2,3/))
         deallocate(x, this_stat5)
         if (this_stat5 .ne. 0)  call bad_fun()
         deallocate(y, this_stat6)
         if (this_stat6 .ne. 0)  call bad_fun()
         deallocate(z, this_stat7)
         if (this_stat7 .ne. 0)  call bad_fun()
         deallocate(b, this_stat8)
         if (this_stat8 .ne. 0)  call bad_fun()
         deallocate(c, this_stat9)
         if (this_stat9 .ne. 0)  call bad_fun()
         end

         module oooo
         integer  this_stat10
         character  se
         integer  ut
         real  r(:,:)
         subroutine func3 ()
         deallocate(r, this_stat10)
         if (this_stat10 .ne. 0)  call bad_fun()
         end subroutine  
         subroutine func2 ()
         allocate(r(ut,ut), this_stat10)
         if (this_stat10 .ne. 0)  call bad_fun()
         end subroutine  
         end module 


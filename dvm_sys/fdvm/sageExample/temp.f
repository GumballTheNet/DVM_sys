      program test1
      integer  this_stat3
      integer  this_stat2
      integer  this_stat1
      integer  this_stat0
      real ,dimension(:,:):: x,y
      real ,dimension(:,:):: z
      real ,dimension(:):: b
      real ,dimension(:,:):: c
      allocate(c(2,3), this_stat3)
      allocate(b(6), this_stat2)
      allocate(z((-(2)):2,20), this_stat1)
      allocate(x(5,20), this_stat0)
      b = (/1,1,2,3,5,8/)
      c = reshape (b,(/2,3/))
      deallocate(x, this_stat0)
      deallocate(z, this_stat1)
      deallocate(b, this_stat2)
      deallocate(c, this_stat3)
      end


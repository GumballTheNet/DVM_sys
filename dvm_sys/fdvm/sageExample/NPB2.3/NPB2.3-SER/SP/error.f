
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine error_norm(rms)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c this function computes the norm of the difference between the
c computed solution and the exact solution
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k, m, d
       double precision xi, eta, zeta, u_exact(5), rms(5), add

       do   m = 1, 5 
          rms(m) = 0.0d0
       end do

       do   k = 0, grid_points(3)-1
          zeta = dble(k) * dnzm1
          do   j = 0, grid_points(2)-1
             eta = dble(j) * dnym1
             do   i = 0, grid_points(1)-1
                xi = dble(i) * dnxm1
                call exact_solution(xi, eta, zeta, u_exact)

                do   m = 1, 5
                   add = u(i,j,k,m)-u_exact(m)
                   rms(m) = rms(m) + add*add
                end do
             end do
          end do
       end do

       do    m = 1, 5
          do    d = 1, 3
             rms(m) = rms(m) / dble(grid_points(d)-2)
          end do
          rms(m) = dsqrt(rms(m))
       end do

       return
       end



       subroutine rhs_norm(rms)

       include 'header.h'

       integer i, j, k, d, m
       double precision rms(5), add

       do    m = 1, 5
          rms(m) = 0.0d0
       end do

       do   k = 0, grid_points(3)-2
          do   j = 0, grid_points(2)-2
             do   i = 0, grid_points(1)-2
                do   m = 1, 5
                   add = rhs(i,j,k,m)
                   rms(m) = rms(m) + add*add
                end do
             end do
          end do
       end do


       do   m = 1, 5
          do   d = 1, 3
             rms(m) = rms(m) / dble(grid_points(d)-2)
          end do
          rms(m) = dsqrt(rms(m))
       end do

       return
       end



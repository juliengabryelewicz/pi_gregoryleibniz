program pi_gregoryleibniz
   implicit none

   integer, parameter :: dp = selected_real_kind(31, 307)
   integer (kind=16) :: step, number
   real (kind=dp) :: pi_value

   write(*,*) "Nombre d'étapes pour calculter pi? "
   read(*,*) number

   pi_value = 1.0_dp

   do step = 1,number
      if (mod(step,2) == 1) then
         pi_value = pi_value - (1.0_dp / ((step*2.0_dp)+1.0_dp))
      else
         pi_value = pi_value + (1.0_dp / ((step*2.0_dp)+1.0_dp))
      end if
   end do

   pi_value = pi_value * 4.0_dp
   print *, pi_value

end program pi_gregoryleibniz
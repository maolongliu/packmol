!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!  
!
! Subroutine resetboxes: Subroutine that reset the occupancy of 
!                        linked cell boxes
!

subroutine resetboxes()
      
  use sizes
  use compute_data, only : nboxes, latomfirst, latomfix, &
                           lboxfirst, lboxnext, hasfree, ntotat
  implicit none
  integer :: i, j, k, ibox

  integer :: ii, jj, kk

  ibox = lboxfirst
  do while( ibox > 0 ) 
    call ibox_to_ijk(ibox,i,j,k)
    latomfirst(i,j,k) = latomfix(i,j,k)
    hasfree(i,j,k) = .false.
    ibox = lboxnext(ibox)
  end do
  lboxfirst = 0

end subroutine resetboxes


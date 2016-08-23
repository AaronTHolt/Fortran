
module matrix_ops
  implicit none

contains
subroutine hMatrixConcat(A1, rA, cA, B1, rB, cB, C1, rC, cC)
  ! Takes <=2d matrices A and B and hoizontally concatenates them
  ! A and B must have the same number of rows
  implicit none
  integer, intent(in) :: rA, rB, cA, cB, rC, cC
  real, intent(in) :: A1(1:rA,1:cA), B1(1:rB,1:cB)
  real, intent(out) :: C1(1:rC,1:cC)
  integer :: i, j

  if( rA /= rB ) then
    write(*,*) rA, " ", rB
    return
  end if

  do i=1,rA
    do j=1,cA
      C1(i,j) = A1(i,j)
      !write(*,*) C(i,j)
    end do
    do j=cA+1,cC
      C1(i,j) = B1(i,j-3)
      !write(*,*) C1(i,j)
    end do
  end do

end subroutine hMatrixConcat

end module




program test
  implicit none
  !real, dimension(2:,3:), allocatable :: A, B
  !real, dimension(2:,6:), allocatable :: C
  real, allocatable :: A(:,:), B(:,:)
  real, allocatable :: C(:,:)
  integer :: rowsA, rowsB, rowsC, colsA, colsB, colsC, i, j

  allocate( A(2,3), B(2,3), C(2,6) )

  rowsA = size(A,1)
  rowsB = size(B,1)
  rowsC = rowsA
  colsA = size(A,2)
  colsB = size(B,2)
  colsC = colsA + colsB

  do i = 1,2
    do j = 1,3
      A(i,j) = i*6 + j - 6
      B(i,j) = i*6 + j - 3
    end do
  end do

  ! do i=1,2
  !   do j=1,6
  !     write(*,*) C(i,j)
  !   end do
  ! end do

  call hMatrixConcat(A, rowsA, colsA, B, rowsB, colsB, C, rowsC, colsC)

  do i=1,2
    do j=1,6
      write(*,*) C(i,j)
    end do
  end do

  deallocate( A, B, C )

end program test


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

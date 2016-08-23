program test
  use matrix_ops
  implicit none
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




program test
  implicit none
  !real, dimension(2:,3:), allocatable :: A, B
  !real, dimension(2:,6:), allocatable :: C
  real, allocatable :: A(:,:), B(:,:)
  real, allocatable :: C(:,:)
  integer :: i, j

  allocate( A(2,3), B(2,3), C(2,6) )

  do i = 1,2
    do j = 1,3
      A(i,j) = i*6 + j - 6
      B(i,j) = i*6 + j - 3
    end do
  end do

  call hMatrixConcat(A, B, C)

  deallocate( A, B, C )

contains
  subroutine hMatrixConcat(A, B, C)
    ! Takes <=2d matrices A and B and hoizontally concatenates them
    ! A and B must have the same number of rows
    implicit none
    real, dimension(:,:), intent(in) :: A, B
    !real, dimension(:,:), allocatable :: C
    real, dimension(:,:), intent(out) :: C
    integer :: rowsA, rowsB, colsA, colsB, colsC, i, j
    rowsA = size(A,1)
    rowsB = size(B,1)
    colsA = size(A,2)
    colsB = size(B,2)
    colsC = colsA + colsB
    if( rowsA /= rowsB ) then
      return
    end if


    !do i=1:rowsA
    !  C(i, :) = [A]
    !end do
    !C = [A, B]
    do i=1,rowsA
      do j=1,colsA
        C(i,j) = A(i,j)
        !write(*,*) C(i,j)
      end do
      do j=colsA+1,colsC
        C(i,j) = B(i,j-3)
        !write(*,*) C(i,j)
      end do
    end do

    !write(*,*) "-----------------"

    do i=1,rowsA
      do j=1,colsC
        !write(*,*) C(i,j)
      end do
    end do

  end subroutine hMatrixConcat

end program test

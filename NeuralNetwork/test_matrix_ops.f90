program test
  use matrix_ops
  use types
  implicit none

  real, allocatable :: mm(:,:)
  integer :: rr, cc, i, j, correct, total
  rr = 2
  cc = 3

  call allocMatrix2D(mm, rr, cc)
  call test_hMatrixConcat()
  call test_hMatrixConcat2()
  call test_kronProd()



contains

subroutine test_hMatrixConcat()
  use types
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

  ! do i=1,2
  !   do j=1,6
  !     write(*,*) C(i,j)
  !   end do
  ! end do

  deallocate( A, B, C )
end subroutine test_hMatrixConcat

subroutine test_hMatrixConcat2()
  use types
  implicit none
  type(matrix) :: A, B, AB, AB2
  integer :: r, c, i, j



  r = 2
  c = 2

  call allocMatrixType(A, r, c)
  call allocMatrixType(B, r, c)
  call allocMatrixType(AB2, r, 2*c)

  do j=1,c
    do i=1,r
      A%mat(i,j) = i + j
      B%mat(i,j) = i + j + 10
    end do
  end do

  AB2%mat(1,:) = [2,3,12,13]
  AB2%mat(2,:) = [3,4,13,14]


  call hMatrixConcat2(A, B, AB)

  ! do j=1,AB%cols
  !   do i=1,AB%rows
  !     write(*,*) AB%mat(i,j)
  !     write(*,*) AB2%mat(i,j)
  !   end do
  ! end do

  write(*,*) test_matrixEquality(AB, AB2)

end subroutine test_hMatrixConcat2

subroutine test_kronProd()
  use types
  implicit none

  integer :: i, j
  type(matrix) :: A, B, kpCalc, kp

  call allocMatrixType(A,2,2)
  call allocMatrixType(B,2,2)
  call allocMatrixType(kp,4,4)

  A%mat(:,:) = reshape([1, 2, 3, 4], [2,2])
  B%mat(:,:) = reshape([0, 5, 6, 7], [2,2])
  kp%mat(:,:) = reshape([0, 5, 0, 10, &
                         6, 7, 12, 14, &
                         0, 15, 0, 20, &
                         18, 21, 24, 28], [4,4])

  call kronProd(A, B, kpCalc)
  write(*,*) test_matrixEquality(kp, kpCalc)

end subroutine test_kronProd



integer function test_matrixEquality(A, B, optTol) result(y)
  !! test if two matrix types A and B are the same
  !! returns 1 if true 0 if false
  !! optTol is optiional tolerance value
  use types
  implicit none
  type(matrix), intent(in) :: A, B
  real, optional, intent(in) :: optTol
  real :: tol
  integer :: i, j

  ! Check if tolereance is specified
  if ( .not. present(optTol)) then
    tol = 1e-6
  else
    tol = optTol
  end if

  y = 1

  ! Check for matrix allocation
  if ( .not. allocated(A%mat) ) then
    y = 0
    write(*,*) "Matrix 1 not allocated"
    return
  end if

  if ( .not. allocated(B%mat) ) then
    y = 0
    write(*,*) "Matrix 2 not allocated"
    return
  end if

  ! rows and columns same size
  if (A%rows /= B%rows) then
    y = 0
    write(*,*) "Different row sizes", A%rows, B%rows
    return
  end if
  if (A%cols /= B%cols) then
    y = 0
    write(*,*) "Different column sizes", A%cols, B%cols
    return
  end if

  ! All values ~equal
  do j=1,A%cols
    do i=1,A%rows
      if( abs(A%mat(i,j) - B%mat(i,j)) < tol ) then
        cycle
      else
        ! write(*,*) i, j, A%mat(i,j), B%mat(i,j)
        y = 0
      end if
    end do
    if ( y == 0 ) then
      exit
    end if
  end do

  return
end function test_matrixEquality





end program test


module matrix_ops
  use types
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

subroutine hMatrixConcat2(A, B, C)
  ! Takes matrix type A and B and horizontally concatenates their matrices
  ! Matrices in A and B must have the same number of rows
  use types
  implicit none

  type(matrix), intent(in) :: A, B
  type(matrix), intent(inout) :: C
  integer :: i, j

  ! C metadata
  call allocMatrixType(C, A%rows, A%cols + B%cols)

  ! Concatenate matrices A%mat and B%mat
  do j=1,A%cols
    do i=1,A%rows
      C%mat(i,j) = A%mat(i,j)
    end do
  end do

  do j=1,B%cols
    do i=1,B%rows
      C%mat(i,j+A%rows) = B%mat(i,j)
    end do
  end do

end subroutine hMatrixConcat2


subroutine allocMatrix2D(M, rows, cols)
  implicit none
  integer, intent(in) :: rows, cols
  real, intent(out), allocatable :: M(:,:)

  allocate( M(rows,cols) )

end subroutine allocMatrix2D


subroutine allocMatrixType(M, r, c)
  !! r and c will be rows and columns to be allocated in M
  use types
  implicit none
  integer, intent(in) :: r, c
  type(matrix), intent(out) :: M

  M%rows = r
  M%cols = c
  allocate( M%mat(r,c) )

end subroutine allocMatrixType


subroutine deallocMatrixType(M)
  ! deallocate array in matrix type
  use types
  implicit none
  type(matrix), intent(out) :: M

  if (allocated(M%mat)) deallocate(M%mat)

end subroutine deallocMatrixType


subroutine kronProd(A, B, kp)
  ! Computes the kronecker product of matrix types A and B
  use types
  implicit none
  type(matrix), intent(in) :: A, B
  type(matrix), intent(inout) :: kp
  ! type(matrix) :: temp
  integer :: ai, aj, bi, bj, i, j

  ! call allocMatrixType(temp, B%rows, B%cols)

  ! kp%rows = A%rows*B%rows
  ! kp%cols = A%cols*B%cols
  call allocMatrixType(kp, A%rows*B%rows, A%cols*B%cols)

  ! Calc knonecker product
  ! Loop over A
  do aj=1,A%cols
    do ai=1,A%rows
      ! Loop over B
      do bj=1,B%cols
        do bi=1,B%rows
          i = (ai-1)*B%rows + bi
          j = (aj-1)*B%cols + bj
          kp%mat(i,j) = A%mat(ai,aj)*B%mat(bi,bj)
        end do
      end do
    end do
  end do

end subroutine kronProd


end module

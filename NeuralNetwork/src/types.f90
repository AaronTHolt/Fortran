module types
  implicit none

  ! Made this so I don't have to call size all the time
  type matrix
    real, allocatable :: mat(:,:)
    integer :: rows
    integer :: cols
  end type matrix

  type matrix_int
    integer, allocatable :: mat(:,:)
    integer :: rows
    integer :: cols
  end type matrix_int

  !! s = sepal, p = petal
  !! output(1),output(2),output(3) combine to make species type
  type iris_row
    real :: s_len, s_wid, p_len, p_wid
    integer :: output(3)
    integer :: class = 0
  end type iris_row

end module

module types
  implicit none

  ! Made this so I don't have to call size all the time
  type matrix
    real, allocatable :: mat(:,:)
    integer :: rows
    integer :: cols
  end type matrix

  !! s = sepal, p = petal
  !! t1,t2,t3 combine to make species type
  type iris_row
    real :: s_len, s_wid, p_len, p_wid
    integer :: t1, t2, t3
  end type iris_row

end module

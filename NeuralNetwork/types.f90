module types
  implicit none

  type matrix
    real, allocatable :: mat(:,:)
    integer :: rows
    integer :: cols
  end type matrix

end module

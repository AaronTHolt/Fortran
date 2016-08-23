module types
  implicit none

  type matrix
    real :: A(:,:)
    integer :: rows
    integer :: cols
  end type matrix

end module

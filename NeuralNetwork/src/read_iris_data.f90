
module read_iris_data
  use types
  implicit none

contains

integer function get_file_length(filepath) result(y)
  !! Takes a file ane returns its length
  use types
  implicit none
  character(*), intent(in) :: filepath

  y = 0
  open(1, file=filepath)
  do
    read(1,*)
    y = y + 1
  end do
  close(1)

  write(*,*) y

  return

end function get_file_length

end module read_iris_data

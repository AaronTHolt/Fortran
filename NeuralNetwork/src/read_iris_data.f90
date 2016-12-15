
module read_iris_data
  use types
  implicit none

contains

integer function get_file_length(filepath) result(y)
  !! Takes a file ane returns its length
  use types
  implicit none
  character(*), intent(in) :: filepath
  integer :: reason

  y = 0
  reason = 0
  open(1, file=filepath)
  do
    read(1,*,iostat=reason)

    if (reason > 0) then       ! Read error
      write(*,*) reason, "Can't read"
      exit
    else if (reason < 0) then  ! End of file
      write(*,*) "Number of lines is", y
      exit
    else                       ! Successfully reads
      y = y + 1
    end if

  end do
  close(1)

  write(*,*) y

  return

end function get_file_length

subroutine read_iris(filepath, i_data)
  !!
  use types
  implicit none
  type(iris_data), allocatable, intent(inout) :: i_data(:)
  integer :: file_len, i, reason
  character(*), intent(in) :: filepath

  file_len = get_file_length(filepath)

  !! Allocate space for data

  !! Read in data
  ! do i=1:file_len
  !   read(*,*,iostat=reason)
end subroutine read_iris

end module read_iris_data

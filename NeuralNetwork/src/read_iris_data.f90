
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
      !write(*,*) "Number of lines is", y
      exit
    else                       ! Successfully reads
      y = y + 1
    end if

  end do
  close(1)

  !write(*,*) y

  return

end function get_file_length

subroutine read_iris(filepath, file_len, i_data)
  !!
  use types
  implicit none
  type(iris_row), allocatable, intent(inout) :: i_data(:)
  integer, intent(in) :: file_len
  integer :: i, reason
  character(*), intent(in) :: filepath

  ! file_len = get_file_length(filepath)

  !! Allocate space for data
  ! allocate( i_data(file_len) )

  !! Read in data
  open (unit=99, file=filepath, status='old', action='read')
  do i=1,file_len
    read(99,*,iostat=reason) i_data(i)%s_len, i_data(i)%s_wid, i_data(i)%p_len, i_data(i)%p_wid, &
                              i_data(i)%t1, i_data(i)%t2, i_data(i)%t3
    ! write(*,*) i_data(i)%s_len, i_data(i)%s_wid, i_data(i)%p_len, i_data(i)%p_wid, &
    !                           i_data(i)%t1, i_data(i)%t2, i_data(i)%t3
  end do
  close(99)

end subroutine read_iris

end module read_iris_data

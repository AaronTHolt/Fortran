program test_read
  use read_iris_data
  use types
  implicit none

  call test_read_iris("datasets/iris/iris_validation.dat")


contains

  subroutine test_read_iris(fileName)
    use types
    implicit none
    character(*), intent(in) :: fileName
    integer :: fileLength

    fileLength = get_file_length(fileName)
    write(*,*) fileLength

    ! First and last lines
    ! 0.813331	0.692682	0.854032	0.975	0	0	1
    ! 0.744044	0.551523	0.793548	0.901998	0	0	1

  end subroutine test_read_iris

end program test_read

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

  end subroutine test_read_iris

end program test_read

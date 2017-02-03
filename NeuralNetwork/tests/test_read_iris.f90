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
    type(iris_row), allocatable :: i_data(:)

    fileLength = get_file_length(fileName)
    write(*,*) "Length ? :", (fileLength == 37)

    allocate( i_data(fileLength))
    ! First and last lines
    ! 0.813331	0.692682	0.854032	0.975	0	0	1
    ! 0.744044	0.551523	0.793548	0.901998	0	0	1
    call read_iris(fileName, fileLength, i_data)

    write(*,*) "First Line :", (i_data(1)%s_len==0.813331), (i_data(1)%s_wid==0.692682), &
                    (i_data(1)%p_len==0.854032), (i_data(1)%p_wid==0.975), &
                    (i_data(1)%output(1)==0), (i_data(1)%output(2)==0), (i_data(1)%output(3)==1)

    write(*,*) "Last Line :", (i_data(fileLength)%s_len==0.744044), (i_data(fileLength)%s_wid==0.551523), &
                    (i_data(fileLength)%p_len==0.793548), (i_data(fileLength)%p_wid==0.901998), &
                    (i_data(fileLength)%output(1)==0), (i_data(fileLength)%output(2)==0), (i_data(fileLength)%output(3)==1)


  end subroutine test_read_iris

end program test_read

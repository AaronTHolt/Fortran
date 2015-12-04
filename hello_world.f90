program hello_world
implicit none

real, dimension(10) :: x
character :: asdf*14
integer :: i

asdf = "Hello World!"
write(*,*) asdf

do i=1,10
    x(i) = i*i
    print *, x(i)
    print *, i
end do

print *, "End program ", asdf

end program hello_world
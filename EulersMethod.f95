subroutine next_point(ty_array,y,t,step,slope,y_new)
    ! Euler's method
    ! step is t step size
    ! slope is the slope at current y and t
    ! t_new and y_new are next point
    real,dimension(0:2,0:2), intent(in) :: ty_array
    real, intent(in) :: step, y, t
    real, intent(out) :: y_new, slope
    integer :: i, j  !i=y exponent, j=t exponent

    slope = 0.0

    do i = 0, 2 
        do j = 0, 2
            ! coef*y^dim*t^dim
            slope = slope + ty_array(i,j)*(y**i)*(t**j)
        end do
    end do

    y_new = y + slope*step

end subroutine next_point

subroutine RK4(ty_array,y,t,step,slope,y_new)
    ! Runge-Kutta 4th order
    ! step is t step size
    ! slope is the slope at current y and t
    ! t_new and y_new are next point
    real,dimension(0:2,0:2), intent(in) :: ty_array
    real, intent(in) :: step, y, t
    real, intent(out) :: y_new, slope
    integer :: i, j  !i=y exponent, j=t exponent

end subroutine RK4


program diffeq
implicit none
    real :: ic ! y(t=0)=initialcondition 
    real :: step ! step size for Euler methos
    real :: t ! initial t value
    real :: y_new
    real :: slope
    integer :: num_steps = 10 ! number of steps to solve for
    real,dimension(0:2,0:2) :: ty_array  
    real,dimension(0:4) :: y ! stores y solutions
    integer :: i

    ty_array(:,:) = 0
    ty_array(1,0) = -1
    ty_array(0,1) = 1

    ic = 1
    step = 0.25
    t = 0
!     num_steps = 4
    y(0) = ic

    do i = 0,num_steps
        print*, "t = ", t, "y = ", y(i)
        call next_point(ty_array,y(i),t,step,slope,y_new)
        y(i+1) = y_new
        t = t + step
    end do

end program diffeq
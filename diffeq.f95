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

subroutine RK4(ty_array,y,t,step,y_new)
    ! Runge-Kutta 4th order
    ! step is t step size
    ! slope is the slope at current y and t
    ! kn's are slopes at different points
    ! t_new and y_new are next point
    real,dimension(0:2,0:2), intent(in) :: ty_array
    real, intent(in) :: step, y, t
    real, intent(out) :: y_new
    real :: kn1, kn2, kn3, kn4 
    integer :: i, j  !i=y exponent, j=t exponent

    ! t(n+1) = t(n) + step
    ! y(n+1) = y(n) + step/6 * (kn1 + 2kn2 + 2kn3 + kn4) 

    kn1 = 0.0
    kn2 = 0.0
    kn3 = 0.0
    kn4 = 0.0

    ! find kn1
    do i = 0, 2
        do j = 0, 2
            kn1 = kn1 + ty_array(i,j)*(y**i)*(t**j)
        end do 
    end do

    ! kn2
    do i = 0, 2
        do j = 0, 2
            kn2 = kn2 + ty_array(i,j)*((y+step/2*kn1)**i)*((t+step/2)**j)
        end do
    end do

    ! kn3
    do i = 0, 2
        do j = 0, 2
            kn3 = kn3 + ty_array(i,j)*((y+step/2*kn2)**i)*((t+step/2)**j)
        end do
    end do

    ! kn4
    do i = 0, 2
        do j = 0, 2
            kn4 = kn4 + ty_array(i,j)*((y+step*kn3)**i)*((t+step)**j)
        end do
    end do
    
    !! debug
!     print*, "kn1 = ", kn1, "kn2 = ", kn2, "kn3 = ", kn3, "kn4 = ", kn4

    y_new = y + (step/6.0)*(kn1 + 2*kn2 + 2*kn3 + kn4)

end subroutine RK4


program diffeq
implicit none
    real :: ic ! y(t=0)=initialcondition 
    real :: step ! step size for Euler methos
    real :: t ! initial t value
    real :: y_new
    real :: slope
    integer :: num_steps = 40 ! number of steps to solve for
    real,dimension(0:2,0:2) :: ty_array  
    real,dimension(0:40) :: y, y4 ! stores y solutions
    integer :: i

    ty_array(:,:) = 0
    ty_array(1,0) = 1
    ty_array(0,1) = 1

    ic = 0
    step = 0.1
    t = 0
    y(0) = ic

    print*, "Euler's"
    ! Eulers Method
    do i = 0, num_steps
        print*, "t = ", t, "y = ", y(i) 
        call next_point(ty_array,y(i),t,step,slope,y_new)
        y(i+1) = y_new
        t = t + step
    end do

    print*, "RK4"
    ! Runge-Kutta 4th order
    ty_array(:,:) = 0
    ty_array(1,0) = 1
    ty_array(0,1) = 1

    ic = 0
    t = 0
    y4(0) = ic

    do i = 0, num_steps
        print*, "t = ", t, "y = ", y4(i)
        call RK4(ty_array,y4(i),t,step,y_new)
        y4(i+1) = y_new
        t = t + step
    end do

end program diffeq
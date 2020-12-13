!these are two neurons chaining up through stdp

program main
    implicit none

    !global parameters
    integer :: rest = -70
    integer :: threshold = -55
    integer :: tau = 30
    integer :: tau_trace = 10
    integer :: noise_lv = 3
    
    !structure Neuron
    type Neuron 
        real :: v = -70
        integer :: s = 0
        real :: trace = 0
    end type Neuron
    
    !STDP  
    real :: stdp = 0
    integer :: tau_s = 30
    real, parameter :: aplus = 2.0
    real, parameter :: aminus = -1.1 * aplus
    integer :: wmax = 20
    
    !connection w
    real :: w = 0
    real :: tauw = 10000  !weight decay
    
    !settings
    type(Neuron) :: cell1
    type(Neuron) :: cell2
    
    integer, parameter :: tmax = 50000
    integer :: t = 0
    integer, dimension(tmax) :: Time
    
        
    !run
        !write the file
        open(1, file='data95.txt', status='new')
        !write X
        write(1,*)
        do t=1,tmax,1
            write(1,*) t
        end do
        write(1,*) '$'
    
    do t=1, tmax, 1
        call run(threshold, rest, tau, cell1%v, cell1%s, noise_lv, w, cell2%s)
        call run(threshold, rest, tau, cell2%v, cell2%s, noise_lv, w, cell1%s)
        
        call traced(tau_trace, cell1%trace, cell1%s)
        call traced(tau_trace, cell2%trace, cell2%s)
    
        stdp = -stdp / tau_s + aplus * cell1%trace * cell2%s + aminus * cell2%trace * cell1%s
        
        w = w - w/tauw + stdp
        
        call wclip(w, wmax)
    
        Time(t) = t
            !write Y
            write(1,*) w
    end do
    print *, ' '
    print *, 'final connection: ', w
    
    close(1)
    
    call system('python plot_fortran.py data95.txt')
    
contains
    real function stimuli() result(stm)
        implicit none
        integer, parameter :: max = 10
        real :: r

        call random_number(r)
    stm = max * r
 
 end function stimuli 
    
end program main


subroutine run(th, r, tau, v, s, noise_lv, w, spre)
    implicit none
    integer :: th, r, tau, s, spre
    real :: v, stm, rand, w
    integer :: noise_lv

    call random_number(rand)
    stm = rand * noise_lv
    v = v + (r - v) / tau + stm + w*spre
    call check(th, r, v, s)
    
end subroutine run

subroutine check(th, r, v, s)
    integer :: th, r, s
    real :: v
    
    if(v > th) then
        v = r
        s = 1
    else
        s = 0
    end if
end subroutine check

subroutine traced(tau, tr, s)
    real :: tr
    integer :: tau, s
    tr = -tr / tau + s
end subroutine traced

subroutine wclip(w, wmax)
    real :: w
    integer :: wmax
    if(w > wmax) then
        w = wmax
    else if(w < -wmax) then
        w = -wmax
    end if
end subroutine wclip




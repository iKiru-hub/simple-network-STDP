
!this is a network of n LIF neurons interacting through STDP 

program main
    implicit none

    !neuron parameters
    integer :: rest = -70
    integer :: threshold = -55
    integer :: tau = 40
    real :: a = 1. !for the neuron trace
    integer :: tau_trace = 100
    real :: noise_lv = 5 
    integer, parameter :: n = 100
    
    !structure Neuron
    type Neuron 
        real :: v = -70
        integer :: s = 0
        real :: trace = 0
    end type Neuron
    
    !structure NetworkType
    type NetworkType
        type(Neuron), dimension(n) :: neurons
        real, dimension(n, n) :: weights
        real, dimension(n, n) :: stdp
        integer, dimension(n) :: spikes
    end type NetworkType
    
    !STDP parameters
    integer :: tau_s = 100
    real, parameter :: aplus = 0.8
    real, parameter :: aminus = -1.0 * aplus
    integer :: wmax = 20
    
    !connection w
    real :: w = 0
    real :: tauw = 5000  !weight decay
    
    !settings
    type(NetworkType) :: network
    
    integer, parameter :: tmax = 60000 !60s
    integer :: t, k = 0
    real, dimension(tmax) :: firing_rate 
    real :: R, stimulus, inputs, avg_w, rate
    
    !network initialization
    integer :: i, j
    type(Neuron) :: something
    do i=1, n
        do j=1, n
            network%weights(i, j) = 0
            network%stdp(i, j) = 0
        end do
    end do
        
    !initialize the file writing
    open(1, file='data95.txt', status='new')
    !write X
    write(1,*)
    do t=1,tmax,1
        write(1,*) t
    end do
    write(1,*) '$'
    
    
    !run
    do t=1, tmax, 1
        rate = 0
        do k=1, n
            !noise
            stimulus = stimuli(noise_lv)
            inputs = 0
            
            !network inputs: weights x spikes
            do j=1, n
                inputs = inputs + network%weights(k, j) * network%spikes(j)
            end do
            
            !voltage update
            network%neurons(k)%v = network%neurons(k)%v + (rest - network%neurons(k)%v) / tau + stimulus + inputs
            
            !check spikes
            call check(threshold, rest, network%neurons(k)%v, network%neurons(k)%s)
            network%spikes(k) = network%neurons(k)%s
            
            !trace update
            network%neurons(k)%trace = - network%neurons(k)%trace + a * network%neurons(k)%s
            
            !firing rate
            rate = rate + network%neurons(k)%s
        
        end do
    
        do i=1, n
            do j=1, n
                !stdp update
                network%stdp(i, j) = network%stdp(i, j) -network%stdp(i, j) / tau_s
                network%stdp(i, j) = network%stdp(i, j) + aplus * network%neurons(i)%trace * network%neurons(j)%s
                network%stdp(i, j) = network%stdp(i, j) + aminus * network%neurons(j)%trace * network%neurons(i)%s
        
                !weight update
                network%weights(i, j) = network%weights(i, j) + network%stdp(i, j) 
                call wclip(network%weights(i, j), wmax)
                
                !average weight
                avg_w = avg_w + network%weights(i, j)
            end do
        end do
        firing_rate(t) = rate / n
        avg_w = avg_w / (n*n)
        
        !record avg weight
        write(1,*) avg_w
        
    end do
    print *, ' '
    print *, 'finished'
    
    !record rate
    write(1, *) '$'
    do k=1, tmax
        write(1, *) firing_rate(k)
    end do
    
    close(1)
    
    !plot
    call system('python plot_fortran.py data95.txt')
    
contains
    real function stimuli(lv) result(stm)
        implicit none
        real :: lv, r
        call random_number(r)
        stm = lv * r
    end function stimuli
    
end program main

!check the spike
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


subroutine wclip(w, wmax)
    real :: w
    integer :: wmax
    if(w > wmax) then
        w = wmax
    else if(w < -wmax) then
        w = -wmax
    end if
end subroutine wclip




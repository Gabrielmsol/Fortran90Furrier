program furrier
implicit none
real(kind=8),dimension(:,:),allocatable :: fa,fb
real(kind=8),dimension(:),allocatable   :: f
character(len=3)                        :: dummy
integer(kind=8)                         :: i,n,si,sn
real(kind=8)                            :: x,dx,xi,xf,pi,ran
open(1,file='variables.txt')
   read(1,1) dummy,pi
   read(1,2) dummy,si
   read(1,2) dummy,sn
close(1)
xi=-pi
xf=pi
dx=(xf-xi)/si
allocate(f(0:si));allocate(fa(0:sn,0:si));allocate(fb(0:sn,0:si))
   do i=0,si
      x=xi+i*dx
      f(i)=x ! this is your function, change it as you want 
   end do
   open(2,file='main_function.txt')
      do i=0,si
         x=xi+i*dx
         write(2,3) x,f(i)
      end do
   close(2)
   do n=0,sn
      do i=0,si
         x=xi+i*dx
         fa(n,i)=f(i)*cos(n*x)
         fb(n,i)=f(i)*sin(n*x)
      end do
   end do
   open(3,file='constants.txt')
      do n=0,sn
         if (n .eq. 0) write(3,3) sum(f)*dx/(2*pi),0.0
         if (n .gt. 0) write(3,3) sum(fa(n,:))*dx/pi,sum(fb(n,:))*dx/pi
      end do
   close(3)
deallocate(f);deallocate(fa);deallocate(fb)
1 format(a3,1x,f11.6)
2 format(a3,1x,i8)
3 format(f11.6,2x,f11.6)
end program furrier

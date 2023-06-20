program furrier
implicit none
real(kind=8),dimension(:,:),allocatable :: f,mat
real(kind=8),dimension(:),allocatable   :: a,b
character(len=3)                        :: dummy
integer(kind=8)                         :: i,n,si,sn
real(kind=8)                            :: x,dx,xi,xf,pi
open(1,file='variables.txt')
   read(1,1) dummy,pi
   read(1,2) dummy,si
   read(1,2) dummy,sn
close(1)
xi=-pi
xf=pi
dx=(xf-xi)/si
allocate(a(0:sn));allocate(b(0:sn));allocate(f(0:sn,0:si));allocate(mat(-1:sn,0:si))
   open(2,file='constants.txt')
      do n=0,sn
         read(2,3) a(n),b(n)
      end do
   close(2)
   do n=0,sn
      do i=0,si
         x=xi+i*dx
         f(n,i)=a(n)*cos(n*x)+b(n)*sin(n*x)
      end do
   end do
   open(3,file='main_furrier.txt')
      do i=0,si
         x=xi+i*dx
         write(3,3) x,sum(f(:,i))
      end do
   close(3)
   do n=-1,6
      do i=0,si
         x=xi+i*dx
         if (n .eq. -1) mat(n,i)=x
         if (n .gt. -1) mat(n,i)=f(n,i)
      end do
   end do
   open(4,file='sub_furrier.txt')
      do i=0,si
         write(4,4) mat(-1:6,i)
      end do
   close(4)
deallocate(a);deallocate(b);deallocate(f);deallocate(mat)
1 format(a3,1x,f11.6)
2 format(a3,1x,i8)
3 format(f11.6,2x,f11.6)
4 format(8(f11.6,2x))
end program furrier

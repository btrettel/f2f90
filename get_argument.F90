subroutine get_argument(i,c)
implicit none
!
! Return command line argument 'I' in the character
! variable 'C'
!
character (len=*) :: c
integer :: i
!
#define STANDARD
#ifdef CRAY
character (len=512) :: string
integer :: getarg, ilen
!
! Under UNICOS, getarg is implemented as an integer
! function, instead of the "standard" subroutine implementation.
!
ilen = getarg(i,string) 
if(ilen >= 1) then
  c = string(1:ilen)
else
  c = ' '
endif
#undef STANDARD
#endif
#ifdef NULL
print *,'get_argument is a dummy routine!'
print *,'Set LGETARG to false in the INPUT subroutine.'
call exit(1)
#endif
#ifdef STANDARD
call getarg(i,c)
#endif
end subroutine get_argument

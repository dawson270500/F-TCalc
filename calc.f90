! Fortran # Terminal Calc (F-TCalc)
!   A terminal driven calculator written in fortran
! Author: Bailey Dawson
! Date: 02.03.2023

! Module from https://stackoverflow.com/a/24077338
module str2int_mod
contains 

  elemental subroutine str2int(str,int,stat)
    implicit none
    ! Arguments
    character(len=*),intent(in) :: str
    integer,intent(out)         :: int
    integer,intent(out)         :: stat

    read(str,*,iostat=stat)  int
  end subroutine str2int

end module

program calc
  use str2int_mod
  implicit none
  
  ! VARS
  integer :: IntFirstNum, IntSecondNum, num_args, stat, result
  character(16) :: args
  logical :: IsNum

  ! Grab number of arguments received and check if valid
  num_args = command_argument_count()
  if (num_args == 1) then 
    call get_command_argument(1,args)
    if (args == '-h') then ! Help text
      print *, 'F-TCalc - Version 0.1'
      print *, ' A terminal based calculator, written in fortran'
      print *, ' Arugments can be at most 16 characters long'
      print *, ' Arguments: '
      print *, '  calc <int-1> [<oper> <int-2>]'
      print *, '  int-1 : First number in operation, or -h for help'
      print *, '  oper : Operation to run. Valid operations are "+", "-", "/" and "x"'
      print *, '  int-2 : Second number in operation'
      print *, ' Returns: '
      print *, '  -1 : Incorrect number of arguments'
      print *, '  -2 : Invalid Arguments'
      print *, '  Otherwise will return answer'
      call exit(0)
    endif
  else if (num_args /= 3) then
    print *, 'Incorrect number of arguments, got ', num_args, '. Expeceted 3'
    call exit(-1)
  endif

  ! Grab first arg and convert to int, if it is a number
  call get_command_argument(1,args)
  if (isNum(args)) then 
    call str2int(args,IntFirstNum,stat)
  else
    print *, 'Invalid Arguments'
    call exit(-2)
  endif
  
  ! Grab third arg and convert to int, if it is a number
  call get_command_argument(3,args)
  if (isNum(args)) then 
    call str2int(args,IntSecondNum,stat)
  else
    print *, 'Invalid Arguments'
    call exit(-2)
  endif

  ! Grab second arg and store it, if its a valid operation
  call get_command_argument(2, args)
  select case (args)
    ! Valid chars
    case ('+')
      result = IntFirstNum + IntSecondNum
    case ('-')
      result = IntFirstNum - IntSecondNum
    case ('/')
      result = IntFirstNum / IntSecondNum
    case ('x')
      result = IntFirstNum * IntSecondNum

    ! Invalid goes here
    case default
      print *, 'Invalid Operation'
      call exit(2)

  end select

  print *, result
  call exit(result)

end program calc


  ! Checks if passed string is a number
  ! Returns:
  !  .true. : string is a number
  !  .false. : string is not a number
  function isNum(s) result(ret)
    character(len=12), intent(in) :: s
    logical :: ret
    ret = .true.
    do i = 1, len(s)
      ! Check for all valid chars
      if (ret .eqv. .false.) then
        exit
      endif
      select case (s(i:i))

        ! Valid chars
        case (' ') ! We ignore spaces as the str 2 int function can handle them
        case ('0') 
        case ('1')
        case ('2') 
        case ('3')
        case ('4')
        case ('5')
        case ('6')
        case ('7')
        case ('8')
        case ('9')
          ret = .true.

        case default
          ret = .false.

      end select
    end do

end function isNum
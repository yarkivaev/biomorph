module evolution
  use kinds
  implicit none

contains

  function selection_valid(choice) result(valid)
    integer, intent(in) :: choice
    logical :: valid
    valid = choice == 0 .or. (choice >= 1 .and. choice <= GRID_SIZE)
  end function selection_valid

end module evolution

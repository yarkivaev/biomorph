module terminal_color
  use kinds
  use phenotype
  implicit none

  integer, parameter :: CUBE_BASE = 16
  integer, parameter :: CUBE_LEVELS = 5

contains

  function stroke_to_cube_index(stroke) result(index)
    type(stroke_style_type), intent(in) :: stroke
    integer :: index
    integer :: r
    integer :: g
    integer :: b
    r = clamp_cube_level(stroke%red)
    g = clamp_cube_level(stroke%green)
    b = clamp_cube_level(stroke%blue)
    index = CUBE_BASE + 36 * r + 6 * g + b
  end function stroke_to_cube_index

  function clamp_cube_level(channel) result(level)
    integer, intent(in) :: channel
    integer :: level
    level = channel * CUBE_LEVELS / COLOR_MAX
    if (level < 0) level = 0
    if (level > CUBE_LEVELS) level = CUBE_LEVELS
  end function clamp_cube_level

end module terminal_color

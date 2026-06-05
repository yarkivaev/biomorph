module raster
  use kinds
  use phenotype
  implicit none

  character(len=1), parameter :: INK = '#'
  character(len=1), parameter :: BLANK = ' '

contains

  function rasterize_view(view) result(buffer)
    type(biomorph_view_type), intent(in) :: view
    character(len=1) :: buffer(ASCII_WIDTH, ASCII_HEIGHT)
    type(bounds_type) :: box
    real :: scale
    real :: span
    integer :: i
    buffer = BLANK
    if (size(view%segments) == 0) return
    box = bounds_from_view(view)
    span = max(box%xmax - box%xmin, box%ymax - box%ymin, 1.0)
    scale = real(min(ASCII_WIDTH - 2, ASCII_HEIGHT - 2)) / span
    do i = 1, size(view%segments)
      call rasterize_segment(view%segments(i), buffer, scale)
    end do
  end function rasterize_view

  subroutine rasterize_segment(segment, buffer, scale)
    type(segment_type), intent(in) :: segment
    character(len=1), intent(inout) :: buffer(ASCII_WIDTH, ASCII_HEIGHT)
    real, intent(in) :: scale
    integer :: x0
    integer :: y0
    integer :: x1
    integer :: y1
    x0 = nint(segment%start%x * scale) + ASCII_WIDTH / 2
    y0 = ASCII_HEIGHT / 2 - nint(segment%start%y * scale)
    x1 = nint(segment%finish%x * scale) + ASCII_WIDTH / 2
    y1 = ASCII_HEIGHT / 2 - nint(segment%finish%y * scale)
    call draw_line(buffer, x0, y0, x1, y1)
  end subroutine rasterize_segment

  subroutine draw_line(buffer, x0, y0, x1, y1)
    character(len=1), intent(inout) :: buffer(ASCII_WIDTH, ASCII_HEIGHT)
    integer, intent(in) :: x0
    integer, intent(in) :: y0
    integer, intent(in) :: x1
    integer, intent(in) :: y1
    integer :: dx
    integer :: dy
    integer :: sx
    integer :: sy
    integer :: err
    integer :: x
    integer :: y
    integer :: step
    integer :: limit
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    limit = dx + dy + 1
    if (limit > 4096) return
    if (x0 < x1) then
      sx = 1
    else
      sx = -1
    end if
    if (y0 < y1) then
      sy = 1
    else
      sy = -1
    end if
    err = dx - dy
    x = x0
    y = y0
    do step = 1, limit
      call plot(buffer, x, y)
      if (x == x1 .and. y == y1) return
      if (2 * err > -dy) then
        err = err - dy
        x = x + sx
      end if
      if (2 * err < dx) then
        err = err + dx
        y = y + sy
      end if
    end do
  end subroutine draw_line

  subroutine plot(buffer, x, y)
    character(len=1), intent(inout) :: buffer(ASCII_WIDTH, ASCII_HEIGHT)
    integer, intent(in) :: x
    integer, intent(in) :: y
    if (x < 1 .or. x > ASCII_WIDTH) return
    if (y < 1 .or. y > ASCII_HEIGHT) return
    buffer(x, y) = INK
  end subroutine plot

end module raster

module ncurses_renderer
  use kinds
  use phenotype
  use renderer_iface
  use raster
  use terminal_color
  use ncurses_bindings
  implicit none

  integer, parameter :: PROMPT_ROW = 44
  character(len=*), parameter :: PROMPT = 'Select creature (1-15, 0=quit): '
  integer, parameter :: INPUT_COL = len(PROMPT)

contains

  function make_renderer() result(r)
    type(renderer_type) :: r
    r%render => ncurses_render_population
    r%read_choice => ncurses_read_choice
    r%start => ncurses_renderer_start
    r%stop => ncurses_renderer_stop
    r%name = "ncurses"
  end function make_renderer

  subroutine ncurses_renderer_start()
    call ncurses_open()
  end subroutine ncurses_renderer_start

  subroutine ncurses_renderer_stop()
    call ncurses_close()
  end subroutine ncurses_renderer_stop

  subroutine ncurses_render_population(views, parent_index, generation)
    type(biomorph_view_type), intent(in) :: views(GRID_SIZE)
    integer, intent(in) :: parent_index
    integer, intent(in) :: generation
    character(len=1) :: buffers(ASCII_WIDTH, ASCII_HEIGHT, GRID_SIZE)
    character(len=64) :: title
    integer :: slot
    integer :: row_index
    integer :: col_index
    integer :: cell_row
    integer :: cell_col
    integer :: screen_row
    integer :: screen_col
    if (.not. ncurses_live()) return
    call ncurses_clear_screen()
    write (title, '(A,I0)') 'Generation ', generation
    call ncurses_put_text(0, 0, title)
    do slot = 1, GRID_SIZE
      buffers(:, :, slot) = rasterize_view(views(slot))
      call ncurses_define_pair(slot, stroke_to_cube_index(views(slot)%stroke), COLOR_BLACK)
    end do
    screen_row = 1
    do row_index = 0, 2
      screen_col = 0
      do col_index = 1, 5
        slot = row_index * 5 + col_index
        call draw_label(screen_row, screen_col, slot, slot == parent_index)
        screen_col = screen_col + ASCII_WIDTH + 2
      end do
      screen_row = screen_row + 1
      do cell_row = 1, ASCII_HEIGHT
        screen_col = 0
        do col_index = 1, 5
          slot = row_index * 5 + col_index
          do cell_col = 1, ASCII_WIDTH
            call ncurses_put_colored_char(screen_row, screen_col + cell_col - 1, buffers(cell_col, cell_row, slot), slot)
          end do
          screen_col = screen_col + ASCII_WIDTH + 2
        end do
        screen_row = screen_row + 1
      end do
      screen_row = screen_row + 1
    end do
    call ncurses_refresh_screen()
  end subroutine ncurses_render_population

  subroutine ncurses_read_choice(choice)
    integer, intent(out) :: choice
    character(len=32) :: digits
    integer :: length
    integer :: code
    integer :: status
    if (.not. ncurses_live()) then
      read (*, *) choice
      return
    end if
    call ncurses_put_text(PROMPT_ROW, 0, PROMPT)
    call ncurses_refresh_screen()
    digits = repeat(' ', len(digits))
    length = 0
    do
      code = ncurses_get_char()
      if (code == 10 .or. code == 13) exit
      if (code == 127 .or. code == 8) then
        if (length > 0) then
          call ncurses_put_char(PROMPT_ROW, INPUT_COL + length - 1, ' ')
          digits(length:length) = ' '
          length = length - 1
          call ncurses_refresh_screen()
        end if
      else if (code >= ichar('0') .and. code <= ichar('9')) then
        length = length + 1
        if (length <= len(digits)) then
          digits(length:length) = char(code)
          call ncurses_put_char(PROMPT_ROW, INPUT_COL + length - 1, char(code))
          call ncurses_refresh_screen()
        end if
      end if
    end do
    if (length == 0) then
      choice = -1
      return
    end if
    read (digits(1:length), *, iostat=status) choice
    if (status /= 0) choice = -1
  end subroutine ncurses_read_choice

  subroutine draw_label(row, col, index, highlight)
    integer, intent(in) :: row
    integer, intent(in) :: col
    integer, intent(in) :: index
    logical, intent(in) :: highlight
    character(len=ASCII_WIDTH + 2) :: cell
    integer :: pos
    cell = repeat(' ', ASCII_WIDTH + 2)
    if (highlight) then
      write (cell, '(A,I0,A)') '[', index, ']'
    else
      write (cell, '(I0)') index
    end if
    do pos = 1, len_trim(cell)
      call ncurses_put_char(row, col + pos - 1, cell(pos:pos))
    end do
  end subroutine draw_label

end module ncurses_renderer

module ncurses_bindings
  use iso_c_binding
  implicit none

  integer, parameter :: COLOR_BLACK = 0
  integer, parameter :: COLOR_PAIR_SHIFT = 8

  interface
    type(c_ptr) function initscr() bind(c, name='initscr')
      import c_ptr
    end function initscr
    subroutine endwin() bind(c, name='endwin')
    end subroutine endwin
    subroutine refresh() bind(c, name='refresh')
    end subroutine refresh
    subroutine clear() bind(c, name='clear')
    end subroutine clear
    subroutine cbreak() bind(c, name='cbreak')
    end subroutine cbreak
    subroutine noecho() bind(c, name='noecho')
    end subroutine noecho
    subroutine start_color() bind(c, name='start_color')
    end subroutine start_color
    integer(c_int) function has_colors() bind(c, name='has_colors')
      import c_int
    end function has_colors
    subroutine init_pair(pair, foreground, background) bind(c, name='init_pair')
      import c_int
      integer(c_int), value :: pair
      integer(c_int), value :: foreground
      integer(c_int), value :: background
    end subroutine init_pair
    integer(c_int) function getch() bind(c, name='getch')
      import c_int
    end function getch
    subroutine mvaddch(y, x, ch) bind(c, name='mvaddch')
      import c_int
      integer(c_int), value :: y
      integer(c_int), value :: x
      integer(c_int), value :: ch
    end subroutine mvaddch
    subroutine mvaddstr(y, x, text) bind(c, name='mvaddstr')
      import c_int, c_char
      integer(c_int), value :: y
      integer(c_int), value :: x
      character(kind=c_char) :: text(*)
    end subroutine mvaddstr
  end interface

  logical, save :: active = .false.
  integer, save :: colors_enabled = 0

contains

  subroutine ncurses_open()
    type(c_ptr) :: screen
    if (active) return
    screen = initscr()
    if (.not. c_associated(screen)) return
    call cbreak()
    call noecho()
    colors_enabled = int(has_colors())
    if (colors_enabled > 0) call start_color()
    active = .true.
  end subroutine ncurses_open

  subroutine ncurses_close()
    if (.not. active) return
    call endwin()
    active = .false.
    colors_enabled = 0
  end subroutine ncurses_close

  function ncurses_live() result(live)
    logical :: live
    live = active
  end function ncurses_live

  subroutine ncurses_define_pair(pair, foreground, background)
    integer, intent(in) :: pair
    integer, intent(in) :: foreground
    integer, intent(in) :: background
    if (colors_enabled <= 0) return
    call init_pair(int(pair, c_int), int(foreground, c_int), int(background, c_int))
  end subroutine ncurses_define_pair

  subroutine ncurses_clear_screen()
    call clear()
  end subroutine ncurses_clear_screen

  subroutine ncurses_refresh_screen()
    call refresh()
  end subroutine ncurses_refresh_screen

  subroutine ncurses_put_char(row, col, symbol)
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=1), intent(in) :: symbol
    call mvaddch(row, col, ichar(symbol))
  end subroutine ncurses_put_char

  subroutine ncurses_put_colored_char(row, col, symbol, pair)
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=1), intent(in) :: symbol
    integer, intent(in) :: pair
    integer :: ch
    if (symbol == ' ') then
      call mvaddch(row, col, ichar(symbol))
    else if (pair > 0 .and. colors_enabled > 0) then
      ch = ior(ichar(symbol), ishft(pair, COLOR_PAIR_SHIFT))
      call mvaddch(row, col, ch)
    else
      call mvaddch(row, col, ichar(symbol))
    end if
  end subroutine ncurses_put_colored_char

  subroutine ncurses_put_text(row, col, text)
    integer, intent(in) :: row
    integer, intent(in) :: col
    character(len=*), intent(in) :: text
    character(len=len(text) + 1) :: c_text
    c_text = trim(text) // c_null_char
    call mvaddstr(row, col, c_text)
  end subroutine ncurses_put_text

  function ncurses_get_char() result(code)
    integer :: code
    code = int(getch())
  end function ncurses_get_char

end module ncurses_bindings

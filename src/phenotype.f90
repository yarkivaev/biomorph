module phenotype
  use kinds
  use genome
  implicit none

  type :: point_type
    real :: x
    real :: y
  end type

  type :: segment_type
    type(point_type) :: start
    type(point_type) :: finish
  end type

  type :: stroke_style_type
    integer :: red
    integer :: green
    integer :: blue
    integer :: weight
  end type

  type :: bounds_type
    real :: xmin
    real :: ymin
    real :: xmax
    real :: ymax
  end type

  type :: biomorph_view_type
    type(segment_type), allocatable :: segments(:)
    type(stroke_style_type) :: stroke
  end type

  type :: stem_type
    real :: x
    real :: y
  end type

contains

  function phenotype_from_genome(source) result(view)
    type(genome_type), intent(in) :: source
    type(biomorph_view_type) :: view
    type(stem_type) :: stems(8)
    type(segment_type), allocatable :: collected(:)
    integer :: total
    allocate(collected(MAX_SEGMENTS))
    stems = stems_from_genes(source)
    total = 0
    call collect_segments(source%gene(GENE_LENGTH), stems, 0, point(0.0, 0.0), collected, total)
    allocate(view%segments(total))
    if (total > 0) view%segments = collected(1:total)
    view%stroke = style_from_genome(source)
  end function phenotype_from_genome

  function stems_from_genes(source) result(stems)
    type(genome_type), intent(in) :: source
    type(stem_type) :: stems(8)
    real :: g1
    real :: g2
    real :: g3
    real :: g4
    real :: g5
    real :: g6
    real :: g7
    g1 = real(source%gene(1))
    g2 = real(source%gene(2))
    g3 = real(source%gene(3))
    g4 = real(source%gene(4))
    g5 = real(source%gene(5))
    g6 = real(source%gene(6))
    g7 = real(source%gene(7))
    stems(1) = stem(0.0, g1)
    stems(2) = stem(g2, g3)
    stems(3) = stem(g4, 0.0)
    stems(4) = stem(g5, -g6)
    stems(5) = stem(0.0, -g7)
    stems(6) = stem(-g5, -g6)
    stems(7) = stem(-g4, 0.0)
    stems(8) = stem(-g2, g3)
  end function stems_from_genes

  function style_from_genome(source) result(style)
    type(genome_type), intent(in) :: source
    type(stroke_style_type) :: style
    style%red = source%gene(GENE_RED)
    style%green = source%gene(GENE_GREEN)
    style%blue = source%gene(GENE_BLUE)
    style%weight = source%gene(GENE_WEIGHT)
  end function style_from_genome

  function bounds_from_view(view) result(box)
    type(biomorph_view_type), intent(in) :: view
    type(bounds_type) :: box
    integer :: i
    if (size(view%segments) == 0) then
      box%xmin = 0.0
      box%ymin = 0.0
      box%xmax = 0.0
      box%ymax = 0.0
      return
    end if
    box%xmin = view%segments(1)%start%x
    box%ymin = view%segments(1)%start%y
    box%xmax = view%segments(1)%start%x
    box%ymax = view%segments(1)%start%y
    do i = 1, size(view%segments)
      box = extend_bounds(box, view%segments(i)%start)
      box = extend_bounds(box, view%segments(i)%finish)
    end do
  end function bounds_from_view

  recursive subroutine collect_segments(depth, stems, direction, origin, collected, count)
    integer, intent(in) :: depth
    type(stem_type), intent(in) :: stems(8)
    integer, intent(in) :: direction
    type(point_type), intent(in) :: origin
    type(segment_type), intent(inout) :: collected(*)
    integer, intent(inout) :: count
    integer :: new_dir
    type(point_type) :: target
    type(segment_type) :: piece
    if (depth <= 0) return
    if (count >= MAX_SEGMENTS) return
    new_dir = mod(mod(direction, 8) + 8, 8) + 1
    target = point(origin%x + real(depth) * stems(new_dir)%x, origin%y + real(depth) * stems(new_dir)%y)
    piece%start = origin
    piece%finish = target
    count = count + 1
    collected(count) = piece
    call collect_segments(depth - 1, stems, direction + 1, target, collected, count)
    call collect_segments(depth - 1, stems, direction - 1, target, collected, count)
  end subroutine collect_segments

  function point(x, y) result(p)
    real, intent(in) :: x
    real, intent(in) :: y
    type(point_type) :: p
    p%x = x
    p%y = y
  end function point

  function stem(x, y) result(s)
    real, intent(in) :: x
    real, intent(in) :: y
    type(stem_type) :: s
    s%x = x
    s%y = y
  end function stem

  function extend_bounds(box, p) result(extended)
    type(bounds_type), intent(in) :: box
    type(point_type), intent(in) :: p
    type(bounds_type) :: extended
    extended = box
    extended%xmin = min(box%xmin, p%x)
    extended%ymin = min(box%ymin, p%y)
    extended%xmax = max(box%xmax, p%x)
    extended%ymax = max(box%ymax, p%y)
  end function extend_bounds

end module phenotype

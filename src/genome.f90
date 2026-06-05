module genome
  use kinds
  implicit none

  type :: genome_type
    integer :: gene(GENE_COUNT)
  end type

contains

  function genome_random() result(g)
    type(genome_type) :: g
    integer :: i
    do i = 1, SKELETON_GENES
      g%gene(i) = random_int(SKELETON_MIN, SKELETON_MAX)
    end do
    g%gene(GENE_LENGTH) = random_int(LENGTH_MIN, LENGTH_MAX)
    g%gene(GENE_WEIGHT) = random_int(WEIGHT_MIN, WEIGHT_MAX)
    g%gene(GENE_RED) = random_int(COLOR_MIN, COLOR_MAX)
    g%gene(GENE_GREEN) = random_int(COLOR_MIN, COLOR_MAX)
    g%gene(GENE_BLUE) = random_int(COLOR_MIN, COLOR_MAX)
  end function genome_random

  function genome_clone(source) result(copy)
    type(genome_type), intent(in) :: source
    type(genome_type) :: copy
    copy%gene = source%gene
  end function genome_clone

  function genome_mutated_at(source, index) result(mutant)
    type(genome_type), intent(in) :: source
    integer, intent(in) :: index
    type(genome_type) :: mutant
    integer :: delta, sign, lower, upper
    mutant = genome_clone(source)
    call gene_bounds(index, lower, upper)
    delta = gene_delta(index)
    sign = random_sign()
    mutant%gene(index) = clamp(mutant%gene(index) + sign * delta, lower, upper)
  end function genome_mutated_at

  subroutine gene_bounds(index, lower, upper)
    integer, intent(in) :: index
    integer, intent(out) :: lower
    integer, intent(out) :: upper
    if (index <= SKELETON_GENES) then
      lower = SKELETON_MIN
      upper = SKELETON_MAX
    else if (index == GENE_LENGTH) then
      lower = LENGTH_MIN
      upper = LENGTH_MAX
    else if (index == GENE_WEIGHT) then
      lower = WEIGHT_MIN
      upper = WEIGHT_MAX
    else
      lower = COLOR_MIN
      upper = COLOR_MAX
    end if
  end subroutine gene_bounds

  function gene_delta(index) result(delta)
    integer, intent(in) :: index
    integer :: delta
    if (index >= GENE_RED) then
      delta = 50
    else
      delta = 1
    end if
  end function gene_delta

  function clamp(value, lower, upper) result(bounded)
    integer, intent(in) :: value
    integer, intent(in) :: lower
    integer, intent(in) :: upper
    integer :: bounded
    bounded = max(lower, min(upper, value))
  end function clamp

  function random_int(lower, upper) result(value)
    integer, intent(in) :: lower
    integer, intent(in) :: upper
    integer :: value
    real :: draw
    call random_number(draw)
    value = lower + int(draw * real(upper - lower + 1))
    if (value > upper) value = upper
    if (value < lower) value = lower
  end function random_int

  function random_sign() result(sign)
    integer :: sign
    if (random_int(0, 1) == 0) then
      sign = -1
    else
      sign = 1
    end if
  end function random_sign

end module genome

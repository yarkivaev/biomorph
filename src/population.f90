module population
  use kinds
  use genome
  implicit none

  type :: population_type
    type(genome_type) :: member(GRID_SIZE)
    integer :: mutation_locus(CHILD_COUNT)
  end type

contains

  function population_breed(parent) result(group)
    type(genome_type), intent(in) :: parent
    type(population_type) :: group
    integer :: slots(CHILD_COUNT)
    integer :: indices(GENE_COUNT)
    integer :: child
    integer :: slot
    integer :: locus
    slots = child_slots()
    indices = shuffled_indices()
    group%member(PARENT_INDEX) = genome_clone(parent)
    do child = 1, CHILD_COUNT
      locus = indices(child)
      group%mutation_locus(child) = locus
      slot = slots(child)
      group%member(slot) = genome_mutated_at(parent, locus)
    end do
  end function population_breed

  function child_slots() result(slots)
    integer :: slots(CHILD_COUNT)
    integer :: i
    integer :: slot
    slot = 0
    do i = 1, GRID_SIZE
      if (i == PARENT_INDEX) cycle
      slot = slot + 1
      slots(slot) = i
    end do
  end function child_slots

  function shuffled_indices() result(order)
    integer :: order(GENE_COUNT)
    integer :: i
    integer :: j
    integer :: swap
    do i = 1, GENE_COUNT
      order(i) = i
    end do
    do i = GENE_COUNT, 2, -1
      j = random_int(1, i)
      swap = order(i)
      order(i) = order(j)
      order(j) = swap
    end do
  end function shuffled_indices

end module population

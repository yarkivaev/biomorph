program biomorph
  use kinds
  use genome
  use phenotype
  use population
  use evolution
  use renderer_iface
  use ncurses_renderer
  implicit none

  type(renderer_type) :: renderer
  type(genome_type) :: parent
  type(population_type) :: group
  type(biomorph_view_type) :: views(GRID_SIZE)
  integer :: generation
  integer :: choice
  integer :: index

  call seed_random()
  renderer = make_renderer()
  call renderer%start()
  parent = genome_random()
  generation = 1

  do
    group = population_breed(parent)
    do index = 1, GRID_SIZE
      views(index) = phenotype_from_genome(group%member(index))
    end do
    call renderer%render(views, PARENT_INDEX, generation)
    call renderer%read_choice(choice)
    if (.not. selection_valid(choice)) cycle
    if (choice == 0) exit
    parent = group%member(choice)
    generation = generation + 1
  end do

  call renderer%stop()

contains

  subroutine seed_random()
    integer :: seed(8)
    seed = 123456789
    call random_seed(put=seed)
  end subroutine seed_random

end program biomorph

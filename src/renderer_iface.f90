module renderer_iface
  use kinds
  use phenotype
  implicit none

  abstract interface
    subroutine render_population_fn(views, parent_index, generation)
      import :: biomorph_view_type, GRID_SIZE
      type(biomorph_view_type), intent(in) :: views(GRID_SIZE)
      integer, intent(in) :: parent_index
      integer, intent(in) :: generation
    end subroutine render_population_fn
    subroutine read_choice_fn(choice)
      integer, intent(out) :: choice
    end subroutine read_choice_fn
    subroutine renderer_lifecycle_fn()
    end subroutine renderer_lifecycle_fn
  end interface

  type :: renderer_type
    procedure(render_population_fn), pointer, nopass :: render => null()
    procedure(read_choice_fn), pointer, nopass :: read_choice => null()
    procedure(renderer_lifecycle_fn), pointer, nopass :: start => null()
    procedure(renderer_lifecycle_fn), pointer, nopass :: stop => null()
    character(len=:), allocatable :: name
  end type

end module renderer_iface


function proc1( model, n, x ) result(f)
   use types
   type(model_t) :: model
   integer :: n
   real(kind=dp) :: x,f

   call random_number(f)
   f = 2*1.0d-3*(f-0.5d0)
end function proc1

function proc2( model, n, x ) result(f)
   use types
   type(model_t) :: model
   integer :: n
   real(kind=dp) :: x,f

   call random_number(f)
   f = 2*1.0d-3*(f-0.5d0)
end function proc2

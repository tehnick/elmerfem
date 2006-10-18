module elmerparam

    implicit none

    interface elmer_param
        module procedure elmer_param_scal, elmer_param_vec
    end interface elmer_param

contains

    function elmer_param_scal (xr, xi, tag) result(y)
        double precision, optional, intent(in) :: xr(:)
        integer, optional, intent(in) :: xi(:)
        character(*), optional, intent(in) :: tag

        double precision :: y

        integer :: ni, nr
        double precision, external :: elmer_param_c
        integer, allocatable :: tmpi(:)
        double precision, allocatable :: tmpr(:)
        character(512) :: tmptag

        if (present(xr)) then
            nr = size(xr)
            allocate(tmpr(nr))
            tmpr = xr
        else
            nr = 0
            allocate(tmpr(0))
        end if

        if (present(xi)) then
            ni = size(xi)
            allocate(tmpi(ni))
            tmpi = xi
        else
            ni = 0
            allocate(tmpi(0))
        end if

        if (present(tag)) then
            tmptag = tag
        else
            tmptag = ""
        end if

        ! Let's hope this works (potentially compiler dependent)
        y = elmer_param_c(nr, tmpr, ni, tmpi, len_trim(tmptag), trim(tmptag))
    end function elmer_param_scal


    function elmer_param_vec (nfun, xr, xi, tag) result(y)
        integer, intent(in) :: nfun
        double precision, optional, intent(in) :: xr(:)
        integer, optional, intent(in) :: xi(:)
        character(*), optional, intent(in) :: tag

        double precision :: y(nfun)

        integer :: ni, nr
        external :: elmer_param_vec_c
        integer, allocatable :: tmpi(:)
        double precision, allocatable :: tmpr(:)
        character(512) :: tmptag

        if (present(xr)) then
            nr = size(xr)
            allocate(tmpr(nr))
            tmpr = xr
        else
            nr = 0
            allocate(tmpr(0))
        end if

        if (present(xi)) then
            ni = size(xi)
            allocate(tmpi(ni))
            tmpi = xi
        else
            ni = 0
            allocate(tmpi(0))
        end if

        if (present(tag)) then
            tmptag = tag
        else
            tmptag = ""
        end if

        ! Let's hope this works (potentially compiler dependent)
        call elmer_param_vec_c(nfun, y, nr, tmpr, ni, tmpi, &
                               len_trim(tmptag), trim(tmptag))
    end function elmer_param_vec

end module elmerparam

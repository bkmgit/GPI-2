program helloworld

 use MPI
 use gaspi
 use , intrinsic :: ISO_C_BINDING
 implicit none
 integer(kind=4)  :: ierr
 integer(gaspi_return_t) :: res
 integer(gaspi_rank_t) :: rank, num
 integer(gaspi_timeout_t) :: timeout
 
 CALL MPI_INIT(ierr)
 timeout = GASPI_BLOCK
 
 res = gaspi_proc_init(timeout)
 res = gaspi_proc_rank(rank)
 res = gaspi_proc_num(num)

 print *,"Hello world from rank ",rank
 
 res = gaspi_proc_term(timeout)
 CALL MPI_FINALIZE(ierr)
end program helloworld

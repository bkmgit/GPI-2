program segments

 use MPI
 use gaspi
 use , intrinsic :: ISO_C_BINDING
 implicit none
 integer(kind=4)  :: ierr
 integer(kind=4)  :: j
 integer(kind=4), parameter :: VLEN = 8
 integer(gaspi_return_t) :: res
 integer(gaspi_rank_t) :: iProc, nProc
 integer(gaspi_timeout_t) :: timeout
 integer(gaspi_alloc_t) :: segment_alloc_policy 
 integer(gaspi_segment_id_t), parameter :: segment_id = 0
 integer(gaspi_size_t), parameter :: segment_size = VLEN*4
 integer(gaspi_rank_t), pointer :: array(:)
 type(c_ptr) :: seg_ptr
 CALL MPI_INIT(ierr)
 
 res = gaspi_proc_init(GASPI_BLOCK)
 res = gaspi_proc_rank(iProc)
 res = gaspi_proc_num(nProc)
 ! create segment
 segment_alloc_policy=GASPI_MEM_UNINITIALIZED
 res = gaspi_segment_create(int(segment_id,1), segment_size, GASPI_GROUP_ALL, GASPI_BLOCK, segment_alloc_policy)  
 ! get segment pointer
 res = gaspi_segment_ptr(int(segment_id,1), seg_ptr)
 ! convert c pointer to Fortran pointer
 call c_f_pointer(seg_ptr, array, shape=[segment_size/sizeof(iProc)])
 ! sync
 res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)

 ! put values in array
 do j=1,VLEN
  array(j) =  iProc*VLEN+j
  print *, "rank ",iProc," element ",j,": ",array(j) 
 end do
 res = gaspi_proc_term(GASPI_BLOCK)
 CALL MPI_FINALIZE(ierr)

end program segments

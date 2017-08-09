subroutine wait_or_die(segment_id, notification_id, expected)
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  integer(gaspi_notification_id_t), intent(inout) :: notification_id
  integer(gaspi_segment_id_t), intent(in) :: segment_id
  integer(gaspi_notification_t), intent(inout) :: expected
  integer(gaspi_notification_id_t) :: id
  integer(gaspi_return_t) :: res
  integer(gaspi_notification_t) :: val

  res = gaspi_notify_waitsome(segment_id, notification_id,1, id, GASPI_BLOCK)
  res = gaspi_notify_reset(segment_id, id, val)

end subroutine wait_or_die


subroutine wait_for_queue_entries(queue, wanted_entries)
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  ! queue should be an array, check how done
  integer(4), intent(in) :: wanted_entries
  integer(gaspi_queue_id_t), intent(inout) :: queue
  integer(gaspi_number_t) :: queue_size_max
  integer(gaspi_number_t) :: queue_size
  integer(gaspi_number_t) :: queue_num
  integer(gaspi_return_t) :: res

  res = gaspi_queue_size_max(queue_size_max)
  res = gaspi_queue_size(queue,queue_size)
  res = gaspi_queue_num(queue_num)

  !if ( (queue_size + wanted_entries) .LEQ. queue_size_max)
    queue = modulo(queue +1,queue_num)
    res = gaspi_wait(queue, GASPI_BLOCK)
  !end if

end subroutine wait_for_queue_entries

subroutine wait_for_queue_entries_for_write_notify(queue_id)
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  integer(gaspi_queue_id_t), intent(inout) :: queue_id
  integer(gaspi_return_t) :: res
  call wait_for_queue_entries(queue_id,2)

end subroutine wait_for_queue_entries_for_write_notify

subroutine wait_for_queue_entries_for_notify(queue_id)
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  integer(gaspi_queue_id_t), intent(inout) :: queue_id
  integer(gaspi_return_t) :: res
  call wait_for_queue_entries(queue_id,1)

end subroutine wait_for_queue_entries_for_notify

subroutine wait_for_flush_queues()
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  integer(gaspi_number_t) :: queue_num
  integer(gaspi_queue_id_t) :: queue=0
  integer(gaspi_return_t) :: res

  res = gaspi_queue_num(queue_num)

  do
    if ( queue >= queue_num) exit
    res = gaspi_wait(queue, GASPI_BLOCK)
    queue = queue + 1
  end do

end subroutine wait_for_flush_queues

subroutine stop_on_error(rval,msg)
   use gaspi
   use , intrinsic :: ISO_C_BINDING
   implicit none

   integer(gaspi_return_t), intent(in) :: rval
   character(len=*), intent(in) :: msg
   if(rval .ne. GASPI_SUCCESS) then
      print *,msg,' error ', rval
      call exit(-1)
   endif

end subroutine stop_on_error

program onesided

 use MPI
 use gaspi
 use , intrinsic :: ISO_C_BINDING
 implicit none
 integer(kind=4)  :: ierr
 integer(kind=4)  :: j
 integer(kind=4), parameter :: VLEN = 8
 integer(gaspi_return_t) :: res
 integer(gaspi_rank_t) :: iProc, nProc, rProc, lProc
 integer(gaspi_timeout_t) :: timeout
 integer(gaspi_alloc_t) :: segment_alloc_policy 
 integer(gaspi_segment_id_t), parameter :: segment_id = 0
 integer(gaspi_size_t), parameter :: segment_size = 2*VLEN*8, write_size = VLEN*8
 integer(gaspi_rank_t), pointer :: array(:), rcv_array(:), src_array(:)
 integer(gaspi_notification_id_t) :: data_available
 integer(gaspi_queue_id_t) :: queue_id = 0
 integer(gaspi_offset_t) :: loc_off = 0
 integer(gaspi_offset_t) :: rem_off = 0
 integer(gaspi_notification_t) :: expected
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
 do j=0,VLEN-1
  array(j) =  iProc*VLEN+j
 end do

 call wait_for_queue_entries_for_write_notify(queue_id)
 rProc = modulo(iProc+nProc+1,nProc)
 !right(iProc,nProc)
 res = gaspi_write_notify(segment_id, loc_off, rProc, &
                          segment_id, rem_off, write_size, &
                          data_available, 1 + iProc, &
                          queue_id, GASPI_BLOCK)

 lProc = modulo(iProc+nProc+1,nProc) 
 !1+left(iProc,nProc)
 expected = lProc
 call wait_or_die(segment_id, data_available, expected) 

 do j = 0,VLEN-1
  print *,"Rank ",iProc," rcv elem ",j," : ",rcv_array(j)
 end do

 call wait_for_flush_queues()

 res = gaspi_proc_term(GASPI_BLOCK)

 call MPI_FINALIZE(ierr)


end program onesided

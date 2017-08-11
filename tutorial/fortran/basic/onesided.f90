subroutine wait_or_die(segment_id, notification_id, expected)
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  integer(gaspi_notification_id_t), intent(inout) :: notification_id
  integer(gaspi_segment_id_t), intent(in) :: segment_id
  integer(gaspi_notification_t), intent(in) :: expected
  integer(gaspi_notification_id_t) :: id
  integer(gaspi_return_t) :: res
  integer(gaspi_notification_t) :: val

  res = gaspi_notify_waitsome(segment_id, notification_id,1, id, GASPI_BLOCK)
  call stop_on_error(res,"gaspi_notify_waitsome")
  res = gaspi_notify_reset(segment_id, id, val)
  call stop_on_error(res,"gaspi_notify_reset")
  if (val .ne. expected) call stop_on_error(-1,"did not get expected value")
end subroutine wait_or_die


subroutine wait_for_queue_entries(queue, wanted_entries)
  use gaspi
  use , intrinsic :: ISO_C_BINDING
  implicit none

  integer(4), intent(in) :: wanted_entries
  integer(gaspi_queue_id_t), intent(inout) :: queue
  integer(gaspi_number_t) :: queue_size_max
  integer(gaspi_number_t) :: queue_size
  integer(gaspi_number_t) :: queue_num
  integer(gaspi_return_t) :: res

  res = gaspi_queue_size_max(queue_size_max)
  call stop_on_error(res,"gaspi_queue_size_max")
  res = gaspi_queue_size(queue,queue_size)
  call stop_on_error(res,"gaspi_queue_size")
  res = gaspi_queue_num(queue_num)
  call stop_on_error(res,"gaspi_queue_num")
  if ( (queue_size + wanted_entries) .gt. queue_size_max) then
    queue = modulo(queue +1,queue_num)
    res = gaspi_wait(queue, GASPI_BLOCK)
    call stop_on_error(res,"gaspi_wait")
  endif

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
  call stop_on_error(res,"gaspi_queue_num")
  do
    res = gaspi_wait(queue, GASPI_BLOCK)
    call stop_on_error(res,"gaspi_wait")
    queue = queue + 1
    if ( queue >= queue_num) exit
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
 integer(gaspi_segment_id_t), parameter :: segment_id_src=0, segment_id_rcv=1
 integer(gaspi_size_t), parameter :: segment_size=VLEN*sizeof(nProc), write_size=VLEN*sizeof(nProc)
 integer(gaspi_rank_t), allocatable, target :: array_rcv(:), array_src(:)
 integer(gaspi_notification_id_t) :: data_available, rcv_notf
 integer(gaspi_notification_t) :: notf, id, val, rcv_val
 integer(gaspi_queue_id_t) :: queue_id = 0
 integer(gaspi_offset_t) :: loc_off = 0
 integer(gaspi_offset_t) :: rem_off = 0
 integer(gaspi_memory_description_t) :: mdesc_src=0, mdesc_rcv=0
 integer(gaspi_notification_t) :: expected
 type(c_ptr) :: seg_ptr_rcv, seg_ptr_src
 call MPI_INIT(ierr)
 
 res = gaspi_proc_init(GASPI_BLOCK)
 call stop_on_error(res,"gaspi_proc_init")
 res = gaspi_proc_rank(iProc)
 call stop_on_error(res,"gaspi_proc_rank")
 res = gaspi_proc_num(nProc)
 call stop_on_error(res,"gaspi_proc_num")
 ! create arrays
 allocate( array_rcv(1:VLEN), array_src(1:VLEN))
 ! map segment to array
 segment_alloc_policy=GASPI_MEM_UNINITIALIZED
 res = gaspi_segment_use(int(segment_id_src,1), c_loc(array_src), segment_size, &
                         GASPI_GROUP_ALL, GASPI_BLOCK, mdesc_src)  
 call stop_on_error(res,"gaspi_segment_use_1")
 res = gaspi_segment_use(int(segment_id_rcv,1), c_loc(array_rcv), write_size, &
                         GASPI_GROUP_ALL, GASPI_BLOCK, mdesc_rcv)
 call stop_on_error(res,"gaspi_segment_use_2")
 ! get segment pointer
 res = gaspi_segment_ptr(int(segment_id_src,1), seg_ptr_src)
 call stop_on_error(res,"gaspi_segment_ptr_1")
 res = gaspi_segment_ptr(int(segment_id_rcv,1), seg_ptr_rcv)
 call stop_on_error(res,"gaspi_segment_ptr_2")
 ! put values in array
 do j=1,VLEN
  array_src(j) =  iProc*VLEN + j
  print *,"Rank ",iProc," src elem ",j," : ",array_src(j)
 enddo

 ! sync
 res = gaspi_barrier(GASPI_GROUP_ALL, GASPI_BLOCK)
 call stop_on_error(res,"gaspi_barrier")
 
 ! Uncomment line below and line 182 to use multiple queues, comment out 183 and
 ! 184
 !call wait_for_queue_entries_for_write_notify(queue_id)

 ! right processor
 rProc = modulo(iProc+nProc+1,nProc)
 notf = 1 + iProc
 res = gaspi_write_notify(segment_id_src, loc_off, rProc, &
                          segment_id_rcv, rem_off, write_size, &
                          data_available, notf, &
                          queue_id, GASPI_BLOCK)
 call stop_on_error(res,"gaspi_write_notify")
 ! left processor
 lProc = modulo(iProc+nProc-1,nProc) 
 expected = 1 + lProc
 call wait_or_die(segment_id_rcv, data_available, expected) 

 do j = 1,VLEN
  print *,"Rank ",iProc," rcv elem ",j," : ",array_rcv(j)
 end do

 !call wait_for_flush_queues()
 res = gaspi_wait(queue_id, GASPI_BLOCK)
 call stop_on_error(res,"gaspi_wait")
 res = gaspi_proc_term(GASPI_BLOCK)
 call stop_on_error(res,"gaspi_proc_term")
 deallocate(array_rcv,array_src)
 call MPI_FINALIZE(ierr)


end program onesided

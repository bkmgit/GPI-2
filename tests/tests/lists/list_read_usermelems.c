#include <test_utils.h>

/* Test read for lists whose maximum length is specified by the
   user */
int
main (int argc, char *argv[])
{
  TSUITE_INIT (argc, argv);

  //setup list length
  gaspi_config_t default_conf;

  ASSERT (gaspi_config_get (&default_conf));

  gaspi_number_t user_elem_max = 348;
  default_conf.rw_list_elem_max = user_elem_max;

  EXPECT_FAIL_WITH (gaspi_config_set (default_conf), GASPI_ERR_CONFIG);

  user_elem_max = 155;
  default_conf.rw_list_elem_max = user_elem_max;

  ASSERT (gaspi_config_set (default_conf));

  //init
  ASSERT (gaspi_proc_init (GASPI_BLOCK));

  // user_elem_max is bounded by elem_max
  gaspi_number_t elem_max;
  ASSERT (gaspi_rw_list_elem_max (&elem_max));
  assert (user_elem_max <= elem_max);

  gaspi_rank_t numranks, myrank;

  ASSERT (gaspi_proc_num (&numranks));
  ASSERT (gaspi_proc_rank (&myrank));

  ASSERT (gaspi_segment_create
          (0, _128MB, GASPI_GROUP_ALL, GASPI_BLOCK, GASPI_MEM_INITIALIZED));

  //prepare memory segment
  gaspi_pointer_t _vptr;

  ASSERT (gaspi_segment_ptr (0, &_vptr));

  gaspi_rank_t *mem = (gaspi_rank_t *) _vptr;

  unsigned long i;
  const unsigned long maxInts = _128MB / sizeof (int);

  for (i = 0; i < maxInts; i++)
  {
    mem[i] = myrank;
  }

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  //construct list of n elems
  gaspi_number_t queue_size = 0;

  const gaspi_number_t nListElems = user_elem_max;

  gaspi_segment_id_t localSegs[nListElems];
  gaspi_offset_t localOffs[nListElems];
  const gaspi_rank_t rank2read = (myrank + 1) % numranks;
  gaspi_segment_id_t remSegs[nListElems];
  gaspi_offset_t remOffs[nListElems];
  gaspi_size_t sizes[nListElems];

  size_t bytes = sizeof (gaspi_rank_t);
  gaspi_offset_t initLocOff = 0;
  gaspi_offset_t initRemOff = (bytes * nListElems + 64);

  for (gaspi_number_t n = 0; n < nListElems; n++)
  {
    sizes[n] = bytes;

    localSegs[n] = 0;
    localOffs[n] = initLocOff;
    initLocOff += bytes;

    remSegs[n] = 0;
    remOffs[n] = initRemOff;
    initRemOff += bytes;
  }

  ASSERT (gaspi_read_list
          (nListElems, localSegs, localOffs, rank2read, remSegs, remOffs,
           sizes, 0, GASPI_BLOCK));

  ASSERT (gaspi_queue_size (0, &queue_size));
  assert (queue_size == nListElems);

  ASSERT (gaspi_wait (0, GASPI_BLOCK));

  //check
  gaspi_offset_t off2check = 0;
  char *chPtr = (char *) _vptr;

  mem = (gaspi_rank_t *) (chPtr + off2check);

  for (gaspi_number_t l = 0; l < nListElems; l++)
  {
    assert (mem[l] == rank2read);
  }

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  ASSERT (gaspi_proc_term (GASPI_BLOCK));

  return EXIT_SUCCESS;
}

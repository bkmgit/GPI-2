#include <stdio.h>
#include <stdlib.h>

#include <test_utils.h>

int
main (int argc, char *argv[])
{
  gaspi_config_t default_conf;
  gaspi_group_t g;
  gaspi_rank_t n;

  TSUITE_INIT (argc, argv);
  ASSERT (gaspi_config_get (&default_conf));
  default_conf.build_infrastructure = GASPI_TOPOLOGY_NONE;
  ASSERT (gaspi_config_set (default_conf));

  ASSERT (gaspi_proc_init (GASPI_BLOCK));

  gaspi_rank_t numranks, myrank;

  ASSERT (gaspi_proc_num (&numranks));
  ASSERT (gaspi_proc_rank (&myrank));

  ASSERT (gaspi_group_create (&g));
  for (n = 0; n < numranks; n++)
  {
    ASSERT (gaspi_group_add (g, n));
  }

  ASSERT (gaspi_group_commit (g, GASPI_BLOCK));

  ASSERT (gaspi_segment_create
          (0, _2MB, g, GASPI_BLOCK, GASPI_MEM_INITIALIZED));

  gaspi_atomic_value_t val;

  for (n = 0; n < numranks; n++)
  {
    ASSERT (gaspi_atomic_fetch_add (0, 0, n, 1, &val, GASPI_BLOCK));
  }

  gaspi_pointer_t _vptr;

  ASSERT (gaspi_segment_ptr (0, &_vptr));

  ASSERT (gaspi_barrier (g, GASPI_BLOCK));

  gaspi_atomic_value_t *end_val;

  end_val = (gaspi_atomic_value_t *) _vptr;

  assert (*end_val == numranks);

  ASSERT (gaspi_barrier (g, GASPI_BLOCK));
  ASSERT (gaspi_proc_term (GASPI_BLOCK));

  return EXIT_SUCCESS;
}

#include <test_utils.h>

/* Test allocates max number of segments defined by th user, registers
   them with all other nodes and then deletes them */
int
main (int argc, char *argv[])
{
  TSUITE_INIT (argc, argv);

  gaspi_config_t default_conf;

  ASSERT (gaspi_config_get (&default_conf));

  gaspi_number_t user_seg_max = 348;
  default_conf.segment_max = user_seg_max;

  EXPECT_FAIL_WITH ( gaspi_config_set (default_conf), GASPI_ERR_CONFIG);

  user_seg_max = 48;
  default_conf.segment_max = user_seg_max;

  ASSERT (gaspi_config_set (default_conf));

  ASSERT (gaspi_proc_init (GASPI_BLOCK));

  gaspi_rank_t rank, nprocs, i;

  ASSERT (gaspi_proc_num (&nprocs));
  ASSERT (gaspi_proc_rank (&rank));

  gaspi_number_t seg_max;

  ASSERT (gaspi_segment_max (&seg_max));

  assert (user_seg_max <= seg_max);

  gaspi_segment_id_t s;

  for (s = 0; s < user_seg_max; s++)
  {
    ASSERT (gaspi_segment_alloc (s, 1024, GASPI_MEM_INITIALIZED));
  }

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  EXPECT_FAIL (gaspi_segment_alloc (s, 1024, GASPI_MEM_INITIALIZED));

  for (i = 0; i < nprocs; i++)
  {
    if (i == rank)
    {
      continue;
    }

    for (s = 0; s < user_seg_max; s++)
    {
      ASSERT (gaspi_segment_register (s, i, GASPI_BLOCK));
    }
  }

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  for (s = 0; s < user_seg_max; s++)
  {
    ASSERT (gaspi_segment_delete (s));
  }

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  ASSERT (gaspi_proc_term (GASPI_BLOCK));

  return EXIT_SUCCESS;
}

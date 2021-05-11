#include <test_utils.h>

int
main (int argc, char *argv[])
{
  gaspi_rank_t rank, nc;

  TSUITE_INIT (argc, argv);

  ASSERT (gaspi_proc_init (GASPI_BLOCK));

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));

  ASSERT (gaspi_proc_rank (&rank));

  ASSERT (gaspi_proc_num (&nc));

  if (rank == 0)
  {
    for (gaspi_rank_t i = 1; i < nc; i++)
    {
      ASSERT (gaspi_proc_kill (i, GASPI_BLOCK));
    }
    EXPECT_FAIL_WITH (gaspi_proc_kill (rank, GASPI_BLOCK),
                      GASPI_ERR_INV_RANK);
  }
  else
  {
    //hang in there
    ASSERT (gaspi_barrier (GASPI_GROUP_ALL, GASPI_BLOCK));
  }

  ASSERT (gaspi_proc_term (GASPI_BLOCK));

  return EXIT_SUCCESS;
}

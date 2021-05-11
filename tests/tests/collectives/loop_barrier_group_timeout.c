#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <test_utils.h>

#define ITERATIONS 1000

int
main (int argc, char *argv[])
{
  gaspi_group_t g;
  gaspi_rank_t nprocs, myrank;
  gaspi_number_t gsize;

  TSUITE_INIT (argc, argv);

  ASSERT (gaspi_proc_init (GASPI_BLOCK));

  ASSERT (gaspi_proc_num (&nprocs));
  ASSERT (gaspi_proc_rank (&myrank));

  ASSERT (gaspi_group_create (&g));
  ASSERT (gaspi_group_size (g, &gsize));
  assert ((gsize == 0));

  gaspi_rank_t i;

  for (i = 0; i < nprocs; i++)
  {
    ASSERT (gaspi_group_add (g, i));
  }

  ASSERT (gaspi_group_size (g, &gsize));
  assert ((gsize == nprocs));

  ASSERT (gaspi_group_commit (g, nprocs * 1000));

  int j;
  for (j = 0; j < ITERATIONS; j++)
  {
    ASSERT (gaspi_barrier (g, nprocs * 1000));
  }

  ASSERT (gaspi_barrier (GASPI_GROUP_ALL, nprocs * 1000));
  ASSERT (gaspi_proc_term (GASPI_BLOCK));

  return EXIT_SUCCESS;
}

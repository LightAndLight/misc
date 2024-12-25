#include "simple.h"
#include "eq.h"
#include "fmap.h"
#include "hof.h"
#include "existential.h"

int main() {
  simple_main();
  eq_main();
  fmap_main();
  hof_main();
  existential_main();
  return 0;
}

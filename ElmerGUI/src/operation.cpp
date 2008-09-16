#include "operation.h"

operation_t::operation_t()
{
  next = 0;
  type = 0;
  angle = 0.0;
  selected = 0;
  select_set = 0;
}

operation_t::~operation_t()
{
}

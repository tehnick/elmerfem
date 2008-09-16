#ifndef OPERATION_H
#define OPERATION_H

enum OpTypes {
  OP_UNIFY_SURFACE,
  OP_DIVIDE_SURFACE,
  OP_UNIFY_EDGE,
  OP_DIVIDE_EDGE
};

class operation_t {
 public:
  operation_t();  
  ~operation_t();
  
  operation_t *next;
  int type;
  double angle;
  int selected;
  int *select_set;
};

#endif // OPERATION_H

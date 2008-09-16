#ifndef OPERATION_H
#define OPERATION_H

#define OP_UNIFY_SURFACE  1
#define OP_DIVIDE_SURFACE 2
#define OP_UNIFY_EDGE     3
#define OP_DIVIDE_EDGE    4

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

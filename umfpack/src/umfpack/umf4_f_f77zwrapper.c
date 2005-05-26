# 1 "umf4_f77zwrapper.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "umf4_f77zwrapper.c"
# 25 "umf4_f77zwrapper.c"
# 1 "include/umfpack.h" 1
# 37 "include/umfpack.h"
# 1 "include/umfpack_symbolic.h" 1
# 11 "include/umfpack_symbolic.h"
int umfpack_di_symbolic
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;

long umfpack_dl_symbolic
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;

int umfpack_zi_symbolic
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;

long umfpack_zl_symbolic
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;
# 38 "include/umfpack.h" 2
# 1 "include/umfpack_numeric.h" 1
# 11 "include/umfpack_numeric.h"
int umfpack_di_numeric
(
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    void *Symbolic,
    void **Numeric,
    const double Control [20],
    double Info [90]
) ;

long umfpack_dl_numeric
(
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    void *Symbolic,
    void **Numeric,
    const double Control [20],
    double Info [90]
) ;

int umfpack_zi_numeric
(
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    void *Symbolic,
    void **Numeric,
    const double Control [20],
    double Info [90]
) ;

long umfpack_zl_numeric
(
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    void *Symbolic,
    void **Numeric,
    const double Control [20],
    double Info [90]
) ;
# 39 "include/umfpack.h" 2
# 1 "include/umfpack_solve.h" 1
# 11 "include/umfpack_solve.h"
int umfpack_di_solve
(
    int sys,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    double X [ ],
    const double B [ ],
    void *Numeric,
    const double Control [20],
    double Info [90]
) ;

long umfpack_dl_solve
(
    long sys,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    double X [ ],
    const double B [ ],
    void *Numeric,
    const double Control [20],
    double Info [90]
) ;

int umfpack_zi_solve
(
    int sys,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    double Xx [ ], double Xz [ ],
    const double Bx [ ], const double Bz [ ],
    void *Numeric,
    const double Control [20],
    double Info [90]
) ;

long umfpack_zl_solve
(
    long sys,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    double Xx [ ], double Xz [ ],
    const double Bx [ ], const double Bz [ ],
    void *Numeric,
    const double Control [20],
    double Info [90]
) ;
# 40 "include/umfpack.h" 2
# 1 "include/umfpack_free_symbolic.h" 1
# 11 "include/umfpack_free_symbolic.h"
void umfpack_di_free_symbolic
(
    void **Symbolic
) ;

void umfpack_dl_free_symbolic
(
    void **Symbolic
) ;

void umfpack_zi_free_symbolic
(
    void **Symbolic
) ;

void umfpack_zl_free_symbolic
(
    void **Symbolic
) ;
# 41 "include/umfpack.h" 2
# 1 "include/umfpack_free_numeric.h" 1
# 11 "include/umfpack_free_numeric.h"
void umfpack_di_free_numeric
(
    void **Numeric
) ;

void umfpack_dl_free_numeric
(
    void **Numeric
) ;

void umfpack_zi_free_numeric
(
    void **Numeric
) ;

void umfpack_zl_free_numeric
(
    void **Numeric
) ;
# 42 "include/umfpack.h" 2


# 1 "include/umfpack_defaults.h" 1
# 11 "include/umfpack_defaults.h"
void umfpack_di_defaults
(
    double Control [20]
) ;

void umfpack_dl_defaults
(
    double Control [20]
) ;

void umfpack_zi_defaults
(
    double Control [20]
) ;

void umfpack_zl_defaults
(
    double Control [20]
) ;
# 45 "include/umfpack.h" 2
# 1 "include/umfpack_qsymbolic.h" 1
# 11 "include/umfpack_qsymbolic.h"
int umfpack_di_qsymbolic
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    const int Qinit [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;

long umfpack_dl_qsymbolic
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    const long Qinit [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;

int umfpack_zi_qsymbolic
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    const int Qinit [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;

long umfpack_zl_qsymbolic
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    const long Qinit [ ],
    void **Symbolic,
    const double Control [20],
    double Info [90]
) ;
# 46 "include/umfpack.h" 2
# 1 "include/umfpack_wsolve.h" 1
# 11 "include/umfpack_wsolve.h"
int umfpack_di_wsolve
(
    int sys,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    double X [ ],
    const double B [ ],
    void *Numeric,
    const double Control [20],
    double Info [90],
    int Wi [ ],
    double W [ ]
) ;

long umfpack_dl_wsolve
(
    long sys,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    double X [ ],
    const double B [ ],
    void *Numeric,
    const double Control [20],
    double Info [90],
    long Wi [ ],
    double W [ ]
) ;

int umfpack_zi_wsolve
(
    int sys,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    double Xx [ ], double Xz [ ],
    const double Bx [ ], const double Bz [ ],
    void *Numeric,
    const double Control [20],
    double Info [90],
    int Wi [ ],
    double W [ ]
) ;

long umfpack_zl_wsolve
(
    long sys,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    double Xx [ ], double Xz [ ],
    const double Bx [ ], const double Bz [ ],
    void *Numeric,
    const double Control [20],
    double Info [90],
    long Wi [ ],
    double W [ ]
) ;
# 47 "include/umfpack.h" 2


# 1 "include/umfpack_triplet_to_col.h" 1
# 11 "include/umfpack_triplet_to_col.h"
int umfpack_di_triplet_to_col
(
    int n_row,
    int n_col,
    int nz,
    const int Ti [ ],
    const int Tj [ ],
    const double Tx [ ],
    int Ap [ ],
    int Ai [ ],
    double Ax [ ],
    int Map [ ]
) ;

long umfpack_dl_triplet_to_col
(
    long n_row,
    long n_col,
    long nz,
    const long Ti [ ],
    const long Tj [ ],
    const double Tx [ ],
    long Ap [ ],
    long Ai [ ],
    double Ax [ ],
    long Map [ ]
) ;

int umfpack_zi_triplet_to_col
(
    int n_row,
    int n_col,
    int nz,
    const int Ti [ ],
    const int Tj [ ],
    const double Tx [ ], const double Tz [ ],
    int Ap [ ],
    int Ai [ ],
    double Ax [ ], double Az [ ],
    int Map [ ]
) ;

long umfpack_zl_triplet_to_col
(
    long n_row,
    long n_col,
    long nz,
    const long Ti [ ],
    const long Tj [ ],
    const double Tx [ ], const double Tz [ ],
    long Ap [ ],
    long Ai [ ],
    double Ax [ ], double Az [ ],
    long Map [ ]
) ;
# 50 "include/umfpack.h" 2
# 1 "include/umfpack_col_to_triplet.h" 1
# 11 "include/umfpack_col_to_triplet.h"
int umfpack_di_col_to_triplet
(
    int n_col,
    const int Ap [ ],
    int Tj [ ]
) ;

long umfpack_dl_col_to_triplet
(
    long n_col,
    const long Ap [ ],
    long Tj [ ]
) ;

int umfpack_zi_col_to_triplet
(
    int n_col,
    const int Ap [ ],
    int Tj [ ]
) ;

long umfpack_zl_col_to_triplet
(
    long n_col,
    const long Ap [ ],
    long Tj [ ]
) ;
# 51 "include/umfpack.h" 2
# 1 "include/umfpack_transpose.h" 1
# 11 "include/umfpack_transpose.h"
int umfpack_di_transpose
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    const int P [ ],
    const int Q [ ],
    int Rp [ ],
    int Ri [ ],
    double Rx [ ]
) ;

long umfpack_dl_transpose
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    const long P [ ],
    const long Q [ ],
    long Rp [ ],
    long Ri [ ],
    double Rx [ ]
) ;

int umfpack_zi_transpose
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    const int P [ ],
    const int Q [ ],
    int Rp [ ],
    int Ri [ ],
    double Rx [ ], double Rz [ ],
    int do_conjugate
) ;

long umfpack_zl_transpose
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    const long P [ ],
    const long Q [ ],
    long Rp [ ],
    long Ri [ ],
    double Rx [ ], double Rz [ ],
    long do_conjugate
) ;
# 52 "include/umfpack.h" 2
# 1 "include/umfpack_scale.h" 1
# 11 "include/umfpack_scale.h"
int umfpack_di_scale
(
    double X [ ],
    const double B [ ],
    void *Numeric
) ;

long umfpack_dl_scale
(
    double X [ ],
    const double B [ ],
    void *Numeric
) ;

int umfpack_zi_scale
(
    double Xx [ ], double Xz [ ],
    const double Bx [ ], const double Bz [ ],
    void *Numeric
) ;

long umfpack_zl_scale
(
    double Xx [ ], double Xz [ ],
    const double Bx [ ], const double Bz [ ],
    void *Numeric
) ;
# 53 "include/umfpack.h" 2


# 1 "include/umfpack_get_lunz.h" 1
# 11 "include/umfpack_get_lunz.h"
int umfpack_di_get_lunz
(
    int *lnz,
    int *unz,
    int *n_row,
    int *n_col,
    int *nz_udiag,
    void *Numeric
) ;

long umfpack_dl_get_lunz
(
    long *lnz,
    long *unz,
    long *n_row,
    long *n_col,
    long *nz_udiag,
    void *Numeric
) ;

int umfpack_zi_get_lunz
(
    int *lnz,
    int *unz,
    int *n_row,
    int *n_col,
    int *nz_udiag,
    void *Numeric
) ;

long umfpack_zl_get_lunz
(
    long *lnz,
    long *unz,
    long *n_row,
    long *n_col,
    long *nz_udiag,
    void *Numeric
) ;
# 56 "include/umfpack.h" 2
# 1 "include/umfpack_get_numeric.h" 1
# 11 "include/umfpack_get_numeric.h"
int umfpack_di_get_numeric
(
    int Lp [ ],
    int Lj [ ],
    double Lx [ ],
    int Up [ ],
    int Ui [ ],
    double Ux [ ],
    int P [ ],
    int Q [ ],
    double Dx [ ],
    int *do_recip,
    double Rs [ ],
    void *Numeric
) ;

long umfpack_dl_get_numeric
(
    long Lp [ ],
    long Lj [ ],
    double Lx [ ],
    long Up [ ],
    long Ui [ ],
    double Ux [ ],
    long P [ ],
    long Q [ ],
    double Dx [ ],
    long *do_recip,
    double Rs [ ],
    void *Numeric
) ;

int umfpack_zi_get_numeric
(
    int Lp [ ],
    int Lj [ ],
    double Lx [ ], double Lz [ ],
    int Up [ ],
    int Ui [ ],
    double Ux [ ], double Uz [ ],
    int P [ ],
    int Q [ ],
    double Dx [ ], double Dz [ ],
    int *do_recip,
    double Rs [ ],
    void *Numeric
) ;

long umfpack_zl_get_numeric
(
    long Lp [ ],
    long Lj [ ],
    double Lx [ ], double Lz [ ],
    long Up [ ],
    long Ui [ ],
    double Ux [ ], double Uz [ ],
    long P [ ],
    long Q [ ],
    double Dx [ ], double Dz [ ],
    long *do_recip,
    double Rs [ ],
    void *Numeric
) ;
# 57 "include/umfpack.h" 2
# 1 "include/umfpack_get_symbolic.h" 1
# 11 "include/umfpack_get_symbolic.h"
int umfpack_di_get_symbolic
(
    int *n_row,
    int *n_col,
    int *n1,
    int *nz,
    int *nfr,
    int *nchains,
    int P [ ],
    int Q [ ],
    int Front_npivcol [ ],
    int Front_parent [ ],
    int Front_1strow [ ],
    int Front_leftmostdesc [ ],
    int Chain_start [ ],
    int Chain_maxrows [ ],
    int Chain_maxcols [ ],
    void *Symbolic
) ;

long umfpack_dl_get_symbolic
(
    long *n_row,
    long *n_col,
    long *n1,
    long *nz,
    long *nfr,
    long *nchains,
    long P [ ],
    long Q [ ],
    long Front_npivcol [ ],
    long Front_parent [ ],
    long Front_1strow [ ],
    long Front_leftmostdesc [ ],
    long Chain_start [ ],
    long Chain_maxrows [ ],
    long Chain_maxcols [ ],
    void *Symbolic
) ;

int umfpack_zi_get_symbolic
(
    int *n_row,
    int *n_col,
    int *n1,
    int *nz,
    int *nfr,
    int *nchains,
    int P [ ],
    int Q [ ],
    int Front_npivcol [ ],
    int Front_parent [ ],
    int Front_1strow [ ],
    int Front_leftmostdesc [ ],
    int Chain_start [ ],
    int Chain_maxrows [ ],
    int Chain_maxcols [ ],
    void *Symbolic
) ;

long umfpack_zl_get_symbolic
(
    long *n_row,
    long *n_col,
    long *n1,
    long *nz,
    long *nfr,
    long *nchains,
    long P [ ],
    long Q [ ],
    long Front_npivcol [ ],
    long Front_parent [ ],
    long Front_1strow [ ],
    long Front_leftmostdesc [ ],
    long Chain_start [ ],
    long Chain_maxrows [ ],
    long Chain_maxcols [ ],
    void *Symbolic
) ;
# 58 "include/umfpack.h" 2
# 1 "include/umfpack_save_numeric.h" 1
# 11 "include/umfpack_save_numeric.h"
int umfpack_di_save_numeric
(
    void *Numeric,
    char *filename
) ;

long umfpack_dl_save_numeric
(
    void *Numeric,
    char *filename
) ;

int umfpack_zi_save_numeric
(
    void *Numeric,
    char *filename
) ;

long umfpack_zl_save_numeric
(
    void *Numeric,
    char *filename
) ;
# 59 "include/umfpack.h" 2
# 1 "include/umfpack_load_numeric.h" 1
# 11 "include/umfpack_load_numeric.h"
int umfpack_di_load_numeric
(
    void **Numeric,
    char *filename
) ;

long umfpack_dl_load_numeric
(
    void **Numeric,
    char *filename
) ;

int umfpack_zi_load_numeric
(
    void **Numeric,
    char *filename
) ;

long umfpack_zl_load_numeric
(
    void **Numeric,
    char *filename
) ;
# 60 "include/umfpack.h" 2
# 1 "include/umfpack_save_symbolic.h" 1
# 11 "include/umfpack_save_symbolic.h"
int umfpack_di_save_symbolic
(
    void *Symbolic,
    char *filename
) ;

long umfpack_dl_save_symbolic
(
    void *Symbolic,
    char *filename
) ;

int umfpack_zi_save_symbolic
(
    void *Symbolic,
    char *filename
) ;

long umfpack_zl_save_symbolic
(
    void *Symbolic,
    char *filename
) ;
# 61 "include/umfpack.h" 2
# 1 "include/umfpack_load_symbolic.h" 1
# 11 "include/umfpack_load_symbolic.h"
int umfpack_di_load_symbolic
(
    void **Symbolic,
    char *filename
) ;

long umfpack_dl_load_symbolic
(
    void **Symbolic,
    char *filename
) ;

int umfpack_zi_load_symbolic
(
    void **Symbolic,
    char *filename
) ;

long umfpack_zl_load_symbolic
(
    void **Symbolic,
    char *filename
) ;
# 62 "include/umfpack.h" 2
# 1 "include/umfpack_get_determinant.h" 1
# 11 "include/umfpack_get_determinant.h"
int umfpack_di_get_determinant
(
    double *Mx,
    double *Ex,
    void *NumericHandle,
    double User_Info [90]
) ;

long umfpack_dl_get_determinant
(
    double *Mx,
    double *Ex,
    void *NumericHandle,
    double User_Info [90]
) ;

int umfpack_zi_get_determinant
(
    double *Mx,
    double *Mz,
    double *Ex,
    void *NumericHandle,
    double User_Info [90]
) ;

long umfpack_zl_get_determinant
(
    double *Mx,
    double *Mz,
    double *Ex,
    void *NumericHandle,
    double User_Info [90]
) ;
# 63 "include/umfpack.h" 2


# 1 "include/umfpack_report_status.h" 1
# 11 "include/umfpack_report_status.h"
void umfpack_di_report_status
(
    const double Control [20],
    int status
) ;

void umfpack_dl_report_status
(
    const double Control [20],
    long status
) ;

void umfpack_zi_report_status
(
    const double Control [20],
    int status
) ;

void umfpack_zl_report_status
(
    const double Control [20],
    long status
) ;
# 66 "include/umfpack.h" 2
# 1 "include/umfpack_report_info.h" 1
# 11 "include/umfpack_report_info.h"
void umfpack_di_report_info
(
    const double Control [20],
    const double Info [90]
) ;

void umfpack_dl_report_info
(
    const double Control [20],
    const double Info [90]
) ;

void umfpack_zi_report_info
(
    const double Control [20],
    const double Info [90]
) ;

void umfpack_zl_report_info
(
    const double Control [20],
    const double Info [90]
) ;
# 67 "include/umfpack.h" 2
# 1 "include/umfpack_report_control.h" 1
# 11 "include/umfpack_report_control.h"
void umfpack_di_report_control
(
    const double Control [20]
) ;

void umfpack_dl_report_control
(
    const double Control [20]
) ;

void umfpack_zi_report_control
(
    const double Control [20]
) ;

void umfpack_zl_report_control
(
    const double Control [20]
) ;
# 68 "include/umfpack.h" 2
# 1 "include/umfpack_report_matrix.h" 1
# 11 "include/umfpack_report_matrix.h"
int umfpack_di_report_matrix
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ],
    int col_form,
    const double Control [20]
) ;

long umfpack_dl_report_matrix
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ],
    long col_form,
    const double Control [20]
) ;

int umfpack_zi_report_matrix
(
    int n_row,
    int n_col,
    const int Ap [ ],
    const int Ai [ ],
    const double Ax [ ], const double Az [ ],
    int col_form,
    const double Control [20]
) ;

long umfpack_zl_report_matrix
(
    long n_row,
    long n_col,
    const long Ap [ ],
    const long Ai [ ],
    const double Ax [ ], const double Az [ ],
    long col_form,
    const double Control [20]
) ;
# 69 "include/umfpack.h" 2
# 1 "include/umfpack_report_triplet.h" 1
# 11 "include/umfpack_report_triplet.h"
int umfpack_di_report_triplet
(
    int n_row,
    int n_col,
    int nz,
    const int Ti [ ],
    const int Tj [ ],
    const double Tx [ ],
    const double Control [20]
) ;

long umfpack_dl_report_triplet
(
    long n_row,
    long n_col,
    long nz,
    const long Ti [ ],
    const long Tj [ ],
    const double Tx [ ],
    const double Control [20]
) ;

int umfpack_zi_report_triplet
(
    int n_row,
    int n_col,
    int nz,
    const int Ti [ ],
    const int Tj [ ],
    const double Tx [ ], const double Tz [ ],
    const double Control [20]
) ;

long umfpack_zl_report_triplet
(
    long n_row,
    long n_col,
    long nz,
    const long Ti [ ],
    const long Tj [ ],
    const double Tx [ ], const double Tz [ ],
    const double Control [20]
) ;
# 70 "include/umfpack.h" 2
# 1 "include/umfpack_report_vector.h" 1
# 11 "include/umfpack_report_vector.h"
int umfpack_di_report_vector
(
    int n,
    const double X [ ],
    const double Control [20]
) ;

long umfpack_dl_report_vector
(
    long n,
    const double X [ ],
    const double Control [20]
) ;

int umfpack_zi_report_vector
(
    int n,
    const double Xx [ ], const double Xz [ ],
    const double Control [20]
) ;

long umfpack_zl_report_vector
(
    long n,
    const double Xx [ ], const double Xz [ ],
    const double Control [20]
) ;
# 71 "include/umfpack.h" 2
# 1 "include/umfpack_report_symbolic.h" 1
# 11 "include/umfpack_report_symbolic.h"
int umfpack_di_report_symbolic
(
    void *Symbolic,
    const double Control [20]
) ;

long umfpack_dl_report_symbolic
(
    void *Symbolic,
    const double Control [20]
) ;

int umfpack_zi_report_symbolic
(
    void *Symbolic,
    const double Control [20]
) ;

long umfpack_zl_report_symbolic
(
    void *Symbolic,
    const double Control [20]
) ;
# 72 "include/umfpack.h" 2
# 1 "include/umfpack_report_numeric.h" 1
# 11 "include/umfpack_report_numeric.h"
int umfpack_di_report_numeric
(
    void *Numeric,
    const double Control [20]
) ;

long umfpack_dl_report_numeric
(
    void *Numeric,
    const double Control [20]
) ;

int umfpack_zi_report_numeric
(
    void *Numeric,
    const double Control [20]
) ;

long umfpack_zl_report_numeric
(
    void *Numeric,
    const double Control [20]
) ;
# 73 "include/umfpack.h" 2
# 1 "include/umfpack_report_perm.h" 1
# 11 "include/umfpack_report_perm.h"
int umfpack_di_report_perm
(
    int np,
    const int Perm [ ],
    const double Control [20]
) ;

long umfpack_dl_report_perm
(
    long np,
    const long Perm [ ],
    const double Control [20]
) ;

int umfpack_zi_report_perm
(
    int np,
    const int Perm [ ],
    const double Control [20]
) ;

long umfpack_zl_report_perm
(
    long np,
    const long Perm [ ],
    const double Control [20]
) ;
# 74 "include/umfpack.h" 2


# 1 "include/umfpack_timer.h" 1
# 11 "include/umfpack_timer.h"
double umfpack_timer ( void ) ;
# 77 "include/umfpack.h" 2
# 1 "include/umfpack_tictoc.h" 1
# 11 "include/umfpack_tictoc.h"
void umfpack_tic (double stats [2]) ;

void umfpack_toc (double stats [2]) ;
# 78 "include/umfpack.h" 2
# 26 "umf4_f77zwrapper.c" 2
# 1 "/usr/include/ctype.h" 1 3
# 27 "/usr/include/ctype.h" 3
# 1 "/usr/include/features.h" 1 3
# 291 "/usr/include/features.h" 3
# 1 "/usr/include/sys/cdefs.h" 1 3
# 292 "/usr/include/features.h" 2 3
# 314 "/usr/include/features.h" 3
# 1 "/usr/include/gnu/stubs.h" 1 3
# 315 "/usr/include/features.h" 2 3
# 28 "/usr/include/ctype.h" 2 3
# 1 "/usr/include/bits/types.h" 1 3
# 28 "/usr/include/bits/types.h" 3
# 1 "/usr/include/bits/wordsize.h" 1 3
# 29 "/usr/include/bits/types.h" 2 3


# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 1 3
# 213 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 3
typedef unsigned int size_t;
# 32 "/usr/include/bits/types.h" 2 3


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;




__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;





__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
# 128 "/usr/include/bits/types.h" 3
# 1 "/usr/include/bits/typesizes.h" 1 3
# 129 "/usr/include/bits/types.h" 2 3


typedef unsigned long long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned int __nlink_t;
typedef long int __off_t;
typedef long long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef long int __swblk_t;
typedef int __key_t;


typedef int __clockid_t;


typedef int __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long long int __fsfilcnt64_t;




typedef int __ssize_t;
typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


typedef int __intptr_t;


typedef unsigned int __socklen_t;
# 29 "/usr/include/ctype.h" 2 3


# 41 "/usr/include/ctype.h" 3
# 1 "/usr/include/endian.h" 1 3
# 37 "/usr/include/endian.h" 3
# 1 "/usr/include/bits/endian.h" 1 3
# 38 "/usr/include/endian.h" 2 3
# 42 "/usr/include/ctype.h" 2 3






enum
{
  _ISupper = ((0) < 8 ? ((1 << (0)) << 8) : ((1 << (0)) >> 8)),
  _ISlower = ((1) < 8 ? ((1 << (1)) << 8) : ((1 << (1)) >> 8)),
  _ISalpha = ((2) < 8 ? ((1 << (2)) << 8) : ((1 << (2)) >> 8)),
  _ISdigit = ((3) < 8 ? ((1 << (3)) << 8) : ((1 << (3)) >> 8)),
  _ISxdigit = ((4) < 8 ? ((1 << (4)) << 8) : ((1 << (4)) >> 8)),
  _ISspace = ((5) < 8 ? ((1 << (5)) << 8) : ((1 << (5)) >> 8)),
  _ISprint = ((6) < 8 ? ((1 << (6)) << 8) : ((1 << (6)) >> 8)),
  _ISgraph = ((7) < 8 ? ((1 << (7)) << 8) : ((1 << (7)) >> 8)),
  _ISblank = ((8) < 8 ? ((1 << (8)) << 8) : ((1 << (8)) >> 8)),
  _IScntrl = ((9) < 8 ? ((1 << (9)) << 8) : ((1 << (9)) >> 8)),
  _ISpunct = ((10) < 8 ? ((1 << (10)) << 8) : ((1 << (10)) >> 8)),
  _ISalnum = ((11) < 8 ? ((1 << (11)) << 8) : ((1 << (11)) >> 8))
};
# 81 "/usr/include/ctype.h" 3
extern __const unsigned short int **__ctype_b_loc (void)
     __attribute__ ((__const));
extern __const __int32_t **__ctype_tolower_loc (void)
     __attribute__ ((__const));
extern __const __int32_t **__ctype_toupper_loc (void)
     __attribute__ ((__const));
# 96 "/usr/include/ctype.h" 3






extern int isalnum (int) ;
extern int isalpha (int) ;
extern int iscntrl (int) ;
extern int isdigit (int) ;
extern int islower (int) ;
extern int isgraph (int) ;
extern int isprint (int) ;
extern int ispunct (int) ;
extern int isspace (int) ;
extern int isupper (int) ;
extern int isxdigit (int) ;



extern int tolower (int __c) ;


extern int toupper (int __c) ;


# 142 "/usr/include/ctype.h" 3
extern int isascii (int __c) ;



extern int toascii (int __c) ;



extern int _toupper (int) ;
extern int _tolower (int) ;
# 323 "/usr/include/ctype.h" 3

# 27 "umf4_f77zwrapper.c" 2
# 1 "/usr/include/stdio.h" 1 3
# 30 "/usr/include/stdio.h" 3




# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 1 3
# 35 "/usr/include/stdio.h" 2 3
# 44 "/usr/include/stdio.h" 3


typedef struct _IO_FILE FILE;





# 62 "/usr/include/stdio.h" 3
typedef struct _IO_FILE __FILE;
# 72 "/usr/include/stdio.h" 3
# 1 "/usr/include/libio.h" 1 3
# 32 "/usr/include/libio.h" 3
# 1 "/usr/include/_G_config.h" 1 3
# 14 "/usr/include/_G_config.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 1 3
# 325 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 3
typedef long int wchar_t;
# 354 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 3
typedef unsigned int wint_t;
# 15 "/usr/include/_G_config.h" 2 3
# 24 "/usr/include/_G_config.h" 3
# 1 "/usr/include/wchar.h" 1 3
# 48 "/usr/include/wchar.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 1 3
# 49 "/usr/include/wchar.h" 2 3

# 1 "/usr/include/bits/wchar.h" 1 3
# 51 "/usr/include/wchar.h" 2 3
# 71 "/usr/include/wchar.h" 3
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 25 "/usr/include/_G_config.h" 2 3

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 44 "/usr/include/_G_config.h" 3
# 1 "/usr/include/gconv.h" 1 3
# 28 "/usr/include/gconv.h" 3
# 1 "/usr/include/wchar.h" 1 3
# 48 "/usr/include/wchar.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 1 3
# 49 "/usr/include/wchar.h" 2 3
# 29 "/usr/include/gconv.h" 2 3


# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stddef.h" 1 3
# 32 "/usr/include/gconv.h" 2 3





enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,

  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,

  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};



enum
{
  __GCONV_IS_LAST = 0x0001,
  __GCONV_IGNORE_ERRORS = 0x0002
};



struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;



typedef int (*__gconv_fct) (struct __gconv_step *, struct __gconv_step_data *,
                            __const unsigned char **, __const unsigned char *,
                            unsigned char **, size_t *, int, int);


typedef wint_t (*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);


typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);



typedef int (*__gconv_trans_fct) (struct __gconv_step *,
                                  struct __gconv_step_data *, void *,
                                  __const unsigned char *,
                                  __const unsigned char **,
                                  __const unsigned char *, unsigned char **,
                                  size_t *);


typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
                                          __const unsigned char *,
                                          unsigned char *, unsigned char *);


typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
                                        size_t *);


typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);

struct __gconv_trans_data
{

  __gconv_trans_fct __trans_fct;
  __gconv_trans_context_fct __trans_context_fct;
  __gconv_trans_end_fct __trans_end_fct;
  void *__data;
  struct __gconv_trans_data *__next;
};



struct __gconv_step
{
  struct __gconv_loaded_object *__shlib_handle;
  __const char *__modname;

  int __counter;

  char *__from_name;
  char *__to_name;

  __gconv_fct __fct;
  __gconv_btowc_fct __btowc_fct;
  __gconv_init_fct __init_fct;
  __gconv_end_fct __end_fct;



  int __min_needed_from;
  int __max_needed_from;
  int __min_needed_to;
  int __max_needed_to;


  int __stateful;

  void *__data;
};



struct __gconv_step_data
{
  unsigned char *__outbuf;
  unsigned char *__outbufend;



  int __flags;



  int __invocation_counter;



  int __internal_use;

  __mbstate_t *__statep;
  __mbstate_t __state;



  struct __gconv_trans_data *__trans;
};



typedef struct __gconv_info
{
  size_t __nsteps;
  struct __gconv_step *__steps;
  __extension__ struct __gconv_step_data __data [];
} *__gconv_t;
# 45 "/usr/include/_G_config.h" 2 3
typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
# 33 "/usr/include/libio.h" 2 3
# 53 "/usr/include/libio.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stdarg.h" 1 3
# 43 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stdarg.h" 3
typedef __builtin_va_list __gnuc_va_list;
# 54 "/usr/include/libio.h" 2 3
# 162 "/usr/include/libio.h" 3
struct _IO_jump_t; struct _IO_FILE;
# 172 "/usr/include/libio.h" 3
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 195 "/usr/include/libio.h" 3
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 263 "/usr/include/libio.h" 3
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 311 "/usr/include/libio.h" 3
  __off64_t _offset;





  void *__pad1;
  void *__pad2;

  int _mode;

  char _unused2[15 * sizeof (int) - 2 * sizeof (void *)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 350 "/usr/include/libio.h" 3
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
                                 size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);
# 402 "/usr/include/libio.h" 3
extern int __underflow (_IO_FILE *) ;
extern int __uflow (_IO_FILE *) ;
extern int __overflow (_IO_FILE *, int) ;
extern wint_t __wunderflow (_IO_FILE *) ;
extern wint_t __wuflow (_IO_FILE *) ;
extern wint_t __woverflow (_IO_FILE *, wint_t) ;
# 432 "/usr/include/libio.h" 3
extern int _IO_getc (_IO_FILE *__fp) ;
extern int _IO_putc (int __c, _IO_FILE *__fp) ;
extern int _IO_feof (_IO_FILE *__fp) ;
extern int _IO_ferror (_IO_FILE *__fp) ;

extern int _IO_peekc_locked (_IO_FILE *__fp) ;





extern void _IO_flockfile (_IO_FILE *) ;
extern void _IO_funlockfile (_IO_FILE *) ;
extern int _IO_ftrylockfile (_IO_FILE *) ;
# 462 "/usr/include/libio.h" 3
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
                        __gnuc_va_list, int *__restrict) ;
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
                         __gnuc_va_list) ;
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) ;
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) ;

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) ;
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) ;

extern void _IO_free_backup_area (_IO_FILE *) ;
# 73 "/usr/include/stdio.h" 2 3
# 86 "/usr/include/stdio.h" 3


typedef _G_fpos_t fpos_t;




# 138 "/usr/include/stdio.h" 3
# 1 "/usr/include/bits/stdio_lim.h" 1 3
# 139 "/usr/include/stdio.h" 2 3



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;









extern int remove (__const char *__filename) ;

extern int rename (__const char *__old, __const char *__new) ;






extern FILE *tmpfile (void) ;
# 173 "/usr/include/stdio.h" 3
extern char *tmpnam (char *__s) ;

# 183 "/usr/include/stdio.h" 3
extern char *tmpnam_r (char *__s) ;
# 195 "/usr/include/stdio.h" 3
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__malloc__));





extern int fclose (FILE *__stream) ;

extern int fflush (FILE *__stream) ;




extern int fflush_unlocked (FILE *__stream) ;
# 218 "/usr/include/stdio.h" 3



extern FILE *fopen (__const char *__restrict __filename,
                    __const char *__restrict __modes) ;

extern FILE *freopen (__const char *__restrict __filename,
                      __const char *__restrict __modes,
                      FILE *__restrict __stream) ;
# 241 "/usr/include/stdio.h" 3

# 252 "/usr/include/stdio.h" 3
extern FILE *fdopen (int __fd, __const char *__modes) ;
# 273 "/usr/include/stdio.h" 3



extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) ;



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
                    int __modes, size_t __n) ;





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
                       size_t __size) ;


extern void setlinebuf (FILE *__stream) ;





extern int fprintf (FILE *__restrict __stream,
                    __const char *__restrict __format, ...) ;

extern int printf (__const char *__restrict __format, ...) ;

extern int sprintf (char *__restrict __s,
                    __const char *__restrict __format, ...) ;


extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
                     __gnuc_va_list __arg) ;

extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg)
     ;

extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
                     __gnuc_va_list __arg) ;





extern int snprintf (char *__restrict __s, size_t __maxlen,
                     __const char *__restrict __format, ...)
     __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
                      __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 3, 0)));

# 351 "/usr/include/stdio.h" 3


extern int fscanf (FILE *__restrict __stream,
                   __const char *__restrict __format, ...) ;

extern int scanf (__const char *__restrict __format, ...) ;

extern int sscanf (__const char *__restrict __s,
                   __const char *__restrict __format, ...) ;

# 381 "/usr/include/stdio.h" 3


extern int fgetc (FILE *__stream) ;
extern int getc (FILE *__stream) ;


extern int getchar (void) ;








extern int getc_unlocked (FILE *__stream) ;
extern int getchar_unlocked (void) ;




extern int fgetc_unlocked (FILE *__stream) ;





extern int fputc (int __c, FILE *__stream) ;
extern int putc (int __c, FILE *__stream) ;


extern int putchar (int __c) ;








extern int fputc_unlocked (int __c, FILE *__stream) ;




extern int putc_unlocked (int __c, FILE *__stream) ;
extern int putchar_unlocked (int __c) ;





extern int getw (FILE *__stream) ;


extern int putw (int __w, FILE *__stream) ;





extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     ;



extern char *gets (char *__s) ;

# 477 "/usr/include/stdio.h" 3


extern int fputs (__const char *__restrict __s, FILE *__restrict __stream)
     ;


extern int puts (__const char *__s) ;



extern int ungetc (int __c, FILE *__stream) ;



extern size_t fread (void *__restrict __ptr, size_t __size,
                     size_t __n, FILE *__restrict __stream) ;

extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
                      size_t __n, FILE *__restrict __s) ;

# 506 "/usr/include/stdio.h" 3
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                              size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
                               size_t __n, FILE *__restrict __stream) ;





extern int fseek (FILE *__stream, long int __off, int __whence) ;

extern long int ftell (FILE *__stream) ;

extern void rewind (FILE *__stream) ;

# 546 "/usr/include/stdio.h" 3



extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos)
     ;

extern int fsetpos (FILE *__stream, __const fpos_t *__pos) ;
# 565 "/usr/include/stdio.h" 3

# 575 "/usr/include/stdio.h" 3


extern void clearerr (FILE *__stream) ;

extern int feof (FILE *__stream) ;

extern int ferror (FILE *__stream) ;




extern void clearerr_unlocked (FILE *__stream) ;
extern int feof_unlocked (FILE *__stream) ;
extern int ferror_unlocked (FILE *__stream) ;





extern void perror (__const char *__s) ;






# 1 "/usr/include/bits/sys_errlist.h" 1 3
# 27 "/usr/include/bits/sys_errlist.h" 3
extern int sys_nerr;
extern __const char *__const sys_errlist[];
# 602 "/usr/include/stdio.h" 2 3




extern int fileno (FILE *__stream) ;




extern int fileno_unlocked (FILE *__stream) ;






extern FILE *popen (__const char *__command, __const char *__modes) ;


extern int pclose (FILE *__stream) ;





extern char *ctermid (char *__s) ;
# 655 "/usr/include/stdio.h" 3
extern void flockfile (FILE *__stream) ;



extern int ftrylockfile (FILE *__stream) ;


extern void funlockfile (FILE *__stream) ;
# 679 "/usr/include/stdio.h" 3

# 28 "umf4_f77zwrapper.c" 2
# 78 "umf4_f77zwrapper.c"
static void make_filename (int filenum, char *prefix, char *filename)
{
    char *psrc, *pdst ;



    sprintf (filename, "%s%d.umf", prefix, filenum) ;


    pdst = filename ;
    for (psrc = filename ; *psrc ; psrc++)
    {
        if (!((*__ctype_b_loc ())[(int) ((*psrc))] & (unsigned short int) _ISspace)) *pdst++ = *psrc ;
    }
    *pdst = '\0' ;
}
# 107 "umf4_f77zwrapper.c"
void umf4zdef_ (double Control [20])
{
    umfpack_zi_defaults (Control) ;
}







void umf4zpcon_ (double Control [20])
{
    fflush (stdout) ;
    umfpack_zi_report_control (Control) ;
    fflush (stdout) ;
}







void umf4zsym_ (int *m, int *n, int Ap [ ], int Ai [ ],
    double Ax [ ], double Az [ ], void **Symbolic,
    double Control [20], double Info [90])
{
    (void) umfpack_zi_symbolic (*m, *n, Ap, Ai, Ax, Az, Symbolic, Control, Info) ;
}







void umf4znum_ (int Ap [ ], int Ai [ ], double Ax [ ], double Az [ ],
    void **Symbolic, void **Numeric,
    double Control [20], double Info [90])
{
    (void) umfpack_zi_numeric (Ap, Ai, Ax, Az, *Symbolic, Numeric, Control, Info);
}







void umf4zsolr_ (int *sys, int Ap [ ], int Ai [ ], double Ax [ ], double Az [ ],
    double x [ ], double xz [ ], double b [ ], double bz [ ], void **Numeric,
    double Control [20], double Info [90])
{
    (void) umfpack_zi_solve (*sys, Ap, Ai, Ax, Az, x, xz, b, bz,
        *Numeric, Control, Info) ;
}







void umf4zsol_ (int *sys, double x [ ], double xz [ ], double b [ ],
    double bz [ ], void **Numeric,
    double Control [20], double Info [90])
{
    Control [7] = 0 ;
    (void) umfpack_zi_solve (*sys, (int *) 0, (int *) 0, (double *) 0,
        (double *) 0, x, xz, b, bz, *Numeric, Control, Info) ;
}







void umf4zscal_ (double x [ ], double xz [ ], double b [ ], double bz [ ],
    void **Numeric, int *status)
{
    *status = umfpack_zi_scale (x, xz, b, bz, *Numeric) ;
}







void umf4zpinf_ (double Control [20], double Info [90])
{
    fflush (stdout) ;
    umfpack_zi_report_info (Control, Info) ;
    fflush (stdout) ;
}







void umf4zfnum_ (void **Numeric)
{
    umfpack_zi_free_numeric (Numeric) ;
}







void umf4zfsym_ (void **Symbolic)
{
    umfpack_zi_free_symbolic (Symbolic) ;
}







void umf4zsnum_ (void **Numeric, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "n", filename) ;
    *status = umfpack_zi_save_numeric (*Numeric, filename) ;
}







void umf4zssym_ (void **Symbolic, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "s", filename) ;
    *status = umfpack_zi_save_symbolic (*Symbolic, filename) ;
}







void umf4zlnum_ (void **Numeric, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "n", filename) ;
    *status = umfpack_zi_load_numeric (Numeric, filename) ;
}







void umf4zlsym_ (void **Symbolic, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "s", filename) ;
    *status = umfpack_zi_load_symbolic (Symbolic, filename) ;
}
# 291 "umf4_f77zwrapper.c"
void umf4zdef (double Control [20])
{
    umfpack_zi_defaults (Control) ;
}







void umf4zpcon (double Control [20])
{
    fflush (stdout) ;
    umfpack_zi_report_control (Control) ;
    fflush (stdout) ;
}







void umf4zsym (int *m, int *n, int Ap [ ], int Ai [ ],
    double Ax [ ], double Az [ ], void **Symbolic,
    double Control [20], double Info [90])
{
    (void) umfpack_zi_symbolic (*m, *n, Ap, Ai, Ax, Az, Symbolic, Control, Info) ;
}







void umf4znum (int Ap [ ], int Ai [ ], double Ax [ ], double Az [ ],
    void **Symbolic, void **Numeric,
    double Control [20], double Info [90])
{
    (void) umfpack_zi_numeric (Ap, Ai, Ax, Az, *Symbolic, Numeric, Control, Info);
}







void umf4zsolr (int *sys, int Ap [ ], int Ai [ ], double Ax [ ], double Az [ ],
    double x [ ], double xz [ ], double b [ ], double bz [ ], void **Numeric,
    double Control [20], double Info [90])
{
    (void) umfpack_zi_solve (*sys, Ap, Ai, Ax, Az, x, xz, b, bz,
        *Numeric, Control, Info) ;
}







void umf4zsol (int *sys, double x [ ], double xz [ ], double b [ ],
    double bz [ ], void **Numeric,
    double Control [20], double Info [90])
{
    Control [7] = 0 ;
    (void) umfpack_zi_solve (*sys, (int *) 0, (int *) 0, (double *) 0,
        (double *) 0, x, xz, b, bz, *Numeric, Control, Info) ;
}







void umf4zscal (double x [ ], double xz [ ], double b [ ], double bz [ ],
    void **Numeric, int *status)
{
    *status = umfpack_zi_scale (x, xz, b, bz, *Numeric) ;
}







void umf4zpinf (double Control [20], double Info [90])
{
    fflush (stdout) ;
    umfpack_zi_report_info (Control, Info) ;
    fflush (stdout) ;
}







void umf4zfnum (void **Numeric)
{
    umfpack_zi_free_numeric (Numeric) ;
}







void umf4zfsym (void **Symbolic)
{
    umfpack_zi_free_symbolic (Symbolic) ;
}







void umf4zsnum (void **Numeric, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "n", filename) ;
    *status = umfpack_zi_save_numeric (*Numeric, filename) ;
}







void umf4zssym (void **Symbolic, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "s", filename) ;
    *status = umfpack_zi_save_symbolic (*Symbolic, filename) ;
}







void umf4zlnum (void **Numeric, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "n", filename) ;
    *status = umfpack_zi_load_numeric (Numeric, filename) ;
}







void umf4zlsym (void **Symbolic, int *filenum, int *status)
{
    char filename [200] ;
    make_filename (*filenum, "s", filename) ;
    *status = umfpack_zi_load_symbolic (Symbolic, filename) ;
}

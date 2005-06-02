# 1 "huti_sfe_c.src"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "huti_sfe_c.src"
# 10 "huti_sfe_c.src"
# 1 "huti_intdefs.h" 1
# 11 "huti_sfe_c.src" 2
# 1 "/usr/include/stdlib.h" 1 3 4
# 25 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 295 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 296 "/usr/include/features.h" 2 3 4
# 318 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4
# 319 "/usr/include/features.h" 2 3 4
# 26 "/usr/include/stdlib.h" 2 3 4







# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 213 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 3 4
typedef unsigned int size_t;
# 325 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 3 4
typedef long int wchar_t;
# 34 "/usr/include/stdlib.h" 2 3 4


# 93 "/usr/include/stdlib.h" 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;



# 137 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) ;




extern double atof (const char *__nptr) ;

extern int atoi (const char *__nptr) ;

extern long int atol (const char *__nptr) ;

# 157 "/usr/include/stdlib.h" 3 4


extern double strtod (const char * __nptr,
                      char ** __endptr) ;

# 174 "/usr/include/stdlib.h" 3 4


extern long int strtol (const char * __nptr,
                        char ** __endptr, int __base) ;

extern unsigned long int strtoul (const char * __nptr,
                                  char ** __endptr, int __base)
     ;

# 264 "/usr/include/stdlib.h" 3 4
extern double __strtod_internal (const char * __nptr,
                                 char ** __endptr, int __group)
     ;
extern float __strtof_internal (const char * __nptr,
                                char ** __endptr, int __group)
     ;
extern long double __strtold_internal (const char * __nptr,
                                       char ** __endptr,
                                       int __group) ;

extern long int __strtol_internal (const char * __nptr,
                                   char ** __endptr,
                                   int __base, int __group) ;



extern unsigned long int __strtoul_internal (const char * __nptr,
                                             char ** __endptr,
                                             int __base, int __group) ;
# 408 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) ;


extern long int a64l (const char *__s) ;




# 1 "/usr/include/sys/types.h" 1 3 4
# 29 "/usr/include/sys/types.h" 3 4


# 1 "/usr/include/bits/types.h" 1 3 4
# 28 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 29 "/usr/include/bits/types.h" 2 3 4


# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 32 "/usr/include/bits/types.h" 2 3 4


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
# 62 "/usr/include/bits/types.h" 3 4
typedef struct
{
  long __val[2];
} __quad_t;
typedef struct
{
  __u_long __val[2];
} __u_quad_t;
# 129 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 130 "/usr/include/bits/types.h" 2 3 4






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
# 32 "/usr/include/sys/types.h" 2 3 4



typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;




typedef __loff_t loff_t;



typedef __ino_t ino_t;
# 62 "/usr/include/sys/types.h" 3 4
typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;





typedef __off_t off_t;
# 100 "/usr/include/sys/types.h" 3 4
typedef __pid_t pid_t;




typedef __id_t id_t;




typedef __ssize_t ssize_t;





typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;
# 133 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/time.h" 1 3 4
# 74 "/usr/include/time.h" 3 4


typedef __time_t time_t;



# 92 "/usr/include/time.h" 3 4
typedef __clockid_t clockid_t;
# 104 "/usr/include/time.h" 3 4
typedef __timer_t timer_t;
# 134 "/usr/include/sys/types.h" 2 3 4
# 147 "/usr/include/sys/types.h" 3 4
# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 148 "/usr/include/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
# 163 "/usr/include/sys/types.h" 3 4
typedef char int8_t;
typedef short int int16_t;
typedef int int32_t;






typedef unsigned char u_int8_t;
typedef unsigned short int u_int16_t;
typedef unsigned int u_int32_t;




typedef int register_t;
# 213 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/endian.h" 1 3 4
# 38 "/usr/include/endian.h" 2 3 4
# 214 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/select.h" 1 3 4
# 31 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 32 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/sigset.h" 1 3 4
# 23 "/usr/include/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 35 "/usr/include/sys/select.h" 2 3 4



typedef __sigset_t sigset_t;





# 1 "/usr/include/time.h" 1 3 4
# 118 "/usr/include/time.h" 3 4
struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };
# 45 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/bits/time.h" 1 3 4
# 69 "/usr/include/bits/time.h" 3 4
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
# 47 "/usr/include/sys/select.h" 2 3 4


typedef __suseconds_t suseconds_t;





typedef long int __fd_mask;
# 67 "/usr/include/sys/select.h" 3 4
typedef struct
  {






    __fd_mask __fds_bits[1024 / (8 * sizeof (__fd_mask))];


  } fd_set;






typedef __fd_mask fd_mask;
# 99 "/usr/include/sys/select.h" 3 4

# 109 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set * __readfds,
                   fd_set * __writefds,
                   fd_set * __exceptfds,
                   struct timeval * __timeout);
# 128 "/usr/include/sys/select.h" 3 4

# 217 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/sysmacros.h" 1 3 4
# 220 "/usr/include/sys/types.h" 2 3 4
# 231 "/usr/include/sys/types.h" 3 4
typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 266 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/bits/pthreadtypes.h" 1 3 4
# 23 "/usr/include/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/bits/sched.h" 1 3 4
# 83 "/usr/include/bits/sched.h" 3 4
struct __sched_param
  {
    int __sched_priority;
  };
# 24 "/usr/include/bits/pthreadtypes.h" 2 3 4

typedef int __atomic_lock_t;


struct _pthread_fastlock
{
  long int __status;
  __atomic_lock_t __spinlock;

};



typedef struct _pthread_descr_struct *_pthread_descr;





typedef struct __pthread_attr_s
{
  int __detachstate;
  int __schedpolicy;
  struct __sched_param __schedparam;
  int __inheritsched;
  int __scope;
  size_t __guardsize;
  int __stackaddr_set;
  void *__stackaddr;
  size_t __stacksize;
} pthread_attr_t;







typedef long __pthread_cond_align_t;


typedef struct
{
  struct _pthread_fastlock __c_lock;
  _pthread_descr __c_waiting;
  char __padding[48 - sizeof (struct _pthread_fastlock)
                 - sizeof (_pthread_descr) - sizeof (__pthread_cond_align_t)];
  __pthread_cond_align_t __align;
} pthread_cond_t;



typedef struct
{
  int __dummy;
} pthread_condattr_t;


typedef unsigned int pthread_key_t;





typedef struct
{
  int __m_reserved;
  int __m_count;
  _pthread_descr __m_owner;
  int __m_kind;
  struct _pthread_fastlock __m_lock;
} pthread_mutex_t;



typedef struct
{
  int __mutexkind;
} pthread_mutexattr_t;



typedef int pthread_once_t;
# 152 "/usr/include/bits/pthreadtypes.h" 3 4
typedef unsigned long int pthread_t;
# 267 "/usr/include/sys/types.h" 2 3 4



# 417 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) ;


extern void srandom (unsigned int __seed) ;





extern char *initstate (unsigned int __seed, char *__statebuf,
                        size_t __statelen) ;



extern char *setstate (char *__statebuf) ;







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data * __buf,
                     int32_t * __result) ;

extern int srandom_r (unsigned int __seed, struct random_data *__buf) ;

extern int initstate_r (unsigned int __seed, char * __statebuf,
                        size_t __statelen,
                        struct random_data * __buf) ;

extern int setstate_r (char * __statebuf,
                       struct random_data * __buf) ;






extern int rand (void) ;

extern void srand (unsigned int __seed) ;




extern int rand_r (unsigned int *__seed) ;







extern double drand48 (void) ;
extern double erand48 (unsigned short int __xsubi[3]) ;


extern long int lrand48 (void) ;
extern long int nrand48 (unsigned short int __xsubi[3]) ;


extern long int mrand48 (void) ;
extern long int jrand48 (unsigned short int __xsubi[3]) ;


extern void srand48 (long int __seedval) ;
extern unsigned short int *seed48 (unsigned short int __seed16v[3]) ;
extern void lcong48 (unsigned short int __param[7]) ;





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
  };


extern int drand48_r (struct drand48_data * __buffer,
                      double * __result) ;
extern int erand48_r (unsigned short int __xsubi[3],
                      struct drand48_data * __buffer,
                      double * __result) ;


extern int lrand48_r (struct drand48_data * __buffer,
                      long int * __result) ;
extern int nrand48_r (unsigned short int __xsubi[3],
                      struct drand48_data * __buffer,
                      long int * __result) ;


extern int mrand48_r (struct drand48_data * __buffer,
                      long int * __result) ;
extern int jrand48_r (unsigned short int __xsubi[3],
                      struct drand48_data * __buffer,
                      long int * __result) ;


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     ;

extern int seed48_r (unsigned short int __seed16v[3],
                     struct drand48_data *__buffer) ;

extern int lcong48_r (unsigned short int __param[7],
                      struct drand48_data *__buffer) ;









extern void *malloc (size_t __size) ;

extern void *calloc (size_t __nmemb, size_t __size)
     ;







extern void *realloc (void *__ptr, size_t __size) ;

extern void free (void *__ptr) ;




extern void cfree (void *__ptr) ;



# 1 "/usr/include/alloca.h" 1 3 4
# 25 "/usr/include/alloca.h" 3 4
# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 26 "/usr/include/alloca.h" 2 3 4







extern void *alloca (size_t __size) ;






# 579 "/usr/include/stdlib.h" 2 3 4




extern void *valloc (size_t __size) ;
# 592 "/usr/include/stdlib.h" 3 4


extern void abort (void) ;



extern int atexit (void (*__func) (void)) ;





extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     ;






extern void exit (int __status) ;

# 624 "/usr/include/stdlib.h" 3 4


extern char *getenv (const char *__name) ;




extern char *__secure_getenv (const char *__name) ;





extern int putenv (char *__string) ;





extern int setenv (const char *__name, const char *__value, int __replace)
     ;


extern int unsetenv (const char *__name) ;






extern int clearenv (void) ;
# 663 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) ;
# 674 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template);
# 693 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) ;








extern int system (const char *__command);

# 720 "/usr/include/stdlib.h" 3 4
extern char *realpath (const char * __name,
                       char * __resolved) ;






typedef int (*__compar_fn_t) (const void *, const void *);









extern void *bsearch (const void *__key, const void *__base,
                      size_t __nmemb, size_t __size, __compar_fn_t __compar);



extern void qsort (void *__base, size_t __nmemb, size_t __size,
                   __compar_fn_t __compar);



extern int abs (int __x) ;
extern long int labs (long int __x) ;












extern div_t div (int __numer, int __denom)
     ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     ;

# 784 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int * __decpt,
                   int * __sign) ;




extern char *fcvt (double __value, int __ndigit, int * __decpt,
                   int * __sign) ;




extern char *gcvt (double __value, int __ndigit, char *__buf) ;




extern char *qecvt (long double __value, int __ndigit,
                    int * __decpt, int * __sign) ;
extern char *qfcvt (long double __value, int __ndigit,
                    int * __decpt, int * __sign) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf) ;




extern int ecvt_r (double __value, int __ndigit, int * __decpt,
                   int * __sign, char * __buf,
                   size_t __len) ;
extern int fcvt_r (double __value, int __ndigit, int * __decpt,
                   int * __sign, char * __buf,
                   size_t __len) ;

extern int qecvt_r (long double __value, int __ndigit,
                    int * __decpt, int * __sign,
                    char * __buf, size_t __len) ;
extern int qfcvt_r (long double __value, int __ndigit,
                    int * __decpt, int * __sign,
                    char * __buf, size_t __len) ;







extern int mblen (const char *__s, size_t __n) ;


extern int mbtowc (wchar_t * __pwc,
                   const char * __s, size_t __n) ;


extern int wctomb (char *__s, wchar_t __wchar) ;



extern size_t mbstowcs (wchar_t * __pwcs,
                        const char * __s, size_t __n) ;

extern size_t wcstombs (char * __s,
                        const wchar_t * __pwcs, size_t __n)
     ;








extern int rpmatch (const char *__response) ;
# 916 "/usr/include/stdlib.h" 3 4
extern int getloadavg (double __loadavg[], int __nelem) ;






# 12 "huti_sfe_c.src" 2
# 1 "/usr/include/stdio.h" 1 3 4
# 30 "/usr/include/stdio.h" 3 4




# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 35 "/usr/include/stdio.h" 2 3 4
# 44 "/usr/include/stdio.h" 3 4


typedef struct _IO_FILE FILE;





# 62 "/usr/include/stdio.h" 3 4
typedef struct _IO_FILE __FILE;
# 72 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/libio.h" 1 3 4
# 32 "/usr/include/libio.h" 3 4
# 1 "/usr/include/_G_config.h" 1 3 4
# 14 "/usr/include/_G_config.h" 3 4
# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 354 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 3 4
typedef unsigned int wint_t;
# 15 "/usr/include/_G_config.h" 2 3 4
# 24 "/usr/include/_G_config.h" 3 4
# 1 "/usr/include/wchar.h" 1 3 4
# 48 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 49 "/usr/include/wchar.h" 2 3 4

# 1 "/usr/include/bits/wchar.h" 1 3 4
# 51 "/usr/include/wchar.h" 2 3 4
# 76 "/usr/include/wchar.h" 3 4
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 25 "/usr/include/_G_config.h" 2 3 4

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
# 44 "/usr/include/_G_config.h" 3 4
# 1 "/usr/include/gconv.h" 1 3 4
# 28 "/usr/include/gconv.h" 3 4
# 1 "/usr/include/wchar.h" 1 3 4
# 48 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 49 "/usr/include/wchar.h" 2 3 4
# 29 "/usr/include/gconv.h" 2 3 4


# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stddef.h" 1 3 4
# 32 "/usr/include/gconv.h" 2 3 4





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
                            const unsigned char **, const unsigned char *,
                            unsigned char **, size_t *, int, int);


typedef wint_t (*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);


typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);



typedef int (*__gconv_trans_fct) (struct __gconv_step *,
                                  struct __gconv_step_data *, void *,
                                  const unsigned char *,
                                  const unsigned char **,
                                  const unsigned char *, unsigned char **,
                                  size_t *);


typedef int (*__gconv_trans_context_fct) (void *, const unsigned char *,
                                          const unsigned char *,
                                          unsigned char *, unsigned char *);


typedef int (*__gconv_trans_query_fct) (const char *, const char ***,
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
  const char *__modname;

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
  struct __gconv_step_data __data [1];
} *__gconv_t;
# 45 "/usr/include/_G_config.h" 2 3 4
typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t ;
typedef int _G_int32_t ;
typedef unsigned int _G_uint16_t ;
typedef unsigned int _G_uint32_t ;
# 33 "/usr/include/libio.h" 2 3 4
# 53 "/usr/include/libio.h" 3 4
# 1 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stdarg.h" 1 3 4
# 43 "/usr/lib/gcc-lib/i486-linux/3.3.5/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 54 "/usr/include/libio.h" 2 3 4
# 163 "/usr/include/libio.h" 3 4
struct _IO_jump_t; struct _IO_FILE;
# 173 "/usr/include/libio.h" 3 4
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 196 "/usr/include/libio.h" 3 4
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 264 "/usr/include/libio.h" 3 4
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
# 312 "/usr/include/libio.h" 3 4
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
# 351 "/usr/include/libio.h" 3 4
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, const char *__buf,
                                 size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);
# 403 "/usr/include/libio.h" 3 4
extern int __underflow (_IO_FILE *) ;
extern int __uflow (_IO_FILE *) ;
extern int __overflow (_IO_FILE *, int) ;
extern wint_t __wunderflow (_IO_FILE *) ;
extern wint_t __wuflow (_IO_FILE *) ;
extern wint_t __woverflow (_IO_FILE *, wint_t) ;
# 433 "/usr/include/libio.h" 3 4
extern int _IO_getc (_IO_FILE *__fp) ;
extern int _IO_putc (int __c, _IO_FILE *__fp) ;
extern int _IO_feof (_IO_FILE *__fp) ;
extern int _IO_ferror (_IO_FILE *__fp) ;

extern int _IO_peekc_locked (_IO_FILE *__fp) ;





extern void _IO_flockfile (_IO_FILE *) ;
extern void _IO_funlockfile (_IO_FILE *) ;
extern int _IO_ftrylockfile (_IO_FILE *) ;
# 463 "/usr/include/libio.h" 3 4
extern int _IO_vfscanf (_IO_FILE * , const char * ,
                        __gnuc_va_list, int *) ;
extern int _IO_vfprintf (_IO_FILE *, const char *,
                         __gnuc_va_list) ;
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) ;
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) ;

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) ;
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) ;

extern void _IO_free_backup_area (_IO_FILE *) ;
# 73 "/usr/include/stdio.h" 2 3 4
# 86 "/usr/include/stdio.h" 3 4


typedef _G_fpos_t fpos_t;




# 138 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 139 "/usr/include/stdio.h" 2 3 4



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;







extern int remove (const char *__filename) ;

extern int rename (const char *__old, const char *__new) ;









extern FILE *tmpfile (void);
# 178 "/usr/include/stdio.h" 3 4
extern char *tmpnam (char *__s) ;





extern char *tmpnam_r (char *__s) ;
# 196 "/usr/include/stdio.h" 3 4
extern char *tempnam (const char *__dir, const char *__pfx)
     ;








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

# 221 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 235 "/usr/include/stdio.h" 3 4






extern FILE *fopen (const char * __filename,
                    const char * __modes);




extern FILE *freopen (const char * __filename,
                      const char * __modes,
                      FILE * __stream);
# 262 "/usr/include/stdio.h" 3 4

# 273 "/usr/include/stdio.h" 3 4
extern FILE *fdopen (int __fd, const char *__modes) ;
# 294 "/usr/include/stdio.h" 3 4



extern void setbuf (FILE * __stream, char * __buf) ;



extern int setvbuf (FILE * __stream, char * __buf,
                    int __modes, size_t __n) ;





extern void setbuffer (FILE * __stream, char * __buf,
                       size_t __size) ;


extern void setlinebuf (FILE *__stream) ;








extern int fprintf (FILE * __stream,
                    const char * __format, ...);




extern int printf (const char * __format, ...);

extern int sprintf (char * __s,
                    const char * __format, ...) ;





extern int vfprintf (FILE * __s, const char * __format,
                     __gnuc_va_list __arg);




extern int vprintf (const char * __format, __gnuc_va_list __arg);

extern int vsprintf (char * __s, const char * __format,
                     __gnuc_va_list __arg) ;





extern int snprintf (char * __s, size_t __maxlen,
                     const char * __format, ...)
     ;

extern int vsnprintf (char * __s, size_t __maxlen,
                      const char * __format, __gnuc_va_list __arg)
     ;

# 388 "/usr/include/stdio.h" 3 4





extern int fscanf (FILE * __stream,
                   const char * __format, ...);




extern int scanf (const char * __format, ...);

extern int sscanf (const char * __s,
                   const char * __format, ...) ;

# 430 "/usr/include/stdio.h" 3 4





extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);

# 454 "/usr/include/stdio.h" 3 4
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 465 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

# 498 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char * __s, int __n, FILE * __stream);






extern char *gets (char *__s);

# 578 "/usr/include/stdio.h" 3 4





extern int fputs (const char * __s, FILE * __stream);





extern int puts (const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void * __ptr, size_t __size,
                     size_t __n, FILE * __stream);




extern size_t fwrite (const void * __ptr, size_t __size,
                      size_t __n, FILE * __s);

# 631 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void * __ptr, size_t __size,
                              size_t __n, FILE * __stream);
extern size_t fwrite_unlocked (const void * __ptr, size_t __size,
                               size_t __n, FILE * __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream);




extern void rewind (FILE *__stream);

# 686 "/usr/include/stdio.h" 3 4






extern int fgetpos (FILE * __stream, fpos_t * __pos);




extern int fsetpos (FILE *__stream, const fpos_t *__pos);
# 709 "/usr/include/stdio.h" 3 4

# 718 "/usr/include/stdio.h" 3 4


extern void clearerr (FILE *__stream) ;

extern int feof (FILE *__stream) ;

extern int ferror (FILE *__stream) ;




extern void clearerr_unlocked (FILE *__stream) ;
extern int feof_unlocked (FILE *__stream) ;
extern int ferror_unlocked (FILE *__stream) ;








extern void perror (const char *__s);






# 1 "/usr/include/bits/sys_errlist.h" 1 3 4
# 27 "/usr/include/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern const char *const sys_errlist[];
# 748 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) ;




extern int fileno_unlocked (FILE *__stream) ;
# 767 "/usr/include/stdio.h" 3 4
extern FILE *popen (const char *__command, const char *__modes);





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) ;
# 807 "/usr/include/stdio.h" 3 4
extern void flockfile (FILE *__stream) ;



extern int ftrylockfile (FILE *__stream) ;


extern void funlockfile (FILE *__stream) ;
# 831 "/usr/include/stdio.h" 3 4

# 13 "huti_sfe_c.src" 2
# 1 "huti_defs.h" 1
# 14 "huti_sfe_c.src" 2
# 1 "../config.h" 1
# 15 "huti_sfe_c.src" 2

extern void huti_zcgsolv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void huti_zcgssolv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void huti_zbicgstabsolv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void huti_zqmrsolv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void huti_ztfqmrsolv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void huti_zgmressolv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern void huti_zbicgstab_2solv_ ( int *ndim, int *wrkdim,
                        void *xvec, void *rhsvec, int *ipar, double *dpar,
                        void *work, void (*matvecsubr)(),
                        void (*pcondlsubr)(), void (*pcondrsubr)(),
                        void (*dotprodfun)(), double (*normfun)(),
                        void (*stopcfun)() );

extern int huti_num_of_procs;

extern void huti_zdummy_pcondfun_ (void *u, void *v, int *ipar);



extern void zdotu_ (int *N, void *x, int *xind, void *y, int *yind);
extern void zdotc_ (int *N, void *x, int *xind, void *y, int *yind);
extern void dznrm2_ (int *N, void *x, int *xind);
# 82 "huti_sfe_c.src"
void huti_z_cg_ ( void *xvec, void *rhsvec,
                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_zcgsolv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
# 118 "huti_sfe_c.src"
void huti_z_tfqmr_ ( void *xvec, void *rhsvec,

                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_ztfqmrsolv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
# 155 "huti_sfe_c.src"
void huti_z_cgs_ ( void *xvec, void *rhsvec,
                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_zcgssolv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
# 191 "huti_sfe_c.src"
void huti_z_qmr_ ( void *xvec, void *rhsvec,
                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_zqmrsolv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );
  return;
}







void huti_z_bicgstab_ ( void *xvec, void *rhsvec,
                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_zbicgstabsolv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}
# 261 "huti_sfe_c.src"
void huti_z_gmres_ ( void *xvec, void *rhsvec,
                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotc_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_zgmressolv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}







void huti_z_bicgstab_2_ ( void *xvec, void *rhsvec,
                int *ipar, double *dpar, void *work,
                void (*matvecsubr)(),
                void (*pcondlsubr)(),
                void (*pcondrsubr)(),
                void (*dotprodfun)(),
                void (*normfun)(),
                void (*mstopfun)() )
{
  HUTI_Init();



  if (*((int *) pcondrsubr) == 0)
    pcondrsubr = huti_zdummy_pcondfun_;
  if (*((int *) pcondlsubr) == 0)
    pcondlsubr = huti_zdummy_pcondfun_;
  if (*((int *) dotprodfun) == 0)
    dotprodfun = zdotu_;
  if (*((int *) normfun) == 0)
    normfun = dznrm2_;

  huti_zbicgstab_2solv_ ( &ipar[2], &ipar[3], xvec, rhsvec,
                 ipar, dpar, work, matvecsubr, pcondlsubr, pcondrsubr,
                 dotprodfun, normfun, mstopfun );

  return;
}

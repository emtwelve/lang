// file: ffi_lang.h
#ifndef FFI_LANG_H
#define FFI_LANG_H

enum ord_symb {
    SLT = -1,
    SEQ = 0,
    SGT = 1
};
typedef char* symbol;

typedef struct expr_s {
    enum expr_cons_e {
        //EXPRDOUBLE,
        EXPRINT,
        EXPRVAR,
        EXPRADD,
        EXPRSUB,
        EXPRMULT,
        EXPRASSIGN,
	EXPRPRINT,
	EXPRCALL,
	EXPRFUNCDEF
    } expr_cons;
    union {
        //double expr_double;
        int expr_int;
        void* expr_var;
        struct expr_s *expr_print;
        struct {
            struct expr_s *left;
            struct expr_s *right;
        } expr_add;
        struct {
            struct expr_s *left;
            struct expr_s *right;
        } expr_mult;
        struct {
            struct expr_s *left;
            struct expr_s *right;
        } expr_assign;
        struct {
            struct expr_s *left;
            struct expr_s *right;
        } expr_sub;
        struct {
            void *name;
            struct expr_s *arg;
            int argcount;
        } expr_call;
        struct {
            void *name;
            struct expr_s *arg;
            int argcount;
	    struct expr_s *stmts;
	    int stmtcount;
	} expr_fndef;
    } expr_value;
} expr;

#endif

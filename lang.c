// file: example2.c
#include <Python.h>

#include <HsFFI.h>
#include "Lang_stub.h"
#include <stdio.h>

#include "ffi_lang.h"

/* Execute func(x, y) in the Python interpreter. The
arguments and return result of the function must
be Python floats */
double call_func(PyObject *func, double x, double y)
{
    PyObject *args;
    PyObject *kwargs;
    PyObject *result = 0;
    double retval;
      
    // Make sure we own the GIL
    PyGILState_STATE state = PyGILState_Ensure();
      
      
    // Verify that func is a proper callable
    if (!PyCallable_Check(func))
    {
        fprintf(stderr, "call_func: expected a callable\n");
        goto fail;
    }
    // Step3
    args = Py_BuildValue("(dd)", x, y);
    kwargs = NULL;
      
    // Step 4
    result = PyObject_Call(func, args, kwargs);
    Py_DECREF(args);
    Py_XDECREF(kwargs);
      
    // Step 5
    if (PyErr_Occurred())
    {
        PyErr_Print();
        goto fail;
    }
      
    // Verify the result is a float object 
    if (!PyFloat_Check(result))
    {
        fprintf(stderr, "call_func: callable didn't return a float\n");
        goto fail;
    }
      
    // Step 6
    retval = PyFloat_AsDouble(result);
    Py_DECREF(result);
      
    // Step 7
    PyGILState_Release(state);
    return retval;
    fail:
        Py_XDECREF(result);
        PyGILState_Release(state);
        abort();
}

/* Load a symbol from a module */
PyObject *import_name(const char *modname, const char *symbol)
{
    PyObject *u_name, *module;
    u_name = PyUnicode_FromString(modname);
    module = PyImport_Import(u_name);
    Py_DECREF(u_name);
      
    return PyObject_GetAttrString(module, symbol);
}

void print_expr(expr *e) {
  switch(e->expr_cons) {
    case EXPRFUNCDEF:
      printf("Funcdef\n");
      break;
    case EXPRINT:
      printf("%d", e->expr_value.expr_int);
      break;
    case EXPRPRINT:
      printf("print(");
      print_expr(e->expr_value.expr_print);
      printf(")");
      break;
    case EXPRCALL:
      printf("%s(", (char*)e->expr_value.expr_call.name);
      print_expr(e->expr_value.expr_call.arg);
      print_expr(&e->expr_value.expr_call.arg[1]);
      printf(")");
      break;
    case EXPRVAR:
      printf("%s", (char*)e->expr_value.expr_var);
      break;
    case EXPRADD:
      printf("(");
      print_expr(e->expr_value.expr_add.left);
      printf(" + ");
      print_expr(e->expr_value.expr_add.right);
      printf(")");
      break;
    case EXPRSUB:
      printf("(");
      print_expr(e->expr_value.expr_sub.left);
      printf(" - ");
      print_expr(e->expr_value.expr_sub.right);
      printf(")");
      break;
    case EXPRMULT:
      printf("(");
      print_expr(e->expr_value.expr_mult.left);
      printf(" * ");
      print_expr(e->expr_value.expr_mult.right);
      printf(")");
      break;
    case EXPRASSIGN:
      print_expr(e->expr_value.expr_assign.left);
      printf(" : ");
      print_expr(e->expr_value.expr_assign.right);
      break;
  }
}

static PyObject *SpamError;

static PyObject *
spam_system(PyObject *self, PyObject *args)
{
    const char *command;
    int sts;

    if (!PyArg_ParseTuple(args, "s", &command))
        return NULL;
    sts = system(command);
    if (sts < 0) {
        PyErr_SetString(SpamError, "System command failed");
        return NULL;
    }
    return PyLong_FromLong(sts);
}

static PyMethodDef SpamMethods[] = {
    //...
    {"system",  spam_system, METH_VARARGS,
     "Execute a shell command."},
    //...
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef spammodule = {
    PyModuleDef_HEAD_INIT,
    "spam",   /* name of module */
    NULL, /* module documentation, may be NULL */
    -1,       /* size of per-interpreter state of the module,
                 or -1 if the module keeps state in global variables. */
    SpamMethods
};

/*
PyMODINIT_FUNC
PyInit_pydata(void)
{
    return PyModule_Create(&spammodule);
}
*/

PyMODINIT_FUNC
PyInit_pydata(void)
{
    PyObject *m;

    m = PyModule_Create(&spammodule);
    if (m == NULL)
        return NULL;

    SpamError = PyErr_NewException("spam.error", NULL, NULL);
    Py_XINCREF(SpamError);
    if (PyModule_AddObject(m, "error", SpamError) < 0) {
        Py_XDECREF(SpamError);
        Py_CLEAR(SpamError);
        Py_DECREF(m);
        return NULL;
    }

    return m;
}

PyObject *ExprExpression;
PyObject *IDExpression;
PyObject *ProgramEvalExpression;
PyObject *PrintExpression;
PyObject *CALLExpression;
PyObject *BundleExpression;
PyObject *StmtBundleExpression;
PyObject *FuncNodeExpression;

PyObject *convertMarshalledHsToPy(expr *e) {
  PyObject *val;
  switch (e->expr_cons) {
    case EXPRFUNCDEF: {
      printf("Funcdef2\n");
      int i = 0;
      PyObject *arr[e->expr_value.expr_fndef.argcount];
      PyObject *arglist = PyList_New(0);
      expr *arg;
      do {
        arg = e->expr_value.expr_fndef.arg + i;
        if (arg->expr_cons == EXPRINT && arg->expr_value.expr_int == -1)
          break;
        arr[i] = convertMarshalledHsToPy(arg);
        int ret = PyList_Append(arglist, arr[i]);
        if (ret == -1) { printf("Append error"); }
        i++;
      } while (arg->expr_cons != EXPRINT || arg->expr_value.expr_int != -1);
      PyObject *argbundletuple = PyTuple_Pack (1, arglist);
      PyObject_Print(arglist, stdout, 0);
      PyObject *argBundleVal = PyObject_CallObject(BundleExpression, argbundletuple);

      PyObject *stmtlist = PyList_New(0);
      i = 0;
      expr *stmt;
      do {
        stmt = e->expr_value.expr_fndef.stmts + i;
        if (stmt->expr_cons == EXPRINT && stmt->expr_value.expr_int == -1)
          break;
        int ret = PyList_Append(stmtlist, convertMarshalledHsToPy(stmt));
        if (ret == -1) { printf("Append error"); }
        i++;
      } while (stmt->expr_cons != EXPRINT || stmt->expr_value.expr_int != -1);
      PyObject *stmtbundletuple = PyTuple_Pack (1, stmtlist);
      PyObject_Print(stmtlist, stdout, 0);
      PyObject *stmtBundleVal = PyObject_CallObject(StmtBundleExpression, stmtbundletuple);

      PyObject *fnID = Py_BuildValue("s", (char*)e->expr_value.expr_fndef.name);
      PyObject *argtuple = PyTuple_Pack (3, fnID, argBundleVal, stmtBundleVal);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(FuncNodeExpression, argtuple);
      return result;
    }
    case EXPRINT:
      val = Py_BuildValue("i", e->expr_value.expr_int);
      return val;
    case EXPRVAR: {
      val = Py_BuildValue("s", (char *)e->expr_value.expr_var);
      PyObject *argtuple = PyTuple_Pack (1, val);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(IDExpression, argtuple);
      return result;
    }
    case EXPRPRINT: {
      val = convertMarshalledHsToPy(e->expr_value.expr_print);
      PyObject *fnID = Py_BuildValue("s", "print");
      PyObject *argtuple = PyTuple_Pack (3, fnID, val, val);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(CALLExpression, argtuple);
      return result;
    }
    case EXPRCALL: {
      int i = 0;
      PyObject *arr[e->expr_value.expr_call.argcount];
      PyObject *list = PyList_New(0);
      expr *arg;
      do {
        arg = e->expr_value.expr_call.arg + i;
        if (arg->expr_cons == EXPRINT && arg->expr_value.expr_int == -1)
          break;
        arr[i] = convertMarshalledHsToPy(arg);
        int ret = PyList_Append(list, arr[i]);
        if (ret == -1) { printf("Append error"); }
        i++;
      } while (arg->expr_cons != EXPRINT || arg->expr_value.expr_int != -1);
      PyObject *bundletuple = PyTuple_Pack (1, list);
      PyObject_Print(list, stdout, 0);
      PyObject *bundleVal = PyObject_CallObject(BundleExpression, bundletuple);

      PyObject *fnID = Py_BuildValue("s", (char*)e->expr_value.expr_call.name);
      PyObject *zero = Py_BuildValue("i", 0);
      PyObject *argtuple = PyTuple_Pack (3, fnID, bundleVal, zero);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(CALLExpression, argtuple);
      return result;
    }
    case EXPRADD: {
      PyObject *valR = convertMarshalledHsToPy(e->expr_value.expr_add.right);
      PyObject *valOp = Py_BuildValue("s", "+");
      PyObject *valL = convertMarshalledHsToPy(e->expr_value.expr_add.left);
      PyObject *argtuple = PyTuple_Pack (3, valL, valOp, valR);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(ExprExpression, argtuple);
      return result;
    }
    case EXPRSUB: {
      PyObject *valR = convertMarshalledHsToPy(e->expr_value.expr_sub.right);
      PyObject *valOp = Py_BuildValue("s", "-");
      PyObject *valL = convertMarshalledHsToPy(e->expr_value.expr_sub.left);
      PyObject *argtuple = PyTuple_Pack (3, valL, valOp, valR);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(ExprExpression, argtuple);
      return result;
    }
    case EXPRMULT: {
      PyObject *valR = convertMarshalledHsToPy(e->expr_value.expr_mult.right);
      PyObject *valOp = Py_BuildValue("s", "*");
      PyObject *valL = convertMarshalledHsToPy(e->expr_value.expr_mult.left);
      PyObject *argtuple = PyTuple_Pack (3, valL, valOp, valR);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(ExprExpression, argtuple);
      return result;
    }
    case EXPRASSIGN: {
      PyObject *valL = convertMarshalledHsToPy(e->expr_value.expr_assign.left);
      PyObject *valOp = Py_BuildValue("s", ":");
      PyObject *valR = convertMarshalledHsToPy(e->expr_value.expr_assign.right);
      PyObject *argtuple = PyTuple_Pack (3, valL, valOp, valR);
      if (argtuple == NULL) { printf("Failed argtuple\n"); fflush(stdout); }
      PyObject *result = PyObject_CallObject(ExprExpression, argtuple);
      return result;
    }
  }
}

int main(int argc, char *argv[])
{
    double x;
    Py_Initialize();
    PyRun_SimpleString("x = 3");
    FILE *exp_file = fopen("lang.py", "r");
    PyRun_SimpleFile(exp_file, "lang.py");
    PyObject *main_module = PyImport_AddModule("__main__");
    PyObject *global_dict = PyModule_GetDict(main_module);
    ExprExpression = PyDict_GetItemString(global_dict, "Expr");
    ProgramEvalExpression = PyDict_GetItemString(global_dict, "program_eval");
    IDExpression = PyDict_GetItemString(global_dict, "ID2");
    PrintExpression = PyDict_GetItemString(global_dict, "mmprint");
    CALLExpression = PyDict_GetItemString(global_dict, "CALL");
    BundleExpression = PyDict_GetItemString(global_dict, "ArgBundle");
    StmtBundleExpression = PyDict_GetItemString(global_dict, "Bundle");
    FuncNodeExpression = PyDict_GetItemString(global_dict, "FuncNode");

    // parse program and marshall from hs to c using ffi
    symbol prog = "def hi(x) { a : x\nb : 2 }\na : pow(2,3) + 8 + int(str (9)) * (13 + 3)\n b : 3 + a\nc : b * a\nd : print(c) - 2\nprint(d)";
    hs_init(&argc, &argv);
    expr *p0 = mylib_parse_expression(prog);
    hs_exit();

    // convert from c objects to py objects
    expr *e;
    int i = 0;
    PyObject *list = PyList_New(0);
    do {
      e = &p0[i];
      print_expr(e);
      PyObject *pyExpr = convertMarshalledHsToPy(e);
      int ret = PyList_Append(list, pyExpr);
      if (ret == -1) {
        printf("Append error");
      }
      printf("\n");
      ++i;
    } while (e->expr_cons != EXPRINT || e->expr_value.expr_int != -1);
    PyObject_Print(list, stdout, 0);

    PyObject *res = PyObject_CallFunction(ProgramEvalExpression, "O", list);
    PyRun_SimpleString("print(x)");
    if (res == NULL) {
      printf("\nFailure\n");
      PyErr_Print();
    } else {
      printf("\nSuccess\n");
      PyObject_Print(res, stdout, 0);
    }


    printf("\n");

    /* Add a built-in module, before Py_Initialize */
    /*if (PyImport_AppendInittab("py_data", PyInit_pydata) == -1) {
        fprintf(stderr, "Error: could not extend in-built modules table\n");
        exit(1);
    }*/
    /*
    PyObject *val = Py_BuildValue("i", 42);
    PyObject *val2 = Py_BuildValue("(ii)", 43, 44);
    PyObject *val3 = Py_BuildValue("(i)", 45);
    PyObject *argtuple = PyTuple_Pack (3, val, val2, val3);
    if (argtuple==NULL) {
      printf("Failed argtuple\n");
      fflush(stdout);
    }
    PyObject *result = PyObject_CallObject(ExprExpression, argtuple);
    if (result == NULL) {
      PyObject_Print(ExprExpression, stdout, 0);
      printf("Failed call object\n");
      printf("\n");
    } else {
      PyObject_Print(ExprExpression, stdout, 0);
      printf("\n");
      PyObject_Print(result, stdout, 0);
      printf("\n");
    }
    */

    //PyObject * pow_func;
    //pow_func = import_name("math", "pow");

    /*PyObject *pmodule = PyImport_ImportModule("py_data");
    if (!pmodule) {
        PyErr_Print();
        fprintf(stderr, "Error: could not import module 'spam'\n");
    }*/

    //PyObject * ID_class;
    //ID_class = import_name("py_data", "ID");
      
    // Call it using our call_func() code 
    /*
    for (x = 0.0; x < 1.0; x += 0.2)
    {
        printf("% 0.2f % 0.2f\n", x, call_func(pow_func, x, 2.0));
    }
          
    Py_DECREF(pow_func);
    */

    // FIXME: This causes a segfault...
    //Py_Finalize();

    return 0;
}

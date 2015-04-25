#include "macros.h"

#define METHOD_TEMPLATE_H QUOTE_MACROS(CONCAT_MACROS(METHOD,TYPE-template.h))

#ifdef X_TYPE_I
  #define X_TYPE 'i'

  #include METHOD_TEMPLATE_H // 00
  #define COLS_TYPE 'i'
  #include METHOD_TEMPLATE_H // 01
  #undef COLS_TYPE
  #define COLS_TYPE 'r'
  #include METHOD_TEMPLATE_H // 02
  #undef COLS_TYPE
  #define COLS_TYPE 'l'
  #include METHOD_TEMPLATE_H // 03
  #undef COLS_TYPE

  #define ROWS_TYPE 'i'
  #include METHOD_TEMPLATE_H // 10
  #define COLS_TYPE 'i'
  #include METHOD_TEMPLATE_H // 11
  #undef COLS_TYPE
  #define COLS_TYPE 'r'
  #include METHOD_TEMPLATE_H // 12
  #undef COLS_TYPE
  #define COLS_TYPE 'l'
  #include METHOD_TEMPLATE_H // 13
  #undef COLS_TYPE
  #undef ROWS_TYPE

  #define ROWS_TYPE 'r'
  #include METHOD_TEMPLATE_H // 20
  #define COLS_TYPE 'i'
  #include METHOD_TEMPLATE_H // 21
  #undef COLS_TYPE
  #define COLS_TYPE 'r'
  #include METHOD_TEMPLATE_H // 22
  #undef COLS_TYPE
  #define COLS_TYPE 'l'
  #include METHOD_TEMPLATE_H // 23
  #undef COLS_TYPE
  #undef ROWS_TYPE

  #define ROWS_TYPE 'l'
  #include METHOD_TEMPLATE_H // 30
  #define COLS_TYPE 'i'
  #include METHOD_TEMPLATE_H // 31
  #undef COLS_TYPE
  #define COLS_TYPE 'r'
  #include METHOD_TEMPLATE_H // 32
  #undef COLS_TYPE
  #define COLS_TYPE 'l'
  #include METHOD_TEMPLATE_H // 33
  #undef COLS_TYPE
  #undef ROWS_TYPE

  RETURN_TYPE (*METHOD_NAME[4][4])(ARGUMENTS_LIST) = {
    {METHOD_NAME_noRows_noCols, METHOD_NAME_noRows_intCols, METHOD_NAME_noRows_realCols, METHOD_NAME_noRows_lglCols},
    {METHOD_NAME_intRows_noCols, METHOD_NAME_intRows_intCols, METHOD_NAME_intRows_realCols, METHOD_NAME_intRows_lglCols},
    {METHOD_NAME_realRows_noCols, METHOD_NAME_realRows_intCols, METHOD_NAME_realRows_realCols, METHOD_NAME_realRows_lglCols},
    {METHOD_NAME_lglRows_noCols, METHOD_NAME_lglRows_intCols, METHOD_NAME_lglRows_realCols, METHOD_NAME_lglRows_lglCols},
  };
  #include "templates-types_undef.h"
#endif

#ifdef X_TYPE_R
  #define X_TYPE 'r'
  #define ROWS_TYPE
  #include METHOD_TEMPLATE_H // 10
  #define COLS_TYPE
  #include METHOD_TEMPLATE_H // 11
  #undef ROWS_TYPE
  #include METHOD_TEMPLATE_H // 01
  #undef COLS_TYPE
  #include METHOD_TEMPLATE_H // 00

  RETURN_TYPE (*METHOD_NAME[4][4])(ARGUMENTS_LIST) = {
    {METHOD_NAME_noRows_noCols, METHOD_NAME_noRows_hasCols},
    {METHOD_NAME_hasRows_noCols, METHOD_NAME_hasRows_hasCols},
  };
  #include "templates-types_undef.h"
#endif

#ifdef X_TYPE_L
  #define X_TYPE 'l'
  #define ROWS_TYPE
  #include METHOD_TEMPLATE_H // 10
  #define COLS_TYPE
  #include METHOD_TEMPLATE_H // 11
  #undef ROWS_TYPE
  #include METHOD_TEMPLATE_H // 01
  #undef COLS_TYPE
  #include METHOD_TEMPLATE_H // 00

  RETURN_TYPE (*METHOD_NAME[4][4])(ARGUMENTS_LIST) = {
    {METHOD_NAME_noRows_noCols, METHOD_NAME_noRows_hasCols},
    {METHOD_NAME_hasRows_noCols, METHOD_NAME_hasRows_hasCols},
  };
  #include "templates-types_undef.h"
#endif

#undef METHOD_TEMPLATE_H

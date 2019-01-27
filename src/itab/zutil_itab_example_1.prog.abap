*&---------------------------------------------------------------------*
*& Report zutil_itab_example_1
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender como utilizar la clase ZCL_CADYNAMIC_TABLES
*& Descripción: Creación de una tabla interna a partir de un catalogo de campos
*&---------------------------------------------------------------------*
REPORT zutil_itab_example_1.

FIELD-SYMBOLS <tbl> TYPE STANDARD TABLE.

START-OF-SELECTION.


* Se rellena el catalogo a partir del cual se creará la tabla interna dinámica
  DATA(lt_fcat) = VALUE lvc_t_fcat( ( fieldname = 'BNAME'  rollname = 'XUBNAME' )
                                    ( fieldname = 'COUNT' inttype = cl_abap_elemdescr=>typekind_int )
                                    ( fieldname = 'AMOUNT' inttype = cl_abap_elemdescr=>typekind_packed decimals = 2 intlen = 13 ) ).

  " Creación de la tabla interna
  zcl_ca_dynamic_tables=>create_it_from_fcat(
    EXPORTING
      i_fields = lt_fcat
    IMPORTING
      e_table  = DATA(lo_table) ).

  IF lo_table IS BOUND.
    ASSIGN lo_table->* TO <tbl>.


* Se rellena la tabla de datos
    SELECT bname, 1 AS count  FROM usr02 INTO CORRESPONDING FIELDS OF TABLE @<tbl>.


    cl_demo_output=>display( <tbl> ).

  ELSE.
    MESSAGE 'Error to create table' TYPE 'S'.
  ENDIF.

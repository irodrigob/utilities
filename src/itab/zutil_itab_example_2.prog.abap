*&---------------------------------------------------------------------*
*& Report zutil_itab_example_2
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender como utilizar la clase ZCL_CADYNAMIC_TABLES
*& Descripción: Creación de una tabla interna a partir de un catalogo de campos +
*& campos de una estructura del diccionario
*&---------------------------------------------------------------------*
REPORT zutil_itab_example_2.

FIELD-SYMBOLS <tbl> TYPE STANDARD TABLE.


START-OF-SELECTION.

* Obtenemos el puntero de la estructura
  CALL METHOD zcl_ca_dynamic_tables=>create_wa_from_struc
    EXPORTING
      i_struc    = 'ZUTIL_ITAB_EX2'
    IMPORTING
      e_workarea = DATA(lo_main_fields).

* Campos a medida
  DATA(lt_fcat) = VALUE lvc_t_fcat( ( fieldname = 'BNAME'  rollname = 'XUBNAME' )
                                    ( fieldname = 'COUNT' inttype = cl_abap_elemdescr=>typekind_int )
                                    ( fieldname = 'AMOUNT' inttype = cl_abap_elemdescr=>typekind_packed decimals = 2 intlen = 13 ) ).


  zcl_ca_dynamic_tables=>create_it_fields_base_ref( EXPORTING i_base_fields = lo_main_fields
                                                              i_new_fields  = lt_fcat
                                                    IMPORTING e_table       = DATA(lo_table) ).

  IF lo_table IS BOUND.
    ASSIGN lo_table->* TO <tbl>.

* Se rellena la tabla de datos
    SELECT bname, 1 AS count, @icon_businav_objects AS navigation, 'hello' AS field2  FROM usr02 INTO CORRESPONDING FIELDS OF TABLE @<tbl>.


    cl_demo_output=>display( <tbl> ).
  ELSE.
    MESSAGE 'Error to create table' TYPE 'S'.
  ENDIF.

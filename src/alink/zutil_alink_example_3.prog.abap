*&---------------------------------------------------------------------*
*& Report zutil_alink_example_2
*&---------------------------------------------------------------------*
*& Description: Example of document list
*&---------------------------------------------------------------------*
REPORT zutil_alink_example_3.

DATA lt_files TYPE filetable.
DATA lv_rc TYPE i.
DATA mt_file_bin TYPE solix_tab.
DATA mv_file_content TYPE xstring.
DATA mv_file_name TYPE string.

PARAMETERS: p_sapobj TYPE saeanwdid OBLIGATORY DEFAULT 'ZTEST_GOS',
            p_obj_id TYPE saeobjid OBLIGATORY.



START-OF-SELECTION.


  NEW zcl_ca_archivelink( )->get_document_list(
    EXPORTING
      iv_object_id       = p_obj_id
      iv_sap_object      = p_sapobj
    IMPORTING
      et_document_list   = DATA(mt_document_list)
      es_return          = DATA(ls_return) ).

  " In this version of SAP (trial version 7.52) ES_RETURN will always return an Archivelink configuration error. This error is ignored

  " Show the data
  DATA(mo_alv) = NEW zcl_ca_alv(  ).
  mo_alv->create_alv( EXPORTING iv_program        = sy-repid
                      CHANGING ct_data           = mt_document_list
                      EXCEPTIONS error_create_alv  = 1
                                 OTHERS            = 2 ).

  IF sy-subrc <> 0.
    WRITE:/ 'Error to create ALV'.
  ELSE.
    mo_alv->show_alv( ).
  ENDIF.

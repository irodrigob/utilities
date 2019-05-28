*&---------------------------------------------------------------------*
*& Report zutil_alink_example_4
*&---------------------------------------------------------------------*
*& Description: Example of delete document
*&---------------------------------------------------------------------*
REPORT zutil_alink_example_4.

DATA ms_key_gos TYPE zcl_ca_archivelink=>ts_key_gos.

PARAMETERS: p_sapobj TYPE saeanwdid OBLIGATORY DEFAULT 'ZTEST_GOS',
            p_obj_id TYPE saeobjid OBLIGATORY.



START-OF-SELECTION.

  DATA(lo_alink) = NEW zcl_ca_archivelink( ).

  lo_alink->get_document_list(
    EXPORTING
      iv_object_id       = p_obj_id
      iv_sap_object      = p_sapobj
    IMPORTING
      et_document_list   = DATA(mt_document_list)
      es_return          = DATA(ls_return) ).

  " In this version of SAP (trial version 7.52) ES_RETURN will always return an Archivelink configuration error. This error is ignored

  LOOP AT mt_document_list ASSIGNING FIELD-SYMBOL(<ls_document_list>) WHERE doc_type = zcl_ca_archivelink=>cv_doc_type_gos.

    CLEAR: ls_return.

    ms_key_gos = <ls_document_list>-doc_id.

    lo_alink->delete_file_gos(
      EXPORTING
        is_key        = ms_key_gos
        iv_sap_object =  p_sapobj
        iv_object_id  =  p_obj_id
      IMPORTING
        es_return     = ls_return ).

    IF ls_return-type = 'E'.
      WRITE:/'Error to delete id:', <ls_document_list>-doc_id.
    ENDIF.

  ENDLOOP.

  " Reload data
  lo_alink->get_document_list(
      EXPORTING
        iv_object_id       = p_obj_id
        iv_sap_object      = p_sapobj
      IMPORTING
        et_document_list   = mt_document_list
        es_return          = ls_return ).

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

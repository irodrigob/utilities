*&---------------------------------------------------------------------*
*& Report zutil_spool_example_1
*&---------------------------------------------------------------------*
*& Objetivo: Ejemplos para entender como utilizar la clase ZCL_CA_UTIL_SPOOL
*& Descripción: Mostra el contenido de una orden SPOOL en PDF
*&---------------------------------------------------------------------*
REPORT zutil_spool_example_1.

DATA mo_pdf_html_control TYPE REF TO cl_gui_html_viewer.
DATA mo_pdf_my_container TYPE REF TO cl_gui_custom_container.

PARAMETERS: p_spool TYPE tsp01-rqident OBLIGATORY.

START-OF-SELECTION.

  NEW zcl_ca_util_spool( )->spool_2_pdf(
    EXPORTING
      iv_spoolno                  = p_spool
    IMPORTING

      ev_numbytes                 = DATA(mv_numbytes)
      ev_xpdf                     = DATA(mv_xpdf)
    EXCEPTIONS
      spool_dont_exist            = 1
      error_read_spool_attributes = 2
      error_spool_conversion      = 3
      OTHERS                      = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE 'Error to load the spool request' TYPE 'S'.

  ELSE.
    CALL SCREEN 9000.
  ENDIF.
*&---------------------------------------------------------------------*
*& Module SHOW_PDF OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE show_pdf OUTPUT.
  SET PF-STATUS 'D9000'.

* Container
  IF mo_pdf_my_container IS INITIAL.
    CREATE OBJECT mo_pdf_my_container
      EXPORTING
        container_name = 'HTML'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'CONTROL ERROR' TYPE 'E'.
    ENDIF.
  ENDIF.

* HTML control
  IF mo_pdf_html_control IS INITIAL.
    CREATE OBJECT mo_pdf_html_control
      EXPORTING
        parent = mo_pdf_my_container
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'CONTROL ERROR' TYPE 'E'.
    ENDIF.
  ENDIF.

* Alignment
  DATA(lv_pdf_alignment) = mo_pdf_html_control->align_at_left  +
                  mo_pdf_html_control->align_at_right +
                  mo_pdf_html_control->align_at_top   +
                  mo_pdf_html_control->align_at_bottom.

  CALL METHOD mo_pdf_html_control->set_alignment
    EXPORTING
      alignment = lv_pdf_alignment
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'CONTROL ERROR' TYPE 'E'.
  ENDIF.

* Show PDF
  PERFORM show_pdf.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SALIR  INPUT
*&---------------------------------------------------------------------*
MODULE salir INPUT.
  IF mo_pdf_html_control IS NOT INITIAL.
    mo_pdf_html_control->free( ).
    FREE mo_pdf_html_control.
  ENDIF.
  IF mo_pdf_my_container IS NOT INITIAL.
    mo_pdf_my_container->free( ).
    FREE mo_pdf_my_container.
  ENDIF.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.

FORM show_pdf.
  DATA lv_url      TYPE char80.
  DATA lv_pdf_data TYPE tsfixml.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = mv_xpdf
    TABLES
      binary_tab = lv_pdf_data.

  CALL METHOD mo_pdf_html_control->load_data
    EXPORTING
      url          = 'smart.pdf'                            "#EC NOTEXT
      size         = mv_numbytes
      type         = 'application'                          "#EC NOTEXT
      subtype      = 'pdf'
    IMPORTING
      assigned_url = lv_url
    CHANGING
      data_table   = lv_pdf_data
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'ERROR: CONTROL->LOAD_DATA' TYPE 'E'.
  ENDIF.

* Show data.
  CALL METHOD mo_pdf_html_control->show_data
    EXPORTING
      url    = lv_url
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'ERROR: CONTROL->SHOW_DATA' TYPE 'E'.
  ENDIF.

ENDFORM.

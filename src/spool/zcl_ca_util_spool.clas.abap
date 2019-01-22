class ZCL_CA_UTIL_SPOOL definition
  public
  final
  create public .

public section.

  class-methods SPOOL_2_PDF
    importing
      !iv_spoolno type TSP01-RQIDENT
    exporting
      !ev_pdf type SOLIX_TAB
      !ev_numbytes type I
      !ev_pdf_tline type TLINE_TAB
      !ev_xpdf type XSTRING
    exceptions
      SPOOL_DONT_EXIST
      ERROR_READ_SPOOL_ATTRIBUTES
      ERROR_SPOOL_CONVERSION .
  class-methods length_bin
    importing
      !I_BIN type RSPC_T_TEXT
    returning
      value(rv_len_bytes) type I .
  CLASS-METHODS spool_2_otf
    IMPORTING
      !iv_spoolno  TYPE tsp01-rqident
    EXPORTING
      !ev_otf      TYPE soli_tab
      !ev_numbytes TYPE i
    EXCEPTIONS
      error_convert_spool .
protected section.

  class-methods CONV_PDF_2_BIN
    importing
      !iv_pdf type TLINE_TAB
      !iv_pdf_length type I
    exporting
      !ev_bin type SOLIX_TAB
      !ev_xbin type XSTRING .
private section.
ENDCLASS.



CLASS ZCL_CA_UTIL_SPOOL IMPLEMENTATION.


METHOD CONV_PDF_2_BIN.
  DATA soli_tab TYPE soli_tab.


* Primero se pasa la tabla de PDF de 132 carácteres a una de 255
  CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
    TABLES
      content_in  = iv_pdf
      content_out = soli_tab
    EXCEPTIONS
      OTHERS      = 4.

* Paso el "binario" a xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = iv_pdf_length
    IMPORTING
      buffer       = ev_xbin
    TABLES
      binary_tab   = soli_tab
    EXCEPTIONS
      OTHERS       = 2.

* Finalmente paso el xstring a binario.
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = ev_xbin
    TABLES
      binary_tab = ev_bin.


ENDMETHOD.


METHOD SPOOL_2_OTF.
  DATA lt_soli_tab TYPE LINE OF soli_tab.

  CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
    EXPORTING
      rqident                    = iv_spoolno
*   IMPORTING
*     REAL_TYPE                  =
*     SP_LANG                    =
    TABLES
      buffer                     = ev_otf
   EXCEPTIONS
     no_such_job                = 1
     job_contains_no_data       = 2
     selection_empty            = 3
     no_permission              = 4
     can_not_access             = 5
     read_error                 = 6
     type_no_match              = 7
     OTHERS                     = 8.

  IF sy-subrc <> 0.
    RAISE error_convert_spool.
  ELSE.

* Calculo el tamño del OTF
    DESCRIBE TABLE ev_otf.
    ev_numbytes = 255 * ( sy-tfill - 1 ).
    READ TABLE ev_otf INTO lt_soli_tab INDEX sy-tfill.
    ev_numbytes = ev_numbytes + STRLEN( lt_soli_tab ).

  ENDIF.


ENDMETHOD.


METHOD SPOOL_2_PDF.

  DATA LS_tsp01 TYPE tsp01 .
  DATA lv_client TYPE tst01-dclient .
  DATA lv_name TYPE tst01-dname .
  DATA lv_objtype TYPE rststype-type .
  DATA lv_type TYPE rststype-type .
  DATA lv_otf TYPE sap_bool .
  DATA lv_numbytes TYPE i .
  DATA lv_arc_idx TYPE toa_dara .
  DATA lv_pdfspoolid TYPE tsp01-rqident .
  DATA lv_jobname TYPE tbtcjob-jobname .
  DATA lv_jobcount TYPE tbtcjob-jobcount .


* Comprobamos que la orden de Spool exista
  SELECT SINGLE * INTO LS_tsp01
         FROM tsp01
         WHERE rqident = iv_spoolno.

  IF sy-subrc NE 0.
    RAISE SPOOL_DONT_EXIST.
  ENDIF.

  lv_client = LS_tsp01-rqclient.
  lv_name   = LS_tsp01-rqo1name.

*   Obtenemos las características de la orden de Spool
  CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
    EXPORTING
      authority     = 'SP01'
      client        = lv_client
      name          = lv_name
      part          = 1
    IMPORTING
      type          = lv_type
      objtype       = lv_objtype
    EXCEPTIONS
      fb_error      = 1
      fb_rsts_other = 2
      no_object     = 3
      no_permission = 4.

  IF sy-subrc = 0.

    IF lv_objtype(3) = 'OTF'.
      lv_otf = 'X'.
    ELSE.
      lv_otf = space.
    ENDIF.

* Dependiendo del tipo de spool se hará uso de una función distinta para leer
* convertir el SPOOL a PDF
    IF lv_otf = 'X'.

      CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = iv_spoolno
          no_dialog                = space
        IMPORTING
          pdf_bytecount            = ev_numbytes
          pdf_spoolid              = lv_pdfspoolid
          btc_jobname              = lv_jobname
          btc_jobcount             = lv_jobcount
        TABLES
          pdf                      = ev_pdf_tline
        EXCEPTIONS
          err_no_otf_spooljob      = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_dstdevice        = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11.
    ELSE.

      CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = iv_spoolno
          no_dialog                = space
        IMPORTING
          pdf_bytecount            = ev_numbytes
          pdf_spoolid              = lv_pdfspoolid
          btc_jobname              = lv_jobname
          btc_jobcount             = lv_jobcount
        TABLES
          pdf                      = ev_pdf_tline
        EXCEPTIONS
          err_no_abap_spooljob     = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_destdevice       = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11.
    ENDIF.

    IF sy-subrc = 0.

* Convierto el PDF a formato binario
      CALL METHOD conv_pdf_2_bin
        EXPORTING
          iv_pdf        = ev_pdf_tline
          iv_pdf_length = ev_numbytes
        IMPORTING
          ev_bin        = ev_pdf
          ev_xbin       = ev_xpdf.

    ELSE.
      RAISE ERROR_SPOOL_CONVERSION.
    ENDIF.


  ELSE.
    RAISE ERROR_READ_SPOOL_ATTRIBUTES.
  ENDIF.

ENDMETHOD.


method length_bin.
DATA le_bin TYPE LINE OF rspc_t_text.

* Calculo el tamaño en bytes del binario
  DESCRIBE TABLE i_bin.
* Leo la última posición para el calculo del tamaño binario.
  READ TABLE i_bin INTO le_bin INDEX sy-tfill.

* Calculo el tamaño del binario
  rv_len_bytes = ( sy-tfill - 1 ) * 255 + STRLEN( le_bin ).

endmethod.
ENDCLASS.

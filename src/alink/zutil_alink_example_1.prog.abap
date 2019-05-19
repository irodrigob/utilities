*&---------------------------------------------------------------------*
*& Report zutil_alink_example_1
*&---------------------------------------------------------------------*
*& Description: Ejemplo de subido de un documento como temporal
*&---------------------------------------------------------------------*
REPORT zutil_alink_example_1.

DATA lt_files TYPE filetable.
DATA lv_rc TYPE i.
DATA mt_file_bin TYPE solix_tab.
DATA mv_file_content TYPE xstring.
DATA mv_file_name TYPE string.

PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.



  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select file..'
      default_filename = '*.*'
      multiselection   = ' '
    CHANGING
      file_table       = lt_files
      rc               = lv_rc.

  READ TABLE lt_files INTO p_file INDEX 1.


START-OF-SELECTION.

  " Upload file
  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = CONV string( p_file )
      filetype                = 'BIN'
 IMPORTING
      filelength              = DATA(mv_len)
    CHANGING
      data_tab                = mt_file_bin
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19
  ).
  IF sy-subrc <> 0.
    WRITE:/ 'Error to read the file'.
  ELSE.
    " Convert binary file to xstring
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = mv_len
      IMPORTING
        buffer       = mv_file_content
      TABLES
        binary_tab   = mt_file_bin
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    " Get the name of file
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = p_file
      IMPORTING
        stripped_name = mv_file_name.

    " Get the file extension
    SPLIT mv_file_name AT '.' INTO: DATA(lv_dummy) DATA(lv_extension).

    " Get the mimetype
    SELECT SINGLE type INTO @DATA(lv_mimetype) FROM sdokfext WHERE extension = @lv_extension.

    " Fill the structure with the values of temporary data
    DATA(ls_attach) = VALUE zca_s_attach( appl = 'TEST' filename = mv_file_name extension = lv_extension mimetype = lv_mimetype content = mv_file_content ).

    " Save temporary data
    NEW zcl_ca_archivelink( )->upload_file_tmp(
      EXPORTING
        is_attach    = ls_attach
      IMPORTING
        ev_id_attach = DATA(lv_id_attach) ).

   " next steps: transfer the temporary data to archivelink with the method conv_tmp_file_2_alink



    BREAK-POINT.
  ENDIF.

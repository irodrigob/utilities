CLASS zcl_ca_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Fill the structure BAPIRET2</p>
    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE any
        !iv_number       TYPE any
        !iv_id           TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_langu        TYPE sylangu DEFAULT sy-langu
        !iv_row          TYPE bapi_line OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Returns next number in a number range</p>
    CLASS-METHODS number_get_next
      IMPORTING
        !iv_nr_range_nr   TYPE nrnr DEFAULT '00'
        !iv_nrobj         TYPE nrobj
        !iv_quantity      TYPE nrquan DEFAULT '1'
        !iv_subobject     TYPE nrsobj DEFAULT space
        !iv_toyear        TYPE nryear DEFAULT '0000'
        !iv_ignore_buffer TYPE boolean_flg DEFAULT abap_false
        !iv_langu         TYPE spras DEFAULT sy-langu
      EXPORTING
        !ev_number        TYPE any
        !ev_subrc         TYPE sy-subrc
        !es_return        TYPE bapiret2 .
    "! <p class="shorttext synchronized">Message convert of BALMSG to BAPIRET2</p>
    CLASS-METHODS conv_balmsg_to_bapiret2
      IMPORTING
        !is_balmsg         TYPE bal_s_msg
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2 .

    "! <p class="shorttext synchronized">Show BAPIRET2 messages in a popup</p>
    CLASS-METHODS show_message_bapiret2
      IMPORTING
        !it_messages TYPE bapiret2_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_utilities IMPLEMENTATION.


  METHOD conv_balmsg_to_bapiret2.

    CLEAR rs_bapiret2.

    IF is_balmsg IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CACS_STDCTR_BALMSG_TO_BAPIRET2'
      EXPORTING
        is_balmsg   = is_balmsg
      CHANGING
        cs_bapiret2 = rs_bapiret2.

  ENDMETHOD.


  METHOD fill_return.
    CLEAR rs_return.

    rs_return-type = iv_type.

    rs_return-id = iv_id.
    rs_return-number = iv_number.
    rs_return-message_v1 = iv_message_v1.
    rs_return-message_v2 = iv_message_v2.
    rs_return-message_v3 = iv_message_v3.
    rs_return-message_v4 = iv_message_v4.
    rs_return-row        = iv_row.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = rs_return-id
        number     = rs_return-number
        language   = iv_langu
        textformat = 'ASC'
        message_v1 = rs_return-message_v1
        message_v2 = rs_return-message_v2
        message_v3 = rs_return-message_v3
        message_v4 = rs_return-message_v4
      IMPORTING
        message    = rs_return-message.
  ENDMETHOD.


  METHOD number_get_next.

    DATA lv_returncode TYPE nrreturn.

    CLEAR: es_return,
           ev_number,
           ev_subrc.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = iv_nr_range_nr
        object                  = iv_nrobj
        quantity                = iv_quantity
        subobject               = iv_subobject
        toyear                  = iv_toyear
        ignore_buffer           = iv_ignore_buffer
      IMPORTING
        number                  = ev_number
        returncode              = lv_returncode
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc NE 0.

      ev_subrc = sy-subrc.

      es_return = fill_return( iv_type       = sy-msgty
                               iv_number     = sy-msgno
                               iv_id         = sy-msgid
                               iv_message_v1 = sy-msgv1
                               iv_message_v2 = sy-msgv2
                               iv_message_v3 = sy-msgv3
                               iv_message_v4 = sy-msgv4
                               iv_langu      = iv_langu ).
    ELSE.
      ev_subrc = lv_returncode.
    ENDIF.

  ENDMETHOD.
  METHOD show_message_bapiret2.

    DATA(lt_messages) = VALUE esp1_message_tab_type( FOR <wa> IN it_messages ( msgid = <wa>-id
                                                                               msgty = <wa>-type
                                                                               msgno = <wa>-number
                                                                               msgv1 = <wa>-message_v1
                                                                               msgv2 = <wa>-message_v2
                                                                               msgv3 = <wa>-message_v3
                                                                               msgv4 = <wa>-message_v4 )  ).

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_messages.

  ENDMETHOD.

ENDCLASS.

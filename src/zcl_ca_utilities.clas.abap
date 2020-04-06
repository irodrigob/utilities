CLASS zcl_ca_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE any
        !iv_id           TYPE any
        !iv_number       TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_field        TYPE any OPTIONAL
        !iv_langu        TYPE sylangu DEFAULT sy-langu
        !iv_row          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_utilities IMPLEMENTATION.

  METHOD fill_return.

    CLEAR rs_return.
    rs_return-field = iv_field.
    rs_return-type = iv_type.
    rs_return-id = iv_id.
    rs_return-number = iv_number.
    rs_return-message_v1 = iv_message_v1.
    rs_return-message_v2 = iv_message_v2.
    rs_return-message_v3 = iv_message_v3.
    rs_return-message_v4 = iv_message_v4.
    rs_return-row = iv_row.

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

ENDCLASS.

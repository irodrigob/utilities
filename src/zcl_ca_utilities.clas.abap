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
    "! <p class="shorttext synchronized">Devuelve los componentes de una estructura de manera recursiva</p>
    "! Útil cuando la estructura tiene includes
    "! @parameter is_structure | <p class="shorttext synchronized">Estructura</p>
    "! @parameter rt_components | <p class="shorttext synchronized">Componentes</p>
    CLASS-METHODS get_struct_components_recus
      IMPORTING is_structure         TYPE any
      RETURNING VALUE(rt_components) TYPE cl_abap_structdescr=>component_table .
  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Determina como obtener los components</p>
    "! Útil cuando la estructura tiene includes
    "! @parameter is_component | <p class="shorttext synchronized">Tipo de objeto del componente</p>
    "! @parameter ct_components | <p class="shorttext synchronized">Componentes</p>
    CLASS-METHODS structure_comp_case
      IMPORTING
        is_component  TYPE abap_componentdescr
      CHANGING
        ct_components TYPE abap_component_tab.
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

  METHOD get_struct_components_recus.
    CLEAR rt_components.

    " Obtención de los componentes. Primero probamos por el valor y luego por nombre
    TRY.
        DATA(lo_struct) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_structure ) ).
      CATCH cx_root.
        lo_struct = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( is_structure ) ).
    ENDTRY.

    IF lo_struct IS BOUND.
      TRY.

          DATA(lt_components) = lo_struct->get_components(  ).

          LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>) WHERE name IS NOT INITIAL.
            structure_comp_case( EXPORTING is_component = <ls_components>
                                 CHANGING ct_components = rt_components ).
          ENDLOOP.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD structure_comp_case.
    CASE is_component-type->kind.
      WHEN cl_abap_typedescr=>kind_elem. "E Elementary Type
        INSERT is_component INTO TABLE ct_components.
      WHEN cl_abap_typedescr=>kind_table. "T Table
        INSERT is_component INTO TABLE ct_components.
      WHEN cl_abap_typedescr=>kind_struct. "S Structure
        DATA(lv_name) = is_component-type->get_relative_name( ).
        DATA(lt_comp_str) = get_struct_components_recus( is_structure = lv_name ).
        INSERT LINES OF lt_comp_str INTO TABLE ct_components.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

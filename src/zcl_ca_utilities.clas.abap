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
    "! <p class="shorttext synchronized">Clona los valores de una estructura a otra</p>
    "! Útil cuando la estructura campos que a su vez son tablas internas o estructuras
    "! @parameter is_source_struc | <p class="shorttext synchronized">Estructura de origen</p>
    "! @parameter it_components | <p class="shorttext synchronized">Componentes que se quieren clonar</p>
    "! @parameter iv_ignore_itab | <p class="shorttext synchronized">Ignora campos que son tablas internas</p>
    "! @parameter cs_destiny_struc | <p class="shorttext synchronized">Estructura destino</p>
    CLASS-METHODS clone_structure_values
      IMPORTING is_source      TYPE any
                it_components  TYPE cl_abap_structdescr=>component_table  OPTIONAL
                iv_ignore_itab TYPE sap_bool DEFAULT abap_false
      CHANGING  cs_destiny     TYPE any.
    "! <p class="shorttext synchronized">Clona los valores de una tabla a otra</p>
    "! Útil cuando la estructura campos que a su vez son tablas internas
    "! @parameter it_source | <p class="shorttext synchronized">Estructura de origen</p>
    "! @parameter iv_ignore_itab | <p class="shorttext synchronized">Ignora campos que son tablas internas</p>
    "! @parameter cs_destiny | <p class="shorttext synchronized">Estructura destino</p>
    CLASS-METHODS clone_table_values
      IMPORTING
        it_source      TYPE STANDARD TABLE
        iv_ignore_itab TYPE sap_bool DEFAULT abap_false
      CHANGING
        ct_destiny     TYPE STANDARD TABLE.

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

          LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>).
            IF <ls_components>-name IS NOT INITIAL.
              structure_comp_case( EXPORTING is_component = <ls_components>
                                   CHANGING ct_components = rt_components ).
            ELSE. " En blanco es cuando hay un include type o similar en la estructura
              " Aun así valido que sea estructura
              IF <ls_components>-type->kind = cl_abap_typedescr=>kind_struct.
                structure_comp_case( EXPORTING is_component = <ls_components>
                                   CHANGING ct_components = rt_components ).
              ENDIF.
            ENDIF.
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

  METHOD clone_structure_values.
    DATA lt_components TYPE cl_abap_structdescr=>component_table .

    " Si los componentes se pasán por parámetro serán estos lo que se usen para el mapeo.
    IF it_components IS NOT INITIAL.
      lt_components = it_components.
    ELSE.
      "lt_components = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_source ) )->get_components(  ).
      lt_components = get_struct_components_recus( is_structure = is_source ).
    ENDIF.

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>).
      ASSIGN COMPONENT <ls_components>-name OF STRUCTURE is_source TO FIELD-SYMBOL(<source>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT <ls_components>-name OF STRUCTURE cs_destiny TO FIELD-SYMBOL(<destiny>).
        IF sy-subrc = 0.

          CASE <ls_components>-type->kind.
            WHEN cl_abap_typedescr=>kind_table. "T Table
            " Los campos que son tablas internas se ignora si se indica por parámetro
              IF iv_ignore_itab = abap_false.
                clone_table_values( EXPORTING it_source = <source>
                                    CHANGING ct_destiny = <destiny> ).
              ENDIF.
            WHEN cl_abap_typedescr=>kind_struct. "S Structure
              clone_structure_values( EXPORTING is_source  = <source>
                                      CHANGING cs_destiny = <destiny> ).
            WHEN OTHERS.
              <destiny> = <source>.
          ENDCASE.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD clone_table_values.
    " El clonado de valores de las tablas es recorre el origen añadir un registro en blanco
    " en el destino y usar el método que clona estructuras para mover los valores.

    IF it_source IS NOT INITIAL.

      " Obtengo los componentes del primer registro para que en el clonado de estructura sea más optimo
      " a no tener que volver a buscarlos.
      DATA(lt_components) = get_struct_components_recus( it_source[ 1 ] ).


      LOOP AT it_source ASSIGNING FIELD-SYMBOL(<ls_source>).
        APPEND INITIAL LINE TO ct_destiny ASSIGNING FIELD-SYMBOL(<ls_destiny>).

        clone_structure_values( EXPORTING is_source  = <ls_source>
                                          it_components = lt_components
                                          iv_ignore_itab = iv_ignore_itab
                                          CHANGING cs_destiny = <ls_destiny> ).

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

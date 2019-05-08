CLASS zcl_ca_archivelink DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_CA_ARCHIVELINK
*"* do not include other source files here!!!
    TYPE-POOLS abap .

    TYPES:
      BEGIN OF ts_object_tbl_enl,
        sap_object TYPE saeanwdid,
        connection TYPE saeverkn,
      END OF ts_object_tbl_enl .
    TYPES:
      tt_object_tbl_enl TYPE STANDARD TABLE OF ts_object_tbl_enl .
    TYPES:
      BEGIN OF ts_key_gos,
        foltp     TYPE so_fol_tp,
        folyr     TYPE so_fol_yr,
        folno     TYPE so_fol_no,
        objtp     TYPE so_obj_tp,
        objyr     TYPE so_obj_yr,
        objno     TYPE so_obj_no,
        forwarder TYPE so_usr_nam,
      END OF ts_key_gos .
    TYPES:
      BEGIN OF ty_tmp_content,
        appl      TYPE zca_t_tmp_conten-appl,
        contenido TYPE zca_t_tmp_conten-contenido,
        type      TYPE zca_t_tmp_conten-type,
        name      TYPE zca_t_tmp_conten-name,
        extension TYPE zca_t_tmp_conten-extension,
      END OF ty_tmp_content .
    TYPES:
      BEGIN OF ty_conf_alink,
        sap_object TYPE saeanwdid,
        ar_object  TYPE saeobjart,
        ar_status  TYPE saearstat2,
        archiv_id  TYPE saearchivi,
        cont_cat   TYPE sdok_stcad,
        doc_type   TYPE saedoktyp,
      END OF ty_conf_alink .
    TYPES:
      tt_bapicompon TYPE STANDARD TABLE OF bapicompon .
    TYPES:
      tt_bapisignat TYPE STANDARD TABLE OF bapisignat .

    CONSTANTS dc_pref_tmp_attach TYPE string VALUE 'TMP-' ##NO_TEXT.
    CONSTANTS lc_doc_type_gos TYPE zca_e_doc_type VALUE 'G' ##NO_TEXT.
    CONSTANTS lc_doc_type_archivelink TYPE zca_e_doc_type VALUE 'A' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !i_langu TYPE sy-langu DEFAULT sy-langu .
    METHODS upload_file_tmp
      IMPORTING
        !is_attach   TYPE zca_s_attach
      EXPORTING
        !e_id_attach TYPE bsstring .
    CLASS-METHODS get_attachs_http
      IMPORTING
        !io_server  TYPE REF TO if_http_server
      EXPORTING
        !e_t_attach TYPE zca_i_attach .
    METHODS conv_tmp_file_2_alink
      IMPORTING
        !i_object_id            TYPE any
        !i_tmp_id               TYPE any
        !i_object_alink         TYPE saeanwdid
        !i_pref_ar_object       TYPE saeobjart
        !i_dont_delete_tmp_file TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !e_alink_id             TYPE saeardoid
        !e_return               TYPE bapiret2 .
    CLASS-METHODS get_url_image
      IMPORTING
        !i_alink_id     TYPE any
        !i_object_alink TYPE saeanwdid
      RETURNING
        VALUE(r_url)    TYPE string .
    METHODS delete_file_alink
      IMPORTING
        !i_alink_id       TYPE saeardoid
        !iv_ar_object     TYPE saeobjart OPTIONAL
        !iv_object_id     TYPE saeobjid OPTIONAL
        !i_object_alink   TYPE saeanwdid
      RETURNING
        VALUE(r_t_return) TYPE bapiret2_t .
    METHODS upload_file_2_alink
      IMPORTING
        !iv_filename       TYPE string
        !iv_length         TYPE num12
        !iv_file_data_bin  TYPE rmps_t_1024
        !iv_object_id      TYPE any
        !iv_object_alink   TYPE saeanwdid
        !iv_pref_ar_object TYPE saeobjart
        !iv_track          TYPE boolean DEFAULT abap_false
        !iv_descr          TYPE toaat-descr OPTIONAL
        !iv_creator        TYPE toaat-creator DEFAULT sy-uname
      EXPORTING
        !e_alink_id        TYPE saeardoid
        !e_ar_object       TYPE saeobjart
        !e_archiv_id       TYPE saearchivi
        !e_doc_type        TYPE saedoktyp
        !e_return          TYPE bapiret2 .
    CLASS-METHODS get_url_file_alink
      IMPORTING
        !i_alink_id   TYPE any
        !i_sap_object TYPE saeanwdid
        !i_arc_doc_id TYPE saeardoid OPTIONAL
        !i_archiv_id  TYPE saearchivi OPTIONAL
        !iv_local_url TYPE sap_bool DEFAULT abap_false
        !iv_ar_object TYPE saeobjart OPTIONAL
        !iv_http      TYPE sap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_url)  TYPE string .
    CLASS-METHODS get_url_object_gos
      IMPORTING
        !is_key       TYPE ts_key_gos
      RETURNING
        VALUE(rv_url) TYPE bsstring .
    CLASS-METHODS extract_filename_head
      IMPORTING
        !it_objhead        TYPE soli_tab
      RETURNING
        VALUE(rv_filename) TYPE bsstring .
    METHODS get_alink_file_data
      IMPORTING
        !iv_alink_id   TYPE saeardoid
        !iv_sap_object TYPE saeanwdid
        !iv_object_id  TYPE saeobjid
        !iv_ar_object  TYPE saeobjart
      EXPORTING
        !ev_content    TYPE xstring
        !ev_length     TYPE i
        !ev_filename   TYPE any
        !ev_mimetype   TYPE any
        !ev_doc_type   TYPE any
      EXCEPTIONS
        error_parameter
        no_data .
    METHODS get_object_content_gos
      IMPORTING
        !is_key      TYPE ts_key_gos
      EXPORTING
        !ev_content  TYPE xstring
        !ev_length   TYPE i
        !ev_mimetype TYPE any
        !ev_filename TYPE any
        !ev_doc_type TYPE any .
    CLASS-METHODS get_filename_object_gos
      IMPORTING
        !is_key            TYPE ts_key_gos
      RETURNING
        VALUE(rv_filename) TYPE bsstring .
    METHODS get_gos_document_list
      IMPORTING
        !iv_object_id     TYPE saeobjid
        !iv_sap_object    TYPE saeanwdid
        !iv_http_protocol TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !et_gos_documents TYPE zca_i_gos_doc_list
        !es_return        TYPE bapiret2 .
    METHODS get_alink_document_list
      IMPORTING
        !iv_object_id    TYPE saeobjid
        !iv_sap_object   TYPE saeanwdid
        !iv_ar_object    TYPE saeobjart OPTIONAL
        !it_arc_doc_id   TYPE table OPTIONAL
        !iv_local_url    TYPE sap_bool DEFAULT abap_false
        !iv_http         TYPE sap_bool DEFAULT abap_false
        !iv_get_url      TYPE sap_bool DEFAULT abap_true
      EXPORTING
        !et_al_documents TYPE zca_i_archivelink_doc_list
        !es_return       TYPE bapiret2 .
    METHODS get_document_list
      IMPORTING
        !iv_object_id       TYPE saeobjid
        !iv_sap_object      TYPE saeanwdid
        !iv_local_url_alink TYPE sap_bool DEFAULT abap_false
        !iv_http            TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !et_document_list   TYPE zca_i_doc_list
        !es_return          TYPE bapiret2 .
    METHODS get_document_data
      IMPORTING
        !iv_doc_id     TYPE zca_e_doc_id
        !iv_sap_object TYPE saeanwdid
        !iv_object_id  TYPE saeobjid
        !iv_ar_object  TYPE saeobjart OPTIONAL
      EXPORTING
        !ev_content    TYPE xstring
        !ev_length     TYPE i
        !ev_mimetype   TYPE any
        !ev_filename   TYPE any
        !ev_doc_type   TYPE any
        !es_return     TYPE bapiret2 .
    METHODS delete_document
      IMPORTING
        !iv_doc_id     TYPE zca_e_doc_id
        !iv_sap_object TYPE saeanwdid
        !iv_object_id  TYPE saeobjid
        !iv_ar_object  TYPE saeobjart OPTIONAL
      EXPORTING
        !es_return     TYPE bapiret2 .
    METHODS delete_file_gos
      IMPORTING
        !is_key        TYPE zcl_ca_archivelink=>ts_key_gos
        !iv_sap_object TYPE saeanwdid
        !iv_object_id  TYPE saeobjid
      EXPORTING
        !es_return     TYPE bapiret2 .
    METHODS generate_gos_url
      IMPORTING
        !is_key           TYPE zcl_ca_archivelink=>ts_key_gos
        !iv_http_protocol TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !ev_url           TYPE string
        !es_return        TYPE bapiret2 .
    METHODS delete_file_tmp
      IMPORTING
        !iv_appl   TYPE zca_e_appl
        !iv_id     TYPE zca_e_id_mime
      EXPORTING
        !es_return TYPE bapiret2 .
    METHODS get_file_temp
      IMPORTING
        !iv_appl    TYPE zca_e_appl
        !iv_id      TYPE zca_e_id_mime
        !iv_get_url TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !es_return  TYPE bapiret2
        !es_attach  TYPE zca_t_tmp_conten
        !ev_url     TYPE string .
    METHODS upload_file_2_gos
      IMPORTING
        !is_attach     TYPE zca_t_tmp_conten
        !iv_sap_object TYPE saeanwdid
        !iv_object_id  TYPE saeobjid
        !iv_langu      TYPE langu DEFAULT sy-langu
        !iv_commit     TYPE sap_bool DEFAULT abap_true
      EXPORTING
        !es_return     TYPE bapiret2 .
    METHODS get_bds_document_list
      IMPORTING
        !iv_object_id     TYPE saeobjid
        !iv_sap_object    TYPE saeanwdid
        !iv_http_protocol TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !et_components    TYPE tt_bapicompon
        !et_signature     TYPE tt_bapisignat
      CHANGING
        !ct_document_list TYPE zca_i_doc_list .
  PROTECTED SECTION.
*"* protected components of class ZCL_CA_ARCHIVELINK
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF ty_toa,
        sap_object TYPE saeanwdid,
        object_id  TYPE saeobjid,
        archiv_id  TYPE saearchivi,
        arc_doc_id TYPE saeardoid,
        ar_object  TYPE saeobjart,
        ar_date    TYPE saeabadate,
        del_date   TYPE saedeldate,
        reserve    TYPE saereserve,
      END OF ty_toa .
    TYPES:
      tt_connection TYPE TABLE OF saeverkn .
    TYPES:
      BEGIN OF ts_toa_document,
        archive_id TYPE saearchivi,
        arc_doc_id TYPE saeardoid,
        ar_object  TYPE saeobjart,
        ar_date    TYPE saeabadate,
        reserve    TYPE saereserve,
        object_id  TYPE toa01-object_id,
        filename   TYPE char255,
        creator    TYPE syuname,
        descr      TYPE c LENGTH 60,
        creatime   TYPE sytime,
      END OF ts_toa_document .
    TYPES:
      tt_toa_document TYPE TABLE OF ts_toa_document .

    TYPES: BEGIN OF ts_toaom,
             connection TYPE toaom-connection,
           END OF ts_toaom.
    TYPES tt_toaom TYPE STANDARD TABLE OF ts_toaom.

    CONSTANTS dc_msg_type_error TYPE bapi_mtype VALUE 'E'.  "#EC NOTEXT
    CONSTANTS dc_ca_id TYPE symsgid VALUE 'ZCA'.            "#EC NOTEXT
    DATA d_langu TYPE sylangu .
  PRIVATE SECTION.
*"* private components of class ZCL_CA_ARCHIVELINK
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_ca_archivelink IMPLEMENTATION.


  METHOD constructor.
    d_langu = i_langu.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD conv_tmp_file_2_alink.

    FIELD-SYMBOLS <ls_binary> TYPE tbl1024.
    DATA ls_tmp_content TYPE ty_tmp_content.
    DATA lt_binary TYPE TABLE OF tbl1024.
    DATA ld_doc_type TYPE toave-doc_type.
    DATA ls_config_alink TYPE ty_conf_alink.
    DATA ld_ar_object TYPE saeobjart.
    DATA ld_doc_id TYPE saeardoid.
    DATA ld_object_id TYPE saeobjid.
    DATA ld_tmp_id TYPE string.
    DATA ld_length TYPE num12.
    DATA ld_length_tmp TYPE i.
    DATA ld_filename TYPE toaat-filename.

    CLEAR: e_return, e_alink_id.

* Le quito el prefijo temporal para poder buscarlo en la tabla
    ld_tmp_id = i_tmp_id.
    REPLACE ALL OCCURRENCES OF dc_pref_tmp_attach IN ld_tmp_id WITH ''.

    SELECT SINGLE appl contenido type name extension INTO ls_tmp_content
    FROM zca_t_tmp_conten
    WHERE id = ld_tmp_id.


    IF sy-subrc = 0.

* Monto la clase de documento donde irá el fichero.
      CONCATENATE i_pref_ar_object ls_tmp_content-extension INTO ld_ar_object.
      TRANSLATE ld_ar_object TO UPPER CASE.

* Busco la configuración del archivado
      SELECT SINGLE sap_object ar_object ar_status archiv_id cont_cat
                    doc_type INTO ls_config_alink
      FROM toaom
      WHERE sap_object = i_object_alink
      AND ar_object  = ld_ar_object
      AND ar_status = abap_true.
      IF sy-subrc = 0.

* Convierto el xstring a binario
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = ls_tmp_content-contenido
          IMPORTING
            output_length = ld_length_tmp
          TABLES
            binary_tab    = lt_binary.

*      LOOP AT lt_binary ASSIGNING <ls_binary>.
*        DESCRIBE FIELD <ls_binary> LENGTH ld_length_tmp IN BYTE MODE.
*        ld_length = ld_length + ld_length_tmp.
*      ENDLOOP.

        ld_length = ld_length_tmp.

* Creación de id del archivo
        CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
          EXPORTING
            archiv_id                = ls_config_alink-archiv_id
            document_type            = ls_tmp_content-extension
            length                   = ld_length
          IMPORTING
            archiv_doc_id            = ld_doc_id
          TABLES
            binarchivobject          = lt_binary
          EXCEPTIONS
            error_archiv             = 1
            error_communicationtable = 2
            error_kernel             = 3
            blocked_by_policy        = 4
            OTHERS                   = 5.
        IF sy-subrc = 0.

          ld_object_id = i_object_id. " ID que tendra el archivado(numero magazine, etc.)
          ld_filename  = ls_tmp_content-name.

* Finalmente se guarda el documento
          CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
            EXPORTING
              archiv_id             = ls_config_alink-archiv_id
              arc_doc_id            = ld_doc_id
              ar_object             = ls_config_alink-ar_object
              object_id             = ld_object_id
              sap_object            = ls_config_alink-sap_object
              doc_type              = ls_tmp_content-extension
              filename              = ld_filename
            EXCEPTIONS
              error_connectiontable = 1
              OTHERS                = 2.
          IF sy-subrc = 0.

            e_alink_id = ld_doc_id.

* Finalmente borro el archivo temporal
            IF i_dont_delete_tmp_file EQ abap_false.
              DELETE FROM zca_t_tmp_conten
              WHERE id = ld_tmp_id.
            ENDIF.
          ELSE.
            e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                       iv_id = sy-msgid
                                                       iv_number = sy-msgno
                                                       iv_langu = d_langu
                                                       iv_message_v1 = sy-msgv1
                                                       iv_message_v2 = sy-msgv2
                                                       iv_message_v3 = sy-msgv3
                                                       iv_message_v4 = sy-msgv4 ).
          ENDIF.

        ELSE.
          e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                     iv_id = sy-msgid
                                                     iv_number = sy-msgno
                                                     iv_langu = d_langu
                                                     iv_message_v1 = sy-msgv1
                                                     iv_message_v2 = sy-msgv2
                                                     iv_message_v3 = sy-msgv3
                                                     iv_message_v4 = sy-msgv4 ).
        ENDIF.

      ELSE.
        e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                   iv_id = dc_ca_id
                                                   iv_number = '002'
                                                   iv_langu = d_langu
                                                   iv_message_v1 = ls_tmp_content-appl ).
      ENDIF.
    ELSE.
      e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                 iv_id = dc_ca_id
                                                 iv_number = '001'
                                                 iv_langu = d_langu
                                                 iv_message_v1 = i_tmp_id ).
    ENDIF.


  ENDMETHOD.                    "conv_tmp_file_2_alink


  METHOD delete_document.

    DATA: ld_alink_id TYPE saeardoid,
          ls_key_gos  TYPE zcl_ca_archivelink=>ts_key_gos,
          lt_bapiret  TYPE TABLE OF bapiret2.

    CLEAR: es_return.

* Es un anexo del Gos
    IF iv_doc_id(3) = 'FOL'.

      ls_key_gos = iv_doc_id.
      CALL METHOD delete_file_gos
        EXPORTING
          is_key        = ls_key_gos
          iv_sap_object = iv_sap_object
          iv_object_id  = iv_object_id
        IMPORTING
          es_return     = es_return.

* Es unn anexo de Archivelink
    ELSE.

      IF iv_ar_object IS NOT INITIAL.
        ld_alink_id = iv_doc_id.
        CALL METHOD delete_file_alink
          EXPORTING
            i_alink_id     = ld_alink_id
            i_object_alink = iv_sap_object
            iv_object_id   = iv_object_id
            iv_ar_object   = iv_ar_object
          RECEIVING
            r_t_return     = lt_bapiret.

        IF lt_bapiret IS NOT INITIAL.
          READ TABLE lt_bapiret INTO es_return INDEX 1.
        ENDIF.

      ELSE.
        es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                    iv_id = dc_ca_id
                                                    iv_number = '010'
                                                    iv_langu = d_langu  ).
      ENDIF.
    ENDIF.


  ENDMETHOD.                    "delete_document


  METHOD delete_file_alink.

**************************************************************************************************
*  Se modifica la interfaz de este metodo para poder eliminar documentos de otras tablas TOA sin *
*  interferir en el funcionamiento actual                                                        *
**************************************************************************************************

    DATA ls_toa TYPE ty_toa.
    DATA ls_return TYPE bapiret2.
    DATA ls_toaom TYPE ts_toaom.

    DATA: lv_connection TYPE saeverkn.

    CLEAR r_t_return.

* INS 31102017
    IF iv_ar_object IS SUPPLIED AND
       iv_object_id IS SUPPLIED AND
       iv_ar_object IS NOT INITIAL AND
       iv_object_id IS NOT INITIAL.

      SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_toaom
        FROM toaom
        WHERE sap_object EQ i_object_alink AND
              ar_object EQ iv_ar_object.

      IF sy-subrc IS INITIAL.
        CLEAR ls_toa.
        SELECT SINGLE sap_object object_id archiv_id arc_doc_id ar_object ar_date del_date reserve
          INTO ls_toa
          FROM (ls_toaom-connection)
          WHERE sap_object EQ i_object_alink AND
                arc_doc_id EQ i_alink_id AND
                object_id  EQ iv_object_id.

      ENDIF.
    ELSE.
* INS 31102017

      SELECT SINGLE sap_object object_id archiv_id arc_doc_id ar_object ar_date del_date reserve
             INTO ls_toa
             FROM toa01
             WHERE sap_object = i_object_alink
               AND arc_doc_id = i_alink_id. "object_id = i_alink_id.

    ENDIF. " INS 31102017

*  IF sy-subrc = 0. "DEL 31102017
    IF ls_toa IS NOT INITIAL. "INS 31112017

      CALL FUNCTION 'ARCHIV_DELETE_META'
        EXPORTING
          archiv_id                = ls_toa-archiv_id
          arc_doc_id               = ls_toa-arc_doc_id
          ar_object                = ls_toa-ar_object
          delete_flag              = 2 "Borrar link + archivo fís sin popup
          object_id                = ls_toa-object_id
          sap_object               = ls_toa-sap_object
          single_entry             = abap_true
          no_auth_check            = abap_true
*         IMPORTING
*         ALL_CONNECTIONS_DELETED  =
        EXCEPTIONS
          error_connectiontable    = 1
          error_parameter          = 2
          error_archiv             = 3
          error_kernel             = 4
          error_communicationtable = 5
          error_authority          = 6
          OTHERS                   = 7.

      IF sy-subrc <> 0.
        ls_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                     iv_id = sy-msgid
                                                     iv_number = sy-msgno
                                                     iv_langu = d_langu
                                                     iv_message_v1 = sy-msgv1
                                                     iv_message_v2 = sy-msgv2
                                                     iv_message_v3 = sy-msgv3
                                                     iv_message_v4 = sy-msgv4 ).
        INSERT ls_return INTO TABLE r_t_return.
      ENDIF.

    ELSE.
      ls_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                        iv_id = dc_ca_id
                                        iv_number = '001'
                                        iv_langu = d_langu
                                        iv_message_v1 = i_alink_id ).
      INSERT ls_return INTO TABLE r_t_return.
    ENDIF.
  ENDMETHOD.                    "delete_file_alink


  METHOD delete_file_gos.

    DATA: ls_folder_id TYPE soodk,
          ls_object_id TYPE soodk,
          ls_object    TYPE sibflporb,
          ls_object_a  TYPE sibflporb,
          ls_object_b  TYPE sibflporb,
          lv_objkey    TYPE swo_typeid,
          lt_relat     TYPE obl_t_relt,
          ls_relat     TYPE obl_s_relt,
          lt_links     TYPE obl_t_link,
          lv_rel_typ   TYPE char4.

    CONSTANTS: lc_url  TYPE string VALUE 'URL',
               lc_raw  TYPE string VALUE 'RAW',
               lc_note TYPE string VALUE 'NOTE',
               lc_ext  TYPE string VALUE 'EXT',
               lc_atta TYPE string VALUE 'ATTA'.
    FIELD-SYMBOLS: <ls_link> LIKE LINE OF lt_links.

* Se obtiene el tipo de relacion
    CASE is_key-objtp.

      WHEN lc_url. "--> Url
        lv_rel_typ = lc_url.

      WHEN lc_raw. "--> Nota
        lv_rel_typ = lc_note.

      WHEN lc_ext. "--> Anexo
        lv_rel_typ = lc_atta.
      WHEN OTHERS.

    ENDCASE.

    ls_object-instid = iv_object_id.
    ls_object-typeid = iv_sap_object.
    ls_object-catid  = 'BO'.

    ls_relat-sign   = 'I'.
    ls_relat-option = 'EQ'.
    ls_relat-low    = lv_rel_typ.
    APPEND ls_relat TO lt_relat.

* Se leen todos los enlaces del tipo del documento que se quiere borrar
    CALL METHOD cl_binary_relation=>read_links
      EXPORTING
        is_object           = ls_object
        it_relation_options = lt_relat
      IMPORTING
        et_links            = lt_links.

    lv_objkey = is_key.

    READ TABLE lt_links ASSIGNING <ls_link> WITH KEY instid_b = lv_objkey.
* Se borra el enlace
    IF sy-subrc IS INITIAL.

      ls_object_a-instid = <ls_link>-instid_a.
      ls_object_a-typeid = <ls_link>-typeid_a.
      ls_object_a-catid  = <ls_link>-catid_a.
      ls_object_b-instid = <ls_link>-instid_b.
      ls_object_b-typeid = <ls_link>-typeid_b.
      ls_object_b-catid  = <ls_link>-catid_b.

      CALL METHOD cl_binary_relation=>delete_link
        EXPORTING
          is_object_a = ls_object_a
          is_object_b = ls_object_b
          ip_reltype  = <ls_link>-reltype.

      ls_folder_id-objtp = is_key-foltp.
      ls_folder_id-objyr = is_key-folyr.
      ls_folder_id-objno = is_key-folno.
      ls_object_id-objtp = is_key-objtp.
      ls_object_id-objyr = is_key-objyr.
      ls_object_id-objno = is_key-objno.

*   Se borra el documento fisico
      CALL FUNCTION 'SO_OBJECT_DELETE'
        EXPORTING
          folder_id                  = ls_folder_id
          object_id                  = ls_object_id
        EXCEPTIONS
          communication_failure      = 1
          folder_not_empty           = 2
          folder_not_exist           = 3
          folder_no_authorization    = 4
          forwarder_not_exist        = 5
          object_not_exist           = 6
          object_no_authorization    = 7
          operation_no_authorization = 8
          owner_not_exist            = 9
          substitute_not_active      = 10
          substitute_not_defined     = 11
          system_failure             = 12
          x_error                    = 13
          OTHERS                     = 14.

      IF sy-subrc IS NOT INITIAL.
*     Error eliminando el documento
        es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                    iv_id = dc_ca_id
                                                    iv_number = '024'
                                                    iv_langu = d_langu  ).
      ENDIF.

    ELSE.
*   Error
      es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                    iv_id = dc_ca_id
                                                    iv_number = '024'
                                                    iv_langu = d_langu  ).
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.                    "delete_file_gos


  METHOD delete_file_tmp.

    IF iv_appl IS INITIAL OR
       iv_id   IS INITIAL.

      es_return = zcl_ca_utilities=>fill_return( iv_type     = dc_msg_type_error
                                                  iv_id       = dc_ca_id
                                                  iv_number   = '052' ).
      EXIT.
    ENDIF.

    DELETE FROM zca_t_tmp_conten
      WHERE appl EQ iv_appl AND
            id   EQ iv_id.

    IF sy-subrc IS NOT INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type       = dc_msg_type_error
                                                  iv_id         = dc_ca_id
                                                  iv_number     = '052' ).
      EXIT.
    ENDIF.

  ENDMETHOD.                    "delete_file_tmp


  METHOD extract_filename_head.
    DATA ls_objhead TYPE soli.


* Busco el tag de nombre de fichero y lo quito para obtener el nombre del fichero
    LOOP AT it_objhead INTO ls_objhead WHERE line CS '&SO_FILENAME='.
      REPLACE ALL OCCURRENCES OF '&SO_FILENAME=' IN ls_objhead-line WITH '' .
      rv_filename = ls_objhead-line.
      EXIT.
    ENDLOOP.

  ENDMETHOD.                    "extract_filename_head


  METHOD generate_gos_url.

    DATA: lv_content  TYPE xstring,
          lv_mimetype TYPE string,
          lv_filename TYPE string,
          lv_doc_type TYPE string,
          lv_service  TYPE string.

    get_object_content_gos(
     EXPORTING
      is_key      = is_key
     IMPORTING
      ev_content  = lv_content
      ev_mimetype = lv_mimetype
      ev_filename = lv_filename
      ev_doc_type = lv_doc_type ).

    " Obtengo las partes del fichero.
    SPLIT lv_filename AT '.' INTO TABLE DATA(lt_values).

    " La extensión estará en el último registro,
    DESCRIBE TABLE lt_values.
    READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<ls_value>) INDEX sy-tfill.
    IF sy-subrc = 0.
      " Si el tipo del fichero no esta como substring en el último registro le añado la extensión.
      IF <ls_value> NS lv_doc_type.
        lv_filename = |{ lv_filename }.{ lv_doc_type }|.
      ENDIF.
    ELSE.
      lv_filename = |{ lv_filename }.{ lv_doc_type }|.
    ENDIF.

    lv_service = |{ is_key-foltp }{ is_key-folyr }{ is_key-folno }{ is_key-objtp }{ is_key-objyr }{ is_key-objno }|.

    zcl_ca_http=>generate_tmp_url(
      EXPORTING
        iv_content  = lv_content
        iv_mimetype = lv_mimetype
        iv_service  = lv_service
        iv_doc_name = lv_filename
        iv_http = iv_http_protocol
      IMPORTING
        ev_url     = ev_url
        es_return  = es_return ).

  ENDMETHOD.                    "generate_gos_url


  METHOD get_alink_document_list.


    DATA: lt_toaom      TYPE tt_toaom,
          lv_join       TYPE string,
          lt_enlace     TYPE tt_toa_document,
          lr_ar_object  TYPE RANGE OF saeobjart,
          ls_ar_object  LIKE LINE OF lr_ar_object,
          lr_arc_doc_id TYPE RANGE OF saeardoid,
          ls_arc_doc_id LIKE LINE OF lr_arc_doc_id.

    FIELD-SYMBOLS: <ls_toaom>       TYPE LINE OF tt_toaom,
                   <ls_enlace>      TYPE ts_toa_document,
                   <ls_al_document> TYPE zca_s_archivelink_doc_list,
                   <lv_arc_doc_id>  TYPE saeardoid.

    CONSTANTS: lc_join        TYPE string VALUE 'AS a LEFT OUTER JOIN toaat AS b ON a~ARC_DOC_ID = b~ARC_DOC_ID'.

    CLEAR: et_al_documents[],
           es_return.

    IF iv_object_id IS INITIAL OR
       iv_sap_object IS INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                  iv_id = dc_ca_id
                                                  iv_number = '022'
                                                  iv_langu = d_langu  ).
      EXIT.
    ENDIF.

    IF iv_ar_object IS NOT INITIAL.
      ls_ar_object-sign = 'I'.
      ls_ar_object-option = 'EQ'.
      ls_ar_object-low = iv_ar_object.
      APPEND ls_ar_object TO lr_ar_object.
    ENDIF.

    IF it_arc_doc_id IS NOT INITIAL.
      LOOP AT it_arc_doc_id ASSIGNING <lv_arc_doc_id>.
        ls_arc_doc_id-sign = 'I'.
        ls_arc_doc_id-option = 'EQ'.
        ls_arc_doc_id-low = <lv_arc_doc_id>.
        APPEND ls_arc_doc_id TO lr_arc_doc_id.
      ENDLOOP.
    ENDIF.
* Se recuperan las tablas de enlace. Por culpa del campo CONNECTION tengo que usar esta manera para acceder a la
* base de datos. Ya que antes con una variable se podia arreglar, pero desde el EHP8 ni eso funciona.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_toaom
      FROM toaom
      WHERE sap_object EQ iv_sap_object AND
            ar_object  IN lr_ar_object.

    IF lt_toaom IS INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                  iv_id = dc_ca_id
                                                  iv_number = '021'
                                                  iv_langu = d_langu ).
    ENDIF.

    SORT lt_toaom DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_toaom.

    LOOP AT lt_toaom ASSIGNING <ls_toaom>.

      CONCATENATE <ls_toaom>-connection lc_join INTO lv_join SEPARATED BY space.
      CONCATENATE '(' lv_join ')' INTO lv_join.

*   Se recuperan los documentos
      SELECT a~archiv_id a~arc_doc_id a~ar_object a~ar_date a~reserve a~object_id
             b~filename b~creator b~descr b~creatime
        INTO TABLE lt_enlace
        FROM (lv_join)
        WHERE a~sap_object    = iv_sap_object AND
              a~object_id     = iv_object_id AND
              a~arc_doc_id IN lr_arc_doc_id.

*   Se borran los documentos que no sean del ar_object
      IF iv_ar_object IS SUPPLIED AND
         iv_ar_object IS NOT INITIAL.
        LOOP AT lt_enlace ASSIGNING <ls_enlace> WHERE ar_object NE iv_ar_object.
          DELETE lt_enlace.
        ENDLOOP.
      ENDIF.

      LOOP AT lt_enlace ASSIGNING <ls_enlace>.
        APPEND INITIAL LINE TO et_al_documents ASSIGNING <ls_al_document>.
        <ls_al_document>-arc_doc_id = <ls_enlace>-arc_doc_id.
        <ls_al_document>-archiv_id  = <ls_enlace>-archive_id.
        " Si no hay nombre fichero porque no existe en la TOAAT le pongo el ID del fichero y la extensión que hay en el campo reserve
        IF <ls_enlace>-filename IS INITIAL.
          <ls_al_document>-filename  = |{ <ls_enlace>-object_id }.{ <ls_enlace>-reserve }|.
        ELSE.
          <ls_al_document>-filename  = <ls_enlace>-filename.
        ENDIF.
        <ls_al_document>-descr     = <ls_enlace>-descr.
        <ls_al_document>-ar_object = <ls_enlace>-ar_object.
        <ls_al_document>-creator   = <ls_enlace>-creator.
        <ls_al_document>-creadate  = <ls_enlace>-ar_date.
        <ls_al_document>-creatime  = <ls_enlace>-creatime.
        <ls_al_document>-reserve   = <ls_enlace>-reserve.

        IF iv_get_url EQ abap_true.

          <ls_al_document>-url = zcl_ca_archivelink=>get_url_file_alink( i_alink_id = <ls_enlace>-object_id
                                                                         i_sap_object = iv_sap_object
                                                                         i_arc_doc_id = <ls_enlace>-arc_doc_id
                                                                         i_archiv_id =  <ls_enlace>-archive_id
                                                                         iv_local_url = iv_local_url
                                                                         iv_ar_object = <ls_enlace>-ar_object
                                                                         iv_http      = iv_http ).

        ENDIF.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.                    "get_alink_document_list


  METHOD get_alink_file_data.

    FIELD-SYMBOLS <ls_object_tbl_enl> TYPE LINE OF tt_object_tbl_enl.
    DATA lt_object_tbl_enl TYPE tt_object_tbl_enl.
    DATA lt_r_object_id TYPE RANGE OF saeobjid.
    DATA ls_r_object_id LIKE LINE OF lt_r_object_id.
    DATA lt_r_alink_id TYPE RANGE OF saeardoid.
    DATA ls_r_alink_id LIKE LINE OF lt_r_alink_id.
    DATA ls_toa TYPE ty_toa.
    DATA ld_length TYPE sapb-length.
    DATA lt_content TYPE STANDARD TABLE OF tbl1024.
    DATA ld_length_int TYPE i.
    DATA: ld_doc_type TYPE toadd-doc_type,
          ls_toaom    TYPE ts_toaom.

    CLEAR: ev_content, ev_length, ev_mimetype, ev_doc_type.


    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_toaom
      FROM toaom
      WHERE sap_object EQ iv_sap_object AND
            ar_object  EQ iv_ar_object.

    IF sy-subrc IS INITIAL.
      CLEAR ls_toa.
      SELECT SINGLE sap_object object_id archiv_id arc_doc_id ar_object ar_date del_date reserve
        INTO ls_toa
        FROM (ls_toaom-connection)
        WHERE sap_object EQ iv_sap_object AND
              arc_doc_id EQ iv_alink_id AND
              object_id  EQ iv_object_id.
      IF sy-subrc IS INITIAL.

        ld_doc_type = ls_toa-reserve.
        CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
          EXPORTING
            archiv_id                = ls_toa-archiv_id
            document_type            = ld_doc_type
            archiv_doc_id            = ls_toa-arc_doc_id
          IMPORTING
            binlength                = ld_length
          TABLES
            binarchivobject          = lt_content
          EXCEPTIONS
            error_archiv             = 1
            error_communicationtable = 2
            error_kernel             = 3
            OTHERS                   = 4.
        IF sy-subrc = 0.

* Se pasa el binario a xstring
          ev_length = ld_length.
          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
            EXPORTING
              input_length = ev_length
            IMPORTING
              buffer       = ev_content
            TABLES
              binary_tab   = lt_content
            EXCEPTIONS
              failed       = 1
              OTHERS       = 2.

*         Recupero el mimetype
          SELECT SINGLE b~mimetype b~doc_type INTO (ev_mimetype, ev_doc_type)
               FROM toadv AS a INNER JOIN toadd AS b ON
                    b~doc_type = a~doc_type
               WHERE a~ar_object = ls_toa-ar_object.

          SELECT SINGLE filename
            FROM toaat
            INTO ev_filename
            WHERE arc_doc_id EQ ls_toa-arc_doc_id.

          IF sy-subrc NE 0. " Si no hay datos el nombre del fichero es el ID de objeto.
            ev_filename = |{ iv_object_id }.{ ls_toa-reserve }|.
          ENDIF.

          TRANSLATE ev_doc_type TO UPPER CASE.
        ELSE.
          RAISE no_data.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_alink_file_data


  METHOD get_attachs_http.


    DATA ld_num_multipart TYPE i.
    DATA ld_cont TYPE i.
    DATA lo_entity TYPE REF TO if_http_entity.
    DATA ls_attach TYPE LINE OF zca_i_attach.
    DATA lt_header_fields TYPE tihttpnvp.
    DATA lt_extension TYPE STANDARD TABLE OF string.

    CLEAR e_t_attach.

* Obtengo el numero de entidades de la cabecera.
    ld_num_multipart = io_server->request->num_multiparts( ).

    ld_cont = 1.
    WHILE ld_cont <= ld_num_multipart.
      CLEAR ls_attach.

* Obtengo la entidad en base al numero que estoy procesando.
      lo_entity = io_server->request->get_multipart( ld_cont ).

      lo_entity->get_form_fields( CHANGING fields = lt_header_fields ).
      lo_entity->get_header_fields( CHANGING fields = lt_header_fields ).

      ls_attach-filename = lo_entity->get_header_field( '~content_filename' ).
* Si la entidad es un fichero leo el tipo y contenido
      IF ls_attach-filename IS NOT INITIAL.
        ls_attach-mimetype = lo_entity->get_header_field( 'Content-Type' ).

* Saco la extensión del fichero. Por si hay varios puntos lo spliteo en una
* tabla y me quedo con el ultimo
        SPLIT ls_attach-filename AT '.' INTO TABLE lt_extension.
        DESCRIBE TABLE lt_extension.
        READ TABLE lt_extension INTO ls_attach-extension INDEX sy-tfill.
        TRANSLATE ls_attach-extension TO UPPER CASE.

* Contenido
        ls_attach-content = lo_entity->get_data( ).
        INSERT ls_attach INTO TABLE e_t_attach.
      ENDIF.

      ld_cont = ld_cont + 1.
    ENDWHILE.



  ENDMETHOD.                    "GET_ATTACHS_HTTP


  METHOD get_bds_document_list.
    FIELD-SYMBOLS <ls_components> TYPE LINE OF tt_bapicompon.
    FIELD-SYMBOLS <ls_signature> TYPE LINE OF tt_bapisignat.
    FIELD-SYMBOLS <ls_document_list> TYPE zca_s_doc_list.
    DATA lv_classname_aux  TYPE bapibds01-classname.
    DATA lv_object_key_aux TYPE bapibds01-objkey.

    DATA lv_tabix TYPE sytabix.

    lv_classname_aux  = iv_sap_object.
    lv_object_key_aux = iv_object_id.

    CALL FUNCTION 'BDS_BUSINESSDOCUMENT_GET_INFO'
      EXPORTING
        classname       = lv_classname_aux
        classtype       = 'BO'
        client          = sy-mandt
        object_key      = lv_object_key_aux
        all             = 'X'
      TABLES
        components      = et_components[]
        signature       = et_signature[]
      EXCEPTIONS
        nothing_found   = 1
        parameter_error = 2
        not_allowed     = 3
        error_kpro      = 4
        internal_error  = 5
        not_authorized  = 6
        OTHERS          = 7.

    IF sy-subrc = 0.
      SORT et_signature BY doc_count ASCENDING.

      LOOP AT et_components ASSIGNING <ls_components>.

        READ TABLE et_signature ASSIGNING <ls_signature>
        WITH KEY doc_count = <ls_components>-doc_count BINARY SEARCH.

        CHECK sy-subrc EQ 0.

        lv_tabix = sy-tabix.

*     Evitamos duplicar el documento en la tabla de resultados
        READ TABLE ct_document_list WITH KEY doc_id = <ls_signature>-doc_id TRANSPORTING NO FIELDS.

        CHECK sy-subrc NE 0.

        UNASSIGN <ls_signature>.

        INSERT INITIAL LINE INTO TABLE ct_document_list ASSIGNING <ls_document_list>.

        CHECK <ls_document_list> IS ASSIGNED.

        LOOP AT et_signature ASSIGNING <ls_signature> FROM lv_tabix.

          IF <ls_signature>-doc_count NE <ls_components>-doc_count.
            EXIT.
          ENDIF.

          <ls_document_list>-doc_id = <ls_signature>-doc_id.

          CASE <ls_signature>-prop_name.
            WHEN 'DESCRIPTION'.
              <ls_document_list>-filename = <ls_signature>-prop_value.
              <ls_document_list>-descr    = <ls_signature>-prop_value.
            WHEN 'CREATED_BY'.
              <ls_document_list>-crea_user = <ls_signature>-prop_value.
            WHEN 'CREATED_AT'.
              <ls_document_list>-crea_date = <ls_signature>-prop_value(8).
              <ls_document_list>-crea_time = <ls_signature>-prop_value+8(6).
          ENDCASE.

        ENDLOOP.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.                    "get_bds_document_list


  METHOD get_document_data.
    DATA: ld_alink_id TYPE saeardoid,
          ls_key_gos  TYPE zcl_ca_archivelink=>ts_key_gos.

    CLEAR: ev_content,
           ev_length,
           ev_mimetype,
           ev_filename,
           ev_doc_type,
           es_return.

    IF iv_doc_id(3) = 'FOL'.

      ls_key_gos = iv_doc_id.
      CALL METHOD get_object_content_gos
        EXPORTING
          is_key      = ls_key_gos
        IMPORTING
          ev_content  = ev_content
          ev_mimetype = ev_mimetype
          ev_filename = ev_filename
          ev_doc_type = ev_doc_type
          ev_length   = ev_length.

    ELSE.

      IF iv_ar_object IS NOT INITIAL.

        ld_alink_id = iv_doc_id.
        CALL METHOD get_alink_file_data
          EXPORTING
            iv_alink_id     = ld_alink_id
            iv_sap_object   = iv_sap_object
            iv_object_id    = iv_object_id
            iv_ar_object    = iv_ar_object
          IMPORTING
            ev_content      = ev_content
            ev_length       = ev_length
            ev_mimetype     = ev_mimetype
            ev_doc_type     = ev_doc_type
            ev_filename     = ev_filename
          EXCEPTIONS
            error_parameter = 1
            no_data         = 2
            OTHERS          = 3.

      ELSE.
        es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                    iv_id = dc_ca_id
                                                    iv_number = '010'
                                                    iv_langu = d_langu  ).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_document_data


  METHOD get_document_list.

    DATA: lt_gos_documents   TYPE zca_i_gos_doc_list,
          lt_alink_documents TYPE zca_i_archivelink_doc_list.

    DATA: lv_classname_aux  TYPE bapibds01-classname,
          lv_object_key_aux TYPE bapibds01-objkey.

    FIELD-SYMBOLS: <ls_document_list>  TYPE zca_s_doc_list,
                   <ls_gos_document>   TYPE zca_s_gos_doc_list,
                   <ls_alink_document> TYPE zca_s_archivelink_doc_list.

    CLEAR: et_document_list,
           es_return.

    CALL METHOD get_gos_document_list(
      EXPORTING
        iv_object_id     = iv_object_id
        iv_sap_object    = iv_sap_object
        iv_http_protocol = iv_http
      IMPORTING
        et_gos_documents = lt_gos_documents
        es_return        = es_return ).

    IF es_return IS NOT INITIAL.
      EXIT.
    ELSE.
      LOOP AT lt_gos_documents ASSIGNING <ls_gos_document>.
        APPEND INITIAL LINE TO et_document_list ASSIGNING <ls_document_list>.
        <ls_document_list>-doc_id    = <ls_gos_document>-loio_id.
        <ls_document_list>-doc_type  = me->lc_doc_type_gos.
        <ls_document_list>-filename  = <ls_gos_document>-filename.
        <ls_document_list>-url       = <ls_gos_document>-url.
        <ls_document_list>-crea_user = <ls_gos_document>-crea_user.
        <ls_document_list>-crea_date = <ls_gos_document>-crea_date.
        <ls_document_list>-crea_time = <ls_gos_document>-crea_time.
        <ls_document_list>-gos_url = <ls_gos_document>-gos_url.
      ENDLOOP.
    ENDIF.

    CALL METHOD get_alink_document_list(
      EXPORTING
        iv_object_id    = iv_object_id
        iv_sap_object   = iv_sap_object
        iv_local_url    = iv_local_url_alink
        iv_http         = iv_http
      IMPORTING
        et_al_documents = lt_alink_documents
        es_return       = es_return ).

    IF es_return IS NOT INITIAL.
      EXIT.
    ELSE.
      LOOP AT lt_alink_documents ASSIGNING <ls_alink_document>.

        APPEND INITIAL LINE TO et_document_list ASSIGNING <ls_document_list>.
        <ls_document_list>-doc_id    = <ls_alink_document>-arc_doc_id.
        <ls_document_list>-doc_type  = me->lc_doc_type_archivelink.
        <ls_document_list>-filename  = <ls_alink_document>-filename.
        <ls_document_list>-descr     = <ls_alink_document>-descr.
*      <ls_document_list>-archiv_id = <ls_alink_document>-archiv_id.
        <ls_document_list>-ar_object = <ls_alink_document>-ar_object.
        <ls_document_list>-url       = <ls_alink_document>-url.
        <ls_document_list>-crea_user = <ls_alink_document>-creator.
        <ls_document_list>-crea_date = <ls_alink_document>-creadate.
        <ls_document_list>-crea_time = <ls_alink_document>-creatime.

      ENDLOOP.
    ENDIF.

* Documentos del BDS
    CALL METHOD get_bds_document_list
      EXPORTING
        iv_object_id     = iv_object_id
        iv_sap_object    = iv_sap_object
        iv_http_protocol = iv_http
      CHANGING
        ct_document_list = et_document_list.





  ENDMETHOD.                    "get_document_list


  METHOD get_filename_object_gos.
    DATA ls_folder_id TYPE soodk.
    DATA ls_object_id TYPE soodk.
    DATA lt_objhead TYPE soli_tab.

    CLEAR rv_filename.

    ls_folder_id-objtp = is_key-foltp.
    ls_folder_id-objyr = is_key-folyr.
    ls_folder_id-objno = is_key-folno.
    ls_object_id-objtp = is_key-objtp.
    ls_object_id-objyr = is_key-objyr.
    ls_object_id-objno = is_key-objno.

    CALL FUNCTION 'SO_OBJECT_READ'
      EXPORTING
        folder_id                  = ls_folder_id
        object_id                  = ls_object_id
      TABLES
        objhead                    = lt_objhead
      EXCEPTIONS
        active_user_not_exist      = 1
        communication_failure      = 2
        component_not_available    = 3
        folder_not_exist           = 4
        folder_no_authorization    = 5
        object_not_exist           = 6
        object_no_authorization    = 7
        operation_no_authorization = 8
        owner_not_exist            = 9
        parameter_error            = 10
        substitute_not_active      = 11
        substitute_not_defined     = 12
        system_failure             = 13
        x_error                    = 14
        OTHERS                     = 15.
    IF sy-subrc = 0.
      rv_filename = extract_filename_head( lt_objhead ).
    ENDIF.

  ENDMETHOD.                    "GET_FILENAME_OBJECT_GOS


  METHOD get_file_temp.

    DATA lv_mimetype TYPE string.
    DATA lv_service TYPE string.
    DATA lv_id TYPE sysuuid_x16.

    IF iv_appl IS INITIAL OR
       iv_id   IS INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type     = dc_msg_type_error
                                                  iv_id       = dc_ca_id
                                                  iv_number   = '053' ).
      EXIT.
    ENDIF.

    SELECT SINGLE appl id contenido type name extension ernam erdat ertim
      FROM zca_t_tmp_conten
      INTO CORRESPONDING FIELDS OF es_attach
      WHERE appl EQ iv_appl AND
            id   EQ iv_id.

    IF sy-subrc IS NOT INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type       = dc_msg_type_error
                                                  iv_id         = dc_ca_id
                                                  iv_number     = '053' ).
      EXIT.
    ENDIF.

    IF iv_get_url IS NOT INITIAL.

      lv_id = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
      lv_mimetype = es_attach-extension.
      lv_service = lv_id.

      zcl_ca_http=>generate_tmp_url(
        EXPORTING
          iv_content  = es_attach-contenido
          iv_mimetype = lv_mimetype
          iv_service  = lv_service
          iv_doc_name = es_attach-name
        IMPORTING
          ev_url      = ev_url ).

    ENDIF.

  ENDMETHOD.                    "get_file_temp


  METHOD get_gos_document_list.

    FIELD-SYMBOLS <ls_connections> TYPE bdn_con.

    DATA: ld_objkey      TYPE swotobjid-objkey,
          ld_sap_object  TYPE bapibds01-classname,
          lt_connections TYPE STANDARD TABLE OF bdn_con,
          ls_key_gos     TYPE zcl_ca_archivelink=>ts_key_gos.

    FIELD-SYMBOLS: <ls_gos_documents> TYPE zca_s_gos_doc_list.

    CLEAR et_gos_documents.

    IF iv_object_id IS INITIAL OR
       iv_sap_object IS INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                  iv_id = dc_ca_id
                                                  iv_number = '022'
                                                  iv_langu = d_langu  ).
      EXIT.
    ENDIF.

* Recupera lista de anexos GOS
    ld_sap_object = iv_sap_object.
    ld_objkey = iv_object_id.
    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = ld_sap_object
        objkey             = ld_objkey
      TABLES
        gos_connections    = lt_connections[]
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    IF sy-subrc IS NOT INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                   iv_id = dc_ca_id
                                                   iv_number = '023'
                                                   iv_langu = d_langu  ).
    ENDIF.

    LOOP AT lt_connections ASSIGNING <ls_connections>.
      APPEND INITIAL LINE TO et_gos_documents ASSIGNING <ls_gos_documents>.
      <ls_gos_documents>-loio_id = <ls_connections>-loio_id.
      <ls_gos_documents>-crea_user = <ls_connections>-crea_user.
      <ls_gos_documents>-crea_date = <ls_connections>-crea_time+0(8).
      <ls_gos_documents>-crea_time = <ls_connections>-crea_time+8(6).

      ls_key_gos = <ls_connections>-loio_id.
*   Clave del GOS
      IF <ls_connections>-comp_id = 'SAP_URL'.
        <ls_gos_documents>-url = get_url_object_gos( ls_key_gos ).
        <ls_gos_documents>-gos_url = abap_true.
      ELSE.
        <ls_gos_documents>-filename = get_filename_object_gos( ls_key_gos ).
        generate_gos_url( EXPORTING is_key    = ls_key_gos
                                    iv_http_protocol = iv_http_protocol
                          IMPORTING ev_url    = <ls_gos_documents>-url
                                    es_return = es_return ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_gos_document_list


  METHOD get_object_content_gos.

    DATA: lv_document_id   TYPE so_entryid,
          ls_document_data TYPE sofolenti1,
          lt_object_header TYPE TABLE OF solisti1,
          lt_bin_content   TYPE TABLE OF solix,
          lv_mimetype      TYPE string,
          lv_sdok_mimetype TYPE mimetypes-type.

    FIELD-SYMBOLS: <ls_content_header> LIKE LINE OF lt_object_header.

    CONSTANTS: lc_mimetype_gos TYPE string VALUE '&SO_CONTTYPE='.

    lv_document_id = |{ is_key-foltp }{ is_key-folyr }{ is_key-folno }{ is_key-objtp }{ is_key-objyr }{ is_key-objno }|.

    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = lv_document_id
      IMPORTING
        document_data              = ls_document_data
      TABLES
        object_header              = lt_object_header
        contents_hex               = lt_bin_content
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.

    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    ev_length = ls_document_data-doc_size.
    ev_filename = ls_document_data-obj_descr.
    ev_doc_type = ls_document_data-obj_type.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = ev_length
      IMPORTING
        buffer       = ev_content
      TABLES
        binary_tab   = lt_bin_content
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

*   Se busca el mimetype en los atributos del GOS
    LOOP AT lt_object_header ASSIGNING <ls_content_header> WHERE line CS lc_mimetype_gos.
      lv_mimetype = <ls_content_header>-line.
      REPLACE lc_mimetype_gos IN lv_mimetype WITH ''.
    ENDLOOP.

*   Si el mimetype no se encuentra en los atributos del GOS se busca a partir de la extensión
    IF lv_mimetype IS NOT INITIAL.
      ev_mimetype = lv_mimetype.
    ELSE.
      CALL FUNCTION 'SDOK_MIMETYPE_GET'
        EXPORTING
          extension = ls_document_data-obj_type
        IMPORTING
          mimetype  = lv_sdok_mimetype.

      MOVE lv_sdok_mimetype TO ev_mimetype.
    ENDIF.

  ENDMETHOD.                    "get_object_content_gos


  METHOD get_url_file_alink.
    DATA lv_content TYPE xstring.
    DATA lv_filename TYPE string.
    DATA lv_mimetype TYPE string.
    DATA lv_doc_type TYPE string.
    DATA lv_service TYPE string.
    DATA ls_toa TYPE ty_toa.
    DATA ld_abs_uri TYPE c LENGTH 500." 255.
    DATA ld_http_uri TYPE c LENGTH 500. "255.
    DATA lo_alink TYPE REF TO zcl_ca_archivelink.
    DATA lv_alink_id TYPE saeobjid.

    CLEAR r_url.

    IF iv_ar_object IS NOT INITIAL.
      ls_toa-ar_object = iv_ar_object.
    ENDIF.

    IF i_arc_doc_id IS NOT INITIAL AND
       i_archiv_id IS NOT INITIAL.
      ls_toa-archiv_id  = i_archiv_id.
      ls_toa-arc_doc_id = i_arc_doc_id.
    ELSE.
      SELECT SINGLE sap_object object_id archiv_id arc_doc_id ar_object ar_date del_date reserve
        INTO ls_toa
        FROM toa01
        WHERE sap_object = i_sap_object
        AND arc_doc_id = i_alink_id.

    ENDIF.
    IF sy-subrc = 0.
      IF iv_local_url = abap_false.
        CALL FUNCTION 'SCMS_URL_GENERATE'
          EXPORTING
            command      = 'get'
            contrep      = ls_toa-archiv_id
            docid        = ls_toa-arc_doc_id
            accessmode   = 'r'
            signature    = 'X'
            docprot      = ''
            security     = 'B'
          IMPORTING
            absolute_uri = ld_abs_uri
            http_uri     = ld_http_uri.

        r_url = ld_abs_uri.

      ELSE.

        CREATE OBJECT lo_alink.
        lv_alink_id = i_alink_id.
        CALL METHOD lo_alink->get_alink_file_data
          EXPORTING
            iv_alink_id     = ls_toa-arc_doc_id
            iv_sap_object   = i_sap_object
            iv_object_id    = lv_alink_id
            iv_ar_object    = ls_toa-ar_object
          IMPORTING
            ev_content      = lv_content
            ev_filename     = lv_filename
            ev_mimetype     = lv_mimetype
            ev_doc_type     = lv_doc_type
          EXCEPTIONS
            error_parameter = 1
            no_data         = 2
            OTHERS          = 3.
        IF sy-subrc = 0.
          lv_service = ls_toa-arc_doc_id.
          zcl_ca_http=>generate_tmp_url(
            EXPORTING
              iv_content  = lv_content
              iv_mimetype = lv_mimetype
              iv_service  = lv_service
              iv_doc_name = lv_filename
              iv_http     = iv_http
            IMPORTING
              ev_url     = r_url ).

        ENDIF.

      ENDIF.


    ENDIF.


  ENDMETHOD.                    "get_url_file_alink


  METHOD get_url_image.

    DATA ls_toa TYPE ty_toa.
    DATA ld_abs_uri TYPE c LENGTH 255.
    DATA ld_http_uri TYPE c LENGTH 255.
    DATA lt_toaom_connections TYPE STANDARD TABLE OF ts_toaom.
    CLEAR r_url.

    FIELD-SYMBOLS: <ls_toaom_connections> LIKE LINE OF lt_toaom_connections.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_toaom_connections
      FROM toaom
      WHERE sap_object EQ i_object_alink.

    SORT lt_toaom_connections DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_toaom_connections.

    LOOP AT lt_toaom_connections ASSIGNING <ls_toaom_connections>.
      TRY.
          SELECT SINGLE sap_object object_id archiv_id arc_doc_id ar_object ar_date del_date reserve
          INTO ls_toa
          FROM (<ls_toaom_connections>-connection) "toa01
          WHERE sap_object = i_object_alink
          AND arc_doc_id = i_alink_id.


          IF sy-subrc = 0.

            CALL FUNCTION 'SCMS_URL_GENERATE'
              EXPORTING
                command      = 'get'
                contrep      = ls_toa-archiv_id
                docid        = ls_toa-arc_doc_id
                accessmode   = 'r'
                signature    = 'X'
                docprot      = ''
                security     = 'B'
              IMPORTING
                absolute_uri = ld_abs_uri
                http_uri     = ld_http_uri.

            r_url = ld_abs_uri.
            EXIT.
          ENDIF.
        CATCH cx_root.
*      do noothing!
      ENDTRY.

    ENDLOOP.



  ENDMETHOD.                    "get_url_image


  METHOD get_url_object_gos.

    DATA ls_folder_id TYPE soodk.
    DATA ls_object_id TYPE soodk.
    DATA lt_objcont TYPE STANDARD TABLE OF soli.

    CLEAR rv_url.

    ls_folder_id-objtp = is_key-foltp.
    ls_folder_id-objyr = is_key-folyr.
    ls_folder_id-objno = is_key-folno.
    ls_object_id-objtp = is_key-objtp.
    ls_object_id-objyr = is_key-objyr.
    ls_object_id-objno = is_key-objno.


    CALL FUNCTION 'SO_OBJECT_READ'
      EXPORTING
        folder_id                  = ls_folder_id
        object_id                  = ls_object_id
      TABLES
        objcont                    = lt_objcont
      EXCEPTIONS
        active_user_not_exist      = 1
        communication_failure      = 2
        component_not_available    = 3
        folder_not_exist           = 4
        folder_no_authorization    = 5
        object_not_exist           = 6
        object_no_authorization    = 7
        operation_no_authorization = 8
        owner_not_exist            = 9
        parameter_error            = 10
        substitute_not_active      = 11
        substitute_not_defined     = 12
        system_failure             = 13
        x_error                    = 14
        OTHERS                     = 15.
    IF sy-subrc = 0.

* Quito el tag que se construye con la URL
      REPLACE ALL OCCURRENCES OF '&KEY&' IN TABLE lt_objcont WITH ''.
      CONCATENATE LINES OF lt_objcont INTO rv_url.
    ENDIF.


  ENDMETHOD.                    "GET_URL_OBJECT_GOS


  METHOD upload_file_2_alink.

    DATA: lt_string TYPE TABLE OF string,
          lv_lines  TYPE i,
          lv_string TYPE string.

    DATA : lv_name         TYPE string,
           lv_extension    TYPE string,
           lv_ar_object    TYPE saeobjart,
           lv_doc_id       TYPE saeardoid,
           lv_object_id    TYPE saeobjid,
           lv_filename     TYPE toaat-filename,
           lv_description  TYPE toaat-descr,
           ls_config_alink TYPE ty_conf_alink.

    " Obtener extensión del fichero
    IF iv_track = abap_false.
      CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
        EXPORTING
          full_name     = iv_filename
        IMPORTING
          stripped_name = lv_name.

      SPLIT lv_name AT '.' INTO lv_name lv_extension.
    ELSE.

      SPLIT iv_filename AT '.' INTO TABLE lt_string.
      lv_lines = lines( lt_string ).
      READ TABLE lt_string INTO lv_string INDEX lv_lines.
      DELETE lt_string INDEX lv_lines.

      CONCATENATE LINES OF lt_string INTO lv_name SEPARATED BY '.'.
      lv_extension = lv_string.
    ENDIF.

    " Obtener la clase de documento donde irá el fichero.
    CONCATENATE iv_pref_ar_object lv_extension INTO lv_ar_object.
    TRANSLATE lv_ar_object TO UPPER CASE.

    " Busco la configuración del archivado
    SELECT SINGLE sap_object ar_object ar_status
                  archiv_id  cont_cat  doc_type
      INTO ls_config_alink
      FROM toaom
      WHERE sap_object = iv_object_alink
        AND ar_object  = lv_ar_object
        AND ar_status  = abap_true.

    IF sy-subrc = 0.

      " Creación de id del archivo
      CALL FUNCTION 'ARCHIVOBJECT_CREATE_TABLE'
        EXPORTING
          archiv_id                = ls_config_alink-archiv_id
          document_type            = ls_config_alink-doc_type
          length                   = iv_length
        IMPORTING
          archiv_doc_id            = lv_doc_id
        TABLES
          binarchivobject          = iv_file_data_bin
        EXCEPTIONS
          error_archiv             = 1
          error_communicationtable = 2
          error_kernel             = 3
          blocked_by_policy        = 4
          OTHERS                   = 5.

      IF sy-subrc = 0.

        lv_object_id   = iv_object_id. " ID que tendra el archivado(numero magazine, etc.)
        IF iv_descr IS INITIAL. " Sin descripción le paso el nombre del fichero
          lv_description = iv_filename.
        ELSE.
          lv_description = iv_descr.
        ENDIF.
        lv_filename    = iv_filename.
        " Finalmente se guarda el documento
        CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
          EXPORTING
            archiv_id             = ls_config_alink-archiv_id
            arc_doc_id            = lv_doc_id
            ar_object             = ls_config_alink-ar_object
            object_id             = lv_object_id
            sap_object            = ls_config_alink-sap_object
            doc_type              = ls_config_alink-doc_type   "lv_extension
            filename              = lv_filename
            descr                 = lv_description
            creator               = iv_creator
          EXCEPTIONS
            error_connectiontable = 1
            OTHERS                = 2.

        IF sy-subrc = 0.

          e_alink_id = lv_doc_id.
          e_ar_object = ls_config_alink-ar_object.
          e_archiv_id = ls_config_alink-archiv_id.
          e_doc_type = ls_config_alink-doc_type.

* Finalmente borro el archivo temporal
*        DELETE FROM zca_t_tmp_conten
*        WHERE id = ld_tmp_id.
        ELSE.
          e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                     iv_id = sy-msgid
                                                     iv_number = sy-msgno
                                                     iv_langu = d_langu
                                                     iv_message_v1 = sy-msgv1
                                                     iv_message_v2 = sy-msgv2
                                                     iv_message_v3 = sy-msgv3
                                                     iv_message_v4 = sy-msgv4 ).
        ENDIF.

      ELSE.
        e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                   iv_id = sy-msgid
                                                   iv_number = sy-msgno
                                                   iv_langu = d_langu
                                                   iv_message_v1 = sy-msgv1
                                                   iv_message_v2 = sy-msgv2
                                                   iv_message_v3 = sy-msgv3
                                                   iv_message_v4 = sy-msgv4 ).
      ENDIF.

    ELSE.
      e_return = zcl_ca_utilities=>fill_return( iv_type = dc_msg_type_error
                                                 iv_id = dc_ca_id
                                                 iv_number = '002'
                                                 iv_langu = d_langu
                                                 iv_message_v1 = iv_object_alink ).
    ENDIF.



  ENDMETHOD.                    "upload_file_2_alink


  METHOD upload_file_2_gos.
*
    DATA: lv_folder_id     TYPE soodk,
          lt_bin           TYPE TABLE OF solix,
          lv_xstring       TYPE xstring,
          lv_bin_length    TYPE i,
          lv_ext           TYPE char100,
          lv_document_type TYPE so_obj_tp,
          ls_document_data TYPE sodocchgi1,
          ls_document_info TYPE sofolenti1,
          ls_object_a      TYPE sibflporb,
          ls_object_b      TYPE sibflporb,
          lv_mimetype      TYPE w3conttype,
          lt_object_header TYPE TABLE OF solisti1.

    FIELD-SYMBOLS: <ls_object_header> LIKE LINE OF lt_object_header.

    CLEAR: es_return.

    CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
      EXPORTING
        region    = 'B'
      IMPORTING
        folder_id = lv_folder_id.

    lv_xstring = is_attach-contenido.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_bin_length
      TABLES
        binary_tab    = lt_bin.
    lv_mimetype = is_attach-type.
    CALL FUNCTION 'SDOK_FILE_NAME_EXTENSION_GET'
      EXPORTING
        mimetype  = lv_mimetype
      IMPORTING
        extension = lv_ext.

    ls_document_data-obj_name  = 'ATTACHMENT'.
    ls_document_data-obj_descr = is_attach-name.
    ls_document_data-obj_langu = iv_langu.
    ls_document_data-doc_size  = lv_bin_length.
    lv_document_type           = lv_ext.

    APPEND INITIAL LINE TO lt_object_header ASSIGNING <ls_object_header>.
    <ls_object_header>-line =  |&SO_FILENAME={ is_attach-name }|.

    CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
      EXPORTING
        folder_id                  = lv_folder_id
        document_data              = ls_document_data
        document_type              = lv_document_type
      IMPORTING
        document_info              = ls_document_info
      TABLES
        object_header              = lt_object_header
        contents_hex               = lt_bin
      EXCEPTIONS
        folder_not_exist           = 1
        document_type_not_exist    = 2
        operation_no_authorization = 3
        parameter_error            = 4
        x_error                    = 5
        enqueue_error              = 6
        OTHERS                     = 7.
    IF sy-subrc IS NOT INITIAL.
      es_return = zcl_ca_utilities=>fill_return( iv_type       = dc_msg_type_error
                                                  iv_id         = dc_ca_id
                                                  iv_number     = '055' ).
      EXIT.
    ENDIF.

    ls_object_a-instid = iv_object_id.
    ls_object_a-typeid = iv_sap_object.
    ls_object_a-catid  = 'BO'.

    ls_object_b-instid = ls_document_info-doc_id.
    ls_object_b-typeid = 'MESSAGE'.
    ls_object_b-catid  = 'BO'.

    TRY.
        CALL METHOD cl_binary_relation=>create_link
          EXPORTING
            is_object_a = ls_object_a
            is_object_b = ls_object_b
            ip_reltype  = 'ATTA'.
      CATCH cx_obl_parameter_error .
        es_return = zcl_ca_utilities=>fill_return( iv_type       = dc_msg_type_error
                                                    iv_id         = dc_ca_id
                                                    iv_number     = '055' ).
        EXIT.
      CATCH cx_obl_model_error .
        es_return = zcl_ca_utilities=>fill_return( iv_type     = dc_msg_type_error
                                                    iv_id       = dc_ca_id
                                                    iv_number   = '055' ).
        EXIT.
      CATCH cx_obl_internal_error .
        es_return = zcl_ca_utilities=>fill_return( iv_type     = dc_msg_type_error
                                                    iv_id       = dc_ca_id
                                                    iv_number   = '055' ).
        EXIT.
    ENDTRY.

    IF iv_commit IS NOT INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.                    "upload_file_2_gos


  METHOD upload_file_tmp.
    DATA ls_content TYPE zca_t_tmp_conten.
    DATA lt_content TYPE STANDARD TABLE OF zca_t_tmp_conten.
    DATA lt_binary TYPE TABLE OF tbl1024.
    DATA ld_timestamp TYPE timestampl.

    IF is_attach IS NOT INITIAL.

      TRY.
          ls_content-id = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
        CATCH cx_uuid_error .
          GET TIME STAMP FIELD ld_timestamp.
          ls_content-id = ld_timestamp.
      ENDTRY.

      ls_content-appl = is_attach-appl.
      ls_content-contenido = is_attach-content.
      ls_content-type = is_attach-mimetype.
*    TRANSLATE ls_content-type TO UPPER CASE.
      ls_content-name = is_attach-filename.
      ls_content-extension = is_attach-extension.
      ls_content-ernam = sy-uname.
      ls_content-erdat = sy-datum.
      ls_content-ertim = sy-uzeit.
      INSERT ls_content INTO TABLE lt_content.

* Pongo el ID del fichero generado con un prefijo para saber a posterior si es temporal o no.
      CONCATENATE dc_pref_tmp_attach ls_content-id INTO e_id_attach.


      MODIFY zca_t_tmp_conten FROM TABLE lt_content.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "upload_file_tmp
ENDCLASS.

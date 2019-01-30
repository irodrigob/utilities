CLASS zcl_ca_http DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_attachs,
             filename  TYPE bsstring,
             mimetype  TYPE bsstring,
             extension TYPE bsstring,
             content   TYPE mime_data,
           END OF ts_attachs.
    TYPES: tt_attachs TYPE STANDARD TABLE OF ts_attachs.

    CLASS-METHODS get_host
      IMPORTING
        !iv_http       TYPE sap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_host) TYPE bsstring .
    CLASS-METHODS generate_tmp_url
      IMPORTING
        !iv_content       TYPE xstring
        !iv_mimetype      TYPE any
        !iv_exp_time      TYPE i DEFAULT 3600
        !iv_service       TYPE string
        !iv_doc_name      TYPE string
        !it_header_fields TYPE tihttpnvp OPTIONAL
        !iv_http          TYPE sap_bool DEFAULT abap_false
        !iv_langu         TYPE sy-langu DEFAULT sy-langu
      EXPORTING
        !ev_url           TYPE string
        !es_return        TYPE bapiret2 .
    CLASS-METHODS unescape_html
      IMPORTING
        !i_html            TYPE any
      RETURNING
        VALUE(r_unescaped) TYPE string .
    CLASS-METHODS get_attach_http
      IMPORTING
        !io_server TYPE REF TO if_http_server
      EXPORTING
        !et_attach TYPE tt_attachs .
    CLASS-METHODS get_http_protocol
      IMPORTING
        !io_server         TYPE REF TO if_http_server
      RETURNING
        VALUE(rv_protocol) TYPE string .
  PROTECTED SECTION.


  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_http IMPLEMENTATION.


  METHOD generate_tmp_url.

    DATA lo_cached_response TYPE REF TO if_http_response.

* Generamos la URL

* Paso 1: generamos la URL
    ev_url = |{ get_host( iv_http = iv_http ) }/sap/public/{ iv_service }/{ cl_http_utility=>escape_url( iv_doc_name ) }|.

    TRY.
* Paso 2: generamos la respuesta HTTP
        CREATE OBJECT lo_cached_response TYPE cl_http_response
          EXPORTING
            add_c_msg = 1.

        lo_cached_response->set_data( iv_content ).

        lo_cached_response->set_header_field( name = if_http_header_fields=>content_type
                                              value = iv_mimetype ).
        IF it_header_fields IS SUPPLIED.
          lo_cached_response->set_header_fields( fields = it_header_fields ).
        ENDIF.

        lo_cached_response->set_status( code = 200 reason = 'OK' ).


        lo_cached_response->server_cache_expire_rel( expires_rel = iv_exp_time ).

        cl_http_server=>server_cache_upload( url      = ev_url
                                             response = lo_cached_response
                                             scope = ihttp_inv_global ).

      CATCH cx_root INTO DATA(lx_root).
        CLEAR ev_url.
        es_return = VALUE #( type = 'E' message = lx_root->get_text( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_attach_http.

    DATA ld_num_multipart TYPE i.
    DATA ld_cont TYPE i.
    DATA lo_entity TYPE REF TO if_http_entity.
    DATA ls_attach TYPE LINE OF tt_attachs.
    DATA lt_header_fields TYPE tihttpnvp.
    DATA lt_extension TYPE STANDARD TABLE OF string.

    CLEAR et_attach.

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
        INSERT ls_attach INTO TABLE et_attach.
      ENDIF.

      ld_cont = ld_cont + 1.
    ENDWHILE.


  ENDMETHOD.


  METHOD get_host.

    FIELD-SYMBOLS <ls_info> TYPE LINE OF tihttpurls2.
    DATA lt_info TYPE tihttpurls2.

    CLEAR rv_host.

* Recupero la info del servidor al cual se esta conectado

    cl_http_server=>get_extension_info( EXPORTING extension_class = 'CL_HTTP_EXT_BSP' IMPORTING urls = lt_info ).

    IF iv_http EQ abap_false.
* Primero buscamos el protocol https
      READ TABLE lt_info ASSIGNING <ls_info> WITH KEY protocol = 'https'.
      IF sy-subrc NE 0.
* Si no existe vamos a por el http
        READ TABLE lt_info ASSIGNING <ls_info> WITH KEY protocol = 'http'.
      ENDIF.
    ELSE.
      READ TABLE lt_info ASSIGNING <ls_info> WITH KEY protocol = 'http'.
    ENDIF.
    IF <ls_info> IS ASSIGNED.
      rv_host = |{ <ls_info>-protocol }://{ <ls_info>-host }:{ <ls_info>-port }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_http_protocol.
    FIELD-SYMBOLS <ls_header_fields> TYPE LINE OF tihttpnvp.
    FIELD-SYMBOLS <ls_info> TYPE LINE OF tihttpurls2.
    DATA lt_info TYPE tihttpurls2.
    DATA lt_header_fields TYPE tihttpnvp.

    rv_protocol = 'HTTPS'. " Valor por defecto

    IF io_server IS BOUND. " Si la clase no esta instanciada se asume que es HTTPS
* Recupero la info del servidor
      cl_http_server=>get_extension_info( EXPORTING extension_class = 'CL_HTTP_EXT_BSP' IMPORTING urls = lt_info ).

* Recuperamos la cabecera HTTP
      io_server->request->get_header_fields( CHANGING fields = lt_header_fields ).

      READ TABLE lt_header_fields ASSIGNING <ls_header_fields> WITH KEY name = '~server_port'.
      IF sy-subrc = 0.
        READ TABLE lt_info ASSIGNING <ls_info> WITH KEY port = <ls_header_fields>-value.
        IF sy-subrc = 0.
          rv_protocol = <ls_info>-protocol.
          TRANSLATE rv_protocol TO UPPER CASE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD unescape_html .

* Replace all "<>&
    r_unescaped = i_html.

    CHECK r_unescaped CA '&' AND r_unescaped CA ';'.        "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT

* Unescape the essential thing
    REPLACE ALL OCCURRENCES OF '&quot;'  IN r_unescaped WITH '"'. "#EC NOTEXT
    REPLACE ALL OCCURRENCES OF '&lt;'    IN r_unescaped WITH '<'. "#EC NOTEXT
    REPLACE ALL OCCURRENCES OF '&gt;'    IN r_unescaped WITH '>'. "#EC NOTEXT
    REPLACE ALL OCCURRENCES OF '&amp;'   IN r_unescaped WITH '&'. "#EC NOTEXT
    REPLACE ALL OCCURRENCES OF '&euro;'   IN r_unescaped WITH ''. "#EC *
    REPLACE ALL OCCURRENCES OF '&dagger;'   IN r_unescaped WITH '#'. "#EC *
    REPLACE ALL OCCURRENCES OF '&Dagger;'   IN r_unescaped WITH '#'. "#EC *

    DATA:
      l_pattern TYPE string,
      l_replace TYPE string.

* Escape the &#number;
    IF r_unescaped CS '&#'.                                 "#EC NOTEXT
      DATA:
        l_start   TYPE i,
        l_i       TYPE i,
        l_length  TYPE i,
        l_content TYPE string.
      l_start = 1.
      DO.
        SEARCH r_unescaped FOR '&#' IN CHARACTER MODE STARTING AT l_start. "#EC NOTEXT
        IF sy-subrc = 0.
          l_start = l_start + sy-fdpos.
          SEARCH r_unescaped FOR ';' IN CHARACTER MODE STARTING AT l_start. "#EC NOTEXT
          IF sy-subrc = 0.
            SUBTRACT 1 FROM l_start.
            l_length = sy-fdpos + 1.
            l_pattern = r_unescaped+l_start(l_length).
            ADD 2 TO l_start.
            l_length = sy-fdpos - 2.
            l_content = r_unescaped+l_start(l_length).
            IF l_content CO ' 0123456789'.                  "#EC NOTEXT
              l_i = l_content.
              TRY.
                  l_replace = cl_abap_conv_in_ce=>uccpi( l_i ).
                CATCH cx_root.
                  l_replace = '#'.
              ENDTRY.
              REPLACE ALL OCCURRENCES OF l_pattern IN r_unescaped WITH l_replace.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&nbsp;' 160.                              "#EC NOTEXT
    unescape_amp '&iexcl;' 161.                             "#EC NOTEXT
    unescape_amp '&cent;' 162.                              "#EC NOTEXT
    unescape_amp '&pound;' 163.                             "#EC NOTEXT
    unescape_amp '&curren;' 164.                            "#EC NOTEXT
    unescape_amp '&yen;' 165.                               "#EC NOTEXT
    unescape_amp '&brvbar;' 166.                            "#EC NOTEXT
    unescape_amp '&sect;' 167.                              "#EC NOTEXT
    unescape_amp '&uml;' 168.                               "#EC NOTEXT
    unescape_amp '&copy;' 169.                              "#EC NOTEXT
    unescape_amp '&ordf;' 170.                              "#EC NOTEXT
    unescape_amp '&laquo;' 171.                             "#EC NOTEXT
    unescape_amp '&not;' 172.                               "#EC NOTEXT
    unescape_amp '&shy;' 173.                               "#EC NOTEXT
    unescape_amp '&reg;' 174.                               "#EC NOTEXT
    unescape_amp '&macr;' 175.                              "#EC NOTEXT
    unescape_amp '&deg;' 176.                               "#EC NOTEXT
    unescape_amp '&plusmn;' 177.                            "#EC NOTEXT
    unescape_amp '&sup2;' 178.                              "#EC NOTEXT
    unescape_amp '&sup3;' 179.                              "#EC NOTEXT
    unescape_amp '&acute;' 180.                             "#EC NOTEXT
    unescape_amp '&micro;' 181.                             "#EC NOTEXT
    unescape_amp '&para;' 182.                              "#EC NOTEXT
    unescape_amp '&middot;' 183.                            "#EC NOTEXT
    unescape_amp '&cedil;' 184.                             "#EC NOTEXT
    unescape_amp '&sup1;' 185.                              "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&ordm;' 186.                              "#EC NOTEXT
    unescape_amp '&raquo;' 187.                             "#EC NOTEXT
    unescape_amp '&frac14;' 188.                            "#EC NOTEXT
    unescape_amp '&frac12;' 189.                            "#EC NOTEXT
    unescape_amp '&frac34;' 190.                            "#EC NOTEXT
    unescape_amp '&iquest;' 191.                            "#EC NOTEXT
    unescape_amp '&Agrave;' 192.                            "#EC NOTEXT
    unescape_amp '&Aacute;' 193.                            "#EC NOTEXT
    unescape_amp '&Acirc;' 194.                             "#EC NOTEXT
    unescape_amp '&Atilde;' 195.                            "#EC NOTEXT
    unescape_amp '&Auml;' 196.                              "#EC NOTEXT
    unescape_amp '&Aring;' 197.                             "#EC NOTEXT
    unescape_amp '&AElig;' 198.                             "#EC NOTEXT
    unescape_amp '&Ccedil;' 199.                            "#EC NOTEXT
    unescape_amp '&Egrave;' 200.                            "#EC NOTEXT
    unescape_amp '&Eacute;' 201.                            "#EC NOTEXT
    unescape_amp '&Ecirc;' 202.                             "#EC NOTEXT
    unescape_amp '&Euml;' 203.                              "#EC NOTEXT
    unescape_amp '&Igrave;' 204.                            "#EC NOTEXT
    unescape_amp '&Iacute;' 205.                            "#EC NOTEXT
    unescape_amp '&Icirc;' 206.                             "#EC NOTEXT
    unescape_amp '&Iuml;' 207.                              "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&ETH;' 208.                               "#EC NOTEXT
    unescape_amp '&Ntilde;' 209.                            "#EC NOTEXT
    unescape_amp '&Ograve;' 210.                            "#EC NOTEXT
    unescape_amp '&Oacute;' 211.                            "#EC NOTEXT
    unescape_amp '&Ocirc;' 212.                             "#EC NOTEXT
    unescape_amp '&Otilde;' 213.                            "#EC NOTEXT
    unescape_amp '&Ouml;' 214.                              "#EC NOTEXT
    unescape_amp '&times;' 215.                             "#EC NOTEXT
    unescape_amp '&Oslash;' 216.                            "#EC NOTEXT
    unescape_amp '&Ugrave;' 217.                            "#EC NOTEXT
    unescape_amp '&Uacute;' 218.                            "#EC NOTEXT
    unescape_amp '&Ucirc;' 219.                             "#EC NOTEXT
    unescape_amp '&Uuml;' 220.                              "#EC NOTEXT
    unescape_amp '&Yacute;' 221.                            "#EC NOTEXT
    unescape_amp '&THORN;' 222.                             "#EC NOTEXT
    unescape_amp '&szlig;' 223.                             "#EC NOTEXT
    unescape_amp '&agrave;' 224.                            "#EC NOTEXT
    unescape_amp '&aacute;' 225.                            "#EC NOTEXT
    unescape_amp '&acirc;' 226.                             "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&atilde;' 227.                            "#EC NOTEXT
    unescape_amp '&auml;' 228.                              "#EC NOTEXT
    unescape_amp '&aring;' 229.                             "#EC NOTEXT
    unescape_amp '&aelig;' 230.                             "#EC NOTEXT
    unescape_amp '&ccedil;' 231.                            "#EC NOTEXT
    unescape_amp '&egrave;' 232.                            "#EC NOTEXT
    unescape_amp '&eacute;' 233.                            "#EC NOTEXT
    unescape_amp '&ecirc;' 234.                             "#EC NOTEXT
    unescape_amp '&euml;' 235.                              "#EC NOTEXT
    unescape_amp '&igrave;' 236.                            "#EC NOTEXT
    unescape_amp '&iacute;' 237.                            "#EC NOTEXT
    unescape_amp '&icirc;' 238.                             "#EC NOTEXT
    unescape_amp '&iuml;' 239.                              "#EC NOTEXT
    unescape_amp '&eth;' 240.                               "#EC NOTEXT
    unescape_amp '&ntilde;' 241.                            "#EC NOTEXT
    unescape_amp '&ograve;' 242.                            "#EC NOTEXT
    unescape_amp '&oacute;' 243.                            "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&ocirc;' 244.                             "#EC NOTEXT
    unescape_amp '&otilde;' 245.                            "#EC NOTEXT
    unescape_amp '&ouml;' 246.                              "#EC NOTEXT
    unescape_amp '&divide;' 247.                            "#EC NOTEXT
    unescape_amp '&oslash;' 248.                            "#EC NOTEXT
    unescape_amp '&ugrave;' 249.                            "#EC NOTEXT
    unescape_amp '&uacute;' 250.                            "#EC NOTEXT
    unescape_amp '&ucirc;' 251.                             "#EC NOTEXT
    unescape_amp '&uuml;' 252.                              "#EC NOTEXT
    unescape_amp '&yacute;' 253.                            "#EC NOTEXT
    unescape_amp '&thorn;' 254.                             "#EC NOTEXT
    unescape_amp '&yuml;' 255.                              "#EC NOTEXT
    unescape_amp '&fnof;' 402.                              "#EC NOTEXT
    unescape_amp '&Alpha;' 913.                             "#EC NOTEXT
    unescape_amp '&Beta;' 914.                              "#EC NOTEXT
    unescape_amp '&Gamma;' 915.                             "#EC NOTEXT
    unescape_amp '&Delta;' 916.                             "#EC NOTEXT
    unescape_amp '&Epsilon;' 917.                           "#EC NOTEXT
    unescape_amp '&Zeta;' 918.                              "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&Eta;' 919.                               "#EC NOTEXT
    unescape_amp '&Theta;' 920.                             "#EC NOTEXT
    unescape_amp '&Iota;' 921.                              "#EC NOTEXT
    unescape_amp '&Kappa;' 922.                             "#EC NOTEXT
    unescape_amp '&Lambda;' 923.                            "#EC NOTEXT
    unescape_amp '&Mu;' 924.                                "#EC NOTEXT
    unescape_amp '&Nu;' 925.                                "#EC NOTEXT
    unescape_amp '&Xi;' 926.                                "#EC NOTEXT
    unescape_amp '&Omicron;' 927.                           "#EC NOTEXT
    unescape_amp '&Pi;' 928.                                "#EC NOTEXT
    unescape_amp '&Rho;' 929.                               "#EC NOTEXT
    unescape_amp '&Sigma;' 931.                             "#EC NOTEXT
    unescape_amp '&Tau;' 932.                               "#EC NOTEXT
    unescape_amp '&Upsilon;' 933.                           "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&Phi;' 934.                               "#EC NOTEXT
    unescape_amp '&Chi;' 935.                               "#EC NOTEXT
    unescape_amp '&Psi;' 936.                               "#EC NOTEXT
    unescape_amp '&Omega;' 937.                             "#EC NOTEXT
    unescape_amp '&alpha;' 945.                             "#EC NOTEXT
    unescape_amp '&beta;' 946.                              "#EC NOTEXT
    unescape_amp '&gamma;' 947.                             "#EC NOTEXT
    unescape_amp '&delta;' 948.                             "#EC NOTEXT
    unescape_amp '&epsilon;' 949.                           "#EC NOTEXT
    unescape_amp '&zeta;' 950.                              "#EC NOTEXT
    unescape_amp '&eta;' 951.                               "#EC NOTEXT
    unescape_amp '&theta;' 952.                             "#EC NOTEXT
    unescape_amp '&iota;' 953.                              "#EC NOTEXT
    unescape_amp '&kappa;' 954.                             "#EC NOTEXT
    unescape_amp '&lambda;' 955.                            "#EC NOTEXT
    unescape_amp '&mu;' 956.                                "#EC NOTEXT
    unescape_amp '&nu;' 957.                                "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&xi;' 958.                                "#EC NOTEXT
    unescape_amp '&omicron;' 959.                           "#EC NOTEXT
    unescape_amp '&pi;' 960.                                "#EC NOTEXT
    unescape_amp '&rho;' 961.                               "#EC NOTEXT
    unescape_amp '&sigmaf;' 962.                            "#EC NOTEXT
    unescape_amp '&sigma;' 963.                             "#EC NOTEXT
    unescape_amp '&tau;' 964.                               "#EC NOTEXT
    unescape_amp '&upsilon;' 965.                           "#EC NOTEXT
    unescape_amp '&phi;' 966.                               "#EC NOTEXT
    unescape_amp '&chi;' 967.                               "#EC NOTEXT
    unescape_amp '&psi;' 968.                               "#EC NOTEXT
    unescape_amp '&omega;' 969.                             "#EC NOTEXT
    unescape_amp '&thetasym;' 977.                          "#EC NOTEXT
    unescape_amp '&upsih;' 978.                             "#EC NOTEXT
    unescape_amp '&piv;' 982.                               "#EC NOTEXT
    unescape_amp '&bull;' 8226.                             "#EC NOTEXT
    unescape_amp '&hellip;' 8230.                           "#EC NOTEXT
    unescape_amp '&prime;' 8242.                            "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&Prime;' 8243.                            "#EC NOTEXT
    unescape_amp '&oline;' 8254.                            "#EC NOTEXT
    unescape_amp '&frasl;' 8260.                            "#EC NOTEXT
    unescape_amp '&weierp;' 8472.                           "#EC NOTEXT
    unescape_amp '&image;' 8465.                            "#EC NOTEXT
    unescape_amp '&real;' 8476.                             "#EC NOTEXT
    unescape_amp '&trade;' 8482.                            "#EC NOTEXT
    unescape_amp '&alefsym;' 8501.                          "#EC NOTEXT
    unescape_amp '&larr;' 8592.                             "#EC NOTEXT
    unescape_amp '&uarr;' 8593.                             "#EC NOTEXT
    unescape_amp '&rarr;' 8594.                             "#EC NOTEXT
    unescape_amp '&darr;' 8595.                             "#EC NOTEXT
    unescape_amp '&harr;' 8596.                             "#EC NOTEXT
    unescape_amp '&crarr;' 8629.                            "#EC NOTEXT
    unescape_amp '&lArr;' 8656.                             "#EC NOTEXT
    unescape_amp '&uArr;' 8657.                             "#EC NOTEXT
    unescape_amp '&rArr;' 8658.                             "#EC NOTEXT
    unescape_amp '&dArr;' 8659.                             "#EC NOTEXT
    unescape_amp '&hArr;' 8660.                             "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&forall;' 8704.                           "#EC NOTEXT
    unescape_amp '&part;' 8706.                             "#EC NOTEXT
    unescape_amp '&exist;' 8707.                            "#EC NOTEXT
    unescape_amp '&empty;' 8709.                            "#EC NOTEXT
    unescape_amp '&nabla;' 8711.                            "#EC NOTEXT
    unescape_amp '&isin;' 8712.                             "#EC NOTEXT
    unescape_amp '&notin;' 8713.                            "#EC NOTEXT
    unescape_amp '&ni;' 8715.                               "#EC NOTEXT
    unescape_amp '&prod;' 8719.                             "#EC NOTEXT
    unescape_amp '&sum;' 8721.                              "#EC NOTEXT
    unescape_amp '&minus;' 8722.                            "#EC NOTEXT
    unescape_amp '&lowast;' 8727.                           "#EC NOTEXT
    unescape_amp '&radic;' 8730.                            "#EC NOTEXT
    unescape_amp '&prop;' 8733.                             "#EC NOTEXT
    unescape_amp '&infin;' 8734.                            "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&ang;' 8736.                              "#EC NOTEXT
    unescape_amp '&and;' 8743.                              "#EC NOTEXT
    unescape_amp '&or;' 8744.                               "#EC NOTEXT
    unescape_amp '&cap;' 8745.                              "#EC NOTEXT
    unescape_amp '&cup;' 8746.                              "#EC NOTEXT
    unescape_amp '&int;' 8747.                              "#EC NOTEXT
    unescape_amp '&there4;' 8756.                           "#EC NOTEXT
    unescape_amp '&sim;' 8764.                              "#EC NOTEXT
    unescape_amp '&cong;' 8773.                             "#EC NOTEXT
    unescape_amp '&asymp;' 8776.                            "#EC NOTEXT
    unescape_amp '&ne;' 8800.                               "#EC NOTEXT
    unescape_amp '&equiv;' 8801.                            "#EC NOTEXT
    unescape_amp '&le;' 8804.                               "#EC NOTEXT
    unescape_amp '&ge;' 8805.                               "#EC NOTEXT
    unescape_amp '&sub;' 8834.                              "#EC NOTEXT
    unescape_amp '&sup;' 8835.                              "#EC NOTEXT
    unescape_amp '&nsub;' 8836.                             "#EC NOTEXT
    unescape_amp '&sube;' 8838.                             "#EC NOTEXT
    unescape_amp '&supe;' 8839.                             "#EC NOTEXT
    unescape_amp '&oplus;' 8853.                            "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&otimes;' 8855.                           "#EC NOTEXT
    unescape_amp '&perp;' 8869.                             "#EC NOTEXT
    unescape_amp '&sdot;' 8901.                             "#EC NOTEXT
    unescape_amp '&lceil;' 8968.                            "#EC NOTEXT
    unescape_amp '&rceil;' 8969.                            "#EC NOTEXT
    unescape_amp '&lfloor;' 8970.                           "#EC NOTEXT
    unescape_amp '&rfloor;' 8971.                           "#EC NOTEXT
    unescape_amp '&lang;' 9001.                             "#EC NOTEXT
    unescape_amp '&rang;' 9002.                             "#EC NOTEXT
    unescape_amp '&loz;' 9674.                              "#EC NOTEXT
    unescape_amp '&spades;' 9824.                           "#EC NOTEXT
    unescape_amp '&clubs;' 9827.                            "#EC NOTEXT
    unescape_amp '&hearts;' 9829.                           "#EC NOTEXT
    unescape_amp '&diams;' 9830.                            "#EC NOTEXT
    unescape_amp '&quot;' 34.                               "#EC NOTEXT
    unescape_amp '&amp;' 38.                                "#EC NOTEXT
    unescape_amp '&lt;' 60.                                 "#EC NOTEXT
    unescape_amp '&gt;' 62.                                 "#EC NOTEXT
    unescape_amp '&OElig;' 338.                             "#EC NOTEXT
    unescape_amp '&oelig;' 339.                             "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&Scaron;' 352.                            "#EC NOTEXT
    unescape_amp '&scaron;' 353.                            "#EC NOTEXT
    unescape_amp '&Yuml;' 376.                              "#EC NOTEXT
    unescape_amp '&circ;' 710.                              "#EC NOTEXT
    unescape_amp '&tilde;' 732.                             "#EC NOTEXT
    unescape_amp '&ensp;' 8194.                             "#EC NOTEXT
    unescape_amp '&emsp;' 8195.                             "#EC NOTEXT
    unescape_amp '&thinsp;' 8201.                           "#EC NOTEXT
    unescape_amp '&zwnj;' 8204.                             "#EC NOTEXT
    unescape_amp '&zwj;' 8205.                              "#EC NOTEXT
    unescape_amp '&lrm;' 8206.                              "#EC NOTEXT
    unescape_amp '&rlm;' 8207.                              "#EC NOTEXT
    unescape_amp '&ndash;' 8211.                            "#EC NOTEXT
    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT
    unescape_amp '&mdash;' 8212.                            "#EC NOTEXT
    unescape_amp '&lsquo;' 8216.                            "#EC NOTEXT
    unescape_amp '&rsquo;' 8217.                            "#EC NOTEXT
    unescape_amp '&sbquo;' 8218.                            "#EC NOTEXT
    unescape_amp '&ldquo;' 8220.                            "#EC NOTEXT
    unescape_amp '&rdquo;' 8221.                            "#EC NOTEXT
    unescape_amp '&bdquo;' 8222.                            "#EC NOTEXT
    unescape_amp '&dagger;' 8224.                           "#EC NOTEXT
    unescape_amp '&Dagger;' 8225.                           "#EC NOTEXT
    unescape_amp '&permil;' 8240.                           "#EC NOTEXT
    unescape_amp '&lsaquo;' 8249.                           "#EC NOTEXT
    unescape_amp '&rsaquo;' 8250.                           "#EC NOTEXT
    unescape_amp '&euro;' 8364.                             "#EC NOTEXT

    CHECK r_unescaped CP '*&*;*'.                           "#EC NOTEXT

  ENDMETHOD.              "#EC NOTEXT                    "UNESCAPE_HTML
ENDCLASS.

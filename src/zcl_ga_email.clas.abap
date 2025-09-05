CLASS zcl_ga_email DEFINITION
  PUBLIC
  INHERITING FROM cl_bcs_message

  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">validate email id</p>
      is_emailid_valid
        IMPORTING
          emailid                 TYPE ad_smtpadr
        RETURNING
          VALUE(is_emailid_valid) TYPE abap_bool .
    METHODS:
      set_placeholder
        IMPORTING
          placeholder_name  TYPE string
          placeholder_value TYPE string,
      get_template
        IMPORTING
                  ip_tpl      TYPE smtg_tmpl_hdr-id
        EXPORTING
                  op_subject  TYPE string
                  op_html     TYPE string
        RETURNING VALUE(r_ok) TYPE abap_bool.
  PROTECTED SECTION.
    METHODS:
      read_so10_text
        IMPORTING ip_text_name          TYPE tdobname
                  ip_language           TYPE bcs_language DEFAULT sy-langu
                  ip_doctype            TYPE bcs_doctype DEFAULT 'txt'
                  ip_tdid               TYPE thead-tdid DEFAULT 'ST'
                  ip_tdobject           TYPE thead-tdobject DEFAULT 'TEXT'
        RETURNING VALUE(r_contents_txt) TYPE string.

  PRIVATE SECTION.
    DATA gt_data_key TYPE if_smtg_email_template=>ty_gt_data_key .
    METHODS:
      replace_placeholder
        IMPORTING replace_string TYPE string
        RETURNING VALUE(result)  TYPE string.
ENDCLASS.



CLASS zcl_ga_email IMPLEMENTATION.
  METHOD get_template.
    " read headers
*    ld_template_id = |ZGA_AIS_{ ip_deparea }|.
    SELECT SINGLE cds_view FROM smtg_tmpl_hdr
    INTO @DATA(ld_cds_view)
    WHERE id      EQ @ip_tpl
      AND version EQ 'A'. "GC_VERSION_ACTIVE
    IF sy-subrc EQ 0.
      TRY.
          DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = ip_tpl ).

          lo_email_api->render(
            EXPORTING
              iv_language  = 'S' "Parece que solo existe en castellano
              it_data_key  = VALUE if_smtg_email_template=>ty_gt_data_key( )
            IMPORTING
              ev_subject   = DATA(ld_subject)
              ev_body_html = DATA(ld_body_html)
*              ev_body_text = DATA(lv_body_text)
              ).

        CATCH cx_smtg_email_common INTO DATA(ex). " E-Mail API Exceptions
          RETURN.
      ENDTRY.
      op_subject = ld_subject.
      op_html    = ld_body_html.
      r_ok = abap_true.

    ENDIF.
  ENDMETHOD.


  METHOD is_emailid_valid.
    DATA ls_address   TYPE sx_address.
    ls_address-type = 'INT'.
    ls_address-address = emailid.

    CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
      EXPORTING
        address_unstruct    = ls_address
      EXCEPTIONS
        error_address_type  = 1
        error_address       = 2
        error_group_address = 3
        OTHERS              = 4.
    IF sy-subrc EQ 0.
      is_emailid_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD read_so10_text.
    DATA :
      li_lines TYPE TABLE OF tline,
      lw_lines TYPE tline.

    DATA: lv_no_of_lines LIKE sy-tabix,
          lv_changed(1)  TYPE c.

    DATA: lv_header TYPE thead.

    IF ip_text_name IS NOT INITIAL.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = ip_tdid "'ST'
          language                = ip_language
          name                    = ip_text_name
          object                  = ip_tdobject "'TEXT'
        IMPORTING
          header                  = lv_header
        TABLES
          lines                   = li_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        LOOP AT gt_data_key INTO DATA(ls_data_key).
          CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
            EXPORTING
              name  = ls_data_key-name
              value = ls_data_key-value.
        ENDLOOP.

        DESCRIBE TABLE li_lines LINES lv_no_of_lines.

        CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
          EXPORTING
            endline       = lv_no_of_lines
            header        = lv_header
            init          = ' '
            option_dialog = ' '
            program       = sy-cprog
          IMPORTING
            changed       = lv_changed
            newheader     = lv_header
          TABLES
            lines         = li_lines.

        LOOP AT li_lines INTO lw_lines.
          IF lw_lines-tdformat = '='  OR
             lw_lines-tdformat = ' '. "   Continuous Text
            r_contents_txt = r_contents_txt && lw_lines-tdline.
          ELSE.
            r_contents_txt = r_contents_txt && cl_abap_char_utilities=>cr_lf && lw_lines-tdline.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD replace_placeholder.
    result = replace_string.
    LOOP AT gt_data_key INTO DATA(ls_data_key).
      REPLACE ALL OCCURRENCES OF ls_data_key-name IN result WITH ls_data_key-value.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_placeholder.
    APPEND  VALUE #( name = placeholder_name
                     value = placeholder_value )
     TO gt_data_key.
  ENDMETHOD.
ENDCLASS.

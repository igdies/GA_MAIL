*----------------------------------------------------------------------*
***INCLUDE LZACS_MAILF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_LEAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_leave .
  LEAVE TO SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_send.

  DATA: ls_mail TYPE zga_s_email.


  MOVE-CORRESPONDING gs_mail TO ls_mail.

  go_mail->content_set( ls_mail ).
  g_ok = go_mail->send_mail( ).

  IF g_ok NE abap_true.
*    MESSAGE go_mail->msg TYPE 'I'.
  ELSE.
    PERFORM f_leave.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_IMPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_input .

*  DATA: lt_receiver LIKE gs_mail-t_receiver.
  DATA lt_receiver TYPE TABLE OF ad_smtpadr.


  IF gs_mail-receiver IS NOT INITIAL.
    SPLIT gs_mail-receiver AT ';' INTO TABLE lt_receiver.
*    INSERT LINES OF lt_receiver INTO TABLE gs_mail-t_receiver.
    gs_mail-t_receiver = VALUE #( BASE gs_mail-t_receiver
                              FOR ls_receiver IN lt_receiver
                              ( ad_smtpadr = ls_receiver )
                              ).
    SORT gs_mail-t_receiver BY ad_smtpadr.
    DELETE ADJACENT DUPLICATES FROM gs_mail-t_receiver COMPARING ad_smtpadr.
  ENDIF.
  PERFORM f_read_body.

ENDFORM.
FORM f_read_body.
  CASE gs_mail-body_type.
    WHEN zcl_ga_email=>document_type-html.
      PERFORM f_read_html_body.
    WHEN OTHERS.
      PERFORM f_read_text_body.
  ENDCASE.
ENDFORM.
FORM f_read_html_body.
  IF go_body_html_view IS NOT BOUND.
    RETURN.
  ENDIF.
  BREAK-POINT.
*  call method go_body_html_edit
ENDFORM.
FORM f_read_text_body.
  IF go_body_textedit IS NOT BOUND.
    RETURN.
  ENDIF.
  go_body_textedit->get_textstream( IMPORTING
      text                   = gs_mail-body
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3
  ).
  IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
FORM f_see_attachments.
  CALL SCREEN 0300 STARTING AT 8 8.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ADD_RECEIVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_add_receiver .

  CALL SCREEN 0200 STARTING AT 8 8.
*
*  CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
*    EXPORTING
*      endpos_col   = 28
*      endpos_row   = 28
*      startpos_col = 8
*      startpos_row = 8
*      titletext    = 'Empfänger'
**   IMPORTING
**     CHOISE       = CHOISE
*    TABLES
*      valuetab     = gs_mail-t_receiver
*    EXCEPTIONS
*      break_off    = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0.
*    break langer.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_STATUS_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_status_0100 .

  DATA: l_tabix TYPE sytabix.

  FIELD-SYMBOLS: <ls_receiver> LIKE LINE OF gs_mail-t_receiver.


  FREE: gs_mail-receiver.
  gs_mail-receiver_cnt = lines( gs_mail-t_receiver ).
  gs_mail-attachment_cnt = lines( gs_mail-t_attachments ).
  LOOP AT gs_mail-t_receiver ASSIGNING <ls_receiver>.
    l_tabix = sy-tabix.
    IF ( strlen( gs_mail-receiver ) + strlen( <ls_receiver>-ad_smtpadr ) + 1 ) GT 90.
      CONCATENATE gs_mail-receiver '; +' INTO gs_mail-receiver.
      EXIT.
    ELSE.
      IF l_tabix EQ 1.
        gs_mail-receiver = <ls_receiver>-ad_smtpadr.
      ELSE.
        CONCATENATE gs_mail-receiver <ls_receiver>-ad_smtpadr INTO gs_mail-receiver SEPARATED BY space.
      ENDIF.
      CONCATENATE gs_mail-receiver ';' INTO gs_mail-receiver.
    ENDIF.
  ENDLOOP.
  UNASSIGN <ls_receiver>.


  SET PF-STATUS g_status.
  SET TITLEBAR '0100'.

  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING
*       parent                      = parent
        container_name              = 'BODY'
*       style                       = style
*       lifetime                    = lifetime_default
*       repid                       = repid
*       dynnr                       = dynnr
*       no_autodef_progid_dynnr     = no_autodef_progid_dynnr
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
  CASE gs_mail-body_type.
    WHEN zcl_ga_email=>document_type-html.

      IF go_body_textedit IS BOUND.
        go_body_textedit->free( ).
        FREE go_body_textedit.
        cl_gui_cfw=>flush( ).
      ENDIF.

      IF go_body_html_view IS NOT BOUND.
        CREATE OBJECT go_body_html_view
          EXPORTING
            parent             = go_container
            saphtmlp           = abap_true
*           uiflag             =
*           end_session_with_browser = 0
*           name               =
*           saphttp            =
*           query_table_disabled     = ''
          EXCEPTIONS
            cntl_error         = 1
            cntl_install_error = 2
            dp_install_error   = 3
            dp_error           = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.

        ENDIF.
        cl_gui_cfw=>flush( ).
        DATA ld_assigned_url TYPE char255.
        DATA ld_html_tab TYPE soli_tab.
        ld_html_tab = cl_bcs_convert=>string_to_soli( gs_mail-body ).
        go_body_html_view->load_data(
*          EXPORTING
*            url                    =
*            type                   = 'text'
*            subtype                = 'html'
*            size                   = 0
*            encoding               =
*            charset                =
*            needfiltering          = 0
*            language               =
*            i_tidyt                =
          IMPORTING
            assigned_url           = ld_assigned_url
          CHANGING
            data_table             = ld_html_tab
*            iscontentchanged       =
          EXCEPTIONS
            dp_invalid_parameter   = 1
            dp_error_general       = 2
            cntl_error             = 3
            html_syntax_notcorrect = 4
            OTHERS                 = 5
        ).
        IF sy-subrc <> 0.
        ENDIF.
        go_body_html_view->show_data(
          EXPORTING
            url                    =  ld_assigned_url
*            frame                  =
*            in_place               = 'X '
          EXCEPTIONS
            cntl_error             = 1
            cnht_error_not_allowed = 2
            cnht_error_parameter   = 3
            dp_error_general       = 4
            OTHERS                 = 5
        ).
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      IF go_body_textedit IS NOT BOUND.
        IF go_body_html_view IS BOUND.
          go_body_html_view->free( ).
          FREE go_body_html_view.
          cl_gui_cfw=>flush( ).
        ENDIF.
        CREATE OBJECT go_body_textedit
          EXPORTING
            parent                 = go_container
          EXCEPTIONS
            error_cntl_create      = 1
            error_cntl_init        = 2
            error_cntl_link        = 3
            error_dp_create        = 4
            gui_type_not_supported = 5
            OTHERS                 = 6.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        go_body_textedit->set_readonly_mode( 0 ).
      ENDIF.
      cl_gui_cfw=>flush( ).
      go_body_textedit->set_textstream(
        EXPORTING
          text                   = gs_mail-body
        EXCEPTIONS
          error_cntl_call_method = 1
          not_supported_by_gui   = 2
          OTHERS                 = 3
      ).
      IF sy-subrc <> 0.
      ENDIF.

  ENDCASE.

ENDFORM.

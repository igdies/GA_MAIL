FUNCTION ZFM_EMAIL_DIALOG.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IO_MAIL) TYPE REF TO  ZCL_GA_EMAIL OPTIONAL
*"     REFERENCE(I_AS_POPUP) TYPE  ABAP_BOOL DEFAULT ABAP_TRUE
*"  EXPORTING
*"     REFERENCE(E_OK) TYPE  ABAP_BOOL
*"----------------------------------------------------------------------

  DATA: ls_mail TYPE zga_s_email.

  IF io_mail IS BOUND.
    go_mail = io_mail.
  ELSE.
    go_mail = zcl_ga_email=>create(
**                io_object   =
*                io_data     =
*                ip_app      =
*                ip_subclass =
              ).
  ENDIF.


  ls_mail = go_mail->content_get( ).
  MOVE-CORRESPONDING ls_mail TO gs_mail.
  IF gs_mail-sender_email IS INITIAL.
    gs_mail-sender_email = zcl_ga_email=>get_email_from_user( i_user = sy-uname ).
  ENDIF.
  IF i_as_popup EQ abap_true.
    g_status = '0101'.
    CALL SCREEN 0100 STARTING AT 5 5.
  ELSE.
    g_status = '0100'.
    CALL SCREEN 0100.
  ENDIF.

  e_ok = g_ok.

ENDFUNCTION.

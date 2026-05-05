*&---------------------------------------------------------------------*
*&  Include           LZACS_MAILOTC
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  PBO_STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_status_0200 OUTPUT.
  SET PF-STATUS '0201'.
  SET TITLEBAR '0200'.

  gt_receiver = gs_mail-t_receiver.
  CLEAR gs_receiver.

ENDMODULE.
MODULE pbo_status_0300 OUTPUT.
  SET PF-STATUS '0301'.
  SET TITLEBAR '0300'.

  gt_attachments = CORRESPONDING #( gs_mail-t_attachments ).
  CLEAR gs_attachment.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_RECEIVER'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_receiver_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_receiver LINES tc_receiver-lines.
  g_next_line = tc_receiver-lines + 1.
  tc_receiver-lines = tc_receiver-lines + 20.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_PBO_FINAL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_pbo_final OUTPUT.
  tc_receiver-current_line = g_next_line.
ENDMODULE.

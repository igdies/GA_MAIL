*----------------------------------------------------------------------*
***INCLUDE LZACS_MAILO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_status_0100 OUTPUT.

perform f_status_0100.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TS_ATTACHMENTS'. DO NOT CHANGE THIS LI
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TS_ATTACHMENTS_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE GT_ATTACHMENTS LINES TS_ATTACHMENTS-lines.
ENDMODULE.

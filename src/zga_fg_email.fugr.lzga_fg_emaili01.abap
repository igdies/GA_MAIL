*----------------------------------------------------------------------*
***INCLUDE LZACS_MAILI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_exit_0100 INPUT.
  PERFORM f_leave.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'CANCEL' OR 'EXIT' OR 'BACK'.
      PERFORM f_leave.
    WHEN 'SEND'.
      perform f_read_body.
      PERFORM f_send.
    WHEN '&SEE_ATT'.
      perform f_read_body.
      PERFORM f_see_attachments.
    WHEN 'PICK' or '&SEE_RECEIVERS'.
      perform f_read_body.
      PERFORM f_add_receiver.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_INPUT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_input_0100 INPUT.

  PERFORM f_input.

ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TS_ATTACHMENTS'. DO NOT CHANGE THIS LINE
*&SPWIZARD: MARK TABLE
MODULE ts_attachments_mark INPUT.
  DATA: g_ts_attachments_wa2 LIKE LINE OF gt_attachments.
  IF ts_attachments-line_sel_mode = 1
  AND gs_attachment-box = 'X'.
    LOOP AT gt_attachments INTO g_ts_attachments_wa2
      WHERE box = 'X'.
      g_ts_attachments_wa2-box = ''.
      MODIFY gt_attachments
        FROM g_ts_attachments_wa2
        TRANSPORTING box.
    ENDLOOP.
  ENDIF.
  MODIFY gt_attachments
    FROM gs_attachment
    INDEX ts_attachments-current_line
    TRANSPORTING box.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TS_ATTACHMENTS'. DO NOT CHANGE THIS LIN
*&SPWIZARD: PROCESS USER COMMAND
MODULE ts_attachments_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TS_ATTACHMENTS'
                              'GT_ATTACHMENTS'
                              'BOX'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           LZACS_MAILITC
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  PERFORM f_get_receiver.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_RECEIVER'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_receiver_modify INPUT.
  IF tc_receiver-current_line GT lines( gt_receiver ).
    INSERT gs_receiver INTO gt_receiver
      INDEX tc_receiver-current_line.

  ELSE.
    MODIFY gt_receiver
      FROM gs_receiver
      INDEX tc_receiver-current_line.
  ENDIF.
ENDMODULE.

MODULE user_command_0300 INPUT.
  PERFORM f_user_command_0300.
ENDMODULE.

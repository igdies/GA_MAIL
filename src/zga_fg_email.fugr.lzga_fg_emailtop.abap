FUNCTION-POOL ZGA_FG_EMAIL.                    "MESSAGE-ID ..

*types: begin of ty_s_receiver,
*        ad_smtpadr type ad_smtpadr,
*        copy       type abap_bool,
*        blind_copy type abap_bool,
*       end of ty_s_receiver.
DATA: gt_receiver TYPE zga_t_email_receivers.
DATA: gs_receiver LIKE LINE OF gt_receiver.
types: begin of ty_s_attachment,
       box type char1.
       include type zga_s_email_attachment.
types: end of ty_s_attachment.

DATA: gt_attachments TYPE table of ty_s_attachment."zan_t_email_attachments.
DATA: gs_attachment LIKE LINE OF gt_attachments.

DATA: gs_mail TYPE zga_s_email_ui.

DATA: go_mail TYPE REF TO zcl_ga_email.
DATA: go_container TYPE REF TO  cl_gui_custom_container.
DATA: go_body_textedit TYPE REF TO cl_gui_textedit.

DATA: go_body_html_view TYPE REF TO cl_gui_html_viewer.
DATA: go_body_html_edit TYPE REF TO cl_gui_html_editor.

DATA: ok_code TYPE syucomm.
DATA: g_status TYPE char4.
DATA: g_next_line TYPE sytabix.
DATA: g_ok TYPE abap_bool.

DATA:     ok_code_0200 LIKE sy-ucomm.
DATA:     ok_code_0300 LIKE sy-ucomm.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_RECEIVER' ITSELF
CONTROLS: tc_receiver TYPE TABLEVIEW USING SCREEN 0200.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TS_ATTACHMENTS' ITSELF
CONTROLS: TS_ATTACHMENTS TYPE TABLEVIEW USING SCREEN 0300.

*&SPWIZARD: LINES OF TABLECONTROL 'TS_ATTACHMENTS'
DATA:     G_TS_ATTACHMENTS_LINES  LIKE SY-LOOPC.

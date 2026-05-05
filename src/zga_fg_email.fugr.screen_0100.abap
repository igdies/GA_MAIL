PROCESS BEFORE OUTPUT.
  MODULE pbo_status_0100.




PROCESS AFTER INPUT.
  MODULE pai_exit_0100 AT EXIT-COMMAND.

  CHAIN.
    FIELD gs_mail-sender_email.
    FIELD gs_mail-subject.
    FIELD gs_mail-receiver.
    MODULE pai_input_0100 ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE pai_user_command_0100.

CLASS zcl_ga_email_demo DEFINITION
  PUBLIC
  INHERITING FROM zcl_ga_email
  CREATE PUBLIC .

  PUBLIC SECTION.
    constants c_email_address type ad_smtpadr value 'perico.menganito@xyz.abc'.
    METHODS create_and_send IMPORTING i_via_dialog TYPE abap_bool DEFAULT abap_false.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ga_email_demo IMPLEMENTATION.
  METHOD create_and_send.
    TRY.
        apply_body(
          EXPORTING
            ip_body    =  'Testing Demo'
            ip_doctype = document_type-txt
*          RECEIVING
*            ro_me      =
        )->apply_subject( ip_subject =  'Testeando demo'
        )->apply_receiver( is_receiver = VALUE #( ad_smtpadr = 'ignacio.diez@antolin.com')
        )->send_mail(
            i_via_dialog = i_via_dialog
        ).
      CATCH zcx_ga_util. " Excepción genérica GA.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

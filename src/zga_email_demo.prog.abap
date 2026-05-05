*&---------------------------------------------------------------------*
*& Report zga_email_demo
*&---------------------------------------------------------------------*
*& This report demonstrates different ways to instantiate and extend
*& the email framework (ZCL_GA_EMAIL). It shows how to use predefined
*& local classes, how to pass simple data dynamically (io_data), and
*& how to pass full objects dynamically (io_object).
*&---------------------------------------------------------------------*
REPORT zga_email_demo.

" ==============================================================================
" 1. TYPES & DATA DEFINITIONS
" ==============================================================================

" Type definition for passing basic string data via generic reference (io_data)
TYPES: BEGIN OF ty_s_mail_2,
         subject TYPE string,
         body    TYPE string,
       END OF ty_s_mail_2.


" ==============================================================================
" 2. CLASS DEFINITIONS
" ==============================================================================

" ------------------------------------------------------------------------------
" lcl_email: Basic Local Class Extension
" ------------------------------------------------------------------------------
" Inherits from the global demo class. Overrides the sending logic to provide
" hardcoded strings as a baseline demonstration.
CLASS lcl_email DEFINITION INHERITING FROM zcl_ga_email_demo.
  PUBLIC SECTION.
    METHODS create_and_send REDEFINITION.
ENDCLASS.

" ------------------------------------------------------------------------------
" lcl_email_data: Data Reference Extension (io_data)
" ------------------------------------------------------------------------------
" Inherits from the local class lcl_email. Uses the generic 'mo_data'
" attribute passed dynamically at creation to render the email contents.
CLASS lcl_email_data DEFINITION INHERITING FROM lcl_email.
  PUBLIC SECTION.
    METHODS create_and_send REDEFINITION.
  PROTECTED SECTION.
    " Variable to store the strongly-typed dereferenced structure
    DATA: my_data TYPE ty_s_mail_2.
ENDCLASS.

" ------------------------------------------------------------------------------
" lcl_email_provider: Object Data Provider
" ------------------------------------------------------------------------------
" A basic independent class acting as a data container (Data Transfer Object).
" An instance of this class will be passed to the email framework via 'io_object'
CLASS lcl_email_provider DEFINITION.
  PUBLIC SECTION.
    DATA: subject   TYPE string,
          body      TYPE string,
          receivers TYPE zga_t_email_receivers.

    METHODS constructor
      IMPORTING
        iv_subject   TYPE string
        iv_body      TYPE string
        it_receivers TYPE zga_t_email_receivers.
ENDCLASS.

" ------------------------------------------------------------------------------
" lcl_email_object: Object Reference Extension (io_object)
" ------------------------------------------------------------------------------
" Inherits from lcl_email. Extracts its content by downloading casting the
" generic 'mo_object' reference into the specific 'lcl_email_provider' class.
CLASS lcl_email_object DEFINITION INHERITING FROM lcl_email.
  PUBLIC SECTION.
    METHODS create_and_send REDEFINITION.
ENDCLASS.





" ==============================================================================
" 4. SELECTION SCREEN
" ==============================================================================

" --- Test 1 Parameters ---
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_test1 TYPE abap_bool AS CHECKBOX DEFAULT space. " Global Class (no inputs needed)
SELECTION-SCREEN END OF BLOCK b1.

" --- Test 2 Parameters ---
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_test2 TYPE abap_bool AS CHECKBOX DEFAULT space USER-COMMAND t2.
  PARAMETERS: p_t2_sub TYPE char100 DEFAULT 'Testeando' LOWER CASE MODIF ID tg2,
              p_t2_bod TYPE char100 DEFAULT 'Testing' LOWER CASE MODIF ID tg2,
              p_t2_rec TYPE ad_smtpadr DEFAULT 'ignacio.diez@antolin.com' LOWER CASE MODIF ID tg2,
              p_t2_pop TYPE abap_bool AS CHECKBOX DEFAULT 'X' MODIF ID tg2.
SELECTION-SCREEN END OF BLOCK b2.

" --- Test 3 Parameters ---
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  PARAMETERS: p_test3 TYPE abap_bool AS CHECKBOX DEFAULT space USER-COMMAND t3.
  PARAMETERS: p_t3_sub TYPE char100 DEFAULT 'Asunto Mail Data' LOWER CASE MODIF ID tg3,
              p_t3_bod TYPE char100 DEFAULT 'Cuerpo Mail Data' LOWER CASE MODIF ID tg3,
              p_t3_rec TYPE ad_smtpadr DEFAULT 'ignacio.diez@antolin.com' LOWER CASE MODIF ID tg3,
              p_t3_pop TYPE abap_bool AS CHECKBOX DEFAULT 'X' MODIF ID tg3.
SELECTION-SCREEN END OF BLOCK b3.

" --- Test 4 Parameters ---
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
  PARAMETERS: p_test4 TYPE abap_bool AS CHECKBOX DEFAULT space USER-COMMAND t4.
  PARAMETERS: p_t4_sub TYPE char100 DEFAULT 'Asunto Mail 3 (from Object)' LOWER CASE MODIF ID tg4,
              p_t4_bod TYPE char100 DEFAULT 'This body is provided by a local class instance...' LOWER CASE MODIF ID tg4,
              p_t4_rec TYPE ad_smtpadr DEFAULT 'ignacio.diez@antolin.com' LOWER CASE MODIF ID tg4,
              p_t4_pop TYPE abap_bool AS CHECKBOX DEFAULT 'X' MODIF ID tg4.
SELECTION-SCREEN END OF BLOCK b4.

" --- Test 5 Parameters ---
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-b05.
  PARAMETERS: p_test5 TYPE abap_bool AS CHECKBOX DEFAULT space USER-COMMAND t5.
  PARAMETERS: p_t5_sub TYPE char100 DEFAULT 'Subject directly from zcl_ga_email' LOWER CASE MODIF ID tg5,
              p_t5_bod TYPE char100 DEFAULT 'This is a message body created directly using zcl_ga_email' LOWER CASE MODIF ID tg5,
              p_t5_rec TYPE ad_smtpadr DEFAULT 'ignacio.diez@antolin.com' LOWER CASE MODIF ID tg5,
              p_t5_pop TYPE abap_bool AS CHECKBOX DEFAULT 'X' MODIF ID tg5.
SELECTION-SCREEN END OF BLOCK b5.

" --- Test 6 Parameters ---
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-b06.
  PARAMETERS: p_test6 TYPE abap_bool AS CHECKBOX DEFAULT space USER-COMMAND t6.
  PARAMETERS: p_t6_sub TYPE char100 DEFAULT 'Subject for ZFM_EMAIL_DIALOG' LOWER CASE MODIF ID tg6,
              p_t6_bod TYPE char100 DEFAULT 'Body provided for Function Module Call' LOWER CASE MODIF ID tg6,
              p_t6_rec TYPE ad_smtpadr DEFAULT 'ignacio.diez@antolin.com' LOWER CASE MODIF ID tg6,
              p_t6_pop TYPE abap_bool AS CHECKBOX DEFAULT 'X' MODIF ID tg6.
SELECTION-SCREEN END OF BLOCK b6.

" ==============================================================================
" 4.5 SELECTION SCREEN EVENTS
" ==============================================================================

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Dynamic visibility for Test 2
    IF screen-group1 = 'TG2'.
      IF p_test2 = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    " Dynamic visibility for Test 3
    IF screen-group1 = 'TG3'.
      IF p_test3 = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    " Dynamic visibility for Test 4
    IF screen-group1 = 'TG4'.
      IF p_test4 = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    " Dynamic visibility for Test 5
    IF screen-group1 = 'TG5'.
      IF p_test5 = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    " Dynamic visibility for Test 6
    IF screen-group1 = 'TG6'.
      IF p_test6 = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.



" ==============================================================================
" 5. MAIN EXECUTABLE (START-OF-SELECTION)
" ==============================================================================

START-OF-SELECTION.

  IF p_test1 = abap_true.
    PERFORM test_1_global_class.
  ENDIF.

  IF p_test2 = abap_true.
    PERFORM test_2_local_subclass.
  ENDIF.

  IF p_test3 = abap_true.
    PERFORM test_3_struct_passing.
  ENDIF.

  IF p_test4 = abap_true.
    PERFORM test_4_object_passing.
  ENDIF.

  IF p_test5 = abap_true.
    PERFORM test_5_direct_usage.
  ENDIF.

  IF p_test6 = abap_true.
    PERFORM test_6_function_module.
  ENDIF.


" ==============================================================================
" 6. SUBROUTINES FOR TESTS
" ==============================================================================

*&---------------------------------------------------------------------*
*& Form test_1_global_class
*&---------------------------------------------------------------------*
FORM test_1_global_class.
  DATA lo_email TYPE REF TO zcl_ga_email_demo.

  " ------------------------------------------------------------------------------
  " Test 1: Global Class Baseline Exeuction
  " ------------------------------------------------------------------------------
  " Create global email reference using the Global Class factory method.
  lo_email ?= zcl_ga_email_demo=>create(  ). " using create from my class directly
*           ?= zcl_ga_email=>create( " or indicating the subclass at create
*                ip_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'ZCL_GA_EMAIL_DEMO' )
*              ).

  lo_email->create_and_send( abap_true ). " message sent using method defined at zcl_ga_email_demo

  FREE lo_email.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_2_local_subclass
*&---------------------------------------------------------------------*
FORM test_2_local_subclass.
  DATA lo_email TYPE REF TO zcl_ga_email_demo.

  " ------------------------------------------------------------------------------
  " Test 2: Local Subclass directly redefining create_and_send (lcl_email)
  " ------------------------------------------------------------------------------
  lo_email "?= lcl_email=>create(  ).
           ?= zcl_ga_email=>create(
*                io_object   =
*                io_data     =
*                ip_app      =
                ip_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'LCL_EMAIL' )
              ).
  lo_email->create_and_send( abap_true ). " message sent using method defined at local class lcl_email

  FREE lo_email.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_3_struct_passing
*&---------------------------------------------------------------------*
FORM test_3_struct_passing.
  DATA lo_email TYPE REF TO zcl_ga_email_demo.
  DATA ls_email_data TYPE ty_s_mail_2.

  " Use selection screen parameters
  ls_email_data-subject = p_t3_sub.
  ls_email_data-body    = p_t3_bod.

  lo_email ?= lcl_email_data=>create(
                io_data = REF #( ls_email_data )
              ).

  " Use the popup parameter
  lo_email->create_and_send( p_t3_pop ).
  FREE lo_email.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_4_object_passing
*&---------------------------------------------------------------------*
FORM test_4_object_passing.
  DATA lo_email TYPE REF TO zcl_ga_email_demo.

  " Use selection screen parameters
  DATA(lo_data_provider) = NEW lcl_email_provider(
    iv_subject   = CONV string( p_t4_sub )
    iv_body      = CONV string( p_t4_bod )
    it_receivers = VALUE #( ( ad_smtpadr = p_t4_rec ) )
  ).

  lo_email ?= lcl_email_object=>create(
                io_object = lo_data_provider
              ).

  lo_email->create_and_send( p_t4_pop ).
  FREE lo_email.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form test_5_direct_usage
*&---------------------------------------------------------------------*
FORM test_5_direct_usage.
  " ------------------------------------------------------------------------------
  " Test 5: Direct Usage of Framework Class (zcl_ga_email)
  " ------------------------------------------------------------------------------
  DATA lo_direct_email TYPE REF TO zcl_ga_email.

  " Create absolute generic email reference using the base class factory
  lo_direct_email = zcl_ga_email=>create( ).

  TRY.
      " Standard fluent API usage directly on the base framework class
      lo_direct_email->apply_body(
          ip_body    = conv #( p_t5_bod )
          ip_doctype = zcl_ga_email=>document_type-txt
      )->apply_subject(
          ip_subject = conv #( p_t5_sub )"'Subject directly from zcl_ga_email'
      )->apply_receiver(
          is_receiver = VALUE #( ad_smtpadr = conv #( p_t5_rec ) )
      )->send_mail(
          i_via_dialog = p_t5_pop
      ).
    CATCH zcx_ga_util. " Handle generic generic GA exception
  ENDTRY.

  FREE lo_direct_email.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form test_6_function_module
*&---------------------------------------------------------------------*
FORM test_6_function_module.
  " ------------------------------------------------------------------------------
  " Test 6: Calling Function Module ZFM_EMAIL_DIALOG
  " ------------------------------------------------------------------------------
  DATA lo_fm_email TYPE REF TO zcl_ga_email.
  DATA lv_ok TYPE abap_bool.

  " Prepare an instance with default configuration to pass to the Functional Module
  lo_fm_email = zcl_ga_email=>create( ).
  TRY.
      lo_fm_email->apply_body(
          ip_body    = conv #( p_t6_bod )
          ip_doctype = zcl_ga_email=>document_type-txt
      )->apply_subject(
          ip_subject = conv #( p_t6_sub )
      )->apply_receiver(
          is_receiver = VALUE #( ad_smtpadr = conv #( p_t6_rec ) )
      ).
    CATCH zcx_ga_util.
  ENDTRY.

  " Call the Dialog UI directly using the Function Module.
  " Notice that send_mail() internally triggers this FM when i_via_dialog = abap_true,
  " but doing it this way gives manual control over the popup usage and output result.
  CALL FUNCTION 'ZFM_EMAIL_DIALOG'
    EXPORTING
      io_mail    = lo_fm_email
      i_as_popup = p_t6_pop
    IMPORTING
      e_ok       = lv_ok.

  FREE lo_fm_email.
ENDFORM.

" ==============================================================================
" 3. CLASS IMPLEMENTATIONS
" ==============================================================================

CLASS lcl_email IMPLEMENTATION.
  METHOD create_and_send.
    TRY.
        apply_body(
          EXPORTING
            ip_body    = CONV string( p_t2_bod )
            ip_doctype = document_type-txt
        )->apply_subject( ip_subject = CONV string( p_t2_sub )
        )->apply_receiver( is_receiver = VALUE #( ad_smtpadr = p_t2_rec )
        )->send_mail(
            i_via_dialog = p_t2_pop
        ).
      CATCH zcx_ga_util.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.



CLASS lcl_email_data IMPLEMENTATION.
  METHOD create_and_send.
    " 1. Retrieve and dereference the generic mo_data reference
    IF mo_data IS BOUND.
      " Dereference the generic data reference using a field symbol
      ASSIGN mo_data->* TO FIELD-SYMBOL(<ls_data>).

      " Assign the dereferenced data to your typed variable
      IF <ls_data> IS ASSIGNED.
        my_data = <ls_data>.
      ENDIF.
    ENDIF.

    TRY.
        " 2. Send email using the dynamically fetched structural data
        apply_body(
          EXPORTING
            ip_body    =  CONV #( my_data-body )
            ip_doctype = document_type-txt
*          RECEIVING
*            ro_me      =
        )->apply_subject( ip_subject =  CONV #( my_data-subject )
        )->apply_receiver( is_receiver = VALUE #( ad_smtpadr = 'ignacio.diez@antolin.com')
        )->send_mail(
            i_via_dialog = i_via_dialog
        ).
      CATCH zcx_ga_util. " Excepción genérica GA.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_email_provider IMPLEMENTATION.
  METHOD constructor.
    " Initialize attributes when object is created
    me->subject   = iv_subject.
    me->body      = iv_body.
    me->receivers = it_receivers.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_email_object IMPLEMENTATION.
  METHOD create_and_send.
    " Variable to hold the strongly-typed object
    DATA lo_provider TYPE REF TO lcl_email_provider.

    " 1. Check if an object was provided and try casting it from mo_object
    IF mo_object IS BOUND.
      TRY.
          " Downcast the generic mo_object to our specific provider class
          lo_provider ?= mo_object.
        CATCH cx_sy_move_cast_error.
          " Handle invalid object types gracefully
          RETURN.
      ENDTRY.
    ENDIF.

    " 2. Proceed only if we successfully casted the provider object
    IF lo_provider IS BOUND.
      TRY.
          apply_body(
              ip_body    = lo_provider->body
              ip_doctype = document_type-txt
          )->apply_subject(
              ip_subject = lo_provider->subject
          )->apply_receivers(
              it_receivers = lo_provider->receivers " Uses array logic for multiple receivers
          )->send_mail(  i_via_dialog ).
        CATCH zcx_ga_util. " Excepción genérica GA.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

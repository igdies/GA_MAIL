CLASS zcl_ga_email DEFINITION
  PUBLIC
  INHERITING FROM cl_bcs_message
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Permitted Document Types</p>
    CONSTANTS:
      BEGIN OF document_type,
        txt    TYPE so_obj_tp VALUE 'txt',
        binary TYPE so_obj_tp VALUE 'BIN',
        html   TYPE so_obj_tp VALUE 'HTM',
        raw    TYPE so_obj_tp VALUE 'RAW',
      END OF document_type.

    " ==============================================================================
    " 1. INSTANTIATION
    " ==============================================================================
    CLASS-METHODS get_driver_class
      IMPORTING
        !ip_app                    TYPE csequence
      RETURNING
        VALUE(ro_driver_class_def) TYPE REF TO cl_abap_classdescr .
    CLASS-METHODS create
      IMPORTING
        !io_object         TYPE REF TO object OPTIONAL
        !io_data           TYPE REF TO data OPTIONAL
        VALUE(ip_app)      TYPE csequence OPTIONAL
        VALUE(ip_subclass) TYPE REF TO cl_abap_typedescr OPTIONAL
      RETURNING
        VALUE(ro_email)    TYPE REF TO zcl_ga_email .


    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "! @parameter io_object | <p class="shorttext synchronized" lang="en">Optional Object Reference</p>
    "! @parameter io_data | <p class="shorttext synchronized" lang="en">Optional Data Reference</p>
    METHODS constructor
      IMPORTING
        !io_object TYPE REF TO object OPTIONAL
        !io_data   TYPE REF TO data OPTIONAL.

    " ==============================================================================
    " 2. CORE STATE MANAGEMENT (FINAL: Prevent breaking core mappings)
    " ==============================================================================
    "! <p class="shorttext synchronized" lang="en">Get current prepared email object</p>
    "! @returning rs_mail | <p class="shorttext synchronized" lang="en">Current Mail State</p>
    METHODS content_get FINAL
      RETURNING
        VALUE(rs_mail) TYPE zga_s_email.

    "! <p class="shorttext synchronized" lang="en">Explicitly set email object</p>
    "! @parameter is_mail | <p class="shorttext synchronized" lang="en">Mail State to set</p>
    METHODS content_set FINAL
      IMPORTING
        !is_mail TYPE zga_s_email.

    " ==============================================================================
    " 3. CONFIGURE PLACEHOLDERS (FINAL & FLUENT API for chaining)
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Set a standard string placeholder replacement</p>
    "! @parameter placeholder_name | <p class="shorttext synchronized" lang="en">Placeholder key (e.g., &USER&)</p>
    "! @parameter placeholder_value | <p class="shorttext synchronized" lang="en">Value to replace with</p>
    "! @returning ro_me | <p class="shorttext synchronized" lang="en">Returns self for method chaining</p>
    METHODS set_placeholder FINAL
      IMPORTING
        !placeholder_name  TYPE string
        !placeholder_value TYPE string
      RETURNING
        VALUE(ro_me)       TYPE REF TO zcl_ga_email.

    "! <p class="shorttext synchronized" lang="en">Convert internal table to HTML table and set as placeholder</p>
    "! @parameter placeholder_name | <p class="shorttext synchronized" lang="en">Placeholder key (e.g., &TABLE_DATA&)</p>
    "! @parameter placeholder_itab | <p class="shorttext synchronized" lang="en">Internal Table with data</p>
    "! @returning ro_me | <p class="shorttext synchronized" lang="en">Returns self for method chaining</p>
    METHODS set_placeholder_itab FINAL
      IMPORTING
        !placeholder_name TYPE string
        !placeholder_itab TYPE ANY TABLE
      RETURNING
        VALUE(ro_me)      TYPE REF TO zcl_ga_email.
    "! <p class="shorttext synchronized" lang="en">Add members of an SAP Distribution List as recipients</p>
    "! @parameter dlinam | <p class="shorttext synchronized" lang="en">Distribution List Name</p>
    "! @parameter copy | <p class="shorttext synchronized" lang="en">Copy type (e.g., CC, BCC)</p>
    "! @returning ro_me | <p class="shorttext synchronized" lang="en">Returns self for method chaining</p>
    METHODS add_dl_recipients
      IMPORTING
                !dlinam      TYPE so_dli_nam
                !copy        TYPE bcs_copy OPTIONAL
      RETURNING VALUE(ro_me) TYPE REF TO zcl_ga_email.
    " ==============================================================================
    " 4. TEMPLATES & CONTENT RETRIEVAL (OPEN: Allow subclasses to override)
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Get Subject and HTML from S/4HANA Email Framework</p>
    "! @parameter ip_tpl | <p class="shorttext synchronized" lang="en">Template ID (SMTG_TMPL_HDR)</p>
    "! @parameter ip_language | <p class="shorttext synchronized" lang="en">Target Language</p>
    "! @parameter op_subject | <p class="shorttext synchronized" lang="en">Exported generated subject</p>
    "! @parameter op_html | <p class="shorttext synchronized" lang="en">Exported generated HTML body</p>
    "! @returning r_ok | <p class="shorttext synchronized" lang="en">True if successful</p>
    "! @raising zcx_ga_util | <p class="shorttext synchronized" lang="en">Exceptions from API rendering wrapper</p>
    METHODS get_template
      IMPORTING
        !ip_tpl      TYPE smtg_tmpl_hdr-id
        !ip_language TYPE bcs_language DEFAULT sy-langu
      EXPORTING
        !op_subject  TYPE string
        !op_html     TYPE string
      RETURNING
        VALUE(r_ok)  TYPE abap_bool
      RAISING
        zcx_ga_util.

    "! <p class="shorttext synchronized" lang="en">Get HTML from SMW0 template with placeholders resolved</p>
    "! @parameter iv_template | <p class="shorttext synchronized" lang="en">SMW0 Template Name</p>
    "! @returning rv_html | <p class="shorttext synchronized" lang="en">Resulting HTML String</p>
    METHODS get_template_smw0
      IMPORTING
        !iv_template   TYPE swww_t_template_name
      RETURNING
        VALUE(rv_html) TYPE string.

    "! <p class="shorttext synchronized" lang="en">Read standard text (SO10) into string</p>
    "! @parameter ip_text_name | <p class="shorttext synchronized" lang="en">Text name</p>
    "! @parameter ip_language | <p class="shorttext synchronized" lang="en">Text language</p>
    "! @parameter ip_doctype | <p class="shorttext synchronized" lang="en">Document Type</p>
    "! @parameter ip_tdid | <p class="shorttext synchronized" lang="en">Text ID</p>
    "! @parameter ip_tdobject | <p class="shorttext synchronized" lang="en">Text Object</p>
    "! @returning r_contents_txt | <p class="shorttext synchronized" lang="en">String containing SO10 contents</p>
    METHODS read_so10_text
      IMPORTING
        !ip_text_name         TYPE tdobname
        !ip_language          TYPE bcs_language DEFAULT sy-langu
        !ip_doctype           TYPE bcs_doctype DEFAULT 'txt'
        !ip_tdid              TYPE thead-tdid DEFAULT 'ST'
        !ip_tdobject          TYPE thead-tdobject DEFAULT 'TEXT'
      RETURNING
        VALUE(r_contents_txt) TYPE string.

    " ==============================================================================
    " 4B. APPLY TEMPLATES (FLUENT API for direct injection into ms_mail)
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Apply S/4HANA Email Template directly to mail state</p>
    METHODS apply_template
      IMPORTING
        !ip_tpl      TYPE smtg_tmpl_hdr-id
        !ip_language TYPE bcs_language DEFAULT sy-langu
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_ga_email
      RAISING
        zcx_ga_util.

    "! <p class="shorttext synchronized" lang="en">Apply HTML from SMW0 template directly to body</p>
    METHODS apply_template_smw0
      IMPORTING
        !iv_template TYPE swww_t_template_name
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_ga_email.
    "! <p class="shorttext synchronized" lang="en">Apply custom body content and document type</p>
    METHODS apply_body
      IMPORTING
        !ip_body     TYPE string
        !ip_doctype  TYPE so_obj_tp DEFAULT document_type-txt
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_ga_email.
    "! <p class="shorttext synchronized" lang="en">Apply SO10 standard text to mail body</p>
    METHODS apply_body_so10
      IMPORTING
        !ip_text_name TYPE tdobname
        !ip_language  TYPE bcs_language DEFAULT sy-langu
        !ip_doctype   TYPE bcs_doctype DEFAULT 'txt'
        !ip_tdid      TYPE thead-tdid DEFAULT 'ST'
        !ip_tdobject  TYPE thead-tdobject DEFAULT 'TEXT'
      RETURNING
        VALUE(ro_me)  TYPE REF TO zcl_ga_email.

    "! <p class="shorttext synchronized" lang="en">Apply SO10 standard text to mail subject</p>
    METHODS apply_subject_so10
      IMPORTING
        !ip_text_name TYPE tdobname
        !ip_language  TYPE bcs_language DEFAULT sy-langu
        !ip_tdid      TYPE thead-tdid DEFAULT 'ST'
        !ip_tdobject  TYPE thead-tdobject DEFAULT 'TEXT'
      RETURNING
        VALUE(ro_me)  TYPE REF TO zcl_ga_email.
    " ==============================================================================
    " 4C. FLUENT CONFIGURATION (OPEN: Allow subclasses to intercept/redefine)
    " ==============================================================================
    METHODS apply_subject
      IMPORTING
        !ip_subject  TYPE csequence
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_ga_email.
    "! <p class="shorttext synchronized" lang="en">Add a single recipient</p>
    METHODS apply_receiver
      IMPORTING
*        !iv_email    TYPE ad_smtpadr
*        !iv_copy     TYPE abap_bool DEFAULT abap_false
*        !iv_blind    TYPE abap_bool DEFAULT abap_false
        is_receiver  TYPE zga_s_email_receiver
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_ga_email.
    METHODS apply_receivers
      IMPORTING it_receivers TYPE zga_t_email_receivers
      RETURNING
                VALUE(ro_me) TYPE REF TO zcl_ga_email.
    "! <p class="shorttext synchronized" lang="en">Add an attachment to the email state</p>
    METHODS apply_attachment
      IMPORTING
        !is_attachment TYPE zga_s_email_attachment
      RETURNING
        VALUE(ro_me)   TYPE REF TO zcl_ga_email.
    METHODS apply_attachments
      IMPORTING
        !it_attachment TYPE zga_t_email_attachments
      RETURNING
        VALUE(ro_me)   TYPE REF TO zcl_ga_email.

    "! <p class="shorttext synchronized" lang="en">Set sender of the email</p>
    METHODS apply_sender
      IMPORTING
        !iv_email    TYPE ad_smtpadr OPTIONAL
        !iv_user     TYPE syuname OPTIONAL
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_ga_email.
    "! <p class="shorttext synchronized" lang="en">Configure if email is sent immediately or queued</p>
    METHODS apply_send_immediately
      IMPORTING
        !iv_immediately TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_me)    TYPE REF TO zcl_ga_email.
    " ==============================================================================
    " 5. EXECUTION (FINAL: Prevent altering the SAP standard Send pipeline)
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Execute standard BCS transmission of the prepared email</p>
    "! @parameter i_via_dialog | <p class="shorttext synchronized" lang="en">Send via SAP GUI Popup / Dialog</p>
    "! @returning r_ok | <p class="shorttext synchronized" lang="en">True if queued successfully</p>
    "! @raising zcx_ga_util | <p class="shorttext synchronized" lang="en">Catches and wraps underlying BCS exceptions</p>
    METHODS send_mail FINAL
      IMPORTING
        !i_via_dialog TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(r_ok)   TYPE abap_bool
      RAISING
        zcx_ga_util.

    " ==============================================================================
    " 6. UTILITIES (STATIC)
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Convert purely binary XSTRING to email attachment structure</p>
    "! @parameter content | <p class="shorttext synchronized" lang="en">XSTRING content</p>
    "! @parameter type | <p class="shorttext synchronized" lang="en">Attachment doc type (EXT by default)</p>
    "! @parameter subject | <p class="shorttext synchronized" lang="en">Filename / Subject</p>
    "! @parameter size | <p class="shorttext synchronized" lang="en">Optional file size</p>
    "! @returning rs_attachment | <p class="shorttext synchronized" lang="en">Properly formatted attachment structure</p>
    CLASS-METHODS xstring_to_attachment
      IMPORTING
        !content             TYPE xstring
        !type                TYPE so_obj_tp OPTIONAL
        !subject             TYPE so_obj_des
        !size                TYPE so_obj_len OPTIONAL
      RETURNING
        VALUE(rs_attachment) TYPE zga_s_email_attachment.

    "! <p class="shorttext synchronized" lang="en">Compress multiple uncompressed attachments into a ZIP file</p>
    "! @parameter it_attachments | <p class="shorttext synchronized" lang="en">Table of standard attachments to compress</p>
    "! @parameter i_zip_filename | <p class="shorttext synchronized" lang="en">Subject name for ZIP file</p>
    "! @returning rs_zip_attachment | <p class="shorttext synchronized" lang="en">Single compacted ZIP attachment structure</p>
    CLASS-METHODS add_zip_attachments
      IMPORTING
        !it_attachments          TYPE zga_t_email_attachments
        !i_zip_filename          TYPE so_obj_des DEFAULT 'archive.zip'
      RETURNING
        VALUE(rs_zip_attachment) TYPE zga_s_email_attachment.

    "! <p class="shorttext synchronized" lang="en">Get user's stored email address</p>
    "! @parameter i_user | <p class="shorttext synchronized" lang="en">SAP Username (sy-uname)</p>
    "! @returning r_email | <p class="shorttext synchronized" lang="en">Resolved Email string</p>
    CLASS-METHODS get_email_from_user
      IMPORTING
        !i_user        TYPE syuname
      RETURNING
        VALUE(r_email) TYPE string.

    "! <p class="shorttext synchronized" lang="en">Validate format of a provided email string</p>
    "! @parameter emailid | <p class="shorttext synchronized" lang="en">SMTP Address to test</p>
    "! @returning is_emailid_valid | <p class="shorttext synchronized" lang="en">True if formatting is valid</p>
    CLASS-METHODS is_emailid_valid
      IMPORTING
        !emailid                TYPE ad_smtpadr
      RETURNING
        VALUE(is_emailid_valid) TYPE abap_bool.

  PROTECTED SECTION.
    " ==============================================================================
    " 1. CORE STATE DATA
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Prepared Mail Parameters (Subject, Sender, Receivers, etc.)</p>
    DATA ms_mail TYPE zga_s_email.

    "! <p class="shorttext synchronized" lang="en">Optional generic object reference passed during instantiation</p>
    DATA mo_object TYPE REF TO object.

    "! <p class="shorttext synchronized" lang="en">Optional generic data reference passed during instantiation</p>
    DATA mo_data TYPE REF TO data.

  PRIVATE SECTION.
    " ==============================================================================
    " 1. INTERNAL STATE
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Memory cache for string placeholder replacement keys and values</p>
    DATA gt_data_key TYPE if_smtg_email_template=>ty_gt_data_key.

    " ==============================================================================
    " 2. HELPERS & FORMATTERS
    " ==============================================================================

    "! <p class="shorttext synchronized" lang="en">Iterates over gt_data_key and replaces all stored placeholders</p>
    "! @parameter replace_string | <p class="shorttext synchronized" lang="en">Original string containing '&PLACEHOLDERS&'</p>
    "! @returning result | <p class="shorttext synchronized" lang="en">Final string with all variable values applied</p>
    METHODS replace_placeholder
      IMPORTING
        !replace_string TYPE string
      RETURNING
        VALUE(result)   TYPE string.

    "! <p class="shorttext synchronized" lang="en">Checks if a given SAP Distribution List (SO15) is public or private</p>
    "! @parameter dl_name | <p class="shorttext synchronized" lang="en">Distribution list internal name</p>
    "! @returning r_shared_dl | <p class="shorttext synchronized" lang="en">'X' if public/shared, Space if private</p>
    METHODS is_dl_shared
      IMPORTING
        !dl_name           TYPE so_dli_nam
      RETURNING
        VALUE(r_shared_dl) TYPE so_text001.

    "! <p class="shorttext synchronized" lang="en">Validate and normalize Document Type</p>
    CLASS-METHODS normalize_doctype
      IMPORTING
        !iv_doctype     TYPE so_obj_tp
      RETURNING
        VALUE(rv_valid) TYPE so_obj_tp.
    CLASS-METHODS check_if_called_from_subclass
      RETURNING
        VALUE(ro_subclass) TYPE REF TO object .
    CLASS-METHODS is_valid_subclass
      IMPORTING
        !io_subclass TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(r_ok)  TYPE abap_bool .
    CLASS-METHODS create_object
      IMPORTING
        !i_subclass   TYPE REF TO cl_abap_typedescr
        !io_object    TYPE REF TO object OPTIONAL
        !io_data      TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(r_mail) TYPE REF TO zcl_ga_email .

    "! <p class="shorttext synchronized" lang="en">Factory method to get an instance of the email class</p>
    "! @parameter i_implementation_class | <p class="shorttext synchronized" lang="en">Target class for instantiation</p>
    "! @parameter io_object | <p class="shorttext synchronized" lang="en">Optional Object Reference</p>
    "! @parameter io_data | <p class="shorttext synchronized" lang="en">Optional Data Reference</p>
    "! @returning ro_instance | <p class="shorttext synchronized" lang="en">Instance of zcl_ga_email</p>
    CLASS-METHODS get_instance
      IMPORTING
        !i_implementation_class TYPE seoclname DEFAULT 'ZCL_GA_EMAIL'
        !io_object              TYPE REF TO object OPTIONAL
        !io_data                TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(ro_instance)      TYPE REF TO zcl_ga_email.
ENDCLASS.



CLASS zcl_ga_email IMPLEMENTATION.


  METHOD add_dl_recipients.
    DATA :
      li_dli TYPE TABLE OF sodlienti1.

    " Determine whether the Distribution list is public or private
    DATA(lv_shared_dli) = is_dl_shared( dlinam ).

    " Request members of the DL
    CALL FUNCTION 'SO_DLI_READ_API1'
      EXPORTING
        dli_name                   = dlinam
        shared_dli                 = lv_shared_dli
      TABLES
        dli_entries                = li_dli
      EXCEPTIONS
        dli_not_exist              = 1
        operation_no_authorization = 2
        parameter_error            = 3
        x_error                    = 4
        OTHERS                     = 5.

    IF sy-subrc = 0.
      " Add each member to the internal receivers list through the BCS API
      LOOP AT li_dli INTO DATA(ls_dli).
        APPEND VALUE #(
          ad_smtpadr = ls_dli-member_adr
          copy       = COND #( WHEN copy = gc_cc THEN abap_true ELSE abap_false )
          blind_copy = COND #( WHEN copy = gc_bcc THEN abap_true ELSE abap_false )
        ) TO ms_mail-t_receiver.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD add_zip_attachments.

    DATA: lo_zip          TYPE REF TO cl_abap_zip,
          lv_file_xstring TYPE xstring,
          lv_zip_xstring  TYPE xstring.

    IF it_attachments IS NOT INITIAL.
      lo_zip = NEW cl_abap_zip( ).

      LOOP AT it_attachments INTO DATA(ls_attachment).
        " Convert the SOLIX content back to XSTRING for the ZIP class
        lv_file_xstring = cl_bcs_convert=>solix_to_xstring( it_solix = ls_attachment-content_hex ).

        " Add file to the ZIP archive (using the subject as the internal filename)
        lo_zip->add( name    = CONV #( ls_attachment-subject )
                     content = lv_file_xstring ).
      ENDLOOP.

      " Generate the final ZIP binary
      lv_zip_xstring = lo_zip->save( ).

      " Use your existing utility to return the formatted attachment
      rs_zip_attachment = xstring_to_attachment(
                              content = lv_zip_xstring
                              type    = 'ZIP'
                              subject = i_zip_filename ).
    ENDIF.
  ENDMETHOD.


  METHOD apply_attachment.
    " Añade el adjunto directamente a la cola de ms_mail
    CHECK is_attachment IS NOT INITIAL.
    APPEND is_attachment TO ms_mail-t_attachments.
    ro_me = me.
  ENDMETHOD.


  METHOD apply_attachments.
    LOOP AT it_attachment INTO DATA(ls_att).
      apply_attachment( ls_att ).
    ENDLOOP.
  ENDMETHOD.

  METHOD apply_body.
    " Inject content string directly into the mail state
    ms_mail-body      = ip_body.

    " Normalize to ensure a valid BCS document type is used
    ms_mail-body_type = normalize_doctype( ip_doctype ).

    ro_me = me.
  ENDMETHOD.

  METHOD apply_body_so10.
    " Inject directly into the mail state
    ms_mail-body = read_so10_text(
                     ip_text_name = ip_text_name
                     ip_language  = ip_language
                     ip_doctype   = ip_doctype
                     ip_tdid      = ip_tdid
                     ip_tdobject  = ip_tdobject ).

    ms_mail-body_type = ip_doctype.

    ro_me = me.
  ENDMETHOD.


  METHOD apply_receiver.
    " Validación Básica: No añadir en blanco
    CHECK is_receiver-ad_smtpadr IS NOT INITIAL.

    " Validación estructural de email (opcional, pero buena práctica)
    IF is_emailid_valid( is_receiver-ad_smtpadr ) = abap_false.
      " Aquí podrías logear el error, tirar excepción o simplemente salir
      RETURN.
    ENDIF.

    APPEND is_receiver
     TO ms_mail-t_receiver.

    ro_me = me.
  ENDMETHOD.


  METHOD apply_receivers.
    LOOP AT it_receivers INTO DATA(ls_receiver).
      apply_receiver( ls_receiver ).
    ENDLOOP.
    ro_me = me.
  ENDMETHOD.


  METHOD apply_sender.
    " 1. Si me pasan un correo directo, lo aplico como remitente principal
    IF iv_email IS NOT INITIAL.
      ms_mail-sender_email = iv_email.

      " 2. Si no hay correo, pero se proporcionó un usuario (o llegó sy-uname por default)
      " Resuelve el correo HR/SMTP asociado y lo asigna
    ELSEIF iv_user IS NOT INITIAL.
      ms_mail-sender_email = CONV #( get_email_from_user( i_user = iv_user ) ).
    ENDIF.

    ro_me = me.
  ENDMETHOD.
  METHOD apply_send_immediately.
    " Update parent BC_MESSAGE inherited flag
    me->set_send_immediately( iv_immediately = iv_immediately ).
    ro_me = me.
  ENDMETHOD.

  METHOD apply_subject.
    ms_mail-subject = ip_subject.
    ro_me = me.
  ENDMETHOD.


  METHOD apply_subject_so10.
    " Inject directly into the mail state
    ms_mail-subject = read_so10_text(
                        ip_text_name = ip_text_name
                        ip_language  = ip_language
                        ip_doctype   = 'txt' " Subjects are plain text
                        ip_tdid      = ip_tdid
                        ip_tdobject  = ip_tdobject ).

    ro_me = me.
  ENDMETHOD.


  METHOD apply_template.
    DATA: lv_subject TYPE string,
          lv_html    TYPE string.

    " Use your existing get_template logic
    get_template( EXPORTING ip_tpl      = ip_tpl
                            ip_language = ip_language
                  IMPORTING op_subject  = lv_subject
                            op_html     = lv_html ).

    " Inject directly into the mail state
    ms_mail-subject   = lv_subject.
    ms_mail-body      = lv_html.
    ms_mail-body_type = document_type-html.

    ro_me = me.
  ENDMETHOD.


  METHOD apply_template_smw0.
    " Inject directly into the mail state
    ms_mail-body      = get_template_smw0( iv_template = iv_template ).
    ms_mail-body_type = document_type-html.

    ro_me = me.
  ENDMETHOD.


  METHOD constructor.
    " Always call the superclass (cl_bcs_message) constructor first
    super->constructor( ).
    mo_object = io_object.
    mo_data   = io_data.
  ENDMETHOD.


  METHOD content_get.
    " Return the current state of the mail
    rs_mail = ms_mail.
  ENDMETHOD.


  METHOD content_set.
    " Directly overwrite the current state of the mail
    ms_mail = is_mail.
  ENDMETHOD.


  METHOD get_email_from_user.
    DATA: rc  TYPE                   sy-subrc,
          err TYPE STANDARD TABLE OF rpbenerr.

    " Fetches SMTP address assigned to an SAP username via HR infotypes
    CALL FUNCTION 'HR_FBN_GET_USER_EMAIL_ADDRESS'
      EXPORTING
        user_id       = i_user
        reaction      = 'N'
      IMPORTING
        subrc         = rc
        email_address = r_email
      TABLES
        error_table   = err.
  ENDMETHOD.
  METHOD create.
    "logic based on ZCL_FALV=>create method.
    "passed class by caller
    IF ip_subclass IS BOUND AND NOT is_valid_subclass(  ip_subclass  ).
      FREE ip_subclass.
    ENDIF.
    "not valid/not supplied
    IF ip_subclass IS INITIAL.
      "check if called from local class.
      ip_subclass ?= check_if_called_from_subclass( ).
      IF ip_subclass IS INITIAL.
        "not valid/not supplied
        "get_driver_class from config
        ip_subclass = get_driver_class( ip_app ).
      ENDIF.
    ENDIF.


    "object creation
    ro_email = create_object(
                   i_subclass = ip_subclass
                   io_data = io_data
                   io_object = io_object
                 ).

  ENDMETHOD.
  METHOD get_driver_class.
    DATA: ld_class_name TYPE string.
    "Implement here the logic to be defined/implemented


    "if we have class name, try to build its definition using rtts
    IF ld_class_name IS NOT INITIAL.
      ro_driver_class_def ?= cl_abap_classdescr=>describe_by_name( CONV #( ld_class_name ) ).
      IF NOT is_valid_subclass( ro_driver_class_def ).
        FREE ro_driver_class_def.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD check_if_called_from_subclass.
    "this method check if it is calles from a local inherited class and its method create
    DATA callstack TYPE abap_callstack.
    DATA src       TYPE TABLE OF string.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

    ASSIGN callstack[ 3 ] TO FIELD-SYMBOL(<stack>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DATA(compiler) = cl_abap_compiler=>create( p_name             = <stack>-mainprogram
                                               p_include          = <stack>-include
                                               p_no_package_check = abap_true ).

    compiler->get_single_ref( EXPORTING  p_full_name = |\\TY:ZCL_GA_EMAIL\\ME:{ callstack[ 2 ]-blockname CASE = UPPER }|
                                         p_grade     = 1   " Grade of Use
                              IMPORTING  p_result    = DATA(delivery_references) " Where-Used List
                              EXCEPTIONS OTHERS      = 5 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ REPORT <stack>-include INTO src.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN src[ <stack>-line ] TO FIELD-SYMBOL(<line>).
    IF <line> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    ASSIGN delivery_references[ line = <stack>-line ] TO FIELD-SYMBOL(<reference>).
    IF sy-subrc = 0.
      DATA subclass_name TYPE string.
      DO.
        DATA(offset) = <reference>-column - sy-index - 2. "-2 because of =>
        IF offset < 0 OR <line>+offset(1) = ` `.
          EXIT.
        ENDIF.
        subclass_name = <line>+offset(1) && subclass_name.
      ENDDO.
      IF subclass_name IS INITIAL OR to_upper( subclass_name ) = 'ZCL_GA_EMAIL'.
        RETURN.
      ENDIF.

      " global class
      cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = to_upper( subclass_name )
                                            RECEIVING  p_descr_ref    = ro_subclass
                                            EXCEPTIONS type_not_found = 1 ).
      IF sy-subrc <> 0.
        " local class
        subclass_name = |\\PROGRAM={ <stack>-mainprogram }\\CLASS={ subclass_name }|.
        cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = to_upper( subclass_name )
                                              RECEIVING  p_descr_ref    = ro_subclass
                                              EXCEPTIONS type_not_found = 1 ).

      ENDIF.
      IF sy-subrc = 0.
        IF NOT is_valid_subclass( CAST cl_abap_classdescr( ro_subclass ) ).
          FREE ro_subclass.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD create_object.
    IF i_subclass IS NOT INITIAL.
      DATA subclass TYPE REF TO object.
      DATA(sublcass_abs_name) = i_subclass->absolute_name.
      CREATE OBJECT subclass TYPE (sublcass_abs_name)
         EXPORTING io_data = io_data
                   io_object = io_object.
      r_mail ?= subclass.
*      rv_falv->subclass_type = i_subclass.

    ELSE.
      r_mail = NEW #( io_data = io_data
                   io_object = io_object ).
    ENDIF.
  ENDMETHOD.
  METHOD is_valid_subclass.

    r_ok = abap_false.
    IF io_subclass IS NOT BOUND.
      RETURN.
    ENDIF.
    "check that passed description object is a class.
    IF io_subclass->type_kind <> cl_abap_typedescr=>typekind_class.
      RETURN.
    ENDIF.
    "get the super/parent class
    DATA(lo_super_class) = CAST cl_abap_classdescr( io_subclass )->get_super_class_type( ).
    WHILE r_ok = abap_false AND lo_super_class IS BOUND.
      "is main class
      IF lo_super_class->absolute_name = '\CLASS=ZCL_GA_EMAIL'.
        r_ok = abap_true.
        EXIT.
      ENDIF.
      "get parent class.
      CALL METHOD lo_super_class->get_super_class_type
        RECEIVING
          p_descr_ref           = lo_super_class
        EXCEPTIONS
          super_class_not_found = 1
          OTHERS                = 2.
      "it is not a subclass.
      IF sy-subrc <> 0.
        r_ok = abap_false.
        FREE lo_super_class.
        RETURN.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_instance.
*    " Ensure a class name is provided, then dynamically create the object.
*    " This allows subclasses to be instantiated using the parent's factory method.
    CHECK i_implementation_class IS NOT INITIAL.

    CREATE OBJECT ro_instance TYPE (i_implementation_class)
      EXPORTING io_object = io_object
                io_data   = io_data.

  ENDMETHOD.


  METHOD get_template.
    " 1. Check if the modern email template (SMTG_TMPL_HDR) exists and is active
    SELECT SINGLE cds_view
      FROM smtg_tmpl_hdr
      INTO @DATA(ld_cds_view)
      WHERE id      EQ @ip_tpl
        AND version EQ 'A'. "GC_VERSION_ACTIVE

    IF sy-subrc EQ 0.

      " 2. If it is a CDS-backed template, prepare the keys to pass to the API
      DATA(lt_data_key) = VALUE if_smtg_email_template=>ty_gt_data_key( ).
      IF ld_cds_view IS NOT INITIAL.
        lt_data_key = gt_data_key.
      ENDIF.

      TRY.
          " Instantiate the standard API and render the template
          DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = ip_tpl ).

          lo_email_api->render(
            EXPORTING
              iv_language  = ip_language         " Use dynamic language parameter
              it_data_key  = lt_data_key         " Pass the configured keys (fixed)
            IMPORTING
              ev_subject   = DATA(ld_subject)
              ev_body_html = DATA(ld_body_html)
          ).

        CATCH cx_smtg_email_common INTO DATA(lx_smtg).
          " Properly bubble up the exception so developers know rendering failed
          RAISE EXCEPTION TYPE zcx_ga_util
            EXPORTING
              previous = lx_smtg.
      ENDTRY.

      " 3. FALLBACK: If the template is NOT CDS-backed, the API won't resolve placeholders automatically.
      " We must manually scan the output string and substitute the stored &PLACEHOLDERS&.
      IF ld_cds_view IS INITIAL.
        ld_subject   = replace_placeholder( ld_subject ).
        ld_body_html = replace_placeholder( ld_body_html ).
      ENDIF.

      " 4. Export the finalized values
      op_subject = ld_subject.
      op_html    = ld_body_html.
      r_ok       = abap_true.
    ELSE.
      r_ok = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD get_template_smw0.
    DATA: lt_html TYPE STANDARD TABLE OF soli.

    " Fetch the raw HTML template from SAP Web Repository (SMW0)
    CALL FUNCTION 'WWW_HTML_MERGER'
      EXPORTING
        template           = iv_template
      IMPORTING
        html_table         = lt_html
      EXCEPTIONS
        template_not_found = 1
        OTHERS             = 2.

    IF sy-subrc = 0.
      " Convert SOLI table to string format
      rv_html = cl_bcs_convert=>txt_to_string( it_soli = lt_html ).

      " Replace any placeholders that were already configured via set_placeholder
      rv_html = replace_placeholder( rv_html ).
    ENDIF.
  ENDMETHOD.


  METHOD is_dl_shared.
    SELECT SINGLE owntp, ownyr, ownno
    FROM soid INTO @DATA(ls_soid)
    WHERE objnam = @dl_name AND
          dlitp EQ 'DLI'.

    IF ls_soid IS INITIAL.
      " If there's no owner record, it's public
      r_shared_dl = 'X'.
    ELSE.
      " If owned by a specific ID, it's Private
      r_shared_dl = space.
    ENDIF.
  ENDMETHOD.
  METHOD normalize_doctype.
    " Comprueba si el tipo pasado se encuentra dentro de las constantes
    " definidas en la clase. Si está presente, lo asume; si está en minúsculas/raro,
    " lo auto-arreglas o lo descartas.
    rv_valid = SWITCH #( iv_doctype
                 WHEN document_type-html   THEN document_type-html
                 WHEN document_type-txt    THEN document_type-txt
                 WHEN document_type-binary THEN document_type-binary
                 WHEN document_type-raw    THEN document_type-raw
                 ELSE document_type-txt ). " <- FALLBACK por defecto
  ENDMETHOD.

  METHOD is_emailid_valid.
    DATA ls_address TYPE sx_address.
    ls_address-type = 'INT'.
    ls_address-address = emailid.

    " Checks strict formatting validation for an email string
    CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
      EXPORTING
        address_unstruct    = ls_address
      EXCEPTIONS
        error_address_type  = 1
        error_address       = 2
        error_group_address = 3
        OTHERS              = 4.

    IF sy-subrc EQ 0.
      is_emailid_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD read_so10_text.
    DATA: lv_no_of_lines LIKE sy-tabix,
          lv_changed(1)  TYPE c,
          lv_header      TYPE thead.

    IF ip_text_name IS INITIAL.
      RETURN.
    ENDIF.

    " 1. Use the utility class to read the text.
    " Benefit: We get automatic language fallback (Requested -> EN -> ES -> First Found)
    DATA(li_lines) = zcl_ga_util=>get_standard_text(
                       ip_tdname   = ip_text_name
                       ip_tdobject = ip_tdobject
                       ip_tdid     = ip_tdid
                       ip_spras    = ip_language
                     ).

    " Check if we successfully got lines back
    IF li_lines IS NOT INITIAL.

      " 2. Reconstruct the minimal header since TEXT_SYMBOL_REPLACE requires it
      lv_header-tdobject = ip_tdobject.
      lv_header-tdname   = ip_text_name.
      lv_header-tdid     = ip_tdid.
      lv_header-tdspras  = ip_language.

      " 3. Dynamically assign values to the TEXT_SYMBOLS inside the specific text
      LOOP AT gt_data_key INTO DATA(ls_data_key).
        CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
          EXPORTING
            name  = ls_data_key-name
            value = ls_data_key-value.
      ENDLOOP.

      DESCRIBE TABLE li_lines LINES lv_no_of_lines.

      " 4. Replace logic for SAP Native symbols in text lines
      CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
        EXPORTING
          endline       = lv_no_of_lines
          header        = lv_header
          init          = ' '
          option_dialog = ' '
          program       = sy-cprog
        IMPORTING
          changed       = lv_changed
          newheader     = lv_header
        TABLES
          lines         = li_lines.

      " 5. Flatten out the replaced internal table lines into a single string
      LOOP AT li_lines INTO DATA(lw_lines).
        " Ignore paragraph formats that indicate continuous text, add breaks for others
        IF lw_lines-tdformat = '='  OR
           lw_lines-tdformat = ' '.
          r_contents_txt = r_contents_txt && lw_lines-tdline.
        ELSE.
          r_contents_txt = r_contents_txt && cl_abap_char_utilities=>cr_lf && lw_lines-tdline.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD replace_placeholder.
    result = replace_string.

    " Execute generic string replacement (Used for HTML, etc.)
    LOOP AT gt_data_key INTO DATA(ls_data_key).
      REPLACE ALL OCCURRENCES OF ls_data_key-name IN result WITH ls_data_key-value.
    ENDLOOP.
  ENDMETHOD.


  METHOD send_mail.
    " 1. Dialog Execution (Shows Popup window).
    " The popup will subsequently call send_mail( i_via_dialog = abap_false ) automatically on accept.
    IF i_via_dialog = abap_true AND sy-batch IS INITIAL.
      CALL FUNCTION 'ZFM_EMAIL_DIALOG'
        EXPORTING
          io_mail    = me
          i_as_popup = abap_true
        IMPORTING
          e_ok       = r_ok.
      EXIT.
    ENDIF.

    " 2. Map local ms_mail fields into the parent class CL_BCS_MESSAGE

    " Sender
    IF ms_mail-sender_email IS NOT INITIAL.
      me->set_sender( iv_address = CONV #( ms_mail-sender_email ) ).
    ENDIF.

    " Recipients
    LOOP AT ms_mail-t_receiver INTO DATA(ls_receiver).
      me->add_recipient(
        iv_address = CONV #( ls_receiver-ad_smtpadr )
        iv_copy    = COND #( WHEN ls_receiver-blind_copy = abap_true THEN gc_bcc
                             WHEN ls_receiver-copy = abap_true THEN gc_cc )
      ).
    ENDLOOP.

    " Subject & Main Body
    me->set_subject( iv_subject = CONV #( ms_mail-subject ) ).
    ms_mail-body_type = normalize_doctype( ms_mail-body_type ).
    me->set_main_doc(
      iv_contents_txt = ms_mail-body
      iv_doctype      = ms_mail-body_type
    ).

    " Attachments
    LOOP AT ms_mail-t_attachments INTO DATA(ls_attachment).
      me->add_attachment(
        iv_doctype      = ls_attachment-type
        iv_filename     = CONV #( ls_attachment-subject )
        iv_contents_bin = cl_bcs_convert=>solix_to_xstring( it_solix = ls_attachment-content_hex )
      ).
    ENDLOOP.

    " 3. Transmit the email natively via BCS
    me->set_send_immediately( iv_immediately = abap_false ).
    DATA(lt_rec_status) = me->send( ).

    " Mark as successfully queued
    r_ok = abap_true.
  ENDMETHOD.


  METHOD set_placeholder.
    " Append the exact key-value pair to the placeholder cache
    APPEND  VALUE #( name = placeholder_name
                     value = placeholder_value )
     TO gt_data_key.
  ENDMETHOD.


  METHOD set_placeholder_itab.
    " Converts an internal table to an HTML table string and adds it as a placeholder
    " Assumes zcl_itab_to_html exists in the system to format the table visually.
    APPEND VALUE #(
      name  = placeholder_name
      value = NEW zcl_itab_to_html( )->convert( placeholder_itab )
    ) TO gt_data_key.
  ENDMETHOD.


  METHOD xstring_to_attachment.
    " Converts standard XSTRING binary files into SOLIX table attachments
    " formatted for the ms_mail-t_attachments internal table
    rs_attachment = VALUE #(
        type        = COND #( WHEN type IS INITIAL THEN 'EXT' ELSE type )
        subject     = subject
        size        = COND #( WHEN size IS NOT INITIAL THEN size ELSE xstrlen( content ) )
        content_hex = cl_document_bcs=>xstring_to_solix( content ) ).
  ENDMETHOD.


ENDCLASS.

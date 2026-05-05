# ZCL_GA_EMAIL

The `ZCL_GA_EMAIL` framework provides a standardized, fluent Object-Oriented API for constructing and sending emails in ABAP. It supports rich HTML/Text bodies, SAP SO10 texts, attachments, and integration with a UI dialog for user verification before sending.

Developers can choose to use the framework directly or inherit from it to map specific business logic securely (Data Transfer Objects or context injection).

---

## Table of Contents

1. [1. Overview](#1-overview)
2. [2. Basic Direct Usage (Fluent API)](#2-basic-direct-usage-fluent-api)
   - [2.1 Dialog Mode (i_via_dialog)](#21-dialog-mode-i_via_dialog)
3. [3. Architecture and Inheritance Model](#3-architecture-and-inheritance-model)
4. [4. Passing Context Data to Subclasses](#4-passing-context-data-to-subclasses)
   - [4.1 Passing Struct Data (io_data)](#41-passing-struct-data-io_data)
   - [4.2 Passing Object Context (io_object)](#42-passing-object-context-io_object)
5. [5. Real-World Implementation: ZCL_QM_NCR_EMAIL](#5-real-world-implementation-zcl_qm_ncr_email)
   - [5.1 Key Design Choices](#51-key-design-choices)
6. [6. Function Module Integration](#6-function-module-integration)


## 1. Overview

The `ZCL_GA_EMAIL` framework provides a standardized, fluent Object-Oriented API for constructing and sending emails in ABAP. It supports rich HTML/Text bodies, SAP SO10 texts, attachments, and integration with a UI dialog for user verification before sending.

Developers can choose to use the framework directly or inherit from it to map specific business logic securely (Data Transfer Objects or context injection).

> **Note/Acknowledgement:** Certain features and points in this framework were inspired by and reference the [ABAP-HTML-eMail](https://github.com/vidyadharg/ABAP-HTML-eMail) project.


---

## 2. Basic Direct Usage (Fluent API)

For simple scenarios, you can instantiate the base class directly utilizing its factory method, then chain its methods seamlessly to build the email payload.

_Reference: `zga_email_demo` - Test 5_

```abap
DATA lo_email TYPE REF TO zcl_ga_email.
lo_email = zcl_ga_email=>create( ).

TRY.
  lo_email->apply_body(
      ip_body    = 'This is a standard email body.'
      ip_doctype = zcl_ga_email=>document_type-txt
    )->apply_subject(
      ip_subject = 'Subject text'
    )->apply_receiver(
      is_receiver = VALUE #( ad_smtpadr = 'user@example.com' )
    )->send_mail(
      i_via_dialog = abap_true " Set to abap_false to send silently in background
    ).
CATCH zcx_ga_util INTO DATA(lx_error).
  " Handle framework exception
ENDTRY.
````

***

## 2.1 Dialog Mode (i\_via\_dialog)

When `i_via_dialog` is set to `abap_true` in the `send_mail` method, the framework opens an interactive review dialog before sending. This screen allows the user to validate and adjust the email content, with dedicated buttons to edit recipients and to manage attachments.



## 3. Architecture and Inheritance Model

To promote cleaner code and the single-responsibility principle, it is heavily recommended to inherit from `ZCL_GA_EMAIL` (or `ZCL_GA_EMAIL_DEMO`) when defining business-specific emails.

When inheriting, subclasses will typically:

1.  Redefine execution logic (for example, `create_and_send` or explicit business methods).
2.  Override the constructor to map dynamically provided attributes.

***

## 4. Passing Context Data to Subclasses

### 4.1 Passing Struct Data (io\_data)

Used for passing simple structures (for example, explicit subject and body literals).  
*Reference: `zga_email_demo` - Test 3*

**Subclass Definition**

```abap
CLASS lcl_email_data DEFINITION INHERITING FROM zcl_ga_email.
  " ...
ENDCLASS.

CLASS lcl_email_data IMPLEMENTATION.
  METHOD create_and_send.
    " mo_data is provided by the super class factory
    IF mo_data IS BOUND.
      ASSIGN mo_data->* TO FIELD-SYMBOL(<ls_data>).
      IF <ls_data> IS ASSIGNED.
        my_data = <ls_data>. " Map to local typed struct
      ENDIF.
    ENDIF.

    " proceed to use apply_... methods
  ENDMETHOD.
ENDCLASS.
```

**Caller Execution**

```abap
DATA ls_email_data TYPE ty_s_mail_2.
ls_email_data-subject = 'Dynamic Subject'.
lo_email ?= lcl_email_data=>create( io_data = REF #( ls_email_data ) ).
```

***

### 4.2 Passing Object Context (io\_object)

Used for passing robust objects (typically a reference to the main business logic object, standardizing it as a Data Provider).  
*Reference: `zga_email_demo` - Test 4*

**Subclass Definition**

```abap
CLASS lcl_email_object IMPLEMENTATION.
  METHOD create_and_send.
    DATA lo_provider TYPE REF TO lcl_email_provider.

    IF mo_object IS BOUND.
      TRY.
          lo_provider ?= mo_object. " Downcast the generic object
        CATCH cx_sy_move_cast_error.
          RETURN.
      ENDTRY.
    ENDIF.

    " Apply dynamic data securely fetched from lo_provider object
  ENDMETHOD.
ENDCLASS.
```

***

## 5. Real-World Implementation: ZCL\_QM\_NCR\_EMAIL

### 5.1 Key Design Choices

1.  **Constructor redefinition:** Safely overrides the constructor, calls `super->constructor`, and binds `io_object` to a typed reference (`mo_ncr`).

    ```abap
    METHOD constructor.
      super->constructor( io_object = io_object io_data = io_data ).
      mo_ncr ?= io_object. " mo_ncr is strongly typed as REF TO zcl_qm_ncr
    ENDMETHOD.
    ```

2.  **Domain-specific methods:** Uses business-focused APIs (for example, `build_and_send_ncr`) and internal retrieval methods (`get_ncr_receivers`) instead of overriding generic execution methods.

3.  **Advanced fluent APIs:** Leverages `apply_body_so10( )`, `apply_attachment( )`, and `apply_sender( )` to standardize templates, attachments, and sender resolution.

4.  **Caller logic in ZCL\_QM\_NCR:** Injects `ME` into the email builder via the factory, offloading rendering to the subclass.

```abap
" Caller logic (inside ZCL_QM_NCR)
METHOD send_email.
  gather_ncr_data( ).
  DATA lo_ncr_mail TYPE REF TO zcl_qm_ncr_email.

  " Self-injection to the email builder
  lo_ncr_mail ?= zcl_qm_ncr_email=>create( io_object = me ).

  lo_ncr_mail->build_and_send_ncr(
    is_file      = is_file
    i_via_dialog = ip_via_dialog
  ).
ENDMETHOD.
```

***

## 6. Function Module Integration

*Reference: `zga_email_demo` - Test 6*

```abap
DATA lo_fm_email TYPE REF TO zcl_ga_email.
lo_fm_email = zcl_ga_email=>create( ).
" Construct the instance via fluent API

" Pass the constructed instance manually into the FM
CALL FUNCTION 'ZFM_EMAIL_DIALOG'
  EXPORTING
    io_mail    = lo_fm_email
    i_as_popup = abap_true.
```

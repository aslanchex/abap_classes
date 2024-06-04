class ZCL_FIINT_NOTIFICATION definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_notif_params,
           it_attachments TYPE  ztt_mail_attachments,
           iv_message     TYPE  string,
           iv_msg_type    TYPE  string,
           it_log         TYPE  bal_t_msg,
         END OF ts_notif_params .
  types:
    BEGIN OF ts_params,
           iv_pt_number   TYPE zepi_ifc_unit,
           iv_pt_name     TYPE zepi_ifc_description,
           iv_msg_type    TYPE string,
           iv_sed_status  TYPE ze_recon_result,
           it_attachments TYPE ztt_mail_attachments,
           it_log         TYPE bal_t_msg,
           iv_message     TYPE string,
         END OF ts_params .
  types:
    BEGIN OF ty_role,
        uname    TYPE agr_users-uname,
        agr_name TYPE agr_users-agr_name,
      END OF ty_role .
  types:
    BEGIN OF ty_email,
        smtp_addr TYPE ad_smtpadr,
        bname     TYPE char60,
      END OF ty_email .
  types:
    BEGIN OF ty_user,
        bname TYPE xubname,
      END OF ty_user .
  types:
    tt_user  TYPE SORTED TABLE OF ty_user WITH UNIQUE KEY bname .
  types:
    tt_email TYPE STANDARD TABLE OF ty_email .
  types:
    tt_role  TYPE STANDARD TABLE OF ty_role .
  types:
    tt_msg TYPE STANDARD TABLE OF ztmail_text .

  data MV_NEED_TO_SEND type BOOLE_D .

  class-methods GET_INSTANCE
    importing
      !IV_BELNR_ABS type ZE_CFTAGREEMENTID
    returning
      value(RO_INSTANCE) type ref to ZCL_FIINT_NOTIFICATION .
  methods CONSTRUCTOR
    importing
      !IV_BELNR_ABS type ZE_CFTAGREEMENTID .
  methods SEND_NOTIF
    importing
      !IS_UNIT type ANY optional
      !IS_PARAMS type TS_PARAMS .
  methods GET_MV_XML
    returning
      value(R_RESULT) type XSTRING .
  methods SET_MV_XML
    importing
      !MV_XML type XSTRING .
  class-methods FREE .
  PROTECTED SECTION.
private section.

  class-data MO_NOTIF type ref to ZCL_FIINT_NOTIFICATION .
  data MV_BELNR_ABS type ZE_CFTAGREEMENTID .
  data MT_MSG type TT_MSG .
  data MV_SEC_LEVEL type CHAR4 .
  data MV_XML type XSTRING .

  methods GET_MSG .
  methods SEND
    importing
      !IS_EMAIL type TY_EMAIL
      !IV_TOPIC_MSG type ZE_TOPIC_MSG
      !IV_TEXT_MSG type ZE_TEXT_MSG
      !IT_PLC_HLD type ZTT_MAIL_PLACEHOLDERS_DATA
      !IT_PLC_HLD_TOPIC type ZTT_MAIL_PLACEHOLDERS_DATA optional
      !IT_ATTACHMENTS type ZTT_MAIL_ATTACHMENTS optional .
  methods GET_EMAIL
    importing
      !IV_MSG_NUM type ZE_VIEW_MSG
    exporting
      !ET_EMAIL type TT_EMAIL .
  methods NOTIF_FI_Z069
    importing
      !IV_PT_NUMBER type ZEPI_IFC_UNIT optional
      !IV_PT_NAME type ZEPI_IFC_DESCRIPTION optional
      !IT_ATTACHMENTS type ZTT_MAIL_ATTACHMENTS optional .
  methods NOTIFICATION
    importing
      !IS_PARAMS type TS_NOTIF_PARAMS .
  methods NOTIF_RE_PT139
    importing
      !IV_PT_NUMBER type ZEPI_IFC_UNIT
      !IV_PT_NAME type ZEPI_IFC_DESCRIPTION
      !IT_ATTACHMENTS type ZTT_MAIL_ATTACHMENTS
      !IV_MSG_TYPE type STRING
      !IT_LOG type BAL_T_MSG
      !IS_UNIT type ANY .
ENDCLASS.



CLASS ZCL_FIINT_NOTIFICATION IMPLEMENTATION.


  METHOD constructor.
    mv_belnr_abs = iv_belnr_abs.
  ENDMETHOD.


  METHOD get_email.

    SELECT email AS smtp_addr , fio AS bname FROM ztmail_email WHERE view_msg = @iv_msg_num INTO TABLE @et_email.

    SORT et_email BY bname.
    DELETE ADJACENT DUPLICATES FROM et_email COMPARING bname.
  ENDMETHOD.


  METHOD get_msg.
    DATA: lr_not_send TYPE RANGE OF ztmail_text-name_msg.

    zcl_tvarvc_params_get=>get_range(
      EXPORTING
        iv_name  = 'ZSRCM_NOT_SEND_MAIL'
      IMPORTING
        er_range = lr_not_send
        ).

    IF lr_not_send IS INITIAL.
      APPEND 'IEQ' TO lr_not_send.
    ENDIF.

    SELECT *
    FROM ztmail_text
    WHERE view_msg NOT IN @lr_not_send
    INTO TABLE @mt_msg.
  ENDMETHOD.


  METHOD notif_fi_z069.

    DATA: lc_fi_z069 TYPE string VALUE 'FI_Z069'.

    DATA: lt_plc_hld  TYPE ztt_mail_placeholders_data.

    get_email( EXPORTING iv_msg_num = CONV #( lc_fi_z069 )
               IMPORTING et_email = DATA(lt_email) ).

    CHECK lt_email IS NOT INITIAL.

    READ TABLE mt_msg INTO DATA(ls_msg) WITH KEY view_msg = lc_fi_z069.
    IF sy-subrc = 0.

      LOOP AT lt_email ASSIGNING FIELD-SYMBOL(<ls_email>).
        CLEAR: lt_plc_hld.

        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1'
                                               data_placeholders = <ls_email>-bname ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&2'
                                               data_placeholders = iv_pt_number ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3'
                                               data_placeholders = iv_pt_name ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&4'
                                               data_placeholders = mv_belnr_abs ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&5'
                                               data_placeholders = |{ sy-datum DATE = USER }, { sy-timlo TIME = USER }| ) TO lt_plc_hld.

        send( EXPORTING is_email = <ls_email>
                        it_plc_hld = lt_plc_hld
                        iv_topic_msg = ls_msg-topic_msg
                        iv_text_msg = ls_msg-text_msg
                        it_attachments = it_attachments ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD send.
    DATA : lt_mail_send TYPE ztt_mail_send.

*    LOOP AT it_email INTO DATA(ls_email).
    APPEND VALUE zsmail_send( email              = is_email-smtp_addr
                              topic_msg          = iv_topic_msg
                              text_msg           = iv_text_msg
                              placeholders_body  = it_plc_hld
                              placeholders_topic = it_plc_hld_topic
                              attachments        = it_attachments ) TO lt_mail_send.
*    ENDLOOP.

    IF lt_mail_send IS NOT INITIAL.
      TRY.
          CALL FUNCTION 'Z_SEND_EMAIL'
            EXPORTING
              iv_update_task = abap_false
              iv_immediately = abap_true
            TABLES
              it_mail_send   = lt_mail_send.

        CATCH cx_bcs_send.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD send_notif.
    DATA: lc_fi_z069  TYPE string VALUE 'FI_Z069',
          lc_re_pt139 TYPE string VALUE 'RE_PT139'.
    " Вид FI_Z069
    get_msg( ).

    CASE is_params-iv_msg_type.
      WHEN lc_fi_z069.
        notif_fi_z069( iv_pt_number   = is_params-iv_pt_number
                       iv_pt_name     = is_params-iv_pt_name
                       it_attachments = is_params-it_attachments ).
      WHEN lc_re_pt139.
        notif_re_pt139( iv_pt_number   = is_params-iv_pt_number
                        iv_pt_name     = is_params-iv_pt_name
                        iv_msg_type    = is_params-iv_msg_type
                        it_attachments = is_params-it_attachments
                        it_log         = is_params-it_log
                        is_unit        = is_unit ).
      WHEN OTHERS.
        notification( is_params = VALUE #( iv_msg_type      = is_params-iv_msg_type
                                           iv_message       = is_params-iv_message
                                           it_attachments   = is_params-it_attachments
                                           it_log           = is_params-it_log ) ).

    ENDCASE.
  ENDMETHOD.


  METHOD free.
    FREE mo_notif.
  ENDMETHOD.


  METHOD get_instance.
    IF mo_notif IS NOT BOUND.
      mo_notif = NEW #( iv_belnr_abs = iv_belnr_abs ).
    ENDIF.

    ro_instance = mo_notif.
  ENDMETHOD.


  METHOD get_mv_xml.
    r_result = me->mv_xml.
  ENDMETHOD.


  METHOD set_mv_xml.
    me->mv_xml = mv_xml.
  ENDMETHOD.


  METHOD notification.

    DATA: lt_plc_hld TYPE ztt_mail_placeholders_data.

    get_email( EXPORTING iv_msg_num = CONV #( is_params-iv_msg_type )
               IMPORTING et_email = DATA(lt_email) ).

    IF lt_email IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE mt_msg INTO DATA(ls_msg) WITH KEY view_msg = is_params-iv_msg_type.
    IF sy-subrc = 0.

      LOOP AT lt_email ASSIGNING FIELD-SYMBOL(<ls_email>).
        CLEAR: lt_plc_hld.

        ls_msg-text_msg = |Добрый день, { <ls_email>-bname }. { is_params-iv_message }|.
        ls_msg-text_msg = |{ ls_msg-text_msg } Дата: { sy-datum DATE = USER } Время: { sy-timlo TIME = USER }|.

        send( EXPORTING is_email = <ls_email>
                        it_plc_hld = lt_plc_hld
                        iv_topic_msg = ls_msg-topic_msg
                        iv_text_msg = ls_msg-text_msg
                        it_attachments = is_params-it_attachments ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD NOTIF_RE_PT139.

    DATA: lt_plc_hld TYPE ztt_mail_placeholders_data,
          lv_message TYPE char255.

    FIELD-SYMBOLS: <ls_unit> TYPE zsre0033_pt0139_k.

    ASSIGN is_unit TO <ls_unit>.

    CLEAR: lv_message.

    get_email( EXPORTING iv_msg_num = CONV #( iv_msg_type )
               IMPORTING et_email = DATA(lt_email) ).

    CHECK lt_email IS NOT INITIAL.

    DATA(lo_log) = zcl_pi_log=>get_instance( ).

    READ TABLE mt_msg INTO DATA(ls_msg) WITH KEY view_msg = iv_msg_type.
    IF sy-subrc = 0.

      LOOP AT lt_email ASSIGNING FIELD-SYMBOL(<ls_email>).
        CLEAR: lt_plc_hld.

        ls_msg-text_msg = |Добрый день, { <ls_email>-bname }. |.

        LOOP AT it_log ASSIGNING FIELD-SYMBOL(<ls_log>).
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = <ls_log>-msgid
              msgnr               = <ls_log>-msgno
              msgv1               = <ls_log>-msgv1
              msgv2               = <ls_log>-msgv2
              msgv3               = <ls_log>-msgv3
              msgv4               = <ls_log>-msgv4
            IMPORTING
              message_text_output = lv_message.

          ls_msg-text_msg = |{ ls_msg-text_msg }{ lv_message }|.
        ENDLOOP.

        ls_msg-text_msg = |{ ls_msg-text_msg } Дата: { sy-datum DATE = USER } Время: { sy-timlo TIME = USER } GUID: { <ls_unit>-key }|.

        send( EXPORTING is_email = <ls_email>
                        it_plc_hld = lt_plc_hld
                        iv_topic_msg = ls_msg-topic_msg
                        iv_text_msg = ls_msg-text_msg
                        it_attachments = it_attachments ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

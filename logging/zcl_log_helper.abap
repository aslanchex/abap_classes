" SBAL_DEMO_01 — простейший вызов журнала приложений
" SBAL_DEMO_02 — различные виды сбора сообщений, детализация, контекст
" SBAL_DEMO_03 — поиск сообщений в памяти
" SBAL_DEMO_04 — разные форматы журнала приложений
" SBAL_DEMO_05 — сохранение и загрузка журналов
" SBAL_DEMO_06 — добавление комплексных данных к журналу
" SBAL_DEMO_07 — добавление исключений
" SBAL_DEMO_08 — поиск исключений в памяти

class ZCL_PI_LOG definition
  public
  final
  create public .

public section.

  methods ADD_BAPIRET2
    importing
      !IV_EXTNUMBER type BALNREXT optional
      !IV_REF_OBJ_KEY type /BOBF/CONF_KEY optional
      !IT_BAPIRET2 type BAPIRET2_TAB .
  methods ADD_BOPF
    importing
      !IV_EXTNUMBER type BALNREXT optional
      !IV_REF_OBJ_KEY type /BOBF/CONF_KEY optional
      !IO_BOPF_MES type ref to /BOBF/IF_FRW_MESSAGE .
  methods ADD_SY
    importing
      !IV_EXTNUMBER type BALNREXT optional
      !IV_REF_OBJ_KEY type /BOBF/CONF_KEY optional .
  methods CONSTRUCTOR .
  methods GET_ALL_MESSAGES
    returning
      value(RT_MESSAGES) type BAL_T_MSG .
  class-methods GET_INSTANCE
    importing
      !IV_OBJECT type BALOBJ_D optional
      !IV_SUBOBJECT type BALSUBOBJ optional
      !IV_STORAGE type INT2 default 90
      !IV_ACT_KEY type /BOBF/CONF_KEY optional
    returning
      value(RO_INSTANCE) type ref to ZCL_PI_LOG .
  methods SAVE
    importing
      !IV_IN_UPDATE_TASK type FLAG default ABAP_TRUE
      !IV_COMMIT type FLAG default ABAP_FALSE
      !IV_CLEAR type FLAG default ABAP_FALSE
      !IV_BOPF_SAVE type FLAG default ABAP_TRUE .
  methods _GET_LOGHANDLE
    importing
      !IV_EXTNUMBER type BALNREXT
      !IV_REF_OBJ_KEY type /BOBF/CONF_KEY
    returning
      value(RV_LOGHANDLE) type BALLOGHNDL .
  methods SHOW .
  methods CLEAR_MESSAGES .
  methods GET_DATA
    exporting
      !ET_LOG_ROOT type ZTTPI_MSG_ROOT_K .
  methods SET_OBJ
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ .
  class-methods FREE .
protected section.
private section.

  data MV_ACT_KEY type /BOBF/CONF_KEY .
  class-data MO_LOG type ref to ZCL_PI_LOG .
  data MV_LOG_NUMBER type ZEPI_LOGNUMBER .
  constants:
    BEGIN OF mc_snum,
               nrnr   TYPE nrnr VALUE '01',
               object TYPE nrobj VALUE 'ZPI_LOG',
             END OF mc_snum .
  data MV_OBJECT type BALOBJ_D .
  data MV_SUBOBJECT type BALSUBOBJ .
  data MT_LOG_ROOT type ZTTPI_MSG_ROOT_K .
  data MV_STORAGE type INT2 .
  data MT_MESSAGES type BAL_T_MSG .

  methods _CONV_BOPF_2_BAPIRET2
    importing
      !IV_SEVERITY type /BOBF/CM_FRW=>TY_MESSAGE_SEVERITY optional
      !IV_CONSISTENCY_MESSAGES type BOOLE_D default ABAP_TRUE
      !IV_ACTION_MESSAGES type BOOLE_D default ABAP_TRUE
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE optional
      !IT_MESSAGE type /BOBF/T_FRW_MESSAGE_K optional
    changing
      !CT_BAPIRET2 type BAPIRET2_TAB .
  methods _INIT_BAL
    importing
      !IV_REF_OBJ_KEY type /BOBF/CONF_KEY
      !IV_EXTNUMBER type BALNREXT
    returning
      value(RV_LOGHANDLE) type BALLOGHNDL .
  methods _SET_LOG_SEVERITY
    importing
      !IV_LOGHANDLE type BALLOGHNDL
      !IV_MSGTY type SYMSGTY .
  methods _SET_OBJ
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_STORAGE type INT2
      !IV_ACT_KEY type /BOBF/CONF_KEY .
ENDCLASS.



CLASS ZCL_PI_LOG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->ADD_BAPIRET2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT(optional)
* | [--->] IV_REF_OBJ_KEY                 TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IT_BAPIRET2                    TYPE        BAPIRET2_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_bapiret2.

    DATA: lv_loghandle TYPE balloghndl,
          ls_msg       TYPE bal_s_msg.

    lv_loghandle = _get_loghandle(
                     iv_extnumber   = iv_extnumber
                     iv_ref_obj_key = iv_ref_obj_key ).

    LOOP AT it_bapiret2 ASSIGNING FIELD-SYMBOL(<ls_bapi>).

      ls_msg-msgty = <ls_bapi>-type.
      ls_msg-msgid = <ls_bapi>-id.
      ls_msg-msgno = <ls_bapi>-number.
      ls_msg-msgv1 = <ls_bapi>-message_v1.
      ls_msg-msgv2 = <ls_bapi>-message_v2.
      ls_msg-msgv3 = <ls_bapi>-message_v3.
      ls_msg-msgv4 = <ls_bapi>-message_v4.



      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_loghandle
          i_s_msg      = ls_msg.

      APPEND ls_msg TO mt_messages.

      _set_log_severity( iv_loghandle = lv_loghandle
                         iv_msgty     = ls_msg-msgty ).

    ENDLOOP.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->ADD_BOPF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT(optional)
* | [--->] IV_REF_OBJ_KEY                 TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IO_BOPF_MES                    TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_bopf.

    DATA: lt_bapiret2  TYPE bapiret2_tab.

    _conv_bopf_2_bapiret2(
      EXPORTING
        io_message              = io_bopf_mes
      CHANGING
        ct_bapiret2             = lt_bapiret2 ).

    add_bapiret2(
        iv_extnumber   = iv_extnumber
        iv_ref_obj_key = iv_ref_obj_key
        it_bapiret2    = lt_bapiret2 ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->ADD_SY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT(optional)
* | [--->] IV_REF_OBJ_KEY                 TYPE        /BOBF/CONF_KEY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_sy.

    DATA: lv_loghandle TYPE balloghndl,
          ls_msg       TYPE bal_s_msg.


    ls_msg-msgty = sy-msgty.
    ls_msg-msgid = sy-msgid.
    ls_msg-msgno = sy-msgno.
    ls_msg-msgv1 = sy-msgv1.
    ls_msg-msgv2 = sy-msgv2.
    ls_msg-msgv3 = sy-msgv3.
    ls_msg-msgv4 = sy-msgv4.
    APPEND ls_msg TO mt_messages.

    lv_loghandle = _get_loghandle(
                     iv_extnumber   = iv_extnumber
                     iv_ref_obj_key = iv_ref_obj_key ).


    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = lv_loghandle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    _set_log_severity( iv_loghandle = lv_loghandle
                       iv_msgty     = ls_msg-msgty ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->CLEAR_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD clear_messages.
    CLEAR mt_log_root.
    CLEAR mt_messages.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = mc_snum-nrnr
        object      = mc_snum-object
      IMPORTING
        number      = mv_log_number
      EXCEPTIONS
        OTHERS      = 8.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->GET_ALL_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_MESSAGES                    TYPE        BAL_T_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_messages.

    rt_messages = mt_messages.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_LOG_ROOT                    TYPE        ZTTPI_MSG_ROOT_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_DATA.

    et_log_root = mt_log_root.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_PI_LOG=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D(optional)
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ(optional)
* | [--->] IV_STORAGE                     TYPE        INT2 (default =90)
* | [--->] IV_ACT_KEY                     TYPE        /BOBF/CONF_KEY(optional)
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_PI_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.

    IF mo_log IS NOT BOUND.
      CREATE OBJECT mo_log.
    ENDIF.

    IF iv_object IS NOT INITIAL AND iv_subobject IS NOT INITIAL.

      mo_log->_set_obj( iv_object    = iv_object
                        iv_subobject = iv_subobject
                        iv_storage   = iv_storage
                        iv_act_key   = iv_act_key ).

    ENDIF.

    ro_instance = mo_log.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IN_UPDATE_TASK              TYPE        FLAG (default =ABAP_TRUE)
* | [--->] IV_COMMIT                      TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_CLEAR                       TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_BOPF_SAVE                   TYPE        FLAG (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save.

    DATA: lt_log_handle TYPE bal_t_logh,
          lt_mod        TYPE /bobf/t_frw_modification,
          lt_exists     TYPE /bobf/t_frw_key_sorted.


    LOOP AT mt_log_root ASSIGNING FIELD-SYMBOL(<ls_root>).
      INSERT <ls_root>-loghandle INTO TABLE lt_log_handle.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_log_handle.


    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = iv_in_update_task
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        OTHERS           = 4.

    IF iv_bopf_save = abap_true.

      CALL FUNCTION 'ZPI_MESSAGES_SAVE'
        IN UPDATE TASK
        EXPORTING
          it_root = mt_log_root.

    ELSEIF mt_log_root IS NOT INITIAL.

      DATA(lo_sm) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_msg_c=>sc_bo_key ).

      SELECT db_key AS key
        FROM ztpi_msg_root
          FOR ALL ENTRIES IN @mt_log_root
            WHERE db_key = @mt_log_root-key
        INTO TABLE @lt_exists.

      IF lt_exists IS NOT INITIAL.

        LOOP AT mt_log_root ASSIGNING <ls_root>.

          READ TABLE lt_exists TRANSPORTING NO FIELDS
               WITH TABLE KEY primary_key COMPONENTS key = <ls_root>-key.
          CHECK sy-subrc = 0.

          DELETE mt_log_root.
        ENDLOOP.

      ENDIF.

      /scmtms/cl_mod_helper=>mod_create_multi(
        EXPORTING
          iv_node        = zif_msg_c=>sc_node-root
          it_data        = mt_log_root
        CHANGING
          ct_mod         = lt_mod ).

      lo_sm->modify( lt_mod ).

      CALL FUNCTION 'ZPI_MESSAGES_SAVE'
        IN UPDATE TASK
        EXPORTING
          iv_only_save = abap_true.

    ENDIF.


    IF iv_clear = abap_true.

      CLEAR mt_log_root.

    ENDIF.

    IF iv_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->SET_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SET_OBJ.
    mv_object = iv_object.
    mv_subobject = iv_subobject.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->SHOW
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show.

    DATA: ls_prof TYPE bal_s_prof,
          lt_hnd  TYPE bal_t_logh.

    LOOP AT mt_log_root ASSIGNING FIELD-SYMBOL(<ls_log>).
      APPEND <ls_log>-loghandle TO lt_hnd.
    ENDLOOP.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_prof.

    IF lt_hnd IS NOT INITIAL.
      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_t_log_handle      = lt_hnd
          i_s_display_profile = ls_prof.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PI_LOG->_CONV_BOPF_2_BAPIRET2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SEVERITY                    TYPE        /BOBF/CM_FRW=>TY_MESSAGE_SEVERITY(optional)
* | [--->] IV_CONSISTENCY_MESSAGES        TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_ACTION_MESSAGES             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE(optional)
* | [--->] IT_MESSAGE                     TYPE        /BOBF/T_FRW_MESSAGE_K(optional)
* | [<-->] CT_BAPIRET2                    TYPE        BAPIRET2_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD _CONV_BOPF_2_BAPIRET2.
  DATA:
    lt_msg      TYPE /bobf/t_frw_message_k,
    ls_t100key  TYPE scx_t100key,
    lo_cm_frw   TYPE REF TO /bobf/cm_frw,
    lv_field    TYPE fieldname,
    ls_bapiret2 TYPE bapiret2.
  FIELD-SYMBOLS:
    <ls_msg>    TYPE /bobf/s_frw_message_k,
    <attr1>     TYPE scx_attrname,
    <attr2>     TYPE scx_attrname,
    <attr3>     TYPE scx_attrname,
    <attr4>     TYPE scx_attrname,
    <attr1_val> TYPE any,
    <attr2_val> TYPE any,
    <attr3_val> TYPE any,
    <attr4_val> TYPE any.

* Anything to do?
  CHECK io_message IS BOUND OR
        it_message IS NOT INITIAL.

* Mapping
  IF io_message IS BOUND.
    io_message->get_messages( EXPORTING iv_severity             = iv_severity
                                        iv_consistency_messages = iv_consistency_messages
                                        iv_action_messages      = iv_action_messages
                              IMPORTING et_message              = lt_msg                ).
  ENDIF.
  APPEND LINES OF it_message TO lt_msg.

  LOOP AT lt_msg ASSIGNING <ls_msg>.
    CLEAR ls_bapiret2.
    UNASSIGN: <attr1_val>,
              <attr2_val>,
              <attr3_val>,
              <attr4_val>.
*   Take over message variable values
    TRY.
        lo_cm_frw ?= <ls_msg>-message.
        ls_t100key = lo_cm_frw->if_t100_message~t100key.
*       Standard CM Class
        ASSIGN lo_cm_frw->('IF_T100_MESSAGE~T100KEY-ATTR1') TO <attr1>.
        IF <attr1> IS ASSIGNED.
          ASSIGN lo_cm_frw->(<attr1>) TO <attr1_val>.
          IF sy-subrc IS NOT INITIAL.
            ASSIGN <attr1> TO <attr1_val>.
          ENDIF.
        ENDIF.
        IF <attr1_val> IS ASSIGNED.
          ls_bapiret2-message_v1 = <attr1_val>.
        ENDIF.
        ASSIGN lo_cm_frw->('IF_T100_MESSAGE~T100KEY-ATTR2') TO <attr2>.
        IF <attr2> IS ASSIGNED.
          ASSIGN lo_cm_frw->(<attr2>) TO <attr2_val>.
          IF sy-subrc IS NOT INITIAL.
            ASSIGN <attr2> TO <attr2_val>.
          ENDIF.
        ENDIF.
        IF <attr2_val> IS ASSIGNED.
          ls_bapiret2-message_v2 = <attr2_val>.
        ENDIF.
        ASSIGN lo_cm_frw->('IF_T100_MESSAGE~T100KEY-ATTR3') TO <attr3>.
        IF <attr3> IS ASSIGNED.
          ASSIGN lo_cm_frw->(<attr3>) TO <attr3_val>.
          IF sy-subrc IS NOT INITIAL.
            ASSIGN <attr3> TO <attr3_val>.
          ENDIF.
        ENDIF.
        IF <attr3_val> IS ASSIGNED.
          ls_bapiret2-message_v3 = <attr3_val>.
        ENDIF.
        ASSIGN lo_cm_frw->('IF_T100_MESSAGE~T100KEY-ATTR4') TO <attr4>.
        IF <attr4> IS ASSIGNED.
          ASSIGN lo_cm_frw->(<attr4>) TO <attr4_val>.
          IF sy-subrc IS NOT INITIAL.
            ASSIGN <attr4> TO <attr4_val>.
          ENDIF.
        ENDIF.
        IF <attr4_val> IS ASSIGNED.
          ls_bapiret2-message_v4 = <attr4_val>.
        ENDIF.
      CATCH cx_sy_move_cast_error.
        ls_t100key = <ls_msg>-message->if_t100_message~t100key.
        ls_bapiret2-message_v1 = ls_t100key-attr1.
        ls_bapiret2-message_v2 = ls_t100key-attr2.
        ls_bapiret2-message_v3 = ls_t100key-attr3.
        ls_bapiret2-message_v4 = ls_t100key-attr4.
    ENDTRY.
*   Take over message body
    ls_bapiret2-type    = <ls_msg>-severity.
    ls_bapiret2-id      = ls_t100key-msgid.
    ls_bapiret2-number  = ls_t100key-msgno.
    ls_bapiret2-message = <ls_msg>-message->if_message~get_text( ).
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = ls_bapiret2-system
      EXCEPTIONS
        own_logical_system_not_defined = 0
        OTHERS                         = 0.
    READ TABLE <ls_msg>-message->ms_origin_location-attributes
      INTO lv_field
      INDEX 1.
    IF sy-subrc EQ 0.
      ls_bapiret2-field = lv_field.
    ELSE.
      CLEAR ls_bapiret2-field.
    ENDIF.
*   Collect message
    APPEND ls_bapiret2 TO ct_bapiret2.
  ENDLOOP.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PI_LOG->_GET_LOGHANDLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT
* | [--->] IV_REF_OBJ_KEY                 TYPE        /BOBF/CONF_KEY
* | [<-()] RV_LOGHANDLE                   TYPE        BALLOGHNDL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_loghandle.

    READ TABLE mt_log_root ASSIGNING FIELD-SYMBOL(<ls_log>)
      WITH TABLE KEY log
      COMPONENTS ref_obj_key = iv_ref_obj_key
                 extnumber   = iv_extnumber
                 prog        = sy-cprog
                 act_key     = mv_act_key.
    IF sy-subrc = 0.

      rv_loghandle = <ls_log>-loghandle.

    ELSE.

      rv_loghandle = _init_bal( iv_ref_obj_key = iv_ref_obj_key
                                iv_extnumber   = iv_extnumber ).
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PI_LOG->_INIT_BAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REF_OBJ_KEY                 TYPE        /BOBF/CONF_KEY
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT
* | [<-()] RV_LOGHANDLE                   TYPE        BALLOGHNDL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _init_bal.

    DATA: ls_log      TYPE bal_s_log,
          ls_log_root TYPE zspi_msg_root_k.


    ls_log-alprog     = sy-repid.
    ls_log-aldate     = sy-datum.
    ls_log-altime     = sy-uzeit.
    ls_log-object     = mv_object.
    ls_log-subobject  = mv_subobject.
    ls_log-aluser     = sy-uname.
    ls_log-aldate_del = sy-datum + mv_storage.
    ls_log-del_before = abap_true.
    ls_log-extnumber  = iv_extnumber.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = rv_loghandle.

    ls_log_root-key         = /bobf/cl_frw_factory=>get_new_key( ).
    ls_log_root-ref_obj_key = iv_ref_obj_key.
    ls_log_root-extnumber   = iv_extnumber.
    ls_log_root-lognumber   = mv_log_number.
    ls_log_root-object      = mv_object.
    ls_log_root-subobject   = mv_subobject.
    ls_log_root-date_del    = ls_log-aldate_del.
    ls_log_root-loghandle   = rv_loghandle.
    ls_log_root-prog        = sy-cprog.
    INSERT ls_log_root INTO TABLE me->mt_log_root.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PI_LOG->_SET_LOG_SEVERITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LOGHANDLE                   TYPE        BALLOGHNDL
* | [--->] IV_MSGTY                       TYPE        SYMSGTY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _set_log_severity.

    READ TABLE mt_log_root ASSIGNING FIELD-SYMBOL(<ls_log>)
         WITH KEY loghandle = iv_loghandle.

    IF <ls_log>-severity IS INITIAL OR
       <ls_log>-severity = 'W' AND iv_msgty = 'E' OR
       <ls_log>-severity = 'S' AND iv_msgty = 'E' OR
       <ls_log>-severity = 'S' AND iv_msgty = 'W'.

      <ls_log>-severity = iv_msgty.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_PI_LOG->_SET_OBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ
* | [--->] IV_STORAGE                     TYPE        INT2
* | [--->] IV_ACT_KEY                     TYPE        /BOBF/CONF_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _set_obj.

    mv_object    = iv_object.
    mv_subobject = iv_subobject.
    mv_act_key   = iv_act_key.
    mv_storage   = iv_storage.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_PI_LOG=>FREE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method FREE.
    FREE mo_log.
  endmethod.
ENDCLASS.

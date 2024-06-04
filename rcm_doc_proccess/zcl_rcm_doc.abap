class ZCL_RCM_DOC definition
  public
  final
  create public .

public section.

  types:
    tt_bapidoccomp TYPE STANDARD TABLE OF bapidoccomp .
  types:
    tt_data_bin TYPE STANDARD TABLE OF bapiconten .
  types:
    tt_bapiret2 TYPE STANDARD TABLE OF bapiret2 .

  constants MC_DOC type STRING value 'DOC' ##NO_TEXT.
  constants MC_DOCX type STRING value 'DOCX' ##NO_TEXT.
  constants MC_XLS type STRING value 'XLS' ##NO_TEXT.
  constants MC_PPT type STRING value 'PPT' ##NO_TEXT.
  constants MC_XML type STRING value 'XML' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods GET_MIMETYPE
    importing
      !IV_EXTENSION type STRING
    returning
      value(RV_MIMETYPE) type MIMETYPES-TYPE .
  methods CREATE_FILE
    importing
      !IV_GUID type SCMG_CASE_GUID
      !IV_SPS_ID type BAPISRMDOC-SPSID
      !IT_COMPONENTS type TT_BAPIDOCCOMP
      !IT_DATA_BIN type TT_DATA_BIN
    exporting
      !ES_RETURN type BAPIRET2
      !EV_OBJID type BAPIGUID
      !EV_DOCCLASS type BAPIDCLASS .
  methods ATTACH_TO_RCM
    importing
      !IM_CASE type ref to CL_SCMG_CASE_VISUALIZATION_WIN
      !IV_OBJID type BAPIGUID
      !IV_SPS_ID type BAPISPSID
      !IV_ANCHOR type STRING
      !IV_DESCRIPTION type STRING
      !IV_DOCCLASS type BAPIDCLASS
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  methods ATTACH_TO_RCM_WITH_GUID
    importing
      !IV_GUID type SCMG_CASE_GUID
      !IV_OBJID type BAPIGUID
      !IV_SPS_ID type BAPISPSID
      !IV_ANCHOR type STRING
      !IV_DESCRIPTION type STRING
      !IV_DOCCLASS type BAPIDCLASS
    exporting
      !ET_RETURN type TT_BAPIRET2 .
  PROTECTED SECTION.
private section.

  methods _GET_FILENAME_BY_FILE
    importing
      !IV_FILE type STRING
    returning
      value(RV_FILENAME) type BAPISRMDOC-DOCID .
ENDCLASS.



CLASS ZCL_RCM_DOC IMPLEMENTATION.


  METHOD attach_to_rcm.
    DATA: lv_doc_guid    TYPE scmg_case_guid,
          lv_doc_id      TYPE string,
          lt_elem_id     TYPE STANDARD TABLE OF bapipropme,
          lt_elem_insert TYPE STANDARD TABLE OF bapidocins,
          lo_record_poid TYPE REF TO cl_srm_sp_record,
          li_element     TYPE REF TO if_srm_sp_record_element,
          li_case_api    TYPE REF TO if_scmg_case_api,
          lo_case_api    TYPE REF TO cl_scmg_case_api,
          ls_return      TYPE bapiret2.

    lv_doc_id = |{ iv_docclass }    { iv_objid }|.

    APPEND VALUE bapipropme( elem_no = 1 name = 'VERSION' value = '0' ) TO lt_elem_id.
    APPEND VALUE bapipropme( elem_no = 1 name = 'VARIANT' value = '0' ) TO lt_elem_id.
    APPEND VALUE bapipropme( elem_no = 1 name = 'DOC_ID' value = lv_doc_id ) TO lt_elem_id.
    APPEND VALUE bapipropme( elem_no = 1 name = '%SPS_ID%' value = iv_sps_id ) TO lt_elem_id.

    APPEND VALUE bapidocins( elem_no = 1
                             anchor = iv_anchor
                             description =  iv_description ) TO lt_elem_insert.
"SELIVERST-VK 05.04.2024 15:22:14 Закомментил, т.к. повторно блокируется SDOK
    CALL METHOD cl_scmg_case_api=>get_case
      EXPORTING
        im_case_guid = im_case->g_case_guid
        im_enqueue   = ''
      RECEIVING
        re_case      = li_case_api
      EXCEPTIONS
        failed       = 1
        invalid_guid = 2.

    lo_case_api ?= li_case_api.

    lo_record_poid ?= lo_case_api->if_scmg_case_api~get_backend_record( ).

    TRY.
        lo_record_poid->if_srm_sp_record~open( for_update   = 'X').
      CATCH cx_srm_sp_record cx_srm_gsp_back.
        APPEND VALUE bapiret2( id = sy-msgid
                               type = sy-msgty
                               number = sy-msgno
                               message_v1 = sy-msgv1
                               message_v2 = sy-msgv2  ) TO et_return.
    ENDTRY.

    CALL FUNCTION 'BAPI_CASE_ADDELEMENTS'
      EXPORTING
        guid                   = im_case->g_case_guid
        new_version            = ''
      TABLES
        element_identification = lt_elem_id
        element_insertion      = lt_elem_insert
        return                 = et_return.

    IF lines( et_return ) <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = ls_return.

      APPEND ls_return TO et_return.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = ls_return.
      TRY.
          im_case->field_check_and_get( ).
        CATCH cx_scmg_attr_display
              cx_srm_framework.
          APPEND VALUE bapiret2( id = sy-msgid
                                 number = sy-msgno
                                 type = sy-msgty
                                 message_v1 = sy-msgv1
                                 message_v2 = sy-msgv2
                                 message_v3 = sy-msgv3
                                 message_v4 = sy-msgv4 ) TO et_return.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD create_file.
    DATA: li_case_api    TYPE REF TO if_scmg_case_api,
          lo_case_api    TYPE REF TO cl_scmg_case_api,
          lv_rms_id      TYPE stringval,
          lv_description TYPE bapidescr,
          ls_return      TYPE bapiret2.


    CALL METHOD cl_scmg_case_api=>get_case
      EXPORTING
        im_case_guid = iv_guid
        im_enqueue   = ''
      RECEIVING
        re_case      = li_case_api
      EXCEPTIONS
        failed       = 1
        invalid_guid = 2.

    CASE sy-subrc.
      WHEN 1.
        CALL FUNCTION 'UDM_GEN_PREPARE_RETURN'
          EXPORTING
            i_number  = '009'
          IMPORTING
            es_return = es_return.
        EXIT.
      WHEN 2.
        CALL FUNCTION 'UDM_GEN_PREPARE_RETURN'
          EXPORTING
            i_number     = '038'
            i_message_v1 = iv_guid
          IMPORTING
            es_return    = es_return.
        EXIT.
    ENDCASE.

    lo_case_api ?= li_case_api.

    TRY.
        lv_rms_id = lo_case_api->get_rms_id( ).
      CATCH cx_srm_initialization.
      CATCH cx_srm_poid.
    ENDTRY.

    READ TABLE it_components INDEX 1 INTO DATA(ls_components).

    CALL FUNCTION 'SRM_DOCUMENT_CREATE'
      EXPORTING
        rms_id            = CONV bapirmsid( lv_rms_id )
        sps_id            = iv_sps_id
        documentid        = _get_filename_by_file( CONV #( ls_components-comp_id ) )
        description       = lv_description
        do_commit         = ' '
*       DOC_CONTEXT       =
      IMPORTING
        return            = es_return
        objectid          = ev_objid
        documentclass     = ev_docclass
      EXCEPTIONS
        internal_error    = 1
        parameter_error   = 2
        doc_id_not_unique = 3
        not_authorized    = 4
        customizing_error = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SRM_DOCUMENT_CHECKIN_VIA_TAB'
      EXPORTING
        objectid        = ev_objid
        documentclass   = ev_docclass
        as_new_version  = ''
        do_commit       = space
      IMPORTING
        return          = es_return
      TABLES
        components      = it_components
        bin_content     = it_data_bin
      EXCEPTIONS
        internal_error  = 1
        parameter_error = 2
        not_authorized  = 3
        doc_not_found   = 4
        yet_locked      = 5
        OTHERS          = 6.
    IF sy-subrc <> 0.
      CALL FUNCTION 'UDM_GEN_PREPARE_RETURN'
        EXPORTING
          i_number     = '166'
          i_message_v1 = iv_guid
        IMPORTING
          es_return    = es_return.
      EXIT.

    ENDIF.

  ENDMETHOD.


  METHOD get_mimetype.
    CLEAR: rv_mimetype.
    CALL FUNCTION 'SDOK_MIMETYPE_GET'
      EXPORTING
        extension = CONV char10( iv_extension )
      IMPORTING
        mimetype  = rv_mimetype.
  ENDMETHOD.


  METHOD ATTACH_TO_RCM_WITH_GUID.
    DATA: lv_doc_guid    TYPE scmg_case_guid,
          lv_doc_id      TYPE string,
          lt_elem_id     TYPE STANDARD TABLE OF bapipropme,
          lt_elem_insert TYPE STANDARD TABLE OF bapidocins,
          lo_record_poid TYPE REF TO cl_srm_sp_record,
          li_element     TYPE REF TO if_srm_sp_record_element,
          li_case_api    TYPE REF TO if_scmg_case_api,
          lo_case_api    TYPE REF TO cl_scmg_case_api,
          ls_return      TYPE bapiret2.

    lv_doc_id = |{ iv_docclass }    { iv_objid }|.

    APPEND VALUE bapipropme( elem_no = 1 name = 'VERSION' value = '0' ) TO lt_elem_id.
    APPEND VALUE bapipropme( elem_no = 1 name = 'VARIANT' value = '0' ) TO lt_elem_id.
    APPEND VALUE bapipropme( elem_no = 1 name = 'DOC_ID' value = lv_doc_id ) TO lt_elem_id.
    APPEND VALUE bapipropme( elem_no = 1 name = '%SPS_ID%' value = iv_sps_id ) TO lt_elem_id.

    APPEND VALUE bapidocins( elem_no = 1
                             anchor = iv_anchor
                             description =  iv_description ) TO lt_elem_insert.
"SELIVERST-VK 05.04.2024 15:22:14 Закомментил, т.к. повторно блокируется SDOK
    CALL METHOD cl_scmg_case_api=>get_case
      EXPORTING
        im_case_guid = iv_guid
        im_enqueue   = ''
      RECEIVING
        re_case      = li_case_api
      EXCEPTIONS
        failed       = 1
        invalid_guid = 2.

    lo_case_api ?= li_case_api.

    lo_record_poid ?= lo_case_api->if_scmg_case_api~get_backend_record( ).

    TRY.
        lo_record_poid->if_srm_sp_record~open( for_update   = 'X').
      CATCH cx_srm_sp_record cx_srm_gsp_back.
        APPEND VALUE bapiret2( id = sy-msgid
                               type = sy-msgty
                               number = sy-msgno
                               message_v1 = sy-msgv1
                               message_v2 = sy-msgv2  ) TO et_return.
    ENDTRY.

    CALL FUNCTION 'BAPI_CASE_ADDELEMENTS'
      EXPORTING
        guid                   = iv_guid
        new_version            = ''
      TABLES
        element_identification = lt_elem_id
        element_insertion      = lt_elem_insert
        return                 = et_return.

    IF lines( et_return ) <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = ls_return.

      APPEND ls_return TO et_return.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = ls_return.
      TRY.
*          im_case->field_check_and_get( ).
        CATCH cx_scmg_attr_display
              cx_srm_framework.
          APPEND VALUE bapiret2( id = sy-msgid
                                 number = sy-msgno
                                 type = sy-msgty
                                 message_v1 = sy-msgv1
                                 message_v2 = sy-msgv2
                                 message_v3 = sy-msgv3
                                 message_v4 = sy-msgv4 ) TO et_return.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  method _GET_FILENAME_BY_FILE.
    DATA: lv_string TYPE string.

    lv_string = iv_file.
    SHIFT lv_string LEFT DELETING LEADING '.'.
    SPLIT lv_string AT '.' INTO TABLE DATA(lt_tab).

    IF lines( lt_tab ) = 1.
      rv_filename = lv_string.
      RETURN.
    ENDIF.


    DELETE lt_tab INDEX lines( lt_tab ).
    LOOP AT lt_tab INTO DATA(ls_tab).
      IF sy-tabix = 1.
        rv_filename = ls_tab.
      ELSE.
        CONCATENATE rv_filename ls_tab INTO rv_filename SEPARATED BY '.'.
      ENDIF.
    ENDLOOP.
  endmethod.
ENDCLASS.

CLASS zcl_int003_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_locationid,
        matnr      TYPE mseg-matnr,
        werks      TYPE mseg-werks,
        lgort      TYPE mseg-lgort,
        locationid TYPE zde_zloc_placeid,
      END OF ts_locationid .
    TYPES:
      tt_locationid TYPE STANDARD TABLE OF ts_locationid .
    TYPES:
      BEGIN OF ty_pt0113,
        item      TYPE ztt_int0003_pt0113_item_k,
        primedocs TYPE ztt_int0003_pt0111_primedocs_k.
        INCLUDE TYPE zsint0003_pt0113_d.
    TYPES: END OF ty_pt0113 .
    TYPES:
      BEGIN OF ty_pt0113_items,
        ebeln   TYPE ebeln,
        ebelp   TYPE ebelp,
        objty   type ZE_TYPGET.
        include TYPE ZSINT0003_PT0113_ITEM_K.
    TYPES: END OF ty_pt0113_items .
    TYPES:
      tt_pt0113_items TYPE STANDARD TABLE OF ty_pt0113_items.

    TYPES:
      BEGIN OF ty_serge,
        serge TYPE serge,
      END OF ty_serge .
    TYPES:
      tt_serge TYPE STANDARD TABLE OF ty_serge WITH DEFAULT KEY .
    TYPES material_doc_num TYPE mblnr .
    TYPES:
      BEGIN OF ty_re_send,
        user_name        TYPE syst_uname,
        cft_agreement_id TYPE zeext_key,
        rcm_agreement_id TYPE ze_abs_num,
        error_text       TYPE ze_error_descr_255,
      END OF ty_re_send .
    TYPES:
      BEGIN OF ty_pt0111_113_send,
        view_msg  TYPE ze_view_msg,
        user_name TYPE syst_uname,
        doc_id    TYPE ze_material_doc_num,
        doc_year  TYPE gjahr,
        errors    TYPE ztt_int0003_pt0111_resp,
      END OF ty_pt0111_113_send .
    TYPES:
      BEGIN OF ty_send,
        material_doc_num     TYPE mblnr,
        material_doc_year    TYPE mjahr,
        user_name            TYPE syst_uname,
        message_type         TYPE ze_view_msg,
        os                   TYPE anln1,
        reject_description   TYPE ze_reject_description,
        reject_extended_info TYPE ze_reject_extended_info,
      END OF ty_send .
    TYPES material_doc_year TYPE mjahr .
    TYPES:
      BEGIN OF ty_mwskz_i,
        belnr TYPE re_belnr,
        gjahr TYPE gjahr,
        mwskz TYPE mwskz,
        netwr TYPE bstwr,
      END OF ty_mwskz_i .
    TYPES:
      tt_mwskz_i TYPE SORTED TABLE OF ty_mwskz_i WITH UNIQUE KEY  belnr gjahr mwskz .
    TYPES:
      tt_respose_data TYPE STANDARD TABLE OF zt003_mtldcrerez .
    TYPES message_type TYPE ze_view_msg .
    TYPES:
      tr_anbwa           TYPE RANGE OF anbwa .
    TYPES:
      tr_gkont           TYPE RANGE OF hkont .

    CLASS-DATA mv_dummy TYPE flag .
    CLASS-DATA mt_rbtx TYPE mrm_t_rbtx .
    CLASS-DATA mt_drseg TYPE mr_drseg .
    CLASS-DATA mv_message_maxindex TYPE i .
    CONSTANTS mc_pt0111_repair TYPE char40 VALUE 'PT0111_REPAIR' ##NO_TEXT.
    CONSTANTS mc_pt0111_nma TYPE char40 VALUE 'PT0111_NMA' ##NO_TEXT.
    CONSTANTS mc_pt0111 TYPE char40 VALUE 'PT0111' ##NO_TEXT.
    CONSTANTS mc_pt0113 TYPE char40 VALUE 'PT0113' ##NO_TEXT.
    CLASS-DATA ms_rbkpv TYPE rbkp_v .

    CLASS-METHODS get_tax
      IMPORTING
        !iv_mwskz       TYPE mwskz
        !iv_wrbtr       TYPE wrbtr
      RETURNING
        VALUE(rs_mwdat) TYPE rtax1u15 .
    CLASS-METHODS get_znp
      IMPORTING
        !io_log  TYPE REF TO zcl_pi_log OPTIONAL
      CHANGING
        !cs_data TYPE zsint0003_pt0112_k .
    CLASS-METHODS release_znp
      IMPORTING
        !iv_rel_code TYPE frgco
        !io_log      TYPE REF TO zcl_pi_log
      CHANGING
        !cs_data     TYPE zsint0003_pt0112_k .
    CLASS-METHODS check_znp
      IMPORTING
        !io_log  TYPE REF TO zcl_pi_log
      CHANGING
        !cs_data TYPE zsint0003_pt0112_k .
    CLASS-METHODS get_sf
      IMPORTING
        !io_log  TYPE REF TO zcl_pi_log
      CHANGING
        !cs_data TYPE zsint0003_pt0112_k .
    CLASS-METHODS change_sf
      IMPORTING
        !io_log  TYPE REF TO zcl_pi_log
      CHANGING
        !cs_data TYPE zsint0003_pt0112_k .
    CLASS-METHODS release_sf
      IMPORTING
        !io_log  TYPE REF TO zcl_pi_log
      CHANGING
        !cs_data TYPE zsint0003_pt0112_k .
    CLASS-METHODS api_220_mat_doc_resp
      CHANGING
        !ct_data TYPE tt_respose_data .
    CLASS-METHODS get_migo_770
      IMPORTING
        !iv_mblnr      TYPE mblnr OPTIONAL
        !iv_mjahr      TYPE mjahr OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE zst_0003_send_material_doc .
    CLASS-METHODS get_mir7_770
      IMPORTING
        !iv_belnr      TYPE mblnr OPTIONAL
        !iv_gjahr      TYPE mjahr OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE zst_0003_send_material_doc .
    CLASS-METHODS get_mir7_770_nma
      IMPORTING
        !iv_belnr      TYPE mblnr OPTIONAL
        !iv_gjahr      TYPE mjahr OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE zst_0003_send_material_doc .
    CLASS-METHODS send_770
      IMPORTING
        !is_data TYPE zst_0003_send_material_doc .
    CLASS-METHODS get_mir7_usl_960
      IMPORTING
        !iv_belnr            TYPE re_belnr
        !iv_gjahr            TYPE gjahr
      EXPORTING
        !et_return           TYPE bapiret2_t
      RETURNING
        VALUE(es_infopt0113) TYPE ty_pt0113 .
    CLASS-METHODS send_usl_960
      IMPORTING
        !is_data TYPE ty_pt0113 .
    CLASS-METHODS get_mir7_230
      IMPORTING
        !iv_ordernum         TYPE ebeln
        !iv_cftagreementid   TYPE ze_abs_num
      RETURNING
        VALUE(es_infpot0136) TYPE zsint0003_pt0136_plcal .
    CLASS-METHODS send_mir7_230
      IMPORTING
        !is_data TYPE zsint0003_pt0136_plcal .
    CLASS-METHODS send_notify_pt0111_pt0113
      IMPORTING
        !is_send          TYPE ty_pt0111_113_send
      RETURNING
        VALUE(rt_message) TYPE bapiret2_t .
    CLASS-METHODS send_notify_re
      IMPORTING
        !is_send          TYPE ty_re_send
      RETURNING
        VALUE(rt_message) TYPE bapiret2_t .
    CLASS-METHODS send_reject_notify
      IMPORTING
        !is_send    TYPE ty_send
      EXPORTING
        !et_message TYPE bapiret2_t .
    CLASS-METHODS get_mir7_770_repair
      IMPORTING
        !iv_belnr      TYPE mblnr OPTIONAL
        !iv_gjahr      TYPE mjahr OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE zst_0003_send_material_doc .
    CLASS-METHODS send_reject_notify_158
      IMPORTING
        !is_send    TYPE ty_send
      EXPORTING
        !et_message TYPE bapiret2_t .
    CLASS-METHODS send_reject_notify_112
      IMPORTING
        !is_send    TYPE ty_send
      EXPORTING
        !et_message TYPE bapiret2_t .
    CLASS-METHODS send_messages_from_log
      IMPORTING
        !is_send    TYPE ty_send
        !io_log     TYPE REF TO zcl_pi_log
      EXPORTING
        !et_message TYPE bapiret2_t .
    CLASS-METHODS send_reject_notify_114
      IMPORTING
        !is_send    TYPE ty_send
      EXPORTING
        !et_message TYPE bapiret2_t .
    CLASS-METHODS _get_locationid
      CHANGING
        !ct_locationid TYPE tt_locationid .
    CLASS-METHODS send_warning_messages_from_log
      IMPORTING
        !is_send    TYPE ty_send
        !io_log     TYPE REF TO zcl_pi_log
      EXPORTING
        !et_message TYPE bapiret2_t .
    CLASS-METHODS send_edo_pt1516
      IMPORTING
        !it_data TYPE ztt_rcmint0006_pt1516_k .
    CLASS-METHODS check_pt0113
      IMPORTING
        !is_rbkp         TYPE rbkp
        !it_rseg         TYPE mrm_tab_mrmrseg
        !it_rbco         TYPE mrm_tab_mrmrbco
      RETURNING
        VALUE(rv_result) TYPE abap_bool .
    CLASS-METHODS _add_sum_diff_pt0113
      IMPORTING
        !iv_belnr TYPE mblnr
        !iv_gjahr TYPE mjahr
      CHANGING
        !cs_data  TYPE ty_pt0113 .
    CLASS-METHODS set_zgosnomer
      IMPORTING
        !is_mseg       TYPE mseg
        !is_vm07m      TYPE vm07m
        !is_dm07m      TYPE dm07m
        !is_mkpf       TYPE mkpf
        !it_characters TYPE ztt_characters
      CHANGING
        !ct_values     TYPE ztt_values .
protected section.
private section.

  class-methods _GET_PARENT_MATDOC
    importing
      !IS_MSEG type MSEG
      !IV_BKTXT type BKTXT
    returning
      value(RV_PARMATDOC) type AWREF .
  class-methods _MATERIALDOCCODE_DONACHENKA
    importing
      !IV_ANLN1 type ANLN1
      !IV_ANLN2 type ANLN2
      !IV_BUKRS type BUKRS
      !IR_ANBWA type TR_ANBWA
      !IR_GKONT type TR_GKONT
    exporting
      !EV_ANLHTXT type ANLHTXT
    returning
      value(RV_OK) type FLAG .
  class-methods _ADD_SUM_DIFF
    importing
      !IV_BELNR type MBLNR
      !IV_GJAHR type MJAHR
    changing
      !CS_DATA type ZST_0003_SEND_MATERIAL_DOC .
  class-methods _GET_PRIMEDOCS
    importing
      !IV_BELNR type RE_BELNR
      !IV_GJAHR type GJAHR
    returning
      value(RT_PRIMEDOCS) type ZTT_INT0003_PT0111_PRIMEDOCS_K .
ENDCLASS.



CLASS ZCL_INT003_HELPER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>API_220_MAT_DOC_RESP
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_DATA                        TYPE        TT_RESPOSE_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD api_220_mat_doc_resp ##NEEDED.


*    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<ls_data>).
*
*
*      SELECT SINGLE rbkp~belnr
*        FROM rbkp
*          JOIN rseg
*            ON rbkp~belnr = rseg~belnr
*        WHERE rbkp~belnr = @<ls_data>-doc_system_num
*          AND rseg~tbtkz = @abap_true
*      INTO @DATA(lv_sf) ##WARN_OK.
*
*      IF lv_sf IS INITIAL.
*
*
*        get_znp(
*          EXPORTING
*            iv_dm  = <ls_data>-doc_system_num
*          IMPORTING
*            ev_znp = DATA(lv_znp) )."Передавать в интеграционную табличку
*
*        <ls_data>-znp = lv_znp.
*
*
*        IF lv_znp IS INITIAL.
*
*          <ls_data>-reject_description = 'Не наден ЗНП'.
*          CONTINUE.
*
*        ENDIF.
*
*
*        check_znp(
*          EXPORTING
*            iv_znp         = lv_znp
*          IMPORTING
*            ev_error_desce = DATA(lv_descr_check_znp) ).
*
*        IF lv_descr_check_znp IS NOT INITIAL.
*          <ls_data>-reject_description = lv_descr_check_znp.
*          CONTINUE.
*        ENDIF.
*
*
*
*        release_znp(
*          EXPORTING
*            iv_znp         = lv_znp
*            iv_rel_code    = 'Z3'
*          IMPORTING
*            ev_error_descr = DATA(lv_descr_release_z3) ).
*
*        IF lv_descr_release_z3 IS NOT INITIAL.
*          <ls_data>-reject_description = |Ошибка при проставлении статуса Z3: { lv_descr_release_z3 }|.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          CONTINUE.
*        ENDIF.
*
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*
*
*
*
*        IF <ls_data>-entry_status = '1'.
*
*          release_znp(
*            EXPORTING
*              iv_znp         = lv_znp
*              iv_rel_code    = 'Z4'
*            IMPORTING
*              ev_error_descr = DATA(lv_descr_release_z4) ).
*
*          IF lv_descr_release_z4 IS NOT INITIAL.
*            <ls_data>-reject_description = |Ошибка при проставлении статуса Z4: { lv_descr_release_z4 }|.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*            CONTINUE.
*          ENDIF.
*
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = 'X'.
*
*        ENDIF.
*
*      ENDIF.
*
*      IF <ls_data>-entry_status = '1' OR lv_sf IS NOT INITIAL.
*
*        IF lv_sf IS INITIAL.
*
*          get_sf(
*            EXPORTING
*              iv_znp = lv_znp
*            IMPORTING
*              ev_sf = lv_sf ). "Передавать в интеграционную табличку
*
*
*          IF lv_sf IS INITIAL.
*
*            <ls_data>-reject_description = 'Не наден СФ'.
*            CONTINUE.
*
*          ENDIF.
*
*        ENDIF.
*
*        <ls_data>-sf = lv_sf.
*
*        change_sf(
*          EXPORTING
*            iv_sf          = lv_sf
*            iv_date        = <ls_data>-entry_date
*          IMPORTING
*            ev_error_descr = DATA(lv_descr_change_sf)  ).
*
*        IF lv_descr_change_sf IS NOT INITIAL.
*          <ls_data>-reject_description = |Ошибка при изменеии СФ { lv_descr_change_sf }|.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          CONTINUE.
*        ENDIF.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        release_sf(
*          EXPORTING
*            iv_sf          = lv_sf
*          IMPORTING
*            ev_error_descr = DATA(lv_descr_release_sf) ).
*
*        IF lv_descr_release_sf IS NOT INITIAL.
*          <ls_data>-reject_description = |Ошибка при проводке СФ { lv_descr_release_sf }|.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          CONTINUE.
*        ENDIF.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*      ENDIF.
*
*
*
*
*
*    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>CHANGE_SF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<-->] CS_DATA                        TYPE        ZSINT0003_PT0112_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD change_sf.

    DATA: lt_return TYPE TABLE OF bapiret2.

    "ATC 04.07.2022
    SELECT SINGLE belnr, gjahr, blart "#EC WARNOK
      FROM rbkp
      WHERE belnr = @cs_data-sf
      INTO @DATA(ls_rbkp).

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CHANGE'
      EXPORTING
        invoicedocnumber   = ls_rbkp-belnr
        fiscalyear         = ls_rbkp-gjahr
        invoice_doc_status = 'A'
        headerdata_change  = VALUE bapi_incinv_chng_header( doc_type = ls_rbkp-blart pstng_date = cs_data-entry_date )
        headerdata_changex = VALUE bapi_incinv_chng_headerx( doc_type = abap_true pstng_date = abap_true )
      TABLES
        return             = lt_return.


    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type   = 'E'
                                                                      id     = 'M8'
                                                                      number = '255'.

    IF sy-subrc = 0.
      DELETE lt_return INDEX sy-tabix.
    ENDIF.

    IF line_exists( lt_return[ type = 'E' ] ) OR
       line_exists( lt_return[ type = 'A' ] ) OR
       line_exists( lt_return[ type = 'X' ] ).

      io_log->add_bapiret2( iv_extnumber   = CONV #( cs_data-material_doc_num )
                            iv_ref_obj_key = cs_data-key
                            it_bapiret2    = lt_return ).

      MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      cs_data-process_status = zif_ifc_const=>mc_process_status-error.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>CHECK_ZNP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<-->] CS_DATA                        TYPE        ZSINT0003_PT0112_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_znp ##NEEDED.

    "Пока без проверки

*    SELECT SINGLE @abap_true
*      FROM ekko
*        INNER JOIN t16fk
*          ON ekko~frggr = t16fk~frggr
*         AND ekko~frgsx = t16fk~frgsx
*      WHERE ekko~ebeln  = @cs_data-znp
**        AND ekko~bsart  = @lv_bsart_tvarvc
*        AND ekko~frgke  = 'F'
*        AND t16fk~frgkx = 'F'
*      INTO @DATA(lv_good).

*    IF sy-subrc <> 0.
*
*      cs_data-process_status = zif_ifc_const=>mc_process_status-error.
*
*      MESSAGE e008(zmmint0003_msg) WITH cs_data-znp INTO mv_dummy.
*      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
*                      iv_ref_obj_key = cs_data-key ).
*      MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
*      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
*                      iv_ref_obj_key = cs_data-key ).
*    ENDIF.

  ENDMETHOD ##NEEDED.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_MIGO_770
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MBLNR                       TYPE        MBLNR(optional)
* | [--->] IV_MJAHR                       TYPE        MJAHR(optional)
* | [<-()] RS_DATA                        TYPE        ZST_0003_SEND_MATERIAL_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_migo_770.


    CONSTANTS: lc_waers      TYPE waers VALUE 'RUB',
               lc_taser      TYPE taser VALUE 'SER07',
               lc_bwart_241  TYPE bwart VALUE '241',
               lc_bwart_z41  TYPE bwart VALUE 'Z41',
               lc_bwart_221  TYPE bwart VALUE '221',
               lc_bwart_261  TYPE bwart VALUE '261',
               lc_bwart_101  TYPE bwart VALUE '101',
               lc_bwart_641  TYPE bwart VALUE '641',
               lc_bwart_301  TYPE bwart VALUE '301',
               lc_bwart_311  TYPE bwart VALUE '311',
               lc_bwart_601  TYPE bwart VALUE '601',
               lc_bwart_201  TYPE bwart VALUE '201',
               lc_bwart_701  TYPE bwart VALUE '701',
               lc_bwart_501  TYPE bwart VALUE '501', " Eremetov 11.11.2022 16:01:19
               lc_grund_0000 TYPE mb_grbew VALUE '0000',
               lc_grund_0001 TYPE mb_grbew VALUE '0001',
               lc_grund_0002 TYPE mb_grbew VALUE '0002',
               lc_grund_0003 TYPE mb_grbew VALUE '0003',
               lc_grund_0004 TYPE mb_grbew VALUE '0004'.

    DATA: lt_mat_move               TYPE ztt_0003_send_material_doc,
          lv_atinn_matsh            TYPE atinn,
          lv_wrbtr                  TYPE bseg-wrbtr,
          lt_rng_rbstat             TYPE RANGE OF rbstat,
          lv_txt_name               TYPE thead-tdname,
          lt_rng_tmcgroup_bwart     TYPE RANGE OF bwart,
          lt_rng_tmcgroup_grund     TYPE RANGE OF mb_grbew,
          lt_rng_bwart_change_batch TYPE RANGE OF bwart,
          lt_rng_anln1              TYPE RANGE OF anln1,
          lt_rng_anln2              TYPE RANGE OF anln2,
          lt_rng_bukrs              TYPE RANGE OF bukrs,
          lv_objek                  TYPE cuobn,
          lt_mwdat                  TYPE cte_t_mwdat,
          lt_long_text              TYPE TABLE OF tline,
          lv_anla1                  TYPE anln1,
          lv_anla2                  TYPE anln2,
          lt_rng_inob_obj           TYPE RANGE OF inob-objek,
          lt_rng_code_not_send      TYPE RANGE OF zint_mm_viddvig-code_doc_abc,
          lt_konv                   TYPE TABLE OF konv,
*          lt_serge                  TYPE tt_serge,
          ls_sernos_key             TYPE rserob,
          lt_sernos                 TYPE TABLE OF rserob,
          lv_molrec_trans           TYPE ze_resp_contract.

***    DATA:
***      lt_locationid TYPE tt_locationid,
***      lv_loc_active TYPE abap_bool.

    DATA(lv_mblnr) = iv_mblnr.
    DATA(lv_mjahr) = iv_mjahr.

    "---------------------------------------------------------------
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_RBSTAT'                  IMPORTING er_range = lt_rng_rbstat ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_TMCGROUP_BWART'          IMPORTING er_range = lt_rng_tmcgroup_bwart ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_TMCGROUP_GRUND'          IMPORTING er_range = lt_rng_tmcgroup_grund ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_NOT_SEND'           IMPORTING er_range = lt_rng_code_not_send ).
    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_ATINN_MATSH'             IMPORTING ev_value = lv_atinn_matsh  ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZTMMINT0003_BWART_CHANGE_BATCH' IMPORTING er_range = lt_rng_bwart_change_batch ).
***    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_LOCATION'                IMPORTING ev_value = lv_loc_active ).
    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZSMMINT0003_MOL_RECEIVER'        IMPORTING ev_value = lv_molrec_trans ).
    "---------------------------------------------------------------



    "Не отправляем 101 вдв, вместо него отправляем 641
    "---------------------------------------------------------------
    SELECT e641~belnr, e641~gjahr
      FROM ekbe AS e101
        JOIN ekbe AS e641
          ON e101~ebeln = e641~ebeln
      WHERE e101~bwart = @lc_bwart_101
        AND e641~bwart = @lc_bwart_641
        AND e101~belnr = @lv_mblnr
      INTO TABLE @DATA(lt_641).

    IF sy-subrc = 0.

      SELECT mblnr, mjahr, zeile
        FROM @lt_641 AS lt
        INNER JOIN mseg ON mseg~mblnr = lt~belnr
                        AND mseg~mjahr = lt~gjahr
        INTO TABLE @DATA(lt_641_mseg).

      SELECT smbln, sjahr, smblp
             FROM @lt_641_mseg AS lt
        INNER JOIN mseg ON smbln = lt~mblnr
                        AND sjahr  = lt~mjahr
        INTO TABLE @DATA(lt_641_storn).

      DELETE ADJACENT DUPLICATES FROM lt_641_storn.
      LOOP AT lt_641_storn ASSIGNING FIELD-SYMBOL(<ls_641_storn>).
        READ TABLE lt_641_mseg WITH KEY mblnr = <ls_641_storn>-smbln
                                        mjahr = <ls_641_storn>-sjahr
                                        zeile  = <ls_641_storn>-smblp ASSIGNING FIELD-SYMBOL(<ls_641_mseg>).
        IF sy-subrc = 0.
          DELETE lt_641_mseg WHERE mblnr = <ls_641_mseg>-mblnr
                            AND  mjahr = <ls_641_mseg>-mjahr
                            AND  zeile = <ls_641_mseg>-zeile .
        ENDIF.
      ENDLOOP.

      READ TABLE lt_641_mseg INDEX 1 INTO DATA(ls_641).
      IF sy-subrc = 0.
        DATA(lv_parent_mat_doc) = lv_mblnr.
        lv_mblnr = ls_641-mblnr."belnr.
        lv_mjahr = ls_641-mjahr."gjahr.
      ELSE.
        RETURN.
      ENDIF.

    ENDIF.
    "---------------------------------------------------------------


    "---------------------------------------------------------------
    SELECT SINGLE bktxt, usnam,
                  xblnr, budat,
                  bldat, zzext_key,
                  mjahr, mblnr
      FROM mkpf
      INTO @DATA(ls_mkpf)
      WHERE mblnr = @lv_mblnr
        AND mjahr = @lv_mjahr.

    SELECT mblnr, mjahr, zeile,
           ebeln, anln1, anln2,
           sgtxt, bukrs, xauto,
           lgort, werks, lifnr,
           bwart, grund, parent_id,
           line_id, matnr, charg,
           dmbtr, ebelp, bprme,
           erfme, menge, umbar,
           bwtar, kzbew, kzzug,
           kunnr, vbeln_im, vbelp_im,
           aufnr, umcha, salk3, lbkum,
           umwrk, umlgo, kostl
      FROM mseg
      INTO TABLE @DATA(lt_mseg)
      WHERE mblnr = @lv_mblnr
        AND mjahr = @lv_mjahr
        AND smbln EQ @space
*      ORDER BY mblnr, mjahr, parent_id.
      ORDER BY zeile.



    IF lt_mseg IS NOT INITIAL.

      SELECT smbln, sjahr, smblp                   "#EC CI_NO_TRANSFORM
           FROM mseg
        INTO TABLE @DATA(lt_mseg_storn)
        FOR ALL ENTRIES IN @lt_mseg
        WHERE smbln = @lt_mseg-mblnr
          AND sjahr  = @lt_mseg-mjahr.

    ENDIF.

    IF sy-subrc EQ 0.

      LOOP AT lt_mseg_storn ASSIGNING FIELD-SYMBOL(<ls_mseg_storn>).
        READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<ls_mseg>)
                                         WITH KEY mblnr = <ls_mseg_storn>-smbln
                                                  mjahr = <ls_mseg_storn>-sjahr
                                                  zeile  = <ls_mseg_storn>-smblp.
        IF sy-subrc EQ 0.
          "Значит док. стор.
          DELETE lt_mseg WHERE mblnr = <ls_mseg>-mblnr
                          AND  mjahr = <ls_mseg>-mjahr
                          AND  zeile = <ls_mseg>-zeile .
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lt_mseg IS INITIAL.
      RETURN.
    ENDIF.

    "{ baryutin-sa 16.11.2023 ЗНИ 317
    SELECT lt~mblnr, lt~werks, lt~umwrk, lt~umlgo, lt~mjahr, lt~bwart, lt~grund ,zf~zzmolreceiver, mkpf~vgart, ztmm_molreceiver~pernr
      FROM @lt_mseg AS lt
      LEFT JOIN ztmm_migo_zf AS zf ON lt~mblnr = zf~mblnr
                                    AND lt~mjahr = zf~mjahr
                                    AND lt~zeile = zf~zeile
      LEFT JOIN mkpf ON mkpf~mblnr = lt~mblnr
                      AND mkpf~mjahr = lt~mjahr
      LEFT JOIN ztmm_molreceiver ON lt~werks = ztmm_molreceiver~werks
      WHERE lt~zeile = 1
      INTO TABLE @DATA(lt_migo_zf).
    "} baryutin-sa 16.11.2023 ЗНИ 317

    "---------------------------------------------------------------


    "---------------------------------------------------------------
    SELECT vbap~vbeln AS vbap_vbeln,
           vbap~posnr AS vbap_posnr,
           vbap~knumv_ana,
           vbap~kzwi4,
           vbap~kzwi5,
           lips~vbeln,
           lips~posnr,
           vbak~zuonr,
           zdgv~abs_num
      FROM vbap
        JOIN lips
          ON vbap~vbeln = lips~vgbel AND
             vbap~posnr = lips~vgpos
        JOIN vbak
            ON lips~vgbel = vbak~vbeln
          LEFT JOIN scmg_t_case_attr AS case
            ON vbak~zuonr = case~ext_key
          LEFT JOIN zrcmt_dgv_attr   AS zdgv
            ON case~case_guid = zdgv~case_guid
        FOR ALL ENTRIES IN @lt_mseg
          WHERE lips~vbeln = @lt_mseg-vbeln_im
            AND lips~posnr = @lt_mseg-vbelp_im
        INTO TABLE @DATA(lt_vbap).

    SORT lt_vbap BY vbeln posnr.
    "---------------------------------------------------------------


    "---------------------------------------------------------------
    SELECT viau~aufnr, anla~invnr,
           viau~anlnr, anlz~pernr,
           anla~gdlgrp, anla~anlkl
      FROM viaufks AS viau
        JOIN anla
          ON viau~anlnr = anla~anln1 AND
             viau~anlun = anla~anln2
        LEFT JOIN anlz
          ON anla~anln1 = anlz~anln1 AND
             anla~anln2 = anlz~anln2 AND
             anla~bukrs = anlz~bukrs
        FOR ALL ENTRIES IN @lt_mseg
          WHERE anla~bukrs = @lt_mseg-bukrs
            AND viau~aufnr = @lt_mseg-aufnr
      INTO TABLE @DATA(lt_anla_viaufks).

    SORT lt_anla_viaufks BY aufnr.
    "---------------------------------------------------------------


    "---------------------------------------------------------------
    SELECT p~ebeln, p~ebelp,
           p~navnw, m~bsart,
           p~creationdate, p~lgort,
           p~werks, adrc~name3
      FROM ekpo AS p
        INNER JOIN ekko AS m
          ON m~ebeln = p~ebeln
        LEFT JOIN twlad
          ON twlad~lgort = p~lgort AND
             twlad~werks = p~werks
        LEFT JOIN adrc                                 "#EC CI_BUFFJOIN
          ON twlad~adrnr = adrc~addrnumber
        FOR ALL ENTRIES IN @lt_mseg
      WHERE p~ebeln = @lt_mseg-ebeln
        AND p~ebelp = @lt_mseg-ebelp ##WARN_OK
        AND p~loekz = @abap_false
      INTO TABLE @DATA(lt_ekpo).
    "---------------------------------------------------------------


    "Изменение MOL
    "---------------------------------------------------------------
    IF lt_ekpo IS NOT INITIAL.

      SELECT *
        FROM ztmm_change_mol
          FOR ALL ENTRIES IN @lt_ekpo
            WHERE bsart = @lt_ekpo-bsart
              AND werks = @lt_ekpo-werks
              AND lgort = @lt_ekpo-lgort
        INTO TABLE @DATA(lt_chane_mol).

    ENDIF.
    "---------------------------------------------------------------


    "Передавать ХML c  SHB_ФИЛ_ПЕРЕДАЧА    в ЦФТ,
    "только после того как выполнять операцию Прихода (101 вдв)
    "---------------------------------------------------------------
    IF line_exists( lt_mseg[ bwart = lc_bwart_641 ] ) AND
       lt_ekpo IS NOT INITIAL.


      SELECT ebeln                                 "#EC CI_NO_TRANSFORM
        FROM ekbe
          FOR ALL ENTRIES IN @lt_ekpo
        WHERE ebeln = @lt_ekpo-ebeln
          AND ebelp = @lt_ekpo-ebelp ##WARN_OK
          AND gjahr = @lt_ekpo-creationdate(4)
          AND vgabe = '1'
        INTO TABLE @DATA(lt_table).

      IF sy-subrc <> 0.
        CLEAR rs_data.
        RETURN.
      ENDIF.

    ENDIF.
    "---------------------------------------------------------------



    "---------------------------------------------------------------
    SELECT mblnr, mjahr, zeile,
           iblnr, gjahr, zeili
      FROM iseg
        FOR ALL ENTRIES IN @lt_mseg
      WHERE mblnr = @lt_mseg-mblnr
        AND zeile = @lt_mseg-zeile
        AND mjahr = @lt_mseg-mjahr
      INTO TABLE @DATA(lt_iseg).

    SORT lt_iseg BY mblnr mjahr zeile.
    "---------------------------------------------------------------



    "---------------------------------------------------------------
    DATA(lv_ebeln) = lt_mseg[ 1 ]-ebeln.

    SELECT belnr, gjahr, stock_posting,
           wrbtr, menge, ebeln, ebelp,
           tbtkz, mwskz, bukrs, shkzg
      FROM rseg
      INTO TABLE @DATA(lt_rseg)
      WHERE ebeln = @lv_ebeln
      ORDER BY ebeln, ebelp.

    IF sy-subrc EQ 0.

      DATA(lv_belnr) = lt_rseg[ 1 ]-belnr.
      DATA(lv_gjahr) = lt_rseg[ 1 ]-gjahr.

      SELECT belnr, gjahr, bldat, budat,
             xblnr, zuonr, lifnr
        FROM rbkp
         INTO TABLE @DATA(lt_rbkp)
        WHERE belnr = @lv_belnr
          AND gjahr  = @lv_gjahr. " and "STBLG ne space. "исключить пары прямой-сторно.

      IF lt_rbkp IS NOT INITIAL.

        SELECT stblg,stjah
          FROM rbkp
          INTO TABLE @DATA(lt_rbkp_stor)           "#EC CI_NO_TRANSFORM
           FOR ALL ENTRIES IN @lt_rbkp
          WHERE stblg = @lt_rbkp-belnr
            AND stjah  = @lt_rbkp-gjahr
            AND rbstat IN @lt_rng_rbstat.

        LOOP AT lt_rbkp_stor ASSIGNING FIELD-SYMBOL(<ls_rbkp_stro>).

          READ TABLE lt_rbkp ASSIGNING FIELD-SYMBOL(<ls_rbkp>)
                                           WITH KEY belnr = <ls_rbkp_stro>-stblg
                                                    gjahr = <ls_rbkp_stro>-stjah.

          IF sy-subrc EQ 0.
            "Значит документ сторнирован
            DELETE lt_rbkp WHERE table_line = <ls_rbkp>.
          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDIF.


    READ TABLE lt_rbkp INTO DATA(ls_rbkp) INDEX 1.      "#EC CI_NOORDER
    "---------------------------------------------------------------



    "---------------------------------------------------------------
    lt_rng_anln1 = VALUE #( FOR ls_ms IN lt_mseg sign = 'I' option = 'EQ' ( low = COND #( WHEN ls_ms-anln1 IS NOT INITIAL
                                                                                          THEN ls_ms-anln1
                                                                                          WHEN ls_ms-sgtxt  IS NOT INITIAL
                                                                                          THEN ls_ms-sgtxt(12) ) ) ).
    lt_rng_anln2 = VALUE #( FOR ls_ms IN lt_mseg sign = 'I' option = 'EQ' ( low = COND #( WHEN ls_ms-anln1 IS NOT INITIAL
                                                                                          THEN ls_ms-anln2
                                                                                          WHEN ls_ms-sgtxt  IS NOT INITIAL
                                                                                          THEN ls_ms-sgtxt+13(4) ) ) ).
    lt_rng_bukrs = VALUE #( FOR ls_ms IN lt_mseg sign = 'I' option = 'EQ' ( low = ls_ms-bukrs ) ).


    DELETE lt_rng_anln1 WHERE low IS INITIAL.
    DELETE lt_rng_anln2 WHERE low IS INITIAL.
    DELETE lt_rng_bukrs WHERE low IS INITIAL.
    SORT: lt_rng_anln1, lt_rng_anln2, lt_rng_bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM: lt_rng_anln1, lt_rng_anln2, lt_rng_bukrs COMPARING low.

    IF lt_rng_anln1 IS NOT INITIAL.




      SELECT m~bukrs, m~anln1, m~anln2,
             m~invnr, z~pernr, m~anlkl,
             m~ktogr, z~kostl, z~werks
          FROM anla AS m
          INNER JOIN anlz AS z
            ON z~bukrs = m~bukrs AND
               z~anln1 = m~anln1 AND
               z~anln2 = m~anln2
                 INTO TABLE @DATA(lt_anla)
           WHERE m~bukrs  IN @lt_rng_bukrs[]
             AND m~anln1  IN @lt_rng_anln1[]
             AND m~anln2  IN @lt_rng_anln2[]
             AND z~bdatu  >= @sy-datum
             AND z~adatu  <= @sy-datum
        ORDER BY m~bukrs, m~anln1, m~anln2.

    ENDIF.
    "---------------------------------------------------------------



    "---------------------------------------------------------------
    IF lt_anla IS NOT INITIAL.

      SELECT anlkl, zgr                            "#EC CI_NO_TRANSFORM
        FROM ztmm_003_cl_gr_1
          FOR ALL ENTRIES IN @lt_anla
            WHERE anlkl = @lt_anla-anlkl
        INTO TABLE @DATA(lt_003_cl_gr_1).

    ENDIF.
    "---------------------------------------------------------------



    "---------------------------------------------------------------
    SELECT bwart, grund,
           code_doc_abc,
           vorgang, bsart
      FROM zint_mm_viddvig
        FOR ALL ENTRIES IN @lt_mseg
         WHERE ( bwart = @lt_mseg-bwart
             AND grund = @lt_mseg-grund )
             OR vorgang = '3'
*             AND ( flow = 'ПТ0111' OR flow IS INITIAL )
      INTO TABLE @DATA(lt_viddvig).
    "---------------------------------------------------------------


    "---------------------------------------------------------------
    SELECT msehi,
           mm_okei
      FROM zint_mm_okei
        FOR ALL ENTRIES IN @lt_mseg
          WHERE msehi = @lt_mseg-erfme
             OR msehi = @lt_mseg-bprme
      INTO TABLE @DATA(lt_mm_okei)
      BYPASSING BUFFER.
    "---------------------------------------------------------------





    "---------------------------------------------------------------
    SELECT m~matnr, m~matkl, t~maktx, c~sernp
      FROM mara AS m
        LEFT JOIN makt AS t
          ON t~matnr = m~matnr AND
             t~spras = @sy-langu
        LEFT JOIN marc AS c
          ON t~matnr = c~matnr
        FOR ALL ENTRIES IN @lt_mseg
      WHERE m~matnr = @lt_mseg-matnr
      INTO TABLE @DATA(lt_mara) ##WARN_OK.

    SORT lt_mara BY matnr.

    IF lt_mara IS NOT INITIAL.

      SELECT zmatkl, zcl, zgr, zgros , zclbnds     "#EC CI_NO_TRANSFORM
        FROM ztmm_cl_gr
*          FOR ALL ENTRIES IN @lt_mara
*            WHERE zmatkl = @lt_mara-matkl
        INTO TABLE @DATA(lt_cl_gr)
        ORDER BY zmatkl.

      SELECT zmatkl, zgr, bwart,
             ktogr, grund                          "#EC CI_NO_TRANSFORM
        FROM ztmm_change_zgr
*          FOR ALL ENTRIES IN @lt_mara
*            WHERE zmatkl = @lt_mara-matkl
        INTO TABLE @DATA(lt_change_gr).

    ENDIF.
    "---------------------------------------------------------------


    "---------------------------------------------------------------
    lt_rng_inob_obj = VALUE #( FOR ls_ms IN lt_mseg sign = 'I' option = 'EQ' ( low = ls_ms-matnr && `                      ` && ls_ms-charg ) ).

    SELECT inob~cuobj, inob~objek,
           ausp~atwrt
        FROM inob
          JOIN ausp
            ON inob~cuobj = ausp~objek AND
               ausp~atinn = @lv_atinn_matsh
      INTO TABLE @DATA(lt_inob_ausp)
      WHERE inob~klart = `023`
        AND inob~obtab = `MCH1`
        AND inob~objek IN @lt_rng_inob_obj ##WARN_OK.
    "---------------------------------------------------------------

* Eremetovi 18.11.2022 16:14:44
* {
    DATA(ls_mseg) = lt_mseg[ 1 ].
    _get_parent_matdoc( is_mseg = CORRESPONDING #( ls_mseg ) iv_bktxt = ls_mkpf-bktxt ).

* }

***    IF lv_loc_active = abap_true.
***      lt_locationid = CORRESPONDING #( lt_mseg ).
***      _get_locationid( CHANGING ct_locationid = lt_locationid ).
***    ENDIF.

    "---------------------------------------------------------------
    SELECT matnr, charge, sernr
      FROM v_equi_eqbs_sml
      INTO TABLE @DATA(lt_sernr)
        FOR ALL ENTRIES IN @lt_mseg
      WHERE matnr = @lt_mseg-matnr
        AND charge = @lt_mseg-charg
        AND s_eqbs = 'X' ##NEEDED.
    "---------------------------------------------------------------

    DATA(lv_bsart) = VALUE #( lt_ekpo[ 1 ]-bsart OPTIONAL ).

    "Заполнение данных
    "---------------------------------------------------------------
    APPEND INITIAL LINE TO lt_mat_move ASSIGNING FIELD-SYMBOL(<ls_mat_move>).

    <ls_mat_move>-materialdocnum = ls_mkpf-mblnr.
    <ls_mat_move>-materialdocyear =  ls_mkpf-mjahr.
    <ls_mat_move>-parent_materialdoc_num = lv_parent_mat_doc.


    SELECT SINGLE m~abs_num
      FROM zrcmt_dgv_attr AS m
        INNER JOIN scmg_t_case_attr AS s
          ON s~case_guid = m~case_guid
      INTO <ls_mat_move>-cftagreementid
      WHERE s~ext_key = ls_mkpf-zzext_key ##WARN_OK .

    <ls_mat_move>-agreementid = ls_mkpf-zzext_key  .

    <ls_mat_move>-materialdocdate = ls_mkpf-bldat.
    <ls_mat_move>-entrysupposeddate = ls_mkpf-budat.
    <ls_mat_move>-primarydocnum = ls_mkpf-xblnr.

    SELECT SINGLE accnt FROM usr02 WHERE bname = @ls_mkpf-usnam INTO @<ls_mat_move>-userlogin.

    <ls_mat_move>-userlogin = CONV persno( |{ <ls_mat_move>-userlogin ALPHA = IN }| ). " Eremetov 02.03.2023 14:33:45


    READ TABLE lt_mseg ASSIGNING <ls_mseg> WITH KEY mblnr = ls_mkpf-mblnr
                                                    mjahr = ls_mkpf-mjahr
                                                    xauto = space.
    IF sy-subrc EQ 0.

      READ TABLE lt_ekpo
           ASSIGNING FIELD-SYMBOL(<ls_ekpo>)
           WITH KEY ebeln = <ls_mseg>-ebeln
                    ebelp = <ls_mseg>-ebelp.
      IF sy-subrc = 0.

        READ TABLE lt_chane_mol WITH KEY bsart    = lv_bsart
                                         werks    = <ls_ekpo>-werks
                                         lgort    = <ls_ekpo>-lgort
                                         mol_type = 'S'
             ASSIGNING FIELD-SYMBOL(<ls_change_mol>).
        IF sy-subrc = 0.
          <ls_mat_move>-molsuppliertabnum = <ls_change_mol>-mol.
          DATA(lv_molsupplier_changed) = abap_true.
        ENDIF.

      ENDIF.

      IF lv_molsupplier_changed <> abap_true.

        SELECT SINGLE name3                            "#EC CI_BUFFJOIN
          INTO @<ls_mat_move>-molsuppliertabnum
          FROM adrc AS a
          INNER JOIN twlad AS t
            ON t~adrnr = a~addrnumber
          WHERE t~lgort = @<ls_mseg>-lgort
            AND t~werks = @<ls_mseg>-werks ##WARN_OK.

      ENDIF.

      <ls_mat_move>-werksupplier      = `0` && <ls_mseg>-werks(2) && `-` && `0` && <ls_mseg>-werks+2(2).
      <ls_mat_move>-ordernum          = <ls_mseg>-ebeln.
      <ls_mat_move>-supplierclientid  = <ls_mseg>-lifnr.

    ENDIF.



    READ TABLE lt_mseg ASSIGNING <ls_mseg> WITH KEY mblnr = ls_mkpf-mblnr
                                                    mjahr = ls_mkpf-mjahr
                                                    xauto = 'X'.
    IF sy-subrc EQ 0.

      READ TABLE lt_ekpo
           ASSIGNING <ls_ekpo>
           WITH KEY ebeln = <ls_mseg>-ebeln
                    ebelp = <ls_mseg>-ebelp.
      IF sy-subrc = 0.

        READ TABLE lt_chane_mol WITH KEY bsart    = lv_bsart
                                         werks    = <ls_ekpo>-werks
                                         lgort    = <ls_ekpo>-lgort
                                         mol_type = 'R'
             ASSIGNING <ls_change_mol>.
        IF sy-subrc = 0.
          <ls_mat_move>-molreceivertabnum = <ls_change_mol>-mol.
          DATA(lv_molreviever_changed) = abap_true.
        ENDIF.

      ENDIF.

      IF lv_molreviever_changed <> abap_true.

        SELECT SINGLE name3 INTO @<ls_mat_move>-molreceivertabnum "#EC CI_BUFFJOIN
          FROM adrc AS a
          INNER JOIN twlad AS t
            ON t~adrnr = a~addrnumber
           WHERE t~lgort = @<ls_mseg>-lgort
             AND t~werks = @<ls_mseg>-werks ##WARN_OK.

      ENDIF.

      <ls_mat_move>-werkreceiver     = `0` && <ls_mseg>-werks(2) && `-` && `0` && <ls_mseg>-werks+2(2).
      <ls_mat_move>-ordernum         = COND #( WHEN <ls_mat_move>-ordernum IS INITIAL THEN <ls_mseg>-ebeln ELSE <ls_mat_move>-ordernum ).
      <ls_mat_move>-supplierclientid = COND #( WHEN <ls_mat_move>-supplierclientid IS INITIAL THEN <ls_mseg>-lifnr ELSE <ls_mat_move>-supplierclientid ).

    ENDIF.







    <ls_mat_move>-materialdoccode = COND #( WHEN line_exists( lt_viddvig[ bwart = lt_mseg[ 1 ]-bwart
                                                                          grund = lt_mseg[ 1 ]-grund
                                                                          bsart = lv_bsart ] )
                                            THEN lt_viddvig[ bwart = lt_mseg[ 1 ]-bwart
                                                             grund = lt_mseg[ 1 ]-grund
                                                             bsart = lv_bsart ]-code_doc_abc
                                            WHEN line_exists( lt_viddvig[ bwart = lt_mseg[ 1 ]-bwart
                                                                          grund = lt_mseg[ 1 ]-grund ] )
                                            THEN lt_viddvig[ bwart = lt_mseg[ 1 ]-bwart
                                                             grund = lt_mseg[ 1 ]-grund ]-code_doc_abc ).

    "{baryutin-sa EXT_ITECO-1605  ЗНИ 280 14.08.23
    READ TABLE lt_mseg INDEX 1 ASSIGNING <ls_mseg>.
    IF <ls_mat_move>-materialdoccode = 'SHB_МАТ_РАСХОД' AND <ls_mseg>-kostl IS NOT INITIAL.
      IF <ls_mseg>-bwart = '201'.
        IF <ls_mseg>-kostl+1(1) <> '0'.
          <ls_mat_move>-werkreceiver = `0` && <ls_mseg>-kostl+1(2) && `-` && `0` && <ls_mseg>-kostl+3(2).
        ELSEIF <ls_mseg>-kostl+1(2) = '00' AND (
          <ls_mseg>-kostl+5(1) = '0' OR <ls_mseg>-kostl+5(1) = '3' OR <ls_mseg>-kostl+5(1) = '4'
          ).
          <ls_mat_move>-werkreceiver = `0` && <ls_mseg>-kostl+1(2) && `-` && `0` && <ls_mseg>-kostl+1(2).
        ELSEIF <ls_mseg>-kostl+1(2) = '00' AND (
          <ls_mseg>-kostl+5(1) = '1' OR <ls_mseg>-kostl+5(1) = '2' OR <ls_mseg>-kostl+5(1) = '5'
          ).
          <ls_mat_move>-werkreceiver = `0` && <ls_mseg>-kostl+1(2) && `-` && `0` && <ls_mseg>-kostl+3(2).
        ELSE.
          <ls_mat_move>-werkreceiver      = `0` && <ls_mseg>-werks(2) && `-` && `0` && <ls_mseg>-werks+2(2).
        ENDIF.
      ENDIF.
*    ELSE.
*      <ls_mat_move>-werkreceiver      = `0` && <ls_mseg>-werks(2) && `-` && `0` && <ls_mseg>-werks+2(2).
    ENDIF.
    "}baryutin-sa EXT_ITECO-1605  ЗНИ 280 14.08.23


    "{ Shanarov B.V. - 19.06.2023 17:09:36 - ДЕФЕКТ 1289
    IF lt_mseg[ 1 ]-bwart = lc_bwart_301.

      DATA(ls_mseg_301) = lt_mseg[ 1 ].

      SELECT SINGLE code_doc_abc
        FROM ztmm_vnepilot
        WHERE ( werks = @ls_mseg_301-werks AND lgort = @ls_mseg_301-lgort )
           OR ( werks = @ls_mseg_301-umwrk AND lgort = @ls_mseg_301-umlgo )
        INTO @DATA(lv_matdoc).

      <ls_mat_move>-materialdoccode        = COND #( WHEN sy-subrc = 0 THEN lv_matdoc ELSE <ls_mat_move>-materialdoccode ).
      <ls_mat_move>-parent_materialdoc_num = <ls_mat_move>-materialdocnum.
    ENDIF.
    "} Shanarov B.V. - 19.06.2023 17:09:36 - ДЕФЕКТ 1289


    <ls_mat_move>-textheader = ls_mkpf-bktxt.



    IF lt_mseg[ 1 ]-bwart = lc_bwart_601.

      LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<ls_vbap>)
                      WHERE zuonr IS NOT INITIAL.
        <ls_mat_move>-agreementid    = <ls_vbap>-zuonr.
        <ls_mat_move>-cftagreementid = <ls_vbap>-abs_num.
        EXIT.
      ENDLOOP.

    ELSE.

      IF <ls_mat_move>-ordernum IS NOT INITIAL AND
         ls_mkpf-zzext_key      IS INITIAL.


        SELECT SINGLE m~abs_num , e~zzext_key
          FROM zrcmt_dgv_attr AS m
          INNER JOIN scmg_t_case_attr AS s ON s~case_guid = m~case_guid
          INNER JOIN ekko AS e ON s~ext_key = e~zzext_key AND e~ebeln = @<ls_mat_move>-ordernum
           INTO @DATA(ls_ext_key).

        IF sy-subrc = 0.

          <ls_mat_move>-cftagreementid = ls_ext_key-abs_num.
          <ls_mat_move>-agreementid    = ls_ext_key-zzext_key  .

        ENDIF.
      ENDIF.
    ENDIF.


*    IF <ls_mat_move>-cftagreementid IS NOT INITIAL.
*      CLEAR <ls_mat_move>-supplierclientid.
*    ENDIF.


*{ Kvizhinadze 14.07.2023 replace select
*    SELECT objk~sernr, anla~anln1, anla~anln2,
*           anla~invnr, anla~anlkl, clgr~zgr,
*           ser03~zeile, ser03~werk, ser03~charge,
*           itob~serge
*      FROM objk
*        JOIN ser03 ON objk~obknr   = ser03~obknr AND
*                      ser03~obknr  = objk~obknr
*        JOIN itob ON itob~sernr = objk~sernr AND
*                     itob~matnr = objk~matnr
*        LEFT JOIN anla  ON anla~sernr = objk~sernr
*        LEFT JOIN ztmm_003_cl_gr_1 AS clgr ON clgr~anlkl = anla~anlkl
*      INTO TABLE @DATA(lt_sernum)
*      WHERE ser03~mblnr  = @<ls_mseg>-mblnr
*        AND ser03~mjahr  = @<ls_mseg>-mjahr.
***    IF <ls_mseg>-bwart = '201'.
    SELECT objk~sernr,
           anla~anln1, anla~anln2,
           anla~invnr, anla~anlkl, clgr~zgr,
           ser03~zeile, ser03~werk, ser03~charge,
           equi~serge

      FROM @lt_mseg AS msg
          JOIN ser03  ON ser03~mblnr  = msg~mblnr
                      AND ser03~mjahr  = msg~mjahr
                      AND ser03~zeile  = msg~zeile
                      AND ser03~werk   = msg~werks
                      AND ser03~charge = msg~charg
          JOIN objk                  ON objk~obknr  = ser03~obknr
          JOIN equi                  ON equi~sernr  = objk~sernr
                                    AND equi~matnr  = objk~matnr
          LEFT JOIN anla             ON anla~sernr  = objk~sernr
                                    AND anla~anln1 IS NOT INITIAL
*                                               AND anla~bukrs  = itob~bukrs
*                                               AND ( anla~anln1 = itob~anlnr AND anla~anln1 IS NOT INITIAL )
*                                               AND anla~anln2 = itob~anlun
        LEFT JOIN ztmm_003_cl_gr_1 AS clgr ON clgr~anlkl = anla~anlkl

      INTO TABLE @DATA(lt_sernum).
***    ELSE.
***      SELECT objk~sernr,
***             anla~anln1, anla~anln2,
***             anla~invnr, anla~anlkl, clgr~zgr,
***             ser03~zeile, ser03~werk, ser03~charge,
***             equi~serge
***
***        FROM ser03 JOIN objk                  ON objk~obknr  = ser03~obknr
***                   JOIN equi                  ON equi~sernr  = objk~sernr
***                                             AND equi~matnr  = objk~matnr
***                   LEFT JOIN anla             ON anla~sernr  = objk~sernr
***                                             AND anla~anln1 IS NOT INITIAL
****                                               AND anla~bukrs  = itob~bukrs
****                                               AND ( anla~anln1 = itob~anlnr AND anla~anln1 IS NOT INITIAL )
****                                               AND anla~anln2 = itob~anlun
***          LEFT JOIN ztmm_003_cl_gr_1 AS clgr ON clgr~anlkl = anla~anlkl
***
***        WHERE ser03~mblnr  = @<ls_mseg>-mblnr
***          AND ser03~mjahr  = @<ls_mseg>-mjahr
***          AND ser03~zeile  = @<ls_mseg>-zeile
***          AND ser03~werk   = @<ls_mseg>-werks
***          AND ser03~charge = @<ls_mseg>-charg
***        INTO TABLE @lt_sernum.
***    ENDIF.

*} Kvizhinadze 14.07.2023

    SORT lt_sernum BY sernr anln1 anln2 invnr anlkl zgr zeile werk charge serge.
    DELETE ADJACENT DUPLICATES FROM lt_sernum COMPARING sernr anln1 anln2 invnr anlkl zgr zeile werk charge serge.



    LOOP AT lt_mseg ASSIGNING <ls_mseg> WHERE parent_id IS INITIAL.

*      CLEAR lt_serge.


      APPEND INITIAL LINE TO <ls_mat_move>-position ASSIGNING FIELD-SYMBOL(<ls_posit>).

      <ls_posit>-bwart = <ls_mseg>-bwart.
      <ls_posit>-grund = <ls_mseg>-grund.


      DO 2 TIMES.

        IF sy-index = 2.

          "Находим зависимую позицию
          "----------------------------------------------------------
          READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<ls_mseg_p>) WITH KEY mblnr     = <ls_mseg>-mblnr
                                                                          mjahr     = <ls_mseg>-mjahr
                                                                          parent_id = <ls_mseg>-line_id.
*                                                                          BINARY SEARCH.
          IF sy-subrc NE 0." Когда связи нет, то

            <ls_mat_move>-werksupplier      = COND #( WHEN <ls_mat_move>-werksupplier IS INITIAL
                                                      THEN <ls_mat_move>-werkreceiver
                                                      ELSE <ls_mat_move>-werksupplier ).

            <ls_mat_move>-werkreceiver      = COND #( WHEN <ls_mat_move>-werkreceiver IS INITIAL
                                                      THEN <ls_mat_move>-werksupplier
                                                      ELSE <ls_mat_move>-werkreceiver ).

            <ls_mat_move>-molreceivertabnum = COND #( WHEN <ls_mat_move>-molreceivertabnum IS INITIAL
                                                      THEN <ls_mat_move>-molsuppliertabnum
                                                      ELSE <ls_mat_move>-molreceivertabnum ).

            <ls_mat_move>-molsuppliertabnum = COND #( WHEN <ls_mat_move>-molsuppliertabnum IS INITIAL
                                                      THEN <ls_mat_move>-molreceivertabnum
                                                      ELSE <ls_mat_move>-molsuppliertabnum ).

            <ls_posit>-batchsuppliernum     = COND #( WHEN <ls_posit>-batchsuppliernum IS INITIAL
                                                      THEN <ls_posit>-batchreceivernum
                                                      ELSE <ls_posit>-batchsuppliernum ).

            <ls_posit>-batchreceivernum     = COND #( WHEN <ls_posit>-batchreceivernum IS INITIAL
                                                      THEN <ls_posit>-batchsuppliernum
                                                      ELSE <ls_posit>-batchreceivernum ).
            <ls_posit>-batchreceivernum     = COND #( WHEN <ls_mseg>-bwart             = lc_bwart_241  AND
                                                           <ls_mseg>-grund             = lc_grund_0002 AND
                                                           <ls_posit>-batchreceivernum = <ls_posit>-batchsuppliernum
                                                      THEN space
                                                      WHEN <ls_mseg>-bwart             = lc_bwart_261  AND
                                                           <ls_mseg>-grund             = lc_grund_0002
                                                      THEN space
                                                      ELSE <ls_posit>-batchreceivernum ).
            CONTINUE.
          ELSE.
            <ls_mseg> = <ls_mseg_p>.
          ENDIF.
          "----------------------------------------------------------
        ENDIF.


        "------------------------------
        lv_objek    = <ls_mseg>-matnr && `                      ` && <ls_mseg>-charg.
        lv_txt_name = <ls_mseg>-ebeln && <ls_mseg>-ebelp.

        lv_anla1 = COND #( WHEN <ls_mseg>-anln1 IS NOT INITIAL THEN <ls_mseg>-anln1
                           WHEN <ls_mseg>-sgtxt IS NOT INITIAL THEN <ls_mseg>-sgtxt(12) ).
        lv_anla2 = COND #( WHEN <ls_mseg>-anln1 IS NOT INITIAL THEN <ls_mseg>-anln2
                           WHEN <ls_mseg>-sgtxt IS NOT INITIAL THEN <ls_mseg>-sgtxt+13(4) ).
        "------------------------------


        "------------------------------
        <ls_mat_move>-molreceivertabnum = COND #( WHEN lv_molreviever_changed = abap_true
                                                  THEN <ls_mat_move>-molreceivertabnum
                                                  WHEN <ls_mseg>-bwart = lc_bwart_641
                                                  THEN VALUE #( lt_ekpo[ ebeln = <ls_mseg>-ebeln
                                                                         ebelp = <ls_mseg>-ebelp ]-name3 OPTIONAL )
                                                  WHEN <ls_mseg>-bwart = lc_bwart_261 AND <ls_mseg>-grund = lc_grund_0002
                                                  THEN VALUE #( lt_anla_viaufks[ aufnr = <ls_mseg>-aufnr ]-pernr OPTIONAL )
                                                  ELSE <ls_mat_move>-molreceivertabnum ).
        "------------------------------

        "------------------------------
        <ls_posit>-inventorynum       = COND #( WHEN <ls_mseg>-bwart = lc_bwart_261 AND <ls_mseg>-grund = lc_grund_0002
                                                  OR <ls_mseg>-bwart = lc_bwart_501 AND <ls_mseg>-grund = lc_grund_0001 " Eremetov 18.11.2022 10:58:15
                                                THEN VALUE #( lt_anla_viaufks[ aufnr = <ls_mseg>-aufnr ]-invnr OPTIONAL ) ).
        <ls_posit>-oscardnum          = COND #( WHEN <ls_posit>-inventorynum IS NOT INITIAL
                                                THEN ''
* Eremetovi 11.11.2022 16:02:31
* {
                                                WHEN <ls_mseg>-bwart = lc_bwart_501 AND <ls_mseg>-grund = lc_grund_0001
                                                THEN lv_anla1 && lv_anla2
* }
                                                WHEN <ls_mseg>-bwart = lc_bwart_261 AND <ls_mseg>-grund = lc_grund_0002
                                                THEN VALUE #( lt_anla_viaufks[ aufnr = <ls_mseg>-aufnr ]-anlnr OPTIONAL )
                                                ELSE lv_anla1 && lv_anla2 ).


        <ls_posit>-charge             = <ls_mseg>-charg.
        <ls_posit>-werks              = <ls_mseg>-werks.
        <ls_posit>-lgort              = <ls_mseg>-lgort.

***        IF lv_loc_active = abap_true.
***          READ TABLE lt_locationid WITH KEY matnr = <ls_mseg>-matnr
***                                            werks = <ls_mseg>-werks
***                                            lgort = <ls_mseg>-lgort
***                                            ASSIGNING FIELD-SYMBOL(<ls_locid>).
***          IF sy-subrc = 0.
***            <ls_posit>-locationid = <ls_locid>-locationid.
***          ENDIF.
***        ENDIF.

*        <ls_posit>-amount             = <ls_mseg>-dmbtr.
        <ls_posit>-amount             = COND #( WHEN <ls_posit>-amount IS INITIAL
                                                THEN COND #( WHEN lt_rng_bwart_change_batch IS NOT INITIAL AND <ls_mseg>-bwart IN lt_rng_bwart_change_batch AND <ls_mseg>-charg = <ls_mseg>-umcha AND <ls_mseg>-bwtar = <ls_mseg>-umbar
                                                             THEN <ls_mseg>-menge * <ls_mseg>-salk3 / <ls_mseg>-lbkum
                                                             ELSE <ls_mseg>-dmbtr )
                                                ELSE <ls_posit>-amount ).
        <ls_posit>-tmcnum             = <ls_mseg>-matnr.
        <ls_posit>-iskindbusinessbank = '1'.
        <ls_posit>-isvatamount        = '1'.
        <ls_posit>-isvatinclude       = '1'.
        <ls_posit>-okof               = space.
        <ls_posit>-usefullifeterm     = space.
        <ls_posit>-mataccount         = COND #( WHEN <ls_posit>-mataccount IS INITIAL
                                                THEN VALUE #( lt_inob_ausp[ objek = lv_objek ]-atwrt OPTIONAL )
                                                ELSE <ls_posit>-mataccount ).
        <ls_posit>-quantity           = COND #( WHEN <ls_posit>-quantity IS INITIAL THEN <ls_mseg>-menge ELSE <ls_posit>-quantity ).
        <ls_posit>-batchsuppliernum   = COND #( WHEN <ls_mseg>-xauto = space
                                                THEN <ls_mseg>-charg
                                                ELSE <ls_posit>-batchsuppliernum ).
        <ls_posit>-batchreceivernum   = COND #( WHEN <ls_mseg>-bwart = lc_bwart_641 THEN <ls_mseg>-bwtar
                                                WHEN <ls_mseg>-xauto = abap_true    THEN <ls_mseg>-charg
                                                ELSE <ls_posit>-batchreceivernum ).
        <ls_posit>-unitmeasurecode    = COND #( WHEN <ls_posit>-unitmeasurecode IS INITIAL AND <ls_mseg>-bprme IS NOT INITIAL
                                                THEN VALUE #( lt_mm_okei[ msehi = <ls_mseg>-bprme ]-mm_okei OPTIONAL )
                                                WHEN <ls_posit>-unitmeasurecode IS INITIAL AND <ls_mseg>-erfme IS NOT INITIAL
                                                THEN VALUE #( lt_mm_okei[ msehi = <ls_mseg>-erfme ]-mm_okei OPTIONAL ) ).
        <ls_posit>-batchreceivernum   = COND #( WHEN <ls_mseg>-bwart             = lc_bwart_241  AND
                                                     <ls_mseg>-grund             = lc_grund_0002 AND
                                                     <ls_posit>-batchreceivernum = <ls_posit>-batchsuppliernum
                                                THEN space
                                                WHEN <ls_mseg>-bwart             = lc_bwart_261  AND
                                                     <ls_mseg>-grund             = lc_grund_0002
                                                THEN space
                                                ELSE <ls_posit>-batchreceivernum ).
        "------------------------------


        "------------------------------
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id       = 'F01'
            language = 'R'
            name     = lv_txt_name
            object   = 'EKPO'
          TABLES
            lines    = lt_long_text
          EXCEPTIONS
            OTHERS   = 8.

        IF sy-subrc = 0.
          LOOP AT lt_long_text ASSIGNING FIELD-SYMBOL(<ls_long_txt>).
            <ls_posit>-textline = <ls_posit>-textline && <ls_long_txt>-tdline.
          ENDLOOP.
        ENDIF.
        "------------------------------


        "------------------------------
        IF <ls_mseg>-bwart = lc_bwart_701 AND <ls_mseg>-grund = lc_grund_0000.

          READ TABLE lt_iseg ASSIGNING FIELD-SYMBOL(<ls_iseg>)
               WITH KEY mblnr = <ls_mseg>-mblnr
                        mjahr = <ls_mseg>-mjahr
                        zeile = <ls_mseg>-zeile
               BINARY SEARCH.

          IF sy-subrc = 0.



            ls_sernos_key = VALUE #( taser = lc_taser
                                     iblnr = <ls_iseg>-iblnr
                                     mjahr = <ls_iseg>-mjahr
                                     zeili = <ls_iseg>-zeili ).

            CALL FUNCTION 'GET_SERNOS_OF_DOCUMENT'
              EXPORTING
                key_data = ls_sernos_key
              TABLES
                sernos   = lt_sernos
              EXCEPTIONS
                OTHERS   = 4.
            IF sy-subrc <> 0.
            ENDIF.

            LOOP AT lt_sernos ASSIGNING FIELD-SYMBOL(<ls_sernos>).

              CASE <ls_sernos>-oltpi.
                WHEN '1'.
                  CHECK NOT line_exists( lt_sernos[ sernr = <ls_sernos>-sernr equnr = <ls_sernos>-equnr matnr = <ls_sernos>-matnr oltpi = '2' ] ).
                WHEN '2'.
                  CHECK NOT line_exists( lt_sernos[ sernr = <ls_sernos>-sernr equnr = <ls_sernos>-equnr matnr = <ls_sernos>-matnr oltpi = '1' ] ).
                WHEN OTHERS.
                  CONTINUE.
              ENDCASE.

              SELECT DISTINCT serge
                FROM itob
                WHERE sernr = @<ls_sernos>-sernr
                  AND equnr = @<ls_sernos>-equnr
                  AND matnr = @<ls_sernos>-matnr
                INTO TABLE @<ls_posit>-serge.
*                INTO TABLE @DATA(lt_sernos_serge).

*              LOOP AT lt_sernos_serge ASSIGNING FIELD-SYMBOL(<ls_sernos_serge>).

*                SHIFT <ls_sernos_serge>-serge LEFT DELETING LEADING '0'.

              "Для удаления дубликатов

*                CHECK NOT line_exists( lt_serge[ serge = <ls_sernos_serge>-serge ] ).
*                <ls_posit>-factorynum = |{ <ls_posit>-factorynum },{ <ls_sernos_serge>-serge }|.
*                COLLECT VALUE ty_serge( serge = <ls_sernos_serge>-serge ) INTO lt_serge.

*              ENDLOOP.
            ENDLOOP.

          ENDIF.


        ELSE.
          IF <ls_mseg>-bwart = lc_bwart_201 AND <ls_mseg>-grund = lc_grund_0001.
            LOOP AT lt_sernum ASSIGNING FIELD-SYMBOL(<ls_sernum>)
              WHERE             werk   = <ls_mseg>-werks
                                AND charge = <ls_mseg>-charg
                                AND serge IS NOT INITIAL.
              APPEND <ls_sernum>-serge TO <ls_posit>-serge.
            ENDLOOP.
          ELSE.
            LOOP AT lt_sernum ASSIGNING <ls_sernum>
                              WHERE zeile  = <ls_mseg>-zeile
                                AND werk   = <ls_mseg>-werks
                                AND charge = <ls_mseg>-charg
                                AND serge IS NOT INITIAL.

              APPEND <ls_sernum>-serge TO <ls_posit>-serge.

*            APPEND
*            SHIFT <ls_sernum>-serge LEFT DELETING LEADING '0'.
*
*
*
*            IF NOT line_exists( <ls_posit>-serge[ serge = <ls_sernum>-serge ] ).
*              <ls_posit>-factorynum = |{ <ls_posit>-factorynum },{ <ls_sernum>-serge }|.
*              COLLECT VALUE ty_serge( serge = <ls_sernum>-serge ) INTO lt_serge.
*            ENDIF.

            ENDLOOP.
          ENDIF.

        ENDIF.

*        SHIFT <ls_posit>-factorynum LEFT DELETING LEADING ','.
        "------------------------------

        READ TABLE lt_anla ASSIGNING FIELD-SYMBOL(<ls_anla>)
             WITH KEY anln1 = lv_anla1
                      anln2 = lv_anla2
                      bukrs = <ls_mseg>-bukrs
                      BINARY SEARCH.

        "------------------------------
        " >> 21.02.2023 SHESTAKOV-AO
        IF ( <ls_mseg>-bwart = lc_bwart_201 AND <ls_mseg>-grund = lc_grund_0002 AND <ls_mat_move>-materialdoccode = 'SHB_ВНЕСИСТ_УЧЕТ') OR
           ( <ls_mseg>-bwart = lc_bwart_241 AND <ls_mseg>-grund = lc_grund_0001 AND <ls_mat_move>-materialdoccode = 'SHB_СКЛАД_ПЕРЕД' ) OR
           ( <ls_mseg>-bwart = lc_bwart_241 AND <ls_mseg>-grund = lc_grund_0002 AND <ls_mat_move>-materialdoccode = 'SHB_МОНТАЖ' ) OR
           ( <ls_mseg>-bwart = lc_bwart_z41 AND <ls_mseg>-grund = lc_grund_0001 AND <ls_mat_move>-materialdoccode = 'SHB_СКЛАД_ПЕРЕД' ) OR
           ( <ls_mseg>-bwart = lc_bwart_z41 AND <ls_mseg>-grund = lc_grund_0002 AND <ls_mat_move>-materialdoccode = 'SHB_МОНТАЖ' ).

          IF <ls_anla> IS NOT INITIAL.

            <ls_posit>-inventorynum         = <ls_anla>-invnr.
            <ls_mat_move>-molreceivertabnum = <ls_anla>-pernr.
            <ls_mat_move>-werkreceiver      = `0` && <ls_anla>-werks(2) && `-` && `0` && <ls_anla>-werks+2(2).

          ELSEIF <ls_sernum> IS ASSIGNED.

            <ls_posit>-inventorynum = <ls_sernum>-invnr.

          ENDIF.

        ENDIF.
        " << 21.02.2023 SHESTAKOV-AO
        "------------------------------


        "------------------------------
        READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = <ls_mseg>-matnr BINARY SEARCH.
        IF sy-subrc = 0.

          DATA(lv_objk) = |{ <ls_mseg>-matnr }                      { <ls_mseg>-charg }|.
*          DATA(lv_objk) = |{ <ls_mseg>-matnr }          { <ls_mseg>-charg }|.

          <ls_posit>-tmcname = ls_mara-maktx.
          <ls_posit>-sernp   = ls_mara-sernp.
          <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-matkl ]-zcl OPTIONAL ).

          "Kashiev-AV 19.06.2023

          SELECT SINGLE ausp~atwrt
          FROM ausp
          INNER JOIN cabn ON cabn~atinn = ausp~atinn
          INNER JOIN inob ON inob~cuobj = ausp~objek
            WHERE cabn~atnam = 'Z_PUR_NDS' AND
                  inob~objek = @lv_objk AND
                  inob~obtab = 'MCH1' AND
                  inob~klart = '023'
          INTO @DATA(lv_atwrt).

          IF sy-subrc = 0 AND ( lv_atwrt = 0 OR lv_atwrt IS INITIAL ).
            <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-matkl ]-zclbnds ).
          ENDIF.

          IF ( <ls_mseg>-bwart = lc_bwart_241 AND <ls_mseg>-grund = lc_grund_0001 ) OR <ls_mseg>-bwart = lc_bwart_z41.


            LOOP AT lt_change_gr ASSIGNING FIELD-SYMBOL(<ls_change_gr>) WHERE bwart = <ls_mseg>-bwart AND
                                                                              grund = <ls_mseg>-grund.

              CHECK ( <ls_anla> IS ASSIGNED AND ( <ls_anla>-ktogr EQ <ls_change_gr>-ktogr OR
                                                  <ls_anla>-ktogr CP <ls_change_gr>-ktogr ) ) OR
                    <ls_change_gr>-ktogr IS INITIAL.

              <ls_posit>-tmcgroup = <ls_change_gr>-zgr.

              EXIT.
            ENDLOOP.


          ELSEIF <ls_mseg>-bwart = lc_bwart_241.


            LOOP AT lt_change_gr ASSIGNING <ls_change_gr> WHERE bwart  = <ls_mseg>-bwart AND
                                                                grund  = <ls_mseg>-grund AND
                                                                zmatkl = ls_mara-matkl.

              CHECK ( <ls_anla> IS ASSIGNED AND ( <ls_anla>-ktogr EQ <ls_change_gr>-ktogr     OR
                                                  <ls_anla>-ktogr CP <ls_change_gr>-ktogr ) ) OR
                    <ls_change_gr>-ktogr IS INITIAL.

              <ls_posit>-tmcgroup = <ls_change_gr>-zgr.

              EXIT.
            ENDLOOP.

          ELSEIF <ls_mseg>-bwart = lc_bwart_221.

            LOOP AT lt_change_gr ASSIGNING <ls_change_gr> WHERE bwart  = <ls_mseg>-bwart.

              <ls_posit>-tmcgroup = <ls_change_gr>-zgr.

              EXIT.
            ENDLOOP.

          ELSEIF <ls_mseg>-bwart = lc_bwart_261 AND <ls_mseg>-grund = lc_grund_0002.

            TRY.
                DATA(ls_anla_via) = lt_anla_viaufks[ aufnr = <ls_mseg>-aufnr ].
                DATA(lv_matkl) = |{ ls_anla_via-gdlgrp(5) }0{ ls_anla_via-gdlgrp+5(3) }|.
                DATA(ls_cl_gr) = lt_cl_gr[ zmatkl = lv_matkl ].
                <ls_posit>-tmcgroup = COND #( WHEN ls_cl_gr-zgr(5) = ls_anla_via-anlkl(5)
                                              THEN ls_cl_gr-zgr ELSE ls_cl_gr-zgros ).
              CATCH cx_sy_itab_line_not_found.
                CLEAR <ls_posit>-tmcgroup.
            ENDTRY.

          ELSEIF <ls_mseg>-bwart IN lt_rng_tmcgroup_bwart[] AND <ls_mseg>-grund IN lt_rng_tmcgroup_grund[].


            <ls_posit>-tmcgroup = COND #( WHEN <ls_anla> IS ASSIGNED THEN VALUE #( lt_003_cl_gr_1[ anlkl = <ls_anla>-anlkl ]-zgr OPTIONAL )
                                          WHEN <ls_sernum> IS ASSIGNED THEN <ls_sernum>-zgr ).

            <ls_posit>-oscardnum = COND #( WHEN <ls_posit>-oscardnum IS INITIAL AND <ls_sernum> IS ASSIGNED
                                           THEN <ls_sernum>-anln1 && <ls_sernum>-anln2
                                           ELSE <ls_posit>-oscardnum ).
          ELSE.

            <ls_posit>-tmcgroup = COND #( WHEN line_exists( lt_change_gr[ bwart = <ls_mseg>-bwart grund = <ls_mseg>-grund zmatkl = ls_mara-matkl ] )
                                          THEN lt_change_gr[ bwart = <ls_mseg>-bwart grund = <ls_mseg>-grund zmatkl = ls_mara-matkl ]-zgr
                                          ELSE VALUE #( lt_cl_gr[ zmatkl = ls_mara-matkl ]-zgr OPTIONAL ) ).
          ENDIF.

        ENDIF.
        "------------------------------


        IF <ls_mseg>-kzbew = 'L'      AND
           <ls_mseg>-kzzug IS INITIAL AND
           <ls_mseg>-kunnr IS NOT INITIAL.
          READ TABLE lt_vbap ASSIGNING <ls_vbap>
                             WITH KEY vbeln = <ls_mseg>-vbeln_im
                                      posnr = <ls_mseg>-vbelp_im BINARY SEARCH.
          IF sy-subrc = 0.

            CALL FUNCTION 'MRM_DBTAB_KONV_READ'
              EXPORTING
                i_knumve = <ls_vbap>-knumv_ana
                i_knumvl = ''
              TABLES
                t_konv   = lt_konv.

            <ls_posit>-amount    = <ls_vbap>-kzwi4.
            <ls_posit>-vatamount = <ls_vbap>-kzwi5.
            <ls_posit>-vatrate   = VALUE #( lt_konv[ kposn = <ls_mseg>-vbelp_im
                                                     kschl = 'MW01' ]-kbetr OPTIONAL ) / 10.
            <ls_posit>-vatcode   = VALUE #( lt_konv[ kposn = <ls_mseg>-vbelp_im
                                                     kschl = 'MW01' ]-mwsk1 OPTIONAL ).

            DATA(lv_vat_done) = abap_true.
          ENDIF.
        ENDIF.


        "------------------------------
        READ TABLE lt_rseg ASSIGNING FIELD-SYMBOL(<ls_rseg>)
                                         WITH KEY ebeln = <ls_mseg>-ebeln
                                                  ebelp = <ls_mseg>-ebelp BINARY SEARCH.
        IF sy-subrc EQ 0.

          IF <ls_rseg>-tbtkz EQ 'X'. "Значит VORGANG3 = 3 - Дополнительное дебетование
            <ls_posit>-amount    = <ls_rseg>-stock_posting.
            <ls_posit>-vatamount = <ls_rseg>-stock_posting  - <ls_rseg>-wrbtr.
            <ls_posit>-quantity  = <ls_rseg>-menge.

            <ls_mat_move>-materialdocnum    = ls_rbkp-belnr.
            <ls_mat_move>-materialdocyear   = ls_rbkp-gjahr.
            <ls_mat_move>-materialdocdate   = ls_rbkp-bldat.
            <ls_mat_move>-entrysupposeddate = ls_rbkp-budat.
            <ls_mat_move>-primarydocnum     = ls_rbkp-xblnr.
            <ls_mat_move>-agreementid       = ls_rbkp-zuonr.
            <ls_mat_move>-supplierclientid  = ls_rbkp-lifnr.
            <ls_mat_move>-materialdoccode   = COND #( WHEN line_exists( lt_viddvig[ vorgang = '3' ] )
                                                      THEN lt_viddvig[ vorgang = '3' ]-code_doc_abc
                                                      ELSE <ls_mat_move>-materialdoccode ).

          ELSEIF lv_vat_done <> abap_true.


            "-------------------------
            <ls_posit>-vatcode   = <ls_rseg>-mwskz.
            <ls_posit>-vatamount = COND #( WHEN line_exists( lt_ekpo[ ebeln = <ls_mseg>-ebeln
                                                                      ebelp = <ls_mseg>-ebelp ] )
                                           THEN lt_ekpo[ ebeln = <ls_mseg>-ebeln
                                                         ebelp = <ls_mseg>-ebelp ]-navnw
                                           ELSE <ls_posit>-vatamount ).

            lv_wrbtr             = <ls_rseg>-wrbtr.
            CLEAR lt_mwdat.

            CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
              EXPORTING
                i_bukrs = <ls_rseg>-bukrs
                i_mwskz = <ls_rseg>-mwskz
                i_waers = lc_waers
                i_wrbtr = lv_wrbtr
              TABLES
                t_mwdat = lt_mwdat
              EXCEPTIONS
                OTHERS  = 15.

            IF sy-subrc EQ 0 AND lt_mwdat IS NOT INITIAL.
              <ls_posit>-vatrate = lt_mwdat[ 1 ]-msatz.
            ELSE.
              CLEAR: <ls_posit>-vatrate,
                     <ls_posit>-vatamount.
            ENDIF.
            "-------------------------

          ENDIF.

        ENDIF.
        "------------------------------

        "------------------------------
        "COMMENT 14.06.2023 - по причине отказа от данного требования
*        " >> 01.06.2023 KVIZHINAD-VS
*        IF <ls_mat_move>-materialdoccode = 'ТМЦ_ДОНАЦЕНКА'
*        OR <ls_mat_move>-materialdoccode = 'SHB_СКЛАД_ПЕРЕД'.
*          SELECT SINGLE anlu~zztransfer, anlh~anlhtxt
*            FROM anlu LEFT JOIN anlh ON anlh~anln1 = anlu~anln1
*                                    AND anlh~bukrs = anlu~bukrs
*            WHERE anlu~anln1 = @lv_anla1
*              AND anlu~anln2 = @lv_anla2
*              AND anlu~bukrs = @<ls_mseg>-bukrs
*            INTO @DATA(ls_zztransfer).
*          IF ls_zztransfer-zztransfer IS NOT INITIAL.
*            <ls_posit>-oscardnum = ''.
*            <ls_posit>-mataccount = ls_zztransfer-anlhtxt.
*          ENDIF.
*
*          IF <ls_mat_move>-materialdoccode = 'SHB_СКЛАД_ПЕРЕД'
*          AND <ls_posit>-tmcgroup IN lr_tmcgroup.
*            CLEAR <ls_posit>-mataccount.
*          ENDIF.
*
*        ENDIF.
*        " << 01.06.2023 KVIZHINAD-VS

      ENDDO.

    ENDLOOP.



    "----------------------------------------------------------
    READ TABLE lt_mat_move INTO rs_data INDEX 1.
    "----------------------------------------------------------


    "Не отправлять документы, которы далее отправятся в тр.MIR7
    "----------------------------------------------------------
    IF lt_rng_code_not_send IS NOT INITIAL
       AND rs_data-materialdoccode IN lt_rng_code_not_send.
      IF iv_mblnr+0(2) = '50'.
      ENDIF.

*    ENDIF.
      CLEAR rs_data.
      RETURN.
    ENDIF.
    "----------------------------------------------------------

    "{baryutin-sa 22.08.23 зни 138 создание внутр заказа
    "{ Shanarov B.V. - 15.09.2023 16:17:01 - закомментировал для прода
*    IF ( sy-uname = 'BARYUTIN-SA' OR sy-uname = 'MURTAZINA-AT' ) AND 1 = 2 .

*          IF  ( <lv_action> = 'A01' AND <lv_refdoc> = 'R01' ).
    DATA: lv_count TYPE i,
          lv_index TYPE i.
    DATA: ls_ord_data     TYPE bapi2075_7,
          ls_ord_data_opt TYPE bapi2075_7b,
          lt_return       TYPE STANDARD TABLE OF bapiret2,
          ls_extensionin  TYPE bapiparex,
          lt_extensionin  TYPE STANDARD TABLE OF bapiparex,
          ls_te_aufk      TYPE bapi_te_aufk.
    FIELD-SYMBOLS: <ls_aufk> TYPE any.
    DATA: lv_aufnr TYPE aufnr.
    SELECT xmsegt~matnr, mara~matkl,ztmm_cl_gr~zmatkl ,ztmm_cl_gr~zmname, ztmm_cl_gr~zidauart,
           orderco~zzservis, orderco~zzservisrun, orderco~zzbov, xmsegt~menge,
           xmsegt~zeile
      FROM mseg AS xmsegt
      INNER JOIN mara ON mara~matnr = xmsegt~matnr
      INNER JOIN ztmm_cl_gr ON ztmm_cl_gr~zmatkl = mara~matkl
      LEFT JOIN ztmm_003_orderco AS orderco ON orderco~zidauart = ztmm_cl_gr~zidauart
      WHERE mblnr = @iv_mblnr
        AND mjahr = @iv_mjahr
      INTO TABLE @DATA(lt_xmseg).
    lv_index = 1.

    LOOP AT lt_xmseg ASSIGNING FIELD-SYMBOL(<ls_xmseg>).
      lv_count =  <ls_xmseg>-menge.
*              READ TABLE lt_selected WITH KEY matnr = <ls_xmseg>-matnr ASSIGNING FIELD-SYMBOL(<ls_selected>).
*              IF sy-subrc <> 0.
*                CONTINUE.
*              ENDIF.
*            SELECT SINGLE *
*              FROM ztmm_003_orderco WHERE zidauart = @<ls_selected>-zidauart
*              INTO @DATA(ls_orderco).

      lv_index = 0.

      SELECT objk~obknr, objk~equnr, sernr, zeile
        INTO TABLE @DATA(lt_sernrs)
        FROM ser03
        JOIN objk ON objk~obknr = ser03~obknr
        WHERE ser03~mblnr = @iv_mblnr
          AND ser03~mjahr = @iv_mjahr
          AND ser03~zeile = @<ls_xmseg>-zeile.

      DO lv_count TIMES.

        CLEAR: ls_ord_data, ls_ord_data_opt, ls_te_aufk, ls_extensionin, lt_extensionin[].

        lv_index = +1.
        " Стандартные поля внутреннего заказа
        ls_ord_data-order_type   = <ls_xmseg>-zidauart.          "Вид заказа
        ls_ord_data-order_name   = <ls_xmseg>-zmname. "Наименование заказа (полное название материала) 40 символов
        ls_ord_data-co_area      = '1000'.          "КЕ
        ls_ord_data-comp_code    = '1000'.          "БЕ
        ls_ord_data-profit_ctr   = '0000000100'.    "МВП 10 символов
        ls_ord_data-currency     = 'RUB'.           "Валюта

        " Стандартные поля внутреннего заказа (опциональные)
        ls_ord_data_opt-planintegrated       = 'X'. "Индикатор 'Интеграция планирования'

        " Пользовательские поля внутреннего заказа
        ls_extensionin-structure = 'BAPI_TE_AUFK'.
        ls_te_aufk-zzservis      = <ls_xmseg>-zzservis."'10103'."'11203'. "Сервис
        ls_te_aufk-zzservisrun   = <ls_xmseg>-zzservisrun."'101'."'112'.   "Сервисное направление
        READ TABLE lt_sernrs ASSIGNING FIELD-SYMBOL(<ls_sernr>) INDEX lv_index.
        IF sy-subrc = 0.
          ls_te_aufk-zzsernr       = <ls_sernr>-sernr."'01687'. "Системный серийный номер
        ENDIF.
        ls_te_aufk-zzbov         = <ls_xmseg>-zzbov."'ZF20'.  "Вид бизнес-операции СО

        APPEND INITIAL LINE TO lt_extensionin ASSIGNING FIELD-SYMBOL(<ls_ext>).
        <ls_ext>-structure = 'BAPI_TE_AUFK'.
        ASSIGN <ls_ext>-valuepart1 TO <ls_aufk> CASTING TYPE bapi_te_aufk.
*            ASSIGN ls_extensionin-valuepart1 TO <te_aufk> CASTING TYPE bapi_te_aufk.
        <ls_aufk> = ls_te_aufk.
*            APPEND ls_extensionin TO lt_extensionin.

        CALL FUNCTION 'BAPI_INTERNALORDER_CREATE'
          EXPORTING
            i_master_data = ls_ord_data
            testrun       = ''
            i_master_datb = ls_ord_data_opt
          IMPORTING
            orderid       = lv_aufnr
          TABLES
            return        = lt_return
            extensionin   = lt_extensionin.

        IF lv_aufnr IS NOT INITIAL. " Номер заказа
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*              MESSAGE e001(ko) WITH 'Ошибка при создании СО-заказа'.
        ENDIF.

      ENDDO.

    ENDLOOP.

*          ENDIF.

*    ENDIF.
    "} Shanarov B.V. - 15.09.2023 16:17:01
    "}


    "Очищаем супллаера
    "----------------------------------------------------------
    IF rs_data-cftagreementid IS NOT INITIAL AND
       rs_data-agreementid    IS NOT INITIAL.
      CLEAR rs_data-supplierclientid.
    ENDIF.
    "----------------------------------------------------------



    "Проверка, отправлялся ли документ уже
    "-------------------------------------
    SELECT SINGLE @abap_true
      FROM ztint_pt0111
      WHERE material_doc_num  = @rs_data-materialdocnum
        AND material_doc_year = @rs_data-materialdocyear
        AND process_status    = @zif_ifc_const=>mc_process_status-success
      INTO @DATA(lv_send).

    IF sy-subrc = 0.
      CLEAR rs_data.
    ENDIF.
    "-------------------------------------


    "Дефект EXT_ITECO-274
    "-------------------------------------
    LOOP AT rs_data-position ASSIGNING FIELD-SYMBOL(<ls_def274>)
         WHERE mataccount IS NOT INITIAL.

*      IF rs_data-materialdoccode = 'SHB_ФИЛ_NEW'.
*        CLEAR: <ls_def274>-batchsuppliernum.
*      ELSE.
*        CLEAR: <ls_def274>-batchreceivernum, <ls_def274>-batchsuppliernum.
*      ENDIF.
      CLEAR: <ls_def274>-batchsuppliernum.
    ENDLOOP.
    "-------------------------------------

    "Группируем позиции, если для них указано в настройке
    "----------------------------------------------------------
    SELECT bwart, grund, group_flag, group_extra_quantity_flag
      FROM ztint_mm_group
      INTO TABLE @DATA(lt_mm_group).

    DATA(lt_pos_group) = rs_data-position.
    CLEAR rs_data-position.

    LOOP AT lt_pos_group ASSIGNING FIELD-SYMBOL(<ls_pos_group>)
                         GROUP BY ( werks  = <ls_pos_group>-werks
                                    lgort  = <ls_pos_group>-lgort
                                    charge = <ls_pos_group>-charge
                                    tmcnum = <ls_pos_group>-tmcnum )
                         INTO DATA(lo_group).

      LOOP AT GROUP lo_group ASSIGNING FIELD-SYMBOL(<ls_group>).
        READ TABLE lt_mm_group
          INTO DATA(ls_mm_group)
          WITH KEY bwart = <ls_group>-bwart
                   grund = <ls_group>-grund.

        IF ls_mm_group-group_flag = abap_false AND ls_mm_group-group_extra_quantity_flag = abap_false.
          APPEND INITIAL LINE TO rs_data-position ASSIGNING <ls_posit>.

          <ls_posit> = <ls_group>.
        ELSE.
          UNASSIGN <ls_posit>.

          READ TABLE rs_data-position
            ASSIGNING <ls_posit>
            WITH KEY werks   = lo_group-werks
                     lgort   = lo_group-lgort
                     charge  = lo_group-charge
                     tmcnum  = lo_group-tmcnum
                     grouped = abap_true.

          IF <ls_posit> IS NOT ASSIGNED OR <ls_posit> IS INITIAL.
            APPEND INITIAL LINE TO rs_data-position ASSIGNING <ls_posit>.

            <ls_posit> = <ls_group>.

            CLEAR <ls_posit>-quantity.

            IF ls_mm_group-group_flag = abap_true.
              <ls_posit>-quantity = <ls_group>-quantity.
            ELSEIF ls_mm_group-group_extra_quantity_flag = abap_true.
              <ls_posit>-extraquantity = <ls_group>-quantity.
            ENDIF.

            <ls_posit>-grouped = abap_true.
          ELSE.
            IF ls_mm_group-group_flag = abap_true.
              ADD <ls_group>-quantity TO <ls_posit>-quantity.
            ELSEIF ls_mm_group-group_extra_quantity_flag = abap_true.
              ADD <ls_group>-quantity TO <ls_posit>-extraquantity.
            ENDIF.
            ADD <ls_group>-amount   TO <ls_posit>-amount.
            APPEND LINES OF <ls_group>-serge TO <ls_posit>-serge.
*            IF <ls_group>-factorynum IS NOT INITIAL.
*              <ls_posit>-factorynum = |{ <ls_posit>-factorynum },{ <ls_group>-factorynum }|.
*              SHIFT <ls_posit>-factorynum LEFT DELETING LEADING ','.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CLEAR ls_mm_group.
    ENDLOOP.
    "----------------------------------------------------------


    "{ baryutin-sa 16.11.2023 ЗНИ 317 замена тега molreceiver на введенный
    READ TABLE lt_migo_zf WITH KEY vgart = 'WA' ASSIGNING FIELD-SYMBOL(<ls_migo_zf>).
    IF sy-subrc = 0.
      IF <ls_migo_zf>-bwart = lc_bwart_261 AND ( <ls_migo_zf>-grund = lc_grund_0003 OR <ls_migo_zf>-grund = lc_grund_0004 ).
*        rs_data-molreceivertabnum = lv_molrec_trans.
        rs_data-molreceivertabnum = <ls_migo_zf>-pernr.
      ELSEIF <ls_migo_zf>-bwart = lc_bwart_311 OR <ls_migo_zf>-bwart = lc_bwart_301.
        SELECT SINGLE name3                            "#EC CI_BUFFJOIN
        INTO @<ls_mat_move>-molreceivertabnum
        FROM adrc AS a
        INNER JOIN twlad AS t
          ON t~adrnr = a~addrnumber
        WHERE t~lgort = @<ls_migo_zf>-umlgo
          AND t~werks = @<ls_migo_zf>-umwrk ##WARN_OK.
      ELSEIF <ls_migo_zf>-zzmolreceiver IS NOT INITIAL.
        rs_data-molreceivertabnum = <ls_migo_zf>-zzmolreceiver.
      ELSE.
        rs_data-molreceivertabnum = <ls_mat_move>-molsuppliertabnum.
      ENDIF.
    ENDIF.
    "} baryutin-sa 16.11.2023 ЗНИ 317



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_MIR7_770
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        MBLNR(optional)
* | [--->] IV_GJAHR                       TYPE        MJAHR(optional)
* | [<-()] RS_DATA                        TYPE        ZST_0003_SEND_MATERIAL_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mir7_770.

    CONSTANTS: lv_waers TYPE waers VALUE 'RUB'.

    DATA: lv_zmm003_zgr            TYPE char20,
          lt_rng_tmcgroup_bwart    TYPE RANGE OF bwart,
          lt_rng_tmcgroup_grund    TYPE RANGE OF mb_grbew,
          lv_txt_name              TYPE thead-tdname,
          lt_rng_rbstat            TYPE RANGE OF rbstat,
          lv_wrbtr                 TYPE bseg-wrbtr,
          lv_atinn_matsh           TYPE atinn,
          lt_mat_move              TYPE ztt_0003_send_material_doc,
          lt_rng_anln1             TYPE RANGE OF anln1,
          ls_rng_anln1             LIKE LINE OF lt_rng_anln1,
          lt_rng_anln2             TYPE RANGE OF anln2,
          ls_rng_anln2             LIKE LINE OF lt_rng_anln2,
          lv_objek                 TYPE cuobn,
          lt_mwdat                 TYPE cte_t_mwdat,
          lv_anla1                 TYPE anln1,
          lv_anla2                 TYPE anln2,
          lt_long_text             TYPE TABLE OF tline,
          lt_rng_zmm003_zgr        TYPE RANGE OF char20,
          lt_rng_not_clear_suppl   TYPE RANGE OF zint_mm_viddvig-code_doc_abc,
*          lt_serge               TYPE tt_serge,
          lt_rng_mtart_nma         TYPE RANGE OF mara-mtart,
          lt_rng_matdoc            TYPE RANGE OF ztint_pt0111-material_doc_num,
          lt_rng_madyear           TYPE RANGE OF ztint_pt0111-material_doc_year,

          lv_amount_sum            TYPE salk3,
          lv_amount_diff           TYPE salk3,

          lr_anbwa_770             TYPE RANGE OF anbwa,
          lr_gkont                 TYPE RANGE OF hkont,

          lv_donacenka_name_filled TYPE abap_bool.
*
***    DATA:
***      lt_locationid TYPE tt_locationid,
***      lv_loc_active TYPE abap_bool.

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_MTART_NMA' IMPORTING er_range = lt_rng_mtart_nma ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name  = 'ZFIAAINT0013_ANBWA_980' IMPORTING er_range = lr_anbwa_770 ).

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_RBSTAT'               IMPORTING er_range = lt_rng_rbstat ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_ZGR'                  IMPORTING er_range = lt_rng_zmm003_zgr ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_TMCGROUP_BWART'       IMPORTING er_range = lt_rng_tmcgroup_bwart ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_TMCGROUP_GRUND'       IMPORTING er_range = lt_rng_tmcgroup_grund ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_NOT_CLEAR_SUPPL' IMPORTING er_range = lt_rng_not_clear_suppl  ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_MTART_NMA'       IMPORTING er_range = lt_rng_mtart_nma  ).
    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_ATINN_MATSH'          IMPORTING ev_value = lv_atinn_matsh  ).
    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_ZGR'                  IMPORTING ev_value = lv_zmm003_zgr  ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZFIAAINT0013_GKONT_INITIAL' IMPORTING er_range = lr_gkont ).
***    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_LOCATION' IMPORTING ev_value = lv_loc_active ).

    IF lt_rng_zmm003_zgr  IS NOT INITIAL.
      lv_zmm003_zgr  = lt_rng_zmm003_zgr[ 1 ]-low.
    ENDIF.


    SELECT SINGLE belnr, gjahr, rbstat, bldat,
                  budat, xblnr, zuonr, lifnr, rmwwr
      FROM rbkp
      INTO @DATA(ls_rbkp)
      WHERE belnr = @iv_belnr
      AND gjahr  = @iv_gjahr
      AND rbstat IN @lt_rng_rbstat. " and "STBLG ne space. "исключить пары прямой-сторно.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    SELECT SINGLE stblg, stjah
      FROM rbkp
      INTO @DATA(ls_rbkp_s)
      WHERE stblg = @iv_belnr
        AND stjah  = @iv_gjahr ##NEEDED ##WARN_OK.

    IF sy-subrc EQ 0.
      MESSAGE i002(zmmint0003_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.


    SELECT  belnr, gjahr, ebeln, ebelp,
            tbtkz, stock_posting, wrbtr,
            menge, mwskz, bukrs, shkzg,
            lfbnr, lfgja, lfpos
      FROM rseg
      INTO TABLE @DATA(lt_rseg)
      WHERE belnr = @iv_belnr
        AND gjahr  = @iv_gjahr ##WARN_OK.

    IF sy-subrc NE 0.
      MESSAGE i005(zmmint0003_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(ls_rseg1) = lt_rseg[ 1 ].

    DATA(lv_ebeln) = ls_rseg1-ebeln.

    SELECT DISTINCT mblnr, mjahr, zeile
      FROM mseg
        FOR ALL ENTRIES IN @lt_rseg
      WHERE mblnr = @lt_rseg-lfbnr
        AND mjahr = @lt_rseg-lfgja
        AND zeile = @lt_rseg-lfpos
        AND smbln EQ @space ##WARN_OK
      INTO TABLE @DATA(lt_mseg_mblnr).

    SELECT DISTINCT mblnr, mjahr, zeile
      FROM mseg
        FOR ALL ENTRIES IN @lt_rseg
      WHERE mblnr = @lt_rseg-lfbnr
        AND mjahr = @lt_rseg-lfgja
        AND smbln EQ @space ##WARN_OK
      INTO TABLE @DATA(lt_mseg_mblnr_all).

    IF sy-subrc <> 0 OR lines( lt_mseg_mblnr ) <> lines( lt_rseg ).

*      SELECT DISTINCT mblnr, mjahr, zeile
*        FROM mseg
*        WHERE ebeln = @lv_ebeln
*          AND smbln = @space ##WARN_OK
*        INTO TABLE @lt_mseg_mblnr.

      SELECT DISTINCT mblnr, mjahr, zeile
        FROM mseg
        FOR ALL ENTRIES IN @lt_rseg
        WHERE ebeln = @lt_rseg-ebeln
          AND ebelp = @lt_rseg-ebelp
          AND smbln = @space ##WARN_OK
        INTO TABLE @lt_mseg_mblnr.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      SELECT DISTINCT mblnr, mjahr, zeile
        FROM mseg
        FOR ALL ENTRIES IN @lt_rseg
        WHERE ebeln = @lt_rseg-ebeln
          AND smbln = @space ##WARN_OK
        INTO TABLE @lt_mseg_mblnr_all.
    ENDIF.

    "---------------------------------------------------
    IF ls_rseg1-tbtkz = abap_true.
      "Донаценка

*      DATA(ls_mseg_mblnr) = lt_mseg_mblnr[ 1 ]. 18.01.2023


**********************************************************************
      SELECT smbln, sjahr, smblp
        FROM mseg
        INTO TABLE @DATA(lt_mseg_strn_donac)
        FOR ALL ENTRIES IN @lt_mseg_mblnr
        WHERE smbln  = @lt_mseg_mblnr-mblnr
          AND sjahr  = @lt_mseg_mblnr-mjahr.

      IF sy-subrc = 0.
        LOOP AT lt_mseg_strn_donac ASSIGNING FIELD-SYMBOL(<ls_mseg_strn_donac>).
          READ TABLE lt_mseg_mblnr ASSIGNING FIELD-SYMBOL(<ls_mseg_mb>)
                                    WITH KEY mblnr  = <ls_mseg_strn_donac>-smbln
                                             mjahr  = <ls_mseg_strn_donac>-sjahr
                                             zeile  = <ls_mseg_strn_donac>-smblp.
          IF sy-subrc EQ 0.
            "Значит док. стор.
            DELETE lt_mseg_mblnr WHERE mblnr = <ls_mseg_mb>-mblnr
                            AND  mjahr = <ls_mseg_mb>-mjahr
                            AND  zeile = <ls_mseg_mb>-zeile .
          ENDIF.
        ENDLOOP.

        IF lt_mseg_mblnr[] IS NOT INITIAL.
          DATA(ls_mseg_mblnr) = lt_mseg_mblnr[ 1 ].
        ENDIF.
      ELSE.
        ls_mseg_mblnr = lt_mseg_mblnr[ 1 ].
      ENDIF.
**********************************************************************

    ELSE.

      lt_rng_matdoc  = VALUE #( FOR ls_wa IN lt_mseg_mblnr sign = 'I' option = 'EQ' ( low = ls_wa-mblnr ) ).
      lt_rng_madyear = VALUE #( FOR ls_wa IN lt_mseg_mblnr sign = 'I' option = 'EQ' ( low = ls_wa-mjahr ) ).

    ENDIF.
    "---------------------------------------------------

*    SELECT  ebeln, smbln, mblnr, mjahr,
*            zeile, anln1, anln2, sgtxt,
*            bukrs, xauto, lgort, werks,
*            lifnr, bwart, parent_id, ebelp,
*            line_id, matnr, dmbtr, grund,
*            charg, bprme, erfme, menge
*      FROM mseg
*      INTO TABLE @DATA(lt_mseg)
*      WHERE mblnr = @ls_mseg_mblnr-mblnr
*        AND mjahr = @ls_mseg_mblnr-mjahr
**      WHERE ebeln = @lv_ebeln
*        AND smbln EQ @space ##WARN_OK.

    SELECT  ebeln, smbln, mblnr, mjahr,
            zeile, anln1, anln2, sgtxt,
            bukrs, xauto, lgort, werks,
            lifnr, bwart, parent_id, ebelp,
            line_id, matnr, dmbtr, grund,
            charg, bprme, erfme, menge
      FROM mseg
      INTO TABLE @DATA(lt_mseg)
      FOR ALL ENTRIES IN @lt_mseg_mblnr
      WHERE mblnr = @lt_mseg_mblnr-mblnr
        AND mjahr = @lt_mseg_mblnr-mjahr
        AND zeile = @lt_mseg_mblnr-zeile
*      WHERE ebeln = @lv_ebeln
        AND smbln EQ @space ##WARN_OK.

    "{ baryutin-sa ТМЦ_ДОНАЦЕНКА

    SELECT mseg~budat_mkpf, mseg~cputm_mkpf,mseg~mblnr,mseg~mjahr, mseg~zeile,
      mseg~xauto, mseg~matnr, mseg~werks, mseg~lgort, mseg~charg, mseg~sjahr, mseg~smbln,mseg~smblp
      FROM @lt_mseg AS loc
      INNER JOIN mseg ON loc~charg = mseg~charg
      WHERE mseg~xauto = @abap_true
        AND mseg~smbln IS INITIAL
      INTO TABLE @DATA(lt_moved_charg).

    SELECT mseg~budat_mkpf, mseg~cputm_mkpf,mseg~mblnr,mseg~mjahr, mseg~zeile,
      mseg~xauto, mseg~matnr, mseg~werks, mseg~lgort, mseg~charg, mseg~sjahr, mseg~smbln,mseg~smblp
      FROM @lt_mseg AS loc
      INNER JOIN mseg ON loc~charg = mseg~charg
      WHERE mseg~xauto = @abap_true
        AND mseg~smbln IS NOT INITIAL
      INTO TABLE @DATA(lt_moved_charg_storn).

    LOOP AT lt_moved_charg_storn ASSIGNING FIELD-SYMBOL(<ls_moved_charg_storn>).
      READ TABLE lt_moved_charg ASSIGNING FIELD-SYMBOL(<ls_moved_charg>)
                                           WITH KEY mblnr = <ls_moved_charg_storn>-smbln
                                                    mjahr = <ls_moved_charg_storn>-sjahr
                                                    zeile = <ls_moved_charg_storn>-smblp.
      IF sy-subrc EQ 0.
        "Значит док. стор.
        DELETE lt_moved_charg WHERE mblnr = <ls_moved_charg>-mblnr
                         AND mjahr = <ls_moved_charg>-mjahr
                         AND zeile = <ls_moved_charg>-zeile.
      ENDIF.

    ENDLOOP.
    "получаем названия
    SELECT loc~budat_mkpf, loc~cputm_mkpf,loc~lgort, loc~werks, loc~charg, a~name3
                          FROM @lt_moved_charg AS loc
                          JOIN twlad AS t ON loc~lgort = t~lgort
                                          AND loc~werks = t~werks
                          JOIN adrc AS a ON t~adrnr = a~addrnumber
      INTO TABLE @DATA(lt_moved_lgort_names).
    "сортируем по дате
    SORT lt_moved_lgort_names BY budat_mkpf cputm_mkpf DESCENDING.


    "} baryutin-sa

    IF lt_mseg IS NOT INITIAL.

      SELECT smbln, sjahr, smblp FROM mseg         "#EC CI_NO_TRANSFORM
        INTO TABLE @DATA(lt_mseg_storn)
          FOR ALL ENTRIES IN @lt_mseg
        WHERE smbln = @lt_mseg-mblnr
          AND sjahr  = @lt_mseg-mjahr ##WARN_OK.

      IF sy-subrc EQ 0.

        LOOP AT lt_mseg_storn ASSIGNING FIELD-SYMBOL(<ls_mseg_storn>).
          READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<ls_mseg>)
                                           WITH KEY mblnr = <ls_mseg_storn>-smbln
                                                    mjahr = <ls_mseg_storn>-sjahr
                                                    zeile = <ls_mseg_storn>-smblp.
          IF sy-subrc EQ 0.
            "Значит док. стор.
            DELETE lt_mseg WHERE mblnr = <ls_mseg>-mblnr
                             AND mjahr = <ls_mseg>-mjahr
                             AND zeile = <ls_mseg>-zeile.
          ENDIF.

          READ TABLE lt_mseg_mblnr_all ASSIGNING FIELD-SYMBOL(<ls_mseg_mblnr_all>)
                                           WITH KEY mblnr = <ls_mseg_storn>-smbln
                                                    mjahr = <ls_mseg_storn>-sjahr
                                                    zeile = <ls_mseg_storn>-smblp.
          IF sy-subrc EQ 0.
            "Значит док. стор.
            DELETE lt_mseg_mblnr_all WHERE mblnr = <ls_mseg_mblnr_all>-mblnr
                                       AND mjahr = <ls_mseg_mblnr_all>-mjahr
                                       AND zeile = <ls_mseg_mblnr_all>-zeile.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lt_rng_matdoc IS NOT INITIAL AND lt_rng_madyear IS NOT INITIAL.
        SELECT material_doc_num, material_doc_year
          FROM ztint_pt0111
           WHERE material_doc_num    IN @lt_rng_matdoc
             AND material_doc_year   IN @lt_rng_madyear
             AND process_status      = @zif_ifc_const=>mc_process_status-success
          INTO TABLE @DATA(lt_sent).
      ENDIF.


      IF lines( lt_rseg ) = lines( lt_mseg_mblnr_all ).
        LOOP AT lt_mseg_mblnr ASSIGNING FIELD-SYMBOL(<ls_mseg_mblnr>).
          CHECK line_exists( lt_sent[ material_doc_num  = <ls_mseg_mblnr>-mblnr
                                      material_doc_year = <ls_mseg_mblnr>-mjahr ] ).

          DELETE lt_mseg_mblnr.
        ENDLOOP.
      ENDIF.

      IF lt_mseg_mblnr IS INITIAL.
        RETURN.
      ELSE.
        ls_mseg_mblnr = lt_mseg_mblnr[ 1 ].
      ENDIF.

    ELSE.
      MESSAGE i001(zmmint0003_msg) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_mblnr) = lt_mseg[ 1 ]-mblnr.
    DATA(lv_mjahr) = lt_mseg[ 1 ]-mjahr.

    SELECT SINGLE mblnr, mjahr, zzext_key, bldat,
                  budat, xblnr, bktxt, usnam
       FROM mkpf INTO  @DATA(ls_mkpf)
       WHERE mblnr = @lv_mblnr
       AND mjahr = @lv_mjahr.


    IF lt_mseg IS NOT INITIAL.

      LOOP AT lt_mseg ASSIGNING <ls_mseg>.
        ls_rng_anln1 = 'IEQ'.
        ls_rng_anln2 = 'IEQ'.

        IF <ls_mseg>-anln1 IS NOT INITIAL.
          ls_rng_anln1-low =  <ls_mseg>-anln1.
          COLLECT ls_rng_anln1 INTO lt_rng_anln1.
          ls_rng_anln2-low =  <ls_mseg>-anln2.
          COLLECT ls_rng_anln2 INTO lt_rng_anln2.

        ELSEIF  <ls_mseg>-sgtxt  IS NOT INITIAL.
          ls_rng_anln1-low =  <ls_mseg>-sgtxt(12).
          COLLECT ls_rng_anln1 INTO lt_rng_anln1.
          ls_rng_anln2-low =  <ls_mseg>-sgtxt+12(4).
          COLLECT ls_rng_anln2 INTO lt_rng_anln2.
        ENDIF.

        IF <ls_mseg>-sgtxt  IS NOT INITIAL.
        ENDIF.

      ENDLOOP.

      SELECT m~bukrs, m~anln1, m~anln2, m~invnr, z~pernr, m~anlkl, z~kostl, u~zztransfer
          FROM anla AS m
          INNER JOIN anlz AS z ON z~bukrs = m~bukrs
                              AND z~anln1 = m~anln1
                              AND z~anln2 = m~anln2
          INNER JOIN anlu AS u ON u~bukrs = m~bukrs
                              AND u~anln1 = m~anln1
                              AND u~anln2 = m~anln2
                 INTO TABLE @DATA(lt_anla)
                 FOR ALL ENTRIES IN @lt_mseg
                 WHERE m~bukrs = @lt_mseg-bukrs
                   AND m~anln1  IN @lt_rng_anln1[]
                   AND m~anln2  IN @lt_rng_anln2[]
                   AND z~bdatu  >= @sy-datum
                   AND z~adatu  <= @sy-datum.

    ENDIF.
*    SELECT bwart, grund, vorgang, bsart, code_doc_abc FROM zint_mm_viddvig WHERE flow = 'ПТ0111' OR flow IS INITIAL INTO TABLE @DATA(lt_viddvig). "#EC CI_NOWHERE
    SELECT bwart, grund, vorgang, bsart, code_doc_abc FROM zint_mm_viddvig INTO TABLE @DATA(lt_viddvig). "#EC CI_NOWHERE
    "закоменчено для переноса в прод 21,08,23 upd: раскоменчено
    SELECT zclbnds, zmatkl, zcl, zgr FROM ztmm_cl_gr INTO TABLE @DATA(lt_cl_gr). "#EC CI_NOWHERE
*    SELECT zmatkl, zcl, zgr FROM ztmm_cl_gr INTO TABLE @DATA(lt_cl_gr). "#EC CI_NOWHERE
    SELECT msehi, mm_okei FROM zint_mm_okei INTO TABLE @DATA(lt_mm_okei) BYPASSING BUFFER. "#EC CI_NOWHERE
    SELECT anlkl, zgr FROM ztmm_003_cl_gr_1 INTO TABLE @DATA(lt_003_cl_gr_1). "#EC CI_NOWHERE


    SELECT ebeln, ebelp, anln1
      FROM ekkn
        FOR ALL ENTRIES IN @lt_mseg
          WHERE ebeln = @lt_mseg-ebeln
            AND ebelp = @lt_mseg-ebelp
      INTO TABLE @DATA(lt_ekkn).

***    IF lv_loc_active = abap_true.
***      lt_locationid = CORRESPONDING #( lt_mseg ).
***      _get_locationid( CHANGING ct_locationid = lt_locationid ).
***    ENDIF.

    APPEND INITIAL LINE TO lt_mat_move ASSIGNING FIELD-SYMBOL(<ls_mat_move>).


    <ls_mat_move>-materialdocnum      = ls_mkpf-mblnr.
    <ls_mat_move>-materialdocyear     = ls_mkpf-mjahr.
    <ls_mat_move>-materialdocnum_from = iv_belnr.


    SELECT SINGLE accnt FROM usr02 WHERE bname = @ls_mkpf-usnam INTO @<ls_mat_move>-userlogin.

    <ls_mat_move>-userlogin = CONV persno( |{ <ls_mat_move>-userlogin ALPHA = IN }| ). " Eremetov 02.03.2023 14:33:45



    SELECT SINGLE m~abs_num
      FROM zrcmt_dgv_attr AS m
        INNER JOIN scmg_t_case_attr AS s
          ON s~case_guid = m~case_guid
      INTO <ls_mat_move>-cftagreementid
      WHERE s~ext_key = ls_mkpf-zzext_key ##WARN_OK.

    <ls_mat_move>-agreementid = ls_mkpf-zzext_key  .

    <ls_mat_move>-materialdocdate = ls_mkpf-bldat.
    <ls_mat_move>-entrysupposeddate = ls_mkpf-budat.
    <ls_mat_move>-primarydocnum = ls_mkpf-xblnr.


    READ TABLE  lt_mseg ASSIGNING <ls_mseg> WITH KEY mblnr = ls_mkpf-mblnr
                                                     mjahr = ls_mkpf-mjahr
                                                     xauto = space.
    IF sy-subrc EQ 0.
      "ATC 04.07.2022
      SELECT SINGLE name3                              "#EC CI_BUFFJOIN
        INTO @DATA(lv_name3)
        FROM adrc AS a
        JOIN twlad AS t
          ON t~adrnr = a~addrnumber
        WHERE t~lgort = @<ls_mseg>-lgort
          AND t~werks = @<ls_mseg>-werks ##WARN_OK.

      IF sy-subrc EQ 0.
        <ls_mat_move>-molsuppliertabnum = lv_name3.
      ENDIF.

      <ls_mat_move>-werksupplier      = `0` && <ls_mseg>-werks(2) && `-` && `0` && <ls_mseg>-werks+2(2).

      <ls_mat_move>-ordernum          = <ls_mseg>-ebeln.

      SELECT SINGLE bsart INTO @DATA(lv_bsart) FROM ekpo JOIN ekko ON ekpo~ebeln = ekko~ebeln
             WHERE ekpo~ebeln = @<ls_mseg>-ebeln AND ekpo~ebelp = @<ls_mseg>-ebelp AND ekpo~loekz = @abap_false.

      READ TABLE lt_viddvig ASSIGNING FIELD-SYMBOL(<ls_viddvig>) WITH KEY bwart = <ls_mseg>-bwart
                                                                          grund = <ls_mseg>-grund
                                                                          bsart = lv_bsart.
      IF sy-subrc EQ 0.
        <ls_mat_move>-materialdoccode = <ls_viddvig>-code_doc_abc.
      ELSE.
        READ TABLE lt_viddvig ASSIGNING <ls_viddvig> WITH KEY bwart = <ls_mseg>-bwart
                                                              grund = <ls_mseg>-grund.
        IF sy-subrc EQ 0.
          <ls_mat_move>-materialdoccode = <ls_viddvig>-code_doc_abc.
        ENDIF.
      ENDIF.

      IF <ls_mat_move>-materialdoccode IN lt_rng_not_clear_suppl.
        SELECT SINGLE idnumber FROM but0id WHERE partner = @<ls_mseg>-lifnr AND type = 'HCM001' INTO @<ls_mat_move>-supplierclientid.
      ENDIF.
*      <ls_mat_move>-supplierclientid  = <ls_mseg>-lifnr.

    ENDIF.


    READ TABLE  lt_mseg ASSIGNING <ls_mseg> WITH KEY mblnr = ls_mkpf-mblnr
                                                     mjahr = ls_mkpf-mjahr
                                                     xauto = 'X'.
    IF sy-subrc EQ 0.

*     АТС (SINGLE -> UP TO 1 ROWS)
      "ATC 04.07.2022
      SELECT name3 UP TO 1 ROWS INTO @lv_name3         "#EC CI_BUFFJOIN
                          FROM adrc AS a                "#EC CI_NOORDER
                          JOIN twlad AS t ON t~adrnr = a~addrnumber
                           WHERE t~lgort = @<ls_mseg>-lgort
                             AND t~werks = @<ls_mseg>-werks.
      ENDSELECT.

      IF sy-subrc EQ 0.
        <ls_mat_move>-molreceivertabnum  = lv_name3.
      ENDIF.

      <ls_mat_move>-werkreceiver       = `0` && <ls_mseg>-werks(2) && `-` && `0` && <ls_mseg>-werks+2(2).

      IF   <ls_mat_move>-ordernum IS INITIAL.
        <ls_mat_move>-ordernum = <ls_mseg>-ebeln.
      ENDIF.

      READ TABLE lt_viddvig ASSIGNING <ls_viddvig>
                             WITH KEY bwart =  <ls_mseg>-bwart
                                      grund = <ls_mseg>-grund.
      IF sy-subrc EQ 0 AND <ls_mat_move>-materialdoccode  IS INITIAL.
        <ls_mat_move>-materialdoccode = <ls_viddvig>-code_doc_abc.
      ENDIF.


      IF <ls_mat_move>-supplierclientid IS INITIAL.
        IF <ls_mat_move>-materialdoccode IN lt_rng_not_clear_suppl.
          SELECT SINGLE idnumber FROM but0id WHERE partner = @<ls_mseg>-lifnr AND type = 'HCM001' INTO @<ls_mat_move>-supplierclientid.
        ENDIF.
*        <ls_mat_move>-supplierclientid = <ls_mseg>-lifnr.
      ENDIF.
    ENDIF.



    <ls_mat_move>-textheader = ls_mkpf-bktxt.


    IF <ls_mat_move>-ordernum IS NOT INITIAL AND
         ls_mkpf-zzext_key      IS INITIAL.

*     ATC (SINGLE -> UP TO 1 ROWS)
      "ATC 04.07.2022
      SELECT m~abs_num , e~zzext_key UP TO 1 ROWS       "#EC CI_NOORDER
        FROM zrcmt_dgv_attr AS m
        INNER JOIN scmg_t_case_attr AS s ON s~case_guid = m~case_guid
        INNER JOIN ekko AS e ON s~ext_key = e~zzext_key AND e~ebeln = @<ls_mat_move>-ordernum
         INTO @DATA(ls_ext_key).
      ENDSELECT.

      IF sy-subrc = 0.

        <ls_mat_move>-cftagreementid = ls_ext_key-abs_num.
        <ls_mat_move>-agreementid = ls_ext_key-zzext_key  .

      ENDIF.
    ENDIF.



*    IF <ls_mat_move>-cftagreementid IS NOT INITIAL.
*      CLEAR <ls_mat_move>-supplierclientid.
*    ENDIF.

    FIELD-SYMBOLS: <ls_posit> LIKE LINE OF  <ls_mat_move>-position.

    SORT lt_mseg ASCENDING BY zeile.

    lv_donacenka_name_filled = space.

    LOOP AT lt_mseg ASSIGNING <ls_mseg> WHERE parent_id IS INITIAL.

      IF line_exists( lt_rseg[ shkzg = 'S' tbtkz = abap_true ] ) AND
         NOT line_exists( lt_rseg[ ebeln = <ls_mseg>-ebeln ebelp = <ls_mseg>-ebelp ] ).
        "Для ТМЦ_ДОНАЦЕНКА
        "Нужно выполнить проверку на позицию заказа = позиции предварительной счет фактуры
        CONTINUE.
      ENDIF.

*      CLEAR lt_serge.


      APPEND INITIAL LINE TO <ls_mat_move>-position ASSIGNING <ls_posit>.

      DO 2 TIMES.

        IF sy-index = 2.
          READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<ls_mseg_p>) WITH KEY mblnr = <ls_mseg>-mblnr
                                                                        mjahr = <ls_mseg>-mjahr
                                                                        parent_id = <ls_mseg>-line_id.
          IF sy-subrc NE 0.

            " Когда связи нет, то
            IF <ls_mat_move>-werksupplier IS INITIAL.
              <ls_mat_move>-werksupplier =   <ls_mat_move>-werkreceiver.
            ENDIF.

            IF <ls_mat_move>-werkreceiver IS INITIAL.
              <ls_mat_move>-werkreceiver =   <ls_mat_move>-werksupplier.
            ENDIF.

            IF <ls_mat_move>-molreceivertabnum  IS INITIAL.
              <ls_mat_move>-molreceivertabnum  =   <ls_mat_move>-molsuppliertabnum.
            ENDIF.

            IF <ls_mat_move>-molsuppliertabnum  IS INITIAL.
              <ls_mat_move>-molsuppliertabnum  =   <ls_mat_move>-molreceivertabnum .
            ENDIF.

            IF <ls_posit>-batchsuppliernum IS INITIAL.
              <ls_posit>-batchsuppliernum  =   <ls_posit>-batchreceivernum.
            ENDIF.

            IF <ls_posit>-batchreceivernum  IS INITIAL.
              <ls_posit>-batchreceivernum =   <ls_posit>-batchsuppliernum .
            ENDIF.

            CONTINUE.
          ELSE.

            <ls_mseg> = <ls_mseg_p>.
          ENDIF.
        ENDIF.


        lv_objek = <ls_mseg>-matnr && `                      ` && <ls_mseg>-charg.

*       ATC (WARN_OK)
        SELECT SINGLE klart, obtab, objek, cuobj        "#EC CI_NOORDER
        FROM inob
        INTO @DATA(ls_inob)
        WHERE klart = `023`
          AND obtab = `MCH1`
          AND objek = @lv_objek ##WARN_OK.

*       ATC (WARN_OK)
        "ATC 04.07.2022
        IF sy-subrc EQ 0.
          SELECT SINGLE objek, atinn, atwrt FROM ausp INTO @DATA(ls_ausp) "#EC WARNOK
                          WHERE objek = @ls_inob-cuobj
                            AND atinn = @lv_atinn_matsh ##WARN_OK.

          IF sy-subrc EQ 0.
            <ls_posit>-mataccount  =  ls_ausp-atwrt.
          ENDIF.

        ENDIF.

        <ls_posit>-tmcnum = <ls_mseg>-matnr.


*       ATC (SINGLE -> UP TO 1 ROWS)
        "ATC 04.07.2022
        SELECT m~*, t~maktx UP TO 1 ROWS FROM mara AS m "#EC CI_NOORDER
          INNER JOIN makt AS t ON t~matnr = m~matnr
          INTO @DATA(ls_mara)
               WHERE m~matnr = @<ls_mseg>-matnr
                 AND t~spras EQ 'R'.
        ENDSELECT.

        <ls_posit>-tmcname = ls_mara-maktx.


        IF <ls_mseg>-anln1 IS NOT INITIAL.
          lv_anla1  =  <ls_mseg>-anln1.
          lv_anla2  =  <ls_mseg>-anln2.

          <ls_posit>-oscardnum = <ls_mseg>-anln1 && <ls_mseg>-anln2.

        ELSEIF <ls_mseg>-sgtxt IS NOT INITIAL.
          lv_anla1  =  <ls_mseg>-sgtxt(12).
          lv_anla2  =  <ls_mseg>-sgtxt+12(4).

          <ls_posit>-oscardnum = <ls_mseg>-sgtxt.

        ENDIF.

*{ KvizhinadzeVS 14.07.2023
        SELECT objk~sernr,
               equi~serge,
               anla~anln1, anla~anln2, anla~invnr, anla~anlkl,
               clgr~zgr

          FROM ser03 JOIN objk                 ON objk~obknr = ser03~obknr
                     JOIN equi                 ON equi~sernr = objk~sernr
                                              AND equi~matnr = objk~matnr
                     LEFT JOIN anla            ON anla~sernr = objk~sernr
                                              AND anla~anln1 IS NOT INITIAL
*                                              AND anla~bukrs = equi~bukrs
*                                              AND ( anla~anln1 = equi~anlnr AND anla~anln1 IS NOT INITIAL )
*                                              AND anla~anln2 = equi~anlun
            LEFT JOIN ztmm_003_cl_gr_1 AS clgr ON clgr~anlkl = anla~anlkl

          WHERE ser03~mblnr  = @<ls_mseg>-mblnr
            AND ser03~mjahr  = @<ls_mseg>-mjahr
            AND ser03~zeile  = @<ls_mseg>-zeile
            AND ser03~werk   = @<ls_mseg>-werks
            AND ser03~charge = @<ls_mseg>-charg
            AND equi~serge IS NOT INITIAL
          ORDER BY equi~sernr DESCENDING, equi~serge DESCENDING
          INTO TABLE @DATA(lt_sernum).

        DELETE ADJACENT DUPLICATES FROM lt_sernum COMPARING sernr serge.

        <ls_posit>-serge = VALUE ztt_int0003_pt0111_serge( FOR ls_sernum IN lt_sernum WHERE ( serge IS NOT INITIAL )
                                                         ( ls_sernum-serge ) ).

        READ TABLE lt_sernum ASSIGNING FIELD-SYMBOL(<ls_sernum>) INDEX 1.

*        LOOP AT lt_sernum ASSIGNING FIELD-SYMBOL(<ls_sernum>)
*        WHERE serge IS NOT INITIAL.
*
*          APPEND <ls_sernum>-serge TO <ls_posit>-serge.
*
**          SHIFT <ls_sernum>-serge LEFT DELETING LEADING '0'.
**
**          IF NOT line_exists( lt_serge[ serge = <ls_sernum>-serge ] ).
**            <ls_posit>-factorynum = |{ <ls_posit>-factorynum },{ <ls_sernum>-serge }|.
**            COLLECT VALUE ty_serge( serge = <ls_sernum>-serge ) INTO lt_serge.
**          ENDIF.
*        ENDLOOP.

*        SHIFT <ls_posit>-factorynum LEFT DELETING LEADING ','.

*} KvizhinadzeVS 14.07.2023



        READ TABLE lt_anla ASSIGNING FIELD-SYMBOL(<ls_anla>)
             WITH KEY anln1 = lv_anla1
                      anln2 = lv_anla2
                      bukrs = <ls_mseg>-bukrs.

        IF <ls_anla> IS ASSIGNED.
          <ls_posit>-inventorynum = <ls_anla>-invnr.
          " Если InventoryNum ≠ пусто, то считываем
          <ls_mat_move>-molreceivertabnum = <ls_anla>-pernr.

          <ls_mat_move>-werkreceiver      = `0` && <ls_anla>-kostl+1(2) && `-` &&  `0` && <ls_anla>-kostl+3(2) .

          " Дефект EXT_ITECO-541
          "---------------------
          IF <ls_anla>-zztransfer = abap_true.
            <ls_viddvig>-code_doc_abc = 'ТМЦ_ДОНАЦЕНКА'.
            <ls_mat_move>-materialdoccode = <ls_viddvig>-code_doc_abc.
          ENDIF.
          "---------------------

        ELSEIF <ls_sernum> IS ASSIGNED AND <ls_mseg>-bwart <> '101'.
          <ls_posit>-inventorynum = <ls_sernum>-invnr.
        ENDIF.

        <ls_posit>-tmcname = ls_mara-maktx.

        IF NOT ( <ls_mseg>-bwart IN lt_rng_tmcgroup_bwart[] AND <ls_mseg>-grund IN lt_rng_tmcgroup_grund[] ) AND
           NOT ( <ls_mseg>-bwart = '241' AND <ls_mseg>-grund = '0001' ).


          <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-m-matkl ]-zcl OPTIONAL ).
          <ls_posit>-tmcgroup = VALUE #( lt_cl_gr[ zmatkl = ls_mara-m-matkl ]-zgr OPTIONAL ).


        ELSEIF <ls_mseg>-bwart = '241' AND <ls_mseg>-grund = '0001'.

          <ls_posit>-tmcgroup =  lv_zmm003_zgr.
          <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-m-matkl ]-zcl OPTIONAL ).

        ELSEIF <ls_mseg>-bwart IN lt_rng_tmcgroup_bwart[] AND <ls_mseg>-grund IN lt_rng_tmcgroup_grund[].


          <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-m-matkl ]-zcl OPTIONAL ).
          <ls_posit>-tmcgroup = COND #( WHEN <ls_anla> IS ASSIGNED THEN VALUE #( lt_003_cl_gr_1[ anlkl = <ls_anla>-anlkl ]-zgr OPTIONAL )
                                        WHEN <ls_sernum> IS ASSIGNED THEN <ls_sernum>-zgr ).


          IF <ls_posit>-oscardnum IS INITIAL AND <ls_sernum> IS ASSIGNED.

            <ls_posit>-oscardnum = <ls_sernum>-anln1 && <ls_sernum>-anln2.

          ENDIF.
        ENDIF.
* { KASHIEV-AV 15.06.2023 13:34:39 ЗНИ 97
        IF <ls_mseg>-bwart = '101'.

          DATA(lv_objk) = |{ <ls_mseg>-matnr }                      { <ls_mseg>-charg }|.
*          DATA(lv_objk) = |{ <ls_mseg>-matnr }          { <ls_mseg>-charg }|.

          <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-m-matkl ]-zcl OPTIONAL ).

          SELECT SINGLE ausp~atwrt
          FROM ausp
          INNER JOIN cabn ON cabn~atinn = ausp~atinn
          INNER JOIN inob ON inob~cuobj = ausp~objek
            WHERE cabn~atnam = 'Z_PUR_NDS' AND
                  inob~objek = @lv_objk AND
                  inob~obtab = 'MCH1' AND
                  inob~klart = '023'
          INTO @DATA(lv_atwrt).

          "закоменчено для переноса в прод 21,08,23 upd раскоменчено
          IF sy-subrc = 0 AND lv_atwrt = 0 OR lv_atwrt IS INITIAL.
            <ls_posit>-tmcclass = VALUE #( lt_cl_gr[ zmatkl = ls_mara-m-matkl ]-zclbnds ).
          ENDIF.

        ENDIF.

        IF lt_rseg[ ebeln = <ls_mseg>-ebeln ebelp = <ls_mseg>-ebelp ]-mwskz = 'P0' OR
           lt_rseg[ ebeln = <ls_mseg>-ebeln ebelp = <ls_mseg>-ebelp ]-mwskz = 'PP' OR
           lt_rseg[ ebeln = <ls_mseg>-ebeln ebelp = <ls_mseg>-ebelp ]-mwskz = 'R0'.

          SELECT SINGLE zclbnds
          FROM ztmm_cl_gr
          WHERE zmatkl = @ls_mara-m-matkl
          INTO @<ls_posit>-tmcclass.


        ELSE.
          SELECT SINGLE zcl
          FROM ztmm_cl_gr
          WHERE zmatkl = @ls_mara-m-matkl
          INTO @<ls_posit>-tmcclass.
        ENDIF.
* } KASHIEV-AV 15.06.2023 13:34:39
        CLEAR lt_sernum.



        <ls_posit>-iskindbusinessbank = '1'.

        <ls_posit>-isvatamount = '1'.
        <ls_posit>-isvatinclude = '1' .

        <ls_posit>-amount = <ls_mseg>-dmbtr.

***        IF lv_loc_active = abap_true.
***          READ TABLE lt_locationid WITH KEY matnr = <ls_mseg>-matnr
***                                            werks = <ls_mseg>-werks
***                                            lgort = <ls_mseg>-lgort
***                                            ASSIGNING FIELD-SYMBOL(<ls_locid>).
***          IF sy-subrc = 0.
***            <ls_posit>-locationid = <ls_locid>-locationid.
***          ENDIF.
***        ENDIF.


        READ TABLE lt_rseg ASSIGNING FIELD-SYMBOL(<ls_rseg>)
                                         WITH KEY ebeln = <ls_mseg>-ebeln
                                                  ebelp = <ls_mseg>-ebelp.
        IF sy-subrc EQ 0.

          IF <ls_rseg>-tbtkz EQ 'X'.
            " значит VORGANG3 = 3 - Дополнительное дебетование
            " и значение для amount нужно брать из rseg
*            <ls_posit>-amount = <ls_rseg>-wrbtr.
*            <ls_posit>-vatamount =  <ls_rseg>-stock_posting  - <ls_rseg>-wrbtr.
            <ls_posit>-quantity = <ls_rseg>-menge.

            <ls_mat_move>-materialdocnum = ls_rbkp-belnr.
            <ls_mat_move>-materialdocyear = ls_rbkp-gjahr.
            <ls_mat_move>-materialdocdate = ls_rbkp-bldat.
            <ls_mat_move>-entrysupposeddate = ls_rbkp-budat.
            <ls_mat_move>-primarydocnum = ls_rbkp-xblnr.
            <ls_mat_move>-agreementid = ls_rbkp-zuonr.
            <ls_mat_move>-supplierclientid = COND #( WHEN <ls_mat_move>-supplierclientid IS INITIAL THEN ls_rbkp-lifnr ELSE <ls_mat_move>-supplierclientid ).

            DATA(lv_ext_key) = CONV scmg_ext_key( ls_rbkp-zuonr ).
            lv_ext_key = |{ lv_ext_key ALPHA = IN }|.

            SELECT SINGLE m~abs_num
              FROM zrcmt_dgv_attr AS m
                INNER JOIN scmg_t_case_attr AS s
                  ON s~case_guid = m~case_guid
              INTO @<ls_mat_move>-cftagreementid
              WHERE s~ext_key = @lv_ext_key ##WARN_OK.

            READ TABLE lt_viddvig ASSIGNING <ls_viddvig>
                           WITH KEY vorgang = '3'.

            IF sy-subrc EQ 0.
              "Если найдено значение по операции то оно приоритетнее
              <ls_mat_move>-materialdoccode = <ls_viddvig>-code_doc_abc.
            ENDIF.

          ELSE.

*          " значит VORGANG3 = 1 - Дополнительное дебетование - как понять что за VORGANG3 = 2 пока не понятно )
*          " и значение для amount нужно брать из rseg
*          <ls_posit>-quantity = <ls_rseg>-menge.

*           ATC (DB_FEATURE_MODE[TABLE_LEN_MAX1])
*            SELECT SINGLE navnw INTO @DATA(lv_vatamoint) FROM ekpo  WHERE ebeln = @<ls_mseg>-ebeln AND ebelp = @<ls_mseg>-ebelp ##DB_FEATURE_MODE[TABLE_LEN_MAX1].
*            IF sy-subrc EQ 0.
*              <ls_posit>-vatamount = lv_vatamoint.
*            ENDIF.

          ENDIF.


*RSEG -EBELN= MSEG -EBELN = EKPO - EBELN и RSEG – EBELP = MSEG – EBELP = EKPO – EBELP


          <ls_posit>-vatcode = <ls_rseg>-mwskz.

          lv_wrbtr = <ls_rseg>-wrbtr.

          CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
            EXPORTING
              i_bukrs           = <ls_rseg>-bukrs
              i_mwskz           = <ls_rseg>-mwskz
              i_waers           = lv_waers
              i_wrbtr           = lv_wrbtr
            TABLES
              t_mwdat           = lt_mwdat
            EXCEPTIONS
              bukrs_not_found   = 1
              country_not_found = 2
              mwskz_not_defined = 3
              mwskz_not_valid   = 4
              ktosl_not_found   = 5
              kalsm_not_found   = 6
              parameter_error   = 7
              knumh_not_found   = 8
              kschl_not_found   = 9
              unknown_error     = 10
              account_not_found = 11
              txjcd_not_valid   = 12
              tdt_error         = 13
              txa_error         = 14
              OTHERS            = 15.


          IF sy-subrc EQ 0 AND lt_mwdat IS NOT INITIAL.
            <ls_posit>-vatrate   = lt_mwdat[ 1 ]-msatz.
            <ls_posit>-vatamount = lt_mwdat[ 1 ]-wmwst.

*            IF <ls_rseg>-tbtkz EQ 'X'.
            <ls_posit>-amount    = <ls_rseg>-wrbtr + <ls_posit>-vatamount.
*            ENDIF.

          ENDIF.
        ENDIF.

        IF ls_mara-m-mtart IN lt_rng_mtart_nma.
          <ls_posit>-batchsuppliernum = VALUE #( lt_ekkn[ ebeln = <ls_mseg>-ebeln ebelp = <ls_mseg>-ebelp ]-anln1 OPTIONAL ).
          CLEAR <ls_posit>-batchreceivernum.
        ELSE.
          IF <ls_mseg>-xauto EQ space.
            <ls_posit>-batchsuppliernum = <ls_mseg>-charg.
          ENDIF.
          IF <ls_mseg>-xauto EQ 'X'.
            <ls_posit>-batchreceivernum = <ls_mseg>-charg.
          ENDIF.
        ENDIF.


        <ls_posit>-okof            =   space.
        <ls_posit>-usefullifeterm  =  space.

        IF <ls_posit>-unitmeasurecode IS INITIAL.
          IF <ls_mseg>-bprme IS NOT INITIAL.

            READ TABLE lt_mm_okei ASSIGNING FIELD-SYMBOL(<ls_mm_okey>) WITH KEY msehi = <ls_mseg>-bprme.
            IF sy-subrc EQ 0.
              <ls_posit>-unitmeasurecode  = <ls_mm_okey>-mm_okei.
            ENDIF.
          ELSE.

            READ TABLE lt_mm_okei ASSIGNING <ls_mm_okey> WITH KEY msehi = <ls_mseg>-erfme.
            IF sy-subrc EQ 0.
              <ls_posit>-unitmeasurecode  = <ls_mm_okey>-mm_okei.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <ls_posit>-quantity  IS INITIAL.
          <ls_posit>-quantity        = <ls_mseg>-menge.
        ENDIF.

        lv_txt_name  = <ls_mseg>-ebeln && <ls_mseg>-ebelp.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'F01'
            language                = 'R'
            name                    = lv_txt_name
            object                  = 'EKPO'
          TABLES
            lines                   = lt_long_text
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF sy-subrc EQ 0.
          LOOP AT lt_long_text ASSIGNING FIELD-SYMBOL(<ls_long_txt>).
            <ls_posit>-textline = <ls_posit>-textline && <ls_long_txt>-tdline.
          ENDLOOP.
        ENDIF.


        "------------------------------
        " >> 01.06.2023 KVIZHINAD-VS
        IF <ls_mat_move>-materialdoccode = 'ТМЦ_ДОНАЦЕНКА'.
          DATA(lv_anlhtxt) = VALUE anlhtxt( ).
          IF _materialdoccode_donachenka( EXPORTING
                                           iv_anln1    = lv_anla1
                                           iv_anln2    = lv_anla2
                                           iv_bukrs    = <ls_mseg>-bukrs
                                           ir_anbwa    = lr_anbwa_770
                                           ir_gkont    = lr_gkont
                                          IMPORTING
                                           ev_anlhtxt  = lv_anlhtxt ).
            <ls_posit>-oscardnum = ''.
            IF lv_anlhtxt IS INITIAL.
              <ls_posit>-mataccount = lv_anlhtxt.
            ENDIF.
            "{ baryutin-sa 24.10.23 EXT_ITECO-2057 ТМЦ_ДОНАЦЕНКА при мигрированной партии
          ELSEIF ls_ausp-atwrt IS NOT INITIAL.
            <ls_posit>-mataccount  =  ls_ausp-atwrt.
            "} baryutin-sa 24.10.23
          ELSE.
            CLEAR <ls_posit>-mataccount.
          ENDIF.

          "{ baryutin-sa 24.10.23 EXT_ITECO-2057 ТМЦ_ДОНАЦЕНКА при мигрированной партии

          IF lv_donacenka_name_filled = space.
            READ TABLE lt_moved_lgort_names WITH KEY charg = <ls_mseg>-charg  ASSIGNING FIELD-SYMBOL(<ls_moved_lgort_names>).
            IF sy-subrc = 0.
              lv_donacenka_name_filled = abap_true.
              <ls_mat_move>-molreceivertabnum = <ls_moved_lgort_names>-name3.
              <ls_mat_move>-molsuppliertabnum = <ls_moved_lgort_names>-name3.
            ENDIF.
          ENDIF.

          "} baryutin-sa 24.10.23

        ENDIF.
        " << 01.06.2023 KVIZHINAD-VS
        "------------------------------

      ENDDO.

    ENDLOOP.

    "Рассчитывание разницы между полным счетом с суммой счетов из каждой позиции, добавляем разницу в первую позицию
    LOOP AT <ls_mat_move>-position ASSIGNING <ls_posit>.
      lv_amount_sum = lv_amount_sum + <ls_posit>-amount.
    ENDLOOP.

    lv_amount_diff = ls_rbkp-rmwwr - lv_amount_sum.

    IF lv_amount_diff IS NOT INITIAL.
      _add_sum_diff( EXPORTING iv_belnr = iv_belnr iv_gjahr = iv_gjahr
                     CHANGING cs_data = rs_data ).
    ENDIF.

*    IF <ls_mat_move>-position IS NOT INITIAL.
*      <ls_mat_move>-position[ 1 ]-amount = <ls_mat_move>-position[ 1 ]-amount + lv_amount_diff.
*      <ls_mat_move>-position[ 1 ]-vatamount = <ls_mat_move>-position[ 1 ]-vatamount + lv_amount_diff.
*    ENDIF.

    READ TABLE lt_mat_move INTO rs_data INDEX 1.

    IF rs_data-materialdoccode IN lt_rng_not_clear_suppl.
      CLEAR rs_data-agreementid.
    ENDIF.

    IF rs_data-cftagreementid IS NOT INITIAL AND
       rs_data-agreementid    IS NOT INITIAL AND
       rs_data-materialdoccode NOT IN lt_rng_not_clear_suppl.
      CLEAR rs_data-supplierclientid.
    ENDIF.


*{KvizhinadzeVS 24.12.2023
    rs_data-primedocs = _get_primedocs( iv_belnr = iv_belnr
                                        iv_gjahr = iv_gjahr ).
*}KvizhinadzeVS 24.12.2023

    "Проверка, отправлялся ли документ уже
    "-------------------------------------
    SELECT SINGLE @abap_true
      FROM ztint_pt0111
      WHERE material_doc_num    = @rs_data-materialdocnum
        AND material_doc_year   = @rs_data-materialdocyear
        AND process_status      = @zif_ifc_const=>mc_process_status-success
        AND materialdocnum_from = @rs_data-materialdocnum_from
      INTO @DATA(lv_send).

    IF sy-subrc = 0.
      CLEAR rs_data.
    ENDIF.
    "-------------------------------------

    "Дефект EXT_ITECO-274
    "-------------------------------------
    LOOP AT rs_data-position ASSIGNING FIELD-SYMBOL(<ls_def274>)
         WHERE mataccount IS NOT INITIAL.
      CLEAR: <ls_def274>-batchreceivernum, <ls_def274>-batchsuppliernum.
    ENDLOOP.
    "-------------------------------------

    "Дефект EXT_ITECO-368
    "-------------------------------------
    SHIFT rs_data-agreementid LEFT DELETING LEADING '0'.
    "-------------------------------------


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_SF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<-->] CS_DATA                        TYPE        ZSINT0003_PT0112_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_sf.
    CONSTANTS lc_stblg_null TYPE RE_STBLG VALUE IS INITIAL.
    "ATC 04.07.2022
    SELECT SINGLE ekbe~belnr "#EC WARNOK
      FROM ekbe
        JOIN rbkp
          on ekbe~belnr = rbkp~belnr AND
             rbkp~stblg = @lc_stblg_null
      WHERE ebeln = @cs_data-znp
        AND ( vgabe = 'P' OR vgabe = '2' )
      INTO @cs_data-sf.

    IF sy-subrc <> 0.

      MESSAGE e010(zmmint0003_msg) WITH cs_data-znp INTO mv_dummy.
      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).
      MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      cs_data-process_status = zif_ifc_const=>mc_process_status-error.

    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_ZNP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG(optional)
* | [<-->] CS_DATA                        TYPE        ZSINT0003_PT0112_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_znp.
    DATA(lv_belnr) = CONV char10( cs_data-material_doc_num ).

    SELECT SINGLE mseg~ebeln
    FROM mkpf INNER JOIN mseg ON mkpf~mblnr = mseg~mblnr
              INNER JOIN ekpo ON mseg~ebeln = ekpo~ebeln
    WHERE mkpf~mblnr = @lv_belnr
    INTO @cs_data-znp ##WARN_OK.

    IF sy-subrc <> 0.

      SELECT SINGLE ekbe~ebeln
      FROM mseg JOIN ekbe ON mseg~matnr = ekbe~matnr
                         AND mseg~werks = ekbe~werks
                         AND mseg~bwtar = ekbe~bwtar

      WHERE mseg~mblnr = @lv_belnr
        AND mseg~mjahr = @cs_data-material_doc_year
      INTO @cs_data-znp.

      IF sy-subrc <> 0.

        cs_data-process_status = zif_ifc_const=>mc_process_status-error.

        MESSAGE e006(zmmint0003_msg) INTO mv_dummy.
        IO_LOG->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                        iv_ref_obj_key = cs_data-key ).
        MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
        IO_LOG->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                        iv_ref_obj_key = cs_data-key ).

      ELSE.

        MESSAGE s033(zmmint0003_msg) WITH cs_data-znp INTO mv_dummy.
        IO_LOG->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                        iv_ref_obj_key = cs_data-key ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>RELEASE_SF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<-->] CS_DATA                        TYPE        ZSINT0003_PT0112_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD release_sf.

    DATA: lt_return TYPE TABLE OF bapiret2.
    "ATC 04.07.2022
    SELECT SINGLE belnr, gjahr                              "#EC WARNOK
      FROM rbkp
      WHERE belnr = @cs_data-sf
      INTO @DATA(ls_rbkp).

    CALL FUNCTION 'MRM_SET_NO_BOPF_SAVE'.


    CALL FUNCTION 'BAPI_INCOMINGINVOICE_POST'
      EXPORTING
        invoicedocnumber = cs_data-sf
        fiscalyear       = ls_rbkp-gjahr
      TABLES
        return           = lt_return.

    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type   = 'E'
                                                                      id     = 'M8'
                                                                      number = '255'.

    IF sy-subrc = 0.
      DELETE lt_return INDEX sy-tabix.
    ENDIF.

    IF line_exists( lt_return[ type = 'E' ] ) OR
       line_exists( lt_return[ type = 'A' ] ) OR
       line_exists( lt_return[ type = 'X' ] ).

      io_log->add_bapiret2( iv_extnumber   = CONV #( cs_data-material_doc_num )
                            iv_ref_obj_key = cs_data-key
                            it_bapiret2    = lt_return ).

      MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      cs_data-process_status = zif_ifc_const=>mc_process_status-error.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>RELEASE_ZNP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REL_CODE                    TYPE        FRGCO
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<-->] CS_DATA                        TYPE        ZSINT0003_PT0112_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD release_znp.

    DATA: lt_return TYPE TABLE OF bapireturn.

    CALL FUNCTION 'BAPI_PO_RELEASE'
      EXPORTING
        purchaseorder          = cs_data-znp
        po_rel_code            = iv_rel_code
        no_commit              = 'X'
      TABLES
        return                 = lt_return
      EXCEPTIONS
        authority_check_fail   = 1
        document_not_found     = 2
        enqueue_fail           = 3
        prerequisite_fail      = 4
        release_already_posted = 5
        responsibility_fail    = 6
        OTHERS                 = 7.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         INTO DATA(lv_dummy) ##NEEDED.

      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      cs_data-process_status = zif_ifc_const=>mc_process_status-error.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSEIF line_exists( lt_return[ type = 'E' ] ) OR
           line_exists( lt_return[ type = 'A' ] ) OR
           line_exists( lt_return[ type = 'X' ] ).

      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WHERE type CA 'EAX'.
        MESSAGE e009(zmmint0003_msg) WITH <ls_return>-message+0(50)
                                          <ls_return>-message+51(50)
                                          <ls_return>-message+101(50)
                                          <ls_return>-message+151(50) INTO mv_dummy.
        io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                        iv_ref_obj_key = cs_data-key ).
      ENDLOOP.

      MESSAGE e007(zmmint0003_msg) INTO mv_dummy.
      io_log->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      cs_data-process_status = zif_ifc_const=>mc_process_status-error.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.

      MESSAGE s034(zmmint0003_msg) WITH cs_data-znp INTO mv_dummy.
      IO_LOG->add_sy( iv_extnumber   = CONV #( cs_data-material_doc_num )
                      iv_ref_obj_key = cs_data-key ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.




*    DATA: lt_return TYPE TABLE OF bapireturn.
*
*
*    CALL FUNCTION 'BAPI_PO_RELEASE'
*      EXPORTING
*        purchaseorder          = iv_znp
*        po_rel_code            = iv_rel_code
*        no_commit              = 'X'
*      TABLES
*        return                 = lt_return
*      EXCEPTIONS
*        authority_check_fail   = 1
*        document_not_found     = 2
*        enqueue_fail           = 3
*        prerequisite_fail      = 4
*        release_already_posted = 5
*        responsibility_fail    = 6
*        OTHERS                 = 7.
*
*    IF sy-subrc <> 0.
*
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*         INTO DATA(lv_dummy).
*
*      io_log->add
*
*      ev_error_descr = |{ ev_error_descr }:{ sy-msgno }{ sy-msgid } - { sy-subrc }|.
*
*    ELSEIF line_exists( lt_return[ type = 'E' ] ) OR
*           line_exists( lt_return[ type = 'A' ] ) OR
*           line_exists( lt_return[ type = 'X' ] ).
*
*      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WHERE type CA 'EAX'.
*        ev_error_descr = |{ ev_error_descr }, { <ls_return>-log_no }({ <ls_return>-code }) - { <ls_return>-message }|.
*      ENDLOOP.
*
*      SHIFT ev_error_descr LEFT DELETING LEADING ','.
*
*    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_770
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ZST_0003_SEND_MATERIAL_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_770.

    DATA: ls_flow_data TYPE zsint0003_pt0111_d,
          lt_flow_item TYPE ztt_int0003_pt0111_items_k,
          lt_flow_serg TYPE ztt_int0003_pt0111_serge_d,
          ls_register  TYPE zsint0003_pt0111_register,
          lt_key       TYPE /bobf/t_frw_key.

    DATA(lo_srv) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_ifc_c=>sc_bo_key ).
    DATA(lo_tra) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    DATA(lt_stack) = cl_abap_get_call_stack=>format_call_stack_with_struct(
        stack = cl_abap_get_call_stack=>get_call_stack( ) ).
    READ TABLE lt_stack TRANSPORTING NO FIELDS WITH KEY progname = 'ZCL_MM_INT_D0003_HANDL_PT0112=CP'.

    IF is_data IS INITIAL OR sy-subrc = 0.
      RETURN.
    ENDIF.

    "меппим данные
    "-------------------------------
    ls_flow_data = VALUE #(  material_doc_num       = is_data-materialdocnum
                             material_doc_year      = is_data-materialdocyear
                             material_doc_code      = is_data-materialdoccode
                             material_doc_date      = is_data-materialdocdate
                             entry_supposed_date    = is_data-entrysupposeddate
                             primary_doc_num        = is_data-primarydocnum
                             werk_supplier          = is_data-werksupplier
                             werk_reciever          = is_data-werkreceiver
                             werks_supplier         = COND #( WHEN is_data-werksupplier IS NOT INITIAL THEN is_data-werksupplier+1(2) && is_data-werksupplier+5(2) )
                             werks_reciever         = COND #( WHEN is_data-werkreceiver IS NOT INITIAL THEN is_data-werkreceiver+1(2) && is_data-werkreceiver+5(2) )
                             mol_supplier_tab_num   = is_data-molsuppliertabnum
                             mol_reciever_tab_num   = is_data-molreceivertabnum
                             order_num              = is_data-ordernum
                             agreement_id           = is_data-agreementid
                             user_login             = is_data-userlogin
                             supplier_client_id     = is_data-supplierclientid
                             reciever_client_id     = is_data-receiverclientid
                             header_text            = is_data-textheader
                             cft_agreement_id       = is_data-cftagreementid
                             parent_materialdoc_num = is_data-parent_materialdoc_num
                             materialdocnum_from    = is_data-materialdocnum_from ).

    IF ls_flow_data-material_doc_num+0(2) = '51'.
      ls_flow_data-materialdocnum_from  = ls_flow_data-material_doc_num.
    ENDIF.



    LOOP AT is_data-position ASSIGNING FIELD-SYMBOL(<ls_pos>).

      DATA(lv_pos_num) = CONV mblpo( sy-tabix ).

      APPEND VALUE #( mat_doc_pos        = lv_pos_num
                      tmc_list_type      = <ls_pos>-tmclisttype
                      tmc_type           = <ls_pos>-tmctype
                      tmc_num            = <ls_pos>-tmcnum
                      tmc_name           = <ls_pos>-tmcname
                      tmc_class          = <ls_pos>-tmcclass
                      tmc_group          = <ls_pos>-tmcgroup
                      is_bank            = <ls_pos>-iskindbusinessbank
                      is_vat_amount      = <ls_pos>-isvatamount
                      is_vat_include     = <ls_pos>-isvatinclude
                      vat_rate           = <ls_pos>-vatrate
                      vat_amount         = <ls_pos>-vatamount
                      vat_code           = <ls_pos>-vatcode
                      inventory_num      = <ls_pos>-inventorynum
*                      factory_num        = <ls_pos>-factorynum
                      batch_supplier_num = <ls_pos>-batchsuppliernum
                      batch_reciever_num = <ls_pos>-batchreceivernum
                      os_card_num        = <ls_pos>-oscardnum
                      okof               = <ls_pos>-okof
                      useful_life_term   = <ls_pos>-usefullifeterm
                      unit_measure_code  = <ls_pos>-unitmeasurecode
                      quantity           = <ls_pos>-quantity
                      amount             = <ls_pos>-amount
                      mat_account        = <ls_pos>-mataccount
                      text_line          = <ls_pos>-textline
                      extraquantity      = <ls_pos>-extraquantity
                      sernp              = <ls_pos>-sernp
                      bwart              = <ls_pos>-bwart
                      grund              = <ls_pos>-grund
*                      ) TO lt_flow_item.
                      locationid = <ls_pos>-locationid ) TO lt_flow_item.

      DATA(lt_serge) = <ls_pos>-serge.
      SORT lt_serge.
      DELETE ADJACENT DUPLICATES FROM lt_serge.

      LOOP AT lt_serge ASSIGNING FIELD-SYMBOL(<lv_serge>).
        APPEND VALUE #( mat_doc_pos = lv_pos_num
                        serge       = <lv_serge> ) TO lt_flow_serg.
      ENDLOOP.

    ENDLOOP.

    DATA(lt_flow_prim) =  CORRESPONDING ztt_int0003_pt0111_primedocs_k( is_data-primedocs )."{KvizhinadzeVS 24.12.2023}


    ls_register = VALUE #( data         = REF #( ls_flow_data )
                           addtabs_data = VALUE #( ( add_tab = 'PT0111_ITEMS'
                                                     data    = REF #( lt_flow_item ) )
                                                   ( add_tab = 'PT0111_SERGE'
                                                     data    = REF #( lt_flow_serg ) )
                                                   ( add_tab = 'PT0111_PRIMEDOC'
                                                     data    = REF #( lt_flow_prim ) ) ) ).
    "-------------------------------



    "Регистрируем данные
    "-------------------------------
    lo_tra->cleanup( ).

    lo_srv->do_action( "Класс ZCL_IFC_A_REGISTER_PT0111
      EXPORTING
        iv_act_key    = zif_ifc_c=>sc_action-pt0111-act_register
        is_parameters = REF #( ls_register )
      IMPORTING
        et_data       = lt_key ).

    lo_tra->save(
      IMPORTING
        ev_rejected   = DATA(lv_rejected) ##NEEDED
        eo_message    = DATA(lo_message) ) ##NEEDED .
    "-------------------------------



    "-------------------------------
    lo_tra->cleanup( ).

    lo_srv->do_action( iv_act_key = zif_ifc_c=>sc_action-pt0111-act_send
                       it_key     = lt_key ). "Класс ZCL_IFC_A_SEND_PT0111

    lo_tra->save( ).
    "-------------------------------


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_INT003_HELPER=>_GET_PRIMEDOCS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        RE_BELNR
* | [--->] IV_GJAHR                       TYPE        GJAHR
* | [<-()] RT_PRIMEDOCS                   TYPE        ZTT_INT0003_PT0111_PRIMEDOCS_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_primedocs.

    CLEAR: rt_primedocs.

    SELECT CASE
           WHEN docflow~doc_source = 'ZTRCM_PT1511' THEN pt1511~documentnum
           WHEN docflow~doc_source = 'ZTRCM_PT1513' AND pt1513~doc_cat = 'P' THEN pt1513~doc_type
           END AS primarydocumentname,
           CASE
           WHEN docflow~doc_source = 'ZTRCM_PT1511' THEN pt1511~edlink
           WHEN docflow~doc_source = 'ZTRCM_PT1513' AND  pt1513~doc_cat = 'P' THEN pt1513~doc_link
           WHEN docflow~doc_source = 'ZTRCM_PT1515' THEN pt1515~doc_link
           END AS primarydocumentlink,
           CASE
           WHEN docflow~doc_source = 'ZTRCM_PT1511' THEN pt1511~documenttype
           WHEN docflow~doc_source = 'ZTRCM_PT1513' AND pt1513~doc_cat = 'P' THEN pt1513~doc_type
           WHEN docflow~doc_source = 'ZTRCM_PT1515' THEN pt1515~doc_type
           END AS primarydocumenttype

    FROM rbkp INNER JOIN rseg                     ON rseg~belnr                   = rbkp~belnr
                                                 AND rseg~gjahr                   = rbkp~gjahr
                                                 AND rseg~ebelp                   = 00010
              INNER JOIN ztint_docflow AS docflow ON docflow~doc_num              = rseg~ebeln

               LEFT JOIN ztrcm_pt1511  AS pt1511  ON pt1511~task_id               = docflow~task_id
                                                 AND pt1511~id_agreement_asu_fhd  = docflow~id_agreement_asu_fhd
                                                 AND pt1511~db_key                = docflow~db_key
               LEFT JOIN ztrcm_pt1513  AS pt1513  ON pt1513~task_id               = docflow~task_id
                                                 AND pt1513~id_agreement_asu_fhd  = docflow~id_agreement_asu_fhd
                                                 AND pt1513~db_key                = docflow~db_key
               LEFT JOIN ztrcm_pt1515  AS pt1515  ON pt1515~task_id               = docflow~task_id
                                                 AND pt1515~id_agreement_asu_fhd  = docflow~id_agreement_asu_fhd
                                                 AND pt1515~db_key                = docflow~db_key
    WHERE rbkp~belnr IS NOT INITIAL
      AND rbkp~belnr = @iv_belnr
      AND rbkp~gjahr = @iv_gjahr
    INTO CORRESPONDING FIELDS OF TABLE @rt_primedocs.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_INT003_HELPER=>_GET_PARENT_MATDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSEG                        TYPE        MSEG
* | [--->] IV_BKTXT                       TYPE        BKTXT
* | [<-()] RV_PARMATDOC                   TYPE        AWREF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_parent_matdoc.
    DATA ltr_tcodes TYPE RANGE OF tcode.
    CONSTANTS lc_bwart_501  TYPE bwart VALUE '501'.
    CONSTANTS lc_grund_0001 TYPE mb_grbew VALUE '0001'.


    SPLIT iv_bktxt AT '/' INTO DATA(lv_anln1) DATA(lv_anln2).
    IF is_mseg-bwart = lc_bwart_501 AND is_mseg-grund = lc_grund_0001 AND lv_anln1 IS NOT INITIAL.
*      zcl_tvarvc_params_get=>get_range(
*        EXPORTING
*          iv_name  = 'ZSAA0096_PROG_TCODE'
*        IMPORTING
*          er_range = ltr_tcodes ).
      DO 30 TIMES.

        SELECT belnr
          FROM anek
          INTO @rv_parmatdoc UP TO 1 ROWS
          WHERE xblnr = @is_mseg-mblnr
            AND bukrs = @is_mseg-bukrs
            AND anln1 = @lv_anln1
            AND anln2 = @lv_anln2
            AND tcode IN @ltr_tcodes
            AND gjahr = @is_mseg-mjahr          .
        ENDSELECT.
        IF rv_parmatdoc IS NOT INITIAL.
          EXIT.
        ENDIF.
        cl_dmc_utilities=>wait( 1 ).
      ENDDO.
      IF rv_parmatdoc IS INITIAL.
        MESSAGE e002(zaa0096) WITH |{ lv_anln1 }/{ lv_anln2 }| INTO DATA(lv_dum).
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>_GET_LOCATIONID
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_LOCATIONID                  TYPE        TT_LOCATIONID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _get_locationid.

    SORT ct_locationid.
    DELETE ADJACENT DUPLICATES FROM ct_locationid.

    IF ct_locationid IS INITIAL.
      RETURN.
    ENDIF.

    "Определяем ТМЦ и НМА
    SELECT m~matnr, m~matkl, z~zgr
      FROM mara AS m
      JOIN ztmm_cl_gr AS z
        ON m~matkl = z~zmatkl
      FOR ALL ENTRIES IN @ct_locationid
      WHERE matnr = @ct_locationid-matnr
      INTO TABLE @DATA(lt_mara).

    "Местонахождения для ТМЦ
    SELECT t~werks, t~lgort, a~home_city
      FROM twlad AS t
      JOIN adrc AS a
        ON t~adrnr = a~addrnumber
      FOR ALL ENTRIES IN @ct_locationid
      WHERE t~werks = @ct_locationid-werks
        AND t~lgort = @ct_locationid-lgort
      INTO TABLE @DATA(lt_adrc).

    "Местонахождения для НМА
    SELECT zloc_werks, zloc_placeid FROM ztmm_rshb_loc
      WHERE zloc_werks IS NOT INITIAL
      INTO TABLE @DATA(lt_loc).

    LOOP AT ct_locationid ASSIGNING FIELD-SYMBOL(<ls_locid>).
      READ TABLE lt_mara WITH KEY matnr = <ls_locid>-matnr ASSIGNING FIELD-SYMBOL(<ls_mara>).
      IF sy-subrc = 0.
        IF <ls_mara>-zgr CP '60415*' OR <ls_mara>-zgr CP '610*'.

          READ TABLE lt_adrc WITH KEY werks = <ls_locid>-werks lgort = <ls_locid>-lgort ASSIGNING FIELD-SYMBOL(<ls_adrc>).
          IF sy-subrc = 0.
            <ls_locid>-locationid = <ls_adrc>-home_city.
          ENDIF.

        ELSEIF <ls_mara>-zgr CP '60906*'.

          READ TABLE lt_loc WITH KEY zloc_werks = <ls_locid>-werks ASSIGNING FIELD-SYMBOL(<ls_loc>).
          IF sy-subrc = 0.
            <ls_locid>-locationid = <ls_loc>-zloc_placeid.
          ELSE.
            LOOP AT lt_loc ASSIGNING <ls_loc> WHERE zloc_werks = |{ <ls_locid>-werks+0(2) }*|.
              <ls_locid>-locationid = <ls_loc>-zloc_placeid.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Удаляем где пусто
    DELETE ct_locationid WHERE locationid IS INITIAL.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>_ADD_SUM_DIFF_PT0113
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        MBLNR
* | [--->] IV_GJAHR                       TYPE        MJAHR
* | [<-->] CS_DATA                        TYPE        TY_PT0113
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _add_sum_diff_pt0113.


    DATA: ls_header TYPE bapi_incinv_create_header,
          lt_items  TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lt_tax    TYPE STANDARD TABLE OF bapi_incinv_create_tax.

    DATA: lt_return TYPE bapirettab,
          lt_accit  TYPE accit_t,
          lt_acccr  TYPE acccr_t.

    DATA: ls_rbkp2 TYPE rbkp,
          lt_rseg2 TYPE STANDARD TABLE OF rseg,
          lt_rbtx  TYPE STANDARD TABLE OF rbtx.

    DATA: lv_index TYPE sy-tabix.

    SELECT SINGLE * FROM rbkp INTO CORRESPONDING FIELDS OF ls_rbkp2 WHERE belnr = iv_belnr AND gjahr = iv_gjahr.

    SELECT * FROM rseg INTO CORRESPONDING FIELDS OF TABLE lt_rseg2 WHERE belnr = iv_belnr AND gjahr = iv_gjahr.

    SELECT * FROM rbtx INTO CORRESPONDING FIELDS OF TABLE lt_rbtx WHERE belnr = iv_belnr AND gjahr = iv_gjahr.

*      IF ls_rbkp2 IS INITIAL OR lt_rseg2 IS INITIAL.
*        MESSAGE ID sy-msgty TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*        RETURN.
*      ENDIF.

    ls_header = VALUE #(
    currency = ls_rbkp2-waers
       doc_type        = ls_rbkp2-blart
     pstng_date      = ls_rbkp2-budat
    doc_date        = ls_rbkp2-bldat
     trans_date      = ls_rbkp2-wwert                       "1983092
      ref_doc_no      = ls_rbkp2-belnr"ls_rbkp2-xblnr
    comp_code       = ls_rbkp2-bukrs
    diff_inv        = ls_rbkp2-lifnr
     calc_tax_ind    = abap_true"p_xmwst "false "ls_rbkp2-xmwst
      pmnttrms        = ls_rbkp2-zterm
    bline_date      = ls_rbkp2-zfbdt
     dsct_days1      = ls_rbkp2-zbd1t
    dsct_days2      = ls_rbkp2-zbd2t
    netterms        = ls_rbkp2-zbd3t
    dsct_pct1       = ls_rbkp2-zbd1p
    dsct_pct2       = ls_rbkp2-zbd2p
     invoice_ind     = ls_rbkp2-xrech
    secco           = ls_rbkp2-secco                        "1227393
     vatdate         = ls_rbkp2-vatdate                     "1600775
*   de_cre_ind      = ls_rbkp2-tbtkz                          "1097704
*      iv_category     = ls_rbkp2-ivtyp
      header_txt      = ls_rbkp2-bktxt
     pmnt_block      = ls_rbkp2-zlspr
      del_costs_taxc  = ls_rbkp2-mwskz_bnk
      del_costs_taxj  = ls_rbkp2-txjcd_bnk
     person_ext      = ls_rbkp2-ername
    pymt_meth       = ls_rbkp2-zlsch
        pmtmthsupl      = ls_rbkp2-uzawe
*     inv_doc_no      = ls_rbkp2-xblnr"ls_rbkp2-belnr
      scbank_ind      = ls_rbkp2-lzbkz
    supcountry      = ls_rbkp2-landl
    bllsrv_ind      = ls_rbkp2-diekz
    po_sub_no       = ls_rbkp2-esrnr
    po_checkdg      = ls_rbkp2-esrpz
    po_ref_no       = ls_rbkp2-esrre
    payee_payer     = ls_rbkp2-empfb
      partner_bk      = ls_rbkp2-bvtyp
      housebankid     = ls_rbkp2-hbkid
      housebankacctid = ls_rbkp2-hktid
      alloc_nmbr      = ls_rbkp2-zuonr
      paymt_ref       = ls_rbkp2-kidno
     inv_ref_no      = ls_rbkp2-rebzg
      inv_year        = ls_rbkp2-rebzj
       inv_rec_date    = ls_rbkp2-reindat
      planning_level  = ls_rbkp2-fdlev
    planning_date   = ls_rbkp2-fdtag
      fixedterms      = ls_rbkp2-zbfix
      bus_area        = ls_rbkp2-gsber
    lot_number      = ls_rbkp2-lotkz
     item_text       = ls_rbkp2-sgtxt
       eu_triang_deal  = ls_rbkp2-xegdr
      repcountry      = ls_rbkp2-egmld
       vat_reg_no      = ls_rbkp2-stceg
      business_place  = ls_rbkp2-bupla
        goods_affected  = ls_rbkp2-xinve
       tax_calc_date   = ls_rbkp2-txdat  "TmDpndntTax
      del_costs_tax_country = ls_rbkp2-tax_country_bnk
      gross_amount = COND #( WHEN ms_rbkpv IS NOT INITIAL THEN ms_rbkpv-rmwwr
                             ELSE ls_rbkp2-rmwwr ) ).

    LOOP AT lt_rseg2 ASSIGNING FIELD-SYMBOL(<ls_rseg>).
      lv_index = sy-tabix.
      APPEND VALUE #(
        invoice_doc_item = lv_index
        po_number    =   <ls_rseg>-ebeln
        po_item      =  <ls_rseg>-ebelp
        ref_doc      =   <ls_rseg>-lfbnr
        ref_doc_year =   <ls_rseg>-lfgja
        ref_doc_it   =   <ls_rseg>-lfpos
        de_cre_ind   =   <ls_rseg>-tbtkz
        tax_code     =   <ls_rseg>-mwskz
        tax_country  =   <ls_rseg>-tax_country  "TaxAbroad
        taxjurcode   =   <ls_rseg>-txjcd
        quantity     =   <ls_rseg>-menge
        po_unit = <ls_rseg>-bstme
        po_pr_qnt    =   <ls_rseg>-bpmng
        po_pr_uom    = <ls_rseg>-bprme
        item_text    =   <ls_rseg>-sgtxt
        cshdis_ind   =   <ls_rseg>-xskrl
        final_inv    =   <ls_rseg>-erekz
        grir_clear_srv = <ls_rseg>-werec
        item_amount = <ls_rseg>-wrbtr                       "1457098
*      del_create_date = <ls_rseg>-ledat
    ) TO lt_items.
    ENDLOOP.

    IF mt_rbtx IS NOT INITIAL.
      LOOP AT mt_rbtx ASSIGNING FIELD-SYMBOL(<ls_rbtx>).
        lv_index = sy-tabix.
        APPEND VALUE #( tax_code = <ls_rbtx>-mwskz
                        tax_amount = <ls_rbtx>-wmwst
                        tax_base_amount = <ls_rbtx>-fwbas
                        cond_type = <ls_rbtx>-kschl
                        taxjurcode = <ls_rbtx>-txjcd
                        taxjurcode_deep = <ls_rbtx>-txjdp
                        itemno_tax = <ls_rbtx>-taxps
*                    tax_amount_local = <ls_rbtx>-hwste
*                    tax_base_amount_local = <ls_rbtx>-hwbas
                        tax_country = <ls_rbtx>-tax_country
        ) TO lt_tax.
      ENDLOOP.
    ELSE.
      LOOP AT lt_rbtx ASSIGNING <ls_rbtx>.
        lv_index = sy-tabix.
        APPEND VALUE #( tax_code = <ls_rbtx>-mwskz
                        tax_amount = <ls_rbtx>-wmwst
                        tax_base_amount = <ls_rbtx>-fwbas
                        cond_type = <ls_rbtx>-kschl
                        taxjurcode = <ls_rbtx>-txjcd
                        taxjurcode_deep = <ls_rbtx>-txjdp
                        itemno_tax = <ls_rbtx>-taxps
*                    tax_amount_local = <ls_rbtx>-hwste
*                    tax_base_amount_local = <ls_rbtx>-hwbas
                        tax_country = <ls_rbtx>-tax_country
        ) TO lt_tax.
      ENDLOOP.
    ENDIF.

    IF lt_tax IS NOT INITIAL.
      CLEAR ls_header-calc_tax_ind.
    ENDIF.

    CALL FUNCTION 'MRM_SRM_INVOICE_SIMULATE'
      EXPORTING
        headerdata    = ls_header
*       ADDRESSDATA   =
      IMPORTING
        return        = lt_return
        t_accit       = lt_accit
        t_acccr       = lt_acccr
      TABLES
        itemdata      = lt_items
*       ACCOUNTINGDATA            =
*       GLACCOUNTDATA =
*       MATERIALDATA  =
        taxdata       = lt_tax
*       WITHTAXDATA   =
*       VENDORITEMSPLITDATA       =
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      DELETE lt_acccr WHERE curtp <> '10' OR wrbtr IS INITIAL OR fwbas IS INITIAL.
      LOOP AT lt_acccr ASSIGNING FIELD-SYMBOL(<ls_acccr>).
        lv_index = sy-tabix.
        READ TABLE cs_data-item INDEX lv_index ASSIGNING FIELD-SYMBOL(<ls_pos>).
        IF sy-subrc = 0.
          IF <ls_pos>-p_operationamount <> <ls_acccr>-wrbtr + <ls_acccr>-fwbas.
*            IF <ls_acccr>-waers = 'RUB'.
              <ls_pos>-p_operationamount = <ls_acccr>-wrbtr + <ls_acccr>-fwbas.
              <ls_pos>-p_vatamount = <ls_acccr>-wrbtr.
              <ls_pos>-p_vatcurramount = <ls_acccr>-wrbtr.
*            ELSE.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_INT003_HELPER=>_MATERIALDOCCODE_DONACHENKA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANLN1                       TYPE        ANLN1
* | [--->] IV_ANLN2                       TYPE        ANLN2
* | [--->] IV_BUKRS                       TYPE        BUKRS
* | [--->] IR_ANBWA                       TYPE        TR_ANBWA
* | [--->] IR_GKONT                       TYPE        TR_GKONT
* | [<---] EV_ANLHTXT                     TYPE        ANLHTXT
* | [<-()] RV_OK                          TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _materialdoccode_donachenka.

    CLEAR: rv_ok, ev_anlhtxt.

    SELECT SINGLE anlu~zztransfer, anlh~anlhtxt
    FROM anlu LEFT JOIN anlh ON anlh~anln1 = anlu~anln1
                            AND anlh~bukrs = anlu~bukrs
    WHERE anlu~anln1 = @iv_anln1
      AND anlu~anln2 = @iv_anln2
      AND anlu~bukrs = @iv_bukrs
    INTO @DATA(ls_zztransfer).
    IF ls_zztransfer-zztransfer IS NOT INITIAL.
      "Проверка что в строке только цифры
      DATA(lv_anlhtxt) =  ls_zztransfer-anlhtxt.
      REPLACE ALL OCCURRENCES OF REGEX '\D' IN lv_anlhtxt WITH ''.
      IF lv_anlhtxt IS NOT INITIAL
        AND strlen( lv_anlhtxt ) = strlen( ls_zztransfer-anlhtxt ).
        ev_anlhtxt = ls_zztransfer-anlhtxt.

        rv_ok = abap_true.
        RETURN.

      ENDIF.
      "если числовое значение нет необходимости идти в acdoca
***      SELECT gkont
***      FROM anek INNER JOIN acdoca ON acdoca~anln1  = anek~anln1
***                                 AND acdoca~anln2  = anek~anln2
***                                 AND acdoca~rbukrs = anek~bukrs
***                                 AND acdoca~gjahr  = anek~gjahr
***      WHERE anek~anln1    = @iv_anln1
***        AND anek~anln2    = @iv_anln2
***        AND anek~bukrs    = @iv_bukrs
***        AND acdoca~anbwa IN @ir_anbwa
***        AND acdoca~gkont IN @ir_gkont
***      INTO TABLE @DATA(lt_acdoca).
***      IF sy-subrc = 0 AND lt_acdoca IS NOT INITIAL.
***
***        "Проверка что в строке только цифры
***        DATA(lv_anlhtxt) =  ls_zztransfer-anlhtxt.
***        REPLACE ALL OCCURRENCES OF REGEX '\D' IN lv_anlhtxt WITH ''.
***        IF lv_anlhtxt IS NOT INITIAL
***          AND strlen( lv_anlhtxt ) = strlen( ls_zztransfer-anlhtxt ).
***          ev_anlhtxt = ls_zztransfer-anlhtxt.
***
***          rv_ok = abap_true.
***          RETURN.
***
***        ENDIF.
***      ENDIF.
***
***      CLEAR lt_acdoca.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_INT003_HELPER=>_ADD_SUM_DIFF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        MBLNR
* | [--->] IV_GJAHR                       TYPE        MJAHR
* | [<-->] CS_DATA                        TYPE        ZST_0003_SEND_MATERIAL_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _add_sum_diff.

    DATA: ls_header TYPE bapi_incinv_create_header,
          lt_items  TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lt_tax    TYPE STANDARD TABLE OF bapi_incinv_create_tax.

    DATA: lt_return TYPE bapirettab,
          lt_accit  TYPE accit_t,
          lt_acccr  TYPE acccr_t.

    DATA: ls_rbkp2 TYPE rbkp,
          lt_rseg2 TYPE STANDARD TABLE OF rseg,
          lt_rbtx  TYPE STANDARD TABLE OF rbtx.

    DATA: lv_index TYPE sy-tabix.

    SELECT SINGLE * FROM rbkp INTO CORRESPONDING FIELDS OF ls_rbkp2 WHERE belnr = iv_belnr AND gjahr = iv_gjahr.

    SELECT * FROM rseg INTO CORRESPONDING FIELDS OF TABLE lt_rseg2 WHERE belnr = iv_belnr AND gjahr = iv_gjahr.

    SELECT * FROM rbtx INTO CORRESPONDING FIELDS OF TABLE lt_rbtx WHERE belnr = iv_belnr AND gjahr = iv_gjahr.

*      IF ls_rbkp2 IS INITIAL OR lt_rseg2 IS INITIAL.
*        MESSAGE ID sy-msgty TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*        RETURN.
*      ENDIF.

    ls_header = VALUE #(
    currency = ls_rbkp2-waers
       doc_type        = ls_rbkp2-blart
     pstng_date      = ls_rbkp2-budat
    doc_date        = ls_rbkp2-bldat
     trans_date      = ls_rbkp2-wwert                       "1983092
      ref_doc_no      = ls_rbkp2-belnr"ls_rbkp2-xblnr
    comp_code       = ls_rbkp2-bukrs
    diff_inv        = ls_rbkp2-lifnr
     calc_tax_ind    = abap_true"p_xmwst "false "ls_rbkp2-xmwst
      pmnttrms        = ls_rbkp2-zterm
    bline_date      = ls_rbkp2-zfbdt
     dsct_days1      = ls_rbkp2-zbd1t
    dsct_days2      = ls_rbkp2-zbd2t
    netterms        = ls_rbkp2-zbd3t
    dsct_pct1       = ls_rbkp2-zbd1p
    dsct_pct2       = ls_rbkp2-zbd2p
     invoice_ind     = ls_rbkp2-xrech
    secco           = ls_rbkp2-secco                        "1227393
     vatdate         = ls_rbkp2-vatdate                     "1600775
*   de_cre_ind      = ls_rbkp2-tbtkz                          "1097704
*      iv_category     = ls_rbkp2-ivtyp
      header_txt      = ls_rbkp2-bktxt
     pmnt_block      = ls_rbkp2-zlspr
      del_costs_taxc  = ls_rbkp2-mwskz_bnk
      del_costs_taxj  = ls_rbkp2-txjcd_bnk
     person_ext      = ls_rbkp2-ername
    pymt_meth       = ls_rbkp2-zlsch
        pmtmthsupl      = ls_rbkp2-uzawe
*     inv_doc_no      = ls_rbkp2-xblnr"ls_rbkp2-belnr
      scbank_ind      = ls_rbkp2-lzbkz
    supcountry      = ls_rbkp2-landl
    bllsrv_ind      = ls_rbkp2-diekz
    po_sub_no       = ls_rbkp2-esrnr
    po_checkdg      = ls_rbkp2-esrpz
    po_ref_no       = ls_rbkp2-esrre
    payee_payer     = ls_rbkp2-empfb
      partner_bk      = ls_rbkp2-bvtyp
      housebankid     = ls_rbkp2-hbkid
      housebankacctid = ls_rbkp2-hktid
      alloc_nmbr      = ls_rbkp2-zuonr
      paymt_ref       = ls_rbkp2-kidno
     inv_ref_no      = ls_rbkp2-rebzg
      inv_year        = ls_rbkp2-rebzj
       inv_rec_date    = ls_rbkp2-reindat
      planning_level  = ls_rbkp2-fdlev
    planning_date   = ls_rbkp2-fdtag
      fixedterms      = ls_rbkp2-zbfix
      bus_area        = ls_rbkp2-gsber
    lot_number      = ls_rbkp2-lotkz
     item_text       = ls_rbkp2-sgtxt
       eu_triang_deal  = ls_rbkp2-xegdr
      repcountry      = ls_rbkp2-egmld
       vat_reg_no      = ls_rbkp2-stceg
      business_place  = ls_rbkp2-bupla
        goods_affected  = ls_rbkp2-xinve
       tax_calc_date   = ls_rbkp2-txdat  "TmDpndntTax
      del_costs_tax_country = ls_rbkp2-tax_country_bnk
      gross_amount = COND #( WHEN ms_rbkpv IS NOT INITIAL THEN ms_rbkpv-rmwwr
                             ELSE ls_rbkp2-rmwwr ) ).

    LOOP AT lt_rseg2 ASSIGNING FIELD-SYMBOL(<ls_rseg>).
      lv_index = sy-tabix.
      APPEND VALUE #(
        invoice_doc_item = lv_index
        po_number    =   <ls_rseg>-ebeln
        po_item      =  <ls_rseg>-ebelp
        ref_doc      =   <ls_rseg>-lfbnr
        ref_doc_year =   <ls_rseg>-lfgja
        ref_doc_it   =   <ls_rseg>-lfpos
        de_cre_ind   =   <ls_rseg>-tbtkz
        tax_code     =   <ls_rseg>-mwskz
        tax_country  =   <ls_rseg>-tax_country  "TaxAbroad
        taxjurcode   =   <ls_rseg>-txjcd
        quantity     =   <ls_rseg>-menge
        po_unit = <ls_rseg>-bstme
        po_pr_qnt    =   <ls_rseg>-bpmng
        po_pr_uom    = <ls_rseg>-bprme
        item_text    =   <ls_rseg>-sgtxt
        cshdis_ind   =   <ls_rseg>-xskrl
        final_inv    =   <ls_rseg>-erekz
        grir_clear_srv = <ls_rseg>-werec
        item_amount = <ls_rseg>-wrbtr                       "1457098
*      del_create_date = <ls_rseg>-ledat
    ) TO lt_items.
    ENDLOOP.

    IF mt_rbtx IS NOT INITIAL.
      LOOP AT mt_rbtx ASSIGNING FIELD-SYMBOL(<ls_rbtx>).
        lv_index = sy-tabix.
        APPEND VALUE #( tax_code = <ls_rbtx>-mwskz
                        tax_amount = <ls_rbtx>-wmwst
                        tax_base_amount = <ls_rbtx>-fwbas
                        cond_type = <ls_rbtx>-kschl
                        taxjurcode = <ls_rbtx>-txjcd
                        taxjurcode_deep = <ls_rbtx>-txjdp
                        itemno_tax = <ls_rbtx>-taxps
*                    tax_amount_local = <ls_rbtx>-hwste
*                    tax_base_amount_local = <ls_rbtx>-hwbas
                        tax_country = <ls_rbtx>-tax_country
        ) TO lt_tax.
      ENDLOOP.
    ELSE.
      LOOP AT lt_rbtx ASSIGNING <ls_rbtx>.
        lv_index = sy-tabix.
        APPEND VALUE #( tax_code = <ls_rbtx>-mwskz
                        tax_amount = <ls_rbtx>-wmwst
                        tax_base_amount = <ls_rbtx>-fwbas
                        cond_type = <ls_rbtx>-kschl
                        taxjurcode = <ls_rbtx>-txjcd
                        taxjurcode_deep = <ls_rbtx>-txjdp
                        itemno_tax = <ls_rbtx>-taxps
*                    tax_amount_local = <ls_rbtx>-hwste
*                    tax_base_amount_local = <ls_rbtx>-hwbas
                        tax_country = <ls_rbtx>-tax_country
        ) TO lt_tax.
      ENDLOOP.
    ENDIF.

    IF lt_tax IS NOT INITIAL.
      CLEAR ls_header-calc_tax_ind.
    ENDIF.

    CALL FUNCTION 'MRM_SRM_INVOICE_SIMULATE'
      EXPORTING
        headerdata    = ls_header
*       ADDRESSDATA   =
      IMPORTING
        return        = lt_return
        t_accit       = lt_accit
        t_acccr       = lt_acccr
      TABLES
        itemdata      = lt_items
*       ACCOUNTINGDATA            =
*       GLACCOUNTDATA =
*       MATERIALDATA  =
        taxdata       = lt_tax
*       WITHTAXDATA   =
*       VENDORITEMSPLITDATA       =
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      DELETE lt_acccr WHERE curtp <> '10' OR navbt = 0.
      LOOP AT lt_acccr ASSIGNING FIELD-SYMBOL(<ls_acccr>).
        lv_index = sy-tabix.
        READ TABLE cs_data-position INDEX lv_index ASSIGNING FIELD-SYMBOL(<ls_pos>).
        IF sy-subrc = 0.
          IF <ls_pos>-amount <> <ls_acccr>-wrbtr.
            <ls_pos>-amount = <ls_acccr>-wrbtr.
            <ls_pos>-vatamount = <ls_acccr>-navbt.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SET_ZGOSNOMER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSEG                        TYPE        MSEG
* | [--->] IS_VM07M                       TYPE        VM07M
* | [--->] IS_DM07M                       TYPE        DM07M
* | [--->] IS_MKPF                        TYPE        MKPF
* | [--->] IT_CHARACTERS                  TYPE        ZTT_CHARACTERS
* | [<-->] CT_VALUES                      TYPE        ZTT_VALUES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_zgosnomer.
    CONSTANTS:
      lc_atnam_zgosnom TYPE atnam VALUE 'ZGOSNOMER'.
    DATA: lv_name  TYPE thead-tdname,
          lt_lines TYPE STANDARD TABLE OF tline.

    CHECK is_mseg-charg IS NOT INITIAL AND
          is_mseg-bwart = '101' AND
          is_mseg-ebeln IS NOT INITIAL AND
          is_mseg-ebelp IS NOT INITIAL.

    lv_name = |{ is_mseg-ebeln }{ is_mseg-ebelp }|."450209497000010

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'F01'
        language                = 'R'
        name                    = lv_name
        object                  = 'EKPO'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*     IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
* Implement suitable error handling here

      LOOP AT it_characters ASSIGNING FIELD-SYMBOL(<ls_characters>) WHERE atnam = lc_atnam_zgosnom.
        APPEND INITIAL LINE TO ct_values ASSIGNING FIELD-SYMBOL(<ls_values>).
        <ls_values>-atinn = <ls_characters>-atinn.
        <ls_values>-atnam = <ls_characters>-atnam.
        READ TABLE lt_lines INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_line>).
        IF sy-subrc = 0.
          <ls_values>-atwtb = <ls_line>-tdline.
        ENDIF.
      ENDLOOP.

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_WARNING_MESSAGES_FROM_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_SEND
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<---] ET_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_warning_messages_from_log.

    CONSTANTS: lc_max      TYPE i VALUE 255.

    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lv_usnam     TYPE usnam,
          lo_msg_cntrl TYPE REF TO if_reca_message_list,
          lv_msg_text  TYPE string,
          lv_message   TYPE string.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lt_text         TYPE bcsy_text,
          note            TYPE bcsy_text,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO cl_cam_address_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs,
          bcs_exception   TYPE REF TO cx_bcs,
          sent_to_all     TYPE os_boolean,
          lv_text         TYPE so_obj_des,
          ls_errors_res   TYPE zsrcmint0053_pt0122_resp,
          lv_error        TYPE abap_bool.

    DATA: lv_sum         TYPE i,
          lv_txt1        TYPE so_text255,
          lt_txt1        TYPE STANDARD TABLE OF so_text255,
          lv_len1        TYPE i,
          lv_txt2        TYPE so_text255,
          lt_txt2        TYPE bcsy_text,
          lv_len2        TYPE i,
          lv_hasmessages TYPE abap_bool.

    DATA: ls_ztmail_text TYPE ztmail_text.

    DATA: lv_starv_rbstat TYPE rbstat.

    DATA(lv_belnr) = is_send-material_doc_num.

    DATA(lt_messages) = io_log->get_all_messages( ).
    CLEAR: lv_hasmessages.
*    LOOP AT lt_messages TRANSPORTING NO FIELDS WHERE msgty = 'W'.
    LOOP AT lt_messages TRANSPORTING NO FIELDS FROM mv_message_maxindex + 1 WHERE msgty = 'W'.
      lv_hasmessages = abap_true.
      EXIT.
    ENDLOOP.
    "если нет предупреждений не отправляем сообщение
    IF lv_hasmessages = space.
      RETURN.
    ENDIF.

*    IF is_send-material_doc_num+0(2) = '51' AND is_send-user_name IS INITIAL.
*      SELECT SINGLE usnam
*              FROM rbkp
*              INTO @lv_usnam
*              WHERE belnr = @is_send-material_doc_num
*                AND gjahr = @is_send-material_doc_year.
*    ENDIF.

*    IF is_send-user_name IS INITIAL AND lv_usnam IS INITIAL.
*
*      SELECT SINGLE usnam
*        FROM mkpf
*        INTO @lv_usnam
*        WHERE mblnr = @lv_belnr
*          AND mjahr = @is_send-material_doc_year.
*
*      IF sy-subrc IS NOT INITIAL.
*
*        SELECT SINGLE usnam
*          FROM bkpf
*          INTO @lv_usnam
*          WHERE belnr = @lv_belnr
*            AND gjahr = @is_send-material_doc_year.
*
*        IF sy-subrc <> 0.
*
*
*          SELECT SINGLE usnam
*            FROM anek
*            INTO @lv_usnam
*            WHERE belnr = @lv_belnr
*              AND gjahr = @is_send-material_doc_year.
*
*          IF sy-subrc <> 0.
*
*            SELECT SINGLE usnam
*              FROM rbkp
*              INTO @lv_usnam
*              WHERE belnr = @lv_belnr
*                AND gjahr = @is_send-material_doc_year.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ELSEIF is_send-user_name IS NOT INITIAL.
*      lv_usnam = is_send-user_name.
*    ENDIF.

    SELECT SINGLE material_doc_num, changed_by
      FROM ztint_pt0111
      WHERE material_doc_num = @is_send-material_doc_num
        AND material_doc_year = @is_send-material_doc_year
    INTO @DATA(ls_pt111).
    IF sy-subrc = 0.
      lv_usnam = ls_pt111-changed_by.
    ELSE.
      SELECT SINGLE material_doc_num, changed_by
        FROM ztint_pt0113
        WHERE material_doc_num = @is_send-material_doc_num
          AND material_doc_year = @is_send-material_doc_year
      INTO @DATA(ls_pt113).
      IF sy-subrc <> 0.
*         OR lv_usnam IS INITIAL.
        MESSAGE w027(zmmint0003_msg) WITH is_send-material_doc_num INTO DATA(lv_dummy).
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
        RETURN.
      ELSE.
        lv_usnam = ls_pt113-changed_by.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_usnam
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.
    ls_ztmail_text-text_msg = 'Уважаемый(-ая) &2, Информируем Вас о том, что возникли следующие предупреждения по документу &3:'.
    ls_ztmail_text-topic_msg = 'Предупреждения обработки документа &3 ПТ112'.
    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.
      lv_msg_text = ls_ztmail_text-text_msg.
      REPLACE '&2' IN lv_msg_text WITH ls_address-fullname.
      REPLACE '&3' IN lv_msg_text WITH is_send-material_doc_num.
      REPLACE '&3' IN ls_ztmail_text-topic_msg WITH is_send-material_doc_num.

      APPEND lv_msg_text TO lt_txt2.
      "запись сообщений
      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>) WHERE msgty = 'W'.
        IF sy-tabix <= mv_message_maxindex OR <ls_message>-msgid = 'ZMMINT0003_MSG' AND <ls_message>-msgno = 96
                                           OR <ls_message>-msgid = 'ZMMINT0003_MSG' AND <ls_message>-msgno = 97
                                           OR <ls_message>-msgid = 'ZFI0001_MSG' AND <ls_message>-msgno = 2.
          CONTINUE.
        ENDIF.

        MESSAGE ID <ls_message>-msgid TYPE 'W' NUMBER <ls_message>-msgno WITH <ls_message>-msgv1
                                                                              <ls_message>-msgv2
                                                                              <ls_message>-msgv3
                                                                              <ls_message>-msgv4
                                                                          INTO lv_dummy.
        APPEND lv_dummy TO lt_txt2.
      ENDLOOP.

      mv_message_maxindex = lines( lt_messages ).

      IF lt_txt2 IS INITIAL.
        RETURN.
      ENDIF.

      TRY.
          lo_send_request = cl_bcs=>create_persistent( ).
          lv_text = ls_ztmail_text-topic_msg.


          lo_document = cl_document_bcs=>create_document(
                            i_type    = 'RAW'
                            i_text    = lt_txt2
                            i_length  = '12'
                            i_subject = lv_text ).

*       add document to send request
          CALL METHOD lo_send_request->set_document( lo_document ).
*       add text to document
          CALL METHOD lo_send_request->set_note( note ).

          lo_sender = cl_cam_address_bcs=>create_internet_address( 'sap@rshb.ru' ).
          lo_send_request->set_sender( lo_sender ).
          lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_address-e_mail ).
*       add recipient with its respective attributes to send request
          lo_send_request->add_recipient( i_recipient = lo_recipient ).

*     ---------- send document ---------------------------------------
          CALL METHOD lo_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = sent_to_all ).
          IF sent_to_all = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.
        CATCH cx_bcs INTO bcs_exception.
          EXIT.

      ENDTRY.

    ELSEIF ls_address IS INITIAL.
      MESSAGE w029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      APPEND LINES OF lt_return TO et_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE w030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_USL_960
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_PT0113
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_usl_960.

    DATA: ls_flow_data TYPE zsint0003_pt0113_d,
          lt_flow_item TYPE ztt_int0003_pt0113_item_k,
          ls_register  TYPE zsint0003_pt0113_register,
          lt_key       TYPE /bobf/t_frw_key.

    DATA(lo_srv) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_ifc_c=>sc_bo_key ).
    DATA(lo_tra) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    IF is_data IS INITIAL.
      RETURN.
    ENDIF.
    IF is_data-originalaccountnumber IS INITIAL.
      RETURN.
    ENDIF.
**RRYCHEL

    ls_flow_data = CORRESPONDING #( is_data ).
    lt_flow_item = CORRESPONDING #( is_data-item ).
    DATA(lt_flow_prim) =  CORRESPONDING ztt_int0003_pt0111_primedocs_k( is_data-primedocs )."{KvizhinadzeVS 24.12.2023}

    ls_register = VALUE #( data = REF #( ls_flow_data )
                           addtabs_data = VALUE #( ( add_tab = 'PT0113_ITEM'
                                                     data    = REF #( lt_flow_item ) )
*{ KvizhinadzeVS 24.12.2023
                                                   ( add_tab = 'PT0113_PRIMEDOC'
                                                     data    = REF #( lt_flow_prim ) ) ) ).
*} KvizhinadzeVS 24.12.2023

*   -------------------------------
*    Регистрируем данные
*   -------------------------------
    lo_tra->cleanup( ).

    lo_srv->do_action(
      EXPORTING
        iv_act_key    = zif_ifc_c=>sc_action-pt0113-act_register
        is_parameters = REF #( ls_register )
      IMPORTING
        et_data       = lt_key ).

    lo_tra->save( ).
    "-------------------------------

*   -------------------------------
*    Отправляем данные
*   -------------------------------
    lo_tra->cleanup( ).

    lo_srv->do_action( iv_act_key = zif_ifc_c=>sc_action-pt0113-act_send
                       it_key     = lt_key ).

    lo_tra->save( ).
*   "-------------------------------


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_REJECT_NOTIFY_158
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_SEND
* | [<---] ET_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_reject_notify_158.
    CONSTANTS: lc_max      TYPE i VALUE 255,
               lc_old_text TYPE ze_text_msg VALUE 'Документ №&1 -отклонён в АБС ЦФТ, &4 &5, Необходимо Удалить СФ в АСУ ФХД'.

    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lv_usnam     TYPE usnam,
          lo_msg_cntrl TYPE REF TO if_reca_message_list,
          lv_msg_text  TYPE string,
          lv_message   TYPE string.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lt_text         TYPE bcsy_text,
          note            TYPE bcsy_text,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO cl_cam_address_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs,
          bcs_exception   TYPE REF TO cx_bcs,
          sent_to_all     TYPE os_boolean,
          lv_text         TYPE so_obj_des,
          ls_errors_res   TYPE zsrcmint0053_pt0122_resp,
          lv_error        TYPE abap_bool.

    DATA: lv_sum  TYPE i,
          lv_txt1 TYPE so_text255,
          lt_txt1 TYPE STANDARD TABLE OF so_text255,
          lv_len1 TYPE i,
          lv_txt2 TYPE so_text255,
          lt_txt2 TYPE bcsy_text,
          lv_len2 TYPE i.

    DATA: lv_starv_rbstat TYPE rbstat.

    DATA(lv_belnr) = is_send-material_doc_num.

    IF is_send-user_name IS INITIAL.

      SELECT SINGLE usnam
        FROM mkpf
        INTO @lv_usnam
        WHERE mblnr = @lv_belnr
          AND mjahr = @is_send-material_doc_year.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE usnam
          FROM bkpf
          INTO @lv_usnam
          WHERE belnr = @lv_belnr
            AND gjahr = @is_send-material_doc_year.

        IF sy-subrc <> 0.


          SELECT SINGLE usnam
            FROM anek
            INTO @lv_usnam
            WHERE belnr = @lv_belnr
              AND gjahr = @is_send-material_doc_year.

          IF sy-subrc <> 0.

            SELECT SINGLE usnam
              FROM rbkp
              INTO @lv_usnam
              WHERE belnr = @lv_belnr
                AND gjahr = @is_send-material_doc_year.

          ENDIF.

        ENDIF.

      ENDIF.


      IF sy-subrc <> 0 OR
         lv_usnam IS INITIAL.

        MESSAGE e027(zmmint0003_msg) WITH is_send-material_doc_num INTO DATA(lv_dummy).
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
        RETURN.
      ENDIF.

    ELSE.
      lv_usnam = is_send-user_name.
    ENDIF.

    SELECT SINGLE topic_msg, text_msg
      FROM ztmail_text
      INTO @DATA(ls_send_text)
      WHERE view_msg = @is_send-message_type. " 'ZMM_0114' или 'ZMM_0112'

    IF sy-subrc <> 0.
      MESSAGE e028(zmmint0003_msg) WITH is_send-message_type INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_RBSTAT' IMPORTING ev_value = lv_starv_rbstat ).
    SELECT SINGLE rbstat
      FROM rbkp
      WHERE belnr = @is_send-material_doc_num
        AND gjahr = @is_send-material_doc_year
      INTO @DATA(lv_rbstat).
    IF lv_rbstat <> lv_starv_rbstat.
      ls_send_text-text_msg = lc_old_text.
    ENDIF.

    SELECT SINGLE agreementid, order_num
        FROM ztint_pt0113
        WHERE material_doc_num = @is_send-material_doc_num
        AND material_doc_year = @is_send-material_doc_year
        INTO @DATA(ls_pt0111_6_7).



    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_usnam
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.

      lv_msg_text = ls_send_text-text_msg.

      "текст
      REPLACE '&1' IN lv_msg_text WITH is_send-material_doc_num.
      REPLACE '&2' IN lv_msg_text WITH ls_address-fullname.
      REPLACE '&4' IN lv_msg_text WITH is_send-reject_description.
      REPLACE '&5' IN lv_msg_text WITH is_send-reject_extended_info.
      REPLACE '&6' IN lv_msg_text WITH ls_pt0111_6_7-agreementid.
      REPLACE '&7' IN lv_msg_text WITH ls_pt0111_6_7-order_num.
      "тема
      REPLACE '&1' IN ls_send_text-topic_msg WITH is_send-material_doc_num.

      "деление сообщения на строки
      SPLIT lv_msg_text AT space INTO TABLE lt_txt1.
      LOOP AT lt_txt1 INTO lv_txt1.
        lv_len1 = strlen( lv_txt1 ).
*      ASSERT lv_len1 <= lc_max.

        IF lv_len1 > lc_max.
          WHILE lv_len1 > lc_max.
            IF lv_len2 IS INITIAL.
              lv_txt2 = lv_txt1+0(lc_max).
              APPEND lv_txt2 TO lt_txt2.
              CLEAR lv_txt2.
              SHIFT lv_txt1 LEFT BY lc_max PLACES.
              lv_len1 = strlen( lv_txt1 ).
            ELSE.
              APPEND lv_txt2 TO lt_txt2.
              CLEAR lv_len2.
            ENDIF.
          ENDWHILE.
        ENDIF.

        lv_len2 = strlen( lv_txt2 ).
        lv_sum = lv_len1 + lv_len2 + 1.
        IF lv_sum <= lc_max.
          CONCATENATE lv_txt2 lv_txt1 INTO lv_txt2 SEPARATED BY space.
          CONDENSE lv_txt2.
        ELSE.
          APPEND lv_txt2 TO lt_txt2.
          lv_txt2 = lv_txt1.
        ENDIF.
      ENDLOOP.
      APPEND lv_txt2 TO lt_txt2.

      TRY.
          lo_send_request = cl_bcs=>create_persistent( ).
          lv_text = ls_send_text-topic_msg.


          lo_document = cl_document_bcs=>create_document(
                            i_type    = 'RAW'
                            i_text    = lt_txt2
                            i_length  = '12'
                            i_subject = lv_text ).

*       add document to send request
          CALL METHOD lo_send_request->set_document( lo_document ).
*       add text to document
          CALL METHOD lo_send_request->set_note( note ).

          lo_sender = cl_cam_address_bcs=>create_internet_address( 'sap@rshb.ru' ).
          lo_send_request->set_sender( lo_sender ).
          lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_address-e_mail ).
*       add recipient with its respective attributes to send request
          lo_send_request->add_recipient( i_recipient = lo_recipient ).

*     ---------- send document ---------------------------------------
          CALL METHOD lo_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = sent_to_all ).
          IF sent_to_all = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.
        CATCH cx_bcs INTO bcs_exception.
          EXIT.

      ENDTRY.

    ELSEIF ls_address IS INITIAL.
      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      APPEND LINES OF lt_return TO et_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

*    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.
*
*      APPEND INITIAL LINE TO lt_mail_tab ASSIGNING FIELD-SYMBOL(<ls_mail_tab>).
*
*      <ls_mail_tab>-email              = ls_address-e_mail.
*      <ls_mail_tab>-topic_msg          = ls_send_text-topic_msg.
*      <ls_mail_tab>-text_msg           = ls_send_text-text_msg.
*      <ls_mail_tab>-placeholders_body  = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
*                                                                             data_placeholders = is_send-material_doc_num )
*                                                                           ( name_placeholders = '&2'
*                                                                             data_placeholders = ls_address-fullname )
*                                                                           ( name_placeholders = '&3'
*                                                                             data_placeholders = is_send-os )
*                                                                           ( name_placeholders = '&4'
*                                                                             data_placeholders = is_send-reject_description )
*                                                                           ( name_placeholders = '&5'
*                                                                             data_placeholders = is_send-reject_extended_info ) ).
*      <ls_mail_tab>-placeholders_topic = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
*                                                                             data_placeholders = is_send-material_doc_num ) ).
*    ELSEIF ls_address IS INITIAL.
*      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
*      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*      APPEND LINES OF lt_return TO et_message.
*      RETURN.
*    ELSEIF ls_address-e_mail IS INITIAL.
*      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
*      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*      RETURN.
*    ENDIF.


*    TRY.
*        CALL FUNCTION 'Z_SEND_EMAIL'
*          EXPORTING
*            iv_immediately = abap_true
*          TABLES
*            it_mail_send   = lt_mail_tab.
*
*        COMMIT WORK AND WAIT.
*
*        MESSAGE s018(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
*        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*
*      CATCH cx_root INTO DATA(lx_bcs).
*
*        MESSAGE e031(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
*        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*
*        lo_msg_cntrl = cf_reca_message_list=>create( ).
*        lo_msg_cntrl->add_from_exception( lx_bcs ).
*        lo_msg_cntrl->get_list_as_bapiret( IMPORTING et_list = lt_return ).
*        APPEND LINES OF lt_return TO et_message.
*
*    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_REJECT_NOTIFY_114
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_SEND
* | [<---] ET_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_reject_notify_114.

    TYPES: BEGIN OF lty_s_send_text,
             topic_msg TYPE ztmail_text-topic_msg,
             text_msg  TYPE ztmail_text-text_msg,
           END OF lty_s_send_text.


    CONSTANTS: lc_max         TYPE i VALUE 255,
*               lc_old_text    TYPE ze_text_msg VALUE 'Документ №&1 -отклонён в АБС ЦФТ, &4 &5, Автоудаление СФ выполнено на стороне АСУ ФХД',
               lc_old_text2   TYPE ze_text_msg VALUE 'Уважаемый(-ая) &2, Информируем Вас о том, что договор &6  Заказ на поставку &7    Документ №&1 -отклонён в АБС ЦФТ, &4 &5, Необходимо Удалить СФ в АСУ ФХД',
               lc_old_text_49 TYPE ze_text_msg VALUE 'Уважаемый(-ая) &2, Информируем Вас о том, что договор &6  Заказ на поставку &7  «Документ №&1 -отклонён в АБС ЦФТ, &4 &5 Автосторно  документа выполнено на стороне АСУ ФХД',
               lc_new_text_49 TYPE ze_text_msg VALUE 'Уважаемый(-ая) &2, Информируем Вас о том, что «Документ №&1 -отклонён в АБС ЦФТ, &4 &5 Автосторно  документа выполнено на стороне АСУ ФХД',
               lc_shb_izlish  TYPE ze_text_msg VALUE 'Уважаемый &2 Информируем Вас о том, что  «Документ №&1 -отклонён в АБС ЦФТ, &4 &5 -необходимо выполнить операцию инвентаризации «Недостача».',
               lc_doc_code    TYPE ze_code_doc_abc VALUE 'SHB_ИЗЛИШКИ',
               lc_zmm_0114    TYPE ze_view_msg VALUE 'ZMM_0114',
               lc_rbstat      TYPE string VALUE 'ZMM003_RBSTAT',
               lc_raw         TYPE so_obj_tp VALUE 'RAW',
               lc_length_12   TYPE so_obj_len VALUE '12'.


    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lv_usnam     TYPE usnam,
          lo_msg_cntrl TYPE REF TO if_reca_message_list,
          lv_msg_text  TYPE string,
          lv_message   TYPE string.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lt_text         TYPE bcsy_text,
          note            TYPE bcsy_text,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO cl_cam_address_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs,
          bcs_exception   TYPE REF TO cx_bcs,
          sent_to_all     TYPE os_boolean,
          lv_text         TYPE so_obj_des,
          ls_errors_res   TYPE zsrcmint0053_pt0122_resp,
          lv_error        TYPE abap_bool.

    DATA: lv_sum  TYPE i,
          lv_txt1 TYPE so_text255,
          lt_txt1 TYPE STANDARD TABLE OF so_text255,
          lv_len1 TYPE i,
          lv_txt2 TYPE so_text255,
          lt_txt2 TYPE bcsy_text,
          lv_len2 TYPE i.

    DATA: lv_starv_rbstat TYPE rbstat.
    DATA(lv_belnr) = is_send-material_doc_num.
    DATA ls_send_text TYPE lty_s_send_text.

    IF is_send-user_name IS INITIAL.

      SELECT SINGLE usnam
        FROM mkpf
        INTO @lv_usnam
        WHERE mblnr = @lv_belnr
          AND mjahr = @is_send-material_doc_year.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE usnam
          FROM bkpf
          INTO @lv_usnam
          WHERE belnr = @lv_belnr
            AND gjahr = @is_send-material_doc_year.

        IF sy-subrc <> 0.


          SELECT SINGLE usnam
            FROM anek
            INTO @lv_usnam
            WHERE belnr = @lv_belnr
              AND gjahr = @is_send-material_doc_year.

          IF sy-subrc <> 0.

            SELECT SINGLE usnam
              FROM rbkp
              INTO @lv_usnam
              WHERE belnr = @lv_belnr
                AND gjahr = @is_send-material_doc_year.

          ENDIF.

        ENDIF.

      ENDIF.

      IF sy-subrc <> 0 OR
         lv_usnam IS INITIAL.

        MESSAGE e027(zmmint0003_msg) WITH is_send-material_doc_num INTO DATA(lv_dummy).
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
        RETURN.
      ENDIF.

    ELSE.
      lv_usnam = is_send-user_name.
    ENDIF.



    SELECT SINGLE topic_msg
      FROM ztmail_text
      INTO @ls_send_text-topic_msg
      WHERE view_msg = @is_send-message_type. " 'ZMM_0114' или 'ZMM_0112'

    IF sy-subrc <> 0.
      MESSAGE e028(zmmint0003_msg) WITH is_send-message_type INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

    IF is_send-message_type = lc_zmm_0114.
      zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = lc_rbstat IMPORTING ev_value = lv_starv_rbstat ).

      SELECT SINGLE rbkp~rbstat
        FROM ztint_pt0111
        INNER JOIN rbkp
        ON belnr = ztint_pt0111~materialdocnum_from
        AND gjahr = ztint_pt0111~material_doc_year
        WHERE material_doc_num = @is_send-material_doc_num
          AND material_doc_year = @is_send-material_doc_year
        INTO @DATA(lv_rbstat).

      IF lv_rbstat <> 5.
        ls_send_text-text_msg = lc_old_text_49.
      ELSE.
        ls_send_text-text_msg = lc_old_text2.
      ENDIF.

      SELECT SINGLE material_doc_num
        FROM ztint_pt0111
        INTO @DATA(lv_vbeln111)
        WHERE material_doc_num = @is_send-material_doc_num
          AND material_doc_code = @lc_doc_code.
      IF lv_vbeln111 IS NOT INITIAL.
        ls_send_text-text_msg = lc_shb_izlish.
      ENDIF.

      SELECT SINGLE agreement_id, order_num
        FROM ztint_pt0111
        WHERE material_doc_num = @is_send-material_doc_num
        AND material_doc_year = @is_send-material_doc_year
        INTO @DATA(ls_pt0111_5_6).
      IF ls_pt0111_5_6 IS INITIAL.
        ls_send_text-text_msg = lc_new_text_49.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_usnam
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.

      lv_msg_text = ls_send_text-text_msg.

      "текст
      REPLACE '&1' IN lv_msg_text WITH is_send-material_doc_num.
      REPLACE '&2' IN lv_msg_text WITH ls_address-fullname.
      REPLACE '&3' IN lv_msg_text WITH is_send-os.
      REPLACE '&4' IN lv_msg_text WITH is_send-reject_description.
      REPLACE '&5' IN lv_msg_text WITH is_send-reject_extended_info.
      REPLACE '&6' IN lv_msg_text WITH ls_pt0111_5_6-agreement_id.
      REPLACE '&7' IN lv_msg_text WITH ls_pt0111_5_6-order_num.

      "тема
      REPLACE '&1' IN ls_send_text-topic_msg WITH is_send-material_doc_num.

      "деление сообщения на строки
      SPLIT lv_msg_text AT space INTO TABLE lt_txt1.
      LOOP AT lt_txt1 INTO lv_txt1.
        lv_len1 = strlen( lv_txt1 ).

        IF lv_len1 > lc_max.
          WHILE lv_len1 > lc_max.
            IF lv_len2 IS INITIAL.
              lv_txt2 = lv_txt1+0(lc_max).
              APPEND lv_txt2 TO lt_txt2.
              CLEAR lv_txt2.
              SHIFT lv_txt1 LEFT BY lc_max PLACES.
              lv_len1 = strlen( lv_txt1 ).
            ELSE.
              APPEND lv_txt2 TO lt_txt2.
              CLEAR lv_len2.
            ENDIF.
          ENDWHILE.
        ENDIF.

        lv_len2 = strlen( lv_txt2 ).
        lv_sum = lv_len1 + lv_len2 + 1.
        IF lv_sum <= lc_max.
          CONCATENATE lv_txt2 lv_txt1 INTO lv_txt2 SEPARATED BY space.
          CONDENSE lv_txt2.
        ELSE.
          APPEND lv_txt2 TO lt_txt2.
          lv_txt2 = lv_txt1.
        ENDIF.
      ENDLOOP.
      APPEND lv_txt2 TO lt_txt2.

      TRY.
          lo_send_request = cl_bcs=>create_persistent( ).
          lv_text = ls_send_text-topic_msg.


          lo_document = cl_document_bcs=>create_document(
                            i_type    = lc_raw
                            i_text    = lt_txt2
                            i_length  = lc_length_12
                            i_subject = lv_text ).

*       add document to send request
          CALL METHOD lo_send_request->set_document( lo_document ).
*       add text to document
          CALL METHOD lo_send_request->set_note( note ).

          lo_sender = cl_cam_address_bcs=>create_internet_address( 'sap@rshb.ru' ).
          lo_send_request->set_sender( lo_sender ).
          lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_address-e_mail ).
*       add recipient with its respective attributes to send request
          lo_send_request->add_recipient( i_recipient = lo_recipient ).

*     ---------- send document ---------------------------------------
          CALL METHOD lo_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = sent_to_all ).
          IF sent_to_all = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.
        CATCH cx_bcs INTO bcs_exception.
          EXIT.

      ENDTRY.

    ELSEIF ls_address IS INITIAL.
      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      APPEND LINES OF lt_return TO et_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

*    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.
*
*      APPEND INITIAL LINE TO lt_mail_tab ASSIGNING FIELD-SYMBOL(<ls_mail_tab>).
*
*      <ls_mail_tab>-email              = ls_address-e_mail.
*      <ls_mail_tab>-topic_msg          = ls_send_text-topic_msg.
*      <ls_mail_tab>-text_msg           = ls_send_text-text_msg.
*      <ls_mail_tab>-placeholders_body  = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
*                                                                             data_placeholders = is_send-material_doc_num )
*                                                                           ( name_placeholders = '&2'
*                                                                             data_placeholders = ls_address-fullname )
*                                                                           ( name_placeholders = '&3'
*                                                                             data_placeholders = is_send-os )
*                                                                           ( name_placeholders = '&4'
*                                                                             data_placeholders = is_send-reject_description )
*                                                                           ( name_placeholders = '&5'
*                                                                             data_placeholders = is_send-reject_extended_info ) ).
*      <ls_mail_tab>-placeholders_topic = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
*                                                                             data_placeholders = is_send-material_doc_num ) ).
*    ELSEIF ls_address IS INITIAL.
*      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
*      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*      APPEND LINES OF lt_return TO et_message.
*      RETURN.
*    ELSEIF ls_address-e_mail IS INITIAL.
*      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
*      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*      RETURN.
*    ENDIF.


*    TRY.
*        CALL FUNCTION 'Z_SEND_EMAIL'
*          EXPORTING
*            iv_immediately = abap_true
*          TABLES
*            it_mail_send   = lt_mail_tab.
*
*        COMMIT WORK AND WAIT.
*
*        MESSAGE s018(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
*        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*
*      CATCH cx_root INTO DATA(lx_bcs).
*
*        MESSAGE e031(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
*        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*
*        lo_msg_cntrl = cf_reca_message_list=>create( ).
*        lo_msg_cntrl->add_from_exception( lx_bcs ).
*        lo_msg_cntrl->get_list_as_bapiret( IMPORTING et_list = lt_return ).
*        APPEND LINES OF lt_return TO et_message.
*
*    ENDTRY.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_REJECT_NOTIFY_112
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_SEND
* | [<---] ET_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SEND_REJECT_NOTIFY_112.
    CONSTANTS: lc_max      TYPE i VALUE 255,
               lc_old_text TYPE ze_text_msg VALUE 'Документ №&1 -отклонён в АБС ЦФТ, &4 &5, Необходимо Удалить СФ в АСУ ФХД'.

    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lv_usnam     TYPE usnam,
          lo_msg_cntrl TYPE REF TO if_reca_message_list,
          lv_msg_text  TYPE string,
          lv_message   TYPE string.

    DATA: lo_send_request TYPE REF TO cl_bcs,
          lt_text         TYPE bcsy_text,
          note            TYPE bcsy_text,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO cl_cam_address_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs,
          bcs_exception   TYPE REF TO cx_bcs,
          sent_to_all     TYPE os_boolean,
          lv_text         TYPE so_obj_des,
          ls_errors_res   TYPE zsrcmint0053_pt0122_resp,
          lv_error        TYPE abap_bool.

    DATA: lv_sum  TYPE i,
          lv_txt1 TYPE so_text255,
          lt_txt1 TYPE STANDARD TABLE OF so_text255,
          lv_len1 TYPE i,
          lv_txt2 TYPE so_text255,
          lt_txt2 TYPE bcsy_text,
          lv_len2 TYPE i.

    DATA: lv_starv_rbstat TYPE rbstat.

    DATA(lv_belnr) = is_send-material_doc_num.

    IF is_send-user_name IS INITIAL.

      SELECT SINGLE usnam
        FROM mkpf
        INTO @lv_usnam
        WHERE mblnr = @lv_belnr
          AND mjahr = @is_send-material_doc_year.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE usnam
          FROM bkpf
          INTO @lv_usnam
          WHERE belnr = @lv_belnr
            AND gjahr = @is_send-material_doc_year.

        IF sy-subrc <> 0.


          SELECT SINGLE usnam
            FROM anek
            INTO @lv_usnam
            WHERE belnr = @lv_belnr
              AND gjahr = @is_send-material_doc_year.

          IF sy-subrc <> 0.

            SELECT SINGLE usnam
              FROM rbkp
              INTO @lv_usnam
              WHERE belnr = @lv_belnr
                AND gjahr = @is_send-material_doc_year.

          ENDIF.

        ENDIF.

      ENDIF.


      IF sy-subrc <> 0 OR
         lv_usnam IS INITIAL.

        MESSAGE e027(zmmint0003_msg) WITH is_send-material_doc_num INTO DATA(lv_dummy).
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
        RETURN.
      ENDIF.

    ELSE.
      lv_usnam = is_send-user_name.
    ENDIF.

    SELECT SINGLE topic_msg, text_msg
      FROM ztmail_text
      INTO @DATA(ls_send_text)
      WHERE view_msg = @is_send-message_type. " 'ZMM_0114' или 'ZMM_0112'

*    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_RBSTAT' IMPORTING ev_value = lv_starv_rbstat ).

    IF sy-subrc <> 0.
      MESSAGE e028(zmmint0003_msg) WITH is_send-message_type INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.


    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_usnam
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.

      lv_msg_text = ls_send_text-text_msg.
*    lv_message = '5105603175 / 2023 Ошибок нет. Для хоз. договора id = 530123776724 создан календарь закрытия за дату 16.06.2023 id = 698793795200'.

      "текст
      REPLACE '&1' IN lv_msg_text WITH is_send-material_doc_num.
      REPLACE '&2' IN lv_msg_text WITH ls_address-fullname.
      REPLACE '&4' IN lv_msg_text WITH is_send-reject_description.
      REPLACE '&5' IN lv_msg_text WITH is_send-reject_extended_info.
      "тема
      REPLACE '&1' IN ls_send_text-topic_msg WITH is_send-material_doc_num.

      "деление сообщения на строки
      SPLIT lv_msg_text AT space INTO TABLE lt_txt1.
      LOOP AT lt_txt1 INTO lv_txt1.
        lv_len1 = strlen( lv_txt1 ).
*      ASSERT lv_len1 <= lc_max.

        IF lv_len1 > lc_max.
          WHILE lv_len1 > lc_max.
            IF lv_len2 IS INITIAL.
              lv_txt2 = lv_txt1+0(lc_max).
              APPEND lv_txt2 TO lt_txt2.
              CLEAR lv_txt2.
              SHIFT lv_txt1 LEFT BY lc_max PLACES.
              lv_len1 = strlen( lv_txt1 ).
            ELSE.
              APPEND lv_txt2 TO lt_txt2.
              CLEAR lv_len2.
            ENDIF.
          ENDWHILE.
        ENDIF.

        lv_len2 = strlen( lv_txt2 ).
        lv_sum = lv_len1 + lv_len2 + 1.
        IF lv_sum <= lc_max.
          CONCATENATE lv_txt2 lv_txt1 INTO lv_txt2 SEPARATED BY space.
          CONDENSE lv_txt2.
        ELSE.
          APPEND lv_txt2 TO lt_txt2.
          lv_txt2 = lv_txt1.
        ENDIF.
      ENDLOOP.
      APPEND lv_txt2 TO lt_txt2.

      TRY.
          lo_send_request = cl_bcs=>create_persistent( ).
          lv_text = ls_send_text-topic_msg.


          lo_document = cl_document_bcs=>create_document(
                            i_type    = 'RAW'
                            i_text    = lt_txt2
                            i_length  = '12'
                            i_subject = lv_text ).

*       add document to send request
          CALL METHOD lo_send_request->set_document( lo_document ).
*       add text to document
          CALL METHOD lo_send_request->set_note( note ).

          lo_sender = cl_cam_address_bcs=>create_internet_address( 'sap@rshb.ru' ).
          lo_send_request->set_sender( lo_sender ).
          lo_recipient = cl_cam_address_bcs=>create_internet_address( ls_address-e_mail ).
*       add recipient with its respective attributes to send request
          lo_send_request->add_recipient( i_recipient = lo_recipient ).

*     ---------- send document ---------------------------------------
          CALL METHOD lo_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = sent_to_all ).
          IF sent_to_all = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.
        CATCH cx_bcs INTO bcs_exception.
          EXIT.

      ENDTRY.

    ELSEIF ls_address IS INITIAL.
      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      APPEND LINES OF lt_return TO et_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

*    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.
*
*      APPEND INITIAL LINE TO lt_mail_tab ASSIGNING FIELD-SYMBOL(<ls_mail_tab>).
*
*      <ls_mail_tab>-email              = ls_address-e_mail.
*      <ls_mail_tab>-topic_msg          = ls_send_text-topic_msg.
*      <ls_mail_tab>-text_msg           = ls_send_text-text_msg.
*      <ls_mail_tab>-placeholders_body  = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
*                                                                             data_placeholders = is_send-material_doc_num )
*                                                                           ( name_placeholders = '&2'
*                                                                             data_placeholders = ls_address-fullname )
*                                                                           ( name_placeholders = '&3'
*                                                                             data_placeholders = is_send-os )
*                                                                           ( name_placeholders = '&4'
*                                                                             data_placeholders = is_send-reject_description )
*                                                                           ( name_placeholders = '&5'
*                                                                             data_placeholders = is_send-reject_extended_info ) ).
*      <ls_mail_tab>-placeholders_topic = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
*                                                                             data_placeholders = is_send-material_doc_num ) ).
*    ELSEIF ls_address IS INITIAL.
*      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
*      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*      APPEND LINES OF lt_return TO et_message.
*      RETURN.
*    ELSEIF ls_address-e_mail IS INITIAL.
*      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
*      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*      RETURN.
*    ENDIF.


*    TRY.
*        CALL FUNCTION 'Z_SEND_EMAIL'
*          EXPORTING
*            iv_immediately = abap_true
*          TABLES
*            it_mail_send   = lt_mail_tab.
*
*        COMMIT WORK AND WAIT.
*
*        MESSAGE s018(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
*        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*
*      CATCH cx_root INTO DATA(lx_bcs).
*
*        MESSAGE e031(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
*        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
*
*        lo_msg_cntrl = cf_reca_message_list=>create( ).
*        lo_msg_cntrl->add_from_exception( lx_bcs ).
*        lo_msg_cntrl->get_list_as_bapiret( IMPORTING et_list = lt_return ).
*        APPEND LINES OF lt_return TO et_message.
*
*    ENDTRY.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_REJECT_NOTIFY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_SEND
* | [<---] ET_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_reject_notify.

    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lv_usnam     TYPE usnam,
          lo_msg_cntrl TYPE REF TO if_reca_message_list.

    DATA(lv_belnr) = is_send-material_doc_num.

    CLEAR et_message.

    IF is_send-user_name IS INITIAL.

      SELECT SINGLE usnam
        FROM mkpf
        INTO @lv_usnam
        WHERE mblnr = @lv_belnr
          AND mjahr = @is_send-material_doc_year.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE usnam
          FROM bkpf
          INTO @lv_usnam
          WHERE belnr = @lv_belnr
            AND gjahr = @is_send-material_doc_year.

        IF sy-subrc <> 0.


          SELECT SINGLE usnam
            FROM anek
            INTO @lv_usnam
            WHERE belnr = @lv_belnr
              AND gjahr = @is_send-material_doc_year.

          IF sy-subrc <> 0.

            SELECT SINGLE usnam
              FROM rbkp
              INTO @lv_usnam
              WHERE belnr = @lv_belnr
                AND gjahr = @is_send-material_doc_year.

          ENDIF.

        ENDIF.

      ENDIF.


      IF sy-subrc <> 0 OR
         lv_usnam IS INITIAL.

        MESSAGE e027(zmmint0003_msg) WITH is_send-material_doc_num INTO DATA(lv_dummy).
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
        RETURN.
      ENDIF.

    ELSE.
      lv_usnam = is_send-user_name.
    ENDIF.


    SELECT SINGLE topic_msg, text_msg
      FROM ztmail_text
      INTO @DATA(ls_send_text)
      WHERE view_msg = @is_send-message_type. " 'ZMM_0114' или 'ZMM_0112'

    IF sy-subrc <> 0.
      MESSAGE e028(zmmint0003_msg) WITH is_send-message_type INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.


    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_usnam
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_mail_tab ASSIGNING FIELD-SYMBOL(<ls_mail_tab>).

      <ls_mail_tab>-email              = ls_address-e_mail.
      <ls_mail_tab>-topic_msg          = ls_send_text-topic_msg.
      <ls_mail_tab>-text_msg           = ls_send_text-text_msg.
      <ls_mail_tab>-placeholders_body  = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
                                                                             data_placeholders = is_send-material_doc_num )
                                                                           ( name_placeholders = '&2'
                                                                             data_placeholders = ls_address-fullname )
                                                                           ( name_placeholders = '&3'
                                                                             data_placeholders = is_send-os )
                                                                           ( name_placeholders = '&4'
                                                                             data_placeholders = is_send-reject_description )
                                                                           ( name_placeholders = '&5'
                                                                             data_placeholders = is_send-reject_extended_info ) ).
      <ls_mail_tab>-placeholders_topic = VALUE ztt_mail_placeholders_data( ( name_placeholders = '&1'
                                                                             data_placeholders = is_send-material_doc_num ) ).
    ELSEIF ls_address IS INITIAL.
      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      APPEND LINES OF lt_return TO et_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.


    TRY.
        CALL FUNCTION 'Z_SEND_EMAIL'
          EXPORTING
            iv_immediately = abap_true
          TABLES
            it_mail_send   = lt_mail_tab.

        COMMIT WORK AND WAIT.

        MESSAGE s018(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).

      CATCH cx_root INTO DATA(lx_bcs).

        MESSAGE e031(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).

        lo_msg_cntrl = cf_reca_message_list=>create( ).
        lo_msg_cntrl->add_from_exception( lx_bcs ).
        lo_msg_cntrl->get_list_as_bapiret( IMPORTING et_list = lt_return ).
        APPEND LINES OF lt_return TO et_message.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_NOTIFY_RE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_RE_SEND
* | [<-()] RT_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SEND_NOTIFY_RE.

    CONSTANTS: lc_view_msg TYPE ztmail_text-view_msg VALUE 'RE_INT-3'.

    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lo_msg_cntrl TYPE REF TO if_reca_message_list.




    SELECT SINGLE topic_msg, text_msg
      FROM ztmail_text
      INTO @DATA(ls_send_text)
      WHERE view_msg = @lc_view_msg.

    IF sy-subrc <> 0.
      MESSAGE e028(zmmint0003_msg) WITH lc_view_msg INTO DATA(lv_dummy).
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).
      RETURN.
    ENDIF.


    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = is_send-user_name
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_mail_tab ASSIGNING FIELD-SYMBOL(<ls_mail_tab>).

      <ls_mail_tab>-email              = ls_address-e_mail.
      <ls_mail_tab>-topic_msg          = ls_send_text-topic_msg.
      <ls_mail_tab>-text_msg           = ls_send_text-text_msg.
      <ls_mail_tab>-placeholders_body  = VALUE ztt_mail_placeholders_data( ( name_placeholders = '[ABC]'
                                                                             data_placeholders = is_send-cft_agreement_id )
                                                                           ( name_placeholders = '[AGR]'
                                                                             data_placeholders = is_send-rcm_agreement_id )
                                                                           ( name_placeholders = '[TEXT]'
                                                                             data_placeholders = is_send-error_text ) ).
    ELSEIF ls_address IS INITIAL.
      MESSAGE e029(zmmint0003_msg) WITH is_send-user_name INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).
      APPEND LINES OF lt_return TO rt_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE e030(zmmint0003_msg) WITH is_send-user_name INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).
      RETURN.
    ENDIF.


    TRY.
        CALL FUNCTION 'Z_SEND_EMAIL'
          EXPORTING
            iv_immediately = abap_true
          TABLES
            it_mail_send   = lt_mail_tab.

        COMMIT WORK AND WAIT.

        MESSAGE s018(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).

      CATCH cx_root INTO DATA(lx_bcs).

        MESSAGE e031(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).

        lo_msg_cntrl = cf_reca_message_list=>create( ).
        lo_msg_cntrl->add_from_exception( lx_bcs ).
        lo_msg_cntrl->get_list_as_bapiret( IMPORTING et_list = lt_return ).
        APPEND LINES OF lt_return TO rt_message.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_NOTIFY_PT0111_PT0113
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_PT0111_113_SEND
* | [<-()] RT_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_notify_pt0111_pt0113.

*    CONSTANTS: lc_view_msg TYPE ztmail_text-view_msg VALUE 'ZMM_0111'.

    DATA: ls_address   TYPE bapiaddr3,
          lt_mail_tab  TYPE ztt_mail_send,
          lt_return    TYPE bapiret2_t,
          lo_msg_cntrl TYPE REF TO if_reca_message_list,
          lv_len       TYPE i,
          lv_doc_id    TYPE string,
          lv_user_name TYPE string.



    SELECT SINGLE topic_msg, text_msg
      FROM ztmail_text
      INTO @DATA(ls_send_text)
      WHERE view_msg = @is_send-view_msg.

    IF sy-subrc <> 0.
      MESSAGE w028(zmmint0003_msg) WITH is_send-view_msg INTO DATA(lv_dummy).
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).
      RETURN.
    ENDIF.


    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = is_send-user_name
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_mail_tab ASSIGNING FIELD-SYMBOL(<ls_mail_tab>).

      <ls_mail_tab>-email              = ls_address-e_mail.
      <ls_mail_tab>-topic_msg          = ls_send_text-topic_msg.

      "{ 18.01.2023
      lv_doc_id = is_send-doc_id.
      lv_user_name = ls_address-firstname.

      REPLACE '&1' WITH lv_doc_id INTO <ls_mail_tab>-topic_msg.

*      <ls_mail_tab>-text_msg           = REDUCE #( INIT text = ''
*                                                   FOR ls_wa IN is_send-errors
*                                                   NEXT text &&= ls_wa-error_descr ).

      <ls_mail_tab>-text_msg = ls_send_text-text_msg.

      REPLACE '&1' WITH lv_doc_id INTO <ls_mail_tab>-text_msg.
      REPLACE '&2' WITH lv_user_name INTO <ls_mail_tab>-text_msg.

*      <ls_mail_tab>-text_msg = |{ <ls_mail_tab>-text_msg } |.
*      LOOP AT is_send-errors[] INTO DATA(ls_errors).
*        <ls_mail_tab>-text_msg = <ls_mail_tab>-text_msg && ls_errors-error_descr.
*      ENDLOOP.

      "}. " 18.01.2023
    ELSEIF ls_address IS INITIAL.
      MESSAGE w029(zmmint0003_msg) WITH is_send-user_name INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).
      APPEND LINES OF lt_return TO rt_message.
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE w030(zmmint0003_msg) WITH is_send-user_name INTO lv_dummy.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).
      RETURN.
    ENDIF.


    TRY.
        CALL FUNCTION 'Z_SEND_EMAIL'
          EXPORTING
            iv_immediately = abap_true
          TABLES
            it_mail_send   = lt_mail_tab.

        COMMIT WORK AND WAIT.

        MESSAGE w018(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).

      CATCH cx_root INTO DATA(lx_bcs).

        MESSAGE w031(zmmint0003_msg) WITH ls_address-e_mail INTO lv_dummy.
        cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = rt_message ).

        lo_msg_cntrl = cf_reca_message_list=>create( ).
        lo_msg_cntrl->add_from_exception( lx_bcs ).
        lo_msg_cntrl->get_list_as_bapiret( IMPORTING et_list = lt_return ).
        APPEND LINES OF lt_return TO rt_message.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_MIR7_230
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ZSINT0003_PT0136_PLCAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_mir7_230.
*    DATA: ls_flow_data TYPE zsint0003_pt0136_d,
*          lt_flow_item TYPE ztt_int0003_pt0136_wo_k,
*          ls_register  TYPE zsint0003_pt0136_register,
*          lt_key       TYPE /bobf/t_frw_key.
*
*    DATA(lo_srv) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_ifc_c=>sc_bo_key ).
*    DATA(lo_tra) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
*
*    IF is_data IS INITIAL.
*      RETURN.
*    ENDIF.
**    "меппим данные
**    "-------------------------------
*    ls_flow_data = VALUE #( balanceunit         = is_data-balanceunit
*                            order_num           = is_data-ordernum
*                            agreementid         = is_data-agreementid
*                            cftagreement_id     = is_data-cftagreementid
*                            conuterpartyid      = is_data-conuterpartyid
*                            amount              = is_data-amount
*                            agreementstartdate  = is_data-agreementstartdate
*                            agreementenddate    = is_data-agreementenddate
*                            periodcode          = is_data-periodcode ).
*
*    LOOP AT is_data-writeofflist ASSIGNING FIELD-SYMBOL(<ls_writeofflist>).
*
*      APPEND VALUE #( periodstartdate = <ls_writeofflist>-periodstartdate
*                      periodenddate   = <ls_writeofflist>-periodenddate
*                      amount_wo       = <ls_writeofflist>-amount_wo
*                      currencycode    = <ls_writeofflist>-currencycode
*                      ordernum_wo     = <ls_writeofflist>-ordernum_wo
*                      positionnum     = <ls_writeofflist>-positionnum
*                      materialcode    = <ls_writeofflist>-materialcode ) TO lt_flow_item.
*
*    ENDLOOP.
*
*    ls_register = VALUE #( data         = REF #( ls_flow_data )
*                           addtabs_data = VALUE #( ( add_tab = 'PT0136_WRITEOFFL'
*                                                     data    = REF #( lt_flow_item ) ) ) ).
**    -------------------------------
**    Регистрируем данные
**    -------------------------------
*    lo_tra->cleanup( ).
*
*    lo_srv->do_action(
*      EXPORTING
*        iv_act_key    = zif_ifc_c=>sc_action-pt0136-act_register
*        is_parameters = REF #( ls_register )
*      IMPORTING
*        et_data       = lt_key ).
*
*    lo_tra->save(
*      IMPORTING
*        ev_rejected   = DATA(lv_rejected) ##NEEDED
*        eo_message    = DATA(lo_message) ) ##NEEDED .
*    "-------------------------------
**
**    "-------------------------------
*    lo_tra->cleanup( ).
*    lo_srv->do_action( iv_act_key = zif_ifc_c=>sc_action-pt0136-act_send
*                       it_key     = lt_key ).
*    lo_tra->save( ).
**    "-------------------------------

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_MESSAGES_FROM_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SEND                        TYPE        TY_SEND
* | [--->] IO_LOG                         TYPE REF TO ZCL_PI_LOG
* | [<---] ET_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_messages_from_log.

*    CONSTANTS: lc_max      TYPE i VALUE 255,
*               lc_old_text TYPE ze_text_msg VALUE 'Документ №&1 -отклонён в АБС ЦФТ, &4 &5, Необходимо Удалить СФ в АСУ ФХД'.

*    DATA: "ls_address   TYPE bapiaddr3,
*      lt_mail_tab  TYPE ztt_mail_send,
      "lt_return    TYPE bapiret2_t,
*          lv_usnam     TYPE usnam,
*      lo_msg_cntrl TYPE REF TO if_reca_message_list,
*      lv_msg_text  TYPE string,
*      lv_message   TYPE string.

***    DATA: "lo_send_request TYPE REF TO cl_bcs,
***      lt_text       TYPE bcsy_text,
***      note          TYPE bcsy_text,
***      "lo_document     TYPE REF TO cl_document_bcs,
***      "lo_sender       TYPE REF TO cl_cam_address_bcs,
***      "lo_recipient    TYPE REF TO if_recipient_bcs,
***      bcs_exception TYPE REF TO cx_bcs,
***      sent_to_all   TYPE os_boolean,
***      lv_text       TYPE so_obj_des,
***      ls_errors_res TYPE zsrcmint0053_pt0122_resp,
***      lv_error      TYPE abap_bool.
***
***    DATA: lv_sum  TYPE i,
***          lv_txt1 TYPE so_text255,
***          lt_txt1 TYPE STANDARD TABLE OF so_text255,
***          lv_len1 TYPE i,
***          lv_txt2 TYPE so_text255,
***          lt_txt2 TYPE bcsy_text,
***          lv_len2 TYPE i.
***
***    DATA: ls_ztmail_text TYPE ztmail_text.

*    DATA: lv_starv_rbstat TYPE rbstat.

*    DATA(lv_belnr) = is_send-material_doc_num.

*    IF is_send-material_doc_num(2) = '51' AND is_send-user_name IS INITIAL.
*      SELECT SINGLE usnam
*      FROM rbkp
*      WHERE belnr = @is_send-material_doc_num
*        AND gjahr = @is_send-material_doc_year
*      INTO @DATA(lv_usnam).
*    ENDIF.


    IF is_send-user_name IS INITIAL." AND lv_usnam IS INITIAL.

      DATA lt_send TYPE STANDARD TABLE OF ty_send.
      lt_send = VALUE #( ( is_send ) ).
      SELECT SINGLE CASE
                    WHEN rbkp~belnr LIKE '51%'
                     AND rbkp~usnam IS NOT INITIAL THEN rbkp~usnam
                    WHEN mkpf~usnam IS NOT INITIAL THEN mkpf~usnam
                    WHEN bkpf~usnam IS NOT INITIAL THEN bkpf~usnam
                    WHEN anek~usnam IS NOT INITIAL THEN anek~usnam
                    WHEN rbkp~usnam IS NOT INITIAL THEN rbkp~usnam
                    END AS usnam
      FROM @lt_send AS it LEFT JOIN mkpf ON mkpf~mblnr = it~material_doc_num
                                        AND mkpf~mjahr = it~material_doc_year
                          LEFT JOIN bkpf ON bkpf~belnr = it~material_doc_num
                                        AND bkpf~gjahr = it~material_doc_year
                          LEFT JOIN anek ON anek~belnr = it~material_doc_num
                                        AND anek~gjahr = it~material_doc_year
                          LEFT JOIN rbkp ON rbkp~belnr = it~material_doc_num
                                        AND rbkp~gjahr = it~material_doc_year
     INTO @DATA(lv_usnam).


*      SELECT SINGLE usnam
*        FROM mkpf
*        INTO @lv_usnam
*        WHERE mblnr = @is_send-material_doc_num
*          AND mjahr = @is_send-material_doc_year.
*
*      IF sy-subrc IS NOT INITIAL.
*
*        SELECT SINGLE usnam
*          FROM bkpf
*          INTO @lv_usnam
*          WHERE belnr = @is_send-material_doc_num
*            AND gjahr = @is_send-material_doc_year.
*
*        IF sy-subrc <> 0.
*
*
*          SELECT SINGLE usnam
*            FROM anek
*            INTO @lv_usnam
*            WHERE belnr = @is_send-material_doc_num
*              AND gjahr = @is_send-material_doc_year.
*
*          IF sy-subrc <> 0.
*
*            SELECT SINGLE usnam
*            FROM rbkp
*            WHERE belnr = @is_send-material_doc_num
*              AND gjahr = @is_send-material_doc_year
*            INTO @lv_usnam.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.

    ELSEIF is_send-user_name IS NOT INITIAL.
      lv_usnam = is_send-user_name.
    ENDIF.

    IF lv_usnam IS INITIAL.
      MESSAGE e027(zmmint0003_msg) WITH is_send-material_doc_num INTO sy-lisel.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

    DATA(ls_address) = VALUE bapiaddr3( ).
    DATA(lt_return) = VALUE bapiret2_t( ).
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_usnam
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

*    ls_ztmail_text-text_msg = 'Уважаемый(-ая) &2, Информируем Вас о том, что возникли следующие ошибки по документу &3:'.
*    ls_ztmail_text-topic_msg = 'Ошибка обработки документа &3 ПТ112'.
    DATA(lt_body) = VALUE bcsy_text( ).
    IF ls_address IS NOT INITIAL AND ls_address-e_mail IS NOT INITIAL.
      DATA(lv_msg_text)  = CONV string( 'Уважаемый(-ая) &2, Информируем Вас о том, что возникли следующие ошибки по документу &3:' )."ls_ztmail_text-text_msg.
      DATA(lv_topic_msg) = CONV string( 'Ошибка обработки документа &3 ПТ112' ).
      REPLACE '&2' IN lv_msg_text WITH ls_address-fullname.
      REPLACE '&3' IN lv_msg_text WITH is_send-material_doc_num.
      REPLACE '&3' IN lv_topic_msg WITH is_send-material_doc_num.

      APPEND lv_msg_text TO lt_body.

      "запись сообщений
      DATA(lt_messages) = io_log->get_all_messages( ).
      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF sy-tabix <= mv_message_maxindex OR <ls_message>-msgid = 'ZMMINT0003_MSG' AND <ls_message>-msgno = 96
                                           OR <ls_message>-msgid = 'ZMMINT0003_MSG' AND <ls_message>-msgno = 97.
          CONTINUE.
        ENDIF.

        MESSAGE ID <ls_message>-msgid TYPE 'E' NUMBER <ls_message>-msgno
        WITH <ls_message>-msgv1 <ls_message>-msgv2
             <ls_message>-msgv3 <ls_message>-msgv4 INTO sy-lisel.
        APPEND sy-lisel TO lt_body.
      ENDLOOP.

      mv_message_maxindex = lines( lt_messages ).

      TRY.
          DATA(lo_send_request) = cl_bcs=>create_persistent( ).
          "add document to send request
          lo_send_request->set_document( cl_document_bcs=>create_document( i_type    = 'RAW'
                                                                           i_text    = lt_body
                                                                           i_length  = '12'
                                                                           i_subject = CONV #( lv_topic_msg ) ) ).
          "add text to document
          lo_send_request->set_note( VALUE #( ) ).
          lo_send_request->set_sender( cl_cam_address_bcs=>create_internet_address( 'sap@rshb.ru' ) ).
          "add recipient with its respective attributes to send request
          lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( ls_address-e_mail ) ).
          "send document
          DATA(lv_sent_to_all) = lo_send_request->send( 'X' ).

          IF lv_sent_to_all = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.

        CATCH cx_bcs INTO DATA(lo_exc).
          sy-lisel = lo_exc->get_text( ).
      ENDTRY.

    ELSEIF ls_address IS INITIAL.
      MESSAGE e029(zmmint0003_msg) WITH lv_usnam INTO sy-lisel.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      INSERT LINES OF lt_return INTO TABLE et_message[].
      RETURN.
    ELSEIF ls_address-e_mail IS INITIAL.
      MESSAGE e030(zmmint0003_msg) WITH lv_usnam INTO sy-lisel.
      cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = et_message ).
      RETURN.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>SEND_EDO_PT1516
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        ZTT_RCMINT0006_PT1516_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_edo_pt1516.

    DATA(lo_srv) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_ifc_c=>sc_bo_key ).
    DATA(lo_tra) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
    DATA(lt_key_all) = VALUE /bobf/t_frw_key( ).

    LOOP AT it_data INTO DATA(ls_data).
      TRY.
          DATA(ls_reg_data) = CORRESPONDING zsrcmint0006_pt1516_d( ls_data EXCEPT error_code
                                                                                  error_descr
                                                                                  process_status
                                                                                  deliver_status ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_reg_data-id_agreement_asu_fhd
        IMPORTING
          output = ls_reg_data-id_agreement_asu_fhd.

      DATA(ls_params_reg) = VALUE zspi_ifc_a_register( ).
      ls_params_reg-data =  REF #( ls_reg_data ).

      DATA(lt_reg_key) = VALUE /bobf/t_frw_key( ).
      lo_srv->do_action( EXPORTING
                           iv_act_key    = zif_ifc_c=>sc_action-pt1516-act_register
                           is_parameters = REF #( ls_params_reg )
                         IMPORTING
                           eo_message    = DATA(lo_reg_message)
                           et_data       = lt_reg_key ).

      INSERT LINES OF lt_reg_key INTO TABLE lt_key_all[].

      lo_tra->save( ).

    ENDLOOP.

    IF lt_key_all IS NOT INITIAL.
      lo_srv->do_action( EXPORTING
                            iv_act_key  =   zif_ifc_c=>sc_action-pt1516-act_send
                            it_key      =   lt_key_all ).
      lo_tra->save( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_TAX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MWSKZ                       TYPE        MWSKZ
* | [--->] IV_WRBTR                       TYPE        WRBTR
* | [<-()] RS_MWDAT                       TYPE        RTAX1U15
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_tax.

    CONSTANTS: lc_bukrs TYPE bukrs VALUE '1000',
               lc_rub   TYPE waers VALUE 'RUB'.

    DATA: lt_mwdat TYPE TABLE OF rtax1u15.


    CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
      EXPORTING
        i_waers = lc_rub
        i_bukrs = lc_bukrs
        i_mwskz = iv_mwskz
        i_wrbtr = iv_wrbtr
      TABLES
        t_mwdat = lt_mwdat
      EXCEPTIONS
        OTHERS  = 15.

    IF sy-subrc = 0.
      rs_mwdat = VALUE #( lt_mwdat[ 1 ] OPTIONAL ).
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_MIR7_USL_960
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        RE_BELNR
* | [--->] IV_GJAHR                       TYPE        GJAHR
* | [<---] ET_RETURN                      TYPE        BAPIRET2_T
* | [<-()] ES_INFOPT0113                  TYPE        TY_PT0113
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mir7_usl_960.


    CONSTANTS:
      lc_shkzgs            TYPE shkzg        VALUE 'S',
      lc_shkzgh            TYPE shkzg        VALUE 'H',
      lc_check             TYPE mrm_vorgang  VALUE '1', "Счет
      lc_creditinvoice     TYPE mrm_vorgang  VALUE '2', "Кредитовое авизо
      lc_additionaldebit   TYPE mrm_vorgang  VALUE '3', "Дополнительное дебетование
      lc_additionallending TYPE mrm_vorgang  VALUE '4', "Дополнительное кредитование
      lc_rub               TYPE waers        VALUE 'RUB',
      lc_r2                TYPE mwskz        VALUE 'R2',
      lc_true(5)           TYPE c            VALUE 'true',
      lc_false(5)          TYPE c            VALUE 'false',
      lc_kurst             TYPE kurst        VALUE 'M',
      lc_bukrs             TYPE bukrs        VALUE '1000',
      lc_skat_spras        TYPE skat-spras   VALUE 'R',
      lc_skat_ktopl        TYPE skat-ktopl   VALUE 'RSHB'.


    DATA: lt_rng_bsart_usl    TYPE RANGE OF ekko-bsart,
          lt_rng_knttp_usl    TYPE RANGE OF ekpo-knttp,
          lv_invdat           TYPE gdatu_inv,
          lv_vorgang          TYPE vorgang,
          lt_mwdat            TYPE rtax1u15_tab,
          lv_vatamount        TYPE fwstev,
          lv_vatcurramount    TYPE fwstev,
          lv_hozoperationtype TYPE char20,
          lt_rng_rbstat       TYPE RANGE OF rbstat,
          lv_mtart_bilet      TYPE mtart. "вид материала для билетов


    "GINIYATYLLIN 20.04.2023 {
    TYPES: BEGIN OF lty_max_operamount,
             p_operationamount TYPE string,
             p_vatamount       TYPE string,
             mwskz             TYPE mwskz,
             tabix             TYPE sy-tabix,
           END OF lty_max_operamount.
    DATA: ls_max_operamount TYPE lty_max_operamount,
          lt_max_operamount TYPE TABLE OF lty_max_operamount,
          lv_sum_vatamount  TYPE string.
    "} GINIYATYLLIN 20.04.2023
    DATA:
*          mem_rseg_113 TYPE TABLE OF zss_rseg_133,
      lt_txt_line TYPE TABLE OF tline,
      lv_txt_name TYPE thead-tdname.

    DATA mt_wmwst TYPE zmr_003_rbtx.

    DATA: lt_rng_ebeln TYPE RANGE OF rseg-ebeln,
          lt_rng_matkl TYPE RANGE OF mara-matkl,
          lt_rng_bprme TYPE RANGE OF rseg-bprme.

    DATA: lv_wmwst1           TYPE fwstev,
*          lt_item_m  TYPE ztt_int0003_pt0113_item_k,
          isvatincluded       TYPE abap_bool,
          lt_mwskz_i          TYPE tt_mwskz_i,
          ls_mwskz_i          TYPE ty_mwskz_i,
          lt_mwskz_vt         TYPE tt_mwskz_i,
          ls_mwskz_vt         TYPE ty_mwskz_i,
          lv_wrbtr_pos_sum    TYPE wrbtr_cs,
          lv_vatamount_part   TYPE wrbtr_cs,
          lv_amount_sum_rub   TYPE salk3,
          lv_amount_sum_curr  TYPE salk3,
          lv_amount_diff_rub  TYPE salk3,
          lv_amount_diff_curr TYPE salk3,
          lv_wrbtr_rub        TYPE wrbtr_cs,
          lv_zukr             TYPE abap_bool,
          lt_obj_rent         TYPE RANGE OF ze_typget,
          lt_obj_own          TYPE RANGE OF ze_typget,
          lt_items_113        TYPE tt_pt0113_items,
          lt_pt0113_agr       TYPE tt_pt0113_items,
          ls_item_line        TYPE zsint0003_pt0113_item_k.

    DATA: lt_fipos TYPE RANGE OF fipos.

    DATA: lv_accnt8(8) TYPE c.

    CLEAR et_return.

    IMPORT lv_wmwst1 TO lv_wmwst1 FROM MEMORY ID 'lcmemidwmwst1' .
    FREE MEMORY ID 'lcmemidwmwst1'.

    IMPORT tab = mt_wmwst FROM MEMORY ID 'mt_wmwst'.
    FREE MEMORY ID 'mt_wmwst'.
    LOOP AT zcl_int003_helper=>mt_drseg ASSIGNING FIELD-SYMBOL(<ls_drseg_i>). """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR ls_mwskz_i.
      MOVE-CORRESPONDING <ls_drseg_i> TO ls_mwskz_i.
      COLLECT ls_mwskz_i INTO lt_mwskz_i.
    ENDLOOP."""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF mt_wmwst IS NOT INITIAL.
      isvatincluded = abap_true.
    ENDIF.


*    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_KNTTP_USL' IMPORTING er_range = lt_rng_knttp_usl ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_BSART_USL' IMPORTING er_range = lt_rng_bsart_usl ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_RBSTAT'         IMPORTING er_range = lt_rng_rbstat ).
    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMMINT003_MTART_ZUKR'  IMPORTING ev_value = lv_zukr ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZPRE0030_ZZTYPGET_RENT'  IMPORTING er_range = lt_obj_rent ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSRE0030_ZZTYPGET_OWN'  IMPORTING er_range = lt_obj_own ).

    SELECT SINGLE rbkp~belnr  AS material_doc_num,
               rbkp~gjahr  AS material_doc_year,
*               rbkp~zfbdt  AS plandate,
*               rbkp~zfbdt  AS ratecalcdate,"plandate,
               rbkp~budat  AS plandate,
               rbkp~budat  AS ratecalcdate,
               rbkp~sgtxt  AS paymentpurpose,
               rbkp~wmwst1 AS vatamount,
               rbkp~rmwwr  AS operationamount,
               rbkp~waers  AS currencycode,
               rbkp~kursf  AS kurs,
               rbkp~usnam  AS usnam,
               rbkp~xblnr,
               rbkp~glo_ref1_hd,
               rbkp~bldat AS originalaccountdate
   FROM rbkp
     WHERE rbkp~belnr = @iv_belnr
       AND rbkp~gjahr = @iv_gjahr
       AND rbstat IN @lt_rng_rbstat
     INTO @DATA(ls_rbkp).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE accnt
      FROM usr02
      WHERE bname = @ls_rbkp-usnam
      INTO @DATA(lv_accnt).

    SELECT SINGLE rseg~werks,
                  rseg~tbtkz,
                  rseg~shkzg,
                  rseg~mwskz,
                  rseg~matnr,
                  rseg~salk3     AS opercurramount_rseg,
                  rseg~ebeln,
                  ekpo~knttp,
                  ekko~zzext_key,
                  vicncn~relevanteval,
                  vicncn~zzaddchar,
                  dgv~ext_key    AS agreementid,
                  zdgv~valuta    AS currencycode,
                  zdgv~abs_num   AS cftagreementid,
                  zdgv~kurs_type AS ratetype,
                  zdgv~kurs      AS opercurrrate,
                  zdgv~sum_val   AS opercurramount_dgv,
                  zdgvp~serv_comp
      FROM rseg
        LEFT JOIN ekko
          ON rseg~ebeln = ekko~ebeln
        LEFT JOIN ekpo
          ON ekko~ebeln = ekpo~ebeln
        LEFT JOIN scmg_t_case_attr AS dgv
          ON ekko~zzext_key = dgv~ext_key
        LEFT JOIN zrcmt_dgv_attr AS zdgv
          ON dgv~case_guid = zdgv~case_guid
        LEFT JOIN ztrcm_dgv_part AS zdgvp
          ON dgv~case_guid = zdgvp~case_guid
      LEFT JOIN vicncn AS vicncn
        ON vicncn~zzrcmnum = ekko~zzext_key
      WHERE rseg~belnr = @iv_belnr
        AND rseg~gjahr = @iv_gjahr
        AND ekpo~loekz = @abap_false
      INTO @DATA(ls_rseg).

    DATA(lv_ebeln) = ls_rseg-ebeln.

    SELECT rseg~belnr, rseg~gjahr,
         rseg~buzei, rseg~matnr,
         rseg~ebeln, rseg~ebelp,
         rseg~bprme, rseg~menge,
         rseg~salk3, rseg~mwskz,
         rseg~sgtxt, rseg~werks,
         rseg~wrbtr, rseg~tbtkz,
         rseg~shkzg, rseg~bukrs,
         rseg~knttp,
         mara~matkl, makt~maktx,
         mara~mtart,
         clgr~zcl, clgr~zgr,
         ekkn~wempf, rbkp~wmwst1
    FROM rseg
      JOIN mara
        ON rseg~matnr = mara~matnr
      LEFT JOIN makt
        ON mara~matnr = makt~matnr AND
           makt~spras = @sy-langu
      LEFT JOIN ztmm_cl_gr AS clgr
        ON mara~matkl = clgr~zmatkl
      LEFT JOIN ekkn
      ON rseg~ebeln = ekkn~ebeln AND
         rseg~ebelp = ekkn~ebelp
      LEFT JOIN rbkp
      ON rbkp~belnr = @iv_belnr AND
         rbkp~gjahr = @iv_gjahr
*      LEFT JOIN ekpo
*      ON rseg~ebeln = ekpo~ebeln AND
*         rseg~ebelp = ekpo~ebelp
*      LEFT JOIN ekko
*      ON rseg~ebeln = ekko~ebeln
*      LEFT JOIN skat
*      ON skat~spras = 'R' AND
*         skat~ktopl = 'RSHB' AND
*         skat~saknr = ekpo~fipos
*      LEFT JOIN zrcmtd_tax_inc
*      ON zrcmtd_tax_inc~tax_income = ekko~zztax_income
    WHERE rseg~belnr = @iv_belnr
      AND rseg~gjahr = @iv_gjahr
    INTO TABLE @DATA(lt_rseg_113).


    lt_rng_ebeln = VALUE #( FOR ls_rs IN lt_rseg_113 sign = 'I' option = 'EQ' ( low = ls_rs-ebeln ) ).
    lt_rng_matkl = VALUE #( FOR ls_rs IN lt_rseg_113 sign = 'I' option = 'EQ' ( low = ls_rs-matkl ) ).
    lt_rng_bprme = VALUE #( FOR ls_rs IN lt_rseg_113 sign = 'I' option = 'EQ' ( low = ls_rs-bprme ) ).

    SELECT ekpo~ebeln, ekpo~ebelp,
        ekpo~brtwr, ekpo~netwr,
        ekpo~menge AS ekpo_menge,
        ekkn~menge AS ekkn_menge,
        ekkn~sakto,
        anla~anln1, anla~anln2,
        anla~invnr, anlz~pernr,
*      ekko~zzext_key, zdgv~abs_num, zdgv~sum_val, ekkn~fipos, skat~txt50, zrcmtd_tax_inc~text, ekko~zztax_income
        ekko~zzext_key, zdgv~abs_num, zdgv~sum_val, ekkn~fipos, skat~txt50, zrcmtd_tax_inc~text, ekko~zztax_income,
        ekpo~fipos AS ekpo_fipos, skat~saknr, skat~txt50 AS skat_txt50
   FROM ekko
     LEFT JOIN ekpo
       ON ekko~ebeln = ekpo~ebeln
     LEFT JOIN ekkn
       ON ekpo~ebeln = ekkn~ebeln AND
          ekpo~ebelp = ekkn~ebelp
     LEFT JOIN anla
       ON ekkn~anln1 = anla~anln1 AND
          ekkn~anln2 = anla~anln2
     LEFT JOIN anlz
       ON anla~anln1 = anlz~anln1 AND
          anla~anln2 = anlz~anln2 AND
          anla~bukrs = anlz~bukrs
     LEFT JOIN scmg_t_case_attr AS case
       ON ekko~zzext_key = case~ext_key
     LEFT JOIN zrcmt_dgv_attr AS zdgv
       ON case~case_guid = zdgv~case_guid
     LEFT JOIN skat
       ON skat~spras = @lc_skat_spras AND
          skat~ktopl = @lc_skat_ktopl AND
*          skat~saknr = ekpo~fipos
          skat~saknr = ekkn~sakto
    LEFT JOIN zrcmtd_tax_inc
      ON zrcmtd_tax_inc~tax_income = ekko~zztax_income
   INTO TABLE @DATA(lt_ekko)
   WHERE ekko~ebeln IN @lt_rng_ebeln
   ORDER BY ekpo~ebeln, ekpo~ebelp.

    SELECT msehi,
           mm_okei
      FROM zint_mm_okei
          WHERE msehi IN @lt_rng_bprme
      INTO TABLE @DATA(lt_mm_okei).

*    LOOP AT lt_rseg_113 ASSIGNING FIELD-SYMBOL(<ls_rseg>).
*      lv_wrbtr_pos_sum = lv_wrbtr_pos_sum + <ls_rseg>-wrbtr.
*    ENDLOOP.

    LOOP AT lt_rseg_113 ASSIGNING FIELD-SYMBOL(<ls_rseg>).
*      APPEND INITIAL LINE TO es_infopt0113-item ASSIGNING FIELD-SYMBOL(<mem_rseg>).
      APPEND INITIAL LINE TO lt_items_113 ASSIGNING FIELD-SYMBOL(<mem_rseg>).
      DATA(lv_tabix_i) = sy-tabix.

      READ TABLE lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>) WITH KEY ebelp = <ls_rseg>-ebelp.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      lv_txt_name = |{ <ls_rseg>-ebeln }{ <ls_rseg>-ebelp }|.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = 'F01'
          language = sy-langu
          name     = lv_txt_name
          object   = 'EKPO'
        TABLES
          lines    = lt_txt_line
        EXCEPTIONS
          OTHERS   = 8.

      IF sy-subrc = 0 AND lt_txt_line IS NOT INITIAL.
        <mem_rseg>-p_tmcname = |{ <ls_rseg>-maktx } { lt_txt_line[ 1 ]-tdline }|.
      ELSE.
        <mem_rseg>-p_tmcname = |{ <ls_rseg>-maktx }|.
      ENDIF.

      DATA(ls_tax) = get_tax( iv_mwskz = <ls_rseg>-mwskz iv_wrbtr = CONV #( <ls_rseg>-wrbtr ) ).

      IF mt_wmwst IS NOT INITIAL.
        READ TABLE mt_wmwst INTO DATA(ls_mt_wmwst) WITH KEY buzei = <ls_rseg>-buzei.
      ENDIF.

      IF sy-subrc EQ 0.
        "baryutinsa 29.06.23 создание билетов добавлена настройка ставки
        IF <ls_rseg>-mtart EQ 'ZUKR' AND lv_zukr = abap_true.
          IF ls_mt_wmwst-wmwst > 30.
            ls_mt_wmwst-wmwst = ls_tax-msatz.
          ENDIF.

          IF ls_mt_wmwst-wmwst NE '0.00'.
            isvatincluded = abap_true.
          ENDIF.

          IF isvatincluded NE 'X'.
            <mem_rseg>-p_vatrate = ls_tax-msatz.
            <mem_rseg>-p_vatrate = round( val = <mem_rseg>-p_vatrate dec = 2 ).
*            CONDENSE <mem_rseg>-p_vatrate.
          ENDIF.
          IF isvatincluded EQ 'X'.
            <mem_rseg>-p_vatrate = ls_mt_wmwst-wmwst.
            <mem_rseg>-p_vatrate = round( val = <mem_rseg>-p_vatrate dec = 2 ).
*            CONDENSE <mem_rseg>-p_vatrate.
          ENDIF.
        ELSE.
          ls_mt_wmwst-wmwst = ls_tax-msatz.
          <mem_rseg>-p_vatrate = ls_tax-msatz.
          <mem_rseg>-p_vatrate = round( val = <mem_rseg>-p_vatrate dec = 2 ).
*          CONDENSE <mem_rseg>-p_vatrate.
        ENDIF.
      ENDIF.
      IF sy-subrc EQ 4.
        <mem_rseg>-p_vatrate = ls_tax-msatz.
        <mem_rseg>-p_vatrate = round( val = <mem_rseg>-p_vatrate dec = 2 ).
*        CONDENSE <mem_rseg>-p_vatrate.
      ENDIF.

      CLEAR ls_mt_wmwst.

      <mem_rseg>-p_usertabnum = <ls_rseg>-wempf.
      IF ls_rbkp-currencycode <> lc_rub.
*        <mem_rseg>-p_vatcurramount = <ls_rseg>-wmwst1.
*        <mem_rseg>-p_operationcurramount = <ls_rseg>-wrbtr.

        lv_vatcurramount = ( <ls_rseg>-wrbtr * <mem_rseg>-p_vatrate ) / 100.

        <mem_rseg>-p_operationcurramount = <ls_rseg>-wrbtr + lv_vatcurramount.
        <mem_rseg>-p_operationcurramount = round( val = <mem_rseg>-p_operationcurramount dec = 2 ).

        IF ls_tax-wmwst < 1 AND <mem_rseg>-p_vatrate IS NOT INITIAL.
          <mem_rseg>-p_vatrate = ls_tax-msatz.
        ENDIF.
      ENDIF.
*      IF ls_rbkp-currencycode = lc_rub. "Для рублевых заказов

      "baryutin-sa расчет билетов изменеено условие
*      IF isvatincluded EQ 'X' AND <ls_rseg>-mtart EQ 'ZUKR'.
      IF isvatincluded EQ 'X' AND <ls_rseg>-mtart EQ 'ZUKR' AND lv_zukr = abap_true.
*        <mem_rseg>-p_operationamount = <ls_rseg>-wrbtr + <mem_rseg>-p_vatrate.

        <mem_rseg>-p_vatamount =  <mem_rseg>-p_vatrate.

        <mem_rseg>-p_operationamount = <ls_rseg>-wrbtr + <mem_rseg>-p_vatrate.
        <mem_rseg>-p_operationamount = round( val = <mem_rseg>-p_operationamount dec = 2 ).

        <mem_rseg>-p_vatrate = <mem_rseg>-p_vatamount * 100 / <mem_rseg>-p_operationamount.
        <mem_rseg>-p_vatrate = round( val = <mem_rseg>-p_vatrate dec = 2 ).

*} KvizhinadzeVS 31.07.2023
        MESSAGE s094(zmmint0003_msg) WITH <ls_rseg>-belnr 1 <mem_rseg>-p_operationamount INTO sy-lisel.
        INSERT VALUE #( type = sy-msgty id = sy-msgid number = sy-msgno
                        message_v1 = sy-msgv1 message_v2 = sy-msgv2
                        message_v3 = sy-msgv3 message_v4 = sy-msgv4
                        message    = sy-lisel ) INTO TABLE et_return.
*}
        "{
        IF ls_rbkp-currencycode <> lc_rub.
          lv_wrbtr_rub = <ls_rseg>-wrbtr * ls_rbkp-kurs.
        ELSE.
          lv_wrbtr_rub = <ls_rseg>-wrbtr.

*          lv_vatcurramount = ( <ls_rseg>-wrbtr * <mem_rseg>-p_vatrate ) / 100.
          lv_vatcurramount = <mem_rseg>-p_vatamount.
        ENDIF.
        "}

**        lv_vatamount = <mem_rseg>-p_vatrate.
      ELSE.
*          <mem_rseg>-p_operationamount = <ls_rseg>-wrbtr + <ls_rseg>-wmwst1.
*          <mem_rseg>-p_operationamount = round( val = <mem_rseg>-p_operationamount dec = 2 ).
*
*          IF ls_tax-wmwst < 1 AND <mem_rseg>-p_vatrate IS NOT INITIAL.
*            <mem_rseg>-p_vatrate = ls_tax-msatz.
*          ENDIF.
*
*          lv_vatamount = ( <ls_rseg>-wrbtr * <mem_rseg>-p_vatrate ) / 100.
*          IF lv_vatamount NE <ls_rseg>-wmwst1.
*            lv_vatamount = <ls_rseg>-wmwst1.
*          ENDIF.

        IF ls_rbkp-currencycode <> lc_rub.
          lv_wrbtr_rub = <ls_rseg>-wrbtr * ls_rbkp-kurs.
        ELSE.
          lv_wrbtr_rub = <ls_rseg>-wrbtr.

          lv_vatcurramount = ( <ls_rseg>-wrbtr * <mem_rseg>-p_vatrate ) / 100.
        ENDIF.

        lv_vatamount = ( lv_wrbtr_rub * <mem_rseg>-p_vatrate ) / 100.
*<<nbbarmykova18042023 GINIYATYLLIN 20.04.2023
        IF zcl_int003_helper=>mt_drseg IS NOT INITIAL AND " sy-uname = 'GINIYATYLLIN' AND
          line_exists( zcl_int003_helper=>mt_drseg[ belnr = <ls_rseg>-belnr
                            gjahr = <ls_rseg>-gjahr buzei = <ls_rseg>-buzei ] ).
          READ TABLE zcl_int003_helper=>mt_drseg ASSIGNING <ls_drseg_i> WITH KEY belnr = <ls_rseg>-belnr
          gjahr = <ls_rseg>-gjahr
          buzei = <ls_rseg>-buzei  .

          <mem_rseg>-p_operationamount = lv_wrbtr_rub + lv_vatamount.

          IF sy-subrc = 0.
            IF ls_rbkp-currencycode = lc_rub.

              IF <ls_drseg_i>-stock_posting IS NOT INITIAL.
                <mem_rseg>-p_operationamount = <ls_drseg_i>-wrbtr + <ls_drseg_i>-stock_posting.
              ELSE.
                <mem_rseg>-p_operationamount = <ls_drseg_i>-wrbtr + lv_vatamount.
              ENDIF.

            ELSE.

              IF <ls_drseg_i>-stock_posting IS NOT INITIAL.
                <mem_rseg>-p_operationcurramount = <ls_drseg_i>-wrbtr + <ls_drseg_i>-stock_posting.
              ELSE.
                <mem_rseg>-p_operationcurramount = <ls_drseg_i>-wrbtr + lv_vatcurramount.
              ENDIF.

            ENDIF.

*} KvizhinadzeVS 31.07.2023
            MESSAGE s094(zmmint0003_msg) WITH <ls_rseg>-belnr 2 <mem_rseg>-p_operationamount INTO sy-lisel.
            INSERT VALUE #( type = sy-msgty id = sy-msgid number = sy-msgno
                            message_v1 = sy-msgv1 message_v2 = sy-msgv2
                            message_v3 = sy-msgv3 message_v4 = sy-msgv4
                            message    = sy-lisel ) INTO TABLE et_return.
*}

            "GINIYATYLLIN 20.04.2023 {
            "сохранение значений <OperationAmount> и <VATAmount>, для определения наибольшего <OperationAmount>
            ls_max_operamount-tabix = lv_tabix_i.
            ls_max_operamount-mwskz = <ls_drseg_i>-mwskz.
            ls_max_operamount-p_operationamount = <mem_rseg>-p_operationamount.
            ls_max_operamount-p_vatamount = <mem_rseg>-p_vatamount.

            APPEND ls_max_operamount TO lt_max_operamount.
            "} GINIYATYLLIN 20.04.2023

          ENDIF.
*>>nbbarmykova18042023 GINIYATYLLIN 20.04.2023
*  zcl_int003_helper=>mt_rbtx   = it_rbtx .
*          <mem_rseg>-p_operationamount = <ls_rseg>-wrbtr + ( <ls_rseg>-wrbtr / lv_wrbtr_pos_sum * <ls_rseg>-wmwst1 ).
        ELSE.
          <mem_rseg>-p_operationamount = lv_wrbtr_rub + lv_vatamount.

*} KvizhinadzeVS 31.07.2023
          MESSAGE s094(zmmint0003_msg) WITH <ls_rseg>-belnr 3 <mem_rseg>-p_operationamount INTO sy-lisel.
          INSERT VALUE #( type = sy-msgty id = sy-msgid number = sy-msgno
                          message_v1 = sy-msgv1 message_v2 = sy-msgv2
                          message_v3 = sy-msgv3 message_v4 = sy-msgv4
                          message    = sy-lisel ) INTO TABLE et_return.
*}

        ENDIF.

        <mem_rseg>-p_operationamount = round( val = <mem_rseg>-p_operationamount dec = 2 ).

        IF ls_tax-wmwst < 1 AND <mem_rseg>-p_vatrate IS NOT INITIAL.
          <mem_rseg>-p_vatrate = ls_tax-msatz.
        ENDIF.

*          lv_vatamount = ( <ls_rseg>-wrbtr * <mem_rseg>-p_vatrate ) / 100.
*          IF lv_vatamount NE <ls_rseg>-wmwst1.
*            lv_vatamount = <ls_rseg>-wmwst1.
*          ENDIF.
*        ENDIF.
*<<nbbarmykova18042023 GINIYATYLLIN 20.04.2023
        "{baryutin-sa комментед ошибка НДС, алгоритм актуален?
***        IF  zcl_int003_helper=>mt_drseg IS NOT INITIAL AND "sy-uname = 'GINIYATYLLIN' AND
***          line_exists( zcl_int003_helper=>mt_drseg[ belnr = <ls_rseg>-belnr
***                            gjahr = <ls_rseg>-gjahr buzei = <ls_rseg>-buzei ] ) AND
***          zcl_int003_helper=>mt_rbtx IS NOT INITIAL.
****MWSKZ
***          READ TABLE zcl_int003_helper=>mt_drseg ASSIGNING <ls_drseg_i> WITH KEY belnr = <ls_rseg>-belnr
***          gjahr = <ls_rseg>-gjahr
***          buzei = <ls_rseg>-buzei  .
***          IF sy-subrc = 0.
**** 3. Рассчитываем сумму <VATAmount> для каждого EBELN/EBELP.
****  1).  Если NAV_FW <> 'пусто', то <VATAmount> = NAV_FW.
***            IF <ls_drseg_i>-nav_fw IS NOT INITIAL.
***              <mem_rseg>-p_vatamount = <ls_drseg_i>-nav_fw.
***              CLEAR ls_mwskz_vt.
***              MOVE-CORRESPONDING <ls_drseg_i> TO ls_mwskz_vt.
***              ls_mwskz_vt-netwr = <mem_rseg>-p_vatamount.
***              COLLECT ls_mwskz_vt INTO lt_mwskz_vt .
***
***            ELSE.
***
***              DATA(lv_netwr) = VALUE bstwr( lt_mwskz_i[ belnr = <ls_rseg>-belnr  gjahr = <ls_rseg>-gjahr mwskz = <ls_drseg_i>-mwskz ]-netwr OPTIONAL ).
***              DATA(lv_hwbas) = VALUE hwbas_bses_cs( zcl_int003_helper=>mt_rbtx[ mwskz = <ls_drseg_i>-mwskz ]-hwbas OPTIONAL ).
****  2). Если NAV_FW = 'пусто', то <VATAmount>  = Z*X/Y, где
****     Z - значение LT_RBTX- HWBAS, при   LT_RBTX - MWSKZ = LT_DRSEG- MWSKZ Nдля исходной позиции с ключом EBELN/EBELP
****     X - сумма всех LT_DRSEG-NETWR с тем же LT_DRSEG-MWSKZ как для исходной позиции с ключом EBELN/EBELP.
****     Y - значение NETWR для позиции EBELN/EBELP с  LT_DRSEG-MWSKZ                               GJAHR = <ls_rseg>-GJAHR BUZEI = <ls_rseg>-BUZEI ] ).
***              TRY.
***                  <mem_rseg>-p_vatamount  = lv_hwbas * lv_netwr / <ls_drseg_i>-netwr.
***                  CLEAR ls_mwskz_vt.
***                  MOVE-CORRESPONDING <ls_drseg_i> TO ls_mwskz_vt.
***                  ls_mwskz_vt-netwr = <mem_rseg>-p_vatamount.
***                  COLLECT ls_mwskz_vt INTO lt_mwskz_vt .
***                CATCH cx_sy_zerodivide.
***              ENDTRY.
***            ENDIF.
***          ENDIF.
***        ELSE.
        "}baryutin-sa комментед ошибка НДС, алгоритм актуален?
        <mem_rseg>-p_vatamount = lv_vatamount.
***        ENDIF.
***        IF <mem_rseg>-p_vatamount IS INITIAL.
***          <mem_rseg>-p_vatamount = lv_vatamount.
***        ENDIF.
        <mem_rseg>-p_vatamount = round( val = <mem_rseg>-p_vatamount dec = 2 ).
        <mem_rseg>-p_vatcurramount = lv_vatcurramount.
        <mem_rseg>-p_vatcurramount = round( val = <mem_rseg>-p_vatcurramount dec = 2 ).
*        CONDENSE <mem_rseg>-p_vatamount.
*        CONDENSE <mem_rseg>-p_operationamount.
      ENDIF.
*******************************************************************

*******************************************************************
      "XSD
*      <mem_rseg>-p_comment = |{ <ls_ekko>-txt50 } { <ls_ekko>-text }|.

      "XSD
      <mem_rseg>-p_comment = <ls_ekko>-text.
      <mem_rseg>-p_financialdesc = <ls_ekko>-skat_txt50.
      <mem_rseg>-p_financialcode = <ls_ekko>-saknr.
      <mem_rseg>-fipos = <ls_ekko>-fipos.
      <mem_rseg>-knttp = <ls_rseg>-knttp.
      <mem_rseg>-ebeln = <ls_rseg>-ebeln.
      <mem_rseg>-ebelp = <ls_rseg>-ebelp.
*      CONDENSE <mem_rseg>-p_operationamount.
    ENDLOOP.




****************************************************************************************************
    "GINIYATYLLIN 20.04.2023 {
    SORT lt_max_operamount BY mwskz ASCENDING p_operationamount  DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_max_operamount COMPARING mwskz.
    LOOP AT zcl_int003_helper=>mt_rbtx ASSIGNING FIELD-SYMBOL(<ls_rbtx>).
      DATA(lv_max_vatamount) = VALUE #( lt_mwskz_vt[ mwskz = <ls_rbtx>-mwskz ]-netwr OPTIONAL ).
      DATA(lv_diff) = <ls_rbtx>-wmwst - lv_max_vatamount.
      IF lv_diff = 0.
        CONTINUE.
      ENDIF.
      DATA(lv_indx) = VALUE #( lt_max_operamount[ mwskz = <ls_rbtx>-mwskz ]-tabix OPTIONAL ).
      IF lv_indx IS INITIAL.
        CONTINUE.
      ENDIF.
*      READ TABLE es_infopt0113-item ASSIGNING <mem_rseg> INDEX lv_indx.
      READ TABLE lt_items_113 ASSIGNING <mem_rseg> INDEX lv_indx.
      IF sy-subrc = 0.
*        <mem_rseg>-p_vatamount = <mem_rseg>-p_vatamount + lv_diff.
      ENDIF.
    ENDLOOP.


*    READ TABLE lt_max_operamount INTO ls_max_operamount INDEX 1.
*    READ TABLE es_infopt0113-item INTO <mem_rseg> INDEX ls_max_operamount-tabix.
*
*    "сумма всех <VATAmount> с таким же mwskz
*    LOOP AT lt_max_operamount ASSIGNING FIELD-SYMBOL(<ls_max_operamount>) WHERE mwskz = ls_max_operamount-mwskz.
*      lv_sum_vatamount = <ls_max_operamount>-p_vatamount + lv_sum_vatamount.
*    ENDLOOP.
*
**4. Распределение разницы по НДС после определения  <VATAmount> для каждого EBELN/EBELP.
**   1) Для каждой строки (LT_RBTX) T_BSET, выполняем:
**    LOOP AT zcl_int003_helper=>mt_rbtx ASSIGNING FIELD-SYMBOL(<ls_rbtx>) WHERE mwskz = ls_max_operamount-mwskz.
**         ZZ = XX-YY
*    DATA(lv_vat_difference) = ls_max_operamount-mwskz - lv_sum_vatamount.
**         Если ZZ <> 0, где ZZ = XX-YY, то <VATAmount> = <VATAmount> + ZZ
**         для EBELN/EBELP с наибольшим значением <OperationAmount>  с учётом MWSKZ
*    IF lv_vat_difference <> 0.
*      <mem_rseg>-p_vatamount = <mem_rseg>-p_vatamount + lv_vat_difference.
*    ENDIF.
**    ENDLOOP.
    "} GINIYATYLLIN 20.04.2023
****************************************************************************************************


    "Рассчитывание разницы между полным счетом с суммой счетов из каждой позиции, добавляем разницу в первую позицию
*    LOOP AT es_infopt0113-item ASSIGNING <mem_rseg>.
    LOOP AT lt_items_113 ASSIGNING <mem_rseg>.
      lv_amount_sum_rub  = lv_amount_sum_rub  + <mem_rseg>-p_operationamount."сумма по позициям по стандарту
      lv_amount_sum_curr = lv_amount_sum_curr + <mem_rseg>-p_operationcurramount."сумма по позициям по стандарту
    ENDLOOP.

    IF ls_rbkp-currencycode <> lc_rub.
      lv_amount_diff_rub  = ls_rbkp-operationamount * ls_rbkp-kurs - lv_amount_sum_rub."разница
    ELSE.
      lv_amount_diff_rub  = ls_rbkp-operationamount - lv_amount_sum_rub."разница
    ENDIF.

    lv_amount_diff_curr = ls_rbkp-operationamount - lv_amount_sum_curr.

    IF lt_items_113 IS NOT INITIAL.
      lt_items_113[ 1 ]-p_operationamount       = lt_items_113[ 1 ]-p_operationamount     + lv_amount_diff_rub."Ндс для XML
      lt_items_113[ 1 ]-p_vatamount             = lt_items_113[ 1 ]-p_vatamount           + lv_amount_diff_rub.
*      es_infopt0113-item[ 1 ]-p_vatamount             = lv_amount_diff_rub.

      IF ls_rbkp-currencycode <> lc_rub.
        lt_items_113[ 1 ]-p_operationcurramount = lt_items_113[ 1 ]-p_operationcurramount + lv_amount_diff_curr.
        lt_items_113[ 1 ]-p_vatcurramount       = lt_items_113[ 1 ]-p_vatcurramount       + lv_amount_diff_curr.
      ELSE.
        lt_items_113[ 1 ]-p_vatcurramount       = lt_items_113[ 1 ]-p_vatamount.
      ENDIF.
*      <ls_mat_move>-position[ 1 ]-vatamount = <ls_mat_move>-position[ 1 ]-vatamount + lv_amount_diff.
    ENDIF.

***    _add_sum_diff_pt0113( EXPORTING iv_belnr = iv_belnr iv_gjahr = iv_gjahr CHANGING cs_data = es_infopt0113 ).

*    EXPORT tab = mem_rseg_113[] TO MEMORY ID 'mem_rseg_113'.



    lv_vorgang = COND #( WHEN ls_rseg-tbtkz IS INITIAL
                         THEN COND #( WHEN ls_rseg-shkzg = lc_shkzgs
                                      THEN lc_check ELSE lc_creditinvoice )
                         ELSE COND #( WHEN ls_rseg-shkzg = lc_shkzgh
                                      THEN lc_additionallending ELSE lc_additionaldebit ) ).


    SELECT *
      FROM mara
      WHERE mara~matnr = @ls_rseg-matnr
      INTO @DATA(ls_mara).
    ENDSELECT.

    "{baryutinsa 29.06.23 схб ремонт закоментил для переноса
    SELECT SINGLE vorgang, knttp, code_doc_abc
      FROM zint_mm_viddvig
      WHERE bsart IN @lt_rng_bsart_usl AND
            knttp EQ @ls_rseg-knttp AND
            vorgang EQ @lv_vorgang
*        AND ( flow = 'ПТ0113' OR flow IS INITIAL )
      INTO @DATA(ls_vid).
    "}

*    IF ls_vid IS INITIAL.
*      MESSAGE e076(zmmint0003_msg) DISPLAY LIKE 'I'.
*      RETURN.
*    ENDIF.

*    "При дополнительном дебетовании и типе контировки из stvarv не отправлять
*    IF lv_vorgang = lc_additionaldebit AND
*       ls_rseg-knttp IN lt_rng_knttp_usl.
*      RETURN.
*    ENDIF.


    CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
      EXPORTING
        i_bukrs = lc_bukrs
        i_mwskz = ls_rseg-mwskz
        i_waers = ls_rseg-currencycode
        i_wrbtr = CONV wrbtr( ls_rbkp-vatamount )
      TABLES
        t_mwdat = lt_mwdat.

    IF lt_mwdat IS NOT INITIAL.
      READ TABLE lt_mwdat INTO DATA(ls_mwdat) INDEX 1.
    ENDIF.


    IF ls_rseg-relevanteval = 'YON' AND ls_rseg-zzaddchar = 1.
      lv_hozoperationtype = 'SHB_ЗАК_СК'.
    ELSEIF ls_rseg-relevanteval = 'YON' AND ( ls_rseg-zzaddchar = 2 OR ls_rseg-zzaddchar = 3 ).
      lv_hozoperationtype = 'SHB_ЗАК_РАСХОДЫ'.
    ELSEIF ls_rseg-relevanteval <> 'YON' AND ls_rseg-serv_comp = 'Да'.
      lv_hozoperationtype = 'SHB_ЗАК_РАСХОДЫ'.
    ENDIF.

*    IF lv_vorgang = lc_check.
*      lv_hozoperationtype = ls_vid-code_doc_abc.
*    ENDIF.

    DATA lv_currencycode TYPE waers.

    IF ls_rseg-currencycode+0(1) EQ 'U' AND strlen( ls_rseg-currencycode ) >= 4.
      REPLACE FIRST OCCURRENCE OF 'U' IN ls_rseg-currencycode WITH ''.
    ENDIF.

    IF ls_rbkp-currencycode+0(1) EQ 'U' AND strlen( ls_rbkp-currencycode ) >= 4.
      REPLACE FIRST OCCURRENCE OF 'U' IN ls_rbkp-currencycode WITH ''.
    ENDIF.

    lv_accnt8 = lv_accnt.
    lv_accnt8 = |{ lv_accnt8 ALPHA = IN }|.
    lv_accnt = lv_accnt8.
    "Общее заполнение
    es_infopt0113 = VALUE #( BASE es_infopt0113
                             material_doc_num    = ls_rbkp-material_doc_num
                             material_doc_year   = ls_rbkp-material_doc_year
                             plandate            = ls_rbkp-plandate
                             paymentpurpose      = ls_rbkp-paymentpurpose
                             vatrate             = ls_mwdat-msatz
                             vatamount           = ''"ls_rbkp-vatamount
                             operationamount     = ls_rbkp-operationamount
*                             isvatincluded       = COND #( WHEN ls_rseg-mwskz = lc_r2 THEN lc_true ELSE lc_false )
                             isvatincluded       = COND #( WHEN ls_rseg-mwskz IS NOT INITIAL THEN lc_true ELSE lc_false )
                             isdaterate          = lc_false
                             currencycode        = COND #( WHEN ls_rbkp-currencycode IS NOT INITIAL THEN ls_rbkp-currencycode ELSE ls_rseg-currencycode )
                             agreementid         = ls_rseg-agreementid
                             cftagreementid      = ls_rseg-cftagreementid
                             departmentid        = |0{ ls_rseg-werks(2) }-0{ ls_rseg-werks+2(2) }|
                             werks               = ls_rseg-werks
*                             hozoperationtype    = COND #( WHEN lv_vorgang = lc_check
*                                                           THEN VALUE #( lt_vid[ vorgang = lv_vorgang knttp = ls_rseg-knttp ]-code_doc_abc OPTIONAL )
*                                                           ELSE VALUE #( lt_vid[ vorgang = lv_vorgang ]-code_doc_abc OPTIONAL ) )
                            hozoperationtype = COND #( WHEN lv_hozoperationtype IS NOT INITIAL THEN lv_hozoperationtype ELSE
                                               COND #( WHEN ls_vid IS INITIAL THEN '' ELSE
                                               COND #( WHEN ls_rseg-relevanteval EQ 'YON' AND ls_vid-vorgang EQ '1'
                                                       THEN 'SHB_ЗАК_СК' ELSE ls_vid-code_doc_abc )
                            ) )
                            userpersonnelnumber  = lv_accnt
                            originalaccountnumber =  ls_rbkp-glo_ref1_hd
*                            originalaccountnumber = COND #( WHEN  ls_rbkp-xblnr IS NOT INITIAL THEN |{ ls_rbkp-xblnr } { ls_rbkp-glo_ref1_hd }|
*                                                            WHEN  ls_rbkp-xblnr IS INITIAL THEN space )
                            originalaccountdate = ls_rbkp-originalaccountdate
                            order_num           = lv_ebeln
                            ).


    "Заполнение для валютных
    IF es_infopt0113-currencycode <> lc_rub OR es_infopt0113-currencycode IS INITIAL.

      "Конвертируем дату в формат TCURR
*      TRY.
*          lv_invdat = ls_rbkp-plandate.
*          TRANSLATE lv_invdat USING '09182736455463728190'.
*        CATCH cx_root.
*      ENDTRY.
*
*      ls_rseg-ratetype = COND #( WHEN ls_rseg-ratetype IS NOT INITIAL THEN ls_rseg-ratetype ELSE lc_kurst ).
*
*      SELECT SINGLE ukurs, gdatu
*        FROM tcurr
*        WHERE fcurr = @es_infopt0113-currencycode
*          AND tcurr = @lc_rub
*          AND kurst = @ls_rseg-ratetype
*          AND gdatu = @lv_invdat
*        INTO @DATA(ls_ukurs).
*
*      IF sy-subrc = 0.
*
*        DATA(lv_ukurs) = ls_ukurs-ukurs.
*        DATA(lv_gdatu) = ls_ukurs-gdatu.
*
*      ELSE.
*
*        SELECT ukurs, gdatu
*          FROM tcurr
*          INTO TABLE @DATA(lt_ukurs)
*          UP TO 1 ROWS
*          WHERE fcurr = @es_infopt0113-currencycode
*            AND tcurr = @lc_rub
*            AND kurst = @ls_rseg-ratetype"lc_kurst
*          ORDER BY gdatu DESCENDING.
*
*        IF sy-subrc = 0.
*
*          lv_ukurs = lt_ukurs[ 1 ]-ukurs.
*          lv_gdatu = lt_ukurs[ 1 ]-gdatu.
*
*        ELSE.
*
*          lv_ukurs = COND #( WHEN ls_rseg-kurs IS INITIAL THEN ls_rbkp-kurs ELSE ls_rseg-kurs ).
*          lv_gdatu = ls_rbkp-plandate.
*
*        ENDIF.
*
*      ENDIF.
*
*      TRY.
*          TRANSLATE lv_gdatu USING '09182736455463728190'.
*        CATCH cx_root.
*      ENDTRY.

      es_infopt0113 = VALUE #( BASE es_infopt0113
                               isdaterate          = lc_true
                               operationcurrrate   = ls_rbkp-kurs
                               ratecalcdate        = ls_rbkp-ratecalcdate
                               operationcurramount = ls_rbkp-operationamount
                               vatcurramount       = ls_rbkp-vatamount
                               operationamount     = ls_rseg-opercurrrate * ls_rbkp-operationamount
                               vatamount           = ''"lv_ukurs * ls_rbkp-vatamount
                               ratetype            = ls_rseg-ratetype
                               currencycode        = COND #( WHEN ls_rbkp-currencycode IS NOT INITIAL THEN ls_rbkp-currencycode ELSE ls_rseg-currencycode )
                               order_num           = lv_ebeln
                              ).

    ENDIF.


    SELECT SINGLE @abap_true
      FROM ztint_pt0113
      WHERE material_doc_num  = @es_infopt0113-material_doc_num
        AND material_doc_year = @es_infopt0113-material_doc_year
        AND process_status    = @zif_ifc_const=>mc_process_status-success
      INTO @DATA(lv_send).

    IF sy-subrc = 0.
      CLEAR es_infopt0113.
    ENDIF.


    "{ Shanarov B.V. - 11.05.2023 14:39:25
    "Агрегация позиций по «Фин.позиция» + "ставка НДС"

*    DATA(lt_item) = es_infopt0113-item.
    CLEAR es_infopt0113-item.

    SELECT 'I' AS sign, 'EQ' AS option, fipos AS low, @space AS high
      FROM ztmm_fipos_pt113
      INTO TABLE @lt_fipos.

    SELECT lt~*, ekkn~imkey, vibdro~zztypget
      FROM @lt_items_113 AS lt
      LEFT JOIN ekkn ON lt~ebeln = ekkn~ebeln
                    AND lt~ebelp = ekkn~ebelp
      LEFT JOIN vibdro ON ekkn~imkey = vibdro~imkey
      INTO TABLE @DATA(lt_new_items).

    CLEAR: lt_pt0113_agr.

    LOOP AT lt_new_items ASSIGNING FIELD-SYMBOL(<ls_item_t>).
*      WHERE lt-fipos IN lt_fipos AND lt-knttp = 'T'.
      IF <ls_item_t>-zztypget NOT IN lt_obj_rent AND <ls_item_t>-zztypget NOT IN lt_obj_own.
*        clear: ls_item_line.
*        ls_item_line = CORRESPONDING #( <ls_item_t>-lt ).
*        APPEND ls_item_line TO es_infopt0113-item.
        CLEAR: <ls_item_t>-lt-objty.
        APPEND <ls_item_t>-lt TO lt_pt0113_agr.
      ELSE.
        <ls_item_t>-lt-objty = COND #( WHEN <ls_item_t>-zztypget IN lt_obj_rent THEN '01' ELSE '02' ).
        APPEND <ls_item_t>-lt TO lt_pt0113_agr.
      ENDIF.
    ENDLOOP.

*      DELETE lt_item WHERE fipos IN lt_fipos AND knttp = 'T'.

*      LOOP AT lt_item INTO DATA(ls_item)
    LOOP AT lt_pt0113_agr INTO DATA(ls_item)
                    GROUP BY ( p_vatrate    = ls_item-p_vatrate
                               fipos        = ls_item-fipos
                               objty        = ls_item-objty
                               size         = GROUP SIZE )
*                               p_tmcname    = ls_item-p_tmcname
*                               p_usertabnum = ls_item-p_usertabnum
*                               p_comment    = ls_item-p_comment
*                               p_waers      = ls_item-p_waers )
                   INTO DATA(lo_item_gr).

      APPEND INITIAL LINE TO es_infopt0113-item ASSIGNING FIELD-SYMBOL(<mem_rseg_agr>).

      <mem_rseg_agr> = CORRESPONDING #( lo_item_gr ).

      DATA(lv_index) = 0.

      LOOP AT GROUP lo_item_gr ASSIGNING FIELD-SYMBOL(<ls_item>).

        lv_index += 1.

        IF lv_index = 1.

          "Собираем все аналитики из первой строки
          <mem_rseg_agr> = CORRESPONDING #( <ls_item> ).
          CONTINUE.

        ELSE.

          "Если мы позиции схлопываем, то TMCName оставлять пустым
          <mem_rseg_agr>-p_tmcname = '-'.

          <mem_rseg_agr>-p_vatamount           += <ls_item>-p_vatamount.
          <mem_rseg_agr>-p_vatcurramount       += <ls_item>-p_vatcurramount.
          <mem_rseg_agr>-p_operationamount     += <ls_item>-p_operationamount.
          <mem_rseg_agr>-p_operationcurramount += <ls_item>-p_operationcurramount.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
    "} Shanarov B.V. - 11.05.2023 14:39:25


*{KvizhinadzeVS 24.12.2023
    es_infopt0113-primedocs = _get_primedocs( iv_belnr = iv_belnr
                                              iv_gjahr = iv_gjahr ).
*}KvizhinadzeVS 24.12.2023


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_MIR7_770_REPAIR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        MBLNR(optional)
* | [--->] IV_GJAHR                       TYPE        MJAHR(optional)
* | [<-()] RS_DATA                        TYPE        ZST_0003_SEND_MATERIAL_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mir7_770_repair.

    CONSTANTS: lc_bukrs             TYPE bukrs VALUE '1000',
               lc_rub               TYPE waers VALUE 'RUB',

               lc_shkzgs            TYPE shkzg        VALUE 'S',
               lc_shkzgh            TYPE shkzg        VALUE 'H',
               lc_check             TYPE mrm_vorgang  VALUE '1', "Счет
               lc_creditinvoice     TYPE mrm_vorgang  VALUE '2', "Кредитовое авизо
               lc_additionaldebit   TYPE mrm_vorgang  VALUE '3', "Дополнительное дебетование
               lc_additionallending TYPE mrm_vorgang  VALUE '4'. "Дополнительное кредитование.



    DATA: lt_rng_ebeln     TYPE RANGE OF rseg-ebeln,
          lt_rng_matkl     TYPE RANGE OF mara-matkl,
          lt_rng_bprme     TYPE RANGE OF rseg-bprme,
          ls_pos           LIKE LINE OF rs_data-position,
          lt_txt_line      TYPE TABLE OF tline,
          lt_mwdat         TYPE cte_t_mwdat,
          lv_txt_name      TYPE thead-tdname,
          lt_rng_mtart_nma TYPE RANGE OF mtart,
          lr_anbwa_770     TYPE RANGE OF anbwa,
          lr_gkont         TYPE RANGE OF hkont,

          lv_amount_sum    TYPE salk3,
          lv_amount_diff   TYPE salk3.

***    DATA: lt_locationid TYPE tt_locationid,
***          lv_loc_active TYPE abap_bool.

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_MTART_NMA' IMPORTING er_range = lt_rng_mtart_nma ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name  = 'ZFIAAINT0013_ANBWA_980' IMPORTING er_range = lr_anbwa_770 ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name  = 'ZFIAAINT0013_GKONT_INITIAL' IMPORTING er_range = lr_gkont ).
***    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_LOCATION' IMPORTING ev_value = lv_loc_active ).

    "Заголовок
    SELECT SINGLE
           rbkp~belnr, rbkp~gjahr,
           rbkp~bldat, rbkp~budat,
           rbkp~xblnr, rbkp~zuonr,
           rbkp~lifnr, rbkp~usnam,
           rbkp~rmwwr,
           usr02~accnt
      FROM rbkp
        LEFT JOIN usr02
          ON rbkp~usnam = usr02~bname
      WHERE rbkp~belnr = @iv_belnr
        AND rbkp~gjahr = @iv_gjahr
      INTO @DATA(ls_rbkp).

    "Позиции
    SELECT rseg~belnr, rseg~gjahr,
           rseg~buzei, rseg~matnr,
           rseg~ebeln, rseg~ebelp,
           rseg~bprme, rseg~menge,
           rseg~salk3, rseg~mwskz,
           rseg~sgtxt, rseg~werks,
           rseg~wrbtr, rseg~tbtkz,
           rseg~shkzg, rseg~bukrs,
           mara~matkl, makt~maktx,
           mara~mtart,
           clgr~zcl, clgr~zgr
      FROM rseg
        JOIN mara
          ON rseg~matnr = mara~matnr
        LEFT JOIN makt
          ON mara~matnr = makt~matnr AND
             makt~spras = @sy-langu
        LEFT JOIN ztmm_cl_gr AS clgr
          ON mara~matkl = clgr~zmatkl
      WHERE rseg~belnr = @iv_belnr
        AND rseg~gjahr = @iv_gjahr
      INTO TABLE @DATA(lt_rseg).

    lt_rng_ebeln = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-ebeln ) ).
    lt_rng_matkl = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-matkl ) ).
    lt_rng_bprme = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-bprme ) ).


    "Дополнительные данные
    SELECT ekpo~ebeln, ekpo~ebelp,
           ekpo~brtwr, ekpo~netwr,
           ekpo~menge AS ekpo_menge,
           ekkn~menge AS ekkn_menge,
           anla~anln1, anla~anln2,
           itob~invnr, anlz~pernr,
           ekko~zzext_key, zdgv~abs_num,
           anlu~zztmcgrp, anlu~zztransfer,
           anlu~bukrs
      FROM ekko
        LEFT JOIN ekpo
          ON ekko~ebeln = ekpo~ebeln
        LEFT JOIN ekkn
          ON ekpo~ebeln = ekkn~ebeln AND
             ekpo~ebelp = ekkn~ebelp
        LEFT JOIN anla
          ON ekkn~anln1 = anla~anln1 AND
             ekkn~anln2 = anla~anln2
        LEFT JOIN anlz
          ON anla~anln1 = anlz~anln1 AND
             anla~anln2 = anlz~anln2 AND
             anla~bukrs = anlz~bukrs
        LEFT JOIN anlu
          ON anla~anln1 = anlu~anln1 AND
             anla~anln2 = anlu~anln2 AND
             anla~bukrs = anlu~bukrs
        LEFT JOIN scmg_t_case_attr AS case
          ON ekko~zzext_key = case~ext_key
        LEFT JOIN zrcmt_dgv_attr AS zdgv
          ON case~case_guid = zdgv~case_guid
        LEFT JOIN viaufks
          ON viaufks~aufnr = ekkn~aufnr
        LEFT JOIN equi
          ON equi~equnr = viaufks~equnr
        LEFT JOIN itob
          ON itob~equnr = equi~equnr
      INTO TABLE @DATA(lt_ekko)
      WHERE ekko~ebeln IN @lt_rng_ebeln
      ORDER BY ekpo~ebeln, ekpo~ebelp.

    SELECT msehi,
           mm_okei
      FROM zint_mm_okei
          WHERE msehi IN @lt_rng_bprme
      INTO TABLE @DATA(lt_mm_okei).

***    IF lv_loc_active = abap_true.
***      lt_locationid = CORRESPONDING #( lt_rseg ).
***      _get_locationid( CHANGING ct_locationid = lt_locationid ).
***    ENDIF.

    rs_data-materialdocnum    = ls_rbkp-belnr.
    rs_data-materialdocyear   = ls_rbkp-gjahr.
    rs_data-materialdocdate   = ls_rbkp-bldat.
    rs_data-entrysupposeddate = ls_rbkp-budat.
    rs_data-primarydocnum     = ls_rbkp-xblnr.
    rs_data-userlogin         = ls_rbkp-accnt.
    rs_data-supplierclientid  = ls_rbkp-lifnr.


    LOOP AT lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>).

      DATA(lv_tabix) = sy-tabix.

      DATA(ls_rseg) = lt_rseg[ ebeln = <ls_ekko>-ebeln ebelp = <ls_ekko>-ebelp ].

      IF lv_tabix = 1.
        rs_data-textheader        = ls_rseg-sgtxt.
        rs_data-ordernum          = ls_rseg-ebeln.
        rs_data-werksupplier      = rs_data-werkreceiver = |0{ ls_rseg-werks(2) }-0{ ls_rseg-werks+2(2) }|.
        rs_data-molreceivertabnum = <ls_ekko>-pernr.
        rs_data-molsuppliertabnum = <ls_ekko>-pernr.
        rs_data-cftagreementid    = <ls_ekko>-abs_num.
        rs_data-agreementid       = <ls_ekko>-zzext_key.

        DATA(lv_vorgang) = COND mrm_vorgang( WHEN ls_rseg-tbtkz IS INITIAL
                                             THEN SWITCH #( ls_rseg-shkzg
                                                            WHEN lc_shkzgs THEN lc_check
                                                            WHEN lc_shkzgh THEN lc_creditinvoice )
                                             ELSE SWITCH #( ls_rseg-shkzg
                                                            WHEN lc_shkzgs THEN lc_additionaldebit
                                                            WHEN lc_shkzgh THEN lc_additionallending ) ).

        " Дефект EXT_ITECO-541
        "---------------------
        IF <ls_ekko>-zztransfer = abap_true.
          rs_data-materialdoccode = 'ТМЦ_ДОНАЦЕНКА'.
        ELSE.
*          SELECT SINGLE code_doc_abc FROM zint_mm_viddvig WHERE knttp = 'A' AND vorgang = @lv_vorgang AND ( flow = 'ПТ0111' OR flow IS INITIAL ) INTO @rs_data-materialdoccode.
          SELECT SINGLE code_doc_abc FROM zint_mm_viddvig WHERE knttp = 'A' AND vorgang = @lv_vorgang INTO @rs_data-materialdoccode.
        ENDIF.
        "---------------------
      ENDIF.

      DATA(ls_tax) = get_tax( iv_mwskz = ls_rseg-mwskz iv_wrbtr = CONV #( ls_rseg-wrbtr ) ).

      " Шестаков А.О. 12.04.2023 - Правка дефекта
      "---------------------
      IF ls_rseg-mtart IN lt_rng_mtart_nma.
        ls_pos-tmcclass = ls_rseg-zcl.
        ls_pos-tmcgroup = ls_rseg-zgr.
      ELSE.
        SELECT SINGLE zcl, zgr
          FROM ztmm_cl_gr
          INTO @DATA(ls_cl_gr)
          WHERE zmatkl = @<ls_ekko>-zztmcgrp.

        ls_pos-tmcclass = ls_cl_gr-zcl.
        ls_pos-tmcgroup = ls_cl_gr-zgr.
      ENDIF.
      "---------------------

      ls_pos-tmcnum           = ls_rseg-matnr.

      ls_pos-vatcode          = ls_rseg-mwskz.
      ls_pos-vatrate          = ls_tax-msatz.

      ls_pos-vatamount        = COND #( WHEN lv_vorgang = 3
                                        THEN ls_tax-wmwst / <ls_ekko>-ekpo_menge
                                        ELSE ( <ls_ekko>-brtwr - <ls_ekko>-netwr ) / <ls_ekko>-ekpo_menge ).

      ls_pos-amount           = ls_rbkp-rmwwr / <ls_ekko>-ekpo_menge.
      ls_pos-quantity         = <ls_ekko>-ekkn_menge.

***      IF lv_loc_active = abap_true.
***        READ TABLE lt_locationid WITH KEY matnr = ls_rseg-matnr
***                                    werks = ls_rseg-werks
****                                  lgort = ls_rseg-lgort
***                                    ASSIGNING FIELD-SYMBOL(<ls_locid>).
***        IF sy-subrc = 0.
***          ls_pos-locationid = <ls_locid>-locationid.
***        ENDIF.
***      ENDIF.


      ls_pos-unitmeasurecode  = VALUE #( lt_mm_okei[ msehi = ls_rseg-bprme ]-mm_okei OPTIONAL ).

      ls_pos-inventorynum     = <ls_ekko>-invnr.
      ls_pos-oscardnum        = |{ <ls_ekko>-anln1 }{ <ls_ekko>-anln2 }|.
      "------------------------------
      " >> 01.06.2023 KVIZHINAD-VS
      IF rs_data-materialdoccode = 'ТМЦ_ДОНАЦЕНКА'.
        DATA(lv_anlhtxt) = VALUE anlhtxt( ).
        IF _materialdoccode_donachenka( EXPORTING
                                         iv_anln1    = <ls_ekko>-anln1
                                         iv_anln2    = <ls_ekko>-anln2
                                         iv_bukrs    = <ls_ekko>-bukrs
                                         ir_anbwa    = lr_anbwa_770
                                         ir_gkont    = lr_gkont
                                        IMPORTING
                                         ev_anlhtxt  = lv_anlhtxt ).
          ls_pos-oscardnum = ''.
          IF lv_anlhtxt IS NOT INITIAL.
            ls_pos-mataccount = lv_anlhtxt.
          ENDIF.
        ELSE.
          CLEAR ls_pos-mataccount.
        ENDIF.
      ENDIF.
      " << 01.06.2023 KVIZHINAD-VS
      "------------------------------

      lv_txt_name = |{ <ls_ekko>-ebeln }{ <ls_ekko>-ebelp }|.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = 'F01'
          language = sy-langu
          name     = lv_txt_name
          object   = 'EKPO'
        TABLES
          lines    = lt_txt_line
        EXCEPTIONS
          OTHERS   = 8.

      IF sy-subrc = 0 AND lt_txt_line IS NOT INITIAL.
        ls_pos-tmcname        = |{ ls_rseg-maktx } { lt_txt_line[ 1 ]-tdline }|.
      ELSE.
        ls_pos-tmcname        = |{ ls_rseg-maktx }|.
      ENDIF.


      APPEND ls_pos TO rs_data-position.

    ENDLOOP.


    "Рассчитывание разницы между полным счетом с суммой счетов из каждой позиции, добавляем разницу в первую позицию
    LOOP AT rs_data-position INTO ls_pos.
      lv_amount_sum = lv_amount_sum + ls_pos-amount.
    ENDLOOP.

    lv_amount_diff = ls_rbkp-rmwwr - lv_amount_sum.

    IF lv_amount_diff IS NOT INITIAL.
      _add_sum_diff( EXPORTING iv_belnr = iv_belnr iv_gjahr = iv_gjahr
                     CHANGING cs_data = rs_data ).
    ENDIF.

    "Дефект EXT_ITECO-274
    "-------------------------------------
    LOOP AT rs_data-position ASSIGNING FIELD-SYMBOL(<ls_def274>)
         WHERE mataccount IS NOT INITIAL.
      CLEAR: <ls_def274>-batchreceivernum, <ls_def274>-batchsuppliernum.
    ENDLOOP.
    "-------------------------------------

    IF rs_data-agreementid IS NOT INITIAL.
      CLEAR rs_data-supplierclientid.
    ENDIF.

*{KvizhinadzeVS 24.12.2023
    rs_data-primedocs = _get_primedocs( iv_belnr = iv_belnr
                                        iv_gjahr = iv_gjahr ).
*}KvizhinadzeVS 24.12.2023


    SELECT SINGLE @abap_true
      FROM ztint_pt0111
      WHERE material_doc_num  = @rs_data-materialdocnum
        AND material_doc_year = @rs_data-materialdocyear
        AND process_status    = @zif_ifc_const=>mc_process_status-success
      INTO @DATA(lv_send).

    IF sy-subrc = 0.
      CLEAR rs_data.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_MIR7_770_NMA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BELNR                       TYPE        MBLNR(optional)
* | [--->] IV_GJAHR                       TYPE        MJAHR(optional)
* | [<-()] RS_DATA                        TYPE        ZST_0003_SEND_MATERIAL_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mir7_770_nma.

    CONSTANTS: lc_bukrs             TYPE bukrs VALUE '1000',
               lc_rub               TYPE waers VALUE 'RUB',

               lc_shkzgs            TYPE shkzg        VALUE 'S',
               lc_shkzgh            TYPE shkzg        VALUE 'H',
               lc_check             TYPE mrm_vorgang  VALUE '1', "Счет
               lc_creditinvoice     TYPE mrm_vorgang  VALUE '2', "Кредитовое авизо
               lc_additionaldebit   TYPE mrm_vorgang  VALUE '3', "Дополнительное дебетование
               lc_additionallending TYPE mrm_vorgang  VALUE '4'. "Дополнительное кредитование.


    TYPES: BEGIN OF ty_ekkn_aufnr2,
             aufnr TYPE j_objnr,
           END OF ty_ekkn_aufnr2.



    DATA: lt_rng_ebeln     TYPE RANGE OF rseg-ebeln,
          lt_rng_ebelp     TYPE RANGE OF rseg-ebelp,
          lt_rng_matkl     TYPE RANGE OF mara-matkl,
          lt_rng_bprme     TYPE RANGE OF rseg-bprme,
          ls_pos           LIKE LINE OF rs_data-position,
          lt_txt_line      TYPE TABLE OF tline,
          lt_mwdat         TYPE cte_t_mwdat,
          lv_txt_name      TYPE thead-tdname,
          lt_rng_rbstat    TYPE RANGE OF rbstat,
          lt_rng_mtart_nma TYPE RANGE OF mtart,

          lv_amount_sum    TYPE salk3,
          lv_amount_diff   TYPE salk3,

          lt_rng_anln1     TYPE RANGE OF anln1,
          ls_rng_anln1     LIKE LINE OF lt_rng_anln1,
          lt_rng_anln2     TYPE RANGE OF anln2,
          ls_rng_anln2     LIKE LINE OF lt_rng_anln2,
          lt_ekkn_aufnr2   TYPE TABLE OF ty_ekkn_aufnr2,

          lr_anbwa_770     TYPE RANGE OF anbwa,
          lr_gkont         TYPE RANGE OF hkont.

***    DATA: lt_locationid TYPE tt_locationid,
***          lv_loc_active TYPE abap_bool.

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_MTART_NMA' IMPORTING er_range = lt_rng_mtart_nma ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name  = 'ZFIAAINT0013_ANBWA_980' IMPORTING er_range = lr_anbwa_770 ).

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZMM003_RBSTAT' IMPORTING er_range = lt_rng_rbstat ).
    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_MTART_NMA' IMPORTING er_range = lt_rng_mtart_nma ).

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name  = 'ZFIAAINT0013_GKONT_INITIAL' IMPORTING er_range = lr_gkont ).
**    zcl_tvarvc_params_get=>get_value( EXPORTING iv_name = 'ZMM003_LOCATION' IMPORTING ev_value = lv_loc_active ).

    "Заголовок
    SELECT SINGLE
           rbkp~belnr, rbkp~gjahr,
           rbkp~bldat, rbkp~budat,
           rbkp~xblnr, rbkp~zuonr,
           rbkp~lifnr, rbkp~usnam,
           rbkp~rmwwr, rbkp~sgtxt,
           usr02~accnt
      FROM rbkp
        LEFT JOIN usr02
          ON rbkp~usnam = usr02~bname
      WHERE rbkp~belnr = @iv_belnr
        AND rbkp~gjahr = @iv_gjahr
        AND rbstat IN @lt_rng_rbstat
      INTO @DATA(ls_rbkp).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Позиции
    SELECT rseg~belnr, rseg~gjahr,
           rseg~buzei, rseg~matnr,
           rseg~ebeln, rseg~ebelp,
           rseg~bprme, rseg~menge,
           rseg~salk3, rseg~mwskz,
           rseg~sgtxt, rseg~werks,
           rseg~wrbtr, rseg~tbtkz,
           rseg~shkzg, rseg~bukrs,
           mara~matkl, makt~maktx,
           mara~mtart,
           clgr~zcl, clgr~zgr
      FROM rseg
        JOIN mara
          ON rseg~matnr = mara~matnr
        LEFT JOIN makt
          ON mara~matnr = makt~matnr AND
             makt~spras = @sy-langu
        LEFT JOIN ztmm_cl_gr AS clgr
          ON mara~matkl = clgr~zmatkl
      WHERE rseg~belnr = @iv_belnr
        AND rseg~gjahr = @iv_gjahr
      INTO TABLE @DATA(lt_rseg).

    lt_rng_ebeln = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-ebeln ) ).
    lt_rng_ebelp = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-ebelp ) ).
    lt_rng_matkl = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-matkl ) ).
    lt_rng_bprme = VALUE #( FOR ls_rs IN lt_rseg sign = 'I' option = 'EQ' ( low = ls_rs-bprme ) ).


    "Дополнительные данные
    "Дефект EXT_ITECO-541
    "---------------------
    SELECT ekkn~aufnr
      FROM ekko
        JOIN rseg
          ON ekko~ebeln = rseg~ebeln
        JOIN ekpo
          ON ekko~ebeln = ekpo~ebeln
        JOIN ekkn
          ON ekkn~ebeln = ekpo~ebeln
        JOIN aufk
          ON aufk~aufnr = ekkn~aufnr AND aufk~autyp = '30'
      WHERE ekpo~knttp = 'F'
        AND ekpo~loekz = @abap_false
        AND rseg~belnr = @iv_belnr
        AND rseg~gjahr = @iv_gjahr
      INTO TABLE @DATA(lt_ekkn_aufnr).

    IF lt_ekkn_aufnr IS INITIAL.
      SELECT ekpo~ebeln, ekpo~ebelp,
             ekpo~brtwr, ekpo~netwr,
             ekpo~menge AS ekpo_menge,
             ekkn~menge AS ekkn_menge,
             anla~anln1, anla~anln2,
             anla~invnr, anlz~pernr,
             ekko~zzext_key, zdgv~abs_num,
             anlu~zztransfer, anlu~zztmcgrp,
             anlu~bukrs
        FROM ekko
          LEFT JOIN ekpo
            ON ekko~ebeln = ekpo~ebeln
          LEFT JOIN ekkn
            ON ekpo~ebeln = ekkn~ebeln AND
               ekpo~ebelp = ekkn~ebelp
          LEFT JOIN anla
            ON ekkn~anln1 = anla~anln1 AND
               ekkn~anln2 = anla~anln2
          LEFT JOIN anlz
            ON anla~anln1 = anlz~anln1 AND
               anla~anln2 = anlz~anln2 AND
               anla~bukrs = anlz~bukrs
          LEFT JOIN anlu
            ON anla~bukrs = anlu~bukrs AND
               anla~anln1 = anlu~anln1 AND
               anla~anln2 = anlu~anln2
          LEFT JOIN scmg_t_case_attr AS case
            ON ekko~zzext_key = case~ext_key
          LEFT JOIN zrcmt_dgv_attr AS zdgv
            ON case~case_guid = zdgv~case_guid
        INTO TABLE @DATA(lt_ekko)
        WHERE ekko~ebeln IN @lt_rng_ebeln
          AND ekpo~ebelp IN @lt_rng_ebelp
          AND ekpo~loekz = @abap_false
        ORDER BY ekpo~ebeln, ekpo~ebelp.
    ELSE.
      LOOP AT lt_ekkn_aufnr INTO DATA(ls_ekkn_aufnr).
        APPEND INITIAL LINE TO lt_ekkn_aufnr2 ASSIGNING FIELD-SYMBOL(<ls_ekkn_aufnr2>).

        <ls_ekkn_aufnr2>-aufnr = 'OR' && ls_ekkn_aufnr-aufnr.
      ENDLOOP.

      SELECT ekpo~ebeln, ekpo~ebelp,
             ekpo~brtwr, ekpo~netwr,
             ekpo~menge AS ekpo_menge,
             ekkn~menge AS ekkn_menge,
             cobrb~anln1, cobrb~anln2,
             anla~invnr, anlz~pernr,
             ekko~zzext_key, zdgv~abs_num,
             anlu~zztransfer, anlu~zztmcgrp,
             anlu~bukrs
        FROM ekko
          LEFT JOIN ekpo
            ON ekko~ebeln = ekpo~ebeln
          LEFT JOIN ekkn
            ON ekpo~ebeln = ekkn~ebeln AND
               ekpo~ebelp = ekkn~ebelp
          LEFT JOIN cobrb
            ON cobrb~objnr <> @abap_false
          LEFT JOIN anla
            ON cobrb~anln1 = anla~anln1 AND
               cobrb~anln2 = anla~anln2
          LEFT JOIN anlz
            ON anla~anln1 = anlz~anln1 AND
               anla~anln2 = anlz~anln2 AND
               anla~bukrs = anlz~bukrs
*            and anlz~BDATU = ?
          LEFT JOIN anlu
            ON anla~bukrs = anlu~bukrs AND
               anla~anln1 = anlu~anln1 AND
               anla~anln2 = anlu~anln2
          LEFT JOIN scmg_t_case_attr AS case
            ON ekko~zzext_key = case~ext_key
          LEFT JOIN zrcmt_dgv_attr AS zdgv
            ON case~case_guid = zdgv~case_guid
        INTO TABLE @lt_ekko
        FOR ALL ENTRIES IN @lt_ekkn_aufnr2
        WHERE ekko~ebeln  IN @lt_rng_ebeln
          AND ekpo~ebelp  IN @lt_rng_ebelp
          AND ekpo~loekz  = @abap_false
          AND cobrb~objnr = @lt_ekkn_aufnr2-aufnr.

      SORT lt_ekko ASCENDING BY ebeln ebelp.
    ENDIF.
    "---------------------

    "{baryutin-sa 05.07.23 удаление дубликантов по anlz
    SORT lt_ekko BY ebeln ebelp anln1 anln2.
    DELETE ADJACENT DUPLICATES FROM lt_ekko COMPARING ebeln ebelp anln1 anln2.
    "}baryutin-sa 05.07.23

    SELECT msehi,
           mm_okei
      FROM zint_mm_okei
          WHERE msehi IN @lt_rng_bprme
      INTO TABLE @DATA(lt_mm_okei).

***    IF lv_loc_active = abap_true.
***      lt_locationid = CORRESPONDING #( lt_rseg ).
***      _get_locationid( CHANGING ct_locationid = lt_locationid ).
***    ENDIF.

    rs_data-materialdocnum    = ls_rbkp-belnr.
    rs_data-materialdocyear   = ls_rbkp-gjahr.
    rs_data-materialdocdate   = ls_rbkp-bldat.
    rs_data-entrysupposeddate = ls_rbkp-budat.
    rs_data-primarydocnum     = ls_rbkp-xblnr.
    rs_data-userlogin         = CONV persno( |{ ls_rbkp-accnt ALPHA = IN }| ). " Eremetov 02.03.2023 14:33:45
    rs_data-supplierclientid  = ls_rbkp-lifnr.


    LOOP AT lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>).

      DATA(lv_tabix) = sy-tabix.

      DATA(ls_rseg) = VALUE #( lt_rseg[ ebeln = <ls_ekko>-ebeln ebelp = <ls_ekko>-ebelp ] OPTIONAL ).

      IF lv_tabix = 1.
        rs_data-textheader        = ls_rbkp-sgtxt.
        rs_data-ordernum          = ls_rseg-ebeln.
        rs_data-werksupplier      = rs_data-werkreceiver = |0{ ls_rseg-werks(2) }-0{ ls_rseg-werks+2(2) }|.
        rs_data-molreceivertabnum = <ls_ekko>-pernr.
        rs_data-molsuppliertabnum = <ls_ekko>-pernr.
        rs_data-cftagreementid    = <ls_ekko>-abs_num.
        rs_data-agreementid       = <ls_ekko>-zzext_key.

        DATA(lv_vorgang) = COND mrm_vorgang( WHEN ls_rseg-tbtkz IS INITIAL
                                             THEN SWITCH #( ls_rseg-shkzg
                                                            WHEN lc_shkzgs THEN lc_check
                                                            WHEN lc_shkzgh THEN lc_creditinvoice )
                                             ELSE SWITCH #( ls_rseg-shkzg
                                                            WHEN lc_shkzgs THEN lc_additionaldebit
                                                            WHEN lc_shkzgh THEN lc_additionallending ) ).

        " Дефект EXT_ITECO-541
        "---------------------
        IF <ls_ekko>-zztransfer = abap_true.
          rs_data-materialdoccode = 'ТМЦ_ДОНАЦЕНКА'.
        ELSE.
*          SELECT SINGLE code_doc_abc FROM zint_mm_viddvig WHERE knttp = 'A' AND vorgang = @lv_vorgang AND ( flow = 'ПТ0111' OR flow IS INITIAL ) INTO @rs_data-materialdoccode.
          SELECT SINGLE code_doc_abc FROM zint_mm_viddvig WHERE knttp = 'A' AND vorgang = @lv_vorgang INTO @rs_data-materialdoccode.
        ENDIF.
        "---------------------
      ENDIF.

      DATA(ls_tax) = get_tax( iv_mwskz = ls_rseg-mwskz iv_wrbtr = CONV #( ls_rseg-wrbtr ) ).

      ls_pos-inventorynum     = <ls_ekko>-invnr.
      ls_pos-oscardnum        = |{ <ls_ekko>-anln1 }{ <ls_ekko>-anln2 }|.


      " Шестаков А.О. 12.04.2023 - Правка дефекта
      "---------------------
      IF ls_rseg-mtart NOT IN lt_rng_mtart_nma AND ls_pos-oscardnum IS NOT INITIAL.
        SELECT SINGLE zcl, zgr
          FROM ztmm_cl_gr
          INTO @DATA(ls_cl_gr)
          WHERE zmatkl = @<ls_ekko>-zztmcgrp.

        ls_pos-tmcclass = ls_cl_gr-zcl.
*        ls_pos-tmcgroup = ls_cl_gr-zgr.
        "03/07/2023 kovaleva-ms{
        SELECT SINGLE zgr
      FROM  ztmm_cl_gr
      INNER JOIN mara ON  ztmm_cl_gr~zmatkl = mara~matkl
                         AND mara~matnr = @ls_rseg-matnr
      INTO @DATA(lv_zgr).
        IF ( lv_zgr = '60415ОС_ЗЕМ_УЧ' OR lv_zgr = '60415ОС_ЗДАН' ).
          ls_pos-tmcgroup = lv_zgr.
        ELSE.
          ls_pos-tmcgroup = '60415ОС_КОМПЛЕКТ'.
        ENDIF.
        "03/07/2023 kovaleva-ms}
        ls_pos-quantity = '1'.
      ELSE.
        ls_pos-tmcclass = ls_rseg-zcl.
        ls_pos-tmcgroup = ls_rseg-zgr.
        ls_pos-quantity = <ls_ekko>-ekkn_menge.
      ENDIF.
      "---------------------
** { KASHIEV-AV 15.06.2023 13:38:35 ЗНИ 97
      IF ls_rseg-mwskz = 'P0' OR
         ls_rseg-mwskz = 'PP' OR
         ls_rseg-mwskz = 'R0'.
        SELECT SINGLE zclbnds
        FROM ztmm_cl_gr
        WHERE zmatkl = @ls_rseg-matkl
        INTO @ls_pos-tmcclass.
      ELSE.
        SELECT SINGLE zcl
        FROM ztmm_cl_gr
        WHERE zmatkl = @ls_rseg-matkl
        INTO @ls_pos-tmcclass.
      ENDIF.
** } KASHIEV-AV 15.06.2023 13:38:35
      ls_pos-tmcnum           = ls_rseg-matnr.

      ls_pos-vatcode          = ls_rseg-mwskz.
      ls_pos-vatrate          = ls_tax-msatz.

      IF ls_rseg-mtart IN lt_rng_mtart_nma. " 27.04.2023 Заплатка дефекта EXT_ITECO-541
        ls_pos-vatamount        = COND #( WHEN lv_vorgang = 3
                                          THEN ls_tax-wmwst / <ls_ekko>-ekpo_menge
                                          ELSE ( <ls_ekko>-brtwr - <ls_ekko>-netwr ) / <ls_ekko>-ekpo_menge ).
      ELSE.
        ls_pos-vatamount        = COND #( WHEN lv_vorgang = 3
                                          THEN ls_tax-wmwst / ls_pos-quantity
                                          ELSE ( <ls_ekko>-brtwr - <ls_ekko>-netwr ) / ls_pos-quantity ).
      ENDIF.

***      IF lv_loc_active = abap_true.
***        READ TABLE lt_locationid WITH KEY matnr = ls_rseg-matnr
***                                    werks = ls_rseg-werks
***                                    ASSIGNING FIELD-SYMBOL(<ls_locid>).
***        IF sy-subrc = 0.
***          ls_pos-locationid = <ls_locid>-locationid.
***        ENDIF.
***      ENDIF.

*      ls_pos-amount           = ls_rbkp-rmwwr / <ls_ekko>-ekpo_menge.
*      ls_pos-amount           = <ls_ekko>-brtwr / <ls_ekko>-ekpo_menge.
*      ls_pos-amount           = ls_rseg-wrbtr / ls_rseg-menge + ls_pos-vatamount.

      IF ls_rseg-mtart IN lt_rng_mtart_nma. " 27.04.2023 Заплатка дефекта EXT_ITECO-541
        ls_pos-amount           = ls_rseg-wrbtr / ls_rseg-menge + ls_pos-vatamount.
      ELSE.
        ls_pos-amount           = ls_rseg-wrbtr + ls_pos-vatamount.
      ENDIF.


      ls_pos-unitmeasurecode  = VALUE #( lt_mm_okei[ msehi = ls_rseg-bprme ]-mm_okei OPTIONAL ).

      lv_txt_name = |{ <ls_ekko>-ebeln }{ <ls_ekko>-ebelp }|.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = 'F01'
          language = sy-langu
          name     = lv_txt_name
          object   = 'EKPO'
        TABLES
          lines    = lt_txt_line
        EXCEPTIONS
          OTHERS   = 8.

      IF sy-subrc = 0 AND lt_txt_line IS NOT INITIAL.
        ls_pos-tmcname        = |{ ls_rseg-maktx } { lt_txt_line[ 1 ]-tdline }|.
      ELSE.
        ls_pos-tmcname        = |{ ls_rseg-maktx }|.
      ENDIF.

      "------------------------------
      " >> 01.06.2023 KVIZHINAD-VS
      IF rs_data-materialdoccode = 'ТМЦ_ДОНАЦЕНКА'.
        DATA(lv_anlhtxt) = VALUE anlhtxt( ).
        IF _materialdoccode_donachenka( EXPORTING
                                          iv_anln1   = <ls_ekko>-anln1
                                          iv_anln2   = <ls_ekko>-anln2
                                          iv_bukrs   = <ls_ekko>-bukrs
                                          ir_anbwa   = lr_anbwa_770
                                          ir_gkont   = lr_gkont
                                        IMPORTING
                                          ev_anlhtxt = lv_anlhtxt ).
          ls_pos-oscardnum = ''.
          IF lv_anlhtxt IS NOT INITIAL.
            ls_pos-mataccount = lv_anlhtxt.
          ENDIF.
        ELSE.
          CLEAR ls_pos-mataccount.
        ENDIF.
      ENDIF.
      " << 01.06.2023 KVIZHINAD-VS
      "------------------------------

      APPEND ls_pos TO rs_data-position.

    ENDLOOP.

    "Рассчитывание разницы между полным счетом с суммой счетов из каждой позиции, добавляем разницу в первую позицию
    LOOP AT rs_data-position INTO ls_pos.
      lv_amount_sum = lv_amount_sum + ls_pos-amount.
    ENDLOOP.

    lv_amount_diff = ls_rbkp-rmwwr - lv_amount_sum.

    IF lv_amount_diff IS NOT INITIAL.
      _add_sum_diff( EXPORTING iv_belnr = iv_belnr iv_gjahr = iv_gjahr
                     CHANGING cs_data = rs_data ).
    ENDIF.

*    IF rs_data-position IS NOT INITIAL.
*      rs_data-position[ 1 ]-amount = rs_data-position[ 1 ]-amount + lv_amount_diff.
*      rs_data-position[ 1 ]-vatamount = rs_data-position[ 1 ]-vatamount + lv_amount_diff.
*    ENDIF.




    "Дефект EXT_ITECO-274
    "-------------------------------------
    LOOP AT rs_data-position ASSIGNING FIELD-SYMBOL(<ls_def274>)
         WHERE mataccount IS NOT INITIAL.
      CLEAR: <ls_def274>-batchreceivernum, <ls_def274>-batchsuppliernum.
    ENDLOOP.
    "-------------------------------------

    IF rs_data-agreementid IS NOT INITIAL.
      CLEAR rs_data-supplierclientid.
    ENDIF.

*{KvizhinadzeVS 24.12.2023
    rs_data-primedocs = _get_primedocs( iv_belnr = iv_belnr
                                        iv_gjahr = iv_gjahr ).
*}KvizhinadzeVS 24.12.2023

    SELECT SINGLE @abap_true
      FROM ztint_pt0111
      WHERE material_doc_num  = @rs_data-materialdocnum
        AND material_doc_year = @rs_data-materialdocyear
        AND process_status    = @zif_ifc_const=>mc_process_status-success
      INTO @DATA(lv_send).

    IF sy-subrc = 0.
      CLEAR rs_data.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>GET_MIR7_230
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ORDERNUM                    TYPE        EBELN
* | [--->] IV_CFTAGREEMENTID              TYPE        ZE_ABS_NUM
* | [<-()] ES_INFPOT0136                  TYPE        ZSINT0003_PT0136_PLCAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mir7_230.


*    DATA: lt_rng_bsart_usl TYPE RANGE OF ekko-bsart.
*    " lt_mat_move      TYPE zsint0003_pt0136_plcal.
*
*    CONSTANTS: lc_zabs TYPE string VALUE 'ZABS'.
*
*    zcl_tvarvc_params_get=>get_range(
*      EXPORTING
*        iv_name  = 'ZMMINT0003_ VAL'
*      IMPORTING
*        er_range =  lt_rng_bsart_usl ).
*
*    SELECT SINGLE bukrs
*                 ,ebeln
*                 ,lifnr
*                 ,waers
*                 ,zzext_key
*      FROM ekko
*      INTO @DATA(ls_ekko)
*      WHERE ekko~ebeln = @iv_ordernum.
*
*    SELECT   ebeln
*             ,creationdate
*             ,aedat
*             ,netpr
*             ,matnr
*             ,ebelp
*        FROM ekpo
*          INTO TABLE @DATA(lt_ekpo)
*          WHERE ebeln = @iv_ordernum ##WARN_OK.
*
*    SELECT SINGLE abs_num
*                 ,valuta
*                 ,sum_rub
*                 ,sum_val
*                 ,date_nach
*                 ,date_okon
*                 ,type_dog
*      FROM zrcmt_dgv_attr
*      JOIN scmg_t_case_attr
*      ON zrcmt_dgv_attr~case_guid     = scmg_t_case_attr~case_guid
*         AND scmg_t_case_attr~ext_key = @ls_ekko-zzext_key
*      INTO @DATA(ls_zrcmt) ##WARN_OK.
*
*    SELECT SINGLE partner
*      FROM but000
*      INTO @DATA(lv_but000)
*      WHERE partner = @ls_ekko-lifnr.
*    "ATC 04.07.2022
*    SELECT SINGLE partner "#EC WARNOK
*                 ,type
*      FROM but0id
*      INTO @DATA(ls_but0ID)
*      WHERE partner = @ls_ekko-lifnr AND type = @lc_zabs ##WARN_OK.
*
*    es_infpot0136-balanceunit           = ls_ekko-bukrs.
*    es_infpot0136-ordernum              = iv_ordernum.
*    es_infpot0136-agreementid           = ls_ekko-zzext_key.
*    es_infpot0136-cftagreementid        = iv_cftagreementid.
*    es_infpot0136-agreementstartdate    = ls_zrcmt-date_nach.
*    es_infpot0136-agreementenddate      = ls_zrcmt-date_okon.
*    es_infpot0136-periodcode            = ls_zrcmt-type_dog.
*
*    IF lv_but000 IS INITIAL.
*      es_infpot0136-conuterpartyid      = ls_but0ID-partner.
*    ELSE.
*      es_infpot0136-conuterpartyid      = lv_but000.
*    ENDIF.
*
*    IF ls_zrcmt-valuta IN lt_rng_bsart_usl.
*      es_infpot0136-amount              = ls_zrcmt-sum_rub.
*    ELSE.
*      es_infpot0136-amount              = ls_zrcmt-sum_val.
*    ENDIF.
*
*    LOOP AT lt_ekpo INTO DATA(ls_ekpo).
*      APPEND INITIAL LINE TO es_infpot0136-writeofflist ASSIGNING FIELD-SYMBOL(<ls_posit>).
*
*      <ls_posit>-periodstartdate = ls_ekpo-creationdate.
*      <ls_posit>-periodenddate = ls_ekpo-aedat.
*      <ls_posit>-amount_wo = ls_ekpo-netpr.
*      <ls_posit>-currencycode = ls_ekko-waers.
*
*      IF ls_ekpo IS INITIAL.
*        <ls_posit>-ordernum_wo       = ls_ekko-ebeln.
*      ELSE.
*        <ls_posit>-ordernum_wo       = ls_ekpo-ebeln.
*      ENDIF.
*      <ls_posit>-positionnum          = ls_ekpo-ebelp.
*      <ls_posit>-materialcode         = ls_ekpo-matnr.
*    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_INT003_HELPER=>CHECK_PT0113
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_RBKP                        TYPE        RBKP
* | [--->] IT_RSEG                        TYPE        MRM_TAB_MRMRSEG
* | [--->] IT_RBCO                        TYPE        MRM_TAB_MRMRBCO
* | [<-()] RV_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_pt0113.

    DATA: lr_bsart_usl TYPE RANGE OF esart.

    zcl_tvarvc_params_get=>get_range( EXPORTING iv_name = 'ZSMMINT0003_BSART_USL' IMPORTING er_range = lr_bsart_usl ).

    DATA(lv_ebeln) = it_rseg[ 1 ]-ebeln.

    DATA(lv_belnr) = is_rbkp-belnr.
    DATA(lv_gjahr) = is_rbkp-gjahr.
    IF lines( it_rseg ) > 0.
      DATA(lv_matnr) = it_rseg[ 1 ]-matnr.
    ENDIF.
    IF lines( it_rbco ) > 0.
      DATA(lv_aufnr) = it_rbco[ 1 ]-aufnr.
    ENDIF.

    SELECT SINGLE bsart FROM ekko WHERE ebeln = @lv_ebeln INTO @DATA(lv_bsart).

*    SELECT SINGLE ekko~bsart, ekko~ebeln
*      FROM ekko
*      JOIN rseg
*        ON ekko~ebeln = rseg~ebeln
*    WHERE rseg~belnr = @lv_belnr
*      AND rseg~gjahr = @lv_gjahr
*    INTO @DATA(ls_ekko_rseg).

*    SELECT SINGLE @abap_true
*      FROM ekko
*      JOIN rseg
*        ON ekko~ebeln = rseg~ebeln
*      JOIN ekpo
*        ON ekko~ebeln = ekpo~ebeln
*    WHERE ekpo~knttp = 'A'
*      AND ekpo~loekz = @abap_false
*      AND rseg~belnr = @lv_belnr
*      AND rseg~gjahr = @lv_gjahr
*    INTO @DATA(lv_nma).
*
*    SELECT ekkn~aufnr, cobrb~anln1
*      FROM ekko
*      JOIN rseg
*        ON ekko~ebeln  = rseg~ebeln
*      JOIN ekpo
*        ON ekko~ebeln  = ekpo~ebeln
*      JOIN ekkn
*        ON ekkn~ebeln  = ekpo~ebeln
*      JOIN aufk
*        ON aufk~aufnr  = ekkn~aufnr
*       AND aufk~autyp  = '30'
*      JOIN cobrb
*        ON cobrb~objnr = aufk~objnr
*       AND cobrb~anln1 IS NOT INITIAL
*    WHERE ekpo~knttp = 'F'
*      AND ekpo~loekz = @abap_false
*      AND rseg~belnr = @lv_belnr
*      AND rseg~gjahr = @lv_gjahr
*    INTO TABLE @DATA(lt_nma2).

    SELECT SINGLE zgr
      FROM ztmm_cl_gr
      JOIN mara
        ON mara~matkl = ztmm_cl_gr~zmatkl
       AND mara~matnr = @lv_matnr
    INTO @DATA(lv_zgr).
    IF lv_zgr IS INITIAL.
      DATA(lv_zd_zu_flag) = abap_true.
    ENDIF.

*    IF lv_aufnr IS NOT INITIAL.
*      SELECT SINGLE @abap_true
*        FROM aufk
*        JOIN afih
*          ON  afih~aufnr = aufk~aufnr
*        JOIN ztmm_remont_usl
*          ON ztmm_remont_usl~auart = aufk~auart
*         AND ztmm_remont_usl~ilart = afih~ilart
*        WHERE aufk~aufnr = @lv_aufnr
*        INTO @DATA(lv_toro).
*    ENDIF.

    IF lv_bsart IN lr_bsart_usl AND lv_zd_zu_flag = abap_true.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

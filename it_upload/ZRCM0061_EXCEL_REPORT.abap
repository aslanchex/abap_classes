*&---------------------------------------------------------------------*
*& Report ZRCM0061_EXCEL_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrcm0061_excel_report.

TYPES: BEGIN OF ty_alv.
TYPES error_message TYPE text256.
TYPES ext_key TYPE scmg_ext_key.
INCLUDE TYPE ztrcm_its_attr.
TYPES itasset TYPE ztrcm_itfs_attr-itasset.
TYPES naimitasset TYPE ztrcm_itfs_attr-naimitasset.
TYPES serfacnum TYPE ztrcm_itfs_attr-serfacnum.
TYPES nameitasset TYPE ztrcm_itfs_attr-nameitasset.
TYPES kindlic TYPE ztrcm_itfpo_attr-kindlic.
TYPES typelic TYPE ztrcm_itfpo_attr-typelic.
TYPES t_color TYPE lvc_t_scol.
TYPES END OF ty_alv.


TABLES: sscrfields.

FIELD-SYMBOLS : <gt_data>       TYPE STANDARD TABLE.
DATA gt_alv TYPE TABLE OF ty_alv.
DATA gt_upsert_table TYPE TABLE OF ztrcm_its_attr.
*DATA my_ucomm TYPE sy-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
  PARAMETERS : p_file TYPE ibipparms-path OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN PUSHBUTTON 2(12) TEXT-010 USER-COMMAND onli.

*--------------------------------------------------------------------*
* at selection screen
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  DATA: lv_rc TYPE i.
  DATA: lt_file_table TYPE filetable,
        ls_file_table TYPE file_table.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*   EXPORTING
*      window_title = 'Window'
    CHANGING
      file_table = lt_file_table
      rc         = lv_rc
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0.
    READ TABLE lt_file_table INTO ls_file_table INDEX 1.
    p_file = ls_file_table-filename.
  ENDIF.


INITIALIZATION.



AT SELECTION-SCREEN.

*  my_ucomm = sscrfields-ucomm.

START-OF-SELECTION .

  PERFORM read_file .
  PERFORM parsing_table.
  PERFORM alv_screen.

FORM read_file .

  DATA : lv_filename      TYPE string,
         lt_records       TYPE solix_tab,
         lv_headerxstring TYPE xstring,
         lv_filelength    TYPE i.

  lv_filename = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_filelength
      header                  = lv_headerxstring
    TABLES
      data_tab                = lt_records
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc NE 0.
    WRITE / 'EXCEPTION OCCURED'.
  ENDIF.
  "convert binary data to xstring
  "if you are using cl_fdt_xl_spreadsheet in odata then skips this step
  "as excel file will already be in xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filelength
    IMPORTING
      buffer       = lv_headerxstring
    TABLES
      binary_tab   = lt_records
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    SKIP.
  ENDIF.

  DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
                              document_name = lv_filename
                              xdocument     = lv_headerxstring ) .
    CATCH cx_fdt_excel_core.
      SKIP.
  ENDTRY .

  "Get List of Worksheets
  lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheets) ).

  IF NOT lt_worksheets IS INITIAL.
    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                             lv_woksheetname ).
    "now you have excel work sheet data in dyanmic internal table
    ASSIGN lo_data_ref->* TO <gt_data>.

    DO 3 TIMES.
      DELETE <gt_data> INDEX 1.
    ENDDO.

  ENDIF.

ENDFORM.

FORM parsing_table.

*  TYPES: BEGIN OF ty_ext.
*  TYPES ext_key TYPE scmg_ext_key.
*  TYPES END OF ty_ext.

**  TYPES: BEGIN OF ty_ext_sorted.
*  TYPES ext_key TYPE scmg_ext_key.
*  TYPES case_guid TYPE scmg_case_guid.
*  TYPES END OF ty_ext_sorted.


  TYPES: BEGIN OF ty_itfpo.
  TYPES case_guid TYPE ztrcm_itfpo_attr-case_guid.
  TYPES kindlic TYPE ztrcm_itfpo_attr-kindlic.
  TYPES typelic TYPE ztrcm_itfpo_attr-typelic.
  TYPES END OF ty_itfpo.

  TYPES: BEGIN OF ty_itfs.
  TYPES case_guid TYPE ztrcm_itfs_attr-case_guid.
  TYPES itasset TYPE ztrcm_itfs_attr-itasset.
  TYPES naimitasset TYPE ztrcm_itfs_attr-naimitasset.
  TYPES serfacnum TYPE ztrcm_itfs_attr-serfacnum.
  TYPES nameitasset TYPE ztrcm_itfs_attr-nameitasset.
  TYPES END OF ty_itfs.

*  TYPES: BEGIN OF ty_yesno.
*  TYPES number TYPE val_single.
*  TYPES value TYPE val_text.
*  TYPES END OF ty_yesno.

  DATA:
    lt_yesno         TYPE STANDARD TABLE OF dd07v,
    lt_approvedsla   TYPE STANDARD TABLE OF dd07v,
    lt_minlevelavorp TYPE STANDARD TABLE OF dd07v,
    lt_us_grnotifchs TYPE STANDARD TABLE OF dd07v,
    ls_dd07v         TYPE dd07v.

  DATA lt_itfs TYPE TABLE OF TY_itfs.
  DATA lt_itfpo TYPE TABLE OF TY_itfpo.

  DATA lv_length TYPE i.

  DATA lt_enq TYPE STANDARD TABLE OF seqg3 WITH DEFAULT KEY.
  DATA ls_enq TYPE seqg3.
  DATA ls_its_attr TYPE ztrcm_its_attr.
  DATA ls_itfs_1 TYPE ty_itfs.
  DATA ls_itfpo_1 TYPE ty_itfpo.

  DATA lt_ext TYPE SORTED TABLE OF scmg_ext_key WITH NON-UNIQUE KEY table_line INITIAL SIZE 0.
  DATA lv_ext TYPE scmg_ext_key.

  DATA lv_flag TYPE char32.
  DATA ls_color TYPE lvc_s_scol.
  DATA lv_ext_key TYPE char32.

  DATA lt_keys TYPE SORTED TABLE OF scmg_case_guid WITH NON-UNIQUE KEY table_line INITIAL SIZE 0.
*  DATA lt_sorted TYPE SORTED TABLE OF ztrcm_its_attr WITH NON-UNIQUE KEY case_guid INITIAL SIZE 0.

  DATA lt_ITFPO_2 TYPE TABLE OF ztrcm_itfpo_attr.
  DATA lt_ITFs_2 TYPE TABLE OF ztrcm_itfs_attr.

*  DATA lt_ext_key TYPE SORTED TABLE OF scmg_ext_key WITH NON-UNIQUE KEY table_line INITIAL SIZE 0.
*  DATA lt_ext_sorted TYPE SORTED TABLE OF ty_ext_sorted WITH NON-UNIQUE KEY ext_key INITIAL SIZE 0.
  DATA lt_ext_sorted TYPE SORTED TABLE OF scmg_t_case_attr WITH NON-UNIQUE KEY ext_key INITIAL SIZE 0.

*  DATA ls_ext TYPE ty_ext.
  DATA lv_garg TYPE eqegraarg.
  DATA lv_mandt TYPE char32.
  DATA lv_error_message TYPE dfs_msg_text.

  lv_mandt = sy-mandt.

  LOOP AT <gt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) .
    ASSIGN COMPONENT 'A' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<ext_key>).
    ASSIGN COMPONENT 'B' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<analypomvz>).
    ASSIGN COMPONENT 'C' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<idsuit>).
    ASSIGN COMPONENT 'D' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<sernumber>).
    ASSIGN COMPONENT 'E' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<itsasufhd>).
    ASSIGN COMPONENT 'F' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<shortname>).
    ASSIGN COMPONENT 'G' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<itssuit>).
    ASSIGN COMPONENT 'H' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<test_produ>).
    ASSIGN COMPONENT 'I' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<whosupport>).
    ASSIGN COMPONENT 'J' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<criticaly>).
    ASSIGN COMPONENT 'K' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<status>).
    ASSIGN COMPONENT 'L' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<visfromportal>).
    ASSIGN COMPONENT 'M' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<typeits>).
    ASSIGN COMPONENT 'N' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<managerits>).
    ASSIGN COMPONENT 'O' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<description>).
    ASSIGN COMPONENT 'P' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<fiorasport>).
    ASSIGN COMPONENT 'Q' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<ssprapor>).
    ASSIGN COMPONENT 'R' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<numunserv>).
    ASSIGN COMPONENT 'S' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<stunserv>).
    ASSIGN COMPONENT 'T' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<nameunserv>).
    ASSIGN COMPONENT 'U' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<manegunserv>).
    ASSIGN COMPONENT 'V' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<us_perimserv>).
    ASSIGN COMPONENT 'W' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<us_critical>).
    ASSIGN COMPONENT 'X' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<us_visfromportal>).
    ASSIGN COMPONENT 'Y' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<us_arrayrg>).
    ASSIGN COMPONENT 'Z' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<us_grnotifchs>).
    ASSIGN COMPONENT 'AA' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<us_acreatinc>).
    ASSIGN COMPONENT 'AB' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<approvedsla>).
    ASSIGN COMPONENT 'AC' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<minlevelav>).
    ASSIGN COMPONENT 'AD' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<minlevelavopr>).
    ASSIGN COMPONENT 'AE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<isprovmontrans>).
    ASSIGN COMPONENT 'AF' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<critical35p>).
    ASSIGN COMPONENT 'AG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<bascritical>).
    ASSIGN COMPONENT 'AH' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<namearcrep>).
    ASSIGN COMPONENT 'AI' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<department>).
    ASSIGN COMPONENT 'AJ' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<fioheaddepart>).
    ASSIGN COMPONENT 'AK' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<idsud>).
    ASSIGN COMPONENT 'AL' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<namesud>).
    ASSIGN COMPONENT 'AM' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<fullnamesud>).
    ASSIGN COMPONENT 'AN' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<idsudald>).
    ASSIGN COMPONENT 'AO' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<namesud2>).
    ASSIGN COMPONENT 'AP' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<idsudalt2>).
    ASSIGN COMPONENT 'AQ' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<namesud3>).
    ASSIGN COMPONENT 'AR' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<itasset>).
    ASSIGN COMPONENT 'AS' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<naimitasset>).
    ASSIGN COMPONENT 'AT' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<serfacnum>).
    ASSIGN COMPONENT 'AU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<nameitasset>).
    ASSIGN COMPONENT 'AV' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<kindlic>).
    ASSIGN COMPONENT 'AW' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<typelic>).
    APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<ls_alv>).

    lv_length = strlen( <ext_key> ).

    WHILE  lv_length < 12.
      <ext_key> = |{ '0' }{ <ext_key> }|.
      lv_length = strlen( <ext_key> ).
    ENDWHILE.

    <ls_alv>-case_guid = <ext_key>.

    <ls_alv>-analypomvz = <analypomvz>.
    <ls_alv>-idsuit = <idsuit>.
    <ls_alv>-sernumber = <sernumber>.
    <ls_alv>-itsasufhd = <itsasufhd>.
    <ls_alv>-shortname = <shortname>.
    <ls_alv>-itssuit = <itssuit>.
    <ls_alv>-test_prod = <test_produ>.
    <ls_alv>-whosupport = <whosupport>.
    <ls_alv>-criticality = <criticaly>.
    <ls_alv>-status = <status>.
    <ls_alv>-visfromportal = <visfromportal>.
    <ls_alv>-typeits = <typeits>.
    <ls_alv>-managerits = <managerits>.
    <ls_alv>-description = <description>.
    <ls_alv>-fioraspor = <fiorasport>.
    <ls_alv>-sspraspor = <ssprapor>.
    <ls_alv>-numunserv = <numunserv>.
    <ls_alv>-stunserv = <stunserv>.
    <ls_alv>-nameunserv = <nameunserv>.
    <ls_alv>-manegunserv = <manegunserv>.
    <ls_alv>-us_perimserv = <us_perimserv>.
    <ls_alv>-us_critical = <us_critical>.
    <ls_alv>-us_visfromportal = <us_visfromportal>.
    <ls_alv>-us_arrayrg = <us_arrayrg>.
    <ls_alv>-us_grnotifchs = <us_grnotifchs>.
    <ls_alv>-us_acreatinc = <us_acreatinc>.
    <ls_alv>-approvedsla = <approvedsla>.
    <ls_alv>-minlevelav = <minlevelav>.
    <ls_alv>-minlevelavopr = <minlevelavopr>.
    <ls_alv>-isprovmontrans = <isprovmontrans>.
    <ls_alv>-critical35p = <critical35p>.
    <ls_alv>-bascritical = <bascritical>.
    <ls_alv>-namearcrep = <namearcrep>.
    <ls_alv>-department = <department>.
    <ls_alv>-fioheaddepart = <fioheaddepart>.
    <ls_alv>-idsud = <idsud>.
    <ls_alv>-namesud = <namesud>.
    <ls_alv>-fullnamesud = <fullnamesud>.
    <ls_alv>-idsudalt = <idsudald>.
    <ls_alv>-namesud2 = <namesud2>.
    <ls_alv>-idsudalt2 = <idsudalt2>.
    <ls_alv>-namesud3 = <namesud3>.
    <ls_alv>-itasset = <itasset>.
    <ls_alv>-naimitasset = <naimitasset>.
    <ls_alv>-serfacnum = <serfacnum>.
    <ls_alv>-nameitasset = <nameitasset>.
    <ls_alv>-kindlic = <kindlic>.
    <ls_alv>-typelic = <typelic>.
    INSERT <ls_alv>-case_guid INTO TABLE lt_keys.
    lv_ext = <ext_key>.
    INSERT lv_ext INTO TABLE lt_ext.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING table_line.

*  CHECK lt_keys IS NOT INITIAL.
*  SELECT *
*    FROM ztrcm_its_attr
*    INTO TABLE lt_sorted
*    FOR ALL ENTRIES IN lt_keys
*    WHERE case_guid = lt_keys-table_line.

  CHECK lt_ext IS NOT INITIAL.
  SELECT ext_key, case_guid, case_type
    FROM scmg_t_case_attr
    INTO CORRESPONDING FIELDS OF TABLE @lt_ext_sorted ##TOO_MANY_ITAB_FIELDS
    FOR ALL ENTRIES IN @lt_ext
    WHERE ext_key = @lt_ext-table_line
    AND (
    case_type EQ 'ITFS'
    OR
    case_type EQ 'ITFO'
    OR
    case_type EQ 'ZITS'
    ).

  lv_flag = 0.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZD_YESNO'
      langu         = sy-langu
    TABLES
      dd07v_tab     = lt_yesno
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZD_APPROVEDSLA'
      langu         = sy-langu
    TABLES
      dd07v_tab     = lt_approvedsla
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZD_MINLEVELAVOPR'
      langu         = sy-langu
    TABLES
      dd07v_tab     = lt_minlevelavorp
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZD_US_GRNOTIFCHS'
      langu         = sy-langu
    TABLES
      dd07v_tab     = lt_us_grnotifchs
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  CHECK sy-subrc EQ 0.


  LOOP AT gt_alv ASSIGNING <ls_alv>.##WARN_OK
    READ TABLE lt_ext_sorted ASSIGNING FIELD-SYMBOL(<ls_ext_sorted>) WITH TABLE KEY ext_key = <ls_alv>-case_guid.
    IF lv_ext_key NE <ls_ext_sorted>-ext_key.
      MOVE-CORRESPONDING <ls_ext_sorted> TO <ls_alv>.
      lv_ext_key = <ls_alv>-ext_key.
    ENDIF.


    IF sy-subrc NE 0.
      lv_flag = 'X'.
      <ls_alv>-error_message = TEXT-002.
      <ls_alv>-ext_key = <ls_alv>-case_guid.
      ls_color-fname = 'ERROR_MESSAGE'.
      CLEAR <ls_alv>-case_guid.
      ls_color-color-col = col_negative.
      APPEND ls_color TO <ls_alv>-t_color.
    ELSEIF
      <ls_alv>-error_message = TEXT-003.
      CLEAR ls_its_attr.
      MOVE-CORRESPONDING <ls_alv> TO ls_its_attr.
      CLEAR ls_its_attr-case_guid.
      IF ls_its_attr IS NOT INITIAL.
        APPEND INITIAL LINE TO gt_upsert_table ASSIGNING FIELD-SYMBOL(<ls_upsert_table>).
        MOVE-CORRESPONDING <ls_alv> TO <ls_upsert_table>.
      ENDIF.

      IF <ls_alv>-typelic IS NOT INITIAL.

        SELECT *
        FROM ztrcm_typelicpo
        INTO TABLE @DATA(lt_sorted_itfpo)
        WHERE typelicpo = @<ls_alv>-typelic.

        IF lt_sorted_itfpo IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'TYPELIC'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.

        ENDIF.
      ENDIF.

      IF <ls_alv>-critical35p IS NOT INITIAL.

        READ TABLE lt_yesno INTO ls_dd07v WITH KEY ddtext = <ls_alv>-critical35p.

        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'CRITICAL35P'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.

          CLEAR ls_dd07v.
        ENDIF.
      ENDIF.

      IF <ls_alv>-criticality IS NOT INITIAL.
        CLEAR ls_dd07v.
        READ TABLE lt_yesno INTO ls_dd07v WITH KEY ddtext = <ls_alv>-criticality.

        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'CRITICALITY'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.


        ENDIF.
      ENDIF.

      IF <ls_alv>-us_acreatinc IS NOT INITIAL.
        CLEAR ls_dd07v.
        READ TABLE lt_yesno INTO ls_dd07v WITH KEY ddtext = <ls_alv>-us_acreatinc.
        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'US_ACREATINC'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.

        ENDIF.
      ENDIF.

      IF <ls_alv>-isprovmontrans IS NOT INITIAL.
        CLEAR ls_dd07v.
        READ TABLE lt_yesno INTO ls_dd07v WITH KEY ddtext = <ls_alv>-isprovmontrans.

        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'ISPROVMONTRANS'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.


        ENDIF.
      ENDIF.


*
      IF <ls_alv>-approvedsla IS NOT INITIAL.
        CLEAR ls_dd07v.
        READ TABLE lt_approvedsla INTO ls_dd07v WITH KEY ddtext = <ls_alv>-approvedsla.

        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'APPROVEDSLA'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
        ENDIF.
      ENDIF.

      IF <ls_alv>-minlevelavopr IS NOT INITIAL.
        CLEAR ls_dd07v.
        READ TABLE lt_minlevelavorp INTO ls_dd07v WITH KEY ddtext = <ls_alv>-minlevelavopr.

        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'MINLEVELAVOPR'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.

        ENDIF.
      ENDIF.

      IF <ls_alv>-us_grnotifchs IS NOT INITIAL.
        CLEAR ls_dd07v.
        READ TABLE lt_us_grnotifchs INTO ls_dd07v WITH KEY ddtext = <ls_alv>-us_grnotifchs.

        IF ls_dd07v IS INITIAL.
          <ls_alv>-error_message = TEXT-020.
          ls_color-fname = 'US_GRNOTIFCHS'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.
          ls_color-fname = 'ERROR_MESSAGE'.
          ls_color-color-col = col_negative.
          APPEND ls_color TO <ls_alv>-t_color.


        ENDIF.
      ENDIF.

      CONCATENATE lv_mandt <ls_alv>-case_guid INTO lv_garg.

      CALL FUNCTION 'ENQUEUE_READ'
*        EXPORTING
*         GCLIENT                     = SY-MANDT
*         GNAME                       = ' '
*          garg = lv_garg
*         GUNAME                      = SY-UNAME
*         LOCAL                       = ' '
*         FAST = ' '
*         GARGNOWC                    = ' '
* IMPORTING
*         NUMBER                      =
*         SUBRC                       =
        TABLES
          enq                   = lt_enq
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        SKIP.
      ENDIF.

      READ TABLE lt_enq INTO ls_enq WITH KEY garg = lv_garg.
      IF ls_enq IS NOT INITIAL.
        lv_flag = 'X'.
        lv_error_message = |{ TEXT-030 } { ls_enq-guname }|.
        <ls_alv>-error_message = lv_error_message.
        ls_color-fname = 'EXT_KEY'.
        ls_color-color-col = col_negative.
        APPEND ls_color TO <ls_alv>-t_color.
        ls_color-fname = 'ERROR_MESSAGE'.
        ls_color-color-col = col_negative.
        APPEND ls_color TO <ls_alv>-t_color.

        CLEAR ls_enq.
      ENDIF.

      CLEAR ls_itfpo_1.
      MOVE-CORRESPONDING <ls_alv> TO ls_itfpo_1.
      CLEAR ls_itfpo_1-case_guid.
      IF ls_itfpo_1 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_itfpo ASSIGNING FIELD-SYMBOL(<ls_itfpo>).
        MOVE-CORRESPONDING <ls_alv> TO <ls_itfpo>.
      ENDIF.

      CLEAR ls_itfs_1.
      MOVE-CORRESPONDING <ls_alv> TO ls_itfs_1.
      CLEAR ls_itfs_1-case_guid.
      IF ls_itfs_1 IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_itfs ASSIGNING FIELD-SYMBOL(<ls_itfs>).
        MOVE-CORRESPONDING <ls_alv> TO <ls_itfs>.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF lv_flag NE 'X'.
    LOOP AT lt_itfpo ASSIGNING <ls_itfpo>.
      ASSIGN COMPONENT 'KINDLIC' OF STRUCTURE <ls_itfpo> TO FIELD-SYMBOL(<kindlic_2>).
      ASSIGN COMPONENT 'TYPELIC' OF STRUCTURE <ls_itfpo> TO FIELD-SYMBOL(<typelic_2>).
      ASSIGN COMPONENT 'CASE_GUID' OF STRUCTURE <ls_itfpo> TO FIELD-SYMBOL(<case_guid_2>).
      APPEND INITIAL LINE TO lt_ITFPO_2 ASSIGNING FIELD-SYMBOL(<lS_ITFPO_2>).
      <ls_itfpo_2>-kindlic = <kindlic_2>.
      <ls_itfpo_2>-typelic = <typelic_2>.
      <ls_itfpo_2>-case_guid = <case_guid_2>.
    ENDLOOP.
    LOOP AT lt_itfs ASSIGNING <ls_itfs>.
      ASSIGN COMPONENT 'ITASSET' OF STRUCTURE <ls_itfs> TO FIELD-SYMBOL(<itasset_2>).
      ASSIGN COMPONENT 'NAIMIATASSET' OF STRUCTURE <ls_itfs> TO FIELD-SYMBOL(<naimiatasse_2>).
      ASSIGN COMPONENT 'SERFACNUM' OF STRUCTURE <ls_itfs> TO FIELD-SYMBOL(<serfacnum_2>).
      ASSIGN COMPONENT 'NAMEITASSET' OF STRUCTURE <ls_itfs> TO FIELD-SYMBOL(<nameitsset_2>).
      ASSIGN COMPONENT 'CASE_GUID' OF STRUCTURE <ls_itfs> TO FIELD-SYMBOL(<case_guid_3>).
      APPEND INITIAL LINE TO lt_itfs_2 ASSIGNING FIELD-SYMBOL(<lS_itfs_2>).
      <lS_itfs_2>-itasset = <itasset_2>.
      <lS_itfs_2>-naimitasset = <nameitsset_2>.
      <lS_itfs_2>-serfacnum = <serfacnum_2>.
      <lS_itfs_2>-nameitasset = <nameitsset_2>.
      <lS_itfs_2>-case_guid = <case_guid_3>.
    ENDLOOP.
    MODIFY ztrcm_itfpo_attr FROM TABLE lt_ITFPO_2.
    MODIFY ztrcm_itfs_attr FROM TABLE lt_ITFs_2.
    MODIFY ztrcm_its_attr FROM TABLE gt_upsert_table.
*    MODIFY ztrcm_itfs_attr FROM TABLE lt_itfs.
  ENDIF.

ENDFORM.

FORM alv_screen.

  DATA lo_salv TYPE REF TO cl_salv_table.
*  DATA lo_salv_layout TYPE REF TO cl_salv_layout.
  DATA lo_salv_columns TYPE REF TO cl_salv_columns_table.
  DATA lo_salv_column TYPE REF TO cl_salv_column.
  TRY.
      CALL METHOD cl_salv_table=>factory
*  EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*    r_container    =
*    container_name =
        IMPORTING
          r_salv_table = lo_salv
        CHANGING
          t_table      = gt_alv.
    CATCH cx_salv_msg.
      SKIP.
  ENDTRY.

  IF lo_salv IS BOUND.
*   lo_salv_layout ?= lo_salv->get_layout( ).
    lo_salv_columns = lo_salv->get_columns( ).
    lo_salv_columns->set_optimize(
*        value = if_salv_c_bool_sap~true
    ).
    TRY.
        lo_salv_columns->set_color_column( value = 'T_COLOR' ).
      CATCH cx_salv_data_error. " ALV: General Error Class (Checked in Syntax Check)
        SKIP.
    ENDTRY.
    TRY.
        lo_salv_column = lo_salv_columns->get_column( columnname = 'MANDT' ).
        lo_salv_column->set_technical(
*         value = if_salv_c_bool_sap=>true
        ).
        lo_salv_column = lo_salv_columns->get_column( columnname = TEXT-005 ).
        lo_salv_column->set_medium_text( value = TEXT-004 ).
*     lo_salv_column->set_short_text( value = text-004 ).
*     lo_salv_column->set_long_text( value = text-004 ).
      CATCH cx_salv_not_found.
        SKIP.
    ENDTRY.
    lo_salv->display( ).
  ENDIF.

ENDFORM.

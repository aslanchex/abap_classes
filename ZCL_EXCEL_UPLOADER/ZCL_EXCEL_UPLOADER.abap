class ZCL_EXCEL_UPLOADER definition
  public
  final
  create public .

public section.

  data MT_DATA type ref to DATA .

  methods CONSTRUCTOR
    importing
      !IV_FILENAME type RLGRAP-FILENAME
      !IV_LAST_COLUMN_NUMBER type I
      !IV_FIRST_COLUMN_NUMBER type I
      !IV_NUMBER_OF_ROWS type I
      !IV_FIRST_DATA_ROW type I default 4
      !IV_NAMES_ROW type I default 1
      !IV_DATAELEMENTS_ROW type I default 2 .
  methods DISPLAY .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_tab,
             name    TYPE char30,
             dt_name TYPE char30,
           END OF ty_tab .
  types:
    tt_tab TYPE TABLE OF ty_tab WITH EMPTY KEY .

  data:
    mt_excel_data         TYPE TABLE OF alsmex_tabline .
  data MT_NAME_DATAELEMENT type TT_TAB .
  data MT_COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  data MS_DATA type ref to DATA .
  data MV_FILENAME type RLGRAP-FILENAME .
  data MV_LAST_COLUMN_NUMBER type I .
  data MV_NUMBER_OF_ROWS type I .
  data MV_FIRST_DATA_ROW type I .
  data MV_NAMES_ROW type I .
  data MV_DATAELEMENTS_ROW type I .
  data MO_SALV type ref to CL_SALV_TABLE .
  data MV_FIRST_COLUMN_NUMBER type I .

  methods UPLOAD_EXCEL .
  methods BUILD_TABLE_FROM_EXCEL .
  methods BUILD_COMPONENTS_TABLE
    returning
      value(RT_COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  methods FILL_DATA_TABLE .
  methods GET_COLUMN_NAMES .
ENDCLASS.



CLASS ZCL_EXCEL_UPLOADER IMPLEMENTATION.


  METHOD build_components_table.
    DATA: lr_description TYPE REF TO cl_abap_typedescr.

    mt_name_dataelement = VALUE #( FOR i IN mt_excel_data
                                    WHERE ( row = mv_dataelements_row )
                                    (
                                      name    = mt_excel_data[ col = i-col row = mv_names_row ]-value
                                      dt_name = i-value
                                     )
                                  ).

    LOOP AT mt_name_dataelement ASSIGNING FIELD-SYMBOL(<ls_name_dataelement>).
      cl_abap_typedescr=>describe_by_name(
        EXPORTING
            p_name = <ls_name_dataelement>-dt_name
        RECEIVING
            p_descr_ref = lr_description
        EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2 ).
      IF sy-subrc <> 0.

      ELSE.
        APPEND VALUE #( name = <ls_name_dataelement>-name
                        type = cl_abap_elemdescr=>get_by_kind( p_type_kind = lr_description->type_kind
                                                               p_length    = lr_description->length
                                                               p_decimals  = lr_description->decimals ) ) TO rt_components.
      ENDIF.
    ENDLOOP.

    CHECK mt_components IS INITIAL.
    mt_components = rt_components.
  ENDMETHOD.


  METHOD build_table_from_excel.
    TRY.
        DATA(lr_structure) = cl_abap_structdescr=>create( me->build_components_table( ) ).
        DATA(lr_table) = cl_abap_tabledescr=>create( p_line_type = CAST #( lr_structure ) ).
      CATCH cx_sy_struct_creation.

      CATCH cx_sy_table_creation.

    ENDTRY.

    CREATE DATA ms_data TYPE HANDLE lr_structure.

    CREATE DATA mt_data TYPE HANDLE lr_table.

  ENDMETHOD.


  METHOD constructor.
    mv_filename = iv_filename.
    mv_last_column_number = iv_last_column_number.
    mv_number_of_rows = iv_number_of_rows.
    mv_first_column_number = iv_first_column_number.
    mv_first_data_row = iv_first_data_row.
    mv_names_row       = iv_names_row.
    mv_dataelements_row  = iv_dataelements_row.

    me->upload_excel( ).
  ENDMETHOD.


  METHOD display.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    ASSIGN mt_data->* TO <lt_table>.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = <lt_table> ).
      CATCH cx_salv_msg.

    ENDTRY.

    me->get_column_names( ).
    mo_salv->display( ).
  ENDMETHOD.


  METHOD fill_data_table.
    FIELD-SYMBOLS: <data>      TYPE ANY TABLE,
                   <data_line> TYPE any.

    ASSIGN mt_data->* TO <data>.
    ASSIGN ms_data->* TO <data_line>.

    LOOP AT mt_excel_data ASSIGNING FIELD-SYMBOL(<excel_data_line>) WHERE row >= mv_first_data_row.
      ASSIGN COMPONENT mt_excel_data[ row = mv_names_row col = <excel_data_line>-col ]-value OF STRUCTURE <data_line> TO FIELD-SYMBOL(<cell>).
      IF sy-subrc = 0.
        <cell> = <excel_data_line>-value.
      ENDIF.
      AT END OF row.
        INSERT <data_line> INTO TABLE <data>.
        CLEAR <data_line>.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_column_names.
    DATA: lt_dd04      TYPE TABLE OF dd04t,
          lv_roll_name TYPE dd04l-rollname.
    DATA(lo_columns) = mo_salv->get_columns( ).

    LOOP AT mt_name_dataelement ASSIGNING FIELD-SYMBOL(<ls_name_dataelement>).
      DATA(lo_column) = lo_columns->get_column( columnname = <ls_name_dataelement>-name ).
      lv_roll_name = <ls_name_dataelement>-dt_name.
      CALL FUNCTION 'DD_DTEL_GET'
        EXPORTING
          langu         = sy-langu
          roll_name     = lv_roll_name
        TABLES
          dd04t_tab_n   = lt_dd04
        EXCEPTIONS
          illegal_value = 1
          OTHERS        = 2.
      IF sy-subrc = 0.
        lo_column->set_long_text( value = lt_dd04[ 1 ]-scrtext_l ).
        lo_column->set_medium_text( value = lt_dd04[ 1 ]-scrtext_m ).
        lo_column->set_short_text( value = lt_dd04[ 1 ]-scrtext_s ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD upload_excel.
    cl_progress_indicator=>progress_indicate(
      EXPORTING
        i_text               = |Загрузка файла Excel...|
        i_processed          = 0
        i_total              = 1
        i_output_immediately = abap_false
    ).

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = mv_filename
        i_begin_col             = mv_first_column_number
        i_begin_row             = mv_first_data_row
        i_end_col               = mv_last_column_number
        i_end_row               = mv_number_of_rows
      TABLES
        intern                  = mt_excel_data
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.

    ENDIF.

*    me->build_table_from_excel( ).
    me->fill_data_table( ).
  ENDMETHOD.
ENDCLASS.

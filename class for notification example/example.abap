METHOD _send_notif_pt0127.
    DATA: ls_address TYPE bapiaddr3,
          lt_return  TYPE bapiret2_t,
          lt_plc_hld TYPE ztt_mail_placeholders_data,
          lt_content TYPE soli_tab,
          lv_content TYPE string,
          lv_string  TYPE string.

    BREAK kashiev-av.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = is_unit-created_by
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    IF lines( lt_return ) > 0.
      io_log->add_bapiret2( it_bapiret2 = lt_return ).
      RETURN.
    ENDIF.

    get_msg( is_msg_params-message_type ).

    lv_content = ms_msg-text_msg.

    IF ms_msg IS INITIAL.
      RETURN.
    ELSE.
      REPLACE '&1' IN ms_msg-topic_msg WITH is_msg_params-val1.

      REPLACE '&1' IN lv_content WITH is_msg_params-val1.
      REPLACE '&2' IN lv_content WITH is_msg_params-val2.
      REPLACE '&3' IN lv_content WITH is_msg_params-val3.
      REPLACE '&4' IN lv_content WITH is_msg_params-val4.
    ENDIF.

    " Разбиение построчно

    DATA(lv_length) = 255.

    WHILE lv_content IS NOT INITIAL.
      IF lv_length > strlen( lv_content ).
        lv_length = strlen( lv_content ).
      ENDIF.

      lv_string = lv_content(lv_length).
      APPEND VALUE soli( line = lv_string ) TO lt_content.
      SHIFT lv_content BY lv_length PLACES LEFT.
    ENDWHILE.

    TRY.
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        DATA(lo_document) = cl_document_bcs=>create_document( i_type    = 'RAW'
                                                              i_text    = lt_content
                                                              i_subject = conv #( ms_msg-topic_msg ) ).

*       add document to send request
        CALL METHOD lo_send_request->set_document( lo_document ).
        TRY.
            lo_send_request->set_message_subject( CONV string( ms_msg-topic_msg ) ).
          CATCH cx_send_req_bcs. " BCS: Send Request Exceptions
            " add log
        ENDTRY.
**       add text to document
*        CALL METHOD lo_send_request->set_note( note ).

        DATA(lo_sender) = cl_cam_address_bcs=>create_internet_address( 'sap@rshb.ru' ).
        lo_send_request->set_sender( lo_sender ).
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( ls_address-e_mail ).
*       add recipient with its respective attributes to send request
        lo_send_request->add_recipient( i_recipient = lo_recipient ).

**     ---------- send document ---------------------------------------
        DATA(lv_sent) = lo_send_request->send( 'X' ).
        IF lv_sent = abap_false.
          MESSAGE e004(zmc_int_d0013_status) INTO DATA(lv_mess).
          io_log->add_sy( ).
        ENDIF.
      CATCH cx_bcs INTO DATA(bcs_exception).
        EXIT.
    ENDTRY.

  ENDMETHOD.

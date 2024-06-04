FUNCTION z_send_email
  IMPORTING
    iv_immediately TYPE flag OPTIONAL
    iv_update_task TYPE flag OPTIONAL
    VALUE(it_placeholders_data) TYPE ztt_connect_view_placehold OPTIONAL
    VALUE(it_view_msg) TYPE ztt_mail_view_msg OPTIONAL
    VALUE(it_par1_msg) TYPE ztt_mail_par1_msg OPTIONAL
    VALUE(it_par2_msg) TYPE ztt_mail_par2_msg OPTIONAL
    VALUE(it_par3_msg) TYPE ztt_mail_par3_msg OPTIONAL
    VALUE(it_par4_msg) TYPE ztt_mail_par4_msg OPTIONAL
  TABLES
    it_mail_send TYPE ztt_mail_send OPTIONAL
    it_mail_pos TYPE ztt_mail_pos OPTIONAL
  RAISING
    cx_bcs_send.





  DATA lr_view_msg TYPE RANGE OF ze_view_msg.
  DATA lr_par1_msg TYPE RANGE OF ze_par1_msg.
  DATA lr_par2_msg TYPE RANGE OF ze_par2_msg.
  DATA lr_par3_msg TYPE RANGE OF ze_par3_msg.
  DATA lr_par4_msg TYPE RANGE OF ze_par4_msg.

  IF it_mail_send[] IS INITIAL.

    CLEAR: lr_view_msg, lr_par1_msg, lr_par2_msg, lr_par3_msg, lr_par4_msg.
    lr_view_msg = VALUE #( FOR ls_view_msg IN it_view_msg ( sign = 'I' option = 'EQ' low = ls_view_msg-line ) ).
    lr_par1_msg = VALUE #( FOR ls_par1_msg IN it_par1_msg ( sign = 'I' option = 'EQ' low = ls_par1_msg-line ) ).
    lr_par2_msg = VALUE #( FOR ls_par2_msg IN it_par2_msg ( sign = 'I' option = 'EQ' low = ls_par2_msg-line ) ).
    lr_par3_msg = VALUE #( FOR ls_par3_msg IN it_par3_msg ( sign = 'I' option = 'EQ' low = ls_par3_msg-line ) ).
    lr_par4_msg = VALUE #( FOR ls_par4_msg IN it_par4_msg ( sign = 'I' option = 'EQ' low = ls_par4_msg-line ) ).


    IF lr_par1_msg IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '*' ) TO lr_par1_msg.
    ENDIF.

    IF lr_par2_msg IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '*' ) TO lr_par2_msg.
    ENDIF.

    IF lr_par3_msg IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '*' ) TO lr_par3_msg.
    ENDIF.

    IF lr_par4_msg IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '*' ) TO lr_par4_msg.
    ENDIF.


    IF it_mail_pos[] IS NOT INITIAL.

      DATA(lt_mail_pos) = it_mail_pos[].
      SELECT
        t1~view_msg,
        t1~email,
        t3~agr_name AS role,
        t2~topic_msg,
        t2~text_msg,
        t2~pattern_msg
        FROM @lt_mail_pos AS t0 INNER JOIN ztmail_email AS t1 ON t1~view_msg = t0~view_msg
                                                             AND t1~par1     = t0~par1
                                                             AND t1~par2     = t0~par2
                                                             AND t1~par3     = t0~par3
                                                             AND t1~par4     = t0~par4
                                INNER JOIN ztmail_text  AS t2 ON t2~view_msg = t1~view_msg
                                LEFT JOIN agr_define   AS t3 ON t3~agr_name = t1~role
        INTO TABLE @DATA(lt_mail).

    ELSE.

      SELECT
        t1~view_msg,
        t1~email,
        t3~agr_name AS role,
        t2~topic_msg,
        t2~text_msg,
        t2~pattern_msg
        FROM ztmail_email AS t1 INNER JOIN ztmail_text AS t2 ON t2~view_msg = t1~view_msg
                                LEFT  JOIN agr_define  AS t3 ON t3~agr_name = t1~role
         WHERE t1~view_msg IN @lr_view_msg
           AND t1~par1     IN @lr_par1_msg
           AND t1~par2     IN @lr_par2_msg
           AND t1~par3     IN @lr_par3_msg
           AND t1~par4     IN @lr_par4_msg
        INTO TABLE @lt_mail.

    ENDIF.


    LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail>).

      READ TABLE it_placeholders_data ASSIGNING FIELD-SYMBOL(<ls_placeholders_data>)
      WITH KEY view_msg = <ls_mail>-view_msg.

      IF sy-subrc = 0.

        APPEND VALUE #( email = <ls_mail>-email role = <ls_mail>-role topic_msg = <ls_mail>-topic_msg
                        text_msg = <ls_mail>-text_msg pattern_msg = <ls_mail>-pattern_msg
                        placeholders_body = <ls_placeholders_data>-placeholders_body
                        placeholders_topic = <ls_placeholders_data>-placeholders_topic ) TO it_mail_send.

      ELSE.

        APPEND VALUE #( email = <ls_mail>-email role = <ls_mail>-role topic_msg = <ls_mail>-topic_msg
                        text_msg = <ls_mail>-text_msg pattern_msg = <ls_mail>-pattern_msg ) TO it_mail_send.

      ENDIF.

    ENDLOOP.

  ENDIF.



  TRY.
      LOOP AT it_mail_send ASSIGNING FIELD-SYMBOL(<ls_mail_send>).

        DATA(lcl_mailer) = NEW zcl_mailer( ).


        "Определение получателей
        IF <ls_mail_send>-email IS NOT INITIAL.

          "lv_smtp_addr = <ls_mail_send>-email."'v.kvizhinadze@vittecon.ru'.
          lcl_mailer->add_recipient_mail( iv_recipient_mail = CONV string( <ls_mail_send>-email ) ).

        ELSE.

          lcl_mailer->add_recipient_mail_role( iv_role = <ls_mail_send>-role ).

        ENDIF.


        "Определение тела и заголовка письма
        lcl_mailer->get_subject_and_body_text( EXPORTING
                                                  iv_subject            = <ls_mail_send>-topic_msg
                                                  iv_text               = <ls_mail_send>-text_msg
                                                  iv_pattern_msg        = <ls_mail_send>-pattern_msg
                                                  it_placeholders_body  = <ls_mail_send>-placeholders_body
                                                  it_placeholders_topic = <ls_mail_send>-placeholders_topic
                                               IMPORTING
                                                  ev_subject            = DATA(lv_subj)
                                                  ev_text               = DATA(lv_text) ).


        lcl_mailer->set_subject_and_body_text( iv_subject = lv_subj
                                               iv_text    = lv_text
                                               iv_type    = 'HTM' ).


        "Определение отправителя
        lcl_mailer->set_sender( iv_sender_address = 'sap@rshb.ru'
                                iv_sender_visname = 'sap@rshb.ru' ).


        "Kashiev-AV 12.01.2024 Добавление функциональности прикрепления документов к письму
        " Прикрепление документов
        LOOP AT <ls_mail_send>-attachments ASSIGNING FIELD-SYMBOL(<ls_attachments>).
          TRY.
              lcl_mailer->add_attachment( iv_doctype      = <ls_attachments>-iv_doctype
                                          iv_description  = <ls_attachments>-iv_description
                                          iv_filename     = <ls_attachments>-iv_filename
                                          iv_codepage     = <ls_attachments>-iv_codepage
                                          iv_contents_txt = <ls_attachments>-iv_contents_txt
                                          iv_contents_bin = <ls_attachments>-iv_contents_bin
                                          iv_content_id   = <ls_attachments>-iv_content_id
              ).
            CATCH cx_bcs_send.
          ENDTRY.
        ENDLOOP.



        "Отправка
        lcl_mailer->send( iv_immediately = iv_immediately
                          iv_update_task = iv_update_task  ).

        <ls_mail_send>-ind_send = abap_true.

        FREE lcl_mailer.

      ENDLOOP.

    CATCH cx_bcs_send INTO DATA(ex).
      MESSAGE ex->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFUNCTION.

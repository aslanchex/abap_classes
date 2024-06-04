@EndUserText.label : 'Таблица ведения событий и адресов отправки на почту'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #C
@AbapCatalog.dataMaintenance : #ALLOWED
define table ztmail_email {
  key mandt     : mandt not null;
  key view_msg  : ze_view_msg not null;
  key count_msg : ze_count_view_msg not null;
  role          : ze_agr_name
    with value help agr_name
      where agr_name = ztmail_email.role;
  fio           : uws_fullname;
  email         : ze_email;
  par1          : ze_par1_msg;
  par2          : ze_par2_msg;
  par3          : ze_par3_msg;
  par4          : ze_par4_msg;

}

@EndUserText.label : 'Таблица ведения сообщения майлера'
@AbapCatalog.enhancementCategory : #EXTENSIBLE_ANY
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #C
@AbapCatalog.dataMaintenance : #ALLOWED
define table ztmail_text {
  key mandt    : mandt not null;
  key view_msg : ze_view_msg not null;
  name_msg     : ze_name_msg;
  topic_msg    : ze_topic_msg;
  text_msg     : ze_text_msg;
  pattern_msg  : smtg_tmpl_id;

}

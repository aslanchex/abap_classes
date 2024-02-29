class CL_PROXY_XML_TRANSFORM definition
  public
  final
  create public .

public section.
*"* public components of class CL_PROXY_XML_TRANSFORM
*"* do not include other source files here!!!

  class-methods ABAP_TO_XML
    importing
      !ABAP_DATA type ANY
      !DDIC_TYPE type TYPENAME optional
      !XML_WRITER type ref to IF_SXML_WRITER
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !XML_HEADER type CSEQUENCE default 'no'
      !ROOT_ELEMENT type QNAME optional
      !XSI_TYPE type QNAME optional
      !SVAR_NAME type PRX_R3NAME optional
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods XML_TO_ABAP
    importing
      !DDIC_TYPE type TYPENAME optional
      !XML_READER type ref to IF_SXML_READER
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !ROOT_ELEMENT type QNAME optional
      !SVAR_NAME type PRX_R3NAME optional
    exporting
      !ABAP_DATA type ANY
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods XML_TO_EXCEPTION
    importing
      !FAULT_TYPE type TYPENAME optional
      !XML_READER type ref to IF_SXML_READER
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !SVAR_NAME type PRX_R3NAME optional
    changing
      !FAULT type ANY
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods ABAP_TO_XML_XSTRING
    importing
      !ABAP_DATA type ANY
      !DDIC_TYPE type TYPENAME optional
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !XML_HEADER type CSEQUENCE default 'no'
      !ROOT_ELEMENT type QNAME optional
      !SVAR_NAME type PRX_R3NAME optional
    returning
      value(XML) type XSTRING
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods EXCEPTION_TO_XML_XSTRING
    importing
      !FAULT type ANY
      !FAULT_TYPE type TYPENAME optional
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !XML_HEADER type CSEQUENCE default 'no'
      !SVAR_NAME type PRX_R3NAME optional
    returning
      value(XML) type XSTRING
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods XML_XSTRING_TO_ABAP
    importing
      !DDIC_TYPE type TYPENAME optional
      !XML type XSTRING
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !SVAR_NAME type PRX_R3NAME optional
    exporting
      !ABAP_DATA type ANY
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods XML_XSTRING_TO_EXCEPTION
    importing
      !FAULT_TYPE type TYPENAME optional
      !XML type XSTRING
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !SVAR_NAME type PRX_R3NAME optional
    changing
      !FAULT type ANY
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods EXCEPTION_TO_XML
    importing
      !FAULT_TYPE type PRX_R3NAME optional
      !FAULT type ANY
      !XML_WRITER type ref to IF_SXML_WRITER
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !SVAR_NAME type PRX_R3NAME optional
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
protected section.
*"* protected components of class CL_PROXY_XML_TRANSFORM
*"* do not include other source files here!!!
private section.
*"* private components of class CL_PROXY_XML_TRANSFORM
*"* do not include other source files here!!!

  class-methods ABAP_TO_XML_DIRECT
    importing
      !ABAP_DATA type ANY
      !DDIC_TYPE type TYPENAME optional
      !XML_WRITER type ref to IF_SXML_WRITER
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !SVAR_NAME type PRX_R3NAME
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods ABAP_TO_XML_INDIRECT
    importing
      !ABAP_DATA type ANY
      !DDIC_TYPE type TYPENAME optional
      !XML_WRITER type ref to IF_SXML_WRITER
      !EXT_XML type ABAP_BOOL default ABAP_FALSE
      !ROOT_ELEMENT type QNAME
      !XSI_TYPE type QNAME
      !SVAR_NAME type PRX_R3NAME
    raising
      CX_PROXY_FAULT
      CX_TRANSFORMATION_ERROR .
  class-methods GET_TYPE
    importing
      !ABAP_DATA type ANY
      !DDIC_TYPE type TYPENAME optional
    returning
      value(RVAL) type TYPENAME
    raising
      CX_PROXY_FAULT .
  class-methods COPY_NS_DECLARATIONS
    importing
      !READER type ref to IF_SXML_READER
      !WRITER type ref to IF_SXML_WRITER .
  class-methods GET_INTERFACE
    importing
      !REF type ANY
      !NAME type CSEQUENCE
    returning
      value(RVAL) type STRING
    raising
      CX_PROXY_FAULT .
ENDCLASS.



CLASS CL_PROXY_XML_TRANSFORM IMPLEMENTATION.


method abap_to_xml.

  statics:
    lv_debug type abap_bool.

  data:
    lr_root       type ref to cx_root,
    lr_st_error   type ref to cx_st_error,
    lv_main_name  type string,                              "#EC NEEDED
    lv_prog_name  type string,                              "#EC NEEDED
    lv_line       type i,                                   "#EC NEEDED
    lv_text       type string,                              "#EC NEEDED
    lv_longtext   type string.                              "#EC NEEDED

  try.
      if root_element is initial.
        cl_proxy_xml_transform=>abap_to_xml_direct(
            abap_data     = abap_data
            ddic_type     = ddic_type
            xml_writer    = xml_writer
            ext_xml       = ext_xml
            svar_name     = svar_name
          ).
      else.
        cl_proxy_xml_transform=>abap_to_xml_indirect(
            abap_data     = abap_data
            ddic_type     = ddic_type
            xml_writer    = xml_writer
            ext_xml       = ext_xml
            root_element  = root_element
            xsi_type      = xsi_type
            svar_name     = svar_name
          ).
      endif.
    cleanup into lr_root.
*     lv_debug can be set to 'X' in the debugger
*     to get more info regarding the error occured
      while lv_debug = abap_true and lr_root is bound.
        try.
            lr_st_error ?= lr_root.
            lr_st_error->get_st_source_position(
              importing
                main_name = lv_main_name
                prog_name = lv_prog_name
                line      = lv_line
            ).
          catch cx_sy_move_cast_error.                  "#EC NO_HANDLER
        endtry.
        lv_text = lr_root->get_text( ).
        lv_longtext = lr_root->get_longtext( ).
        lr_root = lr_root->previous.
      endwhile.
  endtry.
endmethod.


method abap_to_xml_direct.

  data:
    lr_error      type ref to cx_root,
    lr_sxmlp      type ref to cx_sxmlp,
    lr_transformation_error type ref to cx_transformation_error,
    lv_type       type typename,
    lr_serializer type ref to if_sxml_serializable,
    lr_data_ref   type ref to data.

  try.
      lv_type = get_type( ddic_type = ddic_type  abap_data = abap_data ).

      get reference of abap_data into lr_data_ref.

      lr_serializer = cl_proxy_st_part=>create_for_type(
        type                  = lv_type
        ref                   = lr_data_ref
        for_serialize         = abap_true
        for_deserialize       = abap_false
        extended_xml_handling = ext_xml
        svar_name             = svar_name
      ).
      lr_serializer->serialize(
        writer         = xml_writer
      ).

    catch cx_xms_system_error cx_sxmlp into lr_error.
      try.
          lr_sxmlp ?= lr_error.
          lr_transformation_error ?= lr_sxmlp->previous.
          lr_error = lr_transformation_error.
        catch cx_sy_move_cast_error.                    "#EC NO_HANDLER
      endtry.
      raise exception type cx_proxy_fault
        exporting
          previous = lr_error.
  endtry.

endmethod.


method abap_to_xml_indirect.

  data:
    lv_xml    type xstring,
    lv_level  type i,
    lr_reader type ref to if_sxml_reader,
    lv_prefix type string,
    lv_use_def_ns type prx_boolean,
    lv_nsuri  type string,
    lv_xsi_type type string,
    lt_nsbindings type if_sxml_named=>nsbindings,
    lr_nsbinding type ref to if_sxml_named=>nsbinding.


  lv_xml = cl_proxy_xml_transform=>abap_to_xml_xstring(
    abap_data     = abap_data
    ddic_type     = ddic_type
    ext_xml       = ext_xml
    svar_name     = svar_name
  ).

  lr_reader = cl_proxy_sxml_factory=>create_string_reader( input = lv_xml ).
  lr_reader->next_node( ).

  if root_element-namespace is initial.
    lt_nsbindings = lr_reader->get_nsbindings( ).
    read table lt_nsbindings with key prefix = space reference into lr_nsbinding.
    if sy-subrc = 0 and not lr_nsbinding->nsuri is initial.
      raise exception type cx_proxy_serialization_error
        exporting
          name  = root_element-name
          nsuri = lr_nsbinding->nsuri.
    endif.
  else.
*   if there is already a prefix for the namespace use it if possible
    lt_nsbindings = xml_writer->get_nsbindings( ).
    read table lt_nsbindings with key nsuri = root_element-namespace reference into lr_nsbinding. "#EC CI_HASHSEQ
    if sy-subrc = 0.
      lv_prefix = lr_nsbinding->prefix.
      lt_nsbindings = lr_reader->get_nsbindings( ).
      read table lt_nsbindings with key prefix = lv_prefix reference into lr_nsbinding.
      if sy-subrc = 0 and lv_prefix <> lr_nsbinding->prefix.
*       do not use prefix if it clashes with a prefix already in use
        clear lv_prefix.
      endif.
    endif.

    lt_nsbindings = lr_reader->get_nsbindings( ).
    read table lt_nsbindings with key prefix = space reference into lr_nsbinding.
    if sy-subrc = 0.
      lv_prefix = if_sxml_named=>co_use_default_xmlns.
      lv_use_def_ns = sprx_true.
    endif.

    if lv_prefix is initial.
      do.
        lv_prefix = sy-index - 1. " start with 0
        concatenate 'n' lv_prefix into lv_prefix.
        condense lv_prefix no-gaps.
        lv_nsuri = lr_reader->get_nsuri_by_prefix( prefix = lv_prefix ).
        if not lv_nsuri is initial and lv_nsuri <> root_element-namespace.
          continue.
        endif.
        exit.
      enddo.
    endif.
  endif.

  xml_writer->open_element(
    name   = root_element-name
    nsuri  = root_element-namespace
    prefix = lv_prefix
  ).


  if not xsi_type is initial.

    xml_writer->write_namespace_declaration( nsuri = if_proxy_const_ns=>namespace_xsi  prefix = 'xsi' ).
    if xsi_type-namespace = if_proxy_const_ns=>namespace_xsd.
      xml_writer->write_namespace_declaration( nsuri = if_proxy_const_ns=>namespace_xsd  prefix = 'xsd' ).
    endif.

    lv_prefix = xml_writer->get_prefix_by_nsuri( nsuri = xsi_type-namespace ).
    if lv_prefix is initial.
      xml_writer->write_namespace_declaration( nsuri = xsi_type-namespace ).
      lv_prefix = xml_writer->get_prefix_by_nsuri( nsuri = xsi_type-namespace ).
    endif.
    concatenate lv_prefix ':' xsi_type-name into lv_xsi_type.
    xml_writer->write_attribute( name = 'type' nsuri = if_proxy_const_ns=>namespace_xsi value = lv_xsi_type ). "#EC NOTEXT
  endif.

  if lr_reader->node_type <> if_sxml_node=>co_nt_element_open.
    raise exception type cx_xslt_serialization_error.
  endif.

  lv_level = 0.
  while lr_reader->node_type <> if_sxml_node=>co_nt_final.
    case lr_reader->node_type.

      when if_sxml_node=>co_nt_initial.

      when if_sxml_node=>co_nt_element_open.
        if lv_level > 0.
          if lr_reader->prefix is initial and not lr_reader->nsuri is initial and lv_use_def_ns = sprx_true.
            lv_prefix = if_sxml_named=>co_use_default_xmlns.
          else.
            lv_prefix = lr_reader->prefix.
          endif.
          xml_writer->open_element(
            name   = lr_reader->name
            nsuri  = lr_reader->nsuri
            prefix = lv_prefix
          ).
        endif.
        copy_ns_declarations( reader = lr_reader  writer = xml_writer ).
        lr_reader->next_attribute( ).
        if lr_reader->node_type <> if_sxml_node=>co_nt_attribute.
          lr_reader->next_node( ).
        endif.
        lv_level = lv_level + 1.

      when if_sxml_node=>co_nt_element_close.
        lv_level = lv_level - 1.
        if lv_level = 0.
          exit.
        endif.
        xml_writer->close_element( ).
        lr_reader->next_node( ).

      when if_sxml_node=>co_nt_value.
        xml_writer->write_value(
          value = lr_reader->value
        ).
        lr_reader->next_node( ).

      when if_sxml_node=>co_nt_attribute.
        xml_writer->write_attribute(
          name   = lr_reader->name
          nsuri  = lr_reader->nsuri
          prefix = lr_reader->prefix
          value  = lr_reader->value
        ).
        lr_reader->next_attribute( ).
        if lr_reader->node_type <> if_sxml_node=>co_nt_attribute.
          lr_reader->next_node( ).
        endif.

      when if_sxml_node=>co_nt_final.

    endcase.

  endwhile.

  xml_writer->close_element( ).
endmethod.


method abap_to_xml_xstring.

  data:
    lr_xml_writer type ref to cl_sxml_string_writer.

  lr_xml_writer = cl_proxy_sxml_factory=>create_string_writer( ).

  call method abap_to_xml
    exporting
      abap_data     = abap_data
      ddic_type     = ddic_type
      xml_writer    = lr_xml_writer
      ext_xml       = ext_xml
      root_element  = root_element
      svar_name     = svar_name.

  xml = lr_xml_writer->get_output( ).

  case xml_header.
    when 'no'.
    when 'without_encoding'.
      xml = cl_proxy_xml_utils=>add_xml_header( xml = xml ).
    when 'full'.
      xml = cl_proxy_xml_utils=>add_xml_header( xml = xml  full = abap_true ).
  endcase.
endmethod.


method copy_ns_declarations.
  data:
    lt_reader_bindings   type if_sxml_named=>nsbindings,
    lt_writer_bindings   type if_sxml_named=>nsbindings,
    lr_reader_binding   type ref to if_sxml_named=>nsbinding,
    lr_writer_binding   type ref to if_sxml_named=>nsbinding.

  lt_reader_bindings = reader->get_nsbindings( ).
  lt_writer_bindings = writer->get_nsbindings( ).

  loop at lt_reader_bindings reference into lr_reader_binding.
    read table lt_writer_bindings with key prefix = lr_reader_binding->prefix
                                 reference into lr_writer_binding.
    if sy-subrc <> 0 or ( lr_reader_binding->nsuri <> lr_writer_binding->nsuri and not lr_reader_binding->prefix is initial ).
*     workarround: skip namespace declaration for prx to avoid different
*                  namespace declarations for prx even on different levels
      if lr_reader_binding->prefix = 'prx' and lr_reader_binding->nsuri cp '*:proxy:*'.
        continue.
      endif.
      writer->write_namespace_declaration(
        nsuri  = lr_reader_binding->nsuri
        prefix = lr_reader_binding->prefix
      ).
    endif.
  endloop.

endmethod.


method exception_to_xml.
  data:
    lr_error      type ref to cx_root,
    lr_sxmlp      type ref to cx_sxmlp,
    lr_transformation_error type ref to cx_transformation_error,
    lv_type       type prx_r3name,
    lr_serializer type ref to if_sxml_serializable.

  try.
      lv_type = get_interface( name = fault_type  ref = fault ).

      lr_serializer = cl_proxy_st_part=>create_for_exception(
        exception             = lv_type
        for_serialize         = abap_true
        for_deserialize       = abap_false
        extended_xml_handling = ext_xml
        svar_name             = svar_name
        application_fault     = fault
      ).
      lr_serializer->serialize(
        writer         = xml_writer
      ).

    catch cx_xms_system_error cx_sxmlp into lr_error.
      try.
          lr_sxmlp ?= lr_error.
          lr_transformation_error ?= lr_sxmlp->previous.
          raise exception lr_transformation_error.
        catch cx_sy_move_cast_error.                    "#EC NO_HANDLER
      endtry.
      raise exception type cx_proxy_fault
        exporting
          previous = lr_error.
  endtry.

endmethod.


method exception_to_xml_xstring.

  data:
    lr_xml_writer type ref to cl_sxml_string_writer.

  lr_xml_writer = cl_proxy_sxml_factory=>create_string_writer( ).

  call method exception_to_xml
    exporting
      fault      = fault
      fault_type = fault_type
      xml_writer = lr_xml_writer
      ext_xml    = ext_xml
      svar_name  = svar_name.

  xml = lr_xml_writer->get_output( ).

  case xml_header.
    when 'no'.
    when 'without_encoding'.
      xml = cl_proxy_xml_utils=>add_xml_header( xml = xml ).
    when 'full'.
      xml = cl_proxy_xml_utils=>add_xml_header( xml = xml  full = abap_true ).
  endcase.
endmethod.


method get_interface.

  class cl_abap_typedescr definition load.

  data: rd type ref to cl_abap_refdescr,
        td type ref to cl_abap_typedescr.

  if name is initial.
    clear rval.
    try.
        rd ?= cl_abap_typedescr=>describe_by_data( ref ).
        td = rd->get_referenced_type( ).
        rval = td->get_relative_name( ).
      catch cx_root                                       "#EC CATCH_ALL
          .                                               "#EC NO_HANDLER
    endtry.
  else.
    rval = name.
  endif.

endmethod.


method get_type.

  data:
    lr_typedescr type ref to cl_abap_typedescr.

  if ddic_type is initial.
    lr_typedescr = cl_abap_typedescr=>describe_by_data( abap_data ).
    rval = lr_typedescr->get_relative_name( ).
  else.
    rval = ddic_type.
  endif.

endmethod.


method xml_to_abap.

  statics:
    lv_debug type abap_bool.

  data:
    lr_st_error              type ref to cx_st_error,
    lv_main_name             type string,                   "#EC NEEDED
    lv_prog_name             type string,                   "#EC NEEDED
    lv_line                  type i,                        "#EC NEEDED
    lv_text                  type string,                   "#EC NEEDED
    lv_longtext              type string,                   "#EC NEEDED
    lr_root                  type ref to cx_root,
    lr_sxmlp                 type ref to cx_sxmlp,
    lr_transformation_error  type ref to cx_transformation_error,
    lv_type                  type typename,
    lr_deserializer          type ref to if_sxml_serializable,
    lr_data_ref              type ref to data.

  try.
      try.
          if not root_element is initial.
            xml_reader->next_node( ).
            if xml_reader->node_type <> if_sxml_node=>co_nt_element_open
              or xml_reader->name  <> root_element-name
              or xml_reader->nsuri <> root_element-namespace .
              raise exception type cx_xslt_deserialization_error.
            endif.
            xml_reader->push_back( ).
          endif.

          lv_type = get_type( ddic_type = ddic_type  abap_data = abap_data ).
          get reference of abap_data into lr_data_ref.

          lr_deserializer = cl_proxy_st_part=>create_for_type(
            type                  = lv_type
            ref                   = lr_data_ref
            for_serialize         = abap_false
            for_deserialize       = abap_true
            extended_xml_handling = ext_xml
            svar_name             = svar_name
          ).

          lr_deserializer->deserialize(
            reader         = xml_reader
          ).

        catch cx_xms_system_error cx_sxmlp into lr_root.
          try.
              lr_sxmlp ?= lr_root.
              lr_transformation_error ?= lr_sxmlp->previous.
              raise exception lr_transformation_error.
            catch cx_sy_move_cast_error.                "#EC NO_HANDLER
          endtry.
          raise exception type cx_proxy_fault
            exporting
              previous = lr_root.
      endtry.

    cleanup into lr_root.
*     lv_debug can be set to 'X' in the debugger
*     to get more info regarding the error occured
      while lv_debug = abap_true and lr_root is bound.
        try.
            lr_st_error ?= lr_root.
            lr_st_error->get_st_source_position(
              importing
                main_name = lv_main_name
                prog_name = lv_prog_name
                line      = lv_line
            ).
          catch cx_sy_move_cast_error.                  "#EC NO_HANDLER
        endtry.
        lv_text = lr_root->get_text( ).
        lv_longtext = lr_root->get_longtext( ).
        lr_root = lr_root->previous.
      endwhile.
  endtry.


endmethod.


method xml_to_exception.

  statics:
    lv_debug type abap_bool.

  data:
    lr_st_error              type ref to cx_st_error,
    lv_main_name             type string,                   "#EC NEEDED
    lv_prog_name             type string,                   "#EC NEEDED
    lv_line                  type i,                        "#EC NEEDED
    lv_text                  type string,                   "#EC NEEDED
    lv_longtext              type string,                   "#EC NEEDED
    lr_root                  type ref to cx_root,
    lr_sxmlp                 type ref to cx_sxmlp,
    lr_transformation_error  type ref to cx_transformation_error,
    lv_type                  type typename,
    lr_deserializer            type ref to if_sxmlp_data_st,
    lt_data_refs             type abap_trans_resbind_tab,
    ls_data_ref              type abap_trans_resbind,
    lt_param                 type abap_parmbind_tab,
    ls_param                 type abap_parmbind.

  lv_type = get_interface( name = fault_type ref = fault ).
  try.
      try.
          lr_deserializer = cl_proxy_st_part=>create_for_exception(
            exception             = lv_type
            for_serialize         = abap_false
            for_deserialize       = abap_true
            extended_xml_handling = ext_xml
            add_data_refs         = abap_true
            svar_name             = svar_name
          ).

          lr_deserializer->deserialize(
            reader         = xml_reader
          ).

          lt_data_refs = lr_deserializer->get_data_refs( ).

          loop at lt_data_refs into ls_data_ref.
            ls_param-name  = ls_data_ref-name.
            ls_param-value = ls_data_ref-value.
            insert ls_param into table lt_param.
          endloop.

          create object fault type (lv_type)
            parameter-table lt_param.

        catch cx_xms_system_error cx_sxmlp into lr_root.
          try.
              lr_sxmlp ?= lr_root.
              lr_transformation_error ?= lr_sxmlp->previous.
              raise exception lr_transformation_error.
            catch cx_sy_move_cast_error.                "#EC NO_HANDLER
          endtry.
          raise exception type cx_proxy_fault
            exporting
              previous = lr_root.
      endtry.

    cleanup into lr_root.
*     lv_debug can be set to 'X' in the debugger
*     to get more info regarding the error occured
      while lv_debug = abap_true and lr_root is bound.
        try.
            lr_st_error ?= lr_root.
            lr_st_error->get_st_source_position(
              importing
                main_name = lv_main_name
                prog_name = lv_prog_name
                line      = lv_line
            ).
          catch cx_sy_move_cast_error.                  "#EC NO_HANDLER
        endtry.
        lv_text = lr_root->get_text( ).
        lv_longtext = lr_root->get_longtext( ).
        lr_root = lr_root->previous.
      endwhile.
  endtry.


endmethod.


method xml_xstring_to_abap.

  constants:
    c_whitspaces type x length 4 value '09200A0D'.

  data:
    lr_xml_reader type ref to if_sxml_reader,
    lr_parse_error type ref to cx_sxml_parse_error.

  field-symbols:
    <offset> type i.

  lr_xml_reader = cl_proxy_sxml_factory=>create_string_reader( input = xml ).

  call method xml_to_abap
    exporting
      ddic_type  = ddic_type
      xml_reader = lr_xml_reader
      ext_xml    = ext_xml
      svar_name  = svar_name
    importing
      abap_data  = abap_data.

  lr_xml_reader->next_node( ).
  assert lr_xml_reader->node_type = if_sxml_node=>co_nt_final.
  try.
      lr_xml_reader->next_node( ).
      assert ( 1 = 0 ).                                    "#EC BOOL_OK
    catch cx_sxml_parse_error into lr_parse_error.
      assign lr_parse_error->('XML_OFFSET') to <offset>.
      if sy-subrc = 0
         and <offset> > 0
         and <offset> < xstrlen( xml )
         and not xml+<offset> byte-co c_whitspaces.
        raise exception
          type
          cx_proxy_fault_text
          exporting
            error_text = 'Invalid XML, unexpected data after end of first element'(001).
      endif.
  endtry.

endmethod.


method xml_xstring_to_exception.

  constants:
    c_whitspaces type x length 4 value '09200A0D'.

  data:
    lr_xml_reader type ref to if_sxml_reader,
    lr_parse_error type ref to cx_sxml_parse_error.

  field-symbols:
    <offset> type i.

  lr_xml_reader = cl_proxy_sxml_factory=>create_string_reader( input = xml ).

  xml_to_exception(
    exporting
      fault_type = fault_type
      xml_reader = lr_xml_reader
      ext_xml    = ext_xml
      svar_name  = svar_name
    changing
      fault      = fault
  ).

  lr_xml_reader->next_node( ).
  assert lr_xml_reader->node_type = if_sxml_node=>co_nt_final.
  try.
      lr_xml_reader->next_node( ).
      assert ( 1 = 0 ).                                    "#EC BOOL_OK
    catch cx_sxml_parse_error into lr_parse_error.
      assign lr_parse_error->('XML_OFFSET') to <offset>.
      if sy-subrc = 0
         and <offset> > 0
         and <offset> < xstrlen( xml )
         and not xml+<offset> byte-co c_whitspaces.
        raise exception
          type
          cx_proxy_fault_text
          exporting
            error_text = 'Invalid XML, unexpected data after end of first element'(001).
      endif.
  endtry.

endmethod.
ENDCLASS.

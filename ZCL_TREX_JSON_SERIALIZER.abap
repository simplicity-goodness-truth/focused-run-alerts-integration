class ZCL_TREX_JSON_SERIALIZER definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_TREX_JSON_SERIALIZER
*"* do not include other source files here!!!
  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !DATA type DATA .
  methods SERIALIZE .
  methods GET_DATA
    returning
      value(RVAL) type STRING .
protected section.
*"* protected components of class ZL_ASXML_TO_JSON
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_TREX_JSON_SERIALIZER
*"* do not include other source files here!!!

  data FRAGMENTS type TREXT_STRING .
  data DATA_REF type ref to DATA .
  class-data C_COLON type STRING .
  class-data C_COMMA type STRING .

  methods RECURSE
    importing
      !DATA type DATA .
ENDCLASS.



CLASS ZCL_TREX_JSON_SERIALIZER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TREX_JSON_SERIALIZER=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CLASS_CONSTRUCTOR.
  cl_abap_string_utilities=>c2str_preserving_blanks(
      exporting source = ': '
      importing dest   = c_colon ) .
  cl_abap_string_utilities=>c2str_preserving_blanks(
      exporting source = ', '
      importing dest   = c_comma ) .
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TREX_JSON_SERIALIZER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONSTRUCTOR.
  get reference of data into me->data_ref .
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TREX_JSON_SERIALIZER->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RVAL                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_DATA.
  concatenate lines of me->fragments into rval .
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TREX_JSON_SERIALIZER->RECURSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method recurse.
  data:
    l_type  type c,
    l_comps type i,
    l_lines type i,
    l_index type i,
    l_value type string.
  field-symbols:
    <itab> type any table,
    <comp> type any.

  describe field data type l_type components l_comps .

  if l_type = cl_abap_typedescr=>typekind_table .
*   itab -> array
    append '[' to me->fragments .
    assign data to <itab> .
    l_lines = lines( <itab> ) .
    loop at <itab> assigning <comp> .
      add 1 to l_index .
      recurse( <comp> ) .
      if l_index < l_lines .
        append c_comma to me->fragments .
      endif .
    endloop .
    append ']' to fragments .
  else .
    if l_comps is initial .
*     field -> scalar
*     todo: format
      l_value = data .
      replace all occurrences of '\' in l_value with '\\' .
      replace all occurrences of '''' in l_value with '\''' .
      replace all occurrences of '"' in l_value with '\"' .
     " replace all occurrences of '&' in l_value with '\&' .
      replace all occurrences of cl_abap_char_utilities=>cr_lf in l_value with '\r\n' .
      replace all occurrences of cl_abap_char_utilities=>newline in l_value with '\n' .
      replace all occurrences of cl_abap_char_utilities=>horizontal_tab in l_value with '\t' .
      replace all occurrences of cl_abap_char_utilities=>backspace in l_value with '\b' .
      replace all occurrences of cl_abap_char_utilities=>form_feed in l_value with '\f' .
      concatenate '"' l_value '"' into l_value .
      append l_value to me->fragments .
    else .
*     structure -> object
      data l_typedescr type ref to cl_abap_structdescr .
      field-symbols <abapcomp> type abap_compdescr .

      append '{' to me->fragments .
      l_typedescr ?= cl_abap_typedescr=>describe_by_data( data ) .
      loop at l_typedescr->components assigning <abapcomp> .
        l_index = sy-tabix .
        "concatenate <abapcomp>-name c_colon into l_value .

        " Custom development: adding double quotes into attribute

        concatenate '"' <abapcomp>-name '"' c_colon into l_value .

        translate l_value to lower case .
        append l_value to me->fragments .
        assign component <abapcomp>-name of structure data to <comp> .
        recurse( <comp> ) .
        if l_index < l_comps .
          append c_comma to me->fragments .
        endif .
      endloop .
      append '}' to me->fragments .
    endif .
  endif .
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TREX_JSON_SERIALIZER->SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SERIALIZE.
  field-symbols <data> type data .

  assign me->data_ref->* to <data> .
  recurse( <data> ) .
endmethod.
ENDCLASS.
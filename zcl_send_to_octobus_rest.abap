class zcl_send_to_octobus_rest definition
  public
  final
  create public .

  public section.

    interfaces if_acc_reaction_ext .
    interfaces if_badi_interface .

    types:
      begin of ty_metrics_state,
        name        type ac_name,
        rating      type acc_rating,
        value       type string,
        metric_path type string,
      end of ty_metrics_state .
    types:
      t_metrics_state type standard table of ty_metrics_state
             with non-unique default key .
    types:
      begin of as_extracted_data,
        context_id            type ac_guid,
        type_id               type ac_guid,
        rating                type acc_rating,
        hash_metric_path      type hash160,
        start_date_utc        type sydatum,
        start_time_utc        type syuzeit,
        end_date_utc          type sydatum,
        end_time_utc          type syuzeit,
        mo_name               type string,
        mo_type               type ac_context_type,
        name                  type ac_name,
        short_text            type string,
        category              type string,
        tech_scenario         type ac_technical_scenario,
        sap_description       type string,
        custom_description    type string,
        type                  type acc_mea_type,
        reason_for_closure    type acc_reason_for_closure,
        unit                  type ac_unit,
        value                 type string,
        text_value            type string,
        metric_path           type string,
        parent_context_id     type ac_guid,
        parent_type_id        type ac_guid,
        parent_type           type acc_mea_type,
        customer_network      type string,
        customer_display_name type string,
        data_center           type  lmdb_datacenter_name,
        hostname              type string,
        priority              type e2ea_priority,
        severity              type ac_severity,
      end of as_extracted_data .
    types:
      at_extracted_data type table of as_extracted_data .
    types:
      begin of ty_json_req,

        alert_group_state     type char6,
        rating                type acc_rating,
        start_date_utc        type sydatum,
        start_time_utc        type syuzeit,
        end_date_utc          type sydatum,
        end_time_utc          type syuzeit,
        mo_name               type string,
        mo_type               type ac_context_type,
        name                  type ac_name,
        short_text            type string,
        category              type string,
        tech_scenario         type ac_technical_scenario,
        sap_description       type string,
        custom_description    type string,
        reason_for_closure    type acc_reason_for_closure,
        customer_network      type string,
        customer_display_name type string,
        data_center           type  lmdb_datacenter_name,
        hostname              type string,
        priority              type e2ea_priority,
        severity              type ac_severity,
        metrics_list          type t_metrics_state,
        alr_app_url           type string,
        frun_sid              type sy-sysid,
      end of ty_json_req .

    data json_req type ty_json_req .

    methods prepare_and_send_data
      importing
        !ip_alert             type acc_s_mea
        !ip_alert_group_state type char6 .
protected section.
private section.

  data MT_EVENTMETRIC type AT_EXTRACTED_DATA .

  class-methods LOG_OCTOBUS_COMM_RECORD
    importing
      !IP_MSGTY type SYMSGTY
      !IP_LOG_RECORD_TEXT type STRING .
  class-methods PUSH_TO_OCTOBUS
    importing
      !IP_DESTINATION type RFCDEST
      !IP_PARAMLINE type CHAR1024 optional
      !IP_JSON_REQUEST type STRING
    exporting
      !EP_JSON_RESPONSE type STRING .
  methods EXTRACT_EVNTMET_RECURSIVELY
    importing
      !IS_EVENTMETRIC type ACC_S_MEA
      !IV_PARENT_CONTEXT_ID type AC_GUID
      !IV_PARENT_TYPE_ID type AC_GUID
      !IV_PARENT_TYPE type ACC_MEA_TYPE .
ENDCLASS.



CLASS ZCL_SEND_TO_OCTOBUS_REST IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SEND_TO_OCTOBUS_REST->EXTRACT_EVNTMET_RECURSIVELY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_EVENTMETRIC                 TYPE        ACC_S_MEA
* | [--->] IV_PARENT_CONTEXT_ID           TYPE        AC_GUID
* | [--->] IV_PARENT_TYPE_ID              TYPE        AC_GUID
* | [--->] IV_PARENT_TYPE                 TYPE        ACC_MEA_TYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD EXTRACT_EVNTMET_RECURSIVELY.

  "This method recursively extracts subobjects of the incoming object
  "and stores the result in a member variable mt_eventmetric
  "The method also maintains the parent-child relationship of all the entities
  "in the member variable mt_eventmetric

  DATA ls_extracted_data            LIKE LINE OF mt_eventmetric.

  ls_extracted_data-context_id = is_eventmetric-context_id.
  ls_extracted_data-type_id = is_eventmetric-type_id.
  ls_extracted_data-parent_context_id = iv_parent_context_id.
  ls_extracted_data-parent_type_id = iv_parent_type_id.
  IF is_eventmetric-ref IS NOT INITIAL .
    ls_extracted_data-hash_metric_path = is_eventmetric-ref->get_hash_metric_path( ).
    ls_extracted_data-start_date_utc = is_eventmetric-ref->get_start_date_utc( ).
    ls_extracted_data-start_time_utc = is_eventmetric-ref->get_start_time_utc( ).
    ls_extracted_data-mo_name = is_eventmetric-ref->get_managed_object_name( ).
    ls_extracted_data-mo_type = is_eventmetric-ref->get_managed_object_type( ).
    ls_extracted_data-name = is_eventmetric-ref->get_name( ).
    ls_extracted_data-short_text = is_eventmetric-ref->get_short_text( ).
    ls_extracted_data-category = is_eventmetric-ref->get_category( ).
    ls_extracted_data-rating = is_eventmetric-ref->get_rating( ).
    ls_extracted_data-tech_scenario = is_eventmetric-ref->get_technical_scenario( ).
    ls_extracted_data-value = is_eventmetric-ref->get_value( ).
    ls_extracted_data-text_value = is_eventmetric-ref->get_text_value( ).
    ls_extracted_data-unit = is_eventmetric-ref->get_unit( ).
    ls_extracted_data-type = is_eventmetric-ref->get_type( ).
    ls_extracted_data-parent_type = iv_parent_type.
    ls_extracted_data-sap_description = is_eventmetric-ref->get_sap_description( ).
    ls_extracted_data-custom_description = is_eventmetric-ref->get_custom_description( ).
    ls_extracted_data-metric_path = is_eventmetric-ref->get_metric_path( ).
    INSERT ls_extracted_data INTO TABLE mt_eventmetric.
    IF is_eventmetric-ref->has_children( ) EQ abap_true.
      LOOP AT is_eventmetric-ref->get_children( ) INTO DATA(ls_eventmetric).
        me->extract_evntmet_recursively( is_eventmetric = ls_eventmetric iv_parent_context_id = is_eventmetric-context_id iv_parent_type_id = is_eventmetric-type_id
                                          iv_parent_type = is_eventmetric-ref->get_type( ) ).
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SEND_TO_OCTOBUS_REST->IF_ACC_REACTION_EXT~APPLICABLE_FOR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_APPLICABLE_FOR_FLAG         TYPE        AC_APPLICABLE_FOR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD IF_ACC_REACTION_EXT~APPLICABLE_FOR.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SEND_TO_OCTOBUS_REST->IF_ACC_REACTION_EXT~REACT_TO_ALERT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ALERT                       TYPE        ACC_S_MEA
* | [--->] IV_MANUAL_TRIGGER              TYPE        BOOLEAN(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method if_acc_reaction_ext~react_to_alert.


  me->prepare_and_send_data( ip_alert = is_alert
                             ip_alert_group_state = 'Opened' ).


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SEND_TO_OCTOBUS_REST->IF_ACC_REACTION_EXT~REACT_TO_CLOSED_ALERT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ALERT                       TYPE        ACC_S_MEA
* | [--->] IV_MANUAL_TRIGGER              TYPE        BOOLEAN(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method if_acc_reaction_ext~react_to_closed_alert.

  me->prepare_and_send_data( ip_alert = is_alert
                             ip_alert_group_state = 'Closed' ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SEND_TO_OCTOBUS_REST=>LOG_OCTOBUS_COMM_RECORD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_MSGTY                       TYPE        SYMSGTY
* | [--->] IP_LOG_RECORD_TEXT             TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method log_octobus_comm_record.

    " Data declaration for Application Log operations

    data ls_log      type bal_s_log.
    data lv_msgtext  type baltmsg.
    data ev_log_handle  type balloghndl.

    data ls_msg type bal_s_msg.

    data:
      begin of ls_string,
        part1 type symsgv,
        part2 type symsgv,
        part3 type symsgv,
        part4 type symsgv,
      end of ls_string.

    data: lt_log_handle type bal_t_logh,
          lt_log_num    type bal_t_lgnm.

    data ev_subrc  type sysubrc.


    concatenate sy-uzeit ip_log_record_text into ls_string separated by space.

    " Preparing the Application Log

    clear ev_log_handle.

    ls_log-object    = 'ZOCTOBUS'.
    ls_log-subobject = 'ZEVTINTGR'.
    ls_log-aldate    = sy-datum.
    ls_log-altime    = sy-uzeit.
    ls_log-aluser    = sy-uname.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log                 = ls_log
      importing
        e_log_handle            = ev_log_handle
      exceptions
        log_header_inconsistent = 1
        others                  = 2.
    if sy-subrc <> 0.
      ev_subrc = 1.
      return.
    endif.

    ls_msg-msgv1     = ls_string-part1.
    ls_msg-msgv2     = ls_string-part2.
    ls_msg-msgv3     = ls_string-part3.
    ls_msg-msgv4     = ls_string-part4.

    ls_msg-msgty = ip_msgty.
    ls_msg-msgid = 'BL'.
    ls_msg-msgno = '001'.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle  = ev_log_handle
        i_s_msg       = ls_msg
      exceptions
        log_not_found = 0
        others        = 1.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    endif.

    " Finalizing the Application Log records

    insert ev_log_handle into lt_log_handle index 1.

    call function 'BAL_DB_SAVE'
      exporting
        i_client         = sy-mandt
        i_save_all       = ' '
        i_t_log_handle   = lt_log_handle
      importing
        e_new_lognumbers = lt_log_num
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc <> 0.
      ev_subrc = sy-subrc.
    endif.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SEND_TO_OCTOBUS_REST->PREPARE_AND_SEND_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_ALERT                       TYPE        ACC_S_MEA
* | [--->] IP_ALERT_GROUP_STATE           TYPE        CHAR6
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method prepare_and_send_data.

    data: ls_input_alert         type as_extracted_data,
          ls_json_req_obj        type ty_json_req,
          lv_json_req_str        type string,
          lv_json_res_str        type string,
          lr_json_serializer     type ref to zcl_trex_json_serializer,
          lv_log_record_text     type string,
          c_newline              value cl_abap_char_utilities=>newline,
          lv_include_sap_descr   type char1,
          lv_include_alr_app_url type char1,
          wa_zoctobuslog         type zoctobuslog,
          lv_host                type loc_host,
          lv_port                type loc_port,
          lv_url                 type string.

    constants lc_alr_uri type string  value '/sap/bc/ui5_ui5/sap/alertdetail/index.html'.

    clear: ls_input_alert, mt_eventmetric.

    ls_input_alert-context_id = ip_alert-context_id.
    ls_input_alert-type_id = ip_alert-type_id.

    if ip_alert-ref is not initial.

      " -------------------------------------------------------------------------
      "       Fetching regular alert and metrics data
      " -------------------------------------------------------------------------

      ls_input_alert-start_date_utc = ip_alert-ref->get_start_date_utc( ).
      ls_input_alert-start_time_utc = ip_alert-ref->get_start_time_utc( ).
      ls_input_alert-end_date_utc = ip_alert-ref->get_end_date_utc( ).
      ls_input_alert-end_time_utc = ip_alert-ref->get_end_time_utc( ).
      ls_input_alert-mo_name = ip_alert-ref->get_managed_object_name( ).
      ls_input_alert-mo_type = ip_alert-ref->get_managed_object_type( ).
      ls_input_alert-name = ip_alert-ref->get_name( ).
      ls_input_alert-short_text = ip_alert-ref->get_short_text( ).
      ls_input_alert-category = ip_alert-ref->get_category( ).
      ls_input_alert-rating = ip_alert-ref->get_rating( ).
      ls_input_alert-tech_scenario = ip_alert-ref->get_technical_scenario( ).
      ls_input_alert-sap_description = ip_alert-ref->get_sap_description( ).
      ls_input_alert-custom_description = ip_alert-ref->get_custom_description( ).
      ls_input_alert-metric_path = ip_alert-ref->get_metric_path( ).
      ls_input_alert-customer_network = ip_alert-ref->get_customer_network( ).
      ls_input_alert-customer_display_name = ip_alert-ref->get_customer_display_name( ).
      ls_input_alert-data_center = ip_alert-ref->get_data_center( ).
      ls_input_alert-hostname = ip_alert-ref->get_hostname( ).
      ls_input_alert-priority = ip_alert-ref->get_priority( ).
      ls_input_alert-severity = ip_alert-ref->get_severity( ).
      ls_input_alert-tech_scenario = ip_alert-ref->get_technical_scenario( ).
      ls_input_alert-reason_for_closure = ip_alert-ref->get_reason_for_closure( ).

      ls_input_alert-type_id = ip_alert-ref->get_type_id( ).
      ls_input_alert-context_id = ip_alert-ref->get_managed_object_id( ).
      ls_input_alert-hash_metric_path = ip_alert-ref->get_hash_metric_path( ).

      ls_input_alert-type = ip_alert-ref->get_type( ).

      "Check if the alert has any children object

      if ip_alert-ref->has_children( ) eq abap_true.

        read table ip_alert-ref->get_children( ) into data(ls_event) index 1. "Alert always has only one event as its child

        if sy-subrc  = 0.

          "Extract the complete event/metric hierarchy of the incoming alert

          me->extract_evntmet_recursively( is_eventmetric = ls_event iv_parent_context_id = ip_alert-context_id
                                            iv_parent_type_id = ip_alert-type_id  iv_parent_type = ip_alert-ref->get_type( ) ).
        endif. " IF sy-subrc  = 0

      endif. " IF ip_alert-ref->has_children( ) EQ abap_true

      " -------------------------------------------------------------------------
      "       Preparing JSON object
      " -------------------------------------------------------------------------

      clear ls_json_req_obj.

      " Checking parameters table for fields to include

      select single value from zoctobusparam into lv_include_sap_descr
        where param = 'FIELDINCL_SAP_DESCR'.

      select single value from zoctobusparam into lv_include_alr_app_url
      where param = 'FIELDINCL_ALRT_URL'.

      ls_json_req_obj-alert_group_state = ip_alert_group_state.
      ls_json_req_obj-start_date_utc = ls_input_alert-start_date_utc.
      ls_json_req_obj-start_time_utc = ls_input_alert-start_time_utc.
      ls_json_req_obj-end_date_utc = ls_input_alert-end_date_utc.
      ls_json_req_obj-end_time_utc = ls_input_alert-end_time_utc.
      ls_json_req_obj-mo_name = ls_input_alert-mo_name.
      ls_json_req_obj-mo_type = ls_input_alert-mo_type.
      ls_json_req_obj-name = ls_input_alert-name.
      ls_json_req_obj-short_text = ls_input_alert-short_text.
      ls_json_req_obj-category = ls_input_alert-category.
      ls_json_req_obj-tech_scenario = ls_input_alert-tech_scenario.
      ls_json_req_obj-rating = ls_input_alert-rating.
      ls_json_req_obj-custom_description = ls_input_alert-custom_description.
      ls_json_req_obj-customer_network = ls_input_alert-customer_network.
      ls_json_req_obj-customer_display_name = ls_input_alert-customer_display_name.
      ls_json_req_obj-data_center = ls_input_alert-data_center.
      ls_json_req_obj-hostname =  ls_input_alert-hostname.
      ls_json_req_obj-priority = ls_input_alert-priority.
      ls_json_req_obj-severity = ls_input_alert-severity.
      ls_json_req_obj-tech_scenario = ls_input_alert-tech_scenario.
      ls_json_req_obj-reason_for_closure =  ls_input_alert-reason_for_closure.

      " Adding FRUN SID details

      ls_json_req_obj-frun_sid = sy-sysid.

      " Alerting application URL field block

      if  ( lv_include_alr_app_url = 'X' ).

        " Get host name: start searching HTTPS, if not found - take HTTP

        select single host port into ( lv_host, lv_port ) from httpurlloc where
          applicatn = '*' and protocol = 'HTTPS'.

        if ( lv_host is initial ) and ( lv_port is initial ).

          select single host port into ( lv_host, lv_port ) from httpurlloc where
            applicatn = '*' and protocol = 'HTTP'.

        endif. " if ( lv_host is initial ) and ( lv_port is initial )


        if ( lv_host is not initial ) and ( lv_port is not initial ).

          translate lv_host to lower case.

          concatenate 'https://' lv_host ':' lv_port into lv_url.

          concatenate lv_url lc_alr_uri into lv_url.

          concatenate lv_url '?ALERT_TYPE_ID=' ls_input_alert-type_id into lv_url.

          concatenate lv_url '&CONTEXT_ID=' ls_input_alert-context_id into lv_url.

          if ( ls_input_alert-hash_metric_path is not initial ).

            concatenate lv_url '&HASH_METRIC_PATH=' ls_input_alert-hash_metric_path into lv_url.

          endif. " if ( ls_input_alert-hash_metric_path is not initial )

          ls_json_req_obj-alr_app_url = lv_url.

        endif. " if ( lv_host is not initial ) and ( lv_port is not initial )

      endif. " if  ( lv_include_mon_app_url = 'X' )


      " SAP description field block

      if  ( lv_include_sap_descr = 'X' ).

        ls_json_req_obj-sap_description = ls_input_alert-sap_description.

        " Filtering out html tags from description

        replace all occurrences of '<h2>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</h2>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<strong>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</strong>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<p>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</p>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<b>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</b>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<u>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</u>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<i>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</i>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<ul>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</ul>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<li>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</li>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<a href="' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of regex '">[A-Za-z0-9_\~\-+=&[:space:]]*</a>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '\&lt;br /\&gt;' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '&lt;br /&gt;' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '\n' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<br>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '</br>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<br />' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<br / >' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<br/ >' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '<br/>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of regex 'target=".*"' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of regex '>.*</a>' in ls_json_req_obj-sap_description with '' ignoring case.
        replace all occurrences of '"' in ls_json_req_obj-sap_description with '' ignoring case.

      endif. " if  ( lv_include_sap_descr = 'X' )

      " Adding metrics data

      data wa_metrics_list type  ty_metrics_state.

      loop at mt_eventmetric assigning field-symbol(<mt_eventmetric>).

        if ( <mt_eventmetric>-type = 'METRIC' ).

          clear wa_metrics_list.

          wa_metrics_list-name = <mt_eventmetric>-name.
          wa_metrics_list-rating = <mt_eventmetric>-rating.
          wa_metrics_list-value = <mt_eventmetric>-value.
          wa_metrics_list-metric_path = <mt_eventmetric>-metric_path.


          append wa_metrics_list to ls_json_req_obj-metrics_list.

        endif. "  if ( <mt_eventmetric>-type = 'M' )

      endloop. "loop at mt_eventmetric assigning field-symbol(<mt_eventmetric>)

      " Executing serialization

      create object lr_json_serializer
        exporting
          data = ls_json_req_obj.

      lr_json_serializer->serialize( ).

      lv_json_req_str = lr_json_serializer->get_data( ).

      lv_log_record_text = 'Starting payload push to Octobus...'.

      log_octobus_comm_record(
               ip_msgty  = 'I'
               ip_log_record_text = lv_log_record_text ).

      " Executing push to Octobus

      push_to_octobus(
        exporting
         ip_destination = 'ZOCTOBUS'
         ip_json_request = lv_json_req_str
        importing
         ep_json_response = lv_json_res_str ).

      if sy-subrc = 0.

        lv_log_record_text = 'Push to Octobus executed successfully'.

        log_octobus_comm_record(
         ip_msgty  = 'I'
         ip_log_record_text = lv_log_record_text ).

        " Creating a dedicated record in ZOCTOBUSLOG

        wa_zoctobuslog-event_date = sy-datum.
        wa_zoctobuslog-event_time = sy-uzeit.
        wa_zoctobuslog-context_id = ls_input_alert-context_id.
        wa_zoctobuslog-type_id = ls_input_alert-type_id.
        wa_zoctobuslog-alert_group_state = ip_alert_group_state.
        wa_zoctobuslog-network = ls_input_alert-customer_network.
        wa_zoctobuslog-data_center = ls_input_alert-data_center.
        wa_zoctobuslog-context_name = ls_input_alert-mo_name.
        wa_zoctobuslog-alert_name = ls_input_alert-name.
        wa_zoctobuslog-payload = lv_json_req_str.

        insert zoctobuslog from wa_zoctobuslog.

      else.

        lv_log_record_text = 'Push to Octobus failed'.

        log_octobus_comm_record(
         ip_msgty  = 'E'
         ip_log_record_text = lv_log_record_text ).

      endif.



    endif. " IF ip_alert-ref IS NOT INITIAL


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SEND_TO_OCTOBUS_REST=>PUSH_TO_OCTOBUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_DESTINATION                 TYPE        RFCDEST
* | [--->] IP_PARAMLINE                   TYPE        CHAR1024(optional)
* | [--->] IP_JSON_REQUEST                TYPE        STRING
* | [<---] EP_JSON_RESPONSE               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method push_to_octobus.

    data: lo_http_client     type ref to if_http_client,
          lo_rest_client     type ref to cl_rest_http_client,
          lv_url             type        string,
          lv_body            type        string,
          token              type        string,
          agreements         type        string,
          lo_response        type ref to     if_rest_entity,
          lo_request         type ref to if_rest_entity,
          lv_log_record_text type string.


    concatenate 'Sending payload to Octobus:' ip_json_request  into lv_log_record_text separated by space.

    log_octobus_comm_record(
             ip_msgty  = 'I'
             ip_log_record_text = lv_log_record_text ).

    cl_http_client=>create_by_destination(
     exporting
       destination              = ip_destination    " Logical destination (specified in function call)
     importing
       client                   = lo_http_client    " HTTP Client Abstraction
     exceptions
       argument_not_found       = 1
       destination_not_found    = 2
       destination_no_authority = 3
       plugin_not_active        = 4
       internal_error           = 5
       others                   = 6
    ).

    if ( sy-subrc <> 0 ).

      case sy-subrc.
        when '1'.
          lv_log_record_text = 'argument_not_found'.
        when '2'.
          lv_log_record_text = 'destination_not_found'.
        when '3'.
          lv_log_record_text = 'destination_no_authority'.
        when '4'.
          lv_log_record_text = 'plugin_not_active'.
        when '5'.
          lv_log_record_text = 'internal_error'.
        when others.
          lv_log_record_text ='not_known_exception'.

      endcase.

      concatenate 'HTTPS destination error:' lv_log_record_text  into lv_log_record_text separated by space.

      log_octobus_comm_record(
             ip_msgty  = 'E'
             ip_log_record_text = lv_log_record_text ).

      ep_json_response = ''.

      exit.

    endif. " IF ( sy_subrc <> 0 )


* Create REST client instance

    create object lo_rest_client
      exporting
        io_http_client = lo_http_client.

* Set HTTP version

    lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

    if lo_http_client is bound and lo_rest_client is bound.

      " Adding parameters to call

      if ip_paramline is not initial.

        concatenate '?' ip_paramline into lv_url.

      endif. " if ip_paramline is not initial

      " Set the URI

      cl_http_utility=>set_request_uri(
          exporting
            request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
            uri     = lv_url                     " URI String (in the Form of /path?query-string)
        ).


      " Set payload in JSON
      lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
      lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_request->set_string_data( ip_json_request ).



      " Set request header if any
      call method lo_rest_client->if_rest_client~set_request_header
        exporting
          iv_name  = 'auth-token'
          iv_value = token.

      " Executing HTTP POST

      try.

          lo_rest_client->if_rest_client~post( lo_request ).

        catch cx_rest_client_exception into data(lo_rest_client_error) .

          lv_log_record_text = lo_rest_client_error->get_text( ).

          log_octobus_comm_record(
              ip_msgty  = 'E'
              ip_log_record_text = lv_log_record_text ).

          ep_json_response = ''.

          exit.

      endtry.

      " HTTP response

      lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

      " HTTP return status

      data(http_status)   = lo_response->get_header_field( '~status_code' ).

    endif. " IF lo_http_client IS BOUND AND lo_rest_client IS BOUND

    " Filling response

    ep_json_response = lo_response->get_string_data( ).

  endmethod.
ENDCLASS.
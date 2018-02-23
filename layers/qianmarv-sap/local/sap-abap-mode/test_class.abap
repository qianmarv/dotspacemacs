*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_rfd_bunit_agent IMPLEMENTATION.
  METHOD constructor.
    " Set Default Parameter
    lv_test = 888.
    mv_is_sign_adjust = abap_false. " Always False

    DATA(ls_rfd_header) = lcl_utility=>get_instance( )->get_rfd_header( ).
    mv_update_mode    = ls_rfd_header-updmode.

    IF ls_rfd_header-datatype EQ if_fincs_api_rfd_types=>enum_data_type_periodic.
      mv_is_data_periodic = abap_true.
    ENDIF.

    APPEND LINES OF VALUE fc00_t_fields(
     ( fieldname = 'HSL'
       datafield = 'HSL' )
     ( fieldname = 'TSL'
       datafield = 'TSL' )
     ( fieldname = 'KSL'
       datafield = 'KSL' )
     ( fieldname = 'MSL'
       datafield = 'MSL' )
    ) TO mt_kfig_data.
  ENDMETHOD.
  METHOD before_run.

    DATA lv_dummy_msg TYPE char255.
    DATA lo_util TYPE REF TO lcl_utility.
    lo_util = lcl_utility=>get_instance( ).
**********************************************************************
**$ Check RowNumber, Duplicate Row Number Is Not Allowed
**********************************************************************
    TYPES: _ty_n6 TYPE n LENGTH 6.
    TYPES: BEGIN OF _ty_s_row_number,
             ser_num TYPE _ty_n6,
           END OF _ty_s_row_number,
           _ty_t_row_number TYPE STANDARD TABLE OF _ty_s_row_number WITH DEFAULT KEY.
    DATA lt_row_numbers TYPE _ty_t_row_number.
    MOVE-CORRESPONDING mt_rfd_data TO lt_row_numbers.
    DATA(lv_totals) = lines( mt_rfd_data ).
    SORT lt_row_numbers BY ser_num.
    DELETE ADJACENT DUPLICATES FROM lt_row_numbers .
    DATA(lv_totals_wo_dup) = lines( lt_row_numbers ).

    IF lv_totals GT lv_totals_wo_dup.
      MESSAGE e000(fin_cs_api) INTO lv_dummy_msg.
      APPEND VALUE #(
        msgid = sy-msgid
        msgty = sy-msgty
        msgno = sy-msgno
      ) TO rt_messages.
      RETURN.
    ENDIF.

**********************************************************************
**$ Derive Data
    " Rule 1: If Local Currency = Group Currency.
    "         Then Local Amount == Group Amount
    "         Thus If User Didn't Maintain Group Amount,
    "         Need to Assign Automatically
**********************************************************************

    DATA(lv_gcurr) = lo_util->get_ledger_currency( lo_util->get_rfd_header( )-rldnr ).
    DATA(lv_lcurr) = lo_util->get_local_currency(
                       iv_bunit = mv_bunit
                       iv_ryear  = lo_util->get_rfd_header( )-ryear
                     ).

    LOOP AT mt_rfd_data ASSIGNING FIELD-SYMBOL(<rfd_data>).
      IF <rfd_data>-rhcur IS INITIAL.
        <rfd_data>-rhcur = lv_lcurr.
      ENDIF.

      IF <rfd_data>-rgcur IS INITIAL.
        <rfd_data>-rgcur = lv_gcurr.
      ENDIF.

      IF  <rfd_data>-ksl   IS INITIAL AND
          <rfd_data>-rhcur EQ <rfd_data>-rgcur.
        <rfd_data>-ksl   = <rfd_data>-hsl.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_fincs_api_rfd_agent~run.

    DATA lt_message TYPE if_fincs_api_rfd_types=>ty_t_fc05_message.

    lt_message = before_run( ).

    APPEND LINES OF lt_message TO rt_message.
    READ TABLE lt_message TRANSPORTING NO FIELDS WITH KEY msgty = 'E'.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.
    CLEAR lt_message.

    IF io_core_service IS BOUND.
      lt_message = io_core_service->execute(
          iv_update_mode      = mv_update_mode
          iv_is_data_periodic = mv_is_data_periodic
          iv_is_test          = iv_is_test
          it_rfd_data         = mt_rfd_data
      ).
    ELSE.
      DATA lt_doc_num TYPE fc05_t_docnr.
      DATA(lv_log_number) = lcl_task_log_helper=>get_instance( )->get_log_number( ).
      PERFORM api_upload IN PROGRAM ficupl90 IF FOUND
                                USING mv_update_mode
                                      mv_is_sign_adjust
                                      mv_is_data_periodic
                                      iv_is_test
                                      mt_rfd_data
                                      mt_kfig_data
                                      lv_log_number
                                 CHANGING
                                      lt_message
                                      lt_doc_num
                                      mt_rfd_data.
      APPEND LINES OF lt_doc_num TO mt_doc_num.
    ENDIF.
    APPEND LINES OF lt_message TO rt_message.
  ENDMETHOD.

  METHOD if_fincs_api_rfd_agent~collect_rfd_for_bunit.
    mv_bunit          = iv_bunit.

    if_fincs_api_rfd_agent~set_bus_doc_msg_hdr( is_doc_msg_hdr ).
    APPEND LINES OF it_rfd_data TO mt_rfd_data.

  ENDMETHOD.

  METHOD if_fincs_api_rfd_agent~set_bus_doc_msg_hdr.
    ms_msg_hdr = is_msg_hdr.
  ENDMETHOD.

  METHOD if_fincs_api_rfd_agent~get_doc_num.
    rt_doc_num = mt_doc_num.
  ENDMETHOD.

  METHOD if_fincs_api_rfd_agent~get_bus_doc_msg_hdr.
    rs_msg_hdr = ms_msg_hdr.
  ENDMETHOD.


ENDCLASS.

**********************************************************************
**
**********************************************************************
CLASS lcl_task_log_helper IMPLEMENTATION.
  METHOD add_cons_unit.
    APPEND iv_unit TO mt_cons_units.
  ENDMETHOD.


  METHOD get_log_number.
    DATA(lo_util) = lcl_utility=>get_instance( ).
    DATA(ls_rfd_header) = lo_util->get_rfd_header( ).
    DATA lv_external_id TYPE fincs_extid.
    lv_external_id = ls_rfd_header-extid.

    DATA lv_is_cumulative TYPE abap_bool.
    IF ls_rfd_header-datatype EQ if_fincs_api_rfd_types=>enum_data_type_cumulative.
      lv_is_cumulative = abap_true.
    ENDIF.

    DATA(ls_log_header) = VALUE fincs_s_log_header(
     cnsldtntasktype           = if_fincs_api_rfd_constants=>default_task_category
     cnsldtndimension          = ls_rfd_header-rdimen
*        cnsldtngroup              =
     cnsldtnledger             = ls_rfd_header-rldnr
     cnsldtnversion            = ls_rfd_header-rvers
     fiscalyear                = ls_rfd_header-ryear
     fiscalperiod              = ls_rfd_header-poper
     cnsldtntask               = lo_util->get_cacti( )
*        cnsldtnmethod             =
     cnsldtndocumenttype       = lo_util->get_docty( )
     cnsldtnchartofaccounts    = ls_rfd_header-ritclg
*        cnsldtngroupcurrency      =
*        iscnsldtntestrun          =
*        userid                    =
*        cnsldtnlogdate            =
*        cnsldtnlogtime            =
*        deletion                  =
*        status                    =
     externalid                = lv_external_id
*        cnsldtnunit               =
     consolidationlogtype      = if_fincs_generic_log_handler=>gc_log_type-reported_financial_data_api
     update_mode               = ls_rfd_header-updmode
     is_cumulative             = lv_is_cumulative
     consolidationlogitemstruc = if_fincs_generic_log_handler=>gc_item_struc_name-je_api
 ).

    IF mv_log_number IS INITIAL.
      mv_log_number  = cl_fincs_generic_log=>create_log_handler(
                          EXPORTING
                            is_log_header         =  ls_log_header                " Structure for Consolidation Log Header
                            it_consolidation_unit =  mt_cons_units                " Table type for unit
*                          IMPORTING
*                            eo_log_handler        =  mo_generic_log               " Generic Log API
                        ).
    ENDIF.
    rv_log_number = mv_log_number.
  ENDMETHOD.

  METHOD get_instance.
    IF NOT go_me IS BOUND.
      go_me = NEW lcl_task_log_helper(
          iv_log_number = iv_log_number
      ).
    ENDIF.
    ro = go_me.
  ENDMETHOD.

  METHOD constructor.
    mv_log_number = iv_log_number.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_utility IMPLEMENTATION.
  METHOD get_instance.
    IF NOT is_rfd_header IS INITIAL.
      go_self = NEW lcl_utility( is_rfd_header ).
    ENDIF.
    ro = go_self.
  ENDMETHOD.

  METHOD get_docty.
    SELECT SINGLE docty INTO @rv_docty
    FROM tf500
    WHERE dimen = @ms_rfd_header-rdimen AND
          rlevl = @if_fincs_api_rfd_constants=>default_posting_level AND
          cacta = @if_fincs_api_rfd_constants=>default_biz_appl.
  ENDMETHOD.
  METHOD get_cacti.
    CALL FUNCTION 'FC_GET_CACTI'
      EXPORTING
        e_dimen               = ms_rfd_header-rdimen
        e_itclg               = ms_rfd_header-ritclg
        e_rvers               = ms_rfd_header-rvers
        e_ryear               = ms_rfd_header-ryear
        e_perid               = ms_rfd_header-poper
        e_rldnr               = ms_rfd_header-rldnr
        e_cactt               = if_fincs_api_rfd_constants=>default_task_category
*       E_DOCTY               =
        e_monitor_flag        = ''
      IMPORTING
        i_cacti               = rv_cacti
*       I_REFCACTI            =
*       I_CACGR               =
*       I_CACGR1              =
*       I_CACGR2              =
      EXCEPTIONS
        cacti_in_global_cacgr = 1
        cacti_not_in_cacgr    = 2
        docty_not_assigned    = 3
        cacgr_not_assigned    = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
  METHOD get_local_currency.
    DATA lt_tf164 TYPE STANDARD TABLE OF tf164 WITH DEFAULT KEY.
    DATA(lv_dimen) = ms_rfd_header-rdimen.
    DATA lv_ryear_rev TYPE fc_fryear.
    lv_ryear_rev = 9999 - iv_ryear.

    SELECT * FROM tf164 INTO TABLE @lt_tf164
    WHERE dimen =  @lv_dimen AND
          bunit =  @iv_bunit AND
          ryear >= @lv_ryear_rev
    ORDER BY ryear ASCENDING.                           "#EC CI_BYPASS.

    IF lines( lt_tf164 ) GE 1.
      rv_lcurr = lt_tf164[ 1 ]-curr.
    ENDIF.

  ENDMETHOD.

  METHOD get_ledger_currency.
    CALL FUNCTION 'FC_GET_CURR_FROM_RLDNR'
      EXPORTING
        e_rldnr = iv_rldnr
      IMPORTING
        i_gcurr = rv_gcurr
      EXCEPTIONS
        OTHERS  = 0.
  ENDMETHOD.

  METHOD constructor.
    ms_rfd_header = is_rfd_header.
  ENDMETHOD.

  METHOD get_rfd_header.
    rs_rfd_header = ms_rfd_header.
  ENDMETHOD.
ENDCLASS.
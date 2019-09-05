class ZCL_COMMON_FM definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_seltab,
             fieldname TYPE fieldname,
             sign      TYPE ddsign,
             option    TYPE ddoption,
             low       TYPE STRING,
             high      TYPE STRING,
           END OF ty_seltab .
  types:
    tt_seltab  TYPE TABLE OF ty_seltab .

  class-methods F4_FILE
    returning
      value(R_FILEPATH) type STRING .
  class-methods DOWNLOAD_EXCEL
    importing
      !I_ID type W3OBJID .
  class-methods CHECK_EMPTY_FIELD
    importing
      !I_FIELD type ANY
    returning
      value(R_MSG) type BAPI_MSG .
  class-methods BUILD_SM30_TOTAL
    importing
      !I_TOTAL type ANY TABLE
      !I_TABNAME type TABNAME
    returning
      value(E_TOTAL) type ref to DATA .
  class-methods BUILD_SYS_MESSAGE
    importing
      !I_LANGU type SPRAS default '1'
    returning
      value(R_MESSAGE) type STRING .
  class-methods BUILD_MDG_MESSAGE
    importing
      !I_LANGU type LANGU default '1'
      !IS_MDG_MESSAGE type USMD_S_MESSAGE
    returning
      value(R_MESSAGE) type STRING .
  class-methods GET_SNUM
    importing
      !I_OBJECT type INRI-OBJECT
      !I_RANGE type INRI-NRRANGENR default '01'
    changing
      value(R_NUMBER) type ANY .
  class-methods GET_USMD_PHSY_TABLE
    importing
      !I_MODEL type USMD_MODEL
      !I_ENTITY type USMD_ENTITY optional
    exporting
      !E_TABLE type TABNAME
      !E_SUB_TABLE type TABNAME .
  class-methods CHECK_FIELD_UNIQUE
    importing
      !I_MODEL type USMD_MODEL
      !I_MODEL_FIELD type FIELDNAME optional
      !I_CHECK_FIELD type ANY
    exporting
      value(R_MDG_ID) type ANY .
  class-methods FIELD_GET_NAME
    importing
      !I_VALUE type ANY
    exporting
      !E_STRUCTURE type TABNAME
      !E_FIELD type FIELDNAME .
  class-methods GET_MODEL_SNUM
    importing
      !I_MODEL type USMD_MODEL
      !I_NRRANGENR type INRI-NRRANGENR default '01'
    exporting
      !E_SNUM type ANY .
  class-methods GET_SERVER_GROUP
    returning
      value(R_CLASS) type RZLLI_APCL .
  class-methods CREATE_DYNMIC_WHERE
    importing
      !I_TABLE type TABNAME
      !IT_SELOPT type RSDS_FRANGE_T
    exporting
      !E_WHERE type STRING .
  class-methods GET_MODEL_TEMP_KEY
    importing
      !I_MODEL type USMD_MODEL
      !I_NRRANGENR type INRI-NRRANGENR default '01'
    exporting
      !E_SNUM type ANY .
  class-methods WHERE_CLAUSE_2_STRING
    importing
      !IT_WHERE type RSDS_TWHERE
      !I_TABLE_FIELD type CHAR1 default ABAP_FALSE
    exporting
      !E_WHERE type STRING .
  class-methods CORRESPOND_KEEP_OLD
    importing
      !I_SRC type ANY
    changing
      !C_DEST type ANY .
  class-methods GET_GUID
    returning
      value(R_GUID) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_COMMON_FM IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>BUILD_MDG_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LANGU                        TYPE        LANGU (default ='1')
* | [--->] IS_MDG_MESSAGE                 TYPE        USMD_S_MESSAGE
* | [<-()] R_MESSAGE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method build_mdg_message.

    call function 'MESSAGE_PREPARE'
      exporting
        language               = i_langu
        msg_id                 = conv arbgb( is_mdg_message-msgid )
        msg_no                 = conv MSGNR( is_mdg_message-msgno )
        msg_var1               = is_mdg_message-msgv1
        msg_var2               = is_mdg_message-msgv2
        msg_var3               = is_mdg_message-msgv3
        msg_var4               = is_mdg_message-msgv4
      importing
        msg_text               = r_message
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.
    if sy-subrc <> 0.
* Implement suitable error handling here
      clear r_message.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>BUILD_SM30_TOTAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TOTAL                        TYPE        ANY TABLE
* | [--->] I_TABNAME                      TYPE        TABNAME
* | [<-()] E_TOTAL                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method build_sm30_total.
    data:lr_table type ref to data.

    "获取数据库表对应的结构
    data(lo_struct) = cast cl_abap_structdescr( cl_abap_tabledescr=>describe_by_name( i_tabname ) ).
    data(lt_comp) = lo_struct->get_components( ).

    "添加action && mark字段
    data(lo_element) = cast cl_abap_datadescr( cl_abap_elemdescr=>describe_by_name( 'CHAR01') ).
    lt_comp = value #( base lt_comp ( name = 'ACTION' type = lo_element ) ( name = 'MARK' type = lo_element ) ).
    lo_struct = cl_abap_structdescr=>create( p_components = lt_comp ).
    data(lo_table) = cl_abap_tabledescr=>create( p_line_type = lo_struct ).

    create data lr_table type handle lo_table.
    assign lr_table->* to field-symbol(<ft_table>).

    <ft_table> = i_total.

    e_total = lr_table.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>BUILD_SYS_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LANGU                        TYPE        SPRAS (default ='1')
* | [<-()] R_MESSAGE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD build_sys_message.


    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = i_langu
        msg_id                 = sy-msgid
        msg_no                 = CONV msgnr( sy-msgno )
        msg_var1               = sy-msgv1
        msg_var2               = sy-msgv2
        msg_var3               = sy-msgv3
        msg_var4               = sy-msgv4
      IMPORTING
        msg_text               = r_message
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      CLEAR r_message.
    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>CHECK_EMPTY_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FIELD                        TYPE        ANY
* | [<-()] R_MSG                          TYPE        BAPI_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method check_empty_field.

  CHECK i_field IS INITIAL.
  DATA(lo_element_descr) = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( i_field ) ).
  DATA(ls_element) = lo_element_descr->get_ddic_field( ).

  MESSAGE s012(zmdg) WITH ls_element-fieldtext INTO DATA(lv_msg)."&1 是必输字段

  r_msg = r_msg && '/' && lv_msg.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>CHECK_FIELD_UNIQUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MODEL                        TYPE        USMD_MODEL
* | [--->] I_MODEL_FIELD                  TYPE        FIELDNAME(optional)
* | [--->] I_CHECK_FIELD                  TYPE        ANY
* | [<---] R_MDG_ID                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_field_unique.

    DATA:lv_conditon     TYPE string,
         lt_dynmic_sel   TYPE zmdg1_frange_t,
         ls_dynmic_sel   TYPE zmdg1_frange,
         lt_where_clause TYPE zmdg1_where_tab,
         lv_exist,
         lr_record       TYPE REF TO data.

    "RTTS
    DATA(lo_element_descr) = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( i_check_field ) ).
    DATA(ls_element) = lo_element_descr->get_ddic_field( ).

    "获取物理表
    get_usmd_phsy_table(
    EXPORTING
      i_model = i_model
    IMPORTING
    e_table = DATA(lv_master_table)
          ).


    CREATE DATA lr_record TYPE (lv_master_table).
    ASSIGN lr_record->* TO FIELD-SYMBOL(<fs_record>).

    "拼接动态查询条件
    DATA(r_field) = VALUE rsds_selopt_t( ( sign = 'I' option = 'EQ' low = i_check_field high = space  ) ).
    CLEAR ls_dynmic_sel.
    IF i_model_field IS SUPPLIED.
      ls_dynmic_sel-fieldname = ls_element-tabname.
    ELSE.
      ls_dynmic_sel-fieldname = i_model_field.
    ENDIF.

    "MDG字段特殊性
    IF ls_dynmic_sel-fieldname(2) EQ  'ZZ'.
      ls_dynmic_sel-fieldname = '/1MD/' && i_model && ls_dynmic_sel-fieldname.
    ENDIF.

    IF strlen( ls_dynmic_sel-fieldname ) GT 16.
      ls_dynmic_sel-fieldname = ls_dynmic_sel-fieldname(16).
    endif.

      ls_dynmic_sel-selopt_t  = r_field[].
      APPEND ls_dynmic_sel TO lt_dynmic_sel.

      CALL FUNCTION 'ZMDG_SELECT_OPTIONS_2_WHERE'
        EXPORTING
          i_tablename          = lv_master_table
          i_tab_select_options = lt_dynmic_sel
        IMPORTING
          e_tab_where_clause   = lt_where_clause
        EXCEPTIONS
          error                = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        LOOP AT lt_where_clause INTO DATA(ls_where_clause).
          CONCATENATE lv_conditon ls_where_clause-line INTO lv_conditon SEPARATED BY space.
        ENDLOOP.
      ENDIF.

      "查询物理表
      SELECT SINGLE *
      FROM (lv_master_table)
      WHERE (lv_conditon)
      INTO @<fs_record>.

      CLEAR r_mdg_id.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 2 OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_key_field>).
        IF <fs_key_field> IS ASSIGNED.
          r_mdg_id = <fs_key_field>.
        ENDIF.
      ENDIF.

    ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>CORRESPOND_KEEP_OLD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_SRC                          TYPE        ANY
* | [<-->] C_DEST                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD correspond_keep_old.

    DATA(lv_src_kind) = cl_abap_datadescr=>get_data_type_kind( i_src ).
    DATA(lv_dest_kind) = cl_abap_datadescr=>get_data_type_kind( c_dest ).

    IF lv_src_kind NE lv_dest_kind.
      "      MOVE-CORRESPONDING: i_src TO c_dest.
      RETURN.
    ENDIF.

    IF lv_src_kind EQ cl_abap_datadescr=>typekind_struct1."扁平结构
      DATA(lo_src_struct) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( i_src ) ).
      DATA(lt_src_field) = lo_src_struct->get_components( ).
      LOOP AT lt_src_field INTO DATA(ls_src_field).
        ASSIGN COMPONENT ls_src_field-name OF STRUCTURE c_dest TO FIELD-SYMBOL(<fs_dest_field>).
        IF sy-subrc EQ 0 AND  <fs_dest_field> IS INITIAL.
          ASSIGN COMPONENT ls_src_field-name OF STRUCTURE i_src TO FIELD-SYMBOL(<fs_src_field>).
          <fs_dest_field> = <fs_src_field>.
        ENDIF.

      ENDLOOP.

    ELSEIF lv_src_kind EQ cl_abap_datadescr=>typekind_table."内表---等待实现

    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>CREATE_DYNMIC_WHERE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TABLE                        TYPE        TABNAME
* | [--->] IT_SELOPT                      TYPE        RSDS_FRANGE_T
* | [<---] E_WHERE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_dynmic_where.
    DATA:lt_where TYPE rsds_where_tab.

    CALL FUNCTION 'FVD_SELECT_OPTIONS_2_WHERE'
      EXPORTING
        i_tablename          = i_table
        i_tab_select_options = it_selopt
      IMPORTING
        e_tab_where_clause   = lt_where
      EXCEPTIONS
        error                = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      RETURN.
    ENDIF.


    LOOP AT lt_where INTO DATA(ls_where).
      CONCATENATE e_where ls_where INTO e_where SEPARATED BY space.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>DOWNLOAD_EXCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ID                           TYPE        W3OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method download_excel.
    data:lv_folder      type string,
         lv_rc          like sy-subrc,
         ls_object_key  type wwwdatatab,
         lv_destination type rlgrap-filename.

    "校验存在性
    select single relid,
                  objid,
                  text
      from wwwdata
      into @data(ls_objdata)
      where srtf2 eq 0
      and relid eq 'MI'
      and objid eq @i_id.

    if sy-subrc ne 0.
      message '模板对象不存在' type 'E'.
    endif.

    "获取保存路径
    cl_gui_frontend_services=>directory_browse(
    changing selected_folder = lv_folder
       ).


    "下载模板
    check lv_folder is not initial.

    "获取模板后缀
    select single value
      from wwwparams
      where relid eq 'MI'
      and objid eq @i_id
      and name eq 'fileextension'
      into @data(lv_extension).

    lv_destination = lv_folder && '\' && ls_objdata-text && '.' && lv_extension.
    ls_object_key-relid = ls_objdata-relid.
    ls_object_key-objid = ls_objdata-objid.
    call function 'DOWNLOAD_WEB_OBJECT'
      exporting
        key         = ls_object_key
        destination = lv_destination
      importing
        rc          = lv_rc.
    if lv_rc ne 0.
      message '模板下载失败!' type 'S' display like 'E'.
      leave list-processing.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>F4_FILE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_FILEPATH                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method F4_FILE.
    data:lv_filepath type IBIPPARMS-PATH.


    call function 'F4_FILENAME'
*     EXPORTING
*       PROGRAM_NAME        = SYST-CPROG
*       DYNPRO_NUMBER       = SYST-DYNNR
*       FIELD_NAME          = ' '
      IMPORTING
        FILE_NAME           = lv_filepath
              .


    r_filepath = lv_filepath.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>FIELD_GET_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VALUE                        TYPE        ANY
* | [<---] E_STRUCTURE                    TYPE        TABNAME
* | [<---] E_FIELD                        TYPE        FIELDNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD field_get_name.
    DATA:ls_field_descr TYPE sydes_desc,
         lv_name_string TYPE string.

    DESCRIBE FIELD 	i_value INTO ls_field_descr.

    READ TABLE ls_field_descr-names TRANSPORTING NO FIELDS WITH KEY continue = '*'.
    DATA(lv_index) = sy-tabix.

    LOOP AT ls_field_descr-names INTO DATA(ls_name) FROM lv_index.
      lv_name_string = lv_name_string && ls_name-name.
    ENDLOOP.

    SPLIT lv_name_string AT'-' INTO e_structure e_field.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>GET_GUID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_GUID                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_guid.

    data:lv_guid type  SYSUUID_X16.

    TRY.
        lv_guid =  cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        clear lv_guid.
    ENDTRY.

    r_guid = lv_guid.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>GET_MODEL_SNUM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MODEL                        TYPE        USMD_MODEL
* | [--->] I_NRRANGENR                    TYPE        INRI-NRRANGENR (default ='01')
* | [<---] E_SNUM                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_model_snum.

    DATA:lv_sobject TYPE inri-object.

    CASE i_model.
      WHEN 'ZE'.
        lv_sobject = 'ZMDG_ZE'.
      WHEN 'ZB'.
        lv_sobject = 'ZMDG_ZB'.
      WHEN 'ZM'.
        lv_sobject = 'ZMDG_ZM'.
      WHEN 'ZX'.
        lv_sobject = 'ZMDG_ZX'.
      WHEN 'ZI'.
        lv_sobject = 'ZMDG_ZI'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    get_snum(
    EXPORTING
      i_object = lv_sobject
    CHANGING
      r_number = e_snum
    ).

    SHIFT e_snum LEFT DELETING LEADING '0'.
    CASE i_model.
      WHEN 'ZX'."项目的特殊规则
        CONDENSE e_snum.
        e_snum =  'PJ' && sy-datum(4) && e_snum.
      WHEN OTHERS.
    ENDCASE.

   CONDENSE e_snum.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>GET_MODEL_TEMP_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MODEL                        TYPE        USMD_MODEL
* | [--->] I_NRRANGENR                    TYPE        INRI-NRRANGENR (default ='01')
* | [<---] E_SNUM                         TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_MODEL_TEMP_KEY.

    DATA:lv_sobject TYPE inri-object.

"    lv_sobject = 'ZMDG_' && i_model && '_T'.

    CASE i_model.
      WHEN 'ZE'.
        lv_sobject = 'ZMDG_ZE_T'.
      WHEN 'ZB'.
        lv_sobject = 'ZMDG_ZB_T'.
      WHEN 'ZM'.
        lv_sobject = 'ZMDG_ZM_T'.
      WHEN 'ZX'.
        lv_sobject = 'ZMDG_ZX_T'.
      WHEN 'ZI'.
        lv_sobject = 'ZMDG_ZI_T'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    get_snum(
    EXPORTING
      i_object = lv_sobject
    CHANGING
      r_number = e_snum
    ).

    SHIFT e_snum LEFT DELETING LEADING '0'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>GET_SERVER_GROUP
* +-------------------------------------------------------------------------------------------------+
* | [<-()] R_CLASS                        TYPE        RZLLI_APCL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_server_group.

    DATA:l_classname  TYPE rzlli_apcl,   "Server Group Name
         l_applserver TYPE rzllitab-applserver. "RFC Serve Group

    "一般系统默认g_classname = 'parallel_generators'，但为了通用性按照如下方法获取
    CALL 'C_SAPGPARAM'                                    "#EC CI_CCALL
    ID 'NAME'  FIELD 'rdisp/myname'
    ID 'VALUE'  FIELD l_applserver.

    SELECT SINGLE classname
    FROM   rzllitab
    INTO     l_classname   "Server Group Name
    WHERE   applserver = l_applserver
    AND  grouptype = 'S'.   "S:服务器组，空:登陆组

     r_class = l_classname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>GET_SNUM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT                       TYPE        INRI-OBJECT
* | [--->] I_RANGE                        TYPE        INRI-NRRANGENR (default ='01')
* | [<-->] R_NUMBER                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_snum.
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = i_object
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      RETURN.
    ELSE.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = i_range
          object                  = i_object
          quantity                = '1'
  "       subobject               = ' '
  "       toyear                  = '0000'
  "       ignore_buffer           = ' '
        IMPORTING
          number                  = r_number
  "       quantity                =
 "        returncode              =
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
        EXPORTING
          object           = i_object
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>GET_USMD_PHSY_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MODEL                        TYPE        USMD_MODEL
* | [--->] I_ENTITY                       TYPE        USMD_ENTITY(optional)
* | [<---] E_TABLE                        TYPE        TABNAME
* | [<---] E_SUB_TABLE                    TYPE        TABNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_usmd_phsy_table.

    DATA:ls_log_phys_name     TYPE if_usmd_model_gen_adapter=>s_log_phys_name,
         lo_model_gen_adapter TYPE REF TO if_usmd_model_gen_adapter,
         lt_log_phys_name     TYPE if_usmd_model_gen_adapter=>t_log_phys_name,
         lt_message           TYPE usmd_t_message.

    cl_usmd_adapter_provider=>get_model_generation_adapter(
    EXPORTING
      i_usmd_model         = i_model

    IMPORTING
      eo_model_gen_adapter = lo_model_gen_adapter
      et_message           = lt_message
      ).

    IF line_exists( lt_message[ msgty = 'E' ] ).
      RETURN.
    ENDIF.

    lo_model_gen_adapter->get_generated_objects(
    IMPORTING
      et_message         = lt_message
      et_log_phys_name   = lt_log_phys_name
      ).

    IF line_exists( lt_message[ msgty = 'E' ] ).
      RETURN.
    ENDIF.


    IF i_entity IS NOT SUPPLIED OR i_entity IS INITIAL."返回所有物理表

      LOOP AT lt_log_phys_name INTO DATA(ls_logic_table) WHERE kind EQ 'TABL'.
        IF ls_logic_table-sub_kind EQ 'C'.
          e_table = ls_logic_table-phys_name.
          CONTINUE.
        ELSEIF ls_logic_table-entity_cont IS NOT INITIAL.
          e_sub_table = ls_logic_table-phys_name.
        ENDIF.
      ENDLOOP.

    ELSE."返回指定实体的物理表
      LOOP AT lt_log_phys_name INTO ls_logic_table WHERE kind EQ 'TABL' AND entity EQ i_entity.
        e_table = ls_logic_table-phys_name.
        EXIT.
      ENDLOOP.
    ENDIF.



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMON_FM=>WHERE_CLAUSE_2_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_WHERE                       TYPE        RSDS_TWHERE
* | [--->] I_TABLE_FIELD                  TYPE        CHAR1 (default =ABAP_FALSE)
* | [<---] E_WHERE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD where_clause_2_string.

    DATA: lt_rsds_trange  TYPE rsds_trange,
          ls_rsds_trange  TYPE rsds_range,
          lt_frange       TYPE rsds_frange_t,
          ls_frange       TYPE rsds_frange,
          lt_where_clause TYPE rsds_twhere,
          ls_where_clause TYPE rsds_where,
          lt_where_tab    TYPE rsds_where_tab,
          ls_where_tab    TYPE rsdswhere,
          lv_sql_where    TYPE string.
    DATA: ls_where_tab_line TYPE string.

* Concatenate substrings (SQL-like syntax) into SQL-like string CORRECTING THE MISSING INDENTATION!
    LOOP AT it_where INTO ls_where_clause.
      lt_where_tab = ls_where_clause-where_tab.
      LOOP AT lt_where_tab INTO ls_where_tab.
        ls_where_tab_line = ls_where_tab-line.
        SHIFT ls_where_tab_line RIGHT.
        REPLACE ALL OCCURRENCES OF '  ''' IN ls_where_tab_line WITH ''''.

"        IF i_table_field EQ abap_true.
"        ls_where_tab_line = ls_where_clause-tablename && '~' && ls_where_tab_line.
"        ENDIF.

        CONCATENATE lv_sql_where ls_where_tab_line INTO lv_sql_where.
        CONDENSE lv_sql_where.
      ENDLOOP.
    ENDLOOP.

* Leaving only one space between each word.
    CONDENSE lv_sql_where.

* Find and replace literal operands by mathematical ones.
    REPLACE ALL OCCURRENCES OF ' EQ ' IN lv_sql_where WITH ' = ' IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF ' GE ' IN lv_sql_where WITH ' >= ' IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF ' LE ' IN lv_sql_where WITH ' <= ' IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF ' NE ' IN lv_sql_where WITH ' <> ' IN CHARACTER MODE.

    e_where = lv_sql_where.
  ENDMETHOD.
ENDCLASS.

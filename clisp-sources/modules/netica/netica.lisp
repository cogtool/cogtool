;;; Netica API interface
;;; <http://norsys.com/netica_c_api.htm>
;;;
;;; Copyright (C) 2003-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

(defpackage "NETICA"
  (:modern t)
  (:use "COMMON-LISP" "FFI")
  (:shadowing-import-from "EXPORTING"
           #:defconstant #:defvar #:defun #:defmacro #:define-symbol-macro
           #:def-c-type #:def-c-enum #:def-c-struct #:def-c-var #:def-call-out))

(in-package "NETICA")

(setf (documentation (find-package "NETICA") 'sys::impnotes) "netica")

;;; types and constants

(def-c-type bool_ns uchar)

(define-symbol-macro UNDEF_DBL (undef_dbl_func_ns))
(define-symbol-macro INFINITY_ns (inf_dbl_func_ns))

(eval-when (compile eval load)
  (defconstant MESG_LEN_ns 256)
  (defconstant NAME_MAX_ns 30))

(def-c-enum checking_ns
  (NO_CHECK 1)
  QUICK_CHECK
  REGULAR_CHECK
  COMPLETE_CHECK
  (QUERY_CHECK -1))

(def-c-enum errseverity_ns
  (NOTHING_ERR 1)
  REPORT_ERR
  NOTICE_ERR
  WARNING_ERR
  ERROR_ERR
  XXX_ERR)

(def-c-enum errcond_ns
  (OUT_OF_MEMORY_CND  #x08)
  (USER_ABORTED_CND   #x20)
  (FROM_WRAPPER_CND   #x40)
  (FROM_DEVELOPER_CND #x80)
  (INCONS_FINDING_CND #x200))

(def-c-enum eventtype_ns
  (CREATE_EVENT #x01)
  (DUPLICATE_EVENT #x02)
  (REMOVE_EVENT #x04))

(def-c-enum nodetype_bn
  (CONTINUOUS_TYPE 1)
  DISCRETE_TYPE
  TEXT_TYPE)

(def-c-enum nodekind_bn
  (NATURE_NODE 1)
  CONSTANT_NODE
  DECISION_NODE
  UTILITY_NODE
  DISCONNECTED_NODE)


;; special values for state_bn
(def-c-enum state_bn-special-values
  (EVERY_STATE -5)
  IMPOSS_STATE
  UNDEF_STATE)

;; for GetNodeFinding_bn
(def-c-enum get-node-finding-values
  (NEGATIVE_FINDING -7)
  LIKELIHOOD_FINDING
  (NO_FINDING -3))

;; special values for caseposn_bn
(def-c-enum caseposn_bn-special-values
  (FIRST_CASE -15)
  NEXT_CASE
  NO_MORE_CASES)

;; for NewSensvToFinding_bn
(def-c-enum new-sensv-to-finding-values
  (ENTROPY_SENSV #x02)
  (REAL_SENSV #x04)
  (VARIANCE_SENSV #x100))

(defconstant LAST_ENTRY -10)

(defconstant BELIEF_UPDATE #x100 "for SetNetAutoUpdate_bn")

;; for ReadNet_bn
(def-c-enum read-net-values
  (NO_VISUAL_INFO 0)
  (NO_WINDOW #x10)
  (MINIMIZED_WINDOW #x30)
  (REGULAR_WINDOW #x70))

(defconstant QUERY_ns -1)

(def-c-type state_bn int)
(def-c-type prob_bn single-float)
(def-c-type util_bn single-float)
(def-c-type level_bn double-float)
(def-c-type caseposn_bn long)

;;(def-c-struct (environ__ns :external))
(def-c-type environ_ns_ c-pointer) ; environ__ns
;;(def-c-struct (report__ns :external))
(def-c-type report_ns_ c-pointer) ; report__ns
;;(def-c-struct (stream__ns :external))
(def-c-type stream_ns_ c-pointer) ; stream__ns
;;(def-c-struct (net__bn :external))
(def-c-type net_bn_ c-pointer)  ; net__bn
;;(def-c-struct (node__bn :external))
(def-c-type node_bn_ c-pointer) ; node__bn
;;(def-c-struct (nodelist__bn :external))
(def-c-type nodelist_bn_ c-pointer) ; nodelist__bn
;;(def-c-struct (setting__bn :external))
(def-c-type setting_bn_ c-pointer) ; setting__bn
;;(def-c-struct (test__bn :external))
(def-c-type test_bn_ c-pointer) ; test__bn
;;(def-c-struct (sensv__bn :external))
(def-c-type sensv_bn_ c-pointer) ; sensv__bn

;;; foreign function definitions

(ffi:default-foreign-language :stdc)

(c-lines "#include <netica.h>~%")

;; use UNDEF_DBL in your software
(def-call-out undef_dbl_func_ns (:arguments) (:return-type double-float))
;; use INFINITY_ns in your software
(def-call-out inf_dbl_func_ns (:arguments) (:return-type double-float))

(def-call-out NewNeticaEnviron_ns
  (:arguments (license c-string) (env environ_ns_) (locn c-string))
  (:return-type environ_ns_))
(def-call-out InitNetica_bn
  (:arguments (envp (c-ptr environ_ns_) :in-out)
              (mesg (c-ptr (c-array-max character #.MESG_LEN_ns)) :out))
  (:return-type int))

(def-call-out CloseNetica_bn
  (:arguments (env environ_ns_)
              (mesg (c-ptr (c-array-max character #.MESG_LEN_ns)) :out))
  (:return-type int))
(def-call-out ArgumentChecking_ns
  (:arguments (setting checking_ns) (env environ_ns_))
  (:return-type checking_ns))
(def-call-out VisibleGUI_ns     ; ??
  (:arguments (setting int) (env environ_ns_))
  (:return-type int))
(def-call-out UserAllowed_ns    ; ??
  (:arguments (setting int) (env environ_ns_))
  (:return-type int))
(def-call-out MaxMemoryUsage_ns
  (:arguments (max_mem double-float) (env environ_ns_))
  (:return-type double-float))
(def-call-out GetNeticaVersion_bn
  (:arguments (env environ_ns_) (version (c-ptr c-string) :out))
  (:return-type int))
;(def-call-out ExecuteScript_ns
;  (:arguments (env environ_ns_) (language c-string) (script c-string))
;  (:return-type c-string))

(def-call-out GetError_ns
  (:arguments (env environ_ns_) (severity errseverity_ns)
              (after report_ns_))
  (:return-type report_ns_))
(def-call-out ErrorCategory_ns
  (:arguments (cond errcond_ns) (error report_ns_))
  (:return-type bool_ns))
(def-call-out ErrorNumber_ns
  (:arguments (error report_ns_))
  (:return-type int))
(def-call-out ErrorMessage_ns
  (:arguments (error report_ns_))
  (:return-type c-string))
(def-call-out ErrorSeverity_ns
  (:arguments (error report_ns_))
  (:return-type errseverity_ns))
(def-call-out NewError_ns
  (:arguments (env environ_ns_) (number int) (severity errseverity_ns)
              (mesg (c-ptr (c-array character #.MESG_LEN_ns)) :out))
  (:return-type report_ns_))
(def-call-out ClearError_ns
  (:arguments (error report_ns_))
  (:return-type nil))
(def-call-out ClearErrors_ns
  (:arguments (env environ_ns_) (severity errseverity_ns))
  (:return-type nil))

(def-call-out NthProb_bn
  (:arguments (probs (c-array-ptr prob_bn)) (state state_bn))
  (:return-type double-float))
(def-call-out NthLevel_bn
  (:arguments (levels (c-array-ptr level_bn)) (state state_bn))
  (:return-type double-float))
(def-call-out GetChars_ns
  (:arguments (str c-string) (index int) (dest c-string) (num int))
  (:return-type int))
(def-call-out NthChar_ns
  (:arguments (str c-string) (index int))
  (:return-type int))
(def-call-out SetNthState_bn
  (:arguments (states (c-array-ptr state_bn)) (index int) (state state_bn))
  (:return-type nil))

(def-call-out NewStreamFile_ns
  (:arguments (filename c-string) (env environ_ns_) (access c-pointer))
  (:return-type stream_ns_))
(def-call-out OpenedFileDescriptor_ns
  (:arguments (fd int) (filename c-string) (env environ_ns_))
  (:return-type stream_ns_))
(def-call-out WriteNet_bn
  (:arguments (net net_bn_) (file stream_ns_))
  (:return-type nil))
(def-call-out ReadNet_bn
  (:arguments (file stream_ns_) (visual int))
  (:return-type net_bn_))
(def-call-out WriteCase_bn
  (:arguments (nodes nodelist_bn_) (file stream_ns_)
              (ID_num long) (freq double-float))
  (:return-type caseposn_bn))
(def-call-out ReadCase_bn
  (:arguments (case_posn (c-ptr caseposn_bn)) (file stream_ns_)
              (nodes nodelist_bn_) (ID_num (c-ptr long))
              (freq (c-ptr double-float)))
  (:return-type nil))
(def-call-out SetCaseFileDelimChar_ns
  (:arguments (newchar int) (env environ_ns_))
  (:return-type int))
(def-call-out SetMissingDataChar_ns
  (:arguments (newchar int) (env environ_ns_))
  (:return-type int))
(def-call-out DeleteStream_ns
  (:arguments (file stream_ns_))
  (:return-type nil))

(def-call-out EnterFinding_bn
  (:arguments (node node_bn_) (state state_bn))
  (:return-type nil))
(def-call-out EnterFindingNot_bn
  (:arguments (node node_bn_) (state state_bn))
  (:return-type nil))
(def-call-out EnterNodeValue_bn
  (:arguments (node node_bn_) (value double-float))
  (:return-type nil))
(def-call-out EnterNodeLikelihood_bn
  (:arguments (node node_bn_) (likelihood (c-array-ptr prob_bn)))
  (:return-type nil))
(def-call-out GetNodeFinding_bn
  (:arguments (node node_bn_))
  (:return-type state_bn))
(def-call-out GetNodeLikelihood_bn
  (:arguments (node node_bn_))
  ;; `(c-array prob_bn ,(GetNodeNumberStates_bn node))
  (:return-type c-pointer))
(def-call-out GetNodeValueEntered_bn
  (:arguments (node node_bn_))
  (:return-type double-float))
(def-call-out RetractNodeFindings_bn
  (:arguments (node node_bn_))
  (:return-type nil))
(def-call-out RetractNetFindings_bn
  (:arguments (net net_bn_))
  (:return-type nil))

(def-call-out CalcNodeState_bn
  (:arguments (node node_bn_))
  (:return-type state_bn))
(def-call-out CalcNodeValue_bn
  (:arguments (node node_bn_))
  (:return-type double-float))

(def-call-out MapStateList_bn
  (:arguments (src_states (c-array-ptr state_bn)) (src_nodes nodelist_bn_)
              (dest_states (c-array-ptr state_bn)) (dest_nodes nodelist_bn_))
  (:return-type nil))

(def-call-out CompileNet_bn
  (:arguments (net net_bn_))
  (:return-type nil))
(def-call-out UncompileNet_bn
  (:arguments (net net_bn_))
  (:return-type nil))
(def-call-out IsBeliefUpdated_bn
  (:arguments (node node_bn_))
  (:return-type bool_ns))
(def-call-out GetNodeBeliefs_bn
  (:arguments (node node_bn_))
  ;; `(c-array-max prob_bn ,(GetNodeNumberStates_bn node))
  (:return-type c-pointer))
(def-call-out GetNodeExpectedValue_bn
  (:arguments (node node_bn_) (stddev (c-ptr double-float) :out)
              (x3 (c-ptr double-float) :out) (x4 (c-ptr double-float) :out))
  (:return-type double-float))
(def-call-out GetNodeExpectedUtils_bn
  (:arguments (node node_bn_))
  ;; `(c-array prob_bn ,(GetNodeNumberStates_bn node))
  (:return-type c-pointer))
(def-call-out JointProbability_bn
  (:arguments (nodes nodelist_bn_) (states (c-array-ptr state_bn)))
  (:return-type double-float))
(def-call-out FindingsProbability_bn
  (:arguments (net net_bn_))
  (:return-type double-float))
(def-call-out MostProbableConfig_bn
  (:arguments (nodes nodelist_bn_) (config (c-array-ptr state_bn)) (nth int))
  (:return-type nil))
(def-call-out SizeCompiledNet_bn
  (:arguments (net net_bn_) (method int))
  (:return-type double-float))
(def-call-out ReportJunctionTree_bn
  (:arguments (net net_bn_))
  (:return-type c-string))
(def-call-out GenerateRandomCase_bn
  (:arguments (nodes nodelist_bn_) (method int) (num double-float))
  (:return-type int))

(def-call-out NewNodeList_bn
  (:arguments (length int) (env environ_ns_))
  (:return-type nodelist_bn_))
(def-call-out AddNodeToList_bn
  (:arguments (node node_bn_) (nodes nodelist_bn_) (index int))
  (:return-type nil))
(def-call-out RemoveNthNode_bn
  (:arguments (nodes nodelist_bn_) (index int))
  (:return-type node_bn_))
(def-call-out LengthNodeList_bn
  (:arguments (nodes nodelist_bn_))
  (:return-type int))
(def-call-out NthNode_bn
  (:arguments (nodes nodelist_bn_) (index int))
  (:return-type node_bn_))
(def-call-out SetNthNode_bn
  (:arguments (nodes nodelist_bn_) (index int) (node node_bn_))
  (:return-type nil))
(def-call-out IndexOfNodeInList_bn
  (:arguments (node node_bn_) (nodes nodelist_bn_)
              (start_index int))
  (:return-type int))
(def-call-out DupNodeList_bn
  (:arguments (nodes nodelist_bn_))
  (:return-type nodelist_bn_))
(def-call-out DeleteNodeList_bn
  (:arguments (nodes nodelist_bn_))
  (:return-type nil))

(def-call-out ReviseCPTsByFindings_bn
  (:arguments (nodes nodelist_bn_) (updating int) (degree double-float))
  (:return-type nil))
(def-call-out ReviseCPTsByCaseFile_bn
  (:arguments (file stream_ns_) (nodes nodelist_bn_)
              (updating int) (degree double-float))
  (:return-type nil))
(def-call-out FadeCPTable_bn
  (:arguments (node node_bn_) (degree double-float))
  (:return-type nil))

;(def-call-out NewSetting_bn
;  (:arguments (nodes nodelist_bn_) (load bool_ns))
;  (:return-type setting_bn_))
;(def-call-out DeleteSetting_bn
;  (:arguments (cas setting_bn_))
;  (:return-type nil))
;(def-call-out SetState_bn
;  (:arguments (cas setting_bn_) (node node_bn_) (state state_bn))
;  (:return-type nil))
;(def-call-out GetState_bn
;  (:arguments (cas setting_bn_) (bnd node_bn_))
;  (:return-type state_bn))
;(def-call-out ZeroSetting_bn
;  (:arguments (cas setting_bn_))
;  (:return-type nil))
;(def-call-out NextSetting_bn
;  (:arguments (cas setting_bn_))
;  (:return-type bool_ns))
;(def-call-out MostProbableSetting_bn
;  (:arguments (cas setting_bn_) (nth int))
;  (:return-type nil))

(def-call-out ReverseLink_bn
  (:arguments (parent node_bn_) (child node_bn_))
  (:return-type nil))
(def-call-out AbsorbNodes_bn
  (:arguments (nodes nodelist_bn_))
  (:return-type nil))
(def-call-out OptimizeDecisions_bn
  (:arguments (nodes nodelist_bn_))
  (:return-type nil))
(def-call-out EquationToTable_bn
  (:arguments (node node_bn_) (num_samples int) (samp_unc bool_ns)
              (add_exist bool_ns))
  (:return-type nil))
;(def-call-out CopyNodeTables_bn
;  (:arguments (dest node_bn_) (src node_bn_)
;              (parent_order_dest nodelist_bn_))
;  (:return-type nil))

(def-call-out GetNthNet_bn
  (:arguments (nth int) (env environ_ns_))
  (:return-type net_bn_))
(def-call-out NewNet_bn
  (:arguments (name c-string) (env environ_ns_))
  (:return-type net_bn_))
(def-call-out DeleteNet_bn
  (:arguments (net net_bn_))
  (:return-type nil))
(def-call-out NewNode_bn
  (:arguments (name c-string) (num_states int) (net net_bn_))
  (:return-type node_bn_))
(def-call-out DuplicateNodes_bn
  (:arguments (nodes nodelist_bn_) (new_net net_bn_))
  (:return-type nodelist_bn_))
(def-call-out DeleteNode_bn
  (:arguments (node node_bn_))
  (:return-type nil))
(def-call-out AddLink_bn
  (:arguments (parent node_bn_) (child node_bn_))
  (:return-type int))
(def-call-out DeleteLink_bn
  (:arguments (link_index int) (child node_bn_))
  (:return-type nil))
(def-call-out SwitchNodeParent_bn
  (:arguments (link_index int) (node node_bn_)
              (new_parent node_bn_))
  (:return-type nil))

(def-call-out SetNetName_bn
  (:arguments (net net_bn_) (name c-string))
  (:return-type nil))
(def-call-out SetNetAutoUpdate_bn
  (:arguments (net net_bn_) (autoupdate int))
  (:return-type int))
(def-call-out SetNetElimOrder_bn
  (:arguments (net net_bn_) (elim_order nodelist_bn_))
  (:return-type nil))
(def-call-out SetNetTitle_bn
  (:arguments (net net_bn_) (title c-string))
  (:return-type nil))
(def-call-out SetNetComment_bn
  (:arguments (net net_bn_) (comment c-string))
  (:return-type nil))
(def-call-out AddNetListener_bn
  (:arguments (net net_bn_)
              (callback (c-function (:arguments (node net_bn_)
                                                (what eventtype_ns)
                                                (obj c-pointer)
                                                (data c-pointer))
                                    (:return-type int)))
              (obj c-pointer) (filter int))
  (:return-type nil))
(def-call-out SetNetUserField_bn
  (:arguments (net net_bn_) (name c-string) (data c-pointer)
              (length int) (kind int))
  (:return-type nil))
(def-call-out SetNetUserData_bn
  (:arguments (net net_bn_) (kind int) (data c-pointer))
  (:return-type nil))

(def-call-out SetNodeName_bn
  (:arguments (node node_bn_) (name c-string))
  (:return-type nil))
(def-call-out SetNodeKind_bn
  (:arguments (node node_bn_) (kind nodekind_bn))
  (:return-type nil))
(def-call-out SetNodeStateName_bn
  (:arguments (node node_bn_) (state state_bn) (state_name c-string))
  (:return-type nil))
(def-call-out SetNodeStateNames_bn
  (:arguments (node node_bn_) (state_names c-string))
  (:return-type nil))
(def-call-out SetNodeStateTitle_bn
  (:arguments (node node_bn_) (state state_bn) (state_title c-string))
  (:return-type nil))
(def-call-out SetNodeLevels_bn
  (:arguments (node node_bn_) (num_states int)
              (levels (c-array-ptr level_bn)))
  (:return-type nil))
(def-call-out SetNodeInputName_bn
  (:arguments (node node_bn_) (link_index int) (link_name c-string))
  (:return-type nil))
(def-call-out SetNodeEquation_bn
  (:arguments (node node_bn_) (eqn c-string))
  (:return-type nil))
(def-call-out SetNodeProbs_bn
  (:arguments (node node_bn_)
              (parent_states (c-array-ptr state_bn))
              (probs (c-array-ptr prob_bn)))
  (:return-type nil))
(def-call-out SetNodeFuncState_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn))
              (st state_bn))
  (:return-type nil))
(def-call-out SetNodeFuncReal_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn))
              (val double-float))
  (:return-type nil))
(def-call-out SetNodeExperience_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn))
              (experience double-float))
  (:return-type nil))
(def-call-out DeleteNodeTables_bn
  (:arguments (node node_bn_))
  (:return-type nil))
(def-call-out SetNodeTitle_bn
  (:arguments (node node_bn_) (title c-string))
  (:return-type nil))
(def-call-out SetNodeComment_bn
  (:arguments (node node_bn_) (comment c-string))
  (:return-type nil))
(def-call-out SetNodeVisPosition_bn
  (:arguments (node node_bn_) (vis c-pointer)
              (x double-float) (y double-float))
  (:return-type nil))
(def-call-out AddNodeListener_bn
  (:arguments (node node_bn_)
              (callback (c-function (:arguments (node node_bn_)
                                                (what eventtype_ns)
                                                (obj c-pointer)
                                                (data c-pointer))
                                    (:return-type int)))
              (obj c-pointer) (filter int))
  (:return-type nil))
(def-call-out SetNodeUserField_bn
  (:arguments (node node_bn_) (name c-string) (data c-pointer)
              (length int) (kind int))
  (:return-type nil))
(def-call-out SetNodeUserData_bn
  (:arguments (node node_bn_) (kind int) (data c-pointer))
  (:return-type nil))

(def-call-out GetNetName_bn
  (:arguments (net net_bn_))
  (:return-type c-string))
(def-call-out GetNetNodes_bn
  (:arguments (net net_bn_))
  (:return-type nodelist_bn_))
(def-call-out NodeNamed_bn
  (:arguments (name c-string) (net net_bn_))
  (:return-type node_bn_))
(def-call-out GetNetAutoUpdate_bn
  (:arguments (net net_bn_))
  (:return-type int))
(def-call-out GetNetElimOrder_bn
  (:arguments (net net_bn_))
  (:return-type nodelist_bn_))
(def-call-out GetNetTitle_bn
  (:arguments (net net_bn_))
  (:return-type c-string))
(def-call-out GetNetComment_bn
  (:arguments (net net_bn_))
  (:return-type c-string))
(def-call-out GetNetFileName_bn
  (:arguments (net net_bn_))
  (:return-type c-string))
(def-call-out GetNetUserField_bn
  (:arguments (net net_bn_) (name c-string) (length (c-ptr int) :out)
              (kind int))
  (:return-type c-string))
(def-call-out GetNetNthUserField_bn
  (:arguments (net net_bn_) (index int)
              (name (c-ptr c-string) :out) (value (c-ptr c-string) :out)
              (length (c-ptr int) :out) (kind int))
  (:return-type nil))
(def-call-out GetNetUserData_bn
  (:arguments (net net_bn_) (kind int))
  (:return-type c-pointer))

(def-call-out GetNodeNet_bn
  (:arguments (node node_bn_))
  (:return-type net_bn_))
(def-call-out GetNodeName_bn
  (:arguments (node node_bn_))
  (:return-type c-string))
(def-call-out GetNodeType_bn
  (:arguments (node node_bn_))
  (:return-type nodetype_bn))
(def-call-out GetNodeKind_bn
  (:arguments (node node_bn_))
  (:return-type nodekind_bn))
(def-call-out GetNodeNumberStates_bn
  (:arguments (node node_bn_))
  (:return-type int))
(def-call-out GetNodeStateName_bn
  (:arguments (node node_bn_) (state state_bn))
  (:return-type c-string))
(def-call-out GetNodeStateTitle_bn
  (:arguments (node node_bn_) (state state_bn))
  (:return-type c-string))
(def-call-out StateNamed_bn
  (:arguments (name c-string) (node node_bn_))
  (:return-type state_bn))
(def-call-out GetNodeLevels_bn
  (:arguments (node node_bn_))
  #| `(c-array prob_bn
        ,(+ (GetNodeNumberStates_bn node)
            (ecase (GetNodeType_bn node)
              (#.CONTINUOUS_TYPE 1) (#.DISCRETE_TYPE 0)))) |#
  (:return-type c-pointer))
(def-call-out GetNodeParents_bn
  (:arguments (node node_bn_))
  (:return-type nodelist_bn_))
(def-call-out GetNodeChildren_bn
  (:arguments (node node_bn_))
  (:return-type nodelist_bn_))
(def-call-out GetNodeInputName_bn
  (:arguments (node node_bn_) (link_index int))
  (:return-type c-string))
(def-call-out InputNamed_bn
  (:arguments (name c-string) (node node_bn_))
  (:return-type int))
(def-call-out HasNodeTable_bn
  (:arguments (node node_bn_) (complete (c-ptr bool_ns) :out))
  (:return-type bool_ns))
(def-call-out IsNodeDeterministic_bn
  (:arguments (node node_bn_))
  (:return-type bool_ns))
(def-call-out GetNodeEquation_bn
  (:arguments (node node_bn_))
  (:return-type c-string))
(def-call-out GetNodeProbs_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn)))
  ;; `(c-array prob_bn ,(GetNodeNumberStates_bn node))
  (:return-type c-pointer))
(def-call-out GetNodeFuncState_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn)))
  (:return-type state_bn))
(def-call-out GetNodeFuncReal_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn)))
  (:return-type double-float))
(def-call-out GetNodeExperience_bn
  (:arguments (node node_bn_) (parent_states (c-array-ptr state_bn)))
  (:return-type double-float))
(def-call-out GetNodeTitle_bn
  (:arguments (node node_bn_))
  (:return-type c-string))
(def-call-out GetNodeComment_bn
  (:arguments (node node_bn_))
  (:return-type c-string))
(def-call-out GetNodeVisPosition_bn
  (:arguments (node node_bn_) (vis c-pointer)
              (x (c-ptr double-float) :out) (y (c-ptr double-float) :out))
  (:return-type nil))
(def-call-out GetNodeUserField_bn
  (:arguments (node node_bn_) (name c-string) (length (c-ptr int) :out)
              (kind int))
  (:return-type c-string))
(def-call-out GetNodeNthUserField_bn
  (:arguments (node node_bn_) (index int)
              (name (c-ptr c-string) :out) (value (c-ptr c-string) :out)
              (length (c-ptr int) :out) (kind int))
  (:return-type nil))
(def-call-out GetNodeUserData_bn
  (:arguments (node node_bn_) (kind int))
  (:return-type c-pointer))

(def-call-out NewNetTester_bn
  (:arguments (test_nodes nodelist_bn_)
              (unobsv_nodes nodelist_bn_) (tests int))
  (:return-type test_bn_))
(def-call-out DeleteNetTester_bn
  (:arguments (test test_bn_))
  (:return-type nil))
(def-call-out TestWithFile_bn
  (:arguments (test test_bn_) (file stream_ns_))
  (:return-type nil))
(def-call-out GetTestConfusion_bn
  (:arguments (test test_bn_) (node node_bn_)
              (predicted state_bn) (actual state_bn))
  (:return-type double-float))
(def-call-out GetTestErrorRate_bn
  (:arguments (test test_bn_) (node node_bn_))
  (:return-type double-float))
(def-call-out GetTestLogLoss_bn
  (:arguments (test test_bn_) (node node_bn_))
  (:return-type double-float))
(def-call-out GetTestQuadraticLoss_bn
  (:arguments (test test_bn_) (node node_bn_))
  (:return-type double-float))

(def-call-out NewSensvToFinding_bn
  (:arguments (Qnode node_bn_) (Fnodes nodelist_bn_) (what_find int))
  (:return-type sensv_bn_))
(def-call-out DeleteSensvToFinding_bn
  (:arguments (s sensv_bn_))
  (:return-type nil))
(def-call-out MutualInfo_bn
  (:arguments (s sensv_bn_) (Fnode node_bn_))
  (:return-type double-float))
(def-call-out VarianceOfReal_bn
  (:arguments (s sensv_bn_) (Fnode node_bn_))
  (:return-type double-float))

(provide "netica")

module kernel
  use base
  use inductive_state, only: inductive_check_state
  use hash_table, only: table_t
  use parser, only: export_parser
  use declarations, only: daxiom,ddefinition,dtheorem,dopaque,dquot,dinductive,dconstructor,drecursor,hint_opaque,hint_lt
  use names, only: anonymous,name_str
  use levels, only: lsucc,lmax,limax,lparam
  use sequences, only: list_nil
  use expr, only: evar,esort,econst,eapp,epi,elam,elet,elocal,eproj,enat,estr,fvar_level,fvar_unique
  use closure, only: env_arena,closure_t,env_nil,ev_val=>entry_val,entry_neu,entry_v,counter_names
  use nbe, only: value_arena,vrigid,vunfold,vlam,vpi,vsort,vnat,vstr,vthunk,hbvar,hlocal,hconst, &
    caxiom,cctor,crecursor,cquot,cinductive
  use spines, only: spine_empty,elim_app,elim_proj
  use omp_lib, only: omp_get_thread_num
  implicit none
  type :: checker
    type(inductive_check_state) :: ist
    integer :: nat_name=0,nat_zero=0,nat_ops(15)=0,bool_true=0,bool_false=0,string_name=0,string_mk=0,string_of_list=0
    integer :: char_name=0,char_of_nat=0,list_nil_name=0,list_cons_name=0,big_one=0
    logical :: nat_extension=.true.,string_extension=.true.
    integer :: jobs=1,dag_epoch=4194304
    logical :: persist_dag=.true.,standard_axioms=.false.
    integer :: unique_counter=0
    type(table_t) :: find_cache
    type(export_parser) :: ctx
    type(env_arena) :: rp
    type(value_arena) :: nb
    type(status_t) :: status
    integer :: cutoff=huge(0),current_decl=0,reduce_nat=0,reduce_bool=0,quot_lift=0,quot_ind=0,quot_mk=0
    integer(int32), allocatable :: w1(:),w2(:),w3(:),clo_e(:),clo_env(:)
    integer :: work_top=0,clo_count=0
    type(table_t) :: reify_go_cache,infer_cache_check,infer_cache_only,clo_intern,g_unfold,g_inst_ty,prop_cache
    integer(int64) :: totals(26)=0
  contains
    procedure :: init
    procedure :: cached_name
    procedure :: name_id
    procedure :: name_text
    procedure :: work_slot
    procedure :: cache_closure
    procedure :: levels_equal
    procedure :: reset_decl
    procedure :: check_declaration
    procedure :: check_parallel
    procedure :: report_decl_counters
    procedure :: check_all
    procedure :: nb_eval
    procedure :: nb_delay
    procedure :: nb_thunk
    procedure :: entry_val
    procedure :: nb_local
    procedure :: nb_const
    procedure :: nb_force
    procedure :: nb_apply
    procedure :: nb_lam_domain
    procedure :: nb_open
    procedure :: nb_whnf
    procedure :: nb_unfold
    procedure :: nb_unfold_demand
    procedure :: nb_unfold_go
    procedure :: nb_unfold_const
    procedure :: nb_replay
    procedure :: nb_proj
    procedure :: reify
    procedure :: reify_go
    procedure :: nb_readback
    procedure :: nb_readback_spine
    procedure :: nb_type
    procedure :: nb_const_type
    procedure :: nb_spine_type
    procedure :: nb_type_level
    procedure :: nb_sort_of
    procedure :: nb_const_level
    procedure :: nb_conv
    procedure :: cacheable
    procedure :: nb_unify
    procedure :: nb_unify_go
    procedure :: nb_unify_direct
    procedure :: iota_head
    procedure :: nb_unfold_one
    procedure :: nb_unfold_pair
    procedure :: nb_spine_probe
    procedure :: nb_unify_spine
    procedure :: nb_unify_spine_go
    procedure :: nb_proof_irrel
    procedure :: nb_unify_iota
    procedure :: nb_struct_eta
    procedure :: nb_eta_struct
    procedure :: infer
    procedure :: infer_go
    procedure :: check_level
    procedure :: check_levels
    procedure :: infer_const
    procedure :: infer_lambda
    procedure :: infer_pi
    procedure :: infer_let
    procedure :: infer_s
    procedure :: ensure_sort_clo
    procedure :: ensure_sort
    procedure :: nb_of_clo
    procedure :: is_def_eq
    procedure :: cheap_beta_reduce
    procedure :: spine_args
    procedure :: as_inductive
    procedure :: get_structure
    procedure :: major_idx
    procedure :: get_major_induct
    procedure :: nb_iota
    procedure :: nb_iota_go
    procedure :: nb_fire_quot
    procedure :: nb_fire_recursor
    procedure :: nb_k_reduce
    procedure :: nb_struct_eta_reduce
    procedure :: nb_struct_eta_go
    procedure :: nb_field_type
    procedure :: is_prop_of
    procedure :: may_be_prop_of
    procedure :: infer_proj
    procedure :: mk_unique
    procedure :: index_name
    procedure :: list_reverse
    procedure :: list_contains
    procedure :: list_set_equal
    procedure :: fold_apps
    procedure :: unfold_apps
    procedure :: abstr_binder
    procedure :: abstr_binders
    procedure :: whnf
    procedure :: assert_def_eq
    procedure :: ensure_infers_sort
    procedure :: pi_telescope_size
    procedure :: get_local_params
    procedure :: find_const
    procedure :: find_const_go
    procedure :: check_declar_info
    procedure :: collect_mutuals
    procedure :: check_inductive_initial
    procedure :: expr_list_subst
    procedure :: inst_forall_params
    procedure :: replace_if_nested
    procedure :: restored_rec_name
    procedure :: restore_ctor_name
    procedure :: restore_replace
    procedure :: restore_f
    procedure :: restore_e
    procedure :: check_restored_recursor
    procedure :: restore_and_check
    procedure :: replace_all_nested
    procedure :: specialize_nested
    procedure :: check_inductive_specs
    procedure :: is_valid_ind_app
    procedure :: which_valid_ind_app
    procedure :: check_positivity
    procedure :: check_ctor
    procedure :: large_elim_test
    procedure :: mk_elim_level
    procedure :: init_k_target
    procedure :: mk_majors_motives
    procedure :: is_rec_argument
    procedure :: sep_ctor_args
    procedure :: handle_rec_args_aux
    procedure :: get_i_indices
    procedure :: handle_rec_args_minor
    procedure :: mk_minors
    procedure :: all_motives
    procedure :: all_minors
    procedure :: handle_rec_rule_args
    procedure :: mk_rec_rule
    procedure :: mk_recursors
    procedure :: mk_ind_types_extension
    procedure :: mk_ctors_extension
    procedure :: check_recursor_names
    procedure :: compare_inductive_exports
    procedure :: check_inductive
    procedure :: check_ctor_or_rec
    procedure :: expr_apps
    procedure :: expr_binders
    procedure :: arrows
    procedure :: check_eq_for_quot
    procedure :: check_quot
    procedure :: nat_op
    procedure :: nb_nat_red
    procedure :: nb_nat_defer
    procedure :: nb_bignum
    procedure :: nb_is_open
    procedure :: nb_nat_to_ctor
    procedure :: nb_nat_rec
    procedure :: nb_may_be_nat
    procedure :: nb_is_nat_zero
    procedure :: nb_nat_pred
    procedure :: nb_conv_nat
    procedure :: nb_str_to_ctor
    procedure :: nb_is_string_ctor
    procedure :: nb_conv_str
  end type
contains
  include 'kernel_support.inc'
  include 'parallel.inc'
  include 'nbe_eval.inc'
  include 'readback.inc'
  include 'nbe_type.inc'
  include 'nbe_conv.inc'
  include 'tc.inc'
  include 'nbe_inductive.inc'
  include 'projections.inc'
  include 'inductive_helpers.inc'
  include 'inductive_check.inc'
  include 'inductive_nested.inc'
  include 'inductive_generate.inc'
  include 'inductive_driver.inc'
  include 'quot.inc'
  include 'nbe_nat.inc'
  include 'nbe_string.inc'
end module

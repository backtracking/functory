(benchmark binaryusearchupo8
  :status unknown
  :extrasorts (c_Boolean)
  :extrafuns ((c_Boolean_true c_Boolean))
  :extrafuns ((c_Boolean_false c_Boolean))
  :assumption
                   (forall (?bcd c_Boolean) (or (= ?bcd c_Boolean_true) 
                                            (= ?bcd c_Boolean_false)))
  :assumption
                   (not 
                      (= c_Boolean_true  c_Boolean_false))
  :extrasorts (Unit)
  :extrafuns ((div_int Int Int Int))
  :extrafuns ((modulo Int Int Int))
:extrasorts (c_unsorted)
:extrasorts (c_sorted)
:extrasorts (c_type)
;;;; Why logic c_int
:extrafuns ((c_int  c_type))

;;;; Why logic c_bool
:extrafuns ((c_bool  c_type))

;;;; Why logic c_real
:extrafuns ((c_real  c_type))

;;;; Why logic c_unit
:extrafuns ((c_unit  c_type))

;;;; Why logic c_sort
:extrafuns ((c_sort c_type c_unsorted c_sorted))

;;;; Why logic int2u
:extrafuns ((int2u Int c_unsorted))

;;;; Why logic bool2u
:extrafuns ((bool2u c_Boolean c_unsorted))

;;;; Why logic real2u
:extrafuns ((real2u Real c_unsorted))

;;;; Why logic unit2u
:extrafuns ((unit2u Unit c_unsorted))

;;;; Why logic s2int
:extrafuns ((s2int c_sorted Int))

;;;; Why logic s2bool
:extrafuns ((s2bool c_sorted c_Boolean))

;;;; Why logic s2real
:extrafuns ((s2real c_sorted Real))

;;;; Why logic s2unit
:extrafuns ((s2unit c_sorted Unit))

;; Why axiom s2int_inv_int2u
 :assumption
   (forall (?xu0_1 Int) (= (s2int (c_sort c_int (int2u ?xu0_1))) ?xu0_1))

;; Why axiom s2bool_inv_bool2u
 :assumption
   (forall (?xu0_2 c_Boolean)
   (= (s2bool (c_sort c_bool (bool2u ?xu0_2))) ?xu0_2))

;; Why axiom s2real_inv_real2u
 :assumption
   (forall (?xu0_3 Real) (= (s2real (c_sort c_real (real2u ?xu0_3))) ?xu0_3))

;; Why axiom s2unit_inv_unit2u
 :assumption
   (forall (?xu0_4 Unit) (= (s2unit (c_sort c_unit (unit2u ?xu0_4))) ?xu0_4))

;; Why axiom int2u_inv_s2int
 :assumption
   (forall (?xu0_5 c_unsorted)
   (= (int2u (s2int (c_sort c_int ?xu0_5))) ?xu0_5))

;; Why axiom bool2u_inv_s2bool
 :assumption
   (forall (?xu0_6 c_unsorted)
   (= (bool2u (s2bool (c_sort c_bool ?xu0_6))) ?xu0_6))

;; Why axiom real2u_inv_s2real
 :assumption
   (forall (?xu0_7 c_unsorted)
   (= (real2u (s2real (c_sort c_real ?xu0_7))) ?xu0_7))

;; Why axiom unit2u_inv_s2unit
 :assumption
   (forall (?xu0_8 c_unsorted)
   (= (unit2u (s2unit (c_sort c_unit ?xu0_8))) ?xu0_8))

;;;; Why logic eq_unit
:extrapreds ((eq_unit Unit Unit))

;;;; Why logic neq_unit
:extrapreds ((neq_unit Unit Unit))

;;;; Why logic eq_bool
:extrapreds ((eq_bool c_Boolean c_Boolean))

;;;; Why logic neq_bool
:extrapreds ((neq_bool c_Boolean c_Boolean))

;;;; Why logic eq_int
:extrapreds ((eq_int Int Int))

;;;; Why logic neq_int
:extrapreds ((neq_int Int Int))

;;;; Why logic zwf_zero
:extrapreds ((zwf_zero Int Int))

;; Why axiom zwf_zero_def
 :assumption
   (forall (?au2_9 Int)
   (forall (?bu1_10 Int)
   (iff (and (<= 0 ?bu1_10) (< ?au2_9 ?bu1_10))
   (and (<= 0 ?bu1_10) (< ?au2_9 ?bu1_10)))))

;;;; Why logic bw_compl
:extrafuns ((bw_compl Int Int))

;;;; Why logic bw_and
:extrafuns ((bw_and Int Int Int))

;;;; Why logic bw_xor
:extrafuns ((bw_xor Int Int Int))

;;;; Why logic bw_or
:extrafuns ((bw_or Int Int Int))

;;;; Why logic lsl
:extrafuns ((lsl Int Int Int))

;;;; Why logic lsr
:extrafuns ((lsr Int Int Int))

;;;; Why logic pointer
:extrafuns ((pointer c_type c_type))

;;;; Why logic addr
:extrafuns ((addr c_type c_type))

;;;; Why logic alloc_table
:extrafuns ((alloc_table  c_type))

;;;; Why logic block_length
:extrafuns ((block_length c_sorted c_sorted Int))

;;;; Why logic base_addr
:extrafuns ((base_addr c_sorted c_unsorted))

;;;; Why logic offset
:extrafuns ((offset c_sorted Int))

;;;; Why logic shift
:extrafuns ((shift c_sorted Int c_unsorted))

;;;; Why logic sub_pointer
:extrafuns ((sub_pointer c_sorted c_sorted Int))

;;;; Why logic lt_pointer
:extrapreds ((lt_pointer c_sorted c_sorted))

;; Why axiom lt_pointer_def
 :assumption
   (forall (?t1u0_11 c_type)
   (forall (?p1u4_12 c_unsorted)
   (forall (?p2u3_13 c_unsorted)
   (iff
   (lt_pointer
   (c_sort (pointer ?t1u0_11) ?p1u4_12) (c_sort (pointer ?t1u0_11) ?p2u3_13))
   (and
   (= (c_sort
      (addr ?t1u0_11) (base_addr (c_sort (pointer ?t1u0_11) ?p1u4_12)))
   (c_sort (addr ?t1u0_11) (base_addr (c_sort (pointer ?t1u0_11) ?p2u3_13))))
   (< (offset (c_sort (pointer ?t1u0_11) ?p1u4_12)) (offset
                                                    (c_sort
                                                    (pointer ?t1u0_11) ?p2u3_13))))))))

;;;; Why logic le_pointer
:extrapreds ((le_pointer c_sorted c_sorted))

;; Why axiom le_pointer_def
 :assumption
   (forall (?t1u0_14 c_type)
   (forall (?p1u6_15 c_unsorted)
   (forall (?p2u5_16 c_unsorted)
   (iff
   (le_pointer
   (c_sort (pointer ?t1u0_14) ?p1u6_15) (c_sort (pointer ?t1u0_14) ?p2u5_16))
   (and
   (= (c_sort
      (addr ?t1u0_14) (base_addr (c_sort (pointer ?t1u0_14) ?p1u6_15)))
   (c_sort (addr ?t1u0_14) (base_addr (c_sort (pointer ?t1u0_14) ?p2u5_16))))
   (<= (offset (c_sort (pointer ?t1u0_14) ?p1u6_15)) (offset
                                                     (c_sort
                                                     (pointer ?t1u0_14) ?p2u5_16))))))))

;;;; Why logic gt_pointer
:extrapreds ((gt_pointer c_sorted c_sorted))

;; Why axiom gt_pointer_def
 :assumption
   (forall (?t1u0_17 c_type)
   (forall (?p1u8_18 c_unsorted)
   (forall (?p2u7_19 c_unsorted)
   (iff
   (gt_pointer
   (c_sort (pointer ?t1u0_17) ?p1u8_18) (c_sort (pointer ?t1u0_17) ?p2u7_19))
   (and
   (= (c_sort
      (addr ?t1u0_17) (base_addr (c_sort (pointer ?t1u0_17) ?p1u8_18)))
   (c_sort (addr ?t1u0_17) (base_addr (c_sort (pointer ?t1u0_17) ?p2u7_19))))
   (> (offset (c_sort (pointer ?t1u0_17) ?p1u8_18)) (offset
                                                    (c_sort
                                                    (pointer ?t1u0_17) ?p2u7_19))))))))

;;;; Why logic ge_pointer
:extrapreds ((ge_pointer c_sorted c_sorted))

;; Why axiom ge_pointer_def
 :assumption
   (forall (?t1u0_20 c_type)
   (forall (?p1u10_21 c_unsorted)
   (forall (?p2u9_22 c_unsorted)
   (iff
   (ge_pointer
   (c_sort (pointer ?t1u0_20) ?p1u10_21) (c_sort (pointer ?t1u0_20) ?p2u9_22))
   (and
   (= (c_sort
      (addr ?t1u0_20) (base_addr (c_sort (pointer ?t1u0_20) ?p1u10_21)))
   (c_sort (addr ?t1u0_20) (base_addr (c_sort (pointer ?t1u0_20) ?p2u9_22))))
   (>= (offset (c_sort (pointer ?t1u0_20) ?p1u10_21)) (offset
                                                      (c_sort
                                                      (pointer ?t1u0_20) ?p2u9_22))))))))

;;;; Why logic valid
:extrapreds ((valid c_sorted c_sorted))

;; Why axiom valid_def
 :assumption
   (forall (?t1u0_23 c_type)
   (forall (?au12_24 c_unsorted)
   (forall (?pu11_25 c_unsorted)
   (iff
   (valid (c_sort alloc_table ?au12_24) (c_sort (pointer ?t1u0_23) ?pu11_25))
   (and (<= 0 (offset (c_sort (pointer ?t1u0_23) ?pu11_25)))
   (< (offset (c_sort (pointer ?t1u0_23) ?pu11_25)) (block_length
                                                    (c_sort
                                                    alloc_table ?au12_24) 
                                                    (c_sort
                                                    (pointer ?t1u0_23) ?pu11_25))))))))

;;;; Why logic valid_index
:extrapreds ((valid_index c_sorted c_sorted Int))

;; Why axiom valid_index_def
 :assumption
   (forall (?t1u0_26 c_type)
   (forall (?au15_27 c_unsorted)
   (forall (?pu14_28 c_unsorted)
   (forall (?iu13_29 Int)
   (iff
   (valid_index
   (c_sort alloc_table ?au15_27) (c_sort (pointer ?t1u0_26) ?pu14_28) ?iu13_29)
   (and (<= 0 (+ (offset (c_sort (pointer ?t1u0_26) ?pu14_28)) ?iu13_29))
   (< (+ (offset (c_sort (pointer ?t1u0_26) ?pu14_28)) ?iu13_29) (block_length
                                                                 (c_sort
                                                                 alloc_table ?au15_27) 
                                                                 (c_sort
                                                                 (pointer
                                                                 ?t1u0_26) ?pu14_28)))))))))

;;;; Why logic valid_range
:extrapreds ((valid_range c_sorted c_sorted Int Int))

;; Why axiom valid_range_def
 :assumption
   (forall (?t1u0_30 c_type)
   (forall (?au19_31 c_unsorted)
   (forall (?pu18_32 c_unsorted)
   (forall (?iu17_33 Int)
   (forall (?ju16_34 Int)
   (iff
   (valid_range
   (c_sort alloc_table ?au19_31) (c_sort (pointer ?t1u0_30) ?pu18_32) ?iu17_33 ?ju16_34)
   (and (<= 0 (+ (offset (c_sort (pointer ?t1u0_30) ?pu18_32)) ?iu17_33))
   (< (+ (offset (c_sort (pointer ?t1u0_30) ?pu18_32)) ?ju16_34) (block_length
                                                                 (c_sort
                                                                 alloc_table ?au19_31) 
                                                                 (c_sort
                                                                 (pointer
                                                                 ?t1u0_30) ?pu18_32))))))))))

;; Why axiom offset_shift
 :assumption
   (forall (?t1u0_35 c_type)
   (forall (?pu21_36 c_unsorted)
   (forall (?iu20_37 Int)
   (= (offset
      (c_sort
      (pointer ?t1u0_35) (shift
                         (c_sort (pointer ?t1u0_35) ?pu21_36) ?iu20_37)))
   (+ (offset (c_sort (pointer ?t1u0_35) ?pu21_36)) ?iu20_37)))))

;; Why axiom shift_zero
 :assumption
   (forall (?t1u0_38 c_type)
   (forall (?pu22_39 c_unsorted)
   (= (c_sort
      (pointer ?t1u0_38) (shift (c_sort (pointer ?t1u0_38) ?pu22_39) 0))
   (c_sort (pointer ?t1u0_38) ?pu22_39))))

;; Why axiom shift_shift
 :assumption
   (forall (?t1u0_40 c_type)
   (forall (?pu25_41 c_unsorted)
   (forall (?iu24_42 Int)
   (forall (?ju23_43 Int)
   (= (c_sort
      (pointer ?t1u0_40) (shift
                         (c_sort
                         (pointer ?t1u0_40) (shift
                                            (c_sort
                                            (pointer ?t1u0_40) ?pu25_41) ?iu24_42)) ?ju23_43))
   (c_sort
   (pointer ?t1u0_40) (shift
                      (c_sort (pointer ?t1u0_40) ?pu25_41) (+ ?iu24_42 ?ju23_43))))))))

;; Why axiom base_addr_shift
 :assumption
   (forall (?t1u0_44 c_type)
   (forall (?pu27_45 c_unsorted)
   (forall (?iu26_46 Int)
   (= (c_sort
      (addr ?t1u0_44) (base_addr
                      (c_sort
                      (pointer ?t1u0_44) (shift
                                         (c_sort (pointer ?t1u0_44) ?pu27_45) ?iu26_46))))
   (c_sort (addr ?t1u0_44) (base_addr (c_sort (pointer ?t1u0_44) ?pu27_45)))))))

;; Why axiom block_length_shift
 :assumption
   (forall (?t1u0_47 c_type)
   (forall (?au30_48 c_unsorted)
   (forall (?pu29_49 c_unsorted)
   (forall (?iu28_50 Int)
   (= (block_length
      (c_sort alloc_table ?au30_48) (c_sort
                                    (pointer ?t1u0_47) (shift
                                                       (c_sort
                                                       (pointer ?t1u0_47) ?pu29_49) ?iu28_50)))
   (block_length
   (c_sort alloc_table ?au30_48) (c_sort (pointer ?t1u0_47) ?pu29_49)))))))

;; Why axiom base_addr_block_length
 :assumption
   (forall (?t1u0_51 c_type)
   (forall (?au33_52 c_unsorted)
   (forall (?p1u32_53 c_unsorted)
   (forall (?p2u31_54 c_unsorted)
   (implies
   (= (c_sort
      (addr ?t1u0_51) (base_addr (c_sort (pointer ?t1u0_51) ?p1u32_53)))
   (c_sort (addr ?t1u0_51) (base_addr (c_sort (pointer ?t1u0_51) ?p2u31_54))))
   (= (block_length
      (c_sort alloc_table ?au33_52) (c_sort (pointer ?t1u0_51) ?p1u32_53))
   (block_length
   (c_sort alloc_table ?au33_52) (c_sort (pointer ?t1u0_51) ?p2u31_54))))))))

;; Why axiom pointer_pair_1
 :assumption
   (forall (?t1u0_55 c_type)
   (forall (?p1u35_56 c_unsorted)
   (forall (?p2u34_57 c_unsorted)
   (implies
   (and
   (= (c_sort
      (addr ?t1u0_55) (base_addr (c_sort (pointer ?t1u0_55) ?p1u35_56)))
   (c_sort (addr ?t1u0_55) (base_addr (c_sort (pointer ?t1u0_55) ?p2u34_57))))
   (= (offset (c_sort (pointer ?t1u0_55) ?p1u35_56))
   (offset (c_sort (pointer ?t1u0_55) ?p2u34_57))))
   (= (c_sort (pointer ?t1u0_55) ?p1u35_56)
   (c_sort (pointer ?t1u0_55) ?p2u34_57))))))

;; Why axiom pointer_pair_2
 :assumption
   (forall (?t1u0_58 c_type)
   (forall (?p1u37_59 c_unsorted)
   (forall (?p2u36_60 c_unsorted)
   (implies
   (= (c_sort (pointer ?t1u0_58) ?p1u37_59)
   (c_sort (pointer ?t1u0_58) ?p2u36_60))
   (and
   (= (c_sort
      (addr ?t1u0_58) (base_addr (c_sort (pointer ?t1u0_58) ?p1u37_59)))
   (c_sort (addr ?t1u0_58) (base_addr (c_sort (pointer ?t1u0_58) ?p2u36_60))))
   (= (offset (c_sort (pointer ?t1u0_58) ?p1u37_59))
   (offset (c_sort (pointer ?t1u0_58) ?p2u36_60))))))))

;; Why axiom neq_base_addr_neq_shift
 :assumption
   (forall (?t1u0_61 c_type)
   (forall (?p1u41_62 c_unsorted)
   (forall (?p2u40_63 c_unsorted)
   (forall (?iu39_64 Int)
   (forall (?ju38_65 Int)
   (implies
   (not (= (c_sort
           (addr ?t1u0_61) (base_addr (c_sort (pointer ?t1u0_61) ?p1u41_62)))
   (c_sort (addr ?t1u0_61) (base_addr (c_sort (pointer ?t1u0_61) ?p2u40_63)))))
   (not (= (c_sort
           (pointer ?t1u0_61) (shift
                              (c_sort (pointer ?t1u0_61) ?p1u41_62) ?iu39_64))
   (c_sort
   (pointer ?t1u0_61) (shift (c_sort (pointer ?t1u0_61) ?p2u40_63) ?ju38_65))))))))))

;; Why axiom neq_offset_neq_shift
 :assumption
   (forall (?t1u0_66 c_type)
   (forall (?p1u45_67 c_unsorted)
   (forall (?p2u44_68 c_unsorted)
   (forall (?iu43_69 Int)
   (forall (?ju42_70 Int)
   (implies
   (not (= (+ (offset (c_sort (pointer ?t1u0_66) ?p1u45_67)) ?iu43_69)
   (+ (offset (c_sort (pointer ?t1u0_66) ?p2u44_68)) ?ju42_70)))
   (not (= (c_sort
           (pointer ?t1u0_66) (shift
                              (c_sort (pointer ?t1u0_66) ?p1u45_67) ?iu43_69))
   (c_sort
   (pointer ?t1u0_66) (shift (c_sort (pointer ?t1u0_66) ?p2u44_68) ?ju42_70))))))))))

;; Why axiom eq_offset_eq_shift
 :assumption
   (forall (?t1u0_71 c_type)
   (forall (?p1u49_72 c_unsorted)
   (forall (?p2u48_73 c_unsorted)
   (forall (?iu47_74 Int)
   (forall (?ju46_75 Int)
   (implies
   (= (c_sort
      (addr ?t1u0_71) (base_addr (c_sort (pointer ?t1u0_71) ?p1u49_72)))
   (c_sort (addr ?t1u0_71) (base_addr (c_sort (pointer ?t1u0_71) ?p2u48_73))))
   (implies
   (= (+ (offset (c_sort (pointer ?t1u0_71) ?p1u49_72)) ?iu47_74)
   (+ (offset (c_sort (pointer ?t1u0_71) ?p2u48_73)) ?ju46_75))
   (= (c_sort
      (pointer ?t1u0_71) (shift
                         (c_sort (pointer ?t1u0_71) ?p1u49_72) ?iu47_74))
   (c_sort
   (pointer ?t1u0_71) (shift (c_sort (pointer ?t1u0_71) ?p2u48_73) ?ju46_75))))))))))

;; Why axiom valid_index_valid_shift
 :assumption
   (forall (?t1u0_76 c_type)
   (forall (?au52_77 c_unsorted)
   (forall (?pu51_78 c_unsorted)
   (forall (?iu50_79 Int)
   (implies
   (valid_index
   (c_sort alloc_table ?au52_77) (c_sort (pointer ?t1u0_76) ?pu51_78) ?iu50_79)
   (valid
   (c_sort alloc_table ?au52_77) (c_sort
                                 (pointer ?t1u0_76) (shift
                                                    (c_sort
                                                    (pointer ?t1u0_76) ?pu51_78) ?iu50_79))))))))

;; Why axiom valid_range_valid_shift
 :assumption
   (forall (?t1u0_80 c_type)
   (forall (?au57_81 c_unsorted)
   (forall (?pu56_82 c_unsorted)
   (forall (?iu55_83 Int)
   (forall (?ju54_84 Int)
   (forall (?ku53_85 Int)
   (implies
   (valid_range
   (c_sort alloc_table ?au57_81) (c_sort (pointer ?t1u0_80) ?pu56_82) ?iu55_83 ?ju54_84)
   (implies (and (<= ?iu55_83 ?ku53_85) (<= ?ku53_85 ?ju54_84))
   (valid
   (c_sort alloc_table ?au57_81) (c_sort
                                 (pointer ?t1u0_80) (shift
                                                    (c_sort
                                                    (pointer ?t1u0_80) ?pu56_82) ?ku53_85)))))))))))

;; Why axiom valid_range_valid
 :assumption
   (forall (?t1u0_86 c_type)
   (forall (?au61_87 c_unsorted)
   (forall (?pu60_88 c_unsorted)
   (forall (?iu59_89 Int)
   (forall (?ju58_90 Int)
   (implies
   (valid_range
   (c_sort alloc_table ?au61_87) (c_sort (pointer ?t1u0_86) ?pu60_88) ?iu59_89 ?ju58_90)
   (implies (and (<= ?iu59_89 0) (<= 0 ?ju58_90))
   (valid (c_sort alloc_table ?au61_87) (c_sort (pointer ?t1u0_86) ?pu60_88)))))))))

;; Why axiom valid_range_valid_index
 :assumption
   (forall (?t1u0_91 c_type)
   (forall (?au66_92 c_unsorted)
   (forall (?pu65_93 c_unsorted)
   (forall (?iu64_94 Int)
   (forall (?ju63_95 Int)
   (forall (?ku62_96 Int)
   (implies
   (valid_range
   (c_sort alloc_table ?au66_92) (c_sort (pointer ?t1u0_91) ?pu65_93) ?iu64_94 ?ju63_95)
   (implies (and (<= ?iu64_94 ?ku62_96) (<= ?ku62_96 ?ju63_95))
   (valid_index
   (c_sort alloc_table ?au66_92) (c_sort (pointer ?t1u0_91) ?pu65_93) ?ku62_96)))))))))

;; Why axiom sub_pointer_def
 :assumption
   (forall (?t1u0_97 c_type)
   (forall (?p1u68_98 c_unsorted)
   (forall (?p2u67_99 c_unsorted)
   (implies
   (= (c_sort
      (addr ?t1u0_97) (base_addr (c_sort (pointer ?t1u0_97) ?p1u68_98)))
   (c_sort (addr ?t1u0_97) (base_addr (c_sort (pointer ?t1u0_97) ?p2u67_99))))
   (= (sub_pointer
      (c_sort (pointer ?t1u0_97) ?p1u68_98) (c_sort
                                            (pointer ?t1u0_97) ?p2u67_99))
   (- (offset (c_sort (pointer ?t1u0_97) ?p1u68_98)) (offset
                                                     (c_sort
                                                     (pointer ?t1u0_97) ?p2u67_99))))))))

;;;; Why logic memory
:extrafuns ((memory c_type c_type c_type))

;;;; Why logic acc
:extrafuns ((acc c_sorted c_sorted c_unsorted))

;;;; Why logic upd
:extrafuns ((upd c_sorted c_sorted c_sorted c_unsorted))

;; Why axiom acc_upd
 :assumption
   (forall (?t2u0_100 c_type)
   (forall (?t1u0_101 c_type)
   (forall (?mu71_102 c_unsorted)
   (forall (?pu70_103 c_unsorted)
   (forall (?au69_104 c_unsorted)
   (= (c_sort
      ?t1u0_101 (acc
                (c_sort
                (memory ?t1u0_101 ?t2u0_100) (upd
                                             (c_sort
                                             (memory ?t1u0_101 ?t2u0_100) ?mu71_102) 
                                             (c_sort
                                             (pointer ?t2u0_100) ?pu70_103) 
                                             (c_sort ?t1u0_101 ?au69_104))) 
                (c_sort (pointer ?t2u0_100) ?pu70_103)))
   (c_sort ?t1u0_101 ?au69_104)))))))

;; Why axiom acc_upd_neq
 :assumption
   (forall (?t2u0_105 c_type)
   (forall (?t1u0_106 c_type)
   (forall (?mu75_107 c_unsorted)
   (forall (?p1u74_108 c_unsorted)
   (forall (?p2u73_109 c_unsorted)
   (forall (?au72_110 c_unsorted)
   (implies
   (not (= (c_sort (pointer ?t2u0_105) ?p1u74_108)
   (c_sort (pointer ?t2u0_105) ?p2u73_109)))
   (= (c_sort
      ?t1u0_106 (acc
                (c_sort
                (memory ?t1u0_106 ?t2u0_105) (upd
                                             (c_sort
                                             (memory ?t1u0_106 ?t2u0_105) ?mu75_107) 
                                             (c_sort
                                             (pointer ?t2u0_105) ?p1u74_108) 
                                             (c_sort ?t1u0_106 ?au72_110))) 
                (c_sort (pointer ?t2u0_105) ?p2u73_109)))
   (c_sort
   ?t1u0_106 (acc
             (c_sort (memory ?t1u0_106 ?t2u0_105) ?mu75_107) (c_sort
                                                             (pointer
                                                             ?t2u0_105) ?p2u73_109)))))))))))

;; Why axiom false_not_true
 :assumption (not (= c_Boolean_false c_Boolean_true))

;;;; Why logic pset
:extrafuns ((pset c_type c_type))

;;;; Why logic pset_empty
:extrafuns ((pset_empty  c_unsorted))

;;;; Why logic pset_singleton
:extrafuns ((pset_singleton c_sorted c_unsorted))

;;;; Why logic pset_star
:extrafuns ((pset_star c_sorted c_sorted c_unsorted))

;;;; Why logic pset_all
:extrafuns ((pset_all c_sorted c_unsorted))

;;;; Why logic pset_range
:extrafuns ((pset_range c_sorted Int Int c_unsorted))

;;;; Why logic pset_range_left
:extrafuns ((pset_range_left c_sorted Int c_unsorted))

;;;; Why logic pset_range_right
:extrafuns ((pset_range_right c_sorted Int c_unsorted))

;;;; Why logic pset_acc_all
:extrafuns ((pset_acc_all c_sorted c_sorted c_unsorted))

;;;; Why logic pset_acc_range
:extrafuns ((pset_acc_range c_sorted c_sorted Int Int c_unsorted))

;;;; Why logic pset_acc_range_left
:extrafuns ((pset_acc_range_left c_sorted c_sorted Int c_unsorted))

;;;; Why logic pset_acc_range_right
:extrafuns ((pset_acc_range_right c_sorted c_sorted Int c_unsorted))

;;;; Why logic pset_union
:extrafuns ((pset_union c_sorted c_sorted c_unsorted))

;;;; Why logic not_in_pset
:extrapreds ((not_in_pset c_sorted c_sorted))

;;;; Why logic not_assigns
:extrapreds ((not_assigns c_sorted c_sorted c_sorted c_sorted))

;; Why axiom not_assigns_def
 :assumption
   (forall (?t2u0_111 c_type)
   (forall (?t1u0_112 c_type)
   (forall (?au80_113 c_unsorted)
   (forall (?m1u79_114 c_unsorted)
   (forall (?m2u78_115 c_unsorted)
   (forall (?lu77_116 c_unsorted)
   (iff
   (not_assigns
   (c_sort alloc_table ?au80_113) (c_sort
                                  (memory ?t1u0_112 ?t2u0_111) ?m1u79_114) 
   (c_sort (memory ?t1u0_112 ?t2u0_111) ?m2u78_115) (c_sort
                                                    (pset ?t2u0_111) ?lu77_116))
   (forall (?pu76_117 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?au80_113) (c_sort (pointer ?t2u0_111) ?pu76_117))
   (implies
   (not_in_pset
   (c_sort (pointer ?t2u0_111) ?pu76_117) (c_sort (pset ?t2u0_111) ?lu77_116))
   (= (c_sort
      ?t1u0_112 (acc
                (c_sort (memory ?t1u0_112 ?t2u0_111) ?m2u78_115) (c_sort
                                                                 (pointer
                                                                 ?t2u0_111) ?pu76_117)))
   (c_sort
   ?t1u0_112 (acc
             (c_sort (memory ?t1u0_112 ?t2u0_111) ?m1u79_114) (c_sort
                                                              (pointer
                                                              ?t2u0_111) ?pu76_117))))))))))))))

;; Why axiom pset_empty_intro
 :assumption
   (forall (?t1u0_118 c_type)
   (forall (?pu81_119 c_unsorted)
   (not_in_pset
   (c_sort (pointer ?t1u0_118) ?pu81_119) (c_sort
                                          (pset ?t1u0_118) pset_empty))))

;; Why axiom pset_singleton_intro
 :assumption
   (forall (?t1u0_120 c_type)
   (forall (?p1u83_121 c_unsorted)
   (forall (?p2u82_122 c_unsorted)
   (implies
   (not (= (c_sort (pointer ?t1u0_120) ?p1u83_121)
   (c_sort (pointer ?t1u0_120) ?p2u82_122)))
   (not_in_pset
   (c_sort (pointer ?t1u0_120) ?p1u83_121) (c_sort
                                           (pset ?t1u0_120) (pset_singleton
                                                            (c_sort
                                                            (pointer
                                                            ?t1u0_120) ?p2u82_122))))))))

;; Why axiom pset_singleton_elim
 :assumption
   (forall (?t1u0_123 c_type)
   (forall (?p1u85_124 c_unsorted)
   (forall (?p2u84_125 c_unsorted)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_123) ?p1u85_124) (c_sort
                                           (pset ?t1u0_123) (pset_singleton
                                                            (c_sort
                                                            (pointer
                                                            ?t1u0_123) ?p2u84_125))))
   (not (= (c_sort (pointer ?t1u0_123) ?p1u85_124)
   (c_sort (pointer ?t1u0_123) ?p2u84_125)))))))

;; Why axiom not_not_in_singleton
 :assumption
   (forall (?t1u0_126 c_type)
   (forall (?pu86_127 c_unsorted)
   (not
   (not_in_pset
   (c_sort (pointer ?t1u0_126) ?pu86_127) (c_sort
                                          (pset ?t1u0_126) (pset_singleton
                                                           (c_sort
                                                           (pointer
                                                           ?t1u0_126) ?pu86_127)))))))

;; Why axiom pset_union_intro
 :assumption
   (forall (?t1u0_128 c_type)
   (forall (?l1u89_129 c_unsorted)
   (forall (?l2u88_130 c_unsorted)
   (forall (?pu87_131 c_unsorted)
   (implies
   (and
   (not_in_pset
   (c_sort (pointer ?t1u0_128) ?pu87_131) (c_sort
                                          (pset ?t1u0_128) ?l1u89_129))
   (not_in_pset
   (c_sort (pointer ?t1u0_128) ?pu87_131) (c_sort
                                          (pset ?t1u0_128) ?l2u88_130)))
   (not_in_pset
   (c_sort (pointer ?t1u0_128) ?pu87_131) (c_sort
                                          (pset ?t1u0_128) (pset_union
                                                           (c_sort
                                                           (pset ?t1u0_128) ?l1u89_129) 
                                                           (c_sort
                                                           (pset ?t1u0_128) ?l2u88_130)))))))))

;; Why axiom pset_union_elim1
 :assumption
   (forall (?t1u0_132 c_type)
   (forall (?l1u92_133 c_unsorted)
   (forall (?l2u91_134 c_unsorted)
   (forall (?pu90_135 c_unsorted)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_132) ?pu90_135) (c_sort
                                          (pset ?t1u0_132) (pset_union
                                                           (c_sort
                                                           (pset ?t1u0_132) ?l1u92_133) 
                                                           (c_sort
                                                           (pset ?t1u0_132) ?l2u91_134))))
   (not_in_pset
   (c_sort (pointer ?t1u0_132) ?pu90_135) (c_sort
                                          (pset ?t1u0_132) ?l1u92_133)))))))

;; Why axiom pset_union_elim2
 :assumption
   (forall (?t1u0_136 c_type)
   (forall (?l1u95_137 c_unsorted)
   (forall (?l2u94_138 c_unsorted)
   (forall (?pu93_139 c_unsorted)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_136) ?pu93_139) (c_sort
                                          (pset ?t1u0_136) (pset_union
                                                           (c_sort
                                                           (pset ?t1u0_136) ?l1u95_137) 
                                                           (c_sort
                                                           (pset ?t1u0_136) ?l2u94_138))))
   (not_in_pset
   (c_sort (pointer ?t1u0_136) ?pu93_139) (c_sort
                                          (pset ?t1u0_136) ?l2u94_138)))))))

;; Why axiom pset_star_intro
 :assumption
   (forall (?t2u0_140 c_type)
   (forall (?t1u0_141 c_type)
   (forall (?lu99_142 c_unsorted)
   (forall (?mu98_143 c_unsorted)
   (forall (?pu97_144 c_unsorted)
   (implies
   (forall (?p1u96_145 c_unsorted)
   (implies
   (= (c_sort (pointer ?t2u0_140) ?pu97_144)
   (c_sort
   (pointer ?t2u0_140) (acc
                       (c_sort
                       (memory (pointer ?t2u0_140) ?t1u0_141) ?mu98_143) 
                       (c_sort (pointer ?t1u0_141) ?p1u96_145))))
   (not_in_pset
   (c_sort (pointer ?t1u0_141) ?p1u96_145) (c_sort
                                           (pset ?t1u0_141) ?lu99_142))))
   (not_in_pset
   (c_sort (pointer ?t2u0_140) ?pu97_144) (c_sort
                                          (pset ?t2u0_140) (pset_star
                                                           (c_sort
                                                           (pset ?t1u0_141) ?lu99_142) 
                                                           (c_sort
                                                           (memory
                                                           (pointer
                                                           ?t2u0_140) ?t1u0_141) ?mu98_143))))))))))

;; Why axiom pset_star_elim
 :assumption
   (forall (?t2u0_146 c_type)
   (forall (?t1u0_147 c_type)
   (forall (?lu103_148 c_unsorted)
   (forall (?mu102_149 c_unsorted)
   (forall (?pu101_150 c_unsorted)
   (implies
   (not_in_pset
   (c_sort (pointer ?t2u0_146) ?pu101_150) (c_sort
                                           (pset ?t2u0_146) (pset_star
                                                            (c_sort
                                                            (pset ?t1u0_147) ?lu103_148) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t2u0_146) ?t1u0_147) ?mu102_149))))
   (forall (?p1u100_151 c_unsorted)
   (implies
   (= (c_sort (pointer ?t2u0_146) ?pu101_150)
   (c_sort
   (pointer ?t2u0_146) (acc
                       (c_sort
                       (memory (pointer ?t2u0_146) ?t1u0_147) ?mu102_149) 
                       (c_sort (pointer ?t1u0_147) ?p1u100_151))))
   (not_in_pset
   (c_sort (pointer ?t1u0_147) ?p1u100_151) (c_sort
                                            (pset ?t1u0_147) ?lu103_148))))))))))

;; Why axiom pset_all_intro
 :assumption
   (forall (?t1u0_152 c_type)
   (forall (?pu106_153 c_unsorted)
   (forall (?lu105_154 c_unsorted)
   (implies
   (forall (?p1u104_155 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t1u0_152) ?p1u104_155) (c_sort
                                            (pset ?t1u0_152) ?lu105_154)))
   (not (= (c_sort
           (addr ?t1u0_152) (base_addr
                            (c_sort (pointer ?t1u0_152) ?pu106_153)))
   (c_sort
   (addr ?t1u0_152) (base_addr (c_sort (pointer ?t1u0_152) ?p1u104_155)))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_152) ?pu106_153) (c_sort
                                           (pset ?t1u0_152) (pset_all
                                                            (c_sort
                                                            (pset ?t1u0_152) ?lu105_154))))))))

;; Why axiom pset_all_elim
 :assumption
   (forall (?t1u0_156 c_type)
   (forall (?pu109_157 c_unsorted)
   (forall (?lu108_158 c_unsorted)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_156) ?pu109_157) (c_sort
                                           (pset ?t1u0_156) (pset_all
                                                            (c_sort
                                                            (pset ?t1u0_156) ?lu108_158))))
   (forall (?p1u107_159 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t1u0_156) ?p1u107_159) (c_sort
                                            (pset ?t1u0_156) ?lu108_158)))
   (not (= (c_sort
           (addr ?t1u0_156) (base_addr
                            (c_sort (pointer ?t1u0_156) ?pu109_157)))
   (c_sort
   (addr ?t1u0_156) (base_addr (c_sort (pointer ?t1u0_156) ?p1u107_159)))))))))))

;; Why axiom pset_range_intro
 :assumption
   (forall (?t1u0_160 c_type)
   (forall (?pu115_161 c_unsorted)
   (forall (?lu114_162 c_unsorted)
   (forall (?au113_163 Int)
   (forall (?bu112_164 Int)
   (implies
   (forall (?p1u111_165 c_unsorted)
   (or
   (not_in_pset
   (c_sort (pointer ?t1u0_160) ?p1u111_165) (c_sort
                                            (pset ?t1u0_160) ?lu114_162))
   (forall (?iu110_166 Int)
   (implies (and (<= ?au113_163 ?iu110_166) (<= ?iu110_166 ?bu112_164))
   (not (= (c_sort (pointer ?t1u0_160) ?pu115_161)
   (c_sort
   (pointer ?t1u0_160) (shift
                       (c_sort (pointer ?t1u0_160) ?p1u111_165) ?iu110_166))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_160) ?pu115_161) (c_sort
                                           (pset ?t1u0_160) (pset_range
                                                            (c_sort
                                                            (pset ?t1u0_160) ?lu114_162) ?au113_163 ?bu112_164)))))))))

;; Why axiom pset_range_elim
 :assumption
   (forall (?t1u0_167 c_type)
   (forall (?pu121_168 c_unsorted)
   (forall (?lu120_169 c_unsorted)
   (forall (?au119_170 Int)
   (forall (?bu118_171 Int)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_167) ?pu121_168) (c_sort
                                           (pset ?t1u0_167) (pset_range
                                                            (c_sort
                                                            (pset ?t1u0_167) ?lu120_169) ?au119_170 ?bu118_171)))
   (forall (?p1u117_172 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t1u0_167) ?p1u117_172) (c_sort
                                            (pset ?t1u0_167) ?lu120_169)))
   (forall (?iu116_173 Int)
   (implies (and (<= ?au119_170 ?iu116_173) (<= ?iu116_173 ?bu118_171))
   (not (= (c_sort
           (pointer ?t1u0_167) (shift
                               (c_sort (pointer ?t1u0_167) ?p1u117_172) ?iu116_173))
   (c_sort (pointer ?t1u0_167) ?pu121_168)))))))))))))

;; Why axiom pset_range_left_intro
 :assumption
   (forall (?t1u0_174 c_type)
   (forall (?pu126_175 c_unsorted)
   (forall (?lu125_176 c_unsorted)
   (forall (?au124_177 Int)
   (implies
   (forall (?p1u123_178 c_unsorted)
   (or
   (not_in_pset
   (c_sort (pointer ?t1u0_174) ?p1u123_178) (c_sort
                                            (pset ?t1u0_174) ?lu125_176))
   (forall (?iu122_179 Int)
   (implies (<= ?iu122_179 ?au124_177)
   (not (= (c_sort (pointer ?t1u0_174) ?pu126_175)
   (c_sort
   (pointer ?t1u0_174) (shift
                       (c_sort (pointer ?t1u0_174) ?p1u123_178) ?iu122_179))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_174) ?pu126_175) (c_sort
                                           (pset ?t1u0_174) (pset_range_left
                                                            (c_sort
                                                            (pset ?t1u0_174) ?lu125_176) ?au124_177))))))))

;; Why axiom pset_range_left_elim
 :assumption
   (forall (?t1u0_180 c_type)
   (forall (?pu131_181 c_unsorted)
   (forall (?lu130_182 c_unsorted)
   (forall (?au129_183 Int)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_180) ?pu131_181) (c_sort
                                           (pset ?t1u0_180) (pset_range_left
                                                            (c_sort
                                                            (pset ?t1u0_180) ?lu130_182) ?au129_183)))
   (forall (?p1u128_184 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t1u0_180) ?p1u128_184) (c_sort
                                            (pset ?t1u0_180) ?lu130_182)))
   (forall (?iu127_185 Int)
   (implies (<= ?iu127_185 ?au129_183)
   (not (= (c_sort
           (pointer ?t1u0_180) (shift
                               (c_sort (pointer ?t1u0_180) ?p1u128_184) ?iu127_185))
   (c_sort (pointer ?t1u0_180) ?pu131_181))))))))))))

;; Why axiom pset_range_right_intro
 :assumption
   (forall (?t1u0_186 c_type)
   (forall (?pu136_187 c_unsorted)
   (forall (?lu135_188 c_unsorted)
   (forall (?au134_189 Int)
   (implies
   (forall (?p1u133_190 c_unsorted)
   (or
   (not_in_pset
   (c_sort (pointer ?t1u0_186) ?p1u133_190) (c_sort
                                            (pset ?t1u0_186) ?lu135_188))
   (forall (?iu132_191 Int)
   (implies (<= ?au134_189 ?iu132_191)
   (not (= (c_sort (pointer ?t1u0_186) ?pu136_187)
   (c_sort
   (pointer ?t1u0_186) (shift
                       (c_sort (pointer ?t1u0_186) ?p1u133_190) ?iu132_191))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_186) ?pu136_187) (c_sort
                                           (pset ?t1u0_186) (pset_range_right
                                                            (c_sort
                                                            (pset ?t1u0_186) ?lu135_188) ?au134_189))))))))

;; Why axiom pset_range_right_elim
 :assumption
   (forall (?t1u0_192 c_type)
   (forall (?pu141_193 c_unsorted)
   (forall (?lu140_194 c_unsorted)
   (forall (?au139_195 Int)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_192) ?pu141_193) (c_sort
                                           (pset ?t1u0_192) (pset_range_right
                                                            (c_sort
                                                            (pset ?t1u0_192) ?lu140_194) ?au139_195)))
   (forall (?p1u138_196 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t1u0_192) ?p1u138_196) (c_sort
                                            (pset ?t1u0_192) ?lu140_194)))
   (forall (?iu137_197 Int)
   (implies (<= ?au139_195 ?iu137_197)
   (not (= (c_sort
           (pointer ?t1u0_192) (shift
                               (c_sort (pointer ?t1u0_192) ?p1u138_196) ?iu137_197))
   (c_sort (pointer ?t1u0_192) ?pu141_193))))))))))))

;; Why axiom pset_acc_all_intro
 :assumption
   (forall (?t2u0_198 c_type)
   (forall (?t1u0_199 c_type)
   (forall (?pu146_200 c_unsorted)
   (forall (?lu145_201 c_unsorted)
   (forall (?mu144_202 c_unsorted)
   (implies
   (forall (?p1u143_203 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_198) ?p1u143_203) (c_sort
                                            (pset ?t2u0_198) ?lu145_201)))
   (forall (?iu142_204 Int)
   (not (= (c_sort (pointer ?t1u0_199) ?pu146_200)
   (c_sort
   (pointer ?t1u0_199) (acc
                       (c_sort
                       (memory (pointer ?t1u0_199) ?t2u0_198) ?mu144_202) 
                       (c_sort
                       (pointer ?t2u0_198) (shift
                                           (c_sort
                                           (pointer ?t2u0_198) ?p1u143_203) ?iu142_204)))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_199) ?pu146_200) (c_sort
                                           (pset ?t1u0_199) (pset_acc_all
                                                            (c_sort
                                                            (pset ?t2u0_198) ?lu145_201) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_199) ?t2u0_198) ?mu144_202))))))))))

;; Why axiom pset_acc_all_elim
 :assumption
   (forall (?t2u0_205 c_type)
   (forall (?t1u0_206 c_type)
   (forall (?pu151_207 c_unsorted)
   (forall (?lu150_208 c_unsorted)
   (forall (?mu149_209 c_unsorted)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_206) ?pu151_207) (c_sort
                                           (pset ?t1u0_206) (pset_acc_all
                                                            (c_sort
                                                            (pset ?t2u0_205) ?lu150_208) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_206) ?t2u0_205) ?mu149_209))))
   (forall (?p1u148_210 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_205) ?p1u148_210) (c_sort
                                            (pset ?t2u0_205) ?lu150_208)))
   (forall (?iu147_211 Int)
   (not (= (c_sort
           (pointer ?t1u0_206) (acc
                               (c_sort
                               (memory (pointer ?t1u0_206) ?t2u0_205) ?mu149_209) 
                               (c_sort
                               (pointer ?t2u0_205) (shift
                                                   (c_sort
                                                   (pointer ?t2u0_205) ?p1u148_210) ?iu147_211))))
   (c_sort (pointer ?t1u0_206) ?pu151_207))))))))))))

;; Why axiom pset_acc_range_intro
 :assumption
   (forall (?t2u0_212 c_type)
   (forall (?t1u0_213 c_type)
   (forall (?pu158_214 c_unsorted)
   (forall (?lu157_215 c_unsorted)
   (forall (?mu156_216 c_unsorted)
   (forall (?au155_217 Int)
   (forall (?bu154_218 Int)
   (implies
   (forall (?p1u153_219 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_212) ?p1u153_219) (c_sort
                                            (pset ?t2u0_212) ?lu157_215)))
   (forall (?iu152_220 Int)
   (implies (and (<= ?au155_217 ?iu152_220) (<= ?iu152_220 ?bu154_218))
   (not (= (c_sort (pointer ?t1u0_213) ?pu158_214)
   (c_sort
   (pointer ?t1u0_213) (acc
                       (c_sort
                       (memory (pointer ?t1u0_213) ?t2u0_212) ?mu156_216) 
                       (c_sort
                       (pointer ?t2u0_212) (shift
                                           (c_sort
                                           (pointer ?t2u0_212) ?p1u153_219) ?iu152_220))))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_213) ?pu158_214) (c_sort
                                           (pset ?t1u0_213) (pset_acc_range
                                                            (c_sort
                                                            (pset ?t2u0_212) ?lu157_215) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_213) ?t2u0_212) ?mu156_216) ?au155_217 ?bu154_218)))))))))))

;; Why axiom pset_acc_range_elim
 :assumption
   (forall (?t2u0_221 c_type)
   (forall (?t1u0_222 c_type)
   (forall (?pu165_223 c_unsorted)
   (forall (?lu164_224 c_unsorted)
   (forall (?mu163_225 c_unsorted)
   (forall (?au162_226 Int)
   (forall (?bu161_227 Int)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_222) ?pu165_223) (c_sort
                                           (pset ?t1u0_222) (pset_acc_range
                                                            (c_sort
                                                            (pset ?t2u0_221) ?lu164_224) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_222) ?t2u0_221) ?mu163_225) ?au162_226 ?bu161_227)))
   (forall (?p1u160_228 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_221) ?p1u160_228) (c_sort
                                            (pset ?t2u0_221) ?lu164_224)))
   (forall (?iu159_229 Int)
   (implies (and (<= ?au162_226 ?iu159_229) (<= ?iu159_229 ?bu161_227))
   (not (= (c_sort
           (pointer ?t1u0_222) (acc
                               (c_sort
                               (memory (pointer ?t1u0_222) ?t2u0_221) ?mu163_225) 
                               (c_sort
                               (pointer ?t2u0_221) (shift
                                                   (c_sort
                                                   (pointer ?t2u0_221) ?p1u160_228) ?iu159_229))))
   (c_sort (pointer ?t1u0_222) ?pu165_223)))))))))))))))

;; Why axiom pset_acc_range_left_intro
 :assumption
   (forall (?t2u0_230 c_type)
   (forall (?t1u0_231 c_type)
   (forall (?pu171_232 c_unsorted)
   (forall (?lu170_233 c_unsorted)
   (forall (?mu169_234 c_unsorted)
   (forall (?au168_235 Int)
   (implies
   (forall (?p1u167_236 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_230) ?p1u167_236) (c_sort
                                            (pset ?t2u0_230) ?lu170_233)))
   (forall (?iu166_237 Int)
   (implies (<= ?iu166_237 ?au168_235)
   (not (= (c_sort (pointer ?t1u0_231) ?pu171_232)
   (c_sort
   (pointer ?t1u0_231) (acc
                       (c_sort
                       (memory (pointer ?t1u0_231) ?t2u0_230) ?mu169_234) 
                       (c_sort
                       (pointer ?t2u0_230) (shift
                                           (c_sort
                                           (pointer ?t2u0_230) ?p1u167_236) ?iu166_237))))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_231) ?pu171_232) (c_sort
                                           (pset ?t1u0_231) (pset_acc_range_left
                                                            (c_sort
                                                            (pset ?t2u0_230) ?lu170_233) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_231) ?t2u0_230) ?mu169_234) ?au168_235))))))))))

;; Why axiom pset_acc_range_left_elim
 :assumption
   (forall (?t2u0_238 c_type)
   (forall (?t1u0_239 c_type)
   (forall (?pu177_240 c_unsorted)
   (forall (?lu176_241 c_unsorted)
   (forall (?mu175_242 c_unsorted)
   (forall (?au174_243 Int)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_239) ?pu177_240) (c_sort
                                           (pset ?t1u0_239) (pset_acc_range_left
                                                            (c_sort
                                                            (pset ?t2u0_238) ?lu176_241) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_239) ?t2u0_238) ?mu175_242) ?au174_243)))
   (forall (?p1u173_244 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_238) ?p1u173_244) (c_sort
                                            (pset ?t2u0_238) ?lu176_241)))
   (forall (?iu172_245 Int)
   (implies (<= ?iu172_245 ?au174_243)
   (not (= (c_sort
           (pointer ?t1u0_239) (acc
                               (c_sort
                               (memory (pointer ?t1u0_239) ?t2u0_238) ?mu175_242) 
                               (c_sort
                               (pointer ?t2u0_238) (shift
                                                   (c_sort
                                                   (pointer ?t2u0_238) ?p1u173_244) ?iu172_245))))
   (c_sort (pointer ?t1u0_239) ?pu177_240))))))))))))))

;; Why axiom pset_acc_range_right_intro
 :assumption
   (forall (?t2u0_246 c_type)
   (forall (?t1u0_247 c_type)
   (forall (?pu183_248 c_unsorted)
   (forall (?lu182_249 c_unsorted)
   (forall (?mu181_250 c_unsorted)
   (forall (?au180_251 Int)
   (implies
   (forall (?p1u179_252 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_246) ?p1u179_252) (c_sort
                                            (pset ?t2u0_246) ?lu182_249)))
   (forall (?iu178_253 Int)
   (implies (<= ?au180_251 ?iu178_253)
   (not (= (c_sort (pointer ?t1u0_247) ?pu183_248)
   (c_sort
   (pointer ?t1u0_247) (acc
                       (c_sort
                       (memory (pointer ?t1u0_247) ?t2u0_246) ?mu181_250) 
                       (c_sort
                       (pointer ?t2u0_246) (shift
                                           (c_sort
                                           (pointer ?t2u0_246) ?p1u179_252) ?iu178_253))))))))))
   (not_in_pset
   (c_sort (pointer ?t1u0_247) ?pu183_248) (c_sort
                                           (pset ?t1u0_247) (pset_acc_range_right
                                                            (c_sort
                                                            (pset ?t2u0_246) ?lu182_249) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_247) ?t2u0_246) ?mu181_250) ?au180_251))))))))))

;; Why axiom pset_acc_range_right_elim
 :assumption
   (forall (?t2u0_254 c_type)
   (forall (?t1u0_255 c_type)
   (forall (?pu189_256 c_unsorted)
   (forall (?lu188_257 c_unsorted)
   (forall (?mu187_258 c_unsorted)
   (forall (?au186_259 Int)
   (implies
   (not_in_pset
   (c_sort (pointer ?t1u0_255) ?pu189_256) (c_sort
                                           (pset ?t1u0_255) (pset_acc_range_right
                                                            (c_sort
                                                            (pset ?t2u0_254) ?lu188_257) 
                                                            (c_sort
                                                            (memory
                                                            (pointer
                                                            ?t1u0_255) ?t2u0_254) ?mu187_258) ?au186_259)))
   (forall (?p1u185_260 c_unsorted)
   (implies
   (not
   (not_in_pset
   (c_sort (pointer ?t2u0_254) ?p1u185_260) (c_sort
                                            (pset ?t2u0_254) ?lu188_257)))
   (forall (?iu184_261 Int)
   (implies (<= ?au186_259 ?iu184_261)
   (not (= (c_sort
           (pointer ?t1u0_255) (acc
                               (c_sort
                               (memory (pointer ?t1u0_255) ?t2u0_254) ?mu187_258) 
                               (c_sort
                               (pointer ?t2u0_254) (shift
                                                   (c_sort
                                                   (pointer ?t2u0_254) ?p1u185_260) ?iu184_261))))
   (c_sort (pointer ?t1u0_255) ?pu189_256))))))))))))))

;; Why axiom not_assigns_trans
 :assumption
   (forall (?t2u0_262 c_type)
   (forall (?t1u0_263 c_type)
   (forall (?au194_264 c_unsorted)
   (forall (?lu193_265 c_unsorted)
   (forall (?m1u192_266 c_unsorted)
   (forall (?m2u191_267 c_unsorted)
   (forall (?m3u190_268 c_unsorted)
   (implies
   (not_assigns
   (c_sort alloc_table ?au194_264) (c_sort
                                   (memory ?t2u0_262 ?t1u0_263) ?m1u192_266) 
   (c_sort (memory ?t2u0_262 ?t1u0_263) ?m2u191_267) (c_sort
                                                     (pset ?t1u0_263) ?lu193_265))
   (implies
   (not_assigns
   (c_sort alloc_table ?au194_264) (c_sort
                                   (memory ?t2u0_262 ?t1u0_263) ?m2u191_267) 
   (c_sort (memory ?t2u0_262 ?t1u0_263) ?m3u190_268) (c_sort
                                                     (pset ?t1u0_263) ?lu193_265))
   (not_assigns
   (c_sort alloc_table ?au194_264) (c_sort
                                   (memory ?t2u0_262 ?t1u0_263) ?m1u192_266) 
   (c_sort (memory ?t2u0_262 ?t1u0_263) ?m3u190_268) (c_sort
                                                     (pset ?t1u0_263) ?lu193_265)))))))))))

;; Why axiom not_assigns_refl
 :assumption
   (forall (?t2u0_269 c_type)
   (forall (?t1u0_270 c_type)
   (forall (?au197_271 c_unsorted)
   (forall (?lu196_272 c_unsorted)
   (forall (?mu195_273 c_unsorted)
   (not_assigns
   (c_sort alloc_table ?au197_271) (c_sort
                                   (memory ?t2u0_269 ?t1u0_270) ?mu195_273) 
   (c_sort (memory ?t2u0_269 ?t1u0_270) ?mu195_273) (c_sort
                                                    (pset ?t1u0_270) ?lu196_272)))))))

;;;; Why logic valid_acc
:extrapreds ((valid_acc c_sorted))

;; Why axiom valid_acc_def
 :assumption
   (forall (?t2u0_274 c_type)
   (forall (?t1u0_275 c_type)
   (forall (?m1u200_276 c_unsorted)
   (iff
   (valid_acc (c_sort (memory (pointer ?t1u0_275) ?t2u0_274) ?m1u200_276))
   (forall (?pu199_277 c_unsorted)
   (forall (?au198_278 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?au198_278) (c_sort (pointer ?t2u0_274) ?pu199_277))
   (valid
   (c_sort alloc_table ?au198_278) (c_sort
                                   (pointer ?t1u0_275) (acc
                                                       (c_sort
                                                       (memory
                                                       (pointer ?t1u0_275) ?t2u0_274) ?m1u200_276) 
                                                       (c_sort
                                                       (pointer ?t2u0_274) ?pu199_277)))))))))))

;;;; Why logic valid_acc_range
:extrapreds ((valid_acc_range c_sorted Int))

;; Why axiom valid_acc_range_def
 :assumption
   (forall (?t2u0_279 c_type)
   (forall (?t1u0_280 c_type)
   (forall (?m1u204_281 c_unsorted)
   (forall (?sizeu203_282 Int)
   (iff
   (valid_acc_range
   (c_sort (memory (pointer ?t1u0_280) ?t2u0_279) ?m1u204_281) ?sizeu203_282)
   (forall (?pu202_283 c_unsorted)
   (forall (?au201_284 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?au201_284) (c_sort (pointer ?t2u0_279) ?pu202_283))
   (valid_range
   (c_sort alloc_table ?au201_284) (c_sort
                                   (pointer ?t1u0_280) (acc
                                                       (c_sort
                                                       (memory
                                                       (pointer ?t1u0_280) ?t2u0_279) ?m1u204_281) 
                                                       (c_sort
                                                       (pointer ?t2u0_279) ?pu202_283))) 0 
   (- ?sizeu203_282 1))))))))))

;; Why axiom valid_acc_range_valid
 :assumption
   (forall (?t2u0_285 c_type)
   (forall (?t1u0_286 c_type)
   (forall (?m1u208_287 c_unsorted)
   (forall (?sizeu207_288 Int)
   (forall (?pu206_289 c_unsorted)
   (forall (?au205_290 c_unsorted)
   (implies
   (valid_acc_range
   (c_sort (memory (pointer ?t1u0_286) ?t2u0_285) ?m1u208_287) ?sizeu207_288)
   (implies
   (valid
   (c_sort alloc_table ?au205_290) (c_sort (pointer ?t2u0_285) ?pu206_289))
   (valid
   (c_sort alloc_table ?au205_290) (c_sort
                                   (pointer ?t1u0_286) (acc
                                                       (c_sort
                                                       (memory
                                                       (pointer ?t1u0_286) ?t2u0_285) ?m1u208_287) 
                                                       (c_sort
                                                       (pointer ?t2u0_285) ?pu206_289))))))))))))

;;;; Why logic separation1
:extrapreds ((separation1 c_sorted c_sorted))

;; Why axiom separation1_def
 :assumption
   (forall (?t2u0_291 c_type)
   (forall (?t1u0_292 c_type)
   (forall (?m1u212_293 c_unsorted)
   (forall (?m2u211_294 c_unsorted)
   (iff
   (separation1
   (c_sort (memory (pointer ?t1u0_292) ?t2u0_291) ?m1u212_293) (c_sort
                                                               (memory
                                                               (pointer
                                                               ?t1u0_292) ?t2u0_291) ?m2u211_294))
   (forall (?pu210_295 c_unsorted)
   (forall (?au209_296 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?au209_296) (c_sort (pointer ?t2u0_291) ?pu210_295))
   (not (= (c_sort
           (addr ?t1u0_292) (base_addr
                            (c_sort
                            (pointer ?t1u0_292) (acc
                                                (c_sort
                                                (memory
                                                (pointer ?t1u0_292) ?t2u0_291) ?m1u212_293) 
                                                (c_sort
                                                (pointer ?t2u0_291) ?pu210_295)))))
   (c_sort
   (addr ?t1u0_292) (base_addr
                    (c_sort
                    (pointer ?t1u0_292) (acc
                                        (c_sort
                                        (memory
                                        (pointer ?t1u0_292) ?t2u0_291) ?m2u211_294) 
                                        (c_sort
                                        (pointer ?t2u0_291) ?pu210_295)))))))))))))))

;;;; Why logic separation1_range1
:extrapreds ((separation1_range1 c_sorted c_sorted Int))

;; Why axiom separation1_range1_def
 :assumption
   (forall (?t2u0_297 c_type)
   (forall (?t1u0_298 c_type)
   (forall (?m1u219_299 c_unsorted)
   (forall (?m2u218_300 c_unsorted)
   (forall (?sizeu217_301 Int)
   (iff
   (separation1_range1
   (c_sort (memory (pointer ?t1u0_298) ?t2u0_297) ?m1u219_299) (c_sort
                                                               (memory
                                                               (pointer
                                                               ?t1u0_298) ?t2u0_297) ?m2u218_300) ?sizeu217_301)
   (forall (?pu216_302 c_unsorted)
   (forall (?au215_303 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?au215_303) (c_sort (pointer ?t2u0_297) ?pu216_302))
   (forall (?i1u214_304 Int)
   (forall (?i2u213_305 Int)
   (implies (and (<= 0 ?i1u214_304) (< ?i1u214_304 ?sizeu217_301))
   (implies (and (<= 0 ?i2u213_305) (< ?i2u213_305 ?sizeu217_301))
   (not (= (c_sort
           (addr ?t1u0_298) (base_addr
                            (c_sort
                            (pointer ?t1u0_298) (acc
                                                (c_sort
                                                (memory
                                                (pointer ?t1u0_298) ?t2u0_297) ?m1u219_299) 
                                                (c_sort
                                                (pointer ?t2u0_297) (shift
                                                                    (c_sort
                                                                    (pointer
                                                                    ?t2u0_297) ?pu216_302) ?i1u214_304))))))
   (c_sort
   (addr ?t1u0_298) (base_addr
                    (c_sort
                    (pointer ?t1u0_298) (acc
                                        (c_sort
                                        (memory
                                        (pointer ?t1u0_298) ?t2u0_297) ?m2u218_300) 
                                        (c_sort
                                        (pointer ?t2u0_297) (shift
                                                            (c_sort
                                                            (pointer
                                                            ?t2u0_297) ?pu216_302) ?i2u213_305)))))))))))))))))))))

;;;; Why logic separation1_range
:extrapreds ((separation1_range c_sorted Int))

;; Why axiom separation1_range_def
 :assumption
   (forall (?t2u0_306 c_type)
   (forall (?t1u0_307 c_type)
   (forall (?mu224_308 c_unsorted)
   (forall (?sizeu223_309 Int)
   (iff
   (separation1_range
   (c_sort (memory (pointer ?t1u0_307) ?t2u0_306) ?mu224_308) ?sizeu223_309)
   (forall (?pu222_310 c_unsorted)
   (forall (?au221_311 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?au221_311) (c_sort (pointer ?t2u0_306) ?pu222_310))
   (forall (?i1u220_312 Int)
   (implies (and (<= 0 ?i1u220_312) (< ?i1u220_312 ?sizeu223_309))
   (not (= (c_sort
           (addr ?t1u0_307) (base_addr
                            (c_sort
                            (pointer ?t1u0_307) (acc
                                                (c_sort
                                                (memory
                                                (pointer ?t1u0_307) ?t2u0_306) ?mu224_308) 
                                                (c_sort
                                                (pointer ?t2u0_306) (shift
                                                                    (c_sort
                                                                    (pointer
                                                                    ?t2u0_306) ?pu222_310) ?i1u220_312))))))
   (c_sort
   (addr ?t1u0_307) (base_addr
                    (c_sort
                    (pointer ?t1u0_307) (acc
                                        (c_sort
                                        (memory
                                        (pointer ?t1u0_307) ?t2u0_306) ?mu224_308) 
                                        (c_sort
                                        (pointer ?t2u0_306) ?pu222_310)))))))))))))))))

;;;; Why logic separation2
:extrapreds ((separation2 c_sorted c_sorted))

;; Why axiom separation2_def
 :assumption
   (forall (?t2u0_313 c_type)
   (forall (?t1u0_314 c_type)
   (forall (?m1u228_315 c_unsorted)
   (forall (?m2u227_316 c_unsorted)
   (iff
   (separation2
   (c_sort (memory (pointer ?t1u0_314) ?t2u0_313) ?m1u228_315) (c_sort
                                                               (memory
                                                               (pointer
                                                               ?t1u0_314) ?t2u0_313) ?m2u227_316))
   (forall (?p1u226_317 c_unsorted)
   (forall (?p2u225_318 c_unsorted)
   (implies
   (not (= (c_sort (pointer ?t2u0_313) ?p1u226_317)
   (c_sort (pointer ?t2u0_313) ?p2u225_318)))
   (not (= (c_sort
           (addr ?t1u0_314) (base_addr
                            (c_sort
                            (pointer ?t1u0_314) (acc
                                                (c_sort
                                                (memory
                                                (pointer ?t1u0_314) ?t2u0_313) ?m1u228_315) 
                                                (c_sort
                                                (pointer ?t2u0_313) ?p1u226_317)))))
   (c_sort
   (addr ?t1u0_314) (base_addr
                    (c_sort
                    (pointer ?t1u0_314) (acc
                                        (c_sort
                                        (memory
                                        (pointer ?t1u0_314) ?t2u0_313) ?m2u227_316) 
                                        (c_sort
                                        (pointer ?t2u0_313) ?p2u225_318)))))))))))))))

;;;; Why logic separation2_range1
:extrapreds ((separation2_range1 c_sorted c_sorted Int))

;; Why axiom separation2_range1_def
 :assumption
   (forall (?t2u0_319 c_type)
   (forall (?t1u0_320 c_type)
   (forall (?m1u235_321 c_unsorted)
   (forall (?m2u234_322 c_unsorted)
   (forall (?sizeu233_323 Int)
   (iff
   (separation2_range1
   (c_sort (memory (pointer ?t1u0_320) ?t2u0_319) ?m1u235_321) (c_sort
                                                               (memory
                                                               (pointer
                                                               ?t1u0_320) ?t2u0_319) ?m2u234_322) ?sizeu233_323)
   (forall (?pu232_324 c_unsorted)
   (forall (?qu231_325 c_unsorted)
   (forall (?au230_326 c_unsorted)
   (forall (?iu229_327 Int)
   (implies (and (<= 0 ?iu229_327) (< ?iu229_327 ?sizeu233_323))
   (not (= (c_sort
           (addr ?t1u0_320) (base_addr
                            (c_sort
                            (pointer ?t1u0_320) (acc
                                                (c_sort
                                                (memory
                                                (pointer ?t1u0_320) ?t2u0_319) ?m1u235_321) 
                                                (c_sort
                                                (pointer ?t2u0_319) (shift
                                                                    (c_sort
                                                                    (pointer
                                                                    ?t2u0_319) ?pu232_324) ?iu229_327))))))
   (c_sort
   (addr ?t1u0_320) (base_addr
                    (c_sort
                    (pointer ?t1u0_320) (acc
                                        (c_sort
                                        (memory
                                        (pointer ?t1u0_320) ?t2u0_319) ?m2u234_322) 
                                        (c_sort
                                        (pointer ?t2u0_319) ?qu231_325))))))))))))))))))

;;;; Why logic on_heap
:extrapreds ((on_heap c_sorted c_sorted))

;;;; Why logic on_stack
:extrapreds ((on_stack c_sorted c_sorted))

;;;; Why logic fresh
:extrapreds ((fresh c_sorted c_sorted))

;; Why axiom fresh_not_valid
 :assumption
   (forall (?t1u0_328 c_type)
   (forall (?au237_329 c_unsorted)
   (forall (?pu236_330 c_unsorted)
   (implies
   (fresh
   (c_sort alloc_table ?au237_329) (c_sort (pointer ?t1u0_328) ?pu236_330))
   (not
   (valid
   (c_sort alloc_table ?au237_329) (c_sort (pointer ?t1u0_328) ?pu236_330)))))))

;; Why axiom fresh_not_valid_shift
 :assumption
   (forall (?t1u0_331 c_type)
   (forall (?au240_332 c_unsorted)
   (forall (?pu239_333 c_unsorted)
   (implies
   (fresh
   (c_sort alloc_table ?au240_332) (c_sort (pointer ?t1u0_331) ?pu239_333))
   (forall (?iu238_334 Int)
   (not
   (valid
   (c_sort alloc_table ?au240_332) (c_sort
                                   (pointer ?t1u0_331) (shift
                                                       (c_sort
                                                       (pointer ?t1u0_331) ?pu239_333) ?iu238_334)))))))))

;;;; Why logic alloc_extends
:extrapreds ((alloc_extends c_sorted c_sorted))

;; Why axiom alloc_extends_valid
 :assumption
   (forall (?t1u0_335 c_type)
   (forall (?a1u243_336 c_unsorted)
   (forall (?a2u242_337 c_unsorted)
   (implies
   (alloc_extends
   (c_sort alloc_table ?a1u243_336) (c_sort alloc_table ?a2u242_337))
   (forall (?qu241_338 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?a1u243_336) (c_sort (pointer ?t1u0_335) ?qu241_338))
   (valid
   (c_sort alloc_table ?a2u242_337) (c_sort (pointer ?t1u0_335) ?qu241_338))))))))

;; Why axiom alloc_extends_valid_index
 :assumption
   (forall (?t1u0_339 c_type)
   (forall (?a1u247_340 c_unsorted)
   (forall (?a2u246_341 c_unsorted)
   (implies
   (alloc_extends
   (c_sort alloc_table ?a1u247_340) (c_sort alloc_table ?a2u246_341))
   (forall (?qu245_342 c_unsorted)
   (forall (?iu244_343 Int)
   (implies
   (valid_index
   (c_sort alloc_table ?a1u247_340) (c_sort (pointer ?t1u0_339) ?qu245_342) ?iu244_343)
   (valid_index
   (c_sort alloc_table ?a2u246_341) (c_sort (pointer ?t1u0_339) ?qu245_342) ?iu244_343))))))))

;; Why axiom alloc_extends_valid_range
 :assumption
   (forall (?t1u0_344 c_type)
   (forall (?a1u252_345 c_unsorted)
   (forall (?a2u251_346 c_unsorted)
   (implies
   (alloc_extends
   (c_sort alloc_table ?a1u252_345) (c_sort alloc_table ?a2u251_346))
   (forall (?qu250_347 c_unsorted)
   (forall (?iu249_348 Int)
   (forall (?ju248_349 Int)
   (implies
   (valid_range
   (c_sort alloc_table ?a1u252_345) (c_sort (pointer ?t1u0_344) ?qu250_347) ?iu249_348 ?ju248_349)
   (valid_range
   (c_sort alloc_table ?a2u251_346) (c_sort (pointer ?t1u0_344) ?qu250_347) ?iu249_348 ?ju248_349)))))))))

;; Why axiom alloc_extends_refl
 :assumption
   (forall (?au253_350 c_unsorted)
   (alloc_extends
   (c_sort alloc_table ?au253_350) (c_sort alloc_table ?au253_350)))

;; Why axiom alloc_extends_trans
 :assumption
   (forall (?a1u256_351 c_unsorted)
   (forall (?a2u255_352 c_unsorted)
   (forall (?a3u254_353 c_unsorted)
   (implies
   (alloc_extends
   (c_sort alloc_table ?a1u256_351) (c_sort alloc_table ?a2u255_352))
   (implies
   (alloc_extends
   (c_sort alloc_table ?a2u255_352) (c_sort alloc_table ?a3u254_353))
   (alloc_extends
   (c_sort alloc_table ?a1u256_351) (c_sort alloc_table ?a3u254_353)))))))

;;;; Why logic free_stack
:extrapreds ((free_stack c_sorted c_sorted c_sorted))

;; Why axiom free_stack_heap
 :assumption
   (forall (?t1u0_354 c_type)
   (forall (?a1u260_355 c_unsorted)
   (forall (?a2u259_356 c_unsorted)
   (forall (?a3u258_357 c_unsorted)
   (implies
   (free_stack
   (c_sort alloc_table ?a1u260_355) (c_sort alloc_table ?a2u259_356) 
   (c_sort alloc_table ?a3u258_357))
   (forall (?pu257_358 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?a2u259_356) (c_sort (pointer ?t1u0_354) ?pu257_358))
   (implies
   (on_heap
   (c_sort alloc_table ?a2u259_356) (c_sort (pointer ?t1u0_354) ?pu257_358))
   (valid
   (c_sort alloc_table ?a3u258_357) (c_sort (pointer ?t1u0_354) ?pu257_358))))))))))

;; Why axiom free_stack_stack
 :assumption
   (forall (?t1u0_359 c_type)
   (forall (?a1u264_360 c_unsorted)
   (forall (?a2u263_361 c_unsorted)
   (forall (?a3u262_362 c_unsorted)
   (implies
   (free_stack
   (c_sort alloc_table ?a1u264_360) (c_sort alloc_table ?a2u263_361) 
   (c_sort alloc_table ?a3u262_362))
   (forall (?pu261_363 c_unsorted)
   (implies
   (valid
   (c_sort alloc_table ?a1u264_360) (c_sort (pointer ?t1u0_359) ?pu261_363))
   (implies
   (on_stack
   (c_sort alloc_table ?a1u264_360) (c_sort (pointer ?t1u0_359) ?pu261_363))
   (valid
   (c_sort alloc_table ?a3u262_362) (c_sort (pointer ?t1u0_359) ?pu261_363))))))))))

;;;; Why logic null
:extrafuns ((null  c_unsorted))

;; Why axiom null_not_valid
 :assumption
   (forall (?t1u0_364 c_type)
   (forall (?au265_365 c_unsorted)
   (not
   (valid (c_sort alloc_table ?au265_365) (c_sort (pointer ?t1u0_364) null)))))

;;;; Why logic t_0
:extrafuns ((t_0  c_type))

;;;; Why logic t_1
:extrafuns ((t_1  c_type))

;;;; Why logic t_2
:extrafuns ((t_2  c_type))

;; Why axiom mean_1
 :assumption
   (forall (?xu267_366 Int)
   (forall (?yu266_367 Int)
   (implies (<= ?xu267_366 ?yu266_367)
   (and (<= ?xu267_366 (div_int (+ ?xu267_366 ?yu266_367) 2))
   (<= (div_int (+ ?xu267_366 ?yu266_367) 2) ?yu266_367)))))

:formula
  ;; File "binary_search_po8.why", line 600, characters 0-939
  (not
  (forall (?t1 c_type)
  (forall (?tu281_368 c_unsorted)
  (forall (?nu280_369 Int)
  (forall (?vu279_370 Int)
  (forall (?allocu278_371 c_unsorted)
  (forall (?intMutu2u277_372 c_unsorted)
  (implies
  (and
  (and (>= ?nu280_369 0)
  (valid_range
  (c_sort alloc_table ?allocu278_371) (c_sort (pointer ?t1) ?tu281_368) 0 
  (- ?nu280_369 1)))
  (forall (?k1u276_373 Int)
  (forall (?k2u275_374 Int)
  (implies
  (and (and (<= 0 ?k1u276_373) (<= ?k1u276_373 ?k2u275_374))
  (<= ?k2u275_374 (- ?nu280_369 1)))
  (<= (s2int
      (c_sort
      c_int (acc
            (c_sort (memory c_int ?t1) ?intMutu2u277_372) (c_sort
                                                          (pointer ?t1) 
                                                          (shift
                                                          (c_sort
                                                          (pointer ?t1) ?tu281_368) ?k1u276_373))))) 
  (s2int
  (c_sort
  c_int (acc
        (c_sort (memory c_int ?t1) ?intMutu2u277_372) (c_sort
                                                      (pointer ?t1) (shift
                                                                    (c_sort
                                                                    (pointer
                                                                    ?t1) ?tu281_368) ?k2u275_374))))))))))
  (forall (?lu274_375 Int)
  (forall (?uu273_376 Int)
  (implies
  (and (and (<= 0 ?lu274_375) (<= ?uu273_376 (- ?nu280_369 1)))
  (forall (?ku272_377 Int)
  (implies (and (<= 0 ?ku272_377) (< ?ku272_377 ?nu280_369))
  (implies
  (= (s2int
     (c_sort
     c_int (acc
           (c_sort (memory c_int ?t1) ?intMutu2u277_372) (c_sort
                                                         (pointer ?t1) 
                                                         (shift
                                                         (c_sort
                                                         (pointer ?t1) ?tu281_368) ?ku272_377)))))
  ?vu279_370) (and (<= ?lu274_375 ?ku272_377) (<= ?ku272_377 ?uu273_376))))))
  (implies (<= ?lu274_375 ?uu273_376)
  (implies (not (= 2 0))
  (forall (?resultu271_378 Int)
  (implies (= ?resultu271_378 (div_int (+ ?lu274_375 ?uu273_376) 2))
  (forall (?result0u270_379 c_unsorted)
  (implies
  (= (c_sort (pointer ?t1) ?result0u270_379)
  (c_sort
  (pointer ?t1) (shift (c_sort (pointer ?t1) ?tu281_368) ?resultu271_378)))
  (implies
  (valid
  (c_sort alloc_table ?allocu278_371) (c_sort (pointer ?t1) ?result0u270_379))
  (forall (?result1u269_380 Int)
  (implies
  (= ?result1u269_380
  (s2int
  (c_sort
  c_int (acc
        (c_sort (memory c_int ?t1) ?intMutu2u277_372) (c_sort
                                                      (pointer ?t1) ?result0u270_379)))))
  (implies (< ?result1u269_380 ?vu279_370)
  (forall (?l0u268_381 Int)
  (implies (= ?l0u268_381 (+ ?resultu271_378 1))
  (<= 0 (- ?uu273_376 ?lu274_375)))))))))))))))))))))))))


)

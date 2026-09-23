000100 01  OHASH-W460RHC.                                                       
000200*                                 ORDER HASH-TOTAL TRANS  FROM            
000300*                                 VIPS TO  NOAC.                          
000400*                                 RECORD TYPE RHC                         
000500     03 OHASH-IDPTYP         PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 OHASH-IDDISTR        PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 OHASH-IDKUNDNR       PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 OHASH-IDORDNR        PIC 9(7).                                    
001200*                                 ORDER NUMBER        IDORDNR-002         
001300     03 OHASH-SUHASH         PIC 9(14).                                   
001400*                                 HASH TOTAL = SUMMARY OF PART.           
001500*                                  NUMBER , CHECK-DIGIT AND               
001600*                                  ORDERED QUANTITY  FOR ALL              
001700*                                  LINES IN THE ORDER.                    
001800     03 FILLER               PIC X(46).                                   
001900*** END COPY W460RHCCC0  LENGTH=80                                        

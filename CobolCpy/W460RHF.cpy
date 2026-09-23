000100 01  KRED-W460RHF.                                                        
000200*                                 CRED TRANS. FROM VIPS                   
000300*                                 TO NOAC RECORD TYPE RHF                 
000400     03 KRED-IDPTYP          PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 KRED-IDDISTR         PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 KRED-IDKUNDNR        PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 KRED-KDCLAGER        PIC 9.                                       
001100*                                 CENTRAL WAREHOUSE CODE                  
001200     03 KRED-IDLEVANM        PIC X(7).                                    
001300*                                 DISCREPANCY REPORT NO                   
001400     03 KRED-TEKRENOT        PIC X(59).                                   
001500*                                 CREDIT NOTIFY                           
001600*** END COPY W460RHFCC0  LENGTH=80                                        

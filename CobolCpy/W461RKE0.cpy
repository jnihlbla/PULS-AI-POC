000100 01  RKE-W461RKE0-CTX.                                                    
000200*                                 CREDITTRANSACTION-LINE                  
000300*                                 RETURNS PERMISSIONS TRANS.              
000400*                                 RECORD TYP  RKE                         
000500     03 RKE-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RKE-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RKE-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RKE-KDCLAGER         PIC 9.                                       
001200      88 RKE-KDCLAGER-BADA   VALUE 0.                                     
001300      88 RKE-KDCLAGER-C1     VALUE 1.                                     
001400      88 RKE-KDCLAGER-C2     VALUE 2.                                     
001500*                                 CENTRAL WAREHOUSE CODE                  
001600     03 RKE-IDRAPPNR         PIC 9(7).                                    
001700*                                 DISCREPANCY REPORT NUMBER               
001800     03 RKE-IDRADNR          PIC 9(4).                                    
001900*                                 LINE NO                                 
002000     03 RKE-IDARTNR          PIC 9(9).                                    
002100*                                 PART NUMBER                             
002200     03 RKE-REKSIFFR         PIC 9.                                       
002300*                                 PART NO CHECK DIGIT                     
002400     03 RKE-TIRETILL         PIC 9(6).                                    
002500*                                 DATE RETURNPERMIT                       
002600     03 RKE-IDRAPPNR-002     PIC 9(7).                                    
002700*                                 DISCREPANCY REPORT NUMBER               
002800     03 RKE-FILLERX32        PIC X(32).                                   
002900*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  

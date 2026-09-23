000100 01  RKE-W461RKEN-CTX.                                                    
000200*                                 CREDITTRANSACTION-LINE                  
000300*                                 RETURNS PERMISSIONS TRANS.              
000400*                                 RECORD TYP  RKE                         
000500     03 RKE-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RKE-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RKE-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RKE-IDDC             PIC X(2).                                    
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 RKE-IDRAPPNR         PIC 9(7).                                    
001400*                                 DISCREPANCY REPORT NUMBER               
001500     03 RKE-IDRADNR          PIC 9(4).                                    
001600*                                 LINE NO                                 
001700     03 RKE-IDARTNR          PIC 9(9).                                    
001800*                                 PART NUMBER                             
001900     03 RKE-REKSIFFR         PIC 9.                                       
002000*                                 PART NO CHECK DIGIT                     
002100     03 RKE-TIRETILL         PIC 9(6).                                    
002200*                                 DATE RETURNPERMIT                       
002300     03 RKE-IDRAPPNR-002     PIC 9(7).                                    
002400*                                 DISCREPANCY REPORT NUMBER               
002500     03 RKE-FILLERX31        PIC X(31).                                   
002600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  

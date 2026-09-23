000100 01  RKD-W461RKD0.                                                        
000200*                                 CREDITTRANSACTION-LINE                  
000300*                                 REJECTED OR CHANGED                     
000400*                                 RECORD TYP  RKD                         
000500     03 RKD-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RKD-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RKD-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RKD-KDCLAGER         PIC 9.                                       
001200      88 RKD-KDCLAGER-BADA   VALUE 0.                                     
001300      88 RKD-KDCLAGER-C1     VALUE 1.                                     
001400      88 RKD-KDCLAGER-C2     VALUE 2.                                     
001500*                                 CENTRAL WAREHOUSE CODE                  
001600     03 RKD-IDRAPPNR         PIC 9(7).                                    
001700*                                 DISCREPANCY REPORT NUMBER               
001800     03 RKD-IDORDNR          PIC 9(7).                                    
001900*                                 ORDER NUMBER        IDORDNR-002         
002000     03 RKD-IDKOLLI          PIC 9(5).                                    
002100*                                 CASE NUMBER                             
002200     03 RKD-IDARTNR          PIC 9(9).                                    
002300*                                 PART NUMBER                             
002400     03 RKD-REKSIFFR         PIC 9.                                       
002500*                                 PART NO CHECK DIGIT                     
002600     03 RKD-IDRADNR          PIC 9(4).                                    
002700*                                 LINE NO                                 
002800     03 RKD-KDKREBEH         PIC X(3).                                    
002900*                                 TREATMENT STATUS                        
003000     03 RKD-KDANMORS         PIC 9(2).                                    
003100*                                 DISCREPANCY REPORT KDANMORS-002         
003200     03 RKD-KVLEVANM         PIC 9(6).                                    
003300*                                 DISCREPANCY REPORT QTY                  
003400     03 RKD-PRARTBTO         PIC 9(7)V9(2).                               
003500*                                 GROSS SALES PRICE (SEK)                 
003600     03 RKD-FLSKROT          PIC 9.                                       
003700     03 FILLER               PIC X(12).                                   
003800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  

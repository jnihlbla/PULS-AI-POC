000100 01  RKD-W461RKDN.                                                        
000200*                                 CREDITTRANSACTION-LINE                  
000300*                                 REJECTED OR CHANGED                     
000400*                                 RECORD TYP  RKD                         
000500     03 RKD-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RKD-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RKD-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RKD-IDDC             PIC X(2).                                    
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 RKD-IDRAPPNR         PIC 9(7).                                    
001400*                                 DISCREPANCY REPORT NUMBER               
001500     03 RKD-IDORDNR          PIC 9(7).                                    
001600*                                 ORDER NUMBER        IDORDNR-002         
001700     03 RKD-IDKOLLI          PIC 9(5).                                    
001800*                                 CASE NUMBER                             
001900     03 RKD-IDARTNR          PIC 9(9).                                    
002000*                                 PART NUMBER                             
002100     03 RKD-REKSIFFR         PIC 9.                                       
002200*                                 PART NO CHECK DIGIT                     
002300     03 RKD-IDRADNR          PIC 9(4).                                    
002400*                                 LINE NO                                 
002500     03 RKD-KDKREBEH         PIC X(3).                                    
002600*                                 TREATMENT STATUS                        
002700     03 RKD-KDANMORS         PIC 9(2).                                    
002800*                                 DISCREPANCY REPORT KDANMORS-002         
002900     03 RKD-KVLEVANM         PIC 9(6).                                    
003000*                                 DISCREPANCY REPORT QTY                  
003100     03 RKD-PRARTBTO         PIC 9(7)V9(2).                               
003200*                                 GROSS SALES PRICE (SEK)                 
003300     03 RKD-FLSKROT          PIC 9.                                       
003400     03 FILLER               PIC X(11).                                   
003500     03 RKD-KDVALISO         PIC X(3).                                    
003600*                                 CURRENCY CODE BY ISO-STANDARD.          
003700     03 RKD-PRARTBTO-LOC     PIC 9(7)V9(2).                               
003800*                                 LOCAL GROSS SALES PRICE                 
003900     03 RKD-SULNELOC         PIC 9(9)V9(2).                               
004000*                                 INVOICE LINE SUM VAT EXCLUDED           
004100     03 RKD-PRARTSTD         PIC 9(7)V9(2).                               
004200*                                 STANDARD PRICE                          
004300     03 RKD-PRARTSJK         PIC 9(7)V9(2).                               
004400*                                 COST OF SALES                           
004500*** END OF VILMAII-COPY LENGTH= 121 BYTES                                 

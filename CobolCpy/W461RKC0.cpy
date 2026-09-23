000100 01  RKC-W461RKC0.                                                        
000200*                                 CREDITTRANSACTION-LINE                  
000300*                                 RECORD TYP  RKC                         
000400     03 RKC-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RKC-IDFAKT           PIC 9(7).                                    
000700*                                 INVOICE NO.                             
000800     03 RKC-IDORDNR          PIC 9(7).                                    
000900*                                 ORDER NUMBER        IDORDNR-002         
001000     03 RKC-IDRADNR          PIC 9(4).                                    
001100*                                 LINE NO                                 
001200     03 RKC-IDARTNR          PIC 9(9).                                    
001300*                                 PART NUMBER                             
001400     03 RKC-REKSIFFR         PIC 9.                                       
001500*                                 PART NO CHECK DIGIT                     
001600     03 RKC-KDANMORS         PIC 9(2).                                    
001700*                                 DISCR.REPORT CODE                       
001800     03 RKC-KVKREANT         PIC 9(6).                                    
001900*                                 CREDITED QUANTITY                       
002000     03 RKC-PRARTBTO         PIC 9(7)V9(2).                               
002100*                                 GROSS SALES PRICE (SEK)                 
002200     03 RKC-IDKOLLI          PIC 9(5).                                    
002300*                                 CASE NUMBER                             
002400     03 FILLER               PIC X(27).                                   
002500*** END COPY W461RKC0C0  LENGTH=80                                        

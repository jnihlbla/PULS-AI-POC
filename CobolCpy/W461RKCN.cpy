000100 01  RKC-W461RKCN-CTX.                                                    
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
001700*                                 DISCREPANCY REPORT KDANMORS-002         
001800     03 RKC-KVKREANT         PIC 9(6).                                    
001900*                                 CREDITED QUANTITY                       
002000     03 RKC-PRARTBTO         PIC 9(7)V9(2).                               
002100*                                 GROSS SALES PRICE (SEK)                 
002200     03 RKC-IDKOLLI          PIC 9(5).                                    
002300*                                 CASE NUMBER                             
002400     03 RKC-KDPSLLOC         PIC 9(2).                                    
002500*                                 PRODUCT GROUP LOCAL                     
002600     03 RKC-PRAVCOST         PIC 9(7)V9(2).                               
002700*                                 AVERAGE COST FOREIGN CURRENCY           
002800     03 RKC-PRAVCOST-CORE    PIC 9(7)V9(2).                               
002900*                                 AV. COST PRICE OF THE CORE IN F         
003000*                                 OR. CURR.                               
003100     03 RKC-FILLERX7         PIC X(7).                                    
003200     03 RKC-PRARTBTO-LOC     PIC 9(7)V9(2).                               
003300*                                 LOCAL GROSS SALES PRICE                 
003400     03 RKC-KDVAT            PIC X(2).                                    
003500*                                 VAT CODE                                
003600     03 RKC-PRMOMS-RAD       PIC 9(7)V9(2).                               
003700*                                 VAT                                     
003800     03 RKC-SULNELOC         PIC 9(9)V9(2).                               
003900*                                 INVOICE LINE SUM VAT EXCLUDED           
004000     03 RKC-PRARTSTD         PIC 9(7)V9(2).                               
004100*                                 STANDARD PRICE                          
004200     03 RKC-PRARTSJK         PIC 9(7)V9(2).                               
004300*                                 COST OF SALES                           
004400     03 RKC-IDTRACK          PIC X(15).                                   
004500*                                 CUSTOMS TRACKING ID                     
004600     03 RKC-DADATUM          PIC 9(8).                                    
004700*                                 REGISTRATION DATE (YYYYMMDD)            
004800*** END OF VILMAII-COPY LENGTH= 152 BYTES                                 

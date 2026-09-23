000100 01  ORAD-W460RHB-CTX.                                                    
000200*                                 ORDER LINE TRANS. FROM VIPS             
000300*                                  TO NOAC   RECORD TYPE RHB              
000400     03 ORAD-IDPTYP          PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 ORAD-IDDISTR         PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 ORAD-IDKUNDNR        PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 ORAD-IDORDNR         PIC 9(7).                                    
001100*                                 ORDER NUMBER        IDORDNR-002         
001200     03 ORAD-IDARTNR         PIC 9(9).                                    
001300*                                 PART NUMBER                             
001400     03 ORAD-REKSIFFR        PIC 9.                                       
001500*                                 PART NO CHECK DIGIT                     
001600     03 ORAD-BERADREF        PIC X(10).                                   
001700*                                 CUSTOMERS ITEM REF.                     
001800     03 ORAD-KVBEART         PIC 9(6).                                    
001900*                                 ORDERED QUANTITY                        
002000     03 ORAD-KDKVBRYT        PIC 9.                                       
002100*                                 BREAK BULKPACK CODE                     
002200     03 ORAD-KDDSP           PIC 9.                                       
002300*                                 AFFECT ON DSP                           
002400     03 ORAD-TITPO           PIC 9(6).                                    
002500*                                 PLANNED ORDER DATE                      
002600     03 ORAD-FLSLATT         PIC X.                                       
002700     03 ORAD-FLDIRLEV        PIC X.                                       
002800*                                 DIRECT DELIVERY ?                       
002900     03 ORAD-PRARTNTO-LOC    PIC 9(7)V9(2).                               
003000*                                 NET PRICE EACH LOCAL CURRENCY           
003100     03 ORAD-PRARTBTO-LOC    PIC 9(7)V9(2).                               
003200*                                 LOCAL GROSS SALES PRICE                 
003300     03 ORAD-KDVALISO        PIC X(3).                                    
003400*                                 CURRENCY CODE BY ISO-STANDARD.          
003500     03 ORAD-KDVAT           PIC X(2).                                    
003600*                                 VAT CODE                                
003700     03 ORAD-RERAB           PIC 9(2)V9(1).                               
003800     03 ORAD-KDRAB           PIC X(5).                                    
003900     03 ORAD-BEART-VIPS      PIC X(25).                                   
004000*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 

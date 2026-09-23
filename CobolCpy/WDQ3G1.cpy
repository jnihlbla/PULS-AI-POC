000100 01  SEQG-WDQ3G1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 DISTR-KUND INGÅNG                       
000500*                                 FYSISK NYCKEL: WDQ3G1KY                 
000600*                                 (IDDISTR,IDKUNDNR,IDDC,IDKUNDRF         
000700*                                 , IDORDER, IDPRODNR, IDPLKLST)          
000800*                                                                         
000900*                                 SECONDARY NYCKEL: WDQ3GSEQ              
001000*                                 (IDDISTR,IDKUNDNR,IDDC,IDKUNDRF         
001100*                                 )                                       
001200     03 SEQG-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 SEQG-IDKUNDNR        PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 SEQG-IDDC            PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 SEQG-IDKUNDRF        PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400     03 SEQG-IDORDNR7-FILLER REDEFINES SEQG-IDKUNDRF.                     
002500        05 SEQG-IDORDNR7     PIC 9(7).                                    
002600*                                 ORDERNUMMER                             
002700*                                 ORDER NUMBER                            
002800        05 FILLER            PIC X(3).                                    
002900     03 SEQG-IDORDER         PIC S9(7)           COMP-3.                  
003000*                                 VOLVO PARTS ORDERNUMMER                 
003100*                                 VOLVO PARTS ORDER NUMBER                
003200     03 SEQG-IDPRODNR        PIC S9(7)           COMP-3.                  
003300*                                 PRODUKTIONSNUMMER                       
003400*                                 PRODUCTION NUMBER                       
003500     03 SEQG-IDPLKLST        PIC S9(3)           COMP-3.                  
003600*                                 PLOCKLISTNUMMER                         
003700*                                 PICKING LIST NUMBER                     
003800     03 SEQG-IDUSER          PIC X(8).                                    
003900*                                 ANVÄNDARENS SÄKERHETS ID                
004000*                                 USER SECURITY-IDENTITY                  
004100     03 SEQG-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
004200*                                 SUMMA ORDERVÄRDE                        
004300*                                 TOTAL ORDER VALUE                       
004400     03 SEQG-SUORDV-LOC      PIC S9(9)V9(2)      COMP-3.                  
004500*                                 ORDERVÄRDE SLUTKUNDPRIS                 
004600*                                 I LOKAL VALUTA                          
004700*                                 ORDER VALUE, CUSTOMER PRICE             
004800*                                 IN LOCAL CURRENCY                       
004900     03 SEQG-SUORDV-LOCPREL  PIC S9(9)V9(2)      COMP-3.                  
005000*                                 ORDERVÄRDE PREL SLUT-                   
005100*                                 KUNDPRIS, LOKAL VALUTA                  
005200*                                 ORDER VALUE, PREL CUSTOMER              
005300*                                 PRICE IN LOCAL CURRENCY                 
005400     03 SEQG-TIREGDAT        PIC S9(7)           COMP-3.                  
005500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005600*                                 REGISTRATION DATE (YYMMDD)              
005700     03 SEQG-IDWDQ301        PIC X(12).                                   
005800*                                 NYCKEL TILL WDQ301                      
005900*                                 KEY TO WDQ301                           
006000*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  

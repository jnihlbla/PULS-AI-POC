000100 01  SEQB-WDC7B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDC701             
000300*                                 FYSISK NYCKEL: WDC7B1KY                 
000400*                                 (KDPRSTA, IDDISTR, IDKUNDNR +           
000500*                                 (IDBUNDLE IDARTNR, IDPRQUES)            
000600*                                 SECONDARY KEY: WDC7BSEQ                 
000700*                                 (KDPRSTA)                               
000800     03 SEQB-KDPRSTA         PIC X.                                       
000900*                                 STATUS PRISFRÅGA                        
001000*                                 STATUS PRICE QUESTION                   
001100     03 SEQB-IDDISTR         PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQB-IDKUNDNR        PIC 9(7).                                    
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 SEQB-IDBUNDLE-GRP.                                                
001800*                                 QUERY  REFERENS (ORDERID/RAPPN)         
001900*                                 QUERY REFERENCE (ORDERID/REPNO)         
002000        05 SEQB-IDBUNDLE     PIC X(15).                                   
002100*                                 BUNDLE ID                               
002200*                                 BUNDLE ID                               
002300        05 SEQB-IDORDNR7-FILLER REDEFINES SEQB-IDBUNDLE.                  
002400           07 SEQB-IDORDNR7  PIC 9(7).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700           07 FILLER         PIC X(8).                                    
002800        05 SEQB-IDRAPPNR-FILLER REDEFINES SEQB-IDBUNDLE.                  
002900           07 SEQB-IDRAPPNR  PIC 9(7).                                    
003000*                                 RAPPORT NUMMER                          
003100*                                 DISCREPANCY REPORT NUMBER               
003200           07 FILLER         PIC X(8).                                    
003300        05 SEQB-IDORDER-FILLER REDEFINES SEQB-IDBUNDLE.                   
003400           07 SEQB-IDORDER   PIC S9(7)           COMP-3.                  
003500*                                 VOLVO PARTS ORDERNUMMER                 
003600*                                 VOLVO PARTS ORDER NUMBER                
003700           07 FILLER         PIC X(11).                                   
003800     03 SEQB-IDARTNR         PIC 9(9).                                    
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 SEQB-IDPRQUES        PIC 9(7).                                    
004200*                                 PRISFRÅGA NR                            
004300*                                 PRICE QUESTION NO                       
004400     03 SEQB-PRARTBTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
004500*                                 PRIS I LOKAL VALUTA                     
004600*                                 LOCAL GROSS SALES PRICE                 
004700     03 SEQB-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
004800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004900*                                 NET PRICE EACH LOCAL CURRENCY           
005000     03 SEQB-KDRAB           PIC X(5).                                    
005100*                                 RABATTKOD                               
005200     03 SEQB-KDVALISO        PIC X(3).                                    
005300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005400*                                 CURRENCY CODE BY ISO-STANDARD.          
005500*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  

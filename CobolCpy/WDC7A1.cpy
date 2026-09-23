000100 01  SEQA-WDC7A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDC711             
000300*                                 FYSISK NYCKEL: WDC7A1KY                 
000400*                                 (KDSEGKEY + IDDISTR + IDKUNDNR          
000500*                                 +                                       
000600*                                  IDBUNDLE + IDARTNR + IDPRQUES          
000700*                                 SEKUNDÄR NYCKEL: WDC7ASEQ               
000800*                                 (KDSEGKEY)                              
000900*                                                                         
001000     03 SEQA-KDSEGKEY        PIC X.                                       
001100*                                 TEKNISK SEGMENT-NYCKEL                  
001200*                                 TECHNICAL SEGMENT KEY                   
001300     03 SEQA-IDDISTR         PIC 9(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500*                                 DISTRICT NUMBER                         
001600     03 SEQA-IDKUNDNR        PIC 9(7).                                    
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900     03 SEQA-IDBUNDLE-GRP.                                                
002000*                                 QUERY  REFERENS (ORDERID/RAPPN)         
002100*                                 QUERY REFERENCE (ORDERID/REPNO)         
002200        05 SEQA-IDBUNDLE     PIC X(15).                                   
002300*                                 BUNDLE ID                               
002400*                                 BUNDLE ID                               
002500        05 SEQA-IDORDNR7-FILLER REDEFINES SEQA-IDBUNDLE.                  
002600           07 SEQA-IDORDNR7  PIC 9(7).                                    
002700*                                 ORDERNUMMER                             
002800*                                 ORDER NUMBER                            
002900           07 FILLER         PIC X(8).                                    
003000        05 SEQA-IDRAPPNR-FILLER REDEFINES SEQA-IDBUNDLE.                  
003100           07 SEQA-IDRAPPNR  PIC 9(7).                                    
003200*                                 RAPPORT NUMMER                          
003300*                                 DISCREPANCY REPORT NUMBER               
003400           07 FILLER         PIC X(8).                                    
003500        05 SEQA-IDORDER-FILLER REDEFINES SEQA-IDBUNDLE.                   
003600           07 SEQA-IDORDER   PIC S9(7)           COMP-3.                  
003700*                                 VOLVO PARTS ORDERNUMMER                 
003800*                                 VOLVO PARTS ORDER NUMBER                
003900           07 FILLER         PIC X(11).                                   
004000     03 SEQA-IDARTNR         PIC 9(9).                                    
004100*                                 ARTIKELNUMMER                           
004200*                                 PART NUMBER                             
004300     03 SEQA-IDPRQUES        PIC 9(7).                                    
004400*                                 PRISFRÅGA NR                            
004500*                                 PRICE QUESTION NO                       
004600     03 SEQA-KDPRSTA         PIC X.                                       
004700*                                 STATUS PRISFRÅGA                        
004800*                                 STATUS PRICE QUESTION                   
004900     03 SEQA-PRARTBTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
005000*                                 PRIS I LOKAL VALUTA                     
005100*                                 LOCAL GROSS SALES PRICE                 
005200     03 SEQA-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
005300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005400*                                 NET PRICE EACH LOCAL CURRENCY           
005500     03 SEQA-KDRAB           PIC X(5).                                    
005600*                                 RABATTKOD                               
005700     03 SEQA-KDVALISO        PIC X(3).                                    
005800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005900*                                 CURRENCY CODE BY ISO-STANDARD.          
006000*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  

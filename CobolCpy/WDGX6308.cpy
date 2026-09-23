000100 01  6308-WDGX6308.                                                       
000200*                                 SDC + NDC INLEV RETUR INFO              
000300*                                 FYSISK NYCKEL: KEY6308                  
000400     03 6308-KEY6308.                                                     
000500*                                 RADENS NYCKEL                           
000600        05 6308-IDDISTR      PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900        05 6308-IDKUNDNR     PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200        05 6308-IDRAPPNR     PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400*                                 DISCREPANCY REPORT NUMBER               
001500        05 6308-IDARTNR      PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800        05 6308-IDRADNR      PIC S9(5)           COMP-3.                  
001900*                                 RADNUMMER                               
002000*                                 LINE NO                                 
002100     03 6308-IDKOLLI         PIC S9(5)           COMP-3.                  
002200*                                 KOLLINUMMER                             
002300*                                 CASE NUMBER                             
002400     03 6308-IDKUNDRF-GRP.                                                
002500*                                 KUNDENS REFERENS (ORDERID)              
002600*                                 CUSTOMER REFERENCE (ORDER ID)           
002700        05 6308-IDKUNDRF     PIC X(10).                                   
002800*                                 KUNDENS REFERENS (ORDERID)              
002900*                                 CUSTOMER REFERENCE (ORDER ID)           
003000        05 6308-IDORDNR5-FILLER REDEFINES 6308-IDKUNDRF.                  
003100           07 6308-IDORDNR5  PIC 9(5).                                    
003200*                                 ORDERNUMMER                             
003300*                                 ORDER NUMBER                            
003400           07 FILLER         PIC X(5).                                    
003500        05 6308-IDORDNR7-FILLER REDEFINES 6308-IDKUNDRF.                  
003600           07 6308-IDORDNR7  PIC 9(7).                                    
003700*                                 ORDERNUMMER                             
003800*                                 ORDER NUMBER                            
003900           07 FILLER         PIC X(3).                                    
004000     03 6308-KDANMORS        PIC X(2).                                    
004100*                                 ORSAK TILL LEVERANSANMÄRKNING           
004200*                                 DISCREPANCY REPORT REASON CODE          
004300     03 6308-KDEMBLEV        PIC S9              COMP-3.                  
004400*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
004500*                                 PACK CODE DISCREP                       
004600     03 6308-KDFRAKT         PIC S9(3)           COMP-3.                  
004700*                                 FRAKTSÄTT DC TILL KUND                  
004800*                                 FREIGHT CODE                            
004900     03 6308-KVLEVANM        PIC S9(7)           COMP-3.                  
005000*                                 LEVERANSANMÄRKNINGSANTAL                
005100*                                 DISCREPANCY REPORT QTY                  
005200     03 6308-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
005300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005400*                                 GROSS SALES PRICE (SEK)                 
005500     03 6308-TILEVANM        PIC S9(7)           COMP-3.                  
005600*                                 DATUM LEVERANSANMÄRKNING                
005700*                                 DISCREPANCY REPORT DATE                 
005800     03 6308-PRARTBTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
005900*                                 PRIS I LOKAL VALUTA                     
006000*                                 LOCAL GROSS SALES PRICE                 
006100     03 6308-KDVALISO        PIC X(3).                                    
006200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006300*                                 CURRENCY CODE BY ISO-STANDARD.          
006400*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  

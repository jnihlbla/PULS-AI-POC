000100 01  4562-WDGX4562-CTX.                                                   
000200*                                 ÅTERSTARTS REGISTER PROFORMA            
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4562-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4562-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 4562-IDGMTREF.                                                    
001200*                                 GODSMOTTAGAREREFERENS                   
001300*                                 GOODS RECEIVER REFERENS                 
001400        05 4562-IDDISTR      PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700        05 4562-IDKUNDNR     PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000        05 4562-IDKUNDRF     PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200*                                 CUSTOMER REFERENCE (ORDER ID)           
002300        05 4562-IDORDNR5-FILLER REDEFINES 4562-IDKUNDRF.                  
002400           07 4562-IDORDNR5  PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700           07 FILLER         PIC X(5).                                    
002800        05 4562-IDORDNR7-FILLER REDEFINES 4562-IDKUNDRF.                  
002900           07 4562-IDORDNR7  PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200           07 FILLER         PIC X(3).                                    
003300     03 4562-IDLOPNR         PIC S9(3)           COMP-3.                  
003400*                                 LÖPNUMMER                               
003500*                                 SEQUENCE NUMBER                         
003600     03 4562-IDORDER         PIC S9(7)           COMP-3.                  
003700*                                 VOLVO PARTS ORDERNUMMER                 
003800*                                 VOLVO PARTS ORDER NUMBER                
003900     03 4562-KVPOST          PIC S9(7)           COMP-3.                  
004000*                                 RÄKNARE, ANTAL POSTER                   
004100*                                 RECORD COUNTER                          
004200     03 4562-TIUPPDAT        PIC S9(7)           COMP-3.                  
004300*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004400*                                 UPDATING DATE     (YYMMDD)              
004500     03 4562-TIUPPTID        PIC S9(9)           COMP-3.                  
004600*                                 UPPDATERINGSTID  (TTMMSSTH)             
004700*                                 UPDATING TIME    (HHMMSSTH)             
004800     03 4562-FILLERX38       PIC X(38).                                   
004900*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  

000100 01  4751-WDGX4751-CTX.                                                   
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4751-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4751-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 4751-IDFAKT          PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300*                                 INVOICE NO.                             
001400     03 4751-IDDISTR         PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700     03 4751-IDKOLLI         PIC S9(5)           COMP-3.                  
001800*                                 KOLLINUMMER                             
001900*                                 CASE NUMBER                             
002000     03 4751-IDKUNDRF        PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200*                                 CUSTOMER REFERENCE (ORDER ID)           
002300     03 4751-FILLERX5        PIC X(5).                                    
002400*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  

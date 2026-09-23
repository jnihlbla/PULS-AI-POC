000100 01  4554-WDGX4554.                                                       
000200*                                 ÅTERSTARTS REGISTER RESTORDER           
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4554-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4554-KVPOST-IN       PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 4554-IDDISTR-IN      PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 4554-IDKUNDNR-IN     PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 4554-IDKUNDRF-IN     PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900*                                 CUSTOMER REFERENCE (ORDER ID)           
002000     03 4554-IDLOPNRE-IN     PIC S9(3)           COMP-3.                  
002100*                                 LÖPNUMMER ERSÄTTNING                    
002200*                                 SEQUENCE NUMBER FOR EACH SUPER-         
002300*                                 SESSION                                 
002400     03 4554-IDKORTNR-IN     PIC S9(3)           COMP-3.                  
002500*                                 KORTNUMMER                              
002600*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002700*                                 SEQUENCE NUMBER FOR EACH RECORD         
002800*                                  IN A SUPERSESSION                      
002900     03 4554-KVPOST-UT       PIC S9(7)           COMP-3.                  
003000*                                 RÄKNARE, ANTAL POSTER                   
003100*                                 RECORD COUNTER                          
003200     03 4554-IDDISTR-UT      PIC S9(5)           COMP-3.                  
003300*                                 DISTRIKTNUMMER                          
003400*                                 DISTRICT NUMBER                         
003500     03 4554-IDKUNDNR-UT     PIC S9(7)           COMP-3.                  
003600*                                 KUNDNUMMER                              
003700*                                 CUSTOMER NO                             
003800     03 4554-IDKUNDRF-UT     PIC X(10).                                   
003900*                                 KUNDENS REFERENS (ORDERID)              
004000*                                 CUSTOMER REFERENCE (ORDER ID)           
004100     03 4554-IDLOPNRE-UT     PIC S9(3)           COMP-3.                  
004200*                                 LÖPNUMMER ERSÄTTNING                    
004300*                                 SEQUENCE NUMBER FOR EACH SUPER-         
004400*                                 SESSION                                 
004500     03 4554-IDKORTNR-UT     PIC S9(3)           COMP-3.                  
004600*                                 KORTNUMMER                              
004700*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004800*                                 SEQUENCE NUMBER FOR EACH RECORD         
004900*                                  IN A SUPERSESSION                      
005000     03 4554-TIUPPDAT        PIC S9(7)           COMP-3.                  
005100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
005200*                                 UPDATING DATE     (YYMMDD)              
005300     03 4554-TIUPPTID        PIC S9(9)           COMP-3.                  
005400*                                 UPPDATERINGSTID  (TTMMSSTH)             
005500*                                 UPDATING TIME    (HHMMSSTH)             
005600     03 FILLER               PIC X(20).                                   
005700*** END COPY WDGX4554C0  LENGTH=80                                        

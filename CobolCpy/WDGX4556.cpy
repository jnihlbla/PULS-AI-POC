000100 01  4556-WDGX4556.                                                       
000200*                                 ÅTERSTARTS REGISTER RESTORDER           
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4556-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4556-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 4556-IDGMTREF.                                                    
001200*                                 GODSMOTTAGAREREFERENS                   
001300*                                 GOODS RECEIVER REFERENS                 
001400        05 4556-IDDISTR      PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700        05 4556-IDKUNDNR     PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000        05 4556-IDKUNDRF     PIC X(10).                                   
002100*                                 KUNDENS REFERENS (ORDERID)              
002200*                                 CUSTOMER REFERENCE (ORDER ID)           
002300        05 4556-IDORDNR5-FILLER REDEFINES 4556-IDKUNDRF.                  
002400           07 4556-IDORDNR5  PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700           07 FILLER         PIC X(5).                                    
002800        05 4556-IDORDNR7-FILLER REDEFINES 4556-IDKUNDRF.                  
002900           07 4556-IDORDNR7  PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200           07 FILLER         PIC X(3).                                    
003300     03 4556-IDLOPNRE        PIC S9(3)           COMP-3.                  
003400*                                 LÖPNUMMER ERSÄTTNING                    
003500*                                 SEQUENCE NUMBER FOR EACH SUPER-         
003600*                                 SESSION                                 
003700     03 4556-IDKORTNR        PIC S9(3)           COMP-3.                  
003800*                                 KORTNUMMER                              
003900*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
004000*                                 SEQUENCE NUMBER FOR EACH RECORD         
004100*                                  IN A SUPERSESSION                      
004200     03 4556-TIUPPDAT        PIC S9(7)           COMP-3.                  
004300*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004400*                                 UPDATING DATE     (YYMMDD)              
004500     03 4556-TIUPPTID        PIC S9(9)           COMP-3.                  
004600*                                 UPPDATERINGSTID  (TTMMSSTH)             
004700*                                 UPDATING TIME    (HHMMSSTH)             
004800     03 FILLER               PIC X(45).                                   
004900*** END COPY WDGX4556C0  LENGTH=80                                        

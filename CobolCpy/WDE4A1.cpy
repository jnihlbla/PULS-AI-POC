000100 01  SEQA-WDE4A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE401             
000300*                                 FYSISK NYCKEL: WDE4A1KY                 
000400*                                 (IDGMTREF, IDPRODNR, IDPLKLST)          
000500*                                 SECONDARY KEY: WDE4ASEQ                 
000600*                                 (IDGMTREF)                              
000700     03 SEQA-IDGMTREF.                                                    
000800*                                 GODSMOTTAGAREREFERENS                   
000900*                                 GOODS RECEIVER REFERENS                 
001000        05 SEQA-IDDISTR      PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300        05 SEQA-IDKUNDNR     PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600        05 SEQA-IDKUNDRF-GRP.                                             
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900           07 SEQA-IDKUNDRF  PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200           07 SEQA-IDORDNR5-FILLER REDEFINES SEQA-IDKUNDRF.               
002300              09 SEQA-IDORDNR5                                            
002400                             PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700              09 FILLER      PIC X(5).                                    
002800           07 SEQA-IDORDNR7-FILLER REDEFINES SEQA-IDKUNDRF.               
002900              09 SEQA-IDORDNR7                                            
003000                             PIC 9(7).                                    
003100*                                 ORDERNUMMER                             
003200*                                 ORDER NUMBER                            
003300              09 FILLER      PIC X(3).                                    
003400     03 SEQA-IDPRODNR        PIC S9(7)           COMP-3.                  
003500*                                 PRODUKTIONSNUMMER                       
003600*                                 PRODUCTION NUMBER                       
003700     03 SEQA-IDPLKLST        PIC S9(3)           COMP-3.                  
003800*                                 PLOCKLISTNUMMER                         
003900*                                 PICKING LIST NUMBER                     
004000*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  

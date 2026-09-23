000100 01  SEQE-WDE4E1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE401             
000300*                                 PRODUKTIONSNUMMER SEKVENS               
000400*                                 FYSISK NYCKEL: WDE4E1KY                 
000500*                                 (IDPRODNR, IDGMTREF, IDPLKLST)          
000600*                                 SECONDARY KEY: WDE4ESEQ                 
000700*                                 (IDPRODNR)                              
000800     03 SEQE-IDPRODNR        PIC S9(7)           COMP-3.                  
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 SEQE-IDGMTREF.                                                    
001200*                                 GODSMOTTAGAREREFERENS                   
001300*                                 GOODS RECEIVER REFERENS                 
001400        05 SEQE-IDDISTR      PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700        05 SEQE-IDKUNDNR     PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000        05 SEQE-IDKUNDRF-GRP.                                             
002100*                                 KUNDENS REFERENS (ORDERID)              
002200*                                 CUSTOMER REFERENCE (ORDER ID)           
002300           07 SEQE-IDKUNDRF  PIC X(10).                                   
002400*                                 KUNDENS REFERENS (ORDERID)              
002500*                                 CUSTOMER REFERENCE (ORDER ID)           
002600           07 SEQE-IDORDNR5-FILLER REDEFINES SEQE-IDKUNDRF.               
002700              09 SEQE-IDORDNR5                                            
002800                             PIC 9(5).                                    
002900*                                 ORDERNUMMER                             
003000*                                 ORDER NUMBER                            
003100              09 FILLER      PIC X(5).                                    
003200           07 SEQE-IDORDNR7-FILLER REDEFINES SEQE-IDKUNDRF.               
003300              09 SEQE-IDORDNR7                                            
003400                             PIC 9(7).                                    
003500*                                 ORDERNUMMER                             
003600*                                 ORDER NUMBER                            
003700              09 FILLER      PIC X(3).                                    
003800     03 SEQE-IDPLKLST        PIC S9(3)           COMP-3.                  
003900*                                 PLOCKLISTNUMMER                         
004000*                                 PICKING LIST NUMBER                     
004100*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  

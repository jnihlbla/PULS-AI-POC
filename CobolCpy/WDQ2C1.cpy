000100 01  SEQC-WDQ2C1.                                                         
000200*                                 ORDERHUVUDS REGISTER                    
000300*                                 SEKUNDÄRT INDEX TILL WDQ201             
000400*                                 DISTR-KUND INGÅNG                       
000500*                                 FYSISK NYCKEL: WDQ2C1KY                 
000600*                                 (IDGMTREF)                              
000700*                                 SECONDARY NYCKEL: WDQ2CSEQ              
000800*                                 (IDGMTREF)                              
000900     03 SEQC-IDGMTREF.                                                    
001000*                                 GODSMOTTAGAREREFERENS                   
001100*                                 GOODS RECEIVER REFERENS                 
001200        05 SEQC-IDDISTR      PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500        05 SEQC-IDKUNDNR     PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800        05 SEQC-IDKUNDRF-GRP.                                             
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100           07 SEQC-IDKUNDRF  PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400           07 SEQC-IDORDNR5-FILLER REDEFINES SEQC-IDKUNDRF.               
002500              09 SEQC-IDORDNR5                                            
002600                             PIC 9(5).                                    
002700*                                 ORDERNUMMER                             
002800*                                 ORDER NUMBER                            
002900              09 FILLER      PIC X(5).                                    
003000           07 SEQC-IDORDNR7-FILLER REDEFINES SEQC-IDKUNDRF.               
003100              09 SEQC-IDORDNR7                                            
003200                             PIC 9(7).                                    
003300*                                 ORDERNUMMER                             
003400*                                 ORDER NUMBER                            
003500              09 FILLER      PIC X(3).                                    
003600     03 SEQC-IDORDER         PIC S9(7)           COMP-3.                  
003700*                                 VOLVO PARTS ORDERNUMMER                 
003800*                                 VOLVO PARTS ORDER NUMBER                
003900     03 SEQC-FLBORT          PIC X.                                       
004000*                                 BORTTAGNINGSFLAGGA                      
004100*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  

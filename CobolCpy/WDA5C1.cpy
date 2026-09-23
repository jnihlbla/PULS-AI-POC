000100 01  SEQC-WDA5C1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDA501             
000400*                                 ANSKAFFARE ORSAK                        
000500*                                 FYSISK NYCKEL: WDA5C1KY                 
000600*                                  (IDANSK, KDROO, IDARTNR,               
000700*                                   IDDISTR, IDKUNDNR, IDKUNDRF,          
000800*                                   IDLOPNR)                              
000900*                                 SECONDARY NYCKEL: WDA5CSEQ              
001000*                                  (IDANSK, KDROO)                        
001100     03 SEQC-IDANSK          PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300*                                 PROCURER NO.                            
001400     03 SEQC-KDROO           PIC S9              COMP-3.                  
001500*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
001600*                                 REASON CODE FOR WAITING LINE            
001700     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000     03 SEQC-IDDISTR         PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200*                                 DISTRICT NUMBER                         
002300     03 SEQC-IDKUNDNR        PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500*                                 CUSTOMER NO                             
002600     03 SEQC-IDKUNDRF-GRP.                                                
002700*                                 KUNDENS REFERENS (ORDERID)              
002800*                                 CUSTOMER REFERENCE (ORDER ID)           
002900        05 SEQC-IDKUNDRF     PIC X(10).                                   
003000*                                 KUNDENS REFERENS (ORDERID)              
003100*                                 CUSTOMER REFERENCE (ORDER ID)           
003200        05 SEQC-IDORDNR5-FILLER REDEFINES SEQC-IDKUNDRF.                  
003300           07 SEQC-IDORDNR5  PIC 9(5).                                    
003400*                                 ORDERNUMMER                             
003500*                                 ORDER NUMBER                            
003600           07 FILLER         PIC X(5).                                    
003700        05 SEQC-IDORDNR7-FILLER REDEFINES SEQC-IDKUNDRF.                  
003800           07 SEQC-IDORDNR7  PIC 9(7).                                    
003900*                                 ORDERNUMMER                             
004000*                                 ORDER NUMBER                            
004100           07 FILLER         PIC X(3).                                    
004200     03 SEQC-IDLOPNR         PIC S9(3)           COMP-3.                  
004300*                                 LÖPNUMMER                               
004400*                                 SEQUENCE NUMBER                         
004500     03 SEQC-KDSTARAD        PIC X.                                       
004600*                                 RADSTATUSKOD                            
004700*                                 LINE STATUS CODE                        
004800     03 SEQC-KDTPOTYP        PIC S9              COMP-3.                  
004900*                                 TYP AV TIDPLANERAD ORDER                
005000*                                 TYPE OF TIME PLANNED ORDER              
005100     03 SEQC-IDWDA501        PIC X(24).                                   
005200*                                 NYCKEL TILL WDA501                      
005300*                                 KEY TO WDA501                           
005400*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  

000100 01  SEQD-WDA5D1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDA501             
000400*                                 ARTIKLAR PER KUND                       
000500*                                 FYSISK NYCKEL: WDA5D1KY                 
000600*                                  (IDDISTR, IDKUNDNR, IDARTNR,           
000700*                                   IDKUNDRF, IDLOPNR)                    
000800*                                 SECONDARY NYCKEL: WDA5DSEQ              
000900*                                  (IDDISTR, IDKUNDNR)                    
001000     03 SEQD-IDDISTR         PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 SEQD-IDKUNDNR        PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 SEQD-IDKUNDRF-GRP.                                                
002000*                                 KUNDENS REFERENS (ORDERID)              
002100*                                 CUSTOMER REFERENCE (ORDER ID)           
002200        05 SEQD-IDKUNDRF     PIC X(10).                                   
002300*                                 KUNDENS REFERENS (ORDERID)              
002400*                                 CUSTOMER REFERENCE (ORDER ID)           
002500        05 SEQD-IDORDNR5-FILLER REDEFINES SEQD-IDKUNDRF.                  
002600           07 SEQD-IDORDNR5  PIC 9(5).                                    
002700*                                 ORDERNUMMER                             
002800*                                 ORDER NUMBER                            
002900           07 FILLER         PIC X(5).                                    
003000        05 SEQD-IDORDNR7-FILLER REDEFINES SEQD-IDKUNDRF.                  
003100           07 SEQD-IDORDNR7  PIC 9(7).                                    
003200*                                 ORDERNUMMER                             
003300*                                 ORDER NUMBER                            
003400           07 FILLER         PIC X(3).                                    
003500     03 SEQD-IDLOPNR         PIC S9(3)           COMP-3.                  
003600*                                 LÖPNUMMER                               
003700*                                 SEQUENCE NUMBER                         
003800     03 SEQD-IDDC            PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000*                                 WAREHOUSE IDENTIFIER                    
004100     03 SEQD-KDORDKL         PIC S9              COMP-3.                  
004200*                                 ORDERKLASS                              
004300*                                 ORDER CLASS                             
004400     03 SEQD-KDPRODSL        PIC S9(3)           COMP-3.                  
004500*                                 PRODUKTSLAG                             
004600*                                 PRODUCT GROUP                           
004700     03 SEQD-KDSTARAD        PIC X.                                       
004800*                                 RADSTATUSKOD                            
004900*                                 LINE STATUS CODE                        
005000     03 SEQD-KDTPOTYP        PIC S9              COMP-3.                  
005100*                                 TYP AV TIDPLANERAD ORDER                
005200*                                 TYPE OF TIME PLANNED ORDER              
005300     03 SEQD-IDWDA501        PIC X(24).                                   
005400*                                 NYCKEL TILL WDA501                      
005500*                                 KEY TO WDA501                           
005600*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  

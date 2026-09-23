000100 01  SEQE-WDA5E1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDA501             
000400*                                 ARTIKLAR FÖR ANSK ATT BEKR              
000500*                                 EXIT: INDEX FINNS NÄR                   
000600*                                 KDSTARAD = 1      OCH                   
000700*                                 KDTPOTYP = 2 EL 6 OCH                   
000800*                                 FLTPOBEK = N                            
000900*                                 FYSISK NYCKEL: WDA5E1KY                 
001000*                                  (DASENBEK, IDGMTREF, IDARTNR,          
001100*                                   IDLOPNR)                              
001200*                                 SECONDARY NYCKEL: WDA5ESEQ              
001300*                                  (DASENBEK)                             
001400     03 SEQE-DASENBEK.                                                    
001500*                                 SENASTE BEKRÄFTELSETIDPUNKT             
001600*                                 LATEST CONFIRMATION DATE+TIME           
001700        05 SEQE-DASENDAT     PIC 9(8).                                    
001800*                                 SENASTE BEKRÄFTELSEDATUM                
001900*                                 LATEST CONFIRMATION DATE                
002000*                                 (YYYYMMDD)                              
002100        05 SEQE-TISENBEK-KL  PIC 9(6).                                    
002200*                                 TIM - MIN - SEK   (HHMMSS)              
002300*                                 HOUR - MINUTE - SEC (HHMMSS)            
002400     03 SEQE-IDGMTREF.                                                    
002500*                                 GODSMOTTAGAREREFERENS                   
002600*                                 GOODS RECEIVER REFERENS                 
002700        05 SEQE-IDDISTR      PIC S9(5)           COMP-3.                  
002800*                                 DISTRIKTNUMMER                          
002900*                                 DISTRICT NUMBER                         
003000        05 SEQE-IDKUNDNR     PIC S9(7)           COMP-3.                  
003100*                                 KUNDNUMMER                              
003200*                                 CUSTOMER NO                             
003300        05 SEQE-IDKUNDRF-GRP.                                             
003400*                                 KUNDENS REFERENS (ORDERID)              
003500*                                 CUSTOMER REFERENCE (ORDER ID)           
003600           07 SEQE-IDKUNDRF  PIC X(10).                                   
003700*                                 KUNDENS REFERENS (ORDERID)              
003800*                                 CUSTOMER REFERENCE (ORDER ID)           
003900           07 SEQE-IDORDNR5-FILLER REDEFINES SEQE-IDKUNDRF.               
004000              09 SEQE-IDORDNR5                                            
004100                             PIC 9(5).                                    
004200*                                 ORDERNUMMER                             
004300*                                 ORDER NUMBER                            
004400              09 FILLER      PIC X(5).                                    
004500           07 SEQE-IDORDNR7-FILLER REDEFINES SEQE-IDKUNDRF.               
004600              09 SEQE-IDORDNR7                                            
004700                             PIC 9(7).                                    
004800*                                 ORDERNUMMER                             
004900*                                 ORDER NUMBER                            
005000              09 FILLER      PIC X(3).                                    
005100     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
005200*                                 ARTIKELNUMMER                           
005300*                                 PART NUMBER                             
005400     03 SEQE-IDLOPNR         PIC S9(3)           COMP-3.                  
005500*                                 LÖPNUMMER                               
005600*                                 SEQUENCE NUMBER                         
005700     03 SEQE-IDWDA501        PIC X(24).                                   
005800*                                 NYCKEL TILL WDA501                      
005900*                                 KEY TO WDA501                           
006000*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  

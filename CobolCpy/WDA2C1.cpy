000100 01  SEQC-WDA2C1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 RETURTILLSTÅNDSKÖ                       
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 KDLEVANM = 4, 5 & 6                     
000600*                                 FYSISK NYCKEL: WDA2C1KY                 
000700*                                  (IDFTG    +                            
000800*                                   KDLEVANM + KDARBTYP +                 
000900*                                   IDPERSON + DARETANK +                 
001000*                                   DARETILL + IDLEVANM )                 
001100*                                 SEKUNDÄR NYCKEL: WDA2CSEQ               
001200*                                  (IDFTG    + KDLEVANM +                 
001300*                                   KDARBTYP + IDPERSON +                 
001400*                                   DARETANK + DARETILL)                  
001500     03 SEQC-IDFTG           PIC 9(2).                                    
001600*                                 FÖRETAGSID EKONOM REDOVISNING           
001700*                                 COMPANY IDENTITY ACCOUNTING             
001800     03 SEQC-KDLEVANM        PIC X.                                       
001900*                                 STATUS LEVERANSANMÄRKNING               
002000*                                 STATUS DISCREPANCY                      
002100     03 SEQC-KDARBTYP        PIC X(8).                                    
002200*                                 TYP AV ARBETE                           
002300*                                 CATEGORY OF WORK                        
002400     03 SEQC-IDPERSON        PIC S9(3)           COMP-3.                  
002500*                                 PERSONKOD                               
002600*                                 STAFF CODE                              
002700     03 SEQC-DARETANK        PIC 9(8).                                    
002800*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
002900*                                 DATE GOODS RECEIVING(YYYYMMDD)          
003000     03 SEQC-DARETILL        PIC 9(8).                                    
003100*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
003200*                                 DATE RETURNPERMIT    (YYYYMMDD)         
003300     03 SEQC-IDLEVANM.                                                    
003400*                                 LEVERANSANMÄRKNINGSIDENTITET            
003500*                                 DISCREPANCY REPORT IDENTITY             
003600        05 SEQC-IDDISTR      PIC S9(5)           COMP-3.                  
003700*                                 DISTRIKTNUMMER                          
003800*                                 DISTRICT NUMBER                         
003900        05 SEQC-IDKUNDNR     PIC S9(7)           COMP-3.                  
004000*                                 KUNDNUMMER                              
004100*                                 CUSTOMER NO                             
004200        05 SEQC-IDRAPPNR     PIC 9(7).                                    
004300*                                 RAPPORT NUMMER                          
004400*                                 DISCREPANCY REPORT NUMBER               
004500     03 SEQC-KVRADER-RT      PIC S9(5)           COMP-3.                  
004600*                                 ANTAL RADER RETURTILLSTÅND              
004700*                                 NUMBER OF LINES RETURNPERMIT            
004800     03 SEQC-KVRADER-OBEH    PIC S9(5)           COMP-3.                  
004900*                                 ANTAL OBEHANDLADE RADER                 
005000*                                 NUMBER OF NOT TREATED LINES             
005100*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  

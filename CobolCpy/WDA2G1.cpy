000100 01  SEQG-WDA2G1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 RETURTILLSTÅNDSKÖ                       
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 KDLEVANM = 5 & 6                        
000600*                                 FYSISK NYCKEL: WDA2G1KY                 
000700*                                 (IDFTG + KDARBTYP + IDPERSON +          
000800*                                  DARETANK +DARETILL + IDLEVANM)         
000900*                                 SEKUNDÄR NYCKEL: WDA2GSEQ               
001000*                                 (IDFTG + KDARBTYP + IDPERSON)           
001100     03 SEQG-IDFTG           PIC 9(2).                                    
001200*                                 FÖRETAGSID EKONOM REDOVISNING           
001300*                                 COMPANY IDENTITY ACCOUNTING             
001400     03 SEQG-KDARBTYP        PIC X(8).                                    
001500*                                 TYP AV ARBETE                           
001600*                                 CATEGORY OF WORK                        
001700     03 SEQG-IDPERSON        PIC S9(3)           COMP-3.                  
001800*                                 PERSONKOD                               
001900*                                 STAFF CODE                              
002000     03 SEQG-DARETANK        PIC 9(8).                                    
002100*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
002200*                                 DATE GOODS RECEIVING(YYYYMMDD)          
002300     03 SEQG-DARETILL        PIC 9(8).                                    
002400*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
002500*                                 DATE RETURNPERMIT    (YYYYMMDD)         
002600     03 SEQG-IDLEVANM.                                                    
002700*                                 LEVERANSANMÄRKNINGSIDENTITET            
002800*                                 DISCREPANCY REPORT IDENTITY             
002900        05 SEQG-IDDISTR      PIC S9(5)           COMP-3.                  
003000*                                 DISTRIKTNUMMER                          
003100*                                 DISTRICT NUMBER                         
003200        05 SEQG-IDKUNDNR     PIC S9(7)           COMP-3.                  
003300*                                 KUNDNUMMER                              
003400*                                 CUSTOMER NO                             
003500        05 SEQG-IDRAPPNR     PIC 9(7).                                    
003600*                                 RAPPORT NUMMER                          
003700*                                 DISCREPANCY REPORT NUMBER               
003800     03 SEQG-KDLEVANM        PIC X.                                       
003900*                                 STATUS LEVERANSANMÄRKNING               
004000*                                 STATUS DISCREPANCY                      
004100     03 SEQG-KVRADER-RT      PIC S9(5)           COMP-3.                  
004200*                                 ANTAL RADER RETURTILLSTÅND              
004300*                                 NUMBER OF LINES RETURNPERMIT            
004400     03 SEQG-KVRADER-OBEH    PIC S9(5)           COMP-3.                  
004500*                                 ANTAL OBEHANDLADE RADER                 
004600*                                 NUMBER OF NOT TREATED LINES             
004700*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  

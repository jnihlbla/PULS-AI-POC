000100 01  ALOG-W214ALOG.                                                       
000200*                                 COPYBOOK USED TO LOG ALARMS IN          
000300*                                 LOG DB FOR PP - WDR3                    
000400     03 ALOG-IDSYSTEM        PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600*                                 VOLVO VCCS SYSTEM NUMBER                
000700     03 ALOG-KDLARM          PIC S9(3)           COMP-3.                  
000800*                                 LARMORSAKSKOD                           
000900*                                 ALARM REASON CODE                       
001000     03 ALOG-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 ALOG-IDANSK          PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500*                                 PROCURER NO.                            
001600     03 ALOG-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANT÷RNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 ALOG-TIAAVVD         PIC S9(5)           COMP-3.                  
002000*                                 ≈R - VECKA - DAG   (≈≈VVD)              
002100*                                 YEAR - WEEK - DAY  (YYWWD)              
002200     03 ALOG-IDDISTR         PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 ALOG-TIREGDAT        PIC S9(7)           COMP-3.                  
002600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002700*                                 REGISTRATION DATE (YYMMDD)              
002800     03 ALOG-TIPLANDAT       PIC S9(7)           COMP-3.                  
002900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
003000*                                 YEAR - MONTH - DAY  (YYMMDD)            
003100     03 ALOG-KVAVIS          PIC S9(7)           COMP-3.                  
003200*                                 AVISERAT ANTAL                          
003300*                                 QUANTITY NOTIFIED                       
003400     03 ALOG-KVAVROP         PIC S9(7)           COMP-3.                  
003500*                                 AVROPSKVANTITET                         
003600     03 ALOG-FILLER          PIC X(150).                                  
003700*** END OF VILMAII-COPY LENGTH= 190 BYTES                                 

000100 01  EXTB-WXTR21B.                                                        
000200*                                 COPYBOOK FOR EXTRACT FILE FOR D         
000300*                                 EVIATION ALARMS FROM WDD4               
000400     03 EXTB-KDLARM          PIC S9(3)           COMP-3.                  
000500*                                 LARMORSAKSKOD                           
000600*                                 ALARM REASON CODE                       
000700     03 EXTB-IDARTNR         PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 EXTB-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 EXTB-IDANSK          PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500*                                 PROCURER NO.                            
001600     03 EXTB-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANT÷RNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 EXTB-TEORSLRM        PIC X(25).                                   
002000*                                 INFO OM LARMORSAKSKODEN                 
002100*                                 REASON INFORMATION FOR ALERTS           
002200     03 EXTB-TIREGDAT        PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002400*                                 REGISTRATION DATE (YYMMDD)              
002500     03 EXTB-TIPLANDAT       PIC S9(7)           COMP-3.                  
002600*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002700*                                 YEAR - MONTH - DAY  (YYMMDD)            
002800     03 EXTB-KVAVIS          PIC S9(7)           COMP-3.                  
002900*                                 AVISERAT ANTAL                          
003000*                                 QUANTITY NOTIFIED                       
003100     03 EXTB-KVAVROP         PIC S9(7)           COMP-3.                  
003200*                                 AVROPSKVANTITET                         
003300*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  

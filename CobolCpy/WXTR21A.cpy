000100 01  EXTA-WXTR21A.                                                        
000200*                                 COPYBOOK FOR EXTRACT FILE FOR A         
000300*                                 LARMS FROM WDR5                         
000400     03 EXTA-KDLARM          PIC S9(3)           COMP-3.                  
000500*                                 LARMORSAKSKOD                           
000600*                                 ALARM REASON CODE                       
000700     03 EXTA-IDARTNR         PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 EXTA-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 EXTA-IDANSK          PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500*                                 PROCURER NO.                            
001600     03 EXTA-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANT÷RNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 EXTA-TIAAVVD         PIC S9(5)           COMP-3.                  
002000*                                 ≈R - VECKA - DAG   (≈≈VVD)              
002100*                                 YEAR - WEEK - DAY  (YYWWD)              
002200     03 EXTA-IDDISTR         PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 EXTA-TEORSLRM        PIC X(25).                                   
002600*                                 INFO OM LARMORSAKSKODEN                 
002700*                                 REASON INFORMATION FOR ALERTS           
002800     03 EXTA-TIREGDAT        PIC S9(7)           COMP-3.                  
002900*                                 REGISTRERINGSDATUM (≈≈MMDD)             
003000*                                 REGISTRATION DATE (YYMMDD)              
003100*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  

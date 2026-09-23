000100 01  PRIO-W611PRIO.                                                       
000200*                                 LÄNKAREA TILL W611PRIO -                
000300*                                 FYLL ALLTID I IDDC                      
000400*                                 UPPDATERAR PRIORITERAT KOLLI-PA         
000500*                                 RTI                                     
000600     03 PRIO-IDLEVNR         PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000900     03 PRIO-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 PRIO-IDFS            PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400*                                 ADVICE NOTE NUMBER ODETTE               
001500     03 PRIO-TIAVIDAT        PIC S9(7)           COMP-3.                  
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700*                                 ADVICE NOTE DATE                        
001800     03 PRIO-IDRADNR-INL     PIC S9(5)           COMP-3.                  
001900*                                 RADNUMMER INLEVERANS                    
002000*                                 LINE NUMBER GOODS RECEIVING             
002100     03 PRIO-KVUPPDAT        PIC S9(7)           COMP-3.                  
002200     03 PRIO-KDINLPRIO       PIC S9(3)           COMP-3.                  
002300*                                 PRIORITETSGRUPP                         
002400*                                 PRIORITY GROUP                          
002500     03 PRIO-KVAVIS-PRIO     PIC S9(7)           COMP-3.                  
002600*                                 BERÄKN PRIORITERAD KVANT TOT            
002700*                                 CALC PRIO QUANTITY TOT                  
002800     03 PRIO-IDPGM           PIC X(8).                                    
002900*                                 PROGRAM IDENTITET                       
003000*                                 PROGRAM INTENTITY                       
003100*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  

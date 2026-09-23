000100 01  MID-W3I18501.                                                        
000200*                                 MID-COPYTEXT FÖR W3018500               
000300     03 MID-IDDC-REC-IN      PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-SEND-IN     PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDDC-RED-UT      PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-SEND-UT     PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT            OCCURS 15 TIMES.                             
001200        05 MID-KDCMD         PIC X.                                       
001300*                                 RAD-UPPDATERINGSKOMMANDO                
001400*                                  BLANK  = INGENTING                     
001500*                                  D , B  = DELETE                        
001600*                                  R , Ä  = REPLACE                       
001700*                                  I , N  = INSERT                        
001800        05 MID-IDDC-REC      PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 MID-IDDC-SEND     PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 MID-IDFAKT        PIC Z(6)9.                                   
002300*                                 FAKTURANUMMER                           
002400        05 MID-KDTRSTAT      PIC 9.                                       
002500*                                 TRANSAKTIONSSTATUS                      
002600*** END OF VILMAII-COPY LENGTH= 203 BYTES                                 

000100 01  REQU-W60109I1.                                                       
000200*                                 MID-COPYTEXT FÖR W60109                 
000300     03 REQU-IDLEVNR-KOLLI-KEY                                            
000400                             PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000700     03 REQU-IDOKOLLI-KEY    PIC X(9).                                    
000800*                                 ODETTE KOLLINUMMER                      
000900*                                 ODETTE CASE NUMBER                      
001000     03 REQU-IDLOPNRM-KEY    PIC X(9).                                    
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300*                                 SERIAL NO RECEIVING REPORT              
001400*                                 (0WWDLLLLC)                             
001500     03 REQU-IDDC-KEY        PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 REQU-IDSPRAK         PIC X(2).                                    
001900*                                 2-STÄLLIG ISO SPRÅKKOD                  
002000*                                 2-LETTER ISO LANGUAGE CODE              
002100     03 REQU-KDMATT          PIC X.                                       
002200*                                 MÅTTKOD                                 
002300*                                 MEASUREMENT CODE                        
002400     03 REQU-INPUT.                                                       
002500*                                 INDATA FÖR UPPDATERING                  
002600        05 REQU-ADINLOMR-PRT PIC X(4).                                    
002700*                                 PRINTERPLACERING                        
002800*                                 PLACE OF A PRINTER                      
002900*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  

000100 01  MID-W6I14801.                                                        
000200*                                 MID-COPYTEXT FÖR W60148                 
000300     03 MID-IDLOPNRM-IN      PIC X(8).                                    
000400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000500*                                 (0VVDLLLLK)                             
000600*                                 SERIAL NO RECEIVING REPORT              
000700*                                 (0WWDLLLLC)                             
000800     03 MID-IDLOPNRM-UT      PIC X(8).                                    
000900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100*                                 SERIAL NO RECEIVING REPORT              
001200*                                 (0WWDLLLLC)                             
001300     03 MID-IDLEVNR-IN       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500*                                 SUPPLIER NUMBER                         
001600     03 MID-IDLEVNR-UT       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER                         
001900     03 MID-IDOKOLLI-IN      PIC X(9).                                    
002000*                                 ODETTE KOLLINUMMER                      
002100*                                 ODETTE CASE NUMBER                      
002200     03 MID-IDOKOLLI-UT      PIC X(9).                                    
002300*                                 ODETTE KOLLINUMMER                      
002400*                                 ODETTE CASE NUMBER                      
002500     03 MID-IDDC-IN          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 MID-IDDC-UT          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100     03 MID-IDRADNR-ENTER    PIC 9(4).                                    
003200*                                 RADNUMMER                               
003300*                                 LINE NO                                 
003400     03 MID-IDRADNR-NEXT     PIC 9(4).                                    
003500*                                 RADNUMMER                               
003600*                                 LINE NO                                 
003700     03 MID-INPUT.                                                        
003800*                                 INDATA FÖR UPPDATERING                  
003900        05 MID-KDCMDVAL      OCCURS 12 TIMES                              
004000                             PIC X(3).                                    
004100*                                 GENERELL KOMMANDOKOD                    
004200*                                 GENERAL COMMAND-CODE                    
004300     03 MID-IDRADNR          OCCURS 12 TIMES                              
004400                             PIC 9(4).                                    
004500*                                 RADNUMMER                               
004600*                                 LINE NO                                 
004700*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 

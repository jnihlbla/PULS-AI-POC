000100 01  MID-W6I14301.                                                        
000200*                                 MID-COPYTEXT FÖR W6014300               
000300     03 MID-IDLEVNR-KOLLI-IN PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER KOLLI                  
000500*                                 SUPPLIER NUMBER CASE                    
000600     03 MID-IDLEVNR-KOLLI-UT PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER KOLLI                  
000800*                                 SUPPLIER NUMBER CASE                    
000900     03 MID-IDOKOLLI-IN      PIC X(9).                                    
001000*                                 ODETTE KOLLINUMMER                      
001100*                                 ODETTE CASE NUMBER                      
001200     03 MID-IDOKOLLI-UT      PIC X(9).                                    
001300*                                 ODETTE KOLLINUMMER                      
001400*                                 ODETTE CASE NUMBER                      
001500     03 MID-IDLOPNRM-IN      PIC X(9).                                    
001600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001700*                                 (0VVDLLLLK)                             
001800*                                 SERIAL NO RECEIVING REPORT              
001900*                                 (0WWDLLLLC)                             
002000     03 MID-IDLOPNRM-UT      PIC X(9).                                    
002100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002200*                                 (0VVDLLLLK)                             
002300*                                 SERIAL NO RECEIVING REPORT              
002400*                                 (0WWDLLLLC)                             
002500     03 MID-IDDC-IN          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 MID-IDDC-UT          PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100     03 MID-INPUT-OVR.                                                    
003200*                                 INDATA FÖR UPPDATERING                  
003300        05 MID-KVINLART-DIN-UPP                                           
003400                             PIC 9(6).                                    
003500*                                 ANTAL I PARTIRAD                        
003600*                                 QTY/LINE IN A LOT                       
003700        05 MID-KVINLART-UPP  PIC 9(6).                                    
003800*                                 ANTAL I PARTIRAD                        
003900*                                 QTY/LINE IN A LOT                       
004000        05 MID-IDANSTNR-UPP  PIC 9(5).                                    
004100*                                 ANSTÄLLNINGSNUMMER                      
004200*                                 IDENTIFICATION NO EMPLOYEE              
004300        05 MID-ADINLOMR-NXT-UPP                                           
004400                             PIC X(4).                                    
004500*                                 INLEVERANSOMRÅDE NÄSTA                  
004600*                                 RECEIVING AREA NEXT                     
004700*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  

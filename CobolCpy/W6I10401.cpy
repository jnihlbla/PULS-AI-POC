000100 01  MID-W6I10401.                                                        
000200*                                 MID-COPYTEXT FÖR W60104                 
000300     03 MID-IDLEVNR-KOLLI-IN PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER KOLLI                  
000500     03 MID-IDLEVNR-KOLLI-UT PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER KOLLI                  
000700     03 MID-IDOKOLLI-IN      PIC X(9).                                    
000800*                                 ODETTE KOLLINUMMER                      
000900     03 MID-IDOKOLLI-UT      PIC X(9).                                    
001000*                                 ODETTE KOLLINUMMER                      
001100     03 MID-IDLOPNRM-IN      PIC X(9).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400     03 MID-IDLOPNRM-UT      PIC X(9).                                    
001500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001600*                                 (0VVDLLLLK)                             
001700     03 MID-IDDC-IN          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-IDDC-UT          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDRADNR-INL-ENTER                                             
002200                             PIC 9(8).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600     03 MID-IDFS-ENTER       PIC X(8).                                    
002700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002800     03 MID-TIAVIDAT-ENTER   PIC 9(6).                                    
002900*                                 AVISERINGSDATUM (YYMMDD)                
003000     03 MID-IDRADNR-INL-NEXT PIC 9(8).                                    
003100*                                 ARTIKELNUMMER                           
003200     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400     03 MID-IDFS-NEXT        PIC X(8).                                    
003500*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003600     03 MID-TIAVIDAT-NEXT    PIC 9(6).                                    
003700*                                 AVISERINGSDATUM (YYMMDD)                
003800     03 MID-IDLOPNRM-DEF     PIC 9(9).                                    
003900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004000*                                 (0VVDLLLLK)                             
004100     03 MID-IDRADNR-DEF      PIC 9(4).                                    
004200*                                 RADNUMMER                               
004300     03 MID-INPUT.                                                        
004400*                                 INDATA FÖR UPPDATERING                  
004500        05 MID-KVINLART-UPD  PIC X(6).                                    
004600*                                 ANTAL I PARTIRAD                        
004700        05 MID-IDLOPNRM-UPD  PIC X(9).                                    
004800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004900*                                 (0VVDLLLLK)                             
005000        05 MID-IDRADNR-UPD   PIC X(4).                                    
005100*                                 RADNUMMER                               
005200        05 MID-ADINLOMR-PRT-UPD                                           
005300                             PIC X(4).                                    
005400*                                 INLEVERANSOMRÅDE                        
005500*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 

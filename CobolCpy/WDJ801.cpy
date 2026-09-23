000100 01  LOC-WDJ801.                                                          
000200*                                 PLATS REGISTER                          
000300*                                 FYSISK NYCKEL: WDJ801KY                 
000400*                                 (IDDC+ADLAGOMR+ADGANG+ADPLATS)          
000500     03 LOC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 LOC-ADLAGOMR         PIC 9(2).                                    
000900*                                 LAGEROMRÅDE                             
001000*                                 AREA                                    
001100     03 LOC-ADGANG           PIC 9(2).                                    
001200*                                 GÅNG                                    
001300*                                 AISLE                                   
001400     03 LOC-ADPLATS          PIC 9(5).                                    
001500*                                 LAGERPLATSNUMMER                        
001600*                                 LOCATION                                
001700     03 LOC-KDLOC            PIC X.                                       
001800*                                 TYP AV LAGERPLATS                       
001900*                                 TYPE OF LOCATION                        
002000     03 LOC-KDFREQ           PIC X(2).                                    
002100*                                 FREQUENCY CODE                          
002200*                                 FREQUENCY CODE                          
002300     03 LOC-KDSTOR           PIC X(3).                                    
002400*                                 STORAGE CODE                            
002500*                                 STORAGE CODE                            
002600     03 LOC-KVMPART          PIC 9(2).                                    
002700*                                 NUMBER OF PARTNUMBERS ON A LOCA         
002800*                                 TION                                    
002900*                                 NUMBER OF PARTNUMBER                    
003000     03 LOC-TELOC            PIC X(15).                                   
003100*                                 LOCATION INFORMATION                    
003200*                                 LOCATION INFORMATION                    
003300     03 LOC-KVPLATS          PIC S9(3)           COMP-3.                  
003400*                                 NO OF ADDRESSES IN A BAY                
003500*                                 ANTAL LAGERPLATSER I ETT STÄLL          
003600     03 LOC-FILLER           PIC X(5).                                    
003700*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  

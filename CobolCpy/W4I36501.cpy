000100 01  MID-W4I36501.                                                        
000200*                                 MID-COPYTEXT FÖR W4036500               
000300     03 MID-PRCNR-IN.                                                     
000400*                                 PRODUKTIONSKANAL                        
000500        05 MID-IDPRCBAS      PIC X(3).                                    
000600*                                 PRC-BAS                                 
000700        05 MID-IDPRCVAR      PIC X.                                       
000800*                                 PRC-VARIANT                             
000900     03 MID-PRCNR-UT.                                                     
001000*                                 PRODUKTIONSKANAL                        
001100        05 MID-IDPRCBAS      PIC X(3).                                    
001200*                                 PRC-BAS                                 
001300        05 MID-IDPRCVAR      PIC X.                                       
001400*                                 PRC-VARIANT                             
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-LAGOMR-ENTER     PIC 9(2).                                    
002000*                                 LAGEROMRÅDE                             
002100     03 MID-LAGOMR-NEXT      PIC 9(2).                                    
002200*                                 LAGEROMRÅDE                             
002300     03 MID-KDPRTGEN-PU      PIC X(3).                                    
002400*                                 PRINTERKOD PACKUNDERLAG                 
002500     03 MID-KDPRTGEN-PLE     PIC X(3).                                    
002600*                                 PRINTERKOD PLOCKETIKETTER               
002700     03 MID-LAGOMR-UPP       PIC 9(2).                                    
002800*                                 LAGEROMRÅDE                             
002900     03 MID-INPUT.                                                        
003000*                                 INDATA FÖR UPPDATERING                  
003100        05 MID-IDPRC-UPP.                                                 
003200*                                 PRODUKTIONSKANAL                        
003300           07 MID-IDPRCBAS   PIC X(3).                                    
003400*                                 PRC-BAS                                 
003500           07 MID-IDPRCVAR   PIC X.                                       
003600*                                 PRC-VARIANT                             
003700        05 MID-KDPRT-PU-UPP  PIC X(3).                                    
003800*                                 PRINTERKOD PACKUNDERLAG                 
003900        05 MID-KDSS-PU-UPP   PIC X.                                       
004000*                                 SIDOSKIPSKOD                            
004100        05 MID-KDPRT-PLE-UPP PIC X(3).                                    
004200*                                 PRINTERKOD PLOCKETIKETTER               
004300        05 MID-KDSS-PLE-UPP  PIC X.                                       
004400*                                 SIDOSKIPSKOD                            
004500*** END COPY W4I36501    LENGTH=36                                        

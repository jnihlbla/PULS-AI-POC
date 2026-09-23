000100 01  FEL-W15212.                                                          
000200*                                 TEXT SOM BLIVIT ÖVERSATT PÅ             
000300*                                 EXTERNT FÖRETAG OCH SOM AV NÅN          
000400*                                 ANLEDNING INTE KAN GODKÄNNAS.           
000500     03 FEL-DATADEL.                                                      
000600        05 FEL-IDPTYP        PIC X(4).                                    
000700*                                 POSTTYP              IDPTYP-004         
000800        05 FEL-KOMMA1        PIC X.                                       
000900        05 FEL-DIFAELT       PIC 9(3).                                    
001000*                                 FÄLTLÄNGD                               
001100        05 FEL-KOMMA2        PIC X.                                       
001200        05 FEL-IDBENNR       PIC 9(7).                                    
001300*                                 BENÄMNINGSNUMMER                        
001400        05 FEL-KOMMA3        PIC X.                                       
001500        05 FEL-IDSPRAK       PIC X(2).                                    
001600*                                 2-STÄLLIG ISO SPRÅKKOD                  
001700        05 FEL-KOMMA4        PIC X.                                       
001800        05 FEL-OVERSATT-TEXT PIC X(330).                                  
001900*                                 TEXT DÄR SÖKNING SKER                   
002000     03 FEL-FELTEXT          PIC X(73).                                   
002100*                                 TEXT ÖVRIGA MEDDELANDEN                 
002200*** END OF VILMAII-COPY LENGTH= 423 BYTES                                 

000100 01  PBEN-W152PBX.                                                        
000200*                                 PTYP = PBEN                             
000300*                                 ARTIKELBENÄMNING SOM SKA                
000400*                                 SKICKAS FÖR ÖVERSÄTTNING TILL           
000500*                                 SPRÅK = IDSPRAK.                        
000600*                                                                         
000700*                                 DIFAELT = 90 = MAX LÄNGD                
000800*                                 FÖR DEN ÖVERSATTA TEXTEN.               
000900*                                 SOM TAS EMOT MED CTX=W152PBEN           
001000*                                                                         
001100     03 PBEN-IDPTYP          PIC X(4).                                    
001200*                                 POSTTYP              IDPTYP-004         
001300     03 PBEN-KOMMA1          PIC X.                                       
001400     03 PBEN-DIFAELT         PIC 9(3).                                    
001500*                                 FÄLTLÄNGD                               
001600     03 PBEN-KOMMA2          PIC X.                                       
001700     03 PBEN-IDBENNR         PIC 9(7).                                    
001800*                                 BENÄMNINGSNUMMER                        
001900     03 PBEN-KOMMA3          PIC X.                                       
002000     03 PBEN-IDSPRAK         PIC X(2).                                    
002100*                                 2-STÄLLIG ISO SPRÅKKOD                  
002200     03 PBEN-KOMMA4          PIC X.                                       
002300     03 PBEN-BEARTEXT-EN     PIC X(100).                                  
002400*                                 UTÖKAD ARTIKELBENÄMNING                 
002500     03 PBEN-KOMMA5          PIC X.                                       
002600     03 PBEN-BEARTEXT-SV     PIC X(100).                                  
002700*                                 UTÖKAD ARTIKELBENÄMNING                 
002800     03 PBEN-KOMMA6          PIC X.                                       
002900     03 PBEN-IDFKNGRP        PIC 9(4).                                    
003000*                                 FUNKTIONSGRUPP                          
003100     03 PBEN-KOMMA7          PIC X.                                       
003200     03 PBEN-TENOTE          PIC X(40).                                   
003300*                                 NOTERINGSFÄLT                           
003400*** END OF VILMAII-COPY LENGTH= 267 BYTES                                 

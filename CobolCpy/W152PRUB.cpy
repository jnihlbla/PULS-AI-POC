000100 01  PRUB-W152PRUB.                                                       
000200*                                 RUBRIKTEXT-SEGMENT SOM SKA              
000300*                                 UPPDATERAS EFTER ÖVERSÄTTNING           
000400*                                 PTYP = PRUB                             
000500*                                 DIFAELT = 84 = 3*30-3*2                 
000600*                                 DIFAELT = 28 = 1*30-1*2                 
000700*                                 ÄR TOTAL LÄNGD PÅ RUBRIKTEXT            
000800*                                 I DEN MOTTAGNA FILEN.                   
000900*                                 VID 84 FINNS MAX 3 UPD-POSTER           
001000*                                                                         
001100     03 PRUB-IDPTYP          PIC X(4).                                    
001200*                                 POSTTYP              IDPTYP-004         
001300     03 PRUB-KOMMA1          PIC X.                                       
001400     03 PRUB-DIFAELT         PIC 9(3).                                    
001500*                                 FÄLTLÄNGD                               
001600     03 PRUB-KOMMA2          PIC X.                                       
001700     03 PRUB-IDRUBNR         PIC 9(5).                                    
001800*                                 RUBRIKNUMMER                            
001900     03 PRUB-KOMMA3          PIC X.                                       
002000     03 PRUB-IDSPRAK         PIC X(2).                                    
002100*                                 2-STÄLLIG ISO SPRÅKKOD                  
002200     03 PRUB-KOMMA4          PIC X.                                       
002300     03 PRUB-BERUBTXT        PIC X(30).                                   
002400*                                 RUBRIKTEXT                              
002500*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  

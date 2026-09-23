000100 01  PFOT-W152PFOT.                                                       
000200*                                 FOTNOTSTEXT-SEGMENT SOM SKA             
000300*                                 UPPDATERAS EFTER ÖVERSÄTTNING           
000400*                                 PTYP = PFOT                             
000500*                                 DIFAELT =159 = 3*55-3*2                 
000600*                                 ÄR TOTAL LÄNGD PÅ FOTNOTSTEXT           
000700*                                 I DEN MOTTAGNA FILEN.                   
000800*                                 MAX 3 UPD-POSTER                        
000900*                                                                         
001000     03 PFOT-IDPTYP          PIC X(4).                                    
001100*                                 POSTTYP              IDPTYP-004         
001200     03 PFOT-KOMMA1          PIC X.                                       
001300     03 PFOT-DIFAELT         PIC 9(3).                                    
001400*                                 FÄLTLÄNGD                               
001500     03 PFOT-KOMMA2          PIC X.                                       
001600     03 PFOT-IDFOTNR         PIC 9(5).                                    
001700*                                 FOTNOTSNUMMER                           
001800     03 PFOT-KOMMA3          PIC X.                                       
001900     03 PFOT-IDSPRAK         PIC X(2).                                    
002000*                                 2-STÄLLIG ISO SPRÅKKOD                  
002100     03 PFOT-KOMMA4          PIC X.                                       
002200     03 PFOT-BEFOTNOT        PIC X(55).                                   
002300*                                 FOTNOTSTEXT                             
002400*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  

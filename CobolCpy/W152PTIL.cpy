000100 01  PTIL-W152PTIL.                                                       
000200*                                 TILLÄGGSTEXT-SEGMENT SOM SKA            
000300*                                 UPPDATERAS EFTER ÖVERSÄTTNING           
000400*                                 PTYP = PTIL                             
000500*                                 DIFAELT = 23 = TOTAL LÄNGD PÅ           
000600*                                 BETTEXT I DEN MOTTAGNA FILEN            
000700     03 PTIL-IDPTYP          PIC X(4).                                    
000800*                                 POSTTYP              IDPTYP-004         
000900     03 PTIL-KOMMA1          PIC X.                                       
001000     03 PTIL-DIFAELT         PIC 9(3).                                    
001100*                                 FÄLTLÄNGD                               
001200     03 PTIL-KOMMA2          PIC X.                                       
001300     03 PTIL-IDTTEXNR        PIC 9(5).                                    
001400*                                 TILLÄGGSTEXT-NR                         
001500     03 PTIL-KOMMA3          PIC X.                                       
001600     03 PTIL-IDSPRAK         PIC X(2).                                    
001700*                                 2-STÄLLIG ISO SPRÅKKOD                  
001800     03 PTIL-KOMMA4          PIC X.                                       
001900     03 PTIL-BETTEXT         PIC X(25).                                   
002000*                                 TILLÄGGSTEXT                            
002100*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  

000100 01  1142-WDGX1142.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV, NEDCAR            
000300*                                 TRANSAR TILL PV, NEDCAR, PARTS          
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500*                                 VÄRDE = "1"  = PV                       
000600*                                 VÄRDE = "2"  = NEDCAR                   
000700*                                 VÄRDE = "3"  = PARTS                    
000800*                                 VÄRDE = "4"  = PV FÖRP. MISSING         
000900*                                 VÄRDE = "5"  = NAP                      
001000     03 1142-KDSEGKEY        PIC X.                                       
001100*                                 TEKNISK SEGMENT-NYCKEL                  
001200*                                 TECHNICAL SEGMENT KEY                   
001300     03 1142-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 1142-KDSVAR          PIC X.                                       
001700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001800*                                 RETURN CODE FROM PROGRAM                
001900     03 FILLER               PIC X(3).                                    
002000*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  

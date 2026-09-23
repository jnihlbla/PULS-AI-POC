000100 01  W2131142.                                                            
000200*                                 POSTER FÖR UPPD AV HÄNDELSEBAS          
000300*                                                                         
000400     03 IDHTYP               PIC X(4).                                    
000500*                                 HÄNDELSETYP                             
000600     03 WDGX1142.                                                         
000700*                                 NYA ARTIKLAR FRÅN PV, NEDCAR            
000800*                                 TRANSAR TILL PV, NEDCAR, PARTS          
000900*                                 FYSISK NYCKEL: KDSEGKEY                 
001000*                                 VÄRDE = "1"  = PV                       
001100*                                 VÄRDE = "2"  = NEDCAR                   
001200*                                 VÄRDE = "3"  = PARTS                    
001300        05 KDSEGKEY          PIC X.                                       
001400*                                 TEKNISK SEGMENT-NYCKEL                  
001500        05 IDARTNR           PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700        05 KDSVAR            PIC X.                                       
001800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001900        05 FILLER            PIC X(3).                                    
002000*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  

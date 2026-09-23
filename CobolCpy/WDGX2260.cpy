000100 01  2260-WDGX2260.                                                       
000200*                                 BLOCKADE AVROPSPERIODER                 
000300*                                 ANSKAFFARE OCH AVROPSVECKOR             
000400*                                 NYCKEL = KY2260                         
000500*                                 (DAAVROP-FOM + DAAVROP-TOM              
000600*                                  + IDANSK-FOM + IDANSK-TOM)             
000700*                                                                         
000800     03 2260-DAAVROP-FOM     PIC 9(6).                                    
000900*                                 STARTVECKA ≈TGƒRD AVROP                 
001000*                                 (≈≈≈≈VV)                                
001100     03 2260-DAAVROP-TOM     PIC 9(6).                                    
001200*                                 SLUTVECKA ≈TGƒRD AVROP                  
001300*                                 (≈≈≈≈VV)                                
001400     03 2260-IDANSK-FOM      PIC S9(3)           COMP-3.                  
001500*                                 LƒGSTA ANSKAFFARNR I INTERVALL          
001600     03 2260-IDANSK-TOM      PIC S9(3)           COMP-3.                  
001700*                                 H÷GSTA ANSKAFFARNR I INTERVALL          
001800     03 2260-DAAVROP-TFOM    PIC 9(6).                                    
001900*                                 TIDIGARELAGD STARTVECKA AVROP           
002000*                                 (≈≈≈≈VV)                                
002100*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  

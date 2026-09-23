000100 01  RY7-W236RY7.                                                         
000200*                                 RY7 - SIGNAL OM F÷R TIDIG               
000300*                                 INLEVERANS.                             
000400     03 RY7-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 RY7-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 RY7-IDLEVNR          PIC X(5).                                    
000900*                                 LEVERANT÷RNUMMER                        
001000     03 RY7-TIAVROP-INL      PIC S9(5)           COMP-3.                  
001100*                                 INLEVERANSDATUM (PLANERAD)              
001200*                                 (≈≈VV)                                  
001300     03 RY7-TIANKDAG         PIC S9(5)           COMP-3.                  
001400*                                 ≈R - VECKA  (≈≈VV)                      
001500     03 RY7-KVAVIS           PIC S9(7)           COMP-3.                  
001600*                                 AVISERAT ANTAL                          
001700*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  

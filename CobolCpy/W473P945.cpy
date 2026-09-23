000100 01  W473P945.                                                            
000200*                                 POSTER SOM BOKAR UPP LAGERSALDO         
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 KVAVART              PIC S9(7)           COMP-3.                  
001100*                                 AVVIKANDE ANTAL ARTIKLAR                
001200     03 KDAVVORS             PIC S9              COMP-3.                  
001300*                                 AVVIKELSEORSAK                          
001400*                                 1 = FYSISK NOLLNING                     
001500*                                 2 = ANNULLERING                         
001600     03 KVAVART-R            PIC S9(7)           COMP-3.                  
001700*                                 AVVIKANDE ANTAL ARTIKLAR                
001800     03 KDORDKL              PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 TIAAP                PIC S9(3)           COMP-3.                  
002100*                                 ≈R - PLANERINGSPERIOD (≈≈P)             
002200*                                 8 PER ≈R                                
002300     03 KDORDKAT             PIC S9              COMP-3.                  
002400*                                 ORDERKATEGORI                           
002500     03 FILLER               PIC X(17).                                   
002600*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  

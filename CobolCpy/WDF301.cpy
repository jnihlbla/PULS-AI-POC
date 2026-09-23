000100 01  HLG-WDF301.                                                          
000200*                                 HELGDAGAR/LAND                          
000300*                                 FYSISK NYCKEL: WDF301KY                 
000400*                                 IDLANDX2, DADATUM-HELG                  
000500     03 HLG-IDLANDX2         PIC X(2).                                    
000600*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
000700*                                 2-LETTER CODE FOR COUNTRY               
000800     03 HLG-DADATUM-HELG     PIC 9(8).                                    
000900*                                 HELGDAGAR (≈≈≈≈MMDD)                    
001000*                                 HOLIDAYS (YYYYMMDD)                     
001100     03 HLG-FLHELG           PIC X.                                       
001200*                                 NATIONELL HELGDAG = J                   
001300*                                 PUBLIC HOLIDAY = J                      
001400     03 HLG-FILLER           PIC X(9).                                    
001500*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  

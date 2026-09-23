000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - DISTRIKT SOM SKALL SÄNDA /            
000400*                            ***    MOTTAGA LEVERANSANM. TRANSAR          
000500*                            ***    OCH EJ ÄR NOAC-DISTRIKT               
000600*                            ***                                          
000700*                            *************************************        
000800 01  DIS100-IDDISTR         PIC  9(5)    COMP-3.                          
000900*                                                                         
001100   88  DIS100-ITALLEVANM-PV  VALUE  1820.                                 
001300   88  DIS100-DANMLEVANM-PV  VALUE  0970.                                 
001500   88  DIS100-TYSKLEVANM-PV  VALUE  2270.                                 
001610   88  DIS100-FRANLEVANM-PV  VALUE  1470.                                 
001800   88  DIS100-SPANLEVANM-PV  VALUE  2170.                                 
002100   88  DIS100-NORGLEVANM-PV  VALUE  0870.                                 
002210   88  DIS100-FINLLEVANM     VALUE  1000.                                 
002220   88  DIS100-HOLLLEVANM-PV  VALUE  1620.                                 
002230   88  DIS100-JAPANLEVANM    VALUE  5220.                                 
002300                                                                          
002400*                                                                         
002500*** END COPY WWDIS100C0  LENGTH=3     OLD LENGTH=3                        

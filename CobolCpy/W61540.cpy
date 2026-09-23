000100 01  W61540.                                                              
000200*                                 LAGERFYLLNADSGRAD                       
000300     03 IDDC                 PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000600*                                 LAGEROMRÅDE                             
000700     03 ADGANG               PIC S9(3)           COMP-3.                  
000800*                                 GÅNG                                    
000900     03 ADPLATS              PIC S9(5)           COMP-3.                  
001000*                                 LAGERPLATSNUMMER                        
001100     03 KDLOC                PIC X.                                       
001200*                                 TYP AV LAGERPLATS                       
001300     03 KDSTOR               PIC X(3).                                    
001400*                                 STORAGE CODE                            
001500     03 TESTORAGE            PIC X(18).                                   
001600*                                 STORAGE INFORMATION                     
001700     03 KDFREQ               PIC X(2).                                    
001800*                                 FREQUENCY CODE                          
001900     03 KVPB-FOM             PIC S9(6)V9(1)      COMP-3.                  
002000*                                 PERIODBEHOV FOM (PROGNOS)               
002100     03 KVPB-TOM             PIC S9(6)V9(1)      COMP-3.                  
002200*                                 PERIODBEHOV TOM (PROGNOS)               
002300     03 TEFREQ               PIC X(10).                                   
002400*                                 FREQUENCY TYPE INFORMATION              
002500*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  

000100* GENERATION OF COBOL HOST STRUCTURE FROM T01SYST-TAB                     
000200  01 T01SYST.                                                             
000300*              T01SYST                                                    
000400   03 IDSYSTEM                          PIC X(4).                         
000500*              VOLVO VCCS SYSTEMNUMMER                                    
000600   03 KVMINUT                           PIC S9(5) COMP-3.                 
000700*              ANTAL MINUTER                                              
000800   03 KVDAGAR                           PIC S9(3) COMP-3.                 
000900*              ANTAL DAGAR                                                
001000   03 KDBEH                             PIC X(1).                         
001100*              BEHANDLINGSKOD                                             
001200   03 KDBEHX                            PIC X(1).                         
001300*              BEHANDLINGSKOD-X                                           
001400   03 KDBEHS                            PIC X(1).                         
001500*              BEHANDLINGSKOD-S                                           
001600   03 DAEXDAT                           PIC X(8).                         
001700*              EXEKVERINGSDATUM (≈≈≈≈MMDD)                                
001800   03 TIEXTID                           PIC S9(7) COMP-3.                 
001900*              EXEKVERINGSTIDPUNKT                                        
002000   03 DAREGDAT                          PIC X(8).                         
002100*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
002200   03 DAUPPDAT                          PIC X(8).                         
002300*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
002400   03 IDUSER                            PIC X(8).                         
002500*              ANVƒNDARENS SƒKERHETS ID                                   
002600*                                                                         
002700*** END OF VILMAII-COPY LENGTH= 48 OLD LENGTH=                            

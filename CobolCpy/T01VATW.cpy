000100* GENERATION OF COBOL HOST STRUCTURE FROM T01VATW-TAB                     
000200  01 T01VATW.                                                             
000300*              T01VATW                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 IDLANDX2                          PIC X(2).                         
000700*              2-STƒLLIG LANDSBETECKNINGSKOD                              
000800   03 KDVAT                             PIC X(2).                         
000900*              MOMSKOD                                                    
001000   03 REVAT                             PIC S9(3)V9(2) COMP-3.            
001100*              MULTIPLIKATIONSFAKTOR F÷R MOMS                             
001200   03 BEVAT                             PIC X(50).                        
001300*              MOMSKODSBENƒMNING R3                                       
001400   03 DAREGDAT                          PIC X(8).                         
001500*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001600   03 DAUPPDAT                          PIC X(8).                         
001700*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
001800   03 DADELDAT                          PIC X(8).                         
001900*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
002000   03 IDUSER                            PIC X(8).                         
002100*              ANVƒNDARENS SƒKERHETS ID                                   
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 93 OLD LENGTH=                            

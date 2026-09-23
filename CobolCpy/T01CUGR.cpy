000100* GENERATION OF COBOL HOST STRUCTURE FROM T01CUGR-TAB                     
000200  01 T01CUGR.                                                             
000300*              T01CUGR                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 KDPARTTY                          PIC X(3).                         
000700*              TYP AV BETALARE                                            
000800   03 KDPARTGR                          PIC X(15).                        
000900*              GRUPP AV BETALARE                                          
001000   03 KDSTATUS                          PIC S9(3) COMP-3.                 
001100*              STATUSKOD          KDSTATUS-002                            
001200   03 KDINVFRQ                          PIC X(4).                         
001300*              FAKTURERINGSFREKVENS                                       
001400   03 KDAPPEND                          PIC X(4).                         
001500*              APPENDIX KOD                                               
001600   03 DAREGDAT                          PIC X(8).                         
001700*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001800   03 DAUPPDAT                          PIC X(8).                         
001900*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
002000   03 DADELDAT                          PIC X(8).                         
002100*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
002200   03 IDUSER                            PIC X(8).                         
002300*              ANVƒNDARENS SƒKERHETS ID                                   
002400   03 FLVAT                             PIC X(1).                         
002500*              MOMS P≈ FAKTURA                                            
002600*                                                                         
002700*** END OF VILMAII-COPY LENGTH= 65 OLD LENGTH=                            

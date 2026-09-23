000100* GENERATION OF COBOL HOST STRUCTURE FROM T01IVW-TAB                      
000200  01 T01IVW.                                                              
000300*              T01IVW                                                     
000400   03 DAREGDAT   PIC X(8).                                                
000500*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
000600   03 TIREGTID   PIC S9(7) COMP-3.                                        
000700*              REGISTRERINGSTID                                           
000800   03 IDLOPNR    PIC S9(5) COMP-3.                                        
000900*              L÷PNUMMER          IDLOPNR-002                             
001000   03 IDPTYP     PIC X(3).                                                
001100*              POSTTYP                                                    
001200   03 TIRP       PIC S9(2) COMP-3.                                        
001300*              REDOVISNINGSPERIOD                                         
001400*              12 PER ≈R                                                  
001500   03 FLKLAR     PIC X(1).                                                
001600*              AVSLUTNINGSMARKERING                                       
001700   03 IV-DATA    PIC X(200).                                              
001800   03 IV-DATA2   PIC X(200).                                              
001900*                                                                         
002000*** END OF VILMAII-COPY LENGTH= 421 OLD LENGTH=                           

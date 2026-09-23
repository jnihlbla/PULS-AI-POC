000100* GENERATION OF COBOL HOST STRUCTURE FROM T01PROC-TAB                     
000200  01 T01PROC.                                                             
000300*              T01PROC                                                    
000400   03 IDSYSTEM                          PIC X(4).                         
000500*              VOLVO VCCS SYSTEMNUMMER                                    
000600   03 KDBEH                             PIC X(1).                         
000700*              BEHANDLINGSKOD                                             
000800   03 DAEXDAT                           PIC X(8).                         
000900*              EXEKVERINGSDATUM (≈≈≈≈MMDD)                                
001000   03 TIEXTID                           PIC S9(7) COMP-3.                 
001100*              EXEKVERINGSTIDPUNKT                                        
001200   03 IDLEGSEL                          PIC X(4).                         
001300*              FAKTURERANDE F÷RETAG TEX VCCS                              
001400*                                                                         
001500*** END OF VILMAII-COPY LENGTH= 21 OLD LENGTH=                            

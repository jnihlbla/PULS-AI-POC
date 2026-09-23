000100* GENERATION OF COBOL HOST STRUCTURE FROM T01TBUN-TAB                     
000200  01 T01TBUN.                                                             
000300*              T01TBUN                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 IDBUNDLE                          PIC X(15).                        
000700*              BUNDLE ID                                                  
000800   03 DAREGDAT                          PIC X(8).                         
000900*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001000   03 TIREGTID                          PIC S9(10) COMP-3.                
001100*              REGISTRERINGSTID                                           
001200   03 FLFEL                             PIC X(1).                         
001300*              ALLMƒN FELFLAGGA                                           
001400   03 FLKNTRL                           PIC X(1).                         
001500*              FLAGGA DATAKONTROLL                                        
001600   03 FLKLAR                            PIC X(1).                         
001700*              AVSLUTNINGSMARKERING                                       
001800*                                                                         
001900*** END OF VILMAII-COPY LENGTH= 36 OLD LENGTH=                            

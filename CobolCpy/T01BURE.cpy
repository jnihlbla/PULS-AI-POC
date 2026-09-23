000100* GENERATION OF COBOL HOST STRUCTURE FROM T01BURE-TAB                     
000200  01 T01BURE.                                                             
000300*              T01BURE                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 KDFINDOC                          PIC X(4).                         
000700*              TYP FINANSIELLT DOKUMENT                                   
000800   03 KDPARTTY                          PIC X(3).                         
000900*              TYP AV BETALARE                                            
001000   03 KDPARTGR                          PIC X(15).                        
001100*              GRUPP AV BETALARE                                          
001200   03 KDSTATUS                          PIC S9(3) COMP-3.                 
001300*              STATUSKOD          KDSTATUS-002                            
001400   03 BEFORMS                           PIC X(15).                        
001500*              BENƒMNING P≈ DOKUMENTFORMAT                                
001600   03 FLGL                              PIC X(1).                         
001700*              BOKF÷RINGSTRANSAR                                          
001800   03 FLAR                              PIC X(1).                         
001900*              TILL KUNDRESKONTRA                                         
002000   03 FLAP                              PIC X(1).                         
002100*              LEVERANT÷RSRESKONTA                                        
002200   03 DAREGDAT                          PIC X(8).                         
002300*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
002400   03 DAUPPDAT                          PIC X(8).                         
002500*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
002600   03 DADELDAT                          PIC X(8).                         
002700*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
002800   03 IDUSER                            PIC X(8).                         
002900*              ANVƒNDARENS SƒKERHETS ID                                   
003000   03 FLVATCHK                          PIC X(1).                         
003100*              MOMSKONTROLL                                               
003200*                                                                         
003300*** END OF VILMAII-COPY LENGTH= 79 OLD LENGTH=                            

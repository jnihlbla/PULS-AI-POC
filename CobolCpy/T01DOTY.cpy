000100* GENERATION OF COBOL HOST STRUCTURE FROM T01DOTY-TAB                     
000200  01 T01DOTY.                                                             
000300*              T01DOTY                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 KDFINDOC                          PIC X(4).                         
000700*              TYP FINANSIELLT DOKUMENT                                   
000800   03 KDSTATUS                          PIC S9(3) COMP-3.                 
000900*              STATUSKOD          KDSTATUS-002                            
001000   03 BEFINDOC                          PIC X(35).                        
001100*              FINANSIELLT DOKUMENT                                       
001200   03 DAREGDAT                          PIC X(8).                         
001300*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001400   03 DAUPPDAT                          PIC X(8).                         
001500*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
001600   03 DADELDAT                          PIC X(8).                         
001700*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
001800   03 IDUSER                            PIC X(8).                         
001900*              ANVƒNDARENS SƒKERHETS ID                                   
002000   03 FLAP                              PIC X(1).                         
002100*              LEVERANT÷RSRESKONTA                                        
002200   03 FLAR                              PIC X(1).                         
002300*              TILL KUNDRESKONTRA                                         
002400   03 FLGL                              PIC X(1).                         
002500*              BOKF÷RINGSTRANSAR                                          
002600   03 FLINTREP                          PIC X(1).                         
002700*              INTRASTATRAPPORTERING                                      
002800   03 FLVATREP                          PIC X(1).                         
002900*              MOMSRAPPORT                                                
003000   03 FLCUSREP                          PIC X(1).                         
003100*              TULLRAPPORT                                                
003200*                                                                         
003300*** END OF VILMAII-COPY LENGTH= 83 OLD LENGTH=                            

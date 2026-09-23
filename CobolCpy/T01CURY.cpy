000100* GENERATION OF COBOL HOST STRUCTURE FROM T01CURY-TAB                     
000200  01 T01CURY.                                                             
000300*              T01CURY                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 KDVALISO                          PIC X(5).                         
000700*              VALUTAKOD ENLIGT ISO-STANDARD.                             
000800   03 DASTADAT                          PIC X(8).                         
000900*              GENERELLT STARTDATUM                                       
001000   03 REVALUTA                          PIC S9(5) COMP-3.                 
001100*              OMRƒKNINGSTAL F÷R VALUTA                                   
001200   03 PRKURS                            PIC S9(6)V9(5) COMP-3.            
001300*              VALUTAKURS                                                 
001400   03 DAREGDAT                          PIC X(8).                         
001500*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001600   03 DAUPPDAT                          PIC X(8).                         
001700*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
001800   03 DADELDAT                          PIC X(8).                         
001900*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
002000   03 IDUSER                            PIC X(8).                         
002100*              ANVƒNDARENS SƒKERHETS ID                                   
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 58 OLD LENGTH=                            

000100* GENERATION OF COBOL HOST STRUCTURE FROM T01PAIN-TAB                     
000200  01 T01PAIN.                                                             
000300*              T01PAIN                                                    
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
001400   03 KDVALISO                          PIC X(3).                         
001500*              VALUTAKOD ENLIGT ISO-STANDARD.                             
001600   03 DAREGDAT                          PIC X(8).                         
001700*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
001800   03 DAUPPDAT                          PIC X(8).                         
001900*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
002000   03 IDUSER                            PIC X(8).                         
002100*              ANVƒNDARENS SƒKERHETS ID                                   
002200   03 BETEXT-1                          PIC X(50).                        
002300   03 BETEXT-2                          PIC X(50).                        
002400   03 BETEXT-3                          PIC X(50).                        
002500   03 BETEXT-4                          PIC X(50).                        
002600*                                                                         
002700*** END OF VILMAII-COPY LENGTH= 255 OLD LENGTH=                           

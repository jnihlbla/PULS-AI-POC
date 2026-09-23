000100* GENERATION OF COBOL HOST STRUCTURE FROM TP1ARTK-TAB                     
000200  01 TP1ARTK.                                                             
000300*              TP1ARTK                                                    
000400   03 IDKAMP                            PIC X(7).                         
000500*              SERVICEKAMPANJ                                             
000600   03 IDARTNR                           PIC S9(9) COMP-3.                 
000700*              ARTIKELNUMMER                                              
000800   03 KVREPANT                          PIC S9(3)V9(2) COMP-3.            
000900*              ANTAL PER REPARATION OCH BIL                               
001000   03 KVKAMP-TOTAL                      PIC S9(7) COMP-3.                 
001100*              TOTALANTAL AV ARTIKLAR I KAMPANJ                           
001200   03 KVKAMP-LAUNCH                     PIC S9(7) COMP-3.                 
001300*                                                                         
001400   03 KVKAMP-FIRST                      PIC S9(7) COMP-3.                 
001500*                                                                         
001600   03 RERESPRT                          PIC S9(1)V9(2) COMP-3.            
001700*                                                                         
001800   03 FLKVKAMP-TOTAL                    PIC X(1).                         
001900*              MANUELLT ELLER MASKINELLT BERÄKNAD KVANTITET               
002000*                                                                         
002100*** END OF VILMAII-COPY LENGTH= 30 OLD LENGTH=                            

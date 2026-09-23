000100 01  R31-W407R31A.                                                        
000200*                                 R31                                     
000300*                                 SKAPAS VID UPPLÄGG AV                   
000400*                                 PASSIV ARTIKEL PÅ WDK7 OCH              
000500*                                 GÄLLER ENBART ORSAKSKOD 72              
000600*                                 OCH LDC-SE-1A.                          
000700     03 R31-IDARTNR          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 R31-BEART-SVE        PIC X(25).                                   
001000*                                 SVENSK ARTIKELBENÄMNING                 
001100     03 R31-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
001200*                                 ARTIKELSTANDARDPRIS                     
001300     03 R31-KVLS-LDC         PIC S9(7)           COMP-3.                  
001400*                                 LAGERSALDO                              
001500     03 R31-KVAKS-LDC        PIC S9(7)           COMP-3.                  
001600*                                 ANKOMSTSALDO                            
001700     03 R31-SULAGVDE-LDC     PIC S9(9)V9(2)      COMP-3.                  
001800*                                 LAGERVÄRDE PER ARTIKEL                  
001900     03 R31-KDERS            PIC S9(3)           COMP-3.                  
002000*                                 ERSÄTTNINGSKOD                          
002100     03 R31-KVLS-CDC         PIC S9(7)           COMP-3.                  
002200*                                 LAGERSALDO CENTRALLAGRET                
002300     03 R31-KVPB-TOT         PIC S9(6)V9(1)      COMP-3.                  
002400*                                 TOTALT PERIODBEHOV                      
002500     03 R31-IDDC             PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 R31-IDDISTR          PIC S9(5)           COMP-3.                  
002800*                                 DISTRIKTNUMMER                          
002900     03 R31-IDKUNDNR         PIC S9(7)           COMP-3.                  
003000*                                 KUNDNUMMER                              
003100     03 R31-IDRAPPNR         PIC 9(7).                                    
003200*                                 RAPPORT NUMMER                          
003300*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  

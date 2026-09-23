000100 01  W47968B.                                                             
000200*                                 FAKTURERADE OCH LASTADE                 
000300*                                 RADER PER VECKA                         
000400     03 KDMFUP               PIC X(2).                                    
000500*                                 RAPPORTGRUPP  MA/CN/PF/NA               
000600     03 IDDC-LEV             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 ADCITY               PIC X(20).                                   
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 KDORDKL              PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 TIAAVV               PIC 9(4).                                    
001600*                                 ≈R - VECKA  (≈≈VV)                      
001700     03 TIAARP               PIC 9(4).                                    
001800*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
001900*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
002000     03 KVLEVART2            PIC S9(7)           COMP-3.                  
002100*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
002200*                                 IT                                      
002300     03 KVRADER              PIC S9(5)           COMP-3.                  
002400*                                 ANTAL RADER                             
002500     03 KVKOLLI              PIC S9(5)           COMP-3.                  
002600*                                 ANTAL KOLLI                             
002700     03 KVORDER              PIC S9(7)           COMP-3.                  
002800*                                 ANTAL ORDER                             
002900     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
003000*                                 ORDERVIKT BRUTTO (KG)                   
003100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO                       
003300*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  

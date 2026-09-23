000100 01  4402-WDGX4402.                                                       
000200*                                 4402 HTR TRANSPORTREGISTER              
000300*                                 DISTRIKTSTABELL                         
000400*                                 NYCKEL: KY4402                          
000500*                                         (IDDC,                          
000600*                                          IDDISTR, IDKUNDNR,             
000700*                                          KDFRAKT, KDORDKLX)             
000800*                                 SÖKBEGREPP: IDTPRTNR                    
000900     03 4402-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 4402-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 4402-IDKUNDNR        PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 4402-KDFRAKT         PIC S9(3)           COMP-3.                  
001600*                                 FRAKTSÄTT DC TILL KUND                  
001700     03 4402-KDORDKLX        PIC X.                                       
001800*                                 ORDERKLASS + BLANK                      
001900     03 4402-TEFLNOTE        PIC X(20).                                   
002000*                                 NOTERING FÄRDIGLAGRET                   
002100     03 4402-IDTRPTNR        PIC S9(3)           COMP-3.                  
002200*                                 TRANSPORTIDENTITET                      
002300     03 4402-IDDC-CROSS      PIC X(2).                                    
002400*                                 DC FÖR CROSS DOCKING                    
002500*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  

000100 01  W476SAT-CTX.                                                         
000200*                                 FAKTURA-RAD FÖR SATSER                  
000300*                                                                         
000400     03 IDFAKT               PIC S9(7)           COMP-3.                  
000500*                                 FAKTURANUMMER                           
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 KDORDKL              PIC S9              COMP-3.                  
000900*                                 ORDERKLASS                              
001000     03 KDSOFT               PIC S9              COMP-3.                  
001100*                                 0 NORMAL ORDER                          
001200*                                 1 VCEM SOFTWARE ORDER                   
001300*                                 2 VADIS SOFTWARE ORDER                  
001400*                                 3 OTHER SOFTWARE ORDER                  
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 KVBEART              PIC S9(7)           COMP-3.                  
001800*                                 BESTÄLLT ANTAL STYCKEN                  
001900     03 TIFAKT               PIC S9(7)           COMP-3.                  
002000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002100     03 IDKUNDRF             PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300     03 IDKONTO              PIC S9(11)          COMP-3.                  
002400*                                 KONTO                                   
002500     03 IDANALYS             PIC X(12).                                   
002600*                                 ANALYSNUMMER                            
002700     03 IDKST                PIC X(10).                                   
002800*                                 KOSTNADSSTÄLLE                          
002900*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  

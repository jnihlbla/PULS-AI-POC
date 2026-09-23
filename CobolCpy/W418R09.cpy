000100 01  W418R09-CTX.                                                         
000200*                                 R09 KREDITNOTSUNDERLAG 1                
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 SORTAREA             PIC X(25).                                   
000700     03 W418R09-001-GRP REDEFINES SORTAREA.                               
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 KDCLAGER          PIC S9              COMP-3.                  
001300*                                 CENTRALLAGERKOD                         
001400        05 IDKREUND          PIC S9(5)           COMP-3.                  
001500*                                 KREDNOTSUNDERLAGNUMMER                  
001600        05 KDFRAKT           PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800        05 FILLERX12         PIC X(12).                                   
001900     03 KDTEXREF             PIC S9(3)           COMP-3.                  
002000*                                 REFERENSKOD TEXT                        
002100     03 KDFAKTYP             PIC X.                                       
002200*                                 FAKTURATYP                              
002300     03 IDFAKT               PIC S9(7)           COMP-3.                  
002400*                                 FAKTURANUMMER                           
002500     03 TIFAKT               PIC S9(7)           COMP-3.                  
002600*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002700     03 SUKRENOT             PIC S9(7)V9(2)      COMP-3.                  
002800*                                 KREDITNOTASUMMA                         
002900     03 KDSPEKTO             PIC S9              COMP-3.                  
003000*                                 SPECIALKONTOKOD                         
003100     03 KDKREKTO             PIC S9(3)           COMP-3.                  
003200*                                 KONTOKOD KREDITNOTS UNDERLAG            
003300     03 KDFAKTYP-NEW         PIC X.                                       
003400*                                 FAKTURATYP                              
003500     03 IDFAKT-NEW           PIC S9(7)           COMP-3.                  
003600*                                 FAKTURANUMMER                           
003700     03 TIFAKT-NEW           PIC S9(7)           COMP-3.                  
003800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003900     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
004000*                                 SUMMA FAKTURERAT BELOPP                 
004100     03 IDKONTO-RAD          PIC S9(11)          COMP-3.                  
004200*                                 KONTO                                   
004300     03 IDKST                PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500     03 SUKREPRO             PIC S9(2)V9(1)      COMP-3.                  
004600*                                 PROCENT ATT KREDITERA                   
004700     03 KDFTG                PIC S9(3)           COMP-3.                  
004800*                                 UTGÅTT BYT TILL IDFTG                   
004900     03 KDNIVAA4             PIC S9(3)           COMP-3.                  
005000*                                 NIVÅNUMMER-4                            
005100*** END OF VILMAII-COPY LENGTH= 84 BYTES                                  

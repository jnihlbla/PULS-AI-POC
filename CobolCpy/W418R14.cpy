000100 01  W418R14.                                                             
000200*                                 R14 LEVERANSANMÄRKNING 1                
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 SORTAREA             PIC X(25).                                   
000600     03 FILLER REDEFINES SORTAREA.                                        
000700*                                                                         
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 KDCLAGER          PIC S9              COMP-3.                  
001300*                                 CENTRALLAGERKOD                         
001400        05 IDLEVANM          PIC X(7).                                    
001500*                                 LEVANMNUMMER       IDLEVANM-002         
001600        05 KDFRAKT           PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800        05 IDORDNR           PIC S9(5)           COMP-3.                  
001900*                                 ORDERNUMMER UTGÅR PD90                  
002000        05 IDARTNR           PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 IDRADNR              PIC S9(5)           COMP-3.                  
002300*                                 RADNUMMER                               
002400     03 TIM-LEVANM           PIC S9(7)           COMP-3.                  
002500*                                 DATUM LEVERANSANMÄRKN. (ÅÅMMDD)         
002600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002700*                                 KOLLINUMMER                             
002800     03 REKSIFFR             PIC S9              COMP-3.                  
002900*                                 KONTROLLSIFFRA                          
003000     03 KVLEVANM             PIC S9(7)           COMP-3.                  
003100*                                 LEVERANSANMÄRKNINGSANTAL                
003200     03 KDANMORS             PIC S9(3)           COMP-3.                  
003300*                                 ORSAK TILL LEVERAN KDANMORS-002         
003400     03 KDEMBLEV             PIC S9              COMP-3.                  
003500*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
003600     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
003700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003800     03 KDFAKTYP             PIC X.                                       
003900*                                 FAKTURATYP                              
004000     03 IDFAKT               PIC S9(7)           COMP-3.                  
004100*                                 FAKTURANUMMER                           
004200     03 TIFAKT               PIC S9(7)           COMP-3.                  
004300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004400     03 FLDIRLEV             PIC S9              COMP-3.                  
004500*                                 DIREKTLEVERANS?    FLDIRLEV-002         
004600*                                 (1 = JA)                                
004700     03 KDSPEKTO             PIC S9              COMP-3.                  
004800*                                 SPECIALKONTOKOD                         
004900     03 KDFTG                PIC S9(3)           COMP-3.                  
005000*                                 UTGÅTT BYT TILL IDFTG                   
005100     03 IDKONTO-RAD          PIC S9(11)          COMP-3.                  
005200*                                 KONTO                                   
005300     03 FLSTRET              PIC S9              COMP-3.                  
005400*                                 FLAGGA STÖRRE RETUR                     
005500     03 FLSKROT              PIC S9              COMP-3.                  
005600*                                 SKROTNING ? (1=JA)  FLSKROT-002         
005700     03 KDNIVAA4             PIC S9(3)           COMP-3.                  
005800*                                 NIVÅNUMMER-4                            
005900     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
006000*                                 PRIS I LOKAL VALUTA                     
006100     03 KDVALISO             PIC X(3).                                    
006200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006300     03 FILLER               PIC X(5).                                    
006400*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  

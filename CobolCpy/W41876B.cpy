000100 01  W41876B.                                                             
000200*                                 RADPOST  RETURTILLSTÅND                 
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 IDORDNR              PIC S9(5)           COMP-3.                  
001900*                                 ORDERNUMMER UTGÅR PD90                  
002000     03 IDRADNR              PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER                               
002200     03 REKSIFFR             PIC S9              COMP-3.                  
002300*                                 KONTROLLSIFFRA                          
002400     03 BEART                PIC X(15).                                   
002500*                                 ARTIKELBENÄMNING      BEART-002         
002600     03 KVLEVANM             PIC S9(7)           COMP-3.                  
002700*                                 LEVERANSANMÄRKNINGSANTAL                
002800     03 KDANMORS             PIC X(2).                                    
002900*                                 ORSAK TILL LEVERANSANMÄRKNING           
003000     03 KDERS                PIC S9(3)           COMP-3.                  
003100*                                 ERSÄTTNINGSKOD                          
003200     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
003300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003400     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
003500*                                 PRIS I LOKAL VALUTA                     
003600*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  

000100 01  W41803.                                                              
000200*                                 LEVERANSANMÄRKNING IN MED KDCLA         
000300*                                 GER                                     
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 W41803-001-GRP.                                                   
000700*                                                                         
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400        05 IDARTNR           PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600        05 IDRADNR           PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER                               
001800     03 FLDIRLEV             PIC X.                                       
001900*                                 DIREKTLEVERANS ?                        
002000     03 IDDC                 PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 IDFAKT               PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANUMMER                           
002400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002500*                                 KOLLINUMMER                             
002600     03 IDORDNR              PIC S9(5)           COMP-3.                  
002700*                                 ORDERNUMMER UTGÅR PD90                  
002800     03 KDANMORS             PIC X(2).                                    
002900*                                 ORSAK TILL LEVERANSANMÄRKNING           
003000     03 KDEMBLEV             PIC S9              COMP-3.                  
003100*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
003200     03 KDFAKTYP             PIC X.                                       
003300*                                 FAKTURATYP                              
003400     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003500*                                 FRAKTSÄTT DC TILL KUND                  
003600     03 KVLEVANM             PIC S9(7)           COMP-3.                  
003700*                                 LEVERANSANMÄRKNINGSANTAL                
003800     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
003900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004000     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
004100*                                 PRIS I LOKAL VALUTA                     
004200     03 KDVALISO             PIC X(3).                                    
004300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004400     03 TIFAKT               PIC S9(7)           COMP-3.                  
004500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004600     03 TILEVANM             PIC S9(7)           COMP-3.                  
004700*                                 DATUM LEVERANSANMÄRKNING                
004800     03 TEANMNOT-REG         OCCURS 3 TIMES                               
004900                             PIC X(70).                                   
005000*                                 FRI TEXT FRÅN REGISTRERINGEN            
005100*** END OF VILMAII-COPY LENGTH= 279 BYTES                                 

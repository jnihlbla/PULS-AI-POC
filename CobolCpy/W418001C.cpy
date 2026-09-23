000100 01  W418001C.                                                            
000200*                                 LEVERANSANMÄRKNING IN VIA VCOM          
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 W418001C-001-GRP.                                                 
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
002000     03 IDFAKT               PIC S9(7)           COMP-3.                  
002100*                                 FAKTURANUMMER                           
002200     03 IDFAKT-LOC           PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANR LOKALT                        
002400     03 IDFTG                PIC 9(2).                                    
002500*                                 FÖRETAGSID EKONOM REDOVISNING           
002600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002700*                                 KOLLINUMMER                             
002800     03 IDORDNR7             PIC 9(7).                                    
002900*                                 ORDERNUMMER                             
003000     03 KDANMORS             PIC X(2).                                    
003100*                                 ORSAK TILL LEVERANSANMÄRKNING           
003200     03 IDDC                 PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 KDEMBLEV             PIC S9              COMP-3.                  
003500*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
003600     03 KDFAKTYP             PIC X.                                       
003700*                                 FAKTURATYP                              
003800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003900*                                 FRAKTSÄTT DC TILL KUND                  
004000     03 KVLEVANM             PIC S9(7)           COMP-3.                  
004100*                                 LEVERANSANMÄRKNINGSANTAL                
004200     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004400     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
004500*                                 PRIS I LOKAL VALUTA                     
004600     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
004700*                                 FRAKTKOSTNAD                            
004800     03 TIFAKT               PIC S9(7)           COMP-3.                  
004900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005000     03 TIFAKT-LOC           PIC S9(7)           COMP-3.                  
005100*                                 FAKTURADATUM LOKALT                     
005200     03 TILEVANM             PIC S9(7)           COMP-3.                  
005300*                                 DATUM LEVERANSANMÄRKNING                
005400     03 TEANMNOT-REG         OCCURS 3 TIMES                               
005500                             PIC X(70).                                   
005600*                                 FRI TEXT FRÅN REGISTRERINGEN            
005700     03 KDVAT                PIC X(2).                                    
005800*                                 MOMSKOD                                 
005900     03 KDVALISO             PIC X(3).                                    
006000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006100     03 BEART-VIPS           PIC X(25).                                   
006200*                                 VIPS ARTIKELBENÄMNING                   
006300*                                 PÅ DEALERNS SPRÅK                       
006400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
006500*                                 ARTIKELSTANDARDPRIS                     
006600     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
006700*                                 ARTIKELNS SJÄLVKOSTNAD                  
006800*** END OF VILMAII-COPY LENGTH= 335 BYTES                                 

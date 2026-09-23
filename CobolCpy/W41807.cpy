000100 01  W41807-CTX.                                                          
000200*                                 LEVERANSANMÄRKNING                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 W41807-001-GRP.                                                   
000600*                                                                         
000700        05 IDDISTR           PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100        05 IDRAPPNR          PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300        05 IDARTNR           PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500        05 IDRADNR           PIC S9(5)           COMP-3.                  
001600*                                 RADNUMMER                               
001700     03 FLAUTKRE             PIC X.                                       
001800*                                 AUTOMATISK KREDITERING                  
001900     03 FLDIRLEV             PIC X.                                       
002000*                                 DIREKTLEVERANS ?                        
002100     03 IDFAKT               PIC S9(7)           COMP-3.                  
002200*                                 FAKTURANUMMER                           
002300     03 IDFAKT-LOC           PIC S9(7)           COMP-3.                  
002400*                                 FAKTURANR LOKALT                        
002500     03 IDFTG                PIC 9(2).                                    
002600*                                 FÖRETAGSID EKONOM REDOVISNING           
002700     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002800*                                 KOLLINUMMER                             
002900     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
003000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
003100*                                 (0VVDLLLLK)                             
003200     03 IDORDNR              PIC S9(5)           COMP-3.                  
003300*                                 ORDERNUMMER UTGÅR PD90                  
003400     03 KDANMORS             PIC X(2).                                    
003500*                                 ORSAK TILL LEVERANSANMÄRKNING           
003600     03 IDDC                 PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800     03 IDDC-RET             PIC X(2).                                    
003900*                                 MOTTAGANDE LAGER FÖR RETURER            
004000     03 KDEMBLEV             PIC S9              COMP-3.                  
004100*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
004200     03 KDFAKTYP             PIC X.                                       
004300*                                 FAKTURATYP                              
004400     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004500*                                 FRAKTSÄTT DC TILL KUND                  
004600     03 KVLEVANM             PIC S9(7)           COMP-3.                  
004700*                                 LEVERANSANMÄRKNINGSANTAL                
004800     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
004900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005000     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
005100*                                 PRIS I LOKAL VALUTA                     
005200     03 PRARTBTO-LOCINV      PIC S9(7)V9(2)      COMP-3.                  
005300*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
005400     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
005500*                                 FRAKTKOSTNAD                            
005600     03 TIFAKT               PIC S9(7)           COMP-3.                  
005700*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005800     03 TIFAKT-LOC           PIC S9(7)           COMP-3.                  
005900*                                 FAKTURADATUM LOKALT                     
006000     03 TILEVANM             PIC S9(7)           COMP-3.                  
006100*                                 DATUM LEVERANSANMÄRKNING                
006200     03 TEANMNOT-REG         OCCURS 3 TIMES                               
006300                             PIC X(70).                                   
006400*                                 FRI TEXT FRÅN REGISTRERINGEN            
006500     03 KDVAT                PIC X(2).                                    
006600*                                 MOMSKOD                                 
006700     03 KDVALISO             PIC X(3).                                    
006800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006900     03 BEART-VIPS           PIC X(25).                                   
007000*                                 VIPS ARTIKELBENÄMNING                   
007100*                                 PÅ DEALERNS SPRÅK                       
007200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007300*                                 ARTIKELSTANDARDPRIS                     
007400     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
007500*                                 ARTIKELNS SJÄLVKOSTNAD                  
007600*** END OF VILMAII-COPY LENGTH= 344 BYTES                                 

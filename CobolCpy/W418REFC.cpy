000100 01  W418REFC.                                                            
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 REFILL REREFILL                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 RADKEY.                                                           
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
001800     03 FLAUTKRE             PIC X.                                       
001900*                                 AUTOMATISK KREDITERING                  
002000     03 FLDIRLEV             PIC X.                                       
002100*                                 DIREKTLEVERANS ?                        
002200     03 IDFAKT               PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANUMMER                           
002400     03 IDFTG                PIC 9(2).                                    
002500*                                 FÖRETAGSID EKONOM REDOVISNING           
002600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002700*                                 KOLLINUMMER                             
002800     03 IDKUNDRF-GRP.                                                     
002900*                                 KUNDENS REFERENS (ORDERID)              
003000        05 IDKUNDRF          PIC X(10).                                   
003100*                                 KUNDENS REFERENS (ORDERID)              
003200        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
003300           07 IDORDNR5       PIC 9(5).                                    
003400*                                 ORDERNUMMER                             
003500           07 FILLER         PIC X(5).                                    
003600        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
003700           07 IDORDNR7       PIC 9(7).                                    
003800*                                 ORDERNUMMER                             
003900           07 FILLER         PIC X(3).                                    
004000     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
004100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004200*                                 (0VVDLLLLK)                             
004300     03 KDANMORS             PIC X(2).                                    
004400*                                 ORSAK TILL LEVERANSANMÄRKNING           
004500     03 IDDC-SEND            PIC X(2).                                    
004600*                                 SÄNDANDE LAGER                          
004700     03 KDEMBLEV             PIC S9              COMP-3.                  
004800*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
004900     03 KDFAKTYP             PIC X.                                       
005000*                                 FAKTURATYP                              
005100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
005200*                                 FRAKTSÄTT DC TILL KUND                  
005300     03 KVLEVANM             PIC S9(7)           COMP-3.                  
005400*                                 LEVERANSANMÄRKNINGSANTAL                
005500     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
005600*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005700     03 TIFAKT               PIC S9(7)           COMP-3.                  
005800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005900     03 TILEVANM             PIC S9(7)           COMP-3.                  
006000*                                 DATUM LEVERANSANMÄRKNING                
006100     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
006200*                                 PRIS I LOKAL VALUTA                     
006300     03 KDVALISO             PIC X(3).                                    
006400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006500     03 IDDC-REC             PIC X(2).                                    
006600*                                 MOTTAGANDE LAGER                        
006700     03 IDDC-LEV             PIC X(2).                                    
006800*                                 LEVERERANDE DC I EXPORTFLÖDET           
006900*** END OF VILMAII-COPY LENGTH= 88 BYTES                                  

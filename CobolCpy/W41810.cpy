000100 01  W41810.                                                              
000200*                                 LEVERANSANMÄRKNING                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 NYCKLAR.                                                          
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
001900     03 FLANLYSF             PIC X.                                       
002000*                                 FEL ANALYSNUMMER?                       
002100     03 FLDIRLEV             PIC X.                                       
002200*                                 DIREKTLEVERANS ?                        
002300     03 IDANALYS             PIC X(12).                                   
002400*                                 ANALYSNUMMER                            
002500     03 IDKONTO              PIC S9(11)          COMP-3.                  
002600*                                 KONTO                                   
002700     03 IDKST                PIC X(10).                                   
002800*                                 KOSTNADSSTÄLLE                          
002900     03 IDDC                 PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 IDDC-RET             PIC X(2).                                    
003200*                                 MOTTAGANDE LAGER FÖR RETURER            
003300     03 IDFAKT               PIC S9(7)           COMP-3.                  
003400*                                 FAKTURANUMMER                           
003500     03 IDFAKT-LOC           PIC S9(7)           COMP-3.                  
003600*                                 FAKTURANR LOKALT                        
003700     03 IDFTG                PIC 9(2).                                    
003800*                                 FÖRETAGSID EKONOM REDOVISNING           
003900     03 IDKOLLI              PIC S9(5)           COMP-3.                  
004000*                                 KOLLINUMMER                             
004100     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
004200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004300*                                 (0VVDLLLLK)                             
004400     03 IDORDNR              PIC S9(5)           COMP-3.                  
004500*                                 ORDERNUMMER UTGÅR PD90                  
004600     03 IDUSER-PACK          PIC X(8).                                    
004700*                                 ANSVARIGT USERID PACKARE                
004800     03 KDANMORS             PIC X(2).                                    
004900*                                 ORSAK TILL LEVERANSANMÄRKNING           
005000     03 KDEMBLEV             PIC S9              COMP-3.                  
005100*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
005200     03 KDFAKTYP             PIC X.                                       
005300*                                 FAKTURATYP                              
005400     03 KDFRAKT              PIC S9(3)           COMP-3.                  
005500*                                 FRAKTSÄTT DC TILL KUND                  
005600     03 KDKREBEH             PIC X(3).                                    
005700*                                 BEHANDLINGSSTATUS                       
005800     03 KDORDKL              PIC S9              COMP-3.                  
005900*                                 ORDERKLASS                              
006000     03 KVLEVANM             PIC S9(7)           COMP-3.                  
006100*                                 LEVERANSANMÄRKNINGSANTAL                
006200     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
006300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
006400     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
006500*                                 PRIS I LOKAL VALUTA                     
006600     03 PRARTBTO-LOCINV      PIC S9(7)V9(2)      COMP-3.                  
006700*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
006800     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
006900*                                 FRAKTKOSTNAD                            
007000     03 TIFAKT               PIC S9(7)           COMP-3.                  
007100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007200     03 TIFAKT-LOC           PIC S9(7)           COMP-3.                  
007300*                                 FAKTURADATUM LOKALT                     
007400     03 TILEVANM             PIC S9(7)           COMP-3.                  
007500*                                 DATUM LEVERANSANMÄRKNING                
007600     03 KDVAT                PIC X(2).                                    
007700*                                 MOMSKOD                                 
007800     03 KDVALISO             PIC X(3).                                    
007900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008000     03 BEART-VIPS           PIC X(25).                                   
008100*                                 VIPS ARTIKELBENÄMNING                   
008200*                                 PÅ DEALERNS SPRÅK                       
008300     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
008400*                                 ARTIKELSTANDARDPRIS                     
008500     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
008600*                                 ARTIKELNS SJÄLVKOSTNAD                  
008700     03 FLPRQUES             PIC X.                                       
008800*                                 PRISSÄTTNINGSFLAGGA                     
008900     03 TEANMNOT-REG         OCCURS 3 TIMES                               
009000                             PIC X(70).                                   
009100*                                 FRI TEXT FRÅN REGISTRERINGEN            
009200*** END OF VILMAII-COPY LENGTH= 386 BYTES                                 

000100 01  W418KTL1.                                                            
000200*                                 LÄNKCOPYTEXT TILL KONTROLLPROGR         
000300*                                 AM W418KTL1                             
000400     03 FLANLYSF             PIC X.                                       
000500*                                 FEL ANALYSNUMMER?                       
000600     03 FLAUTKRE             PIC X.                                       
000700*                                 AUTOMATISK KREDITERING                  
000800     03 FLDIRLEV             PIC X.                                       
000900*                                 DIREKTLEVERANS ?                        
001000     03 IDANALYS             PIC X(12).                                   
001100*                                 ANALYSNUMMER                            
001200     03 IDKONTO              PIC S9(11)          COMP-3.                  
001300*                                 KONTO                                   
001400     03 IDKST                PIC X(10).                                   
001500*                                 KOSTNADSSTÄLLE                          
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 IDDC                 PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 IDDISTR              PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200     03 IDFAKT               PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANUMMER                           
002400     03 IDFELKOD             OCCURS 50 TIMES                              
002500                             PIC X(3).                                    
002600*                                 FELKOD                                  
002700     03 IDFTG                PIC 9(2).                                    
002800      88 FTG-US              VALUE 53.                                    
002900      88 FTG-CA              VALUE 54.                                    
003000      88 FTG-PV              VALUE 57.                                    
003100      88 FTG-CN              VALUE 60.                                    
003200      88 FTG-IN              VALUE 61.                                    
003300      88 FTG-TH              VALUE 63.                                    
003400      88 FTG-TW              VALUE 64.                                    
003500      88 FTG-KR              VALUE 65.                                    
003600      88 FTG-MY              VALUE 66.                                    
003700      88 FTG-RU              VALUE 81.                                    
003800*                                 FÖRETAGSID EKONOM REDOVISNING           
003900     03 IDKOLLI              PIC S9(5)           COMP-3.                  
004000*                                 KOLLINUMMER                             
004100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
004200*                                 KUNDNUMMER                              
004300     03 IDORDNR5             PIC 9(5).                                    
004400*                                 ORDERNUMMER                             
004500     03 IDPTYP               PIC X(3).                                    
004600*                                 POSTTYP                                 
004700     03 IDRAPPNR             PIC 9(7).                                    
004800*                                 RAPPORT NUMMER                          
004900     03 IDUSER-PACK          PIC X(8).                                    
005000*                                 ANSVARIGT USERID PACKARE                
005100     03 KDANMORS             PIC X(2).                                    
005200*                                 ORSAK TILL LEVERANSANMÄRKNING           
005300     03 KDEMBLEV             PIC S9              COMP-3.                  
005400*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
005500     03 KDFAKTYP             PIC X.                                       
005600      88 KDFAKTYP-HANDELS    VALUE 'R'.                                   
005700      88 KDFAKTYP-KONSIGN    VALUE 'K'.                                   
005800      88 KDFAKTYP-INTERN     VALUE 'N'.                                   
005900      88 KDFAKTYP-GRATIS     VALUE 'G'.                                   
006000      88 KDFAKTYP-TULL       VALUE 'F'.                                   
006100      88 KDFAKTYP-PROFORMA   VALUE 'P'.                                   
006200*                                 FAKTURATYP                              
006300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
006400*                                 FRAKTSÄTT DC TILL KUND                  
006500     03 KDKREBEH             PIC X(3).                                    
006600*                                 BEHANDLINGSSTATUS                       
006700     03 KDORDKL              PIC S9              COMP-3.                  
006800      88 KDORDKL-VOR         VALUE +0.                                    
006900      88 KDORDKL-DAG         VALUE +1.                                    
007000      88 KDORDKL-2           VALUE +2.                                    
007100      88 KDORDKL-SNABB       VALUE +2.                                    
007200      88 KDORDKL-SPECIAL     VALUE +3.                                    
007300      88 KDORDKL-KVANT       VALUE +4.                                    
007400      88 KDORDKL-SATS        VALUE +5.                                    
007500*                                 ORDERKLASS                              
007600     03 KDSVAR               PIC X.                                       
007700      88 KDSVAR-OK           VALUE ' '.                                   
007800      88 KDSVAR-FEL          VALUE 'F'.                                   
007900*                                                       KDSVAR-88         
008000*                                 SVARSKOD FRÅN SUBPROGRAM                
008100     03 KVLEVANM             PIC S9(7)           COMP-3.                  
008200*                                 LEVERANSANMÄRKNINGSANTAL                
008300     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
008400*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
008500     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
008600*                                 PRIS I LOKAL VALUTA                     
008700     03 TIFAKT               PIC S9(7)           COMP-3.                  
008800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
008900     03 TILEVANM             PIC S9(7)           COMP-3.                  
009000*                                 DATUM LEVERANSANMÄRKNING                
009100     03 KDVAT                PIC X(2).                                    
009200*                                 MOMSKOD                                 
009300     03 KDVALISO             PIC X(3).                                    
009400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009500     03 BEART-VIPS           PIC X(25).                                   
009600*                                 VIPS ARTIKELBENÄMNING                   
009700*                                 PÅ DEALERNS SPRÅK                       
009800     03 FLPRQUES             PIC X.                                       
009900*                                 PRISSÄTTNINGSFLAGGA                     
010000     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELSTANDARDPRIS                     
010200     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
010300*                                 ARTIKELNS SJÄLVKOSTNAD                  
010400     03 TEMFSINF             PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010600*** END OF VILMAII-COPY LENGTH= 356 BYTES                                 

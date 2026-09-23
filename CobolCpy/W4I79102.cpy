000100 01  W4I79102.                                                            
000200*                                 SPARAREA FÖR W4079100                   
000300     03 IDSYSTEM             PIC X(4).                                    
000400*                                 VOLVO VCCS SYSTEMNUMMER                 
000500     03 IDDISTR              PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 IDRAPPNR             PIC X(7).                                    
001000*                                 RAPPORT NUMMER                          
001100     03 KDLEVANM             PIC X.                                       
001200*                                 STATUS LEVERANSANMÄRKNING               
001300     03 LEVANM-KLAR          PIC X.                                       
001400*                                 LEVERANSANMÄRKNING KLAR (J/N)           
001500     03 PRFOERS              PIC X(10).                                   
001600*                                 FÖRSÄKRINGSPREMIE                       
001700     03 PRFRAKT              PIC X(10).                                   
001800*                                 FRAKTKOSTNAD                            
001900     03 RELANDCO             PIC X(6).                                    
002000*                                 LANDING COST PROCENT                    
002100     03 PRLEGKST             PIC X(10).                                   
002200*                                 LEGALISERINSKOSTNAD                     
002300     03 REEMBHNT             PIC X(4).                                    
002400*                                 EMB OCH HANTERINGSKOST (%)              
002500     03 TILEVANM             PIC X(6).                                    
002600*                                 DATUM LEVERANSANMÄRKNING                
002700     03 KVRADER              PIC 9(3).                                    
002800*                                 ANTAL RADER                             
002900     03 INFO-RAD             OCCURS 1 TO 999 TIMES                        
003000                             DEPENDING ON KVRADER.                        
003100*                                 RADINFORMATION                          
003200        05 FLDIRLEV          PIC X.                                       
003300*                                 DIREKTLEVERANS ?                        
003400        05 IDANALYS          PIC X(12).                                   
003500*                                 ANALYSNUMMER                            
003600        05 IDKONTO           PIC X(10).                                   
003700*                                 KONTO                                   
003800        05 IDKST             PIC X(10).                                   
003900*                                 KOSTNADSSTÄLLE                          
004000        05 IDARTNR           PIC X(9).                                    
004100*                                 ARTIKELNUMMER                           
004200        05 IDDC              PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400        05 IDFAKT            PIC X(7).                                    
004500*                                 FAKTURANUMMER                           
004600        05 IDFTG             PIC X(2).                                    
004700*                                 FÖRETAGSID EKONOM REDOVISNING           
004800        05 IDKOLLI           PIC X(5).                                    
004900*                                 KOLLINUMMER                             
005000        05 IDKUNDRF          PIC X(10).                                   
005100*                                 KUNDENS REFERENS (ORDERID)              
005200        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
005300           07 IDORDNR5       PIC 9(5).                                    
005400*                                 ORDERNUMMER                             
005500           07 FILLER         PIC X(5).                                    
005600        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
005700           07 IDORDNR7       PIC 9(7).                                    
005800*                                 ORDERNUMMER                             
005900           07 FILLER         PIC X(3).                                    
006000        05 IDRADNR           PIC X(4).                                    
006100*                                 RADNUMMER                               
006200        05 KDANMORS          PIC X(2).                                    
006300*                                 ORSAK TILL LEVERANSANMÄRKNING           
006400        05 KDEMBLEV          PIC X.                                       
006500*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
006600        05 KDFAKTYP          PIC X.                                       
006700*                                 FAKTURATYP                              
006800        05 KDFRAKT           PIC X(2).                                    
006900*                                 FRAKTSÄTT DC TILL KUND                  
007000        05 KDKREBEH          PIC X(3).                                    
007100*                                 BEHANDLINGSSTATUS                       
007200        05 KVLEVANM          PIC X(6).                                    
007300*                                 LEVERANSANMÄRKNINGSANTAL                
007400        05 PRARTBTO          PIC X(10).                                   
007500*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
007600        05 PRARTBTO-LOC      PIC X(10).                                   
007700*                                 PRIS I LOKAL VALUTA                     
007800        05 TIFAKT            PIC X(6).                                    
007900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
008000        05 TILEVANM-RAD      PIC X(6).                                    
008100*                                 DATUM LEVERANSANMÄRKNING                
008200        05 IDUSER-PACK       PIC X(8).                                    
008300*                                 ANSVARIGT USERID PACKARE                
008400        05 KDORDKL           PIC X.                                       
008500*                                 ORDERKLASS                              
008600        05 KDVAT             PIC X(2).                                    
008700*                                 MOMSKOD                                 
008800        05 KDVALISO          PIC X(3).                                    
008900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009000        05 BEART-VIPS        PIC X(25).                                   
009100*                                 VIPS ARTIKELBENÄMNING                   
009200*                                 PÅ DEALERNS SPRÅK                       
009300        05 FLPRQUES          PIC X.                                       
009400*                                 PRISSÄTTNINGSFLAGGA                     
009500        05 PRARTSTD          PIC X(10).                                   
009600*                                 ARTIKELSTANDARDPRIS                     
009700        05 PRARTSJK          PIC X(10).                                   
009800*                                 ARTIKELNS SJÄLVKOSTNAD                  
009900*** END OF VILMAII-COPY LENGTH= 178893 BYTES                              

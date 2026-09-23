000100 01  MID-W4I79101.                                                        
000200*                                 MID-COPYTEXT FÖR W4079100               
000300     03 MID-IDSYSTEM         PIC X(4).                                    
000400*                                 VOLVO VCCS SYSTEMNUMMER                 
000500     03 MID-IDDISTR          PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR         PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDRAPPNR         PIC X(7).                                    
001000*                                 RAPPORT NUMMER                          
001100     03 MID-KDLEVANM         PIC X.                                       
001200*                                 STATUS LEVERANSANMÄRKNING               
001300     03 MID-KVRADER          PIC 9(5).                                    
001400*                                 ANTAL RADER                             
001500     03 MID-LEVANM-KLAR      PIC X.                                       
001600*                                 LEVERANSANMÄRKNING KLAR (J/N)           
001700     03 MID-PRFOERS          PIC X(10).                                   
001800*                                 FÖRSÄKRINGSPREMIE                       
001900     03 MID-PRFRAKT          PIC X(10).                                   
002000*                                 FRAKTKOSTNAD                            
002100     03 MID-RELANDCO         PIC X(6).                                    
002200*                                 LANDING COST PROCENT                    
002300     03 MID-PRLEGKST         PIC X(10).                                   
002400*                                 LEGALISERINSKOSTNAD                     
002500     03 MID-REEMBHNT         PIC X(4).                                    
002600*                                 EMB OCH HANTERINGSKOST (%)              
002700     03 MID-TILEVANM         PIC X(6).                                    
002800*                                 DATUM LEVERANSANMÄRKNING                
002900     03 MID-KDVALISO         PIC X(3).                                    
003000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003100     03 MID-INFO-RAD         OCCURS 4 TIMES.                              
003200*                                 RADINFORMATION                          
003300        05 MID-FLDIRLEV      PIC X.                                       
003400*                                 DIREKTLEVERANS ?                        
003500        05 MID-FLANLYSF      PIC X.                                       
003600*                                 FEL ANALYSNUMMER?                       
003700        05 MID-FLAUTKRE      PIC X.                                       
003800*                                 AUTOMATISK KREDITERING                  
003900        05 MID-IDANALYS      PIC X(12).                                   
004000*                                 ANALYSNUMMER                            
004100        05 MID-IDKONTO       PIC X(10).                                   
004200*                                 KONTO                                   
004300        05 MID-IDKST         PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500        05 MID-IDARTNR       PIC X(9).                                    
004600*                                 ARTIKELNUMMER                           
004700        05 MID-IDDC          PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900        05 MID-IDDC-RET      PIC X(2).                                    
005000*                                 MOTTAGANDE LAGER FÖR RETURER            
005100        05 MID-IDFAKT        PIC X(7).                                    
005200*                                 FAKTURANUMMER                           
005300        05 MID-IDFAKT-LOC    PIC X(7).                                    
005400*                                 FAKTURANR LOKALT                        
005500        05 MID-IDFTG         PIC X(2).                                    
005600*                                 FÖRETAGSID EKONOM REDOVISNING           
005700        05 MID-IDKOLLI       PIC X(5).                                    
005800*                                 KOLLINUMMER                             
005900        05 MID-IDKUNDRF      PIC X(10).                                   
006000*                                 KUNDENS REFERENS (ORDERID)              
006100        05 MID-IDORDNR5-FILLER REDEFINES MID-IDKUNDRF.                    
006200           07 MID-IDORDNR5   PIC 9(5).                                    
006300*                                 ORDERNUMMER                             
006400           07 FILLER         PIC X(5).                                    
006500        05 MID-IDORDNR7-FILLER REDEFINES MID-IDKUNDRF.                    
006600           07 MID-IDORDNR7   PIC 9(7).                                    
006700*                                 ORDERNUMMER                             
006800           07 FILLER         PIC X(3).                                    
006900        05 MID-IDLOPNRM      PIC X(8).                                    
007000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007100*                                 (0VVDLLLLK)                             
007200        05 MID-IDRADNR       PIC X(4).                                    
007300*                                 RADNUMMER                               
007400        05 MID-KDANMORS      PIC X(2).                                    
007500*                                 ORSAK TILL LEVERANSANMÄRKNING           
007600        05 MID-KDEMBLEV      PIC X.                                       
007700*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
007800        05 MID-KDFAKTYP      PIC X.                                       
007900*                                 FAKTURATYP                              
008000        05 MID-KDFRAKT       PIC X(2).                                    
008100*                                 FRAKTSÄTT DC TILL KUND                  
008200        05 MID-KDKREBEH      PIC X(3).                                    
008300*                                 BEHANDLINGSSTATUS                       
008400        05 MID-KVLEVANM      PIC X(6).                                    
008500*                                 LEVERANSANMÄRKNINGSANTAL                
008600        05 MID-PRARTBTO      PIC X(10).                                   
008700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
008800        05 MID-PRARTBTO-LOC  PIC X(10).                                   
008900*                                 PRIS I LOKAL VALUTA                     
009000        05 MID-PRARTBTO-LOCINV                                            
009100                             PIC X(10).                                   
009200*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
009300        05 MID-PRFRAKT-RAD   PIC X(10).                                   
009400*                                 FRAKTKOSTNAD                            
009500        05 MID-TIFAKT        PIC X(6).                                    
009600*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
009700        05 MID-TIFAKT-LOC    PIC X(6).                                    
009800*                                 FAKTURADATUM LOKALT                     
009900        05 MID-TILEVANM-RAD  PIC X(6).                                    
010000*                                 DATUM LEVERANSANMÄRKNING                
010100        05 MID-IDUSER-PACK   PIC X(8).                                    
010200*                                 ANSVARIGT USERID PACKARE                
010300        05 MID-KDORDKL       PIC X.                                       
010400*                                 ORDERKLASS                              
010500        05 MID-FLPRQUES      PIC X.                                       
010600*                                 PRISSÄTTNINGSFLAGGA                     
010700        05 MID-KDVAT         PIC X(2).                                    
010800*                                 MOMSKOD                                 
010900        05 MID-BEART-VIPS    PIC X(25).                                   
011000*                                 VIPS ARTIKELBENÄMNING                   
011100*                                 PÅ DEALERNS SPRÅK                       
011200        05 MID-PRARTSTD      PIC X(10).                                   
011300*                                 ARTIKELSTANDARDPRIS                     
011400        05 MID-PRARTSJK      PIC X(10).                                   
011500*                                 ARTIKELNS SJÄLVKOSTNAD                  
011600*** END OF VILMAII-COPY LENGTH= 961 BYTES                                 

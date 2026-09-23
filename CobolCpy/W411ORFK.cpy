000100 01  ORFK-W411ORFK.                                                       
000200*                                 LÄNKAREA TILL W411ORFK -                
000300*                                 FORMELL KONTROLL AV ORDERRADER          
000400     03 ORFK-IDSYSTEM        PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 ORFK-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 ORFK-IDDISTR         PIC 9(5).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 ORFK-IDKUNDNR        PIC 9(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 ORFK-IDUSER          PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400     03 ORFK-KDFAKTYP        PIC X.                                       
001500*                                 FAKTURATYP                              
001600     03 ORFK-KDORDKL         PIC X.                                       
001700*                                 ORDERKLASS                              
001800     03 ORFK-KDTPOTYP        PIC X.                                       
001900*                                 TYP AV TIDPLANERAD ORDER                
002000     03 ORFK-FLFORBI         PIC X.                                       
002100*                                 FÖRBIORDERFLAGGA                        
002200     03 ORFK-FLORDSPE        PIC X.                                       
002300*                                 SPECIALORDERFLAGGA                      
002400     03 ORFK-FLOVRLEV        PIC X.                                       
002500*                                 ÖVERLEVERANS                            
002600     03 ORFK-FLEMBORD        PIC X.                                       
002700*                                 EMBALLAGEORDER ?                        
002800     03 ORFK-IDFTG           PIC 9(2).                                    
002900*                                 FÖRETAGSID EKONOM REDOVISNING           
003000     03 ORFK-W411ORFK-001    OCCURS 100 TIMES.                            
003100*                                 INFO FRÅN SKÄRMEN                       
003200        05 ORFK-FLINVEST     PIC X.                                       
003300*                                 BYTES INVENTERINGSFLAGGA                
003400        05 ORFK-FLINVEST-OK  PIC X.                                       
003500        05 ORFK-FLRESTN      PIC X.                                       
003600*                                 RESTNOTERING ?                          
003700        05 ORFK-FLRESTN-OK   PIC X.                                       
003800        05 ORFK-FLSLATT      PIC X.                                       
003900*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
004000*                                 LL BERÄKNAS ELLER EJ                    
004100*                                 OM FLRESTN = J OCH FLSLATT = J,         
004200*                                  DÅ BERÄKNAS KVSLATT                    
004300        05 ORFK-FLSLATT-OK   PIC X.                                       
004400        05 ORFK-IDARTNR-IN   PIC X(11).                                   
004500*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
004600        05 ORFK-IDARTNR-OK   PIC X.                                       
004700        05 ORFK-IDKONTO      PIC X(10).                                   
004800*                                 KONTO                                   
004900        05 ORFK-IDKONTO-OK   PIC X.                                       
005000        05 ORFK-IDKST        PIC X(10).                                   
005100*                                 KOSTNADSSTÄLLE                          
005200        05 ORFK-IDKST-OK     PIC X.                                       
005300        05 ORFK-KDKVBRYT     PIC X.                                       
005400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005500        05 ORFK-KDKVBRYT-OK  PIC X.                                       
005600        05 ORFK-KDVRINFO     PIC X.                                       
005700*                                 PÅVERKAN I VR/DSP SYSTEM                
005800        05 ORFK-KDVRINFO-OK  PIC X.                                       
005900        05 ORFK-KVBEART      PIC X(6).                                    
006000*                                 BESTÄLLT ANTAL STYCKEN                  
006100        05 ORFK-KVBEART-OK   PIC X.                                       
006200        05 ORFK-PRARTNTO     PIC X(10).                                   
006300*                                 ARTIKELPRIS NETTO                       
006400        05 ORFK-PRARTNTO-OK  PIC X.                                       
006500        05 ORFK-PRARTNTO-UT  PIC 9(7)V9(2).                               
006600*                                 ARTIKELPRIS NETTO                       
006700        05 ORFK-TITPO-RAD    PIC X(6).                                    
006800*                                 PLANERAD ORDERDATUM                     
006900        05 ORFK-TITPO-OK     PIC X.                                       
007000        05 ORFK-PRARTNTO-LOC PIC X(10).                                   
007100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
007200        05 ORFK-PRARTNTO-LOC-OK                                           
007300                             PIC X.                                       
007400        05 ORFK-PRARTNTO-LOC-UT                                           
007500                             PIC 9(7)V9(2).                               
007600*                                 ARTIKELPRIS NETTO                       
007700        05 ORFK-PRARTNTO-LOCPREL                                          
007800                             PIC X(10).                                   
007900*                                 PREL NETTO SLUTKUNDSPRIS I              
008000*                                 LOKAL VALUTA                            
008100        05 ORFK-PRARTNTO-LOCPREL-OK                                       
008200                             PIC X.                                       
008300        05 ORFK-PRARTNTO-LOCPREL-UT                                       
008400                             PIC 9(7)V9(2).                               
008500*                                 PREL NETTO SLUTKUNDSPRIS I              
008600*                                 LOKAL VALUTA                            
008700        05 ORFK-PRARTBTO-LOC PIC X(10).                                   
008800*                                 PRIS I LOKAL VALUTA                     
008900        05 ORFK-PRARTBTO-LOC-OK                                           
009000                             PIC X.                                       
009100        05 ORFK-PRARTBTO-LOC-UT                                           
009200                             PIC 9(7)V9(2).                               
009300*                                 PRIS I LOKAL VALUTA                     
009400     03 ORFK-W411ORFK-002    OCCURS 100 TIMES.                            
009500*                                 INFO FRÅN ARTIKELREG.                   
009600        05 ORFK-IDARTNR      PIC S9(9)           COMP-3.                  
009700*                                 ARTIKELNUMMER                           
009800        05 ORFK-KDORDBEK     PIC 9(2).                                    
009900*                                 ORDERBEKRÄFTELSEKOD                     
010000        05 ORFK-W411AREG-001.                                             
010100           07 ORFK-CDC-UNIK-INFO.                                         
010200              09 ORFK-ADART.                                              
010300*                                 ARTIKELADRESS I LAGRET                  
010400                 11 ORFK-ADLAGOMR                                         
010500                             PIC S9(3)           COMP-3.                  
010600*                                 LAGEROMRÅDE                             
010700                 11 ORFK-ADGANG                                           
010800                             PIC S9(3)           COMP-3.                  
010900*                                 GÅNG                                    
011000                 11 ORFK-ADPLATS                                          
011100                             PIC S9(5)           COMP-3.                  
011200*                                 LAGERPLATSNUMMER                        
011300              09 ORFK-FILLER1                                             
011400                             PIC X(7).                                    
011500              09 ORFK-FLAVRART                                            
011600                             PIC X.                                       
011700*                                 AVROPSARTIKEL                           
011800              09 ORFK-FLIART PIC X.                                       
011900*                                 ARTIKELN INGÅR I SATS                   
012000              09 ORFK-FLLSRDEL                                            
012100                             PIC X.                                       
012200*                                 LEVERERAS SOM RESDEL                    
012300              09 ORFK-FLMARKSP                                            
012400                             PIC X.                                       
012500*                                 MARKNADSSPÄRR                           
012600              09 ORFK-FLRADREF                                            
012700                             PIC X.                                       
012800*                                 KOMPLETTERANDE INFO. KRÄVS              
012900              09 ORFK-FLREFILL                                            
013000                             PIC X.                                       
013100*                                 REFILLARTIKEL                           
013200              09 ORFK-FLTPO1 PIC X.                                       
013300*                                 ARTIKELN GODKÄND FÖR TPO1               
013400              09 ORFK-IDFKNGRP                                            
013500                             PIC S9(5)           COMP-3.                  
013600*                                 FUNKTIONSGRUPP                          
013700              09 ORFK-IDLEVNR                                             
013800                             PIC X(5).                                    
013900*                                 LEVERANTÖRNUMMER                        
014000              09 ORFK-IDLKTO PIC S9(7)           COMP-3.                  
014100*                                 LAGERKONTO (FFHHHUU)                    
014200              09 ORFK-IDPSN  PIC 9(3).                                    
014300*                                 PROPER SHIPPING NAME                    
014400              09 ORFK-KDERS  PIC S9(3)           COMP-3.                  
014500*                                 ERSÄTTNINGSKOD                          
014600              09 ORFK-KDERS-UTG                                           
014700                             PIC S9(3)           COMP-3.                  
014800*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
014900              09 ORFK-KDFARLIG                                            
015000                             PIC S9              COMP-3.                  
015100*                                 KOD FÖR FARLIGT GODS                    
015200              09 ORFK-KDLEVSP                                             
015300                             PIC S9(3)           COMP-3.                  
015400*                                 SPÄRRKOD LEVERANS                       
015500              09 ORFK-KDPRODSL                                            
015600                             PIC S9(3)           COMP-3.                  
015700*                                 PRODUKTSLAG                             
015800              09 ORFK-KDSORT PIC X(2).                                    
015900*                                 SORT-KOD                                
016000              09 ORFK-KDSPEEMB                                            
016100                             PIC 9.                                       
016200*                                 SPECIALEMBALLAGEKOD                     
016300              09 ORFK-KDUART PIC X.                                       
016400*                                 UNDANTAGSARTIKEL                        
016500              09 ORFK-KDVVKL PIC S9              COMP-3.                  
016600*                                 VOLYMVÄRDESKLASS                        
016700              09 ORFK-KVAKS-CDC                                           
016800                             PIC S9(7)           COMP-3.                  
016900*                                 DEL AV AK SOM LIGGER I CDC              
017000              09 ORFK-KVAKS-PAV                                           
017100                             PIC S9(7)           COMP-3.                  
017200*                                 DEL AV AK PÅ VÄG                        
017300              09 ORFK-KVFRYSTI                                            
017400                             PIC S9(3)           COMP-3.                  
017500*                                 FRYSTID FÖR TPO-ORDER                   
017600              09 ORFK-KVLS   PIC S9(7)           COMP-3.                  
017700*                                 LAGERSALDO                              
017800              09 ORFK-KVLS-SVS                                            
017900                             PIC S9(7)           COMP-3.                  
018000*                                 LAGERSALDO SVS                          
018100              09 ORFK-KDVSOP PIC S9(3)           COMP-3.                  
018200*                                 VSOP-KOD                                
018300              09 ORFK-FILLER2                                             
018400                             PIC X(2).                                    
018500              09 ORFK-KVPB-SATS                                           
018600                             PIC S9(6)V9(1)      COMP-3.                  
018700*                                 SATS-PERIODBEHOV                        
018800              09 ORFK-KVPB-SEP                                            
018900                             PIC S9(6)V9(1)      COMP-3.                  
019000*                                 SEPARAT PERIODBEHOV                     
019100              09 ORFK-KVPB-TPO                                            
019200                             PIC S9(6)V9(1)      COMP-3.                  
019300*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
019400              09 ORFK-KVQPACK-0                                           
019500                             PIC S9(5)           COMP-3.                  
019600*                                 ANTAL I Q0 FÖRPACKNING                  
019700              09 ORFK-KVQPACK-1                                           
019800                             PIC S9(5)           COMP-3.                  
019900*                                 ANTAL I Q1 FÖRPACKNING                  
020000              09 ORFK-KVQPACK-3                                           
020100                             PIC S9(5)           COMP-3.                  
020200*                                 ANTAL I Q3 FÖRPACKNING                  
020300              09 ORFK-KVQPACK-4                                           
020400                             PIC S9(5)           COMP-3.                  
020500*                                 ANTAL I Q4 FÖRPACKNING                  
020600              09 ORFK-KVRESS PIC S9(7)           COMP-3.                  
020700*                                 RESERVERAT ANTAL ARTIKLAR               
020800              09 ORFK-KVROS  PIC S9(7)           COMP-3.                  
020900*                                 RESTORDERSALDO                          
021000              09 ORFK-KVUTRS PIC S9(7)           COMP-3.                  
021100*                                 UTREDNINGSSALDO                         
021200              09 ORFK-PRARTSTD                                            
021300                             PIC S9(7)V9(2)      COMP-3.                  
021400*                                 ARTIKELSTANDARDPRIS                     
021500              09 ORFK-REDIRLEV                                            
021600                             PIC S9V9(2)         COMP-3.                  
021700*                                 DIREKTLEVERANSANDEL                     
021800              09 ORFK-REKSIFFR                                            
021900                             PIC S9              COMP-3.                  
022000*                                 KONTROLLSIFFRA                          
022100              09 ORFK-TIDISPIN                                            
022200                             PIC S9(7)           COMP-3.                  
022300*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
022400              09 ORFK-TIFINLV                                             
022500                             PIC S9(5)           COMP-3.                  
022600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
022700              09 ORFK-FLRELSP                                             
022800                             PIC X.                                       
022900*                                 RELEASEBLOCKAD ARTIKEL .                
023000              09 ORFK-KVSPARR-KVAL                                        
023100                             PIC S9(7)           COMP-3.                  
023200*                                 SPÄRRAT ANTAL KVALITETSFEL              
023300              09 ORFK-IDUSER-SPKVAL                                       
023400                             PIC X(8).                                    
023500*                                 ANVÄNDAR-ID KVALITETSPÄRR               
023600              09 ORFK-FLCCC  PIC X.                                       
023700*                                 CHINA COMPULSORY CERTIFICATE            
023800              09 ORFK-FILLER3                                             
023900                             PIC X.                                       
024000           07 ORFK-DC-UNIK-INFO.                                          
024100              09 ORFK-FLCDCBEH                                            
024200                             PIC X.                                       
024300*                                 REFILLBEHOV                             
024400              09 ORFK-IDDC-REC                                            
024500                             PIC X(2).                                    
024600*                                 MOTTAGANDE LAGER                        
024700           07 ORFK-MULTIPEL-INFO.                                         
024800              09 ORFK-IDANSK PIC S9(3)           COMP-3.                  
024900*                                 ANSKAFFARNUMMER                         
025000              09 ORFK-KDARTURS                                            
025100                             PIC X(2).                                    
025200*                                 ARTIKELURSPRUNGSKOD                     
025300              09 ORFK-KVSLUTKP                                            
025400                             PIC S9(7)           COMP-3.                  
025500*                                 SLUTKÖPSSALDO                           
025600              09 ORFK-KVSPANT                                             
025700                             PIC S9(7)           COMP-3.                  
025800*                                 SPÄRRAT ANTAL                           
025900              09 ORFK-VKART  PIC S9(7)           COMP-3.                  
026000*                                 ARTIKELVIKT (G)                         
026100              09 ORFK-VKART-NTO                                           
026200                             PIC S9(9)           COMP-3.                  
026300*                                 ARTIKELNS NETTOVIKT                     
026400              09 ORFK-VLARTNTO                                            
026500                             PIC S9(8)V9(1)      COMP-3.                  
026600*                                 ARTIKELVOLYM (CM3)                      
026700*** END OF VILMAII-COPY LENGTH= 31234 BYTES                               

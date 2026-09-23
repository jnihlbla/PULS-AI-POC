000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1051200.                                                
000300 AUTHOR.         SUSANNE ENEGARD.                                         
000400 DATE-WRITTEN.   DECEMBER 1984.                                           
000500 DATE-COMPILED.                                                           
000600*    FUNKTION.                                                            
000700*        PROGRAMMETS FRÅGEDELEN HÄMTAR INFORMATION OM KATALOG-            
000800*                    RADER.                                               
000900*        PROGRAMMETS UPPDATERINGSDEL NYREGISTRERAR OCH ÄNDRAR             
001000*                    KATALOGRADER.                                        
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W1T512                                              
001400*        MID:         W1I51201                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W1O51201                                            
001800*                                                                         
001900*    ÄNDRINGAR:                                                           
002000*        92-09-02 ÄT: ATT KUNNA SÖKA PÅ INMATAD POS-BETECKNING            
002100*                    3 NUMERISK ELLER 1-2 NUM + 1 ALFA.                   
002200*                    ÄVEN FIXAT H-JUST AV KVKOL (KAAA-SECTION) /CE        
002300*        94-12-27 VADIS: INLAGT NY KOLUMN FÖR ATT VISA REFERENS           
002400*                        TILL EN RAD FRÅN EN HÄNVISNING.                  
002500*        95-05-10 ÄT: TRE ÄNDRINGAR:                                      
002600*                   - VID ANGIVANDE AV RADNR PÅ INMATNINGSRADEN           
002700*                     PLUS ENTER, GES DEN RADENS VÄRDEN UTSKRIVET         
002800*                     OM DEN REDAN FINNS.                                 
002900*                   - DETTA MÖJLIGGÖR KOPIERING TILL NY RAD VID           
003000*                     UPPDATERING.                                        
003100*                   - SKICKA EN TRANS TILL 1519 VID PF4-TRYCKNING         
003200*                   - ÄVEN MÖJLIGGJORT ATT BACKA 1 SIDA MED PF7           
003300*                     /C.E.                                               
003400*        95-11-15 ÄT: DATUM-MÄRKTA KATALOGRADER, FÖR ATT KUNNA            
003500*                     GENERERA AVSNITT TILL VADIS PER MÅN(PERIOD).        
003600*        NYA DATAELEMENT:                                                 
003700*      KDCATPUB-R-FOM = RAD GÄLLER FR.O.M. (LOW-VALUE = TIDIGASTE)        
003800*                       DETTA ÄR I KOMB.MED IDCATRAD = RADNYCKEL          
003900*      KDCATPUB-R-TOM = RAD GÄLLER T.O.M. (HIGH-VALUE = EJ BEGR.)         
004000*        NYA NYCKLAR:                                                     
004100*        KDCATPUB-R-FOM-F = URVAL FR.O.M. EN VISS PUB-FOM-KOD.            
004200*        KDCATPUB-R-FOM-T = URVAL T.O.M. EN VISS PUB-FOM-KOD.             
004300*                                                                         
004400*        98-10-21: Y2K-ÄNDRINGAR FÖR KDCATPUB TILL KDCATPUB-R             
004500*                                    från 3 till 6 byte.                  
004600*                                                                         
004700*    -- CHECKED BY WY2000  IN 'W.ISPF.EXEC'                               
004800*                                                                         
004900*        98-12-02: DB-ÄNDRINGAR FÖR SEKUNDÄRINDEX-KONV AV WDN5            
005000*                                                                         
005100     EJECT                                                                
005200 ENVIRONMENT DIVISION.                                                    
005300     SKIP3                                                                
005400 DATA DIVISION.                                                           
005500 WORKING-STORAGE SECTION.                                                 
005600*    -COPY WY2000W2                                                       
005700     SKIP3                                                                
005800 77   PROGRAM-NAMN           VALUE 'W1051200'                             
005900                                 PIC X(8).                                
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  OCH                         PIC X       VALUE '&'.                   
006300 77  ELLER                       PIC X       VALUE '!'.                   
006400 77  WS-FELTEXT  PIC X(25)   VALUE SPACE.                                 
006500 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
006600 77  SPRAAK-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
006700 77  KOL                         PIC S9(9)   VALUE +1   COMP SYNC.        
006800 77  RAD                         PIC S9(9)   VALUE +1   COMP SYNC.        
006900 77  RAK-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
007000 77  IX                          PIC S9(9)   VALUE +1   COMP SYNC.        
007100 77  PER-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
007200 77  AAR-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
007300 77  SPRAAK-KOLL                 PIC X(1)    VALUE 'N'.                   
007400 77  INDATA-FEL                  PIC X(1)    VALUE 'N'.                   
007500 77  RAD-FINNS                   PIC X(1)    VALUE 'N'.                   
007600 77  KDHAEN-BLANK                PIC X(1)    VALUE 'N'.                   
007700 77  HAEN-FINNS                  PIC X(1)    VALUE 'N'.                   
007800 77  IDRUBNR-FINNS               PIC X(1)    VALUE 'N'.                   
007900 77  IDFOTNR-FINNS               PIC X(1)    VALUE 'N'.                   
008000 77  BEART-FINNS                 PIC X(1)    VALUE 'N'.                   
008100 77  KVKOL-FINNS                 PIC X(1)    VALUE 'N'.                   
008200 77  BEN-KOLL                    PIC X(1)    VALUE 'N'.                   
008300 77  BORT-FLAGGA                 PIC X(1)    VALUE 'N'.                   
008400 77  RAETT-TEXT                  PIC X(1)    VALUE 'N'.                   
008500 77  RATT-HOMONYM                PIC X(1)    VALUE 'N'.                   
008600 77  FOTNOT-RAKNARE              PIC 9(1)    VALUE ZERO.                  
008700 77  EXTRA-RAD                   PIC 9(1)    VALUE ZERO.                  
008800 77  EXTRA-ANMARK                PIC X(1)    VALUE 'N'.                   
008900 77  UPPDATE-FL                  PIC X(1)    VALUE 'N'.                   
009000 77  AENDR-FL                    PIC X(1)    VALUE 'N'.                   
009100 77  IDCATPOS-OK                 PIC X(1)    VALUE 'N'.                   
009200 77  UPPDAT-GJORD                PIC X(1)    VALUE 'N'.                   
009300 77  GODK-INTERVALL              PIC X(1)    VALUE 'N'.                   
009400 77  SOEK-DATA-SW                PIC X(1)    VALUE 'J'.                   
009500     88 SOEK-DATA-OK     VALUE 'J'.                                       
009600     88 SOEK-DATA-FEL    VALUE 'N'.                                       
009700                                                                          
009800* - - - - - - - - - - -  S U B P R O G R A M                              
009900 01  FILLER                      PIC X(16)   VALUE 'SUBPROGRAM'.          
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100     03 WTXTTR                   PIC X(8)    VALUE 'WTXTTR  '.            
010200     03 W009VADD                 PIC X(8)    VALUE 'W009VADD'.            
010300     03 WDATKONV                 PIC X(8)    VALUE 'WDATKONV'.            
010400     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
010500     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
010600                                                                          
010700* - - - - - - - - - - - - A R B E T S A R E O R                           
010800 01  KEY-IDCATNR-X               PIC X(5)    VALUE SPACE.                 
010900 01  FILLER         REDEFINES KEY-IDCATNR-X.                              
011000     03  KEY-IDCATNR             PIC 9(5).                                
011100* --------------                                                          
011200 01  KEY-IDCATGRP-X              PIC X(2)    VALUE SPACE.                 
011300 01  FILLER         REDEFINES KEY-IDCATGRP-X.                             
011400     03  KEY-IDCATGRP            PIC 9(2).                                
011500* --------------                                                          
011600 01  KEY-IDCATAVS-X              PIC X(4)    VALUE SPACE.                 
011700 01  FILLER         REDEFINES KEY-IDCATAVS-X.                             
011800     03  KEY-IDCATAVS            PIC 9(4).                                
011900* --------------                                                          
012000 01  KEY-IDCATRAD-X              PIC X(4)    VALUE SPACE.                 
012100 01  FILLER         REDEFINES KEY-IDCATRAD-X.                             
012200     03  KEY-IDCATRAD            PIC 9(4).                                
012300* --------------                                                          
012400 01  KEY-IDSKYLT                 PIC X(3)    VALUE SPACE.                 
012500* --------------                                                          
012600 01  KEY-KDCATPUB-MIN-X          PIC X(6)    VALUE SPACE.                 
012700* --------------                                                          
012800 01  KEY-KDCATPUB-MAX-X          PIC X(6)    VALUE SPACE.                 
012900* --------------                                                          
013000 01  WS-IDCATPOS-SOEK-TEST       PIC X(3)    VALUE SPACE.                 
013100*                                                                         
013200     EJECT                                                                
013300* - - - - - - - - - - - - V A R I A B L E R                               
013400 01  FILLER                      PIC X(16)  VALUE 'VARIABLER'.            
013500 01  VARIABLER.                                                           
013600     03  SPAR-IDCATRAD           PIC 9(4)    VALUE ZERO.                  
013700     03  SPAR-WDN512KY-X         PIC X(10)   VALUE SPACE.                 
013800     03  NUVARANDE-IDCATRAD      PIC 9(4)    VALUE ZERO.                  
013900     03  NUVARANDE-KDCATPUB-F    PIC X(6)    VALUE SPACE.                 
014000     03  NUVARANDE-KDCATPUB-T    PIC X(6)    VALUE SPACE.                 
014100     03  SPAR-TEKATANM           PIC X(23)   VALUE SPACE.                 
014200     03  SPAR-IDCATRAD-1         PIC 9(4)    VALUE ZERO.                  
014300     03  SPAR-KDCATPUB-1         PIC X(6)    VALUE SPACE.                 
014400     03  SPAR-IDCATRAD-7         PIC 9(4)    VALUE ZERO.                  
014500     03  SPAR-KDCATPUB-7         PIC X(6)    VALUE SPACE.                 
014600                                                                          
014700*    --- KDCATPUB-R / KDCATPUB-hantering                                  
014800     03 WS-KDCATPUB-R-AVV        PIC X(3)    VALUE SPACE.                 
014900     03 WS-KDCATPUB-AAAAVV       PIC X(6)    VALUE SPACE.                 
015000*    --- Giltiga år =   -1   +0   +1   +2  (indexed by Y2K-IX)            
015100     03 WS-GILTIGA-AAAA.                                                  
015200        05 WS-TIAAAA  OCCURS 4   PIC 9999   VALUE ZERO.                   
015300                                                                          
015400     03 Y2K-IX                   PIC S9(9)  VALUE +1  COMP SYNC.          
015500                                                                          
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)  VALUE 'DATUMAREA'.            
015800 01  DATUMAREA.                                                           
015900     03  DAGENS-TIAAVVD          PIC 9(5)   VALUE ZERO.                   
016000     03  DAGENS-TIAAAA           PIC 9(4)   VALUE ZERO.                   
016100                                                                          
016200     03  DAGENS-DATUM-KOMPL      PIC X(18)   VALUE ZERO.                  
016300     03  DAGENS-DATUM-TIAAMMDD   PIC 9(6)    VALUE ZERO.                  
016400                                                                          
016500     03  WS-TIERSDAT-TIAAVVD     PIC 9(5)    VALUE ZERO.                  
016600     03  FILLER REDEFINES WS-TIERSDAT-TIAAVVD.                            
016700         05  WS-TIERSDAT-TIAAVV  PIC 9(4).                                
016800         05  WS-TIERSDAT-D       PIC 9(1).                                
016900     03  W009VADD-DATUM          PIC S9(5)   VALUE ZERO COMP-3.           
017000     03  W009VADD-ANTAL          PIC S9(3)   VALUE ZERO COMP-3.           
017100                                                                          
017200                                                                          
017300 01  FILLER                      PIC X(16)   VALUE 'HJALP-AREA'.          
017400 01  HJALP-AREA.                                                          
017500     03 FEL-BEART-AREA.                                                   
017600         05 TRUNK-BEART          PIC X(15)   VALUE SPACE.                 
017700         05 FILLER               PIC X(10)   VALUE ALL '*'.               
017800                                                                          
017900     03  WS-GRPAVS .                                                      
018000         05  WS-IDCATGRP         PIC 9(2)   VALUE ZERO.                   
018100         05  WS-IDCATAVS         PIC 9(4)   VALUE ZERO.                   
018200                                                                          
018300*    HÄR LIGGER GODKÄNDA EV. INMATADE KDCATPUB-R FRÅN UPD-RADEN           
018400*    Konverterade till rätt databas-längd.                                
018500     03  WS-KDCATPUB-FOM-UPD     PIC X(6)    VALUE SPACE.                 
018600     03  WS-KDCATPUB-TOM-UPD     PIC X(6)    VALUE SPACE.                 
018700     03  WS-KDCATPUB-H-UPD       PIC X(6)    VALUE SPACE.                 
018800     03  WS-KDCATPUB-BORT-UPD    PIC X(6)    VALUE SPACE.                 
018900                                                                          
019000*    HÄR LIGGER EN GODKÄND INMATAD IDCATRAD-UPD                           
019100     03  WS-IDCATRAD             PIC 9(4)   VALUE ZERO.                   
019200                                                                          
019300     03  WS-GRPAVS-H .                                                    
019400         05  WS-IDCATGRP-H       PIC 9(2)   VALUE ZERO.                   
019500         05  WS-IDCATAVS-H       PIC 9(4)   VALUE ZERO.                   
019600                                                                          
019700     03  WS-IDCATRAD-H           PIC 9(4)   VALUE ZERO.                   
019800     03  WS-KDCATPUB-H           PIC X(6)   VALUE SPACE.                  
019900                                                                          
020000*        FÖR TESTÄNDAMÅL                                                  
020100     03  WS-KDCATPUB-T           PIC X(6)   VALUE SPACE.                  
020200     03  WS-KDCATPUB-SEN-GEN     PIC X(6)    VALUE SPACE.                 
020300     03  WS-KDCATPUB-FOM-BORTRAD PIC X(6)    VALUE SPACE.                 
020400     03  WS-KDCATPUB-TOM-BORTRAD PIC X(6)    VALUE SPACE.                 
020500                                                                          
020600*                                                                         
020700     03  WS-MID-IDCATRAD-UPD     PIC X(4)   VALUE SPACE.                  
020800     03  WS-2-LOWVALUE           PIC XX     VALUE LOW-VALUE.              
020900     03  POS                     PIC 9      VALUE ZERO.                   
021000     03  WS-KDERS                PIC S9(3)   VALUE ZERO COMP-3.           
021100                                                                          
021200     03  FILLER          OCCURS 3.                                        
021300         05  BAS-IDRUBNR         PIC 9(5).                                
021400     03  FILLER          OCCURS 3.                                        
021500         05  BAS-IDFOTNR         PIC 9(5).                                
021600     03  FILLER          OCCURS 3.                                        
021700         05  IN-IDRUBNR          PIC 9(5).                                
021800     03  FILLER          OCCURS 3.                                        
021900         05  IN-IDFOTNR          PIC 9(5).                                
022000                                                                          
022100     03  FILLER          OCCURS 3.                                        
022200         05  SPAR-IDFOTNR        PIC 9(5).                                
022300                                                                          
022400     03  SPAR-ANMARK             PIC X(23).                               
022500     03  FILLER     REDEFINES SPAR-ANMARK.                                
022600         05  ENFOT-1             PIC 9(5).                                
022700         05  ENFOT-TECK-1        PIC X(1).                                
022800         05  ENFOT-SPACE         PIC X(17).                               
022900     03  FILLER     REDEFINES SPAR-ANMARK.                                
023000         05  TVAFOT-1            PIC 9(5).                                
023100         05  TVAFOT-TECK-1       PIC X(1).                                
023200         05  TVAFOT-2            PIC 9(5).                                
023300         05  TVAFOT-TECK-2       PIC X(1).                                
023400         05  TVAFOT-SPACE        PIC X(11).                               
023500     03  FILLER     REDEFINES SPAR-ANMARK.                                
023600         05  TREFOT-1            PIC 9(5).                                
023700         05  TREFOT-TECK-1       PIC X(1).                                
023800         05  TREFOT-2            PIC 9(5).                                
023900         05  TREFOT-TECK-2       PIC X(1).                                
024000         05  TREFOT-3            PIC 9(5).                                
024100         05  TREFOT-TECK-3       PIC X(1).                                
024200         05  TREFOT-SPACE        PIC X(5).                                
024300                                                                          
024400     03  W-IDCATPOS-HEL.                                                  
024500         05  W-IDCATPOS          PIC X(3).                                
024600         05  FILLER       REDEFINES W-IDCATPOS.                           
024700             07  W-IDCATPOS-1    PIC X(1).                                
024800             07  W-IDCATPOS-2    PIC X(1).                                
024900             07  W-IDCATPOS-3    PIC X(1).                                
025000                                                                          
025100     03  UT-BETTEXT              PIC X(25)  VALUE SPACE.                  
025200     03  FILLER          OCCURS 3.                                        
025300         05  UT-BERUBTXT         PIC X(30).                               
025400     EJECT                                                                
025500                                                                          
025600 01  FILLER                      PIC X(16) VALUE 'TETEXTTR-AREA'.         
025700 01  TETEXTTR-AREA.                                                       
025800     03 TETEXT-BEART             PIC X(25)  VALUE SPACE.                  
025900*                                                                         
026000*                     SUMMA LÄNGD MAX = 60                                
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER                      PIC X(16) VALUE 'WTXTAREA'.              
026400*01  FILLER  -COPY WTXTAREA.                                              
026500*                                                                         
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
026800*01  FILLER  -COPY WDATAREA  -PRE IDAG-.                                  
026900     EJECT                                                                
027000* - - - - - - - - - - - - - - - - - - -  TEST-AREA                        
027100 01  FILLER                      PIC X(16)  VALUE 'TEST-AREA'.            
027200 01  TEST-AREA.                                                           
027300     03  TEST-KDCATPUB-FOM       PIC X(6)    VALUE SPACE.                 
027400     03  TEST-KDCATPUB-TOM       PIC X(6)    VALUE SPACE.                 
027500                                                                          
027600     03  TEST-KDFBX              PIC X(1)   VALUE SPACE.                  
027700                                                                          
027800     03  TEST-IDCATPOS-HEL.                                               
027900         05  TEST-IDCATPOS       PIC X(3).                                
028000         05  FILLER       REDEFINES TEST-IDCATPOS.                        
028100             07  TEST-IDCATPOS1  PIC X(1).                                
028200             07  TEST-IDCATPOS2  PIC X(1).                                
028300             07  TEST-IDCATPOS3  PIC X(1).                                
028400                                                                          
028500     03  TEST-IDARTNR            PIC S9(9)  VALUE ZERO  COMP-3.           
028600     03  WS-IDARTNR-UNSTRING     PIC 9(9)  VALUE ZERO.                    
028700                                                                          
028800     03  TEST-KVKOL-GRP.                                                  
028900         05  FILLER   OCCURS 5.                                           
029000             07  TEST-KVKOL      PIC X(3).                                
029100     03  TEST-KDPS               PIC X(2)   VALUE SPACE.                  
029200     03  TEST-KVPUNKT            PIC S9(1)  VALUE ZERO  COMP-3.           
029300     03  TEST-IDTTEXNR           PIC S9(5)  VALUE ZERO  COMP-3.           
029400     03  TEST-BEART              PIC X(25)  VALUE SPACE.                  
029500     03  TEST-KDHOM              PIC S9(1)  VALUE ZERO  COMP-3.           
029600                                                                          
029700     03  FILLER          OCCURS 3.                                        
029800         05  TEST-IDRUBNR        PIC 9(5).                                
029900                                                                          
030000     03  FILLER          OCCURS 3.                                        
030100         05  TEST-IDFOTNR        PIC 9(5).                                
030200     EJECT                                                                
030300* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
030400 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
030500 01  NYCKLAR-TILL-DLI.                                                    
030600   03  W-WDN501-X.                                                        
030700       05  W-IDCATNR             PIC 9(5)   VALUE ZERO.                   
030800       05  W-WDN501-GRPAVS.                                               
030900           07  W-IDCATGRP        PIC 9(2)   VALUE ZERO.                   
031000           07  W-IDCATAVS        PIC 9(4)   VALUE ZERO.                   
031100                                                                          
031200   03 W-WDN5G1KY-X.                                                       
031300     05 W-WDN5GSEQ-HAEN.                                                  
031400*        --- Hänvisat avsnitts RAD-ADRESS                                 
031500         07 W-IDCATNR-GSEQ       PIC 9(5)   VALUE ZERO.                   
031600         07 W-IDCATGRP-GSEQ      PIC 9(2)   VALUE ZERO.                   
031700         07 W-IDCATAVS-GSEQ      PIC 9(4)   VALUE ZERO.                   
031800         07 W-IDCATRAD-GSEQ      PIC 9(4)   VALUE ZERO.                   
031900         07 W-KDCATPUB-GSEQ      PIC X(6)   VALUE SPACE.                  
032000     05 W-IDWDN512-REF-X.                                                 
032100*        --- Hänvisande avsnittets RAD-ADRESS                             
032200*        --- FIELD NAME 'IDCATRKY' i det fysiska DBD:t WDN5G              
032300         07 W-IDCATNR-GSEQ-REF   PIC 9(5)   VALUE ZERO.                   
032400         07 W-IDCATGRP-GSEQ-REF  PIC 9(2)   VALUE ZERO.                   
032500         07 W-IDCATAVS-GSEQ-REF  PIC 9(4)   VALUE ZERO.                   
032600         07 W-IDCATRAD-GSEQ-REF  PIC 9(4)   VALUE ZERO.                   
032700         07 W-KDCATPUB-GSEQ-REF  PIC X(6)   VALUE SPACE.                  
032800                                                                          
032900   03  W-IDCATRKY-LO             PIC X(21) VALUE LOW-VALUE.               
033000   03  W-IDCATRKY-HI             PIC X(21) VALUE HIGH-VALUE.              
033100                                                                          
033200   03  W-WDN512KY-X.                                                      
033300       05  W-IDCATRAD-X.                                                  
033400           07  W-IDCATRAD        PIC 9(4)   VALUE ZERO.                   
033500       05  W-KDCATPUB-X.                                                  
033600           07  W-KDCATPUB        PIC X(6)   VALUE SPACE.                  
033700                                                                          
033800   03  W-WDN512KY-MIN.                                                    
033900       05  W-IDCATRAD-MIN-X.                                              
034000           07  W-IDCATRAD-MIN    PIC 9(4)   VALUE ZERO.                   
034100       05  W-KDCATPUB-MIN-X.                                              
034200           07  W-KDCATPUB-MIN    PIC X(6)   VALUE SPACE.                  
034300                                                                          
034400   03  W-KDCATPUB-MINSOEK        PIC X(6).                                
034500                                                                          
034600   03  W-WDN512KY-MAX.                                                    
034700       05  W-IDCATRAD-MAX-X.                                              
034800           07  W-IDCATRAD-MAX    PIC 9(4)   VALUE ZERO.                   
034900       05  W-KDCATPUB-MAX-X.                                              
035000           07  W-KDCATPUB-MAX    PIC X(6)   VALUE SPACE.                  
035100                                                                          
035200   03  W-KDCATPUB-MAXSOEK        PIC X(6).                                
035300                                                                          
035400   03  W-IDCATPOS-LO-X.                                                   
035500       05  W-IDCATPOS-LO         PIC X(3)   VALUE SPACE.                  
035600                                                                          
035700   03  W-IDCATPOS-HI-X.                                                   
035800       05  W-IDCATPOS-HI         PIC X(3)    VALUE '999'.                 
035900                                                                          
036000   03  W-WDN501-H-X.                                                      
036100       05  W-IDCATNR-H           PIC 9(5)    VALUE ZERO.                  
036200       05  W-WDN501-H-GRPAVS.                                             
036300           07  W-IDCATGRP-H      PIC 9(2)    VALUE ZERO.                  
036400           07  W-IDCATAVS-H      PIC 9(4)    VALUE ZERO.                  
036500                                                                          
036600   03  W-WDN512KY-H-X.                                                    
036700       05  W-IDCATRAD-H-X.                                                
036800           07  W-IDCATRAD-H      PIC 9(4)    VALUE ZERO.                  
036900       05  W-KDCATPUB-H-X.                                                
037000           07  W-KDCATPUB-H      PIC X(6)    VALUE SPACE.                 
037100                                                                          
037200   03  W-IDTTEXNR-X.                                                      
037300       05  W-IDTTEXNR            PIC S9(5)   VALUE ZERO  COMP-3.          
037400   03  W-IDRUBNR-X.                                                       
037500       05  W-IDRUBNR             PIC S9(5)   VALUE ZERO  COMP-3.          
037600   03  W-IDFOTNR-X.                                                       
037700       05  W-IDFOTNR             PIC S9(5)   VALUE ZERO  COMP-3.          
037800                                                                          
037900   03  W-IDCATNR-1-X.                                                     
038000       05  W-IDCATNR-1           PIC 9(5)    VALUE ZERO.                  
038100   03  W-TIAAAA-X.                                                        
038200       05  W-TIAAAA              PIC 9(4)    VALUE ZERO.                  
038300                                                                          
038400   03  W-IDSKYLT-X.                                                       
038500       05  W-IDSKYLT             PIC XXX     VALUE SPACE.                 
038600   03  W-IDARTNR-X.                                                       
038700       05  W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
038800                                                                          
038900   03  W-IDSEGMNR-X.                                                      
039000       05  W-IDSEGMNR            PIC S9      VALUE ZERO  COMP-3.          
039100                                                                          
039200   03  W-BEART-X.                                                         
039300       05  W-BEART               PIC X(25)   VALUE SPACE.                 
039400     EJECT                                                                
039500* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
039600 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
039700 01  MEDDELANDEN.                                                         
039800     03 FILLER-1.                                                         
039900          05 FILLER              PIC X(40)                                
040000              VALUE '    NYCKEL EJ NUMERISK                  '.           
040100          05 FILLER              PIC X(40)                                
040200              VALUE '    KEY NOT NUMERIC                     '.           
040300     03 FILLER REDEFINES FILLER-1.                                        
040400          05 FEL-1   OCCURS 2    PIC X(40).                               
040500     03 FILLER-2.                                                         
040600          05 FILLER              PIC X(40)                                
040700              VALUE '    UPPLYSTA FÄLT FEL                   '.           
040800          05 FILLER              PIC X(40)                                
040900              VALUE '    HIGHLIGHTED FIELDS WRONG            '.           
041000     03 FILLER REDEFINES FILLER-2.                                        
041100          05 FEL-2   OCCURS 2    PIC X(40).                               
041200     03 FILLER-3.                                                         
041300          05 FILLER              PIC X(40)                                
041400              VALUE '    NYCKEL FELAKTIG                     '.           
041500          05 FILLER              PIC X(40)                                
041600              VALUE '    WRONG KEY                           '.           
041700     03 FILLER REDEFINES FILLER-3.                                        
041800          05 FEL-3   OCCURS 2    PIC X(40).                               
041900     03 FILLER-4.                                                         
042000          05 FILLER              PIC X(40)                                
042100              VALUE '    SPRÅK FINNS EJ                      '.           
042200          05 FILLER              PIC X(40)                                
042300              VALUE '    LANGUAGE NOT FOUND                  '.           
042400     03 FILLER REDEFINES FILLER-4.                                        
042500          05 FEL-4   OCCURS 2    PIC X(40).                               
042600     03 FILLER-5.                                                         
042700          05 FILLER              PIC X(40)                                
042800              VALUE '    KATALOG ELLER AVSNITT SAKNAS        '.           
042900          05 FILLER              PIC X(40)                                
043000              VALUE '    CATALOGUE OR TEXT BLOCK IS MISSING  '.           
043100     03 FILLER REDEFINES FILLER-5.                                        
043200          05 FEL-5   OCCURS 2    PIC X(40).                               
043300     03 FILLER-11.                                                        
043400          05 FILLER              PIC X(40)                                
043500              VALUE '    UPPDATERING GJORD                   '.           
043600          05 FILLER              PIC X(40)                                
043700              VALUE '    UPDATING DONE                       '.           
043800     03 FILLER REDEFINES FILLER-11.                                       
043900          05 MED-1   OCCURS 2    PIC X(40).                               
044000     03 FILLER-12.                                                        
044100          05 FILLER              PIC X(40)                                
044200              VALUE '    FLER RADER FINNS                    '.           
044300          05 FILLER              PIC X(40)                                
044400              VALUE '    MORE LINES EXIST                    '.           
044500     03 FILLER REDEFINES FILLER-12.                                       
044600          05 MED-2   OCCURS 2    PIC X(40).                               
044700     03 FILLER-13.                                                        
044800          05 FILLER              PIC X(40)                                
044900              VALUE '    SISTA RADEN HAR KOMPLETTERINGSTEXT  '.           
045000          05 FILLER              PIC X(40)                                
045100              VALUE '    LAST LINE HAS INFORMATION TEXT      '.           
045200     03 FILLER REDEFINES FILLER-13.                                       
045300          05 MED-3   OCCURS 2    PIC X(40).                               
045400     03 FILLER-14.                                                        
045500          05 FILLER              PIC X(40)                                
045600              VALUE '    HOMONYMER FINNS                     '.           
045700          05 FILLER              PIC X(40)                                
045800              VALUE '    HOMONYMOUS EXIST                    '.           
045900     03 FILLER REDEFINES FILLER-14.                                       
046000          05 MED-4   OCCURS 2    PIC X(40).                               
046100     03 FILLER-15.                                                        
046200          05 FILLER              PIC X(25)                                
046300              VALUE '****** INGEN BENÄMN. REG.'.                          
046400          05 FILLER              PIC X(25)                                
046500              VALUE '****** NO DESCR. REGISTR.'.                          
046600     03 FILLER REDEFINES FILLER-15.                                       
046700          05 MED-5   OCCURS 2    PIC X(25).                               
046800     03 FILLER-16.                                                        
046900          05 FILLER              PIC X(55)                                
047000              VALUE 'NOTERING (1513) KAN EJ KOPIERAS TILL NY RAD'.        
047100          05 FILLER              PIC X(55)                                
047200              VALUE 'NOTE (1513) CAN''T BE COPIED TO A NEW LINE'.         
047300     03 FILLER REDEFINES FILLER-16.                                       
047400          05 MED-6   OCCURS 2    PIC X(55).                               
047500     03 FILLER-17.                                                        
047600          05 FILLER              PIC X(55)                                
047700              VALUE ' RADEN FINNS EJ PÅ DETTA AVSNITT'.                   
047800          05 FILLER              PIC X(55)                                
047900              VALUE ' THE LINE DOES NOT EXIST IN THIS TEXTBLOCK'.         
048000     03 FILLER REDEFINES FILLER-17.                                       
048100          05 MED-7   OCCURS 2    PIC X(55).                               
048200     03 FILLER-18.                                                        
048300          05 FILLER              PIC X(55)                                
048400              VALUE ' PRINTNING STARTAD '.                                
048500          05 FILLER              PIC X(55)                                
048600              VALUE ' PRINTOUT STARTED   '.                               
048700     03 FILLER REDEFINES FILLER-18.                                       
048800          05 MED-8   OCCURS 2    PIC X(55).                               
048900     03 FILLER-19.                                                        
049000          05 FILLER              PIC X(55)                                
049100              VALUE ' FLER RADER FINNS,  PÅ SAMMA RADNUMMER  '.           
049200          05 FILLER              PIC X(55)                                
049300              VALUE ' MORE LINES EXIST,  WITH THE SAME LINE ID'.          
049400     03 FILLER REDEFINES FILLER-19.                                       
049500          05 MED-9   OCCURS 2    PIC X(55).                               
049600                                                                          
049700     EJECT                                                                
049800* - - - - - - - - - - - - - - - - - - - - LAND-AREA                       
049900 01  FILLER                      PIC X(16)   VALUE 'LAND-AREA'.           
050000*01  -COPY WWLAND03                                                       
050100     EJECT                                                                
050200* - - - - - - - - - - - - - - - - - - - - BYTES-TEST                      
050300 01  FILLER                      PIC X(16)   VALUE 'BYTES-TEST'.          
050400 01  BYTES-IDARTNR               PIC 9(9)    COMP-3.                      
050500*01  FILLER   -COPY WWBYT02     -RED  BYTES-IDARTNR.                      
050600*01  FILLER   -COPY WWBYT03     -RED  BYTES-IDARTNR.                      
050700     EJECT                                                                
050800* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
050900 01  FILLER                      PIC X(16)   VALUE '1511-MID'.            
051000*01  -COPY W1I51101  -PRE 1511- .                                         
051100 01  FILLER                      PIC X(16)   VALUE '1513-MID'.            
051200*01  -COPY W1I51301  -PRE 1513- .                                         
051300 01  FILLER                      PIC X(16)   VALUE '1514-MID'.            
051400*01  -COPY W1I51401  -PRE 1514- .                                         
051500 01  FILLER                      PIC X(16)   VALUE '1515-MID'.            
051600*01  -COPY W1I51501  -PRE 1515- .                                         
051700 01  FILLER                      PIC X(16)   VALUE '1519-MID'.            
051800*01  -COPY W1I51901  -PRE 1519- .                                         
051900     EJECT                                                                
052000 01  FILLER                      PIC X(16)   VALUE '1512-MID'.            
052100*01  -COPY W1I51201.                                                      
052200     EJECT                                                                
052300* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
052400 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
052500*01  -COPY WMSGAREA                                                       
052600     EJECT                                                                
052700*    03  POST  -COPY W1O51201 -RED MSG-AREA.                              
052800     EJECT                                                                
052900* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
053000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
053100*01  -COPY WMFSAREA                                                       
053200     EJECT                                                                
053300* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
053400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
053500 01  IMS-WS.                                                              
053600*                        **** STATUS-KOD FRÅN IMS                         
053700   03  STATUS-WS                 PIC XX.                                  
053800     88  SEGMENT-FINNS                       VALUE '  '.                  
053900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
054000     88  BASEN-SLUT                          VALUE 'GB'.                  
054100     SKIP2                                                                
054200   03  GODK-STATUSKODER.                                                  
054300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
054400     SKIP2                                                                
054500 01  SSA1                        PIC X(160).                              
054600 01  SSA2                        PIC X(160).                              
054700 01  SSA3                        PIC X(160).                              
054800     EJECT                                                                
054900*                            IMS FUNKTIONSKODER                           
055000*01    -COPY W0003                                                        
055100     EJECT                                                                
055200* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
055300 01  DLI-IO-AREA.                                                         
055400     03         FILLER           PIC X(16)  VALUE 'IO-AREA-G'.            
055500     03         IO-AREA-G        PIC X(43)  VALUE SPACE.                  
055600*    03  WLKATS01 -COPY WDN5G1   -RED IO-AREA-G.                          
055700                                                                          
055800     03  AVSG-REF-IDWDN512.                                               
055900        05  AVSG-REF-IDCATNR       PIC 9(5).                              
056000        05  AVSG-REF-IDCATGRP      PIC 9(2).                              
056100        05  AVSG-REF-IDCATAVS      PIC 9(4).                              
056200        05  AVSG-REF-IDCATRAD      PIC 9(4).                              
056300        05  AVSG-REF-KDCATPUB-FOM  PIC X(6).                              
056400     EJECT                                                                
056500     03         FILLER           PIC X(16)  VALUE 'IO-AREA-1'.            
056600     03         IO-AREA-1        PIC X(70)  VALUE SPACE.                  
056700     SKIP3                                                                
056800*    03  WLKATH01 -COPY WDN501        -RED IO-AREA-1.                     
056900     EJECT                                                                
057000*    03  WLKATH12 -COPY WDN512        -RED IO-AREA-1.                     
057100     EJECT                                                                
057200*    03  WLKATH21 -COPY WDN521        -RED IO-AREA-1.                     
057300     EJECT                                                                
057400*    03  WLKATH22 -COPY WDN522        -RED IO-AREA-1.                     
057500     EJECT                                                                
057600*    03  WLKATH23 -COPY WDN523        -RED IO-AREA-1.                     
057700     EJECT                                                                
057800*    03  WLKATH24 -COPY WDN524        -RED IO-AREA-1.                     
057900     EJECT                                                                
058000*    03  WLKATH25 -COPY WDN525        -RED IO-AREA-1.                     
058100     EJECT                                                                
058200*    03  WLKATH26 -COPY WDN526        -RED IO-AREA-1.                     
058300     EJECT                                                                
058400*    03  WLKATH27 -COPY WDN527        -RED IO-AREA-1.                     
058500     EJECT                                                                
058600     SKIP3                                                                
058700     03         FILLER           PIC X(16)  VALUE 'WDN501-IO2'.           
058800*    03  WLKATH01 -COPY WDN501      -PRE IO2-.                            
058900     EJECT                                                                
059000     03         FILLER           PIC X(16)  VALUE 'WDN512-IO2'.           
059100*    03  WLKATH12 -COPY WDN512      -PRE IO2-.                            
059200     EJECT                                                                
059300     03         FILLER           PIC X(16)  VALUE 'WDN524-IO2'.           
059400*    03  WLKATH24 -COPY WDN524      -PRE IO2-.                            
059500     EJECT                                                                
059600     03         FILLER           PIC X(16)  VALUE 'WDK601-AREA'.          
059700*    03  WLARTC01 -COPY WDK601      -PRE ARTC01-.                         
059800     EJECT                                                                
059900     03         FILLER           PIC X(16)  VALUE 'WDK611-AREA'.          
060000*    03  WLARTC11 -COPY WDK611      -PRE ARTC11-.                         
060100     EJECT                                                                
060200     03         FILLER           PIC X(16)  VALUE 'IO-AREA-2'.            
060300     03         IO-AREA-2        PIC X(120) VALUE SPACE.                  
060400*    03  WLKATB01 -COPY WDN201      -PRE RUB-  -RED IO-AREA-2.            
060500     EJECT                                                                
060600*    03  WLKATB11 -COPY WDN211      -PRE RUB-  -RED IO-AREA-2.            
060700     EJECT                                                                
060800*    03  WLKATF01 -COPY WDN301      -PRE FOT-  -RED IO-AREA-2.            
060900     EJECT                                                                
061000*    03  WLKATD01 -COPY WDN401                 -RED IO-AREA-2.            
061100     EJECT                                                                
061200*    03  WLKATD11 -COPY WDN411                 -RED IO-AREA-2.            
061300     EJECT                                                                
061400*    03  WLBENA01 -COPY WDD301      -PRE BEN-  -RED IO-AREA-2.            
061500     EJECT                                                                
061600*    03  WLBENA11 -COPY WDD311      -PRE BEN-  -RED IO-AREA-2.            
061700     EJECT                                                                
061800*    03  WLBENA13 -COPY WDD313      -PRE BEN-  -RED IO-AREA-2.            
061900     EJECT                                                                
062000*    03  WLKATM01 -COPY WDN101      -PRE KAT-.                            
062100     EJECT                                                                
062200*    03  WLKATM11 -COPY WDN111      -PRE KAT-.                            
062300     EJECT                                                                
062400 LINKAGE SECTION.                                                         
062500     SKIP2                                                                
062600*01  -COPY W0009          -PRE MSG-                                       
062700     EJECT                                                                
062800*01  -COPY W0008          -PRE AVS-                                       
062900**    WDN501KY                                                            
063000        05 AVS-KEY-FB-IDCATNR   PIC 9(5).                                 
063100        05 AVS-KEY-FB-IDCATGRP  PIC 9(2).                                 
063200        05 AVS-KEY-FB-IDCATAVS  PIC 9(4).                                 
063300**    WDN512KY                                                            
063400        05 AVS-KEY-FB-IDCATRAD  PIC 9(4).                                 
063500        05 AVS-KEY-FB-KDCATPUB  PIC X(6).                                 
063600**    WDN521                                                              
063700        05 AVS-KEY-FB-KDSEGKEY  PIC X(1).                                 
063800                                                                          
063900     EJECT                                                                
064000*01  -COPY W0008          -PRE AVS2-                                      
064100     05  FILLER                  PIC X.                                   
064200     EJECT                                                                
064300*01  -COPY W0008          -PRE ARTC-                                      
064400     05  FILLER                  PIC X.                                   
064500     EJECT                                                                
064600*01  -COPY W0008          -PRE RUB-                                       
064700     05  FILLER                  PIC X.                                   
064800     EJECT                                                                
064900*01  -COPY W0008          -PRE FOT-                                       
065000     05  FILLER                  PIC X.                                   
065100     EJECT                                                                
065200*01  -COPY W0008          -PRE TEXT-                                      
065300     05  FILLER                  PIC X.                                   
065400     EJECT                                                                
065500*01  -COPY W0008          -PRE KATM-                                      
065600     05  FILLER                  PIC X.                                   
065700     EJECT                                                                
065800*01  -COPY W0008          -PRE BENA-                                      
065900     05  FILLER                  PIC X.                                   
066000     EJECT                                                                
066100*01  -COPY W0008          -PRE BENB-                                      
066200     05  FILLER                  PIC X.                                   
066300     EJECT                                                                
066400*01  -COPY W0008          -PRE KATS-                                      
066500     05  FILLER                  PIC X.                                   
066600     EJECT                                                                
066700 PROCEDURE DIVISION USING MSG-PCB AVS-PCB AVS2-PCB ARTC-PCB               
066800     RUB-PCB FOT-PCB TEXT-PCB KATM-PCB BENA-PCB BENB-PCB KATS-PCB.        
066900     ENTRY 'DLITCBL' USING MSG-PCB AVS-PCB AVS2-PCB ARTC-PCB              
067000     RUB-PCB FOT-PCB TEXT-PCB KATM-PCB BENA-PCB BENB-PCB KATS-PCB.        
067100     SKIP2                                                                
067200     PERFORM IMS-GET-MSG                                                  
067300                                                                          
067400     IF SEGMENT-FINNS                                                     
067500        PERFORM A-INIT                                                    
067600                                                                          
067700        IF (KEY-IDCATNR-X NOT NUMERIC)                                    
067800        OR (KEY-IDCATGRP-X NOT NUMERIC)                                   
067900        OR (KEY-IDCATAVS-X NOT NUMERIC)                                   
068000        OR (KEY-IDCATRAD-X NOT NUMERIC)                                   
068100           MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                         
068200           PERFORM J-RENSA-UPPDAT-FAELT                                   
068300           PERFORM L-RENSA-UTRADER                                        
068400           PERFORM O-INITIERA-DOLDA-FAELT                                 
068500        ELSE                                                              
068600           PERFORM S04-FYLL-DOLT-FAELT                                    
068700           IF SPRAAK-KOLL = NEJ                                           
068800              MOVE FEL-4(SPRAAK-IX) TO MOD-TEMFSFEL                       
068900           ELSE                                                           
069000              MOVE KEY-IDCATNR      TO W-IDCATNR                          
069100                                       W-IDCATNR-1                        
069200                                       W-IDCATNR-H                        
069300              MOVE KEY-IDCATGRP     TO WS-IDCATGRP                        
069400              MOVE KEY-IDCATAVS     TO WS-IDCATAVS                        
069500              MOVE KEY-IDCATRAD     TO WS-IDCATRAD                        
069600                                                                          
069700              PERFORM IMS-GET-KATM-KAT                                    
069800              IF SEGMENT-SAKNAS                                           
069900                MOVE ' ' TO  UPPDATE-FL                                   
070000              END-IF                                                      
070100                                                                          
070200              IF MFS-UPDATE                                               
070300                 MOVE NEJ TO AENDR-FL                                     
070400                 PERFORM B-KOLLA-INDATA                                   
070500                                                                          
070600                 IF INDATA-FEL = JA                                       
070700                    MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSFEL                
070800*                   STRING 'B-. UPPL FÄLT FEL 1.' WS-FELTEXT              
070900*                   DELIMITED BY SIZE        INTO MOD-TEMFSFEL            
071000                    MOVE SPACE TO UPPDATE-FL                              
071100                 ELSE                                                     
071200                    MOVE WS-GRPAVS        TO W-WDN501-GRPAVS              
071300                    MOVE WS-IDCATRAD      TO W-IDCATRAD                   
071400*                                 rad på uppdatraden                      
071500                    MOVE WS-KDCATPUB-FOM-UPD TO W-KDCATPUB-X              
071600*                                 pub på uppdatraden                      
071700                    MOVE KEY-KDCATPUB-MIN-X TO W-KDCATPUB-MIN-X           
071800                    MOVE KEY-KDCATPUB-MAX-X TO W-KDCATPUB-MAX-X           
071900*                                 pub-intervallet för visning             
072000                                                                          
072100                    IF UPPDATE-FL = 'B'                                   
072200                       PERFORM C-KOLLA-BORTTAG-DATA                       
072300                       IF INDATA-FEL = JA                                 
072400                          MOVE FEL-2 (SPRAAK-IX) TO                       
072500                               MOD-TEMFSFEL                               
072600*                              STRING 'C- UPPL FÄLT FEL 2.'               
072700*                              WS-FELTEXT                                 
072800*                              DELIMITED BY SIZE INTO MOD-TEMFSFEL        
072900                          MOVE SPACE TO UPPDATE-FL                        
073000                       ELSE                                               
073100                          PERFORM D-BORTTAG                               
073200                       END-IF                                             
073300                    ELSE                                                  
073400                       PERFORM E-KOLLA-UPPDATE                            
073500                       IF INDATA-FEL = NEJ                                
073600                          IF UPPDATE-FL = 'N'                             
073700                             PERFORM F-KOLLA-NYREG-DATA                   
073800                                                                          
073900                             IF INDATA-FEL = JA                           
074000                                MOVE FEL-2 (SPRAAK-IX) TO                 
074100                                  MOD-TEMFSFEL                            
074200*                               STRING 'F- UPPL FÄLT FEL 3.'              
074300*                               WS-FELTEXT                                
074400*                              DELIMITED BY SIZE INTO MOD-TEMFSFEL        
074500                                MOVE SPACE TO UPPDATE-FL                  
074600                             ELSE                                         
074700                                PERFORM G-NYREGISTRERA                    
074800                             END-IF                                       
074900                          ELSE                                            
075000                             IF UPPDATE-FL = 'Ä'                          
075100                                PERFORM H-KOLLA-ANDRING-DATA              
075200                                                                          
075300                                IF INDATA-FEL = JA                        
075400                                   MOVE FEL-2 (SPRAAK-IX) TO              
075500                                     MOD-TEMFSFEL                         
075600*                                  STRING 'H- UPPL FÄLT FEL 4.'           
075700*                                  WS-FELTEXT                             
075800*                                  DELIMITED BY SIZE                      
075900*                                  INTO MOD-TEMFSFEL                      
076000                                   MOVE SPACE TO UPPDATE-FL               
076100                                ELSE                                      
076200                                   PERFORM I-AENDRING                     
076300                                   MOVE JA TO AENDR-FL                    
076400                                END-IF                                    
076500                             ELSE                                         
076600                                MOVE FEL-2 (SPRAAK-IX) TO                 
076700                                  MOD-TEMFSFEL                            
076800*                                  STRING 'UPPL FÄLT FEL 5.'              
076900*                                  WS-FELTEXT                             
077000*                                  DELIMITED BY SIZE                      
077100*                                  INTO MOD-TEMFSFEL                      
077200                                MOVE SPACE TO UPPDATE-FL                  
077300                             END-IF                                       
077400                          END-IF                                          
077500                       ELSE                                               
077600                          MOVE FEL-3 (SPRAAK-IX) TO                       
077700                               MOD-TEMFSFEL                               
077800                          MOVE 'F' TO UPPDATE-FL                          
077900                       END-IF                                             
078000                    END-IF                                                
078100                 END-IF                                                   
078200                                                                          
078300                 IF UPPDATE-FL = ' '                                      
078400                    PERFORM M-VISA-BILD-IGEN                              
078500                 ELSE                                                     
078600                    IF UPPDATE-FL = 'F'                                   
078700                       PERFORM J-RENSA-UPPDAT-FAELT                       
078800                       PERFORM L-RENSA-UTRADER                            
078900                       PERFORM O-INITIERA-DOLDA-FAELT                     
079000                    ELSE                                                  
079100                      IF  INDATA-FEL = NEJ                                
079200                      AND UPPDATE-FL = 'B' OR 'N' OR 'Ä'                  
079300                        PERFORM S05-AVSNITT-UPPDAT-STATUS                 
079400                      END-IF                                              
079500                      PERFORM R-FLYTTA-LAS-START                          
079600                      PERFORM K-LAS-KATALOGRAD                            
079700                      PERFORM N-FYLL-DOLDA-FALT                           
079800                      PERFORM J-RENSA-UPPDAT-FAELT                        
079900                      PERFORM L-RENSA-UTRADER                             
080000                      MOVE MFS-ADD-SAETT-CURSOR                           
080100                                       TO MOD-IDCATRAD-UPD-ATTR           
080200                    END-IF                                                
080300                 END-IF                                                   
080400              ELSE                                                        
080500**               bara läsning                                             
080600                 MOVE WS-GRPAVS          TO W-WDN501-GRPAVS               
080700                                            W-WDN501-H-GRPAVS             
080800                 IF MFS-IDTRANS = '1514' OR '1515'                        
080900*                   --- Här hämtas den KDCATPUB-R T.O.M.                  
081000*                   --- som ses på 1514-bildens nyckelrads ut-data        
081100                   MOVE 0001                TO W-IDCATRAD-H               
081200                   MOVE KEY-KDCATPUB-MIN-X  TO W-KDCATPUB-H               
081300*                  --- MINSTA PUB för visnings-intervallet                
081400                                                                          
081500                   PERFORM IMS-GU-AVS-RAD-IO2                             
081600                   IF SEGMENT-FINNS                                       
081700                     MOVE IO2-RAD-KDCATPUB-TOM                            
081800                                           TO KEY-KDCATPUB-MAX-X          
081900                     MOVE IO2-RAD-KDCATPUB-TOM(4:3)                       
082000                                       TO MOD-KDCATPUB-R-MAX-UT           
082100                   ELSE                                                   
082200                     MOVE ALL '999999'     TO KEY-KDCATPUB-MAX-X          
082300                                          MOD-KDCATPUB-R-MAX-UT           
082400                   END-IF                                                 
082500                 END-IF                                                   
082600                 MOVE KEY-KDCATPUB-MIN-X TO W-KDCATPUB-MIN                
082700                 MOVE KEY-KDCATPUB-MAX-X TO W-KDCATPUB-MAX                
082800*                  STÖRSTA katpub för visnings-intervallet                
082900                 IF MFS-FIRST                                             
083000*                   läser värdena för föregående bild vid pf7             
083100                    MOVE MID-IDCATRAD-PF7 TO KEY-IDCATRAD                 
083200                    MOVE 0020             TO MOD-IDCATRAD-PF7             
083300                 ELSE                                                     
083400                    IF MFS-NEXT                                           
083500                       MOVE MID-IDCATRAD-FROM TO MOD-IDCATRAD-PF7         
083600                       MOVE MID-IDCATRAD-TOM  TO KEY-IDCATRAD             
083700                    ELSE                                                  
083800                       PERFORM S03-KOLLA-RAD-FROM                         
083900                    END-IF                                                
084000                 END-IF                                                   
084100                                                                          
084200                 MOVE KEY-IDCATRAD   TO W-IDCATRAD                        
084300                                        W-IDCATRAD-MIN                    
084400                 MOVE  9999          TO W-IDCATRAD-MAX                    
084500                 PERFORM K-LAS-KATALOGRAD                                 
084600                 PERFORM N-FYLL-DOLDA-FALT                                
084700                 PERFORM J-RENSA-UPPDAT-FAELT                             
084800                 PERFORM L-RENSA-UTRADER                                  
084900                                                                          
085000                 IF INDATA-FEL = NEJ                                      
085100                   IF (MID-IDCATRAD-UPD NOT = ALL '+' )                   
085200                   AND MFS-IDTRANS = '1512'                               
085300                     PERFORM T-VISA-UPPDAT-RAD                            
085400                   END-IF                                                 
085500                                                                          
085600                   IF MOD-IDCATRAD-UPD-ATTR = MFS-OEPPNA-NUM-FAELT        
085700                      CONTINUE                                            
085800                   ELSE                                                   
085900                      MOVE MFS-ADD-SAETT-CURSOR                           
086000                                     TO MOD-IDCATRAD-UPD-ATTR             
086100                   END-IF                                                 
086200                 ELSE                                                     
086300                   IF MOD-TEMFSFEL = SPACE                                
086400                    MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSFEL                
086500*                   STRING 'UPPL FÄLT FEL 6.' WS-FELTEXT                  
086600*                   DELIMITED BY SIZE      INTO MOD-TEMFSFEL              
086700                   END-IF                                                 
086800                 END-IF                                                   
086900              END-IF                                                      
087000           END-IF                                                         
087100        END-IF                                                            
087200                                                                          
087300        MOVE LENGTH OF MOD-W1O51201 TO MSG-KVLL                           
087400        ADD               +4       TO MSG-KVLL                            
087500        PERFORM IMS-INSERT-MSG                                            
087600     END-IF                                                               
087700                                                                          
087800     MOVE ZERO TO RETURN-CODE                                             
087900     GOBACK                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 A-INIT SECTION.                                                          
088300     SKIP2                                                                
088400     MOVE SPACE TO KEY-IDSKYLT                                            
088500     IF MSG-DUBBLA-TRANSKODER                                             
088600        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51201                
088700        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
088800        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
088900        MOVE MSG-IDPFK TO MFS-IDPFK                                       
089000                                                                          
089100        IF MFS-IDTRANS = '1512'                                           
089200           MOVE MSG-KDTRTYP  TO MFS-KDTRTYP                               
089300        ELSE                                                              
089400           SET MFS-QUERY TO TRUE                                          
089500        END-IF                                                            
089600     ELSE                                                                 
089700        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51201                 
089800        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
089900        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
090000     END-IF                                                               
090100                                                                          
090200     INSPECT MID-W1I51201 REPLACING ALL '>' BY SPACE                      
090300                                    ALL '<' BY SPACE                      
090400     IF MFS-IDTRANS = '1512'                                              
090500        CONTINUE                                                          
090600     ELSE                                                                 
090700        SET MFS-ENTER TO TRUE                                             
090800        IF MFS-IDTRANS NOT = 1511 AND 1513 AND 1514 AND 1515              
090900                                  AND 1518 AND 1519                       
091000           MOVE SPACE   TO MID-IDSKYLT-IN                                 
091100           MOVE ALL '0' TO MID-IDCATNR-IN                                 
091200                           MID-IDCATGRP-IN                                
091300                           MID-IDCATAVS-IN                                
091400        END-IF                                                            
091500     END-IF                                                               
091600*    ------ Initiera Årtal Och Datum                                      
091700     MOVE 'IDAG  ' TO IDAG-DAT-KDDATFORM                                  
091800     CALL WDATKONV USING  IDAG-DAT-KDDATFORM  IDAG-DAT-I-TIDATUM,         
091900                          IDAG-DAT-O-TIDATUM  IDAG-DAT-KDSVAR             
092000                                                                          
092100     MOVE FUNCTION CURRENT-DATE   TO DAGENS-DATUM-KOMPL                   
092200     MOVE DAGENS-DATUM-KOMPL(3:6) TO DAGENS-DATUM-TIAAMMDD                
092300     MOVE DAGENS-DATUM-KOMPL(1:4) TO DAGENS-TIAAAA                        
092400                                                                          
092500     ADD -1 TO DAGENS-TIAAAA GIVING WS-TIAAAA(1)                          
092600     ADD  0 TO DAGENS-TIAAAA GIVING WS-TIAAAA(2)                          
092700     ADD  1 TO DAGENS-TIAAAA GIVING WS-TIAAAA(3)                          
092800     ADD  2 TO DAGENS-TIAAAA GIVING WS-TIAAAA(4)                          
092900*    ------                                                               
093000     IF MID-IDCATNR-IN = ALL '+'                                          
093100        MOVE MID-IDCATNR-UT TO KEY-IDCATNR-X                              
093200        IF KEY-IDCATNR-X = SPACE                                          
093300           CONTINUE                                                       
093400        ELSE                                                              
093500          INSPECT KEY-IDCATNR-X REPLACING LEADING SPACE BY ZERO           
093600        END-IF                                                            
093700     ELSE                                                                 
093800        MOVE MID-IDCATNR-IN TO KEY-IDCATNR-X                              
093900     END-IF                                                               
094000                                                                          
094100     IF MID-IDCATGRP-IN = ALL '+'                                         
094200        MOVE MID-IDCATGRP-UT TO KEY-IDCATGRP-X                            
094300        IF KEY-IDCATGRP-X = SPACE                                         
094400           CONTINUE                                                       
094500        END-IF                                                            
094600        INSPECT KEY-IDCATGRP-X REPLACING LEADING SPACE BY ZERO            
094700     ELSE                                                                 
094800        MOVE MID-IDCATGRP-IN TO KEY-IDCATGRP-X                            
094900     END-IF                                                               
095000                                                                          
095100     IF MID-IDCATAVS-IN = ALL '+'                                         
095200        MOVE MID-IDCATAVS-UT TO KEY-IDCATAVS-X                            
095300        IF KEY-IDCATAVS-X = SPACE                                         
095400           CONTINUE                                                       
095500        END-IF                                                            
095600        INSPECT KEY-IDCATAVS-X REPLACING LEADING SPACE BY ZERO            
095700     ELSE                                                                 
095800        MOVE MID-IDCATAVS-IN TO KEY-IDCATAVS-X                            
095900     END-IF                                                               
096000                                                                          
096100     IF MID-IDCATRAD-IN = ALL '+'                                         
096200        MOVE MID-IDCATRAD-UT TO KEY-IDCATRAD-X                            
096300        INSPECT KEY-IDCATRAD-X REPLACING LEADING SPACE BY ZERO            
096400     ELSE                                                                 
096500        MOVE MID-IDCATRAD-IN TO KEY-IDCATRAD-X                            
096600     END-IF                                                               
096700                                                                          
096800     IF MID-IDCATPOS-SOEK-UPD = ALL '+'                                   
096900       CONTINUE                                                           
097000     ELSE                                                                 
097100       MOVE MID-IDCATPOS-SOEK-UPD TO WS-IDCATPOS-SOEK-TEST                
097200     END-IF                                                               
097300                                                                          
097400     IF  MID-IDCATNR-IN      = ALL '+'                                    
097500     AND MID-IDCATGRP-IN     = ALL '+'                                    
097600     AND MID-IDCATAVS-IN     = ALL '+'                                    
097700     AND MID-KDCATPUB-R-MIN-IN = ALL '+'                                  
097800     AND MID-KDCATPUB-R-MAX-IN = ALL '+'                                  
097900        IF MID-IDCATRAD-IN   = ALL '+'                                    
098000           CONTINUE                                                       
098100        ELSE                                                              
098200           IF KEY-IDCATRAD-X NUMERIC                                      
098300              IF KEY-IDCATRAD < 0020                                      
098400                 MOVE 0020 TO KEY-IDCATRAD                                
098500              END-IF                                                      
098600           END-IF                                                         
098700           SET MFS-QUERY TO TRUE                                          
098800        END-IF                                                            
098900     ELSE                                                                 
099000        IF MID-IDCATRAD-IN = ALL '+' AND MFS-IDTRANS NOT = '1513'         
099100           MOVE 0020 TO KEY-IDCATRAD                                      
099200        ELSE                                                              
099300           IF KEY-IDCATRAD-X NUMERIC                                      
099400              IF KEY-IDCATRAD < 0020                                      
099500                 MOVE 0020 TO KEY-IDCATRAD                                
099600              END-IF                                                      
099700           ELSE                                                           
099800              MOVE 0020 TO KEY-IDCATRAD                                   
099900           END-IF                                                         
100000        END-IF                                                            
100100        MOVE KEY-IDCATRAD TO WS-IDCATRAD                                  
100200        SET MFS-QUERY     TO TRUE                                         
100300     END-IF                                                               
100400                                                                          
100500     IF MID-IDSKYLT-IN = ALL '+'                                          
100600        MOVE MID-IDSKYLT-UT TO KEY-IDSKYLT                                
100700     ELSE                                                                 
100800        MOVE MID-IDSKYLT-IN TO KEY-IDSKYLT                                
100900     END-IF                                                               
101000                                                                          
101100     IF KEY-IDSKYLT NOT = SPACE                                           
101200       MOVE FUNCTION UPPER-CASE(KEY-IDSKYLT) TO KEY-IDSKYLT               
101300     ELSE                                                                 
101400        MOVE 'S  '            TO KEY-IDSKYLT                              
101500     END-IF                                                               
101600                                                                          
101700     IF MID-KDCATPUB-R-MIN-IN = ALL '+'                                   
101800        MOVE MID-KDCATPUB-R-MIN-UT TO WS-KDCATPUB-R-AVV                   
101900     ELSE                                                                 
102000        MOVE MID-KDCATPUB-R-MIN-IN TO WS-KDCATPUB-R-AVV                   
102100     END-IF                                                               
102200     PERFORM S50-Y2K-KDCATPUB-R                                           
102300     MOVE WS-KDCATPUB-AAAAVV     TO KEY-KDCATPUB-MIN-X                    
102400     INSPECT KEY-KDCATPUB-MIN-X                                           
102500                         REPLACING ALL SPACE BY LOW-VALUE                 
102600                                LEADING ZERO BY LOW-VALUE                 
102700     IF MID-KDCATPUB-R-MAX-IN = ALL '+'                                   
102800        MOVE MID-KDCATPUB-R-MAX-UT TO WS-KDCATPUB-R-AVV                   
102900     ELSE                                                                 
103000        MOVE MID-KDCATPUB-R-MAX-IN TO WS-KDCATPUB-R-AVV                   
103100     END-IF                                                               
103200     PERFORM S50-Y2K-KDCATPUB-R                                           
103300     MOVE WS-KDCATPUB-AAAAVV     TO KEY-KDCATPUB-MAX-X                    
103400     INSPECT KEY-KDCATPUB-MAX-X REPLACING ALL SPACE BY HIGH-VALUE         
103500                                                                          
103600     MOVE LOW-VALUE TO MSG-AREA                                           
103700     MOVE 'W1O51201' TO MFS-IDMOD                                         
103800     MOVE '1512' TO MOD-IDTRANS                                           
103900                                                                          
104000     IF ENGLISH-TEXT                                                      
104100        MOVE +2 TO SPRAAK-IX                                              
104200     ELSE                                                                 
104300        MOVE +1 TO SPRAAK-IX                                              
104400     END-IF                                                               
104500                                                                          
104600     MOVE KEY-IDCATNR-X   TO MOD-IDCATNR-UT                               
104700     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
104800                                                                          
104900     MOVE KEY-IDCATGRP-X  TO MOD-IDCATGRP-UT                              
105000     INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE              
105100     IF MOD-IDCATGRP-UT = SPACE                                           
105200       MOVE ' 0'  TO MOD-IDCATGRP-UT                                      
105300     END-IF                                                               
105400                                                                          
105500     MOVE KEY-IDCATAVS-X  TO MOD-IDCATAVS-UT                              
105600     INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE              
105700     IF MOD-IDCATAVS-UT = SPACE                                           
105800       MOVE '   0'  TO MOD-IDCATAVS-UT                                    
105900     END-IF                                                               
106000                                                                          
106100     MOVE KEY-IDSKYLT     TO MOD-IDSKYLT-UT                               
106200                                                                          
106300     MOVE KEY-IDCATRAD-X  TO MOD-IDCATRAD-UT                              
106400     INSPECT MOD-IDCATRAD-UT REPLACING LEADING ZERO BY SPACE              
106500                                                                          
106600     MOVE KEY-KDCATPUB-MIN-X(4:3) TO MOD-KDCATPUB-R-MIN-UT                
106700     INSPECT MOD-KDCATPUB-R-MIN-UT REPLACING                              
106800                                   LEADING LOW-VALUE BY SPACE             
106900                                                                          
107000     MOVE KEY-KDCATPUB-MAX-X(4:3) TO MOD-KDCATPUB-R-MAX-UT                
107100     INSPECT MOD-KDCATPUB-R-MAX-UT REPLACING                              
107200                                   LEADING HIGH-VALUE BY SPACE            
107300                                                                          
107400     IF MID-KDCATPUB-R-FOM-IN = ALL '+'                                   
107500       MOVE MID-KDCATPUB-R-FOM-UT TO MOD-KDCATPUB-R-FOM-UT                
107600     ELSE                                                                 
107700       MOVE MID-KDCATPUB-R-FOM-IN TO MOD-KDCATPUB-R-FOM-UT                
107800     END-IF                                                               
107900                                                                          
108000     MOVE WS-IDCATPOS-SOEK-TEST TO MOD-IDCATPOS-SOEK-UPD                  
108100                                                                          
108200     IF MFS-IDTRANS NOT = '1512'                                          
108300       PERFORM AA-HOPPNYCKLAR-BREDA-BILDER                                
108400     END-IF                                                               
108500                                                                          
108600     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
108700                             MOD-IDCATGRP-IN                              
108800                             MOD-IDCATAVS-IN                              
108900                             MOD-IDSKYLT-IN                               
109000                             MOD-IDCATRAD-IN                              
109100                             MOD-KDCATPUB-R-FOM-IN                        
109200                             MOD-KDCATPUB-R-MIN-IN                        
109300                             MOD-KDCATPUB-R-MAX-IN                        
109400                             MOD-TEMFSFEL                                 
109500                             MOD-TEMFSINF                                 
109600                                                                          
109700     MOVE +1 TO INDX                                                      
109800                                                                          
109900     SET WWLAND03-IX TO +1                                                
110000     SEARCH WWLAND03-IDSKYLT-RAD                                          
110100            AT END MOVE NEJ TO SPRAAK-KOLL                                
110200         WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = KEY-IDSKYLT                 
110300            MOVE JA TO SPRAAK-KOLL                                        
110400     END-SEARCH                                                           
110500     .                                                                    
110600     EJECT                                                                
110700 AA-HOPPNYCKLAR-BREDA-BILDER  SECTION.                                    
110800     SKIP2                                                                
110900     MOVE SPACE  TO MOD-KDCATPUB-R-MIN-UT                                 
111000                    MOD-KDCATPUB-R-MAX-UT                                 
111100     IF MFS-IDTRANS = '1511'                                              
111200       MOVE MID-W1I51201 TO 1511-MID-W1I51101                             
111300       IF 1511-MID-KDCATPUB-R-MIN = ALL '+'                               
111400         MOVE 1511-MID-KDCATPUB-R-MIN TO MOD-KDCATPUB-R-MIN-UT            
111500       ELSE                                                               
111600         MOVE 1511-MID-KDCATPUB-R-MIN TO MOD-KDCATPUB-R-MIN-UT            
111700       END-IF                                                             
111800       IF 1511-MID-KDCATPUB-R-MAX = ALL '+'                               
111900         MOVE 1511-MID-KDCATPUB-R-MAX TO MOD-KDCATPUB-R-MAX-UT            
112000       ELSE                                                               
112100         MOVE 1511-MID-KDCATPUB-R-MAX TO MOD-KDCATPUB-R-MAX-UT            
112200       END-IF                                                             
112300     END-IF                                                               
112400     IF MFS-IDTRANS = '1513'                                              
112500       MOVE MID-W1I51201 TO 1513-MID-W1I51301                             
112600       IF 1513-MID-KDCATPUB-R-MIN = ALL '+'                               
112700         MOVE 1513-MID-KDCATPUB-R-MIN TO MOD-KDCATPUB-R-MIN-UT            
112800       ELSE                                                               
112900         MOVE 1513-MID-KDCATPUB-R-MIN TO MOD-KDCATPUB-R-MIN-UT            
113000       END-IF                                                             
113100       IF 1513-MID-KDCATPUB-R-MAX = ALL '+'                               
113200         MOVE 1513-MID-KDCATPUB-R-MAX TO MOD-KDCATPUB-R-MAX-UT            
113300       ELSE                                                               
113400         MOVE 1513-MID-KDCATPUB-R-MAX TO MOD-KDCATPUB-R-MAX-UT            
113500       END-IF                                                             
113600     END-IF                                                               
113700     IF  MFS-IDTRANS = '1514'                                             
113800       MOVE MID-W1I51201 TO 1514-MID-W1I51401                             
113900       MOVE  1514-MID-KDCATPUB-R-UT TO MOD-KDCATPUB-R-MIN-UT              
114000     END-IF                                                               
114100     IF  MFS-IDTRANS = '1515'                                             
114200       MOVE MID-W1I51201 TO 1515-MID-W1I51501                             
114300       MOVE  1515-MID-KDCATPUB-R-FOM-UT TO MOD-KDCATPUB-R-MIN-UT          
114400     END-IF                                                               
114500     IF MFS-IDTRANS = '1519'                                              
114600       MOVE MID-W1I51201 TO 1519-MID-W1I51901                             
114700       IF 1519-MID-KDCATPUB-R-FOM-F-IN = ALL '+'                          
114800       MOVE 1519-MID-KDCATPUB-R-FOM-F-UT TO MOD-KDCATPUB-R-MIN-UT         
114900       ELSE                                                               
115000       MOVE 1519-MID-KDCATPUB-R-FOM-F-IN TO MOD-KDCATPUB-R-MIN-UT         
115100       END-IF                                                             
115200       IF 1519-MID-KDCATPUB-R-FOM-T-IN = ALL '+'                          
115300       MOVE 1519-MID-KDCATPUB-R-FOM-T-UT TO MOD-KDCATPUB-R-MAX-UT         
115400       ELSE                                                               
115500       MOVE 1519-MID-KDCATPUB-R-FOM-T-IN TO MOD-KDCATPUB-R-MAX-UT         
115600       END-IF                                                             
115700     END-IF                                                               
115800                                                                          
115900     MOVE MOD-KDCATPUB-R-MIN-UT     TO WS-KDCATPUB-R-AVV                  
116000     PERFORM S50-Y2K-KDCATPUB-R                                           
116100     MOVE WS-KDCATPUB-AAAAVV     TO KEY-KDCATPUB-MIN-X                    
116200                                                                          
116300     INSPECT KEY-KDCATPUB-MIN-X  REPLACING                                
116400                                 LEADING SPACE BY LOW-VALUE               
116500                                                                          
116600     MOVE MOD-KDCATPUB-R-MAX-UT  TO WS-KDCATPUB-R-AVV                     
116700     PERFORM S50-Y2K-KDCATPUB-R                                           
116800     MOVE WS-KDCATPUB-AAAAVV     TO KEY-KDCATPUB-MAX-X                    
116900                                                                          
117000     INSPECT KEY-KDCATPUB-MAX-X  REPLACING                                
117100                                 LEADING SPACE BY HIGH-VALUE              
117200     .                                                                    
117300     EJECT                                                                
117400 B-KOLLA-INDATA SECTION.                                                  
117500     SKIP2                                                                
117600     PERFORM BB-TRANSFORM-TO-VERSAL                                       
117700     PERFORM BC-HAEMTA-SENASTE-VADIS-GEN                                  
117800     MOVE NEJ TO INDATA-FEL                                               
117900     IF MID-IDCATRAD-UPD NOT = ALL '+'                                    
118000        IF MID-IDCATRAD-UPD NUMERIC                                       
118100           IF MID-IDCATRAD-UPD < 0020                                     
118200           OR MID-IDCATRAD-UPD > 7999                                     
118300*             --- Rader från 8000 9998 används av 1551 för omnum.         
118400              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR             
118500              MOVE JA TO INDATA-FEL                                       
118600           ELSE                                                           
118700              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-UPD-ATTR           
118800              MOVE MID-IDCATRAD-UPD TO WS-IDCATRAD                        
118900           END-IF                                                         
119000        ELSE                                                              
119100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR                
119200           MOVE JA TO INDATA-FEL                                          
119300        END-IF                                                            
119400     ELSE                                                                 
119500*      obligatoriskt vid uppdat                                           
119600        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR                   
119700        MOVE JA TO INDATA-FEL                                             
119800     END-IF                                                               
119900     IF INDATA-FEL = NEJ                                                  
120000* MID-KDCATPUB-R-FOM-UPD                                                  
120100     IF MID-KDCATPUB-R-FOM-UPD NOT = ALL '+'                              
120200       IF MID-KDCATPUB-R-FOM-UPD = SPACE                                  
120300         MOVE LOW-VALUE TO WS-KDCATPUB-FOM-UPD                            
120400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-FOM-UPD-ATTR         
120500       ELSE                                                               
120600         IF MID-KDCATPUB-R-FOM-UPD NUMERIC                                
120700           MOVE MID-KDCATPUB-R-FOM-UPD TO WS-KDCATPUB-R-AVV               
120800           PERFORM S50-Y2K-KDCATPUB-R                                     
120900           MOVE WS-KDCATPUB-AAAAVV TO WS-KDCATPUB-FOM-UPD                 
121000                                                                          
121100           INSPECT WS-KDCATPUB-FOM-UPD REPLACING                          
121200                                       LEADING SPACE BY ZERO              
121300                                                                          
121400           IF MID-IDCATRAD-BORT-UPD NOT = ALL '+'                         
121500*            borttag av rad med ej godk pubkod måste kunna göras          
121600             MOVE MFS-ALFA-FAELT-RAETT TO                                 
121700                                      MOD-KDCATPUB-R-FOM-UPD-ATTR         
121800           ELSE                                                           
121900             MOVE WS-KDCATPUB-FOM-UPD TO TEST-KDCATPUB-FOM                
122000             MOVE WS-KDCATPUB-FOM-UPD(1:4) TO W-TIAAAA                    
122100                                                                          
122200             PERFORM S02F-KDCATPUB-FOM-TABELL                             
122300             IF INDATA-FEL = NEJ                                          
122400               MOVE MFS-ALFA-FAELT-RAETT TO                               
122500                                      MOD-KDCATPUB-R-FOM-UPD-ATTR         
122600             ELSE                                                         
122700               MOVE MFS-ALFA-FAELT-FEL  TO                                
122800                                       MOD-KDCATPUB-R-FOM-UPD-ATTR        
122900*              --- Skickar ut radnumret igen                              
123000               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-UPD-ATTR          
123100               MOVE MID-IDCATRAD-UPD    TO MOD-IDCATRAD-UPD               
123200             END-IF                                                       
123300           END-IF                                                         
123400         ELSE                                                             
123500           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCATPUB-R-FOM-UPD-ATTR        
123600           MOVE JA TO INDATA-FEL                                          
123700*          --- Skickar ut radnumret igen                                  
123800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-UPD-ATTR              
123900           MOVE MID-IDCATRAD-UPD    TO MOD-IDCATRAD-UPD                   
124000         END-IF                                                           
124100       END-IF                                                             
124200     ELSE                                                                 
124300*      obligatoriskt vid uppdat                                           
124400        MOVE LOW-VALUE TO WS-KDCATPUB-FOM-UPD                             
124500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-FOM-UPD-ATTR          
124600     END-IF                                                               
124700     ELSE                                                                 
124800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-FOM-UPD-ATTR          
124900     END-IF                                                               
125000                                                                          
125100* MID-KDCATPUB-R-TOM-UPD                                                  
125200     IF INDATA-FEL = JA                                                   
125300       CONTINUE                                                           
125400     ELSE                                                                 
125500       IF ( MID-KDCATPUB-R-TOM-UPD NOT = ALL '+' )                        
125600       AND ( MID-IDCATRAD-BORT-UPD = ALL '+' )                            
125700         IF  MID-KDCATPUB-R-TOM-UPD = SPACE                               
125800           MOVE HIGH-VALUE TO WS-KDCATPUB-TOM-UPD                         
125900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-TOM-UPD-ATTR         
126000         ELSE                                                             
126100           IF MID-KDCATPUB-R-TOM-UPD NUMERIC                              
126200             MOVE MID-KDCATPUB-R-TOM-UPD TO WS-KDCATPUB-R-AVV             
126300             PERFORM S50-Y2K-KDCATPUB-R                                   
126400             MOVE WS-KDCATPUB-AAAAVV  TO TEST-KDCATPUB-TOM                
126500             INSPECT WS-KDCATPUB-AAAAVV  REPLACING                        
126600                                         LEADING SPACE BY ZERO            
126700             MOVE TEST-KDCATPUB-TOM(1:4) TO W-TIAAAA                      
126800                                                                          
126900             PERFORM S02T-KDCATPUB-TOM-TABELL                             
127000             IF INDATA-FEL = JA                                           
127100               IF W-TIAAAA > ZERO                                         
127200                 SUBTRACT 1 FROM W-TIAAAA                                 
127300               END-IF                                                     
127400*              kolla ifall koden finns i föregående årstabell             
127500               PERFORM S02T-KDCATPUB-TOM-TABELL                           
127600             END-IF                                                       
127700             IF INDATA-FEL = JA                                           
127800               MOVE MFS-ALFA-FAELT-FEL                                    
127900                                  TO MOD-KDCATPUB-R-TOM-UPD-ATTR          
128000               MOVE ' SAKN I KATM-TAB' TO WS-FELTEXT                      
128100             ELSE                                                         
128200               MOVE TEST-KDCATPUB-TOM TO WS-KDCATPUB-TOM-UPD              
128300               MOVE MFS-ALFA-FAELT-RAETT                                  
128400                                  TO MOD-KDCATPUB-R-TOM-UPD-ATTR          
128500             END-IF                                                       
128600           ELSE                                                           
128700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-UPD-ATTR         
128800             MOVE JA TO INDATA-FEL                                        
128900           END-IF                                                         
129000         END-IF                                                           
129100       ELSE                                                               
129200         IF MID-KDCATPUB-R-TOM-UPD = ALL '+'                              
129300           MOVE HIGH-VALUE TO WS-KDCATPUB-TOM-UPD                         
129400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-TOM-UPD-ATTR         
129500         END-IF                                                           
129600       END-IF                                                             
129700*   kontrollera att RADENS PUBKODER ligger inom gräns för visning         
129800       IF WS-KDCATPUB-FOM-UPD < KEY-KDCATPUB-MIN-X                        
129900         IF WS-KDCATPUB-TOM-UPD < KEY-KDCATPUB-MIN-X                      
130000*           man vill uppdatera en rad som ligger tidigare än              
130100*           min pub-gräns för visning                                     
130200            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-UPD-ATTR        
130300            MOVE JA TO INDATA-FEL                                         
130400         END-IF                                                           
130500       ELSE                                                               
130600         IF WS-KDCATPUB-FOM-UPD > KEY-KDCATPUB-MIN-X                      
130700           IF WS-KDCATPUB-FOM-UPD > KEY-KDCATPUB-MAX-X                    
130800*            man vill uppdatera en rad som ligger senare än               
130900*            min pub-gräns för visning                                    
131000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FOM-UPD-ATTR         
131100             MOVE JA TO INDATA-FEL                                        
131200             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-UPD-ATTR            
131300           END-IF                                                         
131400         END-IF                                                           
131500       END-IF                                                             
131600*      Kontrollera att inte PFR och PTOM är rätt inbördes                 
131700*      Behöver bara kontrolleras om det inte är borttag                   
131800       IF MID-IDCATRAD-BORT-UPD = ALL '+'                                 
131900         IF WS-KDCATPUB-FOM-UPD <= WS-KDCATPUB-TOM-UPD                    
132000            CONTINUE                                                      
132100         ELSE                                                             
132200            MOVE JA TO INDATA-FEL                                         
132300            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FOM-UPD-ATTR        
132400                                       MOD-KDCATPUB-R-TOM-UPD-ATTR        
132500            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-UPD-ATTR             
132600         END-IF                                                           
132700       ELSE                                                               
132800         IF MID-IDCATRAD-BORT-UPD NUMERIC                                 
132900            IF INDATA-FEL = NEJ                                           
133000               IF MID-IDCATRAD-BORT-UPD >= MID-IDCATRAD-UPD               
133100                  MOVE 'B' TO UPPDATE-FL                                  
133200                  MOVE MFS-NUM-FAELT-RAETT TO                             
133300                                    MOD-IDCATRAD-BORT-UPD-ATTR            
133400               ELSE                                                       
133500                  MOVE MFS-NUM-FAELT-FEL TO                               
133600                                    MOD-IDCATRAD-BORT-UPD-ATTR            
133700                  MOVE JA TO INDATA-FEL                                   
133800               END-IF                                                     
133900            END-IF                                                        
134000         ELSE                                                             
134100            MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-BORT-UPD-ATTR          
134200            MOVE JA TO INDATA-FEL                                         
134300         END-IF                                                           
134400*   MID-KDCATPUB-R-BORT                                                   
134500*   i första releasen får inte PUB-BORT väljas annat än PUB-UPD           
134600         MOVE WS-KDCATPUB-FOM-UPD TO WS-KDCATPUB-BORT-UPD                 
134700       END-IF                                                             
134800*   den sista end-if:en avslutar KDCATPUB-R-fom fel-status kollen         
134900*     i början av denn KDCATPUB-R-tom koll                                
135000     END-IF                                                               
135100*                                                                         
135200     IF MID-IDCATRAD-BORT-UPD = ALL '+'                                   
135300       PERFORM BA-KOLLA-GEM-DATA                                          
135400       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-BORT-UPD-ATTR             
135500*                                  MOD-KDCATPUB-R-BORT-UPD-ATTR           
135600     END-IF                                                               
135700     .                                                                    
135800     EJECT                                                                
135900 BA-KOLLA-GEM-DATA SECTION.                                               
136000     SKIP2                                                                
136100     MOVE JA  TO IDCATPOS-OK                                              
136200     MOVE NEJ TO KDHAEN-BLANK                                             
136300     MOVE NEJ TO KVKOL-FINNS                                              
136400     MOVE NEJ TO BEART-FINNS                                              
136500     MOVE NEJ TO IDRUBNR-FINNS                                            
136600     MOVE NEJ TO IDFOTNR-FINNS                                            
136700     MOVE NEJ TO HAEN-FINNS                                               
136800                                                                          
136900                                                                          
137000     IF MID-KDFBX-UPD NOT = ALL '+'                                       
137100       IF MID-KDFBX-UPD = 'F' OR 'B' OR 'X' OR ' ' OR 'H'                 
137200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFBX-UPD-ATTR                  
137300       ELSE                                                               
137400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFBX-UPD-ATTR                    
137500         MOVE JA TO INDATA-FEL                                            
137600       END-IF                                                             
137700     END-IF                                                               
137800                                                                          
137900     IF MID-IDCATPOS-UPD NOT = ALL '+'                                    
138000        IF MID-IDCATPOS-UPD = SPACE                                       
138100           IF MID-KDFBX-UPD NOT = 'F'                                     
138200             IF KEY-IDCATAVS > 1                                          
138300               MOVE NEJ TO IDCATPOS-OK                                    
138400               MOVE ' POS ÄR BLANK ' TO WS-FELTEXT                        
138500*              Idcatpos får inte vara tom på en huvudrad                  
138600*              såvida det inte är på ett avsnitt 1.                       
138700             ELSE                                                         
138800               CONTINUE                                                   
138900             END-IF                                                       
139000           END-IF                                                         
139100        ELSE                                                              
139200           IF MID-KDFBX-UPD = 'F'                                         
139300             MOVE SPACE TO W-IDCATPOS                                     
139400*              idcatpos skall vara tom på fortsättningsrad                
139500           ELSE                                                           
139600             MOVE MID-IDCATPOS-UPD TO W-IDCATPOS                          
139700             IF W-IDCATPOS-1 = ZERO                                       
139800             OR W-IDCATPOS-1 ALPHABETIC                                   
139900*              får ej börja med NOLL,                                     
140000*              ej heller med ALFABETISKA tecken (SPACE=alfabetic)         
140100               MOVE ' POS INLEDN.FEL ' TO WS-FELTEXT                      
140200               MOVE NEJ TO IDCATPOS-OK                                    
140300             ELSE                                                         
140400                IF  W-IDCATPOS-2 ALPHABETIC                               
140500                AND W-IDCATPOS-2 NOT = SPACE                              
140600                AND W-IDCATPOS-3 ALPHABETIC                               
140700                AND W-IDCATPOS-3 NOT = SPACE                              
140800*                 dubbla alfa är ej tillåtet                              
140900                  MOVE NEJ TO IDCATPOS-OK                                 
141000                  STRING  ' DUBBLA ALFA='  W-IDCATPOS                     
141100                  DELIMITED BY SIZE INTO WS-FELTEXT                       
141200                END-IF                                                    
141300                IF  W-IDCATPOS-2 ALPHABETIC                               
141400                AND W-IDCATPOS-3 NUMERIC                                  
141500*                 blandat är ej tillåtet                                  
141600                  MOVE NEJ TO IDCATPOS-OK                                 
141700                  STRING  ' blandade tkn'  W-IDCATPOS                     
141800                  DELIMITED BY SIZE INTO WS-FELTEXT                       
141900                END-IF                                                    
142000                                                                          
142100                IF IDCATPOS-OK = JA                                       
142200*                 alfa måste vara gemena tecken                           
142300                  IF W-IDCATPOS-2 ALPHABETIC                              
142400                    MOVE FUNCTION LOWER-CASE(W-IDCATPOS-2)                
142500                                         TO  W-IDCATPOS-2                 
142600                  END-IF                                                  
142700                  IF W-IDCATPOS-3 ALPHABETIC                              
142800                    MOVE FUNCTION LOWER-CASE(W-IDCATPOS-3)                
142900                                         TO  W-IDCATPOS-3                 
143000                  END-IF                                                  
143100                END-IF                                                    
143200             END-IF                                                       
143300           END-IF                                                         
143400        END-IF                                                            
143500     END-IF                                                               
143600                                                                          
143700     IF IDCATPOS-OK = NEJ                                                 
143800        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-UPD-ATTR                  
143900        MOVE JA TO INDATA-FEL                                             
144000     ELSE                                                                 
144100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATPOS-UPD-ATTR                
144200     END-IF                                                               
144300                                                                          
144400     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
144500        INSPECT MID-IDARTNR-UPD REPLACING LEADING SPACE BY ZERO           
144600        IF MID-IDARTNR-UPD NUMERIC                                        
144700           MOVE MID-IDARTNR-UPD TO BYTES-IDARTNR                          
144800           IF BYT03-OBJEKT                                                
144900*- - - - - - - - - - - bytes-objekt får ej förekomma i katalogen          
145000              MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR              
145100              MOVE JA TO INDATA-FEL                                       
145200           ELSE                                                           
145300              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-UPD-ATTR            
145400           END-IF                                                         
145500        ELSE                                                              
145600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                 
145700           MOVE JA TO INDATA-FEL                                          
145800        END-IF                                                            
145900     END-IF                                                               
146000                                                                          
146100     MOVE +1 TO INDX                                                      
146200     PERFORM UNTIL INDX = +6                                              
146300        IF MID-KVKOL-UPD(INDX) NOT = ALL '+'                              
146400           MOVE JA TO KVKOL-FINNS                                         
146500        END-IF                                                            
146600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOL-UPD-ATTR(INDX)             
146700        ADD +1 TO INDX                                                    
146800     END-PERFORM                                                          
146900                                                                          
147000     IF MID-KDPS-UPD NOT = ALL '+'                                        
147100        IF MID-KDPS-UPD = '  ' OR 'LS' OR 'KL' OR 'XX' OR 'IK' OR         
147200                      'KS' OR 'NS' OR 'KN' OR 'SW'                        
147300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPS-UPD-ATTR                 
147400        ELSE                                                              
147500           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPS-UPD-ATTR                 
147600           MOVE JA TO INDATA-FEL                                          
147700        END-IF                                                            
147800     END-IF                                                               
147900                                                                          
148000     IF MID-KVPUNKT-UPD NOT = ALL '+'                                     
148100        IF MID-KVPUNKT-UPD NUMERIC                                        
148200           IF MID-KVPUNKT-UPD = ZERO OR > 0 AND < 5                       
148300              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPUNKT-UPD-ATTR            
148400           ELSE                                                           
148500              MOVE MFS-NUM-FAELT-FEL TO MOD-KVPUNKT-UPD-ATTR              
148600              MOVE JA TO INDATA-FEL                                       
148700           END-IF                                                         
148800        ELSE                                                              
148900           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPUNKT-UPD-ATTR                 
149000           MOVE JA TO INDATA-FEL                                          
149100        END-IF                                                            
149200     END-IF                                                               
149300                                                                          
149400     IF MID-BEART-UPD NOT = ALL '+'                                       
149500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-UPD-ATTR                   
149600        MOVE JA TO BEART-FINNS                                            
149700     END-IF                                                               
149800                                                                          
149900     IF MID-KDHOM-UPD NOT = ALL '+'                                       
150000        IF MID-KDHOM-UPD NUMERIC                                          
150100           IF MID-KDHOM-UPD < 9                                           
150200              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHOM-UPD-ATTR              
150300           ELSE                                                           
150400              MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR                
150500              MOVE JA TO INDATA-FEL                                       
150600           END-IF                                                         
150700        ELSE                                                              
150800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR                   
150900           MOVE JA TO INDATA-FEL                                          
151000        END-IF                                                            
151100     END-IF                                                               
151200                                                                          
151300     IF MID-IDTTEXNR-UPD NOT = ALL '+'                                    
151400        IF MID-IDTTEXNR-UPD NUMERIC                                       
151500           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTTEXNR-UPD-ATTR              
151600        ELSE                                                              
151700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDTTEXNR-UPD-ATTR                
151800           MOVE JA TO INDATA-FEL                                          
151900        END-IF                                                            
152000     END-IF                                                               
152100                                                                          
152200     IF MID-TEKATANM-UPD NOT = ALL '+'                                    
152300        MOVE MFS-ALFA-FAELT-RAETT TO  MOD-TEKATANM-UPD-ATTR               
152400     END-IF                                                               
152500                                                                          
152600     MOVE +1 TO INDX                                                      
152700     PERFORM UNTIL INDX = +4                                              
152800        IF MID-IDRUBNR-UPD(INDX) NOT = ALL '+'                            
152900           INSPECT MID-IDRUBNR-UPD(INDX)                                  
153000                   REPLACING LEADING ' ' BY ZERO                          
153100           IF MID-IDRUBNR-UPD(INDX) NUMERIC                               
153200              MOVE MFS-NUM-FAELT-RAETT                                    
153300                                 TO MOD-IDRUBNR-UPD-ATTR(INDX)            
153400              MOVE JA TO IDRUBNR-FINNS                                    
153500              MOVE MID-IDRUBNR-UPD(INDX) TO IN-IDRUBNR(INDX)              
153600           ELSE                                                           
153700              MOVE MFS-NUM-FAELT-FEL                                      
153800                                 TO MOD-IDRUBNR-UPD-ATTR(INDX)            
153900              MOVE JA TO INDATA-FEL                                       
154000           END-IF                                                         
154100        ELSE                                                              
154200           MOVE ZERO TO IN-IDRUBNR(INDX)                                  
154300        END-IF                                                            
154400        ADD +1 TO INDX                                                    
154500     END-PERFORM                                                          
154600                                                                          
154700     MOVE +1 TO INDX                                                      
154800     PERFORM UNTIL INDX = +4                                              
154900        IF MID-KDPS-UPD = 'SW'                                            
155000          IF INDX = +1                                                    
155100*           --- Fast fotnotsnummer för Mjukvaru-artiklar i PIE            
155200*           --- innehåller texten:                                        
155300*           Programvarufunktion som endast kan beställas enligt           
155400*           fastställd rutin.                                             
155500            MOVE 14731 TO MID-IDFOTNR-UPD(INDX)                           
155600          END-IF                                                          
155700        END-IF                                                            
155800        IF MID-IDFOTNR-UPD(INDX) NOT = ALL '+'                            
155900           INSPECT MID-IDFOTNR-UPD(INDX)                                  
156000                   REPLACING LEADING ' ' BY ZERO                          
156100           IF MID-IDFOTNR-UPD(INDX) NUMERIC                               
156200              MOVE MFS-NUM-FAELT-RAETT                                    
156300                                     TO MOD-IDFOTNR-UPD-ATTR(INDX)        
156400              MOVE JA TO IDFOTNR-FINNS                                    
156500              MOVE MID-IDFOTNR-UPD(INDX) TO IN-IDFOTNR(INDX)              
156600           ELSE                                                           
156700              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(INDX)        
156800              MOVE JA TO INDATA-FEL                                       
156900           END-IF                                                         
157000        ELSE                                                              
157100           MOVE ZERO TO IN-IDFOTNR(INDX)                                  
157200        END-IF                                                            
157300        ADD +1 TO INDX                                                    
157400     END-PERFORM                                                          
157500                                                                          
157600     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
157700        IF MID-IDCATGRP-H-UPD NUMERIC                                     
157800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATGRP-H-UPD-ATTR            
157900           MOVE JA TO HAEN-FINNS                                          
158000        ELSE                                                              
158100           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATGRP-H-UPD-ATTR            
158200           MOVE JA TO INDATA-FEL                                          
158300        END-IF                                                            
158400     END-IF                                                               
158500                                                                          
158600     IF MID-IDCATAVS-H-UPD NOT = ALL '+'                                  
158700        IF MID-IDCATAVS-H-UPD NUMERIC                                     
158800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATAVS-H-UPD-ATTR            
158900           MOVE JA TO HAEN-FINNS                                          
159000        ELSE                                                              
159100           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATAVS-H-UPD-ATTR            
159200           MOVE JA TO INDATA-FEL                                          
159300        END-IF                                                            
159400     END-IF                                                               
159500                                                                          
159600     IF MID-IDCATRAD-H-UPD NOT = ALL '+'                                  
159700       IF MID-IDCATRAD-H-UPD NUMERIC                                      
159800          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATRAD-H-UPD-ATTR             
159900          MOVE JA TO HAEN-FINNS                                           
160000       ELSE                                                               
160100          MOVE MFS-NUM-FAELT-FEL    TO MOD-IDCATRAD-H-UPD-ATTR            
160200          MOVE JA TO INDATA-FEL                                           
160300       END-IF                                                             
160400                                                                          
160500*  MID-KDCATPUB-R-H-UPD                                                   
160600       IF INDATA-FEL = NEJ                                                
160700       IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                              
160800         IF MID-KDCATPUB-R-H-UPD = SPACE                                  
160900           MOVE LOW-VALUE TO WS-KDCATPUB-H-UPD                            
161000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-H-UPD-ATTR         
161100           MOVE JA TO HAEN-FINNS                                          
161200         ELSE                                                             
161300           IF MID-KDCATPUB-R-H-UPD NUMERIC                                
161400             MOVE MID-KDCATPUB-R-H-UPD TO WS-KDCATPUB-R-AVV               
161500             PERFORM S50-Y2K-KDCATPUB-R                                   
161600             MOVE WS-KDCATPUB-AAAAVV TO TEST-KDCATPUB-FOM                 
161700             INSPECT TEST-KDCATPUB-FOM REPLACING                          
161800                                       LEADING SPACE BY ZERO              
161900             MOVE TEST-KDCATPUB-FOM(1:4) TO W-TIAAAA                      
162000                                                                          
162100             PERFORM S02F-KDCATPUB-FOM-TABELL                             
162200                                                                          
162300             IF INDATA-FEL = NEJ                                          
162400               MOVE TEST-KDCATPUB-FOM TO WS-KDCATPUB-H-UPD                
162500               MOVE MFS-ALFA-FAELT-RAETT TO                               
162600                                        MOD-KDCATPUB-R-H-UPD-ATTR         
162700               MOVE JA TO HAEN-FINNS                                      
162800             ELSE                                                         
162900               MOVE MFS-ALFA-FAELT-FEL                                    
163000                                     TO MOD-KDCATPUB-R-H-UPD-ATTR         
163100             END-IF                                                       
163200           ELSE                                                           
163300             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-H-UPD-ATTR         
163400             MOVE JA TO INDATA-FEL                                        
163500           END-IF                                                         
163600         END-IF                                                           
163700       ELSE                                                               
163800*        --- MID-KDCATPUB-R-H-UPD = ALL '+'                               
163900         MOVE LOW-VALUE TO WS-KDCATPUB-H-UPD                              
164000       END-IF                                                             
164100       END-IF                                                             
164200                                                                          
164300       IF HAEN-FINNS = JA                                                 
164400*      --- kontroll att hänvisningen är samtida med raden själv           
164500*      --- görs här och nu.                                               
164600*      --- görs OXÅ i resp. uppdateringssektion F- och H-                 
164700         IF WS-KDCATPUB-FOM-UPD NOT = WS-KDCATPUB-H-UPD                   
164800*          --- Hänvisning till rad NOLL skall ALLTID kunna göras          
164900*          --- till BLANK (eg. LOW-VALUE) PUBKOD                          
165000           IF MID-IDCATRAD-H-UPD = ZERO                                   
165100             IF MID-KDCATPUB-R-H-UPD = SPACE OR ALL '+'                   
165200               CONTINUE                                                   
165300             ELSE                                                         
165400               MOVE MFS-ALFA-FAELT-FEL                                    
165500                                   TO MOD-KDCATPUB-R-H-UPD-ATTR           
165600               MOVE JA TO INDATA-FEL                                      
165700             END-IF                                                       
165800           ELSE                                                           
165900             MOVE JA TO INDATA-FEL                                        
166000             IF (MID-IDCATRAD-H-UPD = 10 OR 11 OR 12 OR 13 OR 14 )        
166100             OR  MID-IDCATRAD-H-UPD > 19                                  
166200               MOVE MFS-ALFA-FAELT-FEL                                    
166300                                   TO MOD-KDCATPUB-R-H-UPD-ATTR           
166400*                                     MOD-KDCATPUB-R-FOM-UPD-ATTR         
166500             ELSE                                                         
166600               MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-H-UPD-ATTR          
166700             END-IF                                                       
166800           END-IF                                                         
166900         ELSE                                                             
167000           IF  MID-IDCATRAD-H-UPD = ZERO                                  
167100           OR (MID-IDCATRAD-H-UPD = 10 OR 11 OR 12 OR 13 OR 14 )          
167200           OR  MID-IDCATRAD-H-UPD > 19                                    
167300             CONTINUE                                                     
167400           ELSE                                                           
167500             MOVE JA TO INDATA-FEL                                        
167600             MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-H-UPD-ATTR            
167700           END-IF                                                         
167800         END-IF                                                           
167900       END-IF                                                             
168000     END-IF                                                               
168100*                                                                         
168200     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
168300        IF MID-KDHAEN-UPD NOT NUMERIC                                     
168400           IF MID-KDHAEN-UPD = ' ' OR 'A' OR 'B' OR '*'                   
168500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDHAEN-UPD-ATTR            
168600              MOVE JA TO HAEN-FINNS                                       
168700              IF MID-KDHAEN-UPD = ' '                                     
168800                                                                          
168900                 IF MID-IDCATGRP-H-UPD NOT = ALL '+'                      
169000                    MOVE MFS-NUM-FAELT-FEL TO                             
169100                         MOD-IDCATGRP-H-UPD-ATTR                          
169200                    MOVE JA TO INDATA-FEL                                 
169300                 END-IF                                                   
169400                                                                          
169500                 IF MID-IDCATAVS-H-UPD NOT = ALL '+'                      
169600                    MOVE MFS-NUM-FAELT-FEL TO                             
169700                         MOD-IDCATAVS-H-UPD-ATTR                          
169800                    MOVE JA TO INDATA-FEL                                 
169900                 END-IF                                                   
170000                                                                          
170100                 IF MID-IDCATRAD-H-UPD NOT = ALL '+'                      
170200                    MOVE MFS-NUM-FAELT-FEL TO                             
170300                         MOD-IDCATRAD-H-UPD-ATTR                          
170400                    MOVE JA TO INDATA-FEL                                 
170500                 END-IF                                                   
170600                                                                          
170700                 IF MID-IDCATRAD-H-UPD NOT = ALL '+'                      
170800                    MOVE MFS-NUM-FAELT-FEL TO                             
170900                         MOD-IDCATRAD-H-UPD-ATTR                          
171000                    MOVE JA TO INDATA-FEL                                 
171100                 END-IF                                                   
171200                 IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                    
171300                    MOVE MFS-ALFA-FAELT-FEL TO                            
171400                         MOD-KDCATPUB-R-H-UPD-ATTR                        
171500                    MOVE JA TO INDATA-FEL                                 
171600                 END-IF                                                   
171700                 MOVE JA TO KDHAEN-BLANK                                  
171800              END-IF                                                      
171900           ELSE                                                           
172000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR              
172100              MOVE JA TO INDATA-FEL                                       
172200           END-IF                                                         
172300        ELSE                                                              
172400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                 
172500           MOVE JA TO INDATA-FEL                                          
172600        END-IF                                                            
172700     END-IF                                                               
172800     .                                                                    
172900     EJECT                                                                
173000 BB-TRANSFORM-TO-VERSAL SECTION.                                          
173100     SKIP2                                                                
173200     MOVE FUNCTION UPPER-CASE(MID-KDFBX-UPD)  TO MID-KDFBX-UPD            
173300     MOVE FUNCTION UPPER-CASE(MID-KVKOL-GRP)  TO MID-KVKOL-GRP            
173400     MOVE FUNCTION UPPER-CASE(MID-KDHAEN-UPD) TO MID-KDHAEN-UPD           
173500     MOVE FUNCTION UPPER-CASE(MID-KDPS-UPD)   TO MID-KDPS-UPD             
173600                                                                          
173700*** nedanst call på wtxttr görs p.g.av att den klarar åäö                 
173800     MOVE 'V'                 TO  RTXT-KDTEXTTR                           
173900     MOVE MID-BEART-UPD       TO  RTXT-TETEXTTR                           
174000     CALL WTXTTR  USING RTXT-WTXTAREA                                     
174100     MOVE RTXT-TETEXTTR       TO  MID-BEART-UPD                           
174200     .                                                                    
174300     EJECT                                                                
174400 BC-HAEMTA-SENASTE-VADIS-GEN  SECTION.                                    
174500     SKIP2                                                                
174600     PERFORM IMS-GET-KATM-KAT                                             
174700*    -- Om det finns nya fält på WDN101 som beskriver                     
174800*    -- senaste VADIS-genererings-datum, (KAT-TIVADGEN-SEN)               
174900*    -- senaste VADIS-genererings-pubkod,(KAT-KDCATPUB-SEN-VADGEN)        
175000                                                                          
175100**** MOVE KAT-KAT-KDCATPUB-SEN-VADGEN TO WS-KDCATPUB-SEN-GEN              
175200                                                                          
175300*    -- ANNARS                                                            
175400*    --- Läser tabell-segmenten på WDN111                                 
175500*    --- och sparar PUB-FOM för den senaste VADIS-genereringen.           
175600     IF SEGMENT-FINNS                                                     
175700       MOVE +1 TO AAR-IX                                                  
175800       PERFORM UNTIL AAR-IX > +3                                          
175900         PERFORM IMS-GET-NEXT-KATM-TAB                                    
176000         IF SEGMENT-FINNS                                                 
176100           MOVE +1 TO PER-IX                                              
176200           PERFORM UNTIL PER-IX > +12                                     
176300             IF KAT-TAB-TIVADGEN-UPPD(PER-IX) > ZERO                      
176400               MOVE KAT-TAB-KDCATPUB-FOM(PER-IX)                          
176500                                          TO WS-KDCATPUB-SEN-GEN          
176600             END-IF                                                       
176700             ADD +1 TO PER-IX                                             
176800           END-PERFORM                                                    
176900         END-IF                                                           
177000         ADD +1 TO AAR-IX                                                 
177100       END-PERFORM                                                        
177200     END-IF                                                               
177300*    -- Egentligen behövs bara läsning av de två första segmenten,        
177400*    -- men om inte årskörningen hunnit lägga upp nytt segment            
177500*    -- kollas det också.                                                 
177600     .                                                                    
177700     EJECT                                                                
177800 C-KOLLA-BORTTAG-DATA SECTION.                                            
177900     SKIP2                                                                
178000     IF MID-KDFBX-UPD NOT = ALL '+'                                       
178100        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDFBX-UPD-ATTR                     
178200        MOVE JA TO INDATA-FEL                                             
178300     END-IF                                                               
178400                                                                          
178500     IF MID-IDCATPOS-UPD NOT = ALL '+'                                    
178600        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-UPD-ATTR                  
178700        MOVE JA TO INDATA-FEL                                             
178800     END-IF                                                               
178900                                                                          
179000     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
179100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                    
179200        MOVE JA TO INDATA-FEL                                             
179300     END-IF                                                               
179400                                                                          
179500     MOVE +1 TO INDX                                                      
179600     PERFORM UNTIL INDX = +6                                              
179700        IF MID-KVKOL-UPD(INDX) NOT = ALL '+'                              
179800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVKOL-UPD-ATTR(INDX)            
179900           MOVE JA TO INDATA-FEL                                          
180000        END-IF                                                            
180100        ADD +1 TO INDX                                                    
180200     END-PERFORM                                                          
180300                                                                          
180400     IF MID-KDPS-UPD NOT = ALL '+'                                        
180500        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR                      
180600        MOVE JA TO INDATA-FEL                                             
180700     END-IF                                                               
180800                                                                          
180900     IF MID-KVPUNKT-UPD NOT = ALL '+'                                     
181000        MOVE MFS-NUM-FAELT-FEL TO MOD-KVPUNKT-UPD-ATTR                    
181100        MOVE JA TO INDATA-FEL                                             
181200     END-IF                                                               
181300                                                                          
181400     IF MID-BEART-UPD NOT = ALL '+'                                       
181500        MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR                     
181600        MOVE JA TO INDATA-FEL                                             
181700     END-IF                                                               
181800                                                                          
181900     IF MID-KDHOM-UPD NOT = ALL '+'                                       
182000        MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR                      
182100        MOVE JA TO INDATA-FEL                                             
182200     END-IF                                                               
182300                                                                          
182400     IF MID-IDTTEXNR-UPD NOT = ALL '+'                                    
182500        MOVE MFS-NUM-FAELT-FEL TO MOD-IDTTEXNR-UPD-ATTR                   
182600        MOVE JA TO INDATA-FEL                                             
182700     END-IF                                                               
182800                                                                          
182900     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
183000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATGRP-H-UPD-ATTR               
183100        MOVE JA TO INDATA-FEL                                             
183200     END-IF                                                               
183300                                                                          
183400     IF MID-IDCATAVS-H-UPD NOT = ALL '+'                                  
183500        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATAVS-H-UPD-ATTR               
183600        MOVE JA TO INDATA-FEL                                             
183700     END-IF                                                               
183800                                                                          
183900     IF MID-IDCATRAD-H-UPD NOT = ALL '+'                                  
184000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDCATRAD-H-UPD-ATTR               
184100        MOVE JA TO INDATA-FEL                                             
184200     END-IF                                                               
184300                                                                          
184400     IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                                
184500        MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCATPUB-R-H-UPD-ATTR            
184600        MOVE JA TO INDATA-FEL                                             
184700     END-IF                                                               
184800                                                                          
184900     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
185000        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                    
185100        MOVE JA TO INDATA-FEL                                             
185200     END-IF                                                               
185300                                                                          
185400     MOVE +1 TO INDX                                                      
185500     PERFORM UNTIL INDX = +4                                              
185600        IF MID-IDRUBNR-UPD(INDX) NOT = ALL '+'                            
185700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(INDX)           
185800           MOVE JA TO INDATA-FEL                                          
185900        END-IF                                                            
186000        ADD +1 TO INDX                                                    
186100     END-PERFORM                                                          
186200                                                                          
186300     MOVE +1 TO INDX                                                      
186400     PERFORM UNTIL INDX = +4                                              
186500        IF MID-IDFOTNR-UPD(INDX) NOT = ALL '+'                            
186600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(INDX)           
186700           MOVE JA TO INDATA-FEL                                          
186800        END-IF                                                            
186900        ADD +1 TO INDX                                                    
187000     END-PERFORM                                                          
187100                                                                          
187200     IF MID-IDCATRAD-UPD < MID-IDCATRAD-FROM                              
187300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR                   
187400        MOVE JA TO INDATA-FEL                                             
187500     ELSE                                                                 
187600        IF MID-IDCATRAD-UPD > MID-IDCATRAD-TOM                            
187700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR                
187800           MOVE JA TO INDATA-FEL                                          
187900        END-IF                                                            
188000     END-IF                                                               
188100                                                                          
188200     IF INDATA-FEL = NEJ                                                  
188300        MOVE MID-IDCATRAD-BORT-UPD TO W-IDCATRAD-MAX                      
188400                                                                          
188500        MOVE WS-KDCATPUB-BORT-UPD  TO W-KDCATPUB-MAX-X                    
188600*       I första "releasen" får bara EN katpub anges vid borttag          
188700                                                                          
188800        IF W-IDCATRAD-MAX = 9999                                          
188900           CONTINUE                                                       
189000        ELSE                                                              
189100           PERFORM IMS-GU-AVS-RAD-MAX                                     
189200                                                                          
189300           IF SEGMENT-SAKNAS                                              
189400              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-BORT-UPD-ATTR        
189500              MOVE JA TO INDATA-FEL                                       
189600           END-IF                                                         
189700        END-IF                                                            
189800     END-IF                                                               
189900     .                                                                    
190000     EJECT                                                                
190100 D-BORTTAG SECTION.                                                       
190200     SKIP2                                                                
190300     PERFORM IMS-GHU-AVS                                                  
190400     IF SEGMENT-FINNS                                                     
190500        MOVE AVS-IDCATNR  TO W-IDCATNR-GSEQ                               
190600        MOVE AVS-IDCATGRP TO W-IDCATGRP-GSEQ                              
190700        MOVE AVS-IDCATAVS TO W-IDCATAVS-GSEQ                              
190800                                                                          
190900        PERFORM IMS-GHNP-AVS-RAD-BORT                                     
191000        PERFORM UNTIL SEGMENT-SAKNAS                                      
191100           MOVE W-WDN512KY-X     TO SPAR-WDN512KY-X                       
191200           MOVE RAD-IDCATRAD     TO W-IDCATRAD                            
191300                                    W-IDCATRAD-GSEQ                       
191400           MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-X                          
191500                                    W-KDCATPUB-GSEQ                       
191600                                    WS-KDCATPUB-FOM-BORTRAD               
191700           MOVE RAD-KDCATPUB-TOM TO WS-KDCATPUB-T                         
191800                                    WS-KDCATPUB-TOM-BORTRAD               
191900           PERFORM DD-BEHANDLA-AVS-REF                                    
192000           PERFORM IMS-GHNP-AVS-RAD-BORT                                  
192100           MOVE SPAR-WDN512KY-X TO W-WDN512KY-X                           
192200        END-PERFORM                                                       
192300*- - - - - - - - - - - för att positionera sig rätt                       
192400*- - - - - - - - - - - läser man first                                    
192500        PERFORM IMS-GHNP-AVS-RAD-FIRST                                    
192600        PERFORM IMS-GHNP-AVS-RAD-BORT                                     
192700                                                                          
192800        PERFORM UNTIL SEGMENT-SAKNAS                                      
192900           PERFORM IMS-DLET-AVS                                           
193000           PERFORM IMS-GHNP-AVS-RAD-BORT                                  
193100        END-PERFORM                                                       
193200                                                                          
193300        MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                             
193400     END-IF                                                               
193500     .                                                                    
193600     EJECT                                                                
193700 DD-BEHANDLA-AVS-REF SECTION.                                             
193800     SKIP2                                                                
193900*    --- Denna sektion sparar nyckel-adressen                             
194000*    --- till raden som är i begrepp att tas bort,                        
194100*    --- på den/de hänvisande radens/radernas not-segm wdn524             
194200     PERFORM IMS-GU-AVSG-HAEN                                             
194300     MOVE +2 TO W-IDSEGMNR                                                
194400     PERFORM UNTIL  SEGMENT-SAKNAS                                        
194500        MOVE AVSG-IDWDN512  TO  AVSG-REF-IDWDN512                         
194600        MOVE AVSG-REF-IDCATNR      TO W-IDCATNR-H                         
194700        MOVE AVSG-REF-IDCATGRP     TO W-IDCATGRP-H                        
194800        MOVE AVSG-REF-IDCATAVS     TO W-IDCATAVS-H                        
194900        MOVE AVSG-REF-IDCATRAD     TO W-IDCATRAD-H                        
195000        MOVE AVSG-REF-KDCATPUB-FOM TO W-KDCATPUB-H                        
195100        PERFORM IMS-GU-AVS-RAD-IO2                                        
195200        IF SEGMENT-FINNS                                                  
195300           PERFORM IMS-GHNP-AVS-NOT-IO2                                   
195400           IF SEGMENT-FINNS                                               
195500*            tag inte bort ev. redan inlagd info                          
195600             CONTINUE                                                     
195700           ELSE                                                           
195800             MOVE +2            TO IO2-NOT-IDSEGMNR                       
195900             MOVE W-IDCATGRP    TO IO2-NOT-IDCATGRP                       
196000             MOVE W-IDCATAVS    TO IO2-NOT-IDCATAVS                       
196100             MOVE W-IDCATRAD    TO IO2-NOT-IDCATRAD                       
196200             MOVE W-KDCATPUB-X  TO IO2-NOT-KDCATPUB-FOM                   
196300             MOVE WS-KDCATPUB-T TO IO2-NOT-KDCATPUB-TOM                   
196400             PERFORM IMS-ISRT-AVS-NOT-IO2                                 
196500           END-IF                                                         
196600        END-IF                                                            
196700        PERFORM IMS-GN-AVSG-HAEN                                          
196800     END-PERFORM                                                          
196900     .                                                                    
197000     EJECT                                                                
197100 E-KOLLA-UPPDATE   SECTION.                                               
197200     SKIP2                                                                
197300     MOVE NEJ TO INDATA-FEL                                               
197400     PERFORM IMS-GHU-AVS                                                  
197500                                                                          
197600     IF SEGMENT-FINNS                                                     
197700        PERFORM IMS-GHNP-AVS-RAD                                          
197800                                                                          
197900        IF SEGMENT-FINNS                                                  
198000           MOVE 'Ä' TO UPPDATE-FL                                         
198100        ELSE                                                              
198200           MOVE 'N' TO UPPDATE-FL                                         
198300        END-IF                                                            
198400     ELSE                                                                 
198500       MOVE JA TO INDATA-FEL                                              
198600     END-IF                                                               
198700     .                                                                    
198800     EJECT                                                                
198900 F-KOLLA-NYREG-DATA SECTION.                                              
199000     SKIP2                                                                
199100     IF MID-IDCATRAD-UPD < MID-IDCATRAD-FROM                              
199200        PERFORM FD-KOLLA-RAD-MINDRE                                       
199300     ELSE                                                                 
199400        IF MID-IDCATRAD-UPD > MID-IDCATRAD-TOM                            
199500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR                
199600           MOVE JA TO INDATA-FEL                                          
199700        ELSE                                                              
199800           CONTINUE                                                       
199900        END-IF                                                            
200000     END-IF                                                               
200100                                                                          
200200     PERFORM FE-KOLLA-HAEN                                                
200300                                                                          
200400     IF INDATA-FEL = NEJ                                                  
200500        PERFORM P-BLANKA-TEST                                             
200600        PERFORM FA-FLYTTA-NYREG-MID                                       
200700        PERFORM X-GEMENSAM-KOLL                                           
200800*               inbördes  ARTIKEL, BEART, KDHOM, KDPS, IDRUBNR            
200900        PERFORM Y-KOLLA-KDCATPUB-RAD-KOMB                                 
201000*               Inbördes  KDCATPUB på samma radnr                         
201100        IF INDATA-FEL = NEJ                                               
201200           PERFORM FB-KOMB-UTAN-BAS                                       
201300           IF INDATA-FEL = NEJ                                            
201400              PERFORM FC-KOMB-MED-BAS                                     
201500           END-IF                                                         
201600        END-IF                                                            
201700     END-IF                                                               
201800     .                                                                    
201900     EJECT                                                                
202000 FA-FLYTTA-NYREG-MID SECTION.                                             
202100     SKIP2                                                                
202200     MOVE WS-KDCATPUB-FOM-UPD    TO TEST-KDCATPUB-FOM                     
202300                                                                          
202400     IF MID-KDCATPUB-R-TOM-UPD NOT = ALL '+'                              
202500        MOVE WS-KDCATPUB-TOM-UPD TO TEST-KDCATPUB-TOM                     
202600     ELSE                                                                 
202700        MOVE   HIGH-VALUE        TO TEST-KDCATPUB-TOM                     
202800     END-IF                                                               
202900     IF MID-KDFBX-UPD NOT = ALL '+'                                       
203000        MOVE MID-KDFBX-UPD TO TEST-KDFBX                                  
203100     END-IF                                                               
203200                                                                          
203300     IF MID-IDCATPOS-UPD NOT = ALL '+'                                    
203400       MOVE W-IDCATPOS-HEL TO TEST-IDCATPOS-HEL                           
203500     END-IF                                                               
203600                                                                          
203700     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
203800        MOVE MID-IDARTNR-UPD TO TEST-IDARTNR                              
203900     END-IF                                                               
204000                                                                          
204100     MOVE +1 TO INDX                                                      
204200     PERFORM UNTIL INDX = +6                                              
204300        IF MID-KVKOL-UPD(INDX) NOT = ALL '+'                              
204400           MOVE MID-KVKOL-UPD(INDX) TO TEST-KVKOL(INDX)                   
204500        END-IF                                                            
204600        ADD +1 TO INDX                                                    
204700     END-PERFORM                                                          
204800                                                                          
204900     IF MID-KDPS-UPD NOT = ALL '+'                                        
205000        MOVE MID-KDPS-UPD  TO TEST-KDPS                                   
205100     END-IF                                                               
205200                                                                          
205300     IF MID-KVPUNKT-UPD NOT = ALL '+'                                     
205400        MOVE MID-KVPUNKT-UPD TO TEST-KVPUNKT                              
205500     END-IF                                                               
205600                                                                          
205700     IF MID-IDTTEXNR-UPD NOT = ALL '+'                                    
205800        MOVE MID-IDTTEXNR-UPD TO TEST-IDTTEXNR                            
205900     END-IF                                                               
206000                                                                          
206100     IF MID-BEART-UPD NOT = ALL '+'                                       
206200        MOVE MID-BEART-UPD  TO TEST-BEART                                 
206300     END-IF                                                               
206400                                                                          
206500     IF MID-KDHOM-UPD NOT = ALL '+'                                       
206600        MOVE MID-KDHOM-UPD  TO TEST-KDHOM                                 
206700     END-IF                                                               
206800     .                                                                    
206900     EJECT                                                                
207000 FB-KOMB-UTAN-BAS SECTION.                                                
207100     SKIP2                                                                
207200*- - - - - - - - - - - IDARTNR KOMBINERAT IDRUBNR                         
207300     IF (TEST-IDARTNR NOT = ZERO) AND                                     
207400        (IDRUBNR-FINNS = JA)                                              
207500        MOVE +1 TO INDX                                                   
207600        PERFORM UNTIL INDX = +4                                           
207700           IF MID-IDRUBNR-UPD(INDX) NOT = ALL '+'                         
207800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(INDX)        
207900              MOVE JA TO INDATA-FEL                                       
208000           END-IF                                                         
208100           ADD +1 TO INDX                                                 
208200        END-PERFORM                                                       
208300     END-IF                                                               
208400                                                                          
208500*- - - - - - - - - - - LIKA IDRUBNR                                       
208600     IF IDRUBNR-FINNS = JA                                                
208700        IF IN-IDRUBNR(1) NOT = ZERO                                       
208800           IF IN-IDRUBNR(1) = IN-IDRUBNR(2)                               
208900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(2)           
209000              MOVE JA TO INDATA-FEL                                       
209100           END-IF                                                         
209200           IF IN-IDRUBNR(1) = IN-IDRUBNR(3)                               
209300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(3)           
209400              MOVE JA TO INDATA-FEL                                       
209500           END-IF                                                         
209600           IF IN-IDRUBNR(2) NOT = ZERO                                    
209700              IF IN-IDRUBNR(2) = IN-IDRUBNR(3)                            
209800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD(3)             
209900                 MOVE JA TO INDATA-FEL                                    
210000              END-IF                                                      
210100           END-IF                                                         
210200        ELSE                                                              
210300           IF IN-IDRUBNR(2) NOT = ZERO                                    
210400              IF IN-IDRUBNR(2) = IN-IDRUBNR(3)                            
210500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD(3)             
210600                 MOVE JA TO INDATA-FEL                                    
210700              END-IF                                                      
210800           END-IF                                                         
210900        END-IF                                                            
211000     END-IF                                                               
211100                                                                          
211200*- - - - - - - - - - - LIKA IDFOTNR                                       
211300     IF IDFOTNR-FINNS = JA                                                
211400        IF IN-IDFOTNR(1) NOT = ZERO                                       
211500           IF IN-IDFOTNR(1) = IN-IDFOTNR(2)                               
211600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(2)           
211700              MOVE JA TO INDATA-FEL                                       
211800           END-IF                                                         
211900           IF IN-IDFOTNR(1) = IN-IDFOTNR(3)                               
212000              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(3)           
212100              MOVE JA TO INDATA-FEL                                       
212200           END-IF                                                         
212300           IF IN-IDFOTNR(2) NOT = ZERO                                    
212400              IF IN-IDFOTNR(2) = IN-IDFOTNR(3)                            
212500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD(3)             
212600                 MOVE JA TO INDATA-FEL                                    
212700              END-IF                                                      
212800           END-IF                                                         
212900        ELSE                                                              
213000           IF IN-IDFOTNR(2) NOT = ZERO                                    
213100              IF IN-IDFOTNR(2) = IN-IDFOTNR(3)                            
213200                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD(3)             
213300                 MOVE JA TO INDATA-FEL                                    
213400              END-IF                                                      
213500           END-IF                                                         
213600        END-IF                                                            
213700     END-IF                                                               
213800     .                                                                    
213900     EJECT                                                                
214000 FC-KOMB-MED-BAS SECTION.                                                 
214100     SKIP2                                                                
214200*- - - - - - - - - - - KDHAEN kombinationer                               
214300     IF INDATA-FEL = NEJ                                                  
214400     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
214500        IF MID-KDHAEN-UPD = 'B' OR 'A'                                    
214600           IF MID-IDCATGRP-H-UPD NOT = ALL '+'                            
214700              MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                    
214800                                                                          
214900              IF MID-IDCATAVS-H-UPD NOT = ALL '+'                         
215000                 MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                 
215100              ELSE                                                        
215200                 MOVE ZERO TO WS-IDCATAVS-H                               
215300              END-IF                                                      
215400                                                                          
215500*             Specialfall med IDCATRAD-H                                  
215600              IF MID-IDCATRAD-H-UPD NOT = ALL '+'                         
215700                MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                  
215800                IF WS-IDCATRAD-H = ZERO                                   
215900                  MOVE LOW-VALUE        TO WS-KDCATPUB-H-UPD              
216000*                 Hänv.till rad 0 är ALDRIG tidsbegränsande               
216100                ELSE                                                      
216200                  IF MID-KDCATPUB-R-H-UPD = ALL '+'                       
216300                    MOVE LOW-VALUE TO WS-KDCATPUB-H-UPD                   
216400                  END-IF                                                  
216500                END-IF                                                    
216600              ELSE                                                        
216700                MOVE ZERO               TO WS-IDCATRAD-H                  
216800                MOVE LOW-VALUE          TO WS-KDCATPUB-H-UPD              
216900              END-IF                                                      
217000*             Kontrollera hänvisningens samtidighet mot egna raden        
217100              IF WS-KDCATPUB-H-UPD = WS-KDCATPUB-FOM-UPD                  
217200              OR ( WS-KDCATPUB-H-UPD = LOW-VALUE                          
217300                   AND  WS-IDCATRAD-H     = ZERO )                        
217400                CONTINUE                                                  
217500              ELSE                                                        
217600                IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                     
217700                  MOVE MFS-ALFA-FAELT-FEL TO                              
217800                                      MOD-KDCATPUB-R-H-UPD-ATTR           
217900*                                     MOD-KDCATPUB-R-FOM-UPD-ATTR         
218000                  MOVE NEJ TO HAEN-FINNS                                  
218100                  MOVE JA TO INDATA-FEL                                   
218200                END-IF                                                    
218300              END-IF                                                      
218400                                                                          
218500*             WS-KDCATPUB-H-UPD  har fått relevant värde                  
218600*             kontroll mot hänvisad rads pubkod                           
218700              IF INDATA-FEL = NEJ                                         
218800               MOVE W-IDCATNR        TO W-IDCATNR-H                       
218900               MOVE WS-GRPAVS-H      TO W-WDN501-H-GRPAVS                 
219000               MOVE WS-IDCATRAD-H    TO W-IDCATRAD-H                      
219100               MOVE WS-KDCATPUB-H-UPD TO W-KDCATPUB-H                     
219200               PERFORM IMS-GU-AVS-RAD-IO2                                 
219300                                                                          
219400               IF SEGMENT-SAKNAS                                          
219500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-H-UPD-ATTR        
219600                                           MOD-IDCATAVS-H-UPD-ATTR        
219700                                           MOD-IDCATRAD-H-UPD-ATTR        
219800                 MOVE MFS-ALFA-FAELT-FEL TO                               
219900                                         MOD-KDCATPUB-R-H-UPD-ATTR        
220000                 MOVE JA TO INDATA-FEL                                    
220100               ELSE                                                       
220200*                ---------------------                                    
220300                 IF WS-KDCATPUB-H-UPD NOT = IO2-RAD-KDCATPUB-FOM          
220400*                  hänv. pekar tidsm. utanför den hänvisade raden         
220500                   MOVE MFS-ALFA-FAELT-FEL TO                             
220600                                         MOD-KDCATPUB-R-H-UPD-ATTR        
220700                   MOVE JA TO INDATA-FEL                                  
220800                 END-IF                                                   
220900*                ---------------------                                    
221000                 IF WS-KDCATPUB-TOM-UPD  > IO2-RAD-KDCATPUB-TOM           
221100*                  denna rad gäller längre än hänvisad rad                
221200                   MOVE MFS-ALFA-FAELT-FEL TO                             
221300                                        MOD-KDCATPUB-R-H-UPD-ATTR         
221400                                      MOD-KDCATPUB-R-TOM-UPD-ATTR         
221500                   MOVE JA TO INDATA-FEL                                  
221600                 END-IF                                                   
221700*                ---------------------                                    
221800               END-IF                                                     
221900              END-IF                                                      
222000           ELSE                                                           
222100              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-H-UPD-ATTR           
222200              MOVE JA TO INDATA-FEL                                       
222300           END-IF                                                         
222400        END-IF                                                            
222500     END-IF                                                               
222600     END-IF                                                               
222700                                                                          
222800*- - - - - - - - - - - IDRUBNR GODKÄNDA                                   
222900     IF IDRUBNR-FINNS = JA                                                
223000        MOVE +1 TO INDX                                                   
223100        PERFORM UNTIL INDX = +4                                           
223200                                                                          
223300           IF IN-IDRUBNR(INDX) NOT = ZERO                                 
223400              MOVE IN-IDRUBNR(INDX) TO W-IDRUBNR                          
223500              PERFORM IMS-GU-RUB                                          
223600                                                                          
223700              IF SEGMENT-FINNS                                            
223800                 IF (IN-IDRUBNR(2) NOT = ZERO) OR                         
223900                    (IN-IDRUBNR(3) NOT = ZERO)                            
224000                                                                          
224100                    IF RUB-RUB-FLKOMBINERAS NOT = 'J'                     
224200                       MOVE MFS-NUM-FAELT-FEL TO                          
224300                            MOD-IDRUBNR-UPD-ATTR(INDX)                    
224400                       MOVE JA TO INDATA-FEL                              
224500                    END-IF                                                
224600                 END-IF                                                   
224700                                                                          
224800              ELSE                                                        
224900                 MOVE MFS-NUM-FAELT-FEL TO                                
225000                      MOD-IDRUBNR-UPD-ATTR(INDX)                          
225100                 MOVE JA TO INDATA-FEL                                    
225200              END-IF                                                      
225300           END-IF                                                         
225400           ADD +1 TO INDX                                                 
225500        END-PERFORM                                                       
225600     END-IF                                                               
225700                                                                          
225800*- - - - - - - - - - - IDFOTNR GODKÄNDA                                   
225900     IF IDFOTNR-FINNS = JA                                                
226000        MOVE +1 TO INDX                                                   
226100        PERFORM UNTIL INDX = +4                                           
226200                                                                          
226300           IF IN-IDFOTNR(INDX) NOT = ZERO                                 
226400              MOVE IN-IDFOTNR(INDX) TO W-IDFOTNR                          
226500              PERFORM IMS-GU-FOT                                          
226600                                                                          
226700              IF SEGMENT-SAKNAS                                           
226800                 MOVE MFS-NUM-FAELT-FEL TO                                
226900                      MOD-IDFOTNR-UPD-ATTR(INDX)                          
227000                 MOVE JA TO INDATA-FEL                                    
227100              END-IF                                                      
227200           END-IF                                                         
227300           ADD +1 TO INDX                                                 
227400        END-PERFORM                                                       
227500     END-IF                                                               
227600                                                                          
227700*- - - - - - - - - - - IDTTEXNR GODKÄNDA                                  
227800     IF TEST-IDTTEXNR NOT = ZERO                                          
227900        MOVE TEST-IDTTEXNR TO W-IDTTEXNR                                  
228000        PERFORM IMS-GU-TEXT                                               
228100                                                                          
228200        IF SEGMENT-SAKNAS                                                 
228300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDTTEXNR-UPD-ATTR                
228400           MOVE JA TO INDATA-FEL                                          
228500        END-IF                                                            
228600     END-IF                                                               
228700     .                                                                    
228800     EJECT                                                                
228900 FD-KOLLA-RAD-MINDRE SECTION.                                             
229000     SKIP2                                                                
229100*- - - - - - - - - -  Denna läsning görs för att kolla                    
229200*                     om det är tillåtet att lägga upp                    
229300*                     någon rad som är mindre än befintlig                
229400     PERFORM IMS-GHU-AVS                                                  
229500     IF SEGMENT-FINNS                                                     
229600                                                                          
229700        MOVE  0020 TO W-IDCATRAD-MIN                                      
229800        PERFORM IMS-GNP-AVS-RAD-FIRST                                     
229900        IF SEGMENT-FINNS                                                  
230000           IF RAD-IDCATRAD > MID-IDCATRAD-UPD                             
230100              CONTINUE                                                    
230200           ELSE                                                           
230300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR             
230400              MOVE JA TO INDATA-FEL                                       
230500           END-IF                                                         
230600        END-IF                                                            
230700*- - - - - - - - - -  Denna läsning görs endast för att                   
230800*                     komma till ursprunglig position                     
230900        PERFORM IMS-GHNP-AVS-RAD                                          
231000     END-IF                                                               
231100     .                                                                    
231200     EJECT                                                                
231300 FE-KOLLA-HAEN SECTION.                                                   
231400     SKIP2                                                                
231500     IF MID-KDHAEN-UPD = ALL '+'                                          
231600        IF (MID-IDCATGRP-H-UPD NOT = ALL '+') OR                          
231700           (MID-IDCATAVS-H-UPD NOT = ALL '+') OR                          
231800           (MID-IDCATRAD-H-UPD NOT = ALL '+') OR                          
231900           (MID-KDCATPUB-R-H-UPD NOT = ALL '+')                           
232000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                 
232100           MOVE JA TO INDATA-FEL                                          
232200*          Hänvisning ej fullständigt ifylld  (A B * )                    
232300        END-IF                                                            
232400     END-IF                                                               
232500     .                                                                    
232600     EJECT                                                                
232700 G-NYREGISTRERA SECTION.                                                  
232800     SKIP2                                                                
232900     MOVE NEJ TO UPPDAT-GJORD                                             
233000                                                                          
233100     PERFORM IMS-GHNP-AVS-RAD                                             
233200     IF SEGMENT-SAKNAS                                                    
233300        MOVE W-IDCATRAD        TO RAD-IDCATRAD                            
233400        MOVE WS-KDCATPUB-FOM-UPD TO RAD-KDCATPUB-FOM                      
233500        MOVE WS-KDCATPUB-TOM-UPD TO RAD-KDCATPUB-TOM                      
233600        MOVE DAGENS-DATUM-TIAAMMDD  TO RAD-TIUPPDAT                       
233700        MOVE MSG-SIGNON-USERID TO RAD-IDUSER                              
233800        MOVE 'N'               TO RAD-KDRADST                             
233900        PERFORM IMS-ISRT-AVS-RAD                                          
234000     END-IF                                                               
234100                                                                          
234200     IF  (MID-KDFBX-UPD    = ALL '+')                                     
234300     AND (MID-IDCATPOS-UPD = ALL '+')                                     
234400     AND (MID-IDARTNR-UPD  = ALL '+')                                     
234500     AND (KVKOL-FINNS      = NEJ)                                         
234600     AND (MID-KDPS-UPD     = ALL '+')                                     
234700     AND (MID-KVPUNKT-UPD  = ALL '+')                                     
234800     AND (MID-IDTTEXNR-UPD = ALL '+')                                     
234900        CONTINUE                                                          
235000     ELSE                                                                 
235100        PERFORM GA-NYREG-ART                                              
235200     END-IF                                                               
235300                                                                          
235400     IF MID-TEKATANM-UPD = ALL '+'                                        
235500        CONTINUE                                                          
235600     ELSE                                                                 
235700        PERFORM GB-NYREG-TEXT                                             
235800     END-IF                                                               
235900                                                                          
236000     IF MID-BEART-UPD = ALL '+' AND MID-KDHOM-UPD = ALL '+'               
236100        CONTINUE                                                          
236200     ELSE                                                                 
236300        PERFORM GC-NYREG-BEN                                              
236400     END-IF                                                               
236500                                                                          
236600     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
236700        IF MID-KDHAEN-UPD = '*'                                           
236800           PERFORM GD-NYREG-NOT                                           
236900        END-IF                                                            
237000     END-IF                                                               
237100                                                                          
237200     IF IDRUBNR-FINNS = JA                                                
237300        PERFORM GE-NYREG-RUB                                              
237400     END-IF                                                               
237500                                                                          
237600     IF IDFOTNR-FINNS = JA                                                
237700        PERFORM GF-NYREG-FOT                                              
237800     END-IF                                                               
237900                                                                          
238000     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
238100        IF MID-KDHAEN-UPD = 'B' OR 'A'                                    
238200           PERFORM GG-NYREG-HAEN                                          
238300        END-IF                                                            
238400     END-IF                                                               
238500     MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                                
238600     .                                                                    
238700     EJECT                                                                
238800 GA-NYREG-ART SECTION.                                                    
238900     SKIP2                                                                
239000     PERFORM IMS-GHNP-AVS-ART                                             
239100     IF SEGMENT-SAKNAS                                                    
239200                                                                          
239300        MOVE '1'              TO ART-KDSEGKEY                             
239400        MOVE TEST-KDFBX        TO ART-KDFBX                               
239500        MOVE TEST-IDCATPOS-HEL TO ART-IDCATPOS                            
239600        MOVE TEST-IDARTNR      TO ART-IDARTNR                             
239700                                                                          
239800        MOVE +1 TO IX                                                     
239900        PERFORM UNTIL IX = +6                                             
240000           IF TEST-KVKOL(IX) = SPACE                                      
240100             MOVE    SPACE         TO ART-KVKOL(IX)                       
240200           ELSE                                                           
240300             IF TEST-KVKOL(IX)(1:1) = SPACE                               
240400               IF TEST-KVKOL(IX)(2:1) = SPACE                             
240500                 MOVE TEST-KVKOL(IX)(3:1) TO ART-KVKOL(IX)                
240600               ELSE                                                       
240700                 IF TEST-KVKOL(IX)(3:1) = SPACE                           
240800                   MOVE TEST-KVKOL(IX)(2:1) TO ART-KVKOL(IX)              
240900                 ELSE                                                     
241000                   MOVE TEST-KVKOL(IX)(2:2) TO ART-KVKOL(IX)              
241100                 END-IF                                                   
241200               END-IF                                                     
241300             ELSE                                                         
241400               IF TEST-KVKOL(IX)(2:1) = SPACE                             
241500                 MOVE TEST-KVKOL(IX)(1:1) TO ART-KVKOL(IX)                
241600               ELSE                                                       
241700                 MOVE TEST-KVKOL(IX)      TO ART-KVKOL(IX)                
241800               END-IF                                                     
241900             END-IF                                                       
242000           END-IF                                                         
242100           ADD +1 TO IX                                                   
242200        END-PERFORM                                                       
242300                                                                          
242400        MOVE TEST-KDPS        TO ART-KDPS                                 
242500        MOVE TEST-KVPUNKT     TO ART-KVPUNKT                              
242600        MOVE TEST-IDTTEXNR    TO ART-IDTTEXNR                             
242700                                                                          
242800        PERFORM IMS-ISRT-AVS-ART                                          
242900        MOVE JA TO UPPDAT-GJORD                                           
243000     END-IF                                                               
243100     .                                                                    
243200     EJECT                                                                
243300 GB-NYREG-TEXT SECTION.                                                   
243400     SKIP2                                                                
243500     PERFORM IMS-GHNP-AVS-TEXT                                            
243600                                                                          
243700     IF SEGMENT-SAKNAS                                                    
243800        MOVE '1'           TO TEXT-KDSEGKEY                               
243900        MOVE SPACE         TO TEXT-BERUBTEXT                              
244000        MOVE MID-TEKATANM-UPD  TO TEXT-TEKATANM                           
244100        PERFORM IMS-ISRT-AVS-TEXT                                         
244200        MOVE JA TO UPPDAT-GJORD                                           
244300     END-IF                                                               
244400     .                                                                    
244500     EJECT                                                                
244600 GC-NYREG-BEN SECTION.                                                    
244700     SKIP2                                                                
244800     PERFORM IMS-GHNP-AVS-BEN                                             
244900                                                                          
245000     IF SEGMENT-SAKNAS                                                    
245100        MOVE '1'        TO BEN-KDSEGKEY                                   
245200        MOVE TEST-BEART TO BEN-BEART                                      
245300        MOVE TEST-KDHOM TO BEN-KDHOM                                      
245400                                                                          
245500        IF TEST-KDPS = 'XX'                                               
245600           MOVE ZERO TO BEN-KDHOM                                         
245700           PERFORM IMS-ISRT-AVS-BEN                                       
245800           MOVE JA TO UPPDAT-GJORD                                        
245900        ELSE                                                              
246000           IF TEST-KDPS = 'LS' OR 'KL'                                    
246100              PERFORM IMS-ISRT-AVS-BEN                                    
246200              MOVE JA TO UPPDAT-GJORD                                     
246300           ELSE                                                           
246400              IF TEST-KDPS = 'NS' OR 'KN'                                 
246500                 IF BEN-BEART NOT = SPACE                                 
246600                    PERFORM IMS-ISRT-AVS-BEN                              
246700                    MOVE JA TO UPPDAT-GJORD                               
246800                 END-IF                                                   
246900              ELSE                                                        
247000                 IF TEST-KDPS = '  ' AND TEST-IDARTNR = ZERO              
247100                    IF BEN-BEART NOT = SPACE                              
247200                       PERFORM IMS-ISRT-AVS-BEN                           
247300                       MOVE JA TO UPPDAT-GJORD                            
247400                    END-IF                                                
247500                 END-IF                                                   
247600              END-IF                                                      
247700           END-IF                                                         
247800        END-IF                                                            
247900     END-IF                                                               
248000     .                                                                    
248100     EJECT                                                                
248200 GD-NYREG-NOT SECTION.                                                    
248300     SKIP2                                                                
248400     PERFORM IMS-GHNP-AVS-NOT                                             
248500     MOVE +2         TO NOT-IDSEGMNR                                      
248600*                    2 = segmentnummer för ej klarlagd hänvisning         
248700     MOVE SPACE      TO NOT-TENOTE                                        
248800     MOVE ZERO       TO NOT-IDCATGRP                                      
248900                        NOT-IDCATAVS                                      
249000                        NOT-IDCATRAD                                      
249100     MOVE  LOW-VALUE TO NOT-KDCATPUB-FOM                                  
249200     MOVE HIGH-VALUE TO NOT-KDCATPUB-TOM                                  
249300                                                                          
249400     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
249500        MOVE MID-IDCATGRP-H-UPD TO NOT-IDCATGRP                           
249600     END-IF                                                               
249700                                                                          
249800     IF MID-IDCATAVS-H-UPD NOT = ALL '+'                                  
249900        MOVE MID-IDCATAVS-H-UPD TO NOT-IDCATAVS                           
250000     END-IF                                                               
250100                                                                          
250200     IF MID-IDCATRAD-H-UPD NOT = ALL '+'                                  
250300        MOVE MID-IDCATRAD-H-UPD TO NOT-IDCATRAD                           
250400     END-IF                                                               
250500                                                                          
250600     IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                                
250700        MOVE  WS-KDCATPUB-H-UPD TO NOT-KDCATPUB-FOM                       
250800*             ws-KDCATPUB-h-upd har fått relevanta värden i b-            
250900     END-IF                                                               
251000*    not-KDCATPUB-tom får värden från hänvisad rad då den rivs            
251100                                                                          
251200     PERFORM IMS-ISRT-AVS-NOT                                             
251300     MOVE JA TO UPPDAT-GJORD                                              
251400     .                                                                    
251500     EJECT                                                                
251600 GE-NYREG-RUB SECTION.                                                    
251700     SKIP2                                                                
251800     PERFORM IMS-GHNP-AVS-RUB                                             
251900                                                                          
252000     MOVE +1 TO INDX                                                      
252100                                                                          
252200     PERFORM UNTIL INDX = +4                                              
252300        IF IN-IDRUBNR(INDX) NOT = ZERO                                    
252400           MOVE IN-IDRUBNR(INDX)  TO RUB-IDRUBNR                          
252500           MOVE INDX              TO RUB-IDSEGMNR                         
252600           MOVE 'J'               TO RUB-FLRUBTYP                         
252700           PERFORM IMS-ISRT-AVS-RUB                                       
252800           MOVE JA TO UPPDAT-GJORD                                        
252900        END-IF                                                            
253000        ADD +1 TO INDX                                                    
253100     END-PERFORM                                                          
253200     .                                                                    
253300     EJECT                                                                
253400 GF-NYREG-FOT SECTION.                                                    
253500     SKIP2                                                                
253600     PERFORM IMS-GHNP-AVS-FOT                                             
253700                                                                          
253800     MOVE +1 TO INDX                                                      
253900     PERFORM UNTIL INDX = +4                                              
254000        IF IN-IDFOTNR(INDX) NOT = ZERO                                    
254100           MOVE IN-IDFOTNR(INDX)  TO FOT-IDFOTNR                          
254200           MOVE INDX              TO FOT-IDSEGMNR                         
254300           PERFORM IMS-ISRT-AVS-FOT                                       
254400           MOVE JA TO UPPDAT-GJORD                                        
254500        END-IF                                                            
254600                                                                          
254700        ADD +1 TO INDX                                                    
254800     END-PERFORM                                                          
254900     .                                                                    
255000     SKIP2                                                                
255100 GG-NYREG-HAEN SECTION.                                                   
255200     SKIP2                                                                
255300     PERFORM IMS-GHNP-AVS-HAEN                                            
255400     MOVE W-IDCATNR         TO HAEN-IDCATNR                               
255500     MOVE WS-IDCATGRP-H     TO HAEN-IDCATGRP                              
255600     MOVE WS-IDCATAVS-H     TO HAEN-IDCATAVS                              
255700     MOVE WS-IDCATRAD-H     TO HAEN-IDCATRAD                              
255800     MOVE WS-KDCATPUB-H-UPD TO HAEN-KDCATPUB-FOM                          
255900     MOVE MID-KDHAEN-UPD    TO HAEN-KDHAEN                                
256000     PERFORM IMS-ISRT-AVS-HAEN                                            
256100     MOVE JA TO UPPDAT-GJORD                                              
256200     .                                                                    
256300     EJECT                                                                
256400 H-KOLLA-ANDRING-DATA SECTION.                                            
256500     SKIP2                                                                
256600     IF (MID-IDCATRAD-UPD >= MID-IDCATRAD-FROM)                           
256700     AND (MID-IDCATRAD-UPD <= MID-IDCATRAD-TOM)                           
256800        CONTINUE                                                          
256900     ELSE                                                                 
257000        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-UPD-ATTR                   
257100        MOVE JA TO INDATA-FEL                                             
257200     END-IF                                                               
257300     PERFORM P-BLANKA-TEST                                                
257400     PERFORM HA-KOMB-UTAN-BAS                                             
257500     IF INDATA-FEL = NEJ                                                  
257600        PERFORM HB-LAS-BAS-TILL-TEST                                      
257700        PERFORM HC-FLYTTA-AENDR-MID                                       
257800        PERFORM X-GEMENSAM-KOLL                                           
257900*               Inbördes  ARTIKEL, BEART, KDHOM, KDPS, IDRUBNR            
258000        PERFORM Y-KOLLA-KDCATPUB-RAD-KOMB                                 
258100*               Inbördes  KDCATPUB på samma radnr                         
258200        IF INDATA-FEL = NEJ                                               
258300           PERFORM HD-BAS-KOMB-RUBNR                                      
258400           IF INDATA-FEL = NEJ                                            
258500              PERFORM HH-KOMB-IDARTNR-IDRUBNR                             
258600              IF INDATA-FEL = NEJ                                         
258700                 PERFORM HE-BAS-LAS-FOTNR                                 
258800                 PERFORM HF-BAS-KOMB-KDHAEN                               
258900                 IF INDATA-FEL = NEJ                                      
259000                    PERFORM HG-SLUT-KOMB-MED-BAS                          
259100                 END-IF                                                   
259200              END-IF                                                      
259300           END-IF                                                         
259400        END-IF                                                            
259500     END-IF                                                               
259600     .                                                                    
259700     EJECT                                                                
259800 HA-KOMB-UTAN-BAS SECTION.                                                
259900     SKIP2                                                                
260000*- - - - - - - - - - - IDARTNR KOMBINERAT IDRUBNR                         
260100     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
260200     MOVE +1 TO INDX                                                      
260300        IF IDRUBNR-FINNS = JA                                             
260400           PERFORM UNTIL INDX = +4                                        
260500             IF IN-IDRUBNR(INDX) NOT = ZERO                               
260600                MOVE MFS-NUM-FAELT-FEL                                    
260700                                 TO MOD-IDRUBNR-UPD-ATTR(INDX)            
260800                MOVE JA TO INDATA-FEL                                     
260900             END-IF                                                       
261000             ADD +1 TO INDX                                               
261100          END-PERFORM                                                     
261200        END-IF                                                            
261300     END-IF                                                               
261400*- - - - - - - - - - - KDHAEN KOMBINATIONER                               
261500     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
261600        IF MID-KDHAEN-UPD = '*'                                           
261700*          upplägg av tillfällig hänvisning                               
261800           IF MID-IDCATGRP-H-UPD = ALL '+'                                
261900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-H-UPD-ATTR           
262000              MOVE JA TO INDATA-FEL                                       
262100           ELSE                                                           
262200              MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                    
262300           END-IF                                                         
262400           EVALUATE TRUE                                                  
262500              WHEN MID-IDCATRAD-H-UPD = ALL '+'                           
262600                 MOVE ZERO      TO WS-IDCATRAD-H                          
262700                 MOVE LOW-VALUE TO WS-KDCATPUB-H                          
262800                 IF MID-IDCATAVS-H-UPD = ALL '+'                          
262900                    MOVE ZERO   TO WS-IDCATAVS-H                          
263000                 ELSE                                                     
263100                    MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H              
263200                 END-IF                                                   
263300              WHEN MID-IDCATAVS-H-UPD = ALL '+'                           
263400                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-H-UPD-ATTR        
263500                 MOVE JA TO INDATA-FEL                                    
263600                                                                          
263700              WHEN OTHER                                                  
263800                 IF MID-KDCATPUB-R-H-UPD = ALL '+'                        
263900                   IF MID-IDCATRAD-H-UPD > ZERO                           
264000                     MOVE MFS-ALFA-FAELT-FEL                              
264100                                      TO MOD-KDCATPUB-R-H-UPD-ATTR        
264200                     MOVE JA TO INDATA-FEL                                
264300                   ELSE                                                   
264400                     MOVE LOW-VALUE TO WS-KDCATPUB-H                      
264500                   END-IF                                                 
264600                 ELSE                                                     
264700                   MOVE MID-KDCATPUB-R-H-UPD TO WS-KDCATPUB-R-AVV         
264800                   PERFORM S50-Y2K-KDCATPUB-R                             
264900                   IF WS-KDCATPUB-AAAAVV = SPACE                          
265000                     MOVE MFS-ALFA-FAELT-FEL                              
265100                                      TO MOD-KDCATPUB-R-H-UPD-ATTR        
265200                     MOVE JA TO INDATA-FEL                                
265300                   ELSE                                                   
265400                     MOVE WS-KDCATPUB-AAAAVV TO WS-KDCATPUB-H             
265500                   END-IF                                                 
265600                 END-IF                                                   
265700                 MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                 
265800                 MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                 
265900           END-EVALUATE                                                   
266000        END-IF                                                            
266100     END-IF                                                               
266200     .                                                                    
266300     EJECT                                                                
266400 HB-LAS-BAS-TILL-TEST SECTION.                                            
266500     SKIP2                                                                
266600*    Raden är läst i E-kolla-update  *                                    
266700     MOVE RAD-KDCATPUB-FOM  TO TEST-KDCATPUB-FOM                          
266800     MOVE RAD-KDCATPUB-TOM  TO TEST-KDCATPUB-TOM                          
266900*                                                                         
267000     PERFORM IMS-GHNP-AVS-ART                                             
267100     IF SEGMENT-FINNS                                                     
267200        MOVE ART-KDFBX      TO TEST-KDFBX                                 
267300        MOVE ART-IDCATPOS   TO TEST-IDCATPOS-HEL                          
267400        MOVE ART-IDARTNR    TO TEST-IDARTNR                               
267500*       MOVE ART-KVKOL-GRP  TO TEST-KVKOL-GRP                             
267600*       --- Flyttar här basens värden vänsterställt till TEST-            
267700        MOVE +1 TO IX                                                     
267800        PERFORM UNTIL IX = +6                                             
267900           IF ART-KVKOL(IX) = SPACE                                       
268000             MOVE    SPACE  TO TEST-KVKOL(IX)                             
268100           ELSE                                                           
268200             IF ART-KVKOL(IX)(1:1) = SPACE                                
268300               IF ART-KVKOL(IX)(2:1) = SPACE                              
268400                 MOVE ART-KVKOL(IX)(3:1) TO TEST-KVKOL(IX)                
268500               ELSE                                                       
268600                 IF ART-KVKOL(IX)(3:1) = SPACE                            
268700                   MOVE ART-KVKOL(IX)(2:1) TO TEST-KVKOL(IX)              
268800                 ELSE                                                     
268900                   MOVE ART-KVKOL(IX)(2:2) TO TEST-KVKOL(IX)              
269000                 END-IF                                                   
269100               END-IF                                                     
269200             ELSE                                                         
269300               MOVE ART-KVKOL(IX)  TO TEST-KVKOL(IX)                      
269400             END-IF                                                       
269500           END-IF                                                         
269600           ADD +1 TO IX                                                   
269700        END-PERFORM                                                       
269800        MOVE ART-KDPS       TO TEST-KDPS                                  
269900        MOVE ART-KVPUNKT    TO TEST-KVPUNKT                               
270000        MOVE ART-IDTTEXNR   TO TEST-IDTTEXNR                              
270100     END-IF                                                               
270200*                                                                         
270300     PERFORM IMS-GHNP-AVS-BEN                                             
270400     IF SEGMENT-FINNS                                                     
270500        MOVE BEN-KDHOM      TO TEST-KDHOM                                 
270600        MOVE BEN-BEART      TO TEST-BEART                                 
270700     END-IF                                                               
270800     .                                                                    
270900     EJECT                                                                
271000 HC-FLYTTA-AENDR-MID SECTION.                                             
271100     SKIP2                                                                
271200*    TEST-KDCATPUB-FOM redan ifylld från HB-                              
271300                                                                          
271400     IF MID-KDCATPUB-R-TOM-UPD NOT = ALL '+'                              
271500        MOVE WS-KDCATPUB-TOM-UPD TO TEST-KDCATPUB-TOM                     
271600     END-IF                                                               
271700     IF MID-KDFBX-UPD NOT = ALL '+'                                       
271800        MOVE MID-KDFBX-UPD TO TEST-KDFBX                                  
271900     END-IF                                                               
272000     IF MID-IDCATPOS-UPD NOT = ALL '+'                                    
272100        MOVE W-IDCATPOS-HEL TO TEST-IDCATPOS-HEL                          
272200     END-IF                                                               
272300     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
272400        MOVE MID-IDARTNR-UPD TO TEST-IDARTNR                              
272500     END-IF                                                               
272600                                                                          
272700     MOVE +1 TO INDX                                                      
272800     PERFORM UNTIL INDX = +6                                              
272900        IF MID-KVKOL-UPD(INDX) NOT = ALL '+'                              
273000           MOVE MID-KVKOL-UPD(INDX) TO TEST-KVKOL(INDX)                   
273100        END-IF                                                            
273200        ADD +1 TO INDX                                                    
273300     END-PERFORM                                                          
273400                                                                          
273500     IF MID-KDPS-UPD NOT = ALL '+'                                        
273600        MOVE MID-KDPS-UPD  TO TEST-KDPS                                   
273700     END-IF                                                               
273800     IF MID-KVPUNKT-UPD NOT = ALL '+'                                     
273900        MOVE MID-KVPUNKT-UPD TO TEST-KVPUNKT                              
274000     END-IF                                                               
274100     IF MID-IDTTEXNR-UPD NOT = ALL '+'                                    
274200        MOVE MID-IDTTEXNR-UPD TO TEST-IDTTEXNR                            
274300     END-IF                                                               
274400     IF MID-BEART-UPD NOT = ALL '+'                                       
274500        MOVE MID-BEART-UPD  TO TEST-BEART                                 
274600     END-IF                                                               
274700     IF MID-KDHOM-UPD NOT = ALL '+'                                       
274800        MOVE MID-KDHOM-UPD  TO TEST-KDHOM                                 
274900     END-IF                                                               
275000     .                                                                    
275100     EJECT                                                                
275200 HD-BAS-KOMB-RUBNR SECTION.                                               
275300     SKIP2                                                                
275400     MOVE ZERO TO BAS-IDRUBNR(1)                                          
275500                  BAS-IDRUBNR(2)                                          
275600                  BAS-IDRUBNR(3)                                          
275700                                                                          
275800     PERFORM IMS-GHNP-AVS-RUB                                             
275900     MOVE +1 TO INDX                                                      
276000     PERFORM UNTIL  SEGMENT-SAKNAS OR INDX = +4                           
276100        MOVE RUB-IDRUBNR   TO BAS-IDRUBNR(RUB-IDSEGMNR)                   
276200        ADD +1 TO INDX                                                    
276300        PERFORM IMS-GHNP-AVS-RUB                                          
276400     END-PERFORM                                                          
276500*                                                                         
276600     MOVE BAS-IDRUBNR(1)   TO TEST-IDRUBNR(1)                             
276700     MOVE BAS-IDRUBNR(2)   TO TEST-IDRUBNR(2)                             
276800     MOVE BAS-IDRUBNR(3)   TO TEST-IDRUBNR(3)                             
276900*                                                                         
277000     IF MID-IDRUBNR-UPD(1) NOT = ALL '+'                                  
277100        MOVE MID-IDRUBNR-UPD(1) TO TEST-IDRUBNR(1)                        
277200     END-IF                                                               
277300                                                                          
277400     IF MID-IDRUBNR-UPD(2) NOT = ALL '+'                                  
277500        MOVE MID-IDRUBNR-UPD(2) TO TEST-IDRUBNR(2)                        
277600     END-IF                                                               
277700                                                                          
277800     IF MID-IDRUBNR-UPD(3) NOT = ALL '+'                                  
277900        MOVE MID-IDRUBNR-UPD(3) TO TEST-IDRUBNR(3)                        
278000     END-IF                                                               
278100                                                                          
278200     IF (TEST-BEART NOT = SPACE) OR                                       
278300        (TEST-KDHOM NOT = ZERO) OR                                        
278400        (TEST-IDTTEXNR NOT = ZERO)                                        
278500                                                                          
278600        IF (TEST-IDRUBNR(1) NOT = ZERO) AND                               
278700           (TEST-IDRUBNR(2) NOT = ZERO) AND                               
278800           (TEST-IDRUBNR(3) NOT = ZERO)                                   
278900                                                                          
279000           IF (MID-BEART-UPD = ALL '+' OR = SPACE)                        
279100              CONTINUE                                                    
279200           ELSE                                                           
279300              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR               
279400              MOVE JA TO INDATA-FEL                                       
279500           END-IF                                                         
279600                                                                          
279700           IF (MID-KDHOM-UPD = ALL '+' OR = ZERO)                         
279800              CONTINUE                                                    
279900           ELSE                                                           
280000              MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR                
280100              MOVE JA TO INDATA-FEL                                       
280200           END-IF                                                         
280300        END-IF                                                            
280400     END-IF                                                               
280500     .                                                                    
280600     EJECT                                                                
280700 HE-BAS-LAS-FOTNR SECTION.                                                
280800     SKIP2                                                                
280900     MOVE ZERO TO BAS-IDFOTNR(1)                                          
281000                  BAS-IDFOTNR(2)                                          
281100                  BAS-IDFOTNR(3)                                          
281200                                                                          
281300     PERFORM IMS-GHNP-AVS-FOT                                             
281400     MOVE +1 TO INDX                                                      
281500     PERFORM UNTIL  SEGMENT-SAKNAS OR INDX = +4                           
281600        MOVE FOT-IDFOTNR TO BAS-IDFOTNR(FOT-IDSEGMNR)                     
281700        ADD +1 TO INDX                                                    
281800        PERFORM IMS-GHNP-AVS-FOT                                          
281900     END-PERFORM                                                          
282000     .                                                                    
282100     EJECT                                                                
282200 HF-BAS-KOMB-KDHAEN SECTION.                                              
282300     SKIP2                                                                
282400*- - - - - - - - - - - KDHAEN KOMBINATIONER                               
282500     IF MID-KDHAEN-UPD NOT = ALL '+'                                      
282600        IF MID-KDHAEN-UPD = 'B' OR 'A'                                    
282700           IF (MID-IDCATGRP-H-UPD = ALL '+') AND                          
282800              (MID-IDCATAVS-H-UPD = ALL '+') AND                          
282900              (MID-IDCATRAD-H-UPD = ALL '+') AND                          
283000              (MID-KDCATPUB-R-H-UPD = ALL '+')                            
283100              PERFORM HFA-GAMMAL                                          
283200           ELSE                                                           
283300              IF (MID-IDCATGRP-H-UPD NOT = ALL '+') AND                   
283400                 (MID-IDCATAVS-H-UPD NOT = ALL '+') AND                   
283500                 (MID-IDCATRAD-H-UPD NOT = ALL '+') AND                   
283600                 (MID-KDCATPUB-R-H-UPD NOT = ALL '+')                     
283700                 PERFORM HFB-NY                                           
283800              ELSE                                                        
283900                MOVE +2 TO W-IDSEGMNR                                     
284000                PERFORM IMS-GHNP-AVS-NOT-SEGM                             
284100                                                                          
284200                IF SEGMENT-FINNS                                          
284300                   PERFORM HFC-AENDR-NOT                                  
284400                ELSE                                                      
284500                   PERFORM IMS-GHNP-AVS-HAEN                              
284600                                                                          
284700                   IF SEGMENT-FINNS                                       
284800                      PERFORM HFD-AENDR-HAEN                              
284900                   ELSE                                                   
285000                       IF MID-IDCATGRP-H-UPD NOT = ALL '+'                
285100                          PERFORM HFE-NY-DELAR                            
285200                       ELSE                                               
285300                          MOVE MFS-ALFA-FAELT-FEL TO                      
285400                               MOD-KDHAEN-UPD-ATTR                        
285500                          MOVE JA TO INDATA-FEL                           
285600                       END-IF                                             
285700                                                                          
285800                       IF INDATA-FEL = NEJ                                
285900                          PERFORM S01-KOLLA-WDN501-12                     
286000*                         Kollar om hänvisad rad finns                    
286100                       END-IF                                             
286200                    END-IF                                                
286300                 END-IF                                                   
286400              END-IF                                                      
286500           END-IF                                                         
286600        END-IF                                                            
286700     ELSE                                                                 
286800        IF (MID-IDCATGRP-H-UPD = ALL '+') AND                             
286900           (MID-IDCATAVS-H-UPD = ALL '+') AND                             
287000           (MID-IDCATRAD-H-UPD = ALL '+') AND                             
287100           (MID-KDCATPUB-R-H-UPD = ALL '+')                               
287200           CONTINUE                                                       
287300        ELSE                                                              
287400           MOVE +2 TO W-IDSEGMNR                                          
287500           PERFORM IMS-GHNP-AVS-NOT-SEGM                                  
287600           IF SEGMENT-FINNS                                               
287700              PERFORM HFF-AENDR-NOT-DATA                                  
287800           ELSE                                                           
287900              PERFORM IMS-GHNP-AVS-HAEN                                   
288000              IF SEGMENT-FINNS                                            
288100                 PERFORM HFG-AENDR-HAEN-DATA                              
288200                                                                          
288300                 IF INDATA-FEL = NEJ                                      
288400                    PERFORM S01-KOLLA-WDN501-12                           
288500*                   Kollar om hänvisad rad finns                          
288600                 END-IF                                                   
288700              ELSE                                                        
288800                 PERFORM HFH-FLYTTA-FEL                                   
288900              END-IF                                                      
289000           END-IF                                                         
289100        END-IF                                                            
289200     END-IF                                                               
289300     .                                                                    
289400     EJECT                                                                
289500 HFA-GAMMAL SECTION.                                                      
289600     SKIP2                                                                
289700     MOVE +2 TO W-IDSEGMNR                                                
289800     PERFORM IMS-GHNP-AVS-NOT-SEGM                                        
289900                                                                          
290000     IF SEGMENT-FINNS                                                     
290100        MOVE NOT-IDCATGRP   TO WS-IDCATGRP-H                              
290200        MOVE NOT-IDCATAVS   TO WS-IDCATAVS-H                              
290300        MOVE NOT-IDCATRAD   TO WS-IDCATRAD-H                              
290400        MOVE NOT-KDCATPUB-FOM TO WS-KDCATPUB-H                            
290500        PERFORM S01-KOLLA-WDN501-12                                       
290600*       Kollar om hänvisad rad finns                                      
290700     ELSE                                                                 
290800        PERFORM IMS-GHNP-AVS-HAEN                                         
290900                                                                          
291000        IF SEGMENT-SAKNAS                                                 
291100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                 
291200           MOVE JA TO INDATA-FEL                                          
291300        ELSE                                                              
291400           MOVE HAEN-IDCATGRP  TO WS-IDCATGRP-H                           
291500           MOVE HAEN-IDCATAVS  TO WS-IDCATAVS-H                           
291600           MOVE HAEN-IDCATRAD  TO WS-IDCATRAD-H                           
291700           MOVE HAEN-KDCATPUB-FOM TO WS-KDCATPUB-H                        
291800        END-IF                                                            
291900     END-IF                                                               
292000     .                                                                    
292100     EJECT                                                                
292200 HFB-NY SECTION.                                                          
292300     SKIP2                                                                
292400     MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                             
292500     MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                             
292600     MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                             
292700     MOVE  WS-KDCATPUB-H-UPD TO WS-KDCATPUB-H                             
292800     PERFORM S01-KOLLA-WDN501-12                                          
292900*       Kollar om hänvisad rad finns                                      
293000     .                                                                    
293100     EJECT                                                                
293200 HFC-AENDR-NOT SECTION.                                                   
293300     SKIP2                                                                
293400     MOVE NOT-IDCATGRP TO WS-IDCATGRP-H                                   
293500     MOVE NOT-IDCATAVS TO WS-IDCATAVS-H                                   
293600     MOVE NOT-IDCATRAD TO WS-IDCATRAD-H                                   
293700     MOVE NOT-KDCATPUB-FOM TO WS-KDCATPUB-H                               
293800                                                                          
293900     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
294000        MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                          
294100     END-IF                                                               
294200                                                                          
294300     IF MID-IDCATAVS-H-UPD NOT = ALL '+'                                  
294400        MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                          
294500     END-IF                                                               
294600                                                                          
294700     IF MID-IDCATRAD-H-UPD NOT = ALL '+'                                  
294800        MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                          
294900     END-IF                                                               
295000                                                                          
295100     IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                                
295200        MOVE  WS-KDCATPUB-H-UPD TO WS-KDCATPUB-H                          
295300     END-IF                                                               
295400                                                                          
295500     PERFORM S01-KOLLA-WDN501-12                                          
295600*       Kollar om hänvisad rad finns                                      
295700     .                                                                    
295800     EJECT                                                                
295900 HFD-AENDR-HAEN SECTION.                                                  
296000     SKIP2                                                                
296100     MOVE HAEN-IDCATGRP TO WS-IDCATGRP-H                                  
296200     MOVE HAEN-IDCATAVS TO WS-IDCATGRP-H                                  
296300     MOVE HAEN-IDCATRAD TO WS-IDCATRAD-H                                  
296400     MOVE HAEN-KDCATPUB-FOM TO WS-KDCATPUB-H                              
296500                                                                          
296600     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
296700        MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                          
296800     END-IF                                                               
296900                                                                          
297000     IF MID-IDCATAVS-H-UPD NOT = ALL '+'                                  
297100        MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                          
297200     END-IF                                                               
297300                                                                          
297400     IF MID-IDCATRAD-H-UPD NOT = ALL '+'                                  
297500        MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                          
297600     END-IF                                                               
297700                                                                          
297800     IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                                
297900        MOVE  WS-KDCATPUB-H-UPD TO WS-KDCATPUB-H                          
298000     END-IF                                                               
298100                                                                          
298200     PERFORM S01-KOLLA-WDN501-12                                          
298300*       Kollar om hänvisad rad finns                                      
298400     .                                                                    
298500     EJECT                                                                
298600 HFE-NY-DELAR SECTION.                                                    
298700     SKIP2                                                                
298800     MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                             
298900                                                                          
299000     IF MID-IDCATRAD-H-UPD = ALL '+'                                      
299100        MOVE     ZERO  TO WS-IDCATRAD-H                                   
299200        MOVE LOW-VALUE TO WS-KDCATPUB-H                                   
299300                                                                          
299400        IF MID-IDCATAVS-H-UPD = ALL '+'                                   
299500           MOVE      ZERO         TO WS-IDCATAVS-H                        
299600        ELSE                                                              
299700           MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                       
299800        END-IF                                                            
299900     ELSE                                                                 
300000        IF MID-KDCATPUB-R-H-UPD = ALL '+'                                 
300100          MOVE    LOW-VALUE       TO WS-KDCATPUB-H                        
300200        ELSE                                                              
300300          MOVE  WS-KDCATPUB-H-UPD TO WS-KDCATPUB-H                        
300400        END-IF                                                            
300500                                                                          
300600        IF MID-IDCATAVS-H-UPD = ALL '+'                                   
300700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                 
300800           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDCATAVS-H-UPD-ATTR             
300900           MOVE JA TO INDATA-FEL                                          
301000        ELSE                                                              
301100           IF WS-KDCATPUB-H > LOW-VALUE                                   
301200           AND MID-IDCATAVS-H-UPD = ZERO                                  
301300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-H-UPD-ATTR        
301400              MOVE JA TO INDATA-FEL                                       
301500           ELSE                                                           
301600              MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                    
301700              MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                    
301800           END-IF                                                         
301900        END-IF                                                            
302000     END-IF                                                               
302100     .                                                                    
302200     EJECT                                                                
302300 HFF-AENDR-NOT-DATA SECTION.                                              
302400     SKIP2                                                                
302500     IF  MID-IDCATRAD-H-UPD = ALL '+'                                     
302600     AND MID-KDCATPUB-R-H-UPD = ALL '+'                                   
302700       CONTINUE                                                           
302800     ELSE                                                                 
302900       IF  (MID-IDCATAVS-H-UPD = ALL '+')                                 
303000       AND (NOT-IDCATAVS = 0)                                             
303100         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                   
303200         MOVE JA TO INDATA-FEL                                            
303300       END-IF                                                             
303400     END-IF                                                               
303500     .                                                                    
303600     EJECT                                                                
303700 HFG-AENDR-HAEN-DATA SECTION.                                             
303800     SKIP2                                                                
303900     MOVE HAEN-IDCATGRP     TO WS-IDCATGRP-H                              
304000     MOVE HAEN-IDCATAVS     TO WS-IDCATAVS-H                              
304100     MOVE HAEN-IDCATRAD     TO WS-IDCATRAD-H                              
304200     MOVE HAEN-KDCATPUB-FOM TO WS-KDCATPUB-H                              
304300                                                                          
304400     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
304500        MOVE MID-IDCATGRP-H-UPD TO WS-IDCATGRP-H                          
304600     END-IF                                                               
304700                                                                          
304800     IF MID-IDCATAVS-H-UPD = ALL '+'                                      
304900       IF  MID-IDCATRAD-H-UPD = ALL '+'                                   
305000       AND MID-KDCATPUB-R-H-UPD = ALL '+'                                 
305100         CONTINUE                                                         
305200       ELSE                                                               
305300         IF MID-IDCATRAD-H-UPD NOT = ALL '+'                              
305400           IF HAEN-IDCATAVS = ZERO                                        
305500             MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-H-UPD-ATTR            
305600             IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                        
305700             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-H-UPD-ATTR         
305800             END-IF                                                       
305900             MOVE JA TO INDATA-FEL                                        
306000           ELSE                                                           
306100             MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                     
306200                                                                          
306300             IF MID-KDCATPUB-R-H-UPD = ALL '+'                            
306400             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-H-UPD-ATTR         
306500               MOVE JA TO INDATA-FEL                                      
306600             ELSE                                                         
306700               MOVE  WS-KDCATPUB-H-UPD TO WS-KDCATPUB-H                   
306800             END-IF                                                       
306900           END-IF                                                         
307000         ELSE                                                             
307100           IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                          
307200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-H-UPD-ATTR            
307300             MOVE JA TO INDATA-FEL                                        
307400           END-IF                                                         
307500         END-IF                                                           
307600       END-IF                                                             
307700     ELSE                                                                 
307800       MOVE MID-IDCATAVS-H-UPD TO WS-IDCATAVS-H                           
307900                                                                          
308000       IF  MID-IDCATRAD-H-UPD = ALL '+'                                   
308100       AND MID-KDCATPUB-R-H-UPD = ALL '+'                                 
308200         CONTINUE                                                         
308300       ELSE                                                               
308400         IF MID-IDCATRAD-H-UPD = ALL '+'                                  
308500           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDCATRAD-H-UPD-ATTR             
308600           MOVE JA TO INDATA-FEL                                          
308700         ELSE                                                             
308800           MOVE MID-IDCATRAD-H-UPD TO WS-IDCATRAD-H                       
308900         END-IF                                                           
309000                                                                          
309100         IF MID-KDCATPUB-R-H-UPD = ALL '+'                                
309200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-H-UPD-ATTR           
309300           MOVE JA TO INDATA-FEL                                          
309400         ELSE                                                             
309500           MOVE  WS-KDCATPUB-H-UPD TO WS-KDCATPUB-H                       
309600         END-IF                                                           
309700       END-IF                                                             
309800     END-IF                                                               
309900     .                                                                    
310000     EJECT                                                                
310100 HFH-FLYTTA-FEL SECTION.                                                  
310200     SKIP2                                                                
310300     IF MID-IDCATGRP-H-UPD NOT = ALL '+'                                  
310400        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-H-UPD-ATTR                 
310500        MOVE JA TO INDATA-FEL                                             
310600     END-IF                                                               
310700     IF MID-IDCATAVS-H-UPD NOT = ALL '+'                                  
310800        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATAVS-H-UPD-ATTR                 
310900        MOVE JA TO INDATA-FEL                                             
311000     END-IF                                                               
311100     IF MID-IDCATRAD-H-UPD NOT = ALL '+'                                  
311200        MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATRAD-H-UPD-ATTR                 
311300        MOVE JA TO INDATA-FEL                                             
311400     END-IF                                                               
311500     IF MID-KDCATPUB-R-H-UPD NOT = ALL '+'                                
311600        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-H-UPD-ATTR              
311700        MOVE JA TO INDATA-FEL                                             
311800     END-IF                                                               
311900     .                                                                    
312000     EJECT                                                                
312100 HG-SLUT-KOMB-MED-BAS SECTION.                                            
312200     SKIP2                                                                
312300*- - - - - - - - - - - LIKA IDRUBNR                                       
312400     IF IDRUBNR-FINNS = JA                                                
312500        IF TEST-IDRUBNR(1) NOT = ZERO                                     
312600           IF TEST-IDRUBNR(1) = TEST-IDRUBNR(2)                           
312700              IF MID-IDRUBNR-UPD(2) NOT = ALL '+'                         
312800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(2)        
312900                 MOVE JA TO INDATA-FEL                                    
313000              ELSE                                                        
313100                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(1)        
313200                 MOVE JA TO INDATA-FEL                                    
313300              END-IF                                                      
313400           END-IF                                                         
313500           IF TEST-IDRUBNR(1) = TEST-IDRUBNR(3)                           
313600              IF MID-IDRUBNR-UPD(3) NOT = ALL '+'                         
313700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(3)        
313800                 MOVE JA TO INDATA-FEL                                    
313900              ELSE                                                        
314000                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-UPD-ATTR(1)        
314100                 MOVE JA TO INDATA-FEL                                    
314200              END-IF                                                      
314300           END-IF                                                         
314400           IF TEST-IDRUBNR(2) NOT = ZERO                                  
314500              IF TEST-IDRUBNR(2) = TEST-IDRUBNR(3)                        
314600                 IF MID-IDRUBNR-UPD(3) NOT = ALL '+'                      
314700                    MOVE MFS-NUM-FAELT-FEL                                
314800                                     TO MOD-IDRUBNR-UPD-ATTR(3)           
314900                    MOVE JA TO INDATA-FEL                                 
315000                 ELSE                                                     
315100                    MOVE MFS-NUM-FAELT-FEL                                
315200                                     TO MOD-IDRUBNR-UPD-ATTR(2)           
315300                    MOVE JA TO INDATA-FEL                                 
315400                 END-IF                                                   
315500              END-IF                                                      
315600           END-IF                                                         
315700        ELSE                                                              
315800           IF TEST-IDRUBNR(2) NOT = ZERO                                  
315900              IF TEST-IDRUBNR(2) = TEST-IDRUBNR(3)                        
316000                 IF MID-IDRUBNR-UPD(3) NOT = ALL '+'                      
316100                    MOVE MFS-NUM-FAELT-FEL                                
316200                                      TO MOD-IDRUBNR-UPD-ATTR(3)          
316300                    MOVE JA TO INDATA-FEL                                 
316400                 ELSE                                                     
316500                    MOVE MFS-NUM-FAELT-FEL                                
316600                                      TO MOD-IDRUBNR-UPD-ATTR(2)          
316700                    MOVE JA TO INDATA-FEL                                 
316800                 END-IF                                                   
316900              END-IF                                                      
317000           END-IF                                                         
317100        END-IF                                                            
317200     END-IF                                                               
317300                                                                          
317400     MOVE BAS-IDFOTNR(1)   TO TEST-IDFOTNR(1)                             
317500     MOVE BAS-IDFOTNR(2)   TO TEST-IDFOTNR(2)                             
317600     MOVE BAS-IDFOTNR(3)   TO TEST-IDFOTNR(3)                             
317700                                                                          
317800     IF MID-IDFOTNR-UPD(1) NOT = ALL '+'                                  
317900        MOVE MID-IDFOTNR-UPD(1) TO TEST-IDFOTNR(1)                        
318000     END-IF                                                               
318100                                                                          
318200     IF MID-IDFOTNR-UPD(2) NOT = ALL '+'                                  
318300        MOVE MID-IDFOTNR-UPD(2) TO TEST-IDFOTNR(2)                        
318400     END-IF                                                               
318500                                                                          
318600     IF MID-IDFOTNR-UPD(3) NOT = ALL '+'                                  
318700        MOVE MID-IDFOTNR-UPD(3) TO TEST-IDFOTNR(3)                        
318800     END-IF                                                               
318900                                                                          
319000*- - - - - - - - - - - LIKA IDFOTNR                                       
319100     IF IDFOTNR-FINNS = JA                                                
319200        IF TEST-IDFOTNR(1) NOT = ZERO                                     
319300           IF TEST-IDFOTNR(1) = TEST-IDFOTNR(2)                           
319400              IF MID-IDFOTNR-UPD(2) NOT = ALL '+'                         
319500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(2)        
319600                 MOVE JA TO INDATA-FEL                                    
319700              ELSE                                                        
319800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(1)        
319900                 MOVE JA TO INDATA-FEL                                    
320000              END-IF                                                      
320100           END-IF                                                         
320200           IF TEST-IDFOTNR(1) = TEST-IDFOTNR(3)                           
320300              IF MID-IDFOTNR-UPD(3) NOT = ALL '+'                         
320400                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(3)        
320500                 MOVE JA TO INDATA-FEL                                    
320600              ELSE                                                        
320700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-UPD-ATTR(1)        
320800                 MOVE JA TO INDATA-FEL                                    
320900              END-IF                                                      
321000           END-IF                                                         
321100           IF TEST-IDFOTNR(2) NOT = ZERO                                  
321200              IF TEST-IDFOTNR(2) = TEST-IDFOTNR(3)                        
321300                 IF MID-IDFOTNR-UPD(3) NOT = ALL '+'                      
321400                    MOVE MFS-NUM-FAELT-FEL                                
321500                                      TO MOD-IDFOTNR-UPD-ATTR(3)          
321600                    MOVE JA TO INDATA-FEL                                 
321700                 ELSE                                                     
321800                    MOVE MFS-NUM-FAELT-FEL                                
321900                                      TO MOD-IDFOTNR-UPD-ATTR(2)          
322000                    MOVE JA TO INDATA-FEL                                 
322100                 END-IF                                                   
322200              END-IF                                                      
322300           END-IF                                                         
322400        ELSE                                                              
322500           IF TEST-IDFOTNR(2) NOT = ZERO                                  
322600              IF TEST-IDFOTNR(2) = TEST-IDFOTNR(3)                        
322700                 IF MID-IDFOTNR-UPD(3) NOT = ALL '+'                      
322800                    MOVE MFS-NUM-FAELT-FEL                                
322900                                      TO MOD-IDFOTNR-UPD-ATTR(3)          
323000                    MOVE JA TO INDATA-FEL                                 
323100                 ELSE                                                     
323200                    MOVE MFS-NUM-FAELT-FEL                                
323300                                      TO MOD-IDFOTNR-UPD-ATTR(2)          
323400                    MOVE JA TO INDATA-FEL                                 
323500                 END-IF                                                   
323600              END-IF                                                      
323700           END-IF                                                         
323800        END-IF                                                            
323900     END-IF                                                               
324000                                                                          
324100*- - - - - - - - - - - IDRUBNR GODKÄNDA                                   
324200     IF IDRUBNR-FINNS = JA AND INDATA-FEL = NEJ                           
324300        MOVE +1 TO INDX                                                   
324400        PERFORM UNTIL INDX = +4                                           
324500           IF IN-IDRUBNR(INDX) NOT = ZERO                                 
324600              MOVE IN-IDRUBNR(INDX) TO W-IDRUBNR                          
324700              PERFORM IMS-GU-RUB                                          
324800                                                                          
324900              IF SEGMENT-FINNS                                            
325000                 IF (TEST-IDRUBNR(2) NOT = ZERO) OR                       
325100                    (TEST-IDRUBNR(3) NOT = ZERO)                          
325200                                                                          
325300                    IF RUB-RUB-FLKOMBINERAS = ' ' OR 'N'                  
325400                       MOVE MFS-NUM-FAELT-FEL TO                          
325500                            MOD-IDRUBNR-UPD-ATTR(INDX)                    
325600                       MOVE JA TO INDATA-FEL                              
325700                    END-IF                                                
325800                 END-IF                                                   
325900              ELSE                                                        
326000                 MOVE MFS-NUM-FAELT-FEL TO                                
326100                      MOD-IDRUBNR-UPD-ATTR(INDX)                          
326200                 MOVE JA TO INDATA-FEL                                    
326300              END-IF                                                      
326400           END-IF                                                         
326500           ADD +1 TO INDX                                                 
326600        END-PERFORM                                                       
326700     END-IF                                                               
326800                                                                          
326900*- - - - - - - - - - - IDFOTNR GODKÄNDA                                   
327000     IF IDFOTNR-FINNS = JA AND INDATA-FEL = NEJ                           
327100        MOVE +1 TO INDX                                                   
327200        PERFORM UNTIL INDX = +4                                           
327300           IF IN-IDFOTNR(INDX) NOT = ZERO                                 
327400              MOVE IN-IDFOTNR(INDX) TO W-IDFOTNR                          
327500              PERFORM IMS-GU-FOT                                          
327600                                                                          
327700              IF SEGMENT-SAKNAS                                           
327800                 MOVE MFS-NUM-FAELT-FEL TO                                
327900                      MOD-IDFOTNR-UPD-ATTR(INDX)                          
328000                 MOVE JA TO INDATA-FEL                                    
328100              END-IF                                                      
328200           END-IF                                                         
328300           ADD +1 TO INDX                                                 
328400        END-PERFORM                                                       
328500     END-IF                                                               
328600                                                                          
328700*- - - - - - - - - - - IDTTEXNR GODKÄNDA                                  
328800     IF MID-IDTTEXNR-UPD NOT = ALL '+'                                    
328900        IF MID-IDTTEXNR-UPD NOT = ZERO                                    
329000           MOVE MID-IDTTEXNR-UPD TO W-IDTTEXNR                            
329100           PERFORM IMS-GU-TEXT                                            
329200                                                                          
329300           IF SEGMENT-SAKNAS                                              
329400              MOVE MFS-NUM-FAELT-FEL TO MOD-IDTTEXNR-UPD-ATTR             
329500              MOVE JA TO INDATA-FEL                                       
329600           END-IF                                                         
329700        END-IF                                                            
329800     END-IF                                                               
329900     .                                                                    
330000     EJECT                                                                
330100 HH-KOMB-IDARTNR-IDRUBNR SECTION.                                         
330200     SKIP2                                                                
330300*    Artikelnummer plus rubriknummer på samma rad - förbjudet             
330400     IF TEST-IDARTNR NOT = ZERO                                           
330500        MOVE +1 TO INDX                                                   
330600        PERFORM UNTIL INDX = +4                                           
330700           IF TEST-IDRUBNR(INDX) NOT = ZERO                               
330800              IF MID-IDARTNR-UPD = ALL '+' OR ZERO                        
330900                 CONTINUE                                                 
331000              ELSE                                                        
331100                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR           
331200                 MOVE JA TO INDATA-FEL                                    
331300              END-IF                                                      
331400              IF MID-IDRUBNR-UPD(INDX) = ALL '+' OR ZERO                  
331500                 CONTINUE                                                 
331600              ELSE                                                        
331700                 MOVE MFS-NUM-FAELT-FEL                                   
331800                                  TO MOD-IDRUBNR-UPD-ATTR(INDX)           
331900                 MOVE JA TO INDATA-FEL                                    
332000              END-IF                                                      
332100           END-IF                                                         
332200           ADD +1 TO INDX                                                 
332300        END-PERFORM                                                       
332400     END-IF                                                               
332500     .                                                                    
332600     EJECT                                                                
332700 I-AENDRING SECTION.                                                      
332800     SKIP2                                                                
332900     MOVE NEJ TO UPPDAT-GJORD                                             
333000                                                                          
333100     PERFORM IMS-GHU-AVS-RAD                                              
333200     IF SEGMENT-FINNS                                                     
333300        MOVE W-IDCATRAD        TO RAD-IDCATRAD                            
333400        MOVE W-KDCATPUB        TO RAD-KDCATPUB-FOM                        
333500        MOVE TEST-KDCATPUB-TOM TO RAD-KDCATPUB-TOM                        
333600        MOVE MSG-SIGNON-USERID TO RAD-IDUSER                              
333700        MOVE DAGENS-DATUM-TIAAMMDD TO RAD-TIUPPDAT                        
333800        MOVE 'Ä'               TO RAD-KDRADST                             
333900        PERFORM IMS-REPL-AVS                                              
334000                                                                          
334100        IF  (MID-KDFBX-UPD    = ALL '+')                                  
334200        AND (MID-IDCATPOS-UPD = ALL '+')                                  
334300        AND (MID-IDARTNR-UPD  = ALL '+')                                  
334400        AND (KVKOL-FINNS      = NEJ    )                                  
334500        AND (MID-KDPS-UPD     = ALL '+')                                  
334600        AND (MID-KVPUNKT-UPD  = ALL '+')                                  
334700        AND (MID-IDTTEXNR-UPD = ALL '+')                                  
334800           CONTINUE                                                       
334900        ELSE                                                              
335000           PERFORM IA-AENDR-ART                                           
335100        END-IF                                                            
335200                                                                          
335300        IF MID-TEKATANM-UPD NOT = ALL '+'                                 
335400           PERFORM IB-AENDR-TEXT                                          
335500        END-IF                                                            
335600                                                                          
335700        IF  (MID-KDHOM-UPD = ALL '+')                                     
335800        AND (MID-BEART-UPD = ALL '+')                                     
335900           CONTINUE                                                       
336000        ELSE                                                              
336100           PERFORM IC-UPD-AVS-BEN                                         
336200        END-IF                                                            
336300                                                                          
336400        IF HAEN-FINNS = JA                                                
336500           IF MID-KDHAEN-UPD NOT = ALL '+'                                
336600              PERFORM ID-UPD-AVS-NOT                                      
336700           ELSE                                                           
336800              MOVE +2 TO W-IDSEGMNR                                       
336900              PERFORM IMS-GHNP-AVS-NOT-SEGM                               
337000              PERFORM IE-UPD-AVS-NOT                                      
337100           END-IF                                                         
337200        END-IF                                                            
337300                                                                          
337400        IF IDRUBNR-FINNS = JA                                             
337500           PERFORM IF-UPD-AVS-RUB                                         
337600        END-IF                                                            
337700                                                                          
337800        IF IDFOTNR-FINNS = JA                                             
337900           PERFORM IG-UPD-AVS-FOT                                         
338000        END-IF                                                            
338100                                                                          
338200        IF HAEN-FINNS = JA                                                
338300           IF MID-KDHAEN-UPD NOT = ALL '+'                                
338400              PERFORM IH-AENDR-HAEN                                       
338500           ELSE                                                           
338600              PERFORM II-UPD-AVS-HAEN                                     
338700           END-IF                                                         
338800        END-IF                                                            
338900     END-IF                                                               
339000                                                                          
339100     IF UPPDAT-GJORD = JA                                                 
339200        MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                             
339300     END-IF                                                               
339400     .                                                                    
339500     EJECT                                                                
339600 IA-AENDR-ART SECTION.                                                    
339700     SKIP2                                                                
339800     PERFORM IMS-GHNP-AVS-ART                                             
339900     IF SEGMENT-SAKNAS                                                    
340000        PERFORM IAA-REG-AVS-ART                                           
340100        PERFORM IMS-ISRT-AVS-ART                                          
340200        MOVE JA TO UPPDAT-GJORD                                           
340300     ELSE                                                                 
340400        MOVE NEJ TO BORT-FLAGGA                                           
340500        PERFORM IAB-KOLLA-DELETE                                          
340600                                                                          
340700        IF BORT-FLAGGA = JA                                               
340800           PERFORM IMS-DLET-AVS                                           
340900           MOVE JA TO UPPDAT-GJORD                                        
341000        ELSE                                                              
341100           PERFORM IAC-AENDR-AVS-ART                                      
341200           PERFORM IMS-REPL-AVS                                           
341300           MOVE JA TO UPPDAT-GJORD                                        
341400        END-IF                                                            
341500     END-IF                                                               
341600     .                                                                    
341700     EJECT                                                                
341800 IAA-REG-AVS-ART SECTION.                                                 
341900     SKIP2                                                                
342000     MOVE '1'               TO ART-KDSEGKEY                               
342100     MOVE TEST-KDFBX        TO ART-KDFBX                                  
342200     MOVE TEST-IDCATPOS-HEL TO ART-IDCATPOS                               
342300     MOVE TEST-IDARTNR      TO ART-IDARTNR                                
342400                                                                          
342500     IF KVKOL-FINNS = NEJ                                                 
342600        MOVE SPACE TO ART-KVKOL(1)                                        
342700                      ART-KVKOL(2)                                        
342800                      ART-KVKOL(3)                                        
342900                      ART-KVKOL(4)                                        
343000                      ART-KVKOL(5)                                        
343100     ELSE                                                                 
343200        MOVE +1 TO INDX                                                   
343300        PERFORM UNTIL INDX = +6                                           
343400           IF MID-KVKOL-UPD(INDX) = ALL '+'                               
343500              MOVE SPACE TO ART-KVKOL(INDX)                               
343600           ELSE                                                           
343700              MOVE MID-KVKOL-UPD(INDX) TO ART-KVKOL(INDX)                 
343800           END-IF                                                         
343900           ADD +1 TO INDX                                                 
344000        END-PERFORM                                                       
344100     END-IF                                                               
344200                                                                          
344300     MOVE TEST-KDPS              TO ART-KDPS                              
344400     MOVE TEST-KVPUNKT           TO ART-KVPUNKT                           
344500     MOVE TEST-IDTTEXNR          TO ART-IDTTEXNR                          
344600     .                                                                    
344700     EJECT                                                                
344800 IAB-KOLLA-DELETE SECTION.                                                
344900     SKIP2                                                                
345000     IF  (TEST-KDFBX        = SPACE)                                      
345100     AND (TEST-IDCATPOS-HEL = SPACE)                                      
345200     AND (TEST-IDARTNR      = ZERO)                                       
345300     AND (TEST-KVKOL(1)     = SPACE)                                      
345400     AND (TEST-KVKOL(2)     = SPACE)                                      
345500     AND (TEST-KVKOL(3)     = SPACE)                                      
345600     AND (TEST-KVKOL(4)     = SPACE)                                      
345700     AND (TEST-KVKOL(5)     = SPACE)                                      
345800     AND (TEST-KDPS         = SPACE)                                      
345900     AND (TEST-KVPUNKT      = ZERO)                                       
346000     AND (TEST-IDTTEXNR     = ZERO)                                       
346100         MOVE JA TO BORT-FLAGGA                                           
346200     END-IF                                                               
346300     .                                                                    
346400     EJECT                                                                
346500 IAC-AENDR-AVS-ART SECTION.                                               
346600     SKIP2                                                                
346700     IF MID-KDFBX-UPD NOT = ALL '+'                                       
346800        MOVE TEST-KDFBX       TO ART-KDFBX                                
346900     END-IF                                                               
347000                                                                          
347100     IF MID-IDCATPOS-UPD NOT = ALL '+'                                    
347200        MOVE TEST-IDCATPOS-HEL  TO ART-IDCATPOS                           
347300     END-IF                                                               
347400                                                                          
347500     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
347600        MOVE TEST-IDARTNR     TO ART-IDARTNR                              
347700     END-IF                                                               
347800                                                                          
347900     IF KVKOL-FINNS = JA                                                  
348000        MOVE +1 TO IX                                                     
348100        PERFORM UNTIL IX = +6                                             
348200           IF TEST-KVKOL(IX) = SPACE                                      
348300             MOVE    SPACE         TO ART-KVKOL(IX)                       
348400           ELSE                                                           
348500             IF TEST-KVKOL(IX)(1:1) = SPACE                               
348600               IF TEST-KVKOL(IX)(2:1) = SPACE                             
348700                 MOVE TEST-KVKOL(IX)(3:1) TO ART-KVKOL(IX)                
348800               ELSE                                                       
348900                 IF TEST-KVKOL(IX)(3:1) = SPACE                           
349000                   MOVE TEST-KVKOL(IX)(2:1) TO ART-KVKOL(IX)              
349100                 ELSE                                                     
349200                   MOVE TEST-KVKOL(IX)(2:2) TO ART-KVKOL(IX)              
349300                 END-IF                                                   
349400               END-IF                                                     
349500             ELSE                                                         
349600               IF TEST-KVKOL(IX)(2:1) = SPACE                             
349700                 MOVE TEST-KVKOL(IX)(1:1) TO ART-KVKOL(IX)                
349800               ELSE                                                       
349900                 MOVE TEST-KVKOL(IX)      TO ART-KVKOL(IX)                
350000               END-IF                                                     
350100             END-IF                                                       
350200           END-IF                                                         
350300           ADD +1 TO IX                                                   
350400        END-PERFORM                                                       
350500     END-IF                                                               
350600                                                                          
350700     IF MID-KDPS-UPD NOT = ALL '+'                                        
350800        MOVE TEST-KDPS        TO ART-KDPS                                 
350900     END-IF                                                               
351000                                                                          
351100     IF MID-KVPUNKT-UPD NOT = ALL '+'                                     
351200        MOVE TEST-KVPUNKT     TO ART-KVPUNKT                              
351300     END-IF                                                               
351400                                                                          
351500     IF MID-IDTTEXNR-UPD NOT = ALL '+'                                    
351600        MOVE TEST-IDTTEXNR    TO ART-IDTTEXNR                             
351700     END-IF                                                               
351800     .                                                                    
351900     EJECT                                                                
352000 IB-AENDR-TEXT SECTION.                                                   
352100     SKIP2                                                                
352200     IF MID-TEKATANM-UPD = SPACE                                          
352300        PERFORM IMS-GHNP-AVS-TEXT                                         
352400                                                                          
352500        IF SEGMENT-FINNS                                                  
352600           PERFORM IMS-DLET-AVS                                           
352700           MOVE JA TO UPPDAT-GJORD                                        
352800        END-IF                                                            
352900     ELSE                                                                 
353000        PERFORM IMS-GHNP-AVS-TEXT                                         
353100        MOVE       '1'        TO TEXT-KDSEGKEY                            
353200        MOVE SPACE            TO TEXT-BERUBTEXT                           
353300        MOVE MID-TEKATANM-UPD TO TEXT-TEKATANM                            
353400                                                                          
353500        IF SEGMENT-SAKNAS                                                 
353600           PERFORM IMS-ISRT-AVS-TEXT                                      
353700           MOVE JA TO UPPDAT-GJORD                                        
353800        ELSE                                                              
353900           PERFORM IMS-REPL-AVS                                           
354000           MOVE JA TO UPPDAT-GJORD                                        
354100        END-IF                                                            
354200     END-IF                                                               
354300     .                                                                    
354400     EJECT                                                                
354500 IC-UPD-AVS-BEN SECTION.                                                  
354600     SKIP2                                                                
354700     PERFORM IMS-GHNP-AVS-BEN                                             
354800     MOVE     '1'    TO BEN-KDSEGKEY                                      
354900     MOVE TEST-BEART TO BEN-BEART                                         
355000     MOVE TEST-KDHOM TO BEN-KDHOM                                         
355100                                                                          
355200     IF SEGMENT-FINNS                                                     
355300        IF TEST-KDPS = 'XX'                                               
355400           MOVE ZERO TO BEN-KDHOM                                         
355500           PERFORM IMS-REPL-AVS                                           
355600           MOVE JA TO UPPDAT-GJORD                                        
355700        ELSE                                                              
355800           IF TEST-KDPS = 'LS' OR 'KL'                                    
355900              PERFORM IMS-REPL-AVS                                        
356000              MOVE JA TO UPPDAT-GJORD                                     
356100           ELSE                                                           
356200              IF TEST-KDPS = 'NS' OR 'KN' OR '  '                         
356300                 IF TEST-BEART = SPACE                                    
356400                    PERFORM IMS-DLET-AVS                                  
356500                    MOVE JA TO UPPDAT-GJORD                               
356600                 ELSE                                                     
356700                    PERFORM IMS-REPL-AVS                                  
356800                    MOVE JA TO UPPDAT-GJORD                               
356900                 END-IF                                                   
357000              ELSE                                                        
357100                 PERFORM IMS-DLET-AVS                                     
357200                 MOVE JA TO UPPDAT-GJORD                                  
357300              END-IF                                                      
357400           END-IF                                                         
357500        END-IF                                                            
357600     ELSE                                                                 
357700        IF TEST-KDPS = 'XX'                                               
357800           MOVE ZERO TO BEN-KDHOM                                         
357900           PERFORM IMS-ISRT-AVS-BEN                                       
358000           MOVE JA TO UPPDAT-GJORD                                        
358100        ELSE                                                              
358200           IF TEST-KDPS = 'LS' OR 'KL'                                    
358300              PERFORM IMS-ISRT-AVS-BEN                                    
358400              MOVE JA TO UPPDAT-GJORD                                     
358500           ELSE                                                           
358600              IF TEST-KDPS = 'NS' OR 'KN' OR '  '                         
358700                 IF TEST-BEART NOT = SPACE                                
358800                    PERFORM IMS-ISRT-AVS-BEN                              
358900                    MOVE JA TO UPPDAT-GJORD                               
359000                 END-IF                                                   
359100              END-IF                                                      
359200           END-IF                                                         
359300        END-IF                                                            
359400     END-IF                                                               
359500     .                                                                    
359600     EJECT                                                                
359700 ID-UPD-AVS-NOT SECTION.                                                  
359800     SKIP2                                                                
359900*    mid-kdhaen är ifylld med nå´t                                        
360000                                                                          
360100     MOVE +2 TO W-IDSEGMNR                                                
360200     PERFORM IMS-GHNP-AVS-NOT-SEGM                                        
360300*    init blankt till not-tenote eftersom detta gäller segmnr = 2         
360400     MOVE SPACE TO NOT-TENOTE                                             
360500     IF SEGMENT-FINNS                                                     
360600        IF MID-KDHAEN-UPD = ' ' OR 'A' OR 'B'                             
360700           PERFORM IMS-DLET-AVS                                           
360800           MOVE JA TO UPPDAT-GJORD                                        
360900        ELSE                                                              
361000           MOVE WS-IDCATGRP-H   TO NOT-IDCATGRP                           
361100           MOVE WS-IDCATAVS-H   TO NOT-IDCATAVS                           
361200           MOVE WS-IDCATRAD-H   TO NOT-IDCATRAD                           
361300           MOVE WS-KDCATPUB-H   TO NOT-KDCATPUB-FOM                       
361400           PERFORM IMS-REPL-AVS                                           
361500           MOVE JA TO UPPDAT-GJORD                                        
361600        END-IF                                                            
361700     ELSE                                                                 
361800        IF KDHAEN-BLANK = JA                                              
361900           CONTINUE                                                       
362000        ELSE                                                              
362100           IF MID-KDHAEN-UPD = '*'                                        
362200              MOVE      +2            TO NOT-IDSEGMNR                     
362300              MOVE WS-IDCATGRP-H      TO NOT-IDCATGRP                     
362400              MOVE WS-IDCATAVS-H      TO NOT-IDCATAVS                     
362500              MOVE WS-IDCATRAD-H      TO NOT-IDCATRAD                     
362600              MOVE WS-KDCATPUB-H      TO NOT-KDCATPUB-FOM                 
362700              PERFORM IMS-ISRT-AVS-NOT                                    
362800              MOVE JA TO UPPDAT-GJORD                                     
362900           END-IF                                                         
363000        END-IF                                                            
363100     END-IF                                                               
363200     .                                                                    
363300     EJECT                                                                
363400 IE-UPD-AVS-NOT SECTION.                                                  
363500     SKIP2                                                                
363600     IF SEGMENT-FINNS                                                     
363700*      init blank till not-tenote eftersom detta gäller segmnr = 2        
363800        MOVE     SPACE       TO NOT-TENOTE                                
363900        MOVE WS-IDCATGRP-H   TO NOT-IDCATGRP                              
364000        MOVE WS-IDCATAVS-H   TO NOT-IDCATAVS                              
364100        MOVE WS-IDCATRAD-H   TO NOT-IDCATRAD                              
364200        MOVE WS-KDCATPUB-H   TO NOT-KDCATPUB-FOM                          
364300                                                                          
364400        PERFORM IMS-REPL-AVS                                              
364500        MOVE JA TO UPPDAT-GJORD                                           
364600     END-IF                                                               
364700     .                                                                    
364800     EJECT                                                                
364900 IF-UPD-AVS-RUB SECTION.                                                  
365000     SKIP2                                                                
365100     PERFORM IMS-GHNP-AVS-RUB                                             
365200     PERFORM UNTIL  SEGMENT-SAKNAS                                        
365300        IF RUB-IDSEGMNR = 1                                               
365400           MOVE +1 TO INDX                                                
365500           PERFORM IFA-UPPDATE-RUB                                        
365600        ELSE                                                              
365700           IF RUB-IDSEGMNR = 2                                            
365800              MOVE +2 TO INDX                                             
365900              PERFORM IFA-UPPDATE-RUB                                     
366000           ELSE                                                           
366100              IF RUB-IDSEGMNR = 3                                         
366200                 MOVE +3 TO INDX                                          
366300                 PERFORM IFA-UPPDATE-RUB                                  
366400              END-IF                                                      
366500           END-IF                                                         
366600        END-IF                                                            
366700        PERFORM IMS-GHNP-AVS-RUB                                          
366800     END-PERFORM                                                          
366900                                                                          
367000     IF TEST-IDRUBNR(1) NOT = ZERO                                        
367100        MOVE +1 TO INDX                                                   
367200        PERFORM IFB-UPPDATE-RUB-NY                                        
367300     END-IF                                                               
367400                                                                          
367500     IF TEST-IDRUBNR(2) NOT = ZERO                                        
367600        MOVE +2 TO INDX                                                   
367700        PERFORM IFB-UPPDATE-RUB-NY                                        
367800     END-IF                                                               
367900                                                                          
368000     IF TEST-IDRUBNR(3) NOT = ZERO                                        
368100        MOVE +3 TO INDX                                                   
368200        PERFORM IFB-UPPDATE-RUB-NY                                        
368300     END-IF                                                               
368400     .                                                                    
368500     EJECT                                                                
368600 IFA-UPPDATE-RUB SECTION.                                                 
368700     SKIP2                                                                
368800     IF TEST-IDRUBNR(INDX) = ZERO                                         
368900        PERFORM IMS-DLET-AVS                                              
369000        MOVE JA TO UPPDAT-GJORD                                           
369100     ELSE                                                                 
369200        IF TEST-IDRUBNR(INDX) = RUB-IDRUBNR                               
369300           CONTINUE                                                       
369400        ELSE                                                              
369500           PERFORM IMS-DLET-AVS                                           
369600           MOVE JA TO UPPDAT-GJORD                                        
369700        END-IF                                                            
369800     END-IF                                                               
369900     .                                                                    
370000     EJECT                                                                
370100 IFB-UPPDATE-RUB-NY SECTION.                                              
370200     SKIP2                                                                
370300     IF TEST-IDRUBNR(INDX) = BAS-IDRUBNR(INDX)                            
370400        CONTINUE                                                          
370500     ELSE                                                                 
370600        MOVE TEST-IDRUBNR(INDX) TO RUB-IDRUBNR                            
370700        MOVE INDX               TO RUB-IDSEGMNR                           
370800        MOVE 'J'                TO RUB-FLRUBTYP                           
370900        PERFORM IMS-ISRT-AVS-RUB                                          
371000        MOVE JA TO UPPDAT-GJORD                                           
371100     END-IF                                                               
371200     .                                                                    
371300     EJECT                                                                
371400 IG-UPD-AVS-FOT SECTION.                                                  
371500     SKIP2                                                                
371600     PERFORM IMS-GHNP-AVS-FOT                                             
371700     PERFORM UNTIL  SEGMENT-SAKNAS                                        
371800        IF FOT-IDSEGMNR = 1                                               
371900           MOVE +1 TO INDX                                                
372000           PERFORM IGA-UPPDATE-FOT                                        
372100        ELSE                                                              
372200           IF FOT-IDSEGMNR = 2                                            
372300              MOVE +2 TO INDX                                             
372400              PERFORM IGA-UPPDATE-FOT                                     
372500           ELSE                                                           
372600              IF FOT-IDSEGMNR = 3                                         
372700                 MOVE +3 TO INDX                                          
372800                 PERFORM IGA-UPPDATE-FOT                                  
372900              END-IF                                                      
373000           END-IF                                                         
373100        END-IF                                                            
373200        PERFORM IMS-GHNP-AVS-FOT                                          
373300     END-PERFORM                                                          
373400                                                                          
373500     IF TEST-IDFOTNR(1) NOT = ZERO                                        
373600        MOVE +1 TO INDX                                                   
373700        PERFORM IGB-UPPDATE-FOT-NY                                        
373800     END-IF                                                               
373900                                                                          
374000     IF TEST-IDFOTNR(2) NOT = ZERO                                        
374100        MOVE +2 TO INDX                                                   
374200        PERFORM IGB-UPPDATE-FOT-NY                                        
374300     END-IF                                                               
374400                                                                          
374500     IF TEST-IDFOTNR(3) NOT = ZERO                                        
374600        MOVE +3 TO INDX                                                   
374700        PERFORM IGB-UPPDATE-FOT-NY                                        
374800     END-IF                                                               
374900     .                                                                    
375000     EJECT                                                                
375100 IGA-UPPDATE-FOT SECTION.                                                 
375200     SKIP2                                                                
375300     IF TEST-IDFOTNR(INDX) = ZERO                                         
375400        PERFORM IMS-DLET-AVS                                              
375500        MOVE JA TO UPPDAT-GJORD                                           
375600     ELSE                                                                 
375700        IF TEST-IDFOTNR(INDX) = FOT-IDFOTNR                               
375800           CONTINUE                                                       
375900        ELSE                                                              
376000           PERFORM IMS-DLET-AVS                                           
376100           MOVE JA TO UPPDAT-GJORD                                        
376200        END-IF                                                            
376300     END-IF                                                               
376400     .                                                                    
376500     EJECT                                                                
376600 IGB-UPPDATE-FOT-NY SECTION.                                              
376700     SKIP2                                                                
376800     IF TEST-IDFOTNR(INDX) = BAS-IDFOTNR(INDX)                            
376900        CONTINUE                                                          
377000     ELSE                                                                 
377100        MOVE TEST-IDFOTNR(INDX) TO FOT-IDFOTNR                            
377200        MOVE INDX               TO FOT-IDSEGMNR                           
377300        PERFORM IMS-ISRT-AVS-FOT                                          
377400        MOVE JA TO UPPDAT-GJORD                                           
377500     END-IF                                                               
377600     .                                                                    
377700     EJECT                                                                
377800 IH-AENDR-HAEN SECTION.                                                   
377900     SKIP2                                                                
378000     IF MID-KDHAEN-UPD = 'B' OR 'A' OR ' '                                
378100        PERFORM IHA-UPD-AVS-HAEN                                          
378200     ELSE                                                                 
378300        IF MID-KDHAEN-UPD = '*'                                           
378400           PERFORM IHB-UPD-AVS-HAEN                                       
378500        END-IF                                                            
378600     END-IF                                                               
378700     .                                                                    
378800     EJECT                                                                
378900 IHA-UPD-AVS-HAEN SECTION.                                                
379000     SKIP2                                                                
379100     PERFORM IMS-GHNP-AVS-HAEN                                            
379200     IF SEGMENT-FINNS                                                     
379300        PERFORM IMS-DLET-AVS                                              
379400        MOVE JA TO UPPDAT-GJORD                                           
379500        IF MID-KDHAEN-UPD = ' '                                           
379600           CONTINUE                                                       
379700        ELSE                                                              
379800           MOVE W-IDCATNR          TO HAEN-IDCATNR                        
379900           MOVE WS-IDCATGRP-H      TO HAEN-IDCATGRP                       
380000           MOVE WS-IDCATAVS-H      TO HAEN-IDCATAVS                       
380100           MOVE WS-IDCATRAD-H      TO HAEN-IDCATRAD                       
380200           MOVE WS-KDCATPUB-H      TO HAEN-KDCATPUB-FOM                   
380300           MOVE MID-KDHAEN-UPD     TO HAEN-KDHAEN                         
380400           PERFORM IMS-ISRT-AVS-HAEN                                      
380500           MOVE JA TO UPPDAT-GJORD                                        
380600        END-IF                                                            
380700                                                                          
380800     ELSE                                                                 
380900        IF KDHAEN-BLANK = JA                                              
381000           CONTINUE                                                       
381100        ELSE                                                              
381200           MOVE W-IDCATNR          TO HAEN-IDCATNR                        
381300           MOVE WS-IDCATGRP-H      TO HAEN-IDCATGRP                       
381400           MOVE WS-IDCATAVS-H      TO HAEN-IDCATAVS                       
381500           MOVE WS-IDCATRAD-H      TO HAEN-IDCATRAD                       
381600           MOVE WS-KDCATPUB-H      TO HAEN-KDCATPUB-FOM                   
381700           MOVE MID-KDHAEN-UPD     TO HAEN-KDHAEN                         
381800           PERFORM IMS-ISRT-AVS-HAEN                                      
381900           MOVE JA TO UPPDAT-GJORD                                        
382000        END-IF                                                            
382100     END-IF                                                               
382200     .                                                                    
382300     EJECT                                                                
382400 IHB-UPD-AVS-HAEN SECTION.                                                
382500     SKIP2                                                                
382600     PERFORM IMS-GHNP-AVS-HAEN                                            
382700     IF SEGMENT-FINNS                                                     
382800        PERFORM IMS-DLET-AVS                                              
382900        MOVE JA TO UPPDAT-GJORD                                           
383000     END-IF                                                               
383100     .                                                                    
383200     EJECT                                                                
383300 II-UPD-AVS-HAEN SECTION.                                                 
383400     SKIP2                                                                
383500     PERFORM IMS-GHNP-AVS-HAEN                                            
383600     IF SEGMENT-FINNS                                                     
383700        PERFORM IMS-DLET-AVS                                              
383800        MOVE W-IDCATNR          TO HAEN-IDCATNR                           
383900        MOVE WS-IDCATGRP-H      TO HAEN-IDCATGRP                          
384000        MOVE WS-IDCATAVS-H      TO HAEN-IDCATAVS                          
384100        MOVE WS-IDCATRAD-H      TO HAEN-IDCATRAD                          
384200        MOVE WS-KDCATPUB-H      TO HAEN-KDCATPUB-FOM                      
384300        PERFORM IMS-ISRT-AVS-HAEN                                         
384400        MOVE JA TO UPPDAT-GJORD                                           
384500     END-IF                                                               
384600     .                                                                    
384700     EJECT                                                                
384800 J-RENSA-UPPDAT-FAELT SECTION.                                            
384900     SKIP2                                                                
385000     MOVE MFS-RENSA-FAELT TO MOD-IDCATRAD-UPD                             
385100                             MOD-KDCATPUB-R-FOM-UPD                       
385200                             MOD-KDCATPUB-R-TOM-UPD                       
385300                             MOD-KDFBX-UPD                                
385400                             MOD-IDCATPOS-UPD                             
385500                             MOD-IDARTNR-UPD                              
385600                                                                          
385700                             MOD-KVKOL-UPD(1)                             
385800                             MOD-KVKOL-UPD(2)                             
385900                             MOD-KVKOL-UPD(3)                             
386000                             MOD-KVKOL-UPD(4)                             
386100                             MOD-KVKOL-UPD(5)                             
386200                                                                          
386300                             MOD-KDPS-UPD                                 
386400                             MOD-KVPUNKT-UPD                              
386500                             MOD-BEART-UPD                                
386600                             MOD-KDHOM-UPD                                
386700                             MOD-TEKATANM-UPD                             
386800                                                                          
386900                             MOD-IDCATGRP-H-UPD                           
387000                             MOD-IDCATAVS-H-UPD                           
387100                             MOD-IDCATRAD-H-UPD                           
387200                             MOD-KDCATPUB-R-H-UPD                         
387300                                                                          
387400                             MOD-KDHAEN-UPD                               
387500                             MOD-IDCATRAD-BORT-UPD                        
387600                             MOD-IDCATPOS-SOEK-UPD                        
387700                                                                          
387800                             MOD-IDRUBNR-UPD(1)                           
387900                             MOD-IDRUBNR-UPD(2)                           
388000                             MOD-IDRUBNR-UPD(3)                           
388100                                                                          
388200                             MOD-IDTTEXNR-UPD                             
388300                                                                          
388400                             MOD-IDFOTNR-UPD(1)                           
388500                             MOD-IDFOTNR-UPD(2)                           
388600                             MOD-IDFOTNR-UPD(3)                           
388700     IF SOEK-DATA-OK                                                      
388800       MOVE MFS-RENSA-FAELT TO MOD-IDCATPOS-SOEK-UPD                      
388900     END-IF                                                               
389000     .                                                                    
389100     EJECT                                                                
389200 K-LAS-KATALOGRAD SECTION.                                                
389300     SKIP2                                                                
389400     MOVE  0020  TO SPAR-IDCATRAD-1                                       
389500     MOVE  9999  TO SPAR-IDCATRAD-7                                       
389600                                                                          
389700     MOVE ZERO   TO EXTRA-RAD                                             
389800                    NUVARANDE-IDCATRAD                                    
389900     MOVE SPACE  TO UT-BERUBTXT(1)                                        
390000                    UT-BERUBTXT(2)                                        
390100                    UT-BERUBTXT(3)                                        
390200                    UT-BETTEXT                                            
390300                    NUVARANDE-KDCATPUB-F                                  
390400                    NUVARANDE-KDCATPUB-T                                  
390500     MOVE JA     TO RAD-FINNS                                             
390600     MOVE NEJ    TO EXTRA-ANMARK                                          
390700                                                                          
390800     PERFORM IMS-GU-AVS                                                   
390900     IF SEGMENT-SAKNAS                                                    
391000        MOVE FEL-5 (SPRAAK-IX) TO MOD-TEMFSFEL                            
391100        MOVE JA TO INDATA-FEL                                             
391200     ELSE                                                                 
391300        MOVE +1 TO RAD                                                    
391400        PERFORM IMS-GNP-AVS-RAD-NEXT                                      
391500        IF SEGMENT-FINNS                                                  
391600           MOVE RAD-IDCATRAD     TO NUVARANDE-IDCATRAD                    
391700           MOVE RAD-KDCATPUB-FOM TO NUVARANDE-KDCATPUB-F                  
391800           MOVE RAD-KDCATPUB-TOM TO NUVARANDE-KDCATPUB-T                  
391900        ELSE                                                              
392000           MOVE NEJ TO RAD-FINNS                                          
392100        END-IF                                                            
392200                                                                          
392300        PERFORM UNTIL RAD = +13 OR RAD-FINNS = NEJ                        
392400           MOVE ZERO  TO TEST-IDARTNR  TEST-KDHOM                         
392500           MOVE SPACE TO TEST-BEART    TEST-KDPS                          
392600           IF EXTRA-RAD = ZERO AND EXTRA-ANMARK = NEJ                     
392700              MOVE NUVARANDE-IDCATRAD TO W-IDCATRAD                       
392800              MOVE NUVARANDE-KDCATPUB-F TO W-KDCATPUB-X                   
392900              MOVE NUVARANDE-KDCATPUB-T TO WS-KDCATPUB-T                  
393000              IF RAD-FINNS = JA                                           
393100                IF RAD = 1                                                
393200                   MOVE NUVARANDE-IDCATRAD TO SPAR-IDCATRAD-1             
393300                END-IF                                                    
393400                IF RAD = 7                                                
393500                   MOVE NUVARANDE-IDCATRAD TO SPAR-IDCATRAD-7             
393600                END-IF                                                    
393700                MOVE NUVARANDE-IDCATRAD TO MOD-IDCATRAD(RAD)              
393800                MOVE NUVARANDE-KDCATPUB-F(4:3)                            
393900                                        TO MOD-KDCATPUB-R-FOM(RAD)        
394000                MOVE NUVARANDE-KDCATPUB-T(4:3)                            
394100                                        TO MOD-KDCATPUB-R-TOM(RAD)        
394200                MOVE RAD-KDRADST   TO MOD-KDRADST(RAD)                    
394300                PERFORM KA-AVS-ART                                        
394400                PERFORM KB-AVS-TEXT                                       
394500                PERFORM KC-AVS-BEN                                        
394600                PERFORM KD-AVS-NOT                                        
394700                PERFORM KE-AVS-RUB                                        
394800                PERFORM KI-AVS-FOT                                        
394900                PERFORM KF-AVS-HAEN-REF                                   
395000              END-IF                                                      
395100              IF EXTRA-RAD NOT = ZERO                                     
395200                 IF MOD-BEART(RAD) = SPACE                                
395300                    PERFORM Q-KOLLA-EXTRA-BEART                           
395400                 END-IF                                                   
395500              END-IF                                                      
395600           ELSE                                                           
395700              IF RAD-FINNS = JA                                           
395800                 IF RAD = 1                                               
395900                    MOVE NUVARANDE-IDCATRAD TO SPAR-IDCATRAD-1            
396000                 END-IF                                                   
396100                 IF RAD = 7                                               
396200                    MOVE NUVARANDE-IDCATRAD TO SPAR-IDCATRAD-7            
396300                 END-IF                                                   
396400              END-IF                                                      
396500              PERFORM KH-NOLLSTALL-RAD                                    
396600              MOVE W-IDCATRAD    TO MOD-IDCATRAD(RAD)                     
396700              MOVE W-KDCATPUB-X(4:3)  TO MOD-KDCATPUB-R-FOM(RAD)          
396800              MOVE WS-KDCATPUB-T(4:3) TO MOD-KDCATPUB-R-TOM(RAD)          
396900              PERFORM KG-KOLLA-EXTRA-RAD                                  
397000           END-IF                                                         
397100           ADD +1 TO RAD                                                  
397200           IF EXTRA-RAD = ZERO AND EXTRA-ANMARK = NEJ                     
397300              PERFORM IMS-GNP-AVS-RAD-NEXT                                
397400              IF SEGMENT-SAKNAS                                           
397500                IF RAD = 13                                               
397600                  MOVE NUVARANDE-IDCATRAD TO MOD-IDCATRAD-SISTA           
397700                END-IF                                                    
397800                MOVE NEJ TO RAD-FINNS                                     
397900              ELSE                                                        
398000                 IF RAD > 12                                              
398100                    CONTINUE                                              
398200                 ELSE                                                     
398300                    MOVE RAD-IDCATRAD TO NUVARANDE-IDCATRAD               
398400                    MOVE RAD-KDCATPUB-FOM TO NUVARANDE-KDCATPUB-F         
398500                    MOVE RAD-KDCATPUB-TOM TO NUVARANDE-KDCATPUB-T         
398600                 END-IF                                                   
398700              END-IF                                                      
398800           ELSE                                                           
398900              IF RAD > 12                                                 
399000                 PERFORM IMS-GNP-AVS-RAD-NEXT                             
399100                 IF SEGMENT-FINNS                                         
399200                    MOVE RAD-IDCATRAD TO NUVARANDE-IDCATRAD               
399300                    MOVE RAD-KDCATPUB-FOM TO NUVARANDE-KDCATPUB-F         
399400                    MOVE RAD-KDCATPUB-TOM TO NUVARANDE-KDCATPUB-T         
399500                    MOVE JA TO RAD-FINNS                                  
399600                 ELSE                                                     
399700                    IF RAD = 13                                           
399800                       MOVE NUVARANDE-IDCATRAD TO                         
399900                                              MOD-IDCATRAD-SISTA          
400000                    END-IF                                                
400100                    MOVE NEJ TO RAD-FINNS                                 
400200                 END-IF                                                   
400300              END-IF                                                      
400400           END-IF                                                         
400500        END-PERFORM                                                       
400600     END-IF                                                               
400700     .                                                                    
400800     EJECT                                                                
400900 KA-AVS-ART SECTION.                                                      
401000     SKIP2                                                                
401100     PERFORM IMS-GHNP-AVS-ART                                             
401200                                                                          
401300     PERFORM KAA-FLYTTA-AVS-ART                                           
401400                                                                          
401500     IF W-IDARTNR > ZERO                                                  
401600        MOVE W-IDARTNR TO BYTES-IDARTNR                                   
401700        PERFORM KAB-KOLLA-ARTREG                                          
401800        IF BYT02-RENOV                                                    
401900           MOVE 'EU'        TO MOD-KDPS(RAD)                              
402000                               TEST-KDPS                                  
402100*          MOVE ZERO        TO MOD-KDERS(RAD)                             
402200        END-IF                                                            
402300     END-IF                                                               
402400     .                                                                    
402500     EJECT                                                                
402600 KAA-FLYTTA-AVS-ART SECTION.                                              
402700     SKIP2                                                                
402800     IF SEGMENT-FINNS                                                     
402900        MOVE ART-KDFBX     TO MOD-KDFBX(RAD)                              
403000                                                                          
403100*        höger-ställer den numeriska delen i pos under 100                
403200*        lämnar den tredje pos-positionen åt alfa så långt det går        
403300        IF ART-IDCATPOS NUMERIC                                           
403400        OR ART-IDCATPOS = SPACE                                           
403500          MOVE ART-IDCATPOS TO MOD-IDCATPOS(RAD)                          
403600        ELSE                                                              
403700          IF ART-IDCATPOS(2:2) = SPACE                                    
403800          OR ART-IDCATPOS(2:1) ALPHABETIC                                 
403900            MOVE ART-IDCATPOS TO MOD-IDCATPOS(RAD)(2:2)                   
404000          ELSE                                                            
404100            MOVE ART-IDCATPOS TO MOD-IDCATPOS(RAD)                        
404200          END-IF                                                          
404300        END-IF                                                            
404400        MOVE ART-IDARTNR   TO MOD-IDARTNR(RAD)                            
404500                              TEST-IDARTNR                                
404600        PERFORM KAAA-FLYTTA-KVKOL                                         
404700        MOVE ART-KDPS      TO MOD-KDPS(RAD)                               
404800                              TEST-KDPS                                   
404900        MOVE ART-KVPUNKT   TO MOD-KVPUNKT(RAD)                            
405000        MOVE ART-IDARTNR   TO W-IDARTNR                                   
405100        IF ART-IDTTEXNR > ZERO                                            
405200           PERFORM KAAB-LAS-TEXT                                          
405300        END-IF                                                            
405400     ELSE                                                                 
405500        MOVE SPACE         TO MOD-KDFBX(RAD)                              
405600                              MOD-IDCATPOS(RAD)                           
405700                              MOD-KVKOL(RAD, 1)                           
405800                              MOD-KVKOL(RAD, 2)                           
405900                              MOD-KVKOL(RAD, 3)                           
406000                              MOD-KVKOL(RAD, 4)                           
406100                              MOD-KVKOL(RAD, 5)                           
406200                              MOD-KDPS(RAD)                               
406300                              TEST-KDPS                                   
406400        MOVE ZERO          TO MOD-IDARTNR(RAD)                            
406500                              TEST-IDARTNR                                
406600                              MOD-KVPUNKT(RAD)                            
406700        MOVE ZERO          TO W-IDARTNR                                   
406800        MOVE SPACE         TO UT-BETTEXT                                  
406900     END-IF                                                               
407000     .                                                                    
407100     EJECT                                                                
407200 KAAA-FLYTTA-KVKOL SECTION.                                               
407300     SKIP2                                                                
407400     MOVE +1 TO KOL                                                       
407500     PERFORM UNTIL KOL = +6                                               
407600       IF ART-KVKOL(KOL) = SPACE                                          
407700         IF ART-KDFBX = 'F'                                               
407800           MOVE SPACE TO MOD-KVKOL(RAD, KOL)                              
407900         ELSE                                                             
408000           MOVE '  -' TO MOD-KVKOL(RAD, KOL)                              
408100         END-IF                                                           
408200       ELSE                                                               
408300*        höger-ställer info i kolumnfältet,                               
408400*        för att skärm-visning skall motsvara tryckt katalog              
408500         IF ART-KVKOL(KOL) NUMERIC                                        
408600         OR ART-KVKOL(KOL) = SPACE                                        
408700           MOVE ART-KVKOL(KOL) TO MOD-KVKOL(RAD, KOL)                     
408800         ELSE                                                             
408900           IF ART-KVKOL(KOL)(1:1) = SPACE                                 
409000             IF ART-KVKOL(KOL)(2:1) = SPACE                               
409100               MOVE ART-KVKOL(KOL) TO MOD-KVKOL(RAD, KOL)                 
409200             ELSE                                                         
409300               IF ART-KVKOL(KOL)(3:1) = SPACE                             
409400               MOVE ART-KVKOL(KOL)(2:1) TO                                
409500                                        MOD-KVKOL(RAD, KOL)(3:1)          
409600               ELSE                                                       
409700               MOVE ART-KVKOL(KOL)      TO MOD-KVKOL(RAD, KOL)            
409800               END-IF                                                     
409900             END-IF                                                       
410000           ELSE                                                           
410100             IF ART-KVKOL(KOL)(2:1) = SPACE                               
410200               MOVE ART-KVKOL(KOL)(1:1) TO                                
410300                                      MOD-KVKOL(RAD, KOL)(3:1)            
410400             ELSE                                                         
410500               IF ART-KVKOL(KOL)(3:1) = SPACE                             
410600               MOVE ART-KVKOL(KOL)(1:2) TO                                
410700                                      MOD-KVKOL(RAD, KOL)(2:2)            
410800               ELSE                                                       
410900               MOVE ART-KVKOL(KOL)      TO MOD-KVKOL(RAD, KOL)            
411000               END-IF                                                     
411100             END-IF                                                       
411200           END-IF                                                         
411300         END-IF                                                           
411400       END-IF                                                             
411500       ADD +1 TO KOL                                                      
411600     END-PERFORM                                                          
411700     .                                                                    
411800     EJECT                                                                
411900 KAAB-LAS-TEXT SECTION.                                                   
412000     SKIP2                                                                
412100     MOVE ART-IDTTEXNR TO W-IDTTEXNR                                      
412200     PERFORM IMS-GU-TEXT                                                  
412300     IF SEGMENT-FINNS                                                     
412400        MOVE KEY-IDSKYLT TO W-IDSKYLT                                     
412500        PERFORM IMS-GNP-TEXT-TEXT                                         
412600        IF SEGMENT-FINNS                                                  
412700           MOVE TEXT-BETTEXT TO  UT-BETTEXT                               
412800           MOVE +1 TO EXTRA-RAD                                           
412900        ELSE                                                              
413000           MOVE SPACE TO UT-BETTEXT                                       
413100        END-IF                                                            
413200     ELSE                                                                 
413300        MOVE SPACE TO UT-BETTEXT                                          
413400     END-IF                                                               
413500     .                                                                    
413600     EJECT                                                                
413700 KAB-KOLLA-ARTREG SECTION.                                                
413800     SKIP2                                                                
413900     PERFORM IMS-GU-ARTC01                                                
414000                                                                          
414100     IF SEGMENT-FINNS                                                     
414200        MOVE ARTC01-ART-TIERSDAT TO WS-TIERSDAT-TIAAVVD                   
414300        IF ARTC01-ART-KDERS-UTG > ZERO                                    
414400*          MOVE        ZERO       TO MOD-KDBPSR(RAD)                      
414500           EVALUATE TRUE                                                  
414600             WHEN ARTC01-ART-KDERS-UTG = 29                               
414700                MOVE 'OP' TO MOD-KDPS(RAD)                                
414800                           TEST-KDPS                                      
414900                MOVE ARTC01-ART-KDERS-UTG TO MOD-KDERS(RAD)               
415000             WHEN     ARTC01-ART-KDERS-UTG > 20 AND < 27                  
415100                MOVE 'SP' TO MOD-KDPS(RAD)                                
415200                           TEST-KDPS                                      
415300                MOVE ARTC01-ART-KDERS-UTG TO MOD-KDERS(RAD)               
415400             WHEN OTHER                                                   
415500                MOVE ARTC01-ART-KDERS-UTG TO MOD-KDERS(RAD)               
415600           END-EVALUATE                                                   
415700        ELSE                                                              
415800           PERFORM IMS-GNP-ARTC11                                         
415900                                                                          
416000           IF SEGMENT-FINNS                                               
416100*             MOVE ARTC11-CLAG-KDBPSR TO MOD-KDBPSR(RAD)                  
416200              IF ARTC11-CLAG-FLLSRDEL = NEJ                               
416300                 MOVE 'NS' TO MOD-KDPS(RAD)                               
416400              END-IF                                                      
416500                                                                          
416600*             --- Här slår KDERS över FLLSRDEL='N'                        
416700              EVALUATE TRUE                                               
416800                 WHEN ARTC11-CLAG-KDERS = 29                              
416900                    MOVE 'OP' TO MOD-KDPS(RAD)   TEST-KDPS                
417000                    MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(RAD)              
417100                                                                          
417200                 WHEN ARTC11-CLAG-KDERS > 20 AND < 27                     
417300                    IF ARTC11-CLAG-KDERS = 21 OR 24                       
417400*                      --- Minskar ev. erskoden med 10                    
417500                       PERFORM KABA-KOLLA-TIERSDAT                        
417600                    ELSE                                                  
417700                       MOVE 'SP' TO MOD-KDPS(RAD)                         
417800                       MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(RAD)           
417900                    END-IF                                                
418000                    MOVE 'SP' TO TEST-KDPS                                
418100                                                                          
418200                 WHEN ARTC11-CLAG-KDERS > 29                              
418300                    MOVE 'NS' TO MOD-KDPS(RAD)  TEST-KDPS                 
418400                    MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(RAD)              
418500                    STRING 'ERSKOD / SUP.CODE=' MOD-KDERS(RAD)            
418600                    DELIMITED BY SIZE                                     
418700                    INTO MOD-BEART(RAD)                                   
418800                                                                          
418900                 WHEN OTHER                                               
419000                    MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(RAD)              
419100              END-EVALUATE                                                
419200              IF ARTC11-CLAG-KDUART = 'P'                                 
419300               IF ARTC11-CLAG-KDERS = ZERO                                
419400               MOVE ARTC11-CLAG-KDUART TO MOD-KDPS(RAD) TEST-KDPS         
419500               END-IF                                                     
419600              END-IF                                                      
419700                                                                          
419800           ELSE                                                           
419900              MOVE ZERO TO MOD-KDERS(RAD)                                 
420000           END-IF                                                         
420100        END-IF                                                            
420200     ELSE                                                                 
420300*       MOVE ZERO TO MOD-KDBPSR(RAD)                                      
420400        MOVE ZERO TO MOD-KDERS(RAD)                                       
420500     END-IF                                                               
420600     .                                                                    
420700     EJECT                                                                
420800 KABA-KOLLA-TIERSDAT SECTION.                                             
420900     SKIP2                                                                
421000     IF IDAG-DAT-KDSVAR-OK                                                
421100        MOVE IDAG-DAT-TIAAVVD TO DAGENS-TIAAVVD                           
421200                                                                          
421300        MOVE WS-TIERSDAT-TIAAVV TO W009VADD-DATUM                         
421400        MOVE +32                TO W009VADD-ANTAL                         
421500        CALL W009VADD USING W009VADD-DATUM                                
421600                            W009VADD-ANTAL                                
421700        MOVE W009VADD-DATUM TO WS-TIERSDAT-TIAAVV                         
421800                                                                          
421900        MOVE WS-TIERSDAT-TIAAVVD   TO TMP1-YYWWD                          
422000        MOVE DAGENS-TIAAVVD        TO TMP2-YYWWD                          
422100        PERFORM WY2000P2                                                  
422200        IF TMP1-YYWWD > TMP2-YYWWD                                        
422300           SUBTRACT 10 FROM ARTC11-CLAG-KDERS GIVING WS-KDERS             
422400           MOVE WS-KDERS  TO  MOD-KDERS(RAD)                              
422500        ELSE                                                              
422600           MOVE 'SP' TO MOD-KDPS(RAD)                                     
422700           MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(RAD)                       
422800        END-IF                                                            
422900     END-IF                                                               
423000     .                                                                    
423100     EJECT                                                                
423200 KB-AVS-TEXT SECTION.                                                     
423300     SKIP2                                                                
423400     PERFORM IMS-GNP-AVS-TEXT                                             
423500     IF SEGMENT-FINNS                                                     
423600        MOVE TEXT-TEKATANM TO MOD-TEKATANM(RAD)                           
423700                              SPAR-TEKATANM                               
423800     ELSE                                                                 
423900        MOVE SPACE         TO MOD-TEKATANM(RAD)                           
424000                              SPAR-TEKATANM                               
424100     END-IF                                                               
424200     .                                                                    
424300     EJECT                                                                
424400 KC-AVS-BEN SECTION.                                                      
424500     SKIP2                                                                
424600     MOVE SPACE TO MOD-BEART(RAD)                                         
424700                                                                          
424800     PERFORM IMS-GHNP-AVS-BEN                                             
424900     IF SEGMENT-FINNS                                                     
425000        IF TEST-KDPS = 'XX'                                               
425100           MOVE BEN-BEART               TO MOD-BEART(RAD)                 
425200        ELSE                                                              
425300           IF TEST-KDPS = 'LS' OR 'KL' OR 'NS' or                         
425400                          'KN' OR 'P ' OR SPACE                           
425500              PERFORM KCA-KOLLA-BENREG                                    
425600              IF SEGMENT-SAKNAS                                           
425700                 MOVE BEN-BEART         TO TRUNK-BEART                    
425800                 MOVE FEL-BEART-AREA TO MOD-BEART(RAD)                    
425900              END-IF                                                      
426000           END-IF                                                         
426100        END-IF                                                            
426200     ELSE                                                                 
426300        MOVE KEY-IDSKYLT TO W-IDSKYLT                                     
426400        IF TEST-KDPS = SPACE or 'OP' or 'SP' or 'EU' or 'IK'              
426500                             or 'SW' or 'P ' or 'NS'                      
426600           IF TEST-IDARTNR = ZERO                                         
426700              CONTINUE                                                    
426800           ELSE                                                           
426900              MOVE TEST-IDARTNR TO W-IDARTNR                              
427000              PERFORM IMS-GU-BENB-SEQ                                     
427100              IF SEGMENT-FINNS                                            
427200                IF KEY-IDSKYLT = 'USA'                                    
427300                  MOVE 'GB ' TO W-IDSKYLT                                 
427400                  PERFORM IMS-GNP-BEN-TEXT-BSEQ                           
427500                  IF SEGMENT-FINNS                                        
427600                    MOVE BEN-TEXT-BEART TO MOD-BEART(RAD)                 
427700                  END-IF                                                  
427800                  MOVE 'USA' TO W-IDSKYLT                                 
427900                  PERFORM IMS-GNP-BEN-TEXT-BSEQ                           
428000                  IF SEGMENT-FINNS                                        
428100                    IF BEN-TEXT-BEART NOT = SPACE                         
428200                      MOVE BEN-TEXT-BEART TO MOD-BEART(RAD)               
428300                    END-IF                                                
428400                  END-IF                                                  
428500                ELSE                                                      
428600                  PERFORM IMS-GNP-BEN-TEXT-BSEQ                           
428700                  IF SEGMENT-FINNS                                        
428800                    MOVE BEN-TEXT-BEART TO MOD-BEART(RAD)                 
428900                  END-IF                                                  
429000                END-IF                                                    
429100              ELSE                                                        
429200                 MOVE MED-5(SPRAAK-IX)  TO MOD-BEART(RAD)                 
429300*                KOPPLING MOT BENREG SAKNAS FÖR ANGIVET ARTNR             
429400              END-IF                                                      
429500           END-IF                                                         
429600        END-IF                                                            
429700     END-IF                                                               
429800     .                                                                    
429900     EJECT                                                                
430000 KCA-KOLLA-BENREG SECTION.                                                
430100     SKIP2                                                                
430200     MOVE 'S  ' TO W-IDSKYLT                                              
430300     MOVE BEN-BEART TO W-BEART                                            
430400     PERFORM IMS-GU-BENA-SEQ                                              
430500     IF SEGMENT-FINNS                                                     
430600       IF KEY-IDSKYLT = 'S  '                                             
430700          MOVE BEN-BEART TO MOD-BEART(RAD)                                
430800       ELSE                                                               
430900          IF BEN-BEN-KDHOMONYM = BEN-KDHOM                                
431000* - - - - - - - - - - - - - - - - - - - - - - - - LÄS TEXT                
431100             MOVE KEY-IDSKYLT TO W-IDSKYLT                                
431200             PERFORM IMS-GNP-BEN-TEXT-ASEQ                                
431300             IF SEGMENT-FINNS                                             
431400               MOVE BEN-TEXT-BEART TO MOD-BEART(RAD)                      
431500             END-IF                                                       
431600          ELSE                                                            
431700             PERFORM IMS-GNP-BENA-HOM                                     
431800             IF SEGMENT-FINNS                                             
431900                PERFORM KCAB-HITTA-RAETT-TEXT                             
432000             ELSE                                                         
432100                PERFORM IMS-GU-BENA-SEQ                                   
432200                IF SEGMENT-FINNS                                          
432300* - - - - - - - - - - - - - - - - - - - - - - - - LÄS TEXT                
432400                   MOVE KEY-IDSKYLT TO W-IDSKYLT                          
432500                   PERFORM IMS-GNP-BEN-TEXT-ASEQ                          
432600                   IF SEGMENT-FINNS                                       
432700                     MOVE BEN-TEXT-BEART TO MOD-BEART(RAD)                
432800                   END-IF                                                 
432900                END-IF                                                    
433000             END-IF                                                       
433100          END-IF                                                          
433200       END-IF                                                             
433300     END-IF                                                               
433400     .                                                                    
433500     EJECT                                                                
433600 KCAB-HITTA-RAETT-TEXT SECTION.                                           
433700     SKIP2                                                                
433800     MOVE NEJ TO RAETT-TEXT                                               
433900     PERFORM IMS-GN-BENA-SEQ                                              
434000     PERFORM UNTIL  SEGMENT-SAKNAS OR RAETT-TEXT = JA                     
434100        IF BEN-BEN-KDHOMONYM = BEN-KDHOM                                  
434200           MOVE KEY-IDSKYLT TO W-IDSKYLT                                  
434300                                                                          
434400           PERFORM IMS-GNP-BEN-TEXT-ASEQ                                  
434500           IF SEGMENT-FINNS                                               
434600              MOVE BEN-TEXT-BEART TO MOD-BEART(RAD)                       
434700           END-IF                                                         
434800           MOVE JA TO RAETT-TEXT                                          
434900        ELSE                                                              
435000           PERFORM IMS-GN-BENA-SEQ                                        
435100        END-IF                                                            
435200     END-PERFORM                                                          
435300     .                                                                    
435400     EJECT                                                                
435500 KD-AVS-NOT SECTION.                                                      
435600     SKIP2                                                                
435700     PERFORM IMS-GHNP-AVS-NOT                                             
435800     IF SEGMENT-FINNS                                                     
435900        IF NOT-IDSEGMNR = 1                                               
436000           MOVE 'J'        TO MOD-FLNOTE(RAD)                             
436100           PERFORM IMS-GHNP-AVS-NOT                                       
436200                                                                          
436300           IF SEGMENT-FINNS                                               
436400              MOVE '*'     TO MOD-FLH(RAD)                                
436500           ELSE                                                           
436600              MOVE ' '     TO MOD-FLH(RAD)                                
436700           END-IF                                                         
436800                                                                          
436900        ELSE                                                              
437000           MOVE ' '        TO MOD-FLNOTE(RAD)                             
437100           MOVE '*'        TO MOD-FLH(RAD)                                
437200        END-IF                                                            
437300                                                                          
437400     ELSE                                                                 
437500        MOVE ' ' TO MOD-FLNOTE(RAD)                                       
437600        MOVE ' ' TO MOD-FLH(RAD)                                          
437700     END-IF                                                               
437800     .                                                                    
437900     EJECT                                                                
438000 KE-AVS-RUB SECTION.                                                      
438100     SKIP2                                                                
438200     PERFORM IMS-GHNP-AVS-RUB                                             
438300     PERFORM UNTIL  SEGMENT-SAKNAS                                        
438400        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
438500        PERFORM IMS-GU-RUB                                                
438600        IF SEGMENT-FINNS                                                  
438700           MOVE KEY-IDSKYLT TO W-IDSKYLT                                  
438800           IF RUB-RUB-FLKOMBINERAS = 'N'                                  
438900              MOVE +1 TO RAK-IX                                           
439000              PERFORM IMS-GNP-RUB-TEXT                                    
439100              PERFORM UNTIL  SEGMENT-SAKNAS                               
439200                 MOVE RUB-TEXT-BERUBTXT TO                                
439300                      UT-BERUBTXT(RAK-IX)                                 
439400                 ADD +1 TO EXTRA-RAD                                      
439500                           RAK-IX                                         
439600                 PERFORM IMS-GNP-RUB-TEXT                                 
439700              END-PERFORM                                                 
439800           ELSE                                                           
439900              PERFORM IMS-GNP-RUB-TEXT                                    
440000              IF SEGMENT-FINNS                                            
440100                 MOVE RUB-TEXT-BERUBTXT TO                                
440200                      UT-BERUBTXT(RUB-IDSEGMNR)                           
440300                 ADD +1 TO EXTRA-RAD                                      
440400              END-IF                                                      
440500           END-IF                                                         
440600        END-IF                                                            
440700        PERFORM IMS-GHNP-AVS-RUB                                          
440800     END-PERFORM                                                          
440900     .                                                                    
441000     EJECT                                                                
441100 KF-AVS-HAEN-REF SECTION.                                                 
441200     SKIP2                                                                
441300     PERFORM IMS-GHNP-AVS-HAEN                                            
441400     IF SEGMENT-FINNS                                                     
441500        MOVE HAEN-KDHAEN    TO MOD-FLH(RAD)                               
441600     END-IF                                                               
441700                                                                          
441800     MOVE W-IDCATNR  TO W-IDCATNR-GSEQ                                    
441900     MOVE W-IDCATGRP TO W-IDCATGRP-GSEQ                                   
442000     MOVE W-IDCATAVS TO W-IDCATAVS-GSEQ                                   
442100     MOVE W-IDCATRAD TO W-IDCATRAD-GSEQ                                   
442200     MOVE W-KDCATPUB TO W-KDCATPUB-GSEQ                                   
442300*    --- Se WLKATS (WDN5G) om det finns HAEN (REF) till detta avs.        
442400     PERFORM IMS-GU-AVSG-HAEN                                             
442500     IF SEGMENT-FINNS                                                     
442600       MOVE 'R'  TO MOD-FLH-REF(RAD)                                      
442700     ELSE                                                                 
442800       MOVE ' '  TO MOD-FLH-REF(RAD)                                      
442900     END-IF                                                               
443000     .                                                                    
443100     EJECT                                                                
443200 KG-KOLLA-EXTRA-RAD SECTION.                                              
443300     SKIP2                                                                
443400     IF SPAR-TEKATANM = SPACE                                             
443500        PERFORM Q-KOLLA-EXTRA-BEART                                       
443600     ELSE                                                                 
443700        MOVE SPAR-TEKATANM TO MOD-TEKATANM(RAD)                           
443800        MOVE SPACE TO SPAR-TEKATANM                                       
443900        MOVE NEJ TO EXTRA-ANMARK                                          
444000        IF (UT-BERUBTXT(1) = SPACE) AND                                   
444100           (UT-BERUBTXT(2) = SPACE) AND                                   
444200           (UT-BERUBTXT(3) = SPACE) AND                                   
444300           (UT-BETTEXT = SPACE)                                           
444400           MOVE SPACE TO MOD-BEART(RAD)                                   
444500           MOVE ZERO TO EXTRA-RAD                                         
444600        ELSE                                                              
444700           PERFORM Q-KOLLA-EXTRA-BEART                                    
444800        END-IF                                                            
444900     END-IF                                                               
445000     .                                                                    
445100     EJECT                                                                
445200 KH-NOLLSTALL-RAD SECTION.                                                
445300     SKIP2                                                                
445400     MOVE SPACE TO MOD-KDFBX(RAD)                                         
445500                   MOD-KDCATPUB-R-FOM(RAD)                                
445600                   MOD-KDCATPUB-R-TOM(RAD)                                
445700                   MOD-IDCATPOS(RAD)                                      
445800                   MOD-KVKOL(RAD, 1)                                      
445900                   MOD-KVKOL(RAD, 2)                                      
446000                   MOD-KVKOL(RAD, 3)                                      
446100                   MOD-KVKOL(RAD, 4)                                      
446200                   MOD-KVKOL(RAD, 5)                                      
446300                   MOD-KDPS(RAD)                                          
446400                   MOD-BEART(RAD)                                         
446500                   MOD-TEKATANM(RAD)                                      
446600                   MOD-KDRADST(RAD)                                       
446700                   MOD-FLNOTE(RAD)                                        
446800                   MOD-FLH(RAD)                                           
446900     MOVE ZERO TO  MOD-IDARTNR(RAD)                                       
447000                   MOD-KVPUNKT(RAD)                                       
447100****               MOD-KDBPSR(RAD)                                        
447200                   MOD-KDERS(RAD)                                         
447300     .                                                                    
447400     EJECT                                                                
447500 KI-AVS-FOT SECTION.                                                      
447600     SKIP2                                                                
447700     MOVE ZERO TO SPAR-IDFOTNR(1)                                         
447800                  SPAR-IDFOTNR(2)                                         
447900                  SPAR-IDFOTNR(3)                                         
448000                  FOTNOT-RAKNARE                                          
448100                                                                          
448200     PERFORM IMS-GHNP-AVS-FOT                                             
448300     PERFORM UNTIL  SEGMENT-SAKNAS                                        
448400        MOVE FOT-IDFOTNR TO SPAR-IDFOTNR(FOT-IDSEGMNR)                    
448500        ADD +1 TO FOTNOT-RAKNARE                                          
448600        PERFORM IMS-GHNP-AVS-FOT                                          
448700     END-PERFORM                                                          
448800                                                                          
448900     IF FOTNOT-RAKNARE NOT = ZERO                                         
449000        PERFORM KIA-FLYTTA-FOTNOT                                         
449100        IF SPAR-TEKATANM NOT = SPACE                                      
449200           MOVE JA TO EXTRA-ANMARK                                        
449300        END-IF                                                            
449400     ELSE                                                                 
449500        MOVE SPACE TO SPAR-TEKATANM                                       
449600        MOVE NEJ TO EXTRA-ANMARK                                          
449700     END-IF                                                               
449800     .                                                                    
449900     EJECT                                                                
450000 KIA-FLYTTA-FOTNOT SECTION.                                               
450100     SKIP2                                                                
450200     MOVE +1 TO RAK-IX                                                    
450300     IF FOTNOT-RAKNARE = 1                                                
450400        PERFORM UNTIL RAK-IX = +4                                         
450500           IF SPAR-IDFOTNR(RAK-IX) NOT = ZERO                             
450600              MOVE SPAR-IDFOTNR(RAK-IX) TO ENFOT-1                        
450700              MOVE ')' TO ENFOT-TECK-1                                    
450800              MOVE +3 TO RAK-IX                                           
450900           END-IF                                                         
451000           ADD +1 TO RAK-IX                                               
451100        END-PERFORM                                                       
451200        MOVE SPACE TO ENFOT-SPACE                                         
451300     ELSE                                                                 
451400        IF FOTNOT-RAKNARE = 2                                             
451500           PERFORM UNTIL RAK-IX = +4                                      
451600              IF SPAR-IDFOTNR(RAK-IX) NOT = ZERO                          
451700                 IF FOTNOT-RAKNARE = 2                                    
451800                    MOVE SPAR-IDFOTNR(RAK-IX) TO TVAFOT-1                 
451900                    MOVE ')' TO TVAFOT-TECK-1                             
452000                 ELSE                                                     
452100                    MOVE SPAR-IDFOTNR(RAK-IX) TO TVAFOT-2                 
452200                    MOVE ')' TO TVAFOT-TECK-2                             
452300                 END-IF                                                   
452400                 SUBTRACT 1 FROM FOTNOT-RAKNARE                           
452500              END-IF                                                      
452600              ADD +1 TO RAK-IX                                            
452700           END-PERFORM                                                    
452800           MOVE SPACE TO TVAFOT-SPACE                                     
452900        ELSE                                                              
453000           PERFORM UNTIL RAK-IX = +4                                      
453100              IF SPAR-IDFOTNR(RAK-IX) NOT = ZERO                          
453200                 IF FOTNOT-RAKNARE = 3                                    
453300                    MOVE SPAR-IDFOTNR(RAK-IX) TO TREFOT-1                 
453400                    MOVE ')' TO TREFOT-TECK-1                             
453500                 ELSE                                                     
453600                    IF FOTNOT-RAKNARE = 2                                 
453700                       MOVE SPAR-IDFOTNR(RAK-IX) TO TREFOT-2              
453800                       MOVE ')' TO TREFOT-TECK-2                          
453900                    ELSE                                                  
454000                       MOVE SPAR-IDFOTNR(RAK-IX) TO TREFOT-3              
454100                       MOVE ')' TO TREFOT-TECK-3                          
454200                    END-IF                                                
454300                 END-IF                                                   
454400                 SUBTRACT 1 FROM FOTNOT-RAKNARE                           
454500              END-IF                                                      
454600              ADD +1 TO RAK-IX                                            
454700           END-PERFORM                                                    
454800           MOVE SPACE TO TREFOT-SPACE                                     
454900        END-IF                                                            
455000     END-IF                                                               
455100     MOVE SPAR-ANMARK TO MOD-TEKATANM(RAD)                                
455200     .                                                                    
455300     EJECT                                                                
455400 L-RENSA-UTRADER SECTION.                                                 
455500     SKIP2                                                                
455600     PERFORM UNTIL RAD = +13                                              
455700        MOVE MFS-RENSA-FAELT TO MOD-IDCATRAD(RAD)                         
455800                                MOD-KDCATPUB-R-FOM(RAD)                   
455900                                MOD-KDCATPUB-R-TOM(RAD)                   
456000                                MOD-KDFBX(RAD)                            
456100                                MOD-IDCATPOS(RAD)                         
456200                                MOD-IDARTNR(RAD)                          
456300                                MOD-KVKOL(RAD, 1)                         
456400                                MOD-KVKOL(RAD, 2)                         
456500                                MOD-KVKOL(RAD, 3)                         
456600                                MOD-KVKOL(RAD, 4)                         
456700                                MOD-KVKOL(RAD, 5)                         
456800                                MOD-KDPS(RAD)                             
456900                                MOD-KVPUNKT(RAD)                          
457000                                MOD-BEART(RAD)                            
457100                                MOD-TEKATANM(RAD)                         
457200                                MOD-KDRADST(RAD)                          
457300                                MOD-FLNOTE(RAD)                           
457400                                MOD-KDERS(RAD)                            
457500                                MOD-FLH-REF(RAD)                          
457600                                MOD-FLH(RAD)                              
457700        ADD +1 TO RAD                                                     
457800     END-PERFORM                                                          
457900     .                                                                    
458000     EJECT                                                                
458100 M-VISA-BILD-IGEN SECTION.                                                
458200     SKIP2                                                                
458300     MOVE MFS-ROER-EJ-FAELT TO                                            
458400                   MOD-IDCATRAD-FROM                                      
458500                   MOD-IDCATRAD-MITT                                      
458600                   MOD-IDCATRAD-TOM                                       
458700                   MOD-IDCATRAD-FIRST                                     
458800                   MOD-IDCATRAD-SISTA                                     
458900                                                                          
459000     MOVE +1 TO RAD                                                       
459100     PERFORM UNTIL RAD = +13                                              
459200        MOVE MFS-ROER-EJ-FAELT TO MOD-IDCATRAD(RAD)                       
459300                                  MOD-KDCATPUB-R-FOM(RAD)                 
459400                                  MOD-KDCATPUB-R-TOM(RAD)                 
459500                                  MOD-KDFBX(RAD)                          
459600                                  MOD-IDCATPOS(RAD)                       
459700                                  MOD-IDARTNR(RAD)                        
459800                                  MOD-KVKOL(RAD, 1)                       
459900                                  MOD-KVKOL(RAD, 2)                       
460000                                  MOD-KVKOL(RAD, 3)                       
460100                                  MOD-KVKOL(RAD, 4)                       
460200                                  MOD-KVKOL(RAD, 5)                       
460300                                  MOD-KDPS(RAD)                           
460400                                  MOD-KVPUNKT(RAD)                        
460500                                  MOD-BEART(RAD)                          
460600                                  MOD-TEKATANM(RAD)                       
460700                                  MOD-KDRADST(RAD)                        
460800                                  MOD-FLNOTE(RAD)                         
460900                                  MOD-KDERS(RAD)                          
461000                                  MOD-FLH-REF(RAD)                        
461100                                  MOD-FLH(RAD)                            
461200        ADD +1 TO RAD                                                     
461300     END-PERFORM                                                          
461400                                                                          
461500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDCATRAD-UPD                           
461600                               MOD-KDCATPUB-R-FOM-UPD                     
461700                               MOD-KDCATPUB-R-TOM-UPD                     
461800                               MOD-KDFBX-UPD                              
461900                               MOD-IDCATPOS-UPD                           
462000                               MOD-IDARTNR-UPD                            
462100                               MOD-KVKOL-UPD(1)                           
462200                               MOD-KVKOL-UPD(2)                           
462300                               MOD-KVKOL-UPD(3)                           
462400                               MOD-KVKOL-UPD(4)                           
462500                               MOD-KVKOL-UPD(5)                           
462600                               MOD-KDPS-UPD                               
462700                               MOD-KVPUNKT-UPD                            
462800                               MOD-BEART-UPD                              
462900                               MOD-KDHOM-UPD                              
463000                               MOD-IDTTEXNR-UPD                           
463100                               MOD-TEKATANM-UPD                           
463200                               MOD-IDCATGRP-H-UPD                         
463300                               MOD-IDCATAVS-H-UPD                         
463400                               MOD-IDCATRAD-H-UPD                         
463500                               MOD-KDCATPUB-R-H-UPD                       
463600                               MOD-KDHAEN-UPD                             
463700                               MOD-IDCATRAD-BORT-UPD                      
463800*                              MOD-KDCATPUB-R-BORT-UPD                    
463900                               MOD-IDCATPOS-SOEK-UPD                      
464000                               MOD-IDRUBNR-UPD(1)                         
464100                               MOD-IDRUBNR-UPD(2)                         
464200                               MOD-IDRUBNR-UPD(3)                         
464300                               MOD-IDFOTNR-UPD(1)                         
464400                               MOD-IDFOTNR-UPD(2)                         
464500                               MOD-IDFOTNR-UPD(3)                         
464600     .                                                                    
464700     EJECT                                                                
464800 N-FYLL-DOLDA-FALT SECTION.                                               
464900     SKIP2                                                                
465000     MOVE SPAR-IDCATRAD-1       TO MOD-IDCATRAD-FIRST                     
465100     MOVE NUVARANDE-IDCATRAD    TO MOD-IDCATRAD-SISTA                     
465200                                                                          
465300     IF RAD > 12 AND RAD-FINNS = JA                                       
465400        MOVE SPAR-IDCATRAD-1    TO MOD-IDCATRAD-FROM                      
465500        MOVE SPAR-IDCATRAD-7    TO MOD-IDCATRAD-MITT                      
465600        MOVE NUVARANDE-IDCATRAD TO MOD-IDCATRAD-TOM                       
465700*         KDCATPUB(min/max) skall alltid läsas efter VALT urval           
465800        IF MFS-KDTRTYP = SPACE                                            
465900           IF RAD-IDCATRAD = NUVARANDE-IDCATRAD                           
466000             MOVE MED-9 (SPRAAK-IX) TO MOD-TEMFSINF                       
466100           ELSE                                                           
466200             MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF                       
466300           END-IF                                                         
466400        END-IF                                                            
466500     ELSE                                                                 
466600        IF RAD = 13                                                       
466700           MOVE   0020  TO MOD-IDCATRAD-FROM                              
466800           MOVE   9999  TO MOD-IDCATRAD-MITT                              
466900           MOVE   9999  TO MOD-IDCATRAD-TOM                               
467000        ELSE                                                              
467100           IF RAD > 7                                                     
467200              MOVE SPAR-IDCATRAD-1    TO MOD-IDCATRAD-FROM                
467300              MOVE SPAR-IDCATRAD-7    TO MOD-IDCATRAD-MITT                
467400              MOVE   9999             TO MOD-IDCATRAD-TOM                 
467500           ELSE                                                           
467600              IF RAD > 1                                                  
467700                 MOVE SPAR-IDCATRAD-1 TO MOD-IDCATRAD-FROM                
467800                 MOVE   9999          TO MOD-IDCATRAD-MITT                
467900                 MOVE   9999          TO MOD-IDCATRAD-TOM                 
468000                 IF NUVARANDE-IDCATRAD > MOD-IDCATRAD-FIRST               
468100                    CONTINUE                                              
468200                 ELSE                                                     
468300                    MOVE  9999       TO MOD-IDCATRAD-SISTA                
468400                 END-IF                                                   
468500              ELSE                                                        
468600                 MOVE  0020          TO MOD-IDCATRAD-FROM                 
468700                 MOVE  9999          TO MOD-IDCATRAD-MITT                 
468800                 MOVE  9999          TO MOD-IDCATRAD-TOM                  
468900                 MOVE  9999          TO MOD-IDCATRAD-SISTA                
469000              END-IF                                                      
469100           END-IF                                                         
469200        END-IF                                                            
469300     END-IF                                                               
469400     IF EXTRA-RAD = ZERO                                                  
469500        CONTINUE                                                          
469600     ELSE                                                                 
469700        MOVE MED-3 (SPRAAK-IX) TO MOD-TEMFSINF                            
469800     END-IF                                                               
469900     .                                                                    
470000     EJECT                                                                
470100 O-INITIERA-DOLDA-FAELT SECTION.                                          
470200     SKIP2                                                                
470300     MOVE  0020  TO MOD-IDCATRAD-FROM                                     
470400                    MOD-IDCATRAD-PF7                                      
470500     MOVE ZERO   TO MOD-IDCATRAD-MITT                                     
470600                    MOD-IDCATRAD-TOM                                      
470700     MOVE  0020  TO MOD-IDCATRAD-FIRST                                    
470800     MOVE  9999  TO MOD-IDCATRAD-SISTA                                    
470900*      KDCATPUB(min/max) skall alltid läsas efter inmatat urval           
471000     .                                                                    
471100     EJECT                                                                
471200 P-BLANKA-TEST SECTION.                                                   
471300     SKIP2                                                                
471400     MOVE SPACE       TO TEST-KDCATPUB-FOM                                
471500                         TEST-KDCATPUB-TOM                                
471600                         TEST-KDFBX                                       
471700                         TEST-IDCATPOS-HEL                                
471800     MOVE ZERO        TO TEST-IDARTNR                                     
471900                                                                          
472000     MOVE +1 TO INDX                                                      
472100     PERFORM UNTIL INDX = +6                                              
472200        MOVE SPACE    TO TEST-KVKOL(INDX)                                 
472300        ADD +1 TO INDX                                                    
472400     END-PERFORM                                                          
472500     MOVE SPACE       TO TEST-KDPS                                        
472600                         TEST-BEART                                       
472700     MOVE ZERO        TO TEST-KVPUNKT                                     
472800                         TEST-KDHOM                                       
472900                         TEST-IDTTEXNR                                    
473000     MOVE +1 TO INDX                                                      
473100     PERFORM UNTIL INDX = +4                                              
473200        MOVE ZERO     TO TEST-IDRUBNR(INDX)                               
473300                         TEST-IDFOTNR(INDX)                               
473400        ADD +1 TO INDX                                                    
473500     END-PERFORM                                                          
473600     .                                                                    
473700     EJECT                                                                
473800 Q-KOLLA-EXTRA-BEART SECTION.                                             
473900     SKIP2                                                                
474000     IF (UT-BERUBTXT(1) NOT = SPACE) OR                                   
474100        (UT-BERUBTXT(2) NOT = SPACE) OR                                   
474200        (UT-BERUBTXT(3) NOT = SPACE)                                      
474300        MOVE +1 TO RAK-IX                                                 
474400        PERFORM UNTIL RAK-IX = +4                                         
474500           IF UT-BERUBTXT(RAK-IX) NOT = SPACE                             
474600              MOVE UT-BERUBTXT(RAK-IX) TO MOD-BEART(RAD)                  
474700              SUBTRACT +1 FROM EXTRA-RAD                                  
474800              MOVE SPACE TO UT-BERUBTXT(RAK-IX)                           
474900              MOVE +3 TO RAK-IX                                           
475000           END-IF                                                         
475100           ADD +1 TO RAK-IX                                               
475200        END-PERFORM                                                       
475300     ELSE                                                                 
475400        MOVE UT-BETTEXT TO MOD-BEART(RAD)                                 
475500        MOVE ZERO TO EXTRA-RAD                                            
475600        MOVE SPACE TO UT-BETTEXT                                          
475700     END-IF                                                               
475800     .                                                                    
475900     EJECT                                                                
476000 R-FLYTTA-LAS-START SECTION.                                              
476100     SKIP2                                                                
476200     MOVE KEY-KDCATPUB-MIN-X TO W-KDCATPUB-MIN                            
476300     MOVE KEY-KDCATPUB-MAX-X TO W-KDCATPUB-MAX                            
476400     IF MID-IDCATRAD-UPD < MID-IDCATRAD-FROM                              
476500        MOVE KEY-IDCATRAD TO W-IDCATRAD                                   
476600                             W-IDCATRAD-MIN                               
476700        MOVE  9999        TO W-IDCATRAD-MAX                               
476800     ELSE                                                                 
476900        MOVE MID-IDCATRAD-FIRST TO W-IDCATRAD                             
477000                                   W-IDCATRAD-MIN                         
477100        MOVE  9999        TO W-IDCATRAD-MAX                               
477200        IF MID-IDCATRAD-SISTA =   9999                                    
477300           CONTINUE                                                       
477400        ELSE                                                              
477500           IF MID-IDCATRAD-UPD > MID-IDCATRAD-SISTA                       
477600              IF MID-IDCATRAD-MITT =  9999 AND                            
477700                 MID-IDCATRAD-TOM =  9999                                 
477800*************    IF AENDR-FL = NEJ                                        
477900*************       ADD +1 TO W-IDCATRAD                                  
478000*************    END-IF                                                   
478100                 CONTINUE                                                 
478200              ELSE                                                        
478300                 CONTINUE                                                 
478400              END-IF                                                      
478500           ELSE                                                           
478600              IF MID-IDCATRAD-UPD > MID-IDCATRAD-MITT                     
478700                 IF AENDR-FL = NEJ                                        
478800                    ADD +1 TO W-IDCATRAD                                  
478900                              W-IDCATRAD-MIN                              
479000                 END-IF                                                   
479100              ELSE                                                        
479200                 CONTINUE                                                 
479300              END-IF                                                      
479400           END-IF                                                         
479500        END-IF                                                            
479600     END-IF                                                               
479700     .                                                                    
479800     EJECT                                                                
479900 S01-KOLLA-WDN501-12 SECTION.                                             
480000     SKIP2                                                                
480100     MOVE W-IDCATNR     TO W-IDCATNR-H                                    
480200     MOVE WS-GRPAVS-H   TO W-WDN501-H-GRPAVS                              
480300     MOVE WS-IDCATRAD-H TO W-IDCATRAD-H                                   
480400     MOVE WS-KDCATPUB-H TO W-KDCATPUB-H                                   
480500     PERFORM IMS-GU-AVS-RAD-IO2                                           
480600                                                                          
480700     IF SEGMENT-SAKNAS                                                    
480800       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHAEN-UPD-ATTR                     
480900                                  MOD-KDCATPUB-R-H-UPD-ATTR               
481000       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDCATRAD-H-UPD-ATTR                 
481100       MOVE JA TO INDATA-FEL                                              
481200     END-IF                                                               
481300     .                                                                    
481400     EJECT                                                                
481500 S02F-KDCATPUB-FOM-TABELL SECTION.                                        
481600     SKIP2                                                                
481700     MOVE NEJ TO INDATA-FEL                                               
481800                                                                          
481900     PERFORM IMS-GET-KATM-TAB                                             
482000     IF SEGMENT-FINNS                                                     
482100       MOVE +1 TO IX                                                      
482200       PERFORM UNTIL IX > +12                                             
482300         IF TEST-KDCATPUB-FOM = KAT-TAB-KDCATPUB-FOM(IX)                  
482400           MOVE +99 TO IX                                                 
482500         END-IF                                                           
482600         ADD +1 TO IX                                                     
482700       END-PERFORM                                                        
482800       IF IX < +99                                                        
482900         MOVE JA TO INDATA-FEL                                            
483000       END-IF                                                             
483100     ELSE                                                                 
483200       MOVE JA TO INDATA-FEL                                              
483300     END-IF                                                               
483400     .                                                                    
483500     EJECT                                                                
483600 S02T-KDCATPUB-TOM-TABELL SECTION.                                        
483700     SKIP2                                                                
483800     MOVE NEJ TO INDATA-FEL                                               
483900                                                                          
484000     PERFORM IMS-GET-KATM-TAB                                             
484100     IF SEGMENT-FINNS                                                     
484200       MOVE +1 TO IX                                                      
484300       PERFORM UNTIL IX > +12                                             
484400         IF TEST-KDCATPUB-TOM = KAT-TAB-KDCATPUB-TOM(IX)                  
484500           MOVE +99 TO IX                                                 
484600         END-IF                                                           
484700         ADD +1 TO IX                                                     
484800       END-PERFORM                                                        
484900       IF IX < +99                                                        
485000         MOVE JA TO INDATA-FEL                                            
485100       END-IF                                                             
485200     ELSE                                                                 
485300       MOVE JA TO INDATA-FEL                                              
485400     END-IF                                                               
485500     .                                                                    
485600     EJECT                                                                
485700 S03-KOLLA-RAD-FROM SECTION.                                              
485800     SKIP2                                                                
485900     IF  (MID-IDCATNR-IN  = ALL '+')                                      
486000     AND (MID-IDCATGRP-IN = ALL '+')                                      
486100     AND (MID-IDCATAVS-IN = ALL '+')                                      
486200     AND (MID-IDCATRAD-IN = ALL '+')                                      
486300     AND (MID-KDCATPUB-R-MIN-IN = ALL '+')                                
486400     AND (MID-KDCATPUB-R-MAX-IN = ALL '+')                                
486500     AND (MID-IDCATPOS-SOEK-UPD = ALL '+')                                
486600                                                                          
486700        MOVE  0020 TO KEY-IDCATRAD                                        
486800                                                                          
486900        IF MFS-IDTRANS = '1512'                                           
487000           MOVE MID-IDCATRAD-FROM TO KEY-IDCATRAD                         
487100        ELSE                                                              
487200           PERFORM S03A-FLYTTA-IN-IDCATRAD                                
487300        END-IF                                                            
487400     ELSE                                                                 
487500        IF MFS-IDTRANS = '1512'                                           
487600           IF MID-IDCATPOS-SOEK-UPD NOT = ALL '+'                         
487700              PERFORM S03B-SOEK-POS                                       
487800           END-IF                                                         
487900        ELSE                                                              
488000           PERFORM S03A-FLYTTA-IN-IDCATRAD                                
488100        END-IF                                                            
488200     END-IF                                                               
488300     .                                                                    
488400     EJECT                                                                
488500 S03A-FLYTTA-IN-IDCATRAD SECTION.                                         
488600     SKIP2                                                                
488700     IF MID-IDCATRAD-UT  NOT = ALL '+'                                    
488800       IF (MID-IDCATNR-IN   NOT = ALL '+')                                
488900       OR (MID-IDCATGRP-IN  NOT = ALL '+')                                
489000       OR (MID-IDCATAVS-IN  NOT = ALL '+')                                
489100       OR (MID-KDCATPUB-R-MIN-IN  NOT = ALL '+')                          
489200       OR (MID-KDCATPUB-R-MAX-IN  NOT = ALL '+')                          
489300          IF MID-IDCATRAD-IN   NOT = ALL '+'                              
489400*            --> REDAN FLYTTAD I A-INIT                                   
489500             CONTINUE                                                     
489600          ELSE                                                            
489700             MOVE 0020 TO KEY-IDCATRAD                                    
489800          END-IF                                                          
489900       ELSE                                                               
490000         IF MID-IDCATRAD-IN NOT = ALL '+'                                 
490100*          --> REDAN FLYTTAD I A-INIT                                     
490200           CONTINUE                                                       
490300         ELSE                                                             
490400           MOVE MID-IDCATRAD-UT TO KEY-IDCATRAD-X                         
490500           INSPECT KEY-IDCATRAD-X REPLACING LEADING SPACE BY ZERO         
490600           IF KEY-IDCATRAD-X NUMERIC                                      
490700             IF KEY-IDCATRAD < 0020                                       
490800               MOVE 0020 TO KEY-IDCATRAD                                  
490900             END-IF                                                       
491000           END-IF                                                         
491100         END-IF                                                           
491200       END-IF                                                             
491300     END-IF                                                               
491400     .                                                                    
491500     EJECT                                                                
491600 S03B-SOEK-POS SECTION.                                                   
491700     SKIP2                                                                
491800     IF WS-IDCATPOS-SOEK-TEST(1:1) ALPHABETIC                             
491900     OR WS-IDCATPOS-SOEK-TEST(1:1) = ZERO                                 
492000     OR WS-IDCATPOS-SOEK-TEST      = SPACE                                
492100       MOVE FEL-2(SPRAAK-IX)       TO MOD-TEMFSFEL                        
492200*test                                                                     
492300*      STRING 'UPPL FÄLT FEL 7. POS=' WS-IDCATPOS-SOEK-TEST               
492400*      DELIMITED BY SIZE INTO MOD-TEMFSFEL                                
492500*test-end                                                                 
492600       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-SOEK-UPD-ATTR              
492700       MOVE NEJ TO SOEK-DATA-SW                                           
492800       MOVE 0020 TO KEY-IDCATRAD                                          
492900     ELSE                                                                 
493000       MOVE WS-IDCATPOS-SOEK-TEST TO W-IDCATPOS-LO                        
493100                                                                          
493200       PERFORM IMS-GN-AVS-POS                                             
493300                                                                          
493400       IF SEGMENT-FINNS                                                   
493500         MOVE AVS-KEY-FB-IDCATRAD TO KEY-IDCATRAD                         
493600       ELSE                                                               
493700         MOVE 0020   TO KEY-IDCATRAD                                      
493800         MOVE 'FEL'  TO MOD-IDCATPOS-SOEK-UPD                             
493900         MOVE  NEJ   TO SOEK-DATA-SW                                      
494000       END-IF                                                             
494100     END-IF                                                               
494200     .                                                                    
494300     EJECT                                                                
494400 S04-FYLL-DOLT-FAELT SECTION.                                             
494500     SKIP2                                                                
494600     MOVE 0020 TO MOD-IDCATRAD-FROM                                       
494700                  MOD-IDCATRAD-FIRST                                      
494800     MOVE 9999 TO MOD-IDCATRAD-MITT                                       
494900                  MOD-IDCATRAD-TOM                                        
495000                  MOD-IDCATRAD-SISTA                                      
495100                                                                          
495200     IF MID-IDCATRAD-PF7 <= 0020                                          
495300        MOVE 0020 TO MID-IDCATRAD-PF7                                     
495400                     MOD-IDCATRAD-PF7                                     
495500     END-IF                                                               
495600     .                                                                    
495700     EJECT                                                                
495800 S05-AVSNITT-UPPDAT-STATUS SECTION.                                       
495900     SKIP2                                                                
496000*    Här behandlas UPPDATE-typerna 'Ä'  'N'  'B'                          
496100                                                                          
496200     PERFORM IMS-GHU-AVS                                                  
496300     IF SEGMENT-FINNS                                                     
496400     AND AVS-FLAVSUST = JA                                                
496500*      -- Avsnittet är redan markerat                                     
496600       CONTINUE                                                           
496700     ELSE                                                                 
496800       IF UPPDATE-FL = 'B'                                                
496900*        -- Kollar om den borttagna raden gällde                          
497000*        -- vid SENaste VADIS-GENereringen                                
497100         IF  WS-KDCATPUB-FOM-BORTRAD <=   WS-KDCATPUB-SEN-GEN             
497200         AND WS-KDCATPUB-TOM-BORTRAD >=   WS-KDCATPUB-SEN-GEN             
497300           MOVE JA TO AVS-FLAVSUST                                        
497400         END-IF                                                           
497500       ELSE                                                               
497600*        -- Kollar om den uppdaterade raden gällde/gäller                 
497700*        -- vid SENaste VADIS-GENereringen                                
497800         IF  WS-KDCATPUB-FOM-UPD <=   WS-KDCATPUB-SEN-GEN                 
497900         AND WS-KDCATPUB-TOM-UPD >=   WS-KDCATPUB-SEN-GEN                 
498000           MOVE JA TO AVS-FLAVSUST                                        
498100         END-IF                                                           
498200       END-IF                                                             
498300       PERFORM IMS-REPL-AVS                                               
498400*      --- Kolla nu om det finns referenser till detta avsnitt            
498500*      --- I så fall, skall de avsnitten oxå ha FLAVSUST=JA               
498600*      --- Detta för att de oxå skall komma med i VADIS-GENEN             
498700       IF AVS-FLAVSUST = JA                                               
498800         PERFORM IMS-GU-AVS                                               
498900*        --- Läs alla REF-segment under rad NOLL                          
499000*        --- Dessa är avsnitt som hänvisar till detta generellt           
499100         MOVE W-IDCATNR  TO W-IDCATNR-GSEQ                                
499200         MOVE W-IDCATGRP TO W-IDCATGRP-GSEQ                               
499300         MOVE W-IDCATAVS TO W-IDCATAVS-GSEQ                               
499400         MOVE ZERO       TO W-IDCATRAD-GSEQ                               
499500         MOVE LOW-VALUE  TO W-KDCATPUB-GSEQ                               
499600         PERFORM IMS-GU-AVSG-HAEN                                         
499700                                                                          
499800         IF SEGMENT-FINNS                                                 
499900           MOVE W-IDCATNR TO W-IDCATNR-H                                  
500000           PERFORM UNTIL SEGMENT-SAKNAS                                   
500100*            --- Uppdatera hänvisande avsnitts FLAVSUST                   
500200*            --- med PCB-2                                                
500300             MOVE AVSG-IDWDN512  TO  AVSG-REF-IDWDN512                    
500400                                                                          
500500             MOVE AVSG-REF-IDCATGRP TO W-IDCATGRP-H                       
500600             MOVE AVSG-REF-IDCATAVS TO W-IDCATAVS-H                       
500700             PERFORM IMS-GHU-AVS-IO2                                      
500800             MOVE JA TO IO2-AVS-FLAVSUST                                  
500900             PERFORM IMS-REPL-AVS-IO2                                     
501000*            --- PCB-1's pekare står kvar under rätt rot                  
501100*            --- Hämta nästa referens till detta avsnitt                  
501200             PERFORM IMS-GN-AVSG-HAEN                                     
501300           END-PERFORM                                                    
501400         END-IF                                                           
501500       END-IF                                                             
501600     END-IF                                                               
501700     .                                                                    
501800     EJECT                                                                
501900*S50-Y2K-KDCATPUB-R LIGGER I                                              
502000*COPYTEXT W.PROD.COBOL.W150Y2K1                                           
502100*                                                                         
502200*    -COPY W150Y2K1                                                       
502300     EJECT                                                                
502400 T-VISA-UPPDAT-RAD  SECTION.                                              
502500     SKIP2                                                                
502600     MOVE NEJ TO INDATA-FEL                                               
502700*                            läs raden i uppdat                           
502800*    MOVE MID-IDCATRAD-UPD TO MOD-IDCATRAD-UPD  W-IDCATRAD                
502900*                                                                         
503000*     --- Nedanstående är ett gott försök att få radnumret                
503100*     --- vänsterställt så att man inte behöver flytta cursorn            
503200*     --- åt höger när man har tagit ner en rad.                          
503300*                                                                         
503400     MOVE MID-IDCATRAD-UPD TO WS-MID-IDCATRAD-UPD  W-IDCATRAD             
503500     MOVE ZERO TO POS                                                     
503600     INSPECT WS-MID-IDCATRAD-UPD TALLYING POS FOR LEADING ZERO            
503700     IF POS < 4                                                           
503800       STRING MID-IDCATRAD-UPD((POS + 1):(4 - POS))                       
503900       MFS-RENSA-FAELT DELIMITED BY SIZE INTO MOD-IDCATRAD-UPD            
504000     ELSE                                                                 
504100       MOVE MFS-RENSA-FAELT  TO MOD-IDCATRAD-UPD                          
504200     END-IF                                                               
504300                                                                          
504400     IF MID-KDCATPUB-R-FOM-UPD NOT = ALL '+'                              
504500       INSPECT MID-KDCATPUB-R-FOM-UPD REPLACING LEADING                   
504600                                    SPACE BY LOW-VALUE                    
504700                                                                          
504800       IF MID-KDCATPUB-R-FOM-UPD = LOW-VALUE                              
504900         MOVE LOW-VALUE TO W-KDCATPUB-X                                   
505000         MOVE SPACE     TO MOD-KDCATPUB-R-FOM-UPD                         
505100       ELSE                                                               
505200         MOVE MID-KDCATPUB-R-FOM-UPD TO WS-KDCATPUB-R-AVV                 
505300         PERFORM S50-Y2K-KDCATPUB-R                                       
505400         IF WS-KDCATPUB-AAAAVV = SPACE                                    
505500*          --- När inmatad PUBKOD är utanför godk års-intervall           
505600*          --- lägg in "blank" PUB-FOM.                                   
505700           MOVE LOW-VALUE TO W-KDCATPUB-X                                 
505800           MOVE SPACE     TO MOD-KDCATPUB-R-FOM-UPD                       
505900         ELSE                                                             
506000           MOVE WS-KDCATPUB-AAAAVV TO W-KDCATPUB-X                        
506100           MOVE WS-KDCATPUB-R-AVV  TO MOD-KDCATPUB-R-FOM-UPD              
506200         END-IF                                                           
506300       END-IF                                                             
506400     ELSE                                                                 
506500       MOVE LOW-VALUE TO W-KDCATPUB-X                                     
506600       MOVE SPACE     TO MOD-KDCATPUB-R-FOM-UPD                           
506700     END-IF                                                               
506800                                                                          
506900     MOVE MFS-OEPPNA-NUM-FAELT  TO MOD-IDCATRAD-UPD-ATTR                  
507000     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDCATPUB-R-FOM-UPD-ATTR            
507100     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDCATPUB-R-TOM-UPD-ATTR            
507200                                                                          
507300     PERFORM IMS-GU-AVS                                                   
507400     PERFORM IMS-GNP-AVS-RAD                                              
507500                                                                          
507600     IF SEGMENT-FINNS                                                     
507700*           WDN512                                                        
507800       MOVE RAD-KDCATPUB-TOM(4:3) TO MOD-KDCATPUB-R-TOM-UPD               
507900       INSPECT MOD-KDCATPUB-R-TOM-UPD REPLACING LEADING                   
508000                                    HIGH-VALUE BY SPACE                   
508100       PERFORM IMS-GNP-AVS-INFO                                           
508200                                                                          
508300       IF SEGMENT-FINNS                                                   
508400*           WDN521                                                        
508500         MOVE ART-KDFBX            TO MOD-KDFBX-UPD                       
508600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDFBX-UPD-ATTR                  
508700         MOVE ART-IDCATPOS         TO TEST-IDCATPOS-HEL                   
508800                                      MOD-IDCATPOS-UPD                    
508900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATPOS-UPD-ATTR               
509000         IF ART-IDARTNR = ZERO                                            
509100           MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-UPD                     
509200         ELSE                                                             
509300           MOVE ART-IDARTNR        TO WS-IDARTNR-UNSTRING                 
509400           MOVE ZERO TO POS                                               
509500           INSPECT WS-IDARTNR-UNSTRING                                    
509600           TALLYING POS FOR LEADING ZERO                                  
509700           IF POS < 9                                                     
509800             STRING WS-IDARTNR-UNSTRING((POS + 1):(9 - POS))              
509900                    MFS-RENSA-FAELT                                       
510000             DELIMITED BY SIZE   INTO MOD-IDARTNR-UPD                     
510100           ELSE                                                           
510200             MOVE ART-IDARTNR      TO MOD-IDARTNR-UPD                     
510300           END-IF                                                         
510400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-UPD-ATTR               
510500         END-IF                                                           
510600                                                                          
510700         MOVE +1 TO IX                                                    
510800         PERFORM UNTIL IX > +5                                            
510900*          --- Lägger kol-värdet vä-ställt i mod-en                       
511000           IF ART-KVKOL(IX) = SPACE                                       
511100             MOVE SPACE TO MOD-KVKOL-UPD(IX)                              
511200           ELSE                                                           
511300             IF ART-KVKOL(IX)(1:1) = SPACE                                
511400               IF ART-KVKOL(IX)(2:1) = SPACE                              
511500                 MOVE ART-KVKOL(IX)(3:1) TO MOD-KVKOL-UPD(IX)             
511600               ELSE                                                       
511700                 IF ART-KVKOL(IX)(3:1) = SPACE                            
511800                   MOVE ART-KVKOL(IX)(2:1) TO MOD-KVKOL-UPD(IX)           
511900                 ELSE                                                     
512000                   MOVE ART-KVKOL(IX)(2:2) TO MOD-KVKOL-UPD(IX)           
512100                 END-IF                                                   
512200               END-IF                                                     
512300             ELSE                                                         
512400               MOVE ART-KVKOL(IX) TO MOD-KVKOL-UPD(IX)                    
512500             END-IF                                                       
512600           END-IF                                                         
512700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOL-UPD-ATTR (IX)           
512800           ADD +1 TO IX                                                   
512900         END-PERFORM                                                      
513000         MOVE ART-KDPS             TO MOD-KDPS-UPD                        
513100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPS-UPD-ATTR                   
513200         MOVE ART-KVPUNKT          TO MOD-KVPUNKT-UPD                     
513300         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPUNKT-UPD-ATTR                 
513400         MOVE ART-IDTTEXNR         TO MOD-IDTTEXNR-UPD                    
513500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDTTEXNR-UPD-ATTR                
513600       END-IF                                                             
513700                                                                          
513800       PERFORM IMS-GNP-AVS-TEXT                                           
513900*           WDN522                                                        
514000       IF SEGMENT-FINNS                                                   
514100         MOVE TEXT-TEKATANM        TO MOD-TEKATANM-UPD                    
514200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEKATANM-UPD-ATTR               
514300       END-IF                                                             
514400                                                                          
514500*      KOLLA OM ARTIKELN FINNS PÅ BENREG                                  
514600       PERFORM IMS-GU-BENB-SEQ                                            
514700       IF SEGMENT-FINNS                                                   
514800         MOVE MFS-RENSA-FAELT      TO MOD-KDHOM-UPD                       
514900                                      MOD-BEART-UPD                       
515000       ELSE                                                               
515100*        LÄS IN BEART FRÅN CATAVS                                         
515200         PERFORM IMS-GNP-AVS-BEN                                          
515300*             WDN523                                                      
515400         IF SEGMENT-FINNS                                                 
515500           MOVE BEN-KDHOM            TO MOD-KDHOM-UPD                     
515600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHOM-UPD-ATTR                 
515700           MOVE BEN-BEART            TO MOD-BEART-UPD                     
515800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-UPD-ATTR                
515900         ELSE                                                             
516000           MOVE MFS-RENSA-FAELT      TO MOD-KDHOM-UPD                     
516100                                        MOD-BEART-UPD                     
516200         END-IF                                                           
516300       END-IF                                                             
516400                                                                          
516500       PERFORM IMS-GNP-AVS-NOT                                            
516600*           WDN524                                                        
516700       PERFORM UNTIL SEGMENT-SAKNAS                                       
516800         IF NOT-IDSEGMNR = +1                                             
516900           MOVE MED-6 (SPRAAK-IX) TO MOD-TEMFSINF                         
517000*             'NOTERING (1513) KAN EJ KOPIERAS TILL NY RAD '              
517100         END-IF                                                           
517200*                                                                         
517300         IF NOT-IDSEGMNR = +2                                             
517400           MOVE NOT-IDCATGRP     TO MOD-IDCATGRP-H-UPD                    
517500           MOVE NOT-IDCATAVS     TO MOD-IDCATAVS-H-UPD                    
517600           MOVE NOT-IDCATRAD     TO MOD-IDCATRAD-H-UPD                    
517700           MOVE NOT-KDCATPUB-FOM(4:3) TO MOD-KDCATPUB-R-H-UPD             
517800           MOVE '*'              TO MOD-KDHAEN-UPD                        
517900         END-IF                                                           
518000*                                                                         
518100         PERFORM IMS-GNP-AVS-NOT                                          
518200       END-PERFORM                                                        
518300                                                                          
518400       PERFORM IMS-GNP-AVS-RUB                                            
518500*           WDN525                                                        
518600       PERFORM UNTIL SEGMENT-SAKNAS                                       
518700         MOVE RUB-IDRUBNR   TO MOD-IDRUBNR-UPD (RUB-IDSEGMNR)             
518800         MOVE MFS-NUM-FAELT-RAETT                                         
518900                            TO MOD-IDRUBNR-UPD-ATTR (RUB-IDSEGMNR)        
519000         PERFORM IMS-GNP-AVS-RUB                                          
519100       END-PERFORM                                                        
519200                                                                          
519300       PERFORM IMS-GNP-AVS-FOT                                            
519400*             WDN526                                                      
519500       PERFORM UNTIL SEGMENT-SAKNAS                                       
519600         MOVE FOT-IDFOTNR  TO MOD-IDFOTNR-UPD (FOT-IDSEGMNR)              
519700         MOVE MFS-NUM-FAELT-RAETT                                         
519800                           TO MOD-IDFOTNR-UPD-ATTR (FOT-IDSEGMNR)         
519900         PERFORM IMS-GNP-AVS-FOT                                          
520000       END-PERFORM                                                        
520100                                                                          
520200       PERFORM IMS-GNP-AVS-HAEN                                           
520300*            WDN527                                                       
520400       IF SEGMENT-FINNS                                                   
520500         MOVE HAEN-IDCATGRP        TO MOD-IDCATGRP-H-UPD                  
520600         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDCATGRP-H-UPD-ATTR             
520700         MOVE HAEN-IDCATAVS        TO MOD-IDCATAVS-H-UPD                  
520800         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDCATAVS-H-UPD-ATTR             
520900         MOVE HAEN-IDCATRAD        TO MOD-IDCATRAD-H-UPD                  
521000         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDCATRAD-H-UPD-ATTR             
521100         MOVE HAEN-KDCATPUB-FOM(4:3) TO MOD-KDCATPUB-R-H-UPD              
521200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-H-UPD-ATTR           
521300         MOVE HAEN-KDHAEN          TO MOD-KDHAEN-UPD                      
521400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDHAEN-UPD-ATTR                 
521500       END-IF                                                             
521600     ELSE                                                                 
521700*      RADEN FINNS INTE                                                   
521800       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDCATRAD-UPD-ATTR                   
521900       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FOM-UPD-ATTR             
522000       MOVE      JA            TO INDATA-FEL                              
522100       MOVE MED-7 (SPRAAK-IX)  TO MOD-TEMFSINF                            
522200     END-IF                                                               
522300     .                                                                    
522400     EJECT                                                                
522500 X-GEMENSAM-KOLL SECTION.                                                 
522600     SKIP2                                                                
522700     PERFORM XA-TEST-UTAN-BAS                                             
522800                                                                          
522900     IF INDATA-FEL = NEJ                                                  
523000        PERFORM XB-TEST-MED-BAS                                           
523100     END-IF                                                               
523200     .                                                                    
523300     EJECT                                                                
523400 XA-TEST-UTAN-BAS SECTION.                                                
523500     SKIP2                                                                
523600     PERFORM XAA-BEART-KDPS                                               
523700                                                                          
523800     PERFORM XAB-KDPS-KVKOL                                               
523900                                                                          
524000     PERFORM XAC-KDPS-IDARTNR                                             
524100                                                                          
524200     PERFORM XAD-IDRUBNR-BEART-KDHOM                                      
524300                                                                          
524400     PERFORM XAE-KDFBX-IDCATPOS                                           
524500     .                                                                    
524600     EJECT                                                                
524700 XAA-BEART-KDPS SECTION.                                                  
524800     SKIP2                                                                
524900*- - - - - - - - - - - BEART KOMBINERAT KDPS                              
525000     IF TEST-BEART NOT = SPACE                                            
525100        IF TEST-KDPS = 'IK' OR 'KS'                                       
525200           IF UPPDATE-FL = 'N'                                            
525300              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR               
525400              MOVE JA TO INDATA-FEL                                       
525500           ELSE                                                           
525600              IF MID-BEART-UPD NOT = ALL '+'                              
525700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR            
525800                 MOVE JA TO INDATA-FEL                                    
525900              END-IF                                                      
526000              IF MID-KDPS-UPD NOT = ALL '+'                               
526100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR             
526200                 MOVE JA TO INDATA-FEL                                    
526300              END-IF                                                      
526400           END-IF                                                         
526500        END-IF                                                            
526600     ELSE                                                                 
526700        IF TEST-KDPS = 'XX' OR 'LS' OR 'KL'                               
526800           IF UPPDATE-FL = 'N'                                            
526900              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR               
527000              MOVE JA TO INDATA-FEL                                       
527100           ELSE                                                           
527200              IF MID-BEART-UPD NOT = ALL '+'                              
527300                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR            
527400                 MOVE JA TO INDATA-FEL                                    
527500              END-IF                                                      
527600              IF MID-KDPS-UPD NOT = ALL '+'                               
527700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR             
527800                 MOVE JA TO INDATA-FEL                                    
527900              END-IF                                                      
528000           END-IF                                                         
528100        END-IF                                                            
528200     END-IF                                                               
528300     .                                                                    
528400     EJECT                                                                
528500 XAB-KDPS-KVKOL SECTION.                                                  
528600     SKIP2                                                                
528700*- - - - - - - - - - - KDPS KOMBINERAT KVKOL                              
528800     IF TEST-KDPS NOT = ALL '+'                                           
528900        IF TEST-KDPS = 'LS' OR 'KL' OR 'IK' OR                            
529000                       'KS' OR 'NS' OR 'KN'                               
529100                                                                          
529200           IF (TEST-KVKOL(1) = SPACE) AND                                 
529300              (TEST-KVKOL(2) = SPACE) AND                                 
529400              (TEST-KVKOL(3) = SPACE) AND                                 
529500              (TEST-KVKOL(4) = SPACE) AND                                 
529600              (TEST-KVKOL(5) = SPACE)                                     
529700              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR                
529800              MOVE JA TO INDATA-FEL                                       
529900           END-IF                                                         
530000        END-IF                                                            
530100     END-IF                                                               
530200     .                                                                    
530300     EJECT                                                                
530400 XAC-KDPS-IDARTNR SECTION.                                                
530500     SKIP2                                                                
530600*- - - - - - - - - - - KDPS kombinerat idartnr                            
530700     IF TEST-IDARTNR NOT = ZERO                                           
530800*- - - - - - - - - - - KDPS sätts maskinellt till 'EU' för                
530900*- - - - - - - - - - - BYTES-ARTIKLAR                                     
531000        MOVE TEST-IDARTNR TO BYTES-IDARTNR                                
531100        IF BYT02-RENOV                                                    
531200           IF TEST-KDPS NOT = SPACE                                       
531300              IF UPPDATE-FL = 'N'                                         
531400                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR             
531500                 MOVE JA TO INDATA-FEL                                    
531600              ELSE                                                        
531700                 IF MID-KDPS-UPD NOT = ALL '+'                            
531800                    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR          
531900                    MOVE JA TO INDATA-FEL                                 
532000                 ELSE                                                     
532100                    IF MID-IDARTNR-UPD NOT = ALL '+'                      
532200                       MOVE MFS-NUM-FAELT-FEL                             
532300                                        TO MOD-IDARTNR-UPD-ATTR           
532400                       MOVE JA TO INDATA-FEL                              
532500                    END-IF                                                
532600                 END-IF                                                   
532700              END-IF                                                      
532800           END-IF                                                         
532900        END-IF                                                            
533000     END-IF                                                               
533100                                                                          
533200     IF TEST-KDPS NOT = SPACE                                             
533300        IF TEST-IDARTNR = ZERO                                            
533400           IF TEST-KDPS = 'XX' OR 'NS' OR 'KN' OR '  '                    
533500              CONTINUE                                                    
533600           ELSE                                                           
533700              IF UPPDATE-FL = 'N'                                         
533800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR             
533900                 MOVE JA TO INDATA-FEL                                    
534000              ELSE                                                        
534100                 IF MID-KDPS-UPD NOT = ALL '+'                            
534200                    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR          
534300                    MOVE JA TO INDATA-FEL                                 
534400                 ELSE                                                     
534500                    IF MID-IDARTNR-UPD NOT = ALL '+'                      
534600                       MOVE MFS-NUM-FAELT-FEL                             
534700                                        TO MOD-IDARTNR-UPD-ATTR           
534800                       MOVE JA TO INDATA-FEL                              
534900                    END-IF                                                
535000                 END-IF                                                   
535100              END-IF                                                      
535200           END-IF                                                         
535300        END-IF                                                            
535400     ELSE                                                                 
535500        IF TEST-IDARTNR NOT = ZERO                                        
535600           IF TEST-BEART NOT = SPACE                                      
535700              IF UPPDATE-FL = 'N'                                         
535800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR            
535900                 MOVE JA TO INDATA-FEL                                    
536000              ELSE                                                        
536100                 IF MID-BEART-UPD NOT = ALL '+'                           
536200                    MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR         
536300                    MOVE JA TO INDATA-FEL                                 
536400                 ELSE                                                     
536500                    IF MID-KDPS-UPD NOT = ALL '+'                         
536600                       MOVE MFS-ALFA-FAELT-FEL                            
536700                                       TO MOD-KDPS-UPD-ATTR               
536800                       MOVE JA TO INDATA-FEL                              
536900                    ELSE                                                  
537000                       IF MID-IDARTNR-UPD NOT = ALL '+'                   
537100                          MOVE MFS-NUM-FAELT-FEL TO                       
537200                               MOD-IDARTNR-UPD-ATTR                       
537300                          MOVE JA TO INDATA-FEL                           
537400                       END-IF                                             
537500                    END-IF                                                
537600                 END-IF                                                   
537700              END-IF                                                      
537800           END-IF                                                         
537900        END-IF                                                            
538000     END-IF                                                               
538100     .                                                                    
538200     EJECT                                                                
538300 XAD-IDRUBNR-BEART-KDHOM SECTION.                                         
538400     SKIP2                                                                
538500*- - - - - - - - - - - IDRUBNR KOMBINERAT BEART, KDHOM                    
538600     IF IDRUBNR-FINNS = JA                                                
538700        IF (TEST-BEART    NOT = SPACE) OR                                 
538800           (TEST-KDHOM    NOT = ZERO)                                     
538900                                                                          
539000           IF (MID-BEART-UPD NOT = ALL '+') OR                            
539100              (MID-BEART-UPD = SPACE)                                     
539200              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR               
539300              MOVE JA TO INDATA-FEL                                       
539400           END-IF                                                         
539500                                                                          
539600           IF MID-KDHOM-UPD = ALL '+'                                     
539700              CONTINUE                                                    
539800           ELSE                                                           
539900              IF MID-KDHOM-UPD = ZERO                                     
540000                 CONTINUE                                                 
540100              ELSE                                                        
540200                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR             
540300                 MOVE JA TO INDATA-FEL                                    
540400              END-IF                                                      
540500           END-IF                                                         
540600        END-IF                                                            
540700     END-IF                                                               
540800     .                                                                    
540900 XAE-KDFBX-IDCATPOS    SECTION.                                           
541000     SKIP2                                                                
541100     IF TEST-KDFBX = 'F'                                                  
541200       MOVE SPACE TO TEST-IDCATPOS-HEL                                    
541300*      MASKINELL RÄTTNING AV POSNUMMER PÅ FORTS-RAD                       
541400     ELSE                                                                 
541500       IF  TEST-IDCATPOS-HEL = SPACE                                      
541600       AND  KEY-IDCATAVS     = 1                                          
541700*        --- Avsnitt 1 får ha blanka poser.                               
541800         CONTINUE                                                         
541900       ELSE                                                               
542000         IF TEST-IDCATPOS-HEL = SPACE                                     
542100         OR TEST-IDCATPOS1 ALPHABETIC                                     
542200         OR TEST-IDCATPOS1 = ZERO                                         
542300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDCATPOS-UPD-ATTR               
542400           MOVE JA TO INDATA-FEL                                          
542500         END-IF                                                           
542600       END-IF                                                             
542700     END-IF                                                               
542800     .                                                                    
542900     EJECT                                                                
543000 XB-TEST-MED-BAS SECTION.                                                 
543100     SKIP2                                                                
543200*- - - - - - - - - - - KDPS KOMBINERAT IDARTNR                            
543300     IF TEST-KDPS = '  ' OR 'SW'                                          
543400        IF TEST-IDARTNR NOT = ZERO                                        
543500           IF TEST-BEART = SPACE                                          
543600              PERFORM X2-KOLLA-ARTIKEL-FINNS                              
543700           ELSE                                                           
543800              PERFORM X3-FEL-MEDDELA                                      
543900           END-IF                                                         
544000        END-IF                                                            
544100     ELSE                                                                 
544200        IF TEST-KDPS = 'IK'                                               
544300           IF TEST-IDARTNR = ZERO                                         
544400              PERFORM X3-FEL-MEDDELA                                      
544500           ELSE                                                           
544600              PERFORM X2-KOLLA-ARTIKEL-FINNS                              
544700           END-IF                                                         
544800        ELSE                                                              
544900           IF TEST-KDPS = 'LS' OR 'KL' OR 'KS'                            
545000              IF TEST-IDARTNR = ZERO                                      
545100                 PERFORM X3-FEL-MEDDELA                                   
545200              ELSE                                                        
545300                 PERFORM X1-KOLLA-ARTIKEL-SAKNAS                          
545400              END-IF                                                      
545500           END-IF                                                         
545600        END-IF                                                            
545700     END-IF                                                               
545800*- - - - - - - - - - - KDPS KOMBINERAT BEART                              
545900                                                                          
546000     IF INDATA-FEL = NEJ                                                  
546100        IF TEST-BEART NOT = SPACE                                         
546200           IF MID-BEART-UPD NOT = ALL '+'                                 
546300              PERFORM XBA-BENTEST-BEART                                   
546400           ELSE                                                           
546500              IF TEST-IDARTNR = ZERO                                      
546600                 IF TEST-KDPS = '  '                                      
546700                   PERFORM XBA-BENTEST-BEART                              
546800                 END-IF                                                   
546900              ELSE                                                        
547000                 IF MID-IDARTNR-UPD = ALL '+'                             
547100                    CONTINUE                                              
547200                 ELSE                                                     
547300                    PERFORM XBB-BENTEST-IDARTNR                           
547400                 END-IF                                                   
547500              END-IF                                                      
547600           END-IF                                                         
547700        END-IF                                                            
547800     END-IF                                                               
547900     .                                                                    
548000     EJECT                                                                
548100 XBA-BENTEST-BEART SECTION.                                               
548200     SKIP2                                                                
548300     IF TEST-KDPS = 'LS' OR 'KL' OR 'NS' OR 'KN'                          
548400        PERFORM XBAA-KOLLA-BENA                                           
548500     ELSE                                                                 
548600        IF TEST-KDPS = '  ' AND TEST-IDARTNR = ZERO                       
548700           PERFORM XBAA-KOLLA-BENA                                        
548800        ELSE                                                              
548900           IF TEST-KDPS = 'XX'                                            
549000              CONTINUE                                                    
549100           ELSE                                                           
549200              MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR               
549300              MOVE JA TO INDATA-FEL                                       
549400           END-IF                                                         
549500        END-IF                                                            
549600     END-IF                                                               
549700     .                                                                    
549800     EJECT                                                                
549900 XBAA-KOLLA-BENA SECTION.                                                 
550000     SKIP2                                                                
550100     MOVE TEST-BEART TO W-BEART                                           
550200     MOVE 'S ' TO W-IDSKYLT                                               
550300     IF (UPPDATE-FL = 'Ä') AND                                            
550400        (MID-BEART-UPD = ALL '+' AND MID-KDHOM-UPD = ALL '+')             
550500              CONTINUE                                                    
550600     ELSE                                                                 
550700        PERFORM IMS-GU-BENA-SEQ                                           
550800        IF SEGMENT-FINNS                                                  
550900           IF MID-KDHOM-UPD = ALL '+'                                     
551000              PERFORM IMS-GNP-BENA-HOM                                    
551100              IF SEGMENT-FINNS                                            
551200                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR             
551300                 MOVE JA TO INDATA-FEL                                    
551400**********       MEDDELA ATT HOMONYM-FINNS                                
551500                 MOVE MED-4(SPRAAK-IX) TO MOD-TEMFSINF                    
551600              END-IF                                                      
551700           ELSE                                                           
551800             IF MID-KDHOM-UPD = BEN-BEN-KDHOMONYM                         
551900                CONTINUE                                                  
552000             ELSE                                                         
552100               PERFORM IMS-GNP-BENA-HOM                                   
552200               IF SEGMENT-FINNS                                           
552300                 PERFORM XBAAA-HITTA-RATT-HOMONYM                         
552400               ELSE                                                       
552500*********        HOMONYMKOD FINNS EJ                                      
552600                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR             
552700                 MOVE JA TO INDATA-FEL                                    
552800               END-IF                                                     
552900             END-IF                                                       
553000           END-IF                                                         
553100        ELSE                                                              
553200           MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-UPD-ATTR                  
553300           MOVE JA TO INDATA-FEL                                          
553400        END-IF                                                            
553500     END-IF                                                               
553600     .                                                                    
553700     EJECT                                                                
553800 XBAAA-HITTA-RATT-HOMONYM SECTION.                                        
553900     SKIP2                                                                
554000     MOVE NEJ TO RATT-HOMONYM                                             
554100                                                                          
554200     PERFORM IMS-GN-BENA-SEQ                                              
554300     PERFORM UNTIL SEGMENT-SAKNAS OR RATT-HOMONYM = JA                    
554400       IF BEN-BEN-KDHOMONYM = MID-KDHOM-UPD                               
554500         MOVE JA TO RATT-HOMONYM                                          
554600       ELSE                                                               
554700         PERFORM IMS-GN-BENA-SEQ                                          
554800       END-IF                                                             
554900     END-PERFORM                                                          
555000                                                                          
555100     IF RATT-HOMONYM = NEJ                                                
555200******  HOMONYMKOD FINNS EJ                                               
555300        MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOM-UPD-ATTR                      
555400        MOVE JA TO INDATA-FEL                                             
555500     END-IF                                                               
555600     .                                                                    
555700     EJECT                                                                
555800 XBB-BENTEST-IDARTNR SECTION.                                             
555900     SKIP2                                                                
556000     MOVE TEST-IDARTNR TO W-IDARTNR                                       
556100     PERFORM IMS-GU-BENB-SEQ                                              
556200     IF SEGMENT-SAKNAS                                                    
556300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                    
556400        MOVE JA TO INDATA-FEL                                             
556500     END-IF                                                               
556600     .                                                                    
556700     EJECT                                                                
556800 X1-KOLLA-ARTIKEL-SAKNAS SECTION.                                         
556900     SKIP2                                                                
557000     MOVE TEST-IDARTNR TO W-IDARTNR                                       
557100     PERFORM IMS-GU-ARTC01                                                
557200                                                                          
557300     IF SEGMENT-FINNS                                                     
557400        IF UPPDATE-FL = 'N'                                               
557500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                 
557600           MOVE JA TO INDATA-FEL                                          
557700        ELSE                                                              
557800           IF MID-IDARTNR-UPD NOT = ALL '+'                               
557900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR              
558000              MOVE JA TO INDATA-FEL                                       
558100           ELSE                                                           
558200              IF MID-KDPS-UPD NOT = ALL '+'                               
558300                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR             
558400                 MOVE JA TO INDATA-FEL                                    
558500              END-IF                                                      
558600           END-IF                                                         
558700        END-IF                                                            
558800     END-IF                                                               
558900     .                                                                    
559000     EJECT                                                                
559100 X2-KOLLA-ARTIKEL-FINNS SECTION.                                          
559200     SKIP2                                                                
559300     MOVE TEST-IDARTNR TO W-IDARTNR                                       
559400     PERFORM IMS-GU-ARTC01                                                
559500                                                                          
559600     IF SEGMENT-FINNS                                                     
559700        IF ARTC01-ART-KDERS-UTG > ZERO                                    
559800           IF ARTC01-ART-KDERS-UTG = 52                                   
559900              PERFORM X2A-FLYTTA-FEL                                      
560000           END-IF                                                         
560100        ELSE                                                              
560200           PERFORM IMS-GNP-ARTC11                                         
560300           IF SEGMENT-FINNS                                               
560400              IF ARTC11-CLAG-KDERS = 52                                   
560500                 PERFORM X2A-FLYTTA-FEL                                   
560600              END-IF                                                      
560700           ELSE                                                           
560800              PERFORM X2A-FLYTTA-FEL                                      
560900           END-IF                                                         
561000        END-IF                                                            
561100     ELSE                                                                 
561200        PERFORM X2A-FLYTTA-FEL                                            
561300     END-IF                                                               
561400     .                                                                    
561500     EJECT                                                                
561600 X2A-FLYTTA-FEL SECTION.                                                  
561700     SKIP2                                                                
561800     IF UPPDATE-FL = 'N'                                                  
561900        MOVE MFS-NUM-FAELT-FEL TO                                         
562000             MOD-IDARTNR-UPD-ATTR                                         
562100        MOVE JA TO INDATA-FEL                                             
562200     ELSE                                                                 
562300        IF MID-IDARTNR-UPD NOT = ALL '+'                                  
562400           MOVE MFS-NUM-FAELT-FEL TO                                      
562500                MOD-IDARTNR-UPD-ATTR                                      
562600           MOVE JA TO INDATA-FEL                                          
562700        ELSE                                                              
562800           IF MID-KDPS-UPD NOT = ALL '+'                                  
562900              MOVE MFS-ALFA-FAELT-FEL TO                                  
563000                   MOD-KDPS-UPD-ATTR                                      
563100              MOVE JA TO INDATA-FEL                                       
563200           END-IF                                                         
563300        END-IF                                                            
563400     END-IF                                                               
563500     .                                                                    
563600     EJECT                                                                
563700                                                                          
563800 X3-FEL-MEDDELA SECTION.                                                  
563900     SKIP2                                                                
564000     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
564100        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-UPD-ATTR                    
564200        MOVE JA TO INDATA-FEL                                             
564300     ELSE                                                                 
564400        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPS-UPD-ATTR                      
564500        MOVE JA TO INDATA-FEL                                             
564600     END-IF                                                               
564700     .                                                                    
564800     EJECT                                                                
564900 Y-KOLLA-KDCATPUB-RAD-KOMB  SECTION.                                      
565000     SKIP2                                                                
565100*    TEST-KDCATPUB-FOM  &  TEST-KDCATPUB-TOM                              
565200*    Innehåller nu uppdat-värdena                                         
565300*    Läs med IO2-PCB alla rader med samma radnr. Kolla pub                
565400     PERFORM IMS-GU-AVS-IO2                                               
565500*    MOVE LOW-VALUE  TO W-KDCATPUB-MINSOEK                                
565600*    MOVE HIGH-VALUE TO W-KDCATPUB-MAXSOEK                                
565700     PERFORM IMS-GNP-RAD-NEXT-IO2                                         
565800     IF SEGMENT-FINNS                                                     
565900       PERFORM YA-KOLLA-KDCATPUB-INTERVALL                                
566000       PERFORM UNTIL SEGMENT-SAKNAS OR ( INDATA-FEL = JA )                
566100         PERFORM IMS-GNP-RAD-NEXT-IO2                                     
566200         PERFORM YA-KOLLA-KDCATPUB-INTERVALL                              
566300       END-PERFORM                                                        
566400     ELSE                                                                 
566500       IF UPPDATE-FL = 'N'                                                
566600         CONTINUE                                                         
566700       ELSE                                                               
566800         MOVE 'INGEN GNP IO2-TRÄFF' TO MOD-TEMFSFEL                       
566900         MOVE JA TO INDATA-FEL                                            
567000       END-IF                                                             
567100     END-IF                                                               
567200     .                                                                    
567300     EJECT                                                                
567400 YA-KOLLA-KDCATPUB-INTERVALL  SECTION.                                    
567500     SKIP2                                                                
567600*    KDCATPUB från andra lika radnr på basen i IO2- .          *          
567700*    Ny rad får inte inkräkta på annat lika radmummers         *          
567800*                                        KDCATPUBs-intervall.  *          
567900     IF  TEST-KDCATPUB-FOM = IO2-RAD-KDCATPUB-FOM                         
568000     AND TEST-KDCATPUB-TOM = IO2-RAD-KDCATPUB-TOM                         
568100*      man skall bara ändra befintlig rad                                 
568200       CONTINUE                                                           
568300     ELSE                                                                 
568400       IF IO2-RAD-KDCATPUB-FOM > LOW-VALUE                                
568500         IF TEST-KDCATPUB-FOM < IO2-RAD-KDCATPUB-FOM                      
568600*          Ny-rad start tidigare än den lästa bas-radens start            
568700           IF TEST-KDCATPUB-TOM < IO2-RAD-KDCATPUB-FOM                    
568800*            och slutar före bas-radens start                             
568900             CONTINUE                                                     
569000*            läs ny rad och se om det fortfarande är OK                   
569100           ELSE                                                           
569200             MOVE JA TO INDATA-FEL                                        
569300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-UPD-ATTR         
569400             MOVE 'FOM<BASFOM + TOM>=BASFOM' TO WS-FELTEXT                
569500           END-IF                                                         
569600         ELSE                                                             
569700           IF TEST-KDCATPUB-FOM = IO2-RAD-KDCATPUB-FOM                    
569800*            In-rads start är samma som registrerad. Uppdatering?         
569900             CONTINUE                                                     
570000*            man vill BARA begränsa en befintlig rads slut-tid            
570100*            nästa läsning kontrollerar om utrymme finns för TOM          
570200           ELSE                                                           
570300*            In-rads start ligger efter bas-radens start                  
570400             IF TEST-KDCATPUB-FOM > IO2-RAD-KDCATPUB-TOM                  
570500*              och kan börja efter bas-radens slut                        
570600*              Inget fel än, Läs en ny rad                                
570700               CONTINUE                                                   
570800             ELSE                                                         
570900               MOVE JA TO INDATA-FEL                                      
571000               MOVE MFS-ALFA-FAELT-FEL                                    
571100                                   TO MOD-KDCATPUB-R-FOM-UPD-ATTR         
571200               MOVE 'FOM>=BASFOM + FOM<=BASTOM' TO WS-FELTEXT             
571300             END-IF                                                       
571400           END-IF                                                         
571500         END-IF                                                           
571600       ELSE                                                               
571700*        Bas-raden gäller från "noll-datum"                               
571800         IF  TEST-KDCATPUB-FOM = IO2-RAD-KDCATPUB-FOM                     
571900           CONTINUE                                                       
572000*          man vill BARA begränsa en befintlig rads slut-tid              
572100*          läs ny rad och se om det fortfarande är OK                     
572200         ELSE                                                             
572300           IF IO2-RAD-KDCATPUB-TOM < HIGH-VALUE                           
572400             IF TEST-KDCATPUB-FOM > IO2-RAD-KDCATPUB-TOM                  
572500*              och kan börja efter bas-radens slut                        
572600*              Inget fel än, Läs en ny rad                                
572700               CONTINUE                                                   
572800             ELSE                                                         
572900               MOVE JA TO INDATA-FEL                                      
573000               MOVE MFS-ALFA-FAELT-FEL                                    
573100                                    TO MOD-KDCATPUB-R-FOM-UPD-ATTR        
573200               MOVE 'BASTOM<FF + FOM<=BASTOM' TO WS-FELTEXT               
573300             END-IF                                                       
573400           ELSE                                                           
573500*            Bas-raden gäller för all framtid                             
573600             MOVE JA TO INDATA-FEL                                        
573700             MOVE MFS-ALFA-FAELT-FEL                                      
573800                                  TO MOD-KDCATPUB-R-FOM-UPD-ATTR          
573900                                     MOD-KDCATPUB-R-TOM-UPD-ATTR          
574000               MOVE 'BASFOM=00 + BASTOM=FF  ' TO WS-FELTEXT               
574100           END-IF                                                         
574200         END-IF                                                           
574300       END-IF                                                             
574400     END-IF                                                               
574500     .                                                                    
574600     EJECT                                                                
574700* IMS SEKTIONER                                                           
574800*                                                                         
574900 IMS-GET-MSG SECTION.                                                     
575000     MOVE '  QC' TO GODK-STATUSKODER                                      
575100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
575200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
575300     PERFORM IMS-STATUSKONTROLL                                           
575400     .                                                                    
575500     SKIP3                                                                
575600 IMS-INSERT-MSG SECTION.                                                  
575700     IF ENGLISH-TEXT                                                      
575800         MOVE 'N' TO MFS-KDHUVOMR                                         
575900     END-IF                                                               
576000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
576100     MOVE SPACE TO GODK-STATUSKODER                                       
576200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
576300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
576400     PERFORM IMS-STATUSKONTROLL                                           
576500     .                                                                    
576600     EJECT                                                                
576700 IMS-GU-AVS SECTION.                                                      
576800     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
576900            DELIMITED BY SIZE INTO SSA1                                   
577000     MOVE '  GE' TO GODK-STATUSKODER                                      
577100     CALL CBLTDLI USING GU AVS-PCB IO-AREA-1 SSA1                         
577200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
577300     PERFORM IMS-STATUSKONTROLL                                           
577400     .                                                                    
577500 IMS-GHU-AVS SECTION.                                                     
577600     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
577700            DELIMITED BY SIZE INTO SSA1                                   
577800     MOVE '  GE' TO GODK-STATUSKODER                                      
577900     CALL CBLTDLI USING GHU AVS-PCB IO-AREA-1 SSA1                        
578000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
578100     PERFORM IMS-STATUSKONTROLL                                           
578200     .                                                                    
578300 IMS-GU-AVS-RAD-MAX SECTION.                                              
578400     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
578500            DELIMITED BY SIZE INTO SSA1                                   
578600     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-MAX ')'                      
578700            DELIMITED BY SIZE INTO SSA2                                   
578800     MOVE '  GE' TO GODK-STATUSKODER                                      
578900     CALL CBLTDLI USING GU AVS-PCB IO-AREA-1 SSA1 SSA2                    
579000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
579100     PERFORM IMS-STATUSKONTROLL                                           
579200     .                                                                    
579300 IMS-GHU-AVS-RAD SECTION.                                                 
579400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
579500            DELIMITED BY SIZE INTO SSA1                                   
579600     MOVE '  GE' TO GODK-STATUSKODER                                      
579700     CALL CBLTDLI USING GHU AVS-PCB IO-AREA-1 SSA1                        
579800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
579900     PERFORM IMS-STATUSKONTROLL                                           
580000     .                                                                    
580100     EJECT                                                                
580200 IMS-GNP-AVS-RAD SECTION.                                                 
580300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
580400            DELIMITED BY SIZE INTO SSA1                                   
580500     MOVE '  GE' TO GODK-STATUSKODER                                      
580600     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
580700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
580800     PERFORM IMS-STATUSKONTROLL                                           
580900     .                                                                    
581000 IMS-GNP-AVS-RAD-FIRST SECTION.                                           
581100     STRING 'WLKATH12(WDN512KY=>' W-WDN512KY-MIN ')'                      
581200            DELIMITED BY SIZE INTO SSA1                                   
581300     MOVE '  GE' TO GODK-STATUSKODER                                      
581400     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
581500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
581600     PERFORM IMS-STATUSKONTROLL                                           
581700     .                                                                    
581800 IMS-GNP-AVS-RAD-NEXT SECTION.                                            
581900     STRING 'WLKATH12(IDCATRAD>=' W-IDCATRAD-MIN                          
582000                 OCH 'KDCATPUF<=' W-KDCATPUB-MIN-X                        
582100                 OCH 'KDCATPUT>=' W-KDCATPUB-MIN-X                        
582200                                                                          
582300               ELLER 'IDCATRAD>=' W-IDCATRAD-MIN                          
582400                 OCH 'KDCATPUF>=' W-KDCATPUB-MIN-X                        
582500                 OCH 'KDCATPUT<=' W-KDCATPUB-MAX-X                        
582600                                                                          
582700               ELLER 'IDCATRAD>=' W-IDCATRAD-MIN                          
582800                 OCH 'KDCATPUF<=' W-KDCATPUB-MAX-X                        
582900                 OCH 'KDCATPUT>=' W-KDCATPUB-MAX-X ')'                    
583000            DELIMITED BY SIZE INTO SSA1                                   
583100     MOVE '  GE' TO GODK-STATUSKODER                                      
583200     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
583300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
583400     PERFORM IMS-STATUSKONTROLL                                           
583500     .                                                                    
583600 IMS-GNP-AVS-INFO  SECTION.                                               
583700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
583800            DELIMITED BY SIZE INTO SSA1                                   
583900     MOVE 'WLKATH21 ' TO SSA2                                             
584000     MOVE '  GE' TO GODK-STATUSKODER                                      
584100     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
584200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
584300     PERFORM IMS-STATUSKONTROLL                                           
584400     .                                                                    
584500     EJECT                                                                
584600 IMS-GHNP-AVS-RAD SECTION.                                                
584700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
584800            DELIMITED BY SIZE INTO SSA1                                   
584900     MOVE '  GE' TO GODK-STATUSKODER                                      
585000     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
585100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
585200     PERFORM IMS-STATUSKONTROLL                                           
585300     .                                                                    
585400 IMS-GHNP-AVS-RAD-FIRST SECTION.                                          
585500     MOVE 'WLKATH12*F' TO SSA1                                            
585600     MOVE '  GE' TO GODK-STATUSKODER                                      
585700     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
585800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
585900     PERFORM IMS-STATUSKONTROLL                                           
586000     .                                                                    
586100 IMS-GHNP-AVS-RAD-BORT SECTION.                                           
586200     STRING 'WLKATH12(WDN512KY=>' W-WDN512KY-X                            
586300                 OCH 'WDN512KY<=' W-WDN512KY-MAX                          
586400                 OCH 'KDCATPUF =' W-KDCATPUB-X ')'                        
586500            DELIMITED BY SIZE INTO SSA1                                   
586600     MOVE '  GE' TO GODK-STATUSKODER                                      
586700     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
586800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
586900     PERFORM IMS-STATUSKONTROLL                                           
587000     .                                                                    
587100 IMS-ISRT-AVS-RAD SECTION.                                                
587200     MOVE 'WLKATH12 ' TO SSA1                                             
587300     MOVE '  ' TO GODK-STATUSKODER                                        
587400     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1                       
587500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
587600     PERFORM IMS-STATUSKONTROLL                                           
587700     .                                                                    
587800     EJECT                                                                
587900 IMS-GN-AVS-POS   SECTION.                                                
588000     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
588100            DELIMITED BY SIZE INTO SSA1                                   
588200     MOVE   'WLKATH12 '         TO SSA2                                   
588300     STRING 'WLKATH21(IDCATPOS =' W-IDCATPOS-LO-X ')'                     
588400            DELIMITED BY SIZE INTO SSA3                                   
588500     MOVE '  GE' TO GODK-STATUSKODER                                      
588600     CALL CBLTDLI USING GN AVS-PCB IO-AREA-1 SSA1 SSA2 SSA3               
588700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
588800     PERFORM IMS-STATUSKONTROLL                                           
588900     .                                                                    
589000 IMS-GHNP-AVS-ART SECTION.                                                
589100     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
589200            DELIMITED BY SIZE INTO SSA1                                   
589300     MOVE 'WLKATH21 ' TO SSA2                                             
589400     MOVE '  GE' TO GODK-STATUSKODER                                      
589500     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
589600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
589700     PERFORM IMS-STATUSKONTROLL                                           
589800     .                                                                    
589900 IMS-ISRT-AVS-ART SECTION.                                                
590000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
590100            DELIMITED BY SIZE INTO SSA1                                   
590200     MOVE 'WLKATH21 ' TO SSA2                                             
590300     MOVE '  ' TO GODK-STATUSKODER                                        
590400     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
590500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
590600     PERFORM IMS-STATUSKONTROLL                                           
590700     .                                                                    
590800     EJECT                                                                
590900 IMS-GNP-AVS-TEXT SECTION.                                                
591000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
591100            DELIMITED BY SIZE INTO SSA1                                   
591200     MOVE 'WLKATH22 ' TO SSA2                                             
591300     MOVE '  GE' TO GODK-STATUSKODER                                      
591400     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
591500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
591600     PERFORM IMS-STATUSKONTROLL                                           
591700     .                                                                    
591800 IMS-GHNP-AVS-TEXT SECTION.                                               
591900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
592000            DELIMITED BY SIZE INTO SSA1                                   
592100     MOVE 'WLKATH22 ' TO SSA2                                             
592200     MOVE '  GE' TO GODK-STATUSKODER                                      
592300     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
592400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
592500     PERFORM IMS-STATUSKONTROLL                                           
592600     .                                                                    
592700 IMS-ISRT-AVS-TEXT SECTION.                                               
592800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
592900            DELIMITED BY SIZE INTO SSA1                                   
593000     MOVE 'WLKATH22 ' TO SSA2                                             
593100     MOVE '  ' TO GODK-STATUSKODER                                        
593200     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
593300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
593400     PERFORM IMS-STATUSKONTROLL                                           
593500     .                                                                    
593600 IMS-GNP-AVS-BEN SECTION.                                                 
593700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
593800            DELIMITED BY SIZE INTO SSA1                                   
593900     MOVE 'WLKATH23 ' TO SSA2                                             
594000     MOVE '  GE' TO GODK-STATUSKODER                                      
594100     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
594200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
594300     PERFORM IMS-STATUSKONTROLL                                           
594400     .                                                                    
594500     EJECT                                                                
594600 IMS-GHNP-AVS-BEN SECTION.                                                
594700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
594800            DELIMITED BY SIZE INTO SSA1                                   
594900     MOVE 'WLKATH23 ' TO SSA2                                             
595000     MOVE '  GE' TO GODK-STATUSKODER                                      
595100     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
595200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
595300     PERFORM IMS-STATUSKONTROLL                                           
595400     .                                                                    
595500 IMS-ISRT-AVS-BEN SECTION.                                                
595600     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
595700            DELIMITED BY SIZE INTO SSA1                                   
595800     MOVE 'WLKATH23 ' TO SSA2                                             
595900     MOVE '  ' TO GODK-STATUSKODER                                        
596000     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
596100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
596200     PERFORM IMS-STATUSKONTROLL                                           
596300     .                                                                    
596400 IMS-GNP-AVS-NOT SECTION.                                                 
596500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
596600            DELIMITED BY SIZE INTO SSA1                                   
596700     MOVE   'WLKATH24 '         TO SSA2                                   
596800     MOVE '  GE' TO GODK-STATUSKODER                                      
596900     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
597000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
597100     PERFORM IMS-STATUSKONTROLL                                           
597200     .                                                                    
597300 IMS-GHNP-AVS-NOT SECTION.                                                
597400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
597500            DELIMITED BY SIZE INTO SSA1                                   
597600     MOVE 'WLKATH24 ' TO SSA2                                             
597700     MOVE '  GE' TO GODK-STATUSKODER                                      
597800     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
597900     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
598000     PERFORM IMS-STATUSKONTROLL                                           
598100     .                                                                    
598200     EJECT                                                                
598300 IMS-GHNP-AVS-NOT-SEGM SECTION.                                           
598400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
598500            DELIMITED BY SIZE INTO SSA1                                   
598600     STRING 'WLKATH24(IDSEGMNR =' W-IDSEGMNR-X ')'                        
598700            DELIMITED BY SIZE INTO SSA2                                   
598800     MOVE '  GE' TO GODK-STATUSKODER                                      
598900     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
599000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
599100     PERFORM IMS-STATUSKONTROLL                                           
599200     .                                                                    
599300 IMS-ISRT-AVS-NOT SECTION.                                                
599400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
599500            DELIMITED BY SIZE INTO SSA1                                   
599600     MOVE 'WLKATH24 ' TO SSA2                                             
599700     MOVE '  ' TO GODK-STATUSKODER                                        
599800     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
599900     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
600000     PERFORM IMS-STATUSKONTROLL                                           
600100     .                                                                    
600200 IMS-GNP-AVS-RUB SECTION.                                                 
600300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
600400            DELIMITED BY SIZE INTO SSA1                                   
600500     MOVE 'WLKATH25 ' TO SSA2                                             
600600     MOVE '  GE' TO GODK-STATUSKODER                                      
600700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
600800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
600900     PERFORM IMS-STATUSKONTROLL                                           
601000     .                                                                    
601100 IMS-GHNP-AVS-RUB SECTION.                                                
601200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
601300            DELIMITED BY SIZE INTO SSA1                                   
601400     MOVE 'WLKATH25 ' TO SSA2                                             
601500     MOVE '  GE' TO GODK-STATUSKODER                                      
601600     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
601700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
601800     PERFORM IMS-STATUSKONTROLL                                           
601900     .                                                                    
602000     EJECT                                                                
602100 IMS-ISRT-AVS-RUB SECTION.                                                
602200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
602300            DELIMITED BY SIZE INTO SSA1                                   
602400     MOVE 'WLKATH25 ' TO SSA2                                             
602500     MOVE '  ' TO GODK-STATUSKODER                                        
602600     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
602700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
602800     PERFORM IMS-STATUSKONTROLL                                           
602900     .                                                                    
603000 IMS-GNP-AVS-FOT SECTION.                                                 
603100     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
603200            DELIMITED BY SIZE INTO SSA1                                   
603300     MOVE 'WLKATH26 ' TO SSA2                                             
603400     MOVE '  GE' TO GODK-STATUSKODER                                      
603500     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
603600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
603700     PERFORM IMS-STATUSKONTROLL                                           
603800     .                                                                    
603900 IMS-GHNP-AVS-FOT SECTION.                                                
604000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
604100            DELIMITED BY SIZE INTO SSA1                                   
604200     MOVE 'WLKATH26 ' TO SSA2                                             
604300     MOVE '  GE' TO GODK-STATUSKODER                                      
604400     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
604500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
604600     PERFORM IMS-STATUSKONTROLL                                           
604700     .                                                                    
604800 IMS-ISRT-AVS-FOT SECTION.                                                
604900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
605000            DELIMITED BY SIZE INTO SSA1                                   
605100     MOVE 'WLKATH26 ' TO SSA2                                             
605200     MOVE '  ' TO GODK-STATUSKODER                                        
605300     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
605400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
605500     PERFORM IMS-STATUSKONTROLL                                           
605600     .                                                                    
605700     EJECT                                                                
605800 IMS-GNP-AVS-HAEN SECTION.                                                
605900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
606000            DELIMITED BY SIZE INTO SSA1                                   
606100     MOVE 'WLKATH27 ' TO SSA2                                             
606200     MOVE '  GE' TO GODK-STATUSKODER                                      
606300     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
606400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
606500     PERFORM IMS-STATUSKONTROLL                                           
606600     .                                                                    
606700 IMS-GHNP-AVS-HAEN SECTION.                                               
606800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
606900            DELIMITED BY SIZE INTO SSA1                                   
607000     MOVE 'WLKATH27 ' TO SSA2                                             
607100     MOVE '  GE' TO GODK-STATUSKODER                                      
607200     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
607300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
607400     PERFORM IMS-STATUSKONTROLL                                           
607500     .                                                                    
607600 IMS-ISRT-AVS-HAEN SECTION.                                               
607700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
607800            DELIMITED BY SIZE INTO SSA1                                   
607900     MOVE 'WLKATH27 ' TO SSA2                                             
608000     MOVE '  ' TO GODK-STATUSKODER                                        
608100     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
608200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
608300     PERFORM IMS-STATUSKONTROLL                                           
608400     .                                                                    
608500 IMS-GU-AVSG-HAEN SECTION.                                                
608600     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5GSEQ-HAEN                         
608700                                  W-IDCATRKY-LO                           
608800                 OCH 'WDN5G1KY<=' W-WDN5GSEQ-HAEN                         
608900                                  W-IDCATRKY-HI ')'                       
609000            DELIMITED BY SIZE INTO SSA1                                   
609100     MOVE '  GE' TO GODK-STATUSKODER                                      
609200     CALL CBLTDLI USING GU KATS-PCB IO-AREA-G SSA1                        
609300     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
609400     PERFORM IMS-STATUSKONTROLL                                           
609500     .                                                                    
609600     SKIP2                                                                
609700 IMS-GN-AVSG-HAEN SECTION.                                                
609800     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5GSEQ-HAEN                         
609900                                  W-IDCATRKY-LO                           
610000                 OCH 'WDN5G1KY<=' W-WDN5GSEQ-HAEN                         
610100                                  W-IDCATRKY-HI ')'                       
610200            DELIMITED BY SIZE INTO SSA1                                   
610300     MOVE '  GE' TO GODK-STATUSKODER                                      
610400     CALL CBLTDLI USING GN KATS-PCB IO-AREA-G SSA1                        
610500     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
610600     PERFORM IMS-STATUSKONTROLL                                           
610700     .                                                                    
610800     EJECT                                                                
610900 IMS-REPL-AVS SECTION.                                                    
611000     MOVE '  ' TO GODK-STATUSKODER                                        
611100     CALL CBLTDLI USING REPL AVS-PCB IO-AREA-1                            
611200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
611300     PERFORM IMS-STATUSKONTROLL                                           
611400     .                                                                    
611500 IMS-DLET-AVS SECTION.                                                    
611600     MOVE '  ' TO GODK-STATUSKODER                                        
611700     CALL CBLTDLI USING DLET AVS-PCB IO-AREA-1                            
611800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
611900     PERFORM IMS-STATUSKONTROLL                                           
612000     .                                                                    
612100 IMS-GU-AVS-IO2   SECTION.                                                
612200     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
612300            DELIMITED BY SIZE INTO SSA1                                   
612400     MOVE '  '   TO GODK-STATUSKODER                                      
612500     CALL CBLTDLI USING GU AVS2-PCB IO2-WLKATH01 SSA1                     
612600     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
612700     PERFORM IMS-STATUSKONTROLL                                           
612800     .                                                                    
612900 IMS-GHU-AVS-IO2   SECTION.                                               
613000     STRING 'WLKATH01(WDN501KY =' W-WDN501-H-X ')'                        
613100            DELIMITED BY SIZE INTO SSA1                                   
613200     MOVE '  '   TO GODK-STATUSKODER                                      
613300     CALL CBLTDLI USING GHU AVS2-PCB IO2-WLKATH01 SSA1                    
613400     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
613500     PERFORM IMS-STATUSKONTROLL                                           
613600     .                                                                    
613700 IMS-REPL-AVS-IO2 SECTION.                                                
613800     MOVE '  ' TO GODK-STATUSKODER                                        
613900     CALL CBLTDLI USING REPL AVS2-PCB IO2-WLKATH01                        
614000     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
614100     PERFORM IMS-STATUSKONTROLL                                           
614200     .                                                                    
614300 IMS-GNP-RAD-NEXT-IO2 SECTION.                                            
614400     STRING 'WLKATH12(IDCATRAD =' W-IDCATRAD-X ')'                        
614500            DELIMITED BY SIZE INTO SSA1                                   
614600     MOVE '  GE' TO GODK-STATUSKODER                                      
614700     CALL CBLTDLI USING GNP AVS2-PCB IO2-WLKATH12 SSA1                    
614800     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
614900     PERFORM IMS-STATUSKONTROLL                                           
615000     .                                                                    
615100     EJECT                                                                
615200 IMS-GU-AVS-RAD-IO2 SECTION.                                              
615300     STRING 'WLKATH01(WDN501KY =' W-WDN501-H-X ')'                        
615400            DELIMITED BY SIZE INTO SSA1                                   
615500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
615600            DELIMITED BY SIZE INTO SSA2                                   
615700     MOVE '  GE' TO GODK-STATUSKODER                                      
615800     CALL CBLTDLI USING GU AVS2-PCB IO2-WLKATH12 SSA1 SSA2                
615900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
616000     PERFORM IMS-STATUSKONTROLL                                           
616100     .                                                                    
616200 IMS-GHNP-AVS-NOT-IO2 SECTION.                                            
616300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
616400            DELIMITED BY SIZE INTO SSA1                                   
616500     STRING 'WLKATH24(IDSEGMNR =' W-IDSEGMNR-X ')'                        
616600            DELIMITED BY SIZE INTO SSA2                                   
616700     MOVE '  GE' TO GODK-STATUSKODER                                      
616800     CALL CBLTDLI USING GHNP AVS2-PCB IO2-WLKATH24 SSA1 SSA2              
616900     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
617000     PERFORM IMS-STATUSKONTROLL                                           
617100     .                                                                    
617200 IMS-ISRT-AVS-NOT-IO2 SECTION.                                            
617300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
617400            DELIMITED BY SIZE INTO SSA1                                   
617500     MOVE 'WLKATH24 ' TO SSA2                                             
617600     MOVE '  ' TO GODK-STATUSKODER                                        
617700     CALL CBLTDLI USING ISRT AVS2-PCB IO2-WLKATH24 SSA1 SSA2              
617800     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
617900     PERFORM IMS-STATUSKONTROLL                                           
618000     .                                                                    
618100     EJECT                                                                
618200 IMS-GET-KATM-KAT SECTION.                                                
618300     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-1-X ')'                       
618400                  DELIMITED BY SIZE INTO SSA1                             
618500     MOVE   '  GE' TO GODK-STATUSKODER                                    
618600     CALL CBLTDLI USING GU KATM-PCB KAT-WLKATM01 SSA1                     
618700     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
618800     PERFORM IMS-STATUSKONTROLL                                           
618900     .                                                                    
619000 IMS-GET-KATM-TAB SECTION.                                                
619100     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-1-X ')'                       
619200                  DELIMITED BY SIZE INTO SSA1                             
619300     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
619400                  DELIMITED BY SIZE INTO SSA2                             
619500     MOVE   '  GE' TO GODK-STATUSKODER                                    
619600     CALL CBLTDLI USING GU KATM-PCB KAT-WLKATM11 SSA1 SSA2                
619700     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
619800     PERFORM IMS-STATUSKONTROLL                                           
619900     .                                                                    
620000 IMS-GET-NEXT-KATM-TAB SECTION.                                           
620100     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-1-X ')'                       
620200                  DELIMITED BY SIZE INTO SSA1                             
620300     MOVE   'WLKATM11 '               TO SSA2                             
620400     MOVE   '  GE' TO GODK-STATUSKODER                                    
620500     CALL CBLTDLI USING GNP KATM-PCB KAT-WLKATM11 SSA1 SSA2               
620600     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
620700     PERFORM IMS-STATUSKONTROLL                                           
620800     .                                                                    
620900     EJECT                                                                
621000 IMS-GU-ARTC01 SECTION.                                                   
621100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
621200            DELIMITED BY SIZE INTO SSA1                                   
621300     MOVE '  GE' TO GODK-STATUSKODER                                      
621400     CALL CBLTDLI USING GU ARTC-PCB ARTC01-WLARTC01 SSA1                  
621500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
621600     PERFORM IMS-STATUSKONTROLL                                           
621700     .                                                                    
621800 IMS-GNP-ARTC11 SECTION.                                                  
621900     MOVE 'WLARTC11 ' TO SSA1                                             
622000     MOVE '  GE' TO GODK-STATUSKODER                                      
622100     CALL CBLTDLI USING GNP ARTC-PCB ARTC11-WLARTC11 SSA1                 
622200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
622300     PERFORM IMS-STATUSKONTROLL                                           
622400     .                                                                    
622500 IMS-GU-RUB SECTION.                                                      
622600     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
622700            DELIMITED BY SIZE INTO SSA1                                   
622800     MOVE '  GE' TO GODK-STATUSKODER                                      
622900     CALL CBLTDLI USING GU RUB-PCB IO-AREA-2 SSA1                         
623000     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
623100     PERFORM IMS-STATUSKONTROLL                                           
623200     .                                                                    
623300     EJECT                                                                
623400 IMS-GNP-RUB-TEXT SECTION.                                                
623500     STRING 'WLKATB11(IDSKYLT  =' W-IDSKYLT-X ')'                         
623600            DELIMITED BY SIZE INTO SSA1                                   
623700     MOVE '  GE' TO GODK-STATUSKODER                                      
623800     CALL CBLTDLI USING GNP RUB-PCB IO-AREA-2 SSA1                        
623900     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
624000     PERFORM IMS-STATUSKONTROLL                                           
624100     .                                                                    
624200 IMS-GU-FOT SECTION.                                                      
624300     STRING 'WLKATF01(IDFOTNR  =' W-IDFOTNR-X ')'                         
624400            DELIMITED BY SIZE INTO SSA1                                   
624500     MOVE '  GE' TO GODK-STATUSKODER                                      
624600     CALL CBLTDLI USING GU FOT-PCB IO-AREA-2 SSA1                         
624700     MOVE FOT-STATUS-CODE TO STATUS-WS                                    
624800     PERFORM IMS-STATUSKONTROLL                                           
624900     .                                                                    
625000 IMS-GU-TEXT SECTION.                                                     
625100     STRING 'WLKATD01(IDTTEXNR =' W-IDTTEXNR-X ')'                        
625200            DELIMITED BY SIZE INTO SSA1                                   
625300     MOVE '  GE' TO GODK-STATUSKODER                                      
625400     CALL CBLTDLI USING GU TEXT-PCB IO-AREA-2 SSA1                        
625500     MOVE TEXT-STATUS-CODE TO STATUS-WS                                   
625600     PERFORM IMS-STATUSKONTROLL                                           
625700     .                                                                    
625800 IMS-GNP-TEXT-TEXT SECTION.                                               
625900     STRING 'WLKATD11(IDSKYLT  =' W-IDSKYLT-X ')'                         
626000            DELIMITED BY SIZE INTO SSA1                                   
626100     MOVE '  GE' TO GODK-STATUSKODER                                      
626200     CALL CBLTDLI USING GNP TEXT-PCB IO-AREA-2 SSA1                       
626300     MOVE TEXT-STATUS-CODE TO STATUS-WS                                   
626400     PERFORM IMS-STATUSKONTROLL                                           
626500     .                                                                    
626600     EJECT                                                                
626700 IMS-GU-BENB-SEQ SECTION.                                                 
626800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
626900            DELIMITED BY SIZE INTO SSA1                                   
627000     MOVE '  GE' TO GODK-STATUSKODER                                      
627100     CALL CBLTDLI USING GU BENB-PCB IO-AREA-2 SSA1                        
627200     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
627300     PERFORM IMS-STATUSKONTROLL                                           
627400     .                                                                    
627500 IMS-GNP-BEN-TEXT-BSEQ SECTION.                                           
627600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
627700            DELIMITED BY SIZE INTO SSA1                                   
627800     MOVE '  GE' TO GODK-STATUSKODER                                      
627900     CALL CBLTDLI USING GNP BENB-PCB IO-AREA-2 SSA1                       
628000     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
628100     PERFORM IMS-STATUSKONTROLL                                           
628200     .                                                                    
628300 IMS-GU-BENA-SEQ SECTION.                                                 
628400     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT W-BEART-X ')'                 
628500            DELIMITED BY SIZE INTO SSA1                                   
628600     MOVE '  GE' TO GODK-STATUSKODER                                      
628700     CALL CBLTDLI USING GU BENA-PCB IO-AREA-2 SSA1                        
628800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
628900     PERFORM IMS-STATUSKONTROLL                                           
629000     .                                                                    
629100 IMS-GNP-BEN-TEXT-ASEQ SECTION.                                           
629200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
629300            DELIMITED BY SIZE INTO SSA1                                   
629400     MOVE '  GE' TO GODK-STATUSKODER                                      
629500     CALL CBLTDLI USING GNP BENA-PCB IO-AREA-2 SSA1                       
629600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
629700     PERFORM IMS-STATUSKONTROLL                                           
629800     .                                                                    
629900     EJECT                                                                
630000 IMS-GN-BENA-SEQ SECTION.                                                 
630100     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT W-BEART-X ')'                 
630200            DELIMITED BY SIZE INTO SSA1                                   
630300     MOVE '  GE' TO GODK-STATUSKODER                                      
630400     CALL CBLTDLI USING GN BENA-PCB IO-AREA-2 SSA1                        
630500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
630600     PERFORM IMS-STATUSKONTROLL                                           
630700     .                                                                    
630800 IMS-GNP-BENA-HOM SECTION.                                                
630900     MOVE 'WLBENA13 ' TO SSA1                                             
631000     MOVE '  GE' TO GODK-STATUSKODER                                      
631100     CALL CBLTDLI USING GNP BENA-PCB IO-AREA-2 SSA1                       
631200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
631300     PERFORM IMS-STATUSKONTROLL                                           
631400     .                                                                    
631500 IMS-STATUSKONTROLL SECTION.                                              
631600     SET STATUS-IX TO 1                                                   
631700     SEARCH GODK-STATUS AT END CALL FELLOG                                
631800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
631900         CONTINUE                                                         
632000     END-SEARCH                                                           
632100     .                                                                    
632200     EJECT                                                                
632300*    -COPY WY2000P2                                                       

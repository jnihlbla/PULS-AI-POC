000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1051900.                                                
000300 AUTHOR.         ODD OLSEN.                                               
000400 DATE-WRITTEN.   JUNI 85.                                                 
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION.                                                            
000800*        PROGRAMMET SKRIVER UT ETT KATALOGAVSNITT PÅ PRINTER.             
000900*                                                                         
001000*        UTSKRIFT SKER MED TVÅ VALBARA OMFATTNINGAR.                      
001100*        1 = VADIS-DATA MED AVSNITTSRUBRIKER.                             
001200*        2 = VADIS-DATA MED AVSNITTSRUBRIKER OCH ALLA RADER.              
001300*                                                                         
001400*                                                                         
001500*DATE-AMENDED  DEC-88. INLAGT IMS-ROLB FÖR ATT UNDVIKA ABEND              
001600*                      P.G.A. FÖR MÅNGA FOTNOTER I KATALOG.  /CE          
001700*                                                                         
001800*DATE-AMENDED  JUN-89. FLLSRDEL='N' SKALL MEDFÖRA KDPS='NS' I             
001900*                      OUTPUTEN.                                          
002000*                                                                         
002100*    SUBPROGRAM:                                                          
002200*        W006PRS1 - SKÖTER ALL SKRIVNING MOT VPS-PRINTER.                 
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W1T519                                              
002600*        MID:         W1I51901                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W1O51901                                            
003000*    SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP3                                                                
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W1051900'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  VADIX                       PIC S9(9)   VALUE +0  COMP SYNC.         
004500 77  DOIX                        PIC S9(9)   VALUE +0  COMP SYNC.         
004600 77  SPR-IX                      PIC S9(9)   VALUE +1  COMP SYNC.         
004700 77  KOL-IX                      PIC S9(9)   VALUE +1  COMP SYNC.         
004800 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004900 77  SPRAAK-KOLL                 PIC X       VALUE 'N'.                   
005000 77  INDATA-SW                   PIC X.                                   
005100     88 INDATA-OK                            VALUE 'J'.                   
005200     88 INDATA-FEL                           VALUE 'N'.                   
005300                                                                          
005400 77  SW-MAX-FOTNOT               PIC X.                                   
005500     88 MAX-FOTNOT-OK                        VALUE SPACE.                 
005600     88 MAX-FOTNOT-FEL                       VALUE 'N'.                   
005700                                                                          
005800 77  KONTROLL-IDTRANS            PIC X(4).                                
005900     88 EGEN-IDTRANS                         VALUE '1519'.                
006000     88 1512-IDTRANS                         VALUE '1512'.                
006100     88 1515-IDTRANS                         VALUE '1515'.                
006200     88 1532-IDTRANS                         VALUE '1532'.                
006300     88 GODK-IDTRANS                         VALUE '1511' '1512'          
006400                                                   '1513' '1514'          
006500                                                   '1517' '1518'.         
006600                                                                          
006700 77  FLSKRIV                     PIC X       VALUE 'N'.                   
006800 77  EXTRA-RAD                   PIC X       VALUE 'N'.                   
006900 77  RAETT-TEXT                  PIC X       VALUE 'N'.                   
007000 77  HAEN-FINNS                  PIC X       VALUE 'N'.                   
007100 77  DUMMY-AREA                  PIC X(150)  VALUE SPACE.                 
007200 77  RADRAKNARE                  PIC S9(3)   COMP-3  VALUE +0.            
007300 77  MAX-ANTAL-RADER             PIC S9(3)   COMP-3  VALUE +42.           
007400 77  FLT                         PIC X(09).                               
007500 77  LGD                         PIC X(01).                               
007600 77  KSIFF                       PIC X(01).                               
007700 77  WS-KDFORDON                 PIC X(02)   VALUE SPACE.                 
007800 77  WS-FLLSRDEL                 PIC X       VALUE 'N'.                   
007900 77  WS-FLAVSTVAD                PIC X       VALUE 'N'.                   
008000 77  HAEN-RAD                    PIC S9(9)   VALUE +0 COMP SYNC.          
008100* STANDARDPRINTER PÅ KATALOGEN                                            
008200 77  KAT-AVSNITT-PV-A            PIC X(8)    VALUE '204     '.            
008300                                                                          
008400* ALTERNATIV PRINTER PÅ BEREDNINGEN                                       
008500 77  KAT-AVSNITT-PV-B            PIC X(8)    VALUE '205     '.            
008600                                                                          
008700* ALTERNATIV PRINTER PÅ VOLVO IT, PVV1:2                                  
008800 77  KAT-AVSNITT-IT-C            PIC X(8)    VALUE '203     '.            
008900                                                                          
009000 01  W-DAGENS-VECKA              PIC X(4).                                
009100 01  W-INMATAD-VECKA-FROM        PIC X(4).                                
009200 01  W-INMATAD-VECKA-FROM-NUM    REDEFINES W-INMATAD-VECKA-FROM           
009300                                 PIC 9(4).                                
009400 01  W-INMATAD-VECKA-TOM         PIC X(4).                                
009500 01  W-INMATAD-VECKA-TOM-NUM     REDEFINES W-INMATAD-VECKA-TOM            
009600                                 PIC 9(4).                                
009700 01  W-AAR                       PIC 9(2).                                
009800                                                                          
009900 01  DAGENS-DATUM                PIC 9(6).                                
010000 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
010100 01  DAGENS-AAVVD                PIC 9(5)    VALUE ZERO.                  
010200 01  WS-TIERSDAT-AAVVD           PIC 9(5)    VALUE ZERO.                  
010300 01  FILLER REDEFINES WS-TIERSDAT-AAVVD.                                  
010400     05  WS-TIERSDAT-AAVV        PIC 9(4).                                
010500     05  WS-TIERSDAT-D           PIC 9(1).                                
010600 01  W009VADD-DATUM              PIC S9(5) COMP-3 VALUE ZERO.             
010700 01  W009VADD-ANTAL              PIC S9(3) COMP-3 VALUE ZERO.             
010800 01  WS-ANTAL-GILTIGA            PIC S9(3) COMP-3 VALUE ZERO.             
010900 01  WS-KDCATPUB-R-AVV           PIC X(3)    VALUE SPACE.                 
011000 01  WS-KDCATPUB-AAAAVV          PIC X(6)    VALUE SPACE.                 
011100                                                                          
011200 01  SPAR-IDCATRAD               PIC 9(4)    VALUE ZERO.                  
011300 01  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO  COMP-3.          
011400 01  SPAR-IDRADN                 PIC S9(5)   VALUE ZERO  COMP-3.          
011500 01  SPAR-KVPUNKT                PIC S9(1)   VALUE ZERO  COMP-3.          
011600 01  SPAR-TEKATANM               PIC X(23).                               
011700 01  SPAR-BEART                  PIC X(25).                               
011800 01  SPAR-BETTEXT                PIC X(25)  VALUE SPACE.                  
011900 01  FILLER.                                                              
012000     03 FILLER OCCURS 3.                                                  
012100         06 SPAR-BERUBTXT        PIC X(30).                               
012200 01  SPAR-HAEN-B.                                                         
012300     03  SPAR-HAEN-A.                                                     
012400         05  FILLER              PIC X(4)  VALUE 'SEE '.                  
012500         05  SPAR-HAEN-IDCATGRP  PIC 9(2).                                
012600         05  FILLER              PIC X(1)  VALUE '-'.                     
012700         05  SPAR-HAEN-IDCATAVS  PIC 9(4).                                
012800         05  SPAR-HAEN-TKN       PIC X(1)  VALUE '/'.                     
012900         05  SPAR-HAEN-IDCATRAD  PIC 9(4)  BLANK WHEN ZERO.               
013000 01  SPAR-HAEN-KDHAEN            PIC X(01).                               
013100                                                                          
013200 01  UTSKRIFT-HJALP-AREA.                                                 
013300     03  FILLER OCCURS 5.                                                 
013400         06  UT-BENAMNING        PIC X(27).                               
013500     03  FILLER OCCURS 5.                                                 
013600         06  UT-ANMARKNING       PIC X(23).                               
013700                                                                          
013800 01  FEL-BEART-AREA.                                                      
013900     03  TRUNK-BEART             PIC X(14)   VALUE SPACE.                 
014000     03  FILLER                  PIC X(13)   VALUE ALL '*'.               
014100                                                                          
014200 01  PRINT-DESTINATION-AREA.                                              
014300     03  W-USERID.                                                        
014400        05 W-USERID-PREFIX       PIC X(3)    VALUE SPACE.                 
014500        05 FILLER                PIC X(5)    VALUE SPACE.                 
014600     03  W-IDPRTLST              PIC X(8)    VALUE SPACE.                 
014700                                                                          
014800*                                                                         
014900 01  FILLER                      PIC X(16)  VALUE 'KAT-NYCKLAR'.          
015000*                                                                         
015100 01  IDCATNR-WS                  PIC X(5)    VALUE SPACE.                 
015200 01  FILLER              REDEFINES IDCATNR-WS.                            
015300     03  KEY-IDCATNR             PIC 9(5).                                
015400 01  IDCATGRP-WS                 PIC X(2)    VALUE SPACE.                 
015500 01  FILLER              REDEFINES IDCATGRP-WS.                           
015600     03  KEY-IDCATGRP            PIC 9(2).                                
015700 01  IDCATAVS-WS                 PIC X(4)    VALUE SPACE.                 
015800 01  FILLER              REDEFINES IDCATAVS-WS.                           
015900     03  KEY-IDCATAVS            PIC 9(4).                                
016000 01  IDCATRAD-WS                 PIC X(4)    VALUE SPACE.                 
016100 01  FILLER              REDEFINES IDCATRAD-WS.                           
016200     03  KEY-IDCATRAD            PIC 9(4).                                
016300 01  IDSKYLT-WS                  PIC X(3)    VALUE SPACE.                 
016400 01  KDCATPUB-FOM-WS             PIC X(6)    VALUE SPACE.                 
016500 01  KDCATPUB-TOM-WS             PIC X(6)    VALUE SPACE.                 
016600                                                                          
016700     SKIP2                                                                
016800 01  HJALPVARIABLER.                                                      
016900     03  IND                     PIC S9(9)   COMP SYNC.                   
017000     03  IND2                    PIC S9(9)   COMP SYNC.                   
017100     03  IND2-MAX                PIC S9(9)   COMP SYNC.                   
017200     03  IND-TKN                 PIC S9(9)   COMP SYNC.                   
017300     03  FILLER OCCURS 3.                                                 
017400         06  W-BERUBTEXT         PIC X(30).                               
017500     03  W-BERUBTEXT-R4          PIC X(60)   VALUE SPACE.                 
017600     03  FILLER OCCURS 5.                                                 
017700         06  W-TEKOL             OCCURS 20                                
017800                                 PIC X(25)   VALUE SPACE.                 
017900     03  FILLER OCCURS 5.                                                 
018000         06  W-TEKOL-RED         OCCURS 20                                
018100                                 PIC X(25)   VALUE SPACE.                 
018200     03  FILLER OCCURS 4.                                                 
018300         06  WS-BERUBTEXT        OCCURS 20                                
018400                                 PIC X(60)   VALUE SPACE.                 
018500     03  FILLER OCCURS 20.                                                
018600         06  WS-IDILLU           PIC 9(5)    VALUE ZERO.                  
018700     03  FILLER OCCURS 20.                                                
018800         06  WS-KDCATPUB-R-FOM-RUB PIC X(3)  VALUE SPACE.                 
018900         06  WS-KDCATPUB-R-TOM-RUB PIC X(3)  VALUE SPACE.                 
019000     03  FILLER OCCURS 3.                                                 
019100         06  SPAR-IDFOTNR        PIC S9(5)   COMP-3.                      
019200     03  FILLER OCCURS 3.                                                 
019300         06  SPAR-FOT-RED        PIC X(03).                               
019400     03  FOTNOT-RAKNARE-2        PIC S9(9)   COMP SYNC.                   
019500     03  FOTNOT-RED-AREA         PIC X(15)   VALUE SPACE.                 
019600     03  WS-RUBRIK-RAD.                                                   
019700         06  FILLER              PIC X(9)    VALUE 'PUB FROM'.            
019800         06  WS-RUB-PUB-FROM     PIC X(3)    VALUE SPACE.                 
019900         06  FILLER              PIC X(2)    VALUE SPACE.                 
020000         06  FILLER              PIC X(8)    VALUE 'PUB TOM'.             
020100         06  WS-RUB-PUB-TOM      PIC X(3)    VALUE SPACE.                 
020200         06  FILLER              PIC X(7)    VALUE SPACE.                 
020300     03 WS-GILTIGA-AAR.                                                   
020400       05 WS-TIAAAA              PIC 9(4)    VALUE ZERO                   
020500                                 OCCURS 4.                                
020600                                                                          
020700     EJECT                                                                
020800                                                                          
020900 01  FILLER                      PIC X(16) VALUE 'MODELL-TABELL'.         
021000 01  MODELL-TABELL.                                                       
021100     03 TABELL-ELEMENT  OCCURS 10.                                        
021200       05 MODELL-IDMODELL           PIC X(3).                             
021300       05 MODELL-TIMODAAR-STA       PIC 9(4).                             
021400       05 MODELL-TIMODAAR-STO       PIC 9(4).                             
021500       05 MODELL-IDVARIANT          PIC X(15).                            
021600                                                                          
021700     EJECT                                                                
021800 01  DYNAMISKA-SUBPROGRAM.                                                
021900   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
022000   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
022100   03  W009KSIF                  PIC X(8)   VALUE 'W009KSIF'.             
022200   03  W009VADD                  PIC X(8)   VALUE 'W009VADD'.             
022300   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
022400   03  W006PRT                   PIC X(8)   VALUE 'W006PRT '.             
022500   03  W006PRS1                  PIC X(8)   VALUE 'W006PRS1'.             
022600     EJECT                                                                
022700***************************************************************           
022800*                 LIST RADER FÖR 'W006PRS1'                               
022900***************************************************************           
023000 01  FILLER                    PIC X(16) VALUE 'LIST-RADER'.              
023100     SKIP2                                                                
023200 01  HUVUD-TEXT-AREA.                                                     
023300     03 H-TEXT-S.                                                         
023400        05 FILLER              PIC X(8)  VALUE 'KATALOG'.                 
023500        05 L-IDCATNR-S         PIC Z(5).                                  
023600        05 FILLER              PIC X(4)  VALUE SPACE.                     
023700        05 FILLER              PIC X(5)  VALUE 'GRUPP'.                   
023800        05 L-IDCATGRP-S        PIC Z(3).                                  
023900        05 FILLER              PIC X(4)  VALUE SPACE.                     
024000        05 FILLER              PIC X(8)  VALUE 'AVSNITT'.                 
024100        05 L-IDCATAVS-S        PIC Z(5).                                  
024200        05 FILLER              PIC X(4)  VALUE SPACE.                     
024300        05 L-TEXT-RUB-S        PIC X(32) VALUE SPACE.                     
024400        05 FILLER              PIC X(2)  VALUE SPACE.                     
024500        05 FILLER              PIC X(5)  VALUE 'DATUM'.                   
024600        05 FILLER              PIC X     VALUE SPACE.                     
024700        05 L-DATUM-S           PIC 9(6).                                  
024800        05 FILLER              PIC X(3)  VALUE SPACE.                     
024900        05 FILLER              PIC X(4)  VALUE SPACE.                     
025000        05 L-SIDA-S            PIC Z(3).                                  
025100        05 FILLER              PIC X(14) VALUE SPACE.                     
025200        05 FILLER              PIC X(16) VALUE SPACE.                     
025300*                           SUMMA   132 COL                               
025400     03 H-TEXT-E.                                                         
025500        05 FILLER              PIC X(8)  VALUE 'CATALOG'.                 
025600        05 L-IDCATNR-E         PIC Z(5).                                  
025700        05 FILLER              PIC X(4)  VALUE SPACE.                     
025800        05 FILLER              PIC X(5)  VALUE 'GROUP'.                   
025900        05 L-IDCATGRP-E        PIC Z(3).                                  
026000        05 FILLER              PIC X(4)  VALUE SPACE.                     
026100        05 FILLER              PIC X(10) VALUE 'TEXT BLOCK'.              
026200        05 L-IDCATAVS-E        PIC Z(5).                                  
026300        05 FILLER              PIC X(3)  VALUE SPACE.                     
026400        05 L-TEXT-RUB-E        PIC X(32) VALUE SPACE.                     
026500        05 FILLER              PIC X(1)  VALUE SPACE.                     
026600        05 FILLER              PIC X(04) VALUE 'DATE'.                    
026700        05 FILLER              PIC X(02) VALUE SPACE.                     
026800        05 L-DATUM-E           PIC 9(6).                                  
026900        05 FILLER              PIC X(03) VALUE SPACE.                     
027000        05 FILLER              PIC X(04) VALUE SPACE.                     
027100        05 L-SIDA-E            PIC Z(3).                                  
027200        05 FILLER              PIC X(14) VALUE SPACE.                     
027300        05 FILLER              PIC X(16) VALUE SPACE.                     
027400*                           SUMMA   132 COL                               
027500 01  FILLER REDEFINES HUVUD-TEXT-AREA.                                    
027600     03 HUVUD-TEXT OCCURS 2    PIC X(132).                                
027700                                                                          
027800 01  V-RUB-TXT-S               PIC X(24)                                  
027900                        VALUE '*  NYCKLAR TILL VADIS  *'.                 
028000 01  V-RUB-TXT-E               PIC X(24)                                  
028100                        VALUE '* VADIS SELECTION KEYS *'.                 
028200                                                                          
028300 01  VADIS-RUB-1H.                                                        
028400     03 V-R-1H-S.                                                         
028500        05 FILLER              PIC X(42)  VALUE                           
028600              '------------------------------------------'.               
028700        05 FILLER              PIC X(32)  VALUE                           
028800                 '    GENERELLT FÖR KATALOGEN     '.                      
028900        05 FILLER              PIC X(42)  VALUE                           
029000              '------------------------------------------'.               
029100        05 FILLER              PIC X(16)  VALUE SPACE.                    
029200*                           SUMMA   132 COL                               
029300     03 V-R-1H-E.                                                         
029400        05 FILLER              PIC X(42)  VALUE                           
029500              '------------------------------------------'.               
029600        05 FILLER              PIC X(32)  VALUE                           
029700                 '   GENERAL FOR THE CATALOGUE    '.                      
029800        05 FILLER              PIC X(42)  VALUE                           
029900              '------------------------------------------'.               
030000        05 FILLER              PIC X(16)  VALUE SPACE.                    
030100*                           SUMMA   132 COL                               
030200 01  FILLER REDEFINES VADIS-RUB-1H.                                       
030300     03 VAD-RUB-1H OCCURS 2    PIC X(132).                                
030400                                                                          
030500 01  VADIS-KOL-RUB1-AREA.                                                 
030600     03 TEXT-HR1-S.                                                       
030700        05 FILLER              PIC X(05) VALUE 'PUBF'.                    
030800        05 FILLER              PIC X(05) VALUE 'PUBT'.                    
030900        05 FILLER              PIC X(04) VALUE 'KOL '.                    
031000        05 FILLER              PIC X(04) VALUE 'EXC '.                    
031100        05 FILLER              PIC X(09) VALUE 'MODELL'.                  
031200        05 FILLER              PIC X(09) VALUE 'START'.                   
031300        05 FILLER              PIC X(11) VALUE 'SLUT'.                    
031400        05 FILLER              PIC X(16) VALUE 'VARIANT'.                 
031500        05 FILLER              PIC X(16) VALUE 'KOMB.VARIANT'.            
031600        05 FILLER              PIC X(08) VALUE 'CHA.'.                    
031700        05 FILLER              PIC X(13) VALUE 'CHA.NR.'.                 
031800        05 FILLER              PIC X(13) VALUE 'CHA.NR.'.                 
031900        05 FILLER              PIC X(19) VALUE SPACE.                     
032000*                           SUMMA   132 COL                               
032100     03 TEXT-HR1-E.                                                       
032200        05 FILLER              PIC X(05) VALUE 'PUBF'.                    
032300        05 FILLER              PIC X(05) VALUE 'PUBT'.                    
032400        05 FILLER              PIC X(04) VALUE 'COL'.                     
032500        05 FILLER              PIC X(04) VALUE 'EXC'.                     
032600        05 FILLER              PIC X(09) VALUE 'MODEL'.                   
032700        05 FILLER              PIC X(09) VALUE 'START'.                   
032800        05 FILLER              PIC X(11) VALUE 'END'.                     
032900        05 FILLER              PIC X(16) VALUE 'VARIANT'.                 
033000        05 FILLER              PIC X(16) VALUE 'COMB.VARIANT'.            
033100        05 FILLER              PIC X(08) VALUE 'CHA.'.                    
033200        05 FILLER              PIC X(13) VALUE 'CHA.NO.'.                 
033300        05 FILLER              PIC X(13) VALUE 'CHA.NO.'.                 
033400        05 FILLER              PIC X(19) VALUE SPACE.                     
033500*                           SUMMA   132 COL                               
033600 01  FILLER REDEFINES VADIS-KOL-RUB1-AREA.                                
033700     03 VADIS-KOL-RUB1 OCCURS 2 PIC X(132).                               
033800                                                                          
033900 01  VADIS-KOL-RUB2-AREA.                                                 
034000     03 TEXT-HR2-S.                                                       
034100        05 FILLER              PIC X(10) VALUE SPACE.                     
034200        05 FILLER              PIC X(04) VALUE ' '.                       
034300        05 FILLER              PIC X(04) VALUE ' '.                       
034400        05 FILLER              PIC X(09) VALUE ' '.                       
034500        05 FILLER              PIC X(09) VALUE 'ÅR'.                      
034600        05 FILLER              PIC X(11) VALUE 'ÅR'.                      
034700        05 FILLER              PIC X(16) VALUE ' '.                       
034800        05 FILLER              PIC X(16) VALUE ' '.                       
034900        05 FILLER              PIC X(08) VALUE 'TYP'.                     
035000        05 FILLER              PIC X(13) VALUE 'START'.                   
035100        05 FILLER              PIC X(13) VALUE 'SLUT'.                    
035200        05 FILLER              PIC X(19) VALUE SPACE.                     
035300*                           SUMMA   132 COL                               
035400     03 TEXT-HR2-E.                                                       
035500        05 FILLER              PIC X(10) VALUE SPACE.                     
035600        05 FILLER              PIC X(04) VALUE ' '.                       
035700        05 FILLER              PIC X(04) VALUE ' '.                       
035800        05 FILLER              PIC X(09) VALUE ' '.                       
035900        05 FILLER              PIC X(09) VALUE 'YEAR'.                    
036000        05 FILLER              PIC X(11) VALUE 'YEAR'.                    
036100        05 FILLER              PIC X(16) VALUE ' '.                       
036200        05 FILLER              PIC X(16) VALUE ' '.                       
036300        05 FILLER              PIC X(08) VALUE 'TYPE'.                    
036400        05 FILLER              PIC X(13) VALUE 'START'.                   
036500        05 FILLER              PIC X(13) VALUE 'END'.                     
036600        05 FILLER              PIC X(19) VALUE SPACE.                     
036700*                           SUMMA   132 COL                               
036800 01  FILLER REDEFINES VADIS-KOL-RUB2-AREA.                                
036900     03 VADIS-KOL-RUB2 OCCURS 2 PIC X(132).                               
037000                                                                          
037100*                                                                         
037200 01  VADIS-RUB-2H.                                                        
037300     03 V-R-2H-S.                                                         
037400        05 FILLER              PIC X(42) VALUE                            
037500              '------------------------------------------'.               
037600        05 FILLER              PIC X(32) VALUE                            
037700                 '  SPECIELLT FÖR DETTA AVSNITT   '.                      
037800        05 FILLER              PIC X(42) VALUE                            
037900              '------------------------------------------'.               
038000        05 FILLER              PIC X(16) VALUE SPACE.                     
038100*                           SUMMA   132 COL                               
038200     03 V-R-2H-E.                                                         
038300        05 FILLER              PIC X(42) VALUE                            
038400              '------------------------------------------'.               
038500        05 FILLER              PIC X(32)  VALUE                           
038600                 '  SPECIALLY FOR THIS TEXT-BLOCK '.                      
038700        05 FILLER              PIC X(42) VALUE                            
038800              '------------------------------------------'.               
038900        05 FILLER              PIC X(16) VALUE SPACE.                     
039000*                           SUMMA   132 COL                               
039100 01  FILLER REDEFINES VADIS-RUB-2H.                                       
039200     03 VAD-RUB-2H OCCURS 2    PIC X(132).                                
039300                                                                          
039400*                                                                         
039500 01  VADIS-RAD.                                                           
039600     03 L-KDCATPUB-R-FOM-VADIS                                            
039700                            PIC X(3)  VALUE SPACE.                        
039800     03 FILLER              PIC X(2)  VALUE SPACE.                        
039900     03 L-KDCATPUB-R-TOM-VADIS                                            
040000                            PIC X(3)  VALUE SPACE.                        
040100     03 FILLER              PIC X(3)  VALUE SPACE.                        
040200     03 L-IDKOL             PIC X     VALUE SPACE.                        
040300     03 FILLER              PIC X(2)  VALUE SPACE.                        
040400     03 L-FLEXCL            PIC X     VALUE SPACE.                        
040500     03 FILLER              PIC X(3)  VALUE SPACE.                        
040600     03 L-IDMODELL          PIC X(3)  VALUE SPACE.                        
040700     03 FILLER              PIC X(6)  VALUE SPACE.                        
040800     03 L-TIMODAAR-STA      PIC Z(4)  VALUE ZERO.                         
040900     03 FILLER              PIC X(5)  VALUE SPACE.                        
041000     03 L-TIMODAAR-STO      PIC Z(4)  VALUE ZERO.                         
041100     03 FILLER              PIC X(7)  VALUE SPACE.                        
041200     03 L-IDVARIANT         PIC X(15) VALUE SPACE.                        
041300     03 FILLER              PIC X     VALUE SPACE.                        
041400     03 L-IDVARIANT-2       PIC X(15) VALUE SPACE.                        
041500     03 FILLER              PIC X     VALUE SPACE.                        
041600     03 L-KDCHATYP          PIC Z     VALUE ZERO.                         
041700     03 FILLER              PIC X(7)  VALUE SPACE.                        
041800     03 L-IDCHASSI-STA      PIC Z(6)  VALUE ZERO.                         
041900     03 FILLER              PIC X(7)  VALUE SPACE.                        
042000     03 L-IDCHASSI-STO      PIC Z(6)  VALUE ZERO.                         
042100     03 FILLER              PIC X(8)  VALUE SPACE.                        
042200     03 FILLER              PIC X(18) VALUE SPACE.                        
042300*                           SUMMA   132 COL                               
042400                                                                          
042500 01  RUB-RAD-1.                                                           
042600     03 FILLER                 PIC X(01) VALUE 'A'.                       
042700     03 FILLER                 PIC X(01) VALUE SPACE.                     
042800     03 L-TEKOL-A              PIC X(25).                                 
042900     03 FILLER                 PIC X(02) VALUE SPACE.                     
043000     03 FILLER                 PIC X(02) VALUE 'R1'.                      
043100     03 FILLER                 PIC X(01) VALUE SPACE.                     
043200     03 L-BERUBTEXT-R1         PIC X(30).                                 
043300     03 FILLER                 PIC X(18) VALUE SPACE.                     
043400     03 FILLER                 PIC X(12)                                  
043500                                  VALUE 'ILLUSTRATION'.                   
043600     03 FILLER                 PIC X(01) VALUE SPACE.                     
043700     03 L-IDILLU               PIC 9(05).                                 
043800     03 FILLER                 PIC X(10) VALUE SPACE.                     
043900     03 FILLER                 PIC X(09) VALUE 'PUB FROM '.               
044000     03 L-KDCATPUB-R-FOM-RUB   PIC X(03) VALUE SPACE.                     
044100     03 FILLER                 PIC X(12) VALUE SPACE.                     
044200*                           SUMMA   132 COL                               
044300                                                                          
044400 01  RUB-RAD-2.                                                           
044500     03  FILLER                PIC X(01) VALUE 'B'.                       
044600     03  FILLER                PIC X(01) VALUE SPACE.                     
044700     03  L-TEKOL-B             PIC X(25).                                 
044800     03  FILLER                PIC X(02) VALUE SPACE.                     
044900     03  FILLER                PIC X(02) VALUE 'R2'.                      
045000     03  FILLER                PIC X(01) VALUE SPACE.                     
045100     03  L-BERUBTEXT-R2        PIC X(30).                                 
045200     03  FILLER                PIC X(18) VALUE SPACE.                     
045300     03  L-UTGAVA-TEXT         PIC X(07) VALUE 'UTGÅVA '.                 
045400     03  FILLER                PIC X(06) VALUE SPACE.                     
045500     03  L-IDVERS              PIC 9(03).                                 
045600     03 FILLER                 PIC X(16) VALUE SPACE.                     
045700     03 FILLER                 PIC X(05) VALUE 'TOM  '.                   
045800     03 L-KDCATPUB-R-TOM-RUB   PIC X(03) VALUE SPACE.                     
045900     03 FILLER                 PIC X(12) VALUE SPACE.                     
046000*                           SUMMA   132 COL                               
046100 01  RUB-RAD-3.                                                           
046200     03  FILLER                PIC X(01) VALUE 'C'.                       
046300     03  FILLER                PIC X(01) VALUE SPACE.                     
046400     03  L-TEKOL-C             PIC X(25).                                 
046500     03  FILLER                PIC X(02) VALUE SPACE.                     
046600     03  FILLER                PIC X(02) VALUE 'R3'.                      
046700     03  FILLER                PIC X(01) VALUE SPACE.                     
046800     03  L-BERUBTEXT-R3        PIC X(30).                                 
046900     03  FILLER                PIC X(54) VALUE SPACE.                     
047000     03 FILLER                 PIC X(16) VALUE SPACE.                     
047100*                           SUMMA   132 COL                               
047200 01  RUB-RAD-4.                                                           
047300     03  FILLER                PIC X(01) VALUE 'D'.                       
047400     03  FILLER                PIC X(01) VALUE SPACE.                     
047500     03  L-TEKOL-D             PIC X(25).                                 
047600     03  FILLER                PIC X(02) VALUE SPACE.                     
047700     03  FILLER                PIC X(02) VALUE 'R4'.                      
047800     03  FILLER                PIC X(01) VALUE SPACE.                     
047900     03  L-BERUBTEXT-R4        PIC X(60).                                 
048000     03  FILLER                PIC X(24) VALUE SPACE.                     
048100     03 FILLER                 PIC X(16) VALUE SPACE.                     
048200*                           SUMMA   132 COL                               
048300                                                                          
048400 01  RUB-RAD-5.                                                           
048500     03  FILLER                PIC X(01) VALUE 'E'.                       
048600     03  FILLER                PIC X(01) VALUE SPACE.                     
048700     03  L-TEKOL-E             PIC X(25).                                 
048800     03  FILLER                PIC X(89) VALUE SPACE.                     
048900     03 FILLER                 PIC X(16) VALUE SPACE.                     
049000*                           SUMMA   132 COL                               
049100                                                                          
049200 01  TEXT-HUVUD-RAD-AREA.                                                 
049300     03 TEXT-HR-S.                                                        
049400        05 FILLER              PIC X(03) VALUE 'KOD'.                     
049500        05 FILLER              PIC X(01) VALUE SPACE.                     
049600        05 FILLER              PIC X(01) VALUE 'F'.                       
049700        05 FILLER              PIC X(01) VALUE SPACE.                     
049800        05 FILLER              PIC X(05) VALUE 'RADNR'.                   
049900        05 FILLER              PIC X(01) VALUE SPACE.                     
050000        05 FILLER              PIC X(03) VALUE 'POS'.                     
050100        05 FILLER              PIC X(03) VALUE SPACE.                     
050200        05 FILLER              PIC X(02) VALUE 'K '.                      
050300        05 FILLER              PIC X(01) VALUE SPACE.                     
050400        05 FILLER              PIC X(09) VALUE 'ARTIKELNR'.               
050500        05 FILLER              PIC X(02) VALUE SPACE.                     
050600        05 FILLER              PIC X(03) VALUE ' A '.                     
050700        05 FILLER              PIC X(01) VALUE SPACE.                     
050800        05 FILLER              PIC X(03) VALUE ' B '.                     
050900        05 FILLER              PIC X(01) VALUE SPACE.                     
051000        05 FILLER              PIC X(03) VALUE ' C '.                     
051100        05 FILLER              PIC X(01) VALUE SPACE.                     
051200        05 FILLER              PIC X(03) VALUE ' D '.                     
051300        05 FILLER              PIC X(01) VALUE SPACE.                     
051400        05 FILLER              PIC X(03) VALUE ' E '.                     
051500        05 FILLER              PIC X(01) VALUE SPACE.                     
051600        05 FILLER              PIC X(02) VALUE 'PS'.                      
051700        05 FILLER              PIC X(01) VALUE SPACE.                     
051800        05 FILLER              PIC X(09) VALUE 'BENÄMNING'.               
051900        05 FILLER              PIC X(21) VALUE SPACE.                     
052000        05 FILLER              PIC X(10) VALUE 'ANMÄRKNING'.              
052100        05 FILLER              PIC X(14) VALUE SPACE.                     
052200        05 FILLER              PIC X(06) VALUE 'PUB-F '.                  
052300        05 FILLER              PIC X(05) VALUE 'PUB-T'.                   
052400        05 FILLER              PIC X(12) VALUE SPACE.                     
052500*                           SUMMA   132 COL                               
052600     03 TEXT-HR-E.                                                        
052700        05 FILLER              PIC X(03) VALUE 'CO '.                     
052800        05 FILLER              PIC X(02) VALUE 'FB'.                      
052900        05 FILLER              PIC X(01) VALUE SPACE.                     
053000        05 FILLER              PIC X(05) VALUE 'LINE '.                   
053100        05 FILLER              PIC X(01) VALUE SPACE.                     
053200        05 FILLER              PIC X(03) VALUE 'POS'.                     
053300        05 FILLER              PIC X(03) VALUE SPACE.                     
053400        05 FILLER              PIC X(02) VALUE 'K '.                      
053500        05 FILLER              PIC X(01) VALUE SPACE.                     
053600        05 FILLER              PIC X(10) VALUE 'PARTNUMBER'.              
053700        05 FILLER              PIC X(01) VALUE SPACE.                     
053800        05 FILLER              PIC X(03) VALUE ' A '.                     
053900        05 FILLER              PIC X(01) VALUE SPACE.                     
054000        05 FILLER              PIC X(03) VALUE ' B '.                     
054100        05 FILLER              PIC X(01) VALUE SPACE.                     
054200        05 FILLER              PIC X(03) VALUE ' C '.                     
054300        05 FILLER              PIC X(01) VALUE SPACE.                     
054400        05 FILLER              PIC X(03) VALUE ' D '.                     
054500        05 FILLER              PIC X(01) VALUE SPACE.                     
054600        05 FILLER              PIC X(03) VALUE ' E '.                     
054700        05 FILLER              PIC X(01) VALUE SPACE.                     
054800        05 FILLER              PIC X(02) VALUE 'PS'.                      
054900        05 FILLER              PIC X(01) VALUE SPACE.                     
055000        05 FILLER              PIC X(11) VALUE 'DESCRIPTION'.             
055100        05 FILLER              PIC X(19) VALUE SPACE.                     
055200        05 FILLER              PIC X(11) VALUE 'NOTE COLUMN'.             
055300        05 FILLER              PIC X(12) VALUE SPACE.                     
055400        05 FILLER              PIC X(06) VALUE 'PUB-F '.                  
055500        05 FILLER              PIC X(05) VALUE 'PUB-T'.                   
055600        05 FILLER              PIC X(12) VALUE SPACE.                     
055700*                           SUMMA   132 COL                               
055800 01  FILLER REDEFINES TEXT-HUVUD-RAD-AREA.                                
055900     03 TEXT-HUVUD-RAD OCCURS 2 PIC X(132).                               
056000                                                                          
056100 01  STRECKRAD.                                                           
056200     03 FILLER                PIC X(42) VALUE                             
056300           '------------------------------------------'.                  
056400     03 FILLER                PIC X(32) VALUE                             
056500              '---------RUBRIK/HEADING---------'.                         
056600     03 FILLER                PIC X(42) VALUE                             
056700           '------------------------------------------'.                  
056800     03 FILLER                PIC X(16) VALUE SPACE.                      
056900*                         SUMMA     132 COL                               
057000*                                                                         
057100                                                                          
057200 01  STRECKRAD-2.                                                         
057300     03 FILLER                PIC X(42) VALUE                             
057400           '------------------------------------------'.                  
057500     03 FILLER                PIC X(36) VALUE                             
057600              '------------------------------------'.                     
057700     03 FILLER                PIC X(42) VALUE                             
057800           '------------------------------------------'.                  
057900     03 FILLER                PIC X(12) VALUE SPACE.                      
058000*                         SUMMA     132 COL                               
058100*                                                                         
058200 01  TEXT-SLUT-RAD-AREA.                                                  
058300     03 TEXT-SR-S.                                                        
058400        05 FILLER              PIC X(42) VALUE ALL '-'.                   
058500        05 FILLER              PIC X(32)  VALUE                           
058600                 '    ARTIKELRADER EJ BESTÄLLT    '.                      
058700        05 FILLER              PIC X(42) VALUE ALL '-'.                   
058800        05 FILLER              PIC X(16) VALUE SPACE.                     
058900     03 TEXT-SR-E.                                                        
059000        05 FILLER              PIC X(42) VALUE ALL '-'.                   
059100        05 FILLER              PIC X(32)  VALUE                           
059200                 '  PART LINES ARE NOT ORDERED    '.                      
059300        05 FILLER              PIC X(42) VALUE ALL '-'.                   
059400        05 FILLER              PIC X(16) VALUE SPACE.                     
059500 01  FILLER REDEFINES TEXT-SLUT-RAD-AREA.                                 
059600     03 TEXT-SLUT-RAD OCCURS 2 PIC X(132).                                
059700                                                                          
059800*                                                                         
059900                                                                          
060000 01  TEXT-RAD.                                                            
060100     03 FILLER                 PIC X     VALUE SPACE.                     
060200     03 L-KDRADST              PIC X.                                     
060300     03 FILLER                 PIC X(2)  VALUE SPACE.                     
060400     03 L-KDFBX                PIC X.                                     
060500     03 FILLER                 PIC X     VALUE SPACE.                     
060600     03 L-IDCATRAD             PIC 9(5).                                  
060700     03 FILLER                 PIC X     VALUE SPACE.                     
060800     03 L-IDCATPOS             PIC X(4).                                  
060900     03 FILLER                 PIC X(2)  VALUE SPACE.                     
061000     03 L-KURSIV               PIC X(2).                                  
061100     03 L-IDARTNR              PIC Z(9).                                  
061200     03 L-STRECK               PIC X     VALUE SPACE.                     
061300     03 L-REKSIFFRA            PIC X.                                     
061400     03 FILLER                 PIC X     VALUE SPACE.                     
061500     03 FILLER OCCURS 5.                                                  
061600       05 L-KVKOL              PIC X(3).                                  
061700       05 FILLER               PIC X     VALUE SPACE.                     
061800     03 L-KDPS                 PIC X(2).                                  
061900     03 FILLER                 PIC X     VALUE SPACE.                     
062000     03 L-BEART                PIC X(28)  VALUE SPACE.                    
062100     03 FILLER REDEFINES L-BEART.                                         
062200         05 PKT1               PIC X.                                     
062300         05 L-BEART-PKT1       PIC X(27).                                 
062400     03 FILLER REDEFINES L-BEART.                                         
062500         05 PKT2               PIC X(2).                                  
062600         05 L-BEART-PKT2       PIC X(26).                                 
062700     03 FILLER REDEFINES L-BEART.                                         
062800         05 PKT3               PIC X(3).                                  
062900         05 L-BEART-PKT3       PIC X(25).                                 
063000     03 FILLER REDEFINES L-BEART.                                         
063100         05 PKT4               PIC X(4).                                  
063200         05 L-BEART-PKT4       PIC X(24).                                 
063300     03 FILLER REDEFINES L-BEART.                                         
063400         05 BEART-TKN OCCURS 28 PIC X(1).                                 
063500     03 FILLER                 PIC X(2)    VALUE SPACE.                   
063600     03 L-TEKATANM             PIC X(23).                                 
063700     03 FILLER                 PIC X       VALUE SPACE.                   
063800     03 L-KDCATPUB-R-FOM-RAD   PIC X(3).                                  
063900     03 FILLER                 PIC X(3)    VALUE SPACE.                   
064000     03 L-KDCATPUB-R-TOM-RAD   PIC X(3).                                  
064100     03 FILLER                 PIC X(10) VALUE SPACE.                     
064200*                           SUMMA   132 COL                               
064300 01  EXTRA-TEXTRAD.                                                       
064400     03  FILLER                PIC X(55)   VALUE SPACE.                   
064500     03  L-BEART-E.                                                       
064600         06  PUNKTER           PIC X(04)   VALUE SPACE.                   
064700         06  FILLER            PIC X(24)   VALUE SPACE.                   
064800     03  FILLER REDEFINES L-BEART-E.                                      
064900         06  FILLER            PIC X(1).                                  
065000         06  L-BEART-PKT1-E    PIC X(27).                                 
065100     03  FILLER REDEFINES L-BEART-E.                                      
065200         06  FILLER            PIC X(2).                                  
065300         06  L-BEART-PKT2-E    PIC X(26).                                 
065400     03  FILLER REDEFINES L-BEART-E.                                      
065500         06  FILLER            PIC X(3).                                  
065600         06  L-BEART-PKT3-E    PIC X(25).                                 
065700     03  FILLER REDEFINES L-BEART-E.                                      
065800         06  FILLER            PIC X(4).                                  
065900         06  L-BEART-PKT4-E    PIC X(24).                                 
066000     03  FILLER                PIC X(02) VALUE SPACE.                     
066100     03  L-TEKATANM-E          PIC X(23).                                 
066200     03  FILLER                PIC X(8)   VALUE SPACE.                    
066300     03 FILLER                 PIC X(16) VALUE SPACE.                     
066400*                           SUMMA   132 COL                               
066500 01  FOTNOT-RAD.                                                          
066600     03  L-FOTNOT-RED          PIC Z(04).                                 
066700     03  FILLER                PIC X(04)  VALUE ') = '.                   
066800     03  L-IDFOTNR-FYS         PIC 9(05)  VALUE ZERO.                     
066900     03  FILLER                PIC X(02)  VALUE SPACE.                    
067000     03  L-BEFOTNOT            PIC X(55)  VALUE SPACE.                    
067100     03  FILLER                PIC X(46)  VALUE SPACE.                    
067200     03 FILLER                 PIC X(16) VALUE SPACE.                     
067300*                           SUMMA   132 COL                               
067400 01  FOTNOT-RAD-E.                                                        
067500     03  FILLER                PIC X(15)  VALUE SPACE.                    
067600     03  L-BEFOTNOT-E          PIC X(55)  VALUE SPACE.                    
067700     03  FILLER                PIC X(46)  VALUE SPACE.                    
067800     03 FILLER                 PIC X(16) VALUE SPACE.                     
067900*                           SUMMA   132 COL                               
068000     EJECT                                                                
068100***************************************************************           
068200*                 PRT-AREA FÖR 'W006PRS1'                                 
068300***************************************************************           
068400     SKIP2                                                                
068500*01  -COPY W006PRAR                                                       
068600     EJECT                                                                
068700***************************************************************           
068800*                 PRINTER-AREA FÖR 'W006PRT'                              
068900***************************************************************           
069000     SKIP2                                                                
069100*01  -COPY W006PRT                                                        
069200     EJECT                                                                
069300***************************************************************           
069400*                 TEST OM UTBYTESENHET                                    
069500***************************************************************           
069600     SKIP2                                                                
069700*01  -COPY WWBYT02.                                                       
069800     EJECT                                                                
069900***************************************************************           
070000*                NYCKLAR TILL DLI                                         
070100***************************************************************           
070200     SKIP2                                                                
070300 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
070400 01  NYCKLAR-TILL-DLI.                                                    
070500     SKIP2                                                                
070600   03  W-IDCATNR-X.                                                       
070700       05  W-IDCATNR-1           PIC 9(5)    VALUE ZERO.                  
070800   03  W-WDN501-X.                                                        
070900       05  W-IDCATNR             PIC 9(5)    VALUE ZERO.                  
071000       05  W-IDCATGRP            PIC 9(2)    VALUE ZERO.                  
071100       05  W-IDCATAVS            PIC 9(4)    VALUE ZERO.                  
071200   03  W-WDN511-X.                                                        
071300       05  W-IDILLU              PIC S9(5)   COMP-3                       
071400                                             VALUE ZERO.                  
071500       05  W-KDCATPUB-511        PIC X(6)    VALUE LOW-VALUE.             
071600   03  W-WDN512-X.                                                        
071700       05  W-IDCATRAD            PIC 9(4)    VALUE ZERO.                  
071800       05  W-KDCATPUB-512        PIC X(6)    VALUE LOW-VALUE.             
071900   03  W-WDN513-X.                                                        
072000       05  W-IDRADN              PIC S9(5)   VALUE ZERO  COMP-3.          
072100       05  W-KDCATPUB-513        PIC X(6)    VALUE LOW-VALUE.             
072200   03  W-IDSEGMNR-X.                                                      
072300       05  W-IDSEGMNR            PIC S9(1)   VALUE ZERO  COMP-3.          
072400   03  W-IDARTNR-X.                                                       
072500       05  W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
072600   03  W-IDRUBNR-X.                                                       
072700       05  W-IDRUBNR             PIC S9(5)   VALUE ZERO  COMP-3.          
072800   03  W-IDTTEXNR-X.                                                      
072900       05  W-IDTTEXNR            PIC S9(5)   VALUE ZERO  COMP-3.          
073000   03  W-IDSKYLT-X.                                                       
073100       05  W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
073200   03  W-IDSKYLT-FOT-X.                                                   
073300       05  W-IDSKYLT-FOT         PIC X(3)    VALUE SPACE.                 
073400   03  W-BEART-X.                                                         
073500       05  W-BEART               PIC X(25)   VALUE SPACE.                 
073600   03  W-IDFOTNR-X.                                                       
073700       05  W-IDFOTNR             PIC S9(5)   VALUE ZERO  COMP-3.          
073800     EJECT                                                                
073900***************************************************************           
074000*                     MEDELANDE                                           
074100***************************************************************           
074200 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
074300 01  MEDDELANDEN.                                                         
074400     03 FILLER-1.                                                         
074500          05 FILLER              PIC X(40)                                
074600              VALUE '    NYCKEL EJ NUMERISK                  '.           
074700          05 FILLER              PIC X(40)                                
074800              VALUE '    KEY NOT NUMERIC                     '.           
074900     03 FILLER REDEFINES FILLER-1.                                        
075000          05 FEL-1   OCCURS 2    PIC X(40).                               
075100                                                                          
075200     03 FILLER-2.                                                         
075300          05 FILLER              PIC X(40)                                
075400              VALUE '    UPPLYSTA FÄLT FEL                   '.           
075500          05 FILLER              PIC X(40)                                
075600              VALUE '    HIGHLIGHTED FIELDS WRONG            '.           
075700     03 FILLER REDEFINES FILLER-2.                                        
075800          05 FEL-2   OCCURS 2    PIC X(40).                               
075900                                                                          
076000     03 FILLER-3.                                                         
076100          05 FILLER              PIC X(40)                                
076200              VALUE '    NYCKEL FELAKTIG                     '.           
076300          05 FILLER              PIC X(40)                                
076400              VALUE '    WRONG KEY                           '.           
076500     03 FILLER REDEFINES FILLER-3.                                        
076600          05 FEL-3   OCCURS 2    PIC X(40).                               
076700                                                                          
076800     03 FILLER-4.                                                         
076900          05 FILLER              PIC X(40)                                
077000              VALUE '    SPRÅK FINNS EJ                      '.           
077100          05 FILLER              PIC X(40)                                
077200              VALUE '    LANGUAGE NOT FOUND                  '.           
077300     03 FILLER REDEFINES FILLER-4.                                        
077400          05 FEL-4   OCCURS 2    PIC X(40).                               
077500                                                                          
077600     03 FILLER-6.                                                         
077700          05 FILLER              PIC X(40)                                
077800              VALUE '    UTSKRIFT MED PF11                   '.           
077900          05 FILLER              PIC X(40)                                
078000              VALUE '    USE PF11 FOR PRINT                  '.           
078100     03 FILLER REDEFINES FILLER-6.                                        
078200          05 FEL-5   OCCURS 2    PIC X(40).                               
078300                                                                          
078400     03 FILLER-8.                                                         
078500          05 FILLER              PIC X(40)                                
078600              VALUE 'MER ÄN 25 UNIKA FOTNOTNR.  FIXA TILL !  '.           
078700          05 FILLER              PIC X(40)                                
078800              VALUE 'MORE THAN 25 FOOTNOTES.  FIX THIS !     '.           
078900     03 FILLER REDEFINES FILLER-8.                                        
079000          05 FEL-6   OCCURS 2    PIC X(40).                               
079100                                                                          
079200     03 FILLER-9.                                                         
079300          05 FILLER              PIC X(40)                                
079400              VALUE 'FELAKTIGT PRINTERVAL                    '.           
079500          05 FILLER              PIC X(40)                                
079600              VALUE 'WRONG PRINTER SELECTION                 '.           
079700     03 FILLER REDEFINES FILLER-9.                                        
079800          05 FEL-7   OCCURS 2    PIC X(40).                               
079900                                                                          
080000     03 FILLER-A.                                                         
080100          05 FILLER              PIC X(40)                                
080200              VALUE 'FELAKTIG OMFATTNINGSKOD                 '.           
080300          05 FILLER              PIC X(40)                                
080400              VALUE 'WRONG EXTENT SELECTION                 '.            
080500     03 FILLER REDEFINES FILLER-A.                                        
080600          05 FEL-8   OCCURS 2    PIC X(40).                               
080700                                                                          
080800     03 FILLER-5.                                                         
080900        05 FILLER-5-S.                                                    
081000           07 FILLER              PIC X(28)                               
081100              VALUE 'LISTAN KÖAD FÖR UTSKRIFT PÅ '.                       
081200           07 MED-1S-IDLTERM      PIC X(8).                               
081300           07 FILLER              PIC X(25).                              
081400        05 FILLER-5-E.                                                    
081500           07 FILLER              PIC X(28)                               
081600              VALUE 'LIST IS QUEUED TO PRINTER   '.                       
081700           07 MED-1E-IDLTERM      PIC X(8).                               
081800           07 FILLER              PIC X(25).                              
081900     03 FILLER REDEFINES FILLER-5.                                        
082000        05 MED-1   OCCURS 2      PIC X(61).                               
082100                                                                          
082200     03 FILLER-7.                                                         
082300          05 FILLER              PIC X(25)                                
082400              VALUE '****** INGEN BENÄMN. REG.'.                          
082500          05 FILLER              PIC X(25)                                
082600              VALUE '****** NO DESCR. REGISTR.'.                          
082700     03 FILLER REDEFINES FILLER-7.                                        
082800          05 MED-2   OCCURS 2    PIC X(25).                               
082900     EJECT                                                                
083000******************************************************************        
083100*                  FOTNOT TABELL                                          
083200******************************************************************        
083300 01  FILLER                      PIC X(16) VALUE 'FOTNOT-TABELL'.         
083400     SKIP2                                                                
083500 01  FOTNOT-RAKNARE              PIC S9(2) VALUE +1.                      
083600 01  FOTNOT-TAB-MAX              PIC S9(2) VALUE +25.                     
083700*                                                                         
083800 01  FOTNOT-TABELL-VARDEN.                                                
083900*                                                                         
084000     03  FILLER                  PIC S9(5) VALUE ZERO.                    
084100     03  FILLER                  PIC X(3)  VALUE '1  '.                   
084200     03  FILLER                  PIC S9(5) VALUE ZERO.                    
084300     03  FILLER                  PIC X(3)  VALUE '2  '.                   
084400     03  FILLER                  PIC S9(5) VALUE ZERO.                    
084500     03  FILLER                  PIC X(3)  VALUE '3  '.                   
084600     03  FILLER                  PIC S9(5) VALUE ZERO.                    
084700     03  FILLER                  PIC X(3)  VALUE '4  '.                   
084800     03  FILLER                  PIC S9(5) VALUE ZERO.                    
084900     03  FILLER                  PIC X(3)  VALUE '5  '.                   
085000     03  FILLER                  PIC S9(5) VALUE ZERO.                    
085100     03  FILLER                  PIC X(3)  VALUE '6  '.                   
085200     03  FILLER                  PIC S9(5) VALUE ZERO.                    
085300     03  FILLER                  PIC X(3)  VALUE '7  '.                   
085400     03  FILLER                  PIC S9(5) VALUE ZERO.                    
085500     03  FILLER                  PIC X(3)  VALUE '8  '.                   
085600     03  FILLER                  PIC S9(5) VALUE ZERO.                    
085700     03  FILLER                  PIC X(3)  VALUE '9  '.                   
085800     03  FILLER                  PIC S9(5) VALUE ZERO.                    
085900     03  FILLER                  PIC X(3)  VALUE '10 '.                   
086000     03  FILLER                  PIC S9(5) VALUE ZERO.                    
086100     03  FILLER                  PIC X(3)  VALUE '11 '.                   
086200     03  FILLER                  PIC S9(5) VALUE ZERO.                    
086300     03  FILLER                  PIC X(3)  VALUE '12 '.                   
086400     03  FILLER                  PIC S9(5) VALUE ZERO.                    
086500     03  FILLER                  PIC X(3)  VALUE '13 '.                   
086600     03  FILLER                  PIC S9(5) VALUE ZERO.                    
086700     03  FILLER                  PIC X(3)  VALUE '14 '.                   
086800     03  FILLER                  PIC S9(5) VALUE ZERO.                    
086900     03  FILLER                  PIC X(3)  VALUE '15 '.                   
087000     03  FILLER                  PIC S9(5) VALUE ZERO.                    
087100     03  FILLER                  PIC X(3)  VALUE '16 '.                   
087200     03  FILLER                  PIC S9(5) VALUE ZERO.                    
087300     03  FILLER                  PIC X(3)  VALUE '17 '.                   
087400     03  FILLER                  PIC S9(5) VALUE ZERO.                    
087500     03  FILLER                  PIC X(3)  VALUE '18 '.                   
087600     03  FILLER                  PIC S9(5) VALUE ZERO.                    
087700     03  FILLER                  PIC X(3)  VALUE '19 '.                   
087800     03  FILLER                  PIC S9(5) VALUE ZERO.                    
087900     03  FILLER                  PIC X(3)  VALUE '20 '.                   
088000     03  FILLER                  PIC S9(5) VALUE ZERO.                    
088100     03  FILLER                  PIC X(3)  VALUE '21 '.                   
088200     03  FILLER                  PIC S9(5) VALUE ZERO.                    
088300     03  FILLER                  PIC X(3)  VALUE '22 '.                   
088400     03  FILLER                  PIC S9(5) VALUE ZERO.                    
088500     03  FILLER                  PIC X(3)  VALUE '23 '.                   
088600     03  FILLER                  PIC S9(5) VALUE ZERO.                    
088700     03  FILLER                  PIC X(3)  VALUE '24 '.                   
088800     03  FILLER                  PIC S9(5) VALUE ZERO.                    
088900     03  FILLER                  PIC X(3)  VALUE '25 '.                   
089000*                                                                         
089100 01  FILLER REDEFINES FOTNOT-TABELL-VARDEN.                               
089200     03  FOTNOT-TABELL OCCURS 25 INDEXED BY IX-FOT.                       
089300         06  IDFOTNR-FYSISKT     PIC S9(5).                               
089400         06  FOTNR-REDIGERAT     PIC X(3).                                
089500     EJECT                                                                
089600******************************************************************        
089700*                     LANDAREA                                            
089800******************************************************************        
089900     SKIP2                                                                
090000*01    -COPY WWLAND06                                                     
090100     EJECT                                                                
090200******************************************************************        
090300*                     WDATAREA                                            
090400******************************************************************        
090500     SKIP2                                                                
090600*01    -COPY WDATAREA                                                     
090700     EJECT                                                                
090800******************************************************************        
090900*             AREOR FÖR MFS OCH SKÄRMHANTERING                            
091000******************************************************************        
091100*                                                                         
091200 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
091300     SKIP3                                                                
091400*01    MID -COPY W1I51201 -PRE 1512- .                                    
091500     EJECT                                                                
091600*01    MID -COPY W1I51501 -PRE 1515- .                                    
091700     EJECT                                                                
091800*01    MID -COPY W1I51901.                                                
091900     EJECT                                                                
092000*01    -COPY WMSGAREA                                                     
092100     EJECT                                                                
092200*  03  MOD -COPY W1O51901  -RED MSG-AREA.                                 
092300     EJECT                                                                
092400*01    -COPY WMFSAREA                                                     
092500     EJECT                                                                
092600******************************************************************        
092700*             ARBETS-AREOR TILL IMS-SEKTIONERNA                           
092800******************************************************************        
092900*                                                                         
093000 01    IMS-WS.                                                            
093100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
093200     SKIP3                                                                
093300*                        **** STATUS-KOD FRÅN IMS                         
093400   03    STATUS-WS               PIC XX.                                  
093500     88    SEGMENT-FINNS                     VALUE '  '.                  
093600     88    SEGMENT-SAKNAS                    VALUE 'GE' 'GB'.             
093700     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
093800     SKIP3                                                                
093900   03    GODK-STATUSKODER.                                                
094000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
094100     SKIP3                                                                
094200 01    SSA1                      PIC X(64).                               
094300 01    SSA2                      PIC X(64).                               
094400 01    SSA3                      PIC X(64).                               
094500     EJECT                                                                
094600******************************************************************        
094700*                    IMS FUNKTIONSKODER                                   
094800******************************************************************        
094900*01    -COPY W0003                                                        
095000     EJECT                                                                
095100******************************************************************        
095200*                  DLI INPUT-OUTPUT AREA                                  
095300******************************************************************        
095400     SKIP2                                                                
095500 01  DLI-IO-AREA.                                                         
095600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-1'.             
095700     03        IO-AREA-1        PIC X(16) VALUE SPACE.                    
095800     SKIP3                                                                
095900*    03  WLKATH01 -COPY WDN501        -RED IO-AREA-1.                     
096000     EJECT                                                                
096100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-2'.             
096200     03        IO-AREA-2        PIC X(16) VALUE SPACE.                    
096300     SKIP3                                                                
096400*    03  WLKATH11 -COPY WDN511        -RED IO-AREA-2.                     
096500     EJECT                                                                
096600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-3'.             
096700     03        IO-AREA-3        PIC X(32) VALUE SPACE.                    
096800     SKIP3                                                                
096900*    03  WLKATH12 -COPY WDN512        -RED IO-AREA-3.                     
097000     EJECT                                                                
097100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-4'.             
097200     03        IO-AREA-4        PIC X(80) VALUE SPACE.                    
097300     SKIP3                                                                
097400*    03  WLKATH13 -COPY WDN513        -RED IO-AREA-4.                     
097500     EJECT                                                                
097600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-5'.             
097700     03        IO-AREA-5        PIC X(32) VALUE SPACE.                    
097800     SKIP3                                                                
097900*    03  WLKATH21 -COPY WDN521        -RED IO-AREA-5.                     
098000     EJECT                                                                
098100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-6'.             
098200     03        IO-AREA-6        PIC X(64) VALUE SPACE.                    
098300     SKIP3                                                                
098400*    03  WLKATH22 -COPY WDN522        -RED IO-AREA-6.                     
098500     EJECT                                                                
098600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-7'.             
098700     03        IO-AREA-7        PIC X(32) VALUE SPACE.                    
098800     SKIP3                                                                
098900*    03  WLKATH23 -COPY WDN523        -RED IO-AREA-7.                     
099000     EJECT                                                                
099100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-8'.             
099200     03        IO-AREA-8        PIC X(48) VALUE SPACE.                    
099300     SKIP3                                                                
099400*    03  WLKATH24 -COPY WDN524        -RED IO-AREA-8.                     
099500     EJECT                                                                
099600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-9'.             
099700     03        IO-AREA-9        PIC X(16) VALUE SPACE.                    
099800     SKIP3                                                                
099900*    03  WLKATH25 -COPY WDN525        -RED IO-AREA-9.                     
100000     EJECT                                                                
100100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-10'.            
100200     03        IO-AREA-10       PIC X(16) VALUE SPACE.                    
100300     SKIP3                                                                
100400*    03  WLKATH26 -COPY WDN526        -RED IO-AREA-10.                    
100500     EJECT                                                                
100600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-11'.            
100700     03        IO-AREA-11       PIC X(32) VALUE SPACE.                    
100800     SKIP3                                                                
100900*    03  WLKATH27 -COPY WDN527        -RED IO-AREA-11.                    
101000     EJECT                                                                
101100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-12'.            
101200     03        IO-AREA-12       PIC X(16) VALUE SPACE.                    
101300     SKIP3                                                                
101400*    03  WLKATF11 -COPY WDN311        -RED IO-AREA-12.                    
101500     EJECT                                                                
101600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-13'.            
101700     03        IO-AREA-13       PIC X(64) VALUE SPACE.                    
101800     SKIP3                                                                
101900*    03  WLKATF21 -COPY WDN321        -RED IO-AREA-13.                    
102000     EJECT                                                                
102100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-14'.            
102200     03        IO-AREA-14       PIC X(110) VALUE SPACE.                   
102300     SKIP3                                                                
102400*    03  WLARTC01 -COPY WDK601      -PRE ARTC01- -RED IO-AREA-14.         
102500     EJECT                                                                
102600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-15'.            
102700     03        IO-AREA-15       PIC X(900) VALUE SPACE.                   
102800     SKIP3                                                                
102900*    03  WLARTC11 -COPY WDK611      -PRE ARTC11- -RED IO-AREA-15.         
103000     EJECT                                                                
103100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-16'.            
103200     03        IO-AREA-16       PIC X(16) VALUE SPACE.                    
103300     SKIP3                                                                
103400*    03  WLKATB01 -COPY WDN201      -PRE RUB-   -RED IO-AREA-16.          
103500     EJECT                                                                
103600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-17'.            
103700     03        IO-AREA-17       PIC X(48) VALUE SPACE.                    
103800     SKIP3                                                                
103900*    03  WLKATB11 -COPY WDN211      -PRE RUB-   -RED IO-AREA-17.          
104000     EJECT                                                                
104100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-18'.            
104200     03        IO-AREA-18       PIC X(16) VALUE SPACE.                    
104300     SKIP3                                                                
104400*    03  WLKATD01 -COPY WDN401                  -RED IO-AREA-18.          
104500     EJECT                                                                
104600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-19'.            
104700     03        IO-AREA-19       PIC X(32) VALUE SPACE.                    
104800     SKIP3                                                                
104900*    03  WLKATD11 -COPY WDN411                  -RED IO-AREA-19.          
105000     EJECT                                                                
105100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-20'.            
105200     03        IO-AREA-20       PIC X(16) VALUE SPACE.                    
105300     SKIP3                                                                
105400*    03  WLBENA01 -COPY WDD301      -PRE BEN-   -RED IO-AREA-20.          
105500     EJECT                                                                
105600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-21'.            
105700     03        IO-AREA-21       PIC X(208) VALUE SPACE.                   
105800     SKIP3                                                                
105900*    03  WLBENA11 -COPY WDD311      -PRE BEN-   -RED IO-AREA-21.          
106000     EJECT                                                                
106100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-22'.            
106200     03        IO-AREA-22       PIC X(16) VALUE SPACE.                    
106300     SKIP3                                                                
106400*    03  WLBENA12 -COPY WDD312      -PRE BEN-   -RED IO-AREA-22.          
106500     EJECT                                                                
106600     03        FILLER           PIC X(16)  VALUE 'IO-AREA-23'.            
106700     03        IO-AREA-23       PIC X(64) VALUE SPACE.                    
106800     SKIP3                                                                
106900*    03  WLBENA13 -COPY WDD313      -PRE BEN-   -RED IO-AREA-23.          
107000     EJECT                                                                
107100     03        FILLER           PIC X(16)  VALUE 'IO-AREA-24'.            
107200     03        IO-AREA-24       PIC X(480) VALUE SPACE.                   
107300     SKIP3                                                                
107400*    03  WLKATM01 -COPY WDN101        -RED IO-AREA-24.                    
107500     EJECT                                                                
107600 LINKAGE SECTION.                                                         
107700*01  -COPY W0009          -PRE MSG-                                       
107800                                                                          
107900 01  ALT-PCB1                    PIC X(32).                               
108000     EJECT                                                                
108100*01  -COPY W0008          -PRE CAT-                                       
108200     05  FILLER                  PIC XX.                                  
108300                                                                          
108400*01  -COPY W0008          -PRE AVS-                                       
108500     05  FILLER                  PIC X.                                   
108600     EJECT                                                                
108700*01  -COPY W0008          -PRE ARTC-                                      
108800     05  FILLER                  PIC X.                                   
108900                                                                          
109000*01  -COPY W0008          -PRE RUB-                                       
109100     05  FILLER                  PIC X.                                   
109200     EJECT                                                                
109300*01  -COPY W0008          -PRE FOT-                                       
109400     05  FILLER                  PIC X.                                   
109500                                                                          
109600*01  -COPY W0008          -PRE TEXT-                                      
109700     05  FILLER                  PIC X.                                   
109800     EJECT                                                                
109900*01  -COPY W0008          -PRE BENAA-                                     
110000     05  FILLER                  PIC X.                                   
110100                                                                          
110200*01  -COPY W0008          -PRE BENAB-                                     
110300     05  FILLER                  PIC X.                                   
110400     EJECT                                                                
110500 PROCEDURE DIVISION USING MSG-PCB ALT-PCB1 CAT-PCB AVS-PCB                
110600           ARTC-PCB RUB-PCB FOT-PCB TEXT-PCB BENAA-PCB                    
110700           BENAB-PCB.                                                     
110800 STYR SECTION.                                                            
110900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB1 CAT-PCB AVS-PCB               
111000           ARTC-PCB RUB-PCB FOT-PCB TEXT-PCB BENAA-PCB                    
111100           BENAB-PCB.                                                     
111200                                                                          
111300     PERFORM IMS-GET-MSG                                                  
111400                                                                          
111500     IF SEGMENT-FINNS                                                     
111600       PERFORM A-INIT                                                     
111700       PERFORM AB-KOLLA-FLYTTA-INDATA                                     
111800       IF INDATA-OK                                                       
111900         IF SPRAAK-KOLL = NEJ                                             
112000           MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL                             
112100         ELSE                                                             
112200           IF FLSKRIV = JA                                                
112300             PERFORM B-BEARBETA-VADIS-INFO                                
112400             IF INDATA-OK                                                 
112500               PERFORM C-BEARBETA-RUBRIKER                                
112600                                                                          
112700               IF INDATA-OK                                               
112800                 IF MID-KDSVAR = '2'                                      
112900                   PERFORM D-BEARBETA-RADER                               
113000                                                                          
113100                   IF MAX-FOTNOT-OK                                       
113200                     IF IDFOTNR-FYSISKT(1) NOT = ZERO                     
113300                       PERFORM E-BEARBETA-FOTNOTER                        
113400                                                                          
113500                     END-IF                                               
113600                     MOVE MED-1(SPR-IX) TO MOD-TEMFSINF                   
113700                   END-IF                                                 
113800                 ELSE                                                     
113900                   MOVE MED-1(SPR-IX) TO MOD-TEMFSINF                     
114000                 END-IF                                                   
114100               END-IF                                                     
114200             END-IF                                                       
114300           ELSE                                                           
114400             MOVE FEL-5(SPR-IX) TO MOD-TEMFSFEL                           
114500           END-IF                                                         
114600         END-IF                                                           
114700       END-IF                                                             
114800                                                                          
114900       IF MAX-FOTNOT-OK                                                   
115000         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE W-IDPRTLST           
115100                             ALT-PCB1 DUMMY-AREA DUMMY-AREA               
115200         IF NOT ( 1512-IDTRANS AND MFS-PRINT )                            
115300           MOVE LENGTH OF MOD-W1O51901 TO MSG-KVLL                        
115400           ADD            +4           TO MSG-KVLL                        
115500         END-IF                                                           
115600       END-IF                                                             
115700                                                                          
115800       IF  1512-IDTRANS AND MFS-PRINT                                     
115900         MOVE MED-1(SPR-IX) TO MOD-TEMFSFEL                               
116000         MOVE  'W1O51201'   TO MFS-IDMOD                                  
116100         MOVE    '1512'     TO MOD-IDTRANS                                
116200         MOVE     +48       TO MSG-KVLL                                   
116300       END-IF                                                             
116400       PERFORM IMS-INSERT-MSG                                             
116500     END-IF                                                               
116600                                                                          
116700     MOVE ZERO TO RETURN-CODE                                             
116800     GOBACK                                                               
116900     .                                                                    
117000     EJECT                                                                
117100 A-INIT SECTION.                                                          
117200     SKIP2                                                                
117300     MOVE SPACE TO W-IDSKYLT                                              
117400                   SW-MAX-FOTNOT                                          
117500     MOVE NEJ   TO FLSKRIV                                                
117600     IF MSG-DUBBLA-TRANSKODER                                             
117700        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51901                
117800                                         1512-MID-W1I51201                
117900                                         1515-MID-W1I51501                
118000        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
118100        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
118200        MOVE MSG-IDPFK TO MFS-IDPFK                                       
118300     ELSE                                                                 
118400        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51901                 
118500                                        1512-MID-W1I51201                 
118600                                        1515-MID-W1I51501                 
118700        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
118800        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
118900        MOVE ' ' TO MFS-IDPFK                                             
119000     END-IF                                                               
119100                                                                          
119200     IF MID-IDCATNR-IN = ALL '+'                                          
119300        MOVE MID-IDCATNR-UT TO IDCATNR-WS                                 
119400        INSPECT IDCATNR-WS REPLACING ALL SPACE BY ZERO                    
119500     ELSE                                                                 
119600        MOVE MID-IDCATNR-IN TO IDCATNR-WS                                 
119700     END-IF                                                               
119800                                                                          
119900     IF MID-IDCATGRP-IN = ALL '+'                                         
120000        MOVE MID-IDCATGRP-UT TO IDCATGRP-WS                               
120100        INSPECT IDCATGRP-WS REPLACING ALL SPACE BY ZERO                   
120200     ELSE                                                                 
120300        MOVE MID-IDCATGRP-IN TO IDCATGRP-WS                               
120400     END-IF                                                               
120500                                                                          
120600     IF MID-IDCATAVS-IN = ALL '+'                                         
120700        MOVE MID-IDCATAVS-UT TO IDCATAVS-WS                               
120800        INSPECT IDCATAVS-WS REPLACING ALL SPACE BY ZERO                   
120900     ELSE                                                                 
121000        MOVE MID-IDCATAVS-IN TO IDCATAVS-WS                               
121100     END-IF                                                               
121200                                                                          
121300     IF MID-IDCATRAD-IN = ALL '+'                                         
121400        MOVE MID-IDCATRAD-UT TO IDCATRAD-WS                               
121500        INSPECT IDCATRAD-WS REPLACING ALL SPACE BY ZERO                   
121600                                      ALL '+'   BY ZERO                   
121700     ELSE                                                                 
121800        MOVE MID-IDCATRAD-IN TO IDCATRAD-WS                               
121900     END-IF                                                               
122000                                                                          
122100     IF MID-IDCATNR-IN = ALL '+'                                          
122200     AND MID-IDCATAVS-IN = ALL '+'                                        
122300     AND MID-IDCATGRP-IN = ALL '+'                                        
122400        IF MID-IDCATRAD-IN = ALL '+'                                      
122500           CONTINUE                                                       
122600        ELSE                                                              
122700           IF IDCATRAD-WS NUMERIC                                         
122800              IF KEY-IDCATRAD = ZERO OR < 19                              
122900                 MOVE +20 TO KEY-IDCATRAD                                 
123000              END-IF                                                      
123100           END-IF                                                         
123200        END-IF                                                            
123300     ELSE                                                                 
123400        IF MID-IDCATRAD-IN = ALL '+'                                      
123500           MOVE +20 TO KEY-IDCATRAD                                       
123600        ELSE                                                              
123700           IF IDCATRAD-WS NUMERIC                                         
123800              IF KEY-IDCATRAD = ZERO OR < 19                              
123900                 MOVE +20 TO KEY-IDCATRAD                                 
124000              END-IF                                                      
124100           ELSE                                                           
124200              MOVE +20 TO KEY-IDCATRAD                                    
124300           END-IF                                                         
124400        END-IF                                                            
124500     END-IF                                                               
124600                                                                          
124700     IF MID-IDSKYLT-IN = ALL '+'                                          
124800        MOVE MID-IDSKYLT-UT TO W-IDSKYLT IDSKYLT-WS                       
124900     ELSE                                                                 
125000        MOVE MID-IDSKYLT-IN TO W-IDSKYLT IDSKYLT-WS                       
125100     END-IF                                                               
125200                                                                          
125300     IF W-IDSKYLT = SPACE OR ALL '+'                                      
125400        MOVE 'S  ' TO W-IDSKYLT  IDSKYLT-WS                               
125500     END-IF                                                               
125600                                                                          
125700     MOVE MFS-IDTRANS TO  KONTROLL-IDTRANS                                
125800                                                                          
125900     IF EGEN-IDTRANS                                                      
126000       IF MID-KDCATPUB-R-FOM-F-IN = ALL '+'                               
126100          MOVE MID-KDCATPUB-R-FOM-F-UT                                    
126200                             TO WS-KDCATPUB-R-AVV                         
126300          PERFORM S50-Y2K-KDCATPUB-R                                      
126400          MOVE WS-KDCATPUB-AAAAVV                                         
126500                             TO KDCATPUB-FOM-WS                           
126600       ELSE                                                               
126700          MOVE MID-KDCATPUB-R-FOM-F-IN                                    
126800                             TO WS-KDCATPUB-R-AVV                         
126900          PERFORM S50-Y2K-KDCATPUB-R                                      
127000          MOVE WS-KDCATPUB-AAAAVV                                         
127100                             TO KDCATPUB-FOM-WS                           
127200       END-IF                                                             
127300       IF MID-KDCATPUB-R-FOM-T-IN = ALL '+'                               
127400          MOVE MID-KDCATPUB-R-FOM-T-UT                                    
127500                             TO WS-KDCATPUB-R-AVV                         
127600          PERFORM S50-Y2K-KDCATPUB-R                                      
127700          MOVE WS-KDCATPUB-AAAAVV                                         
127800                             TO KDCATPUB-TOM-WS                           
127900       ELSE                                                               
128000          MOVE MID-KDCATPUB-R-FOM-T-IN                                    
128100                             TO WS-KDCATPUB-R-AVV                         
128200          PERFORM S50-Y2K-KDCATPUB-R                                      
128300          MOVE WS-KDCATPUB-AAAAVV                                         
128400                             TO KDCATPUB-TOM-WS                           
128500       END-IF                                                             
128600     END-IF                                                               
128700     IF 1512-IDTRANS                                                      
128800       IF 1512-MID-KDCATPUB-R-MIN-IN = ALL '+'                            
128900          MOVE 1512-MID-KDCATPUB-R-MIN-UT                                 
129000                             TO WS-KDCATPUB-R-AVV                         
129100          PERFORM S50-Y2K-KDCATPUB-R                                      
129200          MOVE WS-KDCATPUB-AAAAVV                                         
129300                             TO KDCATPUB-FOM-WS                           
129400       ELSE                                                               
129500          MOVE 1512-MID-KDCATPUB-R-MIN-IN                                 
129600                             TO WS-KDCATPUB-R-AVV                         
129700          PERFORM S50-Y2K-KDCATPUB-R                                      
129800          MOVE WS-KDCATPUB-AAAAVV                                         
129900                             TO KDCATPUB-FOM-WS                           
130000       END-IF                                                             
130100       IF 1512-MID-KDCATPUB-R-MAX-IN = ALL '+'                            
130200          MOVE 1512-MID-KDCATPUB-R-MAX-UT                                 
130300                             TO WS-KDCATPUB-R-AVV                         
130400          PERFORM S50-Y2K-KDCATPUB-R                                      
130500          MOVE WS-KDCATPUB-AAAAVV                                         
130600                             TO KDCATPUB-TOM-WS                           
130700       ELSE                                                               
130800          MOVE 1512-MID-KDCATPUB-R-MAX-IN                                 
130900                             TO WS-KDCATPUB-R-AVV                         
131000          PERFORM S50-Y2K-KDCATPUB-R                                      
131100          MOVE WS-KDCATPUB-AAAAVV                                         
131200                             TO KDCATPUB-TOM-WS                           
131300       END-IF                                                             
131400     END-IF                                                               
131500     IF 1515-IDTRANS                                                      
131600       IF 1515-MID-KDCATPUB-R-FOM-IN = ALL '+'                            
131700          MOVE 1515-MID-KDCATPUB-R-FOM-UT                                 
131800                             TO WS-KDCATPUB-R-AVV                         
131900          PERFORM S50-Y2K-KDCATPUB-R                                      
132000          MOVE WS-KDCATPUB-AAAAVV                                         
132100                             TO KDCATPUB-FOM-WS                           
132200       ELSE                                                               
132300          MOVE 1515-MID-KDCATPUB-R-FOM-IN                                 
132400                             TO WS-KDCATPUB-R-AVV                         
132500          PERFORM S50-Y2K-KDCATPUB-R                                      
132600          MOVE WS-KDCATPUB-AAAAVV                                         
132700                             TO KDCATPUB-FOM-WS                           
132800       END-IF                                                             
132900       IF 1515-MID-KDCATPUB-R-TOM = ALL '+'                               
133000          MOVE       SPACES             TO KDCATPUB-TOM-WS                
133100       ELSE                                                               
133200          MOVE 1515-MID-KDCATPUB-R-TOM                                    
133300                             TO WS-KDCATPUB-R-AVV                         
133400          PERFORM S50-Y2K-KDCATPUB-R                                      
133500          MOVE WS-KDCATPUB-AAAAVV                                         
133600                             TO KDCATPUB-TOM-WS                           
133700       END-IF                                                             
133800     END-IF                                                               
133900                                                                          
134000     SET WWLAND06-IX TO +1                                                
134100     SEARCH WWLAND06-IDSKYLT-RAD                                          
134200                 AT END MOVE NEJ TO SPRAAK-KOLL                           
134300        WHEN WWLAND06-IDSKYLT(WWLAND06-IX) = W-IDSKYLT                    
134400        AND WWLAND06-IDSKYLT(WWLAND06-IX) NOT = 'KOR'                     
134500        AND WWLAND06-IDSKYLT(WWLAND06-IX) NOT = 'RC '                     
134600        AND WWLAND06-IDSKYLT(WWLAND06-IX) NOT = 'RUS'                     
134700        AND WWLAND06-IDSKYLT(WWLAND06-IX) NOT = 'T  '                     
134800*       --- IDSKYLT 'MAL' OCH 'TR ' ÄR GODKÄNDA                           
134900           MOVE JA TO SPRAAK-KOLL                                         
135000     END-SEARCH                                                           
135100                                                                          
135200                                                                          
135300                                                                          
135400     IF EGEN-IDTRANS AND MFS-PRINT                                        
135500       MOVE   JA  TO FLSKRIV                                              
135600     ELSE                                                                 
135700       IF NOT EGEN-IDTRANS                                                
135800         MOVE  'A'  TO MID-KDPRTVAL                                       
135900         MOVE  '2'  TO MID-KDSVAR                                         
136000       END-IF                                                             
136100       IF 1532-IDTRANS AND MFS-PRINT                                      
136200         MOVE   JA   TO FLSKRIV                                           
136300         MOVE   '1'  TO MID-KDSVAR                                        
136400       ELSE                                                               
136500         IF (1515-IDTRANS AND MFS-PRINT)                                  
136600         OR (1512-IDTRANS AND MFS-PRINT)                                  
136700           MOVE   JA   TO FLSKRIV                                         
136800         END-IF                                                           
136900       END-IF                                                             
137000     END-IF                                                               
137100                                                                          
137200     MOVE LOW-VALUE TO MSG-AREA                                           
137300     MOVE 'W1O51901' TO MFS-IDMOD                                         
137400     MOVE '1519' TO MOD-IDTRANS                                           
137500                                                                          
137600                                                                          
137700     IF ENGLISH-TEXT                                                      
137800        MOVE +2 TO SPR-IX                                                 
137900     ELSE                                                                 
138000        MOVE +1 TO SPR-IX                                                 
138100     END-IF                                                               
138200                                                                          
138300     MOVE IDCATNR-WS TO MOD-IDCATNR-UT                                    
138400     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
138500                                                                          
138600     IF IDCATGRP-WS = ALL '+' MOVE ZERO TO KEY-IDCATGRP END-IF            
138700*                    * OVAN ÄR STARTVÄRDE VID HOPP FRÅN 1532              
138800     MOVE IDCATGRP-WS TO MOD-IDCATGRP-UT                                  
138900     INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE              
139000                                                                          
139100     IF IDCATAVS-WS = ALL '+' MOVE ZERO TO KEY-IDCATAVS END-IF            
139200*                    * OVAN ÄR STARTVÄRDE VID HOPP FRÅN 1532              
139300     MOVE IDCATAVS-WS TO MOD-IDCATAVS-UT                                  
139400     INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE              
139500                                                                          
139600     MOVE W-IDSKYLT  TO MOD-IDSKYLT-UT                                    
139700                                                                          
139800     MOVE IDCATRAD-WS TO MOD-IDCATRAD-UT                                  
139900     INSPECT MOD-IDCATRAD-UT REPLACING LEADING ZERO BY SPACE              
140000                                                                          
140100     MOVE KDCATPUB-FOM-WS (4:3)                                           
140200                          TO MOD-KDCATPUB-R-FOM-F-UT                      
140300     INSPECT MOD-KDCATPUB-R-FOM-F-UT                                      
140400                                   REPLACING LEADING ZERO BY SPACE        
140500                                                                          
140600     IF KDCATPUB-FOM-WS = SPACE                                           
140700        MOVE LOW-VALUE    TO KDCATPUB-FOM-WS                              
140800     END-IF                                                               
140900                                                                          
141000     MOVE KDCATPUB-TOM-WS (4:3)                                           
141100                          TO MOD-KDCATPUB-R-FOM-T-UT                      
141200     INSPECT MOD-KDCATPUB-R-FOM-T-UT                                      
141300                                   REPLACING LEADING ZERO BY SPACE        
141400                                                                          
141500     IF KDCATPUB-TOM-WS = SPACE                                           
141600        MOVE HIGH-VALUE   TO KDCATPUB-TOM-WS                              
141700     END-IF                                                               
141800                                                                          
141900     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
142000                             MOD-IDCATGRP-IN                              
142100                             MOD-IDCATAVS-IN                              
142200                             MOD-IDCATRAD-IN                              
142300                             MOD-IDSKYLT-IN                               
142400                             MOD-KDCATPUB-R-FOM-F-IN                      
142500                             MOD-KDCATPUB-R-FOM-T-IN                      
142600                             MOD-TEMFSFEL                                 
142700                             MOD-TEMFSINF                                 
142800                                                                          
142900     ACCEPT DAGENS-DATUM FROM DATE                                        
143000                                                                          
143100     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
143200     MOVE DAGENS-DATUM                                                    
143300                   TO DAT-I-TIDATUM                                       
143400                                                                          
143500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
143600                     DAT-O-TIDATUM DAT-KDSVAR                             
143700                                                                          
143800     IF DAT-KDSVAR-OK                                                     
143900****             HÄMTA SEKELSIFFROR                                       
144000                                                                          
144100       MOVE DAT-TISEKEL    TO DAGENS-AAR(1:2)                             
144200                                                                          
144300     ELSE                                                                 
144400         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
144500         DELIMITED BY SIZE INTO FELTEXT                                   
144600         CALL FELLOG                                                      
144700     END-IF                                                               
144800                                                                          
144900     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAR(3:2)                           
145000                                                                          
145100                                                                          
145200     COMPUTE WS-TIAAAA(1) = DAGENS-AAR - 1                                
145300     COMPUTE WS-TIAAAA(2) = DAGENS-AAR                                    
145400     COMPUTE WS-TIAAAA(3) = DAGENS-AAR + 1                                
145500     COMPUTE WS-TIAAAA(4) = DAGENS-AAR + 2                                
145600     .                                                                    
145700     EJECT                                                                
145800 AB-KOLLA-FLYTTA-INDATA SECTION.                                          
145900     SKIP2                                                                
146000     MOVE JA TO INDATA-SW                                                 
146100     IF (IDCATNR-WS NOT NUMERIC)                                          
146200     OR (IDCATGRP-WS NOT NUMERIC)                                         
146300     OR (IDCATAVS-WS NOT NUMERIC)                                         
146400     OR (IDCATRAD-WS NOT NUMERIC)                                         
146500        MOVE FEL-1 (SPR-IX) TO MOD-TEMFSFEL                               
146600        MOVE NEJ TO INDATA-SW                                             
146700     ELSE                                                                 
146800                                                                          
146900        IF KDCATPUB-FOM-WS NOT = LOW-VALUE                                
147000          PERFORM I-KONTR-PUBKOD-FROM                                     
147100        END-IF                                                            
147200                                                                          
147300        IF KDCATPUB-TOM-WS NOT = HIGH-VALUE                               
147400          PERFORM J-KONTR-PUBKOD-TOM                                      
147500        END-IF                                                            
147600                                                                          
147700        IF KDCATPUB-FOM-WS NOT = LOW-VALUE                                
147800        AND KDCATPUB-TOM-WS NOT = HIGH-VALUE                              
147900                                                                          
148000******** KONTROLLERA ATT PUBKOD FROM INTE ÄR STÖRRE ÄN PUBKOD TOM         
148100          IF KDCATPUB-TOM-WS > KDCATPUB-FOM-WS                            
148200               MOVE NEJ TO INDATA-SW                                      
148300          END-IF                                                          
148400                                                                          
148500        END-IF                                                            
148600                                                                          
148700        IF INDATA-OK                                                      
148800          IF MID-KDPRTVAL = '+' OR SPACE                                  
148900*                           * DEFAULT-VÄRDE                               
149000             MOVE 'A'   TO MID-KDPRTVAL MOD-KDPRTVAL                      
149100          END-IF                                                          
149200                                                                          
149300          IF MID-KDPRTVAL NOT = 'A' AND 'B' AND 'C'                       
149400*                           * ALLA GODKÄNDA PRINTERVAL                    
149500             MOVE MFS-ROER-EJ-FAELT TO  MOD-KDPRTVAL                      
149600                                        MOD-KDSVAR                        
149700             MOVE FEL-7 (SPR-IX) TO MOD-TEMFSFEL                          
149800             MOVE NEJ TO INDATA-SW                                        
149900          ELSE                                                            
150000             MOVE MID-KDPRTVAL TO MOD-KDPRTVAL                            
150100             EVALUATE MID-KDPRTVAL                                        
150200                WHEN 'A'                                                  
150300                   MOVE KAT-AVSNITT-PV-A TO W-IDPRTLST                    
150400                WHEN 'B'                                                  
150500                   MOVE KAT-AVSNITT-PV-B TO W-IDPRTLST                    
150600                WHEN 'C'                                                  
150700                   MOVE KAT-AVSNITT-IT-C TO W-IDPRTLST                    
150800                WHEN OTHER                                                
150900                   CONTINUE                                               
151000             END-EVALUATE                                                 
151100*          ***************************************************            
151200*          *  HÄMTAR VALD PRINTERS LOGISKA NAMN TILL TEMFSINF*            
151300*          ***************************************************            
151400             MOVE 1              TO PRT-KDCALL                            
151500             MOVE W-IDPRTLST     TO PRT-IDPRTLST                          
151600             CALL W006PRT  USING PRT-W006PRT                              
151700             MOVE PRT-IDLTERM    TO MOD-IDLTERM                           
151800             IF PRT-IDLTERM = 'SAKNAS  '                                  
151900               MOVE NEJ TO INDATA-SW                                      
152000               MOVE 'FEL I PRINTERDEFINITION. KONTAKTA SYSTEMAVD'         
152100                                 TO MED-1(1)                              
152200               MOVE 'ERROR IN PRINTER DEF. CONTACT SYSTEM SUPPORT'        
152300                                 TO MED-1(2)                              
152400             ELSE                                                         
152500               MOVE PRT-IDLTERM  TO MED-1S-IDLTERM                        
152600                                    MED-1E-IDLTERM                        
152700               CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN                 
152800                                   W-IDPRTLST                             
152900                                   ALT-PCB1 DUMMY-AREA DUMMY-AREA         
153000                                                                          
153100             END-IF                                                       
153200          END-IF                                                          
153300                                                                          
153400          IF INDATA-OK                                                    
153500            IF MID-KDSVAR = '1' OR '2'                                    
153600*                             * ALLA GODKÄNDA OMFATTNINGSVAL              
153700              MOVE MID-KDSVAR TO MOD-KDSVAR                               
153800            ELSE                                                          
153900              IF MID-KDSVAR = SPACE                                       
154000                MOVE '2'    TO MID-KDSVAR MOD-KDSVAR                      
154100              ELSE                                                        
154200                MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR                      
154300                MOVE FEL-8 (SPR-IX) TO MOD-TEMFSFEL                       
154400                MOVE NEJ TO INDATA-SW                                     
154500              END-IF                                                      
154600            END-IF                                                        
154700          END-IF                                                          
154800        END-IF                                                            
154900     END-IF                                                               
155000     .                                                                    
155100     EJECT                                                                
155200 B-BEARBETA-VADIS-INFO    SECTION.                                        
155300     SKIP2                                                                
155400     MOVE KEY-IDCATNR  TO W-IDCATNR                                       
155500                          W-IDCATNR-1                                     
155600     MOVE KEY-IDCATGRP TO W-IDCATGRP                                      
155700     MOVE KEY-IDCATAVS TO W-IDCATAVS                                      
155800                                                                          
155900     PERFORM IMS-GU-KAT                                                   
156000     IF SEGMENT-SAKNAS                                                    
156100        MOVE FEL-3 (SPR-IX) TO MOD-TEMFSFEL                               
156200        MOVE NEJ TO INDATA-SW                                             
156300     END-IF                                                               
156400                                                                          
156500     IF INDATA-OK                                                         
156600       PERFORM IMS-GU-AVS                                                 
156700       IF SEGMENT-SAKNAS                                                  
156800         MOVE FEL-3 (SPR-IX) TO MOD-TEMFSFEL                              
156900         MOVE NEJ TO INDATA-SW                                            
157000       ELSE                                                               
157100         MOVE AVS-FLAVSTVAD  TO WS-FLAVSTVAD                              
157200*                         * SPARA FLAGGAN FÖR VADIS-GENERERING            
157300*                                                                         
157400         MOVE AVS-IDCATNR    TO L-IDCATNR-S                               
157500                                L-IDCATNR-E                               
157600         IF 1532-IDTRANS                                                  
157700           MOVE    JA        TO WS-FLAVSTVAD                              
157800           MOVE   ZERO       TO L-IDCATGRP-S                              
157900                                L-IDCATAVS-S                              
158000                                L-IDCATGRP-E                              
158100                                L-IDCATAVS-E                              
158200*                         * FINNS EJ GRP/AVS PÅ 1532                      
158300         ELSE                                                             
158400           MOVE AVS-IDCATGRP TO L-IDCATGRP-S                              
158500                                L-IDCATGRP-E                              
158600           MOVE AVS-IDCATAVS TO L-IDCATAVS-S                              
158700                                L-IDCATAVS-E                              
158800         END-IF                                                           
158900         MOVE DAGENS-DATUM   TO L-DATUM-S                                 
159000                                L-DATUM-E                                 
159100         IF WS-FLAVSTVAD = JA                                             
159200*                     * AVSNITTET SKALL MED TILL VADIS                    
159300           MOVE V-RUB-TXT-S TO L-TEXT-RUB-S                               
159400           MOVE V-RUB-TXT-E TO L-TEXT-RUB-E                               
159500           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST         
159600                               ALT-PCB1 PRT-NYSIDA-RAD4                   
159700                               HUVUD-TEXT(SPR-IX)                         
159800           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST         
159900                               ALT-PCB1 PRT-AFTER-2                       
160000                               VAD-RUB-1H(SPR-IX)                         
160100           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST         
160200                               ALT-PCB1 PRT-AFTER-2                       
160300                               VADIS-KOL-RUB1(SPR-IX)                     
160400           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST         
160500                               ALT-PCB1 PRT-AFTER-1                       
160600                               VADIS-KOL-RUB2(SPR-IX)                     
160700           MOVE +9 TO RADRAKNARE                                          
160800                                                                          
160900*                                                                         
161000           MOVE +1 TO VADIX                                               
161100           PERFORM UNTIL KAT-IDMODELL (VADIX) = SPACE                     
161200                   OR VADIX > +8                                          
161300             PERFORM BA-FYLL-VADIS-KATALOGRAD                             
161400             PERFORM BC-SKRIV-VADISRAD                                    
161500             ADD +1 TO VADIX                                              
161600           END-PERFORM                                                    
161700*                                                                         
161800           IF 1532-IDTRANS                                                
161900*            * VID PF4 FRÅN 1532 SKALL MAN BARA PRINTA KATALOGDATA        
162000             SET  INDATA-FEL    TO TRUE                                   
162100             MOVE MED-1(SPR-IX) TO MOD-TEMFSINF                           
162200*            * FÖR ATT EJ GÅ IN C- SECTION                                
162300           ELSE                                                           
162400             MOVE ZERO               TO SPAR-IDRADN                       
162500             PERFORM IMS-GNP-AVS-VADIS                                    
162600             PERFORM H-LAES-AVSNITT-VADIS                                 
162700             IF SEGMENT-FINNS                                             
162800               CALL W006PRS1 USING PRT-SPOOL-OVR                          
162900                                   PRT-WRITE W-IDPRTLST                   
163000                                   ALT-PCB1 PRT-AFTER-2                   
163100                                   VAD-RUB-2H(SPR-IX)                     
163200               CALL W006PRS1 USING PRT-SPOOL-OVR                          
163300                                   PRT-WRITE W-IDPRTLST                   
163400                                   ALT-PCB1 PRT-AFTER-2                   
163500                                   VADIS-KOL-RUB1(SPR-IX)                 
163600               CALL W006PRS1 USING PRT-SPOOL-OVR                          
163700                                   PRT-WRITE W-IDPRTLST                   
163800                                   ALT-PCB1 PRT-AFTER-1                   
163900                                   VADIS-KOL-RUB2(SPR-IX)                 
164000               ADD +5 TO RADRAKNARE                                       
164100             END-IF                                                       
164200             PERFORM UNTIL SEGMENT-SAKNAS                                 
164300               PERFORM BB-FYLL-VADIS-AVSNITTSRAD                          
164400               PERFORM BC-SKRIV-VADISRAD                                  
164500               PERFORM IMS-GNP-AVS-VADIS                                  
164600               PERFORM H-LAES-AVSNITT-VADIS                               
164700             END-PERFORM                                                  
164800           END-IF                                                         
164900         END-IF                                                           
165000       END-IF                                                             
165100     END-IF                                                               
165200     .                                                                    
165300     EJECT                                                                
165400 BA-FYLL-VADIS-KATALOGRAD   SECTION.                                      
165500     SKIP2                                                                
165600     MOVE '-'                      TO L-IDKOL                             
165700     MOVE SPACE                    TO L-FLEXCL                            
165800     MOVE KAT-IDMODELL     (VADIX) TO L-IDMODELL                          
165900     MOVE KAT-TIMODAAR-STA (VADIX) TO L-TIMODAAR-STA                      
166000     MOVE KAT-TIMODAAR-STO (VADIX) TO L-TIMODAAR-STO                      
166100     MOVE KAT-IDVARIANT    (VADIX) TO L-IDVARIANT                         
166200     MOVE ZEROES                   TO L-KDCHATYP                          
166300                                      L-IDCHASSI-STA                      
166400                                      L-IDCHASSI-STO                      
166500     .                                                                    
166600     EJECT                                                                
166700 BB-FYLL-VADIS-AVSNITTSRAD  SECTION.                                      
166800     SKIP2                                                                
166900     MOVE VADIS-KDCATPUB-FOM (4:3)                                        
167000                              TO L-KDCATPUB-R-FOM-VADIS                   
167100     MOVE VADIS-KDCATPUB-TOM (4:3)                                        
167200                              TO L-KDCATPUB-R-TOM-VADIS                   
167300     IF VADIS-IDKOL = SPACE                                               
167400       MOVE '-'               TO L-IDKOL                                  
167500     ELSE                                                                 
167600       MOVE VADIS-IDKOL       TO L-IDKOL                                  
167700     END-IF                                                               
167800                                                                          
167900     IF VADIS-FLEXCL = JA                                                 
168000       MOVE 'X'               TO L-FLEXCL                                 
168100     ELSE                                                                 
168200       MOVE SPACE             TO L-FLEXCL                                 
168300     END-IF                                                               
168400                                                                          
168500     MOVE VADIS-IDMODELL      TO L-IDMODELL                               
168600                                                                          
168700     MOVE VADIS-TIMODAAR-STA  TO L-TIMODAAR-STA                           
168800     MOVE VADIS-TIMODAAR-STO  TO L-TIMODAAR-STO                           
168900                                                                          
169000     MOVE VADIS-IDVARIANT     TO L-IDVARIANT                              
169100     MOVE VADIS-IDVARIANT-2   TO L-IDVARIANT-2                            
169200                                                                          
169300     MOVE VADIS-KDCHATYP      TO L-KDCHATYP                               
169400                                                                          
169500     MOVE VADIS-IDCHASSI-STA  TO L-IDCHASSI-STA                           
169600     MOVE VADIS-IDCHASSI-STO  TO L-IDCHASSI-STO                           
169700     .                                                                    
169800     EJECT                                                                
169900 BC-SKRIV-VADISRAD SECTION.                                               
170000     SKIP2                                                                
170100     IF RADRAKNARE <  MAX-ANTAL-RADER                                     
170200        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
170300                            ALT-PCB1 PRT-AFTER-1 VADIS-RAD                
170400        ADD +1 TO RADRAKNARE                                              
170500     ELSE                                                                 
170600        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
170700                            ALT-PCB1 PRT-NYSIDA-RAD4                      
170800                            HUVUD-TEXT(SPR-IX)                            
170900        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
171000                            ALT-PCB1 PRT-AFTER-2                          
171100                            VAD-RUB-2H(SPR-IX)                            
171200        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
171300                            ALT-PCB1 PRT-AFTER-2                          
171400                            VADIS-KOL-RUB1(SPR-IX)                        
171500        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
171600                            ALT-PCB1 PRT-AFTER-1                          
171700                            VADIS-KOL-RUB2(SPR-IX)                        
171800        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
171900                            ALT-PCB1 PRT-AFTER-1                          
172000                            VADIS-RAD                                     
172100        MOVE +10 TO RADRAKNARE                                            
172200     END-IF                                                               
172300                                                                          
172400     .                                                                    
172500     EJECT                                                                
172600 C-BEARBETA-RUBRIKER SECTION.                                             
172700     SKIP2                                                                
172800     PERFORM IMS-GU-AVS                                                   
172900     MOVE AVS-IDVERS   TO L-IDVERS                                        
173000     IF SPR-IX = +2                                                       
173100        MOVE 'EDITION' TO L-UTGAVA-TEXT                                   
173200     END-IF                                                               
173300                                                                          
173400     MOVE MOD-KDCATPUB-R-FOM-F-UT                                         
173500                             TO WS-RUB-PUB-FROM                           
173600     MOVE MOD-KDCATPUB-R-FOM-T-UT                                         
173700                             TO WS-RUB-PUB-TOM                            
173800     MOVE WS-RUBRIK-RAD      TO L-TEXT-RUB-S                              
173900                                L-TEXT-RUB-E                              
174000                                                                          
174100     PERFORM CB-LAS-RUBRIKNR                                              
174200     PERFORM CC-LAS-RUBRIKTEXT                                            
174300     PERFORM CD-LAS-KOLUMNTEXT                                            
174400                                                                          
174500     MOVE +1                 TO IND2                                      
174600                                                                          
174700     PERFORM UNTIL IND2 > IND2-MAX                                        
174800                                                                          
174900       MOVE W-TEKOL-RED(1, IND2)   TO L-TEKOL-A                           
175000       MOVE W-TEKOL-RED(2, IND2)   TO L-TEKOL-B                           
175100       MOVE W-TEKOL-RED(3, IND2)   TO L-TEKOL-C                           
175200       MOVE W-TEKOL-RED(4, IND2)   TO L-TEKOL-D                           
175300       MOVE W-TEKOL-RED(5, IND2)   TO L-TEKOL-E                           
175400       MOVE WS-BERUBTEXT(1, IND2)  TO L-BERUBTEXT-R1                      
175500       MOVE WS-BERUBTEXT(2, IND2)  TO L-BERUBTEXT-R2                      
175600       MOVE WS-BERUBTEXT(3, IND2)  TO L-BERUBTEXT-R3                      
175700       MOVE WS-BERUBTEXT(4, IND2)  TO L-BERUBTEXT-R4                      
175800       MOVE WS-IDILLU(IND2)        TO L-IDILLU                            
175900       MOVE WS-KDCATPUB-R-FOM-RUB(IND2)                                   
176000                                   TO L-KDCATPUB-R-FOM-RUB                
176100       MOVE WS-KDCATPUB-R-TOM-RUB(IND2)                                   
176200                                   TO L-KDCATPUB-R-TOM-RUB                
176300                                                                          
176400       IF  ( RADRAKNARE < 32 )                                            
176500       AND ( MID-KDSVAR = '1' )                                           
176600       AND ( WS-FLAVSTVAD = JA )                                          
176700         IF IND2 = 1                                                      
176800           PERFORM CE-SKRIV-RUBRIKER-NY-SIDA                              
176900           MOVE +4  TO RADRAKNARE                                         
177000         END-IF                                                           
177100                                                                          
177200         PERFORM CF-SKRIV-RUBRIK-RADER                                    
177300         ADD +6     TO RADRAKNARE                                         
177400                                                                          
177500         IF IND2 = IND2-MAX                                               
177600           PERFORM CG-SKRIV-TEXT-RAD                                      
177700           ADD +2   TO RADRAKNARE                                         
177800         END-IF                                                           
177900       ELSE                                                               
178000         IF IND2 = 1                                                      
178100*                    * RUBRIKERNA MÅSTE SKRIVAS PÅ NY SIDA                
178200                                                                          
178300           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST         
178400                               ALT-PCB1 PRT-NYSIDA-RAD4                   
178500                               HUVUD-TEXT(SPR-IX)                         
178600           CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST         
178700                               ALT-PCB1 PRT-AFTER-1 STRECKRAD-2           
178800           MOVE +4  TO RADRAKNARE                                         
178900         END-IF                                                           
179000                                                                          
179100         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST           
179200                             ALT-PCB1 PRT-AFTER-1 RUB-RAD-1               
179300         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST           
179400                             ALT-PCB1 PRT-AFTER-1 RUB-RAD-2               
179500         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST           
179600                             ALT-PCB1 PRT-AFTER-1 RUB-RAD-3               
179700         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST           
179800                             ALT-PCB1 PRT-AFTER-1 RUB-RAD-4               
179900         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST           
180000                             ALT-PCB1 PRT-AFTER-1 RUB-RAD-5               
180100         CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST           
180200                             ALT-PCB1 PRT-AFTER-1 STRECKRAD-2             
180300         ADD +6     TO RADRAKNARE                                         
180400                                                                          
180500         IF IND2 = IND2-MAX                                               
180600           IF MID-KDSVAR = '2'                                            
180700             CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                  
180800                                 W-IDPRTLST ALT-PCB1 PRT-AFTER-1          
180900                                 TEXT-HUVUD-RAD(SPR-IX)                   
181000           ELSE                                                           
181100*                    * MAN HAR VALT ATT INTE PRINTA ARTIKELRAD            
181200             CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE                  
181300                                 W-IDPRTLST ALT-PCB1 PRT-AFTER-1          
181400                                 TEXT-SLUT-RAD(SPR-IX)                    
181500           END-IF                                                         
181600           ADD +2   TO RADRAKNARE                                         
181700         END-IF                                                           
181800       END-IF                                                             
181900       ADD +1                TO IND2                                      
182000     END-PERFORM                                                          
182100     .                                                                    
182200     EJECT                                                                
182300 CB-LAS-RUBRIKNR SECTION.                                                 
182400     SKIP2                                                                
182500     MOVE SPACE TO L-BERUBTEXT-R1                                         
182600                   L-BERUBTEXT-R2                                         
182700                   L-BERUBTEXT-R3                                         
182800                   L-BERUBTEXT-R4                                         
182900     MOVE +1 TO IND2                                                      
183000                IND2-MAX                                                  
183100     MOVE +1 TO W-IDCATRAD                                                
183200                SPAR-IDCATRAD                                             
183300     MOVE LOW-VALUE TO W-KDCATPUB-512                                     
183400                                                                          
183500     PERFORM IMS-GU-AVS                                                   
183600     PERFORM IMS-GNP-AVS-RAD-FIRST                                        
183700     MOVE RAD-IDCATRAD       TO W-IDCATRAD                                
183800     MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                            
183900     PERFORM G-LAES-AVSNITT-RAD                                           
184000                                                                          
184100     PERFORM UNTIL SEGMENT-SAKNAS                                         
184200     OR RAD-IDCATRAD NOT = SPAR-IDCATRAD                                  
184300                                                                          
184400        MOVE ZERO               TO W-IDILLU                               
184500        MOVE LOW-VALUE          TO W-KDCATPUB-511                         
184600        PERFORM IMS-GET-FIRST-AVS-ILLU                                    
184700                                                                          
184800        PERFORM UNTIL NOT SEGMENT-FINNS                                   
184900        OR RAD-KDCATPUB-FOM = ILLU-KDCATPUB-FOM                           
185000          PERFORM IMS-GET-NEXT-AVS-ILLU                                   
185100        END-PERFORM                                                       
185200                                                                          
185300        IF SEGMENT-FINNS                                                  
185400          MOVE ILLU-IDILLU        TO WS-IDILLU(IND2)                      
185500        END-IF                                                            
185600                                                                          
185700        MOVE SPACE           TO W-BERUBTEXT(1)                            
185800                                W-BERUBTEXT(2)                            
185900                                W-BERUBTEXT(3)                            
186000                                                                          
186100        PERFORM CBA-LAS-RUBRIKNR                                          
186200                                                                          
186300        IF IND2 > IND2-MAX                                                
186400           MOVE IND2    TO IND2-MAX                                       
186500        END-IF                                                            
186600        ADD +1          TO IND2                                           
186700        PERFORM IMS-GU-AVS                                                
186800        PERFORM IMS-GNP-AVS-RAD-NEXT                                      
186900        MOVE RAD-IDCATRAD       TO W-IDCATRAD                             
187000        MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                         
187100        PERFORM G-LAES-AVSNITT-RAD                                        
187200     END-PERFORM                                                          
187300     .                                                                    
187400     EJECT                                                                
187500 CBA-LAS-RUBRIKNR SECTION.                                                
187600     SKIP2                                                                
187700                                                                          
187800     MOVE RAD-KDCATPUB-FOM (4:3)                                          
187900                           TO WS-KDCATPUB-R-FOM-RUB(IND2)                 
188000     MOVE RAD-KDCATPUB-TOM (4:3)                                          
188100                           TO WS-KDCATPUB-R-TOM-RUB(IND2)                 
188200                                                                          
188300     PERFORM IMS-GNP-AVS-RUB                                              
188400                                                                          
188500     IF SEGMENT-FINNS                                                     
188600        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
188700        PERFORM IMS-GU-RUB                                                
188800                                                                          
188900        IF SEGMENT-FINNS                                                  
189000           IF RUB-RUB-FLKOMBINERAS = JA                                   
189100              PERFORM IMS-GNP-RUB-TEXT                                    
189200                                                                          
189300              IF SEGMENT-FINNS                                            
189400                 MOVE RUB-TEXT-BERUBTXT TO                                
189500                                W-BERUBTEXT(RUB-IDSEGMNR)                 
189600              END-IF                                                      
189700           ELSE                                                           
189800              PERFORM IMS-GNP-RUB-TEXT                                    
189900              IF SEGMENT-FINNS                                            
190000                 MOVE RUB-TEXT-BERUBTXT TO W-BERUBTEXT(1)                 
190100              END-IF                                                      
190200              PERFORM IMS-GNP-RUB-TEXT                                    
190300              IF SEGMENT-FINNS                                            
190400                 MOVE RUB-TEXT-BERUBTXT TO W-BERUBTEXT(2)                 
190500              END-IF                                                      
190600              PERFORM IMS-GNP-RUB-TEXT                                    
190700              IF SEGMENT-FINNS                                            
190800                 MOVE RUB-TEXT-BERUBTXT TO W-BERUBTEXT(3)                 
190900              END-IF                                                      
191000           END-IF                                                         
191100        END-IF                                                            
191200     END-IF                                                               
191300     PERFORM IMS-GNP-AVS-RUB                                              
191400     IF SEGMENT-FINNS                                                     
191500        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
191600        PERFORM IMS-GU-RUB                                                
191700                                                                          
191800        IF SEGMENT-FINNS                                                  
191900           PERFORM IMS-GNP-RUB-TEXT                                       
192000                                                                          
192100           IF SEGMENT-FINNS                                               
192200              MOVE RUB-TEXT-BERUBTXT TO                                   
192300                          W-BERUBTEXT(RUB-IDSEGMNR)                       
192400           END-IF                                                         
192500        END-IF                                                            
192600     END-IF                                                               
192700     PERFORM IMS-GNP-AVS-RUB                                              
192800     IF SEGMENT-FINNS                                                     
192900        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
193000        PERFORM IMS-GU-RUB                                                
193100        IF SEGMENT-FINNS                                                  
193200           PERFORM IMS-GNP-RUB-TEXT                                       
193300           IF SEGMENT-FINNS                                               
193400              MOVE RUB-TEXT-BERUBTXT TO                                   
193500                             W-BERUBTEXT(RUB-IDSEGMNR)                    
193600           END-IF                                                         
193700        END-IF                                                            
193800     END-IF                                                               
193900     MOVE W-BERUBTEXT(1) TO WS-BERUBTEXT(1, IND2)                         
194000     MOVE W-BERUBTEXT(2) TO WS-BERUBTEXT(2, IND2)                         
194100     MOVE W-BERUBTEXT(3) TO WS-BERUBTEXT(3, IND2)                         
194200     .                                                                    
194300     EJECT                                                                
194400 CC-LAS-RUBRIKTEXT SECTION.                                               
194500     SKIP2                                                                
194600     MOVE +1 TO IND2                                                      
194700     MOVE +4 TO W-IDCATRAD                                                
194800                SPAR-IDCATRAD                                             
194900     MOVE LOW-VALUE TO W-KDCATPUB-512                                     
195000     PERFORM IMS-GU-AVS                                                   
195100     PERFORM IMS-GNP-AVS-RAD-FIRST                                        
195200     MOVE RAD-IDCATRAD       TO W-IDCATRAD                                
195300     MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                            
195400     PERFORM G-LAES-AVSNITT-RAD                                           
195500     PERFORM UNTIL SEGMENT-SAKNAS                                         
195600     OR RAD-IDCATRAD NOT = SPAR-IDCATRAD                                  
195700                                                                          
195800        PERFORM CCA-LAS-RUBRIKTEXT                                        
195900                                                                          
196000        IF IND2 > IND2-MAX                                                
196100           MOVE IND2    TO IND2-MAX                                       
196200        END-IF                                                            
196300        ADD +1          TO IND2                                           
196400        PERFORM IMS-GU-AVS                                                
196500        PERFORM IMS-GNP-AVS-RAD-NEXT                                      
196600        MOVE RAD-IDCATRAD       TO W-IDCATRAD                             
196700        MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                         
196800        PERFORM G-LAES-AVSNITT-RAD                                        
196900     END-PERFORM                                                          
197000     .                                                                    
197100     EJECT                                                                
197200 CCA-LAS-RUBRIKTEXT SECTION.                                              
197300     SKIP2                                                                
197400     PERFORM IMS-GNP-AVS-TEXT                                             
197500     IF SEGMENT-FINNS                                                     
197600        MOVE TEXT-BERUBTEXT TO W-BERUBTEXT-R4                             
197700     ELSE                                                                 
197800        MOVE SPACE TO W-BERUBTEXT-R4                                      
197900     END-IF                                                               
198000     PERFORM CCAA-LAS-FOTNOT                                              
198100     IF FOTNOT-RAKNARE-2 NOT = ZERO                                       
198200        PERFORM CCAB-KOLLA-UPPDAT-FOTNOT-TAB                              
198300        PERFORM CCAC-REDIGERA-BERUBTEXT-R4                                
198400     ELSE                                                                 
198500        MOVE W-BERUBTEXT-R4 TO WS-BERUBTEXT(4, IND2)                      
198600     END-IF                                                               
198700     .                                                                    
198800     EJECT                                                                
198900 CCAA-LAS-FOTNOT SECTION.                                                 
199000     SKIP2                                                                
199100     MOVE ZERO TO SPAR-IDFOTNR(1)                                         
199200                  SPAR-IDFOTNR(2)                                         
199300                  SPAR-IDFOTNR(3)                                         
199400                  FOTNOT-RAKNARE-2                                        
199500     PERFORM IMS-GNP-AVS-FOT                                              
199600     PERFORM UNTIL SEGMENT-SAKNAS                                         
199700        ADD +1 TO FOTNOT-RAKNARE-2                                        
199800        MOVE FOT-IDFOTNR TO SPAR-IDFOTNR(FOTNOT-RAKNARE-2)                
199900        PERFORM IMS-GNP-AVS-FOT                                           
200000     END-PERFORM                                                          
200100                                                                          
200200     .                                                                    
200300     EJECT                                                                
200400 CCAB-KOLLA-UPPDAT-FOTNOT-TAB SECTION.                                    
200500     SKIP2                                                                
200600     MOVE +1 TO DOIX                                                      
200700     PERFORM UNTIL DOIX = +4                                              
200800       IF SPAR-IDFOTNR(DOIX) NOT = ZERO                                   
200900          SET IX-FOT TO +1                                                
201000          SEARCH FOTNOT-TABELL                                            
201100                   AT END PERFORM CCABA-UPPDATERA-SPAR-FOTNOT             
201200            WHEN SPAR-IDFOTNR(DOIX) = IDFOTNR-FYSISKT(IX-FOT)             
201300              MOVE FOTNR-REDIGERAT(IX-FOT) TO SPAR-FOT-RED(DOIX)          
201400          END-SEARCH                                                      
201500       END-IF                                                             
201600       ADD +1 TO DOIX                                                     
201700     END-PERFORM                                                          
201800                                                                          
201900     .                                                                    
202000     EJECT                                                                
202100 CCABA-UPPDATERA-SPAR-FOTNOT SECTION.                                     
202200     SKIP2                                                                
202300     SET IX-FOT TO FOTNOT-RAKNARE                                         
202400     MOVE SPAR-IDFOTNR(DOIX) TO IDFOTNR-FYSISKT(IX-FOT)                   
202500     MOVE FOTNR-REDIGERAT(IX-FOT) TO SPAR-FOT-RED(DOIX)                   
202600     ADD +1 TO FOTNOT-RAKNARE                                             
202700                                                                          
202800     .                                                                    
202900     EJECT                                                                
203000 CCAC-REDIGERA-BERUBTEXT-R4 SECTION.                                      
203100     SKIP2                                                                
203200     IF FOTNOT-RAKNARE-2 = +1                                             
203300         STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ')'                    
203400                DELIMITED BY SIZE                                         
203500                INTO FOTNOT-RED-AREA                                      
203600         STRING FOTNOT-RED-AREA DELIMITED BY SPACE ' '                    
203700                W-BERUBTEXT-R4  DELIMITED BY SIZE                         
203800                INTO WS-BERUBTEXT(4, IND2)                                
203900     END-IF                                                               
204000     IF FOTNOT-RAKNARE-2 = +2                                             
204100         STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ','                    
204200                SPAR-FOT-RED(2) DELIMITED BY SPACE ')'                    
204300                DELIMITED BY SIZE                                         
204400                INTO FOTNOT-RED-AREA                                      
204500         STRING FOTNOT-RED-AREA DELIMITED BY SPACE ' '                    
204600                W-BERUBTEXT-R4  DELIMITED BY SIZE                         
204700                INTO WS-BERUBTEXT(4, IND2)                                
204800     END-IF                                                               
204900     IF FOTNOT-RAKNARE-2 = +3                                             
205000         STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ','                    
205100                SPAR-FOT-RED(2) DELIMITED BY SPACE ','                    
205200                SPAR-FOT-RED(3) DELIMITED BY SPACE ')'                    
205300                DELIMITED BY SIZE                                         
205400                INTO FOTNOT-RED-AREA                                      
205500         STRING FOTNOT-RED-AREA DELIMITED BY SPACE ' '                    
205600                W-BERUBTEXT-R4 DELIMITED BY SIZE                          
205700                INTO WS-BERUBTEXT(4, IND2)                                
205800     END-IF                                                               
205900     MOVE SPACE TO FOTNOT-RED-AREA                                        
206000                                                                          
206100     .                                                                    
206200     EJECT                                                                
206300 CD-LAS-KOLUMNTEXT SECTION.                                               
206400     SKIP2                                                                
206500     MOVE +1 TO IND                                                       
206600                IND2                                                      
206700     MOVE +10 TO W-IDCATRAD                                               
206800     MOVE LOW-VALUE TO W-KDCATPUB-512                                     
206900     PERFORM IMS-GU-AVS                                                   
207000     PERFORM IMS-GNP-AVS-RAD-FIRST                                        
207100     MOVE RAD-IDCATRAD       TO W-IDCATRAD                                
207200     MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                            
207300     PERFORM G-LAES-AVSNITT-RAD                                           
207400     PERFORM UNTIL SEGMENT-SAKNAS                                         
207500     OR RAD-IDCATRAD > 19                                                 
207600     OR IND = +6                                                          
207700        MOVE RAD-IDCATRAD    TO SPAR-IDCATRAD                             
207800*       --- NYTT RADNR,                                                   
207900*       --- TESTA ATT IND2 PEKAR MOT RÄTT HUVUD M.A.P. LÄST FOM           
208000        PERFORM UNTIL WS-KDCATPUB-R-FOM-RUB(IND2) =                       
208100                                  RAD-KDCATPUB-FOM (4:3)                  
208200          ADD +1 TO IND2                                                  
208300        END-PERFORM                                                       
208400                                                                          
208500        PERFORM UNTIL SEGMENT-SAKNAS                                      
208600        OR RAD-IDCATRAD NOT = SPAR-IDCATRAD                               
208700          PERFORM IMS-GNP-AVS-TEXT                                        
208800          IF SEGMENT-FINNS                                                
208900             MOVE TEXT-TEKOL TO W-TEKOL(IND, IND2)                        
209000          ELSE                                                            
209100             MOVE SPACE TO W-TEKOL(IND, IND2)                             
209200          END-IF                                                          
209300          PERFORM CDA-LAS-FOTNOT                                          
209400          IF FOTNOT-RAKNARE-2 NOT = ZERO                                  
209500             PERFORM CDB-KOLLA-UPPDAT-FOTNOT-TAB                          
209600             PERFORM CDC-REDIGERA-TEKOL                                   
209700          ELSE                                                            
209800             MOVE W-TEKOL(IND, IND2)  TO W-TEKOL-RED(IND, IND2)           
209900          END-IF                                                          
210000          IF IND2 > IND2-MAX                                              
210100             MOVE IND2    TO IND2-MAX                                     
210200          END-IF                                                          
210300          ADD +1          TO IND2                                         
210400          PERFORM IMS-GU-AVS                                              
210500          PERFORM IMS-GNP-AVS-RAD-NEXT                                    
210600          MOVE RAD-IDCATRAD       TO W-IDCATRAD                           
210700          MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                       
210800          PERFORM G-LAES-AVSNITT-RAD                                      
210900        END-PERFORM                                                       
211000        ADD +1 TO IND                                                     
211100        MOVE +1 TO IND2                                                   
211200     END-PERFORM                                                          
211300                                                                          
211400     .                                                                    
211500     EJECT                                                                
211600 CDA-LAS-FOTNOT SECTION.                                                  
211700     SKIP2                                                                
211800     MOVE ZERO TO SPAR-IDFOTNR(1)                                         
211900                  SPAR-IDFOTNR(2)                                         
212000                  SPAR-IDFOTNR(3)                                         
212100                  FOTNOT-RAKNARE-2                                        
212200     PERFORM IMS-GNP-AVS-FOT                                              
212300     PERFORM UNTIL SEGMENT-SAKNAS                                         
212400        ADD +1 TO FOTNOT-RAKNARE-2                                        
212500        MOVE FOT-IDFOTNR TO SPAR-IDFOTNR(FOTNOT-RAKNARE-2)                
212600        PERFORM IMS-GNP-AVS-FOT                                           
212700     END-PERFORM                                                          
212800                                                                          
212900     .                                                                    
213000     EJECT                                                                
213100 CDB-KOLLA-UPPDAT-FOTNOT-TAB SECTION.                                     
213200     SKIP2                                                                
213300     MOVE +1 TO DOIX                                                      
213400     PERFORM UNTIL DOIX = +4                                              
213500       IF SPAR-IDFOTNR(DOIX) NOT = ZERO                                   
213600          SET IX-FOT TO +1                                                
213700          SEARCH FOTNOT-TABELL                                            
213800                AT END PERFORM CDBA-UPPDATERA-SPAR-FOTNOT                 
213900             WHEN SPAR-IDFOTNR(DOIX) = IDFOTNR-FYSISKT(IX-FOT)            
214000              MOVE FOTNR-REDIGERAT(IX-FOT) TO SPAR-FOT-RED(DOIX)          
214100          END-SEARCH                                                      
214200       END-IF                                                             
214300       ADD +1 TO DOIX                                                     
214400     END-PERFORM                                                          
214500                                                                          
214600     .                                                                    
214700     EJECT                                                                
214800 CDBA-UPPDATERA-SPAR-FOTNOT SECTION.                                      
214900     SKIP2                                                                
215000     SET IX-FOT TO FOTNOT-RAKNARE                                         
215100     MOVE SPAR-IDFOTNR(DOIX) TO IDFOTNR-FYSISKT(IX-FOT)                   
215200     MOVE FOTNR-REDIGERAT(IX-FOT) TO SPAR-FOT-RED(DOIX)                   
215300     ADD +1 TO FOTNOT-RAKNARE                                             
215400                                                                          
215500     .                                                                    
215600     EJECT                                                                
215700 CDC-REDIGERA-TEKOL SECTION.                                              
215800     SKIP2                                                                
215900     IF FOTNOT-RAKNARE-2 = +1                                             
216000        STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ')'                     
216100                DELIMITED BY SIZE                                         
216200               INTO FOTNOT-RED-AREA                                       
216300        STRING FOTNOT-RED-AREA DELIMITED BY SPACE ' '                     
216400               W-TEKOL(IND, IND2) DELIMITED BY SIZE                       
216500               INTO W-TEKOL-RED(IND, IND2)                                
216600     END-IF                                                               
216700     IF FOTNOT-RAKNARE-2 = +2                                             
216800       STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ','                      
216900              SPAR-FOT-RED(2) DELIMITED BY SPACE ')'                      
217000                DELIMITED BY SIZE                                         
217100              INTO FOTNOT-RED-AREA                                        
217200       STRING FOTNOT-RED-AREA DELIMITED BY SPACE ' '                      
217300              W-TEKOL(IND, IND2) DELIMITED BY SIZE                        
217400              INTO W-TEKOL-RED(IND, IND2)                                 
217500     END-IF                                                               
217600     IF FOTNOT-RAKNARE-2 = +3                                             
217700        STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ','                     
217800               SPAR-FOT-RED(2) DELIMITED BY SPACE ','                     
217900               SPAR-FOT-RED(3) DELIMITED BY SPACE ')'                     
218000                DELIMITED BY SIZE                                         
218100               INTO FOTNOT-RED-AREA                                       
218200       STRING FOTNOT-RED-AREA DELIMITED BY SPACE ' '                      
218300               W-TEKOL(IND, IND2) DELIMITED BY SIZE                       
218400               INTO W-TEKOL-RED(IND, IND2)                                
218500     END-IF                                                               
218600     MOVE SPACE TO FOTNOT-RED-AREA                                        
218700                                                                          
218800     .                                                                    
218900     EJECT                                                                
219000 CE-SKRIV-RUBRIKER-NY-SIDA  SECTION.                                      
219100     SKIP2                                                                
219200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
219300                         ALT-PCB1 PRT-AFTER-1                             
219400                         STRECKRAD                                        
219500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
219600                         ALT-PCB1 PRT-AFTER-1                             
219700                         HUVUD-TEXT(SPR-IX)                               
219800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
219900                         ALT-PCB1 PRT-AFTER-1 STRECKRAD-2                 
220000     .                                                                    
220100     EJECT                                                                
220200 CF-SKRIV-RUBRIK-RADER  SECTION.                                          
220300     SKIP2                                                                
220400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
220500                         ALT-PCB1 PRT-AFTER-1 RUB-RAD-1                   
220600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
220700                         ALT-PCB1 PRT-AFTER-1 RUB-RAD-2                   
220800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
220900                         ALT-PCB1 PRT-AFTER-1 RUB-RAD-3                   
221000     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
221100                         ALT-PCB1 PRT-AFTER-1 RUB-RAD-4                   
221200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
221300                         ALT-PCB1 PRT-AFTER-1 RUB-RAD-5                   
221400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
221500                         ALT-PCB1 PRT-AFTER-1 STRECKRAD-2                 
221600     .                                                                    
221700     EJECT                                                                
221800 CG-SKRIV-TEXT-RAD  SECTION.                                              
221900     SKIP2                                                                
222000                                                                          
222100*                    * MAN HAR VALT ATT INTE PRINTA ARTIKELRADERNA        
222200     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
222300                         ALT-PCB1 PRT-AFTER-1                             
222400                         TEXT-SLUT-RAD(SPR-IX)                            
222500     .                                                                    
222600     EJECT                                                                
222700 D-BEARBETA-RADER SECTION.                                                
222800     SKIP2                                                                
222900     MOVE +20 TO W-IDCATRAD                                               
223000     MOVE LOW-VALUE TO W-KDCATPUB-512                                     
223100     MOVE NEJ TO EXTRA-RAD                                                
223200     MOVE SPACE TO UTSKRIFT-HJALP-AREA                                    
223300                   L-BEART                                                
223400                   L-BEART-E                                              
223500                   L-TEKATANM                                             
223600                   L-TEKATANM-E                                           
223700                   SPAR-BEART                                             
223800                   SPAR-BERUBTXT(1)                                       
223900                   SPAR-BERUBTXT(2)                                       
224000                   SPAR-BERUBTXT(3)                                       
224100                   SPAR-HAEN-KDHAEN                                       
224200                   SPAR-TEKATANM                                          
224300                   FOTNOT-RED-AREA                                        
224400                                                                          
224500     PERFORM IMS-GU-AVS                                                   
224600     PERFORM IMS-GNP-AVS-RAD-FIRST                                        
224700     MOVE RAD-IDCATRAD       TO W-IDCATRAD                                
224800     MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                            
224900     PERFORM G-LAES-AVSNITT-RAD                                           
225000                                                                          
225100     PERFORM UNTIL SEGMENT-SAKNAS OR MAX-FOTNOT-FEL                       
225200        MOVE RAD-IDCATRAD    TO SPAR-IDCATRAD                             
225300        PERFORM UNTIL SEGMENT-SAKNAS OR MAX-FOTNOT-FEL                    
225400        OR            RAD-IDCATRAD NOT = SPAR-IDCATRAD                    
225500           MOVE RAD-IDCATRAD TO L-IDCATRAD                                
225600           MOVE RAD-KDCATPUB-FOM (4:3)                                    
225700                                 TO L-KDCATPUB-R-FOM-RAD                  
225800           MOVE RAD-KDCATPUB-TOM (4:3)                                    
225900                                 TO L-KDCATPUB-R-TOM-RAD                  
226000           MOVE RAD-KDRADST TO L-KDRADST                                  
226100           PERFORM DA-AVS-ART                                             
226200           PERFORM DB-AVS-TEXT                                            
226300           PERFORM DC-LAS-BEART                                           
226400*********  PERFORM DD-AVS-NOT                                             
226500           PERFORM DE-AVS-RUB                                             
226600           PERFORM DF-AVS-FOT                                             
226700           IF MAX-FOTNOT-OK                                               
226800              PERFORM DG-AVS-HAEN                                         
226900              PERFORM DI-UPPDATERA-KONTROLLSIFFRA                         
227000              PERFORM DJ-UPPDATERA-KURSIV-O-PARANTES                      
227100              PERFORM DK-REDIGERA-SKRIV-RAD                               
227200                                                                          
227300           END-IF                                                         
227400           PERFORM IMS-GU-AVS                                             
227500           PERFORM IMS-GNP-AVS-RAD-NEXT                                   
227600           MOVE RAD-IDCATRAD       TO W-IDCATRAD                          
227700           MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                      
227800           PERFORM G-LAES-AVSNITT-RAD                                     
227900        END-PERFORM                                                       
228000     END-PERFORM                                                          
228100     .                                                                    
228200     EJECT                                                                
228300 DA-AVS-ART     SECTION.                                                  
228400     SKIP2                                                                
228500     PERFORM IMS-GNP-AVS-ART                                              
228600                                                                          
228700     IF SEGMENT-FINNS                                                     
228800        MOVE ART-KDFBX    TO L-KDFBX                                      
228900        MOVE ART-IDCATPOS TO L-IDCATPOS                                   
229000        INSPECT L-IDCATPOS REPLACING LEADING ZERO BY SPACE                
229100        MOVE ART-IDARTNR  TO L-IDARTNR  SPAR-IDARTNR                      
229200        MOVE ART-KDPS     TO L-KDPS                                       
229300                                                                          
229400        PERFORM DAA-REDIGERA-KVKOL                                        
229500        IF L-KDFBX NOT = 'F'                                              
229600           MOVE ART-KVPUNKT TO SPAR-KVPUNKT                               
229700        END-IF                                                            
229800        IF ART-IDTTEXNR > ZERO                                            
229900           PERFORM DAB-LAS-TEXT                                           
230000        END-IF                                                            
230100     ELSE                                                                 
230200        MOVE SPACE TO L-KDFBX    L-IDCATPOS    L-KDPS                     
230300        MOVE ZERO  TO L-IDARTNR  SPAR-IDARTNR  SPAR-KVPUNKT               
230400        MOVE +1 TO KOL-IX                                                 
230500        PERFORM UNTIL KOL-IX = +6                                         
230600          MOVE SPACE TO L-KVKOL(KOL-IX)                                   
230700          ADD +1 TO KOL-IX                                                
230800        END-PERFORM                                                       
230900     END-IF                                                               
231000     PERFORM DAC-UPPDATERA-PARTSTATUS                                     
231100     .                                                                    
231200     EJECT                                                                
231300 DAA-REDIGERA-KVKOL SECTION.                                              
231400     SKIP2                                                                
231500     MOVE +1 TO KOL-IX                                                    
231600     PERFORM UNTIL KOL-IX = +6                                            
231700                                                                          
231800*****  INSPECT ART-KVKOL(KOL-IX) REPLACING ALL '.' BY SPACE               
231900                                                                          
232000       IF ART-KVKOL(KOL-IX) = SPACE                                       
232100         MOVE ' - ' TO L-KVKOL(KOL-IX)                                    
232200         IF L-KDFBX = 'F'                                                 
232300           MOVE SPACE TO L-KVKOL(KOL-IX)                                  
232400         END-IF                                                           
232500       ELSE                                                               
232600         IF ART-KVKOL(KOL-IX) (1:1) NOT = SPACE                           
232700           MOVE ART-KVKOL(KOL-IX) TO L-KVKOL(KOL-IX)                      
232800         ELSE                                                             
232900           IF ART-KVKOL(KOL-IX) (2:1) NOT = SPACE                         
233000             MOVE ART-KVKOL(KOL-IX) (2:2)                                 
233100                                  TO L-KVKOL(KOL-IX)                      
233200           ELSE                                                           
233300             IF ART-KVKOL(KOL-IX) (3:1) NOT = SPACE                       
233400               MOVE ART-KVKOL(KOL-IX) (3:1)                               
233500                                  TO L-KVKOL(KOL-IX)                      
233600             END-IF                                                       
233700           END-IF                                                         
233800         END-IF                                                           
233900       END-IF                                                             
234000       ADD +1 TO KOL-IX                                                   
234100     END-PERFORM                                                          
234200     .                                                                    
234300     EJECT                                                                
234400 DAB-LAS-TEXT SECTION.                                                    
234500     SKIP2                                                                
234600     MOVE ART-IDTTEXNR TO W-IDTTEXNR                                      
234700     PERFORM IMS-GU-TEXT                                                  
234800     IF SEGMENT-FINNS                                                     
234900        PERFORM IMS-GNP-TEXT-TEXT                                         
235000        IF SEGMENT-FINNS                                                  
235100           MOVE TEXT-BETTEXT TO SPAR-BETTEXT                              
235200        ELSE                                                              
235300           MOVE SPACE TO SPAR-BETTEXT                                     
235400        END-IF                                                            
235500     END-IF                                                               
235600                                                                          
235700     .                                                                    
235800     EJECT                                                                
235900 DAC-UPPDATERA-PARTSTATUS SECTION.                                        
236000     SKIP2                                                                
236100     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
236200                          BYT02-IDARTNR                                   
236300     IF W-IDARTNR > ZERO                                                  
236400        IF BYT02-RENOV                                                    
236500          MOVE 'EU' TO L-KDPS                                             
236600        END-IF                                                            
236700        PERFORM DACA-KOLLA-ARTREG                                         
236800     END-IF                                                               
236900     .                                                                    
237000     EJECT                                                                
237100 DACA-KOLLA-ARTREG SECTION.                                               
237200     SKIP2                                                                
237300     PERFORM IMS-GU-ARTC01                                                
237400     IF SEGMENT-FINNS                                                     
237500        MOVE ARTC01-ART-TIERSDAT TO WS-TIERSDAT-AAVVD                     
237600        IF ARTC01-ART-KDERS-UTG > ZERO                                    
237700           IF ARTC01-ART-KDERS-UTG = 29                                   
237800              MOVE 'OP' TO L-KDPS                                         
237900           ELSE                                                           
238000              IF ARTC01-ART-KDERS-UTG > 20 AND < 27                       
238100                 MOVE 'SP' TO L-KDPS                                      
238200              ELSE                                                        
238300                 IF ARTC01-ART-KDERS-UTG > 29                             
238400                    MOVE 'NS' TO L-KDPS                                   
238500                 END-IF                                                   
238600              END-IF                                                      
238700           END-IF                                                         
238800        ELSE                                                              
238900           PERFORM IMS-GNP-ARTC11                                         
239000           MOVE ARTC11-CLAG-FLLSRDEL TO WS-FLLSRDEL                       
239100           IF ARTC11-CLAG-FLLSRDEL = NEJ                                  
239200              MOVE 'NS' TO L-KDPS                                         
239300           END-IF                                                         
239400                                                                          
239500           IF ARTC11-CLAG-KDERS = 29                                      
239600              MOVE 'OP' TO L-KDPS                                         
239700           ELSE                                                           
239800              IF (ARTC11-CLAG-KDERS > 20)                                 
239900              AND (ARTC11-CLAG-KDERS < 27)                                
240000                 IF ARTC11-CLAG-KDERS = 21 OR 24                          
240100                    PERFORM DACAA-KOLLA-TIERSDAT                          
240200                 ELSE                                                     
240300                    MOVE 'SP' TO L-KDPS                                   
240400                 END-IF                                                   
240500              ELSE                                                        
240600                 IF ARTC11-CLAG-KDERS > 29                                
240700                    MOVE 'NS' TO L-KDPS                                   
240800                 END-IF                                                   
240900              END-IF                                                      
241000           END-IF                                                         
241100           IF L-KDPS = SPACE                                              
241200              IF ARTC11-CLAG-KDUART = 'P'                                 
241300                 MOVE ARTC11-CLAG-KDUART TO L-KDPS                        
241400              END-IF                                                      
241500           END-IF                                                         
241600        END-IF                                                            
241700     END-IF                                                               
241800     .                                                                    
241900     EJECT                                                                
242000 DACAA-KOLLA-TIERSDAT SECTION.                                            
242100     SKIP2                                                                
242200     MOVE 'IDAG' TO DAT-KDDATFORM                                         
242300     CALL WDATKONV USING DAT-KDDATFORM                                    
242400                         DAT-I-TIDATUM                                    
242500                         DAT-O-TIDATUM                                    
242600                         DAT-KDSVAR                                       
242700     IF DAT-KDSVAR-OK                                                     
242800        MOVE DAT-TIAAVVD TO DAGENS-AAVVD                                  
242900        MOVE WS-TIERSDAT-AAVV TO W009VADD-DATUM                           
243000        MOVE +32              TO W009VADD-ANTAL                           
243100        CALL W009VADD USING W009VADD-DATUM                                
243200                            W009VADD-ANTAL                                
243300        MOVE W009VADD-DATUM TO WS-TIERSDAT-AAVV                           
243400        IF WS-TIERSDAT-AAVVD > DAGENS-AAVVD                               
243500           MOVE SPACE TO L-KDPS                                           
243600        ELSE                                                              
243700           MOVE 'SP' TO L-KDPS                                            
243800        END-IF                                                            
243900     END-IF                                                               
244000     .                                                                    
244100     EJECT                                                                
244200 DB-AVS-TEXT    SECTION.                                                  
244300     SKIP2                                                                
244400     PERFORM IMS-GNP-AVS-TEXT                                             
244500     IF SEGMENT-FINNS                                                     
244600        MOVE TEXT-TEKATANM TO SPAR-TEKATANM                               
244700                              L-TEKATANM                                  
244800     ELSE                                                                 
244900        MOVE SPACE TO SPAR-TEKATANM                                       
245000                      L-TEKATANM                                          
245100     END-IF                                                               
245200     .                                                                    
245300     EJECT                                                                
245400 DC-LAS-BEART   SECTION.                                                  
245500     SKIP2                                                                
245600     MOVE SPACE TO SPAR-BEART                                             
245700                                                                          
245800     PERFORM IMS-GNP-AVS-BEN                                              
245900     IF SEGMENT-FINNS                                                     
246000        IF L-KDPS = 'XX' OR 'NX' OR 'LX'                                  
246100           MOVE BEN-BEART TO SPAR-BEART                                   
246200        ELSE                                                              
246300           IF L-KDPS = SPACE OR 'LS' OR 'KL' OR 'NS' OR 'KN'              
246400              PERFORM DCA-KOLLA-BENREG                                    
246500              IF SEGMENT-SAKNAS                                           
246600                 MOVE BEN-BEART      TO TRUNK-BEART                       
246700                 MOVE FEL-BEART-AREA TO SPAR-BEART                        
246800              END-IF                                                      
246900           END-IF                                                         
247000        END-IF                                                            
247100     ELSE                                                                 
247200        IF L-KDPS = SPACE OR 'OP' OR 'SP' OR 'EU' OR 'IK' OR 'SW'         
247300                          OR 'P '                                         
247400        OR (L-KDPS = 'NS' AND WS-FLLSRDEL = NEJ)                          
247500           IF SPAR-IDARTNR = ZERO                                         
247600              CONTINUE                                                    
247700           ELSE                                                           
247800              MOVE SPAR-IDARTNR TO W-IDARTNR                              
247900                                                                          
248000              PERFORM IMS-GU-BENA-BSEQ                                    
248100              IF SEGMENT-FINNS                                            
248200                 IF W-IDSKYLT = 'USA'                                     
248300                    MOVE 'GB ' TO W-IDSKYLT                               
248400                    PERFORM IMS-GNP-BENA-TEXT-BSEQ                        
248500                    IF SEGMENT-FINNS                                      
248600                       MOVE BEN-TEXT-BEART TO SPAR-BEART                  
248700                    END-IF                                                
248800                    MOVE 'USA' TO W-IDSKYLT                               
248900                    PERFORM IMS-GNP-BENA-TEXT-BSEQ                        
249000                    IF SEGMENT-FINNS                                      
249100                       IF BEN-TEXT-BEART NOT = SPACE                      
249200                          MOVE BEN-TEXT-BEART TO SPAR-BEART               
249300                       END-IF                                             
249400                    END-IF                                                
249500                 ELSE                                                     
249600                    PERFORM IMS-GNP-BENA-TEXT-BSEQ                        
249700                    IF SEGMENT-FINNS                                      
249800                       MOVE BEN-TEXT-BEART TO SPAR-BEART                  
249900                    END-IF                                                
250000                 END-IF                                                   
250100              ELSE                                                        
250200                 MOVE MED-2(SPR-IX) TO SPAR-BEART                         
250300              END-IF                                                      
250400           END-IF                                                         
250500        END-IF                                                            
250600     END-IF                                                               
250700     .                                                                    
250800     EJECT                                                                
250900 DCA-KOLLA-BENREG SECTION.                                                
251000     SKIP2                                                                
251100     MOVE 'S  ' TO W-IDSKYLT                                              
251200     MOVE BEN-BEART TO W-BEART                                            
251300                                                                          
251400     PERFORM IMS-GU-BENA-ASEQ                                             
251500     IF SEGMENT-FINNS                                                     
251600        IF IDSKYLT-WS = 'S  '                                             
251700           MOVE BEN-BEART TO SPAR-BEART                                   
251800        ELSE                                                              
251900           IF BEN-BEN-KDHOMONYM = BEN-KDHOM                               
252000              PERFORM DCAA-LAS-TEXT                                       
252100           ELSE                                                           
252200              PERFORM IMS-GNP-BENA-HOM-ASEQ                               
252300              IF SEGMENT-FINNS                                            
252400                 PERFORM DCAB-HITTA-RAETT-TEXT                            
252500              ELSE                                                        
252600                 PERFORM IMS-GU-BENA-ASEQ                                 
252700                 IF SEGMENT-FINNS                                         
252800                    PERFORM DCAA-LAS-TEXT                                 
252900                 ELSE                                                     
253000                    MOVE SPACE TO SPAR-BEART                              
253100                 END-IF                                                   
253200              END-IF                                                      
253300           END-IF                                                         
253400        END-IF                                                            
253500     END-IF                                                               
253600     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
253700                                                                          
253800     .                                                                    
253900     EJECT                                                                
254000 DCAA-LAS-TEXT SECTION.                                                   
254100     SKIP2                                                                
254200     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
254300     PERFORM IMS-GNP-BENA-TEXT-ASEQ                                       
254400     IF SEGMENT-FINNS                                                     
254500        MOVE BEN-TEXT-BEART TO SPAR-BEART                                 
254600     ELSE                                                                 
254700        MOVE SPACE TO SPAR-BEART                                          
254800     END-IF                                                               
254900                                                                          
255000     .                                                                    
255100     EJECT                                                                
255200 DCAB-HITTA-RAETT-TEXT SECTION.                                           
255300     SKIP2                                                                
255400     MOVE SPACE TO SPAR-BEART                                             
255500     MOVE NEJ TO RAETT-TEXT                                               
255600                                                                          
255700     PERFORM IMS-GN-BENA-ASEQ                                             
255800                                                                          
255900     PERFORM UNTIL SEGMENT-SAKNAS OR RAETT-TEXT = JA                      
256000        IF BEN-BEN-KDHOMONYM = BEN-KDHOM                                  
256100           MOVE IDSKYLT-WS TO W-IDSKYLT                                   
256200           PERFORM IMS-GNP-BENA-TEXT-ASEQ                                 
256300           IF SEGMENT-FINNS                                               
256400              MOVE BEN-TEXT-BEART TO SPAR-BEART                           
256500           END-IF                                                         
256600           MOVE JA TO RAETT-TEXT                                          
256700        ELSE                                                              
256800           PERFORM IMS-GN-BENA-ASEQ                                       
256900        END-IF                                                            
257000     END-PERFORM                                                          
257100                                                                          
257200     .                                                                    
257300*    EJECT                                                                
257400*DD-AVS-NOT     SECTION.                                                  
257500*    SKIP2                                                                
257600*    .                                                                    
257700     EJECT                                                                
257800 DE-AVS-RUB     SECTION.                                                  
257900     SKIP2                                                                
258000     MOVE SPACE TO SPAR-BERUBTXT(1)                                       
258100                   SPAR-BERUBTXT(2)                                       
258200                   SPAR-BERUBTXT(3)                                       
258300     PERFORM IMS-GNP-AVS-RUB                                              
258400     IF SEGMENT-FINNS                                                     
258500        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
258600        PERFORM IMS-GU-RUB                                                
258700        IF SEGMENT-FINNS                                                  
258800           IF RUB-RUB-FLKOMBINERAS = JA                                   
258900              PERFORM IMS-GNP-RUB-TEXT                                    
259000              IF SEGMENT-FINNS                                            
259100                 MOVE RUB-TEXT-BERUBTXT TO                                
259200                   SPAR-BERUBTXT(RUB-IDSEGMNR)                            
259300              END-IF                                                      
259400           ELSE                                                           
259500              PERFORM IMS-GNP-RUB-TEXT                                    
259600              IF SEGMENT-FINNS                                            
259700                 MOVE RUB-TEXT-BERUBTXT TO SPAR-BERUBTXT(1)               
259800              END-IF                                                      
259900              PERFORM IMS-GNP-RUB-TEXT                                    
260000              IF SEGMENT-FINNS                                            
260100                 MOVE RUB-TEXT-BERUBTXT TO SPAR-BERUBTXT(2)               
260200              END-IF                                                      
260300              PERFORM IMS-GNP-RUB-TEXT                                    
260400              IF SEGMENT-FINNS                                            
260500                 MOVE RUB-TEXT-BERUBTXT TO SPAR-BERUBTXT(3)               
260600              END-IF                                                      
260700           END-IF                                                         
260800        END-IF                                                            
260900     END-IF                                                               
261000     PERFORM IMS-GNP-AVS-RUB                                              
261100     IF SEGMENT-FINNS                                                     
261200        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
261300        PERFORM IMS-GU-RUB                                                
261400        IF SEGMENT-FINNS                                                  
261500           PERFORM IMS-GNP-RUB-TEXT                                       
261600           IF SEGMENT-FINNS                                               
261700              MOVE RUB-TEXT-BERUBTXT TO                                   
261800                                     SPAR-BERUBTXT(RUB-IDSEGMNR)          
261900           END-IF                                                         
262000        END-IF                                                            
262100     END-IF                                                               
262200     PERFORM IMS-GNP-AVS-RUB                                              
262300     IF SEGMENT-FINNS                                                     
262400        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
262500        PERFORM IMS-GU-RUB                                                
262600        IF SEGMENT-FINNS                                                  
262700           PERFORM IMS-GNP-RUB-TEXT                                       
262800           IF SEGMENT-FINNS                                               
262900              MOVE RUB-TEXT-BERUBTXT TO                                   
263000                                     SPAR-BERUBTXT(RUB-IDSEGMNR)          
263100           END-IF                                                         
263200        END-IF                                                            
263300     END-IF                                                               
263400                                                                          
263500     .                                                                    
263600     EJECT                                                                
263700 DF-AVS-FOT     SECTION.                                                  
263800     SKIP2                                                                
263900     PERFORM DFA-LAS-FOTNOT                                               
264000     IF FOTNOT-RAKNARE-2 NOT = ZERO                                       
264100        PERFORM DFB-KOLLA-UPPDAT-FOTNOT-TAB                               
264200        IF MAX-FOTNOT-OK                                                  
264300           PERFORM DFC-REDIGERA-FOTNOT                                    
264400        END-IF                                                            
264500     ELSE                                                                 
264600        MOVE SPACE TO FOTNOT-RED-AREA                                     
264700     END-IF                                                               
264800                                                                          
264900     .                                                                    
265000     EJECT                                                                
265100 DFA-LAS-FOTNOT SECTION.                                                  
265200     SKIP2                                                                
265300     MOVE ZERO TO SPAR-IDFOTNR(1)                                         
265400                  SPAR-IDFOTNR(2)                                         
265500                  SPAR-IDFOTNR(3)                                         
265600                  FOTNOT-RAKNARE-2                                        
265700     PERFORM IMS-GNP-AVS-FOT                                              
265800     PERFORM UNTIL SEGMENT-SAKNAS                                         
265900        ADD +1 TO FOTNOT-RAKNARE-2                                        
266000        MOVE FOT-IDFOTNR TO SPAR-IDFOTNR(FOTNOT-RAKNARE-2)                
266100        PERFORM IMS-GNP-AVS-FOT                                           
266200     END-PERFORM                                                          
266300                                                                          
266400     .                                                                    
266500     EJECT                                                                
266600 DFB-KOLLA-UPPDAT-FOTNOT-TAB SECTION.                                     
266700     SKIP2                                                                
266800     MOVE +1 TO DOIX                                                      
266900     PERFORM UNTIL DOIX = +4 OR MAX-FOTNOT-FEL                            
267000        IF SPAR-IDFOTNR(DOIX) NOT = ZERO                                  
267100           SET IX-FOT TO +1                                               
267200           SEARCH FOTNOT-TABELL                                           
267300                     AT END PERFORM DFBA-UPPDATERA-SPAR-FOTNOT            
267400              WHEN SPAR-IDFOTNR(DOIX) = IDFOTNR-FYSISKT(IX-FOT)           
267500                 MOVE FOTNR-REDIGERAT(IX-FOT) TO                          
267600                                              SPAR-FOT-RED(DOIX)          
267700           END-SEARCH                                                     
267800        END-IF                                                            
267900        ADD +1 TO DOIX                                                    
268000     END-PERFORM                                                          
268100                                                                          
268200     .                                                                    
268300     EJECT                                                                
268400 DFBA-UPPDATERA-SPAR-FOTNOT SECTION.                                      
268500     SKIP2                                                                
268600     IF FOTNOT-RAKNARE > FOTNOT-TAB-MAX                                   
268700        PERFORM IMS-ROLLBACK                                              
268800*       ****************************************                          
268900*       * IMS-777-ABEND OCH STARTAR OM BILDEN  *                          
269000*       * IMS BACKAR PRINTERKÖN                *                          
269100*       ****************************************                          
269200        MOVE FEL-6(SPR-IX) TO MOD-TEMFSFEL                                
269300        MOVE NEJ TO SW-MAX-FOTNOT                                         
269400     ELSE                                                                 
269500        SET IX-FOT TO FOTNOT-RAKNARE                                      
269600        MOVE SPAR-IDFOTNR(DOIX) TO IDFOTNR-FYSISKT(IX-FOT)                
269700        MOVE FOTNR-REDIGERAT(IX-FOT) TO SPAR-FOT-RED(DOIX)                
269800        ADD +1 TO FOTNOT-RAKNARE                                          
269900     END-IF                                                               
270000                                                                          
270100     .                                                                    
270200     EJECT                                                                
270300 DFC-REDIGERA-FOTNOT SECTION.                                             
270400     SKIP2                                                                
270500     IF FOTNOT-RAKNARE-2 = +1                                             
270600        STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ')'                     
270700           DELIMITED BY SIZE                                              
270800           INTO FOTNOT-RED-AREA                                           
270900     END-IF                                                               
271000     IF FOTNOT-RAKNARE-2 = +2                                             
271100        STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ','                     
271200               SPAR-FOT-RED(2) DELIMITED BY SPACE ')'                     
271300           DELIMITED BY SIZE                                              
271400           INTO FOTNOT-RED-AREA                                           
271500     END-IF                                                               
271600     IF FOTNOT-RAKNARE-2 = +3                                             
271700       STRING SPAR-FOT-RED(1) DELIMITED BY SPACE ','                      
271800              SPAR-FOT-RED(2) DELIMITED BY SPACE ','                      
271900              SPAR-FOT-RED(3) DELIMITED BY SPACE ')'                      
272000           DELIMITED BY SIZE                                              
272100           INTO FOTNOT-RED-AREA                                           
272200     END-IF                                                               
272300                                                                          
272400     .                                                                    
272500     EJECT                                                                
272600 DG-AVS-HAEN    SECTION.                                                  
272700     SKIP2                                                                
272800     MOVE SPACE TO SPAR-HAEN-KDHAEN                                       
272900                                                                          
273000     PERFORM IMS-GNP-AVS-HAEN                                             
273100     IF SEGMENT-FINNS                                                     
273200        MOVE HAEN-IDCATGRP TO SPAR-HAEN-IDCATGRP                          
273300        MOVE HAEN-IDCATAVS TO SPAR-HAEN-IDCATAVS                          
273400        IF HAEN-IDCATRAD = ZERO                                           
273500           MOVE ZERO TO SPAR-HAEN-IDCATRAD                                
273600           MOVE SPACE TO SPAR-HAEN-TKN                                    
273700        ELSE                                                              
273800           MOVE HAEN-IDCATRAD TO SPAR-HAEN-IDCATRAD                       
273900           MOVE '/' TO SPAR-HAEN-TKN                                      
274000        END-IF                                                            
274100        MOVE HAEN-KDHAEN TO SPAR-HAEN-KDHAEN                              
274200     END-IF                                                               
274300                                                                          
274400     .                                                                    
274500     EJECT                                                                
274600 DI-UPPDATERA-KONTROLLSIFFRA SECTION.                                     
274700     SKIP2                                                                
274800     IF SPAR-IDARTNR > ZERO                                               
274900        IF L-KDPS = SPACE OR 'LS' OR 'KL' OR 'EU' OR 'IK'                 
275000           MOVE '-' TO L-STRECK                                           
275100           MOVE SPAR-IDARTNR TO FLT                                       
275200           MOVE '9' TO LGD                                                
275300           CALL W009KSIF USING FLT LGD KSIFF                              
275400           MOVE KSIFF TO L-REKSIFFRA                                      
275500        ELSE                                                              
275600           MOVE SPACE TO L-STRECK                                         
275700                         L-REKSIFFRA                                      
275800        END-IF                                                            
275900     ELSE                                                                 
276000        MOVE SPACE TO L-STRECK                                            
276100                      L-REKSIFFRA                                         
276200     END-IF                                                               
276300                                                                          
276400     .                                                                    
276500     EJECT                                                                
276600 DJ-UPPDATERA-KURSIV-O-PARANTES SECTION.                                  
276700     SKIP2                                                                
276800     IF SPAR-IDARTNR > ZERO                                               
276900        IF L-KDPS = 'SP'                                                  
277000           MOVE 'K(' TO L-KURSIV                                          
277100           MOVE ')' TO L-STRECK                                           
277200        ELSE                                                              
277300           IF L-KDPS = 'NS' OR 'OP' OR 'KN' OR 'LS'                       
277400              MOVE 'K ' TO L-KURSIV                                       
277500           ELSE                                                           
277600              MOVE SPACE TO L-KURSIV                                      
277700           END-IF                                                         
277800        END-IF                                                            
277900     ELSE                                                                 
278000        MOVE SPACE TO L-KURSIV                                            
278100     END-IF                                                               
278200                                                                          
278300     .                                                                    
278400     EJECT                                                                
278500 DK-REDIGERA-SKRIV-RAD SECTION.                                           
278600     SKIP2                                                                
278700     PERFORM DKA-REDIGERA-BENAMNING                                       
278800     PERFORM DKB-REDIGERA-ANMARKNING                                      
278900     PERFORM DKC-SAMMANSTALL-SKRIV-RAD                                    
279000                                                                          
279100     MOVE SPACE TO UTSKRIFT-HJALP-AREA                                    
279200                   L-BEART                                                
279300                   L-BEART-E                                              
279400                   L-TEKATANM                                             
279500                   L-TEKATANM-E                                           
279600                   SPAR-BEART                                             
279700                   SPAR-BERUBTXT(1)                                       
279800                   SPAR-BERUBTXT(2)                                       
279900                   SPAR-BERUBTXT(3)                                       
280000                   SPAR-HAEN-KDHAEN                                       
280100                   SPAR-TEKATANM                                          
280200                   FOTNOT-RED-AREA                                        
280300                                                                          
280400     MOVE SPACE TO SPAR-BETTEXT                                           
280500                                                                          
280600     .                                                                    
280700     EJECT                                                                
280800 DKA-REDIGERA-BENAMNING SECTION.                                          
280900     SKIP2                                                                
281000     MOVE +1 TO IND                                                       
281100                                                                          
281200     IF SPAR-BEART NOT = SPACE                                            
281300        MOVE SPAR-BEART TO UT-BENAMNING(IND)                              
281400        ADD +1 TO IND                                                     
281500     ELSE                                                                 
281600        IF SPAR-BERUBTXT(1) NOT = SPACE                                   
281700           MOVE SPAR-BERUBTXT(1) TO UT-BENAMNING(IND)                     
281800           ADD +1 TO IND                                                  
281900        END-IF                                                            
282000        IF SPAR-BERUBTXT(2) NOT = SPACE                                   
282100           MOVE SPAR-BERUBTXT(2) TO UT-BENAMNING(IND)                     
282200           ADD +1 TO IND                                                  
282300        END-IF                                                            
282400        IF SPAR-BERUBTXT(3) NOT = SPACE                                   
282500           MOVE SPAR-BERUBTXT(3) TO UT-BENAMNING(IND)                     
282600           ADD +1 TO IND                                                  
282700        END-IF                                                            
282800     END-IF                                                               
282900     IF SPAR-BETTEXT NOT = SPACE                                          
283000        MOVE SPAR-BETTEXT TO UT-BENAMNING(IND)                            
283100        ADD +1 TO IND                                                     
283200     END-IF                                                               
283300     IF SPAR-HAEN-KDHAEN = 'B'                                            
283400        IF IND > 1                                                        
283500           STRING '.' SPAR-HAEN-B DELIMITED BY SIZE                       
283600           INTO UT-BENAMNING(IND)                                         
283700           MOVE IND TO HAEN-RAD                                           
283800        ELSE                                                              
283900           MOVE SPAR-HAEN-B TO UT-BENAMNING(IND)                          
284000           MOVE ZERO TO HAEN-RAD                                          
284100        END-IF                                                            
284200     END-IF                                                               
284300                                                                          
284400     .                                                                    
284500     EJECT                                                                
284600 DKB-REDIGERA-ANMARKNING SECTION.                                         
284700     SKIP2                                                                
284800     MOVE +1 TO IND                                                       
284900                                                                          
285000     IF FOTNOT-RED-AREA NOT = SPACE                                       
285100        MOVE FOTNOT-RED-AREA TO UT-ANMARKNING(IND)                        
285200        ADD +1 TO IND                                                     
285300     END-IF                                                               
285400     IF SPAR-TEKATANM NOT = SPACE                                         
285500        MOVE SPAR-TEKATANM TO UT-ANMARKNING(IND)                          
285600        ADD +1 TO IND                                                     
285700     END-IF                                                               
285800     IF SPAR-HAEN-KDHAEN = 'A'                                            
285900        MOVE SPAR-HAEN-A TO UT-ANMARKNING(IND)                            
286000     END-IF                                                               
286100     MOVE SPACE TO FOTNOT-RED-AREA                                        
286200                                                                          
286300     .                                                                    
286400     EJECT                                                                
286500 DKC-SAMMANSTALL-SKRIV-RAD SECTION.                                       
286600     SKIP2                                                                
286700     IF L-KDFBX = 'F'                                                     
286800        MOVE SPACE TO PKT4                                                
286900     ELSE                                                                 
287000        MOVE ALL '.' TO PKT4                                              
287100     END-IF                                                               
287200                                                                          
287300     IF UT-BENAMNING(1) = SPACE                                           
287400        MOVE SPACE TO L-BEART                                             
287500     ELSE                                                                 
287600        EVALUATE SPAR-KVPUNKT                                             
287700           WHEN ZERO                                                      
287800              MOVE UT-BENAMNING(1) TO L-BEART                             
287900           WHEN  +1                                                       
288000              MOVE UT-BENAMNING(1) TO L-BEART-PKT1                        
288100           WHEN  +2                                                       
288200              MOVE UT-BENAMNING(1) TO L-BEART-PKT2                        
288300           WHEN  +3                                                       
288400              MOVE UT-BENAMNING(1) TO L-BEART-PKT3                        
288500           WHEN  OTHER                                                    
288600              MOVE UT-BENAMNING(1) TO L-BEART-PKT4                        
288700        END-EVALUATE                                                      
288800     END-IF                                                               
288900     IF L-KDFBX NOT = 'F'                                                 
289000        PERFORM DKCA-HJALPLINJE                                           
289100     END-IF                                                               
289200                                                                          
289300     IF UT-ANMARKNING(1) = SPACE                                          
289400        MOVE SPACE TO L-TEKATANM                                          
289500     ELSE                                                                 
289600        MOVE UT-ANMARKNING(1) TO L-TEKATANM                               
289700     END-IF                                                               
289800                                                                          
289900     PERFORM DKCB-SKRIV-TEXTRAD                                           
290000                                                                          
290100     MOVE +2 TO IND                                                       
290200     PERFORM UNTIL IND = +6                                               
290300        IF (UT-BENAMNING(IND) = SPACE)                                    
290400        AND (UT-ANMARKNING(IND) = SPACE)                                  
290500           CONTINUE                                                       
290600        ELSE                                                              
290700           IF UT-BENAMNING(IND) = SPACE                                   
290800              MOVE SPACE TO L-BEART-E                                     
290900           ELSE                                                           
291000              IF HAEN-RAD = IND                                           
291100                 MOVE ALL '.' TO PUNKTER                                  
291200                 MOVE ZERO TO HAEN-RAD                                    
291300              ELSE                                                        
291400                 MOVE SPACE TO PUNKTER                                    
291500              END-IF                                                      
291600              EVALUATE SPAR-KVPUNKT                                       
291700                 WHEN  ZERO                                               
291800                    MOVE UT-BENAMNING(IND) TO L-BEART-E                   
291900                 WHEN  +1                                                 
292000                    MOVE UT-BENAMNING(IND) TO L-BEART-PKT1-E              
292100                 WHEN  +2                                                 
292200                    MOVE UT-BENAMNING(IND) TO L-BEART-PKT2-E              
292300                 WHEN  +3                                                 
292400                    MOVE UT-BENAMNING(IND) TO L-BEART-PKT3-E              
292500                 WHEN  OTHER                                              
292600                    MOVE UT-BENAMNING(IND) TO L-BEART-PKT4-E              
292700              END-EVALUATE                                                
292800           END-IF                                                         
292900           IF UT-ANMARKNING(IND) = SPACE                                  
293000              MOVE SPACE TO L-TEKATANM-E                                  
293100           ELSE                                                           
293200              MOVE UT-ANMARKNING(IND) TO L-TEKATANM-E                     
293300           END-IF                                                         
293400                                                                          
293500           PERFORM DKCC-SKRIV-EXTRA-TEXTRAD                               
293600        END-IF                                                            
293700        ADD +1 TO IND                                                     
293800     END-PERFORM                                                          
293900                                                                          
294000     .                                                                    
294100     EJECT                                                                
294200 DKCA-HJALPLINJE SECTION.                                                 
294300     SKIP2                                                                
294400     MOVE +28 TO IND-TKN                                                  
294500**** DOWHILE BEART-TKN(IND-TKN) = SPACE AND IND-TKN > ZERO                
294600     PERFORM UNTIL (BEART-TKN(IND-TKN) NOT = SPACE)                       
294700               OR  (IND-TKN = ZERO)                                       
294800        MOVE '.' TO BEART-TKN(IND-TKN)                                    
294900        SUBTRACT +1 FROM IND-TKN                                          
295000     END-PERFORM                                                          
295100                                                                          
295200     .                                                                    
295300     EJECT                                                                
295400 DKCB-SKRIV-TEXTRAD SECTION.                                              
295500     SKIP2                                                                
295600     IF RADRAKNARE <  MAX-ANTAL-RADER                                     
295700        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
295800                            ALT-PCB1 PRT-AFTER-1 TEXT-RAD                 
295900        ADD +1 TO RADRAKNARE                                              
296000     ELSE                                                                 
296100        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
296200                            ALT-PCB1 PRT-NYSIDA-RAD4                      
296300                            HUVUD-TEXT(SPR-IX)                            
296400        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
296500                            ALT-PCB1 PRT-AFTER-1 STRECKRAD-2              
296600        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
296700                            ALT-PCB1 PRT-AFTER-1                          
296800                            TEXT-HUVUD-RAD(SPR-IX)                        
296900        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
297000                            ALT-PCB1 PRT-AFTER-1                          
297100                            TEXT-RAD                                      
297200        MOVE +7 TO RADRAKNARE                                             
297300     END-IF                                                               
297400                                                                          
297500     .                                                                    
297600     EJECT                                                                
297700 DKCC-SKRIV-EXTRA-TEXTRAD SECTION.                                        
297800     SKIP2                                                                
297900     IF RADRAKNARE < MAX-ANTAL-RADER                                      
298000        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
298100                            ALT-PCB1 PRT-AFTER-1                          
298200                            EXTRA-TEXTRAD                                 
298300        ADD +1 TO RADRAKNARE                                              
298400     ELSE                                                                 
298500        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
298600                            ALT-PCB1 PRT-NYSIDA-RAD4                      
298700                            HUVUD-TEXT(SPR-IX)                            
298800        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
298900                            ALT-PCB1 PRT-AFTER-1 STRECKRAD-2              
299000        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
299100                            ALT-PCB1 PRT-AFTER-1                          
299200                            TEXT-HUVUD-RAD(SPR-IX)                        
299300        CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST            
299400                            ALT-PCB1 PRT-AFTER-1                          
299500                            EXTRA-TEXTRAD                                 
299600        MOVE +7 TO RADRAKNARE                                             
299700     END-IF                                                               
299800                                                                          
299900     .                                                                    
300000     EJECT                                                                
300100 E-BEARBETA-FOTNOTER SECTION.                                             
300200     SKIP2                                                                
300300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
300400                         ALT-PCB1 PRT-NYSIDA-RAD4                         
300500                         DUMMY-AREA                                       
300600     MOVE +4 TO RADRAKNARE                                                
300700     MOVE +1 TO IND                                                       
300800     PERFORM UNTIL IND > FOTNOT-TAB-MAX                                   
300900        MOVE IND TO L-FOTNOT-RED                                          
301000        MOVE IDFOTNR-FYSISKT(IND) TO L-IDFOTNR-FYS                        
301100                                     W-IDFOTNR                            
301200        MOVE +1 TO W-IDSEGMNR                                             
301300        PERFORM IMS-GU-IDFOTNR-FORD                                       
301400        IF FORD-FLOVERSATT = JA                                           
301500           MOVE W-IDSKYLT TO W-IDSKYLT-FOT                                
301600        ELSE                                                              
301700           MOVE 'S  '    TO W-IDSKYLT-FOT                                 
301800        END-IF                                                            
301900        PERFORM IMS-GNP-IDFOTNR-FORD-TEXT                                 
302000        IF SEGMENT-FINNS                                                  
302100           MOVE TEXT-BEFOTNOT TO L-BEFOTNOT                               
302200           PERFORM EA-SKRIV-FOTNOT-RAD                                    
302300                                                                          
302400           ADD +1 TO W-IDSEGMNR                                           
302500           PERFORM IMS-GNP-IDFOTNR-FORD-TEXT                              
302600                                                                          
302700           PERFORM UNTIL SEGMENT-SAKNAS                                   
302800              MOVE TEXT-BEFOTNOT TO L-BEFOTNOT-E                          
302900              PERFORM EC-SKRIV-FOTNOT-EXTRARAD                            
303000              ADD +1 TO W-IDSEGMNR                                        
303100              PERFORM IMS-GNP-IDFOTNR-FORD-TEXT                           
303200           END-PERFORM                                                    
303300        ELSE                                                              
303400           MOVE SPACE TO L-BEFOTNOT                                       
303500           PERFORM EA-SKRIV-FOTNOT-RAD                                    
303600        END-IF                                                            
303700        IF IND < FOTNOT-TAB-MAX                                           
303800           ADD +1 IND GIVING IND2                                         
303900           IF IDFOTNR-FYSISKT(IND2) = ZERO                                
304000              MOVE FOTNOT-TAB-MAX TO IND                                  
304100           END-IF                                                         
304200        END-IF                                                            
304300        ADD +1 TO IND                                                     
304400     END-PERFORM                                                          
304500                                                                          
304600     .                                                                    
304700     EJECT                                                                
304800 EA-SKRIV-FOTNOT-RAD SECTION.                                             
304900     SKIP2                                                                
305000     IF RADRAKNARE >= MAX-ANTAL-RADER                                     
305100       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST             
305200                           ALT-PCB1 PRT-NYSIDA-RAD4                       
305300                           DUMMY-AREA                                     
305400       MOVE +4 TO RADRAKNARE                                              
305500     END-IF                                                               
305600     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
305700                         ALT-PCB1  PRT-AFTER-1 FOTNOT-RAD                 
305800     ADD +1 TO RADRAKNARE                                                 
305900     .                                                                    
306000     EJECT                                                                
306100 EC-SKRIV-FOTNOT-EXTRARAD SECTION.                                        
306200     SKIP2                                                                
306300     IF RADRAKNARE >= MAX-ANTAL-RADER                                     
306400       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST             
306500                           ALT-PCB1 PRT-NYSIDA-RAD4                       
306600                           DUMMY-AREA                                     
306700       MOVE +4 TO RADRAKNARE                                              
306800     END-IF                                                               
306900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE W-IDPRTLST               
307000                         ALT-PCB1 PRT-AFTER-1 FOTNOT-RAD-E                
307100     ADD +1 TO RADRAKNARE                                                 
307200     .                                                                    
307300     EJECT                                                                
307400 G-LAES-AVSNITT-RAD SECTION.                                              
307500     SKIP2                                                                
307600********* LÄS FRAM POST SOM SKA BEHANDLAS                                 
307700*                       TILLS SEGMENT ÄR SLUT                             
307800*                 ELLER TILLS LÄST RAD ÄR STÖRRE ÄN SENAST                
307900*                 ELLER         LÄST FOM INTE ÄR > BESTÄLLD TOM           
308000*                 SAMTIDIGT SOM LÄST TOM INTE ÄR < BESTÄLLD FOM           
308100     PERFORM UNTIL SEGMENT-SAKNAS                                         
308200       OR ( RAD-IDCATRAD > W-IDCATRAD )                                   
308300       OR ( RAD-KDCATPUB-FOM NOT > KDCATPUB-TOM-WS                        
308400                       AND                                                
308500            RAD-KDCATPUB-TOM NOT < KDCATPUB-FOM-WS  )                     
308600                                                                          
308700         PERFORM IMS-GU-AVS                                               
308800         PERFORM IMS-GNP-AVS-RAD-NEXT                                     
308900         MOVE RAD-IDCATRAD       TO W-IDCATRAD                            
309000         MOVE RAD-KDCATPUB-FOM   TO W-KDCATPUB-512                        
309100                                                                          
309200     END-PERFORM                                                          
309300     .                                                                    
309400     EJECT                                                                
309500 H-LAES-AVSNITT-VADIS SECTION.                                            
309600     SKIP2                                                                
309700********* LÄS FRAM TILL DEN POST SOM SKA BEHANDLAS                        
309800     PERFORM UNTIL SEGMENT-SAKNAS                                         
309900                                                                          
310000     OR  (VADIS-KDCATPUB-FOM NOT > KDCATPUB-TOM-WS                        
310100      AND VADIS-KDCATPUB-TOM NOT < KDCATPUB-FOM-WS)                       
310200                                                                          
310300         PERFORM IMS-GNP-AVS-VADIS                                        
310400                                                                          
310500     END-PERFORM                                                          
310600                                                                          
310700     .                                                                    
310800     EJECT                                                                
310900 I-KONTR-PUBKOD-FROM SECTION.                                             
311000                                                                          
311100     MOVE ZERO                  TO DAT-I-TIDATUM                          
311200     MOVE KDCATPUB-FOM-WS (3:4) TO DAT-I-TIDATUM                          
311300     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
311400                                                                          
311500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
311600                     DAT-O-TIDATUM DAT-KDSVAR                             
311700                                                                          
311800     IF DAT-KDSVAR-FEL                                                    
311900        MOVE FEL-3 (SPR-IX) TO MOD-TEMFSFEL                               
312000        MOVE NEJ TO INDATA-SW                                             
312100     END-IF                                                               
312200     .                                                                    
312300     EJECT                                                                
312400 J-KONTR-PUBKOD-TOM SECTION.                                              
312500                                                                          
312600     MOVE ZERO                  TO DAT-I-TIDATUM                          
312700     MOVE KDCATPUB-TOM-WS       TO DAT-I-TIDATUM                          
312800     MOVE 'AAVV  '              TO DAT-KDDATFORM                          
312900                                                                          
313000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
313100                     DAT-O-TIDATUM DAT-KDSVAR                             
313200                                                                          
313300     IF DAT-KDSVAR-FEL                                                    
313400        MOVE FEL-3 (SPR-IX) TO MOD-TEMFSFEL                               
313500        MOVE NEJ TO INDATA-SW                                             
313600     END-IF                                                               
313700     .                                                                    
313800     EJECT                                                                
313900*                                                                         
314000* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
314100* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
314200*                                                                         
314300*    -COPY W150Y2K1                                                       
314400     EJECT                                                                
314500****************************************************************          
314600*                    IMS SEKTIONER                             *          
314700****************************************************************          
314800     SKIP1                                                                
314900 IMS-GET-MSG SECTION.                                                     
315000     MOVE '  QC' TO GODK-STATUSKODER                                      
315100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
315200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
315300     PERFORM IMS-STATUSKONTROLL                                           
315400     SKIP3                                                                
315500     .                                                                    
315600 IMS-ROLLBACK   SECTION.                                                  
315700     MOVE '  ' TO GODK-STATUSKODER                                        
315800     CALL CBLTDLI USING ROLB MSG-PCB                                      
315900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
316000     PERFORM IMS-STATUSKONTROLL                                           
316100     SKIP3                                                                
316200     .                                                                    
316300 IMS-INSERT-MSG SECTION.                                                  
316400     IF ENGLISH-TEXT                                                      
316500         MOVE 'N' TO MFS-KDHUVOMR                                         
316600     END-IF                                                               
316700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
316800     MOVE SPACE TO GODK-STATUSKODER                                       
316900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
317000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
317100     PERFORM IMS-STATUSKONTROLL                                           
317200     .                                                                    
317300     EJECT                                                                
317400 IMS-GU-KAT SECTION.                                                      
317500     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
317600            DELIMITED BY SIZE INTO SSA1                                   
317700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
317800     CALL CBLTDLI USING GU CAT-PCB IO-AREA-24 SSA1                        
317900     MOVE CAT-STATUS-CODE TO STATUS-WS                                    
318000     PERFORM IMS-STATUSKONTROLL                                           
318100     SKIP2                                                                
318200     .                                                                    
318300 IMS-GU-AVS SECTION.                                                      
318400     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
318500            DELIMITED BY SIZE INTO SSA1                                   
318600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
318700     CALL CBLTDLI USING GU AVS-PCB IO-AREA-1 SSA1                         
318800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
318900     PERFORM IMS-STATUSKONTROLL                                           
319000     SKIP2                                                                
319100     .                                                                    
319200 IMS-GET-FIRST-AVS-ILLU SECTION.                                          
319300                                                                          
319400     STRING 'WLKATH11*F(WDN511KY>=' W-WDN511-X ')'                        
319500            DELIMITED BY SIZE INTO SSA1                                   
319600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
319700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-2 SSA1                        
319800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
319900     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
320100     SKIP3                                                                
320200 IMS-GET-NEXT-AVS-ILLU SECTION.                                           
320300                                                                          
320400     STRING 'WLKATH11(WDN511KY >' W-WDN511-X ')'                          
320500            DELIMITED BY SIZE INTO SSA1                                   
320600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
320700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-2 SSA1                        
320800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
320900     PERFORM IMS-STATUSKONTROLL                                           
321000     .                                                                    
321100     SKIP3                                                                
321200 IMS-GNP-AVS-RAD-FIRST SECTION.                                           
321300     STRING 'WLKATH12*F(WDN512KY=>' W-WDN512-X ')'                        
321400            DELIMITED BY SIZE INTO SSA1                                   
321500     MOVE '  GE' TO GODK-STATUSKODER                                      
321600     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-3 SSA1                        
321700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
321800     PERFORM IMS-STATUSKONTROLL                                           
321900     .                                                                    
322000     SKIP2                                                                
322100 IMS-GNP-AVS-RAD-NEXT SECTION.                                            
322200     STRING 'WLKATH12*F(WDN512KY >' W-WDN512-X ')'                        
322300            DELIMITED BY SIZE INTO SSA2                                   
322400     MOVE '  GE' TO GODK-STATUSKODER                                      
322500     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-3 SSA1 SSA2                   
322600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
322700     PERFORM IMS-STATUSKONTROLL                                           
322800     .                                                                    
322900     SKIP2                                                                
323000 IMS-GNP-AVS-VADIS SECTION.                                               
323100     MOVE 'WLKATH13 ' TO SSA1                                             
323200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
323300     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-4 SSA1                        
323400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
323500     PERFORM IMS-STATUSKONTROLL                                           
323600     .                                                                    
323700     EJECT                                                                
323800 IMS-GNP-AVS-ART SECTION.                                                 
323900     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
324000            DELIMITED BY SIZE INTO SSA1                                   
324100     STRING 'WLKATH12(WDN512KY =' W-WDN512-X ')'                          
324200            DELIMITED BY SIZE INTO SSA2                                   
324300     MOVE 'WLKATH21 ' TO SSA3                                             
324400     MOVE '  GE' TO GODK-STATUSKODER                                      
324500     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-5 SSA1 SSA2 SSA3              
324600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
324700     PERFORM IMS-STATUSKONTROLL                                           
324800     .                                                                    
324900     EJECT                                                                
325000 IMS-GNP-AVS-TEXT SECTION.                                                
325100     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
325200            DELIMITED BY SIZE INTO SSA1                                   
325300     STRING 'WLKATH12(WDN512KY =' W-WDN512-X ')'                          
325400            DELIMITED BY SIZE INTO SSA2                                   
325500     MOVE 'WLKATH22 ' TO SSA3                                             
325600     MOVE '  GE' TO GODK-STATUSKODER                                      
325700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-6 SSA1 SSA2 SSA3              
325800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
325900     PERFORM IMS-STATUSKONTROLL                                           
326000     .                                                                    
326100     EJECT                                                                
326200 IMS-GNP-AVS-BEN SECTION.                                                 
326300     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
326400            DELIMITED BY SIZE INTO SSA1                                   
326500     STRING 'WLKATH12(WDN512KY =' W-WDN512-X ')'                          
326600            DELIMITED BY SIZE INTO SSA2                                   
326700     MOVE 'WLKATH23 ' TO SSA3                                             
326800     MOVE '  GE' TO GODK-STATUSKODER                                      
326900     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-7 SSA1 SSA2 SSA3              
327000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
327100     PERFORM IMS-STATUSKONTROLL                                           
327200     .                                                                    
327300     EJECT                                                                
327400 IMS-GNP-AVS-RUB SECTION.                                                 
327500     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
327600            DELIMITED BY SIZE INTO SSA1                                   
327700     STRING 'WLKATH12(WDN512KY =' W-WDN512-X ')'                          
327800            DELIMITED BY SIZE INTO SSA2                                   
327900     MOVE 'WLKATH25 ' TO SSA3                                             
328000     MOVE '  GE' TO GODK-STATUSKODER                                      
328100     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-9 SSA1 SSA2 SSA3              
328200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
328300     PERFORM IMS-STATUSKONTROLL                                           
328400     .                                                                    
328500     EJECT                                                                
328600 IMS-GNP-AVS-FOT SECTION.                                                 
328700     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
328800            DELIMITED BY SIZE INTO SSA1                                   
328900     STRING 'WLKATH12(WDN512KY =' W-WDN512-X ')'                          
329000            DELIMITED BY SIZE INTO SSA2                                   
329100     MOVE 'WLKATH26 ' TO SSA3                                             
329200     MOVE '  GE' TO GODK-STATUSKODER                                      
329300     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-10 SSA1 SSA2 SSA3             
329400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
329500     PERFORM IMS-STATUSKONTROLL                                           
329600     .                                                                    
329700     EJECT                                                                
329800 IMS-GNP-AVS-HAEN SECTION.                                                
329900     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
330000            DELIMITED BY SIZE INTO SSA1                                   
330100     STRING 'WLKATH12(WDN512KY =' W-WDN512-X ')'                          
330200            DELIMITED BY SIZE INTO SSA2                                   
330300     MOVE 'WLKATH27 ' TO SSA3                                             
330400     MOVE '  GE' TO GODK-STATUSKODER                                      
330500     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-11 SSA1 SSA2 SSA3             
330600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
330700     PERFORM IMS-STATUSKONTROLL                                           
330800     .                                                                    
330900     EJECT                                                                
331000 IMS-GU-ARTC01 SECTION.                                                   
331100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
331200            DELIMITED BY SIZE INTO SSA1                                   
331300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
331400     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-14 SSA1                       
331500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
331600     PERFORM IMS-STATUSKONTROLL                                           
331700     SKIP2                                                                
331800     .                                                                    
331900 IMS-GNP-ARTC11 SECTION.                                                  
332000     MOVE 'WLARTC11 ' TO SSA1                                             
332100     MOVE '  GE' TO GODK-STATUSKODER                                      
332200     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA-15 SSA1                      
332300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
332400     PERFORM IMS-STATUSKONTROLL                                           
332500     .                                                                    
332600     EJECT                                                                
332700 IMS-GU-RUB SECTION.                                                      
332800     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
332900            DELIMITED BY SIZE INTO SSA1                                   
333000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
333100     CALL CBLTDLI USING GU RUB-PCB IO-AREA-16 SSA1                        
333200     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
333300     PERFORM IMS-STATUSKONTROLL                                           
333400     SKIP2                                                                
333500     .                                                                    
333600 IMS-GNP-RUB-TEXT SECTION.                                                
333700     STRING 'WLKATB11(IDSKYLT  =' W-IDSKYLT-X ')'                         
333800            DELIMITED BY SIZE INTO SSA1                                   
333900     MOVE '  GE' TO GODK-STATUSKODER                                      
334000     CALL CBLTDLI USING GNP RUB-PCB IO-AREA-17 SSA1                       
334100     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
334200     PERFORM IMS-STATUSKONTROLL                                           
334300     .                                                                    
334400     EJECT                                                                
334500 IMS-GU-TEXT SECTION.                                                     
334600     STRING 'WLKATD01(IDTTEXNR =' W-IDTTEXNR-X ')'                        
334700            DELIMITED BY SIZE INTO SSA1                                   
334800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
334900     CALL CBLTDLI USING GU TEXT-PCB IO-AREA-18 SSA1                       
335000     MOVE TEXT-STATUS-CODE TO STATUS-WS                                   
335100     PERFORM IMS-STATUSKONTROLL                                           
335200     SKIP2                                                                
335300     .                                                                    
335400 IMS-GNP-TEXT-TEXT SECTION.                                               
335500     STRING 'WLKATD11(IDSKYLT  =' W-IDSKYLT-X ')'                         
335600            DELIMITED BY SIZE INTO SSA1                                   
335700     MOVE '  GE' TO GODK-STATUSKODER                                      
335800     CALL CBLTDLI USING GNP TEXT-PCB IO-AREA-19 SSA1                      
335900     MOVE TEXT-STATUS-CODE TO STATUS-WS                                   
336000     PERFORM IMS-STATUSKONTROLL                                           
336100     .                                                                    
336200     EJECT                                                                
336300 IMS-GU-BENA-BSEQ SECTION.                                                
336400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
336500            DELIMITED BY SIZE INTO SSA1                                   
336600     MOVE '  GE' TO GODK-STATUSKODER                                      
336700     CALL CBLTDLI USING GU BENAB-PCB IO-AREA-20 SSA1                      
336800     MOVE BENAB-STATUS-CODE TO STATUS-WS                                  
336900     PERFORM IMS-STATUSKONTROLL                                           
337000     SKIP2                                                                
337100     .                                                                    
337200 IMS-GNP-BENA-TEXT-BSEQ SECTION.                                          
337300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
337400            DELIMITED BY SIZE INTO SSA1                                   
337500     MOVE '  GE' TO GODK-STATUSKODER                                      
337600     CALL CBLTDLI USING GNP BENAB-PCB IO-AREA-21 SSA1                     
337700     MOVE BENAB-STATUS-CODE TO STATUS-WS                                  
337800     PERFORM IMS-STATUSKONTROLL                                           
337900     .                                                                    
338000     EJECT                                                                
338100 IMS-GU-BENA-ASEQ SECTION.                                                
338200     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X W-BEART-X ')'               
338300            DELIMITED BY SIZE INTO SSA1                                   
338400     MOVE '  GE' TO GODK-STATUSKODER                                      
338500     CALL CBLTDLI USING GU BENAA-PCB IO-AREA-20 SSA1                      
338600     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
338700     PERFORM IMS-STATUSKONTROLL                                           
338800     SKIP2                                                                
338900     .                                                                    
339000 IMS-GNP-BENA-TEXT-ASEQ SECTION.                                          
339100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
339200            DELIMITED BY SIZE INTO SSA1                                   
339300     MOVE '  GE' TO GODK-STATUSKODER                                      
339400     CALL CBLTDLI USING GNP BENAA-PCB IO-AREA-21 SSA1                     
339500     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
339600     PERFORM IMS-STATUSKONTROLL                                           
339700     SKIP2                                                                
339800     .                                                                    
339900 IMS-GN-BENA-ASEQ SECTION.                                                
340000     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X W-BEART-X ')'               
340100            DELIMITED BY SIZE INTO SSA1                                   
340200     MOVE '  GE' TO GODK-STATUSKODER                                      
340300     CALL CBLTDLI USING GN BENAA-PCB IO-AREA-20 SSA1                      
340400     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
340500     PERFORM IMS-STATUSKONTROLL                                           
340600     SKIP2                                                                
340700     .                                                                    
340800 IMS-GNP-BENA-HOM-ASEQ SECTION.                                           
340900     MOVE 'WLBENA13 ' TO SSA1                                             
341000     MOVE '  GE' TO GODK-STATUSKODER                                      
341100     CALL CBLTDLI USING GNP BENAA-PCB IO-AREA-23 SSA1                     
341200     MOVE BENAA-STATUS-CODE TO STATUS-WS                                  
341300     PERFORM IMS-STATUSKONTROLL                                           
341400     .                                                                    
341500     EJECT                                                                
341600 IMS-GU-IDFOTNR-FORD SECTION.                                             
341700     STRING 'WLKATF01(IDFOTNR  =' W-IDFOTNR-X ')'                         
341800            DELIMITED BY SIZE INTO SSA1                                   
341900     MOVE 'WLKATF11 ' TO SSA2                                             
342000     MOVE '  GE' TO GODK-STATUSKODER                                      
342100     CALL CBLTDLI USING GU FOT-PCB IO-AREA-12 SSA1 SSA2                   
342200     MOVE FOT-STATUS-CODE TO STATUS-WS                                    
342300     PERFORM IMS-STATUSKONTROLL                                           
342400     SKIP3                                                                
342500     .                                                                    
342600 IMS-GNP-IDFOTNR-FORD-TEXT SECTION.                                       
342700     STRING 'WLKATF21(WDN321KY =' W-IDSKYLT-FOT-X                         
342800                      W-IDSEGMNR-X ')'                                    
342900            DELIMITED BY SIZE INTO SSA1                                   
343000     MOVE '  GE' TO GODK-STATUSKODER                                      
343100     CALL CBLTDLI USING GNP FOT-PCB IO-AREA-13 SSA1                       
343200     MOVE FOT-STATUS-CODE TO STATUS-WS                                    
343300     PERFORM IMS-STATUSKONTROLL                                           
343400     .                                                                    
343500     EJECT                                                                
343600 IMS-STATUSKONTROLL SECTION.                                              
343700     SET STATUS-IX TO 1                                                   
343800     SEARCH GODK-STATUS AT END CALL FELLOG                                
343900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
344000          CONTINUE                                                        
344100     END-SEARCH                                                           
344200     .                                                                    

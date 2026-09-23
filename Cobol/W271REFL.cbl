000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W271REFL.                                                
000500 AUTHOR.         FRONTEC, GÖTEBORG.                                       
000600 DATE-WRITTEN.   94/12/19.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SUBPROGRAM SOM BERÄKNAR PÅFYLLNADS-PUNKT, -KVANTITET SAMT        
001100*        ÖVERLAGERPUNKT                                                   
001200*                                                                         
001300*!!!!!OBS MAN FLYTTAR IN MATERIALPRIS(PRMATRL) TILL BESTÄLLNINGS-         
001400*!!!!!PRIS(PRARTBES) FÖR KINA ARTIKLAR!!!!!!!                             
001500*                                                                         
001600*        PROGRAMMET LÄSER WL2501 (WDR2)                                   
001700*                         WDB6                                            
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(8)    VALUE 'W271REFL'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400                                                                          
004500 01  ARBETSFAELT.                                                         
004600*                                                                         
004700     03 DAGENS-DATUM.                                                     
004800         05 DAGENS-AAR           PIC 9(4).                                
004900         05 DAGENS-MAANAD        PIC 9(2).                                
005000         05 DAGENS-DAG           PIC 9(2).                                
005100     03 DAGENS-AAR-PLUS2         PIC 9(4)        VALUE ZERO.              
005200     03 JMF-AAAA                 PIC 9(4)        VALUE ZERO.              
005300     03 DAGENS-DATUM-6           PIC 9(6)        VALUE ZERO.              
005400     03 WS-DAGENS-PERIOD         PIC 9(2)        VALUE ZERO.              
005500     03 DAGENS-PERIOD            PIC 9(2)        VALUE ZERO.              
005600                                                                          
005700*                                                                         
005800     03  WS-SNITT-VECKOR         PIC 9(1)V9(2)  VALUE 4.33.               
005900     03  WS-DAGAR-LO2530         PIC 9(1)V9(1)  VALUE 9.5.                
006000     03  WS-RESSFAC              PIC 9(1)V9(2)  VALUE 1.00.               
006010     03  WS-AVR-SEAS-3MONTH      PIC 9(1)V9(2)  VALUE 1.00.               
006100*                                                                         
006200     03  WS-N                 PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
006300     03  WS-NP                PIC S9(10)V9(5) VALUE ZERO COMP-3.          
006400     03  WS-UB                PIC S9(8)V9(5)  VALUE ZERO COMP-3.          
006500     03  WS-NP-UB             PIC S9(10)V9(5) VALUE ZERO COMP-3.          
006600     03  WS-LEDTIDSBEHOV      PIC S9(6)V9(3)  VALUE ZERO COMP-3.          
006700     03  WS-TABELLBEHOV       PIC S9(6)V9(3)  VALUE ZERO COMP-3.          
006800     03  WS-PAFYLLNPKT        PIC S9(7)V9(5)  VALUE ZERO COMP-3.          
006900     03  WS-PAFYLLNKVANT      PIC S9(7)V9(5)  VALUE ZERO COMP-3.          
007000     03  WS-OVERLAGERPKT      PIC S9(7)       VALUE ZERO COMP-3.          
007100     03  WS-KVDAYS               PIC 9(3)     VALUE ZERO.                 
007200     03  WS-KVPB-REF-DAY-BINN-5 PIC S9(6)V9(5)  VALUE ZERO COMP-3.        
007300     03  WS-KVPB-REF-DAY-BINN-6 PIC S9(6)V9(5)  VALUE ZERO COMP-3.        
007400     03  WS-KVPB-REF-DAY-BINN-7 PIC S9(6)V9(5)  VALUE ZERO COMP-3.        
007500     03  WS-KVPB-REF-DAY-BNS-5                                            
007600                              PIC S9(6)V9(5)  VALUE ZERO COMP-3.          
007700     03  WS-KVPB-REF-DAY-BNS-6                                            
007800                              PIC S9(6)V9(5)  VALUE ZERO COMP-3.          
007900     03  WS-KVPB-REF-DAY-BNS-7                                            
008000                              PIC S9(6)V9(5)  VALUE ZERO COMP-3.          
008100***** WS-REPPFAK IS THE FACTOR HOW MUCH IS PREPLANNED                     
008200     03  WS-REPPFAK           PIC S9(1)V9(2)  VALUE ZERO COMP-3.          
008300***** WS-REPPFAK-1 IS THE FACTOR HOW MUCH IS NOT PREPLANNED               
008400     03  WS-REPPFAK-1         PIC S9(1)V9(2)  VALUE ZERO COMP-3.          
008500                                                                          
008600     03  WS-SP-IDDC              PIC X(2)    VALUE SPACE.                 
008700*                                                                         
008800     03  WS-KLASS.                                                        
008900         05  WS-KLASS-RAD        PIC 9(2)    VALUE ZERO.                  
009000         05  WS-KLASS-RAD-X REDEFINES WS-KLASS-RAD.                       
009100             07  FILLER          PIC X(2).                                
009200         05  WS-KLASS-KOL        PIC X(1)    VALUE SPACE.                 
009300*                                                                         
009400     03  WS-PRARTBES               PIC S9(7)V9(2) VALUE ZERO              
009500                                                   COMP-3.                
009600     03  WS-KVPB-VALID-WEEK        PIC S9(6)V9(1) VALUE ZERO              
009700                                                   COMP-3.                
009800     03  WS-REFPB-TOT              PIC S9(6)V9(1) VALUE ZERO              
009900                                                   COMP-3.                
010000     03  WS-KVPB-TOT               PIC S9(6)V9(3) VALUE ZERO              
010100                                                   COMP-3.                
010200     03  WS-KVPB-TOT-NOSEAS        PIC S9(6)V9(3) VALUE ZERO              
010300                                                   COMP-3.                
010400     03  WS-SENASTE-KVPB           PIC S9(6)V9(3) VALUE ZERO              
010500                                                   COMP-3.                
010600     03  WS-KVPBREOI               PIC S9(6)V9(3) VALUE ZERO              
010700                                                   COMP-3.                
010800     03  WS-KVPB-REF               PIC S9(6)V9(1) VALUE ZERO              
010900                                                   COMP-3.                
010910     03  WS-FFC-1                  PIC S9(6)V9(1) VALUE ZERO              
010920                                                   COMP-3.                
010930     03  WS-FFC-2                  PIC S9(6)V9(1) VALUE ZERO              
010940                                                   COMP-3.                
011000     03  WS-KVPBREOI-DAY           PIC S9(6)V9(5) VALUE ZERO              
011100                                                   COMP-3.                
011200     03  WS-KVPB-REF-DAY-PER-I     PIC S9(6)V9(5) VALUE ZERO              
011300                                                   COMP-3.                
011400     03  WS-KVPB-REF-DAY-PER-II    PIC S9(6)V9(5) VALUE ZERO              
011500                                                   COMP-3.                
011600     03  WS-KVPB-REF-DAY-PER-III   PIC S9(6)V9(5) VALUE ZERO              
011700                                                   COMP-3.                
011800     03  WS-KVPB-REF-DAY-PER-IV    PIC S9(6)V9(5) VALUE ZERO              
011900                                                   COMP-3.                
012000     03  WS-KVPB-REF-DAY-PER-V     PIC S9(6)V9(5) VALUE ZERO              
012100                                                   COMP-3.                
012200     03  WS-KVPB-REF-DAY-PER-VI    PIC S9(6)V9(5) VALUE ZERO              
012300                                                   COMP-3.                
012400     03  WS-KVREFLIM               PIC S9(5)      VALUE ZERO              
012500                                                   COMP-3.                
012600     03  WS-KVREFKVA               PIC S9(5)      VALUE ZERO              
012700                                                   COMP-3.                
012800     03  WS-KVARBDAG               PIC 9(3)    VALUE ZERO.                
012900     03  WS-BINNDAY-TIAAMMDD       PIC 9(6)    VALUE ZERO.                
013000     03  WS-WEEKS-IN-PERIOD        PIC 9       VALUE ZERO.                
013100     03  WS-WEEKS-IN-PERIOD-1      PIC 9       VALUE ZERO.                
013200     03  WS-WEEKS-IN-PERIOD-2      PIC 9       VALUE ZERO.                
013300     03  WS-PERIOD-JUST-1          PIC 9(4)    VALUE ZERO.                
013400     03  WS-PERIOD-JUST-2          PIC 9(4)    VALUE ZERO.                
013500     03  WS-TIAAVV                 PIC 9(4)    VALUE ZERO.                
013600     03  WS-TIVV                   PIC 9(2)    VALUE ZERO.                
013700     03  WS-TIVV-VECKA-I-PER       PIC 9(2)    VALUE ZERO.                
013800     03  WS-HIT-VV                 PIC 9(1)    VALUE ZERO.                
013900     03  WS-KVAR-VV                PIC 9(1)    VALUE ZERO.                
014000     03  INNEV-HIT-VV              PIC 9(1)    VALUE ZERO.                
014100     03  INNEV-KVAR-VV             PIC 9(1)    VALUE ZERO.                
014200     03  WS-KVDAYS-BIN             PIC 9(3)    VALUE ZERO.                
014201     03  WS-BINNDATE               PIC 9(6)    VALUE ZERO.                
014202     03  WS-BIN-TIYYMMDD           PIC 9(6)    VALUE ZERO.                
014203     03  WS-BIN-TIYYWWD            PIC 9(5)    VALUE ZERO.                
014204     03  WS-FFC-1-DATE             PIC 9(5)    VALUE ZERO.                
014205     03  WS-FFC-2-DATE             PIC 9(5)    VALUE ZERO.                
014210     03  WS-TIAAMMDD               PIC 9(6)    VALUE ZERO.                
014300     03  WS-TIAAMMDD-FOM           PIC 9(6)    VALUE ZERO.                
014400     03  FILLER REDEFINES WS-TIAAMMDD-FOM.                                
014500         05 WS-TIAA            PIC 9(2).                                  
014600         05 WS-TIMM            PIC 9(2).                                  
014700         05 WS-TIDD            PIC 9(2).                                  
014800     03  WS-BINNDAY-TIAARP     PIC 9(4)    VALUE ZERO.                    
014900     03  FILLER REDEFINES WS-BINNDAY-TIAARP.                              
015000         05 WS-BINNDAY-TIAA    PIC 9(2).                                  
015100         05 WS-BINNDAY-TIRP    PIC 9(2).                                  
015200                                                                          
015210     03 FILLER               PIC X(16) VALUE 'WS-RESEASON-TAB '.          
015220     03 WS-RESEASON-TABELL.                                               
015230       05 WS-RESEASON            OCCURS 12                                
015240                                   PIC S9(1)V9(2)                         
015250                                             COMP-3  VALUE ZERO.          
015260   03 WS-NOLLA-RESEASON.                                                  
015270     05 FILLER                   OCCURS 12                                
015280                                 PIC S9(1)V9(4)                           
015290                                             COMP-3  VALUE 1.00.          
015291                                                                          
015300     03  WS-RADIX                  PIC 9(2)    VALUE ZERO.                
015400     03  WS-PR-KOLIX               PIC 9(2)    VALUE ZERO.                
015500     03  IX                        PIC 9(2)    VALUE ZERO.                
015510     03  SEAS-IX                   PIC 9(2)    VALUE ZERO.                
015600     03  PER-IX                    PIC 9(2)    VALUE ZERO.                
015700     03  TAB-KVARBDAG              PIC 9(3)    VALUE ZERO                 
015800                                   OCCURS 12.                             
015900     03  SPARAREOR.                                                       
016000                                                                          
016100         05  SPARAREA-AKTUELL-TABELL.                                     
016200*            07 -COPY WDGX2502 -PRE AKTTAB-                               
016300                                                                          
016400         05  SPARAREA-GRUNDTABELL.                                        
016500*            07 -COPY WDGX2502 -PRE GRUNDTAB-                             
016600                                                                          
016700     03  TE-TABELL-SAKNAS.                                                
016800         05 TABELL-SAKNAS-TEXT    PIC X(22)                               
016900                           VALUE 'TABELL SAKNAS FÖR DC '.                 
017000         05 TABELL-SAKNAS-DC PIC X(2).                                    
017100                                                                          
017200     03  TE-TABELL-FEL.                                                   
017300         05 TABELL-FEL-TEXT    PIC X(22)                                  
017400                           VALUE 'TABELL FEL FÖR DC    '.                 
017500         05 TABELL-FEL-DC PIC X(2).                                       
017600                                                                          
017700                                                                          
017800 01  FILLER                      PIC X(24)  VALUE 'SWITCHAR'.             
017900                                                                          
018000 77  GRUNDTABELL-SW              PIC X       VALUE 'N'.                   
018100     88  GRUNDTABELL                         VALUE 'J'.                   
018200     88  AKTUELL-TABELL                      VALUE 'N'.                   
018300                                                                          
018400 77  SW-LOCAL                    PIC X       VALUE 'N'.                   
018500     88  LOCAL-PART                          VALUE 'J'.                   
018600     88  NO-LOCAL-PART                       VALUE 'N'.                   
018700                                                                          
018800 77  SW-FLFFC-UTIL               PIC X       VALUE 'N'.                   
018900     88  FFC-EXISTS                          VALUE 'J'.                   
019000     88  NO-FFC                              VALUE 'N'.                   
019100                                                                          
019110 77  SW-SEASON                   PIC X       VALUE 'N'.                   
019120     88  SEASON                              VALUE 'J'.                   
019130     88  NO-SEASON                           VALUE 'N'.                   
019140                                                                          
019150 77  SW-FFC-INT-PGM              PIC X       VALUE 'N'.                   
019160     88  FFC-EXIST-INT-PGM                   VALUE 'J'.                   
019180                                                                          
019200 77  WS-IDREFTAB-ID              PIC X(1).                                
019300     88 VALID-IDREFTAB-ID                    VALUE 'A' THRU 'Z'.          
019400                                                                          
019500                                                                          
019600*      --- VALID IDDC CODES                                               
019700*                                                                         
019800*01    -COPY WWDC99 -PRE REF-                                             
019900       EJECT                                                              
020000*                                                                         
020100     EJECT                                                                
020200 01  DYNAMISKA-SUBPROGRAM.                                                
020300*                                                                         
020400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
020900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021000     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
021100     03  W271LTPB                PIC X(8)    VALUE 'W271LTPB'.            
021200     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
021300     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
021400     EJECT                                                                
021500*    --- PARAMETRAR TILL POSTSUM                                          
021600*                                                                         
021700*01  -COPY W0005   -PRE  POSTSUM-                                         
021800     EJECT                                                                
021900*    ---PARAMETRAR TILL WORKDAY                                           
022000*01  -COPY WORKAREA                                                       
022100     EJECT                                                                
022200*    ---PARAMETRAR TILL DATKONV                                           
022300*01  -COPY WDATAREA                                                       
022400     EJECT                                                                
022500*    ---PARAMETRAR TILL DAGKONV                                           
022600*01  -COPY WDAGAREA                                                       
022700     EJECT                                                                
022800*    ---PARAMETRAR TILL W271LTPB                                          
022900*01  -COPY W271LTPB                                                       
023000     EJECT                                                                
023100*    ---PARAMETRAR TILL W271UTIL                                          
023200*01  -COPY W271UTIL                                                       
023300     EJECT                                                                
023400*    --- PARAMETRAR TILL WZ20DAYS                                         
023500*01 -COPY WZ20DAYS                                                        
023600     EJECT                                                                
023700*    --- PARAMETRAR TILL ABEND                                            
023800                                                                          
023900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
024000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
024100     SKIP2                                                                
024200 01  FELTEXT.                                                             
024300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
024400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
024500     EJECT                                                                
024600 01  TEST1                       PIC X(2).                                
024700 01  TEST2                       PIC X(6).                                
024800 01  TEST3                       PIC S9(5).                               
024900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025000*                                                                         
025100     EJECT                                                                
025200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025300     SKIP3                                                                
025400 01  NYCKLAR-TILL-DLI.                                                    
025500     03  W-WDGXKEY-X.                                                     
025600         05  W-IDHTYP           PIC X(4)    VALUE '2501'.                 
025700         05  W-IDDC             PIC X(2)    VALUE ZERO.                   
025800         05  W-LOWVALUE         PIC X(24)   VALUE LOW-VALUE.              
025900     03  W-IDDC-B6-X.                                                     
026000         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
026100     03  W-IDDC-B616-X.                                                   
026200         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
026300*                                                                         
026400     03  W-IDREFTAB-X.                                                    
026500         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
026600                                                                          
026700     03  W-IDARTNR-X.                                                     
026800         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
026900                                                                          
027000     03  W-IDDC-2-X.                                                      
027100         05  W-IDDC-2            PIC X(2)    VALUE SPACE.                 
027200                                                                          
027300     03  W-KDSEGKEY-X.                                                    
027400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
027500                                                                          
027600     03  W-IDDC-REF-X.                                                    
027700         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
027800                                                                          
027900     SKIP2                                                                
028000*    --- STATUS-KOD FRÅN IMS                                              
028100 01  STATUS-WS                   PIC XX.                                  
028200     88  SEGMENT-FINNS                       VALUE '  '.                  
028300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028400     SKIP2                                                                
028500 01  GODK-STATUSKODER.                                                    
028600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028700     SKIP3                                                                
028800 01  SSA1                        PIC X(128).                              
028900 01  SSA2                        PIC X(128).                              
029000 01  SSA3                        PIC X(64).                               
029100     EJECT                                                                
029200*    --- IMS FUNKTIONSKODER                                               
029300*01  -COPY W0003                                                          
029400     EJECT                                                                
029500*    ---  DLI INPUT-OUTPUT AREA                                           
029600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029700     SKIP3                                                                
029800 01  DLI-IO-AREA.                                                         
029900     03  IO-AREA                 PIC X(1036)   VALUE SPACE.               
030000     03  WL250101 REDEFINES IO-AREA.                                      
030100*        05  -COPY WDGX2501                                               
030200                                                                          
030300     03  WL250111 REDEFINES IO-AREA.                                      
030400*        05  -COPY WDGX2502                                               
030500     EJECT                                                                
030600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030700 01   DLI-IO-AREA-B601.                                                   
030800*     03  -COPY WDB601                                                    
030900                                                                          
031000 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
031100 01   DLI-IO-AREA-B616.                                                   
031200*     03  -COPY WDB616                                                    
031300                                                                          
031400 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
031500 01   DLI-IO-WDK711.                                                      
031600*     03  -COPY WDK711                                                    
031700                                                                          
031800 01  FILLER               PIC X(16)   VALUE 'WDK727 AREA'.                
031900 01   DLI-IO-WDK727.                                                      
032000*     03  -COPY WDK727                                                    
032100                                                                          
032200 01  FILLER               PIC X(16)   VALUE 'DC-REF AREA'.                
032300 01  TAB-IX               PIC S9(4)   COMP    VALUE ZERO.                 
032400 01  MAX-TAB-IX           PIC S9(4)   COMP    VALUE ZERO.                 
032500 01  IDDC-REF-TABELL.                                                     
032600     03  FILLER OCCURS 80.                                                
032700*      05  -COPY WDB616  -PRE TAB-                                        
032800                                                                          
032900     EJECT                                                                
033000 LINKAGE SECTION.                                                         
033100                                                                          
033200*    -COPY W271REFL                                                       
033300                                                                          
033400     EJECT                                                                
033500*01  -COPY W0008  -PRE 2501-                                              
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008  -PRE WDB6-                                              
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01  -COPY W0008  -PRE WDK7-                                              
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400 01  UTIL-WDK6-PCB               PIC X.                                   
034500 01  UTIL-WDK7-PCB               PIC X.                                   
034600 01  UTIL-WDB6-PCB               PIC X.                                   
034700     EJECT                                                                
034800 PROCEDURE DIVISION  USING REFL-W271REFL 2501-PCB                         
034900                           WDB6-PCB WDK7-PCB                              
035000                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
035100                           UTIL-WDB6-PCB.                                 
035200                                                                          
035300     PERFORM A-INIT                                                       
035400     PERFORM D-NOLLSTALL-ARBETSFALT                                       
035500                                                                          
035600     PERFORM C-HAEMTA-LEDTID                                              
035700                                                                          
035800*    - NYCKEL TILL 2501/2502                                              
035900     MOVE REFL-IDDC TO W-IDDC                                             
036000                       W-IDDC-2                                           
036100                                                                          
036200     IF (DCS-NDC-CN AND (REFL-IDDC-REF = SPACE))                          
036300     OR (DCS-USA AND (REFL-IDDC-REF = SPACE))                             
036400**KANADA SKALL EJ GÅ MED HÄR FÖR LOKAL ANSKAFFNING                        
036500       PERFORM X-LAS-2501-ANSK                                            
036600     ELSE                                                                 
036700       PERFORM B-LAS-2501                                                 
036800     END-IF                                                               
036900                                                                          
037000     IF WS-KVPB-TOT = ZERO                                                
037100        PERFORM E-PUNKTER-ARTIKEL-UTAN-PB                                 
037200     ELSE                                                                 
037300        PERFORM F-BERAKNA-PUNKTER                                         
037400     END-IF                                                               
037500                                                                          
037600     MOVE ZERO TO RETURN-CODE                                             
037700     GOBACK                                                               
037800     .                                                                    
037900     EJECT                                                                
038000                                                                          
038100                                                                          
038200 A-INIT SECTION.                                                          
038300                                                                          
038400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
038500     MOVE DAGENS-DATUM (3:6)      TO WS-TIAAMMDD                          
038600     COMPUTE DAGENS-AAR-PLUS2 = DAGENS-AAR + 2                            
038700                                                                          
038800     MOVE 'IDAG' TO DAT-KDDATFORM                                         
038900     CALL WDATKONV USING DAT-KDDATFORM,                                   
039000                         DAT-I-TIDATUM,                                   
039100                         DAT-O-TIDATUM,                                   
039200                         DAT-KDSVAR                                       
039300                                                                          
039400     IF DAT-KDSVAR-FEL                                                    
039500        DISPLAY '****  FEL I WDATKONV  *******'                           
039600        CALL FELLOG                                                       
039700     END-IF                                                               
039800                                                                          
039900     MOVE DAT-TIRP        TO DAGENS-PERIOD                                
040000     MOVE DAT-TIVV          TO WS-TIVV                                    
040100     MOVE DAT-TIAAVVD (1:4) TO WS-TIAAVV                                  
040200                                                                          
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600                                                                          
040700 B-LAS-2501 SECTION.                                                      
040800***                                                                       
040900*   LÄS HÄNDELSEBASEN WL2501 MED OLIKA NYCKLAR                            
041000*   REFILLEN HAR VÄRDE 0-9 MED 0 FÖR GRUNDTABELLEN SOM ANVÄNDS            
041100*   NÄR EN SPECIFIK TABELL INTE FINNS                                     
041200***                                                                       
041300                                                                          
041400     IF REFL-IDDC NOT = WS-SP-IDDC                                        
041500*    - KANSKE FÖREGÅENDE GRUNDTABELL VAR FEL SORT (ANSKAFFNING)           
041600     OR GRUNDTAB-2502-IDREFTAB = 'A'                                      
041700*      -- LÄS GRUNDTABELL TYP REFILL FÖR DETTA DC                         
041800       MOVE '0'  TO W-IDREFTAB                                            
041900       PERFORM IMS-GU-2502                                                
042000       IF SEGMENT-SAKNAS                                                  
042100           MOVE REFL-IDDC TO TABELL-SAKNAS-DC                             
042200           MOVE TE-TABELL-SAKNAS TO FELTEXT-STR                           
042300           DISPLAY FELTEXT                                                
042400           PERFORM S99-ABEND                                              
042500       ELSE                                                               
042600          MOVE 2502-WDGX2502 TO SPARAREA-GRUNDTABELL                      
042700       END-IF                                                             
042800     END-IF                                                               
042900                                                                          
043000     IF REFL-IDDC NOT = WS-SP-IDDC                                        
043100     OR REFL-IDREFTAB NOT = W-IDREFTAB                                    
043200*      - LÄS ANGIVEN TABELL FÖR DETTA DC.                                 
043300       MOVE REFL-IDREFTAB TO W-IDREFTAB                                   
043400       PERFORM IMS-GU-2502                                                
043500       IF SEGMENT-FINNS                                                   
043600         MOVE 2502-WDGX2502 TO SPARAREA-AKTUELL-TABELL                    
043700       ELSE                                                               
043800*        - SÄKERSTÄLL ATT AKTTAB INTE ANVÄNDS                             
043900         MOVE '?' TO AKTTAB-2502-IDREFTAB                                 
044000       END-IF                                                             
044100     END-IF                                                               
044200                                                                          
044300*    - KOM IHÅG OM DET ÄR GRUNDTABELLEN ELLER ANGIVEN TABELL              
044400*    - SOM SKA ANVÄNDAS.                                                  
044500     IF REFL-IDREFTAB = AKTTAB-2502-IDREFTAB                              
044600       MOVE NEJ TO GRUNDTABELL-SW                                         
044700     ELSE                                                                 
044800       MOVE JA  TO GRUNDTABELL-SW                                         
044900     END-IF                                                               
045000                                                                          
045100*    - KOM IHÅG TILL NÄSTA ANROP FÖR VILKET DC SOM TABELLER LÄSTS         
045200     MOVE REFL-IDDC TO WS-SP-IDDC                                         
045300     .                                                                    
045400                                                                          
045500     EJECT                                                                
045600                                                                          
045700 C-HAEMTA-LEDTID SECTION.                                                 
045800***                                                                       
045900*   HÄMTA LEDTID FÖR ETT DC FRÅN SPARADE WDB616 SOM ÄNVÄNDS               
046000*   VID BERÄKNINGEN AV PÅFYLLNADSPUNKT                                    
046100*                                                                         
046200*   REFL-BINNDAY-TIAAMMDD ÄR DET DATUM VILKET BEHOVET                     
046300*   BERÄKNAS FÖR CDC DVS EJ BINNDAY, KOMMER FRÅN W2222200.                
046400*   BEHOVSVECKAN PÅ CDC.                                                  
046500***                                                                       
046600*   BELOW CALCULATION IS FOR LEADTIME ADJUSTMENT OF                       
046700*   SAFETY STOCK WITH FUTURE FORECAST                                     
046800***                                                                       
046900     MOVE REFL-IDDC TO W-IDDC-B6                                          
047000     MOVE REFL-IDDC-REF  TO REF-WS-IDDC                                   
047100     PERFORM IMS-GU-WDB601                                                
047200     IF DCS-NDC                                                           
047300       IF REF-CDC-SE OR REF-NDC                                           
047400         MOVE REFL-IDDC-REF      TO W-IDDC-REF                            
047500                                    W-IDDC-B616                           
047600         PERFORM IMS-GNP-WDB616                                           
047700         IF SEGMENT-FINNS                                                 
047710           MOVE REF-RESSFAC      TO WS-RESSFAC                            
047800           IF REFL-FLFLYG = 'J'                                           
047900             MOVE REF-KVDLTID-AIRETA TO WS-KVDAYS                         
048000           ELSE                                                           
048100             MOVE REF-KVDLTID-TOT TO WS-KVDAYS                            
048200           END-IF                                                         
048300         END-IF                                                           
048400       ELSE                                                               
048500         IF REFL-NDC-KVDAGAR-TBT-DC = ZERO                                
048600            MOVE 1 TO WS-KVDAYS                                           
048700         ELSE                                                             
048800            MOVE REFL-NDC-KVDAGAR-TBT-DC                                  
048900                   TO WS-KVDAYS                                           
049000         END-IF                                                           
049100         MOVE JA    TO SW-LOCAL                                           
049200       END-IF                                                             
049300     ELSE                                                                 
049400       MOVE REFL-IDDC-REF        TO W-IDDC-REF                            
049500                                    W-IDDC-B616                           
049600       PERFORM IMS-GNP-WDB616                                             
049700       IF SEGMENT-FINNS                                                   
049710         MOVE REF-RESSFAC        TO WS-RESSFAC                            
049800         IF REFL-FLFLYG = 'J'                                             
049900           MOVE REF-KVDLTID-AIRETA TO WS-KVDAYS                           
050000         ELSE                                                             
050100           MOVE REF-KVDLTID-TOT TO WS-KVDAYS                              
050200         END-IF                                                           
050300       END-IF                                                             
050400     END-IF                                                               
050531*********************                                                     
050540*********************                                                     
050600**ADD ONE WEEK TO THE LEADTIME TO GET THE TOTAL FORECAST                  
050700**AFTER THE TOTAL LEADTIME PLUS ONE WEEK                                  
050800     COMPUTE DAYS-KVDAYS = WS-KVDAYS + 6                                  
050810     MOVE    DAYS-KVDAYS TO WS-KVDAYS-BIN                                 
050900     IF LOCAL-PART                                                        
051000       MOVE DAYS-KVDAYS   TO UTIL-LEADTIME                                
051100     ELSE                                                                 
051200       MOVE ZERO          TO UTIL-LEADTIME                                
051300     END-IF                                                               
051400                                                                          
051500     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
051600     IF REFL-BINNDAY-TIAAMMDD = ZERO                                      
051700       MOVE WS-TIAAMMDD           TO DAT-I-TIDATUM                        
051710                                     WS-BINNDATE                          
051800     ELSE                                                                 
051900       MOVE REFL-BINNDAY-TIAAMMDD TO DAT-I-TIDATUM                        
051910                                     WS-BINNDATE                          
052000     END-IF                                                               
052100     CALL WDATKONV USING   DAT-KDDATFORM                                  
052200                           DAT-I-TIDATUM                                  
052300                           DAT-O-TIDATUM                                  
052400                           DAT-KDSVAR                                     
052500                                                                          
052600     IF DAT-KDSVAR-OK                                                     
052700        MOVE DAT-TIAAVVD      TO DAYS-TIDATE1                             
052800     END-IF                                                               
052900                                                                          
053000     MOVE 'YYWWD'                   TO DAYS-KDDATFMT1                     
053100     MOVE 'YYWWD'                   TO DAYS-KDDATFMT2                     
053200     MOVE SPACE                      TO DAYS-TIDATE2                      
053300                                      DAYS-IDCALEND                       
053400                                                                          
053500     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
053600                                                                          
053700     IF DAYS-KDRC = 8                                                     
053800       STRING 'FEL 2 VID ANROP TILL WZ20DAYS - SEC C-'                    
053900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
054000       DISPLAY FELTEXT                                                    
054100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
054200     ELSE                                                                 
054300       MOVE DAYS-TIDATE2 (1:5)        TO UTIL-TIAAVVD                     
054400     END-IF                                                               
054410*************************************                                     
054420**CALCULATE DATE WHEN SUPPOSED TO BIN FROM GIVEN DATE                     
054430*************************************                                     
054440     MOVE WS-KVDAYS-BIN       TO DAYS-KVDAYS                              
054451     MOVE WS-BINNDATE         TO DAYS-TIDATE1                             
054460                                                                          
054470                                                                          
054480     MOVE 'YYMMDD'                  TO DAYS-KDDATFMT1                     
054490     MOVE 'YYMMDD'                  TO DAYS-KDDATFMT2                     
054491     MOVE SPACE                      TO DAYS-TIDATE2                      
054492                                      DAYS-IDCALEND                       
054493                                                                          
054494     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
054495                                                                          
054496     IF DAYS-KDRC = 8                                                     
054497       STRING 'FEL 3 VID ANROP TILL WZ20DAYS - SEC C-'                    
054498       DELIMITED BY SIZE INTO FELTEXT-STR                                 
054499       DISPLAY FELTEXT                                                    
054500       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
054501     ELSE                                                                 
054502       MOVE DAYS-TIDATE2 (1:6)        TO WS-BIN-TIYYMMDD                  
054503     END-IF                                                               
054510                                                                          
054600*****                                                                     
054700     MOVE NEJ                       TO SW-FLFFC-UTIL                      
054800     MOVE REFL-IDARTNR              TO UTIL-IDARTNR                       
054900     MOVE REFL-IDDC                 TO UTIL-IDDC                          
055000     MOVE REFL-IDDC-REF             TO UTIL-IDDC-REF                      
055100     MOVE 004                       TO UTIL-KDCALL                        
055200*                                                                         
055300***  IF SIMULATION REQUESTED                                              
055400*                                                                         
055500     IF REFL-IN-FLSIM = JA                                                
055600        MOVE JA                     TO UTIL-FLSIM                         
055700        MOVE REFL-IN-KVPB-REF       TO UTIL-KVPB-REF                      
055800        MOVE REFL-IN-KVPBREOI       TO UTIL-KVPBREOI                      
055900     END-IF                                                               
056000*                                                                         
056100     CALL W271UTIL USING UTIL-W271UTIL                                    
056200                         UTIL-WDK6-PCB                                    
056300                         UTIL-WDK7-PCB                                    
056400                         UTIL-WDB6-PCB                                    
056500                                                                          
056600     IF UTIL-KDSVAR-OK                                                    
056700        MOVE UTIL-KVPB-TOT        TO WS-KVPB-TOT                          
056800        MOVE UTIL-KVPB-TOT-NOSEAS TO WS-KVPB-TOT-NOSEAS                   
056900        MOVE UTIL-FLFFC           TO SW-FLFFC-UTIL                        
057000        IF WS-KVPB-TOT > ZERO                                             
057100          COMPUTE WS-KVPB-REF-DAY-BINN-5 =                                
057200                  (WS-KVPB-TOT / 4.33) / 5                                
057300          COMPUTE WS-KVPB-REF-DAY-BINN-6 =                                
057400                  (WS-KVPB-TOT / 4.33) / 6                                
057500        ELSE                                                              
057600          MOVE ZERO     TO WS-KVPB-REF-DAY-BINN-5                         
057700                           WS-KVPB-REF-DAY-BINN-6                         
057800        END-IF                                                            
057900     ELSE                                                                 
058000        MOVE 'FEL FRÅN W271UTIL '                                         
058100                                TO FELTEXT-STR                            
058200        DISPLAY FELTEXT                                                   
058300        PERFORM S99-ABEND                                                 
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700                                                                          
058800                                                                          
058900 D-NOLLSTALL-ARBETSFALT SECTION.                                          
059000                                                                          
059100     MOVE ZERO TO        WS-N                                             
059200                         WS-NP                                            
059300                         WS-UB                                            
059400                         WS-NP-UB                                         
059500                         WS-LEDTIDSBEHOV                                  
059600                         WS-TABELLBEHOV                                   
059700                         WS-PAFYLLNPKT                                    
059800                         WS-PAFYLLNKVANT                                  
059900                         WS-OVERLAGERPKT                                  
060000                         WS-REFPB-TOT                                     
060100                         WS-KVPB-TOT                                      
060200                         WS-KVPB-TOT-NOSEAS                               
060300                         WS-KVPB-REF                                      
060400                         WS-KVPBREOI-DAY                                  
060500                         WS-KVPB-REF-DAY-PER-I                            
060600                         WS-KVPB-REF-DAY-PER-II                           
060700                         WS-KVPB-REF-DAY-PER-III                          
060800                         WS-KVPB-REF-DAY-PER-VI                           
060900                         WS-KVPB-REF-DAY-PER-V                            
061000                         WS-KVPB-REF-DAY-PER-IV                           
061100                         WS-KVPB-REF-DAY-BINN-5                           
061200                         WS-KVPB-REF-DAY-BINN-6                           
061300                         WS-KVPB-REF-DAY-BINN-7                           
061400                         WS-KVPB-REF-DAY-BNS-5                            
061500                         WS-KVPB-REF-DAY-BNS-6                            
061600                         WS-KVPB-REF-DAY-BNS-7                            
061700                         WS-REPPFAK                                       
061800                         WS-REPPFAK-1                                     
061810                         WS-BIN-TIYYMMDD                                  
061811                         WS-BIN-TIYYWWD                                   
061820                         WS-FFC-1                                         
061830                         WS-FFC-2                                         
061840                         WS-FFC-1-DATE                                    
061850                         WS-FFC-2-DATE                                    
061860                         WS-KVDAYS-BIN                                    
061870                         WS-BINNDATE                                      
061900     MOVE NEJ         TO SW-LOCAL                                         
061910     MOVE 1.00        TO WS-AVR-SEAS-3MONTH                               
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300                                                                          
062400 E-PUNKTER-ARTIKEL-UTAN-PB SECTION.                                       
062500                                                                          
062600                                                                          
062700     PERFORM S09-LETA-I-TABELL                                            
062800     PERFORM S06-HAEMTA-KLASS                                             
062900                                                                          
063000     MOVE +0 TO REFL-KVREFPKT                                             
063100                REFL-KVREFBER                                             
063200                REFL-KVREFOVL                                             
063300     .                                                                    
063400     EJECT                                                                
063500                                                                          
063600                                                                          
063700 F-BERAKNA-PUNKTER SECTION.                                               
063800                                                                          
063900     PERFORM S07-PLATS-I-TABELL-NU                                        
064000     PERFORM S06-HAEMTA-KLASS                                             
064100     PERFORM S05-LEDTIDSBEHOV                                             
064200     PERFORM S01-PAFYLLNPUNKT                                             
064300     PERFORM S02-PAFYLLNKVANT                                             
064400     PERFORM S03-OVERLAGERPUNKT                                           
064500     .                                                                    
064600     EJECT                                                                
064700                                                                          
064800 X-LAS-2501-ANSK SECTION.                                                 
064900***                                                                       
065000*   LÄS HÄNDELSEBASEN WL2501 MED OLIKA NYCKLAR                            
065100*   ANSKAFFNINGEN HAR VÄRDE A-Z MED A FÖR GRUNDTABELLEN SOM               
065200*   ANVÄNDS NÄR EN SPECIFIK TABELL INTE FINNS.                            
065300***                                                                       
065400                                                                          
065500     IF REFL-IDDC NOT = WS-SP-IDDC                                        
065600*    - KANSKE FÖREGÅENDE GRUNDTABELL VAR FEL SORT (REFILL)                
065700     OR GRUNDTAB-2502-IDREFTAB = '0'                                      
065800*      -- LÄS GRUNDTABELL TYP ANSKAFFNING FÖR DETTA DC                    
065900       MOVE 'A'  TO W-IDREFTAB                                            
066000       PERFORM IMS-GU-2502                                                
066100       IF SEGMENT-SAKNAS                                                  
066200           MOVE REFL-IDDC TO TABELL-SAKNAS-DC                             
066300           MOVE TE-TABELL-SAKNAS TO FELTEXT-STR                           
066400           DISPLAY FELTEXT                                                
066500           PERFORM S99-ABEND                                              
066600       ELSE                                                               
066700          MOVE 2502-WDGX2502 TO SPARAREA-GRUNDTABELL                      
066800       END-IF                                                             
066900     END-IF                                                               
067000                                                                          
067100     IF REFL-IDDC NOT = WS-SP-IDDC                                        
067200     OR REFL-IDREFTAB NOT = W-IDREFTAB                                    
067300       MOVE  REFL-IDREFTAB    TO WS-IDREFTAB-ID                           
067400       IF VALID-IDREFTAB-ID                                               
067500*       - LÄS ANGIVEN TABELL FÖR DETTA DC.                                
067600*       - KOM IHÅG OM DET ÄR GRUNDTABELLEN ELLER ANGIVEN TABELL           
067700*       - SOM SKA ANVÄNDAS.                                               
067800         MOVE REFL-IDREFTAB TO W-IDREFTAB                                 
067900         PERFORM IMS-GU-2502                                              
068000         IF SEGMENT-FINNS                                                 
068100           MOVE 2502-WDGX2502 TO SPARAREA-AKTUELL-TABELL                  
068200         ELSE                                                             
068300*          - SÄKERSTÄLL ATT AKTTAB INTE ANVÄNDS                           
068400           MOVE '?' TO AKTTAB-2502-IDREFTAB                               
068500         END-IF                                                           
068600       ELSE                                                               
068700*        -- INVALID IDREFTAB-VÄRDE                                        
068800         MOVE REFL-IDDC     TO TABELL-FEL-DC                              
068900         MOVE TE-TABELL-FEL TO FELTEXT-STR                                
069000         DISPLAY FELTEXT                                                  
069100         PERFORM S99-ABEND                                                
069200       END-IF                                                             
069300     END-IF                                                               
069400                                                                          
069500*    - KOM IHÅG OM DET ÄR GRUNDTABELLEN ELLER ANGIVEN TABELL              
069600*    - SOM SKA ANVÄNDAS.                                                  
069700     IF REFL-IDREFTAB = AKTTAB-2502-IDREFTAB                              
069800       MOVE NEJ TO GRUNDTABELL-SW                                         
069900     ELSE                                                                 
070000       MOVE JA  TO GRUNDTABELL-SW                                         
070100     END-IF                                                               
070200                                                                          
070300*    - KOM IHÅG TILL NÄSTA ANROP FÖR VILKET DC SOM TABELLER LÄSTS         
070400     MOVE REFL-IDDC TO WS-SP-IDDC                                         
070500     .                                                                    
070600     EJECT                                                                
070700                                                                          
070800                                                                          
070900 S01-PAFYLLNPUNKT SECTION.                                                
071000                                                                          
071010     PERFORM S01C-SEASON-SAFTEY-STOCK                                     
071020     IF FFC-EXIST-INT-PGM                                                 
071021     AND SEASON                                                           
071030       PERFORM S01D-CHECK-FFC                                             
071040     END-IF                                                               
071100     PERFORM S01B-PAFYLLNPKT-NORMAL                                       
071200     IF REFL-IN-KVREFPKT > 0                                              
071300*MANUELL PÅFYLLNADSPUNKT GÄLLER                                           
071400        MOVE REFL-IN-KVREFPKT TO WS-PAFYLLNPKT                            
071500                                 REFL-KVREFPKT                            
071600     END-IF                                                               
071610*    IF W-IDARTNR = 42100                                                 
071620*    AND W-IDDC = '41'                                                    
071630*      CALL FELLOG                                                        
071640*    END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000                                                                          
072100 S01B-PAFYLLNPKT-NORMAL SECTION.                                          
072200                                                                          
072210     IF WS-KVPB-REF < 0.6                                                 
072211     OR REFL-FLFLYG = JA                                                  
072220       MOVE 1.00     TO WS-RESSFAC                                        
072230     END-IF                                                               
072240                                                                          
072300     IF GRUNDTABELL                                                       
072400        IF GRUNDTAB-2502-KDREFPKT-LIM(WS-RADIX, WS-PR-KOLIX) = 'D'        
072500                                                                          
072600          MOVE GRUNDTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX)             
072700                             TO WS-KVREFLIM                               
072800          PERFORM S11-TAB-BEHOV-PKT                                       
072900          IF WS-TABELLBEHOV = ZERO                                        
073000          AND NO-FFC                                                      
073100             MOVE ZERO       TO WS-PAFYLLNPKT                             
073200          ELSE                                                            
073300            IF (DCS-NDC-CN AND REFL-IDDC-REF = SPACE)                     
073400            OR (DCS-USA AND REFL-IDDC-REF = SPACE)                        
073500              MOVE WS-TABELLBEHOV  TO WS-PAFYLLNPKT                       
073600            ELSE                                                          
073700              COMPUTE WS-PAFYLLNPKT ROUNDED =                             
073800                            ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)           
073900            END-IF                                                        
074000          END-IF                                                          
074100                                                                          
074200          IF WS-PAFYLLNPKT > ZERO                                         
074300          AND WS-PAFYLLNPKT < 1.0                                         
074400              MOVE 1         TO REFL-KVREFPKT                             
074500          ELSE                                                            
074600             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
074700          END-IF                                                          
074800                                                                          
074900        ELSE                                                              
075000          COMPUTE WS-TABELLBEHOV =                                        
075100              (GRUNDTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX))            
075200                                                                          
075300          IF DCS-SDC                                                      
075400            COMPUTE WS-TABELLBEHOV = WS-REPPFAK-1 *                       
075500                                     WS-TABELLBEHOV *                     
075600                                     WS-RESSFAC *                         
075601                                     WS-AVR-SEAS-3MONTH                   
075610          ELSE                                                            
075800            COMPUTE WS-TABELLBEHOV = WS-TABELLBEHOV *                     
075900                                     WS-RESSFAC *                         
075910                                     WS-AVR-SEAS-3MONTH                   
076000          END-IF                                                          
076100          IF WS-TABELLBEHOV = ZERO                                        
076200          AND NO-FFC                                                      
076300             MOVE ZERO       TO WS-PAFYLLNPKT                             
076400          ELSE                                                            
076500            IF (DCS-NDC-CN AND REFL-IDDC-REF = SPACE)                     
076600            OR (DCS-USA AND REFL-IDDC-REF = SPACE)                        
076700              MOVE WS-TABELLBEHOV  TO WS-PAFYLLNPKT                       
076800            ELSE                                                          
076900              COMPUTE WS-PAFYLLNPKT ROUNDED =                             
077000                            ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)           
077100            END-IF                                                        
077200          END-IF                                                          
077300                                                                          
077400          IF WS-PAFYLLNPKT > ZERO                                         
077500          AND WS-PAFYLLNPKT < 1.0                                         
077600              MOVE 1         TO REFL-KVREFPKT                             
077700          ELSE                                                            
077800             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
077900          END-IF                                                          
078000        END-IF                                                            
078100     ELSE                                                                 
078200        IF AKTTAB-2502-KDREFPKT-LIM (WS-RADIX, WS-PR-KOLIX) = 'D'         
078300                                                                          
078400          MOVE AKTTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX)               
078500                             TO WS-KVREFLIM                               
078600          PERFORM S11-TAB-BEHOV-PKT                                       
078700                                                                          
078800          IF WS-TABELLBEHOV = ZERO                                        
078900          AND NO-FFC                                                      
079000             MOVE ZERO       TO WS-PAFYLLNPKT                             
079100          ELSE                                                            
079200            IF (DCS-NDC-CN AND REFL-IDDC-REF = SPACE)                     
079300            OR (DCS-USA AND REFL-IDDC-REF = SPACE)                        
079400              MOVE WS-TABELLBEHOV  TO WS-PAFYLLNPKT                       
079500            ELSE                                                          
079600              COMPUTE WS-PAFYLLNPKT ROUNDED =                             
079700                            ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)           
079800            END-IF                                                        
079900          END-IF                                                          
080000                                                                          
080100          IF WS-PAFYLLNPKT > ZERO                                         
080200          AND WS-PAFYLLNPKT < 1.0                                         
080300              MOVE 1         TO REFL-KVREFPKT                             
080400          ELSE                                                            
080500             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
080600          END-IF                                                          
080700                                                                          
080800        ELSE                                                              
080900          COMPUTE WS-TABELLBEHOV =                                        
081000             (AKTTAB-2502-KVREFLIM (WS-RADIX, WS-PR-KOLIX))               
081100                                                                          
081200          IF DCS-SDC                                                      
081300            COMPUTE WS-TABELLBEHOV = WS-REPPFAK-1 *                       
081400                                     WS-TABELLBEHOV *                     
081500                                     WS-RESSFAC *                         
081501                                     WS-AVR-SEAS-3MONTH                   
081510          ELSE                                                            
081700            COMPUTE WS-TABELLBEHOV = WS-TABELLBEHOV *                     
081800                                     WS-RESSFAC *                         
081810                                     WS-AVR-SEAS-3MONTH                   
081900          END-IF                                                          
082000                                                                          
082100          IF WS-TABELLBEHOV = ZERO                                        
082200          AND NO-FFC                                                      
082300             MOVE ZERO       TO WS-PAFYLLNPKT                             
082400          ELSE                                                            
082500            IF (DCS-NDC-CN AND REFL-IDDC-REF = SPACE)                     
082600            OR (DCS-USA AND REFL-IDDC-REF = SPACE)                        
082700              MOVE WS-TABELLBEHOV  TO WS-PAFYLLNPKT                       
082800            ELSE                                                          
082900              COMPUTE WS-PAFYLLNPKT ROUNDED =                             
083000                            ( WS-TABELLBEHOV + WS-LEDTIDSBEHOV)           
083100            END-IF                                                        
083200          END-IF                                                          
083300          IF WS-PAFYLLNPKT > ZERO                                         
083400          AND WS-PAFYLLNPKT < 1.0                                         
083500              MOVE 1         TO REFL-KVREFPKT                             
083600          ELSE                                                            
083700             COMPUTE REFL-KVREFPKT ROUNDED = WS-PAFYLLNPKT * 1            
083800          END-IF                                                          
083900        END-IF                                                            
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084300                                                                          
084400                                                                          
084510 S01C-SEASON-SAFTEY-STOCK SECTION.                                        
084511                                                                          
084512     IF  WS-RESEASON(01) = 1.00                                           
084513     AND WS-RESEASON(02) = 1.00                                           
084514     AND WS-RESEASON(03) = 1.00                                           
084515     AND WS-RESEASON(04) = 1.00                                           
084516     AND WS-RESEASON(05) = 1.00                                           
084517     AND WS-RESEASON(06) = 1.00                                           
084518     AND WS-RESEASON(07) = 1.00                                           
084519     AND WS-RESEASON(08) = 1.00                                           
084520     AND WS-RESEASON(09) = 1.00                                           
084521     AND WS-RESEASON(10) = 1.00                                           
084522     AND WS-RESEASON(11) = 1.00                                           
084523     AND WS-RESEASON(12) = 1.00                                           
084524       MOVE NEJ TO SW-SEASON                                              
084525     ELSE                                                                 
084526       MOVE JA TO SW-SEASON                                               
084527     END-IF                                                               
084528                                                                          
084529     IF SEASON                                                            
084530       PERFORM S01CA-AVERAGE-SEASON-FACTOR                                
084531       IF WS-AVR-SEAS-3MONTH < 0.50                                       
084532         MOVE 0.50     TO WS-AVR-SEAS-3MONTH                              
084533       ELSE                                                               
084534         IF WS-AVR-SEAS-3MONTH > 1.75                                     
084535           MOVE 1.75   TO WS-AVR-SEAS-3MONTH                              
084536         END-IF                                                           
084537       END-IF                                                             
084538     END-IF                                                               
084539                                                                          
084540     .                                                                    
084541     EJECT                                                                
084542 S01CA-AVERAGE-SEASON-FACTOR SECTION.                                     
084543                                                                          
084544     MOVE WS-BIN-TIYYMMDD (3:2)   TO SEAS-IX                              
084545***IF JANUARY USE DECEMBER, JANUARY AND FEBRUARY                          
084546     IF SEAS-IX = 1                                                       
084547       COMPUTE WS-AVR-SEAS-3MONTH =                                       
084548              (WS-RESEASON (12)  +                                        
084549               WS-RESEASON (01)  +                                        
084550               WS-RESEASON (02)) / 3                                      
084551     ELSE                                                                 
084552***IF DECEMBER USE NOVEMBER, DECEMBER AND JANUARY                         
084553       IF SEAS-IX = 12                                                    
084554         COMPUTE WS-AVR-SEAS-3MONTH =                                     
084555                (WS-RESEASON (11) +                                       
084556                 WS-RESEASON (12) +                                       
084557                 WS-RESEASON (01)) / 3                                    
084558         ELSE                                                             
084559           COMPUTE WS-AVR-SEAS-3MONTH =                                   
084560                  (WS-RESEASON (SEAS-IX - 1) +                            
084561                   WS-RESEASON (SEAS-IX) +                                
084562                   WS-RESEASON (SEAS-IX + 1)) / 3                         
084563       END-IF                                                             
084564     END-IF                                                               
084568                                                                          
084572     .                                                                    
084573     EJECT                                                                
084574                                                                          
084575 S01D-CHECK-FFC SECTION.                                                  
084576                                                                          
084577**IF FUTURE FORECAST EXIST CHECK WHAT THE FORECAST                        
084578**WILL BE WHEN EXPECTED BINN DATE                                         
084579     IF WS-FFC-1-DATE < WS-BIN-TIYYWWD                                    
084580       MOVE WS-FFC-1     TO WS-KVPB-TOT-NOSEAS                            
084581     ELSE                                                                 
084582       IF WS-FFC-2-DATE NOT = ZERO                                        
084583         IF WS-FFC-2-DATE < WS-BIN-TIYYWWD                                
084584           MOVE WS-FFC-2 TO WS-KVPB-TOT-NOSEAS                            
084585         END-IF                                                           
084586       END-IF                                                             
084587     END-IF                                                               
084588     IF WS-KVPB-TOT-NOSEAS > 99999.9                                      
084589        MOVE +99999.9 TO WS-KVPB-TOT-NOSEAS                               
084590     END-IF                                                               
084591**WE HAVE TO DO A NEW SEARCH IF FFC EXISTS AND SEASON EXISTS              
084592     PERFORM S09-LETA-I-TABELL                                            
084593     .                                                                    
084594     EJECT                                                                
084595                                                                          
084596 S02-PAFYLLNKVANT SECTION.                                                
084600                                                                          
084700                                                                          
084800     IF REFL-IN-KVREFBER > 0                                              
084900*                                                                         
085000*----  MANUELL PÅFYLLNADSKVANTITET ÄR SATT                                
085100*                                                                         
085200        MOVE REFL-IN-KVREFBER TO WS-PAFYLLNKVANT                          
085300                                REFL-KVREFBER                             
085400     ELSE                                                                 
085500        IF REFL-FLWILSON = JA                                             
085600           PERFORM S02B-KVANT-M-WILSON                                    
085700        ELSE                                                              
085800           PERFORM S02C-KVANT-M-TABELL                                    
085900        END-IF                                                            
086000                                                                          
086100        IF WS-PAFYLLNKVANT > ZERO                                         
086200        AND WS-PAFYLLNKVANT < 1.0                                         
086300            MOVE 1           TO REFL-KVREFBER                             
086400        ELSE                                                              
086500           COMPUTE REFL-KVREFBER ROUNDED = WS-PAFYLLNKVANT * 1            
086600        END-IF                                                            
086700     END-IF                                                               
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100                                                                          
087200 S02B-KVANT-M-WILSON SECTION.                                             
087300                                                                          
087400*WILSONFORMEL FÖR BERÄKNING AV PÅFYLLNADSKVANT                            
087500*KVANT = ROTEN UR ((2N * P)/(U * B)                                       
087600*                                                                         
087700*     N = ÅRSBEHOV, P = ORDERSÄRKOSTNAD, U = LAGERSÄRKOSTNAD              
087800*     B = BESTÄLLNINGSPRIS                                                
087900                                                                          
088000     COMPUTE WS-N = (WS-KVPB-TOT * 12)                                    
088100                                                                          
088200     COMPUTE WS-NP = ((2 * WS-N) * 9.25)                                  
088300                                                                          
088400*                    PRARTBES * LAGERRÄNTA                                
088500                                                                          
088600***FÖR KINA LIGGER PRMATRL I PRARTBES                                     
088700     COMPUTE WS-UB = (REFL-PRARTBES * DCS-REWILSON)                       
088800*                                                                         
088900                                                                          
089000     IF WS-UB = ZERO                                                      
089100        MOVE 1.0 TO WS-UB                                                 
089200     END-IF                                                               
089300                                                                          
089400     COMPUTE WS-NP-UB = (WS-NP / WS-UB)                                   
089500                                                                          
089600     COMPUTE WS-PAFYLLNKVANT ROUNDED = (WS-NP-UB ** 0.5)                  
089700                                                                          
089800     .                                                                    
089900     EJECT                                                                
090000                                                                          
090100                                                                          
090200 S02C-KVANT-M-TABELL SECTION.                                             
090300                                                                          
090400     IF GRUNDTABELL                                                       
090500        IF GRUNDTAB-2502-KDREFPKT-KVA(WS-RADIX, WS-PR-KOLIX)              
090600                                               = 'D'                      
090700                                                                          
090800          MOVE GRUNDTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)             
090900                               TO WS-KVREFKVA                             
091000          PERFORM S12-TAB-BEHOV-KVANT                                     
091100                                                                          
091200        ELSE                                                              
091300          MOVE  GRUNDTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)            
091400                               TO WS-PAFYLLNKVANT                         
091500                                                                          
091600        END-IF                                                            
091700     ELSE                                                                 
091800        IF AKTTAB-2502-KDREFPKT-KVA (WS-RADIX, WS-PR-KOLIX) = 'D'         
091900                                                                          
092000          MOVE AKTTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)               
092100                               TO WS-KVREFKVA                             
092200          PERFORM S12-TAB-BEHOV-KVANT                                     
092300                                                                          
092400        ELSE                                                              
092500          MOVE AKTTAB-2502-KVREFKVA (WS-RADIX, WS-PR-KOLIX)               
092600                               TO WS-PAFYLLNKVANT                         
092700                                                                          
092800        END-IF                                                            
092900     END-IF                                                               
093000     .                                                                    
093100     EJECT                                                                
093200                                                                          
093300                                                                          
093400                                                                          
093500 S03-OVERLAGERPUNKT SECTION.                                              
093600                                                                          
093700***                                                                       
093800*   BASERAT PÅ ARTIKELNS PÅFYLLNADSKVANTITET BERÄKNAS                     
093900*   ÖVERLAGERPUNKTEN                                                      
094000***                                                                       
094100                                                                          
094200     COMPUTE WS-OVERLAGERPKT = ((REFL-KVREFBER * 2)                       
094300                               + REFL-KVREFPKT)                           
094400                                                                          
094500     MOVE WS-OVERLAGERPKT TO REFL-KVREFOVL                                
094600                                                                          
094700     .                                                                    
094800     EJECT                                                                
094900                                                                          
095000                                                                          
095100 S05-LEDTIDSBEHOV SECTION.                                                
095200                                                                          
095300     MOVE REFL-IN-LEADTID-BEHOV     TO WS-LEDTIDSBEHOV                    
095400     IF DCS-SDC                                                           
095500       COMPUTE WS-LEDTIDSBEHOV = WS-REPPFAK-1 *                           
095600                                WS-LEDTIDSBEHOV                           
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100 S06-HAEMTA-KLASS SECTION.                                                
096200***                                                                       
096300*   HÄMTA KLASS FRÅN TABELLEN, KAN VARA 1-12 SAMT A-I                     
096400***                                                                       
096500                                                                          
096600                                                                          
096700     MOVE WS-RADIX TO WS-KLASS-RAD                                        
096800                                                                          
096900     EVALUATE WS-PR-KOLIX                                                 
097000       WHEN 1                                                             
097100         MOVE 'A' TO WS-KLASS-KOL                                         
097200       WHEN 2                                                             
097300         MOVE 'B' TO WS-KLASS-KOL                                         
097400       WHEN 3                                                             
097500         MOVE 'C' TO WS-KLASS-KOL                                         
097600       WHEN 4                                                             
097700         MOVE 'D' TO WS-KLASS-KOL                                         
097800       WHEN 5                                                             
097900         MOVE 'E' TO WS-KLASS-KOL                                         
098000       WHEN 6                                                             
098100         MOVE 'F' TO WS-KLASS-KOL                                         
098200       WHEN 7                                                             
098300         MOVE 'G' TO WS-KLASS-KOL                                         
098400       WHEN 8                                                             
098500         MOVE 'H' TO WS-KLASS-KOL                                         
098600       WHEN OTHER                                                         
098700         MOVE 'I' TO WS-KLASS-KOL                                         
098800     END-EVALUATE                                                         
098900                                                                          
099000     MOVE WS-KLASS TO REFL-KLASS                                          
099100     .                                                                    
099200     EJECT                                                                
099300                                                                          
099400                                                                          
099500 S07-PLATS-I-TABELL-NU SECTION.                                           
099600                                                                          
099700                                                                          
099710     MOVE WS-NOLLA-RESEASON      TO WS-RESEASON-TABELL                    
099800     MOVE REFL-IDARTNR    TO W-IDARTNR                                    
099900     PERFORM IMS-GU-WDK711                                                
100000     IF SEGMENT-FINNS                                                     
100101       COMPUTE WS-KVPB-REF = SLAG-KVPB-REF +                              
100102                             SLAG-KVPBREOI                                
100110       MOVE SLAG-REPPFAKT   TO WS-REPPFAK                                 
100200       COMPUTE WS-REPPFAK-1 = 1 - WS-REPPFAK                              
100201       MOVE NEJ TO SW-FFC-INT-PGM                                         
100202                                                                          
100203       MOVE +1 TO PER-IX                                                  
100204       PERFORM UNTIL PER-IX > 12                                          
100205          MOVE SLAG-RESEASON (PER-IX) TO WS-RESEASON (PER-IX)             
100206          ADD +1 TO PER-IX                                                
100207       END-PERFORM                                                        
100208                                                                          
100210       PERFORM IMS-GNP-WDK727                                             
100220       IF SEGMENT-FINNS                                                   
100221         MOVE PROG-KVPB-JUST(1)   TO WS-FFC-1                             
100222         MOVE PROG-KVPB-JUST(2)   TO WS-FFC-2                             
100223         MOVE PROG-TIPBJUST(1)    TO WS-FFC-1-DATE                        
100224         MOVE PROG-TIPBJUST(2)    TO WS-FFC-2-DATE                        
100225         MOVE JA TO SW-FFC-INT-PGM                                        
100230       END-IF                                                             
100300     END-IF                                                               
100400*************                                                             
100500     MOVE REFL-PRARTBES TO WS-PRARTBES                                    
100600                                                                          
100700     IF WS-KVPB-TOT-NOSEAS > 99999.9                                      
100800        MOVE +99999.9 TO WS-KVPB-TOT-NOSEAS                               
100900     END-IF                                                               
101000     IF REFL-PRARTBES > 9999999.99                                        
101100        MOVE +9999999.99 TO WS-PRARTBES                                   
101200     END-IF                                                               
101300     PERFORM S09-LETA-I-TABELL                                            
101400     .                                                                    
101500     EJECT                                                                
101600                                                                          
101700 S09-LETA-I-TABELL SECTION.                                               
101800                                                                          
101900     IF GRUNDTABELL                                                       
102000                                                                          
102100*****   TABELLEN FINNS I DEN SPARADE AREAN FÖR TABELL = 0                 
102200                                                                          
102300        MOVE 1 TO WS-RADIX                                                
102400        IF WS-KVPB-TOT = ZERO                                             
102500           CONTINUE                                                       
102600        ELSE                                                              
102700           PERFORM UNTIL ( GRUNDTAB-2502-PRARTBES (WS-RADIX)              
102800                                        = WS-PRARTBES                     
102900                       OR GRUNDTAB-2502-PRARTBES (WS-RADIX)               
103000                                        > WS-PRARTBES)                    
103100             ADD 1 TO WS-RADIX                                            
103200           END-PERFORM                                                    
103300        END-IF                                                            
103400                                                                          
103500        MOVE 1 TO WS-PR-KOLIX                                             
103600        PERFORM UNTIL                                                     
103700                (GRUNDTAB-2502-KVPB-REF( WS-PR-KOLIX)                     
103800                                        = WS-KVPB-TOT-NOSEAS              
103900              OR GRUNDTAB-2502-KVPB-REF(WS-PR-KOLIX)                      
104000                                        > WS-KVPB-TOT-NOSEAS)             
104100                                                                          
104200          ADD 1 TO WS-PR-KOLIX                                            
104300        END-PERFORM                                                       
104400                                                                          
104500     ELSE                                                                 
104600                                                                          
104700*****   TABELLEN FINNS I DEN SPARADE AREAN FÖR SAMMA SOM FÖREG.           
104800                                                                          
104900        MOVE 1 TO WS-RADIX                                                
105000***FÖR KINA LIGGER PRMATRL I PRARTBES                                     
105100        IF WS-KVPB-TOT = ZERO                                             
105200           CONTINUE                                                       
105300        ELSE                                                              
105400           PERFORM UNTIL (AKTTAB-2502-PRARTBES (WS-RADIX)                 
105500                                        = WS-PRARTBES                     
105600                       OR AKTTAB-2502-PRARTBES (WS-RADIX)                 
105700                                        > WS-PRARTBES )                   
105800             ADD 1 TO WS-RADIX                                            
105900           END-PERFORM                                                    
106000        END-IF                                                            
106100                                                                          
106200        MOVE 1 TO WS-PR-KOLIX                                             
106300        PERFORM UNTIL                                                     
106400                (AKTTAB-2502-KVPB-REF( WS-PR-KOLIX)                       
106500                                        = WS-KVPB-TOT-NOSEAS              
106600              OR AKTTAB-2502-KVPB-REF( WS-PR-KOLIX)                       
106700                                        > WS-KVPB-TOT-NOSEAS)             
106800                                                                          
106900          ADD 1 TO WS-PR-KOLIX                                            
107000        END-PERFORM                                                       
107100     END-IF                                                               
107200     .                                                                    
107300     EJECT                                                                
107400                                                                          
107500                                                                          
107600 S11-TAB-BEHOV-PKT SECTION.                                               
107700                                                                          
107800     IF DCS-CHINA OR DCS-JAPAN OR DCS-ENGLAND OR DCS-INDIA                
107900                  OR DCS-EMIRATES                                         
108000       COMPUTE WS-TABELLBEHOV ROUNDED =                                   
108100               WS-KVREFLIM * ((UTIL-KVPB-TOT-NOSEAS / 4.33) / 6)          
108200     ELSE                                                                 
108300       COMPUTE WS-TABELLBEHOV ROUNDED =                                   
108400               WS-KVREFLIM * ((UTIL-KVPB-TOT-NOSEAS / 4.33) / 5)          
108500     END-IF                                                               
108600     IF DCS-SDC                                                           
108700        COMPUTE WS-TABELLBEHOV = WS-REPPFAK-1 *                           
108800                                 WS-TABELLBEHOV *                         
108900                                 WS-RESSFAC *                             
108901                                 WS-AVR-SEAS-3MONTH                       
108910     ELSE                                                                 
109100        COMPUTE WS-TABELLBEHOV = WS-TABELLBEHOV *                         
109200                                 WS-RESSFAC *                             
109210                                 WS-AVR-SEAS-3MONTH                       
109300     END-IF                                                               
109400     .                                                                    
109500     EJECT                                                                
109600                                                                          
109700                                                                          
109800 S12-TAB-BEHOV-KVANT SECTION.                                             
109900                                                                          
110000*                                                                         
110100     IF DCS-CHINA OR DCS-JAPAN OR DCS-ENGLAND OR DCS-INDIA                
110200                  OR DCS-EMIRATES                                         
110300       COMPUTE WS-PAFYLLNKVANT ROUNDED =                                  
110400               WS-KVREFKVA * WS-KVPB-REF-DAY-BINN-6                       
110500     ELSE                                                                 
110600       COMPUTE WS-PAFYLLNKVANT ROUNDED =                                  
110700               WS-KVREFKVA * WS-KVPB-REF-DAY-BINN-5                       
110800     END-IF                                                               
110900     .                                                                    
111000     EJECT                                                                
111100                                                                          
111200 S99-ABEND SECTION.                                                       
111300                                                                          
111400     SKIP2                                                                
111500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
111600     .                                                                    
111700     EJECT                                                                
111800                                                                          
111900                                                                          
112000                                                                          
112100* --- IMS SEKTIONER ---                                                   
112200     SKIP3                                                                
112300                                                                          
112400 IMS-GU-2502 SECTION.                                                     
112500     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
112600          DELIMITED BY SIZE INTO SSA1                                     
112700     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
112800          DELIMITED BY SIZE INTO SSA2                                     
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GU 2501-PCB DLI-IO-AREA SSA1 SSA2                 
113100     MOVE 2501-STATUS-CODE TO STATUS-WS                                   
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     EJECT                                                                
113500                                                                          
113600 IMS-GU-WDB601    SECTION.                                                
113700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
113800          DELIMITED BY SIZE INTO SSA1                                     
113900     MOVE '  ' TO GODK-STATUSKODER                                        
114000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
114100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
114200     PERFORM IMS-STATUSKONTROLL                                           
114300     .                                                                    
114400                                                                          
114500 IMS-GNP-WDB616    SECTION.                                               
114600                                                                          
114700     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
114800          DELIMITED BY SIZE INTO SSA1                                     
114900     MOVE '  ' TO GODK-STATUSKODER                                        
115000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B616 SSA1                
115100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
115200     PERFORM IMS-STATUSKONTROLL                                           
115300     .                                                                    
115400     EJECT                                                                
115500                                                                          
115600 IMS-GU-WDK711 SECTION.                                                   
115700                                                                          
115800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
115900          DELIMITED BY SIZE INTO SSA1                                     
116000     STRING 'WDK711  (IDDC     =' W-IDDC-2-X ')'                          
116100          DELIMITED BY SIZE INTO SSA2                                     
116200     MOVE '  GE' TO GODK-STATUSKODER                                      
116300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
116400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
116500     PERFORM IMS-STATUSKONTROLL                                           
116600     .                                                                    
116700     EJECT                                                                
116701                                                                          
116710 IMS-GNP-WDK727 SECTION.                                                  
116720                                                                          
116730     MOVE 'WDK727 ' TO SSA1                                               
116740     MOVE '  GEGP' TO GODK-STATUSKODER                                    
116750     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1                   
116760     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
116770     PERFORM IMS-STATUSKONTROLL                                           
116780     .                                                                    
116790     SKIP3                                                                
116800                                                                          
116900 IMS-STATUSKONTROLL SECTION.                                              
117000                                                                          
117100     SET STATUS-IX TO 1                                                   
117200     SEARCH GODK-STATUS                                                   
117300       AT END                                                             
117400         STRING 'OTILLÅTEN STATUSKOD FRÅN IMS: ' STATUS-WS                
117500         DELIMITED BY SIZE INTO FELTEXT                                   
117600         CALL FELLOG                                                      
117700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
117800         CONTINUE                                                         
117900     END-SEARCH                                                           
118000     .                                                                    

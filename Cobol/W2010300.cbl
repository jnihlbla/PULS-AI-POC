000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W2010300.                                            
000400*AUTHOR.             IDK, GÖTEBORG.                                       
000500*                    (TOMMY JOHANSSON).                                   
000600*DATE-WRITTEN.       FEBR 1979.                                           
000700*                    JAN 1986.                                            
000800*    SKIP2                                                                
000900*REMARKS.                                                                 
001000*    FUNCTION.                                                            
001100*            TP-PROGRAM FÖR ANSKAFFNING (LEVERANSPLAN)                    
001200*            PROGRAMMET SKÖTER FRÅGEDELEN AV LEVERANS-                    
001300*            PLANEBILDEN, UPPDATERING AV REGISTER UTFÖRES                 
001400*            AV PROGRAM W2016300.                                         
001500*            FRÅGEPROGRAMMET OMFATTAR FÖLJANDE DELAR:                     
001600*                - UTLÄGG AV NY BILD.                                     
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T103     FRÅGEDEL                                 
001900*        MID:         W2I10301                                            
002000*    UTDATA.                                                              
002100*        MOD:         W2O10301                                            
002200*    SUBPROGRAM.                                                          
002300*        WDATKONV                                                         
002400*        FELLOG                                                           
002500*                                                                         
002600*   ÄNDRINGAR:                                                            
002700*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002800*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002900*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003000*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003100*                                                                         
003200*        07-01-23. TILLAGT FUNKTION FÖR ATT TA EMOT TRANS FRÅN            
003300*                  2147-BILDEN FÖR BEHANDLING AV LEV.PL.FÖRSLAG.          
003400*                  PF8 SKALL NUMERA GE NÄSTA FÖRSLAG PÅ KÖ.               
003500*                  -- OBS PF8 får inte ha annan funktion.  /CE            
003600*        07-02-06. Samtidigt som 07-01-23, fixat till de ställen          
003700*                  som förorsakar RC=04 vid kompilering.    /CE           
003800*                                                                         
003900*      2013-03-13  E'TRACKER 10143273 CHINA  LOCAL SOURCING               
004000*                  Lagt till IDDC på WDD601 och WW20147S cpy.             
004100*                                                                         
004200*                                                                         
004300     EJECT                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP2                                                                
004600 DATA DIVISION.                                                           
004700     SKIP2                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP3                                                                
005000*    -COPY WY2000W7                                                       
005100     SKIP3                                                                
005200*    -COPY WY2000W3                                                       
005300     SKIP3                                                                
005400*    -COPY WY2000W9                                                       
005500     SKIP3                                                                
005600 01  FILLER                  PIC X(8)  VALUE 'FELTEXT='.                  
005700 77  FELTEXT                 PIC X(80) VALUE SPACE.                       
005800 77  AVROP-INL-PASSERAT-SW   PIC X(01) VALUE 'N'.                         
005900                                                                          
006000 01  FILLER                  PIC X(11) VALUE 'IDARTNR-WS='.               
006100 77  IDARTNR-WS              PIC X(9).                                    
006200                                                                          
006300 01  FILLER                  PIC X(13) VALUE 'WS-IDARTNR-9='.             
006400 77  WS-IDARTNR-9-RIGHT      PIC X(9)  VALUE SPACE JUST RIGHT.            
006500                                                                          
006600 01  FILLER                  PIC X(11) VALUE 'IDLEVNR-WS='.               
006700 77  IDLEVNR-WS              PIC X(5).                                    
006800                                                                          
006900 01  FILLER                  PIC X(13) VALUE 'WS-IDLEVNR-8='.             
007000 77  WS-IDLEVNR-8            PIC X(8)  VALUE SPACE.                       
007100                                                                          
007200 01  FILLER                  PIC X(15) VALUE 'KDBEHX-PLAN-WS='.           
007300 77  KDBEHX-PLAN-WS          PIC X(1).                                    
007400                                                                          
007500 77  JA                      PIC X(1)    VALUE 'J'.                       
007600 77  YES                     PIC X(1)    VALUE 'Y'.                       
007700 77  ELLER                   PIC X(1)    VALUE '!'.                       
007800 77  NEJ                     PIC X(1)    VALUE 'N'.                       
007900 77  OCH                     PIC X(1)    VALUE '&'.                       
008000 77  FEL                     PIC X(1)    VALUE 'F'.                       
008100 77  VIP-ARTIKEL             PIC X(1)    VALUE 'V'.                       
008200 77  STRECK                  PIC X(1)    VALUE '-'.                       
008300 77  SPIND                   PIC S9(9)   VALUE +0   COMP SYNC.            
008400 77  ASTERISK                PIC X(1)    VALUE '*'.                       
008500 77  PARENTES                PIC X(1)    VALUE ')'.                       
008600 77  GALLANDE                PIC X(1)    VALUE 'G'.                       
008700 77  FORSLAG                 PIC X(1)    VALUE 'F'.                       
008800 77  AAVV                    PIC X(4)    VALUE 'ÅÅVV'.                    
008900 77  YYWW                    PIC X(4)    VALUE 'YYWW'.                    
009000 77  KOLON                   PIC X(1)    VALUE ':'.                       
009100 77  W-NY-GAMMAL             PIC X(1)    VALUE SPACE.                     
009200 77  W-NY-INDATA             PIC X(1)    VALUE 'N'.                       
009300 77  W-GAMMAL-INDATA         PIC X(1)    VALUE 'G'.                       
009400                                                                          
009500 77  MAX-ANT-ORSAKSKODER     PIC S9(9)   VALUE +29   COMP SYNC.           
009600 77  MAX-ANT-PERIODER        PIC S9(9)   VALUE +12   COMP SYNC.           
009700 77  MAX-ANT-VECKOR-I-TAB    PIC S9(9)   VALUE +5    COMP SYNC.           
009800 77  MAX-ANT-PERIODER-I-TAB  PIC S9(9)   VALUE +10   COMP SYNC.           
009900 77  MAX-ANT-GAMLA-AVROP     PIC S9(9)   VALUE +5    COMP SYNC.           
010000 77  MAX-ANT-INDATA-FLT      PIC S9(9)   VALUE +15   COMP SYNC.           
010100 77  MAX-MOD-LENGD           PIC S9(9)   VALUE +1290 COMP SYNC.           
010200 01  WS-DAGENS-AAAAMMDD      PIC 9(8).                                    
010300 01  WS-JMFR-AAAAMMDD        PIC 9(8).                                    
010400 01  FILLER REDEFINES WS-JMFR-AAAAMMDD.                                   
010500  03 FILLER                  PIC 9(2).                                    
010600  03 WS-JMFR-AA              PIC 9(2).                                    
010700  03 FILLER                  PIC 9(4).                                    
010800 01  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                     
010900 01  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                     
011000 01  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.               
011100 01  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                      
011200 01  TRAFF                   PIC X(1)    VALUE SPACE.                     
011300                                                                          
011400*01  -COPY WWDCKONS                                                       
011500                                                                          
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER                  PIC  X(11)  VALUE 'BYTES-DIST='.             
011900 01  TEST-IDDISTR            PIC  9(5)   COMP-3.                          
012000*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
012100     EJECT                                                                
012200                                                                          
012300 01  FILLER                  PIC  X(10)  VALUE 'BYTES-ART='.              
012400 01  TEST-IDARTNR            PIC  9(9)   COMP-3 VALUE ZERO.               
012500*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
012600     EJECT                                                                
012700*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
012800     EJECT                                                                
012900                                                                          
013000 01  FILLER                      PIC X(10)   VALUE 'W-IDTRANS='.          
013100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013200     88  EGEN-MID                            VALUE '2103'.                
013300*                                                                         
013400*                                         GENERELLA SUBPROGRAM            
013500 01  GENERELLA-SUBPROGRAM.                                                
013600     03  WMEDKONV            PIC X(8)    VALUE 'WMEDKONV'.                
013700     03 WDATKONV             PIC X(8)    VALUE 'WDATKONV'.                
013800     03 CBLTDLI              PIC X(8)    VALUE 'CBLTDLI '.                
013900     03 FELLOG               PIC X(8)    VALUE 'FELLOG  '.                
014000     03 W005INIT             PIC X(8)    VALUE 'W005INIT'.                
014100     03 W222PBTO             PIC X(8)    VALUE 'W222PBTO'.                
014200     03 W009VADD             PIC X(8)    VALUE 'W009VADD'.                
014300     03 WZ20DAYS             PIC X(8)    VALUE 'WZ20DAYS'.                
014400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014500*01 -COPY WMEDAREA                                                        
014600     SKIP3                                                                
014700*    --- PARAMETRAR TILL SUBPROGRAM W222PBTO                              
014800*01  -COPY W222PBTO                                                       
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS   '.         
015100*   -COPY WZ20DAYS                                                        
015200     EJECT                                                                
015300**** VARIABLER TILL W009VADD****                                          
015400 01  W009VADDW-AAVV              PIC S9(5)    COMP-3.                     
015500 01  W009VADDW-ANTAL             PIC S9(3)    COMP-3.                     
015600     EJECT                                                                
015700                                                                          
015800*                                         NYCKELFÄLT FÖR LÄSNING          
015900*                                          AV DATABASER                   
016000 01  W-IDARTNR-X.                                                         
016100     03  W-IDARTNR           PIC S9(9)               COMP-3.              
016200 01  W-WDD901KY-X.                                                        
016300     03  W-IDARTNR-D9        PIC S9(9)               COMP-3.              
016400     03  W-IDDC-D9           PIC X(2).                                    
016500 01  W-IDLEVNR-X.                                                         
016600     03  W-IDLEVNR           PIC X(5).                                    
016700 01  W-KDCLAGER-X.                                                        
016800     03  W-KDCLAGER          PIC S9(1)              COMP-3.               
016900 01  W-KDAVROP-X.                                                         
017000     03  W-KDAVROP           PIC S9(1)               COMP-3.              
017100 01  W-IDSKYLT-X.                                                         
017200     03  W-IDSKYLT           PIC  X(3)   VALUE 'S  '.                     
017300 01  W-WDGXKEY-2223-X.                                                    
017400     03  W-IDHTYP            PIC X(4)    VALUE '2223'.                    
017500     03  W-IDANSK            PIC S9(3)   VALUE ZERO COMP-3.               
017600     03  FILLER              PIC X(24)   VALUE LOW-VALUE.                 
017700 01  W-FLNYLARM-X.                                                        
017800     03  W-FLNYLARM          PIC X       VALUE 'J'.                       
017900 01  W-WDGXKEY-2231-X.                                                    
018000     03  FILLER              PIC X(4)    VALUE '2231'.                    
018100     03  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
018200 01  W-WDGXKEY-2232-X.                                                    
018300     03  W-IDANSK-L          PIC S9(3)   VALUE ZERO COMP-3.               
018400     03  FILLER              PIC X(3)    VALUE LOW-VALUE.                 
018500 01  W-WDGXKEY-IDLEVNR.                                                   
018600     03  X-IDLEVNR           PIC X(5).                                    
018700 01  W-WDGXKEY-2215-X.                                                    
018800     03  W-WDGX-KEY          PIC X(4)    VALUE '2215'.                    
018900     03  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
019000 01  W-IDARTNR-WDA9-X.                                                    
019100     03  W-IDARTNR-WDA9      PIC S9(9)               COMP-3.              
019200 01  W-IDDISTR-WDA9-X.                                                    
019300     03  W-IDDISTR-WDA9      PIC S9(5)               COMP-3.              
019400                                                                          
019500 01  W-WDD7A1KY-MIN.                                                      
019600     03  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
019700     03  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
019800     03  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
019900                                                                          
020000 01  W-WDD7A1KY-MAX.                                                      
020100     03  W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.                  
020200     03  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.            
020300     03  FILLER            PIC S9(3)  COMP-3 VALUE +999.                  
020400                                                                          
020500     EJECT                                                                
020600*    Nycklar till levpl.förslags-kö-basen                                 
020700 01  W-WDD601KY-MIN-X.                                                    
020800        03 W-IDDC-D6-MIN       PIC X(2)  Value Low-Value.                 
020900        03 W-IDLEVNR-D6-MIN    PIC X(5)  Value Low-Value.                 
021000        03 W-IDARTNR-D6-MIN    PIC S9(9) Value Zero COMP-3.               
021100        03 W-IDANSK-D6-MIN-X.                                             
021200          05 W-IDANSK-D6-MIN   PIC S9(3) Value Zero COMP-3.               
021300                                                                          
021400 01  W-WDD601KY-MAX-X.                                                    
021500        03 W-IDDC-D6-MAX       PIC X(2)  Value High-Value.                
021600        03 W-IDLEVNR-D6-MAX    PIC X(5)  Value High-Value.                
021700        03 W-IDARTNR-D6-MAX    PIC S9(9) Value +999999999 COMP-3.         
021800        03 W-IDANSK-D6-MAX-X.                                             
021900          05 W-IDANSK-D6-MAX   PIC S9(3) Value +999 COMP-3.               
022000                                                                          
022100 01  W-IDANSK-MIN-X.                                                      
022200        03  W-IDANSK-MIN       PIC S9(3) Value Zero COMP-3.               
022300 01  W-IDANSK-MAX-X.                                                      
022400        03  W-IDANSK-MAX       PIC S9(3) Value +999 COMP-3.               
022500 01  W-IDARTNR-D6-X.                                                      
022600        03  W-IDARTNR-D6       PIC S9(9) Value Zero COMP-3.               
022700 01  W-KDLPORS-D6-X.                                                      
022800         05  W-KDLPORS-D6      PIC S9(3) Value Zero COMP-3.               
022900 01  W-KDLEVPLF-D6-X.                                                     
023000         05  W-KDLEVPLF-D6     PIC X(1)  Value Space.                     
023100 01  W-KDNOTTYP-X.                                                        
023200         05  W-KDNOTTYP        PIC S9(01) COMP-3 VALUE ZERO.              
023300                                                                          
023400     EJECT                                                                
023500 01  SWITCHAR.                                                            
023600     03  SW-MSG-OK           PIC X(1)    VALUE 'N'.                       
023700     03  SW-FAELT-IFYLLT     PIC X(1)    VALUE 'N'.                       
023800     03  SW-LEVSEGM-FINNS    PIC X(1)    VALUE 'N'.                       
023900     03  SW-HUVUDLEVERANTOER PIC X(1)    VALUE 'N'.                       
024000     03  SW-HOPP-2139        PIC X(1)    VALUE 'N'.                       
024100                                                                          
024200 01  SW-2147-F9              PIC X(1)    VALUE 'N'.                       
024300     88  2147-F9                         VALUE 'J'.                       
024400                                                                          
024500 01  SW-INDATA               PIC X(1).                                    
024600     88  INDATA-OK                       VALUE 'J'.                       
024700                                                                          
024800 01  SW-FEL11               PIC X(1)    VALUE 'N'.                        
024900     88 FEL11                           VALUE 'J'.                        
025000 01  SW-FEL12               PIC X(1)    VALUE 'N'.                        
025100     88 FEL12                           VALUE 'J'.                        
025200                                                                          
025300 01  W-TESTA-INDATA          PIC X(1).                                    
025400     88  W-INDATA-FINNS                  VALUE 'J'.                       
025500     SKIP3                                                                
025600*    --- Arbetsfält för selektion av WDD601-poster                        
025700 01  FILLER                      PIC X(10)   VALUE 'GODK-POST='.          
025800 77  GODK-POST-SW                PIC X       VALUE 'N'.                   
025900     88  GODK-POST                           VALUE 'J'.                   
026000     88  EJ-GODK-POST                        VALUE 'N'.                   
026100                                                                          
026200*                                          REG-INFO FÖR KONTOLL AV        
026300*                                          UPPDAT.DATA REDIGERING         
026400*                                          AV UPPD. BILD                  
026500 01  W-REGISTER-INFO.                                                     
026600     03  W-MATINFO-KDAVT     PIC S9(3)               COMP-3.              
026700     03  W-MATINFO-KDLPSP    PIC S9(1)               COMP-3.              
026800     03  W-OMSPEC-KVBEST-PL  PIC S9(7)               COMP-3.              
026900     03  W-OMSPEC-TISPECST   PIC S9(7)               COMP-3.              
027000     03  W-LEVNR-KVBR        PIC S9(7)               COMP-3.              
027100     EJECT                                                                
027200 01  ARBETSAREOR.                                                         
027300                                                                          
027400     03  INDX                PIC S9(4)  VALUE +0     COMP SYNC.           
027500     03  MAX-INDX            PIC S9(4)  VALUE +15    COMP SYNC.           
027600     03  IX                  PIC S9(9)               COMP SYNC.           
027700     03  IX-PLUS-1           PIC S9(9)               COMP SYNC.           
027800     03  IX-TOT              PIC S9(9)               COMP SYNC.           
027900     03  IX-GAM              PIC S9(9)               COMP SYNC.           
028000     03  IX-VECKA            PIC S9(9)               COMP SYNC.           
028100     03  IX-PP               PIC S9(9)               COMP SYNC.           
028200     03  IY                  PIC S9(9)               COMP SYNC.           
028300     03  IY-PERIOD           PIC S9(9)               COMP SYNC.           
028400     03  PIX                 PIC  9(2).                                   
028500     03  IX-AAPP             PIC  9(2).                                   
028600     03  IX-KOLL             PIC  9(2).                                   
028700     03  IX-SEASON           PIC S9(9)               COMP SYNC.           
028800     SKIP3                                                                
028900     03 WS-IDANSK-RED-AREA.                                               
029000           05 WS-IDANSK-TO-RED       Pic 9(3).                            
029100           05 Filler Redefines WS-IDANSK-TO-RED.                          
029200               07 WS-IDANSK-TO-2  PIC 9(2).                               
029300               07 WS-IDANSK-TO-3  PIC 9.                                  
029400     SKIP3                                                                
029500     03 FILLER               PIC X(16)   VALUE 'W-AVROP-TAB-RAD'.         
029600     03 W-AVROP-TAB-RAD      OCCURS 11.                                   
029700         05  W-PERIOD-TAB-RAD PIC 9(2).                                   
029800         05  W-AVROP-TAB-KOL OCCURS 5.                                    
029900             10  W-AVROP-TAB-AENDRAT                                      
030000                             PIC X.                                       
030100             10  W-KVAVROP-TAB                                            
030200                             PIC S9(7)               COMP-3.              
030300             10  W-TIAVROP-AVS-TAB                                        
030400                             PIC S9(3)               COMP-3.              
030500             10  W-AVROP-TAB-INL-PAST                                     
030600                             PIC X(01).                                   
030700                                                                          
030800     03  W-AVROP-GAMLA       OCCURS 5.                                    
030900         05  W-AVROP-GAM-AENDRAT                                          
031000                             PIC X.                                       
031100         05  W-KVAVROP-GAM   PIC S9(7)               COMP-3.              
031200         05  W-TIAVROP-AVS-GAM                                            
031300                             PIC  9(4).                                   
031400         05  W-AVROP-GAM-INL-PAST                                         
031500                             PIC X(01).                                   
031600                                                                          
031700     03  W-ORSKAS-TAB        OCCURS 3.                                    
031800         05  W-ORSAK-AENDRAD PIC X.                                       
031900         05  W-ORSAK-TAB-KOD PIC 9(2).                                    
032000     SKIP3                                                                
032100     03  W-KDBEHX-PLAN       PIC X.                                       
032200     03  W-SKILJETECKEN      PIC X.                                       
032300     03  W-KVAVROP-ACC       PIC S9(7)               COMP-3.              
032400     03  W-ANTAL-LEVNR       PIC S9(3)               COMP-3.              
032500                                                                          
032600     03  W-HUVUDLEVNR        PIC X(5).                                    
032700     03  W-TILEVPL           PIC S9(7)               COMP-3.              
032800     03  W-KVBR              PIC S9(7)               COMP-3.              
032900     03  W-KVPB-PLAN         PIC S9(6)V9             COMP-3.              
033000     03  W-KVPB-SATS         PIC S9(6)V9             COMP-3.              
033100                                                                          
033200     03  W-DATUM-AKTUELLT    PIC S9(5)               COMP-3.              
033300     03  W-DATUM-AKTUELLT-AAAAVV PIC  9(6).                               
033400                                                                          
033500     03  W-DATUM-AKTUELLT-AAAAVVD PIC  9(7)  VALUE ZERO.                  
033600     03  WS-TIAVRDAT-INL-AAAAVVD  PIC  9(7)  VALUE ZERO.                  
033700                                                                          
033800     03  W-DATUM-AAVV        PIC 9(4).                                    
033900     03  FILLER              REDEFINES W-DATUM-AAVV.                      
034000         05  W-DATUM-AA      PIC 9(2).                                    
034100         05  W-DATUM-VV      PIC S9(2).                                   
034200                                                                          
034300     03  W-PER-PP            PIC 99 VALUE ZERO.                           
034400     03  W-PERIOD-AAPP       PIC 9(4).                                    
034500     03  FILLER              REDEFINES W-PERIOD-AAPP.                     
034600         05  W-PERIOD-AA     PIC 9(2).                                    
034700         05  W-PERIOD-PP     PIC 9(2).                                    
034800     03  W-PERIOD-AAPP-START PIC 9(4).                                    
034900                                                                          
035000     03  W-PER-AA            PIC S9(3)               COMP-3.              
035100     03  W-BER-PERIOD        PIC S9(5)               COMP-3.              
035200     03  W-START-PER-AAPP    PIC S9(5)               COMP-3.              
035300     03  W-START-PER-AA      PIC S9(3)               COMP-3.              
035400     03  W-START-PER-PP      PIC S9(3)               COMP-3.              
035500     03  W-AAVV-MINUS-HALVAR PIC S9(5)               COMP-3.              
035600     03 WS-AAPP              PIC 9(4)    VALUE ZERO.                      
035700     03 FILLER REDEFINES     WS-AAPP.                                     
035800        05 WS-AA             PIC 9(2).                                    
035900        05 WS-PP             PIC 9(2).                                    
036000     03  W-TIOMSPEC          PIC  9(6) VALUE ZERO.                        
036100     03  W-TIOMSPEC-X   REDEFINES W-TIOMSPEC.                             
036200         05  W-TIOMSPEC-1-2  PIC X(2).                                    
036300         05  FILLER          PIC 9(4).                                    
036400                                                                          
036500     03  W-FLJIT             PIC X     VALUE SPACE.                       
036600     03  W-KDLEVPLF          PIC X     VALUE SPACE.                       
036700                                                                          
036800     03  W-KDHF              PIC S9(1) COMP-3  VALUE ZERO.                
036900     03  W-TILPSP            PIC 9(4)  VALUE ZERO.                        
037000                                                                          
037100     03  W-DAXLEVSP          PIC 9(6).                                    
037200     03  FILLER  REDEFINES W-DAXLEVSP.                                    
037300         05  FILLER      PIC 9(2).                                        
037400         05  W-TIXLEVSP  PIC 9(4).                                        
037500     03  W-DASPECST          PIC 9(6).                                    
037600     03  FILLER  REDEFINES W-DASPECST.                                    
037700         05  FILLER      PIC 9(2).                                        
037800         05  W-TISPECST  PIC 9(4).                                        
037900     03  W-DAAVROP-AVS       PIC 9(6).                                    
038000     03  FILLER  REDEFINES W-DAAVROP-AVS.                                 
038100         05  FILLER      PIC 9(2).                                        
038200         05  W-TIAVROP-AVS  PIC 9(4).                                     
038300     03 FILLER                   PIC X(16)   VALUE                        
038400                                             'WS-DB2-SEKTION'.            
038500     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
038600     03 WS-MEDD-ERS.                                                      
038700       05 WS-MEDD-ERSKOD         PIC 9(2)    VALUE ZERO.                  
038800       05 FILLER                 PIC X(1)    VALUE SPACE.                 
038900       05 WS-MEDD-TEXT           PIC X(23)   VALUE SPACE.                 
039000     03 WS-FLERSATT-X2X3         PIC X(1)    VALUE SPACE.                 
039100 01  WS-TIAVRDAT-INL             PIC 9(6)    VALUE ZERO.                  
039200 01  WS-TIAVRDAT-INL-AAAAVV      PIC 9(6)    VALUE ZERO.                  
039300 01  FILLER REDEFINES WS-TIAVRDAT-INL-AAAAVV.                             
039400     03 WS-TIAVRDAT-INL-SS        PIC 9(2).                               
039500     03 WS-TIAVRDAT-INL-AAVV      PIC 9(4).                               
039600     EJECT                                                                
039700                                                                          
039800 01 MEDDELANDEN.                                                          
039900    03  MESSAGE-CODES.                                                    
040000      05  ERR-REFILL-PART       PIC X(3)    VALUE '434'.                  
040100                                                                          
040200* FEL-MEDDELANDEN                                                         
040300   03 W-FEL-1.                                                            
040400     05 FILLER  PIC X(40) VALUE 'EJ NUMERISKT ARTIKELNUMMER'.             
040500     05 FILLER  PIC X(40) VALUE 'NOT NUMERIC PART NO.'.                   
040600   03 FILLER REDEFINES W-FEL-1.                                           
040700     05 FEL-1   PIC X(40) OCCURS 2.                                       
040800                                                                          
040900   03 W-FEL-2.                                                            
041000     05 FILLER  PIC X(40) VALUE 'ARTIKEL SAKNAS I DATABAS'.               
041100     05 FILLER  PIC X(40) VALUE 'PART NO.  MISSING IN DATA BASE'.         
041200   03 FILLER REDEFINES W-FEL-2.                                           
041300     05 FEL-2   PIC X(40) OCCURS 2.                                       
041400                                                                          
041500   03 W-FEL-3.                                                            
041600     05 FILLER  PIC X(40) VALUE 'LEVERANTÖRNUMMER FELAKTIGT'.             
041700     05 FILLER  PIC X(40) VALUE 'ERRONEOUS SUPPLIER NO.'.                 
041800   03 FILLER REDEFINES W-FEL-3.                                           
041900     05 FEL-3   PIC X(40) OCCURS 2.                                       
042000                                                                          
042100   03 W-FEL-4.                                                            
042200     05 FILLER  PIC X(20) VALUE 'TYP-KOD FEL         '.                   
042300     05 FILLER  PIC X(20) VALUE 'TYPE-CODE ERROR     '.                   
042400   03 FILLER REDEFINES W-FEL-4.                                           
042500     05 FEL-4   PIC X(20) OCCURS 2.                                       
042600                                                                          
042700   03 W-FEL-5.                                                            
042800     05 FILLER  PIC X(20) VALUE ' FEL 5 ANV EJ       '.                   
042900     05 FILLER  PIC X(20) VALUE ' FEL 5 ANV EJ       '.                   
043000   03 FILLER REDEFINES W-FEL-5.                                           
043100     05 FEL-5   PIC X(20) OCCURS 2.                                       
043200                                                                          
043300   03 W-FEL-6.                                                            
043400     05 FILLER  PIC X(20) VALUE 'ARTIKEL UTGÅNGEN    '.                   
043500     05 FILLER  PIC X(20) VALUE 'PART EXPIRED        '.                   
043600   03 FILLER REDEFINES W-FEL-6.                                           
043700     05 FEL-6   PIC X(20) OCCURS 2.                                       
043800                                                                          
043900   03 W-FEL-7.                                                            
044000     05 FILLER  PIC X(20) VALUE 'ARTIKEL ERSATT      '.                   
044100     05 FILLER  PIC X(20) VALUE 'PART REPLACED       '.                   
044200   03 FILLER REDEFINES W-FEL-7.                                           
044300     05 FEL-7   PIC X(20) OCCURS 2.                                       
044400                                                                          
044500   03 W-FEL-8.                                                            
044600     05 FILLER  PIC X(20) VALUE 'TEXT ?              '.                   
044700     05 FILLER  PIC X(20) VALUE 'TEXT ?              '.                   
044800   03 FILLER REDEFINES W-FEL-8.                                           
044900     05 FEL-8   PIC X(20) OCCURS 2.                                       
045000                                                                          
045100   03 W-FEL-9.                                                            
045200     05 FILLER  PIC X(20) VALUE 'FÖRSLAG SAKNAS      '.                   
045300     05 FILLER  PIC X(20) VALUE 'PROPOSAL MISSING    '.                   
045400   03 FILLER REDEFINES W-FEL-9.                                           
045500     05 FEL-9   PIC X(20) OCCURS 2.                                       
045600                                                                          
045700   03 W-FEL-10.                                                           
045800     05 FILLER  PIC X(19) VALUE 'OBEHÖRIG ANVÄNDARE '.                    
045900     05 FILLER  PIC X(19) VALUE 'USER NOT AUTHORIZED'.                    
046000   03 FILLER REDEFINES W-FEL-10.                                          
046100     05 FEL-10  PIC X(19) OCCURS 2.                                       
046200                                                                          
046300   03 W-FEL-11.                                                           
046400     05 FILLER  PIC X(27) VALUE 'URVAL FRÅN 2147 SAKNAS     '.            
046500     05 FILLER  PIC X(27) VALUE 'SELECTION FROM 2147 MISSING'.            
046600   03 FILLER REDEFINES W-FEL-11.                                          
046700     05 FEL-11  PIC X(27) OCCURS 2.                                       
046800                                                                          
046900   03 W-FEL-12.                                                           
047000     05 FILLER  PIC X(29) VALUE 'INGA FLER ART. FRÅN 2147-KÖN.'.          
047100     05 FILLER  PIC X(29) VALUE 'NO MORE PARTS FROM 2147-QUEUE'.          
047200   03 FILLER REDEFINES W-FEL-12.                                          
047300     05 FEL-12  PIC X(29) OCCURS 2.                                       
047400                                                                          
047500* INF-MEDDELANDEN                                                         
047600   03 W-INF-1.                                                            
047700     05 FILLER  PIC X(18) VALUE 'OMSPEC UTFÖRD     '.                     
047800     05 FILLER  PIC X(18) VALUE 'RESPEC. DONE      '.                     
047900   03 FILLER REDEFINES W-INF-1.                                           
048000     05 INF-1   PIC X(18) OCCURS 2.                                       
048100                                                                          
048200   03 W-INF-2.                                                            
048300     05 FILLER  PIC X(30) VALUE 'NÄSTA ARTIKEL FRÅN 2147 VISAS.'.         
048400     05 FILLER  PIC X(30) VALUE 'NEXT PARTNO FROM 2147 SHOWN.  '.         
048500   03 FILLER REDEFINES W-INF-2.                                           
048600     05 INF-2   PIC X(30) OCCURS 2.                                       
048700                                                                          
048800* MED-MEDDELANDEN                                                         
048900*                                                                         
049000*       MED-1 används ej längre.                                          
049100*       BC-KOLLA-OM-LARM-FINNS annullerat i B-sektionen                   
049200   03 W-MED-1.                                                            
049300     05 FILLER  PIC X(20) VALUE 'SE LARMKÖ'.                              
049400     05 FILLER  PIC X(20) VALUE 'INSPECT ALARM CUE'.                      
049500   03 FILLER REDEFINES W-MED-1.                                           
049600     05 MED-1   PIC X(20) OCCURS 2.                                       
049700     SKIP3                                                                
049800*                                         KOMMENTAR-TEXTER                
049900*                                         INDEXERADE PER SPRÅK            
050000*                                         FÖR ANV MOT SPIND               
050100 01  KOM-TEXTER.                                                          
050200     03  FILLER  PIC X(26) VALUE 'EJ HUVUDLEVERANTÖR        '.            
050300     03  FILLER  PIC X(26) VALUE 'ANNAT LEVNR FINNS         '.            
050400     03  FILLER  PIC X(26) VALUE 'NYTT PB ÅÅVVD             '.            
050500     03  FILLER  PIC X(26) VALUE 'UTREDNINGSSALDO           '.            
050600     03  FILLER  PIC X(26) VALUE 'FÖR UPPDATERING TRYCK PF11'.            
050700     03  FILLER  PIC X(26) VALUE 'PASSIV ARTIKEL            '.            
050800     03  FILLER  PIC X(26) VALUE 'DIREKTLEVERANS > 0        '.            
050900     03  FILLER  PIC X(26) VALUE 'VARN. SISTA AVROP         '.            
051000     03  FILLER  PIC X(26) VALUE 'TREND                     '.            
051100*    ENGELSK TEXT                                                         
051200     03  FILLER  PIC X(26) VALUE 'NOT MAIN SUPPLIER         '.            
051300     03  FILLER  PIC X(26) VALUE 'ANOTHER SUPPLIER EXIST    '.            
051400     03  FILLER  PIC X(26) VALUE 'NEW  PB YYWWD             '.            
051500     03  FILLER  PIC X(26) VALUE 'INVESTIGATION BALANCE     '.            
051600     03  FILLER  PIC X(26) VALUE 'TO UPDATE   PRESS PF11    '.            
051700     03  FILLER  PIC X(26) VALUE 'PART PASSIVE              '.            
051800     03  FILLER  PIC X(26) VALUE 'DIR. DEL. > 0             '.            
051900     03  FILLER  PIC X(26) VALUE 'WARNING LAST CALL         '.            
052000     03  FILLER  PIC X(26) VALUE 'TREND                     '.            
052100                                                                          
052200 01  KOMMENTAR       REDEFINES KOM-TEXTER.                                
052300     03 KOMMENTAR-SPIND OCCURS 2.                                         
052400*                             SPRÅK-GRUPPER = SV + GB                     
052500         05 KOMMENTAR-TEXT OCCURS 9  PIC X(26).                           
052600*                             SPRÅK-TEXT    = 26 BYTES                    
052700                                                                          
052800                                                                          
052900     SKIP3                                                                
053000 01  W-MESSAGE-BOTTOM.                                                    
053100     03  W-MESSAGE-BOTTOM-1  PIC X(18)  VALUE SPACE.                      
053200     03  FILLER              PIC X(01)  VALUE SPACE.                      
053300     03  W-MESSAGE-BOTTOM-2  PIC X(18)  VALUE SPACE.                      
053400     03  FILLER              PIC X(01)  VALUE SPACE.                      
053500     03  W-MESSAGE-BOTTOM-3  PIC X(18)  VALUE SPACE.                      
053600     EJECT                                                                
053700*01  -COPY W221W005.                                                      
053800     EJECT                                                                
053900*                                          ING  1-12 - AKTUELLT ÅR        
054000*                                          ING 13-24 - NÄSTA ÅR           
054100 01  PERIODINDELNING.                                                     
054200     03  PERIOD-TAB          OCCURS 24.                                   
054300         05  PER-START-VV                                                 
054400                             PIC  9(3)               COMP-3.              
054500                                                                          
054600         05  PER-SLUT-VV                                                  
054700                             PIC S9(3)               COMP-3.              
054800                                                                          
054900         05  PER-ANT-VV                                                   
055000                             PIC  9(1).                                   
055100                                                                          
055200         05  PER-AAPP        PIC 9(4).                                    
055300         05  FILLER          REDEFINES PER-AAPP.                          
055400             07  PER-AAPP-AA PIC 9(2).                                    
055500             07  PER-AAPP-PP PIC 9(2).                                    
055600     EJECT                                                                
055700*01      -COPY WDATAREA.                                                  
055800     EJECT                                                                
055900*                         ****    PARAMTERAR TILL W005INIT                
056000*01      -COPY WMSGINIT.                                                  
056100     EJECT                                                                
056200 01  TP-WS.                                                               
056300   03    FILLER              PIC X(16)   VALUE '   TP - AREAOR  '.        
056400     SKIP3                                                                
056500*01  MID       -COPY W2I10301  -PRE MID-.                                 
056600     EJECT                                                                
056700*01  MID       -COPY W2I14701  -PRE 2147-.                                
056800     EJECT                                                                
056900 01  FILLER                  PIC X(16) VALUE '2147-SPAR-AREA'.            
057000*      *****  SPAR-AREA I MSGI-USEA FRÅN 2147-TRANSEN.                    
057100*01  -COPY WW20147S.                                                      
057200     EJECT                                                                
057300*01  -COPY WMSGAREA.                                                      
057400     EJECT                                                                
057500*    03  MOD   -COPY W2O10301  -PRE MOD-  -RED MSG-AREA.                  
057600     EJECT                                                                
057700*01  -COPY WMFSAREA.                                                      
057800     EJECT                                                                
057900 01  FILLER          PIC X(16) VALUE 'PROG-TO-PROG-SW'.                   
058000 01  W-PROG-TO-PROG-SW.                                                   
058100     03  P-WS-LL     PIC S9(4)  VALUE +350 COMP SYNC.                     
058200     03  P-WS-Z1-Z2  PIC X(2)   VALUE LOW-VALUE.                          
058300     03  KDTRANS-WS  PIC X(8)   VALUE 'W2T139 '.                          
058400     03  P-IDTRANS   PIC X(4)   VALUE '2103'.                             
058500     03  P-KDMFSFOR  PIC X(1)   VALUE '1'.                                
058600*    03  MID   -COPY W2I13901     -PRE PROGSW-.                           
058700     EJECT                                                                
058800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
058900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
059000                                                                          
059100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
059200 01  DB2-WS.                                                              
059300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
059400         88  CURSOR-OK                      VALUE 000.                    
059500         88  LINES-FOUND                    VALUE 000.                    
059600         88  LINES-MISSING                  VALUE 100.                    
059700         88  RESOURCE-WRONG                 VALUE 904.                    
059800     03  GOOD-SQLCODECODES.                                               
059900         05  GOOD-SQLCODE OCCURS 5                                        
060000             INDEXED BY SQLCODE-IX PIC 9(3).                              
060100     EJECT                                                                
060200 01  FILLER                  PIC X(16)   VALUE '     IMS-WS     '.        
060300                                                                          
060400 01  STATUS-WS               PIC X(2).                                    
060500     88  SEGMENT-FINNS                   VALUE '  '.                      
060600     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
060700     88  SEGMENT-SLUT                    VALUE 'GB'.                      
060800                                                                          
060900 01  GODK-STATUSKODER.                                                    
061000     03  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC X(2).              
061100                                                                          
061200 01      SSA1                PIC X(128).                                  
061300 01      SSA2                PIC X(64).                                   
061400 01      SSA3                PIC X(64).                                   
061500     EJECT                                                                
061600*01  -COPY W0003                                                          
061700     EJECT                                                                
061800 01  FILLER                 PIC X(16)   VALUE 'IO-AREA  IO-AREA'.         
061900 01  DLI-IO-AREA-01.                                                      
062000     03  IO-AREA-01         PIC X(200)  VALUE SPACE.                      
062100     SKIP3                                                                
062200*    03  WLARTC01  -COPY WDK601 -RED IO-AREA-01.                          
062300     EJECT                                                                
062400 01  DLI-IO-AREA-11.                                                      
062500     03  IO-AREA-11         PIC X(900)  VALUE SPACE.                      
062600     SKIP3                                                                
062700*    03  WLARTC11  -COPY WDK611 -RED IO-AREA-11.                          
062800     EJECT                                                                
062900 01  DLI-IO-AREA1.                                                        
063000     03  IO-AREA1                  PIC X(200) VALUE SPACE.                
063100     SKIP3                                                                
063200*    03  ARTC -COPY WDK623        -RED IO-AREA1.                          
063300     EJECT                                                                
063400*    03  ARTC -COPY WDK625        -RED IO-AREA1.                          
063500     EJECT                                                                
063600*    03  ARTC -COPY WDK626        -RED IO-AREA1.                          
063700     EJECT                                                                
063800 01  DLI-IO-AREA.                                                         
063900     03  IO-AREA            PIC X(200)  VALUE SPACE.                      
064000     SKIP3                                                                
064100*    03  WLINLB11  -COPY WDD902 -PRE LEVNR-    -RED IO-AREA.              
064200     EJECT                                                                
064300*    03  WLINLB22  -COPY WDD904 -PRE OMSPEC-   -RED IO-AREA.              
064400     EJECT                                                                
064500*    03  WLINLB23  -COPY WDD905 -PRE AVROP-    -RED IO-AREA.              
064600     EJECT                                                                
064700*    03  WLBENA11  -COPY WDD311 -PRE BENA11-   -RED IO-AREA.              
064800     EJECT                                                                
064900*    03  WLXXBU01  -COPY WDGX2223 -PRE XXBU-   -RED IO-AREA.              
065000     EJECT                                                                
065100*    03  WLXXBU11  -COPY WDGX2224 -PRE XXBU-   -RED IO-AREA.              
065200     EJECT                                                                
065300*    03  WLXXBX01  -COPY WDGX01   -PRE XXBX-   -RED IO-AREA.              
065400     EJECT                                                                
065500*    03  WLXXBX11  -COPY WDGX2232 -PRE XXBX-   -RED IO-AREA.              
065600     EJECT                                                                
065700                                                                          
065800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
065900 01  DLI-IO-WDA901.                                                       
066000*    03  -COPY WDA901                                                     
066100     EJECT                                                                
066200                                                                          
066300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
066400 01  DLI-IO-WDA911.                                                       
066500*    03  -COPY WDA911                                                     
066600     EJECT                                                                
066700                                                                          
066800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
066900 01  DLI-IO-WDB201.                                                       
067000*    03  WDB201    -COPY WDB201                                           
067100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7A1'.                      
067200     SKIP3                                                                
067300 01  DLI-IO-WDD7A1.                                                       
067400*    03  -COPY WDD7A1                                                     
067500     EJECT                                                                
067600 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA01'.            
067700     SKIP3                                                                
067800 01  DLI-IO-AREA-ERSA01.                                                  
067900*  03  WLERSA01 -COPY WDD701  -PRE ERSA01-                                
068000     EJECT                                                                
068100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ERSA11'.            
068200     SKIP3                                                                
068300 01  DLI-IO-AREA-ERSA11.                                                  
068400*  03  WLERSA11 -COPY WDD702  -PRE ERSA11-                                
068500     EJECT                                                                
068600 01  FILLER         PIC X(17) VALUE 'DLI-IO-WDK601-OLD'.                  
068700     SKIP3                                                                
068800 01  DLI-IO-WDK601-OLD.                                                   
068900*    03  -COPY WDK601   -PRE OLD-                                         
069000     EJECT                                                                
069100 01  FILLER         PIC X(17) VALUE 'DLI-IO-WDK611-OLD'.                  
069200     SKIP3                                                                
069300 01  DLI-IO-WDK611-OLD.                                                   
069400*    03  -COPY WDK611   -PRE OLD-                                         
069500     EJECT                                                                
069600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
069700 01  DLI-IO-WDD601.                                                       
069800*    03  -COPY WDD601                                                     
069900     EJECT                                                                
070000                                                                          
070100 01  DLI-IO-AREA-F1          PIC X(100).                                  
070200     SKIP2                                                                
070300*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
070400     EJECT                                                                
070500                                                                          
070600 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
070700                                                                          
070800*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
070900     EJECT                                                                
071000 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
071100                                                                          
071200*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
071300     EJECT                                                                
071400     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
071500     EJECT                                                                
071600     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
071700     EJECT                                                                
071800                                                                          
071900 LINKAGE SECTION.                                                         
072000     SKIP3                                                                
072100*01  -COPY W0009 -PRE MSG-                                                
072200     EJECT                                                                
072300*01  -COPY W0009 -PRE ALT-                                                
072400     EJECT                                                                
072500*01  -COPY W0008 -PRE USEA-.                                              
072600     05  FILLER              PIC X.                                       
072700     EJECT                                                                
072800*01  -COPY W0008 -PRE ARTC-.                                              
072900     05  FILLER              PIC X.                                       
073000     EJECT                                                                
073100*01  -COPY W0008 -PRE INLB-.                                              
073200     05  FILLER              PIC X.                                       
073300     EJECT                                                                
073400*01  -COPY W0008 -PRE BENA-.                                              
073500     05  FILLER              PIC X.                                       
073600     EJECT                                                                
073700*01  -COPY W0008 -PRE XXBU-.                                              
073800     05  FILLER              PIC X.                                       
073900     EJECT                                                                
074000*01  -COPY W0008 -PRE XXBX-.                                              
074100     05  FILLER              PIC X.                                       
074200     EJECT                                                                
074300 01  PBTO-WDK6-PCB                 PIC X.                                 
074400 01  PBTO-WDK7-PCB                 PIC X.                                 
074500 01  PBTO-ARTM-PCB                 PIC X.                                 
074600 01  PBTO-2501-PCB                 PIC X.                                 
074700 01  PBTO-WDB6R-PCB                PIC X.                                 
074800 01  PBTO-WDK7R-PCB                PIC X.                                 
074900     EJECT                                                                
075000*01  -COPY W0008     -PRE WDA9-                                           
075100     05  FILLER                    PIC X.                                 
075200     EJECT                                                                
075300*01  -COPY W0008     -PRE WDB2B-                                          
075400         05  FILLER                PIC X.                                 
075500     EJECT                                                                
075600 01  PBTO-WDB6-PCB                 PIC X.                                 
075700 01  PBTO-WDD7-PCB                 PIC X.                                 
075800 01  PBTO-WDK7E-PCB                PIC X.                                 
075900 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
076000 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
076100 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
076200 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
076300 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
076400 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
076500 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
076600 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
076700     EJECT                                                                
076800*01  -COPY W0008     -PRE WDD7A1-                                         
076900     05  FILLER                    PIC X.                                 
077000     EJECT                                                                
077100*01  -COPY W0008     -PRE WDD6-                                           
077200     05  FILLER                    PIC X.                                 
077300     EJECT                                                                
077400*01  -COPY W0008     -PRE ERSA-                                           
077500     05  FILLER                    PIC X.                                 
077600     EJECT                                                                
077700*01  -COPY W0008     -PRE WDF1-                                           
077800     05  FILLER                    PIC X.                                 
077900     EJECT                                                                
078000                                                                          
078100 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  USEA-PCB                     
078200     ARTC-PCB        INLB-PCB       BENA-PCB XXBU-PCB                     
078300     XXBX-PCB                                                             
078400     PBTO-WDK6-PCB  PBTO-WDK7-PCB   PBTO-ARTM-PCB PBTO-2501-PCB           
078500     PBTO-WDB6R-PCB PBTO-WDK7R-PCB                                        
078600     WDA9-PCB       WDB2B-PCB                                             
078700     PBTO-WDB6-PCB  PBTO-WDD7-PCB  PBTO-WDK7E-PCB                         
078800     PBTO-W222-UTIL-WDK6-PCB       PBTO-W222-UTIL-WDK7-PCB                
078900     PBTO-W222-UTIL-WDB6-PCB       PBTO-W222-UTUP-WDK7-PCB                
079000     PBTO-W222-UTUP-WDB6-PCB       PBTO-W222-UTUP-UTIL-WDK6-PCB           
079100     PBTO-W222-UTUP-UTIL-WDK7-PCB  PBTO-W222-UTUP-UTIL-WDB6-PCB           
079200     WDD7A1-PCB     WDD6-PCB       ERSA-PCB WDF1-PCB.                     
079300                                                                          
079400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB  USEA-PCB                      
079500     ARTC-PCB       INLB-PCB       BENA-PCB XXBU-PCB                      
079600     XXBX-PCB                                                             
079700     PBTO-WDK6-PCB  PBTO-WDK7-PCB  PBTO-ARTM-PCB PBTO-2501-PCB            
079800     PBTO-WDB6R-PCB PBTO-WDK7R-PCB                                        
079900     WDA9-PCB       WDB2B-PCB                                             
080000     PBTO-WDB6-PCB  PBTO-WDD7-PCB  PBTO-WDK7E-PCB                         
080100     PBTO-W222-UTIL-WDK6-PCB       PBTO-W222-UTIL-WDK7-PCB                
080200     PBTO-W222-UTIL-WDB6-PCB       PBTO-W222-UTUP-WDK7-PCB                
080300     PBTO-W222-UTUP-WDB6-PCB       PBTO-W222-UTUP-UTIL-WDK6-PCB           
080400     PBTO-W222-UTUP-UTIL-WDK7-PCB  PBTO-W222-UTUP-UTIL-WDB6-PCB           
080500     WDD7A1-PCB     WDD6-PCB       ERSA-PCB WDF1-PCB.                     
080600                                                                          
080700     PERFORM IMS-GET-MSG                                                  
080800     IF SEGMENT-FINNS                                                     
080900       PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                                
081000                                                                          
081100       IF INDATA-OK                                                       
081200         IF SW-HOPP-2139 = JA                                             
081300           PERFORM D-PPSW-2139                                            
081400         ELSE                                                             
081500           IF W-NY-GAMMAL = W-NY-INDATA                                   
081600             PERFORM MFS-STAENG-UPPDAT-FAELT                              
081700             PERFORM B-SKAPA-NY-BILD                                      
081800           ELSE                                                           
081900             PERFORM C-TESTA-INDATA-IFYLLT                                
082000             PERFORM MFS-ROER-EJ-UTDATA-FAELT                             
082100             PERFORM MFS-ROER-EJ-INDATA-FAELT                             
082200           END-IF                                                         
082300         END-IF                                                           
082400       ELSE                                                               
082500         PERFORM MFS-ROER-EJ-UTDATA-FAELT                                 
082600         PERFORM MFS-STAENG-UPPDAT-FAELT                                  
082700       END-IF                                                             
082800       IF SW-HOPP-2139 = NEJ                                              
082900         MOVE W-MESSAGE-BOTTOM TO MOD-MESSAGE-BOTTOM                      
083000         PERFORM IMS-INSERT-MSG                                           
083100       END-IF                                                             
083200     END-IF                                                               
083300                                                                          
083400     MOVE ZERO TO RETURN-CODE                                             
083500     GOBACK                                                               
083600     .                                                                    
083700     EJECT                                                                
083800                                                                          
083900 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
084000* KONTROLL AV NYCKLAR OCH ATT MID 2103 MOTTAGITS              *           
084100     MOVE JA TO SW-INDATA                                                 
084200     MOVE NEJ TO SW-FAELT-IFYLLT                                          
084300     MOVE W-NY-INDATA TO W-NY-GAMMAL                                      
084400     IF MSG-DUBBLA-TRANSKODER                                             
084500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10301                 
084600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
084700                                             W-IDTRANS                    
084800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
084900       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
085000       IF MFS-IDTRANS = '2103'                                            
085100         MOVE SPACE TO MFS-IDTRANS                                        
085200       END-IF                                                             
085300     ELSE                                                                 
085400       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
085500       MOVE SPACE                       TO MFS-IDPFK                      
085600       IF MFS-IDTRANS = '2147'                                            
085700         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO 2147-MID                   
085800       ELSE                                                               
085900         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I10301               
086000         MOVE MSG-IDPFK                     TO MFS-IDPFK                  
086100       END-IF                                                             
086200       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
086300     END-IF                                                               
086400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
086500     MOVE '001'             TO MSGI-KDCALL                                
086600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
086700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
086800     MOVE '2103'            TO MSGI-IDTRANS                               
086900* -- Hämtar MSGI-SPAR-AREA för koll mot div.sparade nycklar               
087000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
087100     MOVE MSGI-SPAR-AREA   TO SPAR-AREA                                   
087200*************** ARTNR UPPDATERAS OCH LÄSES FRÅN NYCKELDATABASEN           
087300*************** LEVNR ENDAST UPPDATERING PÅ NYCKELDATABASEN               
087400     IF MFS-IDTRANS = '2147'                                              
087500* -- Lev.planeförslagskö. beh. ny vald artikel i MID-2147                 
087600       IF  SPAR-KEY-IDANSK-FROM NUMERIC                                   
087700       AND SPAR-KEY-IDANSK-TO   NUMERIC                                   
087800       AND SPAR-KEY-KDLPORS     NUMERIC                                   
087900         PERFORM AD-SPARA-MID-DATA-FRAN-2147                              
088000       ELSE                                                               
088100         CONTINUE                                                         
088200*        SET FEL11 TO TRUE                                                
088300       END-IF                                                             
088400     ELSE                                                                 
088500       IF MFS-NEXT                                                        
088600* -- OBS PF8 får inte ha annan funktion på denna bild.                    
088700* -- Man vill ha nästa artikel i urvalet på lev.pl.f.kön                  
088800* -- Lägg nästa artikel i MID- och kör ord. 2103-koll                     
088900         IF  SPAR-IDTRANS = '2147'                                        
089000         AND SPAR-KEY-IDANSK-FROM NUMERIC                                 
089100         AND SPAR-KEY-IDANSK-TO   NUMERIC                                 
089200         AND SPAR-KEY-KDLPORS     NUMERIC                                 
089300         AND (   SPAR-KEY-IDANSK-TO > 99                                  
089400         OR SPAR-KEY-IDANSK-TO = ZERO )                                   
089500           PERFORM AE-NAESTA-ART-O-LEV-FRAN-WDD6                          
089600         ELSE                                                             
089700* -- Spar-area från 2147 SAKNAS eller är FELAKTIG                         
089800           SET FEL11 TO TRUE                                              
089900         END-IF                                                           
090000       END-IF                                                             
090100       IF MFS-IDTRANS = '2103' OR MFS-RETURN                              
090200* -- KÖR NORMAL 2103-TRANS (enter-trans)                                  
090300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
090400         MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                              
090500       ELSE                                                               
090600* -- ANNAN TRANS.                                                         
090700         IF MFS-IDTRANS = '2147'                                          
090800* -- Behandlat i AD-SPARA-MID-DATA-FRAN-2147                              
090900           CONTINUE                                                       
091000         ELSE                                                             
091100           MOVE ALL '+' TO MID-IDLEVNR-IN                                 
091200                           MID-KDBEHX-PLAN-IN                             
091300           MOVE SPACE TO MID-IDLEVNR-UT                                   
091400                           MID-KDBEHX-PLAN-UT                             
091500         END-IF                                                           
091600         IF MFS-IDTRANS(1:2) NOT = '42'                                   
091700           IF MID-IDARTNR-IN NUMERIC                                      
091800           AND MID-IDARTNR-IN > ZERO                                      
091900             MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                          
092000           END-IF                                                         
092100         END-IF                                                           
092200       END-IF                                                             
092300     END-IF                                                               
092400* -- Checka nu MID-nycklar mot USERBASEN                                  
092500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
092600     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
092700     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
092800     IF MID-IDARTNR-IN = ALL '+'                                          
092900       CONTINUE                                                           
093000     ELSE                                                                 
093100       MOVE SPACE TO MID-IDLEVNR-UT                                       
093200                     MID-KDBEHX-PLAN-UT                                   
093300     END-IF                                                               
093400                                                                          
093500     IF MSGI-IDLAND-SPR = SPACE                                           
093600       IF MSGI-IDSPRAK = SPACE                                            
093700         IF SWEDISH-TEXT                                                  
093800           MOVE +1 TO SPIND                                               
093900         ELSE                                                             
094000           MOVE +2 TO SPIND                                               
094100         END-IF                                                           
094200       ELSE                                                               
094300         IF MSGI-IDSPRAK = 'SV'                                           
094400           MOVE +1 TO SPIND                                               
094500         ELSE                                                             
094600           MOVE +2 TO SPIND                                               
094700         END-IF                                                           
094800       END-IF                                                             
094900     ELSE                                                                 
095000       IF MSGI-IDLAND-SPR = 'SE'                                          
095100         MOVE +1 TO SPIND                                                 
095200       ELSE                                                               
095300         MOVE +2 TO SPIND                                                 
095400       END-IF                                                             
095500     END-IF                                                               
095600                                                                          
095700     IF MID-IDLEVNR-IN = ALL '+'                                          
095800       MOVE MID-IDLEVNR-UT TO IDLEVNR-WS                                  
095900     ELSE                                                                 
096000       MOVE MID-IDLEVNR-IN TO IDLEVNR-WS                                  
096100     END-IF                                                               
096200                                                                          
096300     IF MID-KDBEHX-PLAN-IN = 'E'                                          
096400        MOVE 'G'               TO MID-KDBEHX-PLAN-IN                      
096500     END-IF                                                               
096600     IF MID-KDBEHX-PLAN-IN = 'P'                                          
096700        MOVE 'F'               TO MID-KDBEHX-PLAN-IN                      
096800     END-IF                                                               
096900     IF MID-KDBEHX-PLAN-UT = 'E'                                          
097000        MOVE 'G'               TO MID-KDBEHX-PLAN-UT                      
097100     END-IF                                                               
097200     IF MID-KDBEHX-PLAN-UT = 'P'                                          
097300        MOVE 'F'               TO MID-KDBEHX-PLAN-UT                      
097400     END-IF                                                               
097500     IF MFS-IDPFK = '3' AND MID-KDBEHX-PLAN-UT = 'G'                      
097600       MOVE 'F'               TO MID-KDBEHX-PLAN-IN                       
097700     ELSE                                                                 
097800       IF MFS-IDPFK = '3' AND MID-KDBEHX-PLAN-UT = 'F'                    
097900         MOVE 'G'               TO MID-KDBEHX-PLAN-IN                     
098000       END-IF                                                             
098100     END-IF                                                               
098200     IF MID-KDBEHX-PLAN-IN = ALL '+' OR SPACE                             
098300       MOVE MID-KDBEHX-PLAN-UT TO KDBEHX-PLAN-WS                          
098400     ELSE                                                                 
098500       MOVE MID-KDBEHX-PLAN-IN TO KDBEHX-PLAN-WS                          
098600     END-IF                                                               
098700     IF MID-IDARTNR-IN = ALL '+' AND MID-IDLEVNR-IN = ALL '+'             
098800     AND MID-KDBEHX-PLAN-IN = ALL '+' AND MFS-IDTRANS = '2103'            
098900       MOVE W-GAMMAL-INDATA TO W-NY-GAMMAL                                
099000     END-IF                                                               
099100     MOVE LOW-VALUE TO MOD-W2O10301                                       
099200     MOVE '2103'    TO MOD-IDTRANS                                        
099300     MOVE 'W2O10301' TO MFS-IDMOD                                         
099400     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
099500     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
099600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
099700     MOVE IDLEVNR-WS TO MOD-IDLEVNR-UT                                    
099800                        MOD-IDLEVNR-SHIP-UT                               
099900     MOVE KDBEHX-PLAN-WS TO MOD-KDBEHX-PLAN-UT                            
100000     IF SPIND = 1                                                         
100100       CONTINUE                                                           
100200     ELSE                                                                 
100300       IF KDBEHX-PLAN-WS = 'G'                                            
100400         MOVE 'E'          TO MOD-KDBEHX-PLAN-UT                          
100500       ELSE                                                               
100600         IF KDBEHX-PLAN-WS = 'F'                                          
100700           MOVE 'P'        TO MOD-KDBEHX-PLAN-UT                          
100800         END-IF                                                           
100900       END-IF                                                             
101000     END-IF                                                               
101100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
101200                             MOD-IDLEVNR-IN                               
101300                             MOD-KDBEHX-PLAN-IN                           
101400                             MOD-MESSAGE                                  
101500                             MOD-MESSAGE-BOTTOM                           
101600     IF IDARTNR-WS NOT NUMERIC                                            
101700       MOVE FEL-1 (SPIND) TO MOD-MESSAGE                                  
101800       MOVE NEJ TO SW-INDATA                                              
101900     ELSE                                                                 
102000       CONTINUE                                                           
102100**** De vill kunna använda andra koder än G, E, F, P och då får vi        
102200**** ändra här                                                            
102300       IF  KDBEHX-PLAN-WS NOT = GALLANDE                                  
102400       AND KDBEHX-PLAN-WS NOT = FORSLAG                                   
102500       AND KDBEHX-PLAN-WS NOT = SPACE                                     
102600         MOVE FEL-4 (SPIND) TO MOD-MESSAGE                                
102700         MOVE NEJ TO SW-INDATA                                            
102800       END-IF                                                             
102900     END-IF                                                               
103000* --- testar felkoderna från 2147-hopp-kontrollen                         
103100     IF FEL11                                                             
103200       MOVE FEL-11(SPIND) TO MOD-MESSAGE                                  
103300       MOVE NEJ TO SW-INDATA                                              
103400     ELSE                                                                 
103500       IF FEL12                                                           
103600         MOVE FEL-12(SPIND) TO MOD-MESSAGE                                
103700         MOVE NEJ TO SW-INDATA                                            
103800       END-IF                                                             
103900     END-IF                                                               
104000                                                                          
104100     IF INDATA-OK                                                         
104200       PERFORM AA-DAGENS-DATUM                                            
104300       PERFORM AB-PERIODINDELNING-TAB                                     
104400       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD             
104500                                                                          
104600       IF MFS-IDTRANS = '2135'                                            
104700          MOVE INF-1 (SPIND) TO W-MESSAGE-BOTTOM-3                        
104800       END-IF                                                             
104900                                                                          
105000       PERFORM AC-KOLL-HOPP-2139                                          
105100     END-IF                                                               
105200                                                                          
105300     MOVE WC-CDC-SE TO W-IDDC-D6-MIN                                      
105400                       W-IDDC-D6-MAX                                      
105500     .                                                                    
105600     EJECT                                                                
105700                                                                          
105800 AA-DAGENS-DATUM SECTION.                                                 
105900     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
106000     CALL WDATKONV USING DAT-KDDATFORM                                    
106100                         DAT-I-TIDATUM                                    
106200                         DAT-O-TIDATUM                                    
106300                         DAT-KDSVAR                                       
106400     MOVE DAT-TIAA TO  W-DATUM-AA                                         
106500                       W-START-PER-AA                                     
106600     MOVE DAT-TIRP  TO W-START-PER-PP                                     
106700     MOVE DAT-TIVV  TO W-DATUM-VV                                         
106800     MOVE DAT-TIAARP TO W-PERIOD-AAPP                                     
106900                       W-START-PER-AAPP                                   
107000     MOVE W-DATUM-AAVV TO W-DATUM-AKTUELLT                                
107100     COMPUTE W-DATUM-AKTUELLT-AAAAVV =                                    
107200             W-DATUM-AKTUELLT + 200000                                    
107300                                                                          
107400     MOVE DAT-TISEKEL       TO W-DATUM-AKTUELLT-AAAAVVD(1:2)              
107500     MOVE DAT-TIAAVVD       TO W-DATUM-AKTUELLT-AAAAVVD(3:5)              
107600                                                                          
107700     ADD 1 TO W-DATUM-AA                                                  
107800     ADD 7 TO W-DATUM-VV                                                  
107900     IF  W-DATUM-VV > 52                                                  
108000       ADD 1 TO W-DATUM-AA                                                
108100       SUBTRACT 52 FROM W-DATUM-VV                                        
108200     END-IF                                                               
108300     MOVE W-DATUM-AKTUELLT   TO W009VADDW-AAVV                            
108400     MOVE -26                TO W009VADDW-ANTAL                           
108500     CALL W009VADD USING W009VADDW-AAVV                                   
108600                         W009VADDW-ANTAL                                  
108700     MOVE W009VADDW-AAVV     TO W-AAVV-MINUS-HALVAR                       
108800     .                                                                    
108900     EJECT                                                                
109000                                                                          
109100 AB-PERIODINDELNING-TAB SECTION.                                          
109200* --- FYLL I VECKONR FÖR PERIODERNA                                       
109300     MOVE W-START-PER-AA     TO WS-AA                                     
109400     MOVE 01                 TO WS-PP                                     
109500     MOVE 'AARP'             TO DAT-KDDATFORM                             
109600     MOVE +1                 TO IX-PP                                     
109700     PERFORM UNTIL IX-PP      >  24                                       
109800       IF WS-PP = 13                                                      
109900          MOVE +1             TO WS-PP                                    
110000          ADD  +1             TO WS-AA                                    
110100       END-IF                                                             
110200       MOVE WS-AAPP           TO DAT-I-TIDATUM                            
110300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
110400                             DAT-O-TIDATUM DAT-KDSVAR                     
110500       IF DAT-KDSVAR-OK                                                   
110600          MOVE DAT-TIVV         TO PER-START-VV (IX-PP)                   
110700          MOVE DAT-KVVIPER      TO PER-ANT-VV   (IX-PP)                   
110800          MOVE DAT-TIAARP       TO PER-AAPP     (IX-PP)                   
110900          COMPUTE PER-SLUT-VV (IX-PP) =                                   
111000                  PER-START-VV (IX-PP) + DAT-KVVIPER - 1                  
111100****************fix för period 12 2020                                    
111200          IF PER-AAPP(12) = 2012                                          
111300            MOVE 53   TO PER-SLUT-VV(12)                                  
111400          END-IF                                                          
111500**************************************                                    
111600       ELSE                                                               
111700          MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                    
111800          CALL FELLOG                                                     
111900       END-IF                                                             
112000       ADD +1                   TO WS-PP IX-PP                            
112100     END-PERFORM                                                          
112200     .                                                                    
112300     EJECT                                                                
112400                                                                          
112500 AC-KOLL-HOPP-2139      SECTION.                                          
112600     MOVE NEJ                    TO SW-HOPP-2139                          
112700     IF MFS-IDPFK       = '9'                                             
112800        MOVE JA                  TO SW-HOPP-2139                          
112900        MOVE LOW-VALUE           TO PROGSW-W2I13901                       
113000        MOVE W-START-PER-AAPP    TO WS-AAPP                               
113100        MOVE WS-AAPP             TO PROGSW-PERIOD-IN                      
113200     END-IF                                                               
113300     .                                                                    
113400     EJECT                                                                
113500                                                                          
113600 AD-SPARA-MID-DATA-FRAN-2147   SECTION.                                   
113700* -- Hämta nycklarna som skall användas istället för 2103-input           
113800     MOVE +1 TO INDX                                                      
113900     PERFORM UNTIL INDX > MAX-INDX                                        
114000                OR 2147-MID-KDCMDVAL(INDX) > '+'                          
114100* -- Hitta vald rad från 2147-bilden                                      
114200        ADD +1 TO INDX                                                    
114300     END-PERFORM                                                          
114400     IF INDX <= MAX-INDX                                                  
114500* -- Spara nycklarna från raden.                                          
114600* -- Från 8 till 9-ställigt "justified right"-fält                        
114700        MOVE 2147-MID-IDARTNR(INDX) TO WS-IDARTNR-9-RIGHT                 
114800        INSPECT WS-IDARTNR-9-RIGHT REPLACING LEADING ' ' BY '0'           
114900        MOVE    WS-IDARTNR-9-RIGHT  TO  MID-IDARTNR-IN                    
115000* - Sparar hopp-artikeln                                                  
115100                                       SPAR-KEY-IDARTNR-ENTER             
115200                                       MSGI-IDARTNR                       
115300                                       SPAR-KEY-IDARTNR-NEXT              
115400        MOVE 2147-MID-IDLEVNR(INDX) TO MID-IDLEVNR-IN                     
115500                                       MSGI-IDLEVNR                       
115600                                       SPAR-KEY-IDLEVNR-ENTER             
115700        MOVE WC-CDC-SE              TO SPAR-KEY-IDDC-ENTER                
115800                                       SPAR-KEY-IDDC-NEXT                 
115900        MOVE +1  TO IX                                                    
116000        PERFORM UNTIL IX > 10                                             
116100          MOVE WC-CDC-SE            TO SPAR-KEY-IDDC-PREV (IX)            
116200          ADD +1 TO IX                                                    
116300        END-PERFORM                                                       
116400     ELSE                                                                 
116500        MOVE MSGI-IDARTNR           TO MID-IDARTNR-IN                     
116600        MOVE MSGI-IDLEVNR           TO MID-IDLEVNR-IN                     
116700     END-IF                                                               
116800* -- Spara SPAR-AREA på USEA                                              
116900     MOVE '002'          TO MSGI-KDCALL                                   
117000     MOVE '2147'         TO SPAR-IDTRANS                                  
117100     MOVE SPAR-AREA      TO MSGI-SPAR-AREA                                
117200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
117300* -- Återställ KDCALL                                                     
117400     MOVE '001'          TO MSGI-KDCALL                                   
117500* -- Sätt visning till förslag                                            
117600     MOVE 'F' TO MID-KDBEHX-PLAN-IN                                       
117700                                                                          
117800**** Rensa förslag med gällande plan                                      
117900                                                                          
118000     IF SPAR-KEY-IDLEVNR > SPACE                                          
118100* -- Denna begränsningsnyckel är SPAR-ad på 2147 innan hopp               
118200       MOVE SPAR-KEY-IDLEVNR      TO W-IDLEVNR-D6-MIN                     
118300                                     W-IDLEVNR-D6-MAX                     
118400     ELSE                                                                 
118500       MOVE LOW-VALUE             TO W-IDLEVNR-D6-MIN                     
118600       MOVE HIGH-VALUE            TO W-IDLEVNR-D6-MAX                     
118700     END-IF                                                               
118800                                                                          
118900     MOVE WC-CDC-SE               TO W-IDDC-D6-MIN                        
119000                                     W-IDDC-D6-MAX                        
119100                                                                          
119200     MOVE SPAR-KEY-IDANSK-TO      TO W-IDANSK-MAX                         
119300     MOVE SPAR-KEY-IDANSK-FROM    TO W-IDANSK-MIN                         
119400     MOVE SPAR-KEY-KDLPORS        TO W-KDLPORS-D6                         
119500     MOVE SPAR-KEY-KDLEVPLF       TO W-KDLEVPLF-D6                        
119600* -- Positionera i basen med GHU                                          
119700     IF  SPAR-KEY-IDARTNR-ENTER NUMERIC                                   
119800*      IF  SPAR-KEY-IDLEVNR-ENTER > SPACE                                 
119900       IF  SPAR-KEY-IDARTNR-ENTER > ZERO                                  
120000         PERFORM IMS-GHU-WDD601                                           
120100         PERFORM UNTIL SEGMENT-SAKNAS                                     
120200         OR          SEGMENT-SLUT                                         
120300         OR          (LPF-IDLEVNR = SPAR-KEY-IDLEVNR-ENTER                
120400         AND          LPF-IDARTNR = SPAR-KEY-IDARTNR-ENTER )              
120500           PERFORM IMS-GHN-WDD601                                         
120600         END-PERFORM                                                      
120700         IF SEGMENT-FINNS                                                 
120800           IF LPF-KDLPORS(2) = 26                                         
120900           AND LPF-KDLPORS(3) = 27                                        
121000             MOVE ZERO TO LPF-KDLPORS(2)                                  
121100             MOVE ZERO TO LPF-KDLPORS(3)                                  
121200             PERFORM IMS-DELETE-WDD601                                    
121300             MOVE 'G' TO MID-KDBEHX-PLAN-IN                               
121400           END-IF                                                         
121500         END-IF                                                           
121600       END-IF                                                             
121700     END-IF                                                               
121800     .                                                                    
121900     EJECT                                                                
122000                                                                          
122100 AE-NAESTA-ART-O-LEV-FRAN-WDD6   SECTION.                                 
122200* -- MSGI-SPARA-AREA är redan hämtad.                                     
122300* -- Sök med senast sparade WDD6-nycklar                                  
122400     IF SPAR-KEY-IDLEVNR > SPACE                                          
122500* -- Denna begränsningsnyckel är SPAR-ad på 2147 innan hopp               
122600       MOVE SPAR-KEY-IDLEVNR     TO W-IDLEVNR-D6-MIN                      
122700                                    W-IDLEVNR-D6-MAX                      
122800     ELSE                                                                 
122900       MOVE LOW-VALUE             TO W-IDLEVNR-D6-MIN                     
123000       MOVE HIGH-VALUE            TO W-IDLEVNR-D6-MAX                     
123100     END-IF                                                               
123200                                                                          
123300     MOVE WC-CDC-SE               TO W-IDDC-D6-MIN                        
123400                                     W-IDDC-D6-MAX                        
123500                                                                          
123600     MOVE SPAR-KEY-IDANSK-FROM    TO W-IDANSK-MIN                         
123700     IF SPAR-KEY-IDANSK-TO = ZERO                                         
123800       MOVE W-IDANSK-MIN          TO WS-IDANSK-TO-RED                     
123900       IF  WS-IDANSK-TO-2 > ZERO                                          
124000       AND WS-IDANSK-TO-3 = ZERO                                          
124100         MOVE 9                   TO WS-IDANSK-TO-3                       
124200       END-IF                                                             
124300       MOVE WS-IDANSK-TO-RED      TO SPAR-KEY-IDANSK-TO                   
124400     END-IF                                                               
124500     MOVE SPAR-KEY-IDANSK-TO      TO W-IDANSK-MAX                         
124600     MOVE SPAR-KEY-KDLPORS        TO W-KDLPORS-D6                         
124700     MOVE SPAR-KEY-KDLEVPLF       TO W-KDLEVPLF-D6                        
124800* -- positionera i basen med GU                                           
124900     PERFORM IMS-GU-WDD601                                                
125000     IF SPAR-KEY-IDARTNR-NEXT > ZERO                                      
125100*--- PF8 från egen bild och överhoppad behandling                         
125200*      IF  SPAR-KEY-IDLEVNR-ENTER NUMERIC                                 
125300         IF SPAR-KEY-IDARTNR-ENTER > ZERO                                 
125400*        AND SPAR-KEY-IDLEVNR-ENTER > SPACE                               
125500           PERFORM UNTIL SEGMENT-SAKNAS                                   
125600           OR          SEGMENT-SLUT                                       
125700           OR          (LPF-IDLEVNR = SPAR-KEY-IDLEVNR-ENTER              
125800           AND          LPF-IDARTNR = SPAR-KEY-IDARTNR-ENTER )            
125900             PERFORM IMS-GN-WDD601                                        
126000           END-PERFORM                                                    
126100           IF SEGMENT-FINNS                                               
126200             PERFORM IMS-GN-WDD601                                        
126300           END-IF                                                         
126400         END-IF                                                           
126500*      END-IF                                                             
126600     ELSE                                                                 
126700* -- Kommer från behandlat och borttaget förslag i w20163                 
126800       CONTINUE                                                           
126900     END-IF                                                               
127000     IF SEGMENT-FINNS                                                     
127100* -- kolla om begränsningar finns på LEVNR, KDLPORS, KDLEVPLF             
127200       PERFORM AEA-KOLLA-OM-GODK-POST                                     
127300       PERFORM UNTIL GODK-POST                                            
127400       OR    SEGMENT-SAKNAS OR SEGMENT-SLUT                               
127500         PERFORM IMS-GN-WDD601                                            
127600         PERFORM AEA-KOLLA-OM-GODK-POST                                   
127700       END-PERFORM                                                        
127800                                                                          
127900       IF GODK-POST                                                       
128000* -- Kör denna som nästa 2103-artikel                                     
128100         MOVE LPF-IDARTNR TO MID-IDARTNR-IN                               
128200* -- Spara som kontroll vid nästa F8                                      
128300                             SPAR-KEY-IDARTNR-NEXT                        
128400                             SPAR-KEY-IDARTNR-ENTER                       
128500         MOVE LPF-IDLEVNR TO MID-IDLEVNR-IN                               
128600                             SPAR-KEY-IDLEVNR-ENTER                       
128700         MOVE WC-CDC-SE   TO SPAR-KEY-IDDC-ENTER                          
128800                             SPAR-KEY-IDDC-NEXT                           
128900        MOVE +1  TO IX                                                    
129000        PERFORM UNTIL IX > 10                                             
129100          MOVE WC-CDC-SE  TO SPAR-KEY-IDDC-PREV (IX)                      
129200          ADD +1 TO IX                                                    
129300        END-PERFORM                                                       
129400                                                                          
129500* -- IDLEVNR ingår i WDD601KY                                             
129600* -- Ev. begränsning redan satt ovan                                      
129700*        -- Spara hittat data på USEA till artikel på kö.                 
129800         MOVE '002'       TO MSGI-KDCALL                                  
129900         MOVE '2147'      TO SPAR-IDTRANS                                 
130000         MOVE SPAR-AREA   TO MSGI-SPAR-AREA                               
130100         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
130200* -- Återställ KDCALL                                                     
130300         MOVE '001'       TO MSGI-KDCALL                                  
130400       ELSE                                                               
130500* -- Ingen träff på nästa i urvalet                                       
130600         SET FEL12  TO TRUE                                               
130700* -- Denna felkod tas om hand i A-sektionen                               
130800* -- efter kollen på IDARTNR-WS                                           
130900       END-IF                                                             
131000     ELSE                                                                 
131100       SET FEL12  TO TRUE                                                 
131200     END-IF                                                               
131300     .                                                                    
131400     EJECT                                                                
131500                                                                          
131600 AEA-KOLLA-OM-GODK-POST SECTION.                                          
131700**----------------------------------------------------*                   
131800** Här kollas övriga ev. begränsningsnycklar.         *                   
131900**----------------------------------------------------*                   
132000     SET GODK-POST TO TRUE                                                
132100                                                                          
132200     IF W-KDLPORS-D6 > ZERO                                               
132300        IF LPF-KDLPORS(1)  NOT = W-KDLPORS-D6                             
132400        AND LPF-KDLPORS(2) NOT = W-KDLPORS-D6                             
132500        AND LPF-KDLPORS(3) NOT = W-KDLPORS-D6                             
132600           SET EJ-GODK-POST TO TRUE                                       
132700        END-IF                                                            
132800     END-IF                                                               
132900                                                                          
133000     IF W-KDLEVPLF-D6 > SPACE                                             
133100        IF LPF-KDLEVPLF NOT = W-KDLEVPLF-D6                               
133200           SET EJ-GODK-POST TO TRUE                                       
133300        END-IF                                                            
133400     END-IF                                                               
133500     .                                                                    
133600     EJECT                                                                
133700                                                                          
133800 B-SKAPA-NY-BILD SECTION.                                                 
133900     MOVE JA TO SW-MSG-OK                                                 
134000     MOVE IDARTNR-WS TO W-IDARTNR                                         
134100                        TEST-IDARTNR                                      
134200     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
134300     MOVE KDBEHX-PLAN-WS TO W-KDBEHX-PLAN                                 
134400                                                                          
134500     PERFORM IMS-GET-ARTIKEL-ROT                                          
134600     IF  SEGMENT-FINNS                                                    
134700       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
134800       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
134900       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
135000* -- BEHÖRIG USER                                                         
135100         PERFORM IMS-GET-LEVA01-WDF101                                    
135200         IF SEGMENT-FINNS                                                 
135300           IF ART-KDERS-UTG = ZERO                                        
135400             PERFORM BA-UPPDAT-BILD-FRAN-ARTIKELREG                       
135500             PERFORM BB-UPPDAT-BILD-FRAN-LEVREG                           
135600             PERFORM BC-KOLL-WLC                                          
135700             PERFORM BE-KOLL-TREND                                        
135800             IF SW-MSG-OK = JA                                            
135900               IF W-KDBEHX-PLAN = FORSLAG                                 
136000                 MOVE MFS-FORMATETS-ATTR                                  
136100                                 TO MOD-IDARTNR-IN-ATTR                   
136200               ELSE                                                       
136300                 EVALUATE TRUE                                            
136400                 WHEN W-KDBEHX-PLAN = GALLANDE                            
136500                   MOVE MFS-OEPPNA-NUM-FAELT                              
136600                                   TO MOD-IDARTNR-IN-ATTR                 
136700                 END-EVALUATE                                             
136800               END-IF                                                     
136900               IF SW-LEVSEGM-FINNS = JA                                   
137000               OR W-IDLEVNR = '1000' OR W-IDLEVNR = '1002'                
137100                 PERFORM MFS-OEPPNA-INIT-UPPDAT-FAELT                     
137200               ELSE                                                       
137300                 IF SW-HUVUDLEVERANTOER = JA                              
137400                 AND W-MATINFO-KDAVT > ZERO                               
137500                   PERFORM MFS-KOEP-ENDA-UPPDAT-FAELT                     
137600                 END-IF                                                   
137700               END-IF                                                     
137800             END-IF                                                       
137900             MOVE W-KDLEVPLF TO MOD-KDLEVPLF-IN                           
138000             MOVE W-FLJIT    TO MOD-FLJIT-IN                              
138100           ELSE                                                           
138200             IF ART-KDERS-UTG < 20                                        
138300               MOVE FEL-6 (SPIND) TO MOD-MESSAGE                          
138400             ELSE                                                         
138500               MOVE FEL-7 (SPIND) TO MOD-MESSAGE                          
138600             END-IF                                                       
138700             PERFORM IMS-GET-CLAG-SEG                                     
138800             IF SEGMENT-FINNS                                             
138900               MOVE CLAG-IDANSK        TO W-IDANSK-L                      
139000               IF CLAG-IDDC-REF NOT = SPACE                               
139100                  MOVE ERR-REFILL-PART TO MED-IDMFSFEL                    
139200                  CALL WMEDKONV USING MED-WMEDAREA                        
139300                  MOVE MED-TEMFSFEL    TO MOD-MESSAGE                     
139400                  PERFORM MFS-STAENG-UPPDAT-FAELT                         
139500               END-IF                                                     
139600             END-IF                                                       
139700           END-IF                                                         
139800         ELSE                                                             
139900           MOVE FEL-3 (SPIND) TO MOD-MESSAGE                              
140000         END-IF                                                           
140100       ELSE                                                               
140200* -- OBHÖRIG USER                                                         
140300             MOVE FEL-10 (SPIND) TO MOD-MESSAGE                           
140400       END-IF                                                             
140500     ELSE                                                                 
140600       MOVE FEL-2 (SPIND) TO MOD-MESSAGE                                  
140700     END-IF                                                               
140800     .                                                                    
140900     EJECT                                                                
141000                                                                          
141100 BA-UPPDAT-BILD-FRAN-ARTIKELREG SECTION.                                  
141200     MOVE ART-IDLEVNR     TO W-HUVUDLEVNR                                 
141300     IF  W-IDLEVNR = SPACE                                                
141400       MOVE ART-IDLEVNR TO W-IDLEVNR                                      
141500                           IDLEVNR-WS                                     
141600                           MOD-IDLEVNR-UT                                 
141700     END-IF                                                               
141800     IF  W-IDLEVNR = ART-IDLEVNR                                          
141900       MOVE JA TO SW-HUVUDLEVERANTOER                                     
142000     ELSE                                                                 
142100       MOVE NEJ TO SW-HUVUDLEVERANTOER                                    
142200     END-IF                                                               
142300     MOVE ART-REKSIFFR     TO MOD-REKSIFFR                                
142400     MOVE STRECK           TO MOD-STRECK-1                                
142500     IF MSGI-IDLAND-SPR = 'GB'                                            
142600        MOVE 'GB'  TO W-IDSKYLT                                           
142700     ELSE                                                                 
142800        MOVE 'S'   TO W-IDSKYLT                                           
142900     END-IF                                                               
143000     PERFORM IMS-GET-BENA11-BSEQ                                          
143100     MOVE BENA11-TEXT-BEART TO MOD-BEART-SVE                              
143200                                                                          
143300     MOVE ART-TIFINLV         TO MOD-TIFINLV                              
143400     MOVE ART-TIURPROD        TO MOD-TIURPROD                             
143500                                                                          
143600     PERFORM IMS-GET-CLAG-SEG                                             
143700*                                                                         
143800     MOVE NEJ                 TO MOD-FLTREND                              
143900     IF SEGMENT-FINNS                                                     
144000        IF CLAG-KVPB-TREND NOT = +0                                       
144100           IF SPIND = +1                                                  
144200              MOVE JA         TO MOD-FLTREND                              
144300           ELSE                                                           
144400              MOVE YES        TO MOD-FLTREND                              
144500           END-IF                                                         
144600        END-IF                                                            
144700        IF CLAG-IDDC-REF NOT = SPACE                                      
144800           IF NOT MFS-UPDATE                                              
144900              MOVE ERR-REFILL-PART TO MED-IDMFSFEL                        
145000              CALL WMEDKONV USING MED-WMEDAREA                            
145100              MOVE MED-TEMFSFEL    TO MOD-MESSAGE                         
145200           END-IF                                                         
145300        END-IF                                                            
145400     END-IF                                                               
145500                                                                          
145600     IF W-IDLEVNR = W-HUVUDLEVNR                                          
145700       MOVE CLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                      
145800     ELSE                                                                 
145900       PERFORM IMS-GNP-ARTC23                                             
146000       MOVE NEJ TO TRAFF                                                  
146100       PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                         
146200         IF W-IDLEVNR = AVT-IDLEVNR-AVT                                   
146300           MOVE AVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                   
146400           MOVE JA TO TRAFF                                               
146500         END-IF                                                           
146600         PERFORM IMS-GNP-ARTC23                                           
146700       END-PERFORM                                                        
146800       IF TRAFF = NEJ                                                     
146900         MOVE W-IDLEVNR         TO MOD-IDLEVNR-SHIP-UT                    
147000       END-IF                                                             
147100     END-IF                                                               
147200*                                                                         
147300     MOVE CLAG-IDANSK       TO W-IDANSK-L                                 
147400     MOVE CLAG-TIOMSPEC     TO W-TIOMSPEC                                 
147500     MOVE CLAG-IDANSK       TO MOD-IDANSK                                 
147600     MOVE CLAG-KDAVT        TO MOD-KDAVT                                  
147700                               W-MATINFO-KDAVT                            
147800     MOVE CLAG-KVVECKOR-LT TO MOD-KVVECKOR-LT                             
147900     MOVE CLAG-KDLPSP       TO MOD-KDLPSP                                 
148000                               W-MATINFO-KDLPSP                           
148100     MOVE CLAG-TILPSP       TO W-TILPSP                                   
148200     IF W-TILPSP > ZERO                                                   
148300        MOVE W-TILPSP       TO MOD-TILPSP                                 
148400     ELSE                                                                 
148500        MOVE SPACE          TO MOD-TILPSP                                 
148600     END-IF                                                               
148700     MOVE CLAG-FLJIT        TO W-FLJIT                                    
148800     IF (CLAG-KDLEVPLF NOT = SPACE                                        
148900     AND CLAG-KDLEVPLF NOT = LOW-VALUE)                                   
149000        MOVE CLAG-KDLEVPLF  TO W-KDLEVPLF                                 
149100     ELSE                                                                 
149200        MOVE 'J'            TO W-KDLEVPLF                                 
149300     END-IF                                                               
149400     MOVE CLAG-KDHF         TO W-KDHF                                     
149500     IF  W-KDBEHX-PLAN = GALLANDE                                         
149600       IF MSGI-IDLAND-SPR = 'GB'                                          
149700          MOVE 'EXISTING' TO MOD-TEXT-PLANTYP                             
149800       ELSE                                                               
149900          MOVE 'GÄLLANDE' TO MOD-TEXT-PLANTYP                             
150000       END-IF                                                             
150100       MOVE 2 TO W-KDAVROP                                                
150200     ELSE                                                                 
150300       IF  W-KDBEHX-PLAN = FORSLAG                                        
150400         IF MSGI-IDLAND-SPR = 'GB'                                        
150500            MOVE 'PROPOSAL' TO MOD-TEXT-PLANTYP                           
150600         ELSE                                                             
150700            MOVE 'FÖRSLAG'  TO MOD-TEXT-PLANTYP                           
150800         END-IF                                                           
150900         MOVE 1 TO W-KDAVROP                                              
151000       ELSE                                                               
151100         IF  CLAG-KDLPSP = 5 AND SW-HUVUDLEVERANTOER = JA                 
151200           MOVE FORSLAG   TO W-KDBEHX-PLAN                                
151300           IF MSGI-IDLAND-SPR = 'GB'                                      
151400              MOVE 'PROPOSAL' TO MOD-TEXT-PLANTYP                         
151500           ELSE                                                           
151600              MOVE 'FÖRSLAG'  TO MOD-TEXT-PLANTYP                         
151700           END-IF                                                         
151800           MOVE 1 TO W-KDAVROP                                            
151900         ELSE                                                             
152000           IF MSGI-IDLAND-SPR = 'GB'                                      
152100              MOVE 'EXISTING' TO MOD-TEXT-PLANTYP                         
152200           ELSE                                                           
152300              MOVE 'GÄLLANDE' TO MOD-TEXT-PLANTYP                         
152400           END-IF                                                         
152500           MOVE GALLANDE   TO W-KDBEHX-PLAN                               
152600           MOVE 2 TO W-KDAVROP                                            
152700         END-IF                                                           
152800         MOVE W-KDBEHX-PLAN TO MOD-KDBEHX-PLAN-UT                         
152900         IF SPIND = 1                                                     
153000           CONTINUE                                                       
153100         ELSE                                                             
153200           IF KDBEHX-PLAN-WS = 'G'                                        
153300             MOVE 'E'          TO MOD-KDBEHX-PLAN-UT                      
153400           ELSE                                                           
153500             IF KDBEHX-PLAN-WS = 'F'                                      
153600               MOVE 'P'        TO MOD-KDBEHX-PLAN-UT                      
153700             END-IF                                                       
153800           END-IF                                                         
153900         END-IF                                                           
154000       END-IF                                                             
154100     END-IF                                                               
154200     MOVE W-KDBEHX-PLAN TO MOD-KDBEHX-PLAN-UT                             
154300     IF SPIND = 1                                                         
154400       CONTINUE                                                           
154500     ELSE                                                                 
154600       IF KDBEHX-PLAN-WS = 'G'                                            
154700         MOVE 'E'          TO MOD-KDBEHX-PLAN-UT                          
154800       ELSE                                                               
154900         IF KDBEHX-PLAN-WS = 'F'                                          
155000           MOVE 'P'        TO MOD-KDBEHX-PLAN-UT                          
155100         END-IF                                                           
155200       END-IF                                                             
155300     END-IF                                                               
155400     MOVE CLAG-KVPB-PLAN   TO W-KVPB-PLAN                                 
155500     MOVE CLAG-KVPB-SATS TO W-KVPB-SATS                                   
155600     PERFORM BAA-TEST-UPPLYSNING-3                                        
155700     MOVE W-KVPB-PLAN TO MOD-KVPB-PLAN                                    
155800     MOVE W-KVPB-SATS TO MOD-KVPB-SATS                                    
155900     IF CLAG-DAPBPLAN > ZERO                                              
156000        IF CLAG-DAPBPLAN < WS-DAGENS-AAAAMMDD                             
156100           MOVE MFS-FORMATETS-ATTR TO MOD-KVPB-PLAN-ATTR                  
156200           PERFORM BAB-BER-NYTT-MASK-KVPB-PLAN                            
156300        ELSE                                                              
156400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVPB-PLAN-ATTR               
156500        END-IF                                                            
156600     ELSE                                                                 
156700        MOVE MFS-FORMATETS-ATTR    TO MOD-KVPB-PLAN-ATTR                  
156800        PERFORM BAB-BER-NYTT-MASK-KVPB-PLAN                               
156900     END-IF                                                               
157000     IF CLAG-KVUTRS > ZERO AND W-MESSAGE-BOTTOM-3 = SPACE                 
157100       MOVE KOMMENTAR-TEXT (SPIND, 4)  TO W-MESSAGE-BOTTOM-3              
157200     END-IF                                                               
157300     IF CLAG-KDERS > 20                                                   
157400       MOVE FEL-6 (SPIND) TO MOD-MESSAGE                                  
157500     END-IF                                                               
157600     IF CLAG-KDERS > ZERO                                                 
157700       MOVE CLAG-KDERS               TO MOD-KDERS                         
157800       MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-KDERS-ATTR                    
157900     ELSE                                                                 
158000       MOVE MFS-RENSA-FAELT          TO MOD-KDERS                         
158100     END-IF                                                               
158200                                                                          
158300     MOVE CLAG-KDOTFREK              TO MOD-KDOTFREK                      
158400                                                                          
158500***** GET WDK625                                                          
158600     MOVE IDARTNR-WS TO W-IDARTNR                                         
158700     PERFORM IMS-GET-ARTIKEL-ROT                                          
158800     PERFORM IMS-GET-CLAG-SEG                                             
158900     MOVE 1                          TO W-KDNOTTYP                        
159000     PERFORM IMS-GET-ARTC25                                               
159100     IF SEGMENT-FINNS                                                     
159200        MOVE NOT-TEARTNOT            TO MOD-TEARTNOT1-IN                  
159300     ELSE                                                                 
159400        MOVE MFS-RENSA-FAELT         TO MOD-TEARTNOT1-IN                  
159500     END-IF                                                               
159600                                                                          
159700     MOVE 2                          TO W-KDNOTTYP                        
159800     PERFORM IMS-GET-ARTC25                                               
159900     IF SEGMENT-FINNS                                                     
160000        MOVE NOT-TEARTNOT            TO MOD-TEARTNOT2-IN                  
160100     ELSE                                                                 
160200        MOVE MFS-RENSA-FAELT         TO MOD-TEARTNOT2-IN                  
160300     END-IF                                                               
160400                                                                          
160500     PERFORM BAC-FLSEASON                                                 
160600                                                                          
160700**** GET WDD601                                                           
160800     IF SPAR-KEY-IDLEVNR > SPACE                                          
160900       MOVE SPAR-KEY-IDLEVNR     TO W-IDLEVNR-D6-MIN                      
161000                                    W-IDLEVNR-D6-MAX                      
161100     ELSE                                                                 
161200       MOVE LOW-VALUE             TO W-IDLEVNR-D6-MIN                     
161300       MOVE HIGH-VALUE            TO W-IDLEVNR-D6-MAX                     
161400       MOVE MSGI-IDLEVNR          TO SPAR-KEY-IDLEVNR-ENTER               
161500                                     W-IDLEVNR-D6-MIN                     
161600                                     W-IDLEVNR-D6-MAX                     
161700       MOVE MSGI-IDARTNR          TO WS-IDARTNR-9-RIGHT                   
161800       INSPECT WS-IDARTNR-9-RIGHT REPLACING LEADING ' ' BY '0'            
161900       MOVE    WS-IDARTNR-9-RIGHT TO  SPAR-KEY-IDARTNR-ENTER              
162000     END-IF                                                               
162100     IF SPAR-KEY-IDARTNR-ENTER NUMERIC                                    
162200       MOVE SPAR-KEY-IDARTNR-ENTER  TO W-IDARTNR-D6-MIN                   
162300       MOVE SPAR-KEY-IDARTNR-ENTER  TO W-IDARTNR-D6-MAX                   
162400     END-IF                                                               
162500     MOVE SPACE      TO MOD-TELPORSX                                      
162600     IF  SPAR-KEY-IDARTNR-ENTER NUMERIC                                   
162700*      IF  SPAR-KEY-IDLEVNR-ENTER > SPACE                                 
162800       IF  SPAR-KEY-IDARTNR-ENTER > ZERO                                  
162900         PERFORM IMS-GU-WDD601                                            
163000         PERFORM UNTIL SEGMENT-SAKNAS                                     
163100         OR          SEGMENT-SLUT                                         
163200         OR          (LPF-IDLEVNR = SPAR-KEY-IDLEVNR-ENTER                
163300         AND          LPF-IDARTNR = SPAR-KEY-IDARTNR-ENTER)               
163400           PERFORM IMS-GN-WDD601                                          
163500         END-PERFORM                                                      
163600         IF LPF-TELPORSX > SPACE                                          
163700           MOVE LPF-TELPORSX TO MOD-TELPORSX                              
163800         ELSE                                                             
163900           MOVE SPACE      TO MOD-TELPORSX                                
164000         END-IF                                                           
164100       END-IF                                                             
164200     END-IF                                                               
164300     .                                                                    
164400     EJECT                                                                
164500                                                                          
164600 BAA-TEST-UPPLYSNING-3 SECTION.                                           
164700     IF ART-FLERS = JA                                                    
164800       PERFORM BAAA-ERSATT-ARTIKEL                                        
164900       IF WS-FLERSATT-X2X3 = JA                                           
165000         MOVE OLD-CLAG-KDERS TO WS-MEDD-ERSKOD                            
165100         IF SPIND = 1                                                     
165200           MOVE 'ERSÄTTNING' TO WS-MEDD-TEXT                              
165300         ELSE                                                             
165400           MOVE 'SUPERSESSION'                                            
165500                             TO WS-MEDD-TEXT                              
165600         END-IF                                                           
165700         MOVE WS-MEDD-ERS    TO W-MESSAGE-BOTTOM-3                        
165800       END-IF                                                             
165900     END-IF                                                               
166000     PERFORM BAAB-TILLK-ARTIKEL                                           
166100     IF W-MESSAGE-BOTTOM-3 = SPACE                                        
166200       IF CLAG-KVPB-SEP NOT = CLAG-KVPB-VESL                              
166300         MOVE KOMMENTAR-TEXT (SPIND, 3)  TO W-MESSAGE-BOTTOM-3            
166400       END-IF                                                             
166500     END-IF                                                               
166600     IF CLAG-KDUART = 'P'                                                 
166700         MOVE KOMMENTAR-TEXT (SPIND, 6)  TO W-MESSAGE-BOTTOM-2            
166800     END-IF                                                               
166900                                                                          
167000     IF CLAG-REDIRLEV > ZERO                                              
167100         MOVE KOMMENTAR-TEXT (SPIND, 7)  TO W-MESSAGE-BOTTOM-2            
167200     END-IF                                                               
167300                                                                          
167400     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
167500     IF SQLCODE-WS = ZERO                                                 
167600       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
167700     END-IF                                                               
167800                                                                          
167900     MOVE ZERO               TO WS-ANTAL-KAMP                             
168000     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
168100                                WS-FLAGGA-W-S-KAMP                        
168200     PERFORM UNTIL SQLCODE > ZERO                                         
168300       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
168400         MOVE TP1KAMP-TISTODAT-KAMP                                       
168500                             TO WS-JMFR-AAAAMMDD                          
168600       ELSE                                                               
168700         MOVE TP1KAMP-TISTADAT-KAMP                                       
168800                             TO WS-JMFR-AAAAMMDD                          
168900       END-IF                                                             
169000       IF WS-JMFR-AA > 50                                                 
169100         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
169200       ELSE                                                               
169300         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
169400       END-IF                                                             
169500       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
169600* LÄGG TILL 5 ÅR                                                          
169700         ADD 50000           TO WS-JMFR-AAAAMMDD                          
169800       END-IF                                                             
169900       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
170000         IF TP1KAMP-KDKAMP = 'Q'                                          
170100           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
170200         END-IF                                                           
170300         IF TP1KAMP-KDKAMP = 'W'                                          
170400         OR TP1KAMP-KDKAMP = 'S'                                          
170500           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
170600         END-IF                                                           
170700       END-IF                                                             
170800       ADD 1                 TO WS-ANTAL-KAMP                             
170900       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
171000     END-PERFORM                                                          
171100     IF  WS-FLAGGA-Q-KAMP   = JA                                          
171200     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
171300       MOVE 'CAMPAIGN       '                                             
171400                             TO W-MESSAGE-BOTTOM-2                        
171500     ELSE                                                                 
171600       IF WS-FLAGGA-W-S-KAMP = JA                                         
171700         MOVE 'CAMPAIGN     '                                             
171800                             TO W-MESSAGE-BOTTOM-2                        
171900       END-IF                                                             
172000     END-IF                                                               
172100     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
172200     .                                                                    
172300     EJECT                                                                
172400                                                                          
172500 BAAA-ERSATT-ARTIKEL SECTION.                                             
172600     MOVE NEJ                TO WS-FLERSATT-X2X3                          
172700     MOVE ART-IDARTNR        TO W-IDARTNR-MIN7                            
172800                                W-IDARTNR-MAX7                            
172900     MOVE MFS-RENSA-FAELT    TO MOD-REPLACES                              
173000     MOVE SPACE              TO MOD-REPLACES                              
173100     PERFORM IMS-GU-WDD7A1-MINMAX                                         
173200     PERFORM UNTIL SEGMENT-SAKNAS                                         
173300       IF ERS-IDARTNR NOT = ZERO                                          
173400         MOVE ERS-IDARTNR    TO W-IDARTNR                                 
173500         IF MOD-REPLACES = SPACE                                          
173600           MOVE ERS-IDARTNR    TO MOD-REPLACES                            
173700           INSPECT MOD-REPLACES REPLACING LEADING ZERO BY SPACE           
173800         END-IF                                                           
173900         PERFORM IMS-GET-ARTIKEL-ROT-OLD                                  
174000         IF SEGMENT-FINNS                                                 
174100            PERFORM IMS-GET-CLAG-SEG-OLD                                  
174200            IF SEGMENT-FINNS                                              
174300              IF OLD-CLAG-KDERS = 22                                      
174400              OR OLD-CLAG-KDERS = 23                                      
174500                COMPUTE TMP1-YYWW =                                       
174600                     OLD-ART-TIERSDAT / 10                                
174700                MOVE W-AAVV-MINUS-HALVAR                                  
174800                              TO TMP2-YYWW                                
174900                PERFORM WY2000P3                                          
175000                IF TMP1-YYWW >= TMP2-YYWW                                 
175100                  MOVE JA       TO WS-FLERSATT-X2X3                       
175200                END-IF                                                    
175300              END-IF                                                      
175400              IF OLD-CLAG-KDERS = 02                                      
175500              OR OLD-CLAG-KDERS = 03                                      
175600                MOVE JA       TO WS-FLERSATT-X2X3                         
175700              END-IF                                                      
175800            END-IF                                                        
175900         END-IF                                                           
176000       END-IF                                                             
176100       PERFORM IMS-GN-WDD7A1-MINMAX                                       
176200       IF SEGMENT-FINNS                                                   
176300       AND MOD-REPLACES NOT = SPACE                                       
176400         MOVE 'VARIOUS' TO MOD-REPLACES                                   
176500       END-IF                                                             
176600     END-PERFORM                                                          
176700     .                                                                    
176800     EJECT                                                                
176900                                                                          
177000 BAAB-TILLK-ARTIKEL SECTION.                                              
177100     MOVE ART-IDARTNR        TO W-IDARTNR                                 
177200     MOVE MFS-RENSA-FAELT    TO MOD-REPL-BY                               
177300     PERFORM IMS-GU-WDD7-ERSA01                                           
177400     IF SEGMENT-FINNS                                                     
177500        PERFORM IMS-GNP-WDD7-ERSA11                                       
177600        IF SEGMENT-FINNS                                                  
177700           MOVE ERSA11-IDARTNR-TILLK TO MOD-REPL-BY                       
177800           INSPECT MOD-REPL-BY REPLACING LEADING ZERO BY SPACE            
177900           PERFORM IMS-GNP-WDD7-ERSA11                                    
178000           IF SEGMENT-FINNS                                               
178100             MOVE 'VARIOUS'  TO MOD-REPL-BY                               
178200           END-IF                                                         
178300        END-IF                                                            
178400     END-IF                                                               
178500     MOVE ART-IDARTNR        TO W-IDARTNR                                 
178600     .                                                                    
178700     EJECT                                                                
178800                                                                          
178900 BAB-BER-NYTT-MASK-KVPB-PLAN  SECTION.                                    
179000                                                                          
179100     MOVE ART-IDARTNR TO PBTO-IDARTNR                                     
179200     CALL W222PBTO USING PBTO-W222PBTO                                    
179300                         PBTO-WDK6-PCB                                    
179400                         PBTO-WDK7-PCB                                    
179500                         PBTO-ARTM-PCB                                    
179600                         PBTO-2501-PCB                                    
179700                         PBTO-WDB6R-PCB                                   
179800                         PBTO-WDK7R-PCB                                   
179900                         PBTO-WDB6-PCB                                    
180000                         PBTO-WDD7-PCB                                    
180100                         PBTO-WDK7E-PCB                                   
180200                         PBTO-W222-UTIL-WDK6-PCB                          
180300                         PBTO-W222-UTIL-WDK7-PCB                          
180400                         PBTO-W222-UTIL-WDB6-PCB                          
180500                         PBTO-W222-UTUP-WDK7-PCB                          
180600                         PBTO-W222-UTUP-WDB6-PCB                          
180700                         PBTO-W222-UTUP-UTIL-WDK6-PCB                     
180800                         PBTO-W222-UTUP-UTIL-WDK7-PCB                     
180900                         PBTO-W222-UTUP-UTIL-WDB6-PCB                     
181000                                                                          
181100     IF PBTO-KDSVAR = JA                                                  
181200        MOVE PBTO-KVPB-PLAN TO MOD-KVPB-PLAN                              
181300     END-IF                                                               
181400     .                                                                    
181500     EJECT                                                                
181600                                                                          
181700 BAC-FLSEASON SECTION.                                                    
181800                                                                          
181900     MOVE NEJ          TO MOD-FLSEASON                                    
182000                                                                          
182100     MOVE IDARTNR-WS TO W-IDARTNR                                         
182200     PERFORM IMS-GET-WDK626                                               
182300     IF SEGMENT-FINNS                                                     
182400       MOVE +1        TO IX-SEASON                                        
182500       PERFORM UNTIL IX-SEASON > 12                                       
182600         IF JUST-RESEASON (IX-SEASON) NOT = +1                            
182700           IF SPIND = +1                                                  
182800             MOVE JA  TO MOD-FLSEASON                                     
182900           ELSE                                                           
183000             MOVE YES TO MOD-FLSEASON                                     
183100           END-IF                                                         
183200         END-IF                                                           
183300         ADD +1       TO IX-SEASON                                        
183400       END-PERFORM                                                        
183500     END-IF                                                               
183600     .                                                                    
183700     EJECT                                                                
183800 BB-UPPDAT-BILD-FRAN-LEVREG SECTION.                                      
183900     MOVE NEJ         TO SW-LEVSEGM-FINNS                                 
184000     MOVE ZERO        TO W-ANTAL-LEVNR                                    
184100     MOVE IDARTNR-WS  TO W-IDARTNR-D9                                     
184200     MOVE WC-CDC-SE   TO W-IDDC-D9                                        
184300     PERFORM IMS-GET-LEVART-SEG                                           
184400     IF SEGMENT-FINNS                                                     
184500        PERFORM IMS-GET-LEVERANTOER-SEG                                   
184600        PERFORM UNTIL                                                     
184700        NOT ( SEGMENT-FINNS )                                             
184800          ADD 1 TO W-ANTAL-LEVNR                                          
184900                                                                          
185000          IF LEVNR-IDLEVNR = W-IDLEVNR                                    
185100            MOVE JA TO SW-LEVSEGM-FINNS                                   
185200            MOVE LEVNR-KVBR TO W-KVBR                                     
185300                               W-LEVNR-KVBR                               
185400                               MOD-KVBR                                   
185500            MOVE LEVNR-TILEVPL TO W-TILEVPL                               
185600            IF W-KDBEHX-PLAN = GALLANDE                                   
185700              MOVE LEVNR-TILEVPL TO W-TIOMSPEC                            
185800              MOVE W-TIOMSPEC    TO MOD-TIOMSPEC                          
185900            ELSE                                                          
186000              IF W-KDBEHX-PLAN = FORSLAG                                  
186100                MOVE SPACE        TO W-TIOMSPEC-1-2                       
186200                MOVE W-TIOMSPEC-X TO MOD-TIOMSPEC                         
186300              END-IF                                                      
186400            END-IF                                                        
186500            PERFORM BBA-LAES-OMSPEC                                       
186600            PERFORM BBB-LAES-AVROP-TILL-MOD                               
186700          END-IF                                                          
186800          PERFORM IMS-GET-LEVERANTOER-SEG                                 
186900        END-PERFORM                                                       
187000     END-IF                                                               
187100     IF SW-LEVSEGM-FINNS = NEJ AND W-IDLEVNR NOT = '1000'                 
187200     AND W-IDLEVNR NOT = '1002'                                           
187300        CONTINUE                                                          
187400     ELSE                                                                 
187500       IF W-IDLEVNR NOT = W-HUVUDLEVNR                                    
187600          MOVE KOMMENTAR-TEXT (SPIND, 1)  TO W-MESSAGE-BOTTOM-1           
187700       ELSE                                                               
187800         IF W-ANTAL-LEVNR > 1                                             
187900           MOVE KOMMENTAR-TEXT (SPIND, 2)  TO W-MESSAGE-BOTTOM-1          
188000         END-IF                                                           
188100       END-IF                                                             
188200     END-IF                                                               
188300     IF BYT02-RENOV                                                       
188400       IF BYT16-BYTES                                                     
188500         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
188600                                  6000                                    
188700         END-COMPUTE                                                      
188800       ELSE                                                               
188900         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
189000                                  1000                                    
189100         END-COMPUTE                                                      
189200       END-IF                                                             
189300       PERFORM IMS-GU-WDA901                                              
189400       IF SEGMENT-FINNS                                                   
189500         PERFORM IMS-GU-WDB201-BSEQ                                       
189600         IF SEGMENT-FINNS                                                 
189700           MOVE GMT-IDDISTR     TO W-IDDISTR-WDA9                         
189800           PERFORM IMS-GU-WDA911                                          
189900           IF SEGMENT-FINNS                                               
190000* FIX SOM UTÖKAS FÖR VARJE RENOVÖR SOM ANSLUTS TILL WEB:EN                
190100             MOVE UPD-IDDISTR   TO TEST-IDDISTR                           
190200             IF DIS134-BYTESREN-WEB                                       
190300               ADD UPD-KVLS-REM TO WS-KVLS-REM                            
190400             END-IF                                                       
190500           END-IF                                                         
190600         END-IF                                                           
190700       END-IF                                                             
190800     END-IF                                                               
190900     .                                                                    
191000     EJECT                                                                
191100                                                                          
191200 BBA-LAES-OMSPEC SECTION.                                                 
191300                                                                          
191400     PERFORM IMS-GET-OMSPEC-SEG                                           
191500     IF SEGMENT-FINNS                                                     
191600       MOVE OMSPEC-DASPECST TO W-DASPECST                                 
191700       MOVE W-TISPECST      TO W-OMSPEC-TISPECST                          
191800       MOVE OMSPEC-KVBEST-PL TO W-OMSPEC-KVBEST-PL                        
191900       IF (W-KDBEHX-PLAN = GALLANDE AND OMSPEC-KDPLKOEP = 2)              
192000       OR (W-KDBEHX-PLAN = FORSLAG AND OMSPEC-KDPLKOEP NOT = 2)           
192100         MOVE OMSPEC-KVBEST-PL TO MOD-KVBEST-PL                           
192200       ELSE                                                               
192300         MOVE ZERO TO MOD-KVBEST-PL                                       
192400       END-IF                                                             
192500       ADD  OMSPEC-KVBEST-PL TO W-KVBR                                    
192600       MOVE W-KVBR           TO MOD-KVBR                                  
192700       MOVE OMSPEC-KDLPORS-TAB (1) TO W-ORSAK-TAB-KOD (1)                 
192800       MOVE OMSPEC-KDLPORS-TAB (2) TO W-ORSAK-TAB-KOD (2)                 
192900       MOVE OMSPEC-KDLPORS-TAB (3) TO W-ORSAK-TAB-KOD (3)                 
193000       PERFORM S03-ORSAKSTEXTER-TILL-MOD                                  
193100     ELSE                                                                 
193200       IF W-KDBEHX-PLAN = FORSLAG                                         
193300         MOVE NEJ TO SW-MSG-OK                                            
193400         MOVE FEL-9 (SPIND) TO MOD-MESSAGE                                
193500       END-IF                                                             
193600       MOVE ZERO TO MOD-KVBEST-PL                                         
193700                    W-OMSPEC-TISPECST                                     
193800                    W-OMSPEC-KVBEST-PL                                    
193900     END-IF                                                               
194000     .                                                                    
194100     EJECT                                                                
194200 BBB-LAES-AVROP-TILL-MOD SECTION.                                         
194300* AVROP LÄSES. AVROPEN MELLANLAGRAS I WS.                     *           
194400     PERFORM BBBA-INITIERA-TABELL                                         
194500     PERFORM IMS-GET-AVROP-OKVAL-NEXT                                     
194600     MOVE ZERO TO IX-GAM                                                  
194700     PERFORM UNTIL                                                        
194800     NOT ( SEGMENT-FINNS )                                                
194900       MOVE AVROP-DAAVROP-AVS    TO W-DAAVROP-AVS                         
195000       MOVE W-TIAVROP-AVS        TO TMP1-YYWW                             
195100       MOVE W-OMSPEC-TISPECST    TO TMP2-YYWW                             
195200       PERFORM WY2000P3                                                   
195300       IF TMP1-YYWW < TMP2-YYWW                                           
195400       AND AVROP-KDAVROP  =  2                                            
195500       OR (TMP1-YYWW NOT < TMP2-YYWW                                      
195600       AND W-KDBEHX-PLAN = FORSLAG AND  AVROP-KDAVROP = 1)                
195700       OR (TMP1-YYWW NOT < TMP2-YYWW                                      
195800       AND W-KDBEHX-PLAN = GALLANDE AND AVROP-KDAVROP = 2)                
195900         MOVE NEJ                 TO AVROP-INL-PASSERAT-SW                
196000         PERFORM S08-KOLLA-AVROP-INL-SW                                   
196100         MOVE AVROP-DAAVROP-AVS   TO W-DAAVROP-AVS                        
196200         MOVE W-TIAVROP-AVS       TO TMP1-YYWW                            
196300         MOVE W-DATUM-AKTUELLT    TO TMP2-YYWW                            
196400         PERFORM WY2000P3                                                 
196500         IF  TMP1-YYWW < TMP2-YYWW                                        
196600           PERFORM S04-GAMMALT-AVROP-TILL-TAB                             
196700         ELSE                                                             
196800           PERFORM S01-NYA-AVROP-TILL-TAB                                 
196900         END-IF                                                           
197000       END-IF                                                             
197100       PERFORM IMS-GET-AVROP-OKVAL-NEXT                                   
197200     END-PERFORM                                                          
197300     PERFORM S05-GAM-AVROP-I-TAB-TILL-MOD                                 
197400     PERFORM S06-NYA-AVROP-I-TAB-TILL-MOD                                 
197500     .                                                                    
197600     EJECT                                                                
197700                                                                          
197800 BBBA-INITIERA-TABELL SECTION.                                            
197900     MOVE W-START-PER-AA TO W-PERIOD-AA                                   
198000     MOVE W-START-PER-PP TO W-PERIOD-PP                                   
198100     MOVE W-PERIOD-AAPP  TO W-PERIOD-AAPP-START                           
198200     MOVE 1 TO IY                                                         
198300     PERFORM UNTIL                                                        
198400     ( IY > MAX-ANT-PERIODER-I-TAB )                                      
198500       MOVE W-PERIOD-AA  TO MOD-PERIOD-AA (IY)                            
198600       MOVE W-PERIOD-PP  TO MOD-PERIOD-PP (IY)                            
198700                            W-PERIOD-TAB-RAD (IY)                         
198800       IF  W-PERIOD-PP = 12                                               
198900         ADD 1 TO W-PERIOD-AA                                             
199000         MOVE 1 TO W-PERIOD-PP                                            
199100       ELSE                                                               
199200         ADD 1 TO W-PERIOD-PP                                             
199300       END-IF                                                             
199400       ADD 1 TO IY                                                        
199500     END-PERFORM                                                          
199600     PERFORM BBBAA-ANT-VV-I-PER                                           
199700     PERFORM S07-NOLLSTAELL-AVROPSTABELLER                                
199800     .                                                                    
199900     EJECT                                                                
200000                                                                          
200100 BBBAA-ANT-VV-I-PER     SECTION.                                          
200200     MOVE +1 TO IY IX                                                     
200300     PERFORM UNTIL IY > 24                                                
200400        IF PER-AAPP (IY) = W-PERIOD-AAPP-START                            
200500           MOVE IY TO IX                                                  
200600           MOVE 24 TO IY                                                  
200700        END-IF                                                            
200800        ADD +1 TO IY                                                      
200900     END-PERFORM                                                          
201000                                                                          
201100     MOVE +1 TO IY                                                        
201200     PERFORM UNTIL IY > MAX-ANT-PERIODER-I-TAB                            
201300        MOVE PARENTES        TO MOD-PERIOD-PARENTES(IY)                   
201400        ADD +1 TO IY IX                                                   
201500     END-PERFORM                                                          
201600     .                                                                    
201700     EJECT                                                                
201800                                                                          
201900 BC-KOLL-WLC   SECTION.                                                   
202000                                                                          
202100     IF CLAG-FLSKROT-WLC = JA                                             
202200        IF W-MESSAGE-BOTTOM-1 = SPACE                                     
202300          MOVE KOMMENTAR-TEXT (SPIND, 08)    TO W-MESSAGE-BOTTOM-1        
202400        ELSE                                                              
202500           IF W-MESSAGE-BOTTOM-2 = SPACE                                  
202600             MOVE KOMMENTAR-TEXT (SPIND, 08) TO W-MESSAGE-BOTTOM-2        
202700           ELSE                                                           
202800             MOVE KOMMENTAR-TEXT (SPIND, 08) TO W-MESSAGE-BOTTOM-3        
202900           END-IF                                                         
203000        END-IF                                                            
203100     END-IF                                                               
203200     .                                                                    
203300     EJECT                                                                
203400                                                                          
203500 BE-KOLL-TREND SECTION.                                                   
203600                                                                          
203700     IF CLAG-KVPB-TREND  NOT = ZERO                                       
203800        IF W-MESSAGE-BOTTOM-1 = SPACE                                     
203900          MOVE KOMMENTAR-TEXT (SPIND, 09)    TO W-MESSAGE-BOTTOM-1        
204000        ELSE                                                              
204100          IF W-MESSAGE-BOTTOM-2 = SPACE                                   
204200             MOVE KOMMENTAR-TEXT (SPIND, 09) TO W-MESSAGE-BOTTOM-2        
204300          ELSE                                                            
204400           IF W-MESSAGE-BOTTOM-3 = SPACE                                  
204500             MOVE KOMMENTAR-TEXT (SPIND, 09) TO W-MESSAGE-BOTTOM-3        
204600           END-IF                                                         
204700          END-IF                                                          
204800        END-IF                                                            
204900     END-IF                                                               
205000     .                                                                    
205100     EJECT                                                                
205200                                                                          
205300 C-TESTA-INDATA-IFYLLT SECTION.                                           
205400     MOVE NEJ TO W-TESTA-INDATA                                           
205500     MOVE 1 TO IX                                                         
205600     PERFORM UNTIL                                                        
205700     NOT ( IX < 5 )                                                       
205800       IF MID-TIAVROP-AVS (IX) = AAVV OR YYWW                             
205900         CONTINUE                                                         
206000       ELSE                                                               
206100         EVALUATE TRUE                                                    
206200         WHEN MID-TIAVROP-AVS (IX) NOT = SPACE                            
206300           MOVE JA TO W-TESTA-INDATA                                      
206400         END-EVALUATE                                                     
206500       END-IF                                                             
206600       ADD 1 TO IX                                                        
206700     END-PERFORM                                                          
206800     IF MID-KOMKOD NOT = SPACE                                            
206900       MOVE JA TO W-TESTA-INDATA                                          
207000     END-IF                                                               
207100     IF MID-KVBEST-PL NOT = SPACE                                         
207200       MOVE JA TO W-TESTA-INDATA                                          
207300     END-IF                                                               
207400     IF MID-KDOMSPEC NOT = SPACE                                          
207500       MOVE JA TO W-TESTA-INDATA                                          
207600     END-IF                                                               
207700     IF MID-TILPSP = AAVV OR YYWW                                         
207800       CONTINUE                                                           
207900     ELSE                                                                 
208000       EVALUATE TRUE                                                      
208100       WHEN MID-TILPSP NOT = SPACE                                        
208200         MOVE JA TO W-TESTA-INDATA                                        
208300       END-EVALUATE                                                       
208400     END-IF                                                               
208500     IF MID-KDLEVPLF NOT = SPACE                                          
208600       MOVE JA TO W-TESTA-INDATA                                          
208700     END-IF                                                               
208800     IF MID-FLJIT = '+' OR SPACE                                          
208900       CONTINUE                                                           
209000     ELSE                                                                 
209100       MOVE JA TO W-TESTA-INDATA                                          
209200     END-IF                                                               
209300                                                                          
209400     IF MID-TEARTNOT1 = '+' OR SPACE                                      
209500       CONTINUE                                                           
209600     ELSE                                                                 
209700       MOVE JA TO W-TESTA-INDATA                                          
209800     END-IF                                                               
209900     IF MID-TEARTNOT2 = '+' OR SPACE                                      
210000       CONTINUE                                                           
210100     ELSE                                                                 
210200       MOVE JA TO W-TESTA-INDATA                                          
210300     END-IF                                                               
210400                                                                          
210500     IF W-INDATA-FINNS                                                    
210600       MOVE KOMMENTAR-TEXT (SPIND, 5) TO MOD-MESSAGE                      
210700     ELSE                                                                 
210800       MOVE IDARTNR-WS                TO W-IDARTNR                        
210900       PERFORM IMS-GET-ART-CLAG-SEG                                       
211000       IF SEGMENT-FINNS                                                   
211100         IF CLAG-IDDC-REF NOT = SPACE                                     
211200            MOVE ERR-REFILL-PART      TO MED-IDMFSFEL                     
211300            CALL WMEDKONV          USING MED-WMEDAREA                     
211400            MOVE MED-TEMFSFEL         TO MOD-MESSAGE                      
211500         END-IF                                                           
211600       END-IF                                                             
211700     END-IF                                                               
211800     .                                                                    
211900     EJECT                                                                
212000                                                                          
212100 D-PPSW-2139            SECTION.                                          
212200     MOVE IDARTNR-WS     TO PROGSW-IDARTNR-IN                             
212300     MOVE KDBEHX-PLAN-WS TO PROGSW-KDBEHX-PLAN-IN                         
212400                            PROGSW-KDBEHX-PLAN-UT                         
212500     MOVE IDLEVNR-WS     TO PROGSW-IDLEVNR-IN                             
212600     MOVE '2139'         TO P-IDTRANS                                     
212700                                                                          
212800     PERFORM IMS-INSERT-ALT                                               
212900     .                                                                    
213000     EJECT                                                                
213100                                                                          
213200 S01-NYA-AVROP-TILL-TAB SECTION.                                          
213300     MOVE AVROP-DAAVROP-AVS TO W-DAAVROP-AVS                              
213400     MOVE W-TIAVROP-AVS     TO W-DATUM-AAVV                               
213500     PERFORM S02-BERAKNA-INDEX-I-AVROPSTAB                                
213600                                                                          
213700     IF IY-PERIOD NOT > MAX-ANT-PERIODER-I-TAB                            
213800       ADD  AVROP-KVAVROP TO W-KVAVROP-TAB (IY-PERIOD, IX-VECKA)          
213900       IF AVROP-INL-PASSERAT-SW = JA                                      
214000          MOVE JA TO W-AVROP-TAB-INL-PAST(IY-PERIOD, IX-VECKA)            
214100       END-IF                                                             
214200       MOVE AVROP-DAAVROP-AVS TO W-DAAVROP-AVS                            
214300       MOVE W-TIAVROP-AVS     TO W-DATUM-AAVV                             
214400       MOVE W-DATUM-VV TO W-TIAVROP-AVS-TAB (IY-PERIOD, IX-VECKA)         
214500     END-IF                                                               
214600     .                                                                    
214700     EJECT                                                                
214800                                                                          
214900 S02-BERAKNA-INDEX-I-AVROPSTAB SECTION.                                   
215000* BERÄKNING AV INDEX I TABELL FÖR PERIOD/VECKA (RADER/KOLUMN) *           
215100*                                                              *          
215200* MED UTGÅNGSPUNKT FRÅN START-PERIOD (ÅÅPP) BERÄKNAS INDEX    *           
215300* IY-PERIOD OCH IX-VECKA FÖR GIVEN VECKA (ÅÅVV)               *           
215400* FÖRUTSÄTTNINGAR:                                            *           
215500* - GIVEN VECKA INTE < AKTUELL VECKA                      *               
215600* - IY-PERIOD KAN VARA > MAX-ANT-PERIODER-I-TAB           *               
215700* - FÄLTEN W-START-PERIOD, W-START-PER-AA, W-START-PER-PP *               
215800* - SKALL VARA INITIERADE                                 *               
215900     IF  W-DATUM-AAVV = ZERO                                              
216000       MOVE W-DATUM-AKTUELLT TO W-DATUM-AAVV                              
216100     END-IF                                                               
216200     IF  W-DATUM-AA NOT = W-START-PER-AA                                  
216300       MOVE 12 TO IY                                                      
216400     ELSE                                                                 
216500       MOVE ZERO TO IY                                                    
216600     END-IF                                                               
216700     MOVE ZERO TO IX                                                      
216800     MOVE IY TO IX-TOT                                                    
216900     PERFORM UNTIL                                                        
217000     NOT ( IX < MAX-ANT-PERIODER )                                        
217100       ADD 1 TO IX                                                        
217200                IX-TOT                                                    
217300       IF  W-DATUM-VV NOT < PER-START-VV (IX-TOT)                         
217400            AND W-DATUM-VV NOT > PER-SLUT-VV  (IX-TOT)                    
217500         MOVE IX TO W-PER-PP                                              
217600       END-IF                                                             
217700     END-PERFORM                                                          
217800     MOVE W-PER-PP TO IX-TOT                                              
217900     ADD IY TO IX-TOT                                                     
218000                                                                          
218100     MOVE W-DATUM-AA  TO W-BER-PERIOD                                     
218200     MULTIPLY 100 BY W-BER-PERIOD                                         
218300     ADD W-PER-PP TO W-BER-PERIOD                                         
218400                                                                          
218500                                                                          
218600     MOVE ZERO       TO IY-PERIOD                                         
218700     MOVE W-DATUM-VV TO IX-VECKA                                          
218800     SUBTRACT PER-START-VV (IX-TOT) FROM IX-VECKA                         
218900     ADD 1 TO IX-VECKA                                                    
219000     IF  IX-VECKA > MAX-ANT-VECKOR-I-TAB                                  
219100         MOVE MAX-ANT-VECKOR-I-TAB TO IX-VECKA                            
219200     END-IF                                                               
219300                                                                          
219400     MOVE W-BER-PERIOD   TO W-PERIOD-AAPP                                 
219500     MOVE W-PERIOD-AA    TO TMP1-YY                                       
219600     MOVE W-START-PER-AA TO TMP2-YY                                       
219700     PERFORM WY2000P9                                                     
219800     COMPUTE IY-PERIOD = IY-PERIOD +                                      
219900             (TMP1-YY - TMP2-YY) * 12 +                                   
220000             W-PERIOD-PP - W-START-PER-PP + 1                             
220100                                                                          
220200     MOVE W-START-PER-AA TO W-PERIOD-AA                                   
220300     MOVE 5              TO W-PERIOD-PP                                   
220400     .                                                                    
220500     EJECT                                                                
220600                                                                          
220700 S03-ORSAKSTEXTER-TILL-MOD SECTION.                                       
220800* ORSAKSTEXTER FRÅN TABELL W201W005 FLYTTAS MED HJÄLP         *           
220900* AV TABELLEN FÖR ORSAKSKODER I WS                            *           
221000     IF W-KDBEHX-PLAN = FORSLAG                                           
221100     OR (W-KDBEHX-PLAN = GALLANDE AND W-MATINFO-KDLPSP NOT = 5)           
221200       IF  W-ORSAK-TAB-KOD (1) > ZERO                                     
221300       AND W-ORSAK-TAB-KOD (1) NOT > MAX-ANT-ORSAKSKODER                  
221400         MOVE W-ORSAK-TAB-KOD (1) TO IX                                   
221500         MOVE TELPORS (IX) TO MOD-TEXT-ORSAK1                             
221600         IF W-ORSAK-AENDRAD (1) = JA                                      
221700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEXT-ORSAK1-ATTR             
221800         END-IF                                                           
221900       ELSE                                                               
222000         MOVE MFS-RENSA-FAELT TO MOD-TEXT-ORSAK1                          
222100       END-IF                                                             
222200       IF W-ORSAK-TAB-KOD (2) > ZERO                                      
222300       AND W-ORSAK-TAB-KOD (2) NOT > MAX-ANT-ORSAKSKODER                  
222400         MOVE W-ORSAK-TAB-KOD (2) TO IX                                   
222500         MOVE TELPORS (IX) TO MOD-TEXT-ORSAK2                             
222600         IF W-ORSAK-AENDRAD (2) = JA                                      
222700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEXT-ORSAK2-ATTR             
222800         END-IF                                                           
222900       ELSE                                                               
223000         MOVE MFS-RENSA-FAELT TO MOD-TEXT-ORSAK2                          
223100       END-IF                                                             
223200     END-IF                                                               
223300     .                                                                    
223400     EJECT                                                                
223500                                                                          
223600 S04-GAMMALT-AVROP-TILL-TAB SECTION.                                      
223700* GAMLA AVROP LÄGGS I TABELL I WS. TABELLEN MOTSVARAR         *           
223800* RADEN MED GAMLA AVROP I BILDEN                              *           
223900     MOVE +1                 TO IX-KOLL                                   
224000     MOVE AVROP-DAAVROP-AVS  TO W-DAAVROP-AVS                             
224100     PERFORM UNTIL IX-KOLL > 5                                            
224200        IF W-TIAVROP-AVS-GAM (IX-KOLL) = W-TIAVROP-AVS                    
224300           ADD AVROP-KVAVROP TO W-KVAVROP-GAM (IX-KOLL)                   
224400           IF AVROP-INL-PASSERAT-SW = JA                                  
224500              MOVE JA TO W-AVROP-GAM-INL-PAST(IX-KOLL)                    
224600           END-IF                                                         
224700           MOVE +10          TO IX-KOLL                                   
224800        END-IF                                                            
224900        ADD +1               TO IX-KOLL                                   
225000     END-PERFORM                                                          
225100     IF IX-KOLL > 10                                                      
225200        CONTINUE                                                          
225300     ELSE                                                                 
225400        ADD 1 TO IX-GAM                                                   
225500        IF IX-GAM > 5                                                     
225600          ADD W-KVAVROP-GAM (2) TO W-KVAVROP-GAM (1)                      
225700          MOVE 9999 TO W-TIAVROP-AVS-GAM (1)                              
225800          MOVE 2 TO IX-GAM                                                
225900          MOVE 3 TO IX-PLUS-1                                             
226000          PERFORM UNTIL                                                   
226100          NOT ( IX-GAM < MAX-ANT-GAMLA-AVROP )                            
226200            MOVE W-KVAVROP-GAM (IX-PLUS-1)                                
226300                               TO W-KVAVROP-GAM (IX-GAM)                  
226400            MOVE W-TIAVROP-AVS-GAM (IX-PLUS-1)                            
226500                               TO W-TIAVROP-AVS-GAM (IX-GAM)              
226600            IF W-AVROP-GAM-INL-PAST(IX-PLUS-1) = JA                       
226700               MOVE W-AVROP-GAM-INL-PAST(IX-PLUS-1)                       
226800                               TO W-AVROP-GAM-INL-PAST(IX-GAM)            
226900            ELSE                                                          
227000               MOVE NEJ        TO W-AVROP-GAM-INL-PAST(IX-GAM)            
227100            END-IF                                                        
227200            ADD 1 TO IX-GAM                                               
227300                     IX-PLUS-1                                            
227400          END-PERFORM                                                     
227500          MOVE 5 TO IX-GAM                                                
227600        END-IF                                                            
227700        MOVE AVROP-KVAVROP     TO  W-KVAVROP-GAM (IX-GAM)                 
227800        MOVE AVROP-DAAVROP-AVS TO  W-DAAVROP-AVS                          
227900        MOVE W-TIAVROP-AVS     TO  W-TIAVROP-AVS-GAM (IX-GAM)             
228000                                                                          
228100        IF AVROP-INL-PASSERAT-SW = JA                                     
228200           MOVE JA             TO W-AVROP-GAM-INL-PAST(IX-GAM)            
228300        ELSE                                                              
228400           MOVE NEJ            TO W-AVROP-GAM-INL-PAST(IX-GAM)            
228500        END-IF                                                            
228600     END-IF                                                               
228700     .                                                                    
228800     EJECT                                                                
228900                                                                          
229000 S05-GAM-AVROP-I-TAB-TILL-MOD SECTION.                                    
229100* I BILDEN   ÅTSKILJS VÄRDE OCH VECKA MED  -  FÖR DE          *           
229200* AVROP SOM RYMMS INOM  KÖP + BESTÄLLNINGSREST.               *           
229300* FÖR ÖVRIGA AVROP ANVÄNDES *.                                *           
229400     MOVE W-LEVNR-KVBR TO W-KVBR                                          
229500     ADD W-OMSPEC-KVBEST-PL TO W-KVBR                                     
229600     MOVE STRECK TO W-SKILJETECKEN                                        
229700     MOVE 1 TO IX-GAM                                                     
229800     MOVE ZERO TO W-KVAVROP-ACC                                           
229900     PERFORM UNTIL                                                        
230000     ( IX-GAM > MAX-ANT-GAMLA-AVROP )                                     
230100       IF W-KVAVROP-GAM (IX-GAM) > ZERO                                   
230200         ADD W-KVAVROP-GAM (IX-GAM) TO W-KVAVROP-ACC                      
230300         MOVE W-KVAVROP-GAM (IX-GAM) TO MOD-KVAVROP-GAM (IX-GAM)          
230400         IF W-TIAVROP-AVS-GAM (IX-GAM) > ZERO                             
230500            MOVE W-TIAVROP-AVS-GAM (IX-GAM)                               
230600                                 TO MOD-TIAVROP-AVS-GAM (IX-GAM)          
230700         ELSE                                                             
230800            MOVE SPACE           TO MOD-TIAVROP-AVS-GAM (IX-GAM)          
230900         END-IF                                                           
231000         IF W-KVAVROP-ACC > W-KVBR                                        
231100           MOVE ASTERISK TO W-SKILJETECKEN                                
231200         END-IF                                                           
231300         IF W-AVROP-GAM-AENDRAT (IX-GAM) = JA                             
231400           MOVE MFS-ADD-LYS-UPP-FAELT                                     
231500                                  TO MOD-AVROP-GAM-ATTR (IX-GAM)          
231600         ELSE                                                             
231700           IF W-AVROP-GAM-INL-PAST (IX-GAM) = JA                          
231800              MOVE MFS-ADD-LYS-UPP-FAELT                                  
231900                                  TO MOD-AVROP-GAM-ATTR (IX-GAM)          
232000           END-IF                                                         
232100         END-IF                                                           
232200         MOVE W-SKILJETECKEN TO MOD-SKILJETECKEN-GAM (IX-GAM)             
232300       ELSE                                                               
232400         MOVE MFS-RENSA-FAELT TO MOD-AVROP-GAM (IX-GAM)                   
232500       END-IF                                                             
232600       ADD 1 TO IX-GAM                                                    
232700     END-PERFORM                                                          
232800     .                                                                    
232900     EJECT                                                                
233000                                                                          
233100 S06-NYA-AVROP-I-TAB-TILL-MOD SECTION.                                    
233200* I BILDEN   ÅTSKILJS VÄRDE OCH VECKA MED  -  FÖR DE          *           
233300* AVROP SOM RYMMS INOM  KÖP + BESTÄLLNINGSREST.               *           
233400* FÖR ÖVRIGA AVROP ANVÄNDES *.                                *           
233500     MOVE W-LEVNR-KVBR       TO W-KVBR                                    
233600     ADD W-OMSPEC-KVBEST-PL  TO W-KVBR                                    
233700     MOVE 1 TO IY                                                         
233800     PERFORM UNTIL                                                        
233900     ( IY > MAX-ANT-PERIODER-I-TAB )                                      
234000       MOVE 1 TO IX                                                       
234100       PERFORM UNTIL                                                      
234200       ( IX > MAX-ANT-VECKOR-I-TAB )                                      
234300         IF W-KVAVROP-TAB (IY, IX) > ZERO                                 
234400                                                                          
234500           MOVE W-KVAVROP-TAB (IY, IX) TO MOD-KVAVROP-TAB (IY,            
234600           IX)                                                            
234700           MOVE W-TIAVROP-AVS-TAB (IY, IX)                                
234800                                  TO MOD-TIAVROP-AVS-TAB (IY, IX)         
234900           IF W-SKILJETECKEN = STRECK                                     
235000             ADD W-KVAVROP-TAB (IY, IX) TO W-KVAVROP-ACC                  
235100             IF  W-KVAVROP-ACC > W-KVBR                                   
235200               MOVE ASTERISK TO W-SKILJETECKEN                            
235300             END-IF                                                       
235400           END-IF                                                         
235500           MOVE W-SKILJETECKEN TO MOD-SKILJETECKEN-TAB (IY, IX)           
235600           IF W-AVROP-TAB-AENDRAT (IY, IX) = JA                           
235700             MOVE MFS-ADD-LYS-UPP-FAELT                                   
235800                                 TO MOD-AVROP-TAB-ATTR (IY, IX)           
235900           ELSE                                                           
236000             IF W-AVROP-TAB-INL-PAST (IY, IX) = JA                        
236100                MOVE MFS-ADD-LYS-UPP-FAELT                                
236200                                 TO MOD-AVROP-TAB-ATTR (IY, IX)           
236300             END-IF                                                       
236400           END-IF                                                         
236500         ELSE                                                             
236600           MOVE LOW-VALUE TO MOD-AVROP-TAB (IY, IX)                       
236700         END-IF                                                           
236800         MOVE MOD-PERIOD-AA (IY)    TO W-PERIOD-AA                        
236900         MOVE MOD-PERIOD-PP (IY)    TO W-PERIOD-PP                        
237000         MOVE +1                    TO IX-AAPP PIX                        
237100         PERFORM UNTIL IX-AAPP > 24                                       
237200            IF W-PERIOD-AAPP = PER-AAPP (IX-AAPP)                         
237300               MOVE IX-AAPP         TO PIX                                
237400               MOVE 25              TO IX-AAPP                            
237500            END-IF                                                        
237600            ADD +1                  TO IX-AAPP                            
237700         END-PERFORM                                                      
237800         IF PER-ANT-VV (PIX) < IX                                         
237900            MOVE SPACE    TO MOD-PARENTES-TAB (IY, IX)                    
238000         ELSE                                                             
238100            MOVE PARENTES TO MOD-PARENTES-TAB (IY, IX)                    
238200         END-IF                                                           
238300         ADD 1 TO IX                                                      
238400       END-PERFORM                                                        
238500       ADD 1 TO IY                                                        
238600     END-PERFORM                                                          
238700     .                                                                    
238800     EJECT                                                                
238900                                                                          
239000 S07-NOLLSTAELL-AVROPSTABELLER SECTION.                                   
239100     MOVE 1 TO IX                                                         
239200     PERFORM UNTIL                                                        
239300      ( IX > MAX-ANT-GAMLA-AVROP )                                        
239400       MOVE ZERO TO W-KVAVROP-GAM      (IX)                               
239500                 W-TIAVROP-AVS-GAM  (IX)                                  
239600       MOVE NEJ TO W-AVROP-GAM-AENDRAT (IX)                               
239700     MOVE NEJ TO W-AVROP-GAM-INL-PAST(IX)                                 
239800       ADD 1 TO IX                                                        
239900     END-PERFORM                                                          
240000     MOVE 1 TO IY                                                         
240100     PERFORM UNTIL                                                        
240200     ( IY > MAX-ANT-PERIODER-I-TAB )                                      
240300       MOVE 1 TO IX                                                       
240400       PERFORM UNTIL                                                      
240500        ( IX > MAX-ANT-VECKOR-I-TAB )                                     
240600         MOVE ZERO TO W-KVAVROP-TAB (IY, IX)                              
240700                      W-TIAVROP-AVS-TAB (IY, IX)                          
240800         MOVE NEJ TO W-AVROP-TAB-AENDRAT (IY, IX)                         
240900         MOVE NEJ TO W-AVROP-TAB-INL-PAST(IY, IX)                         
241000         ADD 1 TO IX                                                      
241100       END-PERFORM                                                        
241200       ADD 1 TO IY                                                        
241300     END-PERFORM                                                          
241400     .                                                                    
241500     EJECT                                                                
241600 S08-KOLLA-AVROP-INL-SW SECTION.                                          
241700                                                                          
241800     MOVE AVROP-TIAVRDAT-INL TO WS-TIAVRDAT-INL                           
241900                                                                          
242000     MOVE WS-TIAVRDAT-INL   TO DAYS-TIDATE1                               
242100     MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                             
242200     MOVE 'YYYYWWD'         TO DAYS-KDDATFMT2                             
242300     MOVE 0                 TO DAYS-KVDAYS                                
242400     MOVE SPACE             TO DAYS-TIDATE2                               
242500                               DAYS-IDCALEND                              
242600     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
242700                                                                          
242800     IF DAYS-KDRC = 8                                                     
242900       MOVE 'FEL VID ANROP TILL WZ20DAYS ' TO FELTEXT                     
243000                                                                          
243100       CALL FELLOG                                                        
243200     ELSE                                                                 
243300       MOVE DAYS-TIDATE2(1:7)  TO WS-TIAVRDAT-INL-AAAAVVD                 
243400     END-IF                                                               
243500     IF WS-TIAVRDAT-INL-AAAAVVD < W-DATUM-AKTUELLT-AAAAVVD                
243600       MOVE JA  TO AVROP-INL-PASSERAT-SW                                  
243700     END-IF                                                               
243800                                                                          
243900     .                                                                    
244000     EJECT                                                                
244100                                                                          
244200**   M F S -  SUBRUTINER           **                                     
244300     SKIP3                                                                
244400 MFS-OEPPNA-INIT-UPPDAT-FAELT SECTION.                                    
244500     MOVE 1 TO IX                                                         
244600     PERFORM UNTIL                                                        
244700     NOT ( IX < MAX-ANT-INDATA-FLT )                                      
244800       MOVE MFS-OEPPNA-NUM-FAELT TO MOD-ATTR (IX)                         
244900       MOVE MFS-RENSA-FAELT      TO MOD-FAELT (IX)                        
245000       IF IX = 13 OR 14                                                   
245100*** KDLEVPLF FLJIT EJ NUMERISKT                                           
245200          MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-ATTR (IX)                     
245300       END-IF                                                             
245400       ADD 1 TO IX                                                        
245500     END-PERFORM                                                          
245600     MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-TEARTNOT1-IN-ATTR                
245700                                     MOD-TEARTNOT2-IN-ATTR                
245800                                                                          
245900     IF MSGI-IDLAND-SPR = 'GB'                                            
246000        MOVE YYWW TO  MOD-TIAVROP-AVS-IN (1)                              
246100                      MOD-TIAVROP-AVS-IN (2)                              
246200                      MOD-TIAVROP-AVS-IN (3)                              
246300                      MOD-TIAVROP-AVS-IN (4)                              
246400                      MOD-TILPSP-IN                                       
246500     ELSE                                                                 
246600        MOVE AAVV TO  MOD-TIAVROP-AVS-IN (1)                              
246700                      MOD-TIAVROP-AVS-IN (2)                              
246800                      MOD-TIAVROP-AVS-IN (3)                              
246900                      MOD-TIAVROP-AVS-IN (4)                              
247000                      MOD-TILPSP-IN                                       
247100     END-IF                                                               
247200     .                                                                    
247300     EJECT                                                                
247400                                                                          
247500 MFS-KOEP-ENDA-UPPDAT-FAELT SECTION.                                      
247600     PERFORM MFS-STAENG-UPPDAT-FAELT                                      
247700     MOVE MFS-OEPPNA-NUM-FAELT TO MOD-KVBEST-PL-IN-ATTR                   
247800     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLJIT-IN-ATTR                      
247900                                   MOD-TEARTNOT1-IN-ATTR                  
248000                                   MOD-TEARTNOT2-IN-ATTR                  
248100     MOVE MFS-RENSA-FAELT      TO MOD-KVBEST-PL-IN                        
248200     .                                                                    
248300     SKIP3                                                                
248400                                                                          
248500 MFS-STAENG-UPPDAT-FAELT SECTION.                                         
248600     MOVE 1 TO IX                                                         
248700     PERFORM UNTIL                                                        
248800     NOT ( IX < MAX-ANT-INDATA-FLT )                                      
248900       MOVE MFS-FORMATETS-ATTR TO MOD-ATTR (IX)                           
249000       MOVE MFS-RENSA-FAELT    TO MOD-FAELT (IX)                          
249100       ADD 1 TO IX                                                        
249200     END-PERFORM                                                          
249300                                                                          
249400     MOVE MFS-FORMATETS-ATTR   TO MOD-TEARTNOT1-IN-ATTR                   
249500     MOVE MFS-RENSA-FAELT      TO MOD-TEARTNOT1-IN                        
249600     MOVE MFS-FORMATETS-ATTR   TO MOD-TEARTNOT2-IN-ATTR                   
249700     MOVE MFS-RENSA-FAELT      TO MOD-TEARTNOT2-IN                        
249800     .                                                                    
249900     SKIP3                                                                
250000                                                                          
250100 MFS-ROER-EJ-INDATA-FAELT SECTION.                                        
250200     MOVE 1 TO IX                                                         
250300     PERFORM UNTIL                                                        
250400     NOT ( IX < MAX-ANT-INDATA-FLT )                                      
250500       MOVE MFS-FORMATETS-ATTR TO MOD-ATTR (IX)                           
250600       MOVE MFS-ROER-EJ-FAELT  TO MOD-FAELT (IX)                          
250700       ADD 1 TO IX                                                        
250800     END-PERFORM                                                          
250900     .                                                                    
251000     EJECT                                                                
251100                                                                          
251200 MFS-ROER-EJ-UTDATA-FAELT SECTION.                                        
251300     MOVE MFS-FORMATETS-ATTR TO MOD-TEXT-PLANTYP-ATTR                     
251400                                MOD-TEXT-ORSAK1-ATTR                      
251500                                MOD-TEXT-ORSAK2-ATTR                      
251600                                MOD-KDLPSP-ATTR                           
251700                                MOD-TILPSP-ATTR                           
251800                                MOD-DATA-GRP2-ATTR                        
251900                                MOD-TEXT-UPPLYSNING-ATTR (1)              
252000                                MOD-TEXT-UPPLYSNING-ATTR (2)              
252100                                MOD-TEXT-UPPLYSNING-ATTR (3)              
252200                                MOD-TEXT-UPPLYSNING-ATTR (4)              
252300                                MOD-TEARTNOT1-IN-ATTR                     
252400                                MOD-TEARTNOT2-IN-ATTR                     
252500                                                                          
252600     MOVE MFS-ROER-EJ-FAELT TO MOD-TEXT-PLANTYP                           
252700                               MOD-TIOMSPEC                               
252800                               MOD-IDARTNR-IN                             
252900                               MOD-KDBEHX-PLAN-IN                         
253000                               MOD-IDLEVNR-IN                             
253100                               MOD-IDARTNR                                
253200                               MOD-TEXT-ORSAK1                            
253300                               MOD-TEXT-ORSAK2                            
253400                               MOD-DATA-GRP1                              
253500                               MOD-IDLEVNR-SHIP-UT                        
253600                               MOD-TIFINLV                                
253700                               MOD-TIURPROD                               
253800                               MOD-KDLPSP                                 
253900                               MOD-TILPSP                                 
254000                               MOD-FLSEASON                               
254100                               MOD-FLTREND                                
254200                               MOD-DATA-GRP2                              
254300                               MOD-DATA-GRP4                              
254400                               MOD-TEXT-UPPLYSNING (1)                    
254500                               MOD-TEXT-UPPLYSNING (2)                    
254600                               MOD-TEXT-UPPLYSNING (3)                    
254700                               MOD-TEXT-UPPLYSNING (4)                    
254800                               MOD-BEART-SVE                              
254900                               MOD-KDOTFREK                               
255000                               MOD-KVPB-SATS                              
255100                               MOD-KDERS                                  
255200                               MOD-REPLACES                               
255300                               MOD-REPL-BY                                
255400                               MOD-TEARTNOT1-IN                           
255500                               MOD-TEARTNOT2-IN                           
255600     EJECT                                                                
255700     MOVE 1 TO IY                                                         
255800     PERFORM UNTIL                                                        
255900     ( IY > MAX-ANT-PERIODER-I-TAB )                                      
256000       MOVE 1 TO IX                                                       
256100       MOVE MFS-ROER-EJ-FAELT    TO MOD-PERIOD (IY)                       
256200       PERFORM UNTIL                                                      
256300       ( IX > MAX-ANT-VECKOR-I-TAB )                                      
256400         MOVE MFS-FORMATETS-ATTR TO MOD-AVROP-TAB-ATTR (IY, IX)           
256500         MOVE MFS-ROER-EJ-FAELT  TO MOD-AVROP-TAB (IY, IX)                
256600         ADD 1 TO IX                                                      
256700       END-PERFORM                                                        
256800       ADD 1 TO IY                                                        
256900     END-PERFORM                                                          
257000     MOVE 1 TO IX                                                         
257100     PERFORM UNTIL                                                        
257200      ( IX > MAX-ANT-GAMLA-AVROP )                                        
257300       MOVE MFS-FORMATETS-ATTR   TO MOD-AVROP-GAM-ATTR (IX)               
257400       MOVE MFS-ROER-EJ-FAELT    TO MOD-AVROP-GAM (IX)                    
257500       ADD 1 TO IX                                                        
257600     END-PERFORM                                                          
257700     .                                                                    
257800     EJECT                                                                
257900                                                                          
258000***   I M S -  SUBRUTINER           **                                    
258100     SKIP3                                                                
258200 IMS-GET-MSG SECTION.                                                     
258300     MOVE '  QC' TO GODK-STATUSKODER                                      
258400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
258500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
258600     PERFORM IMS-STATUSKONTROLL                                           
258700     .                                                                    
258800     SKIP3                                                                
258900                                                                          
259000 IMS-INSERT-MSG SECTION.                                                  
259100     IF MSGI-IDLAND-SPR = 'GB'                                            
259200        MOVE 'N' TO MFS-KDHUVOMR                                          
259300     END-IF                                                               
259400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
259500     MOVE SPACE TO GODK-STATUSKODER                                       
259600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
259700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
259800     PERFORM IMS-STATUSKONTROLL                                           
259900     .                                                                    
260000     EJECT                                                                
260100                                                                          
260200 IMS-GET-ART-CLAG-SEG   SECTION.                                          
260300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
260400            DELIMITED BY SIZE INTO SSA1                                   
260500     MOVE 'WLARTC11 ' TO SSA2                                             
260600     MOVE '  GE' TO GODK-STATUSKODER                                      
260700     CALL CBLTDLI USING GU     ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2          
260800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
260900     PERFORM IMS-STATUSKONTROLL                                           
261000     .                                                                    
261100     SKIP3                                                                
261200                                                                          
261300 IMS-GET-ARTIKEL-ROT    SECTION.                                          
261400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
261500            DELIMITED BY SIZE INTO SSA1                                   
261600     MOVE '  GE' TO GODK-STATUSKODER                                      
261700     CALL CBLTDLI USING GU     ARTC-PCB DLI-IO-AREA-01 SSA1               
261800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
261900     PERFORM IMS-STATUSKONTROLL                                           
262000     .                                                                    
262100     SKIP3                                                                
262200                                                                          
262300 IMS-GET-CLAG-SEG SECTION.                                                
262400     MOVE 'WLARTC11 ' TO SSA1                                             
262500     MOVE '  GE' TO GODK-STATUSKODER                                      
262600     CALL CBLTDLI USING GNP    ARTC-PCB DLI-IO-AREA-11 SSA1               
262700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
262800     PERFORM IMS-STATUSKONTROLL                                           
262900     .                                                                    
263000     EJECT                                                                
263100                                                                          
263200 IMS-GNP-ARTC23 SECTION.                                                  
263300     MOVE 'WLARTC11 ' TO SSA1                                             
263400     MOVE 'WLARTC23 ' TO SSA2                                             
263500     MOVE '  GE' TO GODK-STATUSKODER                                      
263600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
263700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
263800     PERFORM IMS-STATUSKONTROLL                                           
263900     .                                                                    
264000 IMS-GET-ARTIKEL-ROT-OLD    SECTION.                                      
264100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
264200            DELIMITED BY SIZE INTO SSA1                                   
264300     MOVE '  GE' TO GODK-STATUSKODER                                      
264400     CALL CBLTDLI USING GU     ARTC-PCB DLI-IO-WDK601-OLD SSA1            
264500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
264600     PERFORM IMS-STATUSKONTROLL                                           
264700     .                                                                    
264800     SKIP3                                                                
264900                                                                          
265000 IMS-GET-CLAG-SEG-OLD SECTION.                                            
265100     MOVE 'WLARTC11 ' TO SSA1                                             
265200     MOVE '  GE' TO GODK-STATUSKODER                                      
265300     CALL CBLTDLI USING GNP    ARTC-PCB DLI-IO-WDK611-OLD SSA1            
265400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
265500     PERFORM IMS-STATUSKONTROLL                                           
265600     .                                                                    
265700     EJECT                                                                
265800                                                                          
265900 IMS-GET-ARTC25 SECTION.                                                  
266000     SKIP2                                                                
266100     MOVE  'WLARTC11*F(KDSEGKEY =1)'  TO SSA1                             
266200     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
266300            DELIMITED BY SIZE INTO SSA2                                   
266400     MOVE '  GE' TO GODK-STATUSKODER                                      
266500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
266600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
266700     PERFORM IMS-STATUSKONTROLL                                           
266800     .                                                                    
266900                                                                          
267000 IMS-GET-WDK626 SECTION.                                                  
267100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
267200          DELIMITED BY SIZE INTO SSA1                                     
267300     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
267400     MOVE 'WLARTC26 ' TO SSA3                                             
267500     MOVE '  GE' TO GODK-STATUSKODER                                      
267600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2 SSA3          
267700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
267800     PERFORM IMS-STATUSKONTROLL                                           
267900     .                                                                    
268000                                                                          
268100 IMS-GET-LEVART-SEG SECTION.                                              
268200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
268300            DELIMITED BY SIZE INTO SSA1                                   
268400     MOVE '  GE' TO GODK-STATUSKODER                                      
268500     CALL CBLTDLI USING GU     INLB-PCB DLI-IO-AREA SSA1                  
268600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
268700     PERFORM IMS-STATUSKONTROLL                                           
268800     .                                                                    
268900     SKIP3                                                                
269000                                                                          
269100 IMS-GET-LEVERANTOER-SEG SECTION.                                         
269200     MOVE 'WLINLB11 ' TO SSA1                                             
269300     MOVE '  GE' TO GODK-STATUSKODER                                      
269400     CALL CBLTDLI USING GNP    INLB-PCB DLI-IO-AREA SSA1                  
269500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
269600     PERFORM IMS-STATUSKONTROLL                                           
269700     .                                                                    
269800     EJECT                                                                
269900                                                                          
270000 IMS-GET-OMSPEC-SEG  SECTION.                                             
270100     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
270200            DELIMITED BY SIZE INTO SSA1                                   
270300     MOVE 'WLINLB22 ' TO SSA2                                             
270400     MOVE '  GE' TO GODK-STATUSKODER                                      
270500     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
270600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
270700     PERFORM IMS-STATUSKONTROLL                                           
270800     .                                                                    
270900     EJECT                                                                
271000                                                                          
271100 IMS-GET-AVROP-OKVAL-NEXT     SECTION.                                    
271200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
271300            DELIMITED BY SIZE INTO SSA1                                   
271400     MOVE 'WLINLB23 ' TO SSA2                                             
271500     MOVE '  GE' TO GODK-STATUSKODER                                      
271600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
271700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
271800     PERFORM IMS-STATUSKONTROLL                                           
271900     .                                                                    
272000     EJECT                                                                
272100                                                                          
272200 IMS-GET-BENA11-BSEQ SECTION.                                             
272300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
272400            DELIMITED BY SIZE INTO SSA1                                   
272500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
272600            DELIMITED BY SIZE INTO SSA2                                   
272700     MOVE '  ' TO GODK-STATUSKODER                                        
272800     CALL CBLTDLI USING GU    BENA-PCB DLI-IO-AREA SSA1 SSA2              
272900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
273000     PERFORM IMS-STATUSKONTROLL                                           
273100     .                                                                    
273200     EJECT                                                                
273300                                                                          
273400 IMS-GU-WDA901 SECTION.                                                   
273500     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
273600          DELIMITED BY SIZE INTO SSA1                                     
273700     MOVE '  GE'           TO GODK-STATUSKODER                            
273800     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
273900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
274000     PERFORM IMS-STATUSKONTROLL                                           
274100     .                                                                    
274200                                                                          
274300 IMS-GU-WDA911 SECTION.                                                   
274400     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
274500          DELIMITED BY SIZE INTO SSA1                                     
274600     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-WDA9-X ')'                    
274700          DELIMITED BY SIZE INTO SSA2                                     
274800     MOVE '  GE'           TO GODK-STATUSKODER                            
274900     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA911 SSA1 SSA2               
275000     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
275100     PERFORM IMS-STATUSKONTROLL                                           
275200     .                                                                    
275300     EJECT                                                                
275400                                                                          
275500 IMS-GU-WDB201-BSEQ SECTION.                                              
275600     STRING 'WDB201  (WDB2BSEQ =' W-IDLEVNR-X ')'                         
275700          DELIMITED BY SIZE INTO SSA1                                     
275800     MOVE '  GE'            TO GODK-STATUSKODER                           
275900     CALL CBLTDLI USING GU WDB2B-PCB DLI-IO-WDB201 SSA1                   
276000     MOVE WDB2B-STATUS-CODE TO STATUS-WS                                  
276100     PERFORM IMS-STATUSKONTROLL                                           
276200     .                                                                    
276300     EJECT                                                                
276400                                                                          
276500 IMS-GU-WDD7A1-MINMAX SECTION.                                            
276600     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
276700                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
276800            DELIMITED BY SIZE INTO SSA1                                   
276900     MOVE '  GE' TO GODK-STATUSKODER                                      
277000     CALL CBLTDLI USING GU WDD7A1-PCB DLI-IO-WDD7A1 SSA1                  
277100     MOVE WDD7A1-STATUS-CODE TO STATUS-WS                                 
277200     PERFORM IMS-STATUSKONTROLL                                           
277300     .                                                                    
277400     EJECT                                                                
277500                                                                          
277600 IMS-GN-WDD7A1-MINMAX SECTION.                                            
277700     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
277800                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
277900            DELIMITED BY SIZE INTO SSA1                                   
278000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
278100     CALL CBLTDLI USING GN WDD7A1-PCB DLI-IO-WDD7A1 SSA1                  
278200     MOVE WDD7A1-STATUS-CODE TO STATUS-WS                                 
278300     PERFORM IMS-STATUSKONTROLL                                           
278400     .                                                                    
278500     EJECT                                                                
278600                                                                          
278700 IMS-GU-WDD7-ERSA01 SECTION.                                              
278800     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
278900            DELIMITED BY SIZE INTO SSA1                                   
279000     MOVE '  GE' TO GODK-STATUSKODER                                      
279100     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA-ERSA01 SSA1               
279200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
279300     PERFORM IMS-STATUSKONTROLL                                           
279400     .                                                                    
279500     SKIP2                                                                
279600                                                                          
279700 IMS-GNP-WDD7-ERSA11 SECTION.                                             
279800     STRING 'WLERSA11(FLTEXT   =N)'                                       
279900            DELIMITED BY SIZE INTO SSA1                                   
280000     MOVE '  GE' TO GODK-STATUSKODER                                      
280100     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA-ERSA11 SSA1              
280200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
280300     PERFORM IMS-STATUSKONTROLL                                           
280400     .                                                                    
280500     SKIP2                                                                
280600 IMS-GU-WDD601        SECTION.                                            
280700     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
280800                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X                        
280900                 OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
281000                 OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
281100     DELIMITED BY SIZE INTO SSA1                                          
281200     MOVE '  GE' TO GODK-STATUSKODER                                      
281300     CALL CBLTDLI USING GU WDD6-PCB DLI-IO-WDD601 SSA1                    
281400     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
281500     PERFORM IMS-STATUSKONTROLL                                           
281600     .                                                                    
281700     EJECT                                                                
281800                                                                          
281900 IMS-GHU-WDD601        SECTION.                                           
282000     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
282100                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X                        
282200                 OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
282300                 OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
282400     DELIMITED BY SIZE INTO SSA1                                          
282500     MOVE '  GE' TO GODK-STATUSKODER                                      
282600     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
282700     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
282800     PERFORM IMS-STATUSKONTROLL                                           
282900     .                                                                    
283000     EJECT                                                                
283100                                                                          
283200 IMS-GN-WDD601        SECTION.                                            
283300     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
283400                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X                        
283500                 OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
283600                 OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
283700     DELIMITED BY SIZE INTO SSA1                                          
283800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
283900     CALL CBLTDLI USING GN WDD6-PCB DLI-IO-WDD601 SSA1                    
284000     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
284100     PERFORM IMS-STATUSKONTROLL                                           
284200     .                                                                    
284300     EJECT                                                                
284400                                                                          
284500 IMS-GHN-WDD601        SECTION.                                           
284600     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
284700                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X                        
284800                 OCH 'IDANSK  >=' W-IDANSK-MIN-X                          
284900                 OCH 'IDANSK  <=' W-IDANSK-MAX-X ')'                      
285000     DELIMITED BY SIZE INTO SSA1                                          
285100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
285200     CALL CBLTDLI USING GHN WDD6-PCB DLI-IO-WDD601 SSA1                   
285300     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
285400     PERFORM IMS-STATUSKONTROLL                                           
285500     .                                                                    
285600     EJECT                                                                
285700                                                                          
285800 IMS-REPL-WDD601        SECTION.                                          
285900                                                                          
286000     MOVE '  ' TO GODK-STATUSKODER                                        
286100     CALL CBLTDLI USING REPL WDD6-PCB DLI-IO-WDD601                       
286200     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
286300     PERFORM IMS-STATUSKONTROLL                                           
286400     .                                                                    
286500     EJECT                                                                
286600                                                                          
286700 IMS-DELETE-WDD601        SECTION.                                        
286800                                                                          
286900     MOVE '  ' TO GODK-STATUSKODER                                        
287000     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
287100     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
287200     PERFORM IMS-STATUSKONTROLL                                           
287300     .                                                                    
287400     EJECT                                                                
287500                                                                          
287600 IMS-GET-LEVA01-WDF101 SECTION.                                           
287700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
287800          DELIMITED BY SIZE INTO SSA1                                     
287900     MOVE '  GE' TO GODK-STATUSKODER                                      
288000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
288100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
288200     PERFORM IMS-STATUSKONTROLL                                           
288300     .                                                                    
288400     SKIP3                                                                
288500                                                                          
288600 IMS-INSERT-ALT SECTION.                                                  
288700     MOVE SPACE TO GODK-STATUSKODER                                       
288800     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
288900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
289000     PERFORM IMS-STATUSKONTROLL                                           
289100     .                                                                    
289200     SKIP3                                                                
289300                                                                          
289400 IMS-STATUSKONTROLL SECTION.                                              
289500     SET STATUS-IX TO 1                                                   
289600     SEARCH GODK-STATUS AT END                                            
289700     CALL FELLOG                                                          
289800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
289900     CONTINUE                                                             
290000     END-SEARCH                                                           
290100     .                                                                    
290200     EJECT                                                                
290300                                                                          
290400 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
290500     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
290600     MOVE 000100  TO GOOD-SQLCODECODES                                    
290700     EXEC SQL                                                             
290800         DECLARE TP1ARTK-CRS CURSOR FOR                                   
290900           SELECT  A.IDKAMP                                               
291000                  ,A.IDARTNR                                              
291100                  ,B.TISTADAT_KAMP                                        
291200                  ,B.TISTODAT_KAMP                                        
291300                  ,B.KDKAMP                                               
291400                                                                          
291500           FROM    TP1ARTK A                                              
291600                  ,TP1KAMP B                                              
291700                                                                          
291800           WHERE   A.IDARTNR = :W-IDARTNR                                 
291900           AND     A.IDKAMP  =  B.IDKAMP                                  
292000                                                                          
292100           ORDER BY A.IDARTNR                                             
292200     END-EXEC                                                             
292300                                                                          
292400     MOVE 000100  TO GOOD-SQLCODECODES                                    
292500     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
292600     .                                                                    
292700     SKIP3                                                                
292800                                                                          
292900 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
293000     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
293100     MOVE 000100  TO GOOD-SQLCODECODES                                    
293200     EXEC SQL                                                             
293300         FETCH TP1ARTK-CRS INTO                                           
293400                    :TP1KAMP-IDKAMP                                       
293500                   ,:TP1ARTK-IDARTNR                                      
293600                   ,:TP1KAMP-TISTADAT-KAMP                                
293700                   ,:TP1KAMP-TISTODAT-KAMP                                
293800                   ,:TP1KAMP-KDKAMP                                       
293900     END-EXEC                                                             
294000                                                                          
294100     MOVE SQLCODE TO SQLCODE-WS                                           
294200     PERFORM DB2-STATUS-CHECK                                             
294300     .                                                                    
294400     SKIP3                                                                
294500                                                                          
294600 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
294700     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
294800     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
294900     .                                                                    
295000     EJECT                                                                
295100                                                                          
295200 DB2-STATUS-CHECK  SECTION.                                               
295300     SET SQLCODE-IX TO 1                                                  
295400     SEARCH GOOD-SQLCODE                                                  
295500       AT END                                                             
295600          CALL FELLOG                                                     
295700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
295800     END-SEARCH                                                           
295900     .                                                                    
296000     EJECT                                                                
296100*    -COPY WY2000P3                                                       
296200     EJECT                                                                
296300*    -COPY WY2000P9                                                       

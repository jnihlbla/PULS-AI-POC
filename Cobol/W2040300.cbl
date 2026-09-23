000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2040300.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/10/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING LEVERANSPLANER                                       
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK6                                       
001100*                   LÄSER      WDK7                                       
001200*                   LÄSER      WDF1                                       
001300*                   LÄSER      WDF3                                       
001400*                   LÄSER      WDD3                                       
001500*                   LÄSER      WDD6                                       
001600*                   LÄSER      WDD7                                       
001700*                   LÄSER      WDD7A1                                     
001800*                   LÄSER      WDK7-ERS  (läser ersatt art.)              
001900*                   LÄSER      WDD9                                       
002000*                   LÄSER      WDR2                                       
002100*                   UPPDATERAR WDG3                                       
002200*                   LÄSER      WDB6                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W2T403                                              
002600*        MID:         W2I40301                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W2O40301                                            
003000******************************************************************        
003100* PROGRAMÄNDRINGAR:                                                       
003200*    2013-11-20   E-TRACKER: 10205391  VECKO/PERIOD BATCH VIA             
003300*                                      BILD 2117.LOGGA WDR501/            
003400*                                      H-TYP 2247+J PERIOD/VECKA.         
003500*                                                                         
003600*    2015-04-22  ETRACKER 10130993                                        
003700*                REDUCE NUMBER OF DELIVERY SCHEDULES                      
003800*                                                                         
003900*    2015-07-06  ETRACKER 10209749    (WDD903)                            
004000*                ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.          
004100*                                                                         
004200*    2015-11-12  E'TRACKER 10243132  KINA EXPORT 2015                     
004300*                                                                         
004400                                                                          
004500 ENVIRONMENT DIVISION.                                                    
004600 DATA DIVISION.                                                           
004700 WORKING-STORAGE SECTION.                                                 
004800*    -COPY WY2000W3                                                       
004900                                                                          
005000 77  IDPGM                       PIC X(08)   VALUE 'W2040300'.            
005100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005200 77  CURRENT-S-SECTION           PIC X(16)   VALUE SPACE.                 
005300 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005400 77  SW-WDK6-FINNS               PIC X       VALUE 'N'.                   
005500 77  SW-WDK611-FINNS             PIC X       VALUE 'N'.                   
005600 77  SW-WDK629-FINNS             PIC X       VALUE 'N'.                   
005700 77  AVROP-INL-PASSERAT-SW       PIC X       VALUE 'N'.                   
005800                                                                          
005900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006000 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
006100 01  FELTEXT                     PIC X(80)   VALUE SPACE.                 
006200 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
006300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
006400                                                                          
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  YES                         PIC X       VALUE 'Y'.                   
006800 77  NOO                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  PROPOSAL-X                  PIC X       VALUE 'P'.                   
007100 77  PROPOSAL                    PIC 9       VALUE 1.                     
007200 77  VALID-X                     PIC X       VALUE 'V'.                   
007300 77  VALID                       PIC 9       VALUE 2.                     
007400 77  ORSAK-BEGAERD               PIC 9(2)    VALUE 17.                    
007500 77  ORSAK-BEG-OPT               PIC 9(2)    VALUE 18.                    
007600                                                                          
007700 77  W-TALLY                     PIC 9(2)   VALUE  ZERO.                  
007800 77  W-LEN                       PIC 9(2)   VALUE  ZERO.                  
007900 77  W-KVAVROP-UP                PIC X(7)   VALUE  SPACES.                
008000 77  W-KVAVROP-UP-N              PIC 9(7)   VALUE  ZERO.                  
008100                                                                          
008200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008300     88  NYCKLAR-OK                          VALUE 'J'.                   
008400     88  NYCKLAR-FEL                         VALUE 'N'.                   
008500                                                                          
008600 77  NYA-NYCKLAR-SW              PIC X       VALUE 'J'.                   
008700     88  NYA-NYCKLAR                         VALUE 'J'.                   
008800     88  GAMLA-NYCKLAR                       VALUE 'N'.                   
008900                                                                          
009000 77  INPUT-SW                    PIC X       VALUE 'J'.                   
009100     88  INPUT-FINNS                         VALUE 'J'.                   
009200     88  INPUT-SAKNAS                        VALUE 'N'.                   
009300                                                                          
009400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009500     88  INDATA-OK                           VALUE 'J'.                   
009600     88  INDATA-FEL                          VALUE 'N'.                   
009700                                                                          
009800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009900     88  ALLT-OK                             VALUE 'J'.                   
010000     88  ALLT-EJ-OK                          VALUE 'N'.                   
010100                                                                          
010200 01  ERSATT-SW                   PIC X       VALUE 'N'.                   
010300     88  ARTIKEL-ERSATT                      VALUE 'J'.                   
010400     88  ARTIKEL-EJ-ERSATT                   VALUE 'N'.                   
010500                                                                          
010600 01  OMSPEC-SW                   PIC X       VALUE 'N'.                   
010700     88  OMSPEC-BEGARD                       VALUE 'J'.                   
010800     88  OMSPEC-EJ-BEGARD                    VALUE 'N'.                   
010900                                                                          
011000 01  TIAVROP-SW                  PIC X       VALUE 'N'.                   
011100     88  TIAVROP-OK                          VALUE 'J'.                   
011200     88  TIAVROP-FEL                         VALUE 'N'.                   
011300                                                                          
011400 01  KVAVROP-SW                  PIC X       VALUE 'N'.                   
011500     88  KVAVROP-OK                          VALUE 'J'.                   
011600     88  KVAVROP-FEL                         VALUE 'N'.                   
011700                                                                          
011800 01  DATUM-SW                    PIC X       VALUE 'N'.                   
011900     88  DATUM-OK                            VALUE 'J'.                   
012000     88  DATUM-FEL                           VALUE 'N'.                   
012100                                                                          
012200 01  GAMMAL-TAB-SW               PIC X       VALUE 'N'.                   
012300     88  GAMMAL-TAB-FINNS                    VALUE 'J'.                   
012400     88  GAMMAL-TAB-SAKNAS                   VALUE 'N'.                   
012500                                                                          
012600 01  NEXT-WDD601-SW              PIC X       VALUE 'N'.                   
012700     88  NEXT-WDD601-FOUND                   VALUE 'J'.                   
012800     88  NEXT-WDD601-NOT-FOUND               VALUE 'N'.                   
012900                                                                          
013000 01  SWAP-SW                     PIC X       VALUE 'N'.                   
013100     88  SWAP                                VALUE 'J'.                   
013200     88  EJ-SWAP                             VALUE 'N'.                   
013300                                                                          
013400                                                                          
013500 01  IX-PLUS-1                   PIC 9       VALUE ZERO.                  
013600                                                                          
013700 01  AVROP-IN-IX                 PIC 9       VALUE ZERO.                  
013800 01  AVROP-IN-IX-MAX             PIC 9       VALUE 4.                     
013900                                                                          
014000 01  RAD-IX                      PIC 9(2)    VALUE ZERO.                  
014100 01  RAD-IX-MAX                  PIC 9(2)    VALUE 10.                    
014200                                                                          
014300 01  AVROP-G-IX                  PIC 9     VALUE ZERO.                    
014400 01  AVROP-G-IX-MAX              PIC 9     VALUE 5.                       
014500                                                                          
014600 01  PERIOD-IX                   PIC 9(2)  VALUE ZERO.                    
014700 01  PERIOD-IX-MAX               PIC 9(2)  VALUE 10.                      
014800                                                                          
014900 01  DAG-IX                      PIC 9     VALUE ZERO.                    
015000 01  DAG-IX-MAX                  PIC 9     VALUE 5.                       
015100                                                                          
015200 01  VECKA-IX                    PIC 9     VALUE ZERO.                    
015300 01  VECKA-IX-MAX                PIC 9     VALUE 5.                       
015400                                                                          
015500 01  AVROP-CHG-IX                PIC 9     VALUE ZERO.                    
015600 01  AVROP-CHG-IX-MAX            PIC 9     VALUE 4.                       
015700                                                                          
015800 01  2447-IX                     PIC 9(2)  VALUE ZERO.                    
015900 01  2447-IX-MAX                 PIC 9(2)  VALUE 15.                      
016000                                                                          
016100                                                                          
016200 01  FILLER                  PIC X(13) VALUE 'WS-IDARTNR-9='.             
016300 01  WS-IDARTNR-9-RIGHT      PIC X(9)  VALUE SPACE JUST RIGHT.            
016400 01  FILLER REDEFINES WS-IDARTNR-9-RIGHT.                                 
016500     03 WS-IDARTNR-9-NUM     PIC 9(9).                                    
016600                                                                          
016700 01  W-DAGENS-DATUM              PIC 9(6)  VALUE ZERO.                    
016800 01  W-DAGENS-DATUM-AAAAMMDD     PIC 9(8)  VALUE ZERO.                    
016900                                                                          
017000 01  W-DATUM-AKTUELLT            PIC S9(5)   COMP-3.                      
017100 01  W-DATUM-AKTUELLT-AAAAVV     PIC  9(6).                               
017200                                                                          
017300 01  W-DATUM-AKTUELLT-AAAAVVD    PIC 9(7) VALUE ZERO.                     
017400 01  WS-TIAVRDAT-INL-AAAAVVD     PIC 9(7) VALUE ZERO.                     
017500                                                                          
017600 01  W-DATUM-AAVV                PIC 9(4).                                
017700 01  FILLER REDEFINES W-DATUM-AAVV.                                       
017800     03  W-DATUM-AA              PIC 9(2).                                
017900     03  W-DATUM-VV              PIC 9(2).                                
018000                                                                          
018100 01  W-AAVV-MINUS-HALVAR         PIC S9(5)   COMP-3.                      
018200 01  W-DATUM-AKT-5VV             PIC 9(5).                                
018300                                                                          
018400 01  W-DAAVROP-AVS               PIC 9(6).                                
018500 01  FILLER  REDEFINES W-DAAVROP-AVS.                                     
018600     03  FILLER                  PIC 9(2).                                
018700     03  W-TIAVROP-AVS           PIC 9(4).                                
018800                                                                          
018900 01  W-START-PER-AAPP            PIC S9(5)   COMP-3.                      
019000 01  W-START-PER-AA              PIC S9(3)   COMP-3.                      
019100 01  W-START-PER-PP              PIC S9(3)   COMP-3.                      
019200                                                                          
019300 01  WS-AAPP                     PIC 9(4)    VALUE ZERO.                  
019400 01  FILLER REDEFINES WS-AAPP.                                            
019500     03  WS-AA                   PIC 9(2).                                
019600     03  WS-PP                   PIC 9(2).                                
019700                                                                          
019800 01  W-DAAVROP-CHG-AAAAVV        PIC 9(6).                                
019900 01  FILLER REDEFINES W-DAAVROP-CHG-AAAAVV.                               
020000     03 W-TIAVROP-CHG-SS         PIC 9(2).                                
020100     03 W-TIAVROP-CHG            PIC 9(4).                                
020200                                                                          
020300 01  W-KVAVROP-CHG               PIC 9(7).                                
020400                                                                          
020500 01  W-TIAAMMDD-AVS              PIC 9(6).                                
020600                                                                          
020700 01  WS-TIAVRDAT-INL             PIC 9(6)    VALUE ZERO.                  
020800 01  WS-TIAVRDAT-INL-AAAAVV      PIC 9(6)    VALUE ZERO.                  
020900 01  FILLER REDEFINES WS-TIAVRDAT-INL-AAAAVV.                             
021000     03 WS-TIAVRDAT-INL-SS        PIC 9(2).                               
021100     03 WS-TIAVRDAT-INL-AAVV      PIC 9(4).                               
021200                                                                          
021300 01  W-D902-TILEVPL              PIC 9(6).                                
021400 01  W-XLAG-TIOMSPEC             PIC 9(5).                                
021500                                                                          
021600 01  W-AAVV-CHECK                PIC 9(4).                                
021700 01  W-AAVV-JUST                 PIC 9(4).                                
021800 01  FILLER REDEFINES W-AAVV-JUST.                                        
021900     03  W-AAVV-JUST-AA          PIC 9(2).                                
022000     03  W-AAVV-JUST-VV          PIC 9(2).                                
022100 01  W-KVVECKOR-CHG              PIC S9(3).                               
022200 01  W-KVVECKOR-DISP             PIC  9(3).                               
022300                                                                          
022400 01  W-IDANSK                    PIC 9(3).                                
022500 01  W-KDLPORS-1                 PIC 9(2).                                
022600 01  W-KDLPORS-2                 PIC 9(2).                                
022700 01  W-KDLPORS-3                 PIC 9(2).                                
022800                                                                          
022900 01  W-DATUM-AAVV-X              PIC X(4).                                
023000 01  FILLER REDEFINES W-DATUM-AAVV-X.                                     
023100     03  W-DATUM-AAVV-N          PIC 9(4).                                
023200                                                                          
023300 01  W-KVAVROP-X                 PIC X(7).                                
023400 01  FILLER REDEFINES W-KVAVROP-X.                                        
023500     03 W-KVAVROP                PIC 9(7).                                
023600                                                                          
023700 01  W-IDLEVNR-COUNT             PIC 9(5).                                
023800                                                                          
023900 01  WS-TILPSP-NUM               PIC 9(5).                                
024000 01  WS-TILPSP-X REDEFINES WS-TILPSP-NUM.                                 
024100     03 FILLLER                  PIC X(1).                                
024200     03 WS-TILPSP                PIC X(4).                                
024300                                                                          
024400 01  W-DAGENS-DAT-PLUS-LT        PIC 9(5)    VALUE ZERO.                  
024500 01  W-ANTAL-VECKOR              PIC 9(4)    VALUE ZERO.                  
024600                                                                          
024700 01  W-ARSOMS                PIC S9(9)V9(2) VALUE ZERO   COMP-3.          
024800 01  W-ARSOMS-100000         PIC S9(9)V9(2) VALUE 100000 COMP-3.          
024900                                                                          
025000 01  W-KVPB-REF-SUM              PIC S9(7)V9 VALUE ZERO COMP-3.           
025100                                                                          
025200 01  WS-MEDD-ERS.                                                         
025300     03  WS-MEDD-ERSKOD          PIC 9(2)    VALUE ZERO.                  
025400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
025500     03  WS-MEDD-TEXT            PIC X(23)   VALUE SPACE.                 
025600                                                                          
025700 01 LPF-WDD6-KEY.                                                         
025800     05 LPF-IDDC-D6             PIC X(2).                                 
025900     05 LPF-IDLEVNR-D6          PIC X(5).                                 
026000     05 LPF-IDARTNR-D6          PIC S9(9)   COMP-3.                       
026100     05 LPF-IDANSK-D6           PIC S9(3)   COMP-3.                       
026200                                                                          
026300 01  FEL-OCH-KOMMENTARER.                                                 
026400     03  ERR-EJ-LOKAL       PIC X(26) VALUE                               
026500                            'NOT LOCAL SOURCED         '.                 
026600     03  ERR-LEV-SAKNAS     PIC X(26) VALUE                               
026700                            'SUPPLIER MISSING          '.                 
026800     03  KOM-NY-PB          PIC X(26) VALUE                               
026900                            'NEW  PB YYWWD             '.                 
027000     03  KOM-PROGNOSVARNING PIC X(26) VALUE                               
027100                            'PROGNOSIS WARNING         '.                 
027200     03  KOM-PASSIV-ARTIKEL PIC X(26) VALUE                               
027300                            'PART PASSIVE              '.                 
027400     03  KOM-FORSLAG-SAKNAS PIC X(26) VALUE                               
027500                            'PROPOSAL MISSING          '.                 
027600     03  KOM-SUPP-HOLIDAY   PIC X(26) VALUE                               
027700                            'CALL ON SUPPLIER HOLIDAY '.                  
027800     03  KOM-UTR-SALDO      PIC X(26) VALUE                               
027900                            'INVESTIGATION BALANCE     '.                 
028000     03  KOM-INTE-HUVUDLEV  PIC X(26) VALUE                               
028100                            'NOT MAIN SUPPLIER         '.                 
028200     03  KOM-FLER-LEV       PIC X(26) VALUE                               
028300                            'ANOTHER SUPPLIER EXIST    '.                 
028400     03  KOM-TREND          PIC X(26) VALUE                               
028500                            'TREND                     '.                 
028600     03  KOM-RE-SPEC-DONE   PIC X(18) VALUE                               
028700                            'RE SPEC. DONE     '.                         
028800                                                                          
028900*01  FILLER                  PIC X(16)   VALUE 'ORSAKSTEXTER   '.         
029000*01  -COPY W221W006                                                       
029100                                                                          
029200 01  W-ORSAKS-KOD            PIC 9(2)    VALUE ZERO.                      
029300                                                                          
029400 01  W-ORSAKS-TABELL.                                                     
029500     03 W-ORSAKS-TAB OCCURS 3.                                            
029600         05 W-ORSAK-AENDRAD  PIC X.                                       
029700         05 W-ORSAK-TAB-KOD  PIC 9(2).                                    
029800 01  ORSAK-IX                PIC 9(2)    VALUE ZERO.                      
029900 01  ORSAK-IX-PLUS-1         PIC 9(2)    VALUE ZERO.                      
030000 01  ORSAK-IX-MAX            PIC 9(2)    VALUE 3.                         
030100                                                                          
030200 01  W-MESSAGE-BOTTOM.                                                    
030300     03  W-MESSAGE-BOTTOM-1  PIC X(18)  VALUE SPACE.                      
030400     03  FILLER              PIC X(01)  VALUE SPACE.                      
030500     03  W-MESSAGE-BOTTOM-2  PIC X(18)  VALUE SPACE.                      
030600     03  FILLER              PIC X(01)  VALUE SPACE.                      
030700     03  W-MESSAGE-BOTTOM-3  PIC X(18)  VALUE SPACE.                      
030800                                                                          
030900                                                                          
031000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
031100                                                                          
031200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
031300     88  EGEN-MID                            VALUE '2403'.                
031400     88  GODK-MID                            VALUE '2403' '244G'          
031500                                                   '2471'.                
031600     88  HOPP-FRAAN-2447                     VALUE '244G'.                
031700     88  HELP-MID                            VALUE '0551'.                
031800                                                                          
031900                                                                          
032000                                                                          
032100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
032200 01  GENERELLA-SUBPROGRAM.                                                
032300     03  W224OMSP                PIC X(8)    VALUE 'W224OMSP'.            
032400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
032500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
032600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
032700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
032800*    03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
032900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
033000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
033100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
033200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
033300     03  W221LPAD                PIC X(8)    VALUE 'W221LPAD'.            
033400     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
033500                                                                          
033600*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
033700 01  W-W221LP-CTX            PIC X(08) VALUE 'W221LP02'.                  
033800 01  W-KDLPORS-GRP.                                                       
033900     03 W-KDLPORS-TAB OCCURS 4 PIC 9(3).                                  
034000                                                                          
034100*    --- LÄNKAREA TILL SUBPROGRAM W224OMSP                                
034200 01  FILLER                      PIC X(16)   VALUE 'W224OMSP'.            
034300*01  -COPY W224OMSP                                                       
034400                                                                          
034500                                                                          
034600*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
034700 01  FILLER                      PIC X(16)   VALUE 'WORKDAY '.            
034800*01  -COPY WORKAREA                                                       
034900                                                                          
035000                                                                          
035100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
035200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
035300*01 -COPY WMSGINIT                                                        
035400                                                                          
035500                                                                          
035600**** VARIABLER TILL W009VADD****                                          
035700 01  W009VADDW-AAVV              PIC S9(5)   COMP-3.                      
035800 01  W009VADDW-ANTAL             PIC S9(3)   COMP-3.                      
035900                                                                          
036000                                                                          
036100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
036200*01 -COPY WDATAREA                                                        
036300                                                                          
036400 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS   '.         
036500*   -COPY WZ20DAYS                                                        
036600     EJECT                                                                
036700                                                                          
036800                                                                          
036900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
037000*01 -COPY WMEDAREA                                                        
037100 01  MESSAGE-CODES.                                                       
037200     03  ERR-KONFLIKT               PIC X(3) VALUE '002'.                 
037300     03  INF-PRESS-PF11-TO-UPDATE   PIC X(3) VALUE '003'.                 
037400     03  ERR-PF11-AND-NO-DATA       PIC X(3) VALUE '011'.                 
037500     03  ERR-ARTIKEL-SAKNAS         PIC X(3) VALUE '017'.                 
037600     03  INF-ARTIKEL-UTGANGEN       PIC X(3) VALUE '018'.                 
037700     03  INF-UPDATE-DONE            PIC X(3) VALUE '101'.                 
037800     03  ERR-SUPPLIER-MISSING       PIC X(3) VALUE '273'.                 
037900     03  ERR-FEL-ATG                PIC X(3) VALUE '304'.                 
038000     03  ERR-WRONG-KEY              PIC X(3) VALUE '401'.                 
038100     03  ERR-NOT-AUTH               PIC X(3) VALUE '405'.                 
038200     03  ERR-HIGHLIGHT-FIELDS-WRONG PIC X(3) VALUE '409'.                 
038300     03  INF-OTHER-SUPP-EXISTS      PIC X(3) VALUE '425'.                 
038400     03  ERR-REFILL-PART            PIC X(3) VALUE '434'.                 
038500     03  INF-OMSPEC-UTF             PIC X(3) VALUE '436'.                 
038600     03  ERR-DC-INVALID             PIC X(3) VALUE '440'.                 
038700                                                                          
038800 01  WS-IDMFSFEL                    PIC X(3) VALUE SPACE.                 
038900                                                                          
039000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
039100                                                                          
039200 01  SPAR-AREA.                                                           
039300     03  SPAR-IDTRANS           PIC X(4)    VALUE SPACE.                  
039400     03  SPAR-KDLEVPLF          PIC X(1)    VALUE SPACE.                  
039500     03  SPAR-FLJIT             PIC X(1)    VALUE SPACE.                  
039600     03  SPAR-KDLPSP            PIC 9(1)    VALUE ZERO.                   
039700     03  SPAR-IDDC              PIC X(2)    VALUE SPACE.                  
039800     03  SPAR-IDLEVNR           PIC X(5)    VALUE SPACE.                  
039900     03  SPAR-2447-DIALOG       PIC X(1)    VALUE SPACE.                  
040000                                                                          
040100     03  SPAR-TEREFMED1         PIC X(36)   VALUE SPACE.                  
040200     03  SPAR-TEREFMED2         PIC X(36)   VALUE SPACE.                  
040300                                                                          
040400     03 SPAR-WDD6-KEY.                                                    
040500         05 SPAR-IDDC-D6        PIC X(2).                                 
040600         05 SPAR-IDLEVNR-D6     PIC X(5).                                 
040700         05 SPAR-IDARTNR-D6     PIC S9(9)   COMP-3.                       
040800         05 SPAR-IDANSK-D6      PIC S9(3)   COMP-3.                       
040900                                                                          
041000     03  W-GAMLA-AVROP.                                                   
041100         05 W-AVROP-GAMLA OCCURS 5.                                       
041200             07 W-AVROP-GAM-AENDRAT     PIC X.                            
041300             07 W-KVAVROP-GAM           PIC 9(7).                         
041400             07 W-TIAVROP-AVS-GAM       PIC 9(4).                         
041500             07 W-AVROP-GAM-INL-PAST    PIC X.                            
041600                                                                          
041700     03  NYA-AVROP.                                                       
041800         05 W-AVROP-TAB-RAD OCCURS 10.                                    
041900             07 W-PERIOD-TAB-RAD        PIC 9(4).                         
042000             07 W-AVROP-TAB-KOL OCCURS 5.                                 
042100                 09 W-AVROP-TAB-AENDRAT PIC X.                            
042200                 09 W-VECKA-TAB         PIC 9(4).                         
042300                 09 FILLER REDEFINES W-VECKA-TAB.                         
042400                    11 W-VECKA-AA-TAB   PIC 9(2).                         
042500                    11 W-VECKA-VV-TAB   PIC 9(2).                         
042600                 09 W-KVAVROP-TAB       PIC 9(7).                         
042700                 09 W-AVROP-TAB-INL-PAST PIC X.                           
042800                                                                          
042900                                                                          
043000 01  SPAR-AREA-2447.                                                      
043100     03 SPAR-IDTRANS-2447         PIC X(4)    VALUE SPACE.                
043200     03 SPAR-IDDC-2447            PIC X(2)    VALUE SPACE.                
043300     03 SPAR-IDLEVNR-2447         PIC X(5)    VALUE SPACE.                
043400     03 SPAR-IDDC-D6-2447         PIC X(2)    VALUE SPACE.                
043500     03 SPAR-IDLEVNR-D6-2447      PIC X(5)    VALUE SPACE.                
043600     03 SPAR-IDARTNR-D6-2447      PIC S9(9)   COMP-3 VALUE ZERO.          
043700     03 SPAR-IDANSK-D6-2447       PIC S9(3)   COMP-3 VALUE ZERO.          
043800                                                                          
043900                                                                          
044000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
044100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
044200*01  MID -COPY W2I40301         -PRE MID-                                 
044300                                                                          
044400 01  FILLER                      PIC X(16)   VALUE 'MID-2447'.            
044500*01  MID -COPY W2I44701         -PRE 2447-                                
044600                                                                          
044700 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
044800*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
044900*01  -COPY WMSGKOM                                                        
045000                                                                          
045100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
045200*01          -COPY WMSGAREA                                               
045300     03  MOD REDEFINES MSG-AREA.                                          
045400*        05  -COPY W2O40301     -PRE MOD-                                 
045500                                                                          
045600                                                                          
045700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
045800*01  -COPY WMFSAREA                                                       
045900                                                                          
046000 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
046100*01 -COPY WWIDFTG                                                         
046200                                                                          
046300     EJECT                                                                
046400                                                                          
046500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046600                                                                          
046700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
046800                                                                          
046900 01  NYCKLAR-TILL-DLI.                                                    
047000     03  W-IDARTNR-X.                                                     
047100         05  W-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.             
047200     03  W-IDARTNR-OLD-X.                                                 
047300         05  W-IDARTNR-OLD     PIC S9(9)   VALUE ZERO COMP-3.             
047400                                                                          
047500     03 W-WDD601KY-MIN.                                                   
047600         05 W-IDDC-D6-MIN      PIC X(2)  VALUE LOW-VALUE.                 
047700         05 W-IDLEVNR-D6-MIN   PIC X(5)  VALUE LOW-VALUE.                 
047800         05 W-IDARTNR-D6-MIN   PIC S9(9) VALUE ZERO COMP-3.               
047900         05 W-IDANSK-D6-MIN-X.                                            
048000            07 W-IDANSK-D6-MIN PIC S9(3) VALUE ZERO COMP-3.               
048100                                                                          
048200     03 W-WDD601KY-MAX.                                                   
048300         05 W-IDDC-D6-MAX      PIC X(2)  VALUE HIGH-VALUE.                
048400         05 W-IDLEVNR-D6-MAX   PIC X(5)  VALUE HIGH-VALUE.                
048500         05 W-IDARTNR-D6-MAX   PIC S9(9) VALUE +999999999 COMP-3.         
048600         05 W-IDANSK-D6-MAX-X.                                            
048700            07 W-IDANSK-D6-MAX PIC S9(3) VALUE +999 COMP-3.               
048800                                                                          
048900     03 W-WDD7A1KY-MIN.                                                   
049000         05 W-IDARTNR-MIN7     PIC S9(9)  COMP-3 VALUE ZERO.              
049100         05 FILLER             PIC S9(9)  COMP-3 VALUE ZERO.              
049200         05 FILLER             PIC S9(3)  COMP-3 VALUE ZERO.              
049300                                                                          
049400     03  W-WDD7A1KY-MAX.                                                  
049500         05  W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.              
049600         05  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.        
049700         05  FILLER            PIC S9(3)  COMP-3 VALUE +999.              
049800     03 W-IDDC-X.                                                         
049900         05  W-IDDC            PIC X(2)    VALUE SPACE.                   
050000     03 W-IDDC-K7-X.                                                      
050100         05  W-IDDC-K7         PIC X(2)    VALUE SPACE.                   
050200     03  W-IDLAND-X.                                                      
050300         05  W-IDLAND          PIC X(2)    VALUE SPACE.                   
050400     03  W-IDSKYLT-X.                                                     
050500         05  W-IDSKYLT         PIC X(3)    VALUE SPACE.                   
050600     03  W-IDLEVNR-X.                                                     
050700         05  W-IDLEVNR         PIC X(5)    VALUE SPACE.                   
050800     03  W-IDLEVNR-D9-X.                                                  
050900         05  W-IDLEVNR-D9      PIC X(5)    VALUE SPACE.                   
051000     03  W-WDD901KY-X.                                                    
051100         05  W-IDARTNR-D9      PIC S9(9)   VALUE ZERO COMP-3.             
051200         05  W-IDDC-D9         PIC X(2)    VALUE SPACE.                   
051300     03  W-WDD905KY-X.                                                    
051400         05  W-DAAVROP-X.                                                 
051500             07 W-DAAVROP-SS   PIC  9(2)   VALUE ZERO.                    
051600             07 W-DAAVROP-AAVV PIC  9(4)   VALUE ZERO.                    
051700         05  W-TILEVDAG        PIC  S9     VALUE ZERO COMP-3.             
051800     03  W-KDAVROP-X.                                                     
051900         05  W-KDAVROP         PIC S9(1)   COMP-3.                        
052000     03  W-WDG301KY-X.                                                    
052100         05  W-IDHTYP-G3       PIC X(4)    VALUE '2203'.                  
052200         05  W-IDDC-G3         PIC X(2)    VALUE SPACE.                   
052300         05  FILLER            PIC X(24)   VALUE LOW-VALUE.               
052400     03  W-WDF301KY-X.                                                    
052500         05  W-IDLANDX2        PIC X(2)    VALUE SPACE.                   
052600         05  W-DADATUM-HELG    PIC 9(8)    VALUE ZERO.                    
052700         05  FILLER  REDEFINES W-DADATUM-HELG.                            
052800             07  W-DADATUM-HELG-SS         PIC 9(2).                      
052900             07  W-DADATUM-HELG-AAMMDD     PIC 9(6).                      
053000                                                                          
053100     03  W-WDGXKEY-2247-P-X.                                              
053200         05  W-IDHTYP            PIC X(04)   VALUE '2247'.                
053300         05  W-FLLEVPLP          PIC X(01)   VALUE 'J'.                   
053400         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
053500                                                                          
053600     03  W-WDGXKEY-2247-X.                                                
053700         05  W-IDHTYP            PIC X(04)   VALUE '2247'.                
053800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
053900                                                                          
054000     03  W-WDGXKEY-2205-X.                                                
054100         05  W-IDHTYP            PIC X(04)    VALUE '2205'.               
054200         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
054300     03  W-KY2206-X.                                                      
054400         05  W-IDLEVNR-2206      PIC X(5)     VALUE SPACE.                
054500         05  W-IDDC-2206         PIC X(2)     VALUE SPACE.                
054600                                                                          
054700     03  W-WDGXKEY-2261-X.                                                
054800         05  W-IDHTYP            PIC X(04)    VALUE '2261'.               
054900         05  W-IDDC-2261         PIC X(2)     VALUE SPACE.                
055000         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
055100                                                                          
055200*    --- STATUS KODER FRÅN IMS                                            
055300 01  STATUS-WS                   PIC XX.                                  
055400     88  SEGMENT-FINNS                       VALUE '  '.                  
055500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
055600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
055700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
055800                                                                          
055900 01  GODK-STATUSKODER.                                                    
056000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
056100                                                                          
056200 01  ALL-SSA.                                                             
056300     03 SSA1                     PIC X(64).                               
056400     03 SSA2                     PIC X(64).                               
056500     03 SSA3                     PIC X(64).                               
056600                                                                          
056700                                                                          
056800                                                                          
056900*    --- IMS FUNKTIONSKODER                                               
057000*01  -COPY W0003                                                          
057100                                                                          
057200*    ---  DLI INPUT-OUTPUT AREA                                           
057300                                                                          
057400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
057500 01  DLI-IO-WDK601.                                                       
057600*    03  -COPY WDK601                                                     
057700                                                                          
057800 01  FILLER         PIC X(16) VALUE 'DLI-WDK601OLD'.                      
057900 01  DLI-IO-WDK601-OLD.                                                   
058000*    03  -COPY WDK601  -PRE OLD-                                          
058100                                                                          
058200                                                                          
058300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
058400 01  DLI-IO-WDK611.                                                       
058500*    03  -COPY WDK611                                                     
058600                                                                          
058700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
058800 01  DLI-IO-WDK629.                                                       
058900*    03  -COPY WDK629                                                     
059000                                                                          
059100 01  FILLER         PIC X(16) VALUE 'DLI-WDK611OLD'.                      
059200 01  DLI-IO-WDK611-OLD.                                                   
059300*    03  -COPY WDK611 -PRE OLD-                                           
059400                                                                          
059500                                                                          
059600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
059700 01  DLI-IO-WDK701.                                                       
059800*    03  -COPY WDK701                                                     
059900                                                                          
060000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
060100 01  DLI-IO-WDK711.                                                       
060200*    03  -COPY WDK711                                                     
060300                                                                          
060400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
060500 01  DLI-IO-WDK712.                                                       
060600*    03  -COPY WDK712                                                     
060700                                                                          
060800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
060900 01  DLI-IO-WDK722.                                                       
061000*    03  -COPY WDK722                                                     
061100                                                                          
061200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
061300 01  DLI-IO-WDK723.                                                       
061400*    03  -COPY WDK723                                                     
061500                                                                          
061600                                                                          
061700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
061800 01  DLI-IO-WDF101.                                                       
061900*    03  -COPY WDF101                                                     
062000                                                                          
062100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
062200 01  DLI-IO-WDF106.                                                       
062300*    03  -COPY WDF106                                                     
062400                                                                          
062500                                                                          
062600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
062700 01  DLI-IO-WDF116.                                                       
062800*    03  -COPY WDF116                                                     
062900                                                                          
063000                                                                          
063100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF301'.                      
063200 01  DLI-IO-WDF301.                                                       
063300*    03  -COPY WDF301                                                     
063400                                                                          
063500                                                                          
063600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
063700 01  DLI-IO-WDD311.                                                       
063800*    03  -COPY WDD311                                                     
063900                                                                          
064000                                                                          
064100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
064200 01  DLI-IO-WDD601.                                                       
064300*    03  -COPY WDD601                                                     
064400                                                                          
064500                                                                          
064600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD701'.                      
064700 01  DLI-IO-WDD701.                                                       
064800*    03  -COPY WDD701                                                     
064900                                                                          
065000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD702'.                      
065100 01  DLI-IO-WDD702.                                                       
065200*    03  -COPY WDD702                                                     
065300                                                                          
065400                                                                          
065500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD7A1'.                      
065600 01  DLI-IO-WDD7A1.                                                       
065700*    03  -COPY WDD7A1                                                     
065800                                                                          
065900                                                                          
066000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
066100 01  DLI-IO-WDD901.                                                       
066200*    03  -COPY WDD901 -PRE D901-                                          
066300                                                                          
066400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
066500 01  DLI-IO-WDD902.                                                       
066600*    03  -COPY WDD902 -PRE D902-                                          
066700                                                                          
066800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
066900 01  DLI-IO-WDD904.                                                       
067000*    03  -COPY WDD904 -PRE D904-                                          
067100                                                                          
067200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
067300 01  DLI-IO-WDD905.                                                       
067400*    03  -COPY WDD905 -PRE D905-                                          
067500                                                                          
067600                                                                          
067700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG302'.                      
067800 01  DLI-IO-WDG302.                                                       
067900*    03  -COPY WDGX2204                                                   
068000                                                                          
068100                                                                          
068200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
068300 01  DLI-IO-WDB601.                                                       
068400*    03  -COPY WDB601                                                     
068500                                                                          
068600                                                                          
068700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2262'.                    
068800 01  DLI-IO-WDGX2262.                                                     
068900*    03  -COPY WDGX2262                                                   
069000                                                                          
069100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
069200 01  DLI-IO-WDGX2248.                                                     
069300*    03  -COPY WDGX2248                                                   
069400                                                                          
069500                                                                          
069600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
069700 01  DLI-IO-WDGX2206.                                                     
069800*    03  -COPY WDGX2206                                                   
069900                                                                          
070000                                                                          
070100                                                                          
070200 LINKAGE SECTION.                                                         
070300*01   -COPY W0009             -PRE MSG-                                   
070400                                                                          
070500*01   -COPY W0009             -PRE MSGKOM-                                
070600                                                                          
070700*01   -COPY W0008             -PRE WDP7-                                  
070800   05  FILLER PIC X.                                                      
070900*01   -COPY W0008             -PRE WDK6-                                  
071000   05  FILLER PIC X.                                                      
071100*01   -COPY W0008             -PRE WDK7-                                  
071200   05  FILLER PIC X.                                                      
071300*01   -COPY W0008             -PRE WDF1-                                  
071400   05  FILLER PIC X.                                                      
071500*01   -COPY W0008             -PRE WDF3-                                  
071600   05  FILLER PIC X.                                                      
071700*01   -COPY W0008             -PRE WDD3-                                  
071800   05  FILLER PIC X.                                                      
071900*01   -COPY W0008             -PRE WDD6-                                  
072000   05  FILLER PIC X.                                                      
072100*01   -COPY W0008             -PRE WDD7-                                  
072200   05  FILLER PIC X.                                                      
072300*01   -COPY W0008             -PRE WDD7A1-                                
072400   05  FILLER PIC X.                                                      
072500*01   -COPY W0008             -PRE WDD9-                                  
072600   05  FILLER PIC X.                                                      
072700*01   -COPY W0008             -PRE 2261-                                  
072800   05  FILLER PIC X.                                                      
072900*01   -COPY W0008             -PRE WDG3-                                  
073000   05  FILLER PIC X.                                                      
073100*01   -COPY W0008             -PRE WDB6-                                  
073200   05  FILLER PIC X.                                                      
073300*01   -COPY W0008             -PRE WDR2-                                  
073400   05  FILLER PIC X.                                                      
073500*01   -COPY W0008             -PRE WDR5-                                  
073600   05  FILLER PIC X.                                                      
073700                                                                          
073800*    PROGRAM W224OMSP MED UNDERLIGGANDE SUBPROGRAM                        
073900 01  OMSP-WDD9-PCB                PIC X.                                  
074000 01  OMSP-WDK6-PCB                PIC X.                                  
074100 01  OMSP-WDK7-PCB                PIC X.                                  
074200 01  OMSP-WDF1-PCB                PIC X.                                  
074300 01  OMSP-WDF3-PCB                PIC X.                                  
074400 01  OMSP-WDB6-PCB                PIC X.                                  
074500 01  OMSP-WDD6-PCB                PIC X.                                  
074600 01  OMSP-WDD3-PCB                PIC X.                                  
074700 01  OMSP-WDD7-PCB                PIC X.                                  
074800                                                                          
074900*    PROGRAM W224PUNK                                                     
075000 01  OM-PUNK-REFL1-2501-PCB       PIC X.                                  
075100 01  OM-PUNK-REFL1-WDB6-PCB       PIC X.                                  
075200 01  OM-PUNK-REFL1-WDK7-PCB       PIC X.                                  
075300 01  OM-PUNK-REFL1-UTIL-WDK6-PCB  PIC X.                                  
075400 01  OM-PUNK-REFL1-UTIL-WDK7-PCB  PIC X.                                  
075500 01  OM-PUNK-REFL1-UTIL-WDB6-PCB  PIC X.                                  
075600 01  OM-PUNK-UTUP1-WDK7-PCB       PIC X.                                  
075700 01  OM-PUNK-UTUP1-WDB6-PCB       PIC X.                                  
075800 01  OM-PUNK-UTUP1-UTIL-WDK6-PCB  PIC X.                                  
075900 01  OM-PUNK-UTUP1-UTIL-WDK7-PCB  PIC X.                                  
076000 01  OM-PUNK-UTUP1-UTIL-WDB6-PCB  PIC X.                                  
076100                                                                          
076200*    PROGRAM W222BHDC                                                     
076300 01  BHDC-WDK6-PCB                PIC X.                                  
076400 01  BHDC-WDK7-PCB                PIC X.                                  
076500 01  BHDC-WDB6-PCB                PIC X.                                  
076600 01  BHDC-WDR2-PCB                PIC X.                                  
076700 01  BHDC-WDD7-PCB                PIC X.                                  
076800 01  BHDC-WDK7E-PCB               PIC X.                                  
076900 01  BHDC-WDD7-2-PCB              PIC X.                                  
077000 01  BHDC-WDK9-PCB                PIC X.                                  
077100 01  BHDC-REFL1-2501-PCB          PIC X.                                  
077200 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
077300 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
077400 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
077500 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
077600 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
077700     EJECT                                                                
077800 01  BHDC-REFL2-2501-PCB          PIC X.                                  
077900 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
078000 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
078100 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
078200 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
078300     EJECT                                                                
078400 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
078500 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
078600 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
078700     EJECT                                                                
078800 01  BHDC-W222-WDK6-PCB           PIC X.                                  
078900 01  BHDC-W222-WDK7-PCB           PIC X.                                  
079000 01  BHDC-W222-ARTM-PCB           PIC X.                                  
079100 01  BHDC-W222-2501-PCB           PIC X.                                  
079200 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
079300 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
079400 01  BHDC-W222-WDB6-PCB           PIC X.                                  
079500 01  BHDC-W222-WDD7-PCB           PIC X.                                  
079600 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
079700 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
079800 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
079900 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
080000 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
080100 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
080200 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
080300 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
080400 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
080500     EJECT                                                                
080600 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
080700 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
080800 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
080900 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
081000 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
081100     EJECT                                                                
081200                                                                          
081300*    PROGRAM W222TILG                                                     
081400 01  TILG-WDK7-PCB                PIC X.                                  
081500 01  TILG-WDL2-PCB                PIC X.                                  
081600 01  TILG-WDB6-PCB                PIC X.                                  
081700 01  TILG-WDD9-PCB                PIC X.                                  
081800 01  TILG-WDK6-PCB                PIC X.                                  
081900 01  TILG-WDK9-PCB                PIC X.                                  
082000                                                                          
082100 PROCEDURE DIVISION  USING MSG-PCB    MSGKOM-PCB                          
082200                           WDP7-PCB   WDK6-PCB  WDK7-PCB WDF1-PCB         
082300                           WDF3-PCB   WDD3-PCB  WDD6-PCB WDD7-PCB         
082400                           WDD7A1-PCB WDD9-PCB  2261-PCB WDG3-PCB         
082500                           WDB6-PCB   WDR2-PCB  WDR5-PCB                  
082600                                                                          
082700                        OMSP-WDD9-PCB OMSP-WDK6-PCB OMSP-WDK7-PCB         
082800                        OMSP-WDF1-PCB OMSP-WDF3-PCB OMSP-WDB6-PCB         
082900                        OMSP-WDD6-PCB OMSP-WDD3-PCB OMSP-WDD7-PCB         
083000                                                                          
083100                        OM-PUNK-REFL1-2501-PCB                            
083200                        OM-PUNK-REFL1-WDB6-PCB                            
083300                        OM-PUNK-REFL1-WDK7-PCB                            
083400                        OM-PUNK-REFL1-UTIL-WDK6-PCB                       
083500                        OM-PUNK-REFL1-UTIL-WDK7-PCB                       
083600                        OM-PUNK-REFL1-UTIL-WDB6-PCB                       
083700                        OM-PUNK-UTUP1-WDK7-PCB                            
083800                        OM-PUNK-UTUP1-WDB6-PCB                            
083900                        OM-PUNK-UTUP1-UTIL-WDK6-PCB                       
084000                        OM-PUNK-UTUP1-UTIL-WDK7-PCB                       
084100                        OM-PUNK-UTUP1-UTIL-WDB6-PCB                       
084200                                                                          
084300                        BHDC-WDK6-PCB BHDC-WDK7-PCB                       
084400                        BHDC-WDB6-PCB BHDC-WDR2-PCB                       
084500                        BHDC-WDD7-PCB BHDC-WDK7E-PCB                      
084600                        BHDC-WDD7-2-PCB BHDC-WDK9-PCB                     
084700                        BHDC-REFL1-2501-PCB                               
084800                        BHDC-REFL1-WDB6-PCB                               
084900                        BHDC-REFL1-WDK7-PCB                               
085000                        BHDC-REFL1-UTIL-WDK6-PCB                          
085100                        BHDC-REFL1-UTIL-WDK7-PCB                          
085200                        BHDC-REFL1-UTIL-WDB6-PCB                          
085300                        BHDC-REFL2-2501-PCB                               
085400                        BHDC-REFL2-WDB6-PCB                               
085500                        BHDC-REFL2-UTIL-WDK6-PCB                          
085600                        BHDC-REFL2-UTIL-WDK7-PCB                          
085700                        BHDC-REFL2-UTIL-WDB6-PCB                          
085800                        BHDC-UTIL-WDK6-PCB                                
085900                        BHDC-UTIL-WDK7-PCB                                
086000                        BHDC-UTIL-WDB6-PCB                                
086100                        BHDC-W222-WDK6-PCB                                
086200                        BHDC-W222-WDK7-PCB                                
086300                        BHDC-W222-ARTM-PCB                                
086400                        BHDC-W222-2501-PCB                                
086500                        BHDC-W222-WDB6R-PCB                               
086600                        BHDC-W222-WDK7R-PCB                               
086700                        BHDC-W222-WDB6-PCB                                
086800                        BHDC-W222-WDD7-PCB                                
086900                        BHDC-W222-WDK7E-PCB                               
087000                        BHDC-W222-UTIL-WDK6-PCB                           
087100                        BHDC-W222-UTIL-WDK7-PCB                           
087200                        BHDC-W222-UTIL-WDB6-PCB                           
087300                        BHDC-W222-UTUP-WDK7-PCB                           
087400                        BHDC-W222-UTUP-WDB6-PCB                           
087500                        BHDC-W222-UTUP-UTIL-WDK6-PCB                      
087600                        BHDC-W222-UTUP-UTIL-WDK7-PCB                      
087700                        BHDC-W222-UTUP-UTIL-WDB6-PCB                      
087800                        BHDC-UTUP-WDK7-PCB                                
087900                        BHDC-UTUP-WDB6-PCB                                
088000                        BHDC-UTUP-UTIL-WDK6-PCB                           
088100                        BHDC-UTUP-UTIL-WDK7-PCB                           
088200                        BHDC-UTUP-UTIL-WDB6-PCB                           
088300                                                                          
088400                        TILG-WDK7-PCB                                     
088500                        TILG-WDL2-PCB                                     
088600                        TILG-WDB6-PCB                                     
088700                        TILG-WDD9-PCB                                     
088800                        TILG-WDK6-PCB                                     
088900                        TILG-WDK9-PCB                                     
089000                        .                                                 
089100                                                                          
089200 MAIN SECTION.                                                            
089300     ENTRY 'DLITCBL' USING MSG-PCB    MSGKOM-PCB                          
089400                           WDP7-PCB   WDK6-PCB  WDK7-PCB WDF1-PCB         
089500                           WDF3-PCB   WDD3-PCB  WDD6-PCB WDD7-PCB         
089600                           WDD7A1-PCB WDD9-PCB  2261-PCB WDG3-PCB         
089700                           WDB6-PCB   WDR2-PCB  WDR5-PCB                  
089800                                                                          
089900           OMSP-WDD9-PCB OMSP-WDK6-PCB OMSP-WDK7-PCB                      
090000           OMSP-WDF1-PCB OMSP-WDF3-PCB OMSP-WDB6-PCB                      
090100           OMSP-WDD6-PCB OMSP-WDD3-PCB OMSP-WDD7-PCB                      
090200                                                                          
090300           OM-PUNK-REFL1-2501-PCB                                         
090400           OM-PUNK-REFL1-WDB6-PCB                                         
090500           OM-PUNK-REFL1-WDK7-PCB                                         
090600           OM-PUNK-REFL1-UTIL-WDK6-PCB                                    
090700           OM-PUNK-REFL1-UTIL-WDK7-PCB                                    
090800           OM-PUNK-REFL1-UTIL-WDB6-PCB                                    
090900           OM-PUNK-UTUP1-WDK7-PCB                                         
091000           OM-PUNK-UTUP1-WDB6-PCB                                         
091100           OM-PUNK-UTUP1-UTIL-WDK6-PCB                                    
091200           OM-PUNK-UTUP1-UTIL-WDK7-PCB                                    
091300           OM-PUNK-UTUP1-UTIL-WDB6-PCB                                    
091400                                                                          
091500           BHDC-WDK6-PCB BHDC-WDK7-PCB                                    
091600           BHDC-WDB6-PCB BHDC-WDR2-PCB                                    
091700           BHDC-WDD7-PCB BHDC-WDK7E-PCB                                   
091800           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                                  
091900           BHDC-REFL1-2501-PCB                                            
092000           BHDC-REFL1-WDB6-PCB                                            
092100           BHDC-REFL1-WDK7-PCB                                            
092200           BHDC-REFL1-UTIL-WDK6-PCB                                       
092300           BHDC-REFL1-UTIL-WDK7-PCB                                       
092400           BHDC-REFL1-UTIL-WDB6-PCB                                       
092500           BHDC-REFL2-2501-PCB                                            
092600           BHDC-REFL2-WDB6-PCB                                            
092700           BHDC-REFL2-UTIL-WDK6-PCB                                       
092800           BHDC-REFL2-UTIL-WDK7-PCB                                       
092900           BHDC-REFL2-UTIL-WDB6-PCB                                       
093000           BHDC-UTIL-WDK6-PCB                                             
093100           BHDC-UTIL-WDK7-PCB                                             
093200           BHDC-UTIL-WDB6-PCB                                             
093300           BHDC-W222-WDK6-PCB                                             
093400           BHDC-W222-WDK7-PCB                                             
093500           BHDC-W222-ARTM-PCB                                             
093600           BHDC-W222-2501-PCB                                             
093700           BHDC-W222-WDB6R-PCB                                            
093800           BHDC-W222-WDK7R-PCB                                            
093900           BHDC-W222-WDB6-PCB                                             
094000           BHDC-W222-WDD7-PCB                                             
094100           BHDC-W222-WDK7E-PCB                                            
094200           BHDC-W222-UTIL-WDK6-PCB                                        
094300           BHDC-W222-UTIL-WDK7-PCB                                        
094400           BHDC-W222-UTIL-WDB6-PCB                                        
094500           BHDC-W222-UTUP-WDK7-PCB                                        
094600           BHDC-W222-UTUP-WDB6-PCB                                        
094700           BHDC-W222-UTUP-UTIL-WDK6-PCB                                   
094800           BHDC-W222-UTUP-UTIL-WDK7-PCB                                   
094900           BHDC-W222-UTUP-UTIL-WDB6-PCB                                   
095000           BHDC-UTUP-WDK7-PCB                                             
095100           BHDC-UTUP-WDB6-PCB                                             
095200           BHDC-UTUP-UTIL-WDK6-PCB                                        
095300           BHDC-UTUP-UTIL-WDK7-PCB                                        
095400           BHDC-UTUP-UTIL-WDB6-PCB                                        
095500                                                                          
095600           TILG-WDK7-PCB                                                  
095700           TILG-WDL2-PCB                                                  
095800           TILG-WDB6-PCB                                                  
095900           TILG-WDD9-PCB                                                  
096000           TILG-WDK6-PCB                                                  
096100           TILG-WDK9-PCB                                                  
096200           .                                                              
096300                                                                          
096400     PERFORM IMS-GET-MSG                                                  
096500     IF SEGMENT-FINNS                                                     
096600        PERFORM IMS-GET-WMSGKOM-MSG                                       
096700                                                                          
096800        PERFORM A-INIT                                                    
096900        PERFORM B-KOLLA-NYCKLAR                                           
097000                                                                          
097100        IF NYCKLAR-OK                                                     
097200           IF MFS-UPDATE OR MFS-UPD-X                                     
097300              PERFORM G-KOLLA-INPUT                                       
097400              IF INDATA-OK                                                
097500                 PERFORM H-UPPDATERA                                      
097600              END-IF                                                      
097700           ELSE                                                           
097800              IF MFS-RETURN                                               
097900                 CONTINUE                                                 
098000              ELSE                                                        
098100                 IF MFS-NEXT                                              
098200                    PERFORM D-NEXT-2447-ARTIKEL                           
098300                 ELSE                                                     
098400                    IF GAMLA-NYCKLAR                                      
098500                       PERFORM E-SAMMA-SIDA                               
098600                    END-IF                                                
098700                 END-IF                                                   
098800              END-IF                                                      
098900           END-IF                                                         
099000                                                                          
099100           IF INDATA-OK AND NOT MFS-UPD-X                                 
099200              PERFORM F-LAES-VISA-INFO                                    
099300           END-IF                                                         
099400        END-IF                                                            
099500                                                                          
099600        IF MFS-UPD-X                                                      
099610           IF MSG-KOM-IDMFSMED = SPACE                                    
099620             MOVE INF-UPDATE-DONE    TO MSG-KOM-IDMFSMED                  
099630           END-IF                                                         
099700           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM + 17          
099800           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
099900        ELSE                                                              
100000           COMPUTE MSG-KVLL = LENGTH OF MOD-W2O40301 + 4                  
100100           PERFORM IMS-INSERT-MSG                                         
100200        END-IF                                                            
100300     END-IF                                                               
100400                                                                          
100500     MOVE ZERO TO RETURN-CODE                                             
100600     GOBACK                                                               
100700     .                                                                    
100800                                                                          
100900                                                                          
101000                                                                          
101100 A-INIT SECTION.                                                          
101200                                                                          
101300     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
101400                                                                          
101500     IF MSG-DUBBLA-TRANSKODER                                             
101600       MOVE MSG-INDATA-MINUS-2-TRANSKODER  TO MID-W2I40301                
101700       MOVE MSG-IDTRANS-2                  TO MFS-IDTRANS                 
101800       MOVE MSG-KDMFSFOR-2                 TO MFS-KDMFSFOR                
101900     ELSE                                                                 
102000       MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W2I40301                
102100       MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                 
102200       MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR                
102300     END-IF                                                               
102400                                                                          
102500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
102600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
102700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
102800                                                                          
102900     MOVE LOW-VALUE  TO MSG-AREA                                          
103000     MOVE 'W2O403N1' TO MFS-IDMOD                                         
103100     MOVE '2403'     TO MOD-IDTRANS                                       
103200     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
103300                                                                          
103400     MOVE 'IDAG  '          TO DAT-KDDATFORM                              
103500                                                                          
103600     CALL WDATKONV USING DAT-KDDATFORM                                    
103700                         DAT-I-TIDATUM                                    
103800                         DAT-O-TIDATUM                                    
103900                         DAT-KDSVAR                                       
104000                                                                          
104100     MOVE DAT-TIAAMMDD      TO W-DAGENS-DATUM                             
104200     MOVE DAT-TIAAMMDD      TO W-DAGENS-DATUM-AAAAMMDD(3:6)               
104300     MOVE DAT-TISEKEL       TO W-DAGENS-DATUM-AAAAMMDD(1:2)               
104400                                                                          
104500     MOVE DAT-TIAA          TO W-DATUM-AA                                 
104600                               W-START-PER-AA                             
104700     MOVE DAT-TIRP          TO W-START-PER-PP                             
104800     MOVE DAT-TIVV          TO W-DATUM-VV                                 
104900     MOVE DAT-TIAARP        TO W-START-PER-AAPP                           
105000     MOVE W-DATUM-AAVV      TO W-DATUM-AKTUELLT                           
105100     COMPUTE W-DATUM-AKTUELLT-AAAAVV =                                    
105200             W-DATUM-AKTUELLT + 200000                                    
105300                                                                          
105400     MOVE DAT-TISEKEL       TO W-DATUM-AKTUELLT-AAAAVVD(1:2)              
105500     MOVE DAT-TIAAVVD       TO W-DATUM-AKTUELLT-AAAAVVD(3:5)              
105600                                                                          
105700     MOVE W-DATUM-AKTUELLT  TO W009VADDW-AAVV                             
105800     MOVE -26               TO W009VADDW-ANTAL                            
105900     CALL W009VADD USING W009VADDW-AAVV                                   
106000                         W009VADDW-ANTAL                                  
106100     MOVE W009VADDW-AAVV     TO W-AAVV-MINUS-HALVAR                       
106200                                                                          
106300     MOVE 20                 TO W-TIAVROP-CHG-SS                          
106400                                                                          
106500                                                                          
106600     IF EGEN-MID OR HELP-MID                                              
106700     OR HOPP-FRAAN-2447                                                   
106800       CONTINUE                                                           
106900     ELSE                                                                 
107000       MOVE SPACE           TO MFS-KDTRTYP                                
107100       MOVE '7'             TO MFS-IDPFK                                  
107200     END-IF                                                               
107300     .                                                                    
107400                                                                          
107500                                                                          
107600 B-KOLLA-NYCKLAR SECTION.                                                 
107700                                                                          
107800     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
107900                                                                          
108000     MOVE ALL '+'             TO MSGI-WMSGINIT                            
108100     MOVE '001'               TO MSGI-KDCALL                              
108200     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
108300     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
108400     MOVE '2403'              TO MSGI-IDTRANS                             
108500                                                                          
108600     IF GODK-MID                                                          
108700        IF MID-IDARTNR-IN = '0000000 '                                    
108800           MOVE ALL '+'       TO MID-IDARTNR-IN                           
108900        END-IF                                                            
109000        MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                             
109100        MOVE MID-IDDC-IN      TO MSGI-IDDC-KEY                            
109200        MOVE MID-KDAVROP-IN   TO MSGI-KDAVROP                             
109300        MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                             
109400     END-IF                                                               
109500                                                                          
109600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
109700     MOVE MSGI-SPAR-AREA       TO SPAR-AREA                               
109800                                                                          
109900     IF HOPP-FRAAN-2447                                                   
110000        MOVE SPAR-AREA            TO SPAR-AREA-2447                       
110100        MOVE SPAR-IDDC-2447       TO SPAR-IDDC                            
110200        MOVE SPAR-IDLEVNR-2447    TO SPAR-IDLEVNR                         
110300        MOVE 'J'                  TO SPAR-2447-DIALOG                     
110400        MOVE SPAR-IDDC-D6-2447    TO SPAR-IDDC-D6                         
110500        MOVE SPAR-IDLEVNR-D6-2447 TO SPAR-IDLEVNR-D6                      
110600        MOVE SPAR-IDARTNR-D6-2447 TO SPAR-IDARTNR-D6                      
110700        MOVE SPAR-IDANSK-D6-2447  TO SPAR-IDANSK-D6                       
110800        IF MSGI-KDLPORS = '0 '                                            
110900           MOVE SPACE TO MSGI-KDLPORS                                     
111000        END-IF                                                            
111100     ELSE                                                                 
111200        MOVE SPACE             TO SPAR-AREA-2447                          
111300     END-IF                                                               
111400                                                                          
111500     MOVE 'GB'                 TO MED-IDSKYLT                             
111600                                                                          
111700     MOVE JA    TO NYCKLAR-SW                                             
111800     MOVE NEJ   TO NYA-NYCKLAR-SW                                         
111900                                                                          
112000     MOVE SPACE TO MED-IDMFSFEL                                           
112100                   MED-IDMFSINF                                           
112200                                                                          
112300      IF MID-IDARTNR-IN     NOT = ALL '+'                                 
112400      OR MID-IDDC-IN        NOT = ALL '+'                                 
112500      OR MID-KDAVROP-IN     NOT = ALL '+'                                 
112600      OR MID-IDLEVNR-IN     NOT = ALL '+'                                 
112700         PERFORM MFS-RENSA-FAELT-UT                                       
112800         PERFORM MFS-RENSA-FAELT-IN                                       
112900      END-IF                                                              
113000                                                                          
113100     IF (MSGI-IDARTNR = ALL '+' OR SPACE OR ZERO)                         
113200     OR (MSGI-IDDC-KEY = ALL '+' OR SPACE)                                
113300        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL MSG-KOM-IDMFSMED               
113400        MOVE NEJ          TO NYCKLAR-SW                                   
113500     END-IF                                                               
113600                                                                          
113700     IF NYCKLAR-OK                                                        
113800        PERFORM BA-KOLLA-KDAVROP                                          
113900        PERFORM BB-KOLLA-IDARTNR                                          
114000        PERFORM BC-KOLLA-IDDC                                             
114100        PERFORM BD-KOLLA-IDLEVNR                                          
114200     END-IF                                                               
114300                                                                          
114400     IF GODK-MID                                                          
114500     OR NYCKLAR-OK                                                        
114600       MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                           
114700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
114800       MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                              
114900       MOVE MSGI-KDAVROP      TO MOD-KDAVROP-UT                           
115000       MOVE MSGI-IDLEVNR      TO MOD-IDLEVNR-UT                           
115100     ELSE                                                                 
115200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
115300                               MOD-IDDC-UT                                
115400                               MOD-KDAVROP-UT                             
115500                               MOD-IDLEVNR-UT                             
115600     END-IF                                                               
115700                                                                          
115800     IF NYCKLAR-FEL                                                       
115900        IF MED-IDMFSFEL = SPACE                                           
116000           MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL MSG-KOM-IDMFSMED          
116100        END-IF                                                            
116200        CALL WMEDKONV USING MED-WMEDAREA                                  
116300        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
116400        PERFORM MFS-RENSA-FAELT-IN                                        
116500        PERFORM MFS-RENSA-FAELT-UT                                        
116600        PERFORM MFS-STAENG-FAELT-IN                                       
116700     END-IF                                                               
116800                                                                          
116900     IF W-MESSAGE-BOTTOM NOT = SPACE                                      
117000        MOVE W-MESSAGE-BOTTOM TO MOD-TEMFSINF                             
117100     END-IF                                                               
117200                                                                          
117300     .                                                                    
117400                                                                          
117500                                                                          
117600                                                                          
117700 BA-KOLLA-KDAVROP SECTION.                                                
117800                                                                          
117900     MOVE 'BA-KOLLA-KDAVROP' TO CURRENT-SECTION                           
118000                                                                          
118100     MOVE MFS-ERASE-FIELD   TO MOD-KDAVROP-IN                             
118200     IF MID-KDAVROP-IN = ALL '+' OR SPACE                                 
118300        CONTINUE                                                          
118400     ELSE                                                                 
118500        IF NOT GODK-MID                                                   
118600           MOVE '+'           TO MID-KDAVROP-IN                           
118700        ELSE                                                              
118800           MOVE JA            TO NYA-NYCKLAR-SW                           
118900           IF NOT MFS-UPD-X                                               
119000              MOVE '7'        TO MFS-IDPFK                                
119100              MOVE SPACE      TO MFS-KDTRTYP                              
119200           END-IF                                                         
119300        END-IF                                                            
119400     END-IF                                                               
119500                                                                          
119600     IF MFS-IDPFK = '3'                                                   
119700        PERFORM S100-KOLLA-OM-INPUT                                       
119800        IF INPUT-SAKNAS                                                   
119900           MOVE JA TO SWAP-SW                                             
120000           IF MSGI-KDAVROP = 'V'                                          
120100              MOVE 'P'        TO MID-KDAVROP-IN                           
120200                                 MSGI-KDAVROP                             
120300           ELSE                                                           
120400              IF MSGI-KDAVROP = 'P'                                       
120500                 MOVE 'V'     TO MID-KDAVROP-IN                           
120600                                 MSGI-KDAVROP                             
120700              END-IF                                                      
120800           END-IF                                                         
120900        END-IF                                                            
121000     END-IF                                                               
121100                                                                          
121200*    -- KONTROLL AV KDAVROP                                               
121300     IF GODK-MID                                                          
121400        IF MID-KDAVROP-IN = '+'                                           
121500           CONTINUE                                                       
121600        ELSE                                                              
121700           IF MSGI-KDAVROP = PROPOSAL-X OR VALID-X                        
121800              CONTINUE                                                    
121900           ELSE                                                           
122000              IF MSGI-KDAVROP = SPACE                                     
122100                 MOVE PROPOSAL-X TO MSGI-KDAVROP                          
122200              ELSE                                                        
122300                 MOVE NEJ        TO NYCKLAR-SW                            
122400              END-IF                                                      
122500           END-IF                                                         
122600        END-IF                                                            
122700     ELSE                                                                 
122800        MOVE PROPOSAL-X TO MSGI-KDAVROP                                   
122900     END-IF                                                               
123000                                                                          
123100     IF NYCKLAR-OK                                                        
123200        IF MSGI-KDAVROP = PROPOSAL-X                                      
123300           MOVE PROPOSAL     TO W-KDAVROP                                 
123400           MOVE PROPOSAL-X   TO MOD-KDAVROP-UT                            
123500        ELSE                                                              
123600           MOVE VALID        TO W-KDAVROP                                 
123700           MOVE VALID-X      TO MOD-KDAVROP-UT                            
123800        END-IF                                                            
123900     END-IF                                                               
124000     .                                                                    
124100                                                                          
124200                                                                          
124300                                                                          
124400 BB-KOLLA-IDARTNR SECTION.                                                
124500     MOVE 'BB-KOLLA-IDARTNR' TO CURRENT-SECTION                           
124600                                                                          
124700*    -- KONTROLL AV IDARTNR                                               
124800                                                                          
124900     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-IN                             
125000     IF MID-IDARTNR-IN NOT = ALL '+'                                      
125100        MOVE JA              TO NYA-NYCKLAR-SW                            
125200        IF NOT MFS-UPD-X                                                  
125300           MOVE '7'          TO MFS-IDPFK                                 
125400           MOVE SPACE        TO MFS-KDTRTYP                               
125500        END-IF                                                            
125600     END-IF                                                               
125700                                                                          
125800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
125900     IF  MSGI-IDARTNR NUMERIC                                             
126000     AND MSGI-IDARTNR > ZERO                                              
126100         MOVE MSGI-IDARTNR   TO W-IDARTNR                                 
126200     ELSE                                                                 
126300        MOVE NEJ             TO NYCKLAR-SW                                
126400     END-IF                                                               
126500     .                                                                    
126600                                                                          
126700                                                                          
126800                                                                          
126900                                                                          
127000 BC-KOLLA-IDDC    SECTION.                                                
127100     MOVE 'BC-KOLLA-IDDC   ' TO CURRENT-SECTION                           
127200                                                                          
127300     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
127400     IF MID-IDDC-IN NOT = ALL '+'                                         
127500        MOVE JA              TO NYA-NYCKLAR-SW                            
127600        IF NOT MFS-UPD-X                                                  
127700           MOVE '7'          TO MFS-IDPFK                                 
127800           MOVE SPACE        TO MFS-KDTRTYP                               
127900        END-IF                                                            
128000     END-IF                                                               
128100                                                                          
128200*    -- KONTROLL AV IDDC                                                  
128300                                                                          
128400     MOVE MSGI-IDDC-KEY        TO W-IDDC                                  
128500     PERFORM IMS-GU-WDB601                                                
128600     IF SEGMENT-SAKNAS                                                    
128700        MOVE ERR-DC-INVALID    TO MED-IDMFSFEL MSG-KOM-IDMFSMED           
128800        MOVE NEJ               TO NYCKLAR-SW                              
128900     ELSE                                                                 
129000        IF DCS-NDC-CN                                                     
129100        OR (DCS-NDC-NA AND DCS-USA)                                       
129200           CONTINUE                                                       
129300        ELSE                                                              
129400           MOVE ERR-DC-INVALID TO MED-IDMFSFEL MSG-KOM-IDMFSMED           
129500           MOVE NEJ            TO NYCKLAR-SW                              
129600         END-IF                                                           
129700     END-IF                                                               
129800     .                                                                    
129900                                                                          
130000                                                                          
130100                                                                          
130200 BD-KOLLA-IDLEVNR SECTION.                                                
130300     MOVE 'BD-KOLLA-IDLEVNR' TO CURRENT-SECTION                           
130400                                                                          
130500     MOVE MFS-ERASE-FIELD    TO MOD-IDLEVNR-IN                            
130600                                                                          
130700     IF NOT GODK-MID                                                      
130800     OR NYA-NYCKLAR                                                       
130900     OR (GODK-MID AND MSGI-IDLEVNR = SPACE)                               
131000                                                                          
131100        INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO              
131200        IF  MSGI-IDARTNR NUMERIC                                          
131300        AND MSGI-IDARTNR > ZERO                                           
131400          MOVE MSGI-IDARTNR    TO W-IDARTNR                               
131500          MOVE MSGI-IDDC-KEY   TO W-IDDC                                  
131600          PERFORM IMS-GU-WDK711                                           
131700          IF SEGMENT-FINNS                                                
131800             MOVE SLAG-IDLEVNR TO MSGI-IDLEVNR                            
131900          ELSE                                                            
132000             MOVE ERR-ARTIKEL-SAKNAS                                      
132100                               TO MED-IDMFSFEL MSG-KOM-IDMFSMED           
132200             MOVE NEJ          TO NYCKLAR-SW                              
132300          END-IF                                                          
132400        ELSE                                                              
132500          MOVE NEJ             TO NYCKLAR-SW                              
132600        END-IF                                                            
132700     END-IF                                                               
132800                                                                          
132900     MOVE MSGI-IDLEVNR       TO W-IDLEVNR                                 
133000     .                                                                    
133100                                                                          
133200                                                                          
133300                                                                          
133400 D-NEXT-2447-ARTIKEL SECTION.                                             
133500                                                                          
133600     MOVE 'D-NEXT-2447     ' TO CURRENT-SECTION                           
133700                                                                          
133800     IF  (SPAR-IDDC-D6    NOT = SPACE)                                    
133900     AND SPAR-IDARTNR-D6 NUMERIC                                          
134000     AND SPAR-IDANSK-D6  NUMERIC                                          
134100        MOVE SPAR-IDDC-D6      TO W-IDDC-D6-MIN                           
134200        MOVE SPAR-IDLEVNR-D6   TO W-IDLEVNR-D6-MIN                        
134300        MOVE SPAR-IDARTNR-D6   TO W-IDARTNR-D6-MIN                        
134400        MOVE SPAR-IDANSK-D6    TO W-IDANSK-D6-MIN                         
134500        MOVE '+'               TO MID-KDAVROP-IN                          
134600     ELSE                                                                 
134700        MOVE NEJ                       TO INDATA-SW                       
134800        MOVE SPACE                     TO MOD-TEMFSFEL                    
134900        MOVE 'PF8 AND NOT FROM 2447  ' TO MOD-TEMFSFEL                    
135000        PERFORM MFS-ROER-EJ-FAELT-IN                                      
135100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
135200        PERFORM MFS-LAES-IN-IGEN                                          
135300     END-IF                                                               
135400     .                                                                    
135500                                                                          
135600                                                                          
135700                                                                          
135800 E-SAMMA-SIDA     SECTION.                                                
135900                                                                          
136000     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
136100                                                                          
136200     PERFORM S100-KOLLA-OM-INPUT                                          
136300     IF INPUT-FINNS                                                       
136400        MOVE NEJ                      TO INDATA-SW                        
136500        MOVE INF-PRESS-PF11-TO-UPDATE TO MED-IDMFSFEL                     
136600        CALL WMEDKONV USING MED-WMEDAREA                                  
136700        MOVE MED-MFSFEL               TO MOD-TEMFSINF                     
136800        PERFORM EA-MID-INDATA-TILL-MOD                                    
136900        PERFORM MFS-ROER-EJ-MOD-FAELT-UT                                  
137000     ELSE                                                                 
137100        MOVE JA                       TO INDATA-SW                        
137200        IF MSGI-KDAVROP = PROPOSAL-X                                      
137300           MOVE PROPOSAL              TO W-KDAVROP                        
137400        ELSE                                                              
137500           MOVE VALID                 TO W-KDAVROP                        
137600        END-IF                                                            
137700                                                                          
137800        PERFORM MFS-RENSA-FAELT-IN                                        
137900     END-IF                                                               
138000     .                                                                    
138100                                                                          
138200 EA-MID-INDATA-TILL-MOD SECTION.                                          
138300     MOVE 'EA-MID-INDATA-TILL-MOD '   TO CURRENT-SECTION.                 
138400                                                                          
138500     IF MID-KDKOM = ALL '+' OR SPACE                                      
138600       MOVE MFS-RENSA-FAELT  TO MOD-KDKOM-IN                              
138700     ELSE                                                                 
138800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKOM-IN-ATTR                    
138900       MOVE MFS-ROER-EJ-FAELT TO MOD-KDKOM-IN                             
139000     END-IF                                                               
139100                                                                          
139200     IF MID-KDOMSPEC = ALL '+' OR SPACE                                   
139300       MOVE MFS-RENSA-FAELT  TO MOD-KDOMSPEC-IN                           
139400     ELSE                                                                 
139500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDOMSPEC-IN-ATTR                 
139600       MOVE MFS-ROER-EJ-FAELT TO MOD-KDOMSPEC-IN                          
139700     END-IF                                                               
139800                                                                          
139900     IF MID-KDLEVPLF = ALL '+'  OR                                        
140000       (MID-KDLEVPLF = SPAR-KDLEVPLF)                                     
140100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVPLF-IN                          
140200     ELSE                                                                 
140300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLEVPLF-IN-ATTR                 
140400       MOVE MFS-ROER-EJ-FAELT TO MOD-KDLEVPLF-IN                          
140500     END-IF                                                               
140600                                                                          
140700     IF MID-FLJIT = ALL '+'  OR (MID-FLJIT = SPAR-FLJIT)                  
140800       MOVE MFS-ROER-EJ-FAELT TO MOD-FLJIT-IN                             
140900     ELSE                                                                 
141000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLJIT-IN-ATTR                    
141100       MOVE MFS-ROER-EJ-FAELT TO MOD-FLJIT-IN                             
141200     END-IF                                                               
141300                                                                          
141400     IF MID-TILPSP = ALL '+' OR (MID-TILPSP = 'YYWW')                     
141500       MOVE MFS-ROER-EJ-FAELT TO MOD-TILPSP-IN                            
141600     ELSE                                                                 
141700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TILPSP-IN-ATTR                   
141800       MOVE MFS-ROER-EJ-FAELT TO MOD-TILPSP-IN                            
141900     END-IF                                                               
142000                                                                          
142100     IF MID-TEREFMED-1 = ALL '+' OR                                       
142200       (MID-TEREFMED-1 = SPAR-TEREFMED1)                                  
142300       MOVE MFS-ROER-EJ-FAELT  TO MOD-TEREFMED1                           
142400     ELSE                                                                 
142500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEREFMED1-ATTR                   
142600       MOVE MFS-ROER-EJ-FAELT TO MOD-TEREFMED1                            
142700     END-IF                                                               
142800                                                                          
142900     IF MID-TEREFMED-2 = ALL '+' OR                                       
143000       (MID-TEREFMED-2 = SPAR-TEREFMED2)                                  
143100       MOVE MFS-ROER-EJ-FAELT  TO MOD-TEREFMED2                           
143200     ELSE                                                                 
143300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEREFMED2-ATTR                   
143400       MOVE MFS-ROER-EJ-FAELT TO MOD-TEREFMED2                            
143500     END-IF                                                               
143600                                                                          
143700*- KOLLA AVROP-INPUT RADER                                                
143800                                                                          
143900     MOVE +1  TO AVROP-IN-IX                                              
144000     PERFORM UNTIL AVROP-IN-IX > AVROP-IN-IX-MAX                          
144100                                                                          
144200       IF MID-TIAVROP-AVS (AVROP-IN-IX) = ALL '+' OR                      
144300          MID-TIAVROP-AVS (AVROP-IN-IX) = 'YYWW'                          
144400         MOVE MFS-ROER-EJ-FAELT TO                                        
144500                          MOD-TIAVROP-AVS-IN (AVROP-IN-IX)                
144600       ELSE                                                               
144700         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
144800                          MOD-TIAVROP-AVS-IN-ATTR(AVROP-IN-IX)            
144900         MOVE MFS-ROER-EJ-FAELT TO                                        
145000                         MOD-TIAVROP-AVS-IN (AVROP-IN-IX)                 
145100       END-IF                                                             
145200                                                                          
145300       IF MID-KVAVROP (AVROP-IN-IX) = ALL '+' OR                          
145400          MID-KVAVROP (AVROP-IN-IX) = SPACE                               
145500         MOVE MFS-RENSA-FAELT  TO MOD-KVAVROP-IN (AVROP-IN-IX)            
145600       ELSE                                                               
145700         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
145800                          MOD-KVAVROP-IN-ATTR(AVROP-IN-IX)                
145900         MOVE MFS-ROER-EJ-FAELT TO                                        
146000                          MOD-KVAVROP-IN (AVROP-IN-IX)                    
146100       END-IF                                                             
146200                                                                          
146300       ADD +1  TO AVROP-IN-IX                                             
146400     END-PERFORM                                                          
146500                                                                          
146600     .                                                                    
146700                                                                          
146800                                                                          
146900 F-LAES-VISA-INFO SECTION.                                                
147000     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
147100                                                                          
147200     MOVE JA      TO ALLT-SW                                              
147300                                                                          
147400     IF MFS-NEXT                                                          
147500        PERFORM FA-READ-NEXT-WDD601                                       
147600     ELSE                                                                 
147700        PERFORM FM-KOLLA-RENSA-CURRENT-PLAN                               
147800     END-IF                                                               
147900                                                                          
148000     IF ALLT-OK                                                           
148100        PERFORM FB-VISA-WDK6INFO                                          
148200     END-IF                                                               
148300                                                                          
148400     IF ALLT-OK                                                           
148500        PERFORM FC-KOLLA-ERSATTNING                                       
148600                                                                          
148700****                                                                      
148800**** FÅR VARA KVAR ETT TAG, KAN TÄNKAS ATT TEXTERNA                       
148900**** TROTS ALLT SKALL VARA MED                                            
149000****                                                                      
149100**** IF W-MESSAGE-BOTTOM-3 = SPACE                                        
149200****    IF CLAG-KVPB-SEP NOT = CLAG-KVPB-VESL                             
149300****                                                                      
149400*--- NEW  PB YYWWD                                                        
149500****       MOVE KOM-NY-PB             TO W-MESSAGE-BOTTOM-3               
149600****    ELSE                                                              
149700****       IF CLAG-RVPROURS > ZERO  OR  CLAG-RVPROFEL > ZERO              
149800****                                                                      
149900*--- PROGNOSOS WARNING                                                    
150000*--- EV. KOLL MOT FLOREGPB = JA ELLER NEJ ?? KAN DEN VARA ANNAT?          
150100****          MOVE KOM-PROGNOSVARNING TO W-MESSAGE-BOTTOM-3               
150200****       END-IF                                                         
150300****    END-IF                                                            
150400**** END-IF                                                               
150500                                                                          
150600     END-IF                                                               
150700                                                                          
150800     IF ALLT-EJ-OK AND MOD-TEMFSFEL = SPACE                               
150900        CALL WMEDKONV USING MED-WMEDAREA                                  
151000        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
151100        PERFORM MFS-RENSA-FAELT-UT                                        
151200        PERFORM MFS-STAENG-FAELT-IN                                       
151300     END-IF                                                               
151400                                                                          
151500     IF ALLT-OK                                                           
151600        PERFORM FD-VISA-WDK7INFO                                          
151700     END-IF                                                               
151800     IF ALLT-OK                                                           
151900        PERFORM FE-KOLLA-LEVERANSPLAN                                     
152000        PERFORM FF-KOLLA-ANTAL-LEVNR                                      
152100        PERFORM FG-INITIERA-TABELLER                                      
152200        PERFORM FH-VISA-OMSPEC-INFO                                       
152300        PERFORM FI-VISA-AVROP-INFO                                        
152400        IF GAMMAL-TAB-FINNS                                               
152500           PERFORM FJ-GAMLA-AVROP-TILL-MOD                                
152600        END-IF                                                            
152700        PERFORM FK-NYA-AVROP-TILL-MOD                                     
152800        PERFORM FL-VISA-TEREFMED                                          
152900                                                                          
153000        MOVE 'YYWW'              TO MOD-TIAVROP-AVS-IN (1)                
153100                                    MOD-TIAVROP-AVS-IN (2)                
153200                                    MOD-TIAVROP-AVS-IN (3)                
153300                                    MOD-TIAVROP-AVS-IN (4)                
153400                                    MOD-TILPSP-IN                         
153500        MOVE SPACE               TO SPAR-IDDC                             
153600        MOVE SPACE               TO SPAR-IDLEVNR                          
153700        MOVE SPACE               TO SPAR-2447-DIALOG                      
153800                                                                          
153900        MOVE '001'               TO MSGI-KDCALL                           
154000        MOVE '2403'              TO SPAR-IDTRANS                          
154100        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
154200                                                                          
154300        MOVE '002'               TO MSGI-KDCALL                           
154400        MOVE '2403'              TO SPAR-IDTRANS                          
154500        MOVE SPAR-AREA           TO MSGI-SPAR-AREA                        
154600        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
154700     END-IF                                                               
154800                                                                          
154900                                                                          
155000     IF W-MESSAGE-BOTTOM NOT = SPACE                                      
155100        MOVE W-MESSAGE-BOTTOM TO MOD-TEMFSINF                             
155200     END-IF                                                               
155300     .                                                                    
155400                                                                          
155500                                                                          
155600 FA-READ-NEXT-WDD601  SECTION.                                            
155700     MOVE 'FA-NEXT-WDD601  ' TO CURRENT-SECTION                           
155800                                                                          
155900     MOVE NEJ TO NEXT-WDD601-SW                                           
156000     IF SPAR-IDDC = SPACE                                                 
156100        MOVE W-IDDC(1:1) TO W-IDDC-D6-MIN(1:1)                            
156200        MOVE W-IDDC(1:1) TO W-IDDC-D6-MAX(1:1)                            
156300        MOVE '1'         TO W-IDDC-D6-MIN(2:1)                            
156400        MOVE '9'         TO W-IDDC-D6-MAX(2:1)                            
156500     ELSE                                                                 
156600        MOVE SPAR-IDDC   TO W-IDDC-D6-MIN                                 
156700                            W-IDDC-D6-MAX                                 
156800     END-IF                                                               
156900     PERFORM FAA-HITTA-WDD601                                             
157000                                                                          
157100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
157200                   NEXT-WDD601-FOUND                                      
157300                                                                          
157400        MOVE JA TO NEXT-WDD601-SW                                         
157500                                                                          
157600        IF SPAR-IDLEVNR NOT = SPACE                                       
157700           IF SPAR-IDLEVNR NOT = LPF-IDLEVNR                              
157800              MOVE NEJ TO NEXT-WDD601-SW                                  
157900           END-IF                                                         
158000        END-IF                                                            
158100                                                                          
158200        MOVE LPF-IDANSK TO W-IDANSK                                       
158300        IF W-IDANSK  >= MSGI-IDANSK-FOM AND                               
158400           W-IDANSK  <= MSGI-IDANSK-TOM                                   
158500           CONTINUE                                                       
158600        ELSE                                                              
158700           MOVE NEJ TO NEXT-WDD601-SW                                     
158800        END-IF                                                            
158900                                                                          
159000        IF MSGI-KDLPORS NOT = SPACE                                       
159100           MOVE LPF-KDLPORS(1) TO W-KDLPORS-1                             
159200           MOVE LPF-KDLPORS(2) TO W-KDLPORS-2                             
159300           MOVE LPF-KDLPORS(3) TO W-KDLPORS-3                             
159400           IF MSGI-KDLPORS = W-KDLPORS-1                                  
159500           OR MSGI-KDLPORS = W-KDLPORS-2                                  
159600           OR MSGI-KDLPORS = W-KDLPORS-3                                  
159700              CONTINUE                                                    
159800           ELSE                                                           
159900              MOVE NEJ TO NEXT-WDD601-SW                                  
160000           END-IF                                                         
160100        END-IF                                                            
160200                                                                          
160300        IF MSGI-KDLEVPLF NOT = SPACE                                      
160400           IF MSGI-KDLEVPLF NOT = LPF-KDLEVPLF                            
160500              MOVE NEJ TO NEXT-WDD601-SW                                  
160600           END-IF                                                         
160700        END-IF                                                            
160800                                                                          
160900        IF NEXT-WDD601-NOT-FOUND                                          
161000           PERFORM FAB-NASTA-WDD601                                       
161100        END-IF                                                            
161200     END-PERFORM                                                          
161300                                                                          
161400     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
161500        MOVE NEJ                       TO ALLT-SW                         
161600        MOVE 'NO MORE ITEMS FROM 2447' TO MOD-TEMFSFEL                    
161700        MOVE SPAR-IDDC-D6    TO MOD-IDDC-UT                               
161800        MOVE SPAR-IDLEVNR-D6 TO MOD-IDLEVNR-UT                            
161900        MOVE SPAR-IDARTNR-D6 TO MOD-IDARTNR-UT                            
162000        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
162100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
162200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
162300        PERFORM MFS-LAES-IN-IGEN                                          
162400     ELSE                                                                 
162500        MOVE LPF-IDDC     TO SPAR-IDDC-D6                                 
162600                             W-IDDC                                       
162700                             MOD-IDDC-UT                                  
162800                             MSGI-IDDC                                    
162900        MOVE LPF-IDLEVNR  TO SPAR-IDLEVNR-D6                              
163000                             W-IDLEVNR                                    
163100                             MOD-IDLEVNR-UT                               
163200                             MSGI-IDLEVNR                                 
163300        MOVE LPF-IDARTNR  TO SPAR-IDARTNR-D6                              
163400                             W-IDARTNR                                    
163500                             MOD-IDARTNR-UT                               
163600                             MSGI-IDARTNR                                 
163700        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
163800        MOVE LPF-IDANSK   TO SPAR-IDANSK-D6                               
163900        MOVE PROPOSAL-X   TO MOD-KDAVROP-UT                               
164000        MOVE PROPOSAL     TO W-KDAVROP                                    
164100     END-IF                                                               
164200     .                                                                    
164300                                                                          
164400                                                                          
164500                                                                          
164600 FAA-HITTA-WDD601 SECTION.                                                
164700     MOVE 'FAA-HITTA-WDD601' TO CURRENT-SECTION                           
164800                                                                          
164900     PERFORM IMS-GU-WDD601-DC-F                                           
165000     IF SEGMENT-FINNS                                                     
165100        MOVE LPF-IDDC       TO LPF-IDDC-D6                                
165200        MOVE LPF-IDARTNR    TO LPF-IDARTNR-D6                             
165300        MOVE LPF-IDLEVNR    TO LPF-IDLEVNR-D6                             
165400        MOVE LPF-IDANSK     TO LPF-IDANSK-D6                              
165500     END-IF                                                               
165600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
165700                   LPF-WDD6-KEY > SPAR-WDD6-KEY                           
165800        PERFORM IMS-GN-WDD601-DC                                          
165900        IF SEGMENT-FINNS                                                  
166000           MOVE LPF-IDDC    TO LPF-IDDC-D6                                
166100           MOVE LPF-IDARTNR TO LPF-IDARTNR-D6                             
166200           MOVE LPF-IDLEVNR TO LPF-IDLEVNR-D6                             
166300           MOVE LPF-IDANSK  TO LPF-IDANSK-D6                              
166400        END-IF                                                            
166500     END-PERFORM                                                          
166600                                                                          
166700     IF SEGMENT-FINNS                                                     
166800        MOVE NEJ             TO SW-WDK6-FINNS                             
166900        MOVE LPF-IDARTNR     TO W-IDARTNR                                 
167000        PERFORM IMS-GU-WDK601                                             
167100        IF SEGMENT-FINNS                                                  
167200           MOVE LPF-IDDC     TO W-IDDC                                    
167300           PERFORM IMS-GU-WDK711                                          
167400           IF SEGMENT-FINNS                                               
167500              MOVE JA        TO SW-WDK6-FINNS                             
167600           END-IF                                                         
167700        END-IF                                                            
167800     END-IF                                                               
167900                                                                          
168000     IF SW-WDK6-FINNS = NEJ                                               
168100        PERFORM FAB-NASTA-WDD601                                          
168200     END-IF                                                               
168300     .                                                                    
168400                                                                          
168500                                                                          
168600 FAB-NASTA-WDD601 SECTION.                                                
168700     MOVE 'FAB-NAST-WDD601 ' TO CURRENT-SECTION                           
168800                                                                          
168900* -- HELA SEKTIONEN FINNS BARA FÖR ATT UNDVIKA                            
169000* -- FEL PGA DÅLIG TESTMILJÖ.                                             
169100* -- BÅDE WDK601 OCH WDK711 SKALL FINNAS                                  
169200                                                                          
169300     MOVE NEJ                TO SW-WDK6-FINNS                             
169400     PERFORM IMS-GN-WDD601-DC                                             
169500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
169600                OR SW-WDK6-FINNS = JA                                     
169700                                                                          
169800        MOVE LPF-IDARTNR     TO W-IDARTNR                                 
169900        PERFORM IMS-GU-WDK601                                             
170000        IF SEGMENT-FINNS                                                  
170100           MOVE LPF-IDDC     TO W-IDDC                                    
170200           PERFORM IMS-GU-WDK711                                          
170300           IF SEGMENT-FINNS                                               
170400              MOVE JA        TO SW-WDK6-FINNS                             
170500           ELSE                                                           
170600              PERFORM IMS-GN-WDD601-DC                                    
170700           END-IF                                                         
170800        ELSE                                                              
170900           PERFORM IMS-GN-WDD601-DC                                       
171000        END-IF                                                            
171100     END-PERFORM                                                          
171200     .                                                                    
171300                                                                          
171400                                                                          
171500 FB-VISA-WDK6INFO     SECTION.                                            
171600     MOVE 'FB-VISA-WDK6INFO' TO CURRENT-SECTION                           
171700                                                                          
171800     PERFORM IMS-GU-WDK601                                                
171900     IF SEGMENT-FINNS                                                     
172000        MOVE ART-TIURPROD    TO MOD-TIURPROD                              
172100        MOVE ART-TIFINLV     TO MOD-TIAAVVD-DAPUBL                        
172200        MOVE '-'             TO MOD-STRECK-1                              
172300        MOVE ART-REKSIFFR    TO MOD-REKSIFFR                              
172400        MOVE 'GB'            TO W-IDSKYLT                                 
172500        PERFORM IMS-GU-WDD311                                             
172600        IF SEGMENT-FINNS                                                  
172700           MOVE TEXT-BEART   TO MOD-BEART-ENG                             
172800        ELSE                                                              
172900           MOVE 'UNKNOWN'    TO MOD-BEART-ENG                             
173000        END-IF                                                            
173100                                                                          
173200        MOVE SPACE           TO MOD-FLERSATT                              
173300                                                                          
173400        MOVE NEJ             TO SW-WDK611-FINNS                           
173500        PERFORM IMS-GNP-WDK611                                            
173600        IF  SEGMENT-FINNS                                                 
173700        AND CLAG-KDERS > ZERO                                             
173800            MOVE CLAG-KDERS            TO MOD-KDERS                       
173900            MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KDERS-ATTR                  
174000            IF CLAG-KDERS > 20                                            
174100               MOVE MED-IDMFSFEL       TO WS-IDMFSFEL                     
174200               MOVE INF-ARTIKEL-UTGANGEN                                  
174300                                       TO MED-IDMFSFEL                    
174400               CALL WMEDKONV USING MED-WMEDAREA                           
174500               MOVE MED-MFSFEL         TO MOD-TEMFSINF                    
174600               MOVE WS-IDMFSFEL        TO MED-IDMFSFEL                    
174700                                          MSG-KOM-IDMFSMED                
174800            END-IF                                                        
174900        ELSE                                                              
175000            MOVE MFS-RENSA-FAELT       TO MOD-KDERS                       
175100        END-IF                                                            
175200        IF SEGMENT-FINNS AND CLAG-KDUART  =  'P'                          
175300*--- PART PASSIVE                                                         
175400           MOVE KOM-PASSIV-ARTIKEL TO W-MESSAGE-BOTTOM-2                  
175500        END-IF                                                            
175600        IF SEGMENT-FINNS                                                  
175700           MOVE JA      TO SW-WDK611-FINNS                                
175800           IF CLAG-IDDC-REF = W-IDDC                                      
175900             MOVE NEJ   TO SW-WDK629-FINNS                                
176000             PERFORM IMS-GNP-WDK629                                       
176100             IF SEGMENT-FINNS                                             
176200               MOVE JA  TO SW-WDK629-FINNS                                
176300             END-IF                                                       
176400           END-IF                                                         
176500        END-IF                                                            
176600     ELSE                                                                 
176700        MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL MSG-KOM-IDMFSMED          
176800        MOVE NEJ                TO ALLT-SW                                
176900     END-IF                                                               
177000     .                                                                    
177100                                                                          
177200                                                                          
177300                                                                          
177400 FC-KOLLA-ERSATTNING SECTION.                                             
177500     MOVE 'FC-KOLLA-ERSATTN' TO CURRENT-SECTION                           
177600                                                                          
177700     IF ART-FLERS = JA                                                    
177800        MOVE NEJ             TO ERSATT-SW                                 
177900        MOVE ART-IDARTNR     TO W-IDARTNR-MIN7                            
178000                                W-IDARTNR-MAX7                            
178100        MOVE SPACE           TO MOD-IDARTNR-ERS                           
178200                                                                          
178300        PERFORM IMS-GU-WDD7A1-MINMAX                                      
178400        PERFORM UNTIL SEGMENT-SAKNAS                                      
178500          IF ERS-IDARTNR NOT = ZERO                                       
178600            MOVE ERS-IDARTNR    TO W-IDARTNR-OLD                          
178700            IF MOD-IDARTNR-ERS = SPACE                                    
178800               MOVE ERS-IDARTNR TO MOD-IDARTNR-ERS                        
178900               INSPECT MOD-IDARTNR-ERS                                    
179000                       REPLACING LEADING ZERO BY SPACE                    
179100            END-IF                                                        
179200            PERFORM IMS-GU-WDK601-OLD                                     
179300            IF SEGMENT-FINNS                                              
179400              PERFORM IMS-GNP-WDK611-OLD                                  
179500              IF SEGMENT-FINNS                                            
179600                 IF OLD-CLAG-KDERS = 22                                   
179700                 OR OLD-CLAG-KDERS = 23                                   
179800                    COMPUTE W-DATUM-AAVV = OLD-ART-TIERSDAT / 10          
179900                    IF W-DATUM-AAVV >= W-AAVV-MINUS-HALVAR                
180000                       MOVE JA       TO ERSATT-SW                         
180100                    END-IF                                                
180200                 END-IF                                                   
180300                 IF OLD-CLAG-KDERS = 02                                   
180400                 OR OLD-CLAG-KDERS = 03                                   
180500                    MOVE JA       TO ERSATT-SW                            
180600                 END-IF                                                   
180700              END-IF                                                      
180800            END-IF                                                        
180900          END-IF                                                          
181000          PERFORM IMS-GN-WDD7A1-MINMAX                                    
181100          IF  SEGMENT-FINNS                                               
181200          AND MOD-IDARTNR-ERS NOT = SPACE                                 
181300              MOVE 'VARIOUS' TO MOD-IDARTNR-ERS                           
181400          END-IF                                                          
181500        END-PERFORM                                                       
181600     ELSE                                                                 
181700        MOVE SPACE            TO MOD-IDARTNR-ERS                          
181800     END-IF                                                               
181900                                                                          
182000     IF ARTIKEL-ERSATT                                                    
182100        MOVE OLD-CLAG-KDERS     TO WS-MEDD-ERSKOD                         
182200        MOVE 'SUPERSESSION'     TO WS-MEDD-TEXT                           
182300        MOVE WS-MEDD-ERS        TO W-MESSAGE-BOTTOM-3                     
182400     END-IF                                                               
182500                                                                          
182600     PERFORM FCA-KOLLA-TILLKOMMANDE                                       
182700     .                                                                    
182800                                                                          
182900                                                                          
183000                                                                          
183100 FCA-KOLLA-TILLKOMMANDE SECTION.                                          
183200     MOVE 'FCA-KOLLA-TILLK '  TO CURRENT-SECTION                          
183300                                                                          
183400     PERFORM IMS-GU-WDD701                                                
183500     IF SEGMENT-FINNS                                                     
183600        PERFORM IMS-GNP-WDD702                                            
183700        IF SEGMENT-FINNS                                                  
183800           MOVE IDARTNR-TILLK TO MOD-IDARTNR-TILLK                        
183900           INSPECT MOD-IDARTNR-TILLK                                      
184000                   REPLACING LEADING ZERO BY SPACE                        
184100           PERFORM IMS-GNP-WDD702                                         
184200           IF SEGMENT-FINNS                                               
184300             MOVE 'VARIOUS'   TO MOD-IDARTNR-TILLK                        
184400           END-IF                                                         
184500        END-IF                                                            
184600     END-IF                                                               
184700     .                                                                    
184800                                                                          
184900                                                                          
185000 FD-VISA-WDK7INFO SECTION.                                                
185100     MOVE 'FD-VISA-WDK7INFO' TO CURRENT-SECTION                           
185200                                                                          
185300     PERFORM IMS-GU-WDK711                                                
185400     IF SEGMENT-SAKNAS                                                    
185500        MOVE NEJ                TO ALLT-SW                                
185600        MOVE ERR-ARTIKEL-SAKNAS TO MED-IDMFSFEL MSG-KOM-IDMFSMED          
185700        CALL WMEDKONV USING MED-WMEDAREA                                  
185800        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
185900        PERFORM MFS-RENSA-WDK7-FAELT-UT                                   
186000        PERFORM MFS-STAENG-FAELT-IN                                       
186100     ELSE                                                                 
186200        IF SLAG-IDDC-REF NOT = SPACE                                      
186300           MOVE NEJ             TO ALLT-SW                                
186400           MOVE ERR-REFILL-PART TO MED-IDMFSFEL MSG-KOM-IDMFSMED          
186500           CALL WMEDKONV USING MED-WMEDAREA                               
186600           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
186700           MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-SEASON                      
186800                                   MOD-KVPB-REF-SUM                       
186900                                                                          
187000           PERFORM MFS-STAENG-FAELT-IN                                    
187100        ELSE                                                              
187200           MOVE NOO             TO MOD-FLAGGA-SEASON                      
187300                                                                          
187400           IF  SLAG-RESEASON (1) = 1.00                                   
187500           AND SLAG-RESEASON (2) = 1.00                                   
187600           AND SLAG-RESEASON (3) = 1.00                                   
187700           AND SLAG-RESEASON (4) = 1.00                                   
187800           AND SLAG-RESEASON (5) = 1.00                                   
187900           AND SLAG-RESEASON (6) = 1.00                                   
188000           AND SLAG-RESEASON (7) = 1.00                                   
188100           AND SLAG-RESEASON (8) = 1.00                                   
188200           AND SLAG-RESEASON (9) = 1.00                                   
188300           AND SLAG-RESEASON (10) = 1.00                                  
188400           AND SLAG-RESEASON (11) = 1.00                                  
188500           AND SLAG-RESEASON (12) = 1.00                                  
188600                                                                          
188700             CONTINUE                                                     
188800           ELSE                                                           
188900             MOVE YES           TO MOD-FLAGGA-SEASON                      
189000           END-IF                                                         
189100                                                                          
189200           PERFORM FDA-BER-KVPB-REF-SUM                                   
189300           MOVE W-KVPB-REF-SUM  TO MOD-KVPB-REF-SUM                       
189400                                                                          
189500           IF  SLAG-KVUTRS > ZERO                                         
189600           AND W-MESSAGE-BOTTOM-3 = SPACE                                 
189700               MOVE KOM-UTR-SALDO TO W-MESSAGE-BOTTOM-3                   
189800           END-IF                                                         
189900        END-IF                                                            
190000                                                                          
190100        PERFORM FDB-KOLLA-LEVERANTOR                                      
190200        PERFORM FDC-VISA-WDK722-INFO                                      
190300        IF W-IDLEVNR NOT = SLAG-IDLEVNR                                   
190400           PERFORM FDD-VISA-WDK723-INFO                                   
190500        END-IF                                                            
190600                                                                          
190700        MOVE DCS-IDLANDX2            TO W-IDLAND                          
190800        PERFORM IMS-GU-WDK712                                             
190900        IF SEGMENT-FINNS                                                  
191000           IF LART-TIERSDAT-VIPS = ZERO                                   
191100              MOVE NOO               TO MOD-FLERSATT                      
191200           ELSE                                                           
191300              MOVE YES               TO MOD-FLERSATT                      
191400           END-IF                                                         
191500                                                                          
191600           MOVE 'AAMMDD'             TO DAT-KDDATFORM                     
191700           MOVE LART-DAPUBL(3:6)     TO DAT-I-TIDATUM                     
191800           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
191900                               DAT-O-TIDATUM DAT-KDSVAR                   
192000                                                                          
192100           IF DAT-KDSVAR-OK                                               
192200              MOVE DAT-TIAAVVD       TO MOD-TIAAVVD-DAPUBL                
192300           END-IF                                                         
192400        END-IF                                                            
192500     END-IF                                                               
192600     .                                                                    
192700                                                                          
192800                                                                          
192900 FDA-BER-KVPB-REF-SUM  SECTION.                                           
193000     MOVE 'FDA-KVPB-REF-SUM'  TO CURRENT-SECTION                          
193100*-------------------------------------------------                        
193200*-   VID REFILL TILL CDC SÅ SKALL KVPB-PLAN RÄKNAS                        
193300*-   MED I DET TOTAL PB-BEHOVET.GLOBAL EXP 2018                           
193400*-   WDK629 KVPB-PLAN CDC = KVPBREOI PÅ WDK711.                           
193500*-------------------------------------------------                        
193600                                                                          
193700     COMPUTE W-KVPB-REF-SUM = SLAG-KVPB-REF                               
193800                                                                          
193900     PERFORM IMS-GU-WDK701                                                
194000                                                                          
194100     MOVE SLAG-IDDC          TO W-IDDC-K7                                 
194200     PERFORM IMS-GNP-WDK711                                               
194300     PERFORM UNTIL SEGMENT-SAKNAS                                         
194400        ADD SLAG-KVPB-REF    TO W-KVPB-REF-SUM                            
194500        ADD SLAG-KVPBREOI    TO W-KVPB-REF-SUM                            
194600        PERFORM IMS-GNP-WDK711                                            
194700     END-PERFORM                                                          
194800                                                                          
194900*--  VI BEHÖVER FÅ TILLBAKA RÄTT SEGMENT I K711-AREAN                     
195000     PERFORM IMS-GU-WDK711                                                
195100                                                                          
195200     IF SW-WDK611-FINNS = JA                                              
195300       IF CLAG-IDDC-REF = W-IDDC                                          
195400         IF SW-WDK629-FINNS = JA                                          
195500           IF CLAG-DAPBPLAN >= W-DAGENS-DATUM-AAAAMMDD                    
195600              ADD CLAG-KVPB-PLAN   TO W-KVPB-REF-SUM                      
195700           ELSE                                                           
195800              ADD CREF-KVPB-PLAN   TO W-KVPB-REF-SUM                      
195900           END-IF                                                         
196000         END-IF                                                           
196100       END-IF                                                             
196200     END-IF                                                               
196300     .                                                                    
196400                                                                          
196500                                                                          
196600 FDB-KOLLA-LEVERANTOR SECTION.                                            
196700                                                                          
196800     MOVE 'FDB-KOLLA-LEV   ' TO CURRENT-SECTION                           
196900                                                                          
197000     IF MSGI-IDLEVNR = SPACE                                              
197100        MOVE SLAG-IDLEVNR TO W-IDLEVNR                                    
197200     ELSE                                                                 
197300        MOVE MSGI-IDLEVNR   TO W-IDLEVNR                                  
197400        PERFORM IMS-GU-WDF101                                             
197500        IF SEGMENT-SAKNAS                                                 
197600           MOVE ERR-SUPPLIER-MISSING                                      
197700                             TO MED-IDMFSFEL MSG-KOM-IDMFSMED             
197800           CALL WMEDKONV USING MED-WMEDAREA                               
197900           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
198000           MOVE NEJ          TO ALLT-SW                                   
198100        END-IF                                                            
198200     END-IF                                                               
198300     .                                                                    
198400                                                                          
198500                                                                          
198600 FDC-VISA-WDK722-INFO  SECTION.                                           
198700     MOVE 'FDC-VISA-WDK722 '  TO CURRENT-SECTION                          
198800                                                                          
198900     PERFORM IMS-GNP-WDK722                                               
199000     IF SEGMENT-FINNS                                                     
199100        IF W-IDLEVNR = SLAG-IDLEVNR                                       
199200           MOVE XLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP                     
199300        END-IF                                                            
199400        IF XLAG-FLJIT = 'J'                                               
199500           MOVE YES               TO MOD-FLJIT-IN                         
199600                                     SPAR-FLJIT                           
199700        ELSE                                                              
199800           MOVE XLAG-FLJIT        TO MOD-FLJIT-IN                         
199900                                     SPAR-FLJIT                           
200000        END-IF                                                            
200100        MOVE XLAG-KDLEVPLF        TO MOD-KDLEVPLF-IN                      
200200                                     SPAR-KDLEVPLF                        
200300        MOVE W-IDARTNR    TO W-IDARTNR-D9                                 
200400        MOVE W-IDDC       TO W-IDDC-D9                                    
200500        PERFORM IMS-GU-WDD902                                             
200600        IF SEGMENT-SAKNAS                                                 
200700           MOVE ZERO               TO W-D902-TILEVPL                      
200800           MOVE ZERO               TO W-XLAG-TIOMSPEC                     
200900        ELSE                                                              
201000           MOVE D902-TILEVPL       TO W-D902-TILEVPL                      
201100           MOVE XLAG-TIOMSPEC      TO W-XLAG-TIOMSPEC                     
201200* VI SÄTTER MOD-TIOMSPEC I FH-VISA-OMSPEC-INFO                            
201300* FÖR ATT FÅ BLANKT I FÄLTET OM NÅGOT GÅR FEL LÄNGRE FRAM                 
201400*          IF MSGI-KDAVROP = VALID-X                                      
201500*             MOVE W-D902-TILEVPL  TO MOD-TIOMSPEC                        
201600*          ELSE                                                           
201700*             MOVE W-XLAG-TIOMSPEC TO MOD-TIOMSPEC                        
201800*          END-IF                                                         
201900        END-IF                                                            
202000                                                                          
202100        IF NYCKLAR-OK                                                     
202200           MOVE XLAG-IDANSK        TO MOD-IDANSK                          
202300           MOVE XLAG-KDAVT         TO MOD-KDAVT                           
202400           MOVE XLAG-KVVECKOR-LT   TO MOD-KVVECKOR-LT                     
202500           MOVE XLAG-KDLPSP        TO MOD-KDLPSP                          
202600                                      SPAR-KDLPSP                         
202700           IF XLAG-TILPSP > ZERO                                          
202800              MOVE XLAG-TILPSP     TO WS-TILPSP-NUM                       
202900              MOVE WS-TILPSP       TO MOD-TILPSP                          
203000           ELSE                                                           
203100              MOVE SPACE           TO MOD-TILPSP                          
203200           END-IF                                                         
203300           IF XLAG-KVPB-TREND NOT = ZERO                                  
203400              IF W-MESSAGE-BOTTOM-1 = SPACE                               
203500                 MOVE KOM-TREND    TO W-MESSAGE-BOTTOM-1                  
203600              ELSE                                                        
203700                 IF W-MESSAGE-BOTTOM-2 = SPACE                            
203800                    MOVE KOM-TREND                                        
203900                                   TO W-MESSAGE-BOTTOM-2                  
204000                 ELSE                                                     
204100                    IF W-MESSAGE-BOTTOM-3 = SPACE                         
204200                       MOVE KOM-TREND                                     
204300                                   TO W-MESSAGE-BOTTOM-3                  
204400                    END-IF                                                
204500                 END-IF                                                   
204600              END-IF                                                      
204700           END-IF                                                         
204800                                                                          
204900           IF (XLAG-KVPB-TREND = ZERO) AND                                
205000              (XLAG-KVVECKOR-TREND = ZERO) AND                            
205100              (XLAG-TIDATUM-TREND  = ZERO)                                
205200                                                                          
205300             MOVE NOO             TO MOD-FLAGGA-TREND                     
205400           ELSE                                                           
205500             MOVE YES             TO MOD-FLAGGA-TREND                     
205600           END-IF                                                         
205700        END-IF                                                            
205800     ELSE                                                                 
205900        MOVE NEJ                  TO ALLT-SW                              
206000        MOVE SPACE                TO MOD-TEMFSINF                         
206100        MOVE ERR-EJ-LOKAL         TO MOD-TEMFSINF                         
206200                                                                          
206300        MOVE SPACE                TO XLAG-KDLEVPLF                        
206400        MOVE ZERO                 TO XLAG-TILEVDAG(1)                     
206500        MOVE ZERO                 TO XLAG-TILEVDAG(2)                     
206600        MOVE ZERO                 TO XLAG-TILEVDAG(3)                     
206700        MOVE ZERO                 TO XLAG-TILEVDAG(4)                     
206800        MOVE ZERO                 TO XLAG-TILEVDAG(5)                     
206900        MOVE SPACE                TO MOD-IDLEVNR-SHIP                     
207000        MOVE ZERO                 TO MOD-TIOMSPEC                         
207100        MOVE ZERO                 TO MOD-IDANSK                           
207200        MOVE ZERO                 TO MOD-KDAVT                            
207300        MOVE ZERO                 TO MOD-KVVECKOR-LT                      
207400        MOVE ZERO                 TO MOD-KDLPSP                           
207500        MOVE ZERO                 TO SPAR-KDLPSP                          
207600        MOVE SPACE                TO MOD-TILPSP                           
207700        MOVE SPACE                TO MOD-FLJIT-IN                         
207800        MOVE SPACE                TO MOD-FLAGGA-TREND                     
207900     END-IF                                                               
208000     .                                                                    
208100                                                                          
208200                                                                          
208300 FDD-VISA-WDK723-INFO SECTION.                                            
208400     MOVE 'FDD-VISA-WDK723 '   TO CURRENT-SECTION                         
208500                                                                          
208600     MOVE KOM-INTE-HUVUDLEV                                               
208700                               TO W-MESSAGE-BOTTOM-1                      
208800     PERFORM IMS-GNP-WDK723                                               
208900     PERFORM UNTIL SEGMENT-SAKNAS                                         
209000                OR SAVT-IDLEVNR-AVT = W-IDLEVNR                           
209100        PERFORM IMS-GNP-WDK723                                            
209200     END-PERFORM                                                          
209300     IF SEGMENT-FINNS                                                     
209400        MOVE SAVT-IDLEVNR-SHIP                                            
209500                               TO MOD-IDLEVNR-SHIP                        
209600     ELSE                                                                 
209700        MOVE W-IDLEVNR         TO MOD-IDLEVNR-SHIP                        
209800     END-IF                                                               
209900     .                                                                    
210000                                                                          
210100                                                                          
210200 FE-KOLLA-LEVERANSPLAN SECTION.                                           
210300                                                                          
210400     MOVE 'FE-KOLLA-LEVPLAN' TO CURRENT-SECTION                           
210500                                                                          
210600     IF SLAG-KVUTRS > ZERO                                                
210700        MOVE W-IDDC          TO W-IDDC-D6-MIN                             
210800                                W-IDDC-D6-MAX                             
210900        MOVE W-IDLEVNR       TO W-IDLEVNR-D6-MIN                          
211000                                W-IDLEVNR-D6-MAX                          
211100        MOVE W-IDARTNR       TO W-IDARTNR-D6-MIN                          
211200                                W-IDARTNR-D6-MAX                          
211300        PERFORM IMS-GU-WDD601                                             
211400        IF SEGMENT-FINNS                                                  
211500           MOVE LPF-TELPORSX TO MOD-TELPORSX                              
211600        ELSE                                                              
211700           MOVE MFS-RENSA-FAELT TO MOD-TELPORSX                           
211800        END-IF                                                            
211900     END-IF                                                               
212000     .                                                                    
212100                                                                          
212200                                                                          
212300                                                                          
212400 FF-KOLLA-ANTAL-LEVNR   SECTION.                                          
212500     MOVE 'FF-KOLLA-ANTAL-LEVNR ' TO CURRENT-SECTION                      
212600                                                                          
212700     MOVE W-IDARTNR          TO W-IDARTNR-D9                              
212800     MOVE W-IDDC             TO W-IDDC-D9                                 
212900     MOVE ZERO               TO W-IDLEVNR-COUNT                           
213000                                                                          
213100     PERFORM IMS-GU-WDD901                                                
213200     IF SEGMENT-FINNS                                                     
213300        PERFORM IMS-GNP-WDD902                                            
213400        PERFORM UNTIL SEGMENT-SAKNAS                                      
213500           ADD 1 TO W-IDLEVNR-COUNT                                       
213600           PERFORM IMS-GNP-WDD902                                         
213700        END-PERFORM                                                       
213800     END-IF                                                               
213900                                                                          
214000     IF W-IDLEVNR-COUNT > 1                                               
214100        IF W-MESSAGE-BOTTOM   = SPACE                                     
214200           MOVE KOM-FLER-LEV  TO W-MESSAGE-BOTTOM                         
214300        END-IF                                                            
214400     END-IF                                                               
214500     .                                                                    
214600                                                                          
214700                                                                          
214800                                                                          
214900 FG-INITIERA-TABELLER SECTION.                                            
215000     MOVE 'FG-INITIERA-TAB ' TO CURRENT-SECTION                           
215100                                                                          
215200     MOVE 1 TO AVROP-G-IX                                                 
215300     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
215400       IF NOT MFS-UPDATE                                                  
215500          MOVE NEJ    TO W-AVROP-GAM-AENDRAT(AVROP-G-IX)                  
215600       END-IF                                                             
215700       MOVE ZERO      TO W-KVAVROP-GAM      (AVROP-G-IX)                  
215800                         W-TIAVROP-AVS-GAM  (AVROP-G-IX)                  
215900                                                                          
216000       MOVE NEJ       TO W-AVROP-GAM-INL-PAST (AVROP-G-IX)                
216100                                                                          
216200       ADD 1 TO AVROP-G-IX                                                
216300     END-PERFORM                                                          
216400                                                                          
216500                                                                          
216600     MOVE W-START-PER-AAPP   TO WS-AAPP                                   
216700     MOVE 'AARP'             TO DAT-KDDATFORM                             
216800     MOVE 1                  TO PERIOD-IX                                 
216900     PERFORM UNTIL PERIOD-IX > PERIOD-IX-MAX                              
217000       IF WS-PP = 13                                                      
217100          MOVE 1             TO WS-PP                                     
217200          ADD  1             TO WS-AA                                     
217300       END-IF                                                             
217400       MOVE WS-AAPP          TO DAT-I-TIDATUM                             
217500       CALL WDATKONV  USING  DAT-KDDATFORM DAT-I-TIDATUM                  
217600                             DAT-O-TIDATUM DAT-KDSVAR                     
217700                                                                          
217800       IF DAT-KDSVAR-OK                                                   
217900          MOVE DAT-TIAARP     TO W-PERIOD-TAB-RAD(PERIOD-IX)              
218000          MOVE DAT-TIAAVV-GRP TO W-DATUM-AAVV                             
218100          MOVE 1              TO VECKA-IX                                 
218200          PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                           
218300             IF NOT MFS-UPDATE                                            
218400                MOVE NEJ     TO                                           
218500                  W-AVROP-TAB-AENDRAT(PERIOD-IX, VECKA-IX)                
218600             END-IF                                                       
218700             IF VECKA-IX > DAT-KVVIPER                                    
218800                MOVE ZERO    TO W-VECKA-TAB(PERIOD-IX, VECKA-IX)          
218900             ELSE                                                         
219000                MOVE W-DATUM-AAVV                                         
219100                             TO W-VECKA-TAB(PERIOD-IX, VECKA-IX)          
219200                ADD 1        TO W-DATUM-AAVV                              
219300             END-IF                                                       
219400             MOVE ZERO       TO W-KVAVROP-TAB(PERIOD-IX, VECKA-IX)        
219500             MOVE NEJ TO                                                  
219600                      W-AVROP-TAB-INL-PAST(PERIOD-IX, VECKA-IX)           
219700             ADD 1 TO VECKA-IX                                            
219800          END-PERFORM                                                     
219900       ELSE                                                               
220000          MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                    
220100          CALL FELLOG                                                     
220200       END-IF                                                             
220300       ADD +1   TO WS-PP                                                  
220400       ADD +1   TO PERIOD-IX                                              
220500     END-PERFORM                                                          
220600     .                                                                    
220700                                                                          
220800                                                                          
220900                                                                          
221000 FH-VISA-OMSPEC-INFO SECTION.                                             
221100     MOVE 'FH-VISA-OMSPEC  '  TO CURRENT-SECTION                          
221200                                                                          
221300     MOVE W-IDARTNR           TO W-IDARTNR-D9                             
221400     MOVE W-IDDC              TO W-IDDC-D9                                
221500                                                                          
221600     PERFORM IMS-GU-WDD904                                                
221700                                                                          
221800     IF SEGMENT-SAKNAS                                                    
221900        MOVE ZERO             TO D904-DASPECST                            
222000                                 D904-KVBEST-PL                           
222100     ELSE                                                                 
222200        PERFORM FHA-VISA-ORSAK                                            
222300     END-IF                                                               
222400                                                                          
222500     IF MID-KDAVROP-IN = ALL '+' OR SPACE OR PROPOSAL-X                   
222600        IF     SEGMENT-FINNS                                              
222700        AND D904-DASPECST > ZERO                                          
222800            IF MFS-UPDATE AND W-KDAVROP = VALID                           
222900               CONTINUE                                                   
223000            ELSE                                                          
223100               MOVE PROPOSAL        TO W-KDAVROP                          
223200               MOVE PROPOSAL-X      TO MSGI-KDAVROP                       
223300                                       MOD-KDAVROP-UT                     
223400               MOVE 'PROPOSAL'      TO MOD-TEXT-PLANTYP                   
223500            END-IF                                                        
223600        ELSE                                                              
223700           IF MID-KDAVROP-IN = PROPOSAL-X AND NOT MFS-NEXT                
223800              MOVE KOM-FORSLAG-SAKNAS                                     
223900                              TO MOD-TEMFSINF                             
224000              PERFORM MFS-STAENG-FAELT-IN                                 
224100              MOVE ZERO       TO W-XLAG-TIOMSPEC                          
224200           ELSE                                                           
224300              MOVE VALID          TO W-KDAVROP                            
224400              MOVE VALID-X        TO MSGI-KDAVROP                         
224500                                    MOD-KDAVROP-UT                        
224600              MOVE '   VALID' TO MOD-TEXT-PLANTYP                         
224700           END-IF                                                         
224800           MOVE ZERO              TO D904-DASPECST                        
224900                                     D904-KVBEST-PL                       
225000        END-IF                                                            
225100     ELSE                                                                 
225200        MOVE VALID                TO W-KDAVROP                            
225300        MOVE VALID-X              TO MSGI-KDAVROP                         
225400                                     MOD-KDAVROP-UT                       
225500        MOVE '   VALID'           TO MOD-TEXT-PLANTYP                     
225600     END-IF                                                               
225700                                                                          
225800     IF W-KDAVROP = PROPOSAL                                              
225900        MOVE W-XLAG-TIOMSPEC      TO MOD-TIOMSPEC                         
226000     ELSE                                                                 
226100        MOVE W-D902-TILEVPL       TO MOD-TIOMSPEC                         
226200     END-IF                                                               
226300     .                                                                    
226400                                                                          
226500                                                                          
226600                                                                          
226700 FHA-VISA-ORSAK   SECTION.                                                
226800                                                                          
226900     MOVE 'FHA-VISA-ORSAK  '  TO CURRENT-SECTION                          
227000                                                                          
227100     IF  D904-KDLPORS-TAB (1) > ZERO                                      
227200     AND D904-KDLPORS-TAB (1) NOT > W006IX-MAX                            
227300        MOVE D904-KDLPORS-TAB (1)     TO ORSAK-IX                         
227400        MOVE TELPORS (ORSAK-IX)       TO MOD-TEXT-ORSAK1                  
227500     ELSE                                                                 
227600        MOVE MFS-RENSA-FAELT          TO MOD-TEXT-ORSAK1                  
227700     END-IF                                                               
227800                                                                          
227900     IF  D904-KDLPORS-TAB (2) > ZERO                                      
228000     AND D904-KDLPORS-TAB (2) NOT > W006IX-MAX                            
228100        MOVE D904-KDLPORS-TAB (2)     TO ORSAK-IX                         
228200        MOVE TELPORS (ORSAK-IX)       TO MOD-TEXT-ORSAK2                  
228300     ELSE                                                                 
228400        MOVE MFS-RENSA-FAELT          TO MOD-TEXT-ORSAK2                  
228500     END-IF                                                               
228600     .                                                                    
228700                                                                          
228800                                                                          
228900                                                                          
229000                                                                          
229100 FI-VISA-AVROP-INFO SECTION.                                              
229200     MOVE 'FI-VISA-AVROP   ' TO CURRENT-SECTION                           
229300                                                                          
229400     MOVE NEJ                    TO GAMMAL-TAB-SW                         
229500                                                                          
229600     PERFORM IMS-GU-WDD902                                                
229700     IF SEGMENT-FINNS                                                     
229800        PERFORM IMS-GNP-WDD905                                            
229900        PERFORM UNTIL SEGMENT-SAKNAS                                      
230000                                                                          
230100           IF  D905-DAAVROP-AVS < D904-DASPECST                           
230200           AND D905-KDAVROP = 2                                           
230300                                                                          
230400           OR  D905-DAAVROP-AVS NOT < D904-DASPECST                       
230500           AND W-KDAVROP = PROPOSAL                                       
230600           AND D905-KDAVROP = 1                                           
230700                                                                          
230800           OR  D905-DAAVROP-AVS NOT < D904-DASPECST                       
230900           AND W-KDAVROP = VALID                                          
231000           AND D905-KDAVROP = 2                                           
231100                                                                          
231200              MOVE NEJ     TO AVROP-INL-PASSERAT-SW                       
231300              PERFORM FIC-KOLLA-AVROP-INL-PASSERAT                        
231400                                                                          
231500              IF D905-DAAVROP-AVS < W-DATUM-AKTUELLT-AAAAVV               
231600                 PERFORM FIA-FLYTTA-TILL-GAMMAL-TABELL                    
231700              ELSE                                                        
231800                 PERFORM FIB-FLYTTA-TILL-NY-TABELL                        
231900              END-IF                                                      
232000                                                                          
232100           END-IF                                                         
232200           PERFORM IMS-GNP-WDD905                                         
232300        END-PERFORM                                                       
232400     END-IF                                                               
232500     .                                                                    
232600                                                                          
232700                                                                          
232800 FIA-FLYTTA-TILL-GAMMAL-TABELL SECTION.                                   
232900                                                                          
233000     MOVE 'FIA-TILL-GAMMAL '  TO CURRENT-SECTION                          
233100                                                                          
233200     MOVE JA                  TO GAMMAL-TAB-SW                            
233300     MOVE 1                   TO AVROP-G-IX                               
233400     MOVE D905-DAAVROP-AVS    TO W-DAAVROP-AVS                            
233500     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
233600        IF W-TIAVROP-AVS-GAM(AVROP-G-IX) = W-TIAVROP-AVS                  
233700        OR W-TIAVROP-AVS-GAM(AVROP-G-IX) = ZERO                           
233800           ADD D905-KVAVROP   TO W-KVAVROP-GAM(AVROP-G-IX)                
233900           IF W-TIAVROP-AVS-GAM(AVROP-G-IX) = ZERO                        
234000              MOVE W-DAAVROP-AVS                                          
234100                              TO W-TIAVROP-AVS-GAM(AVROP-G-IX)            
234200           END-IF                                                         
234300                                                                          
234400           IF AVROP-INL-PASSERAT-SW = JA                                  
234500             MOVE JA  TO W-AVROP-GAM-INL-PAST (AVROP-G-IX)                
234600           END-IF                                                         
234700                                                                          
234800           MOVE 6             TO AVROP-G-IX                               
234900        END-IF                                                            
235000        ADD 1                 TO AVROP-G-IX                               
235100     END-PERFORM                                                          
235200                                                                          
235300                                                                          
235400     IF AVROP-G-IX = 6                                                    
235500*    LÄSTA VECKAN LIGGER UTANFÖR TABELLEN SOM BEHÖVER JUSTERAS            
235600*    AVROP-G-IX = 7 OM VECKAN ÄR INLAGD I TABELLEN                        
235700                                                                          
235800        ADD W-KVAVROP-GAM(2) TO W-KVAVROP-GAM(1)                          
235900        MOVE 9999               TO W-TIAVROP-AVS-GAM(1)                   
236000        MOVE 2                  TO AVROP-G-IX                             
236100        MOVE 3                  TO IX-PLUS-1                              
236200        PERFORM UNTIL AVROP-G-IX = AVROP-G-IX-MAX                         
236300           MOVE W-KVAVROP-GAM(IX-PLUS-1)                                  
236400                             TO W-KVAVROP-GAM(AVROP-G-IX)                 
236500           MOVE W-TIAVROP-AVS-GAM(IX-PLUS-1)                              
236600                             TO W-TIAVROP-AVS-GAM(AVROP-G-IX)             
236700                                                                          
236800           IF W-AVROP-GAM-INL-PAST(IX-PLUS-1)= JA                         
236900              MOVE W-AVROP-GAM-INL-PAST(IX-PLUS-1)                        
237000                             TO W-AVROP-GAM-INL-PAST(AVROP-G-IX)          
237100           ELSE                                                           
237200              MOVE NEJ       TO W-AVROP-GAM-INL-PAST(AVROP-G-IX)          
237300           END-IF                                                         
237400                                                                          
237500           ADD 1                TO AVROP-G-IX                             
237600           ADD 1                TO IX-PLUS-1                              
237700        END-PERFORM                                                       
237800                                                                          
237900        MOVE D905-KVAVROP     TO W-KVAVROP-GAM(AVROP-G-IX)                
238000        MOVE D905-DAAVROP-AVS TO W-DAAVROP-AVS                            
238100        MOVE W-TIAVROP-AVS    TO W-TIAVROP-AVS-GAM(AVROP-G-IX)            
238200                                                                          
238300        IF AVROP-INL-PASSERAT-SW = JA                                     
238400           MOVE JA  TO W-AVROP-GAM-INL-PAST (AVROP-G-IX)                  
238500        ELSE                                                              
238600           MOVE NEJ TO W-AVROP-GAM-INL-PAST (AVROP-G-IX)                  
238700        END-IF                                                            
238800     END-IF                                                               
238900     .                                                                    
239000                                                                          
239100                                                                          
239200 FIB-FLYTTA-TILL-NY-TABELL SECTION.                                       
239300                                                                          
239400     MOVE 'FIB-TILL-NY     ' TO CURRENT-SECTION                           
239500                                                                          
239600     MOVE D905-DAAVROP-AVS   TO W-DAAVROP-AVS                             
239700     MOVE W-TIAVROP-AVS      TO W-DATUM-AAVV                              
239800                                                                          
239900     MOVE 1 TO PERIOD-IX                                                  
240000     PERFORM UNTIL PERIOD-IX > PERIOD-IX-MAX                              
240100        MOVE 1 TO VECKA-IX                                                
240200        PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                             
240300           IF W-DATUM-AAVV = W-VECKA-TAB (PERIOD-IX, VECKA-IX)            
240400              ADD D905-KVAVROP                                            
240500                             TO W-KVAVROP-TAB(PERIOD-IX, VECKA-IX)        
240600                                                                          
240700              IF AVROP-INL-PASSERAT-SW = JA                               
240800                MOVE JA  TO                                               
240900                       W-AVROP-TAB-INL-PAST(PERIOD-IX, VECKA-IX)          
241000              END-IF                                                      
241100                                                                          
241200              MOVE PERIOD-IX-MAX                                          
241300                             TO PERIOD-IX                                 
241400              MOVE VECKA-IX-MAX                                           
241500                             TO VECKA-IX                                  
241600           END-IF                                                         
241700           ADD 1 TO VECKA-IX                                              
241800        END-PERFORM                                                       
241900        ADD 1 TO PERIOD-IX                                                
242000     END-PERFORM                                                          
242100     .                                                                    
242200                                                                          
242300                                                                          
242400 FIC-KOLLA-AVROP-INL-PASSERAT SECTION.                                    
242500                                                                          
242600     MOVE 'FIC-KOLLA-AVROP-INL-PASSERAT' TO CURRENT-SECTION               
242700                                                                          
242800                                                                          
242900     MOVE D905-TIAVRDAT-INL  TO WS-TIAVRDAT-INL                           
243000                                                                          
243100     MOVE WS-TIAVRDAT-INL   TO DAYS-TIDATE1                               
243200     MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                             
243300     MOVE 'YYYYWWD'         TO DAYS-KDDATFMT2                             
243400     MOVE 0                 TO DAYS-KVDAYS                                
243500     MOVE SPACE             TO DAYS-TIDATE2                               
243600                               DAYS-IDCALEND                              
243700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
243800                                                                          
243900     IF DAYS-KDRC = 8                                                     
244000       MOVE 'FEL VID ANROP TILL WZ20DAYS ' TO FELTEXT                     
244100                                                                          
244200       CALL FELLOG                                                        
244300     ELSE                                                                 
244400       MOVE DAYS-TIDATE2(1:7)  TO WS-TIAVRDAT-INL-AAAAVVD                 
244500     END-IF                                                               
244600                                                                          
244700     IF WS-TIAVRDAT-INL-AAAAVVD < W-DATUM-AKTUELLT-AAAAVVD                
244800       MOVE JA  TO AVROP-INL-PASSERAT-SW                                  
244900     END-IF                                                               
245000                                                                          
245100     .                                                                    
245200                                                                          
245300                                                                          
245400 FJ-GAMLA-AVROP-TILL-MOD SECTION.                                         
245500                                                                          
245600     MOVE 'FJ-GAMLA-AVROP  ' TO CURRENT-SECTION                           
245700                                                                          
245800     MOVE 1    TO AVROP-G-IX                                              
245900                                                                          
246000     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
246100        IF W-KVAVROP-GAM (AVROP-G-IX) > ZERO                              
246200           MOVE W-KVAVROP-GAM (AVROP-G-IX)                                
246300                               TO MOD-KVAVROP-GAM (AVROP-G-IX)            
246400           IF W-TIAVROP-AVS-GAM (AVROP-G-IX) > ZERO                       
246500              MOVE W-TIAVROP-AVS-GAM (AVROP-G-IX)                         
246600                               TO MOD-TIAVROP-AVS-GAM(AVROP-G-IX)         
246700           ELSE                                                           
246800              MOVE SPACE       TO MOD-TIAVROP-AVS-GAM(AVROP-G-IX)         
246900           END-IF                                                         
247000           MOVE '-'            TO MOD-SKILJETECKEN-GAM(AVROP-G-IX)        
247100           IF W-AVROP-GAM-AENDRAT (AVROP-G-IX) = JA                       
247200              MOVE MFS-ADD-LYS-UPP-FAELT                                  
247300                               TO MOD-AVROP-ATTR (AVROP-G-IX)             
247400              MOVE NEJ         TO W-AVROP-GAM-AENDRAT (AVROP-G-IX)        
247500           ELSE                                                           
247600              IF W-AVROP-GAM-INL-PAST (AVROP-G-IX) = JA                   
247700                MOVE MFS-ADD-LYS-UPP-FAELT                                
247800                               TO MOD-AVROP-ATTR (AVROP-G-IX)             
247900              END-IF                                                      
248000           END-IF                                                         
248100        ELSE                                                              
248200           MOVE MFS-RENSA-FAELT TO MOD-KVAVROP-GAM (AVROP-G-IX)           
248300                               MOD-SKILJETECKEN-GAM(AVROP-G-IX)           
248400                               MOD-TIAVROP-AVS-GAM(AVROP-G-IX)            
248500        END-IF                                                            
248600        ADD 1 TO AVROP-G-IX                                               
248700     END-PERFORM                                                          
248800     .                                                                    
248900                                                                          
249000                                                                          
249100                                                                          
249200 FK-NYA-AVROP-TILL-MOD SECTION.                                           
249300                                                                          
249400     MOVE 'FK-NYA-AVROP    ' TO CURRENT-SECTION                           
249500                                                                          
249600                                                                          
249700     MOVE 1 TO PERIOD-IX                                                  
249800     PERFORM UNTIL PERIOD-IX > PERIOD-IX-MAX                              
249900        MOVE W-PERIOD-TAB-RAD(PERIOD-IX)                                  
250000                                 TO MOD-PERIOD-AAPP(PERIOD-IX)            
250100        MOVE ')'                 TO MOD-PERIOD-PARENTES(PERIOD-IX)        
250200                                                                          
250300        MOVE 1 TO VECKA-IX                                                
250400        PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                             
250500                                                                          
250600           IF W-VECKA-TAB (PERIOD-IX, VECKA-IX) > ZERO                    
250700                                                                          
250800              IF W-KVAVROP-TAB (PERIOD-IX, VECKA-IX) > ZERO               
250900                 MOVE W-KVAVROP-TAB (PERIOD-IX, VECKA-IX)                 
251000                 TO   MOD-KVAVROP-TAB    (PERIOD-IX, VECKA-IX)            
251100                                                                          
251200                 MOVE W-VECKA-TAB   (PERIOD-IX, VECKA-IX)                 
251300                 TO   MOD-TIAVROP-AVS-TAB(PERIOD-IX, VECKA-IX)            
251400                                                                          
251500                 MOVE '-'           TO                                    
251600                      MOD-IDTECKEN-TAB   (PERIOD-IX, VECKA-IX)            
251700                 IF W-AVROP-TAB-AENDRAT (PERIOD-IX, VECKA-IX) = JA        
251800                    MOVE MFS-ADD-LYS-UPP-FAELT TO                         
251900                       MOD-AVROP-TAB-ATTR (PERIOD-IX, VECKA-IX)           
252000                    MOVE NEJ TO                                           
252100                       W-AVROP-TAB-AENDRAT (PERIOD-IX, VECKA-IX)          
252200                 ELSE                                                     
252300                    IF W-AVROP-TAB-INL-PAST(PERIOD-IX, VECKA-IX)          
252400                                                     = JA                 
252500                       MOVE MFS-ADD-LYS-UPP-FAELT TO                      
252600                       MOD-AVROP-TAB-ATTR (PERIOD-IX, VECKA-IX)           
252700                    END-IF                                                
252800                 END-IF                                                   
252900                                                                          
253000              ELSE                                                        
253100                 MOVE ZERO                                                
253200                 TO   MOD-KVAVROP-TAB    (PERIOD-IX, VECKA-IX)            
253300                                                                          
253400                 MOVE ZERO                                                
253500                 TO   MOD-TIAVROP-AVS-TAB(PERIOD-IX, VECKA-IX)            
253600                                                                          
253700                 MOVE SPACE         TO                                    
253800                      MOD-IDTECKEN-TAB   (PERIOD-IX, VECKA-IX)            
253900              END-IF                                                      
254000              IF W-VECKA-TAB (PERIOD-IX, VECKA-IX) <                      
254100                 W-DATUM-AKTUELLT                                         
254200                 MOVE ' '        TO                                       
254300                 MOD-IDTECKEN-PARENTES-TAB (PERIOD-IX, VECKA-IX)          
254400              ELSE                                                        
254500                 MOVE ')'        TO                                       
254600                 MOD-IDTECKEN-PARENTES-TAB (PERIOD-IX, VECKA-IX)          
254700              END-IF                                                      
254800           END-IF                                                         
254900           ADD 1 TO VECKA-IX                                              
255000        END-PERFORM                                                       
255100        ADD 1 TO PERIOD-IX                                                
255200     END-PERFORM                                                          
255300     .                                                                    
255400                                                                          
255500                                                                          
255600                                                                          
255700 FL-VISA-TEREFMED   SECTION.                                              
255800                                                                          
255900     MOVE 'FL-VISA-TEREFMED' TO CURRENT-SECTION                           
256000     MOVE W-IDDC                  TO W-IDDC-2261                          
256100     PERFORM IMS-GHU-WDGX2262                                             
256200     IF SEGMENT-FINNS                                                     
256300        MOVE 2262-TEREFMED(01:36) TO MOD-TEREFMED1                        
256400                                   SPAR-TEREFMED1                         
256500        MOVE 2262-TEREFMED(37:36) TO MOD-TEREFMED2                        
256600                                   SPAR-TEREFMED2                         
256700     ELSE                                                                 
256800        MOVE MFS-RENSA-FAELT    TO MOD-TEREFMED1                          
256900                                   MOD-TEREFMED2                          
257000        MOVE SPACE              TO SPAR-TEREFMED1                         
257100                                   SPAR-TEREFMED2                         
257200     END-IF                                                               
257300     MOVE MFS-ADD-LAES-IN-FAELT                                           
257400                             TO MOD-TEREFMED1-ATTR                        
257500                                MOD-TEREFMED2-ATTR                        
257600     .                                                                    
257700                                                                          
257800 FM-KOLLA-RENSA-CURRENT-PLAN SECTION.                                     
257900                                                                          
258000     MOVE 'FM-KOLLA-RENSA-CURRENT-PLAN' TO CURRENT-SECTION                
258100                                                                          
258200     IF SPAR-IDTRANS-2447 = '2447'                                        
258300        MOVE SPAR-IDDC-D6-2447  TO W-IDDC-D6-MIN                          
258400                                     W-IDDC-D6-MAX                        
258500        MOVE SPAR-IDLEVNR-D6-2447 TO W-IDLEVNR-D6-MIN                     
258600                                     W-IDLEVNR-D6-MAX                     
258700        MOVE SPAR-IDARTNR-D6-2447 TO W-IDARTNR-D6-MIN                     
258800                                     W-IDARTNR-D6-MAX                     
258900        MOVE SPAR-IDANSK-D6-2447  TO W-IDANSK-D6-MIN                      
259000                                     W-IDANSK-D6-MAX                      
259100        PERFORM IMS-GHU-WDD601                                            
259200                                                                          
259300        IF SEGMENT-FINNS                                                  
259400           IF LPF-KDLPORS(2) = 26                                         
259500           AND LPF-KDLPORS(3) = 27                                        
259600               MOVE ZERO          TO LPF-KDLPORS(2)                       
259700                                     LPF-KDLPORS(3)                       
259800               PERFORM IMS-DLET-WDD601                                    
259900               MOVE VALID-X       TO MOD-KDAVROP-UT                       
260000               MOVE VALID         TO W-KDAVROP                            
260100           END-IF                                                         
260200        END-IF                                                            
260300     END-IF                                                               
260400     .                                                                    
260500                                                                          
260600                                                                          
260700                                                                          
260800                                                                          
260900 G-KOLLA-INPUT    SECTION.                                                
261000                                                                          
261100     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
261200                                                                          
261300     PERFORM GA-CHECK-DC-FTG-USER                                         
261400                                                                          
261500     IF INDATA-OK                                                         
261600        PERFORM S100-KOLLA-OM-INPUT                                       
261700        IF INPUT-SAKNAS                                                   
261800          MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                      
261900          MOVE NEJ                   TO INDATA-SW                         
262000        ELSE                                                              
262100                                                                          
262200           PERFORM IMS-GU-WDK711                                          
262300           IF SEGMENT-FINNS                                               
262400              PERFORM IMS-GNP-WDK722                                      
262500           END-IF                                                         
262600           IF SEGMENT-SAKNAS                                              
262700              MOVE ERR-ARTIKEL-SAKNAS    TO MED-IDMFSFEL                  
262800              MOVE NEJ                   TO INDATA-SW                     
262900           ELSE                                                           
263000              MOVE JA TO INDATA-SW                                        
263100              MOVE MSGI-IDARTNR      TO W-IDARTNR-D9                      
263200                                           W-IDARTNR                      
263300              MOVE MSGI-IDDC-KEY     TO W-IDDC-D9                         
263400                                           W-IDDC                         
263500              MOVE MSGI-IDLEVNR      TO W-IDLEVNR                         
263600                                                                          
263700              PERFORM IMS-GHU-WDD904                                      
263800              IF SEGMENT-FINNS                                            
263900                 MOVE D904-KDLPORS-TAB (1) TO W-ORSAK-TAB-KOD (1)         
264000                 MOVE D904-KDLPORS-TAB (2) TO W-ORSAK-TAB-KOD (2)         
264100                 MOVE D904-KDLPORS-TAB (3) TO W-ORSAK-TAB-KOD (3)         
264200              ELSE                                                        
264300                 MOVE ZERO              TO W-ORSAK-TAB-KOD (1)            
264400                 MOVE ZERO              TO W-ORSAK-TAB-KOD (2)            
264500                 MOVE ZERO              TO W-ORSAK-TAB-KOD (3)            
264600                                                                          
264700                 MOVE ZERO              TO D904-DASPECST                  
264800                                              D904-KVBEST-PL              
264900              END-IF                                                      
265000                                                                          
265100              PERFORM GB-KOLLA-KDKOM                                      
265200              IF MFS-UPDATE                                               
265300                 PERFORM GC-KOLLA-KDOMSPEC                                
265400                 PERFORM GD-KOLLA-AVROP                                   
265500                 PERFORM GE-KOLLA-KDLEVPLF                                
265600                 PERFORM GF-KOLLA-FLJIT                                   
265700                 PERFORM GG-KOLLA-TILPSP                                  
265800                 PERFORM GH-KOLLA-TEREFMED                                
265900                                                                          
266000                 IF MID-KDKOM NOT = SPACE                                 
266100                 OR MID-KDOMSPEC NOT = SPACE                              
266200                    PERFORM GX-ORSAKTEXT-TILL-MOD                         
266300                 END-IF                                                   
266400              END-IF                                                      
266500           END-IF                                                         
266600        END-IF                                                            
266700     END-IF                                                               
266800                                                                          
266900     IF INDATA-FEL                                                        
267000       CALL WMEDKONV USING MED-WMEDAREA                                   
267100       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
267200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
267300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
267400     END-IF                                                               
267500     .                                                                    
267600                                                                          
267700                                                                          
267800 GA-CHECK-DC-FTG-USER SECTION.                                            
267900     MOVE 'GA-CHECK-DC-FTG-USER ' TO CURRENT-SECTION                      
268000                                                                          
268100     PERFORM IMS-GU-WDB601                                                
268200     IF SEGMENT-FINNS                                                     
268300        IF  DCS-NDC-CN                                                    
268400        OR (DCS-NDC-NA AND DCS-USA)                                       
268500           MOVE MSGI-IDFTG          TO WS-IDFTG                           
268600           IF (DCS-NDC-CN AND IDFTG-CN)                                   
268700           OR (DCS-NDC-NA AND IDFTG-US)                                   
268800           OR MSGI-IDFTG  = WC-IDFTG-PV                                   
268900              CONTINUE                                                    
269000           ELSE                                                           
269100              MOVE NEJ              TO INDATA-SW                          
269200              MOVE ERR-NOT-AUTH     TO MED-IDMFSFEL                       
269300           END-IF                                                         
269400        ELSE                                                              
269500           MOVE NEJ                 TO INDATA-SW                          
269600           MOVE ERR-NOT-AUTH        TO MED-IDMFSFEL                       
269700        END-IF                                                            
269800     ELSE                                                                 
269900        MOVE NEJ                    TO INDATA-SW                          
270000        MOVE ERR-NOT-AUTH           TO MED-IDMFSFEL                       
270100     END-IF                                                               
270200                                                                          
270300     .                                                                    
270400     EJECT                                                                
270500                                                                          
270600 GB-KOLLA-KDKOM   SECTION.                                                
270700                                                                          
270800     MOVE 'GB-KOLLA-KDKOM  '  TO CURRENT-SECTION                          
270900                                                                          
271000     IF MID-KDKOM = ALL '+' OR SPACE                                      
271100        MOVE SPACE            TO MID-KDKOM                                
271200     ELSE                                                                 
271300                                                                          
271400        IF MID-KDKOM = '1' OR '2' OR '5'                                  
271500           MOVE MFS-ALFA-FAELT-RAETT                                      
271600                                 TO MOD-KDKOM-IN-ATTR                     
271700                                                                          
271800           IF MID-KDKOM = ('1' OR '2')                                    
271900           AND W-KDAVROP = VALID                                          
272000               MOVE NEJ          TO INDATA-SW                             
272100               MOVE ERR-FEL-ATG  TO MED-IDMFSFEL MSG-KOM-IDMFSMED         
272200               MOVE MFS-ALFA-FAELT-FEL                                    
272300                                 TO MOD-KDKOM-IN-ATTR                     
272400           END-IF                                                         
272500           IF  MID-KDKOM = '5'                                            
272600           AND W-KDAVROP = PROPOSAL                                       
272700               MOVE NEJ          TO INDATA-SW                             
272800               MOVE ERR-FEL-ATG  TO MED-IDMFSFEL MSG-KOM-IDMFSMED         
272900               MOVE MFS-ALFA-FAELT-FEL                                    
273000                                 TO MOD-KDKOM-IN-ATTR                     
273100           END-IF                                                         
273200        ELSE                                                              
273300           MOVE NEJ           TO INDATA-SW                                
273400           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
273500                      TO MED-IDMFSFEL MSG-KOM-IDMFSMED                    
273600           MOVE MFS-ALFA-FAELT-FEL                                        
273700                      TO MOD-KDKOM-IN-ATTR                                
273800        END-IF                                                            
273900     END-IF                                                               
274000     .                                                                    
274100                                                                          
274200                                                                          
274300                                                                          
274400 GC-KOLLA-KDOMSPEC  SECTION.                                              
274500                                                                          
274600     MOVE 'GC-KOLLA-KDOMSPE'  TO CURRENT-SECTION                          
274700                                                                          
274800     MOVE NEJ                 TO OMSPEC-SW                                
274900                                                                          
275000     IF MID-KDOMSPEC = ALL '+' OR SPACE                                   
275100        IF MID-KDOMSPEC = ALL '+'                                         
275200           MOVE NEJ           TO MID-KDOMSPEC                             
275300        END-IF                                                            
275400     ELSE                                                                 
275500        IF MID-KDOMSPEC = YES OR NOO                                      
275600           IF MID-KDOMSPEC = YES                                          
275700              PERFORM IMS-GU-WDD902                                       
275800              IF SEGMENT-SAKNAS                                           
275900                 PERFORM IMS-GU-WDD901                                    
276000                 IF SEGMENT-SAKNAS                                        
276100                    MOVE W-IDARTNR-D9  TO D901-IDARTNR                    
276200                    MOVE W-IDDC-D9     TO D901-IDDC                       
276300                    PERFORM IMS-ISRT-WDD901                               
276400                 END-IF                                                   
276500                 PERFORM IMS-GU-WDD902                                    
276600                 IF SEGMENT-SAKNAS                                        
276700                    MOVE SPACE         TO D902-WDD902                     
276800                    MOVE W-IDLEVNR     TO D902-IDLEVNR                    
276900                    MOVE ZERO          TO D902-KVBR                       
277000                                          D902-TILEVPL                    
277100                    PERFORM IMS-ISRT-WDD902                               
277200                 END-IF                                                   
277300                 PERFORM IMS-GU-WDD902                                    
277400              END-IF                                                      
277500                                                                          
277600              IF  SEGMENT-FINNS                                           
277700              AND W-IDLEVNR = SLAG-IDLEVNR                                
277800                  MOVE JA   TO OMSPEC-SW                                  
277900              ELSE                                                        
278000                 MOVE NEJ     TO INDATA-SW                                
278100                 MOVE ERR-HIGHLIGHT-FIELDS-WRONG                          
278200                              TO MED-IDMFSFEL                             
278300                 MOVE MFS-ALFA-FAELT-FEL                                  
278400                              TO MOD-KDOMSPEC-IN-ATTR                     
278500              END-IF                                                      
278600           END-IF                                                         
278700        ELSE                                                              
278800           MOVE NEJ           TO INDATA-SW                                
278900           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
279000                              TO MED-IDMFSFEL                             
279100           MOVE MFS-ALFA-FAELT-FEL                                        
279200                              TO MOD-KDOMSPEC-IN-ATTR                     
279300        END-IF                                                            
279400     END-IF                                                               
279500                                                                          
279600     IF OMSPEC-BEGARD                                                     
279700        IF  (MID-KDKOM          = ALL '+' OR                              
279800             MID-KDKOM          = SPACE)                                  
279900        AND  MID-TIAVROP-AVS(1) = 'YYWW'                                  
280000        AND  MID-TIAVROP-AVS(2) = 'YYWW'                                  
280100        AND  MID-TIAVROP-AVS(3) = 'YYWW'                                  
280200        AND  MID-TIAVROP-AVS(4) = 'YYWW'                                  
280300        AND (MID-KVAVROP(1)     = ALL '+' OR SPACE)                       
280400        AND (MID-KVAVROP(2)     = ALL '+' OR SPACE)                       
280500        AND (MID-KVAVROP(3)     = ALL '+' OR SPACE)                       
280600        AND (MID-KVAVROP(4)     = ALL '+' OR SPACE)                       
280700        AND (MID-KDLEVPLF       = ALL '+' OR                              
280800             MID-KDLEVPLF       = SPAR-KDLEVPLF)                          
280900        AND (MID-FLJIT          = ALL '+' OR                              
281000             MID-FLJIT          = SPAR-FLJIT)                             
281100        AND  MID-TILPSP         = 'YYWW'                                  
281200        AND (MID-TEREFMED-1 = ALL '+' OR                                  
281300             MID-TEREFMED-1 = SPAR-TEREFMED1)                             
281400        AND (MID-TEREFMED-2 = ALL '+' OR                                  
281500             MID-TEREFMED-2 = SPAR-TEREFMED2)                             
281600           CONTINUE                                                       
281700        ELSE                                                              
281800           MOVE NEJ              TO INDATA-SW                             
281900           MOVE ERR-KONFLIKT     TO MED-IDMFSFEL MSG-KOM-IDMFSMED         
282000           MOVE MFS-ALFA-FAELT-FEL                                        
282100                                 TO MOD-KDOMSPEC-IN-ATTR                  
282200        END-IF                                                            
282300     END-IF                                                               
282400     .                                                                    
282500                                                                          
282600                                                                          
282700                                                                          
282800 GD-KOLLA-AVROP   SECTION.                                                
282900                                                                          
283000     MOVE 'GD-KOLLA-AVROP  ' TO CURRENT-SECTION                           
283100                                                                          
283200     MOVE 1 TO AVROP-CHG-IX                                               
283300     PERFORM UNTIL AVROP-CHG-IX > AVROP-CHG-IX-MAX                        
283400                                                                          
283500        MOVE MID-TIAVROP-AVS (AVROP-CHG-IX)                               
283600                              TO W-DATUM-AAVV-X                           
283700        PERFORM GDA-KOLLA-TIAVROP                                         
283800        IF TIAVROP-FEL                                                    
283900           MOVE NEJ           TO INDATA-SW                                
284000           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
284100                              TO MED-IDMFSFEL                             
284200           MOVE MFS-ALFA-FAELT-FEL   TO                                   
284300                   MOD-TIAVROP-AVS-IN-ATTR(AVROP-CHG-IX)                  
284400        ELSE                                                              
284500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
284600                   MOD-TIAVROP-AVS-IN-ATTR(AVROP-CHG-IX)                  
284700           MOVE 'TIAVROP OK'  TO MOD-TEMFSINF                             
284800        END-IF                                                            
284900*--- REMOVE TRAILING SPACES KVAVROP                                       
285000        IF (MID-KVAVROP (AVROP-CHG-IX) = SPACE) OR                        
285100           (MID-KVAVROP (AVROP-CHG-IX) = ALL '+')                         
285200           CONTINUE                                                       
285300        ELSE                                                              
285400          MOVE ZERO TO W-TALLY                                            
285500                       W-LEN                                              
285600                       W-KVAVROP-UP-N                                     
285700          MOVE SPACES TO W-KVAVROP-UP                                     
285800          MOVE MID-KVAVROP (AVROP-CHG-IX) TO W-KVAVROP-UP                 
285900          INSPECT FUNCTION REVERSE(W-KVAVROP-UP) TALLYING                 
286000                  W-TALLY FOR LEADING SPACES                              
286100          COMPUTE W-LEN = (LENGTH OF W-KVAVROP-UP ) - W-TALLY             
286200          MOVE W-KVAVROP-UP(1:W-LEN) TO                                   
286300               W-KVAVROP-UP-N (W-TALLY + 1:W-LEN)                         
286400                                                                          
286500          MOVE W-KVAVROP-UP-N TO MID-KVAVROP (AVROP-CHG-IX)               
286600                                                                          
286700          IF W-KVAVROP-UP-N NOT NUMERIC                                   
286800             MOVE NEJ       TO INDATA-SW                                  
286900             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
287000                            TO MED-IDMFSFEL                               
287100             MOVE MFS-NUM-FAELT-FEL                                       
287200                            TO MOD-KVAVROP-IN-ATTR(AVROP-CHG-IX)          
287300          END-IF                                                          
287400        END-IF                                                            
287500*---                                                                      
287600        IF INDATA-OK                                                      
287700          MOVE MID-KVAVROP (AVROP-CHG-IX)                                 
287800                                TO W-KVAVROP-X                            
287900          PERFORM GDB-KOLLA-KVAVROP                                       
288000          IF KVAVROP-FEL                                                  
288100             MOVE NEJ        TO INDATA-SW                                 
288200             MOVE ERR-HIGHLIGHT-FIELDS-WRONG                              
288300                             TO MED-IDMFSFEL                              
288400             MOVE MFS-NUM-FAELT-FEL                                       
288500                             TO MOD-KVAVROP-IN-ATTR(AVROP-CHG-IX)         
288600          ELSE                                                            
288700             MOVE MFS-NUM-FAELT-RAETT TO                                  
288800                     MOD-KVAVROP-IN-ATTR(AVROP-CHG-IX)                    
288900             MOVE 'KVAVROP OK'  TO MOD-TEMFSINF                           
289000          END-IF                                                          
289100        END-IF                                                            
289200        ADD 1 TO AVROP-CHG-IX                                             
289300     END-PERFORM                                                          
289400                                                                          
289500     .                                                                    
289600                                                                          
289700                                                                          
289800                                                                          
289900 GDA-KOLLA-TIAVROP   SECTION.                                             
290000                                                                          
290100     MOVE 'GDA-KOLLA-TIAVRO' TO CURRENT-SECTION                           
290200                                                                          
290300                                                                          
290400     MOVE JA     TO TIAVROP-SW                                            
290500     IF W-DATUM-AAVV-X = (ALL '+' OR 'YYWW')                              
290600        OR                                                                
290700       (W-DATUM-AAVV-N NUMERIC AND                                        
290800        W-DATUM-AAVV-N > ZERO)                                            
290900                                                                          
291000        IF W-DATUM-AAVV-N NUMERIC                                         
291100           IF  W-DATUM-AAVV-N = 9999                                      
291200           AND W-TIAVROP-AVS-GAM(1) NOT = 9999                            
291300              MOVE NEJ    TO TIAVROP-SW                                   
291400           ELSE                                                           
291500              IF W-DATUM-AAVV-N < W-DATUM-AKTUELLT                        
291600              OR W-DATUM-AAVV-N = 9999                                    
291700* GAMLA AVROP                                                             
291800                 MOVE 1 TO AVROP-G-IX                                     
291900                 PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX OR             
292000                  W-TIAVROP-AVS-GAM(AVROP-G-IX) = W-DATUM-AAVV-N          
292100                                                                          
292200                    ADD 1 TO AVROP-G-IX                                   
292300                 END-PERFORM                                              
292400                 IF AVROP-G-IX NOT > AVROP-G-IX-MAX                       
292500                    MOVE JA TO                                            
292600                            W-AVROP-GAM-AENDRAT (AVROP-G-IX)              
292700                    MOVE JA  TO TIAVROP-SW                                
292800                 ELSE                                                     
292900                    MOVE NEJ TO TIAVROP-SW                                
293000                 END-IF                                                   
293100              ELSE                                                        
293200* NYA AVROP                                                               
293300                 MOVE 1 TO PERIOD-IX                                      
293400                 MOVE 1 TO VECKA-IX                                       
293500                 PERFORM UNTIL PERIOD-IX > PERIOD-IX-MAX                  
293600                            OR W-VECKA-TAB (PERIOD-IX, VECKA-IX)          
293700                               = W-DATUM-AAVV-N                           
293800                                                                          
293900                    PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                 
294000                              OR W-VECKA-TAB (PERIOD-IX, VECKA-IX)        
294100                                 = W-DATUM-AAVV-N                         
294200                                                                          
294300                          ADD 1 TO VECKA-IX                               
294400                                                                          
294500                    END-PERFORM                                           
294600                                                                          
294700                    IF VECKA-IX > VECKA-IX-MAX                            
294800                       ADD  1 TO PERIOD-IX                                
294900                       MOVE 1 TO VECKA-IX                                 
295000                                                                          
295100                    ELSE                                                  
295200                       MOVE JA TO                                         
295300                       W-AVROP-TAB-AENDRAT (PERIOD-IX, VECKA-IX)          
295400                    END-IF                                                
295500                 END-PERFORM                                              
295600                 IF PERIOD-IX > PERIOD-IX-MAX                             
295700                    MOVE NEJ TO TIAVROP-SW                                
295800                 END-IF                                                   
295900              END-IF                                                      
296000           END-IF                                                         
296100        END-IF                                                            
296200     ELSE                                                                 
296300        MOVE NEJ TO TIAVROP-SW                                            
296400     END-IF                                                               
296500     .                                                                    
296600                                                                          
296700                                                                          
296800                                                                          
296900 GDB-KOLLA-KVAVROP   SECTION.                                             
297000                                                                          
297100     MOVE 'GDB-KOLLA-KVAVRO' TO CURRENT-SECTION                           
297200                                                                          
297300                                                                          
297400     MOVE JA     TO KVAVROP-SW                                            
297500     IF (W-KVAVROP-X = ALL '+'  OR                                        
297600         W-KVAVROP-X = SPACE )                                            
297700     AND                                                                  
297800       (W-DATUM-AAVV-X = 'YYWW' OR                                        
297900        W-DATUM-AAVV-X =  SPACE OR                                        
298000        W-DATUM-AAVV-X =  ALL '+' )                                       
298100        CONTINUE                                                          
298200     ELSE                                                                 
298300        IF W-KVAVROP NOT NUMERIC                                          
298400           MOVE NEJ           TO KVAVROP-SW                               
298500        ELSE                                                              
298600           IF  W-DATUM-AAVV-N = 9999                                      
298700           AND (W-KVAVROP > W-KVAVROP-GAM(1)                              
298800            OR  W-KVAVROP = 0)                                            
298900               MOVE NEJ       TO KVAVROP-SW                               
299000           END-IF                                                         
299100        END-IF                                                            
299200     END-IF                                                               
299300     .                                                                    
299400                                                                          
299500                                                                          
299600                                                                          
299700 GE-KOLLA-KDLEVPLF     SECTION.                                           
299800                                                                          
299900     MOVE 'GE-KOLLA-KDLEVPL' TO CURRENT-SECTION                           
300000                                                                          
300100     IF MID-KDLEVPLF NOT = XLAG-KDLEVPLF                                  
300200        IF MID-KDLEVPLF NOT = ALL '+'                                     
300300           IF XLAG-KDLEVPLF = 'G' OR                                      
300400             (XLAG-KDLEVPLF = 'P' AND                                     
300500             (MID-KDLEVPLF NOT = 'Y'   AND                                
300600              MID-KDLEVPLF NOT = 'S'))                                    
300700              MOVE NEJ        TO INDATA-SW                                
300800              MOVE ERR-HIGHLIGHT-FIELDS-WRONG                             
300900                              TO MED-IDMFSFEL                             
301000              MOVE MFS-ALFA-FAELT-FEL                                     
301100                              TO MOD-KDLEVPLF-IN-ATTR                     
301200           ELSE                                                           
301300              IF MID-KDLEVPLF = 'Y' OR 'N' OR 'S'                         
301400                 CONTINUE                                                 
301500              ELSE                                                        
301600                 MOVE NEJ     TO INDATA-SW                                
301700                 MOVE ERR-HIGHLIGHT-FIELDS-WRONG                          
301800                              TO MED-IDMFSFEL                             
301900                 MOVE MFS-ALFA-FAELT-FEL                                  
302000                              TO MOD-KDLEVPLF-IN-ATTR                     
302100              END-IF                                                      
302200           END-IF                                                         
302300        END-IF                                                            
302400     END-IF                                                               
302500     .                                                                    
302600                                                                          
302700                                                                          
302800                                                                          
302900 GF-KOLLA-FLJIT        SECTION.                                           
303000                                                                          
303100     MOVE 'GF-KOLLA-FLJIT  ' TO CURRENT-SECTION                           
303200                                                                          
303300     IF MID-FLJIT = 'J'                                                   
303400        MOVE 'Y'             TO MID-FLJIT                                 
303500                                MOD-FLJIT-IN                              
303600     END-IF                                                               
303700     IF MID-FLJIT = '+' OR 'Y' OR 'N'                                     
303800        CONTINUE                                                          
303900     ELSE                                                                 
304000        MOVE NEJ             TO INDATA-SW                                 
304100        MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                   
304200                             TO MED-IDMFSFEL                              
304300        MOVE MFS-ALFA-FAELT-FEL                                           
304400                             TO MOD-FLJIT-IN-ATTR                         
304500     END-IF                                                               
304600     .                                                                    
304700                                                                          
304800                                                                          
304900                                                                          
305000 GG-KOLLA-TILPSP       SECTION.                                           
305100                                                                          
305200     MOVE 'GG-KOLLA-TILPSP ' TO CURRENT-SECTION                           
305300                                                                          
305400                                                                          
305500     IF MID-TILPSP = 'YYWW' OR SPACE                                      
305600        CONTINUE                                                          
305700     ELSE                                                                 
305800        IF MID-TILPSP NUMERIC                                             
305900        AND (SPAR-KDLPSP = 0 OR 3 OR 6)                                   
306000                                                                          
306100           MOVE MID-TILPSP      TO W-AAVV-CHECK                           
306200           PERFORM S910-KOLLA-AAVV                                        
306300           IF DATUM-FEL                                                   
306400              MOVE NEJ          TO INDATA-SW                              
306500              MOVE ERR-HIGHLIGHT-FIELDS-WRONG                             
306600                                TO MED-IDMFSFEL                           
306700              MOVE MFS-ALFA-FAELT-FEL                                     
306800                                TO MOD-TILPSP-IN-ATTR                     
306900           ELSE                                                           
307000              IF  SPAR-KDLPSP = 6                                         
307100              AND W-AAVV-CHECK > W-DATUM-AKTUELLT                         
307200                 MOVE NEJ       TO INDATA-SW                              
307300                 MOVE ERR-HIGHLIGHT-FIELDS-WRONG                          
307400                                TO MED-IDMFSFEL                           
307500                 MOVE MFS-ALFA-FAELT-FEL                                  
307600                                TO MOD-TILPSP-IN-ATTR                     
307700              END-IF                                                      
307800           END-IF                                                         
307900        ELSE                                                              
308000           MOVE NEJ          TO INDATA-SW                                 
308100           MOVE ERR-HIGHLIGHT-FIELDS-WRONG                                
308200                             TO MED-IDMFSFEL                              
308300           MOVE MFS-ALFA-FAELT-FEL                                        
308400                             TO MOD-TILPSP-IN-ATTR                        
308500       END-IF                                                             
308600     END-IF                                                               
308700     .                                                                    
308800                                                                          
308900 GH-KOLLA-TEREFMED     SECTION.                                           
309000                                                                          
309100     MOVE 'GH-KOLLA-TEREFMED '  TO CURRENT-SECTION                        
309200                                                                          
309300     IF MID-TEREFMED-1 NOT = ALL '+'                                      
309400       MOVE MFS-ALFA-FAELT-RAETT    TO MOD-TEREFMED1-ATTR                 
309500     END-IF                                                               
309600                                                                          
309700     IF MID-TEREFMED-2 NOT = ALL '+'                                      
309800       MOVE MFS-ALFA-FAELT-RAETT    TO MOD-TEREFMED2-ATTR                 
309900     END-IF                                                               
310000                                                                          
310100     .                                                                    
310200                                                                          
310300 GX-ORSAKTEXT-TILL-MOD SECTION.                                           
310400                                                                          
310500     MOVE 'GX-ORS-TILL-MOD ' TO CURRENT-SECTION                           
310600                                                                          
310700     IF  W-KDAVROP = PROPOSAL                                             
310800     OR (W-KDAVROP = VALID AND XLAG-KDLPSP NOT = 5)                       
310900                                                                          
311000        IF  W-ORSAK-TAB-KOD (1) > ZERO                                    
311100        AND W-ORSAK-TAB-KOD (1) NOT > ORSAK-IX-MAX                        
311200            MOVE W-ORSAK-TAB-KOD (1)                                      
311300                             TO ORSAK-IX                                  
311400            MOVE TELPORS (ORSAK-IX)                                       
311500                             TO MOD-TEREFMED1                             
311600        END-IF                                                            
311700                                                                          
311800        IF  W-ORSAK-TAB-KOD (2) > ZERO                                    
311900        AND W-ORSAK-TAB-KOD (2) NOT > ORSAK-IX-MAX                        
312000            MOVE W-ORSAK-TAB-KOD (2)                                      
312100                             TO ORSAK-IX                                  
312200            MOVE TELPORS (ORSAK-IX)                                       
312300                             TO MOD-TEREFMED2                             
312400        END-IF                                                            
312500     END-IF                                                               
312600     .                                                                    
312700                                                                          
312800                                                                          
312900                                                                          
313000 H-UPPDATERA      SECTION.                                                
313100                                                                          
313200     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
313300                                                                          
313400     IF MID-KDKOM NOT = (ALL '+' OR SPACE)                                
313500                                                                          
313600        PERFORM HA-KOMKOD-ANDRAD                                          
313700     END-IF                                                               
313800                                                                          
313900     IF MID-KDOMSPEC = YES                                                
314000        PERFORM HB-STARTA-OMSPEC                                          
314100     END-IF                                                               
314200* VI KOLLAR LITE EFTERSOM X-TRANSEN BARA KOMMER MED MOMKOD 2              
314300* HOPPAR VI ÖVER RESTEN SOM BARA STÄLLER TILL DET (gk)                    
314400                                                                          
314500     IF NOT MFS-UPD-X                                                     
314600       PERFORM HC-UPPDATERA-AVROP                                         
314700*    Sektionen HC- hanterar både ändrade avrop                            
314800*    samt extraleveranser                                                 
314900                                                                          
315000       PERFORM HD-KDLEVPLF-ANDRAD                                         
315100       PERFORM HE-UPPDATERA-FLJIT                                         
315200                                                                          
315300       IF (MID-TEREFMED-1 = ALL '+') AND                                  
315400          (MID-TEREFMED-2 = ALL '+')                                      
315500                                                                          
315600         CONTINUE                                                         
315700       ELSE                                                               
315800         PERFORM HF-UPPDATERA-TEREFMED                                    
315900       END-IF                                                             
316000     END-IF                                                               
316100                                                                          
316200     MOVE INF-UPDATE-DONE    TO MED-IDMFSFEL MSG-KOM-IDMFSMED             
316300     CALL WMEDKONV USING MED-WMEDAREA                                     
316400     MOVE MED-MFSFEL(4:18)   TO W-MESSAGE-BOTTOM-1                        
316500     MOVE W-MESSAGE-BOTTOM   TO MOD-TEMFSINF                              
316600     .                                                                    
316700                                                                          
316800                                                                          
316900                                                                          
317000 HA-KOMKOD-ANDRAD   SECTION.                                              
317100                                                                          
317200     MOVE 'HA-KOMKOD-ANDRAD' TO CURRENT-SECTION                           
317300                                                                          
317400*    För KDKOM 1 och 2 kommer leveransplaneförslag att tas bort           
317500*    från WDD9                                                            
317600*    Om vi har dialog med 2447 behöver vi därför spara nycklar            
317700*    till nästa post på WDD9                                              
317800*                                                                         
317900     IF MID-KDKOM = '1'                                                   
318000        PERFORM HAA-UPPDATERA-KOMKOD-1                                    
318100     END-IF                                                               
318200     IF MID-KDKOM = '2'                                                   
318300        PERFORM HAB-UPPDATERA-KOMKOD-2                                    
318400        PERFORM S200-GEN-UTSKRIFTSBEGARAN                                 
318500     END-IF                                                               
318600     IF MID-KDKOM = '5'                                                   
318700        PERFORM HAC-UPPDATERA-KOMKOD-5                                    
318800        PERFORM S200-GEN-UTSKRIFTSBEGARAN                                 
318900     END-IF                                                               
319000                                                                          
319100                                                                          
319200     .                                                                    
319300                                                                          
319400                                                                          
319500                                                                          
319600 HAA-UPPDATERA-KOMKOD-1 SECTION.                                          
319700                                                                          
319800     MOVE 'HAA-UPD-KOMKOD-1' TO CURRENT-SECTION                           
319900                                                                          
320000     PERFORM IMS-GHU-WDK722                                               
320100     MOVE ZERO            TO XLAG-TILPSP                                  
320200                             XLAG-KDLPSP                                  
320300     PERFORM IMS-REPL-WDK722                                              
320400                                                                          
320500     PERFORM IMS-GHU-WDD904                                               
320600     IF SEGMENT-FINNS                                                     
320700        PERFORM IMS-DLET-WDD904                                           
320800     END-IF                                                               
320900                                                                          
321000     PERFORM IMS-GU-WDD902                                                
321100     IF SEGMENT-FINNS                                                     
321200        PERFORM IMS-GHNP-WDD905                                           
321300        PERFORM UNTIL SEGMENT-SAKNAS                                      
321400           IF D905-KDAVROP = 1                                            
321500                PERFORM IMS-DLET-WDD905                                   
321600           END-IF                                                         
321700           PERFORM IMS-GHNP-WDD905                                        
321800        END-PERFORM                                                       
321900     END-IF                                                               
322000                                                                          
322100     MOVE W-IDDC             TO W-IDDC-D6-MIN                             
322200                                W-IDDC-D6-MAX                             
322300     MOVE W-IDLEVNR          TO W-IDLEVNR-D6-MIN                          
322400                                W-IDLEVNR-D6-MAX                          
322500     MOVE W-IDARTNR          TO W-IDARTNR-D6-MIN                          
322600                                W-IDARTNR-D6-MAX                          
322700     MOVE ZERO               TO W-IDANSK-D6-MIN                           
322800     MOVE 999                TO W-IDANSK-D6-MAX                           
322900     PERFORM IMS-GHU-WDD601                                               
323000     IF SEGMENT-FINNS                                                     
323100        PERFORM IMS-DLET-WDD601                                           
323200     END-IF                                                               
323300                                                                          
323400     MOVE     VALID   TO W-KDAVROP                                        
323500     MOVE '   VALID'  TO MOD-TEXT-PLANTYP                                 
323600     .                                                                    
323700                                                                          
323800                                                                          
323900                                                                          
324000 HAB-UPPDATERA-KOMKOD-2 SECTION.                                          
324100                                                                          
324200     MOVE 'HAB-UPD-KOMKOD-2' TO CURRENT-SECTION                           
324300                                                                          
324400*    OM FÖRSLAG GODKÄNNES JUSTERAS ÄVEN LEVERANSSPÄRR            *        
324500                                                                          
324600       MOVE ZERO              TO W-ARSOMS                                 
324700       MOVE MSGI-IDARTNR      TO W-IDARTNR                                
324800       MOVE MSGI-IDDC-KEY     TO W-IDDC                                   
324900       PERFORM IMS-GU-WDK711                                              
325000       IF SEGMENT-FINNS                                                   
325100          PERFORM IMS-GU-WDB601                                           
325200          IF SEGMENT-FINNS                                                
325300             MOVE DCS-IDLANDX2 TO W-IDLAND                                
325400             PERFORM IMS-GU-WDK712                                        
325500             IF SEGMENT-FINNS                                             
325600                COMPUTE W-ARSOMS = 12                                     
325700                     * (SLAG-KVPB-REF + SLAG-KVPBREOI)                    
325800                     * LART-PRMATRL                                       
325900             END-IF                                                       
326000          END-IF                                                          
326100       END-IF                                                             
326200                                                                          
326300       MOVE W-DATUM-AKTUELLT TO W-AAVV-CHECK                              
326400       IF W-ARSOMS > W-ARSOMS-100000                                      
326500          MOVE 1             TO W-KVVECKOR-CHG                            
326600       ELSE                                                               
326700          MOVE 3             TO W-KVVECKOR-CHG                            
326800       END-IF                                                             
326900                                                                          
327000       PERFORM S920-JUSTERA-AAVV                                          
327100       MOVE W-AAVV-JUST      TO W-DATUM-AAVV                              
327200                                                                          
327300       PERFORM IMS-GHU-WDK722                                             
327400       MOVE W-DATUM-AKTUELLT TO XLAG-TIOMSPEC                             
327500       MOVE W-DATUM-AAVV     TO XLAG-TILPSP                               
327600       MOVE ZERO             TO XLAG-KDLPSP                               
327700       PERFORM IMS-REPL-WDK722                                            
327800                                                                          
327900       MOVE W-DATUM-AAVV     TO MOD-TILPSP                                
328000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TILPSP-ATTR                      
328100                                                                          
328200       PERFORM IMS-GU-WDD902                                              
328300       IF SEGMENT-FINNS                                                   
328400          MOVE D904-DASPECST TO W-DAAVROP-AAVV                            
328500          MOVE 20            TO W-DAAVROP-SS                              
328600          PERFORM IMS-GHNP-WDD905                                         
328700          PERFORM UNTIL SEGMENT-SAKNAS                                    
328800             IF D905-KDAVROP = 1                                          
328900                MOVE 2       TO D905-KDAVROP                              
329000                PERFORM IMS-REPL-WDD905                                   
329100             ELSE                                                         
329200                IF D905-KDAVROP = 2                                       
329300                   PERFORM IMS-DLET-WDD905                                
329400                END-IF                                                    
329500             END-IF                                                       
329600             PERFORM IMS-GHNP-WDD905                                      
329700          END-PERFORM                                                     
329800       END-IF                                                             
329900                                                                          
330000     PERFORM IMS-GHU-WDD904                                               
330100     IF SEGMENT-FINNS                                                     
330200        PERFORM IMS-DLET-WDD904                                           
330300     END-IF                                                               
330400                                                                          
330500     MOVE W-IDDC             TO W-IDDC-D6-MIN                             
330600                                W-IDDC-D6-MAX                             
330700     MOVE W-IDLEVNR          TO W-IDLEVNR-D6-MIN                          
330800                                W-IDLEVNR-D6-MAX                          
330900     MOVE W-IDARTNR          TO W-IDARTNR-D6-MIN                          
331000                                W-IDARTNR-D6-MAX                          
331100     MOVE ZERO               TO W-IDANSK-D6-MIN                           
331200     MOVE 999                TO W-IDANSK-D6-MAX                           
331300     PERFORM IMS-GHU-WDD601                                               
331400     IF SEGMENT-FINNS                                                     
331500        PERFORM IMS-DLET-WDD601                                           
331600     END-IF                                                               
331700                                                                          
331800     MOVE VALID       TO W-KDAVROP                                        
331900     MOVE '   VALID'  TO MOD-TEXT-PLANTYP                                 
332000     .                                                                    
332100                                                                          
332200                                                                          
332300                                                                          
332400 HAC-UPPDATERA-KOMKOD-5 SECTION.                                          
332500                                                                          
332600     MOVE 'HAC-UPD-KOMKOD-5' TO CURRENT-SECTION                           
332700                                                                          
332800*--- KOD= 5 OCH GÄLLANDE : GÄLLANDE PLAN RADERAS EFTER AKTUELL            
332900*---                       VECKA                                          
333000                                                                          
333100     PERFORM IMS-GU-WDD902                                                
333200     MOVE W-DATUM-AKTUELLT   TO W-DAAVROP-AAVV                            
333300     MOVE 20                 TO W-DAAVROP-SS                              
333400     IF SEGMENT-FINNS                                                     
333500        PERFORM IMS-GHNP-WDD905                                           
333600        PERFORM UNTIL SEGMENT-SAKNAS                                      
333700           IF D905-KDAVROP = 2                                            
333800             PERFORM IMS-DLET-WDD905                                      
333900           END-IF                                                         
334000           PERFORM IMS-GHNP-WDD905                                        
334100        END-PERFORM                                                       
334200     END-IF                                                               
334300     .                                                                    
334400                                                                          
334500                                                                          
334600                                                                          
334700 HB-STARTA-OMSPEC SECTION.                                                
334800                                                                          
334900     MOVE 'HB-STARTA-OMSPE'  TO CURRENT-SECTION                           
335000                                                                          
335100     MOVE W-IDARTNR                  TO OMSP-IDARTNR                      
335200     MOVE W-IDDC                     TO OMSP-IDDC                         
335300                                                                          
335400     CALL W224OMSP USING OMSP-W224OMSP                                    
335500                         OMSP-WDD9-PCB OMSP-WDK6-PCB OMSP-WDK7-PCB        
335600                         OMSP-WDF1-PCB OMSP-WDF3-PCB OMSP-WDB6-PCB        
335700                         OMSP-WDD6-PCB OMSP-WDD3-PCB OMSP-WDD7-PCB        
335800                                                                          
335900                         OM-PUNK-REFL1-2501-PCB                           
336000                         OM-PUNK-REFL1-WDB6-PCB                           
336100                         OM-PUNK-REFL1-WDK7-PCB                           
336200                         OM-PUNK-REFL1-UTIL-WDK6-PCB                      
336300                         OM-PUNK-REFL1-UTIL-WDK7-PCB                      
336400                         OM-PUNK-REFL1-UTIL-WDB6-PCB                      
336500                         OM-PUNK-UTUP1-WDK7-PCB                           
336600                         OM-PUNK-UTUP1-WDB6-PCB                           
336700                         OM-PUNK-UTUP1-UTIL-WDK6-PCB                      
336800                         OM-PUNK-UTUP1-UTIL-WDK7-PCB                      
336900                         OM-PUNK-UTUP1-UTIL-WDB6-PCB                      
337000                                                                          
337100                         BHDC-WDK6-PCB BHDC-WDK7-PCB                      
337200                         BHDC-WDB6-PCB BHDC-WDR2-PCB                      
337300                         BHDC-WDD7-PCB BHDC-WDK7E-PCB                     
337400                         BHDC-WDD7-2-PCB BHDC-WDK9-PCB                    
337500                         BHDC-REFL1-2501-PCB                              
337600                         BHDC-REFL1-WDB6-PCB                              
337700                         BHDC-REFL1-WDK7-PCB                              
337800                         BHDC-REFL1-UTIL-WDK6-PCB                         
337900                         BHDC-REFL1-UTIL-WDK7-PCB                         
338000                         BHDC-REFL1-UTIL-WDB6-PCB                         
338100                         BHDC-REFL2-2501-PCB                              
338200                         BHDC-REFL2-WDB6-PCB                              
338300                         BHDC-REFL2-UTIL-WDK6-PCB                         
338400                         BHDC-REFL2-UTIL-WDK7-PCB                         
338500                         BHDC-REFL2-UTIL-WDB6-PCB                         
338600                         BHDC-UTIL-WDK6-PCB                               
338700                         BHDC-UTIL-WDK7-PCB                               
338800                         BHDC-UTIL-WDB6-PCB                               
338900                         BHDC-W222-WDK6-PCB                               
339000                         BHDC-W222-WDK7-PCB                               
339100                         BHDC-W222-ARTM-PCB                               
339200                         BHDC-W222-2501-PCB                               
339300                         BHDC-W222-WDB6R-PCB                              
339400                         BHDC-W222-WDK7R-PCB                              
339500                         BHDC-W222-WDB6-PCB                               
339600                         BHDC-W222-WDD7-PCB                               
339700                         BHDC-W222-WDK7E-PCB                              
339800                         BHDC-W222-UTIL-WDK6-PCB                          
339900                         BHDC-W222-UTIL-WDK7-PCB                          
340000                         BHDC-W222-UTIL-WDB6-PCB                          
340100                         BHDC-W222-UTUP-WDK7-PCB                          
340200                         BHDC-W222-UTUP-WDB6-PCB                          
340300                         BHDC-W222-UTUP-UTIL-WDK6-PCB                     
340400                         BHDC-W222-UTUP-UTIL-WDK7-PCB                     
340500                         BHDC-W222-UTUP-UTIL-WDB6-PCB                     
340600                         BHDC-UTUP-WDK7-PCB                               
340700                         BHDC-UTUP-WDB6-PCB                               
340800                         BHDC-UTUP-UTIL-WDK6-PCB                          
340900                         BHDC-UTUP-UTIL-WDK7-PCB                          
341000                         BHDC-UTUP-UTIL-WDB6-PCB                          
341100                                                                          
341200                         TILG-WDK7-PCB                                    
341300                         TILG-WDL2-PCB                                    
341400                         TILG-WDB6-PCB                                    
341500                         TILG-WDD9-PCB                                    
341600                         TILG-WDK6-PCB                                    
341700                         TILG-WDK9-PCB                                    
341800                                                                          
341900     MOVE INF-OMSPEC-UTF             TO MED-IDMFSFEL                      
342000     MOVE INF-UPDATE-DONE            TO MSG-KOM-IDMFSMED                  
342100     CALL WMEDKONV USING MED-WMEDAREA                                     
342200     MOVE MED-MFSFEL(4:18)           TO W-MESSAGE-BOTTOM-3                
342300     MOVE W-MESSAGE-BOTTOM           TO MOD-TEMFSINF                      
342400                                                                          
342500     MOVE  PROPOSAL   TO W-KDAVROP                                        
342600     MOVE 'PROPOSAL'  TO MOD-TEXT-PLANTYP                                 
342700     PERFORM MFS-ROER-EJ-FAELT-UT                                         
342800     .                                                                    
342900                                                                          
343000                                                                          
343100                                                                          
343200 HC-UPPDATERA-AVROP   SECTION.                                            
343300                                                                          
343400     MOVE 'HC-UPPDATERA-AVR' TO CURRENT-SECTION                           
343500                                                                          
343600*    Uppdatera gällande leveransplan                                      
343700*                                                                         
343800     MOVE 1 TO AVROP-CHG-IX                                               
343900     PERFORM UNTIL AVROP-CHG-IX > AVROP-CHG-IX-MAX                        
344000        IF MID-TIAVROP-AVS (AVROP-CHG-IX) = 'YYWW' OR                     
344100           MID-TIAVROP-AVS (AVROP-CHG-IX) = ALL '+'                       
344200           CONTINUE                                                       
344300        ELSE                                                              
344400           MOVE MID-TIAVROP-AVS (AVROP-CHG-IX) TO W-TIAVROP-CHG           
344500           MOVE MID-KVAVROP     (AVROP-CHG-IX) TO W-KVAVROP-CHG           
344600           PERFORM HCA-UPPDATERA-AVROP                                    
344700           PERFORM S200-GEN-UTSKRIFTSBEGARAN                              
344800        END-IF                                                            
344900        ADD 1 TO AVROP-CHG-IX                                             
345000     END-PERFORM                                                          
345100     .                                                                    
345200                                                                          
345300 HCA-UPPDATERA-AVROP      SECTION.                                        
345400                                                                          
345500     MOVE 'HCA-UPPDATERA-AV' TO CURRENT-SECTION                           
345600                                                                          
345700     IF W-TIAVROP-CHG = 9999                                              
345800        PERFORM HCAA-UPD-GAMLA-PASSERADE                                  
345900     ELSE                                                                 
346000        PERFORM HCAB-JUSTERA-KDAVROP                                      
346100        PERFORM HCAC-RENSA-BEFINTLIG-VECKA                                
346200                                                                          
346300        IF W-KVAVROP-CHG > ZERO                                           
346400           PERFORM HCAD-TILLAGG-NYTT-AVROP                                
346500        END-IF                                                            
346600     END-IF                                                               
346700     .                                                                    
346800                                                                          
346900                                                                          
347000                                                                          
347100 HCAA-UPD-GAMLA-PASSERADE   SECTION.                                      
347200                                                                          
347300     MOVE 'HCAA-GAMLA-PASSE' TO CURRENT-SECTION                           
347400                                                                          
347500        PERFORM IMS-GU-WDD902                                             
347600        PERFORM IMS-GHNP-WDD905-OKVAL                                     
347700        PERFORM UNTIL W-KVAVROP-CHG = ZERO                                
347800           IF W-KVAVROP-CHG < D905-KVAVROP                                
347900              SUBTRACT W-KVAVROP-CHG FROM D905-KVAVROP                    
348000              PERFORM IMS-REPL-WDD905                                     
348100              MOVE ZERO TO W-KVAVROP-CHG                                  
348200           ELSE                                                           
348300              SUBTRACT D905-KVAVROP FROM W-KVAVROP-CHG                    
348400              PERFORM IMS-DLET-WDD905                                     
348500           END-IF                                                         
348600           PERFORM IMS-GHNP-WDD905-OKVAL                                  
348700        END-PERFORM                                                       
348800     .                                                                    
348900                                                                          
349000                                                                          
349100                                                                          
349200 HCAB-JUSTERA-KDAVROP       SECTION.                                      
349300                                                                          
349400     MOVE 'HCAB-FIX-KDAVROP' TO CURRENT-SECTION                           
349500                                                                          
349600     IF W-DAAVROP-CHG-AAAAVV < D904-DASPECST                              
349700        MOVE 2 TO W-KDAVROP                                               
349800     END-IF                                                               
349900     .                                                                    
350000                                                                          
350100                                                                          
350200                                                                          
350300 HCAC-RENSA-BEFINTLIG-VECKA SECTION.                                      
350400                                                                          
350500     MOVE 'HCAC-RENSA-BEFIN' TO CURRENT-SECTION                           
350600                                                                          
350700     MOVE W-TIAVROP-CHG    TO W-DAAVROP-AAVV                              
350800     MOVE 20               TO W-DAAVROP-SS                                
350900     PERFORM IMS-GU-WDD905                                                
351000                                                                          
351100     IF SEGMENT-FINNS                                                     
351200        PERFORM IMS-GU-WDD902                                             
351300        PERFORM IMS-GHNP-WDD905-VECKA                                     
351400                                                                          
351500        PERFORM UNTIL SEGMENT-SAKNAS                                      
351600           PERFORM IMS-DLET-WDD905                                        
351700           PERFORM IMS-GHNP-WDD905-VECKA                                  
351800        END-PERFORM                                                       
351900     END-IF                                                               
352000     .                                                                    
352100                                                                          
352200                                                                          
352300                                                                          
352400 HCAD-TILLAGG-NYTT-AVROP SECTION.                                         
352500                                                                          
352600     MOVE 'HCAD-TILLAGG-NYTT-AVROP' TO  CURRENT-SECTION                   
352700                                                                          
352800     MOVE W-KDAVROP            TO D905-KDAVROP                            
352900     MOVE W-DAAVROP-CHG-AAAAVV TO D905-DAAVROP-AVS                        
353000     MOVE W-KVAVROP-CHG        TO D905-KVAVROP                            
353100                                                                          
353200     PERFORM HCADA-TILEVDAG                                               
353300                                                                          
353400*AVS-AAMMDD                                                               
353500     MOVE 'AAVVD '             TO DAT-KDDATFORM                           
353600     COMPUTE DAT-I-TIDATUM = 10 * W-TIAVROP-CHG  + D905-TILEVDAG          
353700     CALL WDATKONV USING DAT-KDDATFORM                                    
353800                         DAT-I-TIDATUM                                    
353900                         DAT-O-TIDATUM                                    
354000                         DAT-KDSVAR                                       
354100                                                                          
354200     MOVE DAT-TIAAMMDD   TO W-TIAAMMDD-AVS                                
354300                            W-DADATUM-HELG-AAMMDD                         
354400                                                                          
354500*INL + DISP                                                               
354600     PERFORM HCADB-BERAEKNA-INL-DISP-AAMMDD                               
354700                                                                          
354800     MOVE W-IDARTNR      TO W-IDARTNR-D9                                  
354900     MOVE W-IDDC         TO W-IDDC-D9                                     
355000     PERFORM IMS-GU-WDD901                                                
355100     IF SEGMENT-SAKNAS                                                    
355200        MOVE W-IDARTNR     TO D901-IDARTNR                                
355300        MOVE W-IDDC        TO D901-IDDC                                   
355400        PERFORM IMS-ISRT-WDD901                                           
355500     END-IF                                                               
355600     PERFORM IMS-GU-WDD902                                                
355700     IF SEGMENT-SAKNAS                                                    
355800        MOVE SPACE         TO D902-WDD902                                 
355900        MOVE W-IDLEVNR     TO D902-IDLEVNR                                
356000        MOVE ZERO          TO D902-KVBR                                   
356100                              D902-TILEVPL                                
356200        PERFORM IMS-ISRT-WDD902                                           
356300     END-IF                                                               
356400     PERFORM IMS-ISRT-WDD905                                              
356500                                                                          
356600     PERFORM HCADC-KOLL-LEV-HELGDAG                                       
356700     .                                                                    
356800                                                                          
356900                                                                          
357000 HCADA-TILEVDAG           SECTION.                                        
357100                                                                          
357200     MOVE 'HCADA-TILEVDAG  ' TO CURRENT-SECTION                           
357300                                                                          
357400                                                                          
357500     MOVE ZERO               TO D905-TILEVDAG                             
357600                                                                          
357700     MOVE 1  TO DAG-IX                                                    
357800     PERFORM UNTIL DAG-IX > DAG-IX-MAX                                    
357900        IF XLAG-TILEVDAG (DAG-IX) > ZERO                                  
358000           MOVE XLAG-TILEVDAG (DAG-IX) TO D905-TILEVDAG                   
358100           MOVE DAG-IX-MAX   TO DAG-IX                                    
358200        END-IF                                                            
358300        ADD 1                TO DAG-IX                                    
358400     END-PERFORM                                                          
358500                                                                          
358600     IF D905-TILEVDAG = ZERO                                              
358700        PERFORM IMS-GU-WDF101                                             
358800        IF SEGMENT-FINNS                                                  
358900           PERFORM IMS-GU-WDF116                                          
359000                                                                          
359100           IF SEGMENT-FINNS                                               
359200              MOVE 1  TO DAG-IX                                           
359300              PERFORM UNTIL DAG-IX > DAG-IX-MAX                           
359400                 IF NDC-TILEVDAG (DAG-IX) > ZERO                          
359500                    MOVE NDC-TILEVDAG (DAG-IX) TO D905-TILEVDAG           
359600                    MOVE DAG-IX-MAX TO DAG-IX                             
359700                 END-IF                                                   
359800                 ADD 1            TO DAG-IX                               
359900              END-PERFORM                                                 
360000                                                                          
360100              IF D905-TILEVDAG = ZERO                                     
360200                 MOVE 1           TO D905-TILEVDAG                        
360300              END-IF                                                      
360400           ELSE                                                           
360500              MOVE 1              TO D905-TILEVDAG                        
360600           END-IF                                                         
360700        END-IF                                                            
360800     END-IF                                                               
360900     .                                                                    
361000                                                                          
361100                                                                          
361200                                                                          
361300 HCADB-BERAEKNA-INL-DISP-AAMMDD SECTION.                                  
361400                                                                          
361500     MOVE 'HCADB-BER-INL-DI' TO CURRENT-SECTION                           
361600                                                                          
361700*INL                                                                      
361800     MOVE 2                    TO WORK-KDCALL                             
361900     MOVE W-IDDC               TO WORK-IDDC                               
362000     MOVE W-TIAAMMDD-AVS       TO WORK-TIAAMMDD-FOM                       
362100     IF NDC-IDDC NOT = W-IDDC                                             
362200        PERFORM IMS-GU-WDF116                                             
362300        IF SEGMENT-SAKNAS                                                 
362400           MOVE ZERO           TO NDC-KVDAGAR-TT                          
362500        END-IF                                                            
362600     END-IF                                                               
362700     MOVE NDC-KVDAGAR-TT       TO WORK-KVWORKD                            
362800     ADD +1                    TO WORK-KVWORKD                            
362900     CALL WORKDAY USING  WORK-KDCALL                                      
363000                         WORK-DATE-AREA WORK-KDSVAR                       
363100     MOVE WORK-TIAAMMDD-TOM    TO D905-TIAVRDAT-INL                       
363200*DISP                                                                     
363300     MOVE 2                    TO WORK-KDCALL                             
363400     MOVE W-IDDC               TO WORK-IDDC                               
363500     MOVE D905-TIAVRDAT-INL    TO WORK-TIAAMMDD-FOM                       
363600     MOVE LART-KVDAGAR-INLEV   TO WORK-KVWORKD                            
363700     ADD +1                    TO WORK-KVWORKD                            
363800     CALL WORKDAY USING  WORK-KDCALL                                      
363900                         WORK-DATE-AREA WORK-KDSVAR                       
364000     MOVE WORK-TIAAMMDD-TOM    TO D905-TIAVRDAT-DISP                      
364100     .                                                                    
364200                                                                          
364300                                                                          
364400 HCADC-KOLL-LEV-HELGDAG SECTION.                                          
364500                                                                          
364600     MOVE 'HCADC-KOLLA-HELG' TO CURRENT-SECTION                           
364700                                                                          
364800     MOVE SPACE              TO W-IDLANDX2                                
364900     PERFORM IMS-GU-WDF106                                                
365000     IF SEGMENT-FINNS                                                     
365100        MOVE ADR-IDLANDX2    TO W-IDLANDX2                                
365200     END-IF                                                               
365300                                                                          
365400     MOVE 20                TO W-DADATUM-HELG-SS                          
365500     PERFORM IMS-GU-WDF301                                                
365600     IF SEGMENT-FINNS                                                     
365700        MOVE KOM-SUPP-HOLIDAY TO MOD-TEMFSFEL                             
365800     END-IF                                                               
365900     .                                                                    
366000                                                                          
366100                                                                          
366200 HD-KDLEVPLF-ANDRAD    SECTION.                                           
366300                                                                          
366400     MOVE 'HD-KDLEVPLF-ANDR' TO CURRENT-SECTION                           
366500                                                                          
366600     IF (MID-TILPSP   = 'YYWW' OR                                         
366700         MID-TILPSP   = ALL '+' ) AND                                     
366800        (MID-KDLEVPLF = ALL '+' OR                                        
366900         MID-KDLEVPLF = XLAG-KDLEVPLF)                                    
367000                                                                          
367100        CONTINUE                                                          
367200     ELSE                                                                 
367300        PERFORM IMS-GHU-WDK722                                            
367400        IF MID-TILPSP = 'YYWW' OR                                         
367500           MID-TILPSP = ALL '+'                                           
367600           CONTINUE                                                       
367700        ELSE                                                              
367800           MOVE MID-TILPSP TO XLAG-TILPSP                                 
367900                                                                          
368000           IF XLAG-TILPSP > ZERO                                          
368100              MOVE XLAG-TILPSP TO WS-TILPSP-NUM                           
368200              MOVE WS-TILPSP   TO MOD-TILPSP                              
368300           ELSE                                                           
368400              MOVE SPACE       TO MOD-TILPSP                              
368500           END-IF                                                         
368600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TILPSP-ATTR                  
368700           IF  XLAG-KDLPSP = ZERO                                         
368800               MOVE 3 TO XLAG-KDLPSP                                      
368900                         MOD-KDLPSP                                       
369000               MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLPSP-ATTR              
369100           END-IF                                                         
369200        END-IF                                                            
369300        IF MID-KDLEVPLF = ALL '+' OR                                      
369400           MID-KDLEVPLF = XLAG-KDLEVPLF                                   
369500           CONTINUE                                                       
369600        ELSE                                                              
369700           MOVE MID-KDLEVPLF TO XLAG-KDLEVPLF                             
369800                                MOD-KDLEVPLF-IN                           
369900        END-IF                                                            
370000        PERFORM IMS-REPL-WDK722                                           
370100     END-IF                                                               
370200     .                                                                    
370300                                                                          
370400 HE-UPPDATERA-FLJIT   SECTION.                                            
370500                                                                          
370600     MOVE 'HE-UPPD-FLJIT   ' TO CURRENT-SECTION                           
370700                                                                          
370800     IF MID-FLJIT = ALL '+'                                               
370900     OR MID-FLJIT = SPACE                                                 
371000     OR MID-FLJIT = XLAG-FLJIT                                            
371100        CONTINUE                                                          
371200     ELSE                                                                 
371300        PERFORM IMS-GHU-WDK722                                            
371400        IF MID-FLJIT = YES                                                
371500           MOVE JA        TO XLAG-FLJIT                                   
371600        ELSE                                                              
371700           MOVE MID-FLJIT TO XLAG-FLJIT                                   
371800        END-IF                                                            
371900        PERFORM IMS-REPL-WDK722                                           
372000     END-IF                                                               
372100     .                                                                    
372200                                                                          
372300 HF-UPPDATERA-TEREFMED   SECTION.                                         
372400     MOVE 'HF-UPPDATERA-TEREFMED ' TO CURRENT-SECTION                     
372500     MOVE W-IDDC             TO W-IDDC-2261                               
372600     PERFORM IMS-GHU-WDGX2262                                             
372700     IF SEGMENT-FINNS                                                     
372800       IF MID-TEREFMED-1 NOT = ALL '+'                                    
372900         IF MID-TEREFMED-1 = SPAR-TEREFMED1                               
373000           CONTINUE                                                       
373100         ELSE                                                             
373200           IF MID-TEREFMED-1 = SPACE                                      
373300             MOVE SPACE           TO 2262-TEREFMED (1:36)                 
373400           ELSE                                                           
373500             MOVE MID-TEREFMED-1  TO 2262-TEREFMED (1:36)                 
373600           END-IF                                                         
373700           MOVE MFS-ADD-LYS-UPP-FAELT                                     
373800                                  TO MOD-TEREFMED1-ATTR                   
373900         END-IF                                                           
374000       END-IF                                                             
374100                                                                          
374200       IF MID-TEREFMED-2 NOT = ALL '+'                                    
374300         IF MID-TEREFMED-2 = SPAR-TEREFMED2                               
374400           CONTINUE                                                       
374500         ELSE                                                             
374600           IF MID-TEREFMED-2 = SPACE                                      
374700             MOVE SPACE           TO 2262-TEREFMED (37:39)                
374800           ELSE                                                           
374900             MOVE MID-TEREFMED-2  TO 2262-TEREFMED (37:39)                
375000           END-IF                                                         
375100           MOVE MFS-ADD-LYS-UPP-FAELT                                     
375200                                TO MOD-TEREFMED2-ATTR                     
375300         END-IF                                                           
375400       END-IF                                                             
375500       PERFORM IMS-REPL-WDGX2262                                          
375600     ELSE                                                                 
375700       MOVE W-IDARTNR     TO 2262-IDARTNR                                 
375800       MOVE SPACE         TO 2262-TEREFMED                                
375900       IF MID-TEREFMED-1 NOT = ALL '+'                                    
376000         MOVE MID-TEREFMED-1   TO 2262-TEREFMED (1:36)                    
376100       END-IF                                                             
376200       IF MID-TEREFMED-2 NOT = ALL '+'                                    
376300         MOVE MID-TEREFMED-2   TO 2262-TEREFMED (37:39)                   
376400       END-IF                                                             
376500                                                                          
376600       IF 2262-TEREFMED NOT = SPACE                                       
376700         PERFORM IMS-ISRT-WDGX2262                                        
376800       END-IF                                                             
376900                                                                          
377000       IF MID-TEREFMED-1 NOT = ALL '+'                                    
377100         IF MID-TEREFMED-1 = SPACE                                        
377200            MOVE MFS-ADD-LYS-UPP-FAELT                                    
377300                                 TO MOD-TEREFMED1-ATTR                    
377400         ELSE                                                             
377500            MOVE MFS-ADD-LYS-UPP-FAELT                                    
377600                                 TO MOD-TEREFMED1-ATTR                    
377700         END-IF                                                           
377800       END-IF                                                             
377900                                                                          
378000       IF MID-TEREFMED-2 NOT = ALL '+'                                    
378100         IF MID-TEREFMED-2 = SPACE                                        
378200            MOVE MFS-ADD-LYS-UPP-FAELT                                    
378300                                 TO MOD-TEREFMED2-ATTR                    
378400         ELSE                                                             
378500            MOVE MFS-ADD-LYS-UPP-FAELT                                    
378600                                 TO MOD-TEREFMED2-ATTR                    
378700         END-IF                                                           
378800       END-IF                                                             
378900                                                                          
379000     END-IF                                                               
379100     .                                                                    
379200                                                                          
379300 S100-KOLLA-OM-INPUT SECTION.                                             
379400                                                                          
379500     MOVE 'S100-KOLLA-INPUT ' TO CURRENT-S-SECTION                        
379600                                                                          
379700     IF GODK-MID                                                          
379800        IF   MID-KDKOM          = (ALL '+' OR SPACE)                      
379900        AND  MID-KDOMSPEC       = (ALL '+' OR SPACE)                      
380000        AND (MID-TIAVROP-AVS(1) = 'YYWW' OR                               
380100             MID-TIAVROP-AVS(1) = (ALL '+' OR SPACE))                     
380200        AND (MID-TIAVROP-AVS(2) = 'YYWW' OR                               
380300             MID-TIAVROP-AVS(2) = (ALL '+' OR SPACE))                     
380400        AND (MID-TIAVROP-AVS(3) = 'YYWW' OR                               
380500             MID-TIAVROP-AVS(3) = (ALL '+' OR SPACE))                     
380600        AND (MID-TIAVROP-AVS(4) = 'YYWW' OR                               
380700             MID-TIAVROP-AVS(4) = (ALL '+' OR SPACE))                     
380800        AND  MID-KVAVROP(1)     = (ALL '+' OR SPACE)                      
380900        AND  MID-KVAVROP(2)     = (ALL '+' OR SPACE)                      
381000        AND  MID-KVAVROP(3)     = (ALL '+' OR SPACE)                      
381100        AND  MID-KVAVROP(4)     = (ALL '+' OR SPACE)                      
381200        AND (MID-KDLEVPLF       = (ALL '+' OR SPACE) OR                   
381300             MID-KDLEVPLF       = SPAR-KDLEVPLF)                          
381400        AND (MID-FLJIT          = (ALL '+' OR SPACE) OR                   
381500             MID-FLJIT          = SPAR-FLJIT)                             
381600        AND (MID-TILPSP         = 'YYWW' OR                               
381700             MID-TILPSP         = SPACE OR                                
381800             MID-TILPSP         = ALL '+')                                
381900        AND ((MID-TEREFMED-1 = ALL '+') OR                                
382000             (MID-TEREFMED-1 = SPAR-TEREFMED1))                           
382100        AND ((MID-TEREFMED-2 = ALL '+') OR                                
382200             (MID-TEREFMED-2 = SPAR-TEREFMED2))                           
382300                                                                          
382400           MOVE NEJ   TO INPUT-SW                                         
382500        ELSE                                                              
382600           MOVE JA    TO INPUT-SW                                         
382700        END-IF                                                            
382800     ELSE                                                                 
382900        MOVE NEJ   TO INPUT-SW                                            
383000     END-IF                                                               
383100     .                                                                    
383200                                                                          
383300                                                                          
383400 S200-GEN-UTSKRIFTSBEGARAN SECTION.                                       
383500                                                                          
383600     MOVE 'S200-GEN-UTSKRBEG' TO CURRENT-S-SECTION                        
383700                                                                          
383800*UPDATE CURRENT DATE IN WDD902                                            
383900     PERFORM IMS-GHU-WDD902                                               
384000     IF SEGMENT-FINNS                                                     
384100        MOVE W-DAGENS-DATUM  TO D902-TILEVPL                              
384200        PERFORM IMS-REPL-WDD902                                           
384300     END-IF                                                               
384400                                                                          
384500     PERFORM IMS-GHU-WDD904                                               
384600     IF SEGMENT-FINNS                                                     
384700       PERFORM IMS-GU-WDK722                                              
384800       IF SEGMENT-FINNS                                                   
384900          IF XLAG-KDLPSP NOT = 5                                          
385000          OR W-IDLEVNR NOT = SLAG-IDLEVNR                                 
385100             PERFORM IMS-DLET-WDD904                                      
385200          END-IF                                                          
385300       END-IF                                                             
385400     END-IF                                                               
385500                                                                          
385600                                                                          
385700     MOVE W-IDLEVNR            TO W-IDLEVNR-2206                          
385800     MOVE W-IDDC               TO W-IDDC-2206                             
385900     PERFORM IMS-GU-WDGX2206                                              
386000     IF SEGMENT-FINNS                                                     
386100     AND (2206-KDEDI NOT = 'T')                                           
386200                                                                          
386300       MOVE W-IDDC           TO 2248-IDDC                                 
386400       MOVE W-IDLEVNR        TO 2248-IDLEVNR                              
386500       MOVE W-IDARTNR        TO 2248-IDARTNR                              
386600       MOVE ZERO             TO 2248-KVDAGAR                              
386700                                2248-KVBEART                              
386800                                                                          
386900       IF 2206-KDVECKOSL NOT = 'P'                                        
387000         PERFORM IMS-ISRT-WDGX2248                                        
387100       END-IF                                                             
387200                                                                          
387300       IF 2206-FLLEVPLP = JA OR                                           
387400          2206-FLLEVVB  = JA                                              
387500                                                                          
387600         PERFORM IMS-ISRT-WDGX2248-PERIOD                                 
387700       END-IF                                                             
387800     END-IF                                                               
387900     .                                                                    
388000     EJECT                                                                
388100                                                                          
388200 S910-KOLLA-AAVV    SECTION.                                              
388300                                                                          
388400     MOVE 'S910-KOLLA-AAVV  ' TO CURRENT-S-SECTION                        
388500                                                                          
388600     MOVE 'AAVV  '             TO DAT-KDDATFORM                           
388700     MOVE W-AAVV-CHECK         TO DAT-I-TIDATUM                           
388800     CALL WDATKONV USING DAT-KDDATFORM                                    
388900                         DAT-I-TIDATUM                                    
389000                         DAT-O-TIDATUM                                    
389100                         DAT-KDSVAR                                       
389200     IF DAT-KDSVAR-OK                                                     
389300        MOVE JA  TO DATUM-SW                                              
389400     ELSE                                                                 
389500        MOVE NEJ TO DATUM-SW                                              
389600     END-IF                                                               
389700     .                                                                    
389800                                                                          
389900                                                                          
390000                                                                          
390100 S920-JUSTERA-AAVV    SECTION.                                            
390200                                                                          
390300     MOVE 'S920-JUSTERA-AAVV' TO CURRENT-S-SECTION                        
390400                                                                          
390500     IF W-KVVECKOR-CHG > ZERO                                             
390600        PERFORM S921-ADDERA-VECKOR                                        
390700     ELSE                                                                 
390800        PERFORM S922-SUBTRAHERA-VECKOR                                    
390900     END-IF                                                               
391000     .                                                                    
391100                                                                          
391200                                                                          
391300                                                                          
391400 S921-ADDERA-VECKOR SECTION.                                              
391500                                                                          
391600     MOVE 'S921-ADDERA-VECKA' TO CURRENT-S-SECTION                        
391700                                                                          
391800     MOVE W-AAVV-CHECK        TO W-AAVV-JUST                              
391900     IF W-AAVV-JUST-VV + W-KVVECKOR-CHG > 52                              
392000      PERFORM UNTIL W-KVVECKOR-CHG = ZERO                                 
392100        MOVE 53               TO W-AAVV-JUST-VV                           
392200        MOVE 'AAVV  '         TO DAT-KDDATFORM                            
392300        MOVE W-AAVV-JUST      TO DAT-I-TIDATUM                            
392400        CALL WDATKONV USING DAT-KDDATFORM                                 
392500                            DAT-I-TIDATUM                                 
392600                            DAT-O-TIDATUM                                 
392700                            DAT-KDSVAR                                    
392800        IF DAT-KDSVAR-OK                                                  
392900* ÅRET HAR 53 VECKOR                                                      
393000           MOVE W-AAVV-CHECK        TO W-AAVV-JUST                        
393100           IF W-KVVECKOR-CHG > 53                                         
393200              SUBTRACT 53 FROM W-KVVECKOR-CHG                             
393300              ADD       1           TO W-AAVV-JUST-AA                     
393400              MOVE W-AAVV-JUST      TO W-AAVV-CHECK                       
393500           ELSE                                                           
393600              IF W-AAVV-JUST-VV + W-KVVECKOR-CHG > 53                     
393700                 SUBTRACT 53 FROM W-KVVECKOR-CHG                          
393800                 ADD       1        TO W-AAVV-JUST-AA                     
393900                 MOVE W-AAVV-JUST   TO W-AAVV-CHECK                       
394000              ELSE                                                        
394100                 ADD W-KVVECKOR-CHG TO W-AAVV-JUST-VV                     
394200                 MOVE ZERO          TO W-KVVECKOR-CHG                     
394300              END-IF                                                      
394400           END-IF                                                         
394500        ELSE                                                              
394600* ÅRET HAR 52 VECKOR                                                      
394700           MOVE W-AAVV-CHECK        TO W-AAVV-JUST                        
394800           IF W-KVVECKOR-CHG > 52                                         
394900              SUBTRACT 52 FROM W-KVVECKOR-CHG                             
395000              ADD       1           TO W-AAVV-JUST-AA                     
395100              MOVE W-AAVV-JUST      TO W-AAVV-CHECK                       
395200           ELSE                                                           
395300              IF W-AAVV-JUST-VV + W-KVVECKOR-CHG > 52                     
395400                 SUBTRACT 52 FROM W-KVVECKOR-CHG                          
395500                 ADD       1        TO W-AAVV-JUST-AA                     
395600                 MOVE W-AAVV-JUST   TO W-AAVV-CHECK                       
395700              ELSE                                                        
395800                 ADD W-KVVECKOR-CHG TO W-AAVV-JUST-VV                     
395900                 MOVE ZERO          TO W-KVVECKOR-CHG                     
396000              END-IF                                                      
396100           END-IF                                                         
396200        END-IF                                                            
396300      END-PERFORM                                                         
396400     ELSE                                                                 
396500        COMPUTE W-AAVV-JUST = W-AAVV-CHECK + W-KVVECKOR-CHG               
396600     END-IF                                                               
396700     .                                                                    
396800                                                                          
396900                                                                          
397000                                                                          
397100 S922-SUBTRAHERA-VECKOR SECTION.                                          
397200                                                                          
397300     MOVE 'S922-SUBTRA-VECKA' TO CURRENT-S-SECTION                        
397400                                                                          
397500     MOVE W-AAVV-CHECK        TO W-AAVV-JUST                              
397600     COMPUTE W-KVVECKOR-CHG = W-KVVECKOR-CHG * -1                         
397700     MOVE W-KVVECKOR-CHG TO W-KVVECKOR-DISP                               
397800* BARA SÅ VI FÅR ETT POSITIVT TAL SOM VI KAN DRAA IFRÅN :-)               
397900                                                                          
398000     IF W-AAVV-JUST-VV - W-KVVECKOR-CHG < 1                               
398100      PERFORM UNTIL W-KVVECKOR-CHG = ZERO                                 
398200                                                                          
398300        SUBTRACT 1          FROM W-AAVV-JUST-AA                           
398400        MOVE 53               TO W-AAVV-JUST-VV                           
398500        MOVE 'AAVV  '         TO DAT-KDDATFORM                            
398600        MOVE W-AAVV-JUST      TO DAT-I-TIDATUM                            
398700        CALL WDATKONV USING DAT-KDDATFORM                                 
398800                            DAT-I-TIDATUM                                 
398900                            DAT-O-TIDATUM                                 
399000                            DAT-KDSVAR                                    
399100        IF DAT-KDSVAR-OK                                                  
399200* FÖREGÅENDE ÅR HAR 53 VECKOR                                             
399300           MOVE W-AAVV-CHECK  TO W-AAVV-JUST                              
399400           IF W-KVVECKOR-CHG > 53                                         
399500              SUBTRACT 53 FROM W-KVVECKOR-CHG                             
399600              SUBTRACT  1 FROM W-AAVV-JUST-AA                             
399700              MOVE W-AAVV-JUST   TO W-AAVV-CHECK                          
399800           ELSE                                                           
399900              IF W-AAVV-JUST-VV - W-KVVECKOR-CHG < 1                      
400000                 SUBTRACT 53 FROM W-KVVECKOR-CHG                          
400100                 SUBTRACT  1 FROM W-AAVV-JUST-AA                          
400200                 MOVE W-AAVV-JUST   TO W-AAVV-CHECK                       
400300              ELSE                                                        
400400                 SUBTRACT W-KVVECKOR-CHG FROM W-AAVV-JUST-VV              
400500                 MOVE ZERO TO W-KVVECKOR-CHG                              
400600              END-IF                                                      
400700           END-IF                                                         
400800        ELSE                                                              
400900* FÖREGÅENDE ÅR HAR 52 VECKOR                                             
401000           MOVE W-AAVV-CHECK  TO W-AAVV-JUST                              
401100           IF W-KVVECKOR-CHG > 52                                         
401200              SUBTRACT 52 FROM W-KVVECKOR-CHG                             
401300              SUBTRACT  1 FROM W-AAVV-JUST-AA                             
401400              MOVE W-AAVV-JUST   TO W-AAVV-CHECK                          
401500           ELSE                                                           
401600              IF W-AAVV-JUST-VV - W-KVVECKOR-CHG < 1                      
401700                 SUBTRACT 52 FROM W-KVVECKOR-CHG                          
401800                 SUBTRACT  1 FROM W-AAVV-JUST-AA                          
401900                 MOVE W-AAVV-JUST   TO W-AAVV-CHECK                       
402000              ELSE                                                        
402100                 SUBTRACT W-KVVECKOR-CHG FROM W-AAVV-JUST-VV              
402200                 MOVE ZERO TO W-KVVECKOR-CHG                              
402300              END-IF                                                      
402400           END-IF                                                         
402500        END-IF                                                            
402600      END-PERFORM                                                         
402700     ELSE                                                                 
402800        COMPUTE W-AAVV-JUST = W-AAVV-CHECK - W-KVVECKOR-CHG               
402900     END-IF                                                               
403000     .                                                                    
403100                                                                          
403200                                                                          
403300                                                                          
403400 S930-SUBTRACT-FRAN-ORSAKOMSPEC SECTION.                                  
403500                                                                          
403600     MOVE 'S930-SUBTRACT-ORS' TO CURRENT-S-SECTION                        
403700                                                                          
403800     MOVE 1 TO ORSAK-IX                                                   
403900     PERFORM UNTIL ORSAK-IX > ORSAK-IX-MAX                                
404000        IF W-ORSAKS-KOD = D904-KDLPORS-TAB (ORSAK-IX)                     
404100           MOVE ORSAK-IX TO ORSAK-IX-PLUS-1                               
404200           ADD  1        TO ORSAK-IX-PLUS-1                               
404300           PERFORM UNTIL ORSAK-IX = ORSAK-IX-MAX                          
404400              MOVE D904-KDLPORS-TAB (ORSAK-IX-PLUS-1)                     
404500                         TO D904-KDLPORS-TAB (ORSAK-IX)                   
404600              ADD 1 TO ORSAK-IX                                           
404700                       ORSAK-IX-PLUS-1                                    
404800           END-PERFORM                                                    
404900           MOVE 4        TO ORSAK-IX                                      
405000           MOVE ZERO     TO D904-KDLPORS-TAB (3)                          
405100        END-IF                                                            
405200        ADD 1 TO ORSAK-IX                                                 
405300     END-PERFORM                                                          
405400     .                                                                    
405500                                                                          
405600                                                                          
405700                                                                          
405800 MFS-RENSA-WDK7-FAELT-UT SECTION.                                         
405900     MOVE 'MFS-RENSA-WDK7-UT    ' TO CURRENT-SECTION                      
406000                                                                          
406100*    --- ALLA UTDATA-FÄLT FÖR WDK7                                        
406200     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-SHIP                             
406300                             MOD-TEXT-PLANTYP                             
406400                             MOD-TIOMSPEC                                 
406500                             MOD-TEXT-ORSAK1                              
406600                             MOD-TEXT-ORSAK2                              
406700                             MOD-TELPORSX                                 
406800                             MOD-IDANSK                                   
406900                             MOD-KDAVT                                    
407000                             MOD-KVVECKOR-LT                              
407100**--SE FB-                   MOD-TIAAVVD-DAPUBL                           
407200                             MOD-KDLPSP                                   
407300                             MOD-TILPSP                                   
407400                             MOD-FLAGGA-SEASON                            
407500                             MOD-FLAGGA-TREND                             
407600                             MOD-KVPB-REF-SUM                             
407700                             MOD-TEREFMED1                                
407800                             MOD-TEREFMED2                                
407900                                                                          
408000     MOVE 1 TO AVROP-G-IX                                                 
408100     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
408200        MOVE MFS-RENSA-FAELT TO MOD-KVAVROP-GAM     (AVROP-G-IX)          
408300                                MOD-SKILJETECKEN-GAM(AVROP-G-IX)          
408400                                MOD-TIAVROP-AVS-GAM (AVROP-G-IX)          
408500        ADD 1 TO AVROP-G-IX                                               
408600     END-PERFORM                                                          
408700                                                                          
408800     MOVE 1 TO RAD-IX                                                     
408900     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
409000        MOVE MFS-RENSA-FAELT TO MOD-PERIOD-AAPP     (RAD-IX)              
409100                                MOD-PERIOD-PARENTES (RAD-IX)              
409200        MOVE 1 TO VECKA-IX                                                
409300        PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                             
409400           MOVE MFS-RENSA-FAELT TO                                        
409500                MOD-KVAVROP-TAB          (RAD-IX, VECKA-IX)               
409600                MOD-IDTECKEN-TAB         (RAD-IX, VECKA-IX)               
409700                MOD-TIAVROP-AVS-TAB      (RAD-IX, VECKA-IX)               
409800                MOD-IDTECKEN-PARENTES-TAB(RAD-IX, VECKA-IX)               
409900           ADD 1 TO VECKA-IX                                              
410000        END-PERFORM                                                       
410100        ADD 1 TO RAD-IX                                                   
410200     END-PERFORM                                                          
410300     .                                                                    
410400                                                                          
410500 MFS-RENSA-FAELT-UT SECTION.                                              
410600                                                                          
410700     MOVE 'MFS-RENSA-UT    ' TO CURRENT-SECTION                           
410800                                                                          
410900*    --- ALLA UTDATA-FÄLT                                                 
411000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-SHIP                             
411100                             MOD-TEXT-PLANTYP                             
411200                             MOD-TIOMSPEC                                 
411300                             MOD-BEART-ENG                                
411400                             MOD-TEXT-ORSAK1                              
411500                             MOD-TEXT-ORSAK2                              
411600                             MOD-KDERS                                    
411700                             MOD-FLERSATT                                 
411800                             MOD-IDARTNR-ERS                              
411900                             MOD-IDARTNR-TILLK                            
412000                             MOD-TELPORSX                                 
412100                             MOD-IDANSK                                   
412200                             MOD-KDAVT                                    
412300                             MOD-KVVECKOR-LT                              
412400                             MOD-TIAAVVD-DAPUBL                           
412500                             MOD-TIURPROD                                 
412600                             MOD-KDLPSP                                   
412700                             MOD-TILPSP                                   
412800                             MOD-FLAGGA-SEASON                            
412900                             MOD-FLAGGA-TREND                             
413000**-- se defaultvärden        MOD-TILPSP-IN                                
413100                             MOD-KVPB-REF-SUM                             
413200                             MOD-TEREFMED1                                
413300                             MOD-TEREFMED2                                
413400                                                                          
413500     MOVE 1 TO AVROP-G-IX                                                 
413600     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
413700        MOVE MFS-RENSA-FAELT TO MOD-KVAVROP-GAM     (AVROP-G-IX)          
413800                                MOD-SKILJETECKEN-GAM(AVROP-G-IX)          
413900                                MOD-TIAVROP-AVS-GAM (AVROP-G-IX)          
414000        ADD 1 TO AVROP-G-IX                                               
414100     END-PERFORM                                                          
414200                                                                          
414300     MOVE 1 TO RAD-IX                                                     
414400     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
414500        MOVE MFS-RENSA-FAELT TO MOD-PERIOD-AAPP     (RAD-IX)              
414600                                MOD-PERIOD-PARENTES (RAD-IX)              
414700        MOVE 1 TO VECKA-IX                                                
414800        PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                             
414900           MOVE MFS-RENSA-FAELT TO                                        
415000                MOD-KVAVROP-TAB          (RAD-IX, VECKA-IX)               
415100                MOD-IDTECKEN-TAB         (RAD-IX, VECKA-IX)               
415200                MOD-TIAVROP-AVS-TAB      (RAD-IX, VECKA-IX)               
415300                MOD-IDTECKEN-PARENTES-TAB(RAD-IX, VECKA-IX)               
415400           ADD 1 TO VECKA-IX                                              
415500        END-PERFORM                                                       
415600        ADD 1 TO RAD-IX                                                   
415700     END-PERFORM                                                          
415800     MOVE 'MFS-RENSA-UT XIT' TO CURRENT-SECTION                           
415900     .                                                                    
416000                                                                          
416100                                                                          
416200                                                                          
416300 MFS-RENSA-FAELT-IN SECTION.                                              
416400                                                                          
416500     MOVE 'MFS-RENSA-IN    ' TO CURRENT-SECTION                           
416600                                                                          
416700*    --- ALLA INDATA-FÄLT                                                 
416800     MOVE MFS-RENSA-FAELT TO MOD-KDKOM-IN                                 
416900                             MOD-KDOMSPEC-IN                              
417000                             MOD-KDLEVPLF-IN                              
417100                             MOD-FLJIT-IN                                 
417200                             MOD-TILPSP-IN                                
417300                             MOD-TEREFMED1                                
417400                             MOD-TEREFMED2                                
417500                                                                          
417600     MOVE 1 TO AVROP-IN-IX                                                
417700     PERFORM UNTIL AVROP-IN-IX > AVROP-IN-IX-MAX                          
417800        MOVE MFS-RENSA-FAELT TO MOD-TIAVROP-AVS-IN(AVROP-IN-IX)           
417900                                MOD-KVAVROP-IN    (AVROP-IN-IX)           
418000        ADD 1 TO AVROP-IN-IX                                              
418100     END-PERFORM                                                          
418200     .                                                                    
418300                                                                          
418400                                                                          
418500                                                                          
418600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
418700                                                                          
418800     MOVE 'MFS-ROR-EJ-UT   ' TO CURRENT-SECTION                           
418900                                                                          
419000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-SHIP                           
419100                               MOD-TEXT-PLANTYP                           
419200                               MOD-TIOMSPEC                               
419300                               MOD-BEART-ENG                              
419400                               MOD-TEXT-ORSAK1                            
419500                               MOD-TEXT-ORSAK2                            
419600                               MOD-KDERS                                  
419700                               MOD-FLERSATT                               
419800                               MOD-IDARTNR-ERS                            
419900                               MOD-IDARTNR-TILLK                          
420000                               MOD-TELPORSX                               
420100                               MOD-IDANSK                                 
420200                               MOD-KDAVT                                  
420300                               MOD-KVVECKOR-LT                            
420400                               MOD-TIAAVVD-DAPUBL                         
420500                               MOD-TIURPROD                               
420600                               MOD-KDLPSP                                 
420700                               MOD-TILPSP                                 
420800                               MOD-FLAGGA-SEASON                          
420900                               MOD-FLAGGA-TREND                           
421000                               MOD-KVPB-REF-SUM                           
421100                               MOD-TEREFMED1                              
421200                               MOD-TEREFMED2                              
421300                                                                          
421400     MOVE 1 TO AVROP-G-IX                                                 
421500     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
421600        MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVROP-GAM     (AVROP-G-IX)        
421700                                  MOD-SKILJETECKEN-GAM(AVROP-G-IX)        
421800                                  MOD-TIAVROP-AVS-GAM (AVROP-G-IX)        
421900        ADD 1 TO AVROP-G-IX                                               
422000     END-PERFORM                                                          
422100                                                                          
422200     MOVE 1 TO RAD-IX                                                     
422300     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
422400        MOVE MFS-ROER-EJ-FAELT TO MOD-PERIOD-AAPP     (RAD-IX)            
422500                                  MOD-PERIOD-PARENTES (RAD-IX)            
422600        MOVE 1 TO VECKA-IX                                                
422700        PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                             
422800           MOVE MFS-ROER-EJ-FAELT TO                                      
422900                MOD-KVAVROP-TAB          (RAD-IX, VECKA-IX)               
423000                MOD-IDTECKEN-TAB         (RAD-IX, VECKA-IX)               
423100                MOD-TIAVROP-AVS-TAB      (RAD-IX, VECKA-IX)               
423200                MOD-IDTECKEN-PARENTES-TAB(RAD-IX, VECKA-IX)               
423300           ADD 1 TO VECKA-IX                                              
423400        END-PERFORM                                                       
423500        ADD 1 TO RAD-IX                                                   
423600     END-PERFORM                                                          
423700     .                                                                    
423800                                                                          
423900                                                                          
424000                                                                          
424100 MFS-ROER-EJ-MOD-FAELT-UT  SECTION.                                       
424200                                                                          
424300     MOVE 'MFS-ROR-EJ-MOD-UT   ' TO CURRENT-SECTION                       
424400                                                                          
424500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-SHIP                           
424600                               MOD-TEXT-PLANTYP                           
424700                               MOD-TIOMSPEC                               
424800                               MOD-BEART-ENG                              
424900                               MOD-TEXT-ORSAK1                            
425000                               MOD-TEXT-ORSAK2                            
425100                               MOD-KDERS                                  
425200                               MOD-FLERSATT                               
425300                               MOD-IDARTNR-ERS                            
425400                               MOD-IDARTNR-TILLK                          
425500                               MOD-TELPORSX                               
425600                               MOD-IDANSK                                 
425700                               MOD-KDAVT                                  
425800                               MOD-KVVECKOR-LT                            
425900                               MOD-TIAAVVD-DAPUBL                         
426000                               MOD-TIURPROD                               
426100                               MOD-KDLPSP                                 
426200                               MOD-TILPSP                                 
426300                               MOD-FLAGGA-SEASON                          
426400                               MOD-FLAGGA-TREND                           
426500                               MOD-KVPB-REF-SUM                           
426600                                                                          
426700     MOVE 1 TO AVROP-G-IX                                                 
426800     PERFORM UNTIL AVROP-G-IX > AVROP-G-IX-MAX                            
426900        MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVROP-GAM     (AVROP-G-IX)        
427000                                  MOD-SKILJETECKEN-GAM(AVROP-G-IX)        
427100                                  MOD-TIAVROP-AVS-GAM (AVROP-G-IX)        
427200        ADD 1 TO AVROP-G-IX                                               
427300     END-PERFORM                                                          
427400                                                                          
427500     MOVE 1 TO RAD-IX                                                     
427600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
427700        MOVE MFS-ROER-EJ-FAELT TO MOD-PERIOD-AAPP     (RAD-IX)            
427800                                  MOD-PERIOD-PARENTES (RAD-IX)            
427900        MOVE 1 TO VECKA-IX                                                
428000        PERFORM UNTIL VECKA-IX > VECKA-IX-MAX                             
428100           MOVE MFS-ROER-EJ-FAELT TO                                      
428200                MOD-KVAVROP-TAB          (RAD-IX, VECKA-IX)               
428300                MOD-IDTECKEN-TAB         (RAD-IX, VECKA-IX)               
428400                MOD-TIAVROP-AVS-TAB      (RAD-IX, VECKA-IX)               
428500                MOD-IDTECKEN-PARENTES-TAB(RAD-IX, VECKA-IX)               
428600           ADD 1 TO VECKA-IX                                              
428700        END-PERFORM                                                       
428800        ADD 1 TO RAD-IX                                                   
428900     END-PERFORM                                                          
429000     .                                                                    
429100                                                                          
429200                                                                          
429300                                                                          
429400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
429500                                                                          
429600     MOVE 'MFS-ROR-EJ-IN   ' TO CURRENT-SECTION                           
429700                                                                          
429800*    --- ALLA INDATA-FÄLT                                                 
429900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDKOM-IN                               
430000                               MOD-KDOMSPEC-IN                            
430100                               MOD-KDLEVPLF-IN                            
430200                               MOD-FLJIT-IN                               
430300                               MOD-TILPSP-IN                              
430400                               MOD-TEREFMED1                              
430500                               MOD-TEREFMED2                              
430600                                                                          
430700     MOVE 1 TO AVROP-IN-IX                                                
430800     PERFORM UNTIL AVROP-IN-IX > AVROP-IN-IX-MAX                          
430900        MOVE MFS-ROER-EJ-FAELT TO MOD-TIAVROP-AVS-IN(AVROP-IN-IX)         
431000                                  MOD-KVAVROP-IN    (AVROP-IN-IX)         
431100        ADD 1 TO AVROP-IN-IX                                              
431200     END-PERFORM                                                          
431300     .                                                                    
431400                                                                          
431500                                                                          
431600                                                                          
431700 MFS-STAENG-FAELT-IN  SECTION.                                            
431800                                                                          
431900     MOVE 'MFS-STAEN-IN    ' TO CURRENT-SECTION                           
432000                                                                          
432100*    --- ALLA INDATA-FÄLT                                                 
432200     MOVE MFS-STAENG-FAELT  TO MOD-KDKOM-IN-ATTR                          
432300                               MOD-KDOMSPEC-IN-ATTR                       
432400                               MOD-KDLEVPLF-IN-ATTR                       
432500                               MOD-FLJIT-IN-ATTR                          
432600                               MOD-TILPSP-IN-ATTR                         
432700                               MOD-TEREFMED1-ATTR                         
432800                               MOD-TEREFMED2-ATTR                         
432900                                                                          
433000     MOVE 1 TO AVROP-IN-IX                                                
433100     PERFORM UNTIL AVROP-IN-IX > AVROP-IN-IX-MAX                          
433200        MOVE MFS-STAENG-FAELT TO                                          
433300                              MOD-TIAVROP-AVS-IN-ATTR(AVROP-IN-IX)        
433400                              MOD-KVAVROP-IN-ATTR    (AVROP-IN-IX)        
433500        ADD 1 TO AVROP-IN-IX                                              
433600     END-PERFORM                                                          
433700     .                                                                    
433800                                                                          
433900                                                                          
434000                                                                          
434100 MFS-FORM-ATTR SECTION.                                                   
434200                                                                          
434300     MOVE 'MFS-FORM-ATTR   ' TO CURRENT-SECTION                           
434400                                                                          
434500*    --- ALLA INDATA-FÄLT                                                 
434600     MOVE MFS-FORMATETS-ATTR TO MOD-KDKOM-IN-ATTR                         
434700                                MOD-KDOMSPEC-IN-ATTR                      
434800                                MOD-KDLEVPLF-IN-ATTR                      
434900                                MOD-FLJIT-IN-ATTR                         
435000                                MOD-TILPSP-IN-ATTR                        
435100                                MOD-TEREFMED1-ATTR                        
435200                                MOD-TEREFMED2-ATTR                        
435300                                                                          
435400     MOVE 1 TO AVROP-IN-IX                                                
435500     PERFORM UNTIL AVROP-IN-IX > AVROP-IN-IX-MAX                          
435600       MOVE MFS-FORMATETS-ATTR TO                                         
435700            MOD-TIAVROP-AVS-IN-ATTR (AVROP-IN-IX)                         
435800            MOD-KVAVROP-IN-ATTR     (AVROP-IN-IX)                         
435900        ADD 1 TO AVROP-IN-IX                                              
436000     END-PERFORM                                                          
436100     .                                                                    
436200                                                                          
436300                                                                          
436400                                                                          
436500 MFS-LAES-IN-IGEN SECTION.                                                
436600                                                                          
436700     MOVE 'MFS-LAES-IN-IGEN' TO CURRENT-SECTION                           
436800                                                                          
436900*    --- ALLA INDATA-FÄLT                                                 
437000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKOM-IN-ATTR                      
437100                                   MOD-KDOMSPEC-IN-ATTR                   
437200                                   MOD-KDLEVPLF-IN-ATTR                   
437300                                   MOD-FLJIT-IN-ATTR                      
437400                                   MOD-TILPSP-IN-ATTR                     
437500                                   MOD-TEREFMED1-ATTR                     
437600                                   MOD-TEREFMED2-ATTR                     
437700                                                                          
437800     MOVE 1 TO AVROP-IN-IX                                                
437900     PERFORM UNTIL AVROP-IN-IX > AVROP-IN-IX-MAX                          
438000        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
438100             MOD-TIAVROP-AVS-IN-ATTR(AVROP-IN-IX)                         
438200             MOD-KVAVROP-IN-ATTR    (AVROP-IN-IX)                         
438300        ADD 1 TO AVROP-IN-IX                                              
438400     END-PERFORM                                                          
438500     .                                                                    
438600                                                                          
438700                                                                          
438800                                                                          
438900* --- IMS SEKTIONER ---                                                   
439000                                                                          
439100 IMS-GET-MSG SECTION.                                                     
439200                                                                          
439300     MOVE '  QC'          TO GODK-STATUSKODER                             
439400     CALL CBLTDLI      USING GU MSG-PCB MSG-IO-AREA                       
439500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
439600     PERFORM IMS-STATUSKONTROLL                                           
439700     .                                                                    
439800                                                                          
439900 IMS-GET-WMSGKOM-MSG SECTION.                                             
440000                                                                          
440100     MOVE '  QD'          TO GODK-STATUSKODER                             
440200     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
440300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
440400     PERFORM IMS-STATUSKONTROLL                                           
440500     .                                                                    
440600                                                                          
440700 IMS-INSERT-MSG SECTION.                                                  
440800                                                                          
440900     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
441000     MOVE SPACE           TO GODK-STATUSKODER                             
441100     CALL CBLTDLI      USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD           
441200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
441300     PERFORM IMS-STATUSKONTROLL                                           
441400     .                                                                    
441500                                                                          
441600 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
441700                                                                          
441800     MOVE '  '               TO GODK-STATUSKODER                          
441900     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
442000     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
442100     PERFORM IMS-STATUSKONTROLL                                           
442200     .                                                                    
442300                                                                          
442400                                                                          
442500                                                                          
442600 IMS-GU-WDK601 SECTION.                                                   
442700                                                                          
442800     MOVE 'IMS-GU-WDK601   '    TO CURRENT-IMS-SECTION                    
442900                                                                          
443000     MOVE SPACE                 TO ALL-SSA                                
443100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
443200          DELIMITED BY SIZE   INTO SSA1                                   
443300     MOVE '  GE'                TO GODK-STATUSKODER                       
443400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
443500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
443600     PERFORM IMS-STATUSKONTROLL                                           
443700     .                                                                    
443800                                                                          
443900 IMS-GU-WDK601-OLD          SECTION.                                      
444000                                                                          
444100     MOVE 'IMS-GU-WDK601OLD'    TO CURRENT-IMS-SECTION                    
444200                                                                          
444300     MOVE SPACE                 TO ALL-SSA                                
444400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-OLD-X ')'                     
444500            DELIMITED BY SIZE INTO SSA1                                   
444600     MOVE '  GE' TO GODK-STATUSKODER                                      
444700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601-OLD SSA1                
444800     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
444900     PERFORM IMS-STATUSKONTROLL                                           
445000     .                                                                    
445100                                                                          
445200 IMS-GNP-WDK611 SECTION.                                                  
445300                                                                          
445400     MOVE 'IMS-GNP-WDK611  '    TO CURRENT-IMS-SECTION                    
445500                                                                          
445600     MOVE SPACE                 TO ALL-SSA                                
445700     MOVE 'WDK611 '             TO SSA1                                   
445800     MOVE '  GE'                TO GODK-STATUSKODER                       
445900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
446000     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
446100     PERFORM IMS-STATUSKONTROLL                                           
446200     .                                                                    
446300                                                                          
446400 IMS-GNP-WDK629 SECTION.                                                  
446500                                                                          
446600     MOVE 'IMS-GNP-WDK629  '    TO CURRENT-IMS-SECTION                    
446700                                                                          
446800     MOVE SPACE                 TO ALL-SSA                                
446900     MOVE 'WDK629 '             TO SSA1                                   
447000     MOVE '  GE'                TO GODK-STATUSKODER                       
447100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
447200     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
447300     PERFORM IMS-STATUSKONTROLL                                           
447400     .                                                                    
447500                                                                          
447600 IMS-GNP-WDK611-OLD SECTION.                                              
447700                                                                          
447800     MOVE 'IMS-GNP-WDK611-O'    TO CURRENT-IMS-SECTION                    
447900                                                                          
448000     MOVE SPACE                 TO ALL-SSA                                
448100     MOVE 'WDK611 '             TO SSA1                                   
448200     MOVE '  GE'                TO GODK-STATUSKODER                       
448300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611-OLD SSA1               
448400     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
448500     PERFORM IMS-STATUSKONTROLL                                           
448600     .                                                                    
448700                                                                          
448800                                                                          
448900                                                                          
449000 IMS-GU-WDK701 SECTION.                                                   
449100                                                                          
449200     MOVE 'IMS-GU-WDK701   '    TO CURRENT-IMS-SECTION                    
449300                                                                          
449400     MOVE SPACE                 TO ALL-SSA                                
449500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
449600          DELIMITED BY SIZE   INTO SSA1                                   
449700     MOVE '    ' TO GODK-STATUSKODER                                      
449800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
449900     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
450000     PERFORM IMS-STATUSKONTROLL                                           
450100     .                                                                    
450200                                                                          
450300 IMS-GU-WDK711 SECTION.                                                   
450400                                                                          
450500     MOVE 'IMS-GU-WDK711   '    TO CURRENT-IMS-SECTION                    
450600                                                                          
450700     MOVE SPACE                 TO ALL-SSA                                
450800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
450900          DELIMITED BY SIZE   INTO SSA1                                   
451000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
451100          DELIMITED BY SIZE   INTO SSA2                                   
451200     MOVE '  GE' TO GODK-STATUSKODER                                      
451300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
451400     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
451500     PERFORM IMS-STATUSKONTROLL                                           
451600     .                                                                    
451700                                                                          
451800 IMS-GNP-WDK711 SECTION.                                                  
451900     MOVE 'IMS-GNP-WDK711  '  TO CURRENT-IMS-SECTION                      
452000                                                                          
452100     MOVE SPACE               TO ALL-SSA                                  
452200     STRING 'WDK711  (IDDCREF  =' W-IDDC-K7-X ')'                         
452300          DELIMITED BY SIZE INTO SSA1                                     
452400     MOVE '  GE' TO GODK-STATUSKODER                                      
452500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
452600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
452700     PERFORM IMS-STATUSKONTROLL                                           
452800     .                                                                    
452900                                                                          
453000 IMS-GU-WDK712 SECTION.                                                   
453100                                                                          
453200     MOVE 'IMS-GU-WDK712   '    TO CURRENT-IMS-SECTION                    
453300                                                                          
453400     MOVE SPACE                 TO ALL-SSA                                
453500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
453600          DELIMITED BY SIZE   INTO SSA1                                   
453700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
453800          DELIMITED BY SIZE   INTO SSA2                                   
453900     MOVE '  GE' TO GODK-STATUSKODER                                      
454000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
454100     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
454200     PERFORM IMS-STATUSKONTROLL                                           
454300     .                                                                    
454400                                                                          
454500 IMS-GU-WDK722 SECTION.                                                   
454600                                                                          
454700     MOVE 'IMS-GU-WDK722   '    TO CURRENT-IMS-SECTION                    
454800                                                                          
454900     MOVE SPACE                 TO ALL-SSA                                
455000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
455100          DELIMITED BY SIZE   INTO SSA1                                   
455200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
455300          DELIMITED BY SIZE   INTO SSA2                                   
455400     MOVE 'WDK722 '             TO SSA3                                   
455500     MOVE '  GE' TO GODK-STATUSKODER                                      
455600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
455700     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
455800     PERFORM IMS-STATUSKONTROLL                                           
455900     .                                                                    
456000                                                                          
456100 IMS-GNP-WDK722 SECTION.                                                  
456200                                                                          
456300     MOVE 'IMS-GNP-WDK722  '    TO CURRENT-IMS-SECTION                    
456400                                                                          
456500     MOVE SPACE                 TO ALL-SSA                                
456600     MOVE 'WDK722 '             TO SSA1                                   
456700     MOVE '  GE' TO GODK-STATUSKODER                                      
456800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
456900     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
457000     PERFORM IMS-STATUSKONTROLL                                           
457100     .                                                                    
457200                                                                          
457300 IMS-GHU-WDK722 SECTION.                                                  
457400                                                                          
457500     MOVE 'IMS-GHU-WDK722  '    TO CURRENT-IMS-SECTION                    
457600                                                                          
457700     MOVE SPACE                 TO ALL-SSA                                
457800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
457900          DELIMITED BY SIZE   INTO SSA1                                   
458000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
458100          DELIMITED BY SIZE   INTO SSA2                                   
458200     MOVE 'WDK722 '             TO SSA3                                   
458300     MOVE '  GE' TO GODK-STATUSKODER                                      
458400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
458500     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
458600     PERFORM IMS-STATUSKONTROLL                                           
458700     .                                                                    
458800                                                                          
458900 IMS-REPL-WDK722 SECTION.                                                 
459000                                                                          
459100     MOVE 'IMS-REPL-WDK722 '    TO CURRENT-IMS-SECTION                    
459200                                                                          
459300     MOVE SPACE                 TO ALL-SSA                                
459400     MOVE '    ' TO GODK-STATUSKODER                                      
459500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
459600     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
459700     PERFORM IMS-STATUSKONTROLL                                           
459800     .                                                                    
459900                                                                          
460000 IMS-GNP-WDK723 SECTION.                                                  
460100                                                                          
460200     MOVE 'IMS-GNP-WDK723  '    TO CURRENT-IMS-SECTION                    
460300                                                                          
460400     MOVE SPACE                 TO ALL-SSA                                
460500     MOVE 'WDK723 '             TO SSA1                                   
460600     MOVE '  GE' TO GODK-STATUSKODER                                      
460700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
460800     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
460900     PERFORM IMS-STATUSKONTROLL                                           
461000     .                                                                    
461100                                                                          
461200                                                                          
461300                                                                          
461400 IMS-GU-WDF101 SECTION.                                                   
461500                                                                          
461600     MOVE 'IMS-GU-WDF101   '    TO CURRENT-IMS-SECTION                    
461700                                                                          
461800     MOVE SPACE                 TO ALL-SSA                                
461900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
462000          DELIMITED BY SIZE   INTO SSA1                                   
462100     MOVE '  GE'                TO GODK-STATUSKODER                       
462200     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
462300     MOVE WDF1-STATUS-CODE      TO STATUS-WS                              
462400     PERFORM IMS-STATUSKONTROLL                                           
462500     .                                                                    
462600                                                                          
462700                                                                          
462800 IMS-GU-WDF106 SECTION.                                                   
462900                                                                          
463000     MOVE 'IMS-GU-WDF106   '    TO CURRENT-IMS-SECTION                    
463100                                                                          
463200     MOVE SPACE                 TO ALL-SSA                                
463300     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
463400          DELIMITED BY SIZE   INTO SSA1                                   
463500     STRING 'WDF106     '                                                 
463600          DELIMITED BY SIZE   INTO SSA2                                   
463700     MOVE '  GE' TO GODK-STATUSKODER                                      
463800     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF106 SSA1 SSA2               
463900     MOVE WDF1-STATUS-CODE      TO STATUS-WS                              
464000     PERFORM IMS-STATUSKONTROLL                                           
464100     .                                                                    
464200                                                                          
464300                                                                          
464400 IMS-GU-WDF116 SECTION.                                                   
464500                                                                          
464600     MOVE 'IMS-GU-WDF116   '    TO CURRENT-IMS-SECTION                    
464700                                                                          
464800     MOVE SPACE                 TO ALL-SSA                                
464900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
465000          DELIMITED BY SIZE   INTO SSA1                                   
465100     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
465200          DELIMITED BY SIZE   INTO SSA2                                   
465300     MOVE '  GE' TO GODK-STATUSKODER                                      
465400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
465500     MOVE WDF1-STATUS-CODE      TO STATUS-WS                              
465600     PERFORM IMS-STATUSKONTROLL                                           
465700     .                                                                    
465800                                                                          
465900                                                                          
466000                                                                          
466100 IMS-GU-WDF301 SECTION.                                                   
466200                                                                          
466300     MOVE 'IMS-GU-WDF301   '    TO CURRENT-IMS-SECTION                    
466400                                                                          
466500     MOVE SPACE                 TO ALL-SSA                                
466600     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
466700          DELIMITED BY SIZE   INTO SSA1                                   
466800     MOVE '  GE'                TO GODK-STATUSKODER                       
466900     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-WDF301 SSA1                    
467000     MOVE WDF3-STATUS-CODE      TO STATUS-WS                              
467100     PERFORM IMS-STATUSKONTROLL                                           
467200     .                                                                    
467300                                                                          
467400                                                                          
467500                                                                          
467600 IMS-GU-WDD311      SECTION.                                              
467700                                                                          
467800     MOVE 'IMS-GU-WDD311   '    TO CURRENT-IMS-SECTION                    
467900                                                                          
468000     MOVE SPACE                 TO ALL-SSA                                
468100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
468200          DELIMITED BY SIZE   INTO SSA1                                   
468300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
468400          DELIMITED BY SIZE   INTO SSA2                                   
468500     MOVE '  GE'                TO GODK-STATUSKODER                       
468600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
468700     MOVE WDD3-STATUS-CODE      TO STATUS-WS                              
468800     PERFORM IMS-STATUSKONTROLL                                           
468900     .                                                                    
469000                                                                          
469100                                                                          
469200 IMS-GU-WDD601        SECTION.                                            
469300                                                                          
469400     MOVE 'IMS-GU-WDD601   '    TO CURRENT-IMS-SECTION                    
469500                                                                          
469600     MOVE SPACE                 TO ALL-SSA                                
469700     STRING 'WDD601  (WDD601KY=>' W-WDD601KY-MIN ')'                      
469800                    '&WDD601KY=<' W-WDD601KY-MAX ')'                      
469900            DELIMITED BY SIZE INTO SSA1                                   
470000     MOVE '  GE'                TO GODK-STATUSKODER                       
470100     CALL CBLTDLI USING GU WDD6-PCB DLI-IO-WDD601 SSA1                    
470200     MOVE WDD6-STATUS-CODE      TO STATUS-WS                              
470300     PERFORM IMS-STATUSKONTROLL                                           
470400     .                                                                    
470500                                                                          
470600 IMS-GU-WDD601-DC-F   SECTION.                                            
470700                                                                          
470800     MOVE 'IMS-GU-WDD601-DC'    TO CURRENT-IMS-SECTION                    
470900                                                                          
471000     MOVE SPACE                 TO ALL-SSA                                
471100     MOVE 'WDD601 '             TO SSA1                                   
471200     MOVE '  GEGB'              TO GODK-STATUSKODER                       
471300     CALL CBLTDLI USING GU WDD6-PCB DLI-IO-WDD601 SSA1                    
471400     MOVE WDD6-STATUS-CODE      TO STATUS-WS                              
471500     PERFORM IMS-STATUSKONTROLL                                           
471600     .                                                                    
471700                                                                          
471800 IMS-GN-WDD601-DC     SECTION.                                            
471900                                                                          
472000     MOVE 'IMS-GN-WDD601-DC'    TO CURRENT-IMS-SECTION                    
472100                                                                          
472200     MOVE SPACE                 TO ALL-SSA                                
472300     STRING 'WDD601  (IDDC    =>' W-IDDC-D6-MIN                           
472400                    '&IDDC    =<' W-IDDC-D6-MAX ')'                       
472500            DELIMITED BY SIZE INTO SSA1                                   
472600     MOVE '  GEGB'              TO GODK-STATUSKODER                       
472700     CALL CBLTDLI USING GN WDD6-PCB DLI-IO-WDD601 SSA1                    
472800     MOVE WDD6-STATUS-CODE      TO STATUS-WS                              
472900     PERFORM IMS-STATUSKONTROLL                                           
473000     .                                                                    
473100                                                                          
473200 IMS-GHU-WDD601       SECTION.                                            
473300                                                                          
473400     MOVE 'IMS-GHU-WDD601  '    TO CURRENT-IMS-SECTION                    
473500                                                                          
473600     MOVE SPACE                 TO ALL-SSA                                
473700     STRING 'WDD601  (WDD601KY=>' W-WDD601KY-MIN                          
473800                    '&WDD601KY=<' W-WDD601KY-MAX ')'                      
473900            DELIMITED BY SIZE INTO SSA1                                   
474000     MOVE '  GE'                TO GODK-STATUSKODER                       
474100     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
474200     MOVE WDD6-STATUS-CODE      TO STATUS-WS                              
474300     PERFORM IMS-STATUSKONTROLL                                           
474400     .                                                                    
474500                                                                          
474600 IMS-DLET-WDD601      SECTION.                                            
474700                                                                          
474800     MOVE 'IMS-DLET-WDD601 '    TO CURRENT-IMS-SECTION                    
474900                                                                          
475000     MOVE SPACE                 TO ALL-SSA                                
475100     MOVE '    '                TO GODK-STATUSKODER                       
475200     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
475300     MOVE WDD6-STATUS-CODE      TO STATUS-WS                              
475400     PERFORM IMS-STATUSKONTROLL                                           
475500     .                                                                    
475600                                                                          
475700                                                                          
475800                                                                          
475900 IMS-GU-WDD7A1-MINMAX SECTION.                                            
476000                                                                          
476100     MOVE 'IMS-GU-WDD7A1-MM'    TO CURRENT-IMS-SECTION                    
476200                                                                          
476300     MOVE SPACE                 TO ALL-SSA                                
476400     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
476500                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
476600            DELIMITED BY SIZE INTO SSA1                                   
476700     MOVE '  GE' TO GODK-STATUSKODER                                      
476800     CALL CBLTDLI USING GU WDD7A1-PCB DLI-IO-WDD7A1 SSA1                  
476900     MOVE WDD7A1-STATUS-CODE    TO STATUS-WS                              
477000     PERFORM IMS-STATUSKONTROLL                                           
477100     .                                                                    
477200                                                                          
477300 IMS-GN-WDD7A1-MINMAX SECTION.                                            
477400                                                                          
477500     MOVE 'IMS-GN-WDD7A1-MM'    TO CURRENT-IMS-SECTION                    
477600                                                                          
477700     MOVE SPACE                 TO ALL-SSA                                
477800     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
477900                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
478000            DELIMITED BY SIZE INTO SSA1                                   
478100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
478200     CALL CBLTDLI USING GN WDD7A1-PCB DLI-IO-WDD7A1 SSA1                  
478300     MOVE WDD7A1-STATUS-CODE TO STATUS-WS                                 
478400     PERFORM IMS-STATUSKONTROLL                                           
478500     .                                                                    
478600                                                                          
478700                                                                          
478800 IMS-GU-WDD701      SECTION.                                              
478900                                                                          
479000     MOVE 'IMS-GU-WDD701   '    TO CURRENT-IMS-SECTION                    
479100                                                                          
479200     MOVE SPACE                 TO ALL-SSA                                
479300     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
479400            DELIMITED BY SIZE INTO SSA1                                   
479500     MOVE '  GE'                TO GODK-STATUSKODER                       
479600     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
479700     MOVE WDD7-STATUS-CODE      TO STATUS-WS                              
479800     PERFORM IMS-STATUSKONTROLL                                           
479900     .                                                                    
480000                                                                          
480100                                                                          
480200 IMS-GNP-WDD702      SECTION.                                             
480300                                                                          
480400     MOVE 'IMS-GNP-WDD702  '    TO CURRENT-IMS-SECTION                    
480500                                                                          
480600     MOVE SPACE                 TO ALL-SSA                                
480700     STRING 'WDD702  (FLTEXT   =N)'                                       
480800            DELIMITED BY SIZE INTO SSA1                                   
480900     MOVE '  GE'                TO GODK-STATUSKODER                       
481000     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
481100     MOVE WDD7-STATUS-CODE      TO STATUS-WS                              
481200     PERFORM IMS-STATUSKONTROLL                                           
481300     .                                                                    
481400                                                                          
481500                                                                          
481600                                                                          
481700 IMS-GU-WDD901 SECTION.                                                   
481800                                                                          
481900     MOVE 'IMS-GU-WDD901   '    TO CURRENT-IMS-SECTION                    
482000                                                                          
482100     MOVE SPACE                 TO ALL-SSA                                
482200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
482300          DELIMITED BY SIZE   INTO SSA1                                   
482400     MOVE '  GE'                TO GODK-STATUSKODER                       
482500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
482600     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
482700     PERFORM IMS-STATUSKONTROLL                                           
482800     .                                                                    
482900                                                                          
483000 IMS-ISRT-WDD901 SECTION.                                                 
483100                                                                          
483200     MOVE 'IMS-ISRT-WDD901 '    TO CURRENT-IMS-SECTION                    
483300                                                                          
483400     MOVE SPACE                 TO ALL-SSA                                
483500     MOVE 'WDD901   '           TO SSA1                                   
483600     MOVE '    '                TO GODK-STATUSKODER                       
483700     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD901 SSA1                  
483800     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
483900     PERFORM IMS-STATUSKONTROLL                                           
484000     .                                                                    
484100                                                                          
484200 IMS-GU-WDD902 SECTION.                                                   
484300                                                                          
484400     MOVE 'IMS-GU-WDD902   '    TO CURRENT-IMS-SECTION                    
484500                                                                          
484600     MOVE SPACE                 TO ALL-SSA                                
484700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
484800          DELIMITED BY SIZE   INTO SSA1                                   
484900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
485000          DELIMITED BY SIZE   INTO SSA2                                   
485100     MOVE '  GE'                TO GODK-STATUSKODER                       
485200     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
485300     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
485400     PERFORM IMS-STATUSKONTROLL                                           
485500     .                                                                    
485600                                                                          
485700 IMS-GHU-WDD902 SECTION.                                                  
485800                                                                          
485900     MOVE 'IMS-GHU-WDD902  '    TO CURRENT-IMS-SECTION                    
486000                                                                          
486100     MOVE SPACE                 TO ALL-SSA                                
486200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
486300          DELIMITED BY SIZE   INTO SSA1                                   
486400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
486500          DELIMITED BY SIZE   INTO SSA2                                   
486600     MOVE '  GE'                TO GODK-STATUSKODER                       
486700     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
486800     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
486900     PERFORM IMS-STATUSKONTROLL                                           
487000     .                                                                    
487100                                                                          
487200 IMS-GNP-WDD902 SECTION.                                                  
487300                                                                          
487400     MOVE 'IMS-GNP-WDD902  '    TO CURRENT-IMS-SECTION                    
487500                                                                          
487600     MOVE SPACE                 TO ALL-SSA                                
487700     MOVE 'WDD902 '             TO SSA1                                   
487800     MOVE '  GE'                TO GODK-STATUSKODER                       
487900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
488000     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
488100     PERFORM IMS-STATUSKONTROLL                                           
488200     .                                                                    
488300                                                                          
488400 IMS-ISRT-WDD902 SECTION.                                                 
488500                                                                          
488600     MOVE 'IMS-ISRT-WDD902 '    TO CURRENT-IMS-SECTION                    
488700                                                                          
488800     MOVE SPACE                 TO ALL-SSA                                
488900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
489000            DELIMITED BY SIZE INTO SSA1                                   
489100     MOVE 'WDD902   '           TO SSA2                                   
489200     MOVE '    '                TO GODK-STATUSKODER                       
489300     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
489400     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
489500     PERFORM IMS-STATUSKONTROLL                                           
489600     .                                                                    
489700                                                                          
489800 IMS-REPL-WDD902 SECTION.                                                 
489900                                                                          
490000     MOVE 'IMS-REPL-WDD902 '    TO CURRENT-IMS-SECTION                    
490100                                                                          
490200     MOVE SPACE                 TO ALL-SSA                                
490300     MOVE '    '                TO GODK-STATUSKODER                       
490400     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD902                       
490500     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
490600     PERFORM IMS-STATUSKONTROLL                                           
490700     .                                                                    
490800                                                                          
490900 IMS-GU-WDD904 SECTION.                                                   
491000                                                                          
491100     MOVE 'IMS-GU-WDD904   '    TO CURRENT-IMS-SECTION                    
491200                                                                          
491300     MOVE SPACE                 TO ALL-SSA                                
491400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
491500          DELIMITED BY SIZE   INTO SSA1                                   
491600     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
491700          DELIMITED BY SIZE   INTO SSA2                                   
491800     MOVE 'WDD904 '             TO SSA3                                   
491900     MOVE '  GE'                TO GODK-STATUSKODER                       
492000     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3          
492100     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
492200     PERFORM IMS-STATUSKONTROLL                                           
492300     .                                                                    
492400                                                                          
492500 IMS-GHU-WDD904 SECTION.                                                  
492600                                                                          
492700     MOVE 'IMS-GHU-WDD904  '    TO CURRENT-IMS-SECTION                    
492800                                                                          
492900     MOVE SPACE                 TO ALL-SSA                                
493000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
493100          DELIMITED BY SIZE   INTO SSA1                                   
493200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
493300          DELIMITED BY SIZE   INTO SSA2                                   
493400     MOVE 'WDD904 '             TO SSA3                                   
493500     MOVE '  GE'                TO GODK-STATUSKODER                       
493600     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
493700     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
493800     PERFORM IMS-STATUSKONTROLL                                           
493900     .                                                                    
494000                                                                          
494100 IMS-ISRT-WDD904 SECTION.                                                 
494200                                                                          
494300     MOVE 'IMS-ISRT-WDD904 '    TO CURRENT-IMS-SECTION                    
494400                                                                          
494500     MOVE SPACE                 TO ALL-SSA                                
494600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
494700            DELIMITED BY SIZE INTO SSA1                                   
494800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
494900            DELIMITED BY SIZE INTO SSA2                                   
495000     MOVE 'WDD904   '           TO SSA3                                   
495100     MOVE '    '                TO GODK-STATUSKODER                       
495200     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3        
495300     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
495400     PERFORM IMS-STATUSKONTROLL                                           
495500     .                                                                    
495600 IMS-REPL-WDD904 SECTION.                                                 
495700                                                                          
495800     MOVE 'IMS-REPL-WDD904 '    TO CURRENT-IMS-SECTION                    
495900                                                                          
496000     MOVE SPACE                 TO ALL-SSA                                
496100     MOVE '    '                TO GODK-STATUSKODER                       
496200     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD904                       
496300     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
496400     PERFORM IMS-STATUSKONTROLL                                           
496500     .                                                                    
496600                                                                          
496700 IMS-DLET-WDD904 SECTION.                                                 
496800                                                                          
496900     MOVE 'IMS-DLET-WDD904 '    TO CURRENT-IMS-SECTION                    
497000                                                                          
497100     MOVE SPACE                 TO ALL-SSA                                
497200     MOVE '  GE'                TO GODK-STATUSKODER                       
497300     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
497400     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
497500     PERFORM IMS-STATUSKONTROLL                                           
497600     .                                                                    
497700                                                                          
497800 IMS-GU-WDD905 SECTION.                                                   
497900                                                                          
498000     MOVE 'IMS-GU-WDD905   '    TO CURRENT-IMS-SECTION                    
498100                                                                          
498200     MOVE SPACE                 TO ALL-SSA                                
498300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
498400          DELIMITED BY SIZE   INTO SSA1                                   
498500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
498600          DELIMITED BY SIZE   INTO SSA2                                   
498700     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X                             
498800                    '&KDAVROP  =' W-KDAVROP-X ')'                         
498900          DELIMITED BY SIZE   INTO SSA3                                   
499000     MOVE '  GE'                TO GODK-STATUSKODER                       
499100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
499200     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
499300     PERFORM IMS-STATUSKONTROLL                                           
499400     .                                                                    
499500                                                                          
499600 IMS-GNP-WDD905 SECTION.                                                  
499700                                                                          
499800     MOVE 'IMS-GNP-WDD905  '    TO CURRENT-IMS-SECTION                    
499900                                                                          
500000     MOVE SPACE                 TO ALL-SSA                                
500100     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
500200          DELIMITED BY SIZE   INTO SSA1                                   
500300     MOVE 'WDD905 '             TO SSA2                                   
500400     MOVE '  GE'                TO GODK-STATUSKODER                       
500500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
500600     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
500700     PERFORM IMS-STATUSKONTROLL                                           
500800     .                                                                    
500900                                                                          
501000 IMS-GHNP-WDD905-OKVAL SECTION.                                           
501100                                                                          
501200     MOVE 'IMS-GHNP-WDD905O'    TO CURRENT-IMS-SECTION                    
501300                                                                          
501400     MOVE SPACE                 TO ALL-SSA                                
501500     MOVE 'WDD905 '         TO SSA1                                       
501600     MOVE '  GE'            TO GODK-STATUSKODER                           
501700     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
501800     MOVE WDD9-STATUS-CODE  TO STATUS-WS                                  
501900     PERFORM IMS-STATUSKONTROLL                                           
502000     .                                                                    
502100                                                                          
502200 IMS-GHNP-WDD905 SECTION.                                                 
502300                                                                          
502400     MOVE 'IMS-GHNP-WDD905 '    TO CURRENT-IMS-SECTION                    
502500                                                                          
502600     MOVE SPACE                 TO ALL-SSA                                
502700     STRING 'WDD905  (DAAVROP >=' W-DAAVROP-X ')'                         
502800          DELIMITED BY SIZE   INTO SSA1                                   
502900     MOVE '  GE'                TO GODK-STATUSKODER                       
503000     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
503100     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
503200     PERFORM IMS-STATUSKONTROLL                                           
503300     .                                                                    
503400                                                                          
503500 IMS-GHNP-WDD905-VECKA SECTION.                                           
503600                                                                          
503700     MOVE 'IMS-GHNP-WDD905V'    TO CURRENT-IMS-SECTION                    
503800                                                                          
503900     MOVE SPACE                 TO ALL-SSA                                
504000     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
504100          DELIMITED BY SIZE   INTO SSA1                                   
504200     MOVE '  GE'                TO GODK-STATUSKODER                       
504300     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
504400     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
504500     PERFORM IMS-STATUSKONTROLL                                           
504600     .                                                                    
504700                                                                          
504800 IMS-ISRT-WDD905 SECTION.                                                 
504900                                                                          
505000     MOVE 'IMS-ISRT-WDD905 '    TO CURRENT-IMS-SECTION                    
505100                                                                          
505200     MOVE SPACE                 TO ALL-SSA                                
505300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
505400            DELIMITED BY SIZE INTO SSA1                                   
505500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
505600            DELIMITED BY SIZE INTO SSA2                                   
505700     MOVE 'WDD905   '           TO SSA3                                   
505800     MOVE '    '                TO GODK-STATUSKODER                       
505900     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
506000     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
506100     PERFORM IMS-STATUSKONTROLL                                           
506200     .                                                                    
506300                                                                          
506400 IMS-REPL-WDD905 SECTION.                                                 
506500                                                                          
506600     MOVE 'IMS-REPL-WDD905 '    TO CURRENT-IMS-SECTION                    
506700                                                                          
506800     MOVE SPACE                 TO ALL-SSA                                
506900     MOVE '    '                TO GODK-STATUSKODER                       
507000     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
507100     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
507200     PERFORM IMS-STATUSKONTROLL                                           
507300     .                                                                    
507400                                                                          
507500 IMS-DLET-WDD905 SECTION.                                                 
507600                                                                          
507700     MOVE 'IMS-DLET-WDD905 '    TO CURRENT-IMS-SECTION                    
507800                                                                          
507900     MOVE SPACE                 TO ALL-SSA                                
508000     MOVE '    '                TO GODK-STATUSKODER                       
508100     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
508200     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
508300     PERFORM IMS-STATUSKONTROLL                                           
508400     .                                                                    
508500                                                                          
508600                                                                          
508700                                                                          
508800 IMS-GU-WDB601  SECTION.                                                  
508900                                                                          
509000     MOVE 'IMS-GU-WDB601   '    TO CURRENT-IMS-SECTION                    
509100                                                                          
509200     MOVE SPACE                 TO ALL-SSA                                
509300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
509400            DELIMITED BY SIZE INTO SSA1                                   
509500     MOVE '  GE'                TO GODK-STATUSKODER                       
509600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
509700     MOVE WDB6-STATUS-CODE      TO STATUS-WS                              
509800     PERFORM IMS-STATUSKONTROLL                                           
509900     .                                                                    
510000                                                                          
510100 IMS-ISRT-WDG302  SECTION.                                                
510200                                                                          
510300     MOVE 'IMS-ISRT-WDG302 '    TO CURRENT-IMS-SECTION                    
510400                                                                          
510500     MOVE SPACE                 TO ALL-SSA                                
510600     STRING 'WDG301  (WDG3KEY  =' W-WDG301KY-X ')'                        
510700            DELIMITED BY SIZE INTO SSA1                                   
510800     MOVE   'WDG302  '          TO SSA2                                   
510900     MOVE '    '                TO GODK-STATUSKODER                       
511000     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDG302 SSA1 SSA2             
511100     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
511200     PERFORM IMS-STATUSKONTROLL                                           
511300     .                                                                    
511400                                                                          
511500                                                                          
511600 IMS-GU-WDGX2206 SECTION.                                                 
511700                                                                          
511800     MOVE 'IMS-GU-WDGX2206 '    TO CURRENT-IMS-SECTION                    
511900                                                                          
512000     MOVE SPACE               TO ALL-SSA                                  
512100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
512200          DELIMITED BY SIZE INTO SSA1                                     
512300     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
512400          DELIMITED BY SIZE INTO SSA2                                     
512500     MOVE '  GE'              TO GODK-STATUSKODER                         
512600     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2206 SSA1 SSA2             
512700     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
512800     PERFORM IMS-STATUSKONTROLL                                           
512900     .                                                                    
513000     SKIP3                                                                
513100 IMS-ISRT-WDGX2248 SECTION.                                               
513200                                                                          
513300     MOVE 'IMS-ISRT-WDGX2248'   TO CURRENT-IMS-SECTION                    
513400                                                                          
513500     MOVE SPACE               TO ALL-SSA                                  
513600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X ')'                    
513700          DELIMITED BY SIZE INTO SSA1                                     
513800     MOVE   'WDGX2248'        TO SSA2                                     
513900     MOVE '  II'              TO GODK-STATUSKODER                         
514000     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2248 SSA1 SSA2           
514100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
514200     PERFORM IMS-STATUSKONTROLL                                           
514300     .                                                                    
514400     EJECT                                                                
514500 IMS-ISRT-WDGX2248-PERIOD SECTION.                                        
514600                                                                          
514700     MOVE 'IMS-ISRT-WDGX2248-PERIOD'   TO CURRENT-IMS-SECTION             
514800                                                                          
514900     MOVE SPACE               TO ALL-SSA                                  
515000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-P-X ')'                  
515100          DELIMITED BY SIZE INTO SSA1                                     
515200     MOVE   'WDGX2248'        TO SSA2                                     
515300     MOVE '  II'              TO GODK-STATUSKODER                         
515400     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2248 SSA1 SSA2           
515500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
515600     PERFORM IMS-STATUSKONTROLL                                           
515700     .                                                                    
515800     EJECT                                                                
515900 IMS-GHU-WDGX2262 SECTION.                                                
516000                                                                          
516100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
516200          DELIMITED BY SIZE INTO SSA1                                     
516300     STRING 'WDGX2262(IDARTNR  =' W-IDARTNR-X ')'                         
516400          DELIMITED BY SIZE INTO SSA2                                     
516500     MOVE '  GE' TO GODK-STATUSKODER                                      
516600     CALL CBLTDLI USING GHU 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2            
516700     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
516800     PERFORM IMS-STATUSKONTROLL                                           
516900     .                                                                    
517000     SKIP3                                                                
517100 IMS-ISRT-WDGX2262   SECTION.                                             
517200                                                                          
517300     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
517400          DELIMITED BY SIZE INTO SSA1                                     
517500     MOVE 'WDGX2262 ' TO SSA2                                             
517600     MOVE '  ' TO GODK-STATUSKODER                                        
517700     CALL CBLTDLI USING ISRT 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2           
517800     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
517900     PERFORM IMS-STATUSKONTROLL                                           
518000     .                                                                    
518100     SKIP3                                                                
518200 IMS-REPL-WDGX2262 SECTION.                                               
518300                                                                          
518400     MOVE '  ' TO GODK-STATUSKODER                                        
518500     CALL CBLTDLI USING REPL 2261-PCB DLI-IO-WDGX2262                     
518600     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
518700     PERFORM IMS-STATUSKONTROLL                                           
518800     .                                                                    
518900     EJECT                                                                
519000 IMS-STATUSKONTROLL SECTION.                                              
519100                                                                          
519200     SET STATUS-IX TO 1                                                   
519300     SEARCH GODK-STATUS                                                   
519400       AT END                                                             
519500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
519600         DELIMITED BY SIZE INTO FELTEXT                                   
519700         CALL FELLOG                                                      
519800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
519900         CONTINUE                                                         
520000     END-SEARCH                                                           
520100     .                                                                    
520200     EJECT                                                                
520300*    -COPY WY2000P3                                                       

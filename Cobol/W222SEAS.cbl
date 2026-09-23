000110*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W222SEAS                                                 
000400 AUTHOR.         STEFAN ANDREASSON.                                       
000500 DATE-WRITTEN.   SEPTEMBER 1999.                                          
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        SUBPROGRAM FÖR ATT LÄSA ORDERSTATISTIK OCH                       
001000*        RÄKNA UT SÄSONGSINDEX SAMT TA FRAM EN                            
001100*        OSÄKERHETSFAKTOR                                                 
001200*                                                                         
001300*    INDATA.                                                              
001400*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001500*          W222SEAS                                                       
001600*          WDL8-PCB                                                       
001700*                                                                         
001800*    UTDATA.                                                              
001900*        W222SEAS                                                         
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(8)    VALUE 'W222SEAS'.            
002800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003010                                                                          
003020 77  KVARTAL-SW                  PIC X       VALUE 'J'.                   
003030     88  KVARTAL                             VALUE 'J'.                   
003040     88  EJ-KVARTAL                          VALUE 'N'.                   
003050                                                                          
003100 01  DAGENS-DATUM                PIC 9(6).                                
003200 01  FILLER                      REDEFINES DAGENS-DATUM.                  
003300   03 DAGENS-AA                  PIC 9(2).                                
003400   03 DAGENS-MM                  PIC 9(2).                                
003500   03 DAGENS-DD                  PIC 9(2).                                
003510                                                                          
003520 01 DAGENS-TISSAAMMDD            PIC 9(8)       VALUE ZERO.               
003530 01 DAGENS-TISSAAMMDD-GRP        REDEFINES DAGENS-TISSAAMMDD.             
003540   03 DAGENS-TISS                PIC 9(2).                                
003550   03 DAGENS-TIAAMMDD            PIC 9(6).                                
003551 01 DAGENS-TIVV                  PIC 9(2).                                
003560                                                                          
003600 01  WS-ARBETSFAELT.                                                      
003610   03 WS-NOLL                    PIC 9(10)   VALUE ZERO.                  
003700   03 WS-TIME-START              PIC 9(10)   VALUE ZERO.                  
003800   03 WS-TIME-SLUT               PIC 9(10)   VALUE ZERO.                  
003900   03 WS-AAPP                    PIC 9(4)    VALUE ZERO.                  
004000   03 FILLER REDEFINES           WS-AAPP.                                 
004100    05 WS-AA                     PIC 9(2).                                
004200    05 WS-PP                     PIC 9(2).                                
004300   03 WS-KVOI-RED                PIC S9(9)V99 COMP-3 VALUE ZERO.          
004400   03 WS-PER                     PIC  9(2)    VALUE ZERO.                 
004500   03 WS-VV                      PIC  9(2)   VALUE ZERO.                  
004600   03 WS-TABELL    OCCURS 12.                                             
004700    05 WS-FORSTA-V               PIC  9(2)   VALUE ZERO.                  
004800    05 WS-SISTA-V                PIC  9(2)   VALUE ZERO.                  
004900    05 WS-KVVIPER                PIC 9       VALUE ZERO.                  
005000    05 WS-KVOI                   PIC S9(9)   VALUE ZERO COMP-3.           
005100                                                                          
005200   03  WS-ARS-FORBRUKN.                                                   
005210    04 FILLER                    OCCURS 6.                                
005300     05  WS-KVOI-PER             OCCURS 12.                               
005400       07  WS-KVOI-CDC           PIC S9(9)   COMP-3  VALUE ZERO.          
005500       07  WS-KVOI-TOT-AR        PIC S9(9)   COMP-3  VALUE ZERO.          
005600       07  WS-RESEASON-PER       PIC S9(5)V9(3)                           
005700                                             COMP-3  VALUE ZERO.          
005710   03  WS-NOLLA-ARS-FORBRUKN.                                             
005720    04 FILLER                    OCCURS 6.                                
005730     05  FILLER                  OCCURS 12.                               
005740       07  FILLER                PIC S9(9)   COMP-3  VALUE ZERO.          
005750       07  FILLER                PIC S9(9)   COMP-3  VALUE ZERO.          
005760       07  FILLER                PIC S9(5)V9(3)                           
005770                                             COMP-3  VALUE ZERO.          
005800   03  WS-KVOI-TOTALT-2AR        PIC S9(9)   COMP-3  VALUE ZERO.          
005900   03  WS-KVOI-TOTALT            PIC S9(9)   COMP-3  VALUE ZERO.          
006000   03  WS-KVOI-MEDEL-PER         PIC S9(9)   COMP-3  VALUE ZERO.          
006100   03  WS-RESEASON-AVR           OCCURS 12                                
006210                                 PIC S9(5)   COMP-3  VALUE ZERO.          
006300*****                                                                     
006400*****  WS-RESEASON-JUST (MED EN DECIMAL) BEHÖVS                           
006500*****  VID UTRÄKNING AV OSÄKERHETSFAKTORN                                 
006600*****                                                                     
006700   03  WS-RESEASON-JUST          OCCURS 12                                
006810                                 PIC S9(5)V9 COMP-3  VALUE ZERO.          
006900   03  WS-RESEASON-SNITT         OCCURS 12                                
007000                                 PIC S9(5)V9 COMP-3  VALUE ZERO.          
007100   03  WS-RESEASON-SUM           PIC S9(5)V9 COMP-3  VALUE ZERO.          
007200   03  WS-RESEASON-TOTAL         PIC S9(5)   COMP-3  VALUE ZERO.          
007300   03  WS-RESEASON-TOT-AVR       PIC S9(5)   COMP-3  VALUE ZERO.          
007310   03  WS-MAX1                   PIC S9(5)   COMP-3  VALUE ZERO.          
007320   03  WS-MAX2                   PIC S9(5)   COMP-3  VALUE ZERO.          
007330   03  WS-MAX3                   PIC S9(5)   COMP-3  VALUE ZERO.          
007340   03  WS-MIN1                   PIC S9(5)   COMP-3  VALUE ZERO.          
007350   03  WS-MIN2                   PIC S9(5)   COMP-3  VALUE ZERO.          
007360   03  WS-MIN3                   PIC S9(5)   COMP-3  VALUE ZERO.          
007400   03  WS-KVOI-SNITT             OCCURS 12                                
007500                                 PIC S9(9)   COMP-3  VALUE ZERO.          
007600   03  WS-KVOI-SUM               PIC S9(9)   COMP-3  VALUE ZERO.          
007601   03  WS-KVOI-RULL-TOT          PIC S9(9)   COMP-3  VALUE ZERO.          
007700   03  WS-SKILLNAD               PIC S9(3)V9(3)                           
007800                                             COMP-3  VALUE ZERO.          
007900   03  WS-KVADRAT                PIC S9(9)V9(4)                           
008000                                             COMP-3  VALUE ZERO.          
008100   03  WS-KVADRAT-SUM            PIC S9(9)V9(4)                           
008200                                             COMP-3  VALUE ZERO.          
008300   03  WS-OSAKERHET              PIC S9(9)V9(4)                           
008400                                             COMP-3  VALUE ZERO.          
008500   03  WS-ANTAL-HIST-AR          PIC S9(3)   COMP-3  VALUE ZERO.          
008510   03  WS-ANTAL-HIST-MAN         PIC S9(3)   COMP-3  VALUE ZERO.          
008600   03  WS-JUSTERA                PIC S9V9(2) COMP-3  VALUE ZERO.          
008610   03  WS-GRAENS-OI              PIC 9(5)            VALUE ZERO.          
008700   03  WS-NOLLSTAELL-PARM.                                                
008800     05 FILLER                   OCCURS 12 TIMES                          
008900                                 PIC 9(4)            VALUE ZERO.          
009000*                                 SÄSONGSINDEX                            
009100     05 FILLER                   OCCURS 12 TIMES                          
009200                                 PIC S9(7)   COMP-3  VALUE ZERO.          
009300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009400     05 FILLER                   PIC 9               VALUE ZERO.          
009500     05 FILLER                   PIC 9(3)V9(1)       VALUE ZERO.          
009510                                                                          
009513                                                                          
009514 01  TAB-A-P-TRAFF.                                                       
009515     03  TAB-A             OCCURS 6.                                      
009516         05  TAB-P         OCCURS 12.                                     
009517             07  TAB-A-P   PIC 99.                                        
009518         05  TAB-PSUM      PIC 99.                                        
009519 01  TABIX.                                                               
009520     03  AX                PIC 9(3)  COMP-3.                              
009521     03  PX                PIC 9(3)  COMP-3.                              
009522     03  VX                PIC 9(3)  COMP-3.                              
009523     03  ASUM              PIC 9(3)  COMP-3.                              
009524     EJECT                                                                
009525 01  ARBETSAREA-WDL811           PIC X(24)   VALUE                        
009526                                             'ARBETSAREA-WDL811'.         
009527     SKIP2                                                                
009528                                                                          
009529 01  TAB-AREA-WDL811.                                                     
009530     03 FILLER                   OCCURS 6.                                
009531*       05 AREA -COPY WDL811      -PRE TAB-                               
009532*                                                                         
009540                                                                          
009600 01  IX                          PIC S9(3)           VALUE ZERO.          
009700 01  IX-AR                       PIC S9(3)           VALUE ZERO.          
009710 01  IX-VECKA                    PIC S9(3)           VALUE ZERO.          
009800 01  IX-FRAN-AR                  PIC S9(3)           VALUE ZERO.          
009900 01  IX-PER                      PIC S9(3)           VALUE ZERO.          
010000 01  IX-START-AR                 PIC S9(3)           VALUE ZERO.          
010100 01  IX-START-PER                PIC S9(3)           VALUE ZERO.          
010200 01  IX-SLUT-AR                  PIC S9(3)           VALUE ZERO.          
010300 01  IX-SLUT-PER                 PIC S9(3)           VALUE ZERO.          
010400 01  IX-SEASON-AR                PIC S9(3)           VALUE ZERO.          
010500 01  IX-SEASON-PER               PIC S9(3)           VALUE ZERO.          
010600 01  IX-ARB1-AR                  PIC S9(3)           VALUE ZERO.          
010700 01  IX-ARB1-PER                 PIC S9(3)           VALUE ZERO.          
010800 01  IX-ARB2-AR                  PIC S9(3)           VALUE ZERO.          
010900 01  IX-ARB2-PER                 PIC S9(3)           VALUE ZERO.          
011000 01  IX-HIST-AR                  PIC S9(3)           VALUE ZERO.          
011100 01  IX-HIST-PER                 PIC S9(3)           VALUE ZERO.          
011200 01  IX-TEMP-AR                  PIC S9(3)           VALUE ZERO.          
011300 01  IX-TEMP-PER                 PIC S9(3)           VALUE ZERO.          
011400 01  IX-RULL                     PIC S9(3)           VALUE ZERO.          
011500 01  IX-ANTAL                    PIC S9(3)           VALUE ZERO.          
011600 01  IX-HAMTA-AR                 PIC S9(3)           VALUE ZERO.          
011739                                                                          
011740     SKIP3                                                                
011741*      --- VALID IDDC CODES                                               
011742*                                                                         
011743*01    -COPY WWDC99                                                       
011750       EJECT                                                              
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
012000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
012100   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012400*01  -COPY WDATAREA                                                       
012500     EJECT                                                                
012600******************************************************************        
012700*                                                                         
012800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012900*                                                                         
013000 01  IMS-WS.                                                              
013100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
013200     SKIP3                                                                
013300*                        **** STATUS-KOD FRÅN IMS                         
013400   03  STATUS-WS                 PIC XX.                                  
013500     88  SEGMENT-FINNS                       VALUE '  '.                  
013600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013700     88  INSERTEN-OK                         VALUE '  '.                  
013800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013900                                                                          
014000   03  GODK-STATUSKODER.                                                  
014100     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  NYCKLAR-TILL-DLI.                                                    
014400     03  W-IDARTNR-X.                                                     
014500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014600     03  W-IDDC-X.                                                        
014700         05  W-IDDC              PIC X(02)    VALUE SPACE.                
014710     03  W-TIAAAA-X.                                                      
014720         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
014800     SKIP3                                                                
014900 01  SSA1                        PIC X(64).                               
015000 01  SSA2                        PIC X(64).                               
015100     EJECT                                                                
016210*    ---  DLI INPUT-OUTPUT AREA                                           
016293 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL801'.             
016294     SKIP3                                                                
016295 01  DLI-IO-AREA-WDL801.                                                  
016296*        05  -COPY WDL801                                                 
016297     EJECT                                                                
016298 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
016299     SKIP3                                                                
016300 01  DLI-IO-AREA-WDL811.                                                  
016301*        05  -COPY WDL811                                                 
016310     EJECT                                                                
016400*                            IMS FUNKTIONSKODER                           
016500*01    -COPY W0003                                                        
016501     EJECT                                                                
016510 LINKAGE SECTION.                                                         
016520     EJECT                                                                
016530*01  -COPY W222SEAS                                                       
016540     EJECT                                                                
016550*01  -COPY W0008 -PRE WDL8-                                               
016560     05  FILLER                  PIC X.                                   
016570     EJECT                                                                
016580 PROCEDURE DIVISION USING  SEAS-W222SEAS                                  
016590                           WDL8-PCB.                                      
016600     EJECT                                                                
017500 STYR SECTION.                                                            
017600                                                                          
017700     PERFORM A-INIT                                                       
017800     IF SEAS-KDSVAR = SPACE                                               
017900       PERFORM B-BEARBETA                                                 
017910       PERFORM F-KOLL-3-PER-AR                                            
018000     ELSE                                                                 
018100       MOVE WS-NOLLSTAELL-PARM                                            
018200                             TO SEAS-UTDATA                               
018300     END-IF                                                               
018310     MOVE SPACE              TO SEAS-KDSVAR                               
018400     ACCEPT WS-TIME-SLUT  FROM TIME                                       
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 A-INIT SECTION.                                                          
019100                                                                          
019110     MOVE SPACE              TO SEAS-KDSVAR                               
019200     PERFORM AB-NOLLSTAELL                                                
019300                                                                          
019400     ACCEPT DAGENS-DATUM FROM DATE                                        
019500     ACCEPT WS-TIME-START FROM TIME                                       
019600                                                                          
019700     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
019800     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
019900                                                                          
020000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020100                     DAT-O-TIDATUM DAT-KDSVAR                             
020200                                                                          
020300     IF DAT-KDSVAR-OK                                                     
020400                                                                          
020500        MOVE DAT-TIAARP(3:2) TO WS-PER                                    
020510        MOVE DAT-TISEKEL     TO DAGENS-TISS                               
020520        MOVE DAT-TIAAMMDD    TO DAGENS-TIAAMMDD                           
020530        MOVE DAT-TIVV        TO DAGENS-TIVV                               
020600                                                                          
020700     ELSE                                                                 
020800         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
020900         DELIMITED BY SIZE INTO FELTEXT                                   
021000         CALL FELLOG                                                      
021100     END-IF                                                               
021200                                                                          
021201     PERFORM AA-HAMTA-VV-I-PER                                            
021210     MOVE SEAS-FLKVARTAL     TO KVARTAL-SW                                
021300     MOVE SEAS-IDARTNR       TO W-IDARTNR                                 
021400     MOVE DAGENS-TISSAAMMDD (1:4)                                         
021401                             TO W-TIAAAA                                  
021412     MOVE 1                  TO IX                                        
021420                                                                          
021500     PERFORM UNTIL IX > 6                                                 
021710                                                                          
021720       PERFORM IMS-GU-L811                                                
021810       IF SEGMENT-FINNS                                                   
021820          MOVE AAR-WDL811    TO TAB-AAR-WDL811 (IX)                       
022400       END-IF                                                             
022410       ADD 1                 TO IX                                        
022420       SUBTRACT 1            FROM W-TIAAAA                                
022430     END-PERFORM                                                          
022500     .                                                                    
022600     EJECT                                                                
022700 AA-HAMTA-VV-I-PER SECTION.                                               
022800                                                                          
022900*    --- FYLL I VECKONR FÖR PERIODERNA                                    
023000     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
023100     MOVE 01                 TO WS-AAPP(3:2)                              
023200     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
023300     MOVE 'AARP'             TO DAT-KDDATFORM                             
023400     MOVE +1                 TO WS-PP                                     
023500                                                                          
023600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
023700                     DAT-O-TIDATUM DAT-KDSVAR                             
023800                                                                          
023900     IF DAT-KDSVAR-OK                                                     
024000                                                                          
024100       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
024200       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
024300                                                                          
024400     ELSE                                                                 
024500         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
024600         DELIMITED BY SIZE INTO FELTEXT                                   
024700         CALL FELLOG                                                      
024800     END-IF                                                               
024900                                                                          
025000     PERFORM UNTIL WS-PP      >  12                                       
025100       ADD +1                 TO WS-PP                                    
025200       IF WS-PP = +13                                                     
025300         MOVE 52             TO WS-SISTA-V(12)                            
025400       ELSE                                                               
025500                                                                          
025600         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
025700         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
025800                             DAT-O-TIDATUM DAT-KDSVAR                     
025900         IF DAT-KDSVAR-OK                                                 
026000             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
026100             IF WS-PP > +1                                                
026200               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
026300               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
026400             END-IF                                                       
026500         ELSE                                                             
026600           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
026700           CALL FELLOG                                                    
026800         END-IF                                                           
026900       END-IF                                                             
027000     END-PERFORM                                                          
027100     .                                                                    
027200     EJECT                                                                
027300 AB-NOLLSTAELL SECTION.                                                   
027400                                                                          
027500     MOVE +1                 TO IX-PER                                    
027600     PERFORM UNTIL IX-PER > +12                                           
027700       MOVE ZERO             TO WS-FORSTA-V (IX-PER)                      
027800                                WS-SISTA-V (IX-PER)                       
027900                                WS-KVVIPER (IX-PER)                       
028000                                WS-KVOI (IX-PER)                          
028100                                WS-RESEASON-AVR (IX-PER)                  
028200                                WS-RESEASON-JUST (IX-PER)                 
028300                                WS-RESEASON-SNITT (IX-PER)                
028400                                WS-KVOI-SNITT (IX-PER)                    
028500       ADD +1                TO IX-PER                                    
028600     END-PERFORM                                                          
028700                                                                          
028710                                                                          
028711     MOVE WS-NOLLA-ARS-FORBRUKN                                           
028712                             TO WS-ARS-FORBRUKN                           
028730                                                                          
028800     MOVE +1                 TO IX-AR                                     
028900     PERFORM UNTIL IX-AR > +6                                             
029000       MOVE ZERO             TO TAB-AAR-TIAAAA (IX-AR)                    
029010       MOVE +1               TO IX-VECKA                                  
029100       PERFORM UNTIL IX-VECKA > +53                                       
029200         MOVE ZERO        TO TAB-AAR-KVOI-DIV (IX-AR, IX-VECKA)           
029300                             TAB-AAR-KVOI-NDC (IX-AR, IX-VECKA)           
029400                             TAB-AAR-KVOI-PROG (IX-AR, IX-VECKA)          
029401                             TAB-AAR-KVOI-REFILL (IX-AR, IX-VECKA)        
029402                             TAB-AAR-KVOI-SATS (IX-AR, IX-VECKA)          
029403                             TAB-AAR-KVOI-SDC (IX-AR, IX-VECKA)           
029404                             TAB-AAR-KVOI-LEDTID (IX-AR, IX-VECKA)        
029405                             TAB-AAR-KVOT-DIV (IX-AR, IX-VECKA)           
029406                             TAB-AAR-KVOT-PROG (IX-AR, IX-VECKA)          
029500         ADD +1              TO IX-VECKA                                  
029600       END-PERFORM                                                        
029700       ADD +1                TO IX-AR                                     
029800     END-PERFORM                                                          
029900                                                                          
030000     MOVE ZERO               TO WS-AAPP                                   
030100                                WS-KVOI-RED                               
030200                                WS-PER                                    
030300                                WS-VV                                     
030400                                WS-KVOI-TOTALT-2AR                        
030500                                WS-KVOI-TOTALT                            
030600                                WS-KVOI-MEDEL-PER                         
030700                                WS-RESEASON-SUM                           
030800                                WS-RESEASON-TOTAL                         
030900                                WS-RESEASON-TOT-AVR                       
031000                                WS-KVOI-SUM                               
031100                                WS-SKILLNAD                               
031200                                WS-KVADRAT                                
031300                                WS-KVADRAT-SUM                            
031400                                WS-OSAKERHET                              
031500                                WS-ANTAL-HIST-AR                          
031510                                WS-ANTAL-HIST-MAN                         
031600                                WS-JUSTERA                                
031700                                IX                                        
031800                                IX-AR                                     
031900                                IX-FRAN-AR                                
032000                                IX-PER                                    
032100                                IX-START-AR                               
032200                                IX-START-PER                              
032300                                IX-SLUT-AR                                
032400                                IX-SLUT-PER                               
032500                                IX-SEASON-AR                              
032600                                IX-SEASON-PER                             
032700                                IX-ARB1-AR                                
032800                                IX-ARB1-PER                               
032900                                IX-ARB2-AR                                
033000                                IX-ARB2-PER                               
033100                                IX-HIST-AR                                
033200                                IX-HIST-PER                               
033300                                IX-TEMP-AR                                
033400                                IX-TEMP-PER                               
033500                                IX-RULL                                   
033600                                IX-ANTAL                                  
033700                                IX-HAMTA-AR                               
033770                                                                          
033800     .                                                                    
033900     EJECT                                                                
034000 B-BEARBETA SECTION.                                                      
034100                                                                          
034200****                                                                      
034300****                                                                      
034400***************  TA REDA PÅ HUR MYCKET HISTORIK SOM FINNS                 
034500****                                                                      
034600****             < 1 ÅR ELLER                                             
034700****               1 ÅR ELLER                                             
034800****         MINST 2 ÅR                                                   
034900****                                                                      
035000                                                                          
035010     IF KVARTAL                                                           
035020     AND WS-PER = 12                                                      
035030       MOVE +6               TO WS-ANTAL-HIST-AR                          
035031       MOVE 72               TO WS-ANTAL-HIST-MAN                         
035040     ELSE                                                                 
035050       MOVE +5               TO WS-ANTAL-HIST-AR                          
035051       IF KVARTAL                                                         
035052         COMPUTE WS-ANTAL-HIST-MAN = 60 + WS-PER                          
035053       ELSE                                                               
035054         COMPUTE WS-ANTAL-HIST-MAN = 60 + (WS-PER - 1)                    
035055       END-IF                                                             
035060     END-IF                                                               
035070                                                                          
035100     MOVE +6                 TO IX-AR                                     
035300     MOVE +1                 TO IX-PER                                    
035310                                IX-VECKA                                  
035400     MOVE ZERO               TO IX-HIST-AR                                
035500                                IX-HIST-PER                               
035510                                                                          
035520     PERFORM UNTIL IX-VECKA > WS-SISTA-V (IX-PER)                         
035521     OR TAB-AAR-KVOI-PROG (IX-AR, IX-VECKA) > ZERO                        
035522        ADD +1               TO IX-VECKA                                  
035523     END-PERFORM                                                          
035530                                                                          
035600     PERFORM UNTIL IX-AR < +1                                             
035710     OR TAB-AAR-KVOI-PROG (IX-AR, IX-VECKA) > ZERO                        
035800                                                                          
035900       PERFORM UNTIL IX-PER > +12                                         
036000       OR TAB-AAR-KVOI-PROG (IX-AR, IX-VECKA) > ZERO                      
036001                                                                          
036010         IF KVARTAL                                                       
036020                                                                          
036030****                                                                      
036040****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
036050****                                                                      
036060                                                                          
036070           IF (WS-PER = 12                                                
036080           AND IX-PER = 1)                                                
036090           OR IX-PER = WS-PER + 1                                         
036300              SUBTRACT +1    FROM WS-ANTAL-HIST-AR                        
036400           END-IF                                                         
036401         ELSE                                                             
036420           IF IX-PER = WS-PER                                             
036430              SUBTRACT +1    FROM WS-ANTAL-HIST-AR                        
036440           END-IF                                                         
036441         END-IF                                                           
036500                                                                          
036600         ADD +1              TO IX-PER                                    
036601         SUBTRACT +1    FROM WS-ANTAL-HIST-MAN                            
036602                                                                          
036610         PERFORM UNTIL IX-PER > 12                                        
036611         OR IX-VECKA > WS-SISTA-V (IX-PER)                                
036620         OR TAB-AAR-KVOI-PROG (IX-AR, IX-VECKA) > ZERO                    
036630            ADD +1           TO IX-VECKA                                  
036640         END-PERFORM                                                      
036641                                                                          
036643         IF  IX-PER < 12                                                  
036644         AND IX-VECKA > WS-SISTA-V (IX-PER)                               
036645         AND TAB-AAR-KVOI-PROG (IX-AR, IX-VECKA) > ZERO                   
036646                                                                          
036647             IF KVARTAL                                                   
036648****                                                                      
036649****       VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED        
036650****                                                                      
036651                                                                          
036652               IF (WS-PER = 12                                            
036653               AND IX-PER = 1)                                            
036654               OR IX-PER = WS-PER + 1                                     
036655                  SUBTRACT +1 FROM WS-ANTAL-HIST-AR                       
036656               END-IF                                                     
036657             ELSE                                                         
036658               IF IX-PER = WS-PER                                         
036659                  SUBTRACT +1 FROM WS-ANTAL-HIST-AR                       
036660               END-IF                                                     
036661             END-IF                                                       
036662                                                                          
036663             ADD +1          TO IX-PER                                    
036664         END-IF                                                           
036670                                                                          
036700       END-PERFORM                                                        
036800                                                                          
036900       IF IX-PER > +12                                                    
037000         SUBTRACT +1         FROM IX-AR                                   
037100         MOVE +1             TO IX-PER                                    
037110                                IX-VECKA                                  
037200       END-IF                                                             
037300     END-PERFORM                                                          
037400                                                                          
037500     IF IX-AR < +1                                                        
037600       CONTINUE                                                           
037700     ELSE                                                                 
037800       MOVE IX-AR            TO IX-HIST-AR                                
037900       MOVE IX-PER           TO IX-HIST-PER                               
038000     END-IF                                                               
038100                                                                          
038200     IF WS-ANTAL-HIST-AR > +1                                             
038300****                                                                      
038400****     MINST TVÅ ÅRS HISTORIK                                           
038500****                                                                      
038600        PERFORM BA-AVANCERAD-BERAKNING                                    
038700        PERFORM BC-UPD-PARM-MINST-2AR                                     
038800     ELSE                                                                 
038900       IF WS-ANTAL-HIST-AR = +1                                           
039000****                                                                      
039100****     MER ÄN ETT ÅRS HISTORIK MEN MINDRE ÄN TVÅ                        
039200****     (MAN BERÄKNAR SOM OM DET FANNS TVÅ ÅRS HISTORIK)                 
039300****                                                                      
039400          PERFORM BB-ENKEL-BERAKNING                                      
039500          PERFORM BD-UPD-PARM-1AR                                         
039600       ELSE                                                               
039700****                                                                      
039800****     MINDRE ÄN ETT ÅRS HISTORIK                                       
039900****     FÖR LITE HISTORIK FÖR ATT KUNNA BERÄKNAS MASKINELLT              
040000****                                                                      
040100          PERFORM BE-UPD-PARM-MINDRE-1AR                                  
040200       END-IF                                                             
040300     END-IF                                                               
040310                                                                          
040320***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT START                   
040330*    DIVIDE WS-PER BY WS-NOLL               GIVING WS-PER                 
040340***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT SLUT                    
040350                                                                          
040400     .                                                                    
040500     EJECT                                                                
040600****                                                                      
040700****                                                                      
040800****                                                                      
040900 BA-AVANCERAD-BERAKNING SECTION.                                          
041000****                                                                      
041100****                                                                      
041200****     MINST TVÅ ÅRS HISTORIK                                           
041300****                                                                      
041400****                                                                      
041500                                                                          
041600                                                                          
041700****                                                                      
041800*******  FYLL PÅ AKTUELLT ÅR,                                             
041900*******  GÖRS SOM ÅR 1 I TABELLEN                                         
042000****                                                                      
042100                                                                          
042200     MOVE 1                  TO IX-AR                                     
042300                                IX-RULL                                   
042500     MOVE WS-PER             TO IX-PER                                    
042501                                                                          
042510     IF KVARTAL                                                           
042511                                                                          
042512****                                                                      
042513****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
042514****                                                                      
042515                                                                          
042516       MOVE IX-AR            TO IX-START-AR                               
042517       MOVE IX-PER           TO IX-START-PER                              
042550     ELSE                                                                 
042600       SUBTRACT +1           FROM IX-PER                                  
042700                                                                          
042800       IF IX-PER = ZERO                                                   
042900         MOVE +2             TO IX-START-AR                               
043000         MOVE +12            TO IX-START-PER                              
043100       ELSE                                                               
043200         MOVE IX-AR          TO IX-START-AR                               
043300         MOVE IX-PER         TO IX-START-PER                              
043400       END-IF                                                             
043410     END-IF                                                               
043500                                                                          
043600     PERFORM C-HAMTA-HIST-I-AR                                            
043700                                                                          
043800****                                                                      
043900*******  FYLL PÅ HISTORIK (UPP TILL 5 ÅR)                                 
044000*******  GÖRS SOM ÅR 2 - 6 I TABELLEN                                     
044100****                                                                      
044200                                                                          
044400     MOVE +2                 TO IX-AR                                     
044500     MOVE +12                TO IX-PER                                    
044600                                                                          
044700     PERFORM UNTIL IX-AR > IX-HIST-AR                                     
044800     OR           (IX-AR = IX-HIST-AR                                     
044900     AND           IX-PER < IX-HIST-PER)                                  
045000       PERFORM UNTIL (IX-AR > IX-HIST-AR                                  
045100       OR            (IX-AR = IX-HIST-AR                                  
045200       AND            IX-PER < IX-HIST-PER))                              
045300       OR             IX-PER < +1                                         
045400          MOVE WS-FORSTA-V (IX-PER)                                       
045410                             TO WS-VV                                     
045411          MOVE ZERO          TO WS-KVOI (IX-PER)                          
045420          PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                       
045430             IF TAB-AAR-KVOI-PROG (IX-AR, WS-VV) > ZERO                   
045431                ADD TAB-AAR-KVOI-PROG (IX-AR, WS-VV)                      
045440                             TO WS-KVOI (IX-PER)                          
045441             END-IF                                                       
045450             ADD 1           TO WS-VV                                     
045460          END-PERFORM                                                     
045470                                                                          
045500          COMPUTE WS-KVOI-RED ROUNDED =                                   
045600               (WS-KVOI (IX-PER) * +4.33)                                 
045700                     / WS-KVVIPER (IX-PER)                                
045800          COMPUTE WS-KVOI-CDC (IX-AR, IX-PER) ROUNDED =                   
045900                   WS-KVOI-RED * 1                                        
046100          SUBTRACT +1        FROM IX-PER                                  
046200       END-PERFORM                                                        
046300       ADD +1                TO IX-AR                                     
046500       MOVE +12              TO IX-PER                                    
046600     END-PERFORM                                                          
046700                                                                          
046800     MOVE IX-HIST-AR         TO IX-SLUT-AR                                
047000     MOVE IX-HIST-PER        TO IX-SLUT-PER                               
047100                                                                          
047200                                                                          
047300****                                                                      
047400*******  RÄKNA UT SÄSONGSINDEX FÖR RESP PERIOD                            
047500*******  INITIERA INDEX                                                   
047600****                                                                      
047700****                                                                      
047800****                                                                      
047900****     SÄSONGSINDEX SKA BERÄKNAS FÖR VARJE PERIOD                       
048000****     SOM HAR HISTORIK 5 PERIODER EFTER SAMT 6 PERIODER FÖRE           
048100****                                                                      
048200****     ALLA PERIODER SOM UPPFYLLER OVANSTÅENDE VILLKOR                  
048300****     SKA JÄMFÖRA FÖRSÄLJNINGEN MOT EN                                 
048400****     ÅRSTOTAL SOM OMFATTAR 5 PERIODER EFTER                           
048500****     SAMT 6 PERIODER FÖRE (TOTALT 12)                                 
048600****     ÅRSTOTALEN (WS-KVOI-TOT-AR) ÄR UNIK FÖR VARJE PERIOD             
048700****                                                                      
048800****                                                                      
048900****     DÄRFÖR BÖRJAR MAN MED ATT BYGGA UPP EN ÅRSTOTAL                  
049000****     (WS-KVOI-TOTALT) FÖR DOM 12 FÖRSTA PERIODERNA                    
049100****     SAMT HÄMTAR FÖRSTA RELEVANTA PERIOD NÄR IX = 7                   
049200****     (FÖRSTA PERIOD SOM SKA BERÄKNA SÄSONGSINDEX)                     
049300****                                                                      
049400****     DÄREFTER TAR MAN EN PERIOD I TAGET OCH LÄGGER                    
049500****     TILL NY PERIOD MHA IX-ARB2 SAMT DRAR IFRÅN "FÖRBRUKAD"           
049600****     PERIOD MHA IX-ARB1                                               
049700****                                                                      
049800****                                                                      
049900                                                                          
050000     MOVE +1                 TO IX                                        
050100     MOVE IX-START-AR        TO IX-ARB1-AR                                
050200                                IX-ARB2-AR                                
050300     MOVE IX-START-PER       TO IX-ARB1-PER                               
050400                                IX-ARB2-PER                               
050500     MOVE ZERO               TO WS-KVOI-TOTALT                            
050600     PERFORM UNTIL IX > +12                                               
050700       ADD WS-KVOI-CDC (IX-ARB2-AR, IX-ARB2-PER)                          
050800                             TO WS-KVOI-TOTALT                            
050900       IF IX = +7                                                         
051000         MOVE IX-ARB2-AR     TO IX-SEASON-AR                              
051100         MOVE IX-ARB2-PER    TO IX-SEASON-PER                             
051200       END-IF                                                             
051300       IF IX-ARB2-PER = +1                                                
051400          ADD +1             TO IX-ARB2-AR                                
051500          MOVE +12           TO IX-ARB2-PER                               
051600       ELSE                                                               
051700          SUBTRACT +1        FROM IX-ARB2-PER                             
051800       END-IF                                                             
051900       ADD +1                TO IX                                        
052000     END-PERFORM                                                          
052100                                                                          
052200****                                                                      
052300****   UPPDATERA FÖRSTA PERIOD SOM HAR EN ÅRSTOTAL (ENLIGT                
052400****   FASTSTÄLLDA REGLER)                                                
052500****                                                                      
052600                                                                          
052700     MOVE WS-KVOI-TOTALT     TO                                           
052800                    WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER)          
052900     COMPUTE WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)                
053000         ROUNDED = (WS-KVOI-CDC (IX-SEASON-AR, IX-SEASON-PER) /           
053100             (WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER) / +12))        
053200                 * 100                                                    
053300                  ON SIZE ERROR                                           
053400                    MOVE ZERO TO                                          
053500                     WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)        
053600     END-COMPUTE                                                          
053700****                                                                      
053800****   IX-START-AR OCH IX-START-PER UPPDATERAS MED FÖRSTA PERIOD          
053900****   SOM HAR EN ÅRSTOTAL (ENLIGT FASTSTÄLLDA REGLER)                    
054000****                                                                      
054100                                                                          
054200     MOVE IX-SEASON-AR       TO IX-START-AR                               
054300     MOVE IX-SEASON-PER      TO IX-START-PER                              
054400                                                                          
054500     IF IX-SEASON-PER = +1                                                
054600       ADD +1                TO IX-SEASON-AR                              
054700       MOVE +12              TO IX-SEASON-PER                             
054800     ELSE                                                                 
054900       SUBTRACT +1           FROM IX-SEASON-PER                           
055000     END-IF                                                               
055100                                                                          
055200     PERFORM UNTIL  IX-ARB2-AR > IX-SLUT-AR                               
055300     OR            (IX-ARB2-AR = IX-SLUT-AR                               
055400     AND            IX-ARB2-PER < IX-SLUT-PER)                            
055500       MOVE IX-SEASON-AR     TO IX-TEMP-AR                                
055600       MOVE IX-SEASON-PER    TO IX-TEMP-PER                               
055700       SUBTRACT WS-KVOI-CDC (IX-ARB1-AR, IX-ARB1-PER)                     
055800                             FROM WS-KVOI-TOTALT                          
055900       ADD WS-KVOI-CDC (IX-ARB2-AR, IX-ARB2-PER)                          
056000                             TO WS-KVOI-TOTALT                            
056100       MOVE WS-KVOI-TOTALT   TO                                           
056200                    WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER)          
056300       COMPUTE WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)              
056400         ROUNDED = (WS-KVOI-CDC (IX-SEASON-AR, IX-SEASON-PER) /           
056500             (WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER) / +12))        
056600                 * 100                                                    
056700                  ON SIZE ERROR                                           
056800                    MOVE ZERO TO                                          
056900                     WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)        
057000       END-COMPUTE                                                        
057100       PERFORM BAA-ADDERA-TILL-INDEX                                      
057200     END-PERFORM                                                          
057300                                                                          
057400****                                                                      
057500****   IX-SLUT-AR OCH IX-SLUT-PER UPPDATERAS MED SISTA PERIOD             
057600****   SOM HAR EN ÅRSTOTAL (ENLIGT FASTSTÄLLDA REGLER)                    
057700****                                                                      
057800                                                                          
057900     MOVE IX-TEMP-AR         TO IX-SLUT-AR                                
058000     MOVE IX-TEMP-PER        TO IX-SLUT-PER                               
058100                                                                          
058200****                                                                      
058300****   RÄKNA UT SÄSONGSINDEX FÖR SAMTLIGA PERIODER                        
058400****   SOM HAR EN ÅRSTOTAL (DVS FÖRSÄLJNING 5 PERIODER EFTER              
058500****   SAMT 6 PERIODER FÖRE)                                              
058600****   RÄKNA UT MEDELVÄRDET AV SÄSONGSINDEXEN PER PERIOD                  
058700****                                                                      
058800                                                                          
058900     MOVE IX-START-AR        TO IX-ARB1-AR                                
059000     MOVE 1                  TO IX-ARB1-PER                               
059100                                                                          
059200****                                                                      
059300****   TA FÖRST ALLA PERIODER FROM PERIOD 1                               
059400****   TOM PERIOD = IX-START-PER                                          
059500****                                                                      
059600                                                                          
059700     MOVE ZERO               TO WS-RESEASON-TOTAL                         
059800     PERFORM UNTIL IX-ARB1-PER > IX-START-PER                             
059900       PERFORM BAB-BERAKNA-MEDELVARDE                                     
060000       MOVE IX-START-AR      TO IX-ARB1-AR                                
060100       ADD +1                TO IX-ARB1-PER                               
060200     END-PERFORM                                                          
060300                                                                          
060400                                                                          
060500****                                                                      
060600****   TA NU ALLA RESTERANDE PERIODER                                     
060700****   (ALLA PERIODER ÄR REDAN GENOMGÅNGA OM IX-START-PER = 1,            
060800****    IX-ARB1-PER KOMMER ATT VARA > 12 SÅ MAN KOMMER INTE               
060900****    IN I NÄSTA ITERATION)                                             
061000****   ALLA PERIODER FROM IX-START-PER + 1                                
061100****   TOM PERIOD = 12                                                    
061200****                                                                      
061300                                                                          
061400     MOVE IX-START-AR        TO IX-ARB1-AR                                
061500     ADD +1                  TO IX-ARB1-AR                                
061600                                                                          
061700     PERFORM UNTIL IX-ARB1-PER > 12                                       
061800       PERFORM BAB-BERAKNA-MEDELVARDE                                     
061900       MOVE IX-START-AR      TO IX-ARB1-AR                                
062000       ADD +1                TO IX-ARB1-AR                                
062100       ADD +1                TO IX-ARB1-PER                               
062200     END-PERFORM                                                          
062300                                                                          
062400                                                                          
062500****                                                                      
062600****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
062700****                                                                      
062800                                                                          
062900     PERFORM D-NORMERA-SASONGSINDEX                                       
063000                                                                          
063100                                                                          
063200                                                                          
063300****                                                                      
063400****   RÄKNA UT OSÄKERHETSFAKTORN MHA STANDARDAVVIKELSEN                  
063500****                                                                      
063600                                                                          
063700****                                                                      
063800****   1) RÄKNA UT SKILLNADEN MELLAN VARJE INDEX (WS-RESEASON-PER)        
063900****      OCH MEDELVÄRDET FÖR PERIODEN (WS-RESEASON-SNITT)                
064000****   2) TA KVADRATEN (** 2) PÅ RESULTATET AV 1) FÖR                     
064100****      RESPEKTIVE SÄSONGSINDEX (WS-RESEASON-PER)                       
064200****   3) SUMMERA RESULTATEN FRÅN 2)                                      
064300****   4) DIVIDERA SUMMAN FRÅN 3)                                         
064400****      MED ANTALET SÄSONGSINDEX (IX-ANTAL)                             
064500****   5) TA ROTEN (** 0,5) UR RESULTATET AV 4)                           
064600****                                                                      
064700                                                                          
064800     MOVE IX-START-AR        TO IX-ARB1-AR                                
064900     MOVE IX-START-PER       TO IX-ARB1-PER                               
065000     MOVE ZERO               TO IX-ANTAL                                  
065100                                WS-KVADRAT-SUM                            
065200                                                                          
065300     PERFORM UNTIL IX-ARB1-AR > IX-SLUT-AR                                
065400     OR           (IX-ARB1-AR = IX-SLUT-AR                                
065500     AND           IX-ARB1-PER < IX-SLUT-PER)                             
065600       PERFORM UNTIL (IX-ARB1-AR > IX-SLUT-AR                             
065700       OR            (IX-ARB1-AR = IX-SLUT-AR                             
065800       AND            IX-ARB1-PER < IX-SLUT-PER))                         
065900       OR             IX-ARB1-PER < +1                                    
066000         ADD +1              TO IX-ANTAL                                  
066100         COMPUTE WS-SKILLNAD ROUNDED =                                    
066200                 WS-RESEASON-PER (IX-ARB1-AR, IX-ARB1-PER) -              
066300                 WS-RESEASON-SNITT (IX-ARB1-PER)                          
066400         COMPUTE WS-KVADRAT ROUNDED = WS-SKILLNAD ** 2                    
066500         ADD WS-KVADRAT      TO WS-KVADRAT-SUM                            
066600         SUBTRACT +1         FROM IX-ARB1-PER                             
066700                                                                          
066800       END-PERFORM                                                        
066900       ADD +1                TO IX-ARB1-AR                                
067000       MOVE +12              TO IX-ARB1-PER                               
067100     END-PERFORM                                                          
067200                                                                          
067310     COMPUTE WS-KVADRAT-SUM ROUNDED = WS-KVADRAT-SUM / IX-ANTAL           
067320                  ON SIZE ERROR                                           
067330                    MOVE ZERO TO                                          
067340                     WS-KVADRAT-SUM                                       
067350     END-COMPUTE                                                          
067400                                                                          
067500****                                                                      
067600****   TA ROTEN UR SUMMAN FÖR ATT FÅ FRAM                                 
067700****   OSÄKERHETSTALET DVS STANDARDAVVIKELSEN                             
067800****                                                                      
067900     COMPUTE WS-OSAKERHET ROUNDED = WS-KVADRAT-SUM ** 0.5                 
068000                                                                          
068100                                                                          
068200***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT START                   
068300*    DIVIDE WS-PER BY WS-NOLL               GIVING WS-PER                 
068400***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT SLUT                    
068500                                                                          
068600                                                                          
068700     .                                                                    
068800     EJECT                                                                
068900                                                                          
069000****                                                                      
069100****                                                                      
069200****                                                                      
069300 BAA-ADDERA-TILL-INDEX SECTION.                                           
069400****                                                                      
069500****                                                                      
069600                                                                          
069700                                                                          
069800     IF IX-ARB1-PER = +1                                                  
069900       ADD +1                TO IX-ARB1-AR                                
070000       MOVE +12              TO IX-ARB1-PER                               
070100     ELSE                                                                 
070200       SUBTRACT +1           FROM IX-ARB1-PER                             
070300     END-IF                                                               
070400                                                                          
070500     IF IX-SEASON-PER = +1                                                
070600       ADD +1                TO IX-SEASON-AR                              
070700       MOVE +12              TO IX-SEASON-PER                             
070800     ELSE                                                                 
070900       SUBTRACT +1           FROM IX-SEASON-PER                           
071000     END-IF                                                               
071100                                                                          
071200     IF IX-ARB2-PER = +1                                                  
071300       ADD +1                TO IX-ARB2-AR                                
071400       MOVE +12              TO IX-ARB2-PER                               
071500     ELSE                                                                 
071600       SUBTRACT +1           FROM IX-ARB2-PER                             
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000 BAB-BERAKNA-MEDELVARDE SECTION.                                          
072100****                                                                      
072200****   ITERATION LODRÄTT I TABELLEN                                       
072300****                                                                      
072400       MOVE ZERO             TO WS-RESEASON-SUM                           
072500                                WS-KVOI-SUM                               
072600                                IX-ANTAL                                  
072700       PERFORM UNTIL IX-ARB1-AR > IX-SLUT-AR                              
072800       OR           (IX-ARB1-AR = IX-SLUT-AR                              
072900       AND           IX-ARB1-PER < IX-SLUT-PER)                           
073000****                                                                      
073100****   ITERATION HORISONTELLT I TABELLEN                                  
073200****                                                                      
073300                                                                          
073400         ADD +1              TO IX-ANTAL                                  
073500         ADD WS-RESEASON-PER (IX-ARB1-AR, IX-ARB1-PER)                    
073600                             TO WS-RESEASON-SUM                           
073700         ADD WS-KVOI-CDC (IX-ARB1-AR, IX-ARB1-PER)                        
073800                             TO WS-KVOI-SUM                               
073900         ADD +1              TO IX-ARB1-AR                                
074000       END-PERFORM                                                        
074100                                                                          
074200****                                                                      
074300****   RÄKNA UT MEDELVÄRDET FÖR SÄSONGSINDEXEN                            
074400****                        OCH ORDERINGÅNGEN (KVOI)                      
074500****                                                                      
074510                                                                          
074520       COMPUTE WS-KVOI-SNITT (IX-ARB1-PER) ROUNDED =                      
074530               WS-KVOI-SUM / IX-ANTAL                                     
074540                  ON SIZE ERROR                                           
074550                    MOVE ZERO TO                                          
074560                     WS-KVOI-SNITT (IX-ARB1-PER)                          
074570       END-COMPUTE                                                        
074580                                                                          
074590       COMPUTE WS-RESEASON-SNITT (IX-ARB1-PER) ROUNDED =                  
074591               WS-RESEASON-SUM / IX-ANTAL                                 
074592                  ON SIZE ERROR                                           
074593                    MOVE ZERO TO                                          
074594                     WS-RESEASON-SNITT (IX-ARB1-PER)                      
074595       END-COMPUTE                                                        
074600                                                                          
075100       ADD WS-RESEASON-SNITT (IX-ARB1-PER)                                
075200                             TO WS-RESEASON-TOTAL                         
075300     .                                                                    
075400     EJECT                                                                
075500****                                                                      
075600****                                                                      
075700****                                                                      
075800 BB-ENKEL-BERAKNING SECTION.                                              
075900****                                                                      
076000****                                                                      
076100****     MER ÄN ETT ÅRS HISTORIK MEN MINDRE ÄN TVÅ                        
076200****                                                                      
076300****                                                                      
076400                                                                          
076500                                                                          
076600****                                                                      
076700****     FYLL PÅ AKTUELLT ÅR,                                             
076800****                                                                      
076900                                                                          
077000     MOVE ZERO               TO WS-KVOI-TOTALT-2AR                        
077100     MOVE +1                 TO IX-AR                                     
077200                                IX-RULL                                   
077400     MOVE WS-PER             TO IX-PER                                    
077401                                                                          
077410     IF KVARTAL                                                           
077411                                                                          
077412****                                                                      
077413****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
077414****                                                                      
077415                                                                          
077420       MOVE IX-AR            TO IX-START-AR                               
077430       MOVE IX-PER           TO IX-START-PER                              
077440     ELSE                                                                 
077500       SUBTRACT +1           FROM IX-PER                                  
077600                                                                          
077700       IF IX-PER = ZERO                                                   
077800         MOVE +2             TO IX-START-AR                               
077900         MOVE +12            TO IX-START-PER                              
078000       ELSE                                                               
078100         MOVE IX-AR          TO IX-START-AR                               
078200         MOVE IX-PER         TO IX-START-PER                              
078300       END-IF                                                             
078310     END-IF                                                               
078400                                                                          
078500     PERFORM C-HAMTA-HIST-I-AR                                            
078600                                                                          
078700     MOVE +1                 TO IX-PER                                    
078800     PERFORM UNTIL  IX-PER > WS-PER                                       
078900       ADD WS-KVOI-CDC (1, IX-PER)                                        
079000                             TO WS-KVOI-TOTALT-2AR                        
079100       ADD +1                TO IX-PER                                    
079200     END-PERFORM                                                          
079300                                                                          
079400****                                                                      
079500*******  FYLL PÅ DEN HISTORIK SOM FINNS                                   
079600****                                                                      
079700                                                                          
079800     MOVE +1                 TO IX-AR                                     
080000     MOVE +12                TO IX-PER                                    
080100                                                                          
080200     PERFORM UNTIL IX-AR > IX-HIST-AR                                     
080300     OR           (IX-AR = IX-HIST-AR                                     
080400     AND           IX-PER < IX-HIST-PER)                                  
080500                                                                          
080600       ADD +1                TO IX-AR                                     
080800       MOVE +12              TO IX-PER                                    
080900       PERFORM UNTIL (IX-AR > IX-HIST-AR                                  
081000       OR            (IX-AR = IX-HIST-AR                                  
081100       AND            IX-PER < IX-HIST-PER))                              
081200       OR            IX-PER < +1                                          
081300          MOVE WS-FORSTA-V (IX-PER)                                       
081400                             TO WS-VV                                     
081410          MOVE ZERO          TO WS-KVOI (IX-PER)                          
081500          PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                       
081600             IF TAB-AAR-KVOI-PROG (IX-AR, WS-VV) > ZERO                   
081610                ADD TAB-AAR-KVOI-PROG (IX-AR, WS-VV)                      
081700                             TO WS-KVOI (IX-PER)                          
081710             END-IF                                                       
081800             ADD 1           TO WS-VV                                     
081900          END-PERFORM                                                     
082000                                                                          
082100          COMPUTE WS-KVOI-RED ROUNDED =                                   
082200               (WS-KVOI (IX-PER) * +4.33)                                 
082210                     / WS-KVVIPER (IX-PER)                                
082211                  ON SIZE ERROR                                           
082212                    MOVE ZERO    TO WS-KVOI-RED                           
082213          END-COMPUTE                                                     
082220          COMPUTE WS-KVOI-CDC (IX-AR, IX-PER) ROUNDED =                   
082230                   WS-KVOI-RED * 1                                        
082300          ADD WS-KVOI-CDC (IX-AR, IX-PER)                                 
082400                             TO WS-KVOI-TOTALT-2AR                        
082500          SUBTRACT +1        FROM IX-PER                                  
082600       END-PERFORM                                                        
082700                                                                          
082800     END-PERFORM                                                          
082900                                                                          
083000                                                                          
083100****                                                                      
083200*******  FYLL UT HISTORIKTABELLEN TILL TVÅ ÅR MHA                         
083300*******  DEN HISTORIK MAN HAR (MINDRE ÄN TVÅ ÅR),                         
083400****                                                                      
083500                                                                          
083600     MOVE +3                 TO IX-SLUT-AR                                
083700*    MOVE DAGENS-DATUM (3:2) TO IX-SLUT-PER                               
083800     MOVE WS-PER             TO IX-SLUT-PER                               
083900                                                                          
084000     PERFORM UNTIL IX-AR > IX-SLUT-AR                                     
084100     OR           (IX-AR = IX-SLUT-AR                                     
084200     AND           IX-PER = IX-SLUT-PER)                                  
084300                                                                          
084400       SUBTRACT +1           FROM IX-AR                                   
084500                             GIVING IX-HAMTA-AR                           
084600                                                                          
084700       PERFORM UNTIL (IX-AR > IX-SLUT-AR                                  
084800       OR            (IX-AR = IX-SLUT-AR                                  
084900       AND            IX-PER = IX-SLUT-PER))                              
085000       OR             IX-PER < +1                                         
085100         MOVE WS-KVOI-CDC (IX-HAMTA-AR, IX-PER)                           
085200                             TO WS-KVOI-CDC (IX-AR, IX-PER)               
085300         ADD WS-KVOI-CDC (IX-AR, IX-PER)                                  
085400                             TO WS-KVOI-TOTALT-2AR                        
085500         SUBTRACT +1         FROM IX-PER                                  
085600       END-PERFORM                                                        
085700                                                                          
085800       ADD +1                TO IX-AR                                     
085900       MOVE +12              TO IX-PER                                    
086000     END-PERFORM                                                          
086100                                                                          
086200                                                                          
086300****                                                                      
086400*******  RÄKNA UT MEDELVÄRDET FÖR TVÅ ÅR                                  
086500****                                                                      
086600                                                                          
086700                                                                          
086800     COMPUTE WS-KVOI-MEDEL-PER ROUNDED =                                  
086900                           WS-KVOI-TOTALT-2AR / +24                       
087000                  ON SIZE ERROR                                           
087100                      MOVE ZERO   TO WS-KVOI-MEDEL-PER                    
087200     END-COMPUTE                                                          
087300                                                                          
087400                                                                          
087500                                                                          
087600****                                                                      
087700*******  RÄKNA UT MEDELVÄRDEN FÖR SÄSONGSINDEX                            
087800****                          OCH ORDERINGÅNGEN (KVOI)                    
087900****                                                                      
088000                                                                          
088100                                                                          
088200     MOVE IX-START-AR        TO IX-AR                                     
088300     MOVE IX-START-PER       TO IX-PER                                    
088400     MOVE ZERO               TO WS-RESEASON-TOTAL                         
088500                                                                          
088600     PERFORM UNTIL IX-AR + 1 > IX-SLUT-AR                                 
088700     OR           (IX-AR + 1 = IX-SLUT-AR                                 
088800     AND           IX-PER = IX-SLUT-PER)                                  
088900                                                                          
089000       PERFORM UNTIL (IX-AR + 1 > IX-SLUT-AR                              
089100       OR            (IX-AR + 1 = IX-SLUT-AR                              
089200       AND            IX-PER = IX-SLUT-PER))                              
089300       OR             IX-PER < +1                                         
089400         COMPUTE WS-KVOI-SNITT (IX-PER) ROUNDED =                         
089500                (WS-KVOI-CDC (IX-AR, IX-PER) +                            
089600                 WS-KVOI-CDC (IX-AR + 1, IX-PER)) / 2                     
089700         COMPUTE WS-RESEASON-SNITT (IX-PER) ROUNDED =                     
089800                 WS-KVOI-SNITT (IX-PER) * +100                            
089900                 / WS-KVOI-MEDEL-PER                                      
090000                  ON SIZE ERROR                                           
090100                      MOVE ZERO   TO WS-RESEASON-SNITT (IX-PER)           
090200         END-COMPUTE                                                      
090300         ADD WS-RESEASON-SNITT (IX-PER)                                   
090400                             TO WS-RESEASON-TOTAL                         
090500         SUBTRACT +1         FROM IX-PER                                  
090600       END-PERFORM                                                        
090700                                                                          
090800       ADD +1                TO IX-AR                                     
090900       MOVE +12              TO IX-PER                                    
091000     END-PERFORM                                                          
091100                                                                          
091200****                                                                      
091300****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
091400****                                                                      
091500                                                                          
091600     PERFORM D-NORMERA-SASONGSINDEX                                       
091700     .                                                                    
091800     EJECT                                                                
091900****                                                                      
092000****                                                                      
092100****                                                                      
092200 BC-UPD-PARM-MINST-2AR SECTION.                                           
092300****                                                                      
092400****                                                                      
092500                                                                          
092600     MOVE +1                 TO IX                                        
092700     PERFORM UNTIL IX > +12                                               
092800       MOVE WS-RESEASON-AVR (IX)                                          
092900                             TO SEAS-RESEASON (IX)                        
093000       MOVE WS-KVOI-SNITT (IX)                                            
093100                             TO SEAS-KVOI (IX)                            
093200       ADD +1                TO IX                                        
093300     END-PERFORM                                                          
093400     MOVE WS-ANTAL-HIST-AR   TO SEAS-ANT-HIST-AR                          
093500     COMPUTE SEAS-OSAKERHET ROUNDED = WS-OSAKERHET * 1                    
093501                                                                          
093502     PERFORM E-KOLLA-OM-SAESONG                                           
093600     .                                                                    
093700     EJECT                                                                
093800****                                                                      
093900****                                                                      
094000****                                                                      
094100 BD-UPD-PARM-1AR SECTION.                                                 
094200****                                                                      
094300****                                                                      
094400                                                                          
094500     MOVE +1                 TO IX                                        
094600     PERFORM UNTIL IX > +12                                               
094700       MOVE WS-RESEASON-AVR (IX)                                          
094800                             TO SEAS-RESEASON (IX)                        
094900       MOVE WS-KVOI-SNITT (IX)                                            
095000                             TO SEAS-KVOI (IX)                            
095100       ADD +1                TO IX                                        
095200     END-PERFORM                                                          
095300     MOVE WS-ANTAL-HIST-AR   TO SEAS-ANT-HIST-AR                          
095400     MOVE ZERO               TO SEAS-OSAKERHET                            
095401                                                                          
095402     PERFORM E-KOLLA-OM-SAESONG                                           
095500     .                                                                    
095600     EJECT                                                                
095700****                                                                      
095800****                                                                      
095900****                                                                      
096000 BE-UPD-PARM-MINDRE-1AR SECTION.                                          
096100****                                                                      
096200****                                                                      
096300                                                                          
096400     MOVE +1                 TO IX                                        
096500     PERFORM UNTIL IX > +12                                               
096600       MOVE 100              TO SEAS-RESEASON (IX)                        
096700                                SEAS-KVOI (IX)                            
096800       ADD +1                TO IX                                        
096900     END-PERFORM                                                          
097000     MOVE WS-ANTAL-HIST-AR   TO SEAS-ANT-HIST-AR                          
097100     MOVE ZERO               TO SEAS-OSAKERHET                            
097110     MOVE NEJ                TO SEAS-SEASON-ARTIKEL                       
097200     .                                                                    
097300     EJECT                                                                
097400 C-HAMTA-HIST-I-AR SECTION.                                               
097500     MOVE +1                   TO IX-PER                                  
097600                                                                          
097700     PERFORM UNTIL IX-PER      =  WS-PER                                  
097800       MOVE WS-FORSTA-V (IX-PER)                                          
097900                             TO WS-VV                                     
097910       MOVE ZERO             TO WS-KVOI (IX-PER)                          
098000       PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                          
098100          IF TAB-AAR-KVOI-PROG (1, WS-VV) > ZERO                          
098110             ADD TAB-AAR-KVOI-PROG (1, WS-VV)                             
098200                             TO WS-KVOI (IX-PER)                          
098210          END-IF                                                          
098300          ADD 1              TO WS-VV                                     
098400       END-PERFORM                                                        
098500                                                                          
098600       COMPUTE WS-KVOI-RED ROUNDED =                                      
098700               (WS-KVOI (IX-PER) * +4.33)                                 
098800                     / WS-KVVIPER (IX-PER)                                
098810                  ON SIZE ERROR                                           
098820                    MOVE ZERO TO  WS-KVOI-RED                             
098830       END-COMPUTE                                                        
098900       COMPUTE WS-KVOI-CDC (1, IX-PER) ROUNDED =                          
099000                   WS-KVOI-RED * 1                                        
099200       ADD +1                  TO IX-PER                                  
099300     END-PERFORM                                                          
099314                                                                          
099315     IF KVARTAL                                                           
099320                                                                          
099321****                                                                      
099322****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
099323****                                                                      
099324                                                                          
099325       MOVE WS-FORSTA-V (IX-PER)                                          
099326                             TO WS-VV                                     
099327       PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                          
099328          IF TAB-AAR-KVOI-PROG (1, WS-VV) > ZERO                          
099329             ADD TAB-AAR-KVOI-PROG (1, WS-VV)                             
099330                             TO WS-KVOI (IX-PER)                          
099331          END-IF                                                          
099332          ADD 1              TO WS-VV                                     
099333       END-PERFORM                                                        
099334                                                                          
099335       COMPUTE WS-KVOI-RED ROUNDED =                                      
099336               (WS-KVOI (IX-PER) * +4.33)                                 
099337                     / WS-KVVIPER (IX-PER)                                
099338                  ON SIZE ERROR                                           
099339                    MOVE ZERO TO  WS-KVOI-RED                             
099340       END-COMPUTE                                                        
099341       COMPUTE WS-KVOI-CDC (1, IX-PER) ROUNDED =                          
099350                   WS-KVOI-RED * 1                                        
099393     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600 D-NORMERA-SASONGSINDEX SECTION.                                          
099700                                                                          
099800****                                                                      
099900****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
100000****                                                                      
100100                                                                          
100200                                                                          
100300     MOVE +1                 TO IX-PER                                    
100400     MOVE ZERO               TO WS-RESEASON-TOT-AVR                       
100500     PERFORM UNTIL IX-PER > +12                                           
100600       COMPUTE WS-RESEASON-JUST (IX-PER) ROUNDED =                        
100700               WS-RESEASON-SNITT (IX-PER) * +1200                         
100800                            / WS-RESEASON-TOTAL                           
100900                  ON SIZE ERROR                                           
101000                      MOVE ZERO   TO WS-RESEASON-JUST (IX-PER)            
101100       END-COMPUTE                                                        
101200       COMPUTE WS-RESEASON-AVR (IX-PER) ROUNDED =                         
101300               WS-RESEASON-JUST (IX-PER) * +1                             
101400       ADD WS-RESEASON-AVR (IX-PER)                                       
101500                             TO WS-RESEASON-TOT-AVR                       
101600       ADD +1                TO IX-PER                                    
101700     END-PERFORM                                                          
101770                                                                          
101801     IF WS-RESEASON-TOT-AVR = ZERO                                        
101802     OR WS-RESEASON-TOT-AVR < +800                                        
101803     OR WS-RESEASON-TOT-AVR > +1400                                       
101804     OR WS-RESEASON-AVR (1) > +999                                        
101805     OR WS-RESEASON-AVR (2) > +999                                        
101806     OR WS-RESEASON-AVR (3) > +999                                        
101807     OR WS-RESEASON-AVR (4) > +999                                        
101808     OR WS-RESEASON-AVR (5) > +999                                        
101809     OR WS-RESEASON-AVR (6) > +999                                        
101810     OR WS-RESEASON-AVR (7) > +999                                        
101811     OR WS-RESEASON-AVR (8) > +999                                        
101812     OR WS-RESEASON-AVR (9) > +999                                        
101813     OR WS-RESEASON-AVR (10) > +999                                       
101814     OR WS-RESEASON-AVR (11) > +999                                       
101815     OR WS-RESEASON-AVR (12) > +999                                       
101816                                                                          
101817****                                                                      
101818****   OM TOTALEN ÄR MINDRE +800 ELLER STÖRRE ÄN +1400                    
101819****   BEROR AVVIKELSEN PÅ PROBLEM VID AVRUNDINGEN                        
101820****   I COMPUTESATS, TA EJ HÄNSYN TILL DENNA                             
101821****   DVS SÄTT DEN SOM ICKE SÄSONGSARTIKEL                               
101822****   EJ HELLER OM NÅGON ENSTAKA PERIOD HAR MER ÄN +999                  
101823****                                                                      
101824                                                                          
101825       MOVE +1               TO IX-PER                                    
101826       MOVE +1200            TO WS-RESEASON-TOT-AVR                       
101830       PERFORM UNTIL IX-PER > +12                                         
101841         MOVE +100           TO WS-RESEASON-AVR (IX-PER)                  
101842         ADD +1              TO IX-PER                                    
101843       END-PERFORM                                                        
101844     END-IF                                                               
101850                                                                          
101900     IF WS-RESEASON-TOT-AVR > +1200                                       
102000       MOVE -1               TO WS-JUSTERA                                
102100     ELSE                                                                 
102200       MOVE +1               TO WS-JUSTERA                                
102300     END-IF                                                               
102400                                                                          
102500     PERFORM UNTIL WS-RESEASON-TOT-AVR = +1200                            
102510     OR            WS-RESEASON-TOT-AVR = ZERO                             
102600                                                                          
102700       MOVE +1               TO IX-PER                                    
102800       PERFORM UNTIL WS-RESEASON-TOT-AVR = +1200                          
102900       OR IX-PER > +12                                                    
103000         IF WS-RESEASON-AVR (IX-PER) > +1                                 
103100           ADD WS-JUSTERA    TO WS-RESEASON-AVR (IX-PER)                  
103200                                WS-RESEASON-TOT-AVR                       
103300         END-IF                                                           
103400         ADD +1              TO IX-PER                                    
103500       END-PERFORM                                                        
103600     END-PERFORM                                                          
103700     .                                                                    
103800     EJECT                                                                
103963 E-KOLLA-OM-SAESONG SECTION.                                              
103964                                                                          
103966       MOVE ZERO             TO WS-KVOI-RULL-TOT                          
103967       MOVE 1                TO IX-VECKA                                  
103968                                                                          
103969       PERFORM UNTIL IX-VECKA > DAGENS-TIVV                               
103970         IF TAB-AAR-KVOI-PROG (1, IX-VECKA) > ZERO                        
103971            ADD TAB-AAR-KVOI-PROG (1, IX-VECKA)                           
103972                             TO WS-KVOI-RULL-TOT                          
103973         END-IF                                                           
103974         ADD 1               TO IX-VECKA                                  
103975       END-PERFORM                                                        
103976                                                                          
103977       PERFORM UNTIL IX-VECKA > 52                                        
103978         IF TAB-AAR-KVOI-PROG (2, IX-VECKA) > ZERO                        
103979            ADD TAB-AAR-KVOI-PROG (2, IX-VECKA)                           
103980                             TO WS-KVOI-RULL-TOT                          
103981         END-IF                                                           
103982         ADD 1               TO IX-VECKA                                  
103983       END-PERFORM                                                        
103984                                                                          
103987****                                                                      
103988****   MIN OCH MAX ÄR ETT KOMPLEMENT TILL OSÄKERHETSFAKTORN               
103989****   FÖR ATT HITTA SÄSONGSARTIKLAR                                      
103990****   3 PERIODER I RAD SKA VARA ÖVER 400 ELLER UNDER 200                 
103991****   ORDERINGÅNGEN ETT ÅR TILLBAKA MÅSTE VARA MINST 50                  
103992****   VID INSTALLATION    FEB 2007 ÄNDRAS GRÄNSERNA TILL                 
103993****   370 OCH 230,                                                       
103994****                                                                      
104000                                                                          
104001       IF  SEAS-IDFKNGRP > 8699                                           
104002       AND SEAS-IDFKNGRP < 8750                                           
104003           MOVE 20           TO WS-GRAENS-OI                              
104004       ELSE                                                               
104006           MOVE 50           TO WS-GRAENS-OI                              
104007       END-IF                                                             
104008                                                                          
104009       MOVE NEJ            TO SEAS-SEASON-ARTIKEL                         
104020       IF WS-KVOI-RULL-TOT > WS-GRAENS-OI  AND                            
104021          WS-ANTAL-HIST-MAN >= 30                                         
104022*         ENDAST SÄSONG OM MINST 2,5 ÅRS HISTORIK                         
104030         IF (SEAS-OSAKERHET > ZERO                                        
104031         AND   SEAS-OSAKERHET < 50)                                       
104032*********AND   SEAS-OSAKERHET < 15)                                       
104033           MOVE JA             TO SEAS-SEASON-ARTIKEL                     
104034         ELSE                                                             
104035          IF SEAS-OSAKERHET < 100                                         
104036           IF ((WS-RESEASON-AVR (1) +                                     
104037                WS-RESEASON-AVR (2) +                                     
104038                WS-RESEASON-AVR (3)) > 370)                               
104039           OR ((WS-RESEASON-AVR (1) +                                     
104040                WS-RESEASON-AVR (2) +                                     
104041                WS-RESEASON-AVR (3)) < 230)                               
104042           OR ((WS-RESEASON-AVR (2) +                                     
104043                WS-RESEASON-AVR (3) +                                     
104044                WS-RESEASON-AVR (4)) > 370)                               
104045           OR ((WS-RESEASON-AVR (2) +                                     
104046                WS-RESEASON-AVR (3) +                                     
104047                WS-RESEASON-AVR (4)) < 230)                               
104048           OR ((WS-RESEASON-AVR (3) +                                     
104049                WS-RESEASON-AVR (4) +                                     
104050                WS-RESEASON-AVR (5)) > 370)                               
104051           OR ((WS-RESEASON-AVR (3) +                                     
104052                WS-RESEASON-AVR (4) +                                     
104053                WS-RESEASON-AVR (5)) < 230)                               
104054           OR ((WS-RESEASON-AVR (4) +                                     
104055                WS-RESEASON-AVR (5) +                                     
104056                WS-RESEASON-AVR (6)) > 370)                               
104057           OR ((WS-RESEASON-AVR (4) +                                     
104058                WS-RESEASON-AVR (5) +                                     
104059                WS-RESEASON-AVR (6)) < 230)                               
104060           OR ((WS-RESEASON-AVR (5) +                                     
104061                WS-RESEASON-AVR (6) +                                     
104062                WS-RESEASON-AVR (7)) > 370)                               
104063           OR ((WS-RESEASON-AVR (5) +                                     
104064                WS-RESEASON-AVR (6) +                                     
104065                WS-RESEASON-AVR (7)) < 230)                               
104066           OR ((WS-RESEASON-AVR (6) +                                     
104067                WS-RESEASON-AVR (7) +                                     
104068                WS-RESEASON-AVR (8)) > 370)                               
104069           OR ((WS-RESEASON-AVR (6) +                                     
104070                WS-RESEASON-AVR (7) +                                     
104071                WS-RESEASON-AVR (8)) < 230)                               
104072           OR ((WS-RESEASON-AVR (7) +                                     
104073                WS-RESEASON-AVR (8) +                                     
104074                WS-RESEASON-AVR (9)) > 370)                               
104075           OR ((WS-RESEASON-AVR (7) +                                     
104076                WS-RESEASON-AVR (8) +                                     
104077                WS-RESEASON-AVR (9)) < 230)                               
104078           OR ((WS-RESEASON-AVR (8) +                                     
104079                WS-RESEASON-AVR (9) +                                     
104080                WS-RESEASON-AVR (10)) > 370)                              
104081           OR ((WS-RESEASON-AVR (8) +                                     
104082                WS-RESEASON-AVR (9) +                                     
104083                WS-RESEASON-AVR (10)) < 230)                              
104084           OR ((WS-RESEASON-AVR (9)  +                                    
104085                WS-RESEASON-AVR (10) +                                    
104086                WS-RESEASON-AVR (11)) > 370)                              
104087           OR ((WS-RESEASON-AVR (9)  +                                    
104088                WS-RESEASON-AVR (10) +                                    
104089                WS-RESEASON-AVR (11)) < 230)                              
104090           OR ((WS-RESEASON-AVR (10) +                                    
104091                WS-RESEASON-AVR (11) +                                    
104092                WS-RESEASON-AVR (12)) > 370)                              
104093           OR ((WS-RESEASON-AVR (10) +                                    
104094                WS-RESEASON-AVR (11) +                                    
104095                WS-RESEASON-AVR (12)) < 230)                              
104096           OR ((WS-RESEASON-AVR (11) +                                    
104097                WS-RESEASON-AVR (12) +                                    
104098                WS-RESEASON-AVR (1)) > 370)                               
104099           OR ((WS-RESEASON-AVR (11) +                                    
104100                WS-RESEASON-AVR (12) +                                    
104101                WS-RESEASON-AVR (1)) < 230)                               
104102           OR ((WS-RESEASON-AVR (12) +                                    
104103                WS-RESEASON-AVR (1)  +                                    
104104                WS-RESEASON-AVR (2)) > 370)                               
104105           OR ((WS-RESEASON-AVR (12) +                                    
104106                WS-RESEASON-AVR (1)  +                                    
104107                WS-RESEASON-AVR (2)) < 230)                               
104108             MOVE JA             TO SEAS-SEASON-ARTIKEL                   
104109           END-IF                                                         
104110          END-IF                                                          
104111         END-IF                                                           
104112       END-IF                                                             
104113                                                                          
104114     .                                                                    
104115     EJECT                                                                
104116 F-KOLL-3-PER-AR    SECTION.                                              
104117                                                                          
104118*    FÖR ATT ARTIKELN SKALL FÅ SÄSONG KRÄVS MINST                         
104119*    3 MÅNADER MED ORDERINGÅNG PER (RULLANDE) ÅR                          
104120*                                AX INDEX RULLANDE ÅR BAKÅT               
104121*                                PX INDEX PERIOD (KALENDER,12)            
104122*                                VX INDEX VECKA UNDER ÅRET                
104123                                                                          
104124     IF SEAS-SEASON-ARTIKEL = SPACE AND                                   
104125        WS-ANTAL-HIST-AR > ZERO                                           
104126*       VARJE PERIOD MED INGÅNG MARKERAS MED 1                            
104127        MOVE 1      TO AX                                                 
104128        MOVE WS-PER TO PX                                                 
104129        PERFORM UNTIL AX > WS-ANTAL-HIST-AR                               
104130           PERFORM UNTIL PX = WS-PER                                      
104131             ADD +1 TO PX                                                 
104132             IF PX > 12                                                   
104133                MOVE 1 TO PX                                              
104134             END-IF                                                       
104135             MOVE WS-FORSTA-V (PX) TO VX                                  
104136             PERFORM UNTIL VX > WS-SISTA-V (PX)                           
104137                IF TAB-AAR-KVOI-PROG (AX, VX) > ZERO                      
104138                   MOVE 1 TO TAB-A-P (AX, PX)                             
104139                END-IF                                                    
104140                ADD +1 TO VX                                              
104141             END-PERFORM                                                  
104142           END-PERFORM                                                    
104143           ADD +1 TO AX                                                   
104144        END-PERFORM                                                       
104145                                                                          
104146*       SUMMERA VARJE ÅRS PERIODTRÄFFAR                                   
104147        MOVE  +1 TO AX                                                    
104148        PERFORM UNTIL AX > WS-ANTAL-HIST-AR                               
104149           MOVE +1 TO PX                                                  
104150           PERFORM UNTIL PX > 12                                          
104151              ADD TAB-A-P (AX, PX) TO TAB-PSUM (AX)                       
104152              ADD +1 TO PX                                                
104153           END-PERFORM                                                    
104154           ADD +1 TO AX                                                   
104155        END-PERFORM                                                       
104156                                                                          
104157*       HAR VARJE ÅR MINST 3 ORDERINGÅNGS-PERIODER ?                      
104158        MOVE ZERO TO ASUM                                                 
104159        MOVE +1 TO AX                                                     
104160        PERFORM UNTIL AX > WS-ANTAL-HIST-AR                               
104161           IF TAB-PSUM (AX) > 2                                           
104162              ADD +1 TO ASUM                                              
104163           END-IF                                                         
104164           ADD +1 TO AX                                                   
104165        END-PERFORM                                                       
104166                                                                          
104167        IF ASUM < WS-ANTAL-HIST-AR                                        
104168           MOVE NEJ TO SEAS-SEASON-ARTIKEL                                
104169        END-IF                                                            
104170     END-IF                                                               
104171     .                                                                    
104172     EJECT                                                                
104173* IMS SEKTIONER                                                           
104174                                                                          
104175                                                                          
104176 IMS-GU-L811  SECTION.                                                    
104177                                                                          
104178     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
104180          DELIMITED BY SIZE INTO SSA1                                     
104200     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
104300          DELIMITED BY SIZE INTO SSA2                                     
104400     MOVE '  GE' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GU WDL8-PCB                                       
104600                                 DLI-IO-AREA-WDL811 SSA1 SSA2             
104700     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
104800     PERFORM IMS-STATUSKONTROLL                                           
104900     .                                                                    
105000     EJECT                                                                
105300 IMS-STATUSKONTROLL SECTION.                                              
105400     SET STATUS-IX TO 1                                                   
105500     SEARCH GODK-STATUS                                                   
105600       AT END                                                             
105700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
105800         DELIMITED BY SIZE INTO FELTEXT                                   
105900         CALL FELLOG                                                      
106000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
106100         CONTINUE                                                         
106200     END-SEARCH                                                           
106300     .                                                                    

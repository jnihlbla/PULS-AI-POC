000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4123100.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   91/05/31.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION: PROGRAMMET SKAPAR EN UTFIL FÖR DAGENS                      
001000*              REGISTRERADE ORDER SOM KAN GENERERA                        
001100*              ORDERBEKRÄFTELSER.                                         
001200*                                                                         
001300*              PROGRAMMET LÄSER WLORQM (WDQ1)                             
001400*              PROGRAMMET LÄSER WLGMTA (WDB2)                             
001500*                                                                         
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL DAGENS REGISTRERADE ORDER                            
002600     SELECT W41230                     ASSIGN TO W41231D1.                
002700*          --- UTFIL DAGENS REGISTRERADE ORDER SOM KAN                    
002800*          ---       GENERERA ORDERBEKRÄFTELSER                           
002900     SELECT W41231                     ASSIGN TO W41231D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W41230                                                               
003600     LABEL RECORD STANDARD                                                
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  IN30-POST  -COPY W412030  -L                                         
004100     EJECT                                                                
004200 FD  W41231                                                               
004300     LABEL RECORD STANDARD                                                
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700*01  UT31-POST  -COPY W412031  -L                                         
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005001                                                                          
005010*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W4123100'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  W41230-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W41230                       VALUE 'J'.                   
005700                                                                          
005800 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
005900     88  FIRST-TIME                          VALUE 'J'.                   
006000                                                                          
006100     EJECT                                                                
006200 77  WS-KDBEKALT                 PIC S9(1) COMP-3 VALUE ZERO.             
006300 01  DAGENS-DATUM-I-DELAR.                                                
006400   03  DAGENS-DATUM-AAR          PIC 9(2)  VALUE ZERO.                    
006500   03  DAGENS-DATUM-MAANAD       PIC 9(2)  VALUE ZERO.                    
006600   03  DAGENS-DATUM-DAG          PIC 9(2)  VALUE ZERO.                    
006700                                                                          
006800 01  DAGENS-DATUM REDEFINES DAGENS-DATUM-I-DELAR     PIC 9(6).            
006900 77  SPAR-IDDISTR                PIC S9(5) COMP-3 VALUE ZERO.             
007000 77  SPAR-IDKUNDNR               PIC S9(7) COMP-3 VALUE ZERO.             
007100 77  SPAR-IDKUNDRF               PIC X(10) VALUE SPACE.                   
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
008000     SKIP2                                                                
008100                                                                          
008200*    ---- PARAMETRAR TILL DATKORT                                         
008300 01  FILLER                      PIC X(8)    VALUE 'DATKORT'.             
008400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41231'.              
008500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008600*01  -COPY WDATKORT                                                       
008700     EJECT                                                                
008800                                                                          
008900*    --- PARAMETRAR TILL ABEND                                            
009000                                                                          
009100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009300     SKIP2                                                                
009400 01  FELTEXT.                                                             
009500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*                                                                         
010000*01  -COPY W0005   -PRE  POSTSUM-                                         
010100     EJECT                                                                
010200 01  IN30-AREA-START             PIC X(24)   VALUE                        
010300                                 'IN30-AREA-START  '.                     
010400     SKIP2                                                                
010500                                                                          
010600*01  AREA -COPY W412030     -PRE IN30-                                    
010700     EJECT                                                                
010800 01  UT31-AREA-START             PIC X(24)   VALUE                        
010900                                 'UT31-AREA-START  '.                     
011000     SKIP2                                                                
011100                                                                          
011200*01  AREA -COPY W412031     -PRE UT31-                                    
011300     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP3                                                                
011900 01  NYCKLAR-TILL-DLI.                                                    
012000*    ---------TILL WDQ101                                                 
012100     03  W-WDQ101KY-MIN-X.                                                
012200         05  W-OBKR-IDORDER-MIN   PIC S9(7)   VALUE ZERO COMP-3.          
012600         05  FILLER               PIC X(13)   VALUE LOW-VALUE.            
012800                                                                          
012900     03  W-WDQ101KY-MAX-X.                                                
013000         05  W-OBKR-IDORDER-MAX   PIC S9(7)   VALUE ZERO COMP-3.          
013010         05  FILLER               PIC X(13)   VALUE HIGH-VALUE.           
013020                                                                          
013600     03  W-IDSYSTEM-X.                                                    
013700         05  W-OBKR-IDSYSTEM      PIC X(4)     VALUE 'PROF'.              
013800     03  W-TIREGDAT-X.                                                    
013900         05  W-OBKR-TIREGDAT      PIC S9(7)    VALUE ZERO COMP-3.         
014000                                                                          
014100*    ---------TILL WDB201                                                 
014200     03  W-IDGMT-X.                                                       
014300         05  W-IDDISTR            PIC S9(5)    VALUE ZERO COMP-3.         
014500         05  W-IDKUNDNR           PIC S9(7)    VALUE ZERO COMP-3.         
014600     SKIP2                                                                
014700*    --- STATUS-KOD FRÅN IMS                                              
014800 01  STATUS-WS                   PIC XX.                                  
014900     88  SEGMENT-FINNS                       VALUE '  '.                  
015000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015100     88  BASEN-SLUT                          VALUE 'GB'.                  
015200     SKIP2                                                                
015300 01  GODK-STATUSKODER.                                                    
015400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015600 01  SSA1                        PIC X(128).                              
015700 01  SSA2                        PIC X(64).                               
015800     EJECT                                                                
015900*    --- IMS FUNKTIONSKODER                                               
016000*01  -COPY W0003                                                          
016100     EJECT                                                                
016200*    ---  DLI INPUT-OUTPUT AREA                                           
016300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016400     SKIP3                                                                
016500 01  DLI-IO-AREA-ORQM.                                                    
016600     03  WLORQM01.                                                        
016700*        05  -COPY WDQ101                                                 
016800     EJECT                                                                
016900 01  DLI-IO-AREA-GMTA.                                                    
017000     03  WLGMTA01.                                                        
017100*        05  -COPY WDB201                                                 
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400                                                                          
017500     EJECT                                                                
017600*01  -COPY W0008  -PRE ORQM-                                              
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
017900*01  -COPY W0008  -PRE GMTA-                                              
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200 PROCEDURE DIVISION  USING ORQM-PCB GMTA-PCB.                             
018300     ENTRY 'DLITCBL' USING ORQM-PCB GMTA-PCB.                             
018400                                                                          
018500     SKIP2                                                                
018600     PERFORM A-INIT                                                       
018700                                                                          
018800     PERFORM S01-READ-W41230                                              
018900                                                                          
019000     PERFORM UNTIL END-OF-W41230                                          
019100                                                                          
019200       PERFORM B-BEHANDLA-INDATA                                          
019300                                                                          
019400       PERFORM S01-READ-W41230                                            
019500                                                                          
019600     END-PERFORM                                                          
019700                                                                          
019800     PERFORM C-BEHANDLA-PROFORMA                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN INPUT  W41230                                                   
020900     OPEN OUTPUT W41231                                                   
021000     SKIP2                                                                
021100                                                                          
021200     MOVE NEJ TO W41230-EOF-SW                                            
021300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021400                                                                          
021500                                                                          
021600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
021700                                                                          
021800     MOVE D-AAR              TO DAGENS-DATUM-AAR                          
021900     MOVE D-MAANAD           TO DAGENS-DATUM-MAANAD                       
022000     MOVE D-DAG              TO DAGENS-DATUM-DAG                          
022100                                                                          
022200     MOVE DAGENS-DATUM       TO W-OBKR-TIREGDAT                           
022300                                                                          
022400     MOVE LOW-VALUE          TO W-WDQ101KY-MIN-X                          
022500     MOVE HIGH-VALUE         TO W-WDQ101KY-MAX-X                          
022600     .                                                                    
022700     EJECT                                                                
022800 B-BEHANDLA-INDATA SECTION.                                               
022900                                                                          
023000     PERFORM BA-INIT-KEYVALUE                                             
023100                                                                          
023200     PERFORM BB-KOLLA-VILLKOR                                             
023300     .                                                                    
023400     EJECT                                                                
023500 BA-INIT-KEYVALUE SECTION.                                                
023600     MOVE IN30-IDORDER       TO W-OBKR-IDORDER-MIN                        
023700                                W-OBKR-IDORDER-MAX                        
023800     MOVE IN30-IDDISTR       TO W-IDDISTR                                 
023900     MOVE IN30-IDKUNDNR      TO W-IDKUNDNR                                
024000     .                                                                    
024100     EJECT                                                                
024200 BB-KOLLA-VILLKOR SECTION.                                                
024300                                                                          
024400     PERFORM IMS-GU-GMTA01                                                
024500     IF SEGMENT-FINNS                                                     
024600       IF GMT-KDBEKALT > +0                                               
024700          MOVE GMT-KDBEKALT TO WS-KDBEKALT                                
024800          PERFORM S11-SKRIV-W41231                                        
024900       END-IF                                                             
025000     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 C-BEHANDLA-PROFORMA SECTION.                                             
025400     MOVE JA                 TO FIRST-TIME-SW                             
025500     MOVE LOW-VALUE          TO W-WDQ101KY-MIN-X                          
025600     MOVE HIGH-VALUE         TO W-WDQ101KY-MAX-X                          
025700     PERFORM IMS-GU-ORQM-ORQM01                                           
025800     IF SEGMENT-FINNS                                                     
025900        MOVE OBKR-IDDISTR    TO SPAR-IDDISTR                              
026000        MOVE OBKR-IDKUNDNR   TO SPAR-IDKUNDNR                             
026100        MOVE OBKR-IDKUNDRF   TO SPAR-IDKUNDRF                             
026200        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
026300           IF OBKR-BERADREF NOT = 'W480      '                            
026400              IF FIRST-TIME OR                                            
026500                 OBKR-IDDISTR NOT = SPAR-IDDISTR OR                       
026600                 OBKR-IDKUNDNR NOT = SPAR-IDKUNDNR OR                     
026700                 OBKR-IDKUNDRF NOT = SPAR-IDKUNDRF                        
026800                 MOVE OBKR-IDDISTR TO SPAR-IDDISTR                        
026900                 MOVE OBKR-IDKUNDNR TO SPAR-IDKUNDNR                      
027000                 MOVE OBKR-IDKUNDRF TO SPAR-IDKUNDRF                      
027100                 PERFORM CA-SKRIV-W41231                                  
027200                 MOVE NEJ       TO FIRST-TIME-SW                          
027300              END-IF                                                      
027400           END-IF                                                         
027500           PERFORM IMS-GN-ORQM-ORQM01                                     
027600        END-PERFORM                                                       
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 CA-SKRIV-W41231 SECTION.                                                 
028100     SKIP2                                                                
028200     MOVE OBKR-IDDISTR       TO UT31-IDDISTR                              
028300     MOVE OBKR-IDKUNDNR      TO UT31-IDKUNDNR                             
028400     MOVE OBKR-IDKUNDRF      TO UT31-IDKUNDRF                             
028500     MOVE '002'              TO UT31-IDPTYP                               
028600     MOVE OBKR-IDORDER       TO UT31-IDORDER                              
028700     MOVE OBKR-TIREGDAT      TO UT31-TIREGDAT                             
028800     MOVE OBKR-KDORDKL       TO UT31-KDORDKL                              
028900     MOVE +0                 TO UT31-KDBEKALT                             
029000     WRITE UT31-POST FROM UT31-AREA                                       
029100                                                                          
029200     MOVE UT31-IDPTYP TO POSTSUM-TRANSTYP                                 
029300     MOVE 'W41231' TO POSTSUM-FDNAMN                                      
029400     MOVE 'W41231D1' TO POSTSUM-DDNAMN2                                   
029500     CALL POSTSUM USING POSTSUM-PARM                                      
029600     .                                                                    
029700     EJECT                                                                
029800 Z-FINIT SECTION.                                                         
029900     CLOSE W41230                                                         
030000     CLOSE W41231                                                         
030100     SKIP2                                                                
030200     MOVE 'S' TO POSTSUM-OPKOD                                            
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500     EJECT                                                                
030600 S01-READ-W41230 SECTION.                                                 
030700     SKIP2                                                                
030800     READ W41230 RECORD INTO IN30-AREA                                    
030900     AT END                                                               
031000        MOVE JA              TO W41230-EOF-SW                             
031100     END-READ                                                             
031200     .                                                                    
031300     EJECT                                                                
031400 S11-SKRIV-W41231 SECTION.                                                
031500     SKIP2                                                                
031600     MOVE IN30-IDDISTR       TO UT31-IDDISTR                              
031700     MOVE IN30-IDKUNDNR      TO UT31-IDKUNDNR                             
031800     MOVE IN30-IDKUNDRF      TO UT31-IDKUNDRF                             
031900     MOVE '001'              TO UT31-IDPTYP                               
032000     MOVE IN30-IDORDER       TO UT31-IDORDER                              
032100     MOVE IN30-TIREGDAT      TO UT31-TIREGDAT                             
032200     MOVE IN30-KDORDKL       TO UT31-KDORDKL                              
032300     MOVE WS-KDBEKALT        TO UT31-KDBEKALT                             
032400     WRITE UT31-POST FROM UT31-AREA                                       
032500                                                                          
032600     MOVE UT31-IDPTYP TO POSTSUM-TRANSTYP                                 
032700     MOVE 'W41231' TO POSTSUM-FDNAMN                                      
032800     MOVE 'W41231D1' TO POSTSUM-DDNAMN2                                   
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200* --- IMS SEKTIONER ---                                                   
033300     SKIP3                                                                
033600 IMS-GU-ORQM-ORQM01 SECTION.                                              
033700     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
033800                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
033900                    '&TIREGDAT =' W-TIREGDAT-X                            
034000                    '&IDSYSTEM =' W-IDSYSTEM-X ')'                        
034100          DELIMITED BY SIZE INTO SSA1                                     
034200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
034300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-ORQM SSA1                 
034400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
034500     PERFORM IMS-STATUSKONTROLL                                           
034600     .                                                                    
034700                                                                          
034800 IMS-GN-ORQM-ORQM01 SECTION.                                              
034900     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
035000                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
035100                    '&TIREGDAT =' W-TIREGDAT-X                            
035200                    '&IDSYSTEM =' W-IDSYSTEM-X ')'                        
035300          DELIMITED BY SIZE INTO SSA1                                     
035400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
035500     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA-ORQM SSA1                 
035600     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     EJECT                                                                
036000 IMS-GU-GMTA01 SECTION.                                                   
036100     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
036200          DELIMITED BY SIZE INTO SSA1                                     
036500     MOVE '  GE' TO GODK-STATUSKODER                                      
036600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-GMTA SSA1                 
036700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000     EJECT                                                                
037100 IMS-STATUSKONTROLL SECTION.                                              
037200     SKIP2                                                                
037300     SET STATUS-IX TO 1                                                   
037400     SEARCH GODK-STATUS                                                   
037500       AT END CALL FELLOG                                                 
037600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
037700     END-SEARCH                                                           
037800     .                                                                    

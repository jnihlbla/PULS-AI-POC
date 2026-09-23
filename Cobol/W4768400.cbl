000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4768400.                                                 
000300 AUTHOR.        THOMAS LARSSON.                                           
000400 DATE-WRITTEN.  AUGUSTI 2021.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*        PROGRAM READS WDM/ WITH FLKLAR = YES                             
000900*        IF TIREGDAT + 21 DAYS <= TODAYS DATE                             
001000*        CREATE CLEANING FILE                                             
001100*                                                                         
001200*        PGM:ET LÄSER:                                                    
001300*        * WDM7                                                           
001400*                                                                         
001500*        SKAPAR FIL MED INFO FÖR RENSNING                                 
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*- - - - - - - - - - - - UTFIL:                                           
002400*          --- FIL RENSNING                                               
002500     SELECT W4768A                       ASSIGN TO W47684D1.              
002600     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000 FD  W4768A                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300     SKIP2                                                                
003400 01  UT-RENS.                                                             
003500*03   -COPY W476TU3   -L.                                                 
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4768400'.               
004100                                                                          
004200 77  IX                          PIC S9(9) COMP SYNC VALUE +0.            
004300 77  MAX-IX                      PIC S9(9) COMP SYNC VALUE +1000.         
004400 77  FORF-DATUM                  PIC 9(6)   VALUE ZERO.                   
004500 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
004600 77  DAGENS-DATUM-PLUS-21-DAGAR PIC 9(6)   VALUE ZERO.                    
004610 77  DAGENS-DATUM-PLUS-30-DAGAR PIC 9(6)   VALUE ZERO.                    
004620 77  DAGENS-DATUM-PLUS-180-DAGAR PIC 9(6)  VALUE ZERO.                    
004700                                                                          
004800 77  INFIL-EOF-SW                PIC X    VALUE 'N'.                      
004900     88  END-OF-W476IN                    VALUE 'J'.                      
005000                                                                          
005100 01  DATUM-AR-DAGNR              PIC 9(5).                                
005200 01  FILLER REDEFINES DATUM-AR-DAGNR.                                     
005300     03  DATUM-AR                PIC 9(2).                                
005400     03  DATUM-DAGNR             PIC 9(3).                                
005500                                                                          
005600 01  MAX-FORF-DATUM              PIC 9(6)   VALUE ZERO.                   
005700 01  FILLER REDEFINES MAX-FORF-DATUM.                                     
005800     03  MAX-AR                  PIC 9(2).                                
005900     03  MAX-MAN                 PIC 9(2).                                
006000     03  FILLER                  PIC 9(2).                                
006100                                                                          
006200                                                                          
006300 01  FILLER                      PIC X(16)  VALUE 'WS-SEKTION'.           
006400 01  WS-SEKTION                  PIC X(30)  VALUE SPACE.                  
006500                                                                          
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900     EJECT                                                                
007000 01  TEST-IDDISTR     PIC S9(5)        VALUE ZERO  COMP-3.                
007100*01  FILLER     -COPY WWDIST35   -RED  TEST-IDDISTR.                      
007200     EJECT                                                                
007900******************************************************************        
008000*       CONSTANTS                                                *        
008100******************************************************************        
008200     SKIP2                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'CONSTANTS '.          
008400 01  KONSTANTER.                                                          
008500     03  JA                      PIC X(1)    VALUE 'J'.                   
008600     03  YES                     PIC X(1)    VALUE 'Y'.                   
008700     03  NEJ                     PIC X(1)    VALUE 'N'.                   
008710     03  SENT                    PIC X(1)    VALUE 'S'.                   
008800     SKIP2                                                                
008900                                                                          
009000 01  SPAR-IDDISTR                PIC S9(5)  VALUE ZERO COMP-3.            
009100                                                                          
009200     SKIP3                                                                
009300******************************************************************        
009400*       VARIABLES                                                *        
009500******************************************************************        
009600     SKIP2                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'VARIABLES'.           
009800 01  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
009900     SKIP3                                                                
010000     EJECT                                                                
010100*    --- VALID IDDC CODES                                                 
010200*                                                                         
010300*    -COPY WWDCKONS                                                       
010400     EJECT                                                                
010500 01  DYNAMISKA-SUBPROGRAM.                                                
010600   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
010700   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010800   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
010900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
011000   03    WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011100     SKIP2                                                                
011200 01  RETURKODER.                                                          
011300     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16   COMP SYNC.         
011400     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
011500     SKIP2                                                                
011600*01   -COPY W0005       -PRE POSTSUM-.                                    
011700     EJECT                                                                
011800 01  FILLER                PIC X(16) VALUE 'WDATKONV-IO-AREA'.            
011900*   -COPY WDATAREA                                                        
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
012200 01  IN-AREA                       PIC X(80) VALUE SPACE.                 
012300 01  FILLER                        REDEFINES IN-AREA.                     
012400     03  IN-IDTULL                 PIC X(10).                             
012500     EJECT                                                                
012600 01  RENS-AREA-START             PIC X(16)   VALUE                        
012700                                 'RENS-AREA-START '.                      
012800     SKIP2                                                                
012900*01  RENS-AREA -COPY W476TU3                                              
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013200     SKIP3                                                                
013300 01  NYCKLAR-TILL-DLI.                                                    
013400                                                                          
013500     03  W-WDM701KY-X.                                                    
013600         05  W-M7-IDFAKT         PIC S9(7)    VALUE ZERO COMP-3.          
013700         05  W-M7-IDORDNR7       PIC S9(7)    VALUE ZERO COMP-3.          
013800         05  W-M7-IDKOLLI        PIC S9(5)    VALUE ZERO COMP-3.          
013900         05  W-M7-IDPRODNR       PIC S9(7)    VALUE ZERO COMP-3.          
014000*                                                                         
014100     03  W-WDM701KY-MIN-X.                                                
014200         05  FILLER              PIC X(15)   VALUE LOW-VALUE.             
014300                                                                          
014400     03  W-WDM701KY-MAX-X.                                                
014500         05  FILLER              PIC X(15)   VALUE HIGH-VALUE.            
014600                                                                          
014700*    03  W-WDM701KY-MIN-X.                                                
014800*        05  W-IDFAKT-MIN        PIC S9(7)   VALUE ZERO COMP-3.           
014900*        05  W-IDORDNR7-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
015000*        05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
015100*        05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
015200*                                                                         
015300*    03  W-WDM701KY-MAX-X.                                                
015400*        05  W-IDFAKT-MAX        PIC S9(7)   VALUE ZERO COMP-3.           
015500*        05  FILLER              PIC X(11)   VALUE HIGH-VALUE.            
015600                                                                          
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FINNS                       VALUE '  '.                  
016000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016200     88  BASEN-SLUT                          VALUE 'GB'.                  
016300     SKIP2                                                                
016400 01  GODK-STATUSKODER.                                                    
016500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016600     SKIP3                                                                
016700 01  SSA1                        PIC X(128).                              
016800 01  SSA2                        PIC X(64).                               
016900 01  SSA3                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNKTIONSKODER                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDM701'.         
017600 01  DLI-IO-WDM701.                                                       
017700*    03  -COPY WDM701                                                     
017800     EJECT                                                                
017900 LINKAGE SECTION.                                                         
018000*01  -COPY W0008  -PRE WDM7-                                              
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300 PROCEDURE DIVISION  USING WDM7-PCB.                                      
018400 MAIN SECTION.                                                            
018500     ENTRY 'DLITCBL' USING WDM7-PCB.                                      
018600                                                                          
018700     PERFORM A-INIT                                                       
018800                                                                          
018900     PERFORM IMS-GU-WDM701                                                
019000     IF SEGMENT-FINNS                                                     
019100                                                                          
019200       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
019300                                                                          
019400                                                                          
019500         PERFORM D-SKRIV-RENSNINGSFIL                                     
019600                                                                          
019700         PERFORM IMS-GN-WDM701                                            
019800                                                                          
019900       END-PERFORM                                                        
020000                                                                          
020100     END-IF                                                               
020200     PERFORM Z-FINIT                                                      
020300                                                                          
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 A-INIT SECTION.                                                          
020900     MOVE 'A-INIT            '      TO WS-SEKTION                         
021100     OPEN OUTPUT W4768A                                                   
021200                                                                          
021300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
021400                                                                          
021500     ACCEPT DAGENS-DATUM FROM DATE                                        
021600     MOVE LOW-VALUE    TO W-WDM701KY-MIN-X                                
021700                                                                          
021800     MOVE HIGH-VALUE   TO W-WDM701KY-MAX-X                                
021900     .                                                                    
022000     EJECT                                                                
022100                                                                          
022110******************************************************************        
022120*CLEANING RULES-WRITE IN O/P FILE FOR FOLLOWING CASES                     
022130*HUV-FLKLAR = Y:REFILL ORDERS    -IF RELEASE DATE IS PAST 21 DAYS         
022140*               NON REFILL ORDERS-IF RELEASE DATE IS PAST 14 DAYS         
022150*HUV-FLKLAR = S:IF RELEASE DATE IS PAST 30 DAYS[EXCEPT DIST 7050]         
022160*HUV-FLKLAR =  :IF INVOICE DATE IS PAST 180 DAYS[EXCEPT DIST 7050]        
022170******************************************************************        
022200 D-SKRIV-RENSNINGSFIL SECTION.                                            
022300     MOVE 'D-SKRIV-RENSN.FIL '   TO WS-SEKTION                            
022320                                                                          
022400     IF HUV-FLKLAR = YES                                                  
022500       PERFORM DA-SAETT-FORF-DATUM                                        
022600                                                                          
022610       IF DAGENS-DATUM > DAGENS-DATUM-PLUS-21-DAGAR                       
022620         PERFORM S10-FILL-UT-FIELDS                                       
022630         PERFORM S11-SKRIV-W4768A                                         
022640       END-IF                                                             
022650     ELSE                                                                 
022660       IF HUV-IDDISTR NOT = 7050                                          
022670         IF HUV-FLKLAR = SENT                                             
022680           PERFORM DB-CHECK-FLKLAR-S                                      
022690           IF DAGENS-DATUM > DAGENS-DATUM-PLUS-30-DAGAR                   
022692             PERFORM S10-FILL-UT-FIELDS                                   
022693             PERFORM S11-SKRIV-W4768A                                     
022694           END-IF                                                         
022695         ELSE                                                             
022696           IF HUV-TIFAKT > 0                                              
022697             PERFORM DC-CHECK-FLKLAR-SPACE                                
022699             IF DAGENS-DATUM > DAGENS-DATUM-PLUS-180-DAGAR                
022702               PERFORM S10-FILL-UT-FIELDS                                 
022710               PERFORM S11-SKRIV-W4768A                                   
022720             END-IF                                                       
022721           END-IF                                                         
022730         END-IF                                                           
022740       END-IF                                                             
022750     END-IF                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 DA-SAETT-FORF-DATUM  SECTION.                                            
024510     MOVE 'DA-SAETT-FORF-DAT '   TO WS-SEKTION                            
024600                                                                          
024800     MOVE HUV-IDDISTR        TO TEST-IDDISTR                              
025000     MOVE HUV-TIREGDAT       TO FORF-DATUM                                
025100     MOVE FORF-DATUM         TO DAT-I-TIDATUM                             
025200     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
025300                                                                          
025400                                                                          
025500     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
025600                         DAT-O-TIDATUM, DAT-KDSVAR                        
025700     IF DAT-KDSVAR-OK                                                     
025800        MOVE DAT-TIAADDD        TO DATUM-AR-DAGNR                         
025900     ELSE                                                                 
026000*       DATUMKONVERTERING HAR GÅTT FEL                                    
026100        MOVE                                                              
026200*       'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'                    
026300        'KONVERTERING NUMMER ETT GETT RETURKOD > NOLL'                    
026400                                     TO FELTEXT                           
026500        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
026600     END-IF                                                               
026700                                                                          
026800     IF DIST35-REFILL                                                     
026900       COMPUTE DATUM-DAGNR = DATUM-DAGNR + 21                             
027000     ELSE                                                                 
027100       COMPUTE DATUM-DAGNR = DATUM-DAGNR + 14                             
027200     END-IF                                                               
027300                                                                          
027400     IF DATUM-DAGNR > 365                                                 
027500        COMPUTE DATUM-DAGNR = DATUM-DAGNR - 365                           
027600        ADD 1 TO DATUM-AR                                                 
027700     END-IF                                                               
027800                                                                          
027900     MOVE DATUM-AR-DAGNR     TO DAT-I-TIDATUM                             
028000     MOVE 'AADDD'            TO DAT-KDDATFORM                             
028100                                                                          
028200     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
028300                         DAT-O-TIDATUM, DAT-KDSVAR                        
028400     IF DAT-KDSVAR-OK                                                     
028500        MOVE DAT-TIAAMMDD       TO DAGENS-DATUM-PLUS-21-DAGAR             
028600     ELSE                                                                 
028700*       DATUMKONVERTERING HAR GÅTT FEL                                    
028800        MOVE                                                              
028900        'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'                    
029000                                     TO FELTEXT                           
029100        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500                                                                          
029510 DB-CHECK-FLKLAR-S SECTION.                                               
029520     MOVE 'DB-CHECK-FLKLAR-S '   TO WS-SEKTION                            
029530                                                                          
029540     MOVE HUV-IDDISTR            TO TEST-IDDISTR                          
029550     MOVE HUV-TIREGDAT           TO FORF-DATUM                            
029560     MOVE FORF-DATUM             TO DAT-I-TIDATUM                         
029570     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
029590                                                                          
029591     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
029592                         DAT-O-TIDATUM, DAT-KDSVAR                        
029593                                                                          
029594     IF DAT-KDSVAR-OK                                                     
029595        MOVE DAT-TIAADDD         TO DATUM-AR-DAGNR                        
029596     ELSE                                                                 
029597        MOVE 'KONVERTERING NUMMER ETT GETT RETURKOD > NOLL'               
029599                                 TO FELTEXT                               
029600        CALL ABEND               USING RKOD-ABEND-MED-DUMP                
029601     END-IF                                                               
029602                                                                          
029603     COMPUTE DATUM-DAGNR    = DATUM-DAGNR + 30                            
029604                                                                          
029605     IF DATUM-DAGNR > 365                                                 
029606        COMPUTE DATUM-DAGNR = DATUM-DAGNR - 365                           
029607        ADD 1                    TO DATUM-AR                              
029608     END-IF                                                               
029609                                                                          
029610     MOVE DATUM-AR-DAGNR         TO DAT-I-TIDATUM                         
029611     MOVE 'AADDD'                TO DAT-KDDATFORM                         
029612                                                                          
029613     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
029614                         DAT-O-TIDATUM, DAT-KDSVAR                        
029615                                                                          
029616     IF DAT-KDSVAR-OK                                                     
029617        MOVE DAT-TIAAMMDD        TO DAGENS-DATUM-PLUS-30-DAGAR            
029618     ELSE                                                                 
029619        MOVE 'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'               
029622                                 TO FELTEXT                               
029623        CALL ABEND               USING RKOD-ABEND-MED-DUMP                
029624     END-IF                                                               
029625     .                                                                    
029626     EJECT                                                                
029627 DC-CHECK-FLKLAR-SPACE SECTION.                                           
029628     MOVE 'DC-CHECK-FLKLAR-SP'   TO WS-SEKTION                            
029629                                                                          
029630     MOVE HUV-IDDISTR            TO TEST-IDDISTR                          
029631     MOVE HUV-TIFAKT             TO FORF-DATUM                            
029632     MOVE FORF-DATUM             TO DAT-I-TIDATUM                         
029633     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
029634                                                                          
029635                                                                          
029636     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
029637                         DAT-O-TIDATUM, DAT-KDSVAR                        
029638     IF DAT-KDSVAR-OK                                                     
029639        MOVE DAT-TIAADDD         TO DATUM-AR-DAGNR                        
029640     ELSE                                                                 
029641        MOVE 'KONVERTERING NUMMER ETT GETT RETURKOD > NOLL'               
029643                                 TO FELTEXT                               
029644        CALL ABEND               USING RKOD-ABEND-MED-DUMP                
029645     END-IF                                                               
029646                                                                          
029647     COMPUTE DATUM-DAGNR    = DATUM-DAGNR + 180                           
029648                                                                          
029649     IF DATUM-DAGNR > 365                                                 
029650        COMPUTE DATUM-DAGNR = DATUM-DAGNR - 365                           
029651        ADD 1                    TO DATUM-AR                              
029652     END-IF                                                               
029653                                                                          
029654     MOVE DATUM-AR-DAGNR         TO DAT-I-TIDATUM                         
029655     MOVE 'AADDD'                TO DAT-KDDATFORM                         
029656                                                                          
029657     CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,                    
029658                         DAT-O-TIDATUM, DAT-KDSVAR                        
029659                                                                          
029660     IF DAT-KDSVAR-OK                                                     
029661        MOVE DAT-TIAAMMDD        TO DAGENS-DATUM-PLUS-180-DAGAR           
029662     ELSE                                                                 
029663        MOVE 'DATUMKONVERTERINGEN HAR GETT RETURKOD > NOLL'               
029665                                 TO FELTEXT                               
029666        CALL ABEND               USING RKOD-ABEND-MED-DUMP                
029667     END-IF                                                               
029668     .                                                                    
029669     EJECT                                                                
029670 Z-FINIT SECTION.                                                         
029700     MOVE 'Z-FINIT           '   TO WS-SEKTION                            
029800                                                                          
030000     CLOSE W4768A                                                         
030100     SKIP2                                                                
030200     MOVE 'S' TO POSTSUM-OPKOD                                            
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500     EJECT                                                                
030510 S10-FILL-UT-FIELDS SECTION.                                              
030520     MOVE 'S10-FILL-UT-FIELD'   TO WS-SEKTION                             
030530                                                                          
030540     MOVE 'TU3'            TO TU3-IDPTYP                                  
030550     MOVE HUV-IDFAKT       TO TU3-IDFAKT                                  
030560     MOVE HUV-IDORDNR7     TO TU3-IDORDNR7                                
030570     MOVE HUV-IDKOLLI      TO TU3-IDKOLLI                                 
030580     MOVE HUV-IDPRODNR     TO TU3-IDPRODNR                                
030592     .                                                                    
030593     EJECT                                                                
030600 S11-SKRIV-W4768A SECTION.                                                
030700     MOVE 'S11-SKRIV-W4768A  '   TO WS-SEKTION                            
030800                                                                          
031000     WRITE UT-RENS FROM RENS-AREA                                         
031100                                                                          
031200     MOVE TU3-IDPTYP TO POSTSUM-TRANSTYP                                  
031300     MOVE 'W4768A' TO POSTSUM-FDNAMN                                      
031400     MOVE 'W47684D1' TO POSTSUM-DDNAMN2                                   
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600     .                                                                    
031700     EJECT                                                                
031800 IMS-GU-WDM701  SECTION.                                                  
031900     MOVE 'IMS-GU-WDM701     '      TO WS-SEKTION                         
032000                                                                          
032200     STRING 'WDM701  (WDM701KY>=' W-WDM701KY-MIN-X                        
032300                    '&WDM701KY<=' W-WDM701KY-MAX-X ')'                    
032400           DELIMITED BY SIZE INTO SSA1                                    
032500     MOVE '  GE'   TO GODK-STATUSKODER                                    
032600     CALL CBLTDLI USING GU WDM7-PCB DLI-IO-WDM701 SSA1                    
032700     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000     SKIP2                                                                
033100 IMS-GN-WDM701  SECTION.                                                  
033200     MOVE 'IMS-GN-WDM701     '      TO WS-SEKTION                         
033300                                                                          
033500     STRING 'WDM701  (WDM701KY>=' W-WDM701KY-MIN-X                        
033600                    '&WDM701KY<=' W-WDM701KY-MAX-X ')'                    
033700           DELIMITED BY SIZE INTO SSA1                                    
033800     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
033900     CALL CBLTDLI USING GN WDM7-PCB DLI-IO-WDM701 SSA1                    
034000     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-STATUSKONTROLL SECTION.                                              
034500     MOVE 'IMS-STATUSKONTR.'        TO WS-SEKTION                         
034600                                                                          
034700     SET STATUS-IX TO 1                                                   
034800     SEARCH GODK-STATUS                                                   
034900       AT END                                                             
035000         CALL FELLOG                                                      
035100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035200         CONTINUE                                                         
035300     END-SEARCH                                                           
035400     .                                                                    
035500     EJECT                                                                

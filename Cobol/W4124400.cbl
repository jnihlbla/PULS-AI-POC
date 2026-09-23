000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4124400.                                                
000300 AUTHOR.         GERRY CARMICHAEL.                                        
000400 DATE-WRITTEN.   00/07/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDQ4 FÖR LDC ORDRAR MED RFS INOM 4 DAGAR                   
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDQ4                                       
001100*                              WDB6                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL                                                      
002600     SELECT W41243                     ASSIGN TO W41244D1.                
002700     SKIP2                                                                
002800*          --- UTFIL                                                      
002900     SELECT W41244                     ASSIGN TO W41244D2.                
003000     SKIP2                                                                
003100*          --- UTFIL                                                      
003200     SELECT W41241A                    ASSIGN TO W41244D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W41243                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W41243      -L.                                                
004300     SKIP3                                                                
004400 FD  W41244                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W41244 -PRE  UT- -L.                                      
004900     SKIP3                                                                
005000 FD  W41241A                                                              
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W41244 -PRE  UT1A- -L.                                    
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W4124400'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  W41243-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W41243                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     SKIP2                                                                
007900                                                                          
008000*    --- PARAMETRAR TILL ABEND                                            
008100                                                                          
008200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400 01  IN-AREA-START               PIC X(24)   VALUE                        
009500                                 'IN-AREA-START  '.                       
009600     SKIP2                                                                
009700                                                                          
009800*01  AREA -COPY W41243     -PRE IN-                                       
009900     EJECT                                                                
010000 01  UT-AREA-START               PIC X(24)   VALUE                        
010100                                 'UT-AREA-START  '.                       
010200     SKIP2                                                                
010300*01  AREA -COPY W41244     -PRE UT-                                       
010400     EJECT                                                                
010500 01  UT-AREA-START               PIC X(24)   VALUE                        
010600                                 'UT1A-AREA-START'.                       
010700     SKIP2                                                                
010800*01  AREA -COPY W41244     -PRE UT1A-                                     
010900     EJECT                                                                
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600     03  W-WDQ401KY-MIN-X.                                                
011700         05  W-IDORDER-MIN        PIC S9(7)    VALUE ZERO COMP-3.         
011800         05  W-IDDC-MIN           PIC X(2)     VALUE SPACE.               
011900         05  FILLER               PIC X(14)    VALUE SPACE.               
012000     03  W-WDQ401KY-MAX-X.                                                
012100         05  W-IDORDER-MAX        PIC S9(7)    VALUE ZERO COMP-3.         
012200         05  W-IDDC-MAX           PIC X(2)     VALUE SPACE.               
012300         05  FILLER               PIC X(14)    VALUE SPACE.               
012400                                                                          
012500     03  W-IDDC-B6-X.                                                     
012600         05 W-IDDC-B6                  PIC X(2).                          
012700                                                                          
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(96).                               
013900     EJECT                                                                
014000*    --- IMS FUNKTIONSKODER                                               
014100*01  -COPY W0003                                                          
014200     EJECT                                                                
014300*    ---  DLI INPUT-OUTPUT AREA                                           
014400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ401'.                      
014500 01  DLI-IO-WDQ401.                                                       
014600*    03  -COPY WDQ401                                                     
014700                                                                          
014800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014900 01   DLI-IO-AREA-B601.                                                   
015000*     03  -COPY WDB601                                                    
015100                                                                          
015200     EJECT                                                                
015300 LINKAGE SECTION.                                                         
015400                                                                          
015500                                                                          
015600*01  -COPY W0008  -PRE WDQ4-                                              
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008  -PRE WDB6-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200 PROCEDURE DIVISION  USING WDQ4-PCB WDB6-PCB.                             
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL' USING WDQ4-PCB WDB6-PCB.                             
016500                                                                          
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     PERFORM S01-LAES-W41243                                              
017000     PERFORM UNTIL END-OF-W41243                                          
017100                                                                          
017200       PERFORM B-SKAPA-UTFIL                                              
017300                                                                          
017400       PERFORM S01-LAES-W41243                                            
017500                                                                          
017600     END-PERFORM                                                          
017700                                                                          
017800                                                                          
017900     PERFORM Z-FINIT                                                      
018000                                                                          
018100     MOVE ZERO TO RETURN-CODE                                             
018200     GOBACK                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 A-INIT SECTION.                                                          
018600                                                                          
018700     OPEN INPUT  W41243                                                   
018800                                                                          
018900     OPEN OUTPUT W41244 W41241A                                           
019000                                                                          
019100     ACCEPT DAGENS-DATUM  FROM DATE                                       
019200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019300     .                                                                    
019400     EJECT                                                                
019500 B-SKAPA-UTFIL SECTION.                                                   
019600     MOVE LOW-VALUE  TO W-WDQ401KY-MIN-X                                  
019700     MOVE HIGH-VALUE TO W-WDQ401KY-MAX-X                                  
019800                                                                          
019900     MOVE IN-IDORDER TO W-IDORDER-MIN                                     
020000                        W-IDORDER-MAX                                     
020100     MOVE IN-IDDC    TO W-IDDC-MIN                                        
020200                        W-IDDC-MAX                                        
020300                        W-IDDC-B6                                         
020400     PERFORM IMS-GU-WDB601                                                
020500                                                                          
020600     PERFORM IMS-GU-WDQ401                                                
020700     IF SEGMENT-FINNS                                                     
020800       PERFORM UNTIL SEGMENT-SAKNAS                                       
020900         IF DCS-CDC                                                       
021000           MOVE ORAD-IDARTNR  TO UT-IDARTNR                               
021100           MOVE IN-IDDC-LDC   TO UT-IDDC-LDC                              
021200           MOVE ORAD-IDDC     TO UT-IDDC                                  
021300           MOVE IN-DARFS      TO UT-DARFS                                 
021400           MOVE IN-TIREPDAT   TO UT-TIREPDAT                              
021500           MOVE ORAD-IDDISTR  TO UT-IDDISTR                               
021600           MOVE ORAD-IDKUNDNR TO UT-IDKUNDNR                              
021700           MOVE ORAD-IDORDNR7 TO UT-IDORDNR7                              
021800           MOVE ORAD-KVBEART  TO UT-KVBEART                               
021900           MOVE ORAD-BERADREF TO UT-BERADREF                              
022000                                                                          
022100           PERFORM S11-SKRIV-W41244                                       
022200         ELSE                                                             
022300           IF DCS-SDC AND (DCS-SWEDEN  OR DCS-ENGLAND OR                  
022400                           DCS-HOLLAND OR DCS-ITALY   OR                  
022500                           DCS-GERMANY OR DCS-FINLAND OR                  
022510                           DCS-NORWAY  OR DCS-BELGIUM OR                  
022520                           DCS-FRANCE  OR DCS-SWIZERLAND OR               
022530                           DCS-POLAND)                                    
022600             MOVE ORAD-IDARTNR  TO UT1A-IDARTNR                           
022700             MOVE IN-IDDC-LDC   TO UT1A-IDDC-LDC                          
022800             MOVE ORAD-IDDC     TO UT1A-IDDC                              
022900             MOVE IN-DARFS      TO UT1A-DARFS                             
023000             MOVE IN-TIREPDAT   TO UT1A-TIREPDAT                          
023100             MOVE ORAD-IDDISTR  TO UT1A-IDDISTR                           
023200             MOVE ORAD-IDKUNDNR TO UT1A-IDKUNDNR                          
023300             MOVE ORAD-IDORDNR7 TO UT1A-IDORDNR7                          
023400             MOVE ORAD-KVBEART  TO UT1A-KVBEART                           
023500             MOVE ORAD-BERADREF TO UT1A-BERADREF                          
023600                                                                          
023700             PERFORM S13-SKRIV-W41241A                                    
023800           END-IF                                                         
023900         END-IF                                                           
024000         PERFORM IMS-GN-WDQ401                                            
024100       END-PERFORM                                                        
024200     END-IF                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 Z-FINIT SECTION.                                                         
024600     CLOSE W41243                                                         
024700           W41244                                                         
024800           W41241A                                                        
024900     SKIP2                                                                
025000     MOVE 'S' TO POSTSUM-OPKOD                                            
025100     CALL POSTSUM USING POSTSUM-PARM                                      
025200     .                                                                    
025300     EJECT                                                                
025400 S01-LAES-W41243  SECTION.                                                
025500     READ W41243 INTO IN-AREA                                             
025600     AT END                                                               
025700        MOVE HIGH-VALUE TO IN-AREA                                        
025800        SET END-OF-W41243 TO TRUE                                         
025900                                                                          
026000     NOT AT END                                                           
026100        MOVE 'W41243' TO POSTSUM-FDNAMN                                   
026200        MOVE 'W41244D1' TO POSTSUM-DDNAMN2                                
026300*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
026400*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
026500        MOVE '001'     TO POSTSUM-TRANSTYP                                
026600        CALL POSTSUM USING POSTSUM-PARM                                   
026700     END-READ                                                             
026800     .                                                                    
026900     EJECT                                                                
027000 S11-SKRIV-W41244 SECTION.                                                
027100                                                                          
027200     WRITE UT-POST FROM UT-AREA                                           
027300                                                                          
027400     MOVE '002'     TO POSTSUM-TRANSTYP                                   
027500     MOVE 'W41244' TO POSTSUM-FDNAMN                                      
027600     MOVE 'W41244D2' TO POSTSUM-DDNAMN2                                   
027700     CALL POSTSUM USING POSTSUM-PARM                                      
027800     .                                                                    
027900     EJECT                                                                
028000 S13-SKRIV-W41241A SECTION.                                               
028100                                                                          
028200     WRITE UT1A-POST FROM UT1A-AREA                                       
028300                                                                          
028400     MOVE '003'     TO POSTSUM-TRANSTYP                                   
028500     MOVE 'W41244' TO POSTSUM-FDNAMN                                      
028600     MOVE 'W41244D3' TO POSTSUM-DDNAMN2                                   
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028900     EJECT                                                                
029000* --- IMS SEKTIONER ---                                                   
029100                                                                          
029200     EJECT                                                                
029300 IMS-GU-WDQ401 SECTION.                                                   
029400     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
029500                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
029600          DELIMITED BY SIZE INTO SSA1                                     
029700     MOVE '  GE' TO GODK-STATUSKODER                                      
029800     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
029900     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     .                                                                    
030200     EJECT                                                                
030300 IMS-GN-WDQ401 SECTION.                                                   
030400     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
030500                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
030600          DELIMITED BY SIZE INTO SSA1                                     
030700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
030800     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ401 SSA1                    
030900     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
031000     PERFORM IMS-STATUSKONTROLL                                           
031100     .                                                                    
031200     EJECT                                                                
031300 IMS-GU-WDB601    SECTION.                                                
031400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     MOVE '  GE' TO GODK-STATUSKODER                                      
031700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     IF SEGMENT-SAKNAS                                                    
032100        MOVE SPACE TO DCS-KDDC                                            
032200     END-IF                                                               
032300     .                                                                    
032400 IMS-STATUSKONTROLL SECTION.                                              
032500                                                                          
032600     SET STATUS-IX TO 1                                                   
032700     SEARCH GODK-STATUS                                                   
032800       AT END                                                             
032900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033000           DELIMITED BY SIZE INTO FELTEXT                                 
033100         DISPLAY FELTEXT                                                  
033200         CALL FELLOG                                                      
033300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033400         CONTINUE                                                         
033500     END-SEARCH                                                           
033600     .                                                                    

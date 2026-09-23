000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4323000.                                                 
000400 AUTHOR.        BO SVENSSON.                                              
000500 DATE-WRITTEN.  MARS 1997.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMET ÄR ETT SB-PGM.INGÅENDE I RUTINEN(W432V1).             
001100*                                                                         
001200*        PGM. LÄSER WDB2 KOMPLETTERAR FRÅN WDB1.                          
001300*        DÄREFTER SKAPARS EN UTFIL(W43230)                                
001400*                                                                         
001500*    DB.                                                                  
001600*        DB              WDB201 (SB)                                      
001700*        LÄSER           WDB101                                           
001800*                                                                         
001900*    UTDATA.                                                              
002000*        FIL             W43230                                           
002100                                                                          
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600     SKIP2                                                                
002700 FILE-CONTROL.                                                            
002800                                                                          
002900     SELECT W43230    ASSIGN TO W43230D1.                                 
003000                                                                          
003100     SELECT W43231    ASSIGN TO W43230D2.                                 
003200                                                                          
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500                                                                          
003600 FD  W43230                                                               
003700     LABEL RECORD    STANDARD                                             
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  UTGMT-POST  -COPY W4323002  -L                                       
004200     SKIP3                                                                
004300                                                                          
004400 FD  W43231                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700 01  UT-HEAD-LINE.                                                        
004800     03  FILLER       PIC X(81) VALUE SPACE.                              
004900                                                                          
005000*01  UT2-POST    -COPY W4323001  -L                                       
005100     SKIP3                                                                
005200                                                                          
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600*                                                                         
005700 77  PROGRAM-NAMN                PIC X(08) VALUE 'W4323000'.              
005800                                                                          
005900 77  JA                          PIC X(1)    VALUE 'J'.                   
006000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006100 01  TAB                         PIC X       VALUE X'05'.                 
006200*                                                                         
006300 01  WS-DAGENS-AAAAMMDD          PIC 9(8)    VALUE ZERO.                  
006400 01  DAGENS-DATUM.                                                        
006500     03 DAGENS-AAR               PIC 9(2)    VALUE ZERO.                  
006600     03 DAGENS-MANAD             PIC 9(2)    VALUE ZERO.                  
006700     03 DAGENS-DAG               PIC 9(2)    VALUE ZERO.                  
006800*                                                                         
006900 01  WS-GMT-TISTODAT             PIC 9(6)    VALUE ZERO.                  
007000 01  WS-GMT-TIFAKT               PIC 9(6)    VALUE ZERO.                  
007100*                                                                         
007200 01  HEADLINE-AREA.                                                       
007300     03   HRAD-IDDISTR           PIC X(8)  VALUE 'DISTRIKT'.              
007400     03   FILLER                 PIC X(1)  VALUE X'05'.                   
007500     03   HRAD-IDKUNDNR          PIC X(4)  VALUE 'KUND'.                  
007600     03   FILLER                 PIC X(1)  VALUE X'05'.                   
007700     03   HRAD-BEGMT             PIC X(4)  VALUE 'NAME'.                  
007800     03   FILLER                 PIC X(1)  VALUE X'05'.                   
007900     03   HRAD-CITY              PIC X(4)  VALUE 'CITY'.                  
008000     03   FILLER                 PIC X(1)  VALUE X'05'.                   
008100     03   HRAD-COUNTRY           PIC X(7)  VALUE 'COUNTRY'.               
008200     03   FILLER                 PIC X(1)  VALUE X'05'.                   
008300     03   HRAD-STARTDATUM        PIC X(10) VALUE 'STARTDATUM'.            
008400     03   FILLER                 PIC X(1)  VALUE X'05'.                   
008500     03   HRAD-STOPPDATUM        PIC X(10) VALUE 'STOPPDATUM'.            
008600     03   FILLER                 PIC X(1)  VALUE X'05'.                   
008700     03   HRAD-FAKTDATUM     PIC X(17) VALUE 'FAKT.DATUM SENAST'.         
008800     03   FILLER                 PIC X(1)  VALUE X'05'.                   
008900     03   HRAD-KOMMENTAR         PIC X(9)  VALUE 'KOMMENTAR'.             
009000                                                                          
009100*                                                                         
009200 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
009300     SKIP2                                                                
009400*01  W43230  -COPY W4323002  -PRE  UTGMT-                                 
009500*                                                                         
009600 01  FILLER                      PIC X(8)    VALUE 'UTW43231'.            
009700     SKIP2                                                                
009800*01  W43231  -COPY W4323001  -PRE  UT2-                                   
009900     EJECT                                                                
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100*                                                                         
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600                                                                          
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL ABEND                                            
010900                                                                          
011000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011200     SKIP2                                                                
011300 01  FELTEXT-1.                                                           
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-1-STR           PIC X(72)   VALUE SPACE.                 
011600 01  FELTEXT-2.                                                           
011700     03  FILLER                  PIC X(8)    VALUE SPACE.                 
011800     03  FELTEXT-2-STR           PIC X(72)   VALUE SPACE.                 
011900 01  FELTEXT-3.                                                           
012000     03  FILLER                  PIC X(8)    VALUE SPACE.                 
012100     03  TRACE-STR               PIC X(72)   VALUE SPACE.                 
012200     SKIP2                                                                
012300                                                                          
012400 01  FILLER                      PIC X(16)   VALUE                        
012500                                             'UT-HEAD-START'.             
012600 01  UT-HEADLINE.                                                         
012700     03  UT-HEADLINE-AREA        PIC X(400)  VALUE SPACE.                 
012800     EJECT                                                                
012900*                                                                         
013000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013100*                                                                         
013200     SKIP2                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013400     SKIP3                                                                
013500                                                                          
013600 01  NYCKLAR-TILL-DLI.                                                    
013700                                                                          
013800     03  W-WDB101KY-X.                                                    
013900         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
014000         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
014100                                                                          
014200                                                                          
014300 01  IMS-WS.                                                              
014400                                                                          
014500     03  STATUS-WS               PIC X(2).                                
014600        88  SEGMENT-FINNS                    VALUE '  '.                  
014700        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
014800        88  END-OF-DATA                      VALUE 'GB'.                  
014900                                                                          
015000     03  GODK-STATUSKODER.                                                
015100         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
015200     SKIP3                                                                
015300 01  SSA1                        PIC X(64).                               
015400                                                                          
015500     EJECT                                                                
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*01  -COPY W0005         -PRE POSTSUM-.                                   
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE                        
016100                                             'WDB201-AREA'.               
016200 01  DLI-IO-AREA.                                                         
016300*    03  WDB201    -COPY WDB201                                           
016400     EJECT                                                                
016500                                                                          
016600 01  FILLER                      PIC X(16)   VALUE                        
016700                                             'WDB101-AREA'.               
016800 01  DLI-IO-AREA-1.                                                       
016900     03  WLGMTB01.                                                        
017000*        05  -COPY WDB101                                                 
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300     SKIP3                                                                
017400*01  -COPY W0008         -PRE WDB2-                                       
017500     05  FILLER           PIC X.                                          
017600     EJECT                                                                
017700*01  -COPY W0008         -PRE WDB1-                                       
017800     05  FILLER           PIC X.                                          
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING WDB2-PCB WDB1-PCB.                             
018100     ENTRY 'DLITCBL' USING WDB2-PCB WDB1-PCB.                             
018200                                                                          
018300     PERFORM A-INIT                                                       
018400     PERFORM IMS-GN-WDB2                                                  
018500                                                                          
018600     IF SEGMENT-FINNS                                                     
018700       PERFORM S01-WRITE-W43231-HEAD-LINE                                 
018800     END-IF                                                               
018900                                                                          
019000     PERFORM UNTIL END-OF-DATA                                            
019100                                                                          
019200       PERFORM C-MOVE-TAB                                                 
019300                                                                          
019400       EVALUATE WDB2-SEG-NAME-FB                                          
019500         WHEN 'WDB201'                                                    
019600           PERFORM B-CONTROL-PAYER                                        
019700       END-EVALUATE                                                       
019800                                                                          
019900       PERFORM IMS-GN-WDB2                                                
020000     END-PERFORM                                                          
020100                                                                          
020200     PERFORM Z-FINIT                                                      
020300     MOVE ZERO TO RETURN-CODE                                             
020400     GOBACK                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 A-INIT SECTION.                                                          
020800                                                                          
020900     OPEN OUTPUT W43230                                                   
021000     OPEN OUTPUT W43231                                                   
021100                                                                          
021200     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
021300                                                                          
021400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
021500     MOVE WS-DAGENS-AAAAMMDD(3:6)     TO DAGENS-DATUM                     
021600     COMPUTE DAGENS-AAR = DAGENS-AAR - 2                                  
021700     END-COMPUTE                                                          
021800                                                                          
021900     DISPLAY 'DAGENS-AAR======' DAGENS-AAR                                
022000     DISPLAY 'DAGENS-DATUM====' DAGENS-DATUM                              
022100                                                                          
022200     MOVE ZERO             TO UT-HEAD-LINE                                
022300     MOVE ZERO             TO UT2-POST                                    
022400     .                                                                    
022500                                                                          
022600 B-CONTROL-PAYER          SECTION.                                        
022700                                                                          
022800     MOVE GMT-IDPARTNR     TO W-WDB1-IDPARTNR                             
022900     MOVE GMT-IDFTG        TO W-WDB1-IDFTG                                
023000     MOVE GMT-TISTODAT     TO WS-GMT-TISTODAT                             
023100     MOVE GMT-TIFAKT       TO WS-GMT-TIFAKT                               
023200                                                                          
023300     PERFORM IMS-GU-WDB101                                                
023400     IF  SEGMENT-FINNS                                                    
023500       MOVE BET-BEBETRAD-1      TO UTGMT-BEBETRAD-1                       
023600       MOVE BET-BEBETRAD-2      TO UTGMT-BEBETRAD-2                       
023700       MOVE BET-ADBETRAD-1      TO UTGMT-ADBETRAD-1                       
023800       MOVE BET-ADBETRAD-2      TO UTGMT-ADBETRAD-2                       
023900     ELSE                                                                 
024000       MOVE 'BETALARE SAKNAS'   TO UTGMT-BEBETRAD-1                       
024100                                   UTGMT-BEBETRAD-2                       
024200       MOVE SPACE               TO UTGMT-ADBETRAD-1                       
024300                                   UTGMT-ADBETRAD-2                       
024400     END-IF                                                               
024500                                                                          
024600     PERFORM S02-WRITE-W43230                                             
024700                                                                          
024800     PERFORM BA-CONTROL-TIME                                              
024900     .                                                                    
025000     EJECT                                                                
025100                                                                          
025200 BA-CONTROL-TIME    SECTION.                                              
025300                                                                          
025400     IF GMT-TISTODAT > ZERO                                               
025500       IF WS-GMT-TISTODAT < DAGENS-DATUM                                  
025600                                                                          
025700         MOVE 'STOP > ÄN 2 ÅR ' TO UT2-COMMENT                            
025800         PERFORM S03-WRITE-W43231                                         
025900       END-IF                                                             
026000     ELSE                                                                 
026100       IF GMT-TISTADAT = ZERO                                             
026200                                                                          
026300         MOVE 'INTE STARTAT   ' TO UT2-COMMENT                            
026400         PERFORM S03-WRITE-W43231                                         
026500       END-IF                                                             
026600       IF WS-GMT-TIFAKT < DAGENS-DATUM                                    
026700                                                                          
026800         MOVE 'EJ FAKT PÅ 2 ÅR' TO UT2-COMMENT                            
026900         PERFORM S03-WRITE-W43231                                         
027000       END-IF                                                             
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
027500 C-MOVE-TAB     SECTION.                                                  
027600                                                                          
027700     MOVE TAB               TO UT2-TAB-1                                  
027800     MOVE TAB               TO UT2-TAB-2                                  
027900     MOVE TAB               TO UT2-TAB-3                                  
028000     MOVE TAB               TO UT2-TAB-4                                  
028100     MOVE TAB               TO UT2-TAB-5                                  
028200     MOVE TAB               TO UT2-TAB-6                                  
028300     MOVE TAB               TO UT2-TAB-7                                  
028400     MOVE TAB               TO UT2-TAB-8                                  
028500                                                                          
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900 S01-WRITE-W43231-HEAD-LINE     SECTION.                                  
029000                                                                          
029100*    WRITE HEAD LINE (RUBRIK-RAD)                                         
029200                                                                          
029300     WRITE UT-HEAD-LINE FROM HEADLINE-AREA                                
029400                                                                          
029500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029600     MOVE 'W43231 '  TO POSTSUM-FDNAMN                                    
029700     MOVE 'W43230D2' TO POSTSUM-DDNAMN2                                   
029800     MOVE 'UT2'      TO POSTSUM-TRANSTYP                                  
029900                                                                          
030000     CALL POSTSUM USING POSTSUM-PARM                                      
030100     .                                                                    
030200     SKIP3                                                                
030300                                                                          
030400 S02-WRITE-W43230    SECTION.                                             
030500                                                                          
030600     MOVE GMT-IDDISTR     TO UTGMT-IDDISTR                                
030700     MOVE GMT-IDKUNDNR    TO UTGMT-IDKUNDNR                               
030800     MOVE GMT-BEGMT       TO UTGMT-BEGMT                                  
030900     MOVE GMT-ADGMT       TO UTGMT-ADGMT                                  
031000     MOVE GMT-KDSPRAK     TO UTGMT-KDSPRAK                                
031100     MOVE GMT-TIFAKT      TO UTGMT-TIFAKT                                 
031200     MOVE GMT-TISTADAT    TO UTGMT-TISTADAT                               
031300     MOVE GMT-TISTODAT    TO UTGMT-TISTODAT                               
031400                                                                          
031500     WRITE UTGMT-POST     FROM UTGMT-W43230                               
031600                                                                          
031700     MOVE 'W43230'        TO POSTSUM-FDNAMN                               
031800     MOVE 'W43230D1'      TO POSTSUM-DDNAMN2                              
031900     MOVE 'UTGMT'         TO POSTSUM-TRANSTYP                             
032000                                                                          
032100     CALL POSTSUM USING POSTSUM-PARM                                      
032200     .                                                                    
032300     EJECT                                                                
032400                                                                          
032500 S03-WRITE-W43231    SECTION.                                             
032600                                                                          
032700     MOVE GMT-IDDISTR     TO UT2-IDDISTR                                  
032800     MOVE GMT-IDKUNDNR    TO UT2-IDKUNDNR                                 
032900     MOVE GMT-BEGMT       TO UT2-BEGMT                                    
033000     MOVE GMT-ADGMT-PADR  TO UT2-ADGMT-PADR                               
033100     MOVE GMT-ADGMT-LAND  TO UT2-ADGMT-LAND                               
033200     MOVE GMT-TISTADAT    TO UT2-TISTADAT                                 
033300     MOVE GMT-TISTODAT    TO UT2-TISTODAT                                 
033400     MOVE GMT-TIFAKT      TO UT2-TIFAKT                                   
033500                                                                          
033600     WRITE UT2-POST       FROM UT2-W43231                                 
033700                                                                          
033800     MOVE 'W43231'        TO POSTSUM-FDNAMN                               
033900     MOVE 'W43230D2'      TO POSTSUM-DDNAMN2                              
034000     MOVE 'UT2'           TO POSTSUM-TRANSTYP                             
034100                                                                          
034200     CALL POSTSUM USING POSTSUM-PARM                                      
034300     .                                                                    
034400     EJECT                                                                
034500                                                                          
034600 Z-FINIT  SECTION.                                                        
034700                                                                          
034800     CLOSE W43230                                                         
034900     CLOSE W43231                                                         
035000                                                                          
035100     MOVE 'S'             TO POSTSUM-OPKOD                                
035200                                                                          
035300     CALL POSTSUM USING POSTSUM-PARM                                      
035400     .                                                                    
035500     EJECT                                                                
035600 IMS-GN-WDB2              SECTION.                                        
035700                                                                          
035800     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
035900     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-AREA                           
036000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
036100     PERFORM IMS-STATUSKONTROLL                                           
036200     .                                                                    
036300     SKIP3                                                                
036400 IMS-GU-WDB101 SECTION.                                                   
036500                                                                          
036600     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
036700          DELIMITED BY SIZE INTO SSA1                                     
036800     MOVE '  GE' TO GODK-STATUSKODER                                      
036900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-1 SSA1                    
037000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
037100     PERFORM IMS-STATUSKONTROLL                                           
037200     .                                                                    
037300 IMS-STATUSKONTROLL       SECTION.                                        
037400                                                                          
037500     SET STATUS-IX TO 1                                                   
037600     SEARCH GODK-STATUS                                                   
037700       AT END CALL FELLOG                                                 
037800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
037900     END-SEARCH                                                           
038000     .                                                                    

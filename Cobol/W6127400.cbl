000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6127400.                                                
000400 AUTHOR.         JOHAN LINDKVIST.                                         
000500 DATE-WRITTEN.   97/06/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR UTLISTA PÅ LEDTIDER MELLAN FAKTURERING OCH                
001100*        INLÄGGNINGEN                                                     
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
002500*          --- SORTERAD INFIL                                             
002600     SELECT W61264                     ASSIGN TO W61274D1.                
002700     SKIP2                                                                
002800*          --- UTLISTA                                                    
002900     SELECT W61274-001                 ASSIGN TO W61274D2.                
003000*          --- UTFLISTA SOM UTFIL TILL D&P                                
003100     SELECT UTFIL61                    ASSIGN TO W61274D3.                
003200*          --- UTFLISTA SOM UTFIL TILL D&P                                
003300     SELECT UTFIL62                    ASSIGN TO W61274D4.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W61264                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W61264      -L.                                                
004400     SKIP3                                                                
004500 FD  W61274-001                                                           
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900 01  W61274-001-RAD              PIC X(121).                              
005000 FD  UTFIL61                                                              
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400 01  UTPOST61                    PIC X(125).                              
005500 FD  UTFIL62                                                              
005600     RECORDING       V                                                    
005700     BLOCK CONTAINS  0.                                                   
005800     SKIP2                                                                
005900 01  UTPOST62                    PIC X(125).                              
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300*    -- CHECKED BY WY2000                                                 
006400 01  I                           PIC 9  COMP-3.                           
006500                                                                          
006600 01 TALLYS.                                                               
006700    03 SPAR-IDDC                PIC XX      VALUE SPACE.                  
006800    03 SAVES.                                                             
006900       05 BOAT-LINES             PIC S9(5)       COMP-3.                  
007000       05 AIR-LINES              PIC S9(5)       COMP-3.                  
007100       05 AIR2-LINES             PIC S9(5)       COMP-3.                  
007200       05 BOAT-TIME              PIC S9(8)V9     COMP-3.                  
007300       05 AIR-TIME               PIC S9(8)V9     COMP-3.                  
007400       05 AIR2-TIME              PIC S9(8)V9     COMP-3.                  
007500                                                                          
007600 01 TEMP-TIME                    PIC S9(8)V9  COMP-3  VALUE ZERO.         
007700                                                                          
007800 01  INLEVNR-BERAK-X             PIC 9(16).                               
007900 01  INLEVNR-BERAK REDEFINES INLEVNR-BERAK-X.                             
008000     03 INLEV-SEKEL              PIC 9(2).                                
008100     03 INLEV-DATUM              PIC 9(6).                                
008200     03 INLEV-KLOCKA             PIC 9(4).                                
008300     03 FILLER                   PIC 9(4).                                
008400                                                                          
008500 01 BOAT-GOAL                    PIC S9(3)       COMP-3.                  
008600 01 AIR-GOAL                     PIC S9(3)       COMP-3.                  
008700 01 AIR2-GOAL                    PIC S9(3)       COMP-3.                  
008800                                                                          
008900 01  TEMP-HELTAL                 PIC Z(9)9.                               
009000 01  TEMP-DECTAL                 PIC Z(4)9.9.                             
009100                                                                          
009200 77  IDPGM                       PIC X(8)    VALUE 'W6127400'.            
009300 77  JA                          PIC X       VALUE 'J'.                   
009400 77  NEJ                         PIC X       VALUE 'N'.                   
009500                                                                          
009600 77  W61264-EOF-SW               PIC X       VALUE 'N'.                   
009700     88  END-OF-W61264                       VALUE 'J'.                   
009800     EJECT                                                                
009900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010000 01  FILLER REDEFINES DAGENS-DATUM.                                       
010100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010400     EJECT                                                                
010500*      --- VALID IDDC CODES                                               
010600*                                                                         
010700*01    -COPY WWDC99                                                       
010800       EJECT                                                              
010900 01  DYNAMISKA-SUBPROGRAM.                                                
011000*                                                                         
011100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011500     03  W612TIME                PIC X(8)    VALUE 'W612TIME'.            
011600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011800     SKIP2                                                                
011900*    --- PARAMETRAR TILL ABEND                                            
012000                                                                          
012100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012400     SKIP2                                                                
012500 01  FELTEXT.                                                             
012600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL DATKORT                                          
013000*                                                                         
013100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61274'.              
013200     SKIP2                                                                
013300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013400     SKIP2                                                                
013500*    --- PARAMETRAR TILL DATKORT                                          
013600*01  -COPY WDATKORT                                                       
013700     EJECT                                                                
013800*    --- PARAMETRAR TILL WDATKONV                                         
013900*01  -COPY WDATAREA                                                       
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL POSTSUM                                          
014200*                                                                         
014300*01  -COPY W0005   -PRE  POSTSUM-                                         
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL W612TIME                                         
014600*                                                                         
014700*01  -COPY W612TID     -PRE TIME-                                         
014800     EJECT                                                                
014900 01  NYCKLAR-TILL-DLI.                                                    
015000     03  W-IDDC-B6-X.                                                     
015100         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
015200     EJECT                                                                
015300 01  IN-AREA-START               PIC X(24)   VALUE                        
015400                                 'IN-AREA-START  '.                       
015500     SKIP2                                                                
015600                                                                          
015700*01  AREA -COPY W61264     -PRE IN-                                       
015800     EJECT                                                                
015900 01  W001-AREA-START             PIC X(24)   VALUE                        
016000                                 'W001-AREA-START  '.                     
016100     SKIP2                                                                
016200 01  W001-HJALPAREOR.                                                     
016300*                                                                         
016400     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
016500     03  W001-ANTAL-RADER                                                 
016600                                 PIC 9(3)    VALUE 999.                   
016700     03  W001-MAX-RADER-PER-SIDA                                          
016800                                 PIC 9(3)    VALUE 42.                    
016900     03  W001-MAX-POSITIONER-PER-RAD                                      
017000                                 PIC 9(3)    VALUE 120.                   
017100     03  W001-LISTNR             PIC X(11)   VALUE 'W61274-001'.          
017200     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
017300     EJECT                                                                
017400 01  W001-RAD.                                                            
017500*                                                                         
017600     03  FILLER                  PIC X(121)  VALUE SPACE.                 
017700     EJECT                                                                
017800 01  W001-RUBRIK1.                                                        
017900*                                                                         
018000     03  FILLER                  PIC X(3) VALUE SPACE.                    
018100     03  FILLER                  PIC X(21)                                
018200                                VALUE 'VOLVO CAR PARTS      '.            
018300     03  FILLER                  PIC X(12)                                
018400                                 VALUE 'W61274-001'.                      
018500     03  FILLER                  PIC X(35)                                
018600              VALUE '       LEADTIME ACCORDING TO GOAL'.                  
018700     03  FILLER                  PIC X(5) VALUE SPACE.                    
018800     03  FILLER                  PIC X(4) VALUE 'DC '.                    
018900     03  RUBRIK-DC-1             PIC XX.                                  
019000     03  FILLER                  PIC X(4) VALUE SPACE.                    
019100     03  FILLER                  PIC X(6) VALUE 'WEEK: '.                 
019200     03  RUBRIK-VECKA            PIC XX.                                  
019300     03  FILLER                  PIC X(4) VALUE SPACE.                    
019400     03  W001-DATUM              PIC XXBXXBXX.                            
019500     03  FILLER                  PIC X(4) VALUE SPACE.                    
019600     03  FILLER                  PIC X(4)                                 
019700                                 VALUE 'PAGE'.                            
019800     03  W001-SID                PIC Z(4)9.                               
019900     EJECT                                                                
020000 01  W001-RUBRIK2.                                                        
020100*                                                                         
020200     03  FILLER                  PIC X(20) VALUE SPACE.                   
020300     03  FILLER                  PIC X(18)                                
020400                                 VALUE 'GOALTIME IN SYSTEM'.              
020500     03  FILLER                  PIC X(15)  VALUE SPACE.                  
020600     03  FILLER                  PIC X(24)                                
020700                                 VALUE 'AVERAGE TIME DURING WEEK'.        
020800     EJECT                                                                
020900 01  W001-DETALJ1.                                                        
021000     03 FILLER                   PIC XXXX    VALUE 'NDC '.                
021100     03 RUBRIK-DC-2              PIC XX      VALUE SPACE.                 
021200     03 FILLER                   PIC XXX     VALUE SPACE.                 
021300     03 RUBRIK-RAD1              PIC X(4)    VALUE 'BOAT'.                
021400     03 FILLER                   PIC X(8)    VALUE SPACE.                 
021500     03 GOAL-RAD1                PIC X(10).                               
021600     03 FILLER                   PIC X(29)   VALUE SPACE.                 
021700     03 TIME-RAD1                PIC X(10).                               
021800     EJECT                                                                
021900 01  W001-DETALJ2.                                                        
022000     03 FILLER                   PIC X(9)    VALUE SPACE.                 
022100     03 RUBRIK-RAD2              PIC X(9)    VALUE 'AIR (17)'.            
022200     03 FILLER                   PIC X(3)    VALUE SPACE.                 
022300     03 GOAL-RAD2                PIC X(10).                               
022400     03 FILLER                   PIC X(29)   VALUE SPACE.                 
022500     03 TIME-RAD2                PIC X(10).                               
022600     EJECT                                                                
022700 01  W001-DETALJ3.                                                        
022800     03 FILLER                   PIC X(9)    VALUE SPACE.                 
022900     03 RUBRIK-RAD3              PIC X(9)    VALUE 'AIR (19)'.            
023000     03 FILLER                   PIC X(3)    VALUE SPACE.                 
023100     03 GOAL-RAD3                PIC X(10).                               
023200     03 FILLER                   PIC X(29)   VALUE SPACE.                 
023300     03 TIME-RAD3                PIC X(10).                               
023400     EJECT                                                                
023500 01  UT-AREA-START               PIC X(24)   VALUE                        
023600                                 'IN-AREA-START  '.                       
023700     SKIP2                                                                
023800 01  FILLER                      PIC X(24)  VALUE 'UTPOST'.               
023900 01  UT-LINE                     PIC X(125) VALUE SPACE.                  
024000     EJECT                                                                
024100*    --- IMS FUNKTIONSKODER                                               
024200*01  -COPY W0003                                                          
024300     EJECT                                                                
024400*    --- STATUS-KOD FRÅN IMS                                              
024500 01  STATUS-WS                   PIC XX.                                  
024600     88  SEGMENT-FINNS                       VALUE '  '.                  
024700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024900                                                                          
025000 01  GODK-STATUSKODER.                                                    
025100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025200     SKIP3                                                                
025300 01  SSA1                        PIC X(160).                              
025400 01  SSA2                        PIC X(128).                              
025500     EJECT                                                                
025600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
025700 01   DLI-IO-AREA-B601.                                                   
025800*     03  -COPY WDB601                                                    
025900 LINKAGE SECTION.                                                         
026000                                                                          
026100*01  -COPY W0008  -PRE WDB6-                                              
026200     05  FILLER                  PIC X.                                   
026300                                                                          
026400 PROCEDURE DIVISION  USING WDB6-PCB.                                      
026500 MAIN SECTION.                                                            
026600     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
026700     PERFORM A-INIT                                                       
026800     PERFORM S01-LAES-W61264                                              
026900     PERFORM UNTIL END-OF-W61264                                          
027000       PERFORM B-KONTROLL-NYTT-DC                                         
027100       PERFORM C-BERAKNA-IDINLEV                                          
027200       PERFORM D-TIDSBERAK                                                
027300       PERFORM E-ASSIGN-TALLYS                                            
027400       PERFORM S01-LAES-W61264                                            
027500     END-PERFORM                                                          
027600                                                                          
027700     IF NOT END-OF-W61264                                                 
027800       PERFORM S11-LIST-UTSKRIFT                                          
027810     END-IF                                                               
027900     PERFORM Z-FINIT                                                      
028000                                                                          
028100     MOVE ZERO TO RETURN-CODE                                             
028200     GOBACK                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 A-INIT SECTION.                                                          
028600                                                                          
028700     OPEN INPUT  W61264                                                   
028800     OPEN OUTPUT W61274-001                                               
028900     OPEN OUTPUT UTFIL61                                                  
029000     OPEN OUTPUT UTFIL62                                                  
029100                                                                          
029200     INITIALIZE TALLYS                                                    
029300     SKIP2                                                                
029400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
029500     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
029600     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
029700     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
029800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029900                                                                          
030000     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
030100     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
030200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
030300                         DAT-O-TIDATUM DAT-KDSVAR                         
030400                                                                          
030500     IF DAT-KDSVAR-OK                                                     
030600       MOVE DAT-TIVV TO RUBRIK-VECKA                                      
030700     ELSE                                                                 
030800       MOVE 'FEL I WDATKONV' TO FELTEXT-STR                               
030900       DISPLAY FELTEXT                                                    
031000       PERFORM S99-ABEND                                                  
031100     END-IF                                                               
031200                                                                          
031300     .                                                                    
031400     EJECT                                                                
031500 B-KONTROLL-NYTT-DC SECTION.                                              
031600     IF SPAR-IDDC = SPACE                                                 
031700       MOVE IN-IDDC TO SPAR-IDDC                                          
031800       MOVE SPAR-IDDC TO W-IDDC-B6                                        
031900       PERFORM IMS-GU-WDB601                                              
032000     ELSE                                                                 
032100       MOVE SPAR-IDDC TO W-IDDC-B6                                        
032200       PERFORM IMS-GU-WDB601                                              
032300       IF IN-IDDC = SPAR-IDDC                                             
032400         CONTINUE                                                         
032500       ELSE                                                               
032600         PERFORM S11-LIST-UTSKRIFT                                        
032700         MOVE IN-IDDC TO SPAR-IDDC                                        
032800       END-IF                                                             
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 C-BERAKNA-IDINLEV SECTION.                                               
033300                                                                          
033400     SUBTRACT IN-DAINLEV FROM 9999999999999999                            
033500                       GIVING INLEVNR-BERAK-X                             
033600     .                                                                    
033700     EJECT                                                                
033800 D-TIDSBERAK SECTION.                                                     
033900                                                                          
034000     MOVE IN-IDDC            TO  TIME-IDDC                                
034100     MOVE INLEV-DATUM        TO  TIME-TIINLMOT                            
034200     MOVE INLEV-KLOCKA       TO  TIME-TIINLMTI                            
034300     MOVE IN-TIINLINL        TO  TIME-TIINLINL                            
034400     MOVE IN-TIINLITI        TO  TIME-TIINLITI                            
034500                                                                          
034600     CALL W612TIME USING TIME-W612TID WDB6-PCB                            
034700                                                                          
034800     IF TIME-KDSVAR = JA                                                  
034900       MOVE TIME-KVDAGDEC TO TEMP-TIME                                    
035000     ELSE                                                                 
035100       MOVE 'FELAKTIG RETURKOD FRÅN W612TIME' TO FELTEXT-STR              
035200       DISPLAY FELTEXT                                                    
035300       PERFORM S99-ABEND                                                  
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 E-ASSIGN-TALLYS SECTION.                                                 
035800                                                                          
035900     IF IN-KDFRAKT = 17                                                   
036000       ADD 1          TO AIR-LINES                                        
036100       ADD TEMP-TIME  TO AIR-TIME                                         
036200     ELSE                                                                 
036300        IF IN-KDFRAKT = 19                                                
036400          ADD 1         TO  AIR2-LINES                                    
036500          ADD TEMP-TIME TO  AIR2-TIME                                     
036600        ELSE                                                              
036700          ADD 1         TO  BOAT-LINES                                    
036800          ADD TEMP-TIME TO  BOAT-TIME                                     
036900        END-IF                                                            
037000     END-IF                                                               
037100     .                                                                    
037200     EJECT                                                                
037300 Z-FINIT SECTION.                                                         
037400                                                                          
037500     CLOSE W61264                                                         
037600     CLOSE W61274-001                                                     
037700     CLOSE UTFIL61                                                        
037800     CLOSE UTFIL62                                                        
037900     SKIP2                                                                
038000     MOVE 'S' TO POSTSUM-OPKOD                                            
038100     CALL POSTSUM USING POSTSUM-PARM                                      
038200     .                                                                    
038300     EJECT                                                                
038400 S01-LAES-W61264  SECTION.                                                
038500     READ W61264 INTO IN-AREA                                             
038600     AT END                                                               
038700        MOVE HIGH-VALUE TO IN-AREA                                        
038800        SET END-OF-W61264 TO TRUE                                         
038900                                                                          
039000     NOT AT END                                                           
039100        MOVE 'W61264' TO POSTSUM-FDNAMN                                   
039200        MOVE 'W61274D1' TO POSTSUM-DDNAMN2                                
039300        MOVE IN-IDDC TO POSTSUM-TRANSTYP                                  
039400        CALL POSTSUM USING POSTSUM-PARM                                   
039500     END-READ                                                             
039600     .                                                                    
039700     EJECT                                                                
039800 S11-LIST-UTSKRIFT SECTION.                                               
039900     PERFORM S12-SKRIV-RUBRIKER                                           
040000     PERFORM S13-SKAPA-RADER                                              
040100     PERFORM S14-SKRIV-W61274-001                                         
040200     INITIALIZE TALLYS                                                    
040300     .                                                                    
040400     EJECT                                                                
040500 S12-SKRIV-RUBRIKER SECTION.                                              
040600                                                                          
040700     MOVE 1 TO W001-SIDRAKNARE                                            
040800     MOVE 3 TO W001-SKIP                                                  
040900     MOVE +9 TO W001-ANTAL-RADER                                          
041000     MOVE DAGENS-DATUM    TO W001-DATUM                                   
041100     MOVE W001-SIDRAKNARE TO W001-SID                                     
041200     MOVE SPAR-IDDC TO RUBRIK-DC-1                                        
041300                       RUBRIK-DC-2                                        
041400     WRITE W61274-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
041500     MOVE W001-RUBRIK1 TO UT-LINE                                         
041600     PERFORM S20-SKRIV-UTFIL                                              
041700     MOVE 1 TO W001-SKIP                                                  
041800     WRITE W61274-001-RAD FROM W001-RUBRIK2 AFTER W001-SKIP               
041900     MOVE W001-RUBRIK2 TO UT-LINE                                         
042000     PERFORM S20-SKRIV-UTFIL                                              
042100     MOVE 2 TO W001-SKIP                                                  
042200     MOVE SPACE TO W001-RAD                                               
042300     MOVE SPACE TO UT-LINE                                                
042400     .                                                                    
042500     EJECT                                                                
042600 S13-SKAPA-RADER SECTION.                                                 
042700                                                                          
042800******************* RÄKNAR OM TOTAL TID TILL TID PER RAD ***              
042900     IF NOT BOAT-LINES = 0                                                
043000       COMPUTE BOAT-TIME ROUNDED =                                        
043100               BOAT-TIME / BOAT-LINES                                     
043200     END-IF                                                               
043300                                                                          
043400     IF NOT AIR-LINES = 0                                                 
043500       COMPUTE AIR-TIME ROUNDED =                                         
043600               AIR-TIME / AIR-LINES                                       
043700     END-IF                                                               
043800                                                                          
043900     IF NOT AIR2-LINES = 0                                                
044000       COMPUTE AIR2-TIME ROUNDED =                                        
044100               AIR2-TIME / AIR2-LINES                                     
044200     END-IF                                                               
044300******************* HÄMTAR HÅRDPROGRAMMERADE LEDTIDSMÅL  ***              
044400     PERFORM S15-HAEMTA-LEDTIDER                                          
044500******************* TILLDELAR VÄRDEN TILL UTLISTA-FÄLTEN ***              
044600     MOVE BOAT-GOAL    TO  TEMP-HELTAL                                    
044700     MOVE TEMP-HELTAL  TO  GOAL-RAD1                                      
044800                                                                          
044900     MOVE BOAT-TIME    TO  TEMP-DECTAL                                    
045000     MOVE TEMP-DECTAL  TO  TIME-RAD1                                      
045100                                                                          
045200     MOVE AIR-GOAL     TO  TEMP-HELTAL                                    
045300     MOVE TEMP-HELTAL  TO  GOAL-RAD2                                      
045400                                                                          
045500     MOVE AIR-TIME     TO  TEMP-DECTAL                                    
045600     MOVE TEMP-DECTAL  TO  TIME-RAD2                                      
045700                                                                          
045800     MOVE AIR2-GOAL     TO  TEMP-HELTAL                                   
045900     MOVE TEMP-HELTAL  TO  GOAL-RAD3                                      
046000                                                                          
046100     MOVE AIR2-TIME     TO  TEMP-DECTAL                                   
046200     MOVE TEMP-DECTAL  TO  TIME-RAD3                                      
046300     .                                                                    
046400     EJECT                                                                
046500 S14-SKRIV-W61274-001  SECTION.                                           
046600                                                                          
046700     MOVE W001-DETALJ1 TO  W001-RAD                                       
046800     WRITE W61274-001-RAD FROM W001-RAD AFTER W001-SKIP                   
046900     MOVE W001-RAD TO UT-LINE                                             
047000     PERFORM S20-SKRIV-UTFIL                                              
047100     MOVE SPACE TO W001-RAD                                               
047200     IF NDC-AU                                                            
047300         MOVE W001-DETALJ3 TO  W001-RAD                                   
047400     ELSE                                                                 
047500         MOVE W001-DETALJ2 TO  W001-RAD                                   
047600     END-IF                                                               
047700     WRITE W61274-001-RAD FROM W001-RAD AFTER W001-SKIP                   
047800     MOVE W001-RAD TO UT-LINE                                             
047900     PERFORM S20-SKRIV-UTFIL                                              
048000     MOVE SPACE TO W001-RAD                                               
048100     ADD  +6 TO W001-ANTAL-RADER                                          
048200     .                                                                    
048300     EJECT                                                                
048400 S15-HAEMTA-LEDTIDER SECTION.                                             
048500                                                                          
048600     MOVE SPAR-IDDC     TO WS-IDDC                                        
048700     EVALUATE TRUE                                                        
048800     WHEN NDC-US-RU                                                       
048900       MOVE 17 TO BOAT-GOAL                                               
049000       MOVE 3  TO AIR-GOAL                                                
049100       MOVE 0  TO AIR2-GOAL                                               
049600     WHEN NDC-US-LA                                                       
049700       MOVE 22 TO BOAT-GOAL                                               
049800       MOVE 4  TO AIR-GOAL                                                
049900       MOVE 0  TO AIR2-GOAL                                               
049910     WHEN NDC-US-SE                                                       
049920       MOVE 22 TO BOAT-GOAL                                               
049930       MOVE 4  TO AIR-GOAL                                                
049940       MOVE 0  TO AIR2-GOAL                                               
049950     WHEN NDC-US-CH                                                       
049960       MOVE 22 TO BOAT-GOAL                                               
049970       MOVE 4  TO AIR-GOAL                                                
049980       MOVE 0  TO AIR2-GOAL                                               
049990     WHEN NDC-US-JA                                                       
049991       MOVE 22 TO BOAT-GOAL                                               
049992       MOVE 4  TO AIR-GOAL                                                
049993       MOVE 0  TO AIR2-GOAL                                               
049990     WHEN NDC-US-DA                                                       
049991       MOVE 22 TO BOAT-GOAL                                               
049992       MOVE 4  TO AIR-GOAL                                                
049993       MOVE 0  TO AIR2-GOAL                                               
050000     WHEN NDC-CA                                                          
050100       MOVE 18 TO BOAT-GOAL                                               
050200       MOVE 4  TO AIR-GOAL                                                
050300       MOVE 0  TO AIR2-GOAL                                               
050400     WHEN NDC-JP                                                          
050500       MOVE 51 TO BOAT-GOAL                                               
050600       MOVE 7  TO AIR-GOAL                                                
050700       MOVE 0  TO AIR2-GOAL                                               
050800     WHEN NDC-AU                                                          
050900       MOVE 63 TO BOAT-GOAL                                               
051000       MOVE 0  TO AIR-GOAL                                                
051100       MOVE 10 TO AIR2-GOAL                                               
051200     END-EVALUATE                                                         
051300     .                                                                    
051400     EJECT                                                                
051500 S20-SKRIV-UTFIL  SECTION.                                                
051600     IF DCS-NDC-PF OR DCS-NDC-OTHERS                                      
051700       IF (DCS-NDC-PF OR DCS-NDC-OTHERS) AND DCS-IDLANDX2 = 'JP'          
051800         WRITE UTPOST61 FROM UT-LINE                                      
051900                                                                          
052000         MOVE 'UT-'      TO POSTSUM-TRANSTYP                              
052100         MOVE 'W61274'   TO POSTSUM-FDNAMN                                
052200         MOVE 'W61274D3' TO POSTSUM-DDNAMN2                               
052300         CALL POSTSUM USING POSTSUM-PARM                                  
052400         MOVE SPACE      TO UT-LINE                                       
052500       END-IF                                                             
052600       IF (DCS-NDC-PF OR DCS-NDC-OTHERS) AND DCS-IDLANDX2 = 'AU'          
052700         WRITE UTPOST62  FROM UT-LINE                                     
052800                                                                          
052900         MOVE 'UT-'      TO POSTSUM-TRANSTYP                              
053000         MOVE 'W61274'   TO POSTSUM-FDNAMN                                
053100         MOVE 'W61274D4' TO POSTSUM-DDNAMN2                               
053200         CALL POSTSUM USING POSTSUM-PARM                                  
053300         MOVE SPACE      TO UT-LINE                                       
053400       END-IF                                                             
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-GU-WDB601    SECTION.                                                
053900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
054000          DELIMITED BY SIZE INTO SSA1                                     
054100     MOVE '  ' TO GODK-STATUSKODER                                        
054200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
054300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     .                                                                    
054600     SKIP3                                                                
054700                                                                          
054800 S99-ABEND SECTION.                                                       
054900                                                                          
055000     SKIP2                                                                
055100     MOVE 'S' TO POSTSUM-OPKOD                                            
055200     CALL POSTSUM USING POSTSUM-PARM                                      
055300     CALL ABEND USING RKOD-ABEND                                          
055400     .                                                                    
055500 IMS-STATUSKONTROLL SECTION.                                              
055600                                                                          
055700     SET STATUS-IX TO 1                                                   
055800     SEARCH GODK-STATUS                                                   
055900       AT END                                                             
056000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
056100           DELIMITED BY SIZE INTO FELTEXT                                 
056200         DISPLAY FELTEXT                                                  
056300         CALL FELLOG                                                      
056400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056500         CONTINUE                                                         
056600     END-SEARCH                                                           
056700     .                                                                    

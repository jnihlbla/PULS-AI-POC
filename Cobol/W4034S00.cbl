000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034S00.                                                
000300 AUTHOR.         LENA BROMANDER.                                          
000400 DATE-WRITTEN.   17/06/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDSMPP. STARTAS AV 4345.                 
001000*        SKAPAR FÖLJESEDEL FÖR SAMLINGSKOLLI SOM ENDAST                   
001100*        VISAR INGÅENDE KOLLIN.                                           
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T34SX                                             
001500*                                                                         
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200 77  CURRENT-SECTION             PIC X(24)   VALUE SPACE.                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4034S00'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80)        VALUE SPACE.            
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
003300 77  RADINDX                     PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  RADINDX-MAX                 PIC S9(9)   VALUE +50  COMP SYNC.        
003500 77  SIDNR                       PIC 9(3)    VALUE ZERO.                  
003600                                                                          
003700 77  ALLT-SW                     PIC X.                                   
003800     88  ALLT-OK                             VALUE 'J'.                   
003900     88  ALLT-FEL                            VALUE 'N'.                   
004000                                                                          
004100     EJECT                                                                
004200 01  LISTVAL                     PIC X(8)    VALUE SPACE.                 
004300 01  WS-PRT-DUMMY                PIC X       VALUE SPACE.                 
004400                                                                          
004500     EJECT                                                                
004600*****************************************************************         
004700*                                                                         
004800                                                                          
004900 01  FELKODER.                                                            
005000     03  FEL-IDKOLLI-SAMP        PIC 9(5)    VALUE ZERO.                  
005100     03  FEL-IDDISTR             PIC 9(4)    VALUE ZERO.                  
005200     03  FEL-KDPRTVAL            PIC X(2)    VALUE SPACE.                 
005300     03  FEL-IDTRANS             PIC X(4)    VALUE SPACE.                 
005400     03  FEL-IDDC                PIC X(2)    VALUE SPACE.                 
005500                                                                          
005600 01  RETURKODER.                                                          
005700     03  RKOD-ABEND-MED-DUMP     PIC S9(4) VALUE +1000 COMP SYNC.         
005800     EJECT                                                                
005900     EJECT                                                                
006000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006100 01  GENERELLA-SUBPROGRAM.                                                
006200     03 W006PRS1                 PIC X(8)    VALUE 'W006PRS1'.            
006300     03 W006PRT                  PIC X(8)    VALUE 'W006PRT '.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006700*                                                                         
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
007000*   -COPY W006PRT                                                         
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE  'WORKAREA'.           
007300*01  -COPY WORKAREA                                                       
007400     EJECT                                                                
007500*01  -COPY W006PRAR                                                       
007600     EJECT                                                                
007700*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007900                                                                          
008000*01  MID -COPY W4I34S01                                                   
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
008300                                                                          
008400*01  -COPY WMSGAREA                                                       
008500     EJECT                                                                
008600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900                                                                          
009000 01  NYCKLAR-TILL-DLI.                                                    
009100                                                                          
009200     03  W-WDE7ASEQ-X.                                                    
009300         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
009400         05 W-IDKOLLI-SAMP       PIC S9(5)   VALUE ZERO  COMP-3.          
009500                                                                          
009600     03  W-WDE721KY-MIN-X.                                                
009700         05 W-IDDISTR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
009800         05 W-IDKUNDNR-MIN       PIC S9(7)   VALUE ZERO  COMP-3.          
009900         05 W-IDORDNR7-MIN       PIC 9(7)    VALUE ZERO.                  
010000         05 W-IDKOLLI-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
010100                                                                          
010200     03  W-WDE721KY-MAX-X.                                                
010300         05 W-IDDISTR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
010400         05 W-IDKUNDNR-MAX       PIC S9(7)   VALUE ZERO  COMP-3.          
010500         05 W-IDORDNR7-MAX       PIC 9(7)    VALUE ZERO.                  
010600         05 W-IDKOLLI-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
010700                                                                          
010800                                                                          
010900     SKIP3                                                                
011000*****************D E L I V E R Y    N O T E ****************              
011100 01  FILLER                    PIC X(16) VALUE 'PRINT-LIST-AREA'.         
011200 01  LIST-RADER.                                                          
011300                                                                          
011400     03  RUBRIKRAD-1.                                                     
011500         05  RUB1-LIST-NAMN      PIC X(18)   VALUE                        
011600             'MIXED CASE NUMBER:' .                                       
011700         05  FILLER              PIC X       VALUE SPACE.                 
011800         05  RUB1-IDKOLLI-SAMP   PIC 9(5).                                
011900         05  FILLER              PIC X(20)   VALUE SPACE.                 
011901         05  FILLER              PIC X(4)    VALUE 'DC: '.                
011902         05  RUB1-IDDC           PIC X(2).                                
011910         05  FILLER              PIC X(24)   VALUE SPACE.                 
012000         05  RUB1-SIDNR          PIC Z9.                                  
012100         05  FILLER              PIC X(4)    VALUE SPACE.                 
012200                                                                          
012300     03  RUBRIKRAD-2.                                                     
012400         05  RUB2-LIST-NAMN      PIC X(26)   VALUE                        
012500             'DISTRICT:    CUSTOMER:    '.                                
012600         05  RUB2-LIST-NAMN      PIC X(29)   VALUE                        
012700             'ORDER NUMBER:    CASE NUMBER:'.                             
012800         05  FILLER              PIC X(25)   VALUE SPACE.                 
012900                                                                          
013000     03  DETALJRAD-1.                                                     
013100         05  RAD1-IDDISTR        PIC Z(4)9.                               
013200         05  FILLER              PIC X(8)    VALUE SPACE.                 
013300         05  RAD1-IDKUNDNR       PIC Z(6)9.                               
013400         05  FILLER              PIC X(6)    VALUE SPACE.                 
013500         05  RAD1-IDORDNR7       PIC Z(6)9.                               
013600         05  FILLER              PIC X(10)   VALUE SPACE.                 
013700         05  RAD1-IDKOLLI        PIC Z(4)9.                               
013800         05  FILLER              PIC X(22)   VALUE SPACE.                 
013900                                                                          
014000     03  TOTALRAD-1.                                                      
014100         05  FILLER              PIC X(13)   VALUE                        
014200                                 'TOTAL CASES: '.                         
014300         05  TOT1-KVKOLLI-SAMP   PIC Z(4)9.                               
014400         05  FILLER              PIC X(62)   VALUE SPACE.                 
014500                                                                          
014510 01  UT-RAD                      PIC X(80)   VALUE SPACE.                 
014520                                                                          
014600     EJECT                                                                
014700*    --- STATUS-KOD FRÅN IMS                                              
014800 01  STATUS-WS                   PIC XX.                                  
014900     88  SEGMENT-FINNS                       VALUE '  '.                  
015000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015200     SKIP2                                                                
015300 01  GODK-STATUSKODER.                                                    
015400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015600 01  SSA1                        PIC X(128).                              
015700 01  SSA2                        PIC X(128).                              
015800     EJECT                                                                
015900                                                                          
016000*    --- IMS FUNKTIONSKODER                                               
016100*01  -COPY W0003                                                          
016200     EJECT                                                                
016300*    ---  DLI INPUT-OUTPUT AREA                                           
016400 01  DLI-IO-WDE711.                                                       
016500*    03  -COPY WDE711                                                     
016600     EJECT                                                                
016700 01  DLI-IO-WDE721.                                                       
016800*    03  -COPY WDE721                                                     
016900     EJECT                                                                
017000 LINKAGE SECTION.                                                         
017100*01  -COPY W0009   -PRE MSG-                                              
017200                                                                          
017300*01  -COPY W0009   -PRE ALT-                                              
017400     EJECT                                                                
017500*01  -COPY W0008   -PRE WDE7A-                                            
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
017900                           WDE7A-PCB.                                     
018000 MAIN SECTION.                                                            
018100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
018200                           WDE7A-PCB.                                     
018300                                                                          
018400     PERFORM IMS-GET-MSG                                                  
018500     IF SEGMENT-FINNS                                                     
018600       IF MSG-KDTRTYP = 'X'                                               
018700         PERFORM A-INIT                                                   
018800                                                                          
018900         PERFORM IMS-GU-WDE711-ASEQ                                       
019000                                                                          
019100         PERFORM B-SKRIV-FOLJESEDEL-SAMP                                  
019200                                                                          
019300       END-IF                                                             
019400     END-IF                                                               
019500                                                                          
019600     MOVE ZERO TO RETURN-CODE                                             
019700     GOBACK                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 A-INIT SECTION.                                                          
020100     MOVE 'A-INIT         '     TO CURRENT-SECTION                        
020200                                                                          
020300     MOVE JA  TO ALLT-SW                                                  
020400     MOVE +1                              TO RADINDX                      
020500     MOVE +1                              TO SIDNR                        
020600                                                                          
020700     MOVE MSG-INDATA-MINUS-1-TRANSKOD     TO MID-W4I34S01                 
020800                                                                          
020900     IF MID-KDPRTVAL = SPACE                                              
021000       MOVE MID-KDPRTVAL                  TO FEL-KDPRTVAL                 
021100       MOVE NEJ TO ALLT-SW                                                
021200     END-IF                                                               
021300                                                                          
021310     IF MID-IDDC = SPACE                                                  
021320       MOVE MID-IDDC                      TO FEL-KDPRTVAL                 
021330       MOVE NEJ TO ALLT-SW                                                
021331     ELSE                                                                 
021332       MOVE MID-IDDC                      TO W-IDDC                       
021340     END-IF                                                               
021350                                                                          
021400     IF MID-IDKOLLI-SAMP NUMERIC                                          
021500       MOVE MID-IDKOLLI-SAMP              TO W-IDKOLLI-SAMP               
021600     ELSE                                                                 
021700       MOVE MID-IDKOLLI-SAMP              TO FEL-IDKOLLI-SAMP             
021800       MOVE NEJ TO ALLT-SW                                                
021900     END-IF                                                               
022000                                                                          
022100     MOVE LOW-VALUE      TO W-WDE721KY-MIN-X                              
022200     MOVE HIGH-VALUE     TO W-WDE721KY-MAX-X                              
022300                                                                          
022400     IF MSG-IDTRANS-1 NOT = '4345'                                        
022500        MOVE MSG-IDTRANS-1                TO FEL-IDTRANS                  
022600        MOVE NEJ TO ALLT-SW                                               
022700     END-IF                                                               
022800                                                                          
022900     IF NOT ALLT-OK                                                       
023000        MOVE '*** FEL PÅ INDATAT KOLLA PÅ FEL-  **'                       
023100                               TO FELTEXT                                 
023200        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
023300     END-IF                                                               
023400                                                                          
023500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
023600     .                                                                    
023700     EJECT                                                                
023800 B-SKRIV-FOLJESEDEL-SAMP SECTION.                                         
023900     MOVE 'B-SKRIV-FOLJESEDEL-SAMP'     TO CURRENT-SECTION                
023901                                                                          
023910     MOVE 'W4034S-001'                 TO PRT-IDLIST                      
024100     MOVE '4FS11'                      TO LISTVAL (1:5)                   
024300     MOVE MID-KDPRTVAL                 TO LISTVAL (6:2)                   
024500                                                                          
024600     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-OPEN  LISTVAL                  
024700                         ALT-PCB WS-PRT-DUMMY WS-PRT-DUMMY                
024800                                                                          
024900     PERFORM BA-PRINT-RUBRIKER                                            
025000                                                                          
025100     PERFORM IMS-GNP-WDE721-ASEQ-KVAL                                     
025200                                                                          
025300     PERFORM UNTIL SEGMENT-SAKNAS                                         
025400                                                                          
025500       IF RADINDX > RADINDX-MAX                                           
025600         PERFORM BA-PRINT-RUBRIKER                                        
025700       END-IF                                                             
025800       PERFORM BB-PRINT-RAD                                               
025900                                                                          
026000       PERFORM IMS-GNP-WDE721-ASEQ-KVAL                                   
026100                                                                          
026200     END-PERFORM                                                          
026300                                                                          
026400                                                                          
026500     PERFORM BC-PRINT-TOTAL                                               
026600                                                                          
026700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-CLOSE  LISTVAL                 
026800                         ALT-PCB WS-PRT-DUMMY WS-PRT-DUMMY                
026900                                                                          
027000     .                                                                    
027100     EJECT                                                                
027200 BA-PRINT-RUBRIKER    SECTION.                                            
027300     MOVE 'BA-PRINT-RUBRIKER '     TO CURRENT-SECTION                     
027400                                                                          
027500     MOVE SKLI-IDKOLLI-SAMP            TO RUB1-IDKOLLI-SAMP               
027510     MOVE SKLI-IDDC                    TO RUB1-IDDC                       
027600     MOVE SIDNR                        TO RUB1-SIDNR                      
027700     MOVE RUBRIKRAD-1                  TO UT-RAD                          
027800                                                                          
027900     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
028000                         ALT-PCB PRT-NYSIDA-RAD1 UT-RAD                   
028100                                                                          
028200     MOVE RUBRIKRAD-2                  TO UT-RAD                          
028300                                                                          
028400     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
028500                         ALT-PCB PRT-AFTER-3 UT-RAD                       
028600                                                                          
028700     MOVE +4                           TO RADINDX                         
028800     ADD  +1                           TO SIDNR                           
028900     .                                                                    
029000     EJECT                                                                
029100                                                                          
029200 BB-PRINT-RAD                   SECTION.                                  
029300     MOVE 'BB-PRINT-RAD  '     TO CURRENT-SECTION                         
029400                                                                          
029500     MOVE SKOR-IDDISTR          TO RAD1-IDDISTR                           
029600     MOVE SKOR-IDKUNDNR         TO RAD1-IDKUNDNR                          
029700     MOVE SKOR-IDORDNR7         TO RAD1-IDORDNR7                          
029800     MOVE SKOR-IDKOLLI          TO RAD1-IDKOLLI                           
029900                                                                          
030000     MOVE DETALJRAD-1           TO UT-RAD                                 
030100                                                                          
030200     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
030300                         ALT-PCB PRT-AFTER-1 UT-RAD                       
030400                                                                          
030500     ADD +1                     TO RADINDX                                
030600     .                                                                    
030700                                                                          
030800     EJECT                                                                
030900                                                                          
031000 BC-PRINT-TOTAL                 SECTION.                                  
031100     MOVE 'BC-PRINT-TOTAL'     TO CURRENT-SECTION                         
031200                                                                          
031300     MOVE SKLI-KVKOLLI-SAMP     TO TOT1-KVKOLLI-SAMP                      
031400                                                                          
031500     MOVE TOTALRAD-1            TO UT-RAD                                 
031600                                                                          
031700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE LISTVAL                  
031800                         ALT-PCB PRT-AFTER-4 UT-RAD                       
031900     .                                                                    
032000                                                                          
032100     SKIP3                                                                
032200                                                                          
032300* --- IMS SEKTIONER ---                                                   
032400                                                                          
032500 IMS-GET-MSG SECTION.                                                     
032600                                                                          
032700     MOVE '  QC' TO GODK-STATUSKODER                                      
032800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
032900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033000     PERFORM IMS-STATUSKONTROLL                                           
033100     .                                                                    
033200 IMS-GU-WDE711-ASEQ  SECTION.                                             
033300                                                                          
033400     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
033500          DELIMITED BY SIZE INTO SSA1                                     
033600                                                                          
033700                                                                          
033800     MOVE '  ' TO GODK-STATUSKODER                                        
033900     CALL CBLTDLI USING GU WDE7A-PCB DLI-IO-WDE711 SSA1                   
034000     MOVE WDE7A-STATUS-CODE TO STATUS-WS                                  
034100                                                                          
034200                                                                          
034300     PERFORM IMS-STATUSKONTROLL                                           
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700 IMS-GNP-WDE721-ASEQ-KVAL  SECTION.                                       
034900                                                                          
035000     STRING 'WDE721  (WDE721KY>=' W-WDE721KY-MIN-X                        
035100                    '&WDE721KY<=' W-WDE721KY-MAX-X ')'                    
035200          DELIMITED BY SIZE INTO SSA1                                     
035300                                                                          
035400     MOVE '  GE'                 TO GODK-STATUSKODER                      
035500     CALL CBLTDLI USING GNP  WDE7A-PCB DLI-IO-WDE721 SSA1                 
035600     MOVE WDE7A-STATUS-CODE      TO STATUS-WS                             
035700                                                                          
035800     PERFORM IMS-STATUSKONTROLL                                           
035900     .                                                                    
036000     EJECT                                                                
036100 IMS-STATUSKONTROLL SECTION.                                              
036200                                                                          
036300     SET STATUS-IX TO 1                                                   
036400     SEARCH GODK-STATUS                                                   
036500       AT END                                                             
036600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036700           DELIMITED BY SIZE INTO FELTEXT                                 
036800         CALL FELLOG                                                      
036900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037000         CONTINUE                                                         
037100     END-SEARCH                                                           
037200     .                                                                    

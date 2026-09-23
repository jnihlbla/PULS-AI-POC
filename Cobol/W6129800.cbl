000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6129800.                                                
000300 AUTHOR.         LINDKVIST JOHAN.                                         
000400 DATE-WRITTEN.   05/10/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE PARTS LOCATION AND PARTS LOCATION HISTORY                 
001000*                                                                         
001600*        THE PROGRAM UPDATES   WDK7                                       
001700*        THE PROGRAM UPDATES   WDJ9                                       
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INPUT FILE, PARTS LOCATIONS TO BE DELETED                  
002800     SELECT W61297                     ASSIGN TO W61298D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61297                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W61297  -L.                                                    
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W6129800'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500     SKIP2                                                                
004600 01  ERRTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
004910 01  CHKP-VAR.                                                            
004920 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004930 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004940 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004950 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004960 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004970 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
004980                                                                          
005000*    --- SWITCHAR                                                         
005100 77  WDJ911-SW                   PIC X       VALUE 'N'.                   
005200     88  WDJ911-P-SEG-NOT-FOUND              VALUE 'N'.                   
005300     88  WDJ911-P-SEG-FOUND                  VALUE 'J'.                   
005400*                                                                         
005500 77  W61297-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W61297                       VALUE 'Y'.                   
005700     EJECT                                                                
005800*                                                                         
005900 01  WS-LOGG-DATUM               PIC S9(9)   COMP-3.                      
006000 01  WS-LOGG-TID                 PIC S9(7)   COMP-3.                      
006010 01  WS-LOGG-TID-NY              PIC S9(7)   COMP-3.                      
006100 01  WS-LOGG-USER                PIC X(8).                                
006200*                                                                         
006300 01  WS-ADLAGOMR-OLD             PIC 9(2).                                
006400 01  WS-ADGANG-OLD               PIC 9(2).                                
006500 01  WS-ADPLATS-OLD              PIC 9(5).                                
006600*                                                                         
006700 01  WS-DASTADAT-9KOMPL          PIC S9(9)   COMP-3.                      
006800 01  WS-TISTATID-9KOMPL          PIC S9(7)   COMP-3.                      
006900     EJECT                                                                
007000 01  GENERAL-SUBPROGRAMS.                                                 
007100*                                                                         
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL POSTSUM                                          
007700*                                                                         
007800*01  -COPY W0005   -PRE  POSTSUM-                                         
007900     EJECT                                                                
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W61297     -PRE IN-                                       
008600*                                                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  KEYS-TILL-DLI.                                                       
009100     03  W-IDARTNR-X.                                                     
009200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009300     03  W-IDDC-X.                                                        
009400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009500     SKIP2                                                                
009600     03  W-WDJ911KY-MIN-X.                                                
009700         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
009800         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
009900     03  W-WDJ911KY-MAX-X.                                                
010000         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
010100         05  FILLER              PIC X(18)   VALUE HIGH-VALUE.            
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FOUND                       VALUE '  '.                  
010600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010810     88  SEGMENT-MISSING-PARENT              VALUE 'GP'.                  
010900     88  IMS-NOT-OK                          VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GOOD-STATUSCODES.                                                    
011200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNCTION CODES                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100                                                                          
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
012300 01  DLI-IO-WDK701.                                                       
012400*    03  -COPY WDK701                                                     
012500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
012600 01  DLI-IO-WDK711.                                                       
012700*    03  -COPY WDK711                                                     
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ901'.                      
012900 01  DLI-IO-WDJ901.                                                       
013000*    03  -COPY WDJ901                                                     
013100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ911'.                      
013200 01  DLI-IO-WDJ911.                                                       
013300*    03  -COPY WDJ911                                                     
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0009   -PRE MSG-                                              
013800                                                                          
013900*01  -COPY W0008  -PRE WDK7-                                              
014000     05  FILLER                  PIC X.                                   
014100                                                                          
014200*01  -COPY W0008  -PRE WDJ9-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDJ9-PCB.                     
014600 MAIN SECTION.                                                            
014700     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDJ9-PCB.                     
014800                                                                          
014900     SKIP2                                                                
015000     PERFORM A-INIT                                                       
015100     PERFORM S01-READ-W61297                                              
015200     PERFORM UNTIL END-OF-W61297                                          
015210       IF CHKP-ANT > CHKP-MAX                                             
015220         PERFORM IMS-CHECKPOINT                                           
015230         MOVE ZERO TO CHKP-ANT                                            
015240         MOVE ZERO TO W-IDARTNR                                           
015241         MOVE ZERO TO W-IDDC                                              
015250       END-IF                                                             
015260                                                                          
015400       MOVE IN-IDARTNR TO  W-IDARTNR                                      
015500       MOVE IN-IDDC    TO  W-IDDC                                         
015520                                                                          
015530       ADD +1 TO CHKP-ANT                                                 
015700       PERFORM B-REPL-PLATS-WDK7                                          
015800       PERFORM C-ADD-PLATSHIST-WDJ9                                       
015900                                                                          
016000       PERFORM S01-READ-W61297                                            
016100     END-PERFORM                                                          
016200                                                                          
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
016910                                                                          
017000 A-INIT SECTION.                                                          
017100     PERFORM IMS-RESTART                                                  
017200                                                                          
017300     OPEN INPUT W61297                                                    
017400                                                                          
017500     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-LOGG-DATUM                    
017600     MOVE FUNCTION CURRENT-DATE(9:6)  TO WS-LOGG-TID                      
017700     MOVE 'W61298'                    TO WS-LOGG-USER                     
017800                                                                          
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000     .                                                                    
018100     EJECT                                                                
018110                                                                          
018200 B-REPL-PLATS-WDK7 SECTION.                                               
018400     PERFORM IMS-GET-WDK701                                               
018500     IF SEGMENT-FOUND                                                     
018600        PERFORM IMS-GET-WDK711                                            
018700        IF SEGMENT-FOUND                                                  
018900***        SPARA UNDAN GAMMAL PLATS                                       
019000           MOVE SLAG-ADLAGOMR TO WS-ADLAGOMR-OLD                          
019100           MOVE SLAG-ADGANG   TO WS-ADGANG-OLD                            
019200           MOVE SLAG-ADPLATS  TO WS-ADPLATS-OLD                           
019300***        NOLLSTÄLL PLATS                                                
019400           MOVE ZERO TO SLAG-ADLAGOMR                                     
019500           MOVE ZERO TO SLAG-ADGANG                                       
019600           MOVE ZERO TO SLAG-ADPLATS                                      
019800           PERFORM IMS-REPL-WDK711                                        
019900        ELSE                                                              
020000          DISPLAY 'ART. EJ DC (11-SEG) ' IN-IDARTNR ' ' IN-IDDC           
020100        END-IF                                                            
020200     ELSE                                                                 
020300       DISPLAY 'ARTIKEL FOUND EJ PÅ WDK701 !? ' IN-IDARTNR                
020400     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
020610                                                                          
020700 C-ADD-PLATSHIST-WDJ9 SECTION.                                            
020900     PERFORM IMS-GU-WDJ901                                                
021000     IF SEGMENT-MISSING                                                   
021100***    OM EV. PLATSHISTORIK SKULLE SAKNAS LÄGGS SEGMENT UPP...            
021200        MOVE W-IDARTNR TO ART-IDARTNR                                     
021300        PERFORM IMS-ISRT-WDJ901                                           
021400        PERFORM IMS-GU-WDJ901                                             
021500     END-IF                                                               
021510                                                                          
021600     PERFORM IMS-GHNP-WDJ911                                              
021700     PERFORM UNTIL SEGMENT-MISSING OR WDJ911-P-SEG-FOUND                  
021800                   OR SEGMENT-MISSING-PARENT                              
021900        IF SEGMENT-FOUND AND HIST-KDLOC = 'P'                             
022000           MOVE WS-LOGG-DATUM TO HIST-DASTODAT                            
022100           MOVE WS-LOGG-USER  TO HIST-IDUSER-STO                          
022200           PERFORM IMS-REPL-WDJ911                                        
022300           PERFORM E-LOGG                                                 
022400           MOVE YES TO WDJ911-SW                                          
023701        ELSE                                                              
023702           PERFORM IMS-GHNP-WDJ911                                        
023710        END-IF                                                            
023800     END-PERFORM                                                          
023810     MOVE NOO TO WDJ911-SW                                                
023820                                                                          
023900     COMPUTE HIST-DASTADAT-9KOMPL = 99999999 - WS-LOGG-DATUM              
023910     MOVE FUNCTION CURRENT-DATE(9:6)     TO WS-LOGG-TID-NY                
024000     COMPUTE HIST-TISTATID-9KOMPL = 999999 - WS-LOGG-TID-NY               
024100                                                                          
024200     MOVE IN-IDDC                        TO HIST-IDDC                     
024300     MOVE 'P'                            TO HIST-KDLOC                    
024400     MOVE ZERO                           TO HIST-ADLAGOMR                 
024500     MOVE ZERO                           TO HIST-ADGANG                   
024600     MOVE ZERO                           TO HIST-ADPLATS                  
024700     MOVE WS-LOGG-USER                   TO HIST-IDUSER                   
024800     MOVE SPACE                          TO HIST-IDUSER-STO               
024900     MOVE ZERO                           TO HIST-DASTODAT                 
025000                                                                          
025100     PERFORM IMS-ISRT-WDJ911                                              
025300     .                                                                    
025400     EJECT                                                                
025401                                                                          
025402 E-LOGG SECTION.                                                          
025410     IF HIST-ADLAGOMR = WS-ADLAGOMR-OLD                                   
025420       IF HIST-ADGANG = WS-ADGANG-OLD                                     
025430         IF HIST-ADPLATS = WS-ADPLATS-OLD                                 
025440           CONTINUE                                                       
025450         ELSE                                                             
025460           DISPLAY 'FEL: PLATS SAKN.' IN-IDARTNR ' ' HIST-ADPLATS         
025470         END-IF                                                           
025480       ELSE                                                               
025490         DISPLAY 'FEL: GÅNG SAKN.   ' IN-IDARTNR ' ' HIST-ADGANG          
025491       END-IF                                                             
025492     ELSE                                                                 
025493       DISPLAY 'FEL: LAG.OMR SAKN.  ' IN-IDARTNR ' ' HIST-ADLAGOMR        
025494     END-IF                                                               
025495     .                                                                    
025496     EJECT                                                                
025497                                                                          
025500 Z-FINIT SECTION.                                                         
025800     CLOSE W61297                                                         
025900     SKIP2                                                                
026000     MOVE 'S' TO POSTSUM-OPKOD                                            
026100     CALL POSTSUM USING POSTSUM-PARM                                      
026200     .                                                                    
026300     EJECT                                                                
026310                                                                          
026400 S01-READ-W61297  SECTION.                                                
026600     READ W61297 INTO IN-AREA                                             
026700     AT END                                                               
026800        SET END-OF-W61297 TO TRUE                                         
026900                                                                          
027000     NOT AT END                                                           
027100        MOVE 'W61297' TO POSTSUM-FDNAMN                                   
027200        MOVE 'W61298D1' TO POSTSUM-DDNAMN2                                
027300        MOVE 'TYP'     TO POSTSUM-TRANSTYP                                
027400        CALL POSTSUM USING POSTSUM-PARM                                   
027500     END-READ                                                             
027600     .                                                                    
027700     EJECT                                                                
027710                                                                          
027800* --- IMS SECTIONS  ---                                                   
028010                                                                          
028100 IMS-GET-WDK701 SECTION.                                                  
028300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
028400          DELIMITED BY SIZE INTO SSA1                                     
028500     MOVE '  GE' TO GOOD-STATUSCODES                                      
028600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
028700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSCHECK                                              
028900     .                                                                    
029000     SKIP3                                                                
029010                                                                          
029100 IMS-GET-WDK711 SECTION.                                                  
029300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
029400          DELIMITED BY SIZE INTO SSA1                                     
029500     MOVE '  GE' TO GOOD-STATUSCODES                                      
029600     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
029700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSCHECK                                              
029900     .                                                                    
030000     SKIP3                                                                
030010                                                                          
030100 IMS-REPL-WDK711 SECTION.                                                 
030300     MOVE '  ' TO GOOD-STATUSCODES                                        
030400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
030500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSCHECK                                              
030700     .                                                                    
030800     SKIP3                                                                
030810                                                                          
030900 IMS-GU-WDJ901 SECTION.                                                   
031000     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
031100          DELIMITED BY SIZE INTO SSA1                                     
031200     MOVE '  GE' TO GOOD-STATUSCODES                                      
031300     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1                    
031400     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSCHECK                                              
031600     .                                                                    
031700     SKIP3                                                                
031710                                                                          
031800 IMS-ISRT-WDJ901 SECTION.                                                 
031900     MOVE 'WDJ901   ' TO SSA1                                             
032000     MOVE '  II' TO GOOD-STATUSCODES                                      
032100     CALL CBLTDLI USING ISRT WDJ9-PCB DLI-IO-WDJ901 SSA1                  
032200     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
032300     PERFORM IMS-STATUSCHECK                                              
032400     .                                                                    
032500     SKIP3                                                                
032510                                                                          
032600 IMS-GHNP-WDJ911 SECTION.                                                 
032700     STRING 'WDJ911  (IDDC     =' W-IDDC-X ')'                            
032800             DELIMITED BY SIZE INTO SSA1                                  
032900     MOVE '  GE' TO GOOD-STATUSCODES                                      
033000     CALL CBLTDLI USING GHNP WDJ9-PCB DLI-IO-WDJ911 SSA1                  
033100     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
033200     PERFORM IMS-STATUSCHECK                                              
033300     .                                                                    
033400     EJECT                                                                
033410                                                                          
033500 IMS-REPL-WDJ911 SECTION.                                                 
033600     MOVE '  ' TO GOOD-STATUSCODES                                        
033700     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ911                       
033800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSCHECK                                              
034000     .                                                                    
034100     SKIP3                                                                
034110                                                                          
034200 IMS-ISRT-WDJ911 SECTION.                                                 
034300     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
034400          DELIMITED BY SIZE INTO SSA1                                     
034500     MOVE 'WDJ911   ' TO SSA2                                             
034600     MOVE '  II' TO GOOD-STATUSCODES                                      
034700     CALL CBLTDLI USING ISRT WDJ9-PCB DLI-IO-WDJ911 SSA1 SSA2             
034800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
034900     PERFORM IMS-STATUSCHECK                                              
035000     .                                                                    
035100     SKIP3                                                                
035101                                                                          
035102 IMS-RESTART SECTION.                                                     
035104     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035105     MOVE '  ' TO GOOD-STATUSCODES                                        
035106     CALL CBLTDLI USING XRST MSG-PCB                                      
035107                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035108                        CHKP-AREA-LENGTH CHKP-AREA                        
035109     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035110     PERFORM IMS-STATUSCHECK                                              
035111     .                                                                    
035112                                                                          
035113                                                                          
035120 IMS-CHECKPOINT SECTION.                                                  
035140     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035150     MOVE '  XD' TO GOOD-STATUSCODES                                      
035160     CALL CBLTDLI USING CHKP MSG-PCB                                      
035170                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035180                        CHKP-AREA-LENGTH CHKP-AREA                        
035190     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035191     PERFORM IMS-STATUSCHECK                                              
035192                                                                          
035193     IF IMS-NOT-OK                                                        
035194       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO ERRTEXT-STR            
035195       DISPLAY ERRTEXT                                                    
035196       CALL FELLOG                                                        
035197     END-IF                                                               
035198     .                                                                    
035199                                                                          
035200     EJECT                                                                
035201                                                                          
035210 IMS-STATUSCHECK SECTION.                                                 
035300     SKIP2                                                                
035400     SET STATUS-IX TO 1                                                   
035500     SEARCH GOOD-STATUS                                                   
035600       AT END                                                             
035700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035800           DELIMITED BY SIZE INTO ERRTEXT                                 
035900         DISPLAY ERRTEXT                                                  
036000         CALL FELLOG                                                      
036100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036200         CONTINUE                                                         
036300     END-SEARCH                                                           
036400     .                                                                    

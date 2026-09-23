000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2243800.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        DELETE WDG3 - 2255                                               
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDG3                                       
001200*        THE PROGRAM READS     WDD9                                       
001300*        THE PROGRAM READS     WDR4                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(8)    VALUE 'W2243800'.            
002910 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
002920 77  DBS-SECTION                 PIC X(30)  VALUE SPACE.                  
003000 01  CHKP-VAR.                                                            
003100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
003600     03 CHKP-MAX                 PIC S9(3)   VALUE +300 COMP-3.           
003700 77  YES                         PIC X       VALUE 'J'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900     SKIP2                                                                
004000 01  ERROR-TEXT.                                                          
004100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004300     EJECT                                                                
004400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES TODAYS-DATE.                                        
004600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004800     03  TODAYS-DATE-DAY         PIC 9(2).                                
004900     EJECT                                                                
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100*                                                                         
005200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400*                                                                         
005500     EJECT                                                                
005600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
005700     SKIP3                                                                
005800 01  KEYS-TILL-DLI.                                                       
005900     03  W-WDG3KEY01-X.                                                   
006000         05  W-IDHTYP            PIC X(4)    VALUE '2255'.                
006100         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
006200     03  W-WDGX2256KY-X.                                                  
006300         05  W-IDDC              PIC X(2)    VALUE LOW-VALUE.             
006400         05  W-IDLEVNR           PIC X(5)    VALUE LOW-VALUE.             
006500     03  W-WDGX2203-X.                                                    
006600         05  W-IDPTYP-2203       PIC X(4)    VALUE '2203'.                
006700         05  W-IDDC-2203         PIC X(2)    VALUE LOW-VALUE.             
006800         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
006900     03  W-WDD9A1KY-MIN-X.                                                
007000         05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                 
007010         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
007300     03  W-WDD9A1KY-MAX-X.                                                
007400         05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                 
007410         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
007700     03  W-IDDC-A1-X.                                                     
007800         05  W-IDDC-A1           PIC X(2)    VALUE SPACE.                 
007900     03  W-WDD901KY-X.                                                    
008000         05  W-IDARTNR-WDD9      PIC S9(9)   VALUE ZERO COMP-3.           
008100         05  W-IDDC-WDD9         PIC X(2)    VALUE SPACE.                 
008200     03  W-IDLEVNR-D9-X.                                                  
008300         05  W-IDLEVNR-D9        PIC X(5)    VALUE SPACE.                 
008400     03  W-KDAVROP-X.                                                     
008500         05  W-KDAVROP           PIC S9(1)   VALUE 2 COMP-3.              
008600                                                                          
008610     03  W-WDGXKEY-X.                                                     
008620         05  W-IDHTYP            PIC X(4)    VALUE '4579'.                
008630         05  W-IDPGM             PIC X(8)    VALUE 'W2243800'.            
008640         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
008650                                                                          
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FOUND                       VALUE '  '.                  
009100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009300     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009400     88  IMS-NOT-OK                          VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GOOD-STATUSCODES.                                                    
009700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(128).                              
010000 01  SSA2                        PIC X(128).                              
010100     EJECT                                                                
010200*    --- IMS FUNCTION CODES                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010900 01  FILLER         PIC X(20) VALUE 'DLI-IO-AREA-WDG301'.                 
011000 01  DLI-IO-AREA-WDG301.                                                  
011100*    03  -COPY WDG301                                                     
011200                                                                          
011210 01  FILLER         PIC X(20) VALUE 'DLI-IO-AREA-WDGX2256'.               
011220 01  DLI-IO-AREA-WDGX2256.                                                
011230*    03  -COPY WDGX2256                                                   
011240                                                                          
011300 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDD9A1'.                      
011400 01  DLI-IO-WDD9A1.                                                       
011500*    03  -COPY WDD9A1                                                     
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDD902'.                      
011900 01  DLI-IO-WDD902.                                                       
012000*    03  -COPY WDD902                                                     
012100     EJECT                                                                
012200                                                                          
012300 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDD905'.                      
012310 01  DLI-IO-WDD905.                                                       
012320*    03  -COPY WDD905                                                     
012330     EJECT                                                                
012340                                                                          
012400 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2204'.                    
012500 01  DLI-IO-WDGX2204.                                                     
012600*    03  -COPY WDGX2204                                                   
012700     EJECT                                                                
012800                                                                          
012900*-ÅTERSTARTSREGISTER WDR4                                                 
013000 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX4580'.                    
013100 01  DLI-IO-WDGX4580.                                                     
013200*    03  -COPY WDGX4580                                                   
013300                                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0009   -PRE MSG-                                              
013800                                                                          
013900*01  -COPY W0008  -PRE WDG3-                                              
014000     05  FILLER                  PIC X.                                   
014100                                                                          
014200*01  -COPY W0008  -PRE WDD9A-                                             
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500                                                                          
014600*01  -COPY W0008  -PRE WDD9-                                              
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900                                                                          
015000*01  -COPY W0008  -PRE 4579-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING MSG-PCB WDG3-PCB WDD9A-PCB WDD9-PCB            
015400                                   4579-PCB.                              
015500 MAIN SECTION.                                                            
015600     ENTRY 'DLITCBL' USING MSG-PCB WDG3-PCB WDD9A-PCB WDD9-PCB            
015700                                   4579-PCB.                              
015800                                                                          
015900     PERFORM A-INIT                                                       
016000                                                                          
016110     PERFORM IMS-GHU-WDG301-2255                                          
016200     PERFORM IMS-GNP-WDG301-2256                                          
016300     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
016420                                                                          
017100       MOVE 2256-IDLEVNR    TO W-IDLEVNR-MAX                              
017200                               W-IDLEVNR-MIN                              
017300       MOVE 2256-IDDC       TO W-IDDC-A1                                  
017400       PERFORM IMS-GU-WDD9A1                                              
017500       PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                    
017600         MOVE LEVA-IDARTNR  TO W-IDARTNR-WDD9                             
017700         MOVE LEVA-IDDC     TO W-IDDC-WDD9                                
017800         MOVE LEVA-IDLEVNR  TO W-IDLEVNR-D9                               
017900         PERFORM IMS-GU-WDD902                                            
018100         PERFORM IMS-GNP-WDD905                                           
018200         IF SEGMENT-FOUND                                                 
018300            PERFORM B-UPDATE-WDGX2204                                     
018400         END-IF                                                           
018600         PERFORM IMS-GN-WDD9A1                                            
018700       END-PERFORM                                                        
019000                                                                          
019001       IF CHKP-ANT > CHKP-MAX                                             
019002         PERFORM X-TAKE-CHECKPOINT                                        
019003       END-IF                                                             
019020                                                                          
019100       PERFORM IMS-GNP-WDG301-2256                                        
019200     END-PERFORM                                                          
019300                                                                          
019400     PERFORM IMS-GHU-WDG301-2255                                          
019500     PERFORM IMS-DLET-WDG301-2255                                         
019600     PERFORM IMS-ISRT-WDG301-2255                                         
019700                                                                          
019800     PERFORM Z-FINIT                                                      
019900                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700     MOVE 'A-INIT'             TO CURRENT-SECTION                         
020800                                                                          
020900     PERFORM IMS-RESTART                                                  
021000                                                                          
021100     PERFORM IMS-LAS-ATERSTART                                            
021200     IF 4580-IDWDGX2256 NOT = SPACE                                       
021300        MOVE 4580-IDWDGX2256   TO W-WDGX2256KY-X                          
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 B-UPDATE-WDGX2204 SECTION.                                               
021710     MOVE 'B-UPDATE-WDGX2204'   TO CURRENT-SECTION                        
021800                                                                          
021900     MOVE LEVA-IDDC            TO W-IDDC-2203                             
022000     MOVE LEVA-IDARTNR         TO 2204-IDARTNR                            
022100     MOVE 19                   TO 2204-KDLPORS                            
022200     PERFORM IMS-ISRT-WDGX2204                                            
022210     ADD  +1                   TO CHKP-ANT                                
022300     .                                                                    
022400                                                                          
022500 Z-FINIT SECTION.                                                         
022510     MOVE 'Z-FINIT         '   TO CURRENT-SECTION                         
022600                                                                          
022900     PERFORM IMS-LAS-ATERSTART                                            
023000     MOVE SPACE                TO 4580-IDWDGX2256                         
023100     MOVE +0                   TO 4580-TIUPPDAT                           
023200     MOVE +0                   TO 4580-TIUPPTID                           
023300     PERFORM IMS-REPL-ATERSTART                                           
023400     .                                                                    
023500                                                                          
023600 X-TAKE-CHECKPOINT   SECTION.                                             
023610     MOVE 'X-TAKE-CHECKPOINT'  TO CURRENT-SECTION                         
023700                                                                          
024300     PERFORM IMS-LAS-ATERSTART                                            
024310                                                                          
024400     MOVE 2256-IDDC            TO W-IDDC                                  
024401     MOVE 2256-IDLEVNR         TO W-IDLEVNR                               
024410     MOVE W-WDGX2256KY-X       TO 4580-IDWDGX2256                         
024500     ACCEPT 4580-TIUPPDAT    FROM DATE                                    
024600     ACCEPT 4580-TIUPPTID    FROM TIME                                    
024610                                                                          
024700     PERFORM IMS-REPL-ATERSTART                                           
024701                                                                          
024710     PERFORM IMS-CHECKPOINT                                               
024718                                                                          
024719     PERFORM IMS-GHU-WDG301-2255                                          
024723                                                                          
024730     MOVE +0                   TO CHKP-ANT                                
024800     .                                                                    
025700     EJECT                                                                
025800* --- IMS SECTIONS  ---                                                   
025900                                                                          
026000     EJECT                                                                
028000 IMS-GHU-WDG301-2255 SECTION.                                             
028010     MOVE 'IMS-GHU-WDG301-2255  ' TO DBS-SECTION                          
028100                                                                          
028200     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
028300            DELIMITED BY SIZE INTO SSA1                                   
028400     MOVE '  ' TO GOOD-STATUSCODES                                        
028500     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-AREA-WDG301 SSA1              
028600     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
028700     PERFORM IMS-STATUSCHECK                                              
028800     .                                                                    
028801                                                                          
028810 IMS-GNP-WDG301-2256 SECTION.                                             
028820     MOVE 'IMS-GNP-WDG301-2256  ' TO DBS-SECTION                          
028830                                                                          
028840     STRING 'WDGX2256(KY2256   >' W-WDGX2256KY-X ')'                      
028850            DELIMITED BY SIZE INTO SSA1                                   
028860     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
028870     CALL CBLTDLI USING GNP WDG3-PCB DLI-IO-AREA-WDGX2256 SSA1            
028880     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
028890     PERFORM IMS-STATUSCHECK                                              
028891     .                                                                    
028900                                                                          
029000 IMS-DLET-WDG301-2255 SECTION.                                            
029010     MOVE 'IMS-DLET-WDG301-2255 ' TO DBS-SECTION                          
029100                                                                          
029200     MOVE '  ' TO GOOD-STATUSCODES                                        
029300     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-AREA-WDG301                  
029400     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSCHECK                                              
029600     .                                                                    
029700     SKIP3                                                                
029710 IMS-ISRT-WDG301-2255 SECTION.                                            
029711     MOVE 'IMS-ISRT-WDG301-2255 ' TO DBS-SECTION                          
029720                                                                          
029730     MOVE 'WDG301 '        TO SSA1                                        
029740     MOVE '  II' TO GOOD-STATUSCODES                                      
029750     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-AREA-WDG301 SSA1             
029760     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
029770     PERFORM IMS-STATUSCHECK                                              
029780     .                                                                    
029790     SKIP3                                                                
029800 IMS-ISRT-WDGX2204 SECTION.                                               
029810     MOVE 'IMS-ISRT-WDGX2204    ' TO DBS-SECTION                          
029900                                                                          
030000     STRING 'WDG301  (WDG3KEY  =' W-WDGX2203-X ')'                        
030100          DELIMITED BY SIZE INTO SSA1                                     
030200     MOVE   'WDG302  '        TO SSA2                                     
030300     MOVE '  '              TO GOOD-STATUSCODES                           
030400     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2204 SSA1 SSA2           
030500     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
030600     PERFORM IMS-STATUSCHECK                                              
030700     .                                                                    
031700 IMS-GU-WDD9A1 SECTION.                                                   
031710     MOVE 'IMS-GU-WDD9A1        ' TO DBS-SECTION                          
031800                                                                          
031810     STRING 'WDD9A1  (WDD9A1KY>=' W-WDD9A1KY-MIN-X                        
032010                    '&WDD9A1KY<=' W-WDD9A1KY-MAX-X                        
032100                    '&IDDC     =' W-IDDC-A1-X ')'                         
032200        DELIMITED BY SIZE INTO SSA1                                       
032300     MOVE '  GE'            TO GOOD-STATUSCODES                           
032400     CALL CBLTDLI        USING GU WDD9A-PCB DLI-IO-WDD9A1 SSA1            
032500     MOVE WDD9A-STATUS-CODE TO STATUS-WS                                  
032600     PERFORM IMS-STATUSCHECK                                              
032700     .                                                                    
032800     SKIP3                                                                
032900 IMS-GN-WDD9A1 SECTION.                                                   
032901     MOVE 'IMS-GN-WDD9A1        ' TO DBS-SECTION                          
032910                                                                          
032920     STRING 'WDD9A1  (WDD9A1KY>=' W-WDD9A1KY-MIN-X                        
032930                    '&WDD9A1KY<=' W-WDD9A1KY-MAX-X                        
032940                    '&IDDC     =' W-IDDC-A1-X ')'                         
032950          DELIMITED BY SIZE INTO SSA1                                     
032960     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
032970     CALL CBLTDLI USING GN WDD9A-PCB DLI-IO-WDD9A1 SSA1                   
032980     MOVE WDD9A-STATUS-CODE   TO STATUS-WS                                
032990     PERFORM IMS-STATUSCHECK                                              
032991     .                                                                    
032992     SKIP3                                                                
033000 IMS-GU-WDD902 SECTION.                                                   
033100     MOVE 'IMS-GU-WDD902        ' TO DBS-SECTION                          
033110                                                                          
033200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
033300          DELIMITED BY SIZE   INTO SSA1                                   
033400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-D9-X ')'                      
033500          DELIMITED BY SIZE INTO SSA2                                     
033600     MOVE '    ' TO GOOD-STATUSCODES                                      
033700     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
033800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSCHECK                                              
034000     .                                                                    
034100     SKIP3                                                                
034200 IMS-GNP-WDD905 SECTION.                                                  
034300     MOVE 'IMS-GNP-WDD905       ' TO DBS-SECTION                          
034310                                                                          
034400     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
034500          DELIMITED BY SIZE INTO SSA1                                     
034600     MOVE '  GE' TO GOOD-STATUSCODES                                      
034700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
034800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
034900     PERFORM IMS-STATUSCHECK                                              
035000     .                                                                    
035100     EJECT                                                                
035200 IMS-LAS-ATERSTART SECTION.                                               
035210     MOVE 'IMS-LAS-ATERSTART ' TO DBS-SECTION                             
035211     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
035212                    DELIMITED BY SIZE INTO SSA1                           
035213     MOVE 'WDR470 '      TO SSA2                                          
035214     MOVE '  '           TO GOOD-STATUSCODES                              
035215     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
035216     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
035217     PERFORM IMS-STATUSCHECK                                              
035800     .                                                                    
035801                                                                          
035880 IMS-REPL-ATERSTART SECTION.                                              
035890     MOVE 'IMS-REPL-ATERSTART    ' TO DBS-SECTION                         
035891                                                                          
035892     MOVE '  ' TO GOOD-STATUSCODES                                        
035893     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
035894     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
035895     PERFORM IMS-STATUSCHECK                                              
035896     .                                                                    
035897                                                                          
035900 IMS-RESTART SECTION.                                                     
035910     MOVE 'IMS-RESTART          ' TO DBS-SECTION                          
035920                                                                          
036100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036200     MOVE '  ' TO GOOD-STATUSCODES                                        
036300     CALL CBLTDLI USING XRST MSG-PCB                                      
036400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036500                        CHKP-AREA-LENGTH CHKP-AREA                        
036600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036700     PERFORM IMS-STATUSCHECK                                              
036800     .                                                                    
036900     SKIP3                                                                
037000 IMS-CHECKPOINT SECTION.                                                  
037110     MOVE 'IMS-CHECKPOINT       ' TO DBS-SECTION                          
037120                                                                          
037200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037300     MOVE '  XD' TO GOOD-STATUSCODES                                      
037400     CALL CBLTDLI USING CHKP MSG-PCB                                      
037500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037600                        CHKP-AREA-LENGTH CHKP-AREA                        
037700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037800     PERFORM IMS-STATUSCHECK                                              
037900                                                                          
038000     IF IMS-NOT-OK                                                        
038110     MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERROR-TEXT-STR        
038200      DISPLAY ERROR-TEXT                                                  
038300      CALL FELLOG                                                         
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700 IMS-STATUSCHECK SECTION.                                                 
038820                                                                          
038900     SET STATUS-IX TO 1                                                   
039000     SEARCH GOOD-STATUS                                                   
039100       AT END                                                             
039200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039300           DELIMITED BY SIZE INTO ERROR-TEXT                              
039400         DISPLAY ERROR-TEXT                                               
039500         CALL FELLOG                                                      
039600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
039700         CONTINUE                                                         
039800     END-SEARCH                                                           
039900     .                                                                    

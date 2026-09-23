000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W271G200.                                                
000300 AUTHOR.         SATHEESH RAGUR.                                          
000400 DATE-WRITTEN.   18/07/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE WDE301                                                    
001000*               WDK711                                                    
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- INPUT FILE FROM W271G1                                     
002100     SELECT W271G2                     ASSIGN TO W271G2D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W271G2                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY W271G1      -L.                                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W271G200'.            
003600 01  CHKP-VAR.                                                            
003700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004200     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004401 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004402 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
004420 77  WS-KVBEART                  PIC S9(7)   VALUE ZERO COMP-3.           
004500     SKIP2                                                                
004600 01  ERROR-TEXT.                                                          
004700     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
004800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 77  W271G2-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W271G2                       VALUE 'Y'.                   
005200     EJECT                                                                
005300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES TODAYS-DATE.                                        
005500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005700     03  TODAYS-DATE-DAY         PIC 9(2).                                
005800     EJECT                                                                
005900 01  GENERAL-SUBPROGRAMS.                                                 
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  IN-AREA-START               PIC X(24)   VALUE                        
007000                                             'IN-AREA-START'.             
007100                                                                          
007200*01  AREA -COPY W271G1     -PRE IN-                                       
007300*                                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700 01  KEYS-TILL-DLI.                                                       
007800                                                                          
007900     03 W-IDARTNR-X.                                                      
008000         05  W-IDARTNR           PIC S9(9) VALUE +0 COMP-3.               
008100                                                                          
010210     03 W-WDE301KY-MIN-X.                                                 
010220         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
010230         05  FILLER              PIC X(11) VALUE LOW-VALUE.               
010240                                                                          
010241     03  W-KDREFTYP-MIN-X.                                                
010242         05  W-KDREFTYP-A-MIN    PIC X       VALUE 'A'.                   
010243                                                                          
010250     03 W-WDE301KY-MAX-X.                                                 
010260         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
010270         05  FILLER              PIC X(11) VALUE HIGH-VALUE.              
           03  W-IDDC-X.                                                        
               05  W-IDDC              PIC X(2)    VALUE ZERO.                  
010280                                                                          
010290     03  W-KDREFTYP-MAX-X.                                                
010291         05  W-KDREFTYP-O-MAX    PIC X       VALUE 'O'.                   
010292                                                                          
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FOUND                       VALUE '  '.                  
010600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010900     88  IMS-NOT-OK                          VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GOOD-STATUSCODES.                                                    
011200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(128).                              
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNCTION CODES                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE301'.                      
012200 01  DLI-IO-WDE301.                                                       
012300*    03  -COPY WDE301                                                     
012400     EJECT                                                                
012500                                                                          
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
012700 01  DLI-IO-WDK711.                                                       
012800*    03  -COPY WDK711                                                     
012900     EJECT                                                                
012910                                                                          
013100 LINKAGE SECTION.                                                         
013200                                                                          
013300*01  -COPY W0009   -PRE MSG-                                              
013400                                                                          
013500*01  -COPY W0008  -PRE WDE3-                                              
013600     05  FILLER                  PIC X.                                   
013700                                                                          
013800*01  -COPY W0008  -PRE WDK7-                                              
013900     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100 PROCEDURE DIVISION  USING MSG-PCB WDE3-PCB WDK7-PCB.                     
014200 MAIN SECTION.                                                            
014300     ENTRY 'DLITCBL' USING MSG-PCB WDE3-PCB WDK7-PCB.                     
014400                                                                          
014500     SKIP2                                                                
014600     PERFORM A-INIT                                                       
014700     PERFORM S01-READ-W271G2                                              
014800     PERFORM UNTIL END-OF-W271G2                                          
014900       IF CHKP-ANT > CHKP-MAX                                             
015000         PERFORM X-TAKE-CHECKPOINT                                        
015100       END-IF                                                             
015200                                                                          
015300       PERFORM D-BATFORSLAG-DELETE                                        
015400                                                                          
015500       PERFORM S01-READ-W271G2                                            
015600     END-PERFORM                                                          
015700                                                                          
015800                                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600     SKIP2                                                                
016700                                                                          
016800     PERFORM IMS-RESTART                                                  
016900                                                                          
017000     OPEN INPUT W271G2                                                    
017100                                                                          
017200                                                                          
017300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017400     .                                                                    
017500     EJECT                                                                
017510 D-BATFORSLAG-DELETE SECTION.                                             
017511     MOVE 'D-BATFORSLAG-DELETE' TO CURRENT-SECTION                        
017512                                                                          
017513     MOVE IN-IDDC     TO W-IDDC-MIN                                       
017514                         W-IDDC-MAX                                       
017514                         W-IDDC                                           
017515     MOVE IN-IDARTNR  TO W-IDARTNR                                        
017518                                                                          
017521     MOVE ZERO        TO WS-KVBEART                                       
017522     PERFORM IMS-GHU-WDE301-E3                                            
017523     IF SEGMENT-FOUND                                                     
017526       IF (REF-KDREFTYP = 'O') OR (REF-KDREFORS = 'O')                    
017527         MOVE REF-KVBEART TO WS-KVBEART                                   
017529       END-IF                                                             
017530                                                                          
017532       PERFORM IMS-DLET-WDE301                                            
017533       ADD +1 TO CHKP-ANT                                                 
017534     END-IF                                                               
017535                                                                          
017537     IF WS-KVBEART > ZERO                                                 
017538       MOVE IN-IDARTNR     TO W-IDARTNR                                   
017540       PERFORM IMS-GHU-WDK7-SLAG                                          
017541       IF SEGMENT-FOUND                                                   
017544         COMPUTE SLAG-KVBEART = SLAG-KVBEART - WS-KVBEART                 
017548         PERFORM IMS-REPL-WDK711                                          
017549         ADD +1 TO CHKP-ANT                                               
017550       END-IF                                                             
017551     END-IF                                                               
017552     .                                                                    
017560     EJECT                                                                
023900 Z-FINIT SECTION.                                                         
024000                                                                          
024100                                                                          
024200     CLOSE W271G2                                                         
024300     SKIP2                                                                
024400     MOVE 'S' TO POSTSUM-OPKOD                                            
024500     CALL POSTSUM   USING POSTSUM-PARM                                    
024600     .                                                                    
024700     EJECT                                                                
024800 S01-READ-W271G2  SECTION.                                                
024810     MOVE 'S01-READ-W271G2       ' TO CURRENT-SECTION                     
024900     SKIP2                                                                
025000     READ W271G2 INTO IN-AREA                                             
025100     AT END                                                               
025200        SET END-OF-W271G2 TO TRUE                                         
025300                                                                          
025400     NOT AT END                                                           
025500        MOVE 'W271G2' TO POSTSUM-FDNAMN                                   
025600        MOVE 'W271G2D1' TO POSTSUM-DDNAMN2                                
025700        MOVE '        ' TO POSTSUM-TRANSTYP                               
025800        CALL POSTSUM USING POSTSUM-PARM                                   
025900                                                                          
026100     END-READ                                                             
026200     .                                                                    
026300     EJECT                                                                
034200 X-TAKE-CHECKPOINT   SECTION.                                             
034300                                                                          
034400* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
034500* --- SAVE DATABASE KEYS IF NECESSARY                                     
034600     PERFORM IMS-CHECKPOINT                                               
034700     MOVE ZERO TO CHKP-ANT                                                
034800* --- REREAD DATABASE IF NECESSARY                                        
034900     .                                                                    
035000     EJECT                                                                
035100* --- IMS SECTIONS  ---                                                   
035200                                                                          
035300     EJECT                                                                
035400 IMS-RESTART SECTION.                                                     
035410     MOVE 'IMS-RESTART      '  TO DBS-SECTION                             
035500     SKIP2                                                                
035600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035700     MOVE '  ' TO GOOD-STATUSCODES                                        
035800     CALL CBLTDLI USING XRST MSG-PCB                                      
035900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
036000                        CHKP-AREA-LENGTH CHKP-AREA                        
036100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036200     PERFORM IMS-STATUSCHECK                                              
036300     .                                                                    
036400     SKIP3                                                                
036500 IMS-CHECKPOINT SECTION.                                                  
036510     MOVE 'IMS-CHECKPOINT   '  TO DBS-SECTION                             
036600     SKIP2                                                                
036700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
036800     MOVE '  XD' TO GOOD-STATUSCODES                                      
036900     CALL CBLTDLI USING CHKP MSG-PCB                                      
037000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
037100                        CHKP-AREA-LENGTH CHKP-AREA                        
037200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037300     PERFORM IMS-STATUSCHECK                                              
037400                                                                          
037500     IF IMS-NOT-OK                                                        
037600       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO                     
037610                                                   ERROR-TEXT-STR         
037700       DISPLAY ERROR-TEXT                                                 
037800       CALL FELLOG                                                        
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038110 IMS-GHU-WDE301-E3 SECTION.                                               
038120     MOVE 'IMS-GHU-WDE301-E3' TO DBS-SECTION                              
038130                                                                          
038150     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
038160                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
038170                    '&KDREFTYP>=' W-KDREFTYP-MIN-X                        
038180                    '&KDREFTYP<=' W-KDREFTYP-MAX-X                        
038190                    '&IDARTNR  =' W-IDARTNR-X ')'                         
038191          DELIMITED BY SIZE INTO SSA1                                     
038192     MOVE '  GE' TO GOOD-STATUSCODES                                      
038193     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-WDE301 SSA1                   
038194     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
038195     PERFORM IMS-STATUSCHECK                                              
038197     .                                                                    
038198     EJECT                                                                
041210 IMS-DLET-WDE301        SECTION.                                          
041220     MOVE 'IMS-DLET-WDE301 '  TO DBS-SECTION                              
041230                                                                          
041240     MOVE '  ' TO GOOD-STATUSCODES                                        
041250     CALL CBLTDLI USING DLET WDE3-PCB DLI-IO-WDE301                       
041260     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
041261     PERFORM IMS-STATUSCHECK                                              
041280     .                                                                    
041290     EJECT                                                                
041300 IMS-GHU-WDK7-SLAG SECTION.                                               
041310     MOVE 'IMS-GHU-WDK7-SLAG      ' TO DBS-SECTION                        
041400                                                                          
041500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
041600          DELIMITED BY SIZE INTO SSA1                                     
           STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
                DELIMITED BY SIZE INTO SSA2                                     
041800     MOVE '  GE' TO GOOD-STATUSCODES                                      
041900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
042000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
042001     PERFORM IMS-STATUSCHECK                                              
042200     .                                                                    
042300     SKIP3                                                                
042400                                                                          
042410 IMS-REPL-WDK711 SECTION.                                                 
042420     MOVE 'IMS-REPL-WDK711 '  TO DBS-SECTION                              
042430                                                                          
042440     MOVE '  ' TO GOOD-STATUSCODES                                        
042450     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
042460     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
042461     PERFORM IMS-STATUSCHECK                                              
042480     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 IMS-STATUSCHECK SECTION.                                                 
045200     SKIP2                                                                
045300     SET STATUS-IX TO 1                                                   
045400     SEARCH GOOD-STATUS                                                   
045500       AT END                                                             
045600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
045700           DELIMITED BY SIZE INTO ERROR-TEXT                              
045800         DISPLAY ERROR-TEXT                                               
045900         CALL FELLOG                                                      
046000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
046100         CONTINUE                                                         
046200     END-SEARCH                                                           
046300     .                                                                    

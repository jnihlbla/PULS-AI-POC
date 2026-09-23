000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2241100.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/02/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        MATRIALFÖRSÖRJNING LOKAL ANSKAFFNING KINA                        
001000*                                                                         
001100*        THE PROGRAM READS     WDG3                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- 2203 RECORDS FROM W22410                                   
002200     SELECT W22411                     ASSIGN TO W22411D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W22411                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W2242203    -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W2241100'.            
003700 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
003800 77  DBS-SECTION                 PIC X(30)  VALUE SPACE.                  
003900*01  -COPY WWDCKONS                                                       
004000 01  CHKP-VAR.                                                            
004100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004601     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004610 01  W-KVPOST-IN                 PIC S9(9)  VALUE ZERO COMP SYNC.         
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900     SKIP2                                                                
004910 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004920 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004930 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004940                                                                          
005000 01  ERROR-TEXT.                                                          
005100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W22411-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W22411                       VALUE 'Y'.                   
005600     EJECT                                                                
005700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES TODAYS-DATE.                                        
005900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006100     03  TODAYS-DATE-DAY         PIC 9(2).                                
006110                                                                          
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400*                                                                         
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006610     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300 01  IN-AREA-START               PIC X(24)   VALUE                        
007400                                             'IN-AREA-START'.             
007500     SKIP2                                                                
007600                                                                          
007700*01  AREA -COPY W2242203   -PRE IN-                                       
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  KEYS-TILL-DLI.                                                       
008300     03  W-WDGXKEY-2203-X.                                                
008400         05  W-IDHTYP-2203       PIC X(04)   VALUE '2203'.                
008500         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
008600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
008700                                                                          
008710     03  W-WDGXKEY-4579-X.                                                
008720         05  W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
008730         05  W-IDPGM             PIC X(8)    VALUE 'W2241100'.            
008740         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
008750                                                                          
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
010500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2203'.                    
010600 01  DLI-IO-WDGX2203.                                                     
010700*    03  -COPY WDGX01DC                                                   
010800     EJECT                                                                
010810*-ÅTERSTARTSREGISTER WDR4                                                 
010820 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
010830 01  DLI-IO-WDGX4580.                                                     
010840*    03  -COPY WDGX4580                                                   
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200                                                                          
011300*01  -COPY W0009   -PRE MSG-                                              
011400                                                                          
011500*01  -COPY W0008  -PRE WDG3-                                              
011600     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011710*01  -COPY W0008  -PRE WDR4-                                              
011720     05  FILLER                  PIC X.                                   
011730     EJECT                                                                
011800 PROCEDURE DIVISION  USING MSG-PCB WDG3-PCB WDR4-PCB.                     
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING MSG-PCB WDG3-PCB WDR4-PCB.                     
012100                                                                          
012200     PERFORM A-INIT                                                       
012202                                                                          
012203     PERFORM IMS-LAS-ATERSTART                                            
012226                                                                          
012227     IF 4580-KVPOST > +0                                                  
012229        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
012230     ELSE                                                                 
012300        PERFORM S01-READ-W22411                                           
012311     END-IF                                                               
012320                                                                          
012400     PERFORM UNTIL END-OF-W22411                                          
012900       MOVE IN-IDHTYP    TO W-IDHTYP-2203                                 
013000       MOVE IN-IDDC      TO W-IDDC                                        
013100       PERFORM IMS-GHU-WDG301-2203                                        
013200       IF SEGMENT-FOUND                                                   
013300          PERFORM IMS-DELETE-WDG301-2203                                  
013301                                                                          
013302          PERFORM IMS-INSERT-WDG301-2203                                  
013320                                                                          
013400          PERFORM X-TAKE-CHECKPOINT                                       
013500       END-IF                                                             
013600                                                                          
013700       PERFORM S01-READ-W22411                                            
013800     END-PERFORM                                                          
014300                                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015010     MOVE 'A-INIT                   ' TO CURRENT-SECTION                  
015200                                                                          
015300     PERFORM IMS-RESTART                                                  
015400                                                                          
015500     OPEN INPUT W22411                                                    
015600                                                                          
015610     MOVE +0                 TO W-KVPOST-IN                               
015620                                CHKP-ANT                                  
015730                                                                          
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900     .                                                                    
016000     EJECT                                                                
016100 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
016200     MOVE 'B-LAES-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                  
016300                                                                          
016700     PERFORM S01-READ-W22411                                              
016800     PERFORM UNTIL END-OF-W22411 OR                                       
016900                   W-KVPOST-IN = 4580-KVPOST                              
017000        PERFORM S01-READ-W22411                                           
017100     END-PERFORM                                                          
017200                                                                          
017300     IF END-OF-W22411                                                     
017310        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
017320                      TO ERROR-TEXT                                       
017330        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
017340     END-IF                                                               
017360     .                                                                    
017370     EJECT                                                                
017400 Z-FINIT SECTION.                                                         
017500                                                                          
017700     CLOSE W22411                                                         
017800                                                                          
018010*    NOLLA ÅTERSTARTINFORMATIONEN                                         
018020     PERFORM IMS-LAS-ATERSTART                                            
018030     MOVE +0                TO 4580-KVPOST                                
018040     ACCEPT 4580-TIUPPDAT FROM DATE                                       
018050     ACCEPT 4580-TIUPPTID FROM TIME                                       
018060                                                                          
018070     PERFORM IMS-REPL-ATERSTART                                           
018080                                                                          
018090     MOVE 'S'               TO POSTSUM-OPKOD                              
018091     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018200     EJECT                                                                
018300 S01-READ-W22411  SECTION.                                                
018400                                                                          
018500     READ W22411 INTO IN-AREA                                             
018600     AT END                                                               
018700        SET END-OF-W22411 TO TRUE                                         
018800                                                                          
018900     NOT AT END                                                           
019000        MOVE 'W22411'   TO POSTSUM-FDNAMN                                 
019100        MOVE 'W22411D1' TO POSTSUM-DDNAMN2                                
019200        MOVE '2203'     TO POSTSUM-TRANSTYP                               
019300        CALL POSTSUM USING POSTSUM-PARM                                   
019301                                                                          
019310        ADD +1              TO W-KVPOST-IN                                
019400                                                                          
019600     END-READ                                                             
019700     .                                                                    
019800     EJECT                                                                
019900 X-TAKE-CHECKPOINT   SECTION.                                             
019901                                                                          
019910*    UPPDATERA ÅTERSTARTREGISTRET                                         
019920     PERFORM IMS-LAS-ATERSTART                                            
019930                                                                          
019940     MOVE W-KVPOST-IN       TO 4580-KVPOST                                
019950     ACCEPT 4580-TIUPPDAT FROM DATE                                       
019960     ACCEPT 4580-TIUPPTID FROM TIME                                       
019970                                                                          
019980     PERFORM IMS-REPL-ATERSTART                                           
019990                                                                          
019991*    TAG CHECKPOINT                                                       
019992     PERFORM IMS-CHECKPOINT                                               
019993                                                                          
019994     MOVE +0                TO CHKP-ANT                                   
020600     .                                                                    
020700     EJECT                                                                
020800* --- IMS SECTIONS  ---                                                   
020900                                                                          
021000     EJECT                                                                
021100 IMS-GHU-WDG301-2203 SECTION.                                             
021200     MOVE 'IMS-GHU-WDG301-2203  ' TO DBS-SECTION                          
021300                                                                          
021400     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
021500            DELIMITED BY SIZE INTO SSA1                                   
021600     MOVE '  GE'           TO GOOD-STATUSCODES                            
021700     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-WDGX2203 SSA1                 
021800     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
021900     PERFORM IMS-STATUSCHECK                                              
022000     .                                                                    
022100                                                                          
022200 IMS-DELETE-WDG301-2203 SECTION.                                          
022300     MOVE 'IMS-DELETE-WDG301-2203' TO DBS-SECTION                         
022400                                                                          
022500     MOVE '  '       TO GOOD-STATUSCODES                                  
022600     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-WDGX2203                     
022700     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
022800     PERFORM IMS-STATUSCHECK                                              
022900     .                                                                    
023000                                                                          
023100 IMS-INSERT-WDG301-2203 SECTION.                                          
023200     MOVE 'IMS-INSERT-WDG301-2203' TO DBS-SECTION                         
023300                                                                          
023510     MOVE 'WDG301  '       TO SSA1                                        
023600     MOVE '  II'           TO GOOD-STATUSCODES                            
023700     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2203 SSA1                
023800     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
023900     PERFORM IMS-STATUSCHECK                                              
024000     .                                                                    
024100                                                                          
024200 IMS-RESTART SECTION.                                                     
024300     SKIP2                                                                
024400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024500     MOVE '  ' TO GOOD-STATUSCODES                                        
024600     CALL CBLTDLI USING XRST MSG-PCB                                      
024700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024800                        CHKP-AREA-LENGTH CHKP-AREA                        
024900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025000     PERFORM IMS-STATUSCHECK                                              
025100     .                                                                    
025200     SKIP3                                                                
025300 IMS-CHECKPOINT SECTION.                                                  
025400     SKIP2                                                                
025500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025600     MOVE '  XD' TO GOOD-STATUSCODES                                      
025700     CALL CBLTDLI USING CHKP MSG-PCB                                      
025800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025900                        CHKP-AREA-LENGTH CHKP-AREA                        
026000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026100     PERFORM IMS-STATUSCHECK                                              
026200                                                                          
026300     IF IMS-NOT-OK                                                        
026400       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
026500                                      TO ERROR-TEXT-STR                   
026600       DISPLAY ERROR-TEXT-STR                                             
026700       CALL FELLOG                                                        
026800     END-IF                                                               
026900     .                                                                    
027000     EJECT                                                                
027010 IMS-LAS-ATERSTART SECTION.                                               
027020     MOVE 'IMS-LAS-ATERSTART    ' TO DBS-SECTION                          
027030                                                                          
027040     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
027050                    DELIMITED BY SIZE INTO SSA1                           
027060     MOVE 'WDR470 '        TO SSA2                                        
027070     MOVE '  GE'           TO GOOD-STATUSCODES                            
027080     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-WDGX4580 SSA1 SSA2            
027090     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
027091     PERFORM IMS-STATUSCHECK                                              
027092     .                                                                    
027093                                                                          
027118 IMS-REPL-ATERSTART SECTION.                                              
027119     MOVE 'IMS-REPL-ATERSTART   ' TO DBS-SECTION                          
027120                                                                          
027121     MOVE '  '             TO GOOD-STATUSCODES                            
027122     CALL CBLTDLI USING REPL WDR4-PCB DLI-IO-WDGX4580                     
027123     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
027124     PERFORM IMS-STATUSCHECK                                              
027125     .                                                                    
027126                                                                          
027127     EJECT                                                                
027130 IMS-STATUSCHECK SECTION.                                                 
027200     SKIP2                                                                
027300     SET STATUS-IX TO 1                                                   
027400     SEARCH GOOD-STATUS                                                   
027500       AT END                                                             
027600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027700           DELIMITED BY SIZE INTO ERROR-TEXT                              
027800         DISPLAY ERROR-TEXT                                               
027900         CALL FELLOG                                                      
028000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
028100         CONTINUE                                                         
028200     END-SEARCH                                                           
028300     .                                                                    

000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1611010.                                                 
000500 AUTHOR.        LEIF LJUNGKVIST.                                          
000600 DATE-WRITTEN.  OCTOBER 1993                                              
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * MAIN ADM VIOS                                                        
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*        EXTRACT                                                          
001500*                                                                         
001600*  * SUBPROGRAMS:                                                         
001700*        V16162 V16163 V16164 V16165                                      
001800*        V16170 V16193 V16194                                             
001900*                                                                         
002000*  * RETURNCODES:                                                         
002100*                                                                         
002200*        16 - SEVERE FILE-ERROR                                           
002300*        20 - SEVERE DB2-ERROR                                            
002400*                                                                         
002500***************************************************************           
002600     EJECT                                                                
002700***************************************************************           
002800 ENVIRONMENT DIVISION.                                                    
002900***************************************************************           
003000     SKIP2                                                                
003100*--------------------------------------------------------------           
003200 CONFIGURATION SECTION.                                                   
003300*--------------------------------------------------------------           
003400 SOURCE-COMPUTER. IBM-370.                                                
003500*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003600     SKIP2                                                                
003700***************************************************************           
003800 DATA DIVISION.                                                           
003900***************************************************************           
004000     SKIP2                                                                
004100*--------------------------------------------------------------           
004200 WORKING-STORAGE SECTION.                                                 
004300*--------------------------------------------------------------           
004400 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616000'.             
004500 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004600 01  RETURN-CODES.                                                        
004700     03  SQLCODE-1                PIC S9(9) COMP SYNC VALUE ZERO.         
004800     03  SQLCODE-2                PIC S9(9) COMP SYNC VALUE ZERO.         
004900     03  SQLCODE-3                PIC S9(9) COMP SYNC VALUE ZERO.         
005000     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
005100     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
005200     03  RCODE-7                  PIC S9(4) COMP SYNC VALUE 7.            
005300     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005400     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005500     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005600     03  RCODE-DISPL              PIC Z(4)-.                              
005700     SKIP2                                                                
005800 01  GENERAL-CONSTANTS.                                                   
005900     03 YES                       PIC X(1)  VALUE 'Y'.                    
006000     03 NOO                       PIC X(1)  VALUE 'N'.                    
006100     03 AAA                       PIC X(1)  VALUE 'A'.                    
006200     SKIP2                                                                
006300 01  WS-VARIABLER.                                                        
006400     03 WS-NULL-IND1              PIC S9(4) COMP.                         
006500     SKIP2                                                                
006600 01  SWITCHES.                                                            
006700     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
006800     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
006900     03 SW-EXTRACT-LISTED         PIC X(1)  VALUE 'N'.                    
007000     03 SW-EQUAL                  PIC X(1)  VALUE 'N'.                    
007100     03 SW-LIKE                   PIC X(1)  VALUE 'N'.                    
007200     SKIP2                                                                
007300 01  W-AREAS.                                                             
007400     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
007500     03 W-MAIN-EXTRACT            PIC X(8)  VALUE SPACE.                  
007600     03 W-MAIN-RESTR              PIC X(1)  VALUE SPACE.                  
007700     03 W-EXTRACTID               PIC X(8)  VALUE SPACE.                  
007800     03 FILLER REDEFINES W-EXTRACTID.                                     
007900       05  W-EXTR-POS             PIC X(1)  OCCURS 8                      
008000           INDEXED BY IX-EXTR.                                            
008100     03 W-EXTR-MAX                PIC S9(4) COMP VALUE +8.                
008200                                                                          
008300     03 W-ZZLCMD                  PIC X(1)  VALUE SPACE.                  
008400     03 W-EMPTY-PANEL             PIC X(8)  VALUE SPACE.                  
008500     03 W-COUNTER                 PIC S9(6) VALUE ZERO COMP.              
008600     03 W-NEW-EXTRACT             PIC X(8)  VALUE SPACE.                  
008700     03 W-DESCR                   PIC X(25) VALUE SPACE.                  
008800     SKIP2                                                                
008900 01  N-GENERAL.                                                           
009000     03 N-EXTRACTID               PIC X(8)  VALUE 'XTRACTID'.             
009100     03 N-EXTTYPE                 PIC X(8)  VALUE 'XTRTYPE '.             
009200     03 N-EXTDESCR                PIC X(8)  VALUE 'XTRDESCR'.             
009300     03 N-EXTCOPYT                PIC X(8)  VALUE 'XTRCOPYT'.             
009400     03 N-EXTSIZE                 PIC X(8)  VALUE 'XTRSIZE '.             
009500     03 N-EXTDAYS                 PIC X(8)  VALUE 'XTRDAYS '.             
009600     03 N-EXTFILE                 PIC X(8)  VALUE 'XTRFILE '.             
009700     03 N-EXTJOB                  PIC X(8)  VALUE 'XTRJOB  '.             
009800     03 N-EXTJOBTY                PIC X(8)  VALUE 'XTRJOBTY'.             
009900     03 N-EXTOWNER                PIC X(8)  VALUE 'XTROWNER'.             
010000     03 N-EXTRESTR                PIC X(8)  VALUE 'XTRRESTR'.             
010100                                                                          
010200     SKIP2                                                                
010300 01  LTH-GENERAL.                                                         
010400     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
010500     03 LTH-EXTTYPE               PIC S9(6) VALUE 8  COMP.                
010600     03 LTH-EXTDESCR              PIC S9(6) VALUE 25 COMP.                
010700     03 LTH-EXTCOPYT              PIC S9(6) VALUE 8  COMP.                
010800     03 LTH-EXTSIZE               PIC S9(6) VALUE 5  COMP.                
010900     03 LTH-EXTDAYS               PIC S9(6) VALUE 3  COMP.                
011000     03 LTH-EXTFILE               PIC S9(6) VALUE 42 COMP.                
011100     03 LTH-EXTJOB                PIC S9(6) VALUE 52 COMP.                
011200     03 LTH-EXTJOBTY              PIC S9(6) VALUE 1  COMP.                
011300     03 LTH-EXTOWNER              PIC S9(6) VALUE 7  COMP.                
011400     03 LTH-EXTRESTR              PIC S9(6) VALUE 1  COMP.                
011500     SKIP2                                                                
011600 01  ISPF-TABLE.                                                          
011700     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
011800     03 ISPF-EXTTYPE              PIC X(8)  VALUE SPACE.                  
011900     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
012000     03 ISPF-EXTCOPYT             PIC X(08) VALUE ZERO.                   
012100     03 ISPF-EXTSIZE              PIC S9(5) VALUE ZERO.                   
012200     03 ISPF-EXTDAYS              PIC S9(3) VALUE ZERO.                   
012300     03 ISPF-EXTFILE              PIC X(42) VALUE ZERO.                   
012400     03 ISPF-EXTJOB               PIC X(52) VALUE ZERO.                   
012500     03 ISPF-EXTJOBTY             PIC X(01) VALUE ZERO.                   
012600     03 ISPF-EXTOWNER             PIC X(07) VALUE ZERO.                   
012700     03 ISPF-RESTRICTION          PIC X(1)  VALUE SPACE.                  
012800                                                                          
012900 01  DYNAMIC-SUBPROGRAMS.                                                 
013000     03 ISPLINK                   PIC X(8)  VALUE 'ISPLINK '.             
013100     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
013200     SKIP2                                                                
013300*--------------------------------------------------------------           
013400*ISPF-CONSTANTS                                                           
013500*--------------------------------------------------------------           
013600 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
013700 01  SELECTT                      PIC X(8)  VALUE 'SELECT  '.             
013800 01  ADDPOP                       PIC X(8)  VALUE 'ADDPOP  '.             
013900 01  REMPOP                       PIC X(8)  VALUE 'REMPOP  '.             
014000 01  CONTROLL                     PIC X(8)  VALUE 'CONTROL '.             
014100 01  SAVEE                        PIC X(8)  VALUE 'SAVE    '.             
014200 01  RESTOREE                     PIC X(8)  VALUE 'RESTORE '.             
014300 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
014400 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
014500 01  TBADD                        PIC X(8)  VALUE 'TBADD   '.             
014600 01  TBTOP                        PIC X(8)  VALUE 'TBTOP   '.             
014700 01  TBCREATE                     PIC X(8)  VALUE 'TBCREATE'.             
014800 01  TBDISPL                      PIC X(8)  VALUE 'TBDISPL '.             
014900 01  TBSKIP                       PIC X(8)  VALUE 'TBSKIP  '.             
015000 01  TABNOW                       PIC X(8)  VALUE 'NOWRITE '.             
015100 01  TABREP                       PIC X(8)  VALUE 'REPLACE '.             
015200 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
015300*--------------------------------------------------------------           
015400*DEKLARATION AV ISPF-FÄLT                                                 
015500*--------------------------------------------------------------           
015600 01  V161600M                     PIC X(8)   VALUE 'V1616000'.            
015700 01  V1616001                     PIC X(8)   VALUE 'V1616001'.            
015800 01  V1616002                     PIC X(8)   VALUE 'V1616002'.            
015900 01  V1616003                     PIC X(8)   VALUE 'V1616003'.            
016000     EJECT                                                                
016100 01  EXTRACTTAB.                                                          
016200     03 EXTRACTTAB-NAME           PIC X(8)  VALUE 'V1611010'.             
016300     03 EXTRACTTAB-KEY            PIC X(10)  VALUE                        
016400     '(XTRACTID)'.                                                        
016500     03 EXTRACTTAB-VAR.                                                   
016600       05 FILLER                  PIC X(43)  VALUE                        
016700     '(XTRDESCR  XTRSIZE XTRDAYS XTRFILE XTRRESTR'.                       
016800       05 FILLER                  PIC X(43)  VALUE                        
016900     ' XTRTYPE XTRJOB XTRJOBTY XTRCOPYT XTROWNER)'.                       
017000*-------------------------------- DB2 ERROR HANDLING                      
017100*    -COPY V161WS                                                         
017200*-------------------------------- DB2-AREAS                               
017300     EXEC SQL                                                             
017400          INCLUDE SQLCA                                                   
017500     END-EXEC.                                                            
017600     EXEC SQL                                                             
017700          INCLUDE EXTRACT                                                 
017800     END-EXEC.                                                            
017900*    -COPY EXTRACT -PRE EXTRACT-                                          
018000     EXEC SQL                                                             
018100          INCLUDE EXTUSER                                                 
018200     END-EXEC.                                                            
018300*    -COPY EXTUSER -PRE EXTUSER-                                          
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600                                                                          
018700 01  SW-EXTRACT-FOUND          PIC X(1).                                  
018800                                                                          
018900 01  PARM-CODE                 PIC X(1).                                  
019000                                                                          
019100*    -COPY V16110 -PRE PARM-                                              
019200                                                                          
019300*************************************************************             
019400 PROCEDURE DIVISION USING                                                 
019500     SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110.                            
019600*************************************************************             
019700                                                                          
019800     PERFORM A-INIT                                                       
019900                                                                          
020000     IF PARM-CODE = 'I'                                                   
020100        PERFORM  B-SEL-EXTRACT                                            
020200        PERFORM S01-TBTOP-ISPFTAB                                         
020300     ELSE                                                                 
020400         PERFORM S01-TBSKIP-ISPFTAB                                       
020500         IF RCODE = +0                                                    
020600            PERFORM C-INIT-PARM                                           
020700         ELSE                                                             
020800            MOVE HIGH-VALUE TO PARM-CODE                                  
020900        END-IF                                                            
021000     END-IF                                                               
021100     PERFORM Z-FINIT                                                      
021200     GOBACK                                                               
021300     CONTINUE.                                                            
021400                                                                          
021500*--------------------------------------------------------------           
021600 A-INIT SECTION.                                                          
021700*--------------------------------------------------------------           
021800     MOVE 'A-INIT V16161010        ' TO ABEND-SECTION                     
021900D    DISPLAY ABEND-SECTION                                                
022000     SKIP2                                                                
022100     MOVE NOO TO SW-ERROR                                                 
022200     PERFORM AA-VDEF-GENERAL                                              
022300     CONTINUE.                                                            
022400     EJECT                                                                
022500*--------------------------------------------------------------           
022600 AA-VDEF-GENERAL SECTION.                                                 
022700*--------------------------------------------------------------           
022800     MOVE 'AA-VDEF-GENERAL         ' TO ABEND-SECTION                     
022900D    DISPLAY ABEND-SECTION                                                
023000     SKIP2                                                                
023100     CALL ISPLINK USING VDEFINE N-EXTRACTID ISPF-EXTRACTID                
023200                         CHAR LTH-EXTRACTID                               
023300     CALL ISPLINK USING VDEFINE N-EXTTYPE   ISPF-EXTTYPE                  
023400                         CHAR LTH-EXTTYPE                                 
023500     CALL ISPLINK USING VDEFINE N-EXTDESCR  ISPF-DESCRIPTION              
023600                         CHAR LTH-EXTDESCR                                
023700     CALL ISPLINK USING VDEFINE N-EXTCOPYT  ISPF-EXTCOPYT                 
023800                         CHAR LTH-EXTCOPYT                                
023900     CALL ISPLINK USING VDEFINE N-EXTSIZE   ISPF-EXTSIZE                  
024000                         CHAR LTH-EXTSIZE                                 
024100     CALL ISPLINK USING VDEFINE N-EXTDAYS   ISPF-EXTDAYS                  
024200                         CHAR LTH-EXTDAYS                                 
024300     CALL ISPLINK USING VDEFINE N-EXTFILE   ISPF-EXTFILE                  
024400                         CHAR LTH-EXTFILE                                 
024500     CALL ISPLINK USING VDEFINE N-EXTJOB    ISPF-EXTJOB                   
024600                         CHAR LTH-EXTJOB                                  
024700     CALL ISPLINK USING VDEFINE N-EXTJOBTY  ISPF-EXTJOBTY                 
024800                         CHAR LTH-EXTJOBTY                                
024900     CALL ISPLINK USING VDEFINE N-EXTOWNER  ISPF-EXTOWNER                 
025000                         CHAR LTH-EXTOWNER                                
025100     CALL ISPLINK USING VDEFINE N-EXTRESTR  ISPF-RESTRICTION              
025200                         CHAR LTH-EXTRESTR                                
025300     CONTINUE.                                                            
025400     EJECT                                                                
025500*--------------------------------------------------------------           
025600 B-SEL-EXTRACT  SECTION.                                                  
025700*--------------------------------------------------------------           
025800     MOVE 'B-SEL-EXTARCT  ' TO ABEND-SECTION                              
025900D    DISPLAY ABEND-SECTION                                                
026000                                                                          
026100     PERFORM S01-TBCREATE-ISPFTAB                                         
026200     PERFORM  SQL-DECLARE-OPEN-EXTRACT                                    
026300     PERFORM  SQL-FETCH-EXTRACT                                           
026400     PERFORM UNTIL SQLCODE-1 NOT = +0                                     
026500        PERFORM BA-TEST-LIKE                                              
026600        IF SW-LIKE = YES                                                  
026700           PERFORM  SQL-DECLARE-OPEN-EXTRACT-CRS-2                        
026800           PERFORM  SQL-FETCH-EXTRACT-CRS-2                               
026900           PERFORM UNTIL SQLCODE-2 NOT = +0                               
027000              PERFORM  BB-ADD-TO-TABLE                                    
027100              PERFORM  SQL-FETCH-EXTRACT-CRS-2                            
027200           END-PERFORM                                                    
027300           PERFORM SQL-CLOSE-EXTRACT-CRS-2                                
027400                                                                          
027500           PERFORM  SQL-DECLARE-OPEN-EXTRACT-CRS-3                        
027600           PERFORM  SQL-FETCH-EXTRACT-CRS-3                               
027700           PERFORM UNTIL SQLCODE-3 NOT = +0                               
027800              PERFORM  BB-ADD-TO-TABLE                                    
027900              PERFORM  SQL-FETCH-EXTRACT-CRS-3                            
028000           END-PERFORM                                                    
028100           PERFORM SQL-CLOSE-EXTRACT-CRS-3                                
028200        ELSE                                                              
028300           PERFORM  BB-ADD-TO-TABLE                                       
028400        END-IF                                                            
028500        PERFORM  SQL-FETCH-EXTRACT                                        
028600     END-PERFORM                                                          
028700     PERFORM SQL-CLOSE-EXTRACT                                            
028800     PERFORM SQL-COMMIT                                                   
028900     CONTINUE.                                                            
029000     EJECT                                                                
029100*--------------------------------------------------------------           
029200 BA-TEST-LIKE    SECTION.                                                 
029300*--------------------------------------------------------------           
029400     MOVE 'BA-TEST-LIKE  ' TO ABEND-SECTION                               
029500D    DISPLAY ABEND-SECTION                                                
029600                                                                          
029700     MOVE EXTRACT-EXTRACTID TO W-EXTRACTID                                
029800     MOVE EXTRACT-EXTRACTID TO W-MAIN-EXTRACT                             
029900     MOVE NOO TO SW-LIKE                                                  
030000     SET IX-EXTR TO W-EXTR-MAX                                            
030100     PERFORM UNTIL IX-EXTR = +0                                           
030200                                                                          
030300      IF W-EXTR-POS (IX-EXTR) = '*'                                       
030400         MOVE YES TO SW-LIKE                                              
030500         PERFORM UNTIL IX-EXTR > W-EXTR-MAX                               
030600           MOVE '%' TO W-EXTR-POS (IX-EXTR)                               
030700           SET IX-EXTR UP BY +1                                           
030800         END-PERFORM                                                      
030900         SET IX-EXTR TO +1                                                
031000       ELSE                                                               
031100         IF W-EXTR-POS(IX-EXTR) NOT = SPACE                               
031200            IF W-EXTR-POS(IX-EXTR) NOT = '*'                              
031300               SET IX-EXTR TO +1                                          
031400            END-IF                                                        
031500         END-IF                                                           
031600       END-IF                                                             
031700                                                                          
031800       SET IX-EXTR DOWN BY +1                                             
031900                                                                          
032000     END-PERFORM                                                          
032100                                                                          
032200     CONTINUE.                                                            
032300     EJECT                                                                
032400*--------------------------------------------------------------           
032500 BB-ADD-TO-TABLE SECTION.                                                 
032600*--------------------------------------------------------------           
032700     MOVE 'BB-ADD-TO-TABLE ' TO ABEND-SECTION                             
032800D    DISPLAY ABEND-SECTION                                                
032900                                                                          
033000     MOVE EXTRACT-EXTRACTID     TO ISPF-EXTRACTID                         
033100     MOVE EXTRACT-DESCRIPTION   TO ISPF-DESCRIPTION                       
033200     MOVE EXTRACT-SIZEX         TO ISPF-EXTSIZE                           
033300     MOVE EXTRACT-VALID-NO-DAYS TO ISPF-EXTDAYS                           
033400     MOVE EXTRACT-EXTRACT-FILE  TO ISPF-EXTFILE                           
033500     MOVE EXTRACT-TYPEX         TO ISPF-EXTTYPE                           
033600     MOVE EXTRACT-COPYTEXT      TO ISPF-EXTCOPYT                          
033700     MOVE EXTRACT-EXTRACT-JOB   TO ISPF-EXTJOB                            
033800     MOVE EXTRACT-JOB-TYPE      TO ISPF-EXTJOBTY                          
033900     MOVE EXTRACT-OWNER         TO ISPF-EXTOWNER                          
034000     MOVE W-MAIN-RESTR          TO ISPF-RESTRICTION                       
034100                                                                          
034200     PERFORM S01-TBADD-ISPFTAB                                            
034300                                                                          
034400     CONTINUE.                                                            
034500     EJECT                                                                
034600*--------------------------------------------------------------           
034700 C-INIT-PARM  SECTION.                                                    
034800*--------------------------------------------------------------           
034900     MOVE 'C-INIT-PARM     ' TO ABEND-SECTION                             
035000D    DISPLAY ABEND-SECTION                                                
035100                                                                          
035200     MOVE ISPF-EXTRACTID   TO PARM-EXTRACTID                              
035300     MOVE ISPF-DESCRIPTION TO PARM-DESCRIPTION                            
035400     MOVE ISPF-EXTSIZE     TO PARM-SIZE                                   
035500     MOVE ISPF-EXTDAYS     TO PARM-VALID-NO-DAYS                          
035600     MOVE ISPF-EXTFILE     TO PARM-EXTRACT-FILE                           
035700     MOVE ISPF-EXTTYPE     TO PARM-TYPE                                   
035800     MOVE ISPF-EXTCOPYT    TO PARM-COPYTEXT                               
035900     MOVE ISPF-EXTJOB      TO PARM-EXTRACT-JOB                            
036000     MOVE ISPF-EXTJOBTY    TO PARM-JOB-TYPE                               
036100     MOVE ISPF-EXTOWNER    TO PARM-OWNER                                  
036200     MOVE ISPF-RESTRICTION TO PARM-AUTH                                   
036300                                                                          
036400     CONTINUE.                                                            
036500     EJECT                                                                
036600*--------------------------------------------------------------           
036700 Z-FINIT SECTION.                                                         
036800*--------------------------------------------------------------           
036900     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
037000D    DISPLAY ABEND-SECTION                                                
037100     SKIP2                                                                
037200     MOVE RCODE TO RETURN-CODE                                            
037300     CONTINUE.                                                            
037400     EJECT                                                                
037500*--------------------------------------------------------------           
037600 S01-TBCREATE-ISPFTAB SECTION.                                            
037700*--------------------------------------------------------------           
037800     MOVE 'S01-TBCREATE                ' TO ABEND-SECTION                 
037900D    DISPLAY ABEND-SECTION                                                
038000     SKIP2                                                                
038100     CALL 'ISPLINK' USING TBCREATE                                        
038200                          EXTRACTTAB-NAME                                 
038300                          EXTRACTTAB-KEY                                  
038400                          EXTRACTTAB-VAR                                  
038500                          TABNOW                                          
038600                          TABREP                                          
038700     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
038800     IF RCODE <= +8                                                       
038900        MOVE NOO TO SW-ERROR                                              
039000     ELSE                                                                 
039100        MOVE YES TO SW-ERROR                                              
039200        PERFORM S99-ERROR-ROUTINE                                         
039300        MOVE RCODE-16 TO RCODE                                            
039400     END-IF                                                               
039500     CONTINUE.                                                            
039600     EJECT                                                                
039700*--------------------------------------------------------------           
039800 S01-TBADD-ISPFTAB SECTION.                                               
039900*--------------------------------------------------------------           
040000     MOVE 'S01-TBADD-ISPFTAB           ' TO ABEND-SECTION                 
040100D    DISPLAY ABEND-SECTION                                                
040200     SKIP2                                                                
040300     CALL 'ISPLINK' USING TBADD                                           
040400                          EXTRACTTAB-NAME                                 
040500     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
040600     IF RCODE <= +8                                                       
040700        MOVE NOO TO SW-ERROR                                              
040800     ELSE                                                                 
040900        MOVE YES TO SW-ERROR                                              
041000        PERFORM S99-ERROR-ROUTINE                                         
041100        MOVE RCODE-16 TO RCODE                                            
041200     END-IF                                                               
041300                                                                          
041400     CONTINUE.                                                            
041500     EJECT                                                                
041600*--------------------------------------------------------------           
041700 S01-TBSKIP-ISPFTAB SECTION.                                              
041800*--------------------------------------------------------------           
041900     MOVE 'S01-TBSKIP-ISPFTAB        ' TO ABEND-SECTION                   
042000D    DISPLAY ABEND-SECTION                                                
042100     SKIP2                                                                
042200     CALL 'ISPLINK' USING TBSKIP                                          
042300                          EXTRACTTAB-NAME                                 
042400     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
042500                                                                          
042600     IF RCODE <= +8                                                       
042700        MOVE NOO TO SW-ERROR                                              
042800     ELSE                                                                 
042900        MOVE YES TO SW-ERROR                                              
043000        PERFORM S99-ERROR-ROUTINE                                         
043100        MOVE RCODE-16 TO RCODE                                            
043200     END-IF                                                               
043300                                                                          
043400                                                                          
043500     CONTINUE.                                                            
043600     EJECT                                                                
043700*--------------------------------------------------------------           
043800 S01-TBTOP-ISPFTAB SECTION.                                               
043900*--------------------------------------------------------------           
044000     MOVE 'S01-TBTOP-ISPFTAB           ' TO ABEND-SECTION                 
044100D    DISPLAY ABEND-SECTION                                                
044200     SKIP2                                                                
044300     CALL 'ISPLINK' USING TBTOP                                           
044400                          EXTRACTTAB-NAME                                 
044500     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
044600     IF RCODE = +0                                                        
044700        MOVE NOO TO SW-ERROR                                              
044800     ELSE                                                                 
044900        MOVE YES TO SW-ERROR                                              
045000        PERFORM S99-ERROR-ROUTINE                                         
045100        MOVE RCODE-16 TO RCODE                                            
045200     END-IF                                                               
045300     CONTINUE.                                                            
045400     EJECT                                                                
045500*--------------------------------------------------------------           
045600*--------------------------------------------------------------           
045700 S99-ERROR-ROUTINE SECTION.                                               
045800*--------------------------------------------------------------           
045900     SKIP2                                                                
046000     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
046100     RCODE-DISPL DELIMITED BY SIZE                                        
046200     INTO W-ERROR-MESSAGE                                                 
046300     DISPLAY W-ERROR-MESSAGE                                              
046400     CONTINUE.                                                            
046500     EJECT                                                                
046600*--------------------------------------------------------------           
046700 SQL-DECLARE-OPEN-EXTRACT SECTION.                                        
046800*--------------------------------------------------------------           
046900     MOVE 'SQL-DECLARE-OPEN-EXTRACT-1  ' TO ABEND-SECTION                 
047000D    DISPLAY ABEND-SECTION                                                
047100                                                                          
047200     EXEC SQL                                                             
047300         DECLARE CRS-EXTRACT-1 CURSOR FOR                                 
047400         SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                       
047500              A.VALID_NO_DAYS, A.EXTRACT_FILE,                            
047600              A.TYPE, A.EXTRACT_JOB,                                      
047700              A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                     
047800         FROM EXTRACT A, EXTRACT_USER B                                   
047900         WHERE A.EXTRACTID IN                                             
048000         (SELECT EXTRACTID FROM EXTRACT_USER                              
048100          WHERE USERID = USER )                                           
048200          AND A.EXTRACTID = B.EXTRACTID                                   
048300          AND B.USERID    = USER                                          
048400          ORDER BY  EXTRACTID                                             
048500     END-EXEC.                                                            
048600     EXEC SQL                                                             
048700         OPEN CRS-EXTRACT-1                                               
048800     END-EXEC                                                             
048900     PERFORM S95-CONTROL-SQLCODE                                          
049000*         B.EXTRACTID LIKE '%*%%%%%%' AND USERID = USER )                 
049100     CONTINUE.                                                            
049200     EJECT                                                                
049300*--------------------------------------------------------------           
049400 SQL-FETCH-EXTRACT SECTION.                                               
049500*--------------------------------------------------------------           
049600     MOVE 'SQL-FETCH-EXTRACT           ' TO ABEND-SECTION                 
049700D    DISPLAY ABEND-SECTION                                                
049800     SKIP2                                                                
049900     EXEC SQL                                                             
050000     FETCH CRS-EXTRACT-1 INTO :EXTRACT-EXTRACTID,                         
050100                              :EXTRACT-DESCRIPTION,                       
050200                              :EXTRACT-SIZEX,                             
050300                              :EXTRACT-VALID-NO-DAYS,                     
050400                              :EXTRACT-EXTRACT-FILE,                      
050500                              :EXTRACT-TYPEX,                             
050600                              :EXTRACT-EXTRACT-JOB,                       
050700                              :EXTRACT-JOB-TYPE,                          
050800                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
050900                              :EXTRACT-OWNER,                             
051000                              :W-MAIN-RESTR                               
051100     END-EXEC                                                             
051200     MOVE SQLCODE TO SQLCODE-1                                            
051300     MOVE SQLCODE TO RCODE-DISPL                                          
051400     PERFORM S95-CONTROL-SQLCODE                                          
051500     IF SQLCODE = +000 THEN                                               
051600       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
051700         MOVE '-' TO EXTRACT-COPYTEXT                                     
051800       END-IF                                                             
051900       MOVE YES  TO SW-EXTRACT-FOUND                                      
052000     ELSE                                                                 
052100       MOVE NOO  TO SW-EXTRACT-FOUND                                      
052200     END-IF                                                               
052300     CONTINUE.                                                            
052400     EJECT                                                                
052500*--------------------------------------------------------------           
052600 SQL-CLOSE-EXTRACT SECTION.                                               
052700*--------------------------------------------------------------           
052800     MOVE 'SQL-CLOSE-EXTRACT           ' TO ABEND-SECTION                 
052900     SKIP2                                                                
053000                                                                          
053100     EXEC SQL                                                             
053200         CLOSE CRS-EXTRACT-1                                              
053300     END-EXEC                                                             
053400     PERFORM S95-CONTROL-SQLCODE                                          
053500     CONTINUE.                                                            
053600     EJECT                                                                
053700*--------------------------------------------------------------           
053800 SQL-DECLARE-OPEN-EXTRACT-CRS-2 SECTION.                                  
053900*--------------------------------------------------------------           
054000     MOVE 'SQL-DECLARE-OPEN-EXTRACT-CRS-2  ' TO ABEND-SECTION             
054100D    DISPLAY ABEND-SECTION                                                
054200                                                                          
054300D    DISPLAY 'CRS2'                                                       
054400     EXEC SQL                                                             
054500                                                                          
054600        DECLARE CRS-EXTRACT-2 CURSOR FOR                                  
054700                                                                          
054800        SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                        
054900               A.VALID_NO_DAYS, A.EXTRACT_FILE,                           
055000               A.TYPE, A.EXTRACT_JOB,                                     
055100               A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                    
055200        FROM EXTRACT A, EXTRACT_USER B                                    
055300        WHERE A.EXTRACTID IN                                              
055400        (SELECT EXTRACTID FROM EXTRACT_USER B WHERE                       
055500         B.EXTRACTID LIKE :W-EXTRACTID AND USERID = USER )                
055600         AND A.EXTRACTID ^= :W-MAIN-EXTRACT                               
055700         AND B.USERID     = USER                                          
055800         AND A.EXTRACTID = B.EXTRACTID                                    
055900         ORDER BY  EXTRACTID                                              
056000                                                                          
056100     END-EXEC.                                                            
056200     EXEC SQL                                                             
056300         OPEN CRS-EXTRACT-2                                               
056400     END-EXEC                                                             
056500     PERFORM S95-CONTROL-SQLCODE                                          
056600     CONTINUE.                                                            
056700     EJECT                                                                
056800*--------------------------------------------------------------           
056900 SQL-FETCH-EXTRACT-CRS-2 SECTION.                                         
057000*--------------------------------------------------------------           
057100     MOVE 'SQL-FETCH-EXTRACT-CRS-2      ' TO ABEND-SECTION                
057200D    DISPLAY ABEND-SECTION                                                
057300     SKIP2                                                                
057400     EXEC SQL                                                             
057500     FETCH CRS-EXTRACT-2 INTO :EXTRACT-EXTRACTID,                         
057600                              :EXTRACT-DESCRIPTION,                       
057700                              :EXTRACT-SIZEX,                             
057800                              :EXTRACT-VALID-NO-DAYS,                     
057900                              :EXTRACT-EXTRACT-FILE,                      
058000                              :EXTRACT-TYPEX,                             
058100                              :EXTRACT-EXTRACT-JOB,                       
058200                              :EXTRACT-JOB-TYPE,                          
058300                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
058400                              :EXTRACT-OWNER,                             
058500                              :W-MAIN-RESTR                               
058600     END-EXEC                                                             
058700                                                                          
058800     MOVE SQLCODE TO SQLCODE-2                                            
058900     MOVE SQLCODE TO RCODE-DISPL                                          
059000     PERFORM S95-CONTROL-SQLCODE                                          
059100     IF SQLCODE = +000 THEN                                               
059200       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
059300         MOVE '-' TO EXTRACT-COPYTEXT                                     
059400       END-IF                                                             
059500     END-IF                                                               
059600     CONTINUE.                                                            
059700     EJECT                                                                
059800*--------------------------------------------------------------           
059900 SQL-CLOSE-EXTRACT-CRS-2 SECTION.                                         
060000*--------------------------------------------------------------           
060100     MOVE 'SQL-CLOSE-EXTRACT-CRS-2     ' TO ABEND-SECTION                 
060200     SKIP2                                                                
060300                                                                          
060400     EXEC SQL                                                             
060500         CLOSE CRS-EXTRACT-2                                              
060600     END-EXEC                                                             
060700     PERFORM S95-CONTROL-SQLCODE                                          
060800     CONTINUE.                                                            
060900*--------------------------------------------------------------           
061000 SQL-DECLARE-OPEN-EXTRACT-CRS-3 SECTION.                                  
061100*--------------------------------------------------------------           
061200     MOVE 'SQL-DECLARE-OPEN-EXTRACT-CRS-3' TO ABEND-SECTION               
061300D    DISPLAY ABEND-SECTION                                                
061400                                                                          
061500D    DISPLAY 'CRS3'                                                       
061600     EXEC SQL                                                             
061700                                                                          
061800        DECLARE CRS-EXTRACT-3 CURSOR FOR                                  
061900        SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                        
062000               A.VALID_NO_DAYS, A.EXTRACT_FILE,                           
062100               A.TYPE, A.EXTRACT_JOB,                                     
062200               A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                    
062300        FROM EXTRACT A, EXTRACT_USER B                                    
062400        WHERE A.EXTRACTID LIKE :W-EXTRACTID                               
062500          AND B.EXTRACTID  = :W-MAIN-EXTRACT                              
062600          AND USERID = USER                                               
062700          AND A.EXTRACTID ^= :W-MAIN-EXTRACT                              
062800         ORDER BY  EXTRACTID                                              
062900     END-EXEC.                                                            
063000     EXEC SQL                                                             
063100         OPEN CRS-EXTRACT-3                                               
063200     END-EXEC                                                             
063300     PERFORM S95-CONTROL-SQLCODE                                          
063400                                                                          
063500     CONTINUE.                                                            
063600     EJECT                                                                
063700*--------------------------------------------------------------           
063800 SQL-FETCH-EXTRACT-CRS-3 SECTION.                                         
063900*--------------------------------------------------------------           
064000     MOVE 'SQL-FETCH-EXTRACT-CRS-3     ' TO ABEND-SECTION                 
064100D    DISPLAY ABEND-SECTION                                                
064200     SKIP2                                                                
064300     EXEC SQL                                                             
064400     FETCH CRS-EXTRACT-3 INTO :EXTRACT-EXTRACTID,                         
064500                              :EXTRACT-DESCRIPTION,                       
064600                              :EXTRACT-SIZEX,                             
064700                              :EXTRACT-VALID-NO-DAYS,                     
064800                              :EXTRACT-EXTRACT-FILE,                      
064900                              :EXTRACT-TYPEX,                             
065000                              :EXTRACT-EXTRACT-JOB,                       
065100                              :EXTRACT-JOB-TYPE,                          
065200                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
065300                              :EXTRACT-OWNER,                             
065400                              :W-MAIN-RESTR                               
065500     END-EXEC                                                             
065600                                                                          
065700     MOVE SQLCODE TO SQLCODE-3                                            
065800     MOVE SQLCODE TO RCODE-DISPL                                          
065900     PERFORM S95-CONTROL-SQLCODE                                          
066000     IF SQLCODE = +000 THEN                                               
066100       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
066200         MOVE '-' TO EXTRACT-COPYTEXT                                     
066300       END-IF                                                             
066400     END-IF                                                               
066500     CONTINUE.                                                            
066600     EJECT                                                                
066700*--------------------------------------------------------------           
066800 SQL-CLOSE-EXTRACT-CRS-3 SECTION.                                         
066900*--------------------------------------------------------------           
067000     MOVE 'SQL-CLOSE-EXTRACT-CRS-3  ' TO ABEND-SECTION                    
067100     SKIP2                                                                
067200                                                                          
067300     EXEC SQL                                                             
067400         CLOSE CRS-EXTRACT-3                                              
067500     END-EXEC                                                             
067600     PERFORM S95-CONTROL-SQLCODE                                          
067700     CONTINUE.                                                            
067800*--------------------------------------------------------------           
067900 SQL-COMMIT SECTION.                                                      
068000*--------------------------------------------------------------           
068100     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
068200D    DISPLAY ABEND-SECTION                                                
068300     SKIP2                                                                
068400     EXEC SQL                                                             
068500         COMMIT                                                           
068600     END-EXEC                                                             
068700     PERFORM S95-CONTROL-SQLCODE                                          
068800     CONTINUE.                                                            
068900     EJECT                                                                
069000*-------------------------------- DB2 ERROR HANDLING                      
069100*    -COPY V161PS                                                         
069200*++INCLUDE V161PS                                                         

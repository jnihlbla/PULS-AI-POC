000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1615110.                                                 
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
001600*                                                                         
001700*  * RETURNCODES:                                                         
001800*                                                                         
001900*        16 - SEVERE FILE-ERROR                                           
002000*        20 - SEVERE DB2-ERROR                                            
002100*                                                                         
002200***************************************************************           
002300     EJECT                                                                
002400***************************************************************           
002500 ENVIRONMENT DIVISION.                                                    
002600***************************************************************           
002700     SKIP2                                                                
002800*--------------------------------------------------------------           
002900 CONFIGURATION SECTION.                                                   
003000*--------------------------------------------------------------           
003100 SOURCE-COMPUTER. IBM-370.                                                
003200*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003300     SKIP2                                                                
003400***************************************************************           
003500 DATA DIVISION.                                                           
003600***************************************************************           
003700     SKIP2                                                                
003800*--------------------------------------------------------------           
003900 WORKING-STORAGE SECTION.                                                 
004000*--------------------------------------------------------------           
004100 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616000'.             
004200 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004300 01  RETURN-CODES.                                                        
004400     03  SQLCODE-1                PIC S9(9) COMP SYNC VALUE ZERO.         
004500     03  SQLCODE-2                PIC S9(9) COMP SYNC VALUE ZERO.         
004600     03  SQLCODE-3                PIC S9(9) COMP SYNC VALUE ZERO.         
004700     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
004800     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
004900     03  RCODE-7                  PIC S9(4) COMP SYNC VALUE 7.            
005000     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005100     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005200     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005300     03  RCODE-DISPL              PIC Z(4)-.                              
005400     SKIP2                                                                
005500 01  GENERAL-CONSTANTS.                                                   
005600     03 YES                       PIC X(1)  VALUE 'Y'.                    
005700     03 NOO                       PIC X(1)  VALUE 'N'.                    
005800     03 AAA                       PIC X(1)  VALUE 'A'.                    
005900     SKIP2                                                                
006000 01  WS-VARIABLER.                                                        
006100     03 WS-NULL-IND1              PIC S9(4) COMP.                         
006200     SKIP2                                                                
006300 01  SWITCHES.                                                            
006400     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
006500     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
006600     03 SW-EXTRACT-LISTED         PIC X(1)  VALUE 'N'.                    
006700     03 SW-EQUAL                  PIC X(1)  VALUE 'N'.                    
006800     03 SW-LIKE                   PIC X(1)  VALUE 'N'.                    
006900     SKIP2                                                                
007000 01  W-AREAS.                                                             
007100     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
007200     03 W-MAIN-EXTRACT            PIC X(8)  VALUE SPACE.                  
007300     03 W-MAIN-RESTR              PIC X(1)  VALUE SPACE.                  
007400     03 W-EXTRACTID               PIC X(8)  VALUE SPACE.                  
007500     03 FILLER REDEFINES W-EXTRACTID.                                     
007600       05  W-EXTR-POS             PIC X(1)  OCCURS 8                      
007700           INDEXED BY IX-EXTR.                                            
007800     03 W-EXTR-MAX                PIC S9(4) COMP VALUE +8.                
007900                                                                          
008000     03 W-ZZLCMD                  PIC X(1)  VALUE SPACE.                  
008100     03 W-EMPTY-PANEL             PIC X(8)  VALUE SPACE.                  
008200     03 W-COUNTER                 PIC S9(6) VALUE ZERO COMP.              
008300     03 W-NEW-EXTRACT             PIC X(8)  VALUE SPACE.                  
008400     03 W-DESCR                   PIC X(25) VALUE SPACE.                  
008500     SKIP2                                                                
008600 01  N-GENERAL.                                                           
008700     03 N-EXTRACTID               PIC X(8)  VALUE 'XTRACTID'.             
008800     03 N-EXTTYPE                 PIC X(8)  VALUE 'XTRTYPE '.             
008900     03 N-EXTDESCR                PIC X(8)  VALUE 'XTRDESCR'.             
009000     03 N-EXTCOPYT                PIC X(8)  VALUE 'XTRCOPYT'.             
009100     03 N-EXTSIZE                 PIC X(8)  VALUE 'XTRSIZE '.             
009200     03 N-EXTDAYS                 PIC X(8)  VALUE 'XTRDAYS '.             
009300     03 N-EXTFILE                 PIC X(8)  VALUE 'XTRFILE '.             
009400     03 N-EXTJOB                  PIC X(8)  VALUE 'XTRJOB  '.             
009500     03 N-EXTJOBTY                PIC X(8)  VALUE 'XTRJOBTY'.             
009600     03 N-EXTOWNER                PIC X(8)  VALUE 'XTROWNER'.             
009700     03 N-EXTRESTR                PIC X(8)  VALUE 'XTRRESTR'.             
009800                                                                          
009900     SKIP2                                                                
010000 01  LTH-GENERAL.                                                         
010100     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
010200     03 LTH-EXTTYPE               PIC S9(6) VALUE 8  COMP.                
010300     03 LTH-EXTDESCR              PIC S9(6) VALUE 25 COMP.                
010400     03 LTH-EXTCOPYT              PIC S9(6) VALUE 8  COMP.                
010500     03 LTH-EXTSIZE               PIC S9(6) VALUE 5  COMP.                
010600     03 LTH-EXTDAYS               PIC S9(6) VALUE 3  COMP.                
010700     03 LTH-EXTFILE               PIC S9(6) VALUE 42 COMP.                
010800     03 LTH-EXTJOB                PIC S9(6) VALUE 52 COMP.                
010900     03 LTH-EXTJOBTY              PIC S9(6) VALUE 1  COMP.                
011000     03 LTH-EXTOWNER              PIC S9(6) VALUE 7  COMP.                
011100     03 LTH-EXTRESTR              PIC S9(6) VALUE 1  COMP.                
011200     SKIP2                                                                
011300 01  ISPF-TABLE.                                                          
011400     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
011500     03 ISPF-EXTTYPE              PIC X(8)  VALUE SPACE.                  
011600     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
011700     03 ISPF-EXTCOPYT             PIC X(08) VALUE ZERO.                   
011800     03 ISPF-EXTSIZE              PIC S9(5) VALUE ZERO.                   
011900     03 ISPF-EXTDAYS              PIC S9(3) VALUE ZERO.                   
012000     03 ISPF-EXTFILE              PIC X(42) VALUE ZERO.                   
012100     03 ISPF-EXTJOB               PIC X(52) VALUE ZERO.                   
012200     03 ISPF-EXTJOBTY             PIC X(01) VALUE ZERO.                   
012300     03 ISPF-EXTOWNER             PIC X(07) VALUE ZERO.                   
012400     03 ISPF-RESTRICTION          PIC X(1)  VALUE SPACE.                  
012500                                                                          
012600 01  DYNAMIC-SUBPROGRAMS.                                                 
012700     03 ISPLINK                   PIC X(8)  VALUE 'ISPLINK '.             
012800     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
012900     SKIP2                                                                
013000*--------------------------------------------------------------           
013100*ISPF-CONSTANTS                                                           
013200*--------------------------------------------------------------           
013300 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
013400 01  SELECTT                      PIC X(8)  VALUE 'SELECT  '.             
013500 01  ADDPOP                       PIC X(8)  VALUE 'ADDPOP  '.             
013600 01  REMPOP                       PIC X(8)  VALUE 'REMPOP  '.             
013700 01  CONTROLL                     PIC X(8)  VALUE 'CONTROL '.             
013800 01  SAVEE                        PIC X(8)  VALUE 'SAVE    '.             
013900 01  RESTOREE                     PIC X(8)  VALUE 'RESTORE '.             
014000 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
014100 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
014200 01  TBADD                        PIC X(8)  VALUE 'TBADD   '.             
014300 01  TBTOP                        PIC X(8)  VALUE 'TBTOP   '.             
014400 01  TBCREATE                     PIC X(8)  VALUE 'TBCREATE'.             
014500 01  TBDISPL                      PIC X(8)  VALUE 'TBDISPL '.             
014600 01  TBSKIP                       PIC X(8)  VALUE 'TBSKIP  '.             
014700 01  TABNOW                       PIC X(8)  VALUE 'NOWRITE '.             
014800 01  TABREP                       PIC X(8)  VALUE 'REPLACE '.             
014900 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
015000*--------------------------------------------------------------           
015100*DEKLARATION AV ISPF-FÄLT                                                 
015200*--------------------------------------------------------------           
015300 01  V161600M                     PIC X(8)   VALUE 'V1616000'.            
015400 01  V1616001                     PIC X(8)   VALUE 'V1616001'.            
015500 01  V1616002                     PIC X(8)   VALUE 'V1616002'.            
015600 01  V1616003                     PIC X(8)   VALUE 'V1616003'.            
015700     EJECT                                                                
015800 01  EXTRACTTAB.                                                          
015900     03 EXTRACTTAB-NAME           PIC X(8)  VALUE 'V1611010'.             
016000     03 EXTRACTTAB-KEY            PIC X(10)  VALUE                        
016100     '(XTRACTID)'.                                                        
016200     03 EXTRACTTAB-VAR.                                                   
016300       05 FILLER                  PIC X(43)  VALUE                        
016400     '(XTRDESCR  XTRSIZE XTRDAYS XTRFILE XTRRESTR'.                       
016500       05 FILLER                  PIC X(43)  VALUE                        
016600     ' XTRTYPE XTRJOB XTRJOBTY XTRCOPYT XTROWNER)'.                       
016700*-------------------------------- DB2 ERROR HANDLING                      
016800*    -COPY V161WS                                                         
016900*++INCLUDE V161WS                                                         
017000*-------------------------------- DB2-AREAS                               
017100     EXEC SQL                                                             
017200          INCLUDE SQLCA                                                   
017300     END-EXEC.                                                            
017400     EXEC SQL                                                             
017500          INCLUDE EXTRACT                                                 
017600     END-EXEC.                                                            
017700*    -COPY EXTRACT -PRE EXTRACT-                                          
017800     EXEC SQL                                                             
017900          INCLUDE EXTUSER                                                 
018000     END-EXEC.                                                            
018100*    -COPY EXTUSER -PRE EXTUSER-                                          
018200     EJECT                                                                
018300 LINKAGE SECTION.                                                         
018400                                                                          
018500 01  SW-EXTRACT-FOUND          PIC X(1).                                  
018600                                                                          
018700 01  PARM-CODE                 PIC X(1).                                  
018800                                                                          
018900 01  PARM-USERID               PIC X(8).                                  
019000                                                                          
019100*    -COPY V16110CC -PRE PARM-                                            
019200*++INCLUDE  V16110CC                                                      
019300                                                                          
019400*************************************************************             
019500 PROCEDURE DIVISION USING                                                 
019600     SW-EXTRACT-FOUND, PARM-CODE, PARM-USERID, PARM-V16110.               
019700*************************************************************             
019800                                                                          
019900     PERFORM A-INIT                                                       
020000                                                                          
020100                                                                          
020200     IF PARM-CODE = 'I'                                                   
020300        PERFORM  B-SEL-EXTRACT                                            
020400        PERFORM S01-TBTOP-ISPFTAB                                         
020500     ELSE                                                                 
020600         PERFORM S01-TBSKIP-ISPFTAB                                       
020700         IF RCODE = +0                                                    
020800            PERFORM C-INIT-PARM                                           
020900         ELSE                                                             
021000            MOVE HIGH-VALUE TO PARM-CODE                                  
021100        END-IF                                                            
021200     END-IF                                                               
021300     PERFORM Z-FINIT                                                      
021400     GOBACK                                                               
021500     CONTINUE.                                                            
021600                                                                          
021700*--------------------------------------------------------------           
021800 A-INIT SECTION.                                                          
021900*--------------------------------------------------------------           
022000     MOVE 'A-INIT V16161010        ' TO ABEND-SECTION                     
022100D    DISPLAY ABEND-SECTION                                                
022200     SKIP2                                                                
022300     MOVE NOO TO SW-ERROR                                                 
022400     PERFORM AA-VDEF-GENERAL                                              
022500     CONTINUE.                                                            
022600     EJECT                                                                
022700*--------------------------------------------------------------           
022800 AA-VDEF-GENERAL SECTION.                                                 
022900*--------------------------------------------------------------           
023000     MOVE 'AA-VDEF-GENERAL         ' TO ABEND-SECTION                     
023100D    DISPLAY ABEND-SECTION                                                
023200     SKIP2                                                                
023300     CALL ISPLINK USING VDEFINE N-EXTRACTID ISPF-EXTRACTID                
023400                         CHAR LTH-EXTRACTID                               
023500     CALL ISPLINK USING VDEFINE N-EXTTYPE   ISPF-EXTTYPE                  
023600                         CHAR LTH-EXTTYPE                                 
023700     CALL ISPLINK USING VDEFINE N-EXTDESCR  ISPF-DESCRIPTION              
023800                         CHAR LTH-EXTDESCR                                
023900     CALL ISPLINK USING VDEFINE N-EXTCOPYT  ISPF-EXTCOPYT                 
024000                         CHAR LTH-EXTCOPYT                                
024100     CALL ISPLINK USING VDEFINE N-EXTSIZE   ISPF-EXTSIZE                  
024200                         CHAR LTH-EXTSIZE                                 
024300     CALL ISPLINK USING VDEFINE N-EXTDAYS   ISPF-EXTDAYS                  
024400                         CHAR LTH-EXTDAYS                                 
024500     CALL ISPLINK USING VDEFINE N-EXTFILE   ISPF-EXTFILE                  
024600                         CHAR LTH-EXTFILE                                 
024700     CALL ISPLINK USING VDEFINE N-EXTJOB    ISPF-EXTJOB                   
024800                         CHAR LTH-EXTJOB                                  
024900     CALL ISPLINK USING VDEFINE N-EXTJOBTY  ISPF-EXTJOBTY                 
025000                         CHAR LTH-EXTJOBTY                                
025100     CALL ISPLINK USING VDEFINE N-EXTOWNER  ISPF-EXTOWNER                 
025200                         CHAR LTH-EXTOWNER                                
025300     CALL ISPLINK USING VDEFINE N-EXTRESTR  ISPF-RESTRICTION              
025400                         CHAR LTH-EXTRESTR                                
025500     CONTINUE.                                                            
025600     EJECT                                                                
025700*--------------------------------------------------------------           
025800 B-SEL-EXTRACT  SECTION.                                                  
025900*--------------------------------------------------------------           
026000     MOVE 'B-SEL-EXTARCT  ' TO ABEND-SECTION                              
026100D    DISPLAY ABEND-SECTION                                                
026200                                                                          
026300     PERFORM S01-TBCREATE-ISPFTAB                                         
026400     PERFORM  SQL-DECLARE-OPEN-EXTRACT-1                                  
026500                                                                          
026600     PERFORM  SQL-FETCH-EXTRACT                                           
026700     PERFORM UNTIL SQLCODE-1 NOT = +0                                     
026800        PERFORM BA-TEST-LIKE                                              
026900        IF SW-LIKE = YES                                                  
027000           PERFORM  SQL-DECLARE-OPEN-EXTRACT-CRS-2                        
027100           PERFORM  SQL-FETCH-EXTRACT-CRS-2                               
027200           PERFORM UNTIL SQLCODE-2 NOT = +0                               
027300              PERFORM  BB-ADD-TO-TABLE                                    
027400              PERFORM  SQL-FETCH-EXTRACT-CRS-2                            
027500           END-PERFORM                                                    
027600           PERFORM SQL-CLOSE-EXTRACT-CRS-2                                
027700                                                                          
027800           PERFORM  SQL-DECLARE-OPEN-EXTRACT-CRS-3                        
027900           PERFORM  SQL-FETCH-EXTRACT-CRS-3                               
028000           PERFORM UNTIL SQLCODE-3 NOT = +0                               
028100              PERFORM  BB-ADD-TO-TABLE                                    
028200              PERFORM  SQL-FETCH-EXTRACT-CRS-3                            
028300           END-PERFORM                                                    
028400           PERFORM SQL-CLOSE-EXTRACT-CRS-3                                
028500        ELSE                                                              
028600           PERFORM  BB-ADD-TO-TABLE                                       
028700        END-IF                                                            
028800        PERFORM  SQL-FETCH-EXTRACT                                        
028900     END-PERFORM                                                          
029000     PERFORM SQL-CLOSE-EXTRACT                                            
029100     PERFORM SQL-COMMIT                                                   
029200     CONTINUE.                                                            
029300     EJECT                                                                
029400*--------------------------------------------------------------           
029500 BA-TEST-LIKE    SECTION.                                                 
029600*--------------------------------------------------------------           
029700     MOVE 'BA-TEST-LIKE  ' TO ABEND-SECTION                               
029800D    DISPLAY ABEND-SECTION                                                
029900                                                                          
030000     MOVE EXTRACT-EXTRACTID TO W-EXTRACTID                                
030100     MOVE EXTRACT-EXTRACTID TO W-MAIN-EXTRACT                             
030200     MOVE NOO TO SW-LIKE                                                  
030300     SET IX-EXTR TO W-EXTR-MAX                                            
030400     PERFORM UNTIL IX-EXTR = +0                                           
030500                                                                          
030600      IF W-EXTR-POS (IX-EXTR) = '*'                                       
030700         MOVE YES TO SW-LIKE                                              
030800         PERFORM UNTIL IX-EXTR > W-EXTR-MAX                               
030900           MOVE '%' TO W-EXTR-POS (IX-EXTR)                               
031000           SET IX-EXTR UP BY +1                                           
031100         END-PERFORM                                                      
031200         SET IX-EXTR TO +1                                                
031300       ELSE                                                               
031400         IF W-EXTR-POS(IX-EXTR) NOT = SPACE                               
031500            IF W-EXTR-POS(IX-EXTR) NOT = '*'                              
031600               SET IX-EXTR TO +1                                          
031700            END-IF                                                        
031800         END-IF                                                           
031900       END-IF                                                             
032000                                                                          
032100       SET IX-EXTR DOWN BY +1                                             
032200                                                                          
032300     END-PERFORM                                                          
032400                                                                          
032500     CONTINUE.                                                            
032600     EJECT                                                                
032700*--------------------------------------------------------------           
032800 BB-ADD-TO-TABLE SECTION.                                                 
032900*--------------------------------------------------------------           
033000     MOVE 'BB-ADD-TO-TABLE ' TO ABEND-SECTION                             
033100D    DISPLAY ABEND-SECTION                                                
033200                                                                          
033300     MOVE EXTRACT-EXTRACTID     TO ISPF-EXTRACTID                         
033400     MOVE EXTRACT-DESCRIPTION   TO ISPF-DESCRIPTION                       
033500     MOVE EXTRACT-SIZEX         TO ISPF-EXTSIZE                           
033600     MOVE EXTRACT-VALID-NO-DAYS TO ISPF-EXTDAYS                           
033700     MOVE EXTRACT-EXTRACT-FILE  TO ISPF-EXTFILE                           
033800     MOVE EXTRACT-TYPEX         TO ISPF-EXTTYPE                           
033900     MOVE EXTRACT-COPYTEXT      TO ISPF-EXTCOPYT                          
034000     MOVE EXTRACT-EXTRACT-JOB   TO ISPF-EXTJOB                            
034100     MOVE EXTRACT-JOB-TYPE      TO ISPF-EXTJOBTY                          
034200     MOVE EXTRACT-OWNER         TO ISPF-EXTOWNER                          
034300     MOVE W-MAIN-RESTR          TO ISPF-RESTRICTION                       
034400                                                                          
034500     PERFORM S01-TBADD-ISPFTAB                                            
034600                                                                          
034700     CONTINUE.                                                            
034800     EJECT                                                                
034900*--------------------------------------------------------------           
035000 C-INIT-PARM  SECTION.                                                    
035100*--------------------------------------------------------------           
035200     MOVE 'C-INIT-PARM     ' TO ABEND-SECTION                             
035300D    DISPLAY ABEND-SECTION                                                
035400                                                                          
035500     MOVE ISPF-EXTRACTID   TO PARM-EXTRACTID                              
035600     MOVE ISPF-DESCRIPTION TO PARM-DESCRIPTION                            
035700     MOVE ISPF-EXTSIZE     TO PARM-SIZE                                   
035800     MOVE ISPF-EXTDAYS     TO PARM-VALID-NO-DAYS                          
035900     MOVE ISPF-EXTFILE     TO PARM-EXTRACT-FILE                           
036000     MOVE ISPF-EXTTYPE     TO PARM-TYPE                                   
036100     MOVE ISPF-EXTCOPYT    TO PARM-COPYTEXT                               
036200     MOVE ISPF-EXTJOB      TO PARM-EXTRACT-JOB                            
036300     MOVE ISPF-EXTJOBTY    TO PARM-JOB-TYPE                               
036400     MOVE ISPF-EXTOWNER    TO PARM-OWNER                                  
036500     MOVE ISPF-RESTRICTION TO PARM-AUTH                                   
036600                                                                          
036700     CONTINUE.                                                            
036800     EJECT                                                                
036900*--------------------------------------------------------------           
037000 Z-FINIT SECTION.                                                         
037100*--------------------------------------------------------------           
037200     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
037300D    DISPLAY ABEND-SECTION                                                
037400     SKIP2                                                                
037500     MOVE RCODE TO RETURN-CODE                                            
037600     CONTINUE.                                                            
037700     EJECT                                                                
037800*--------------------------------------------------------------           
037900 S01-TBCREATE-ISPFTAB SECTION.                                            
038000*--------------------------------------------------------------           
038100     MOVE 'S01-TBCREATE                ' TO ABEND-SECTION                 
038200D    DISPLAY ABEND-SECTION                                                
038300     SKIP2                                                                
038400     CALL 'ISPLINK' USING TBCREATE                                        
038500                          EXTRACTTAB-NAME                                 
038600                          EXTRACTTAB-KEY                                  
038700                          EXTRACTTAB-VAR                                  
038800                          TABNOW                                          
038900                          TABREP                                          
039000     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
039100     IF RCODE <= +8                                                       
039200        MOVE NOO TO SW-ERROR                                              
039300     ELSE                                                                 
039400        MOVE YES TO SW-ERROR                                              
039500        PERFORM S99-ERROR-ROUTINE                                         
039600        MOVE RCODE-16 TO RCODE                                            
039700     END-IF                                                               
039800     CONTINUE.                                                            
039900     EJECT                                                                
040000*--------------------------------------------------------------           
040100 S01-TBADD-ISPFTAB SECTION.                                               
040200*--------------------------------------------------------------           
040300     MOVE 'S01-TBADD-ISPFTAB           ' TO ABEND-SECTION                 
040400D    DISPLAY ABEND-SECTION                                                
040500     SKIP2                                                                
040600     CALL 'ISPLINK' USING TBADD                                           
040700                          EXTRACTTAB-NAME                                 
040800     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
040900     IF RCODE <= +8                                                       
041000        MOVE NOO TO SW-ERROR                                              
041100     ELSE                                                                 
041200        MOVE YES TO SW-ERROR                                              
041300        PERFORM S99-ERROR-ROUTINE                                         
041400        MOVE RCODE-16 TO RCODE                                            
041500     END-IF                                                               
041600                                                                          
041700     CONTINUE.                                                            
041800     EJECT                                                                
041900*--------------------------------------------------------------           
042000 S01-TBSKIP-ISPFTAB SECTION.                                              
042100*--------------------------------------------------------------           
042200     MOVE 'S01-TBSKIP-ISPFTAB        ' TO ABEND-SECTION                   
042300D    DISPLAY ABEND-SECTION                                                
042400     SKIP2                                                                
042500     CALL 'ISPLINK' USING TBSKIP                                          
042600                          EXTRACTTAB-NAME                                 
042700     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
042800                                                                          
042900     IF RCODE <= +8                                                       
043000        MOVE NOO TO SW-ERROR                                              
043100     ELSE                                                                 
043200        MOVE YES TO SW-ERROR                                              
043300        PERFORM S99-ERROR-ROUTINE                                         
043400        MOVE RCODE-16 TO RCODE                                            
043500     END-IF                                                               
043600                                                                          
043700                                                                          
043800     CONTINUE.                                                            
043900     EJECT                                                                
044000*--------------------------------------------------------------           
044100 S01-TBTOP-ISPFTAB SECTION.                                               
044200*--------------------------------------------------------------           
044300     MOVE 'S01-TBTOP-ISPFTAB           ' TO ABEND-SECTION                 
044400D    DISPLAY ABEND-SECTION                                                
044500     SKIP2                                                                
044600     CALL 'ISPLINK' USING TBTOP                                           
044700                          EXTRACTTAB-NAME                                 
044800     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
044900     IF RCODE = +0                                                        
045000        MOVE NOO TO SW-ERROR                                              
045100     ELSE                                                                 
045200        MOVE YES TO SW-ERROR                                              
045300        PERFORM S99-ERROR-ROUTINE                                         
045400        MOVE RCODE-16 TO RCODE                                            
045500     END-IF                                                               
045600     CONTINUE.                                                            
045700     EJECT                                                                
045800*--------------------------------------------------------------           
045900*--------------------------------------------------------------           
046000 S99-ERROR-ROUTINE SECTION.                                               
046100*--------------------------------------------------------------           
046200     SKIP2                                                                
046300     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
046400     RCODE-DISPL DELIMITED BY SIZE                                        
046500     INTO W-ERROR-MESSAGE                                                 
046600     DISPLAY W-ERROR-MESSAGE                                              
046700     CONTINUE.                                                            
046800     EJECT                                                                
046900*--------------------------------------------------------------           
047000 SQL-DECLARE-OPEN-EXTRACT-1  SECTION.                                     
047100*--------------------------------------------------------------           
047200     MOVE 'SQL-DECLARE-OPEN-EXTRACT-1   ' TO ABEND-SECTION                
047300D    DISPLAY ABEND-SECTION                                                
047400                                                                          
047500     EXEC SQL                                                             
047600         DECLARE CRS-EXTRACT-1 CURSOR FOR                                 
047700                                                                          
047800       SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE, A.VALID_NO_DAYS,        
047900              A.EXTRACT_FILE, A.TYPE, A.EXTRACT_JOB, A.JOB_TYPE,          
048000              A.COPYTEXT, A.OWNER, B.AUTH                                 
048100       FROM   EXTRACT A,                                                  
048200              EXTRACT_USER B                                              
048300       WHERE  B.USERID = :PARM-USERID                                     
048400       AND    A.EXTRACTID = B.EXTRACTID                                   
048500     END-EXEC                                                             
048600                                                                          
048700     EXEC SQL                                                             
048800         OPEN CRS-EXTRACT-1                                               
048900     END-EXEC                                                             
049000     PERFORM S95-CONTROL-SQLCODE                                          
049100*         B.EXTRACTID LIKE '%*%%%%%%' AND USERID = USER )                 
049200     CONTINUE.                                                            
049300     EJECT                                                                
049400*--------------------------------------------------------------           
049500 SQL-FETCH-EXTRACT SECTION.                                               
049600*--------------------------------------------------------------           
049700     MOVE 'SQL-FETCH-EXTRACT           ' TO ABEND-SECTION                 
049800D    DISPLAY ABEND-SECTION                                                
049900     SKIP2                                                                
050000     EXEC SQL                                                             
050100     FETCH CRS-EXTRACT-1 INTO :EXTRACT-EXTRACTID,                         
050200                              :EXTRACT-DESCRIPTION,                       
050300                              :EXTRACT-SIZEX,                             
050400                              :EXTRACT-VALID-NO-DAYS,                     
050500                              :EXTRACT-EXTRACT-FILE,                      
050600                              :EXTRACT-TYPEX,                             
050700                              :EXTRACT-EXTRACT-JOB,                       
050800                              :EXTRACT-JOB-TYPE,                          
050900                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
051000                              :EXTRACT-OWNER,                             
051100                              :W-MAIN-RESTR                               
051200     END-EXEC                                                             
051300     MOVE SQLCODE TO SQLCODE-1                                            
051400     MOVE SQLCODE TO RCODE-DISPL                                          
051500     PERFORM S95-CONTROL-SQLCODE                                          
051600     IF SQLCODE = +000 THEN                                               
051700       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
051800         MOVE '-' TO EXTRACT-COPYTEXT                                     
051900       END-IF                                                             
052000       MOVE YES  TO SW-EXTRACT-FOUND                                      
052100     ELSE                                                                 
052200       MOVE NOO  TO SW-EXTRACT-FOUND                                      
052300     END-IF                                                               
052400     CONTINUE.                                                            
052500     EJECT                                                                
052600*--------------------------------------------------------------           
052700 SQL-CLOSE-EXTRACT SECTION.                                               
052800*--------------------------------------------------------------           
052900     MOVE 'SQL-CLOSE-EXTRACT           ' TO ABEND-SECTION                 
053000     SKIP2                                                                
053100                                                                          
053200     EXEC SQL                                                             
053300         CLOSE CRS-EXTRACT-1                                              
053400     END-EXEC                                                             
053500     PERFORM S95-CONTROL-SQLCODE                                          
053600     CONTINUE.                                                            
053700     EJECT                                                                
053800*--------------------------------------------------------------           
053900 SQL-DECLARE-OPEN-EXTRACT-CRS-2 SECTION.                                  
054000*--------------------------------------------------------------           
054100     MOVE 'SQL-DECLARE-OPEN-EXTRACT-CRS-2  ' TO ABEND-SECTION             
054200D    DISPLAY ABEND-SECTION                                                
054300                                                                          
054400D    DISPLAY 'CRS2'                                                       
054500     EXEC SQL                                                             
054600                                                                          
054700        DECLARE CRS-EXTRACT-2 CURSOR FOR                                  
054800                                                                          
054900        SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                        
055000               A.VALID_NO_DAYS, A.EXTRACT_FILE,                           
055100               A.TYPE, A.EXTRACT_JOB,                                     
055200               A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                    
055300        FROM EXTRACT A, EXTRACT_USER B                                    
055400        WHERE A.EXTRACTID IN                                              
055500        (SELECT EXTRACTID FROM EXTRACT_USER B WHERE                       
055600         B.EXTRACTID LIKE :W-EXTRACTID AND USERID = USER )                
055700         AND A.EXTRACTID ^= :W-MAIN-EXTRACT                               
055800         AND B.USERID     = :PARM-USERID                                  
055900         AND A.EXTRACTID = B.EXTRACTID                                    
056000         ORDER BY  EXTRACTID                                              
056100                                                                          
056200     END-EXEC.                                                            
056300     EXEC SQL                                                             
056400         OPEN CRS-EXTRACT-2                                               
056500     END-EXEC                                                             
056600     PERFORM S95-CONTROL-SQLCODE                                          
056700     CONTINUE.                                                            
056800     EJECT                                                                
056900*--------------------------------------------------------------           
057000 SQL-FETCH-EXTRACT-CRS-2 SECTION.                                         
057100*--------------------------------------------------------------           
057200     MOVE 'SQL-FETCH-EXTRACT-CRS-2      ' TO ABEND-SECTION                
057300D    DISPLAY ABEND-SECTION                                                
057400     SKIP2                                                                
057500     EXEC SQL                                                             
057600     FETCH CRS-EXTRACT-2 INTO :EXTRACT-EXTRACTID,                         
057700                              :EXTRACT-DESCRIPTION,                       
057800                              :EXTRACT-SIZEX,                             
057900                              :EXTRACT-VALID-NO-DAYS,                     
058000                              :EXTRACT-EXTRACT-FILE,                      
058100                              :EXTRACT-TYPEX,                             
058200                              :EXTRACT-EXTRACT-JOB,                       
058300                              :EXTRACT-JOB-TYPE,                          
058400                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
058500                              :EXTRACT-OWNER,                             
058600                              :W-MAIN-RESTR                               
058700     END-EXEC                                                             
058800                                                                          
058900     MOVE SQLCODE TO SQLCODE-2                                            
059000     MOVE SQLCODE TO RCODE-DISPL                                          
059100     PERFORM S95-CONTROL-SQLCODE                                          
059200     IF SQLCODE = +000 THEN                                               
059300       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
059400         MOVE '-' TO EXTRACT-COPYTEXT                                     
059500       END-IF                                                             
059600     END-IF                                                               
059700     CONTINUE.                                                            
059800     EJECT                                                                
059900*--------------------------------------------------------------           
060000 SQL-CLOSE-EXTRACT-CRS-2 SECTION.                                         
060100*--------------------------------------------------------------           
060200     MOVE 'SQL-CLOSE-EXTRACT-CRS-2     ' TO ABEND-SECTION                 
060300     SKIP2                                                                
060400                                                                          
060500     EXEC SQL                                                             
060600         CLOSE CRS-EXTRACT-2                                              
060700     END-EXEC                                                             
060800     PERFORM S95-CONTROL-SQLCODE                                          
060900     CONTINUE.                                                            
061000*--------------------------------------------------------------           
061100 SQL-DECLARE-OPEN-EXTRACT-CRS-3 SECTION.                                  
061200*--------------------------------------------------------------           
061300     MOVE 'SQL-DECLARE-OPEN-EXTRACT-CRS-3' TO ABEND-SECTION               
061400D    DISPLAY ABEND-SECTION                                                
061500                                                                          
061600D    DISPLAY 'CRS3'                                                       
061700     EXEC SQL                                                             
061800                                                                          
061900        DECLARE CRS-EXTRACT-3 CURSOR FOR                                  
062000        SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                        
062100               A.VALID_NO_DAYS, A.EXTRACT_FILE,                           
062200               A.TYPE, A.EXTRACT_JOB,                                     
062300               A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                    
062400        FROM EXTRACT A, EXTRACT_USER B                                    
062500        WHERE A.EXTRACTID LIKE :W-EXTRACTID                               
062600          AND B.EXTRACTID  = :W-MAIN-EXTRACT                              
062700          AND USERID = :PARM-USERID                                       
062800          AND A.EXTRACTID ^= :W-MAIN-EXTRACT                              
062900         ORDER BY  EXTRACTID                                              
063000     END-EXEC.                                                            
063100     EXEC SQL                                                             
063200         OPEN CRS-EXTRACT-3                                               
063300     END-EXEC                                                             
063400     PERFORM S95-CONTROL-SQLCODE                                          
063500                                                                          
063600     CONTINUE.                                                            
063700     EJECT                                                                
063800*--------------------------------------------------------------           
063900 SQL-FETCH-EXTRACT-CRS-3 SECTION.                                         
064000*--------------------------------------------------------------           
064100     MOVE 'SQL-FETCH-EXTRACT-CRS-3     ' TO ABEND-SECTION                 
064200D    DISPLAY ABEND-SECTION                                                
064300     SKIP2                                                                
064400     EXEC SQL                                                             
064500     FETCH CRS-EXTRACT-3 INTO :EXTRACT-EXTRACTID,                         
064600                              :EXTRACT-DESCRIPTION,                       
064700                              :EXTRACT-SIZEX,                             
064800                              :EXTRACT-VALID-NO-DAYS,                     
064900                              :EXTRACT-EXTRACT-FILE,                      
065000                              :EXTRACT-TYPEX,                             
065100                              :EXTRACT-EXTRACT-JOB,                       
065200                              :EXTRACT-JOB-TYPE,                          
065300                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
065400                              :EXTRACT-OWNER,                             
065500                              :W-MAIN-RESTR                               
065600     END-EXEC                                                             
065700                                                                          
065800     MOVE SQLCODE TO SQLCODE-3                                            
065900     MOVE SQLCODE TO RCODE-DISPL                                          
066000     PERFORM S95-CONTROL-SQLCODE                                          
066100     IF SQLCODE = +000 THEN                                               
066200       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
066300         MOVE '-' TO EXTRACT-COPYTEXT                                     
066400       END-IF                                                             
066500     END-IF                                                               
066600     CONTINUE.                                                            
066700     EJECT                                                                
066800*--------------------------------------------------------------           
066900 SQL-CLOSE-EXTRACT-CRS-3 SECTION.                                         
067000*--------------------------------------------------------------           
067100     MOVE 'SQL-CLOSE-EXTRACT-CRS-3  ' TO ABEND-SECTION                    
067200     SKIP2                                                                
067300                                                                          
067400     EXEC SQL                                                             
067500         CLOSE CRS-EXTRACT-3                                              
067600     END-EXEC                                                             
067700     PERFORM S95-CONTROL-SQLCODE                                          
067800     CONTINUE.                                                            
067900*--------------------------------------------------------------           
068000 SQL-COMMIT SECTION.                                                      
068100*--------------------------------------------------------------           
068200     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
068300D    DISPLAY ABEND-SECTION                                                
068400     SKIP2                                                                
068500     EXEC SQL                                                             
068600         COMMIT                                                           
068700     END-EXEC                                                             
068800     PERFORM S95-CONTROL-SQLCODE                                          
068900     CONTINUE.                                                            
069000     EJECT                                                                
069100*-------------------------------- DB2 ERROR HANDLING                      
069200*    -COPY V161PS                                                         
069300*++INCLUDE V161PS                                                         

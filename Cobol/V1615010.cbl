000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1615010.                                                 
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
017200*++INCLUDE V161WS                                                         
017300*-------------------------------- DB2-AREAS                               
017400     EXEC SQL                                                             
017500          INCLUDE SQLCA                                                   
017600     END-EXEC.                                                            
017700     EXEC SQL                                                             
017800          INCLUDE EXTRACT                                                 
017900     END-EXEC.                                                            
018000*    -COPY EXTRACT -PRE EXTRACT-                                          
018100     EXEC SQL                                                             
018200          INCLUDE EXTUSER                                                 
018300     END-EXEC.                                                            
018400*    -COPY EXTUSER -PRE EXTUSER-                                          
018500     EJECT                                                                
018600 LINKAGE SECTION.                                                         
018700                                                                          
018800 01  SW-EXTRACT-FOUND          PIC X(1).                                  
018900                                                                          
019000 01  PARM-CODE                 PIC X(1).                                  
019100                                                                          
019200*    -COPY V16110CC -PRE PARM-                                            
019300*++INCLUDE  V16110CC                                                      
019400                                                                          
019500*************************************************************             
019600 PROCEDURE DIVISION USING                                                 
019700     SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110.                            
019800*************************************************************             
019900                                                                          
020000     PERFORM A-INIT                                                       
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
048300       WHERE  A.EXTRACTID = :PARM-EXTRACTID                               
048400       AND    A.EXTRACTID = B.EXTRACTID                                   
048500       AND    B.USERID = USER                                             
048600     END-EXEC                                                             
048700                                                                          
048800     EXEC SQL                                                             
048900         OPEN CRS-EXTRACT-1                                               
049000     END-EXEC                                                             
049100     PERFORM S95-CONTROL-SQLCODE                                          
049200*         B.EXTRACTID LIKE '%*%%%%%%' AND USERID = USER )                 
049300     CONTINUE.                                                            
049400     EJECT                                                                
049500*--------------------------------------------------------------           
049600 SQL-FETCH-EXTRACT SECTION.                                               
049700*--------------------------------------------------------------           
049800     MOVE 'SQL-FETCH-EXTRACT           ' TO ABEND-SECTION                 
049900D    DISPLAY ABEND-SECTION                                                
050000     SKIP2                                                                
050100     EXEC SQL                                                             
050200     FETCH CRS-EXTRACT-1 INTO :EXTRACT-EXTRACTID,                         
050300                              :EXTRACT-DESCRIPTION,                       
050400                              :EXTRACT-SIZEX,                             
050500                              :EXTRACT-VALID-NO-DAYS,                     
050600                              :EXTRACT-EXTRACT-FILE,                      
050700                              :EXTRACT-TYPEX,                             
050800                              :EXTRACT-EXTRACT-JOB,                       
050900                              :EXTRACT-JOB-TYPE,                          
051000                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
051100                              :EXTRACT-OWNER,                             
051200                              :W-MAIN-RESTR                               
051300     END-EXEC                                                             
051400     MOVE SQLCODE TO SQLCODE-1                                            
051500     MOVE SQLCODE TO RCODE-DISPL                                          
051600     PERFORM S95-CONTROL-SQLCODE                                          
051700     IF SQLCODE = +000 THEN                                               
051800       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
051900         MOVE '-' TO EXTRACT-COPYTEXT                                     
052000       END-IF                                                             
052100       MOVE YES  TO SW-EXTRACT-FOUND                                      
052200     ELSE                                                                 
052300       MOVE NOO  TO SW-EXTRACT-FOUND                                      
052400     END-IF                                                               
052500     CONTINUE.                                                            
052600     EJECT                                                                
052700*--------------------------------------------------------------           
052800 SQL-CLOSE-EXTRACT SECTION.                                               
052900*--------------------------------------------------------------           
053000     MOVE 'SQL-CLOSE-EXTRACT           ' TO ABEND-SECTION                 
053100     SKIP2                                                                
053200                                                                          
053300     EXEC SQL                                                             
053400         CLOSE CRS-EXTRACT-1                                              
053500     END-EXEC                                                             
053600     PERFORM S95-CONTROL-SQLCODE                                          
053700     CONTINUE.                                                            
053800     EJECT                                                                
053900*--------------------------------------------------------------           
054000 SQL-DECLARE-OPEN-EXTRACT-CRS-2 SECTION.                                  
054100*--------------------------------------------------------------           
054200     MOVE 'SQL-DECLARE-OPEN-EXTRACT-CRS-2  ' TO ABEND-SECTION             
054300D    DISPLAY ABEND-SECTION                                                
054400                                                                          
054500D    DISPLAY 'CRS2'                                                       
054600     EXEC SQL                                                             
054700                                                                          
054800        DECLARE CRS-EXTRACT-2 CURSOR FOR                                  
054900                                                                          
055000        SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                        
055100               A.VALID_NO_DAYS, A.EXTRACT_FILE,                           
055200               A.TYPE, A.EXTRACT_JOB,                                     
055300               A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                    
055400        FROM EXTRACT A, EXTRACT_USER B                                    
055500        WHERE A.EXTRACTID IN                                              
055600        (SELECT EXTRACTID FROM EXTRACT_USER B WHERE                       
055700         B.EXTRACTID LIKE :W-EXTRACTID AND USERID = USER )                
055800         AND A.EXTRACTID ^= :W-MAIN-EXTRACT                               
055900         AND B.USERID     = USER                                          
056000         AND A.EXTRACTID = B.EXTRACTID                                    
056100         ORDER BY  EXTRACTID                                              
056200                                                                          
056300     END-EXEC.                                                            
056400     EXEC SQL                                                             
056500         OPEN CRS-EXTRACT-2                                               
056600     END-EXEC                                                             
056700     PERFORM S95-CONTROL-SQLCODE                                          
056800     CONTINUE.                                                            
056900     EJECT                                                                
057000*--------------------------------------------------------------           
057100 SQL-FETCH-EXTRACT-CRS-2 SECTION.                                         
057200*--------------------------------------------------------------           
057300     MOVE 'SQL-FETCH-EXTRACT-CRS-2      ' TO ABEND-SECTION                
057400D    DISPLAY ABEND-SECTION                                                
057500     SKIP2                                                                
057600     EXEC SQL                                                             
057700     FETCH CRS-EXTRACT-2 INTO :EXTRACT-EXTRACTID,                         
057800                              :EXTRACT-DESCRIPTION,                       
057900                              :EXTRACT-SIZEX,                             
058000                              :EXTRACT-VALID-NO-DAYS,                     
058100                              :EXTRACT-EXTRACT-FILE,                      
058200                              :EXTRACT-TYPEX,                             
058300                              :EXTRACT-EXTRACT-JOB,                       
058400                              :EXTRACT-JOB-TYPE,                          
058500                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
058600                              :EXTRACT-OWNER,                             
058700                              :W-MAIN-RESTR                               
058800     END-EXEC                                                             
058900                                                                          
059000     MOVE SQLCODE TO SQLCODE-2                                            
059100     MOVE SQLCODE TO RCODE-DISPL                                          
059200     PERFORM S95-CONTROL-SQLCODE                                          
059300     IF SQLCODE = +000 THEN                                               
059400       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
059500         MOVE '-' TO EXTRACT-COPYTEXT                                     
059600       END-IF                                                             
059700     END-IF                                                               
059800     CONTINUE.                                                            
059900     EJECT                                                                
060000*--------------------------------------------------------------           
060100 SQL-CLOSE-EXTRACT-CRS-2 SECTION.                                         
060200*--------------------------------------------------------------           
060300     MOVE 'SQL-CLOSE-EXTRACT-CRS-2     ' TO ABEND-SECTION                 
060400     SKIP2                                                                
060500                                                                          
060600     EXEC SQL                                                             
060700         CLOSE CRS-EXTRACT-2                                              
060800     END-EXEC                                                             
060900     PERFORM S95-CONTROL-SQLCODE                                          
061000     CONTINUE.                                                            
061100*--------------------------------------------------------------           
061200 SQL-DECLARE-OPEN-EXTRACT-CRS-3 SECTION.                                  
061300*--------------------------------------------------------------           
061400     MOVE 'SQL-DECLARE-OPEN-EXTRACT-CRS-3' TO ABEND-SECTION               
061500D    DISPLAY ABEND-SECTION                                                
061600                                                                          
061700D    DISPLAY 'CRS3'                                                       
061800     EXEC SQL                                                             
061900                                                                          
062000        DECLARE CRS-EXTRACT-3 CURSOR FOR                                  
062100        SELECT A.EXTRACTID, A.DESCRIPTION, A.SIZE,                        
062200               A.VALID_NO_DAYS, A.EXTRACT_FILE,                           
062300               A.TYPE, A.EXTRACT_JOB,                                     
062400               A.JOB_TYPE, A.COPYTEXT, A.OWNER, B.AUTH                    
062500        FROM EXTRACT A, EXTRACT_USER B                                    
062600        WHERE A.EXTRACTID LIKE :W-EXTRACTID                               
062700          AND B.EXTRACTID  = :W-MAIN-EXTRACT                              
062800          AND USERID = USER                                               
062900          AND A.EXTRACTID ^= :W-MAIN-EXTRACT                              
063000         ORDER BY  EXTRACTID                                              
063100     END-EXEC.                                                            
063200     EXEC SQL                                                             
063300         OPEN CRS-EXTRACT-3                                               
063400     END-EXEC                                                             
063500     PERFORM S95-CONTROL-SQLCODE                                          
063600                                                                          
063700     CONTINUE.                                                            
063800     EJECT                                                                
063900*--------------------------------------------------------------           
064000 SQL-FETCH-EXTRACT-CRS-3 SECTION.                                         
064100*--------------------------------------------------------------           
064200     MOVE 'SQL-FETCH-EXTRACT-CRS-3     ' TO ABEND-SECTION                 
064300D    DISPLAY ABEND-SECTION                                                
064400     SKIP2                                                                
064500     EXEC SQL                                                             
064600     FETCH CRS-EXTRACT-3 INTO :EXTRACT-EXTRACTID,                         
064700                              :EXTRACT-DESCRIPTION,                       
064800                              :EXTRACT-SIZEX,                             
064900                              :EXTRACT-VALID-NO-DAYS,                     
065000                              :EXTRACT-EXTRACT-FILE,                      
065100                              :EXTRACT-TYPEX,                             
065200                              :EXTRACT-EXTRACT-JOB,                       
065300                              :EXTRACT-JOB-TYPE,                          
065400                              :EXTRACT-COPYTEXT:WS-NULL-IND1,             
065500                              :EXTRACT-OWNER,                             
065600                              :W-MAIN-RESTR                               
065700     END-EXEC                                                             
065800                                                                          
065900     MOVE SQLCODE TO SQLCODE-3                                            
066000     MOVE SQLCODE TO RCODE-DISPL                                          
066100     PERFORM S95-CONTROL-SQLCODE                                          
066200     IF SQLCODE = +000 THEN                                               
066300       IF WS-NULL-IND1 < 0 OR EXTRACT-COPYTEXT = SPACE THEN               
066400         MOVE '-' TO EXTRACT-COPYTEXT                                     
066500       END-IF                                                             
066600     END-IF                                                               
066700     CONTINUE.                                                            
066800     EJECT                                                                
066900*--------------------------------------------------------------           
067000 SQL-CLOSE-EXTRACT-CRS-3 SECTION.                                         
067100*--------------------------------------------------------------           
067200     MOVE 'SQL-CLOSE-EXTRACT-CRS-3  ' TO ABEND-SECTION                    
067300     SKIP2                                                                
067400                                                                          
067500     EXEC SQL                                                             
067600         CLOSE CRS-EXTRACT-3                                              
067700     END-EXEC                                                             
067800     PERFORM S95-CONTROL-SQLCODE                                          
067900     CONTINUE.                                                            
068000*--------------------------------------------------------------           
068100 SQL-COMMIT SECTION.                                                      
068200*--------------------------------------------------------------           
068300     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
068400D    DISPLAY ABEND-SECTION                                                
068500     SKIP2                                                                
068600     EXEC SQL                                                             
068700         COMMIT                                                           
068800     END-EXEC                                                             
068900     PERFORM S95-CONTROL-SQLCODE                                          
069000     CONTINUE.                                                            
069100     EJECT                                                                
069200*-------------------------------- DB2 ERROR HANDLING                      
069300*    -COPY V161PS                                                         
069400*++INCLUDE V161PS                                                         

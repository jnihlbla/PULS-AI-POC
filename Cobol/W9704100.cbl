000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9704100.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   07/10/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        EXTRACT ACF2 WARNINGS FROM SYSTEM LOG                            
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800                                                                          
001900*          --- OPER LOG                                                   
002000     SELECT OLOG                       ASSIGN TO W97041D1.                
002100                                                                          
002200*          --- EXTRACTED ACF2 LOG RECORDS                                 
002300     SELECT ALOG                       ASSIGN TO W97041D2.                
002400                                                                          
002500*          --- EXTRACTED SOP MESSAGE RECORDS                              
002600     SELECT SMSG                       ASSIGN TO W97041D3.                
002700                                                                          
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  OLOG                                                                 
003300     RECORDING       V                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600 01  FILLER                      PIC X(128).                              
003700     SKIP3                                                                
003800                                                                          
003900 FD  ALOG                                                                 
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300 01  ALOG-POST                   PIC X(82).                               
004400     SKIP3                                                                
004500                                                                          
004600 FD  SMSG                                                                 
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  SMSG-POST                   PIC X(98).                               
005100                                                                          
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W9704100'.            
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NOO                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  OLOG-EOF-SW                 PIC X       VALUE 'N'.                   
005900     88  END-OF-OLOG                         VALUE 'Y'.                   
006000                                                                          
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600                                                                          
006700 01  NOT-USED                    PIC X(10).                               
006800                                                                          
006900 01  GENERAL-SUBPROGRAMS.                                                 
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300                                                                          
007400*    --- PARAMETERS TO ABEND                                              
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007900                                                                          
008000 01  ERRTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300                                                                          
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700                                                                          
008800 01  OLOG-AREA-START             PIC X(24)   VALUE                        
008900                                 'OLOG-AREA-START  '.                     
009000 01  OLOG-AREA.                                                           
009100     03  FILLER                  PIC X(19).                               
009200     03  OLOG-YYYYDDD            PIC X(7).                                
009300     03  FILLER                  PIC X.                                   
009400     03  OLOG-HHMMSSHH           PIC X(11).                               
009500     03  FILLER                  PIC X.                                   
009600     03  OLOG-TASKID             PIC X(8).                                
009700     03  FILLER                  PIC X.                                   
009800     03  OLOG-MSGNUM             PIC X(8).                                
009900     03  FILLER                  PIC XX.                                  
010000                                                                          
010100     03  OLOG-ACF2-LOG.                                                   
010200       05  OLOG-ACF2-MSGID         PIC X(8).                              
010300       05  FILLER                  PIC X.                                 
010400       05  OLOG-ACF2-TEXTHEADER    PIC X(12).                             
010500       05  FILLER                  PIC X(7).                              
010600       05  OLOG-ACF2-MSGTEXT       PIC X(60).                             
010700                                                                          
010800     03  OLOG-SOP-MSG  REDEFINES OLOG-ACF2-LOG.                           
010900       05  FILLER                  PIC X.                                 
011000       05  OLOG-SOP-USERID         PIC X(8).                              
011100       05  OLOG-SOP-MSG-PREFIX     PIC X(4).                              
011200       05  OLOG-SOP-MSGNUM         PIC X(4).                              
011300       05  FILLER                  PIC XX.                                
011400       05  OLOG-SOP-MSGTEXT        PIC X(60).                             
011500                                                                          
011600     EJECT                                                                
011700 01  ALOG-AREA-START             PIC X(24)   VALUE                        
011800                                 'ALOG-AREA-START  '.                     
011900 01  ALOG-AREA.                                                           
012000     03  ALOG-YYYYDDD            PIC X(7).                                
012100     03  FILLER                  PIC X.                                   
012200     03  ALOG-HHMMSSHH           PIC X(11).                               
012300     03  FILLER                  PIC X.                                   
012400     03  ALOG-TASKID             PIC X(8).                                
012500     03  FILLER                  PIC X.                                   
012600     03  ALOG-USERID             PIC X(8).                                
012700     03  FILLER                  PIC X.                                   
012800     03  ALOG-FILENAME           PIC X(44).                               
012900                                                                          
013000                                                                          
013100 01  SMSG-AREA-START             PIC X(24)   VALUE                        
013200                                 'SMSG-AREA-START  '.                     
013300 01  SMSG-AREA.                                                           
013400     03  SMSG-YYYYDDD            PIC X(7).                                
013500     03  FILLER                  PIC X.                                   
013600     03  SMSG-HHMMSSHH           PIC X(11).                               
013700     03  FILLER                  PIC X.                                   
013800     03  SMSG-TASKID             PIC X(8).                                
013900     03  FILLER                  PIC X.                                   
014000     03  SMSG-USERID             PIC X(8).                                
014100     03  FILLER                  PIC X.                                   
014200     03  SMSG-SOP-MSGTEXT        PIC X(60).                               
014300                                                                          
014400                                                                          
014500     EJECT                                                                
014600 PROCEDURE DIVISION.                                                      
014700 MAIN SECTION.                                                            
014800                                                                          
014900     PERFORM A-INIT                                                       
015000     PERFORM S01-READ-OLOG                                                
015100     PERFORM UNTIL END-OF-OLOG                                            
015200                                                                          
015300*      -- SELECT ACF2 SEC ID MESSAGES                                     
015400       IF OLOG-ACF2-TEXTHEADER = 'ACF2 LOGGING'                           
015500                                                                          
015600         MOVE SPACE TO ALOG-AREA                                          
015700         MOVE OLOG-YYYYDDD      TO ALOG-YYYYDDD                           
015800         MOVE OLOG-HHMMSSHH     TO ALOG-HHMMSSHH                          
015900         UNSTRING OLOG-ACF2-MSGTEXT                                       
016000            DELIMITED BY ','                                              
016100            INTO ALOG-USERID, NOT-USED, ALOG-FILENAME                     
016200                                                                          
016300         IF ALOG-FILENAME(1:1) = 'W'                                      
016400           PERFORM S11-WRITE-ALOG                                         
016500         END-IF                                                           
016600       END-IF                                                             
016700                                                                          
016800*      -- SELECT PROD-SOP MESSAGES                                        
016900       IF OLOG-SOP-MSG-PREFIX  = 'PSOP'                                   
017000                                                                          
017100         MOVE SPACE TO SMSG-AREA                                          
017200         MOVE OLOG-YYYYDDD      TO SMSG-YYYYDDD                           
017300         MOVE OLOG-HHMMSSHH     TO SMSG-HHMMSSHH                          
017400         MOVE OLOG-TASKID       TO SMSG-TASKID                            
017500         MOVE OLOG-SOP-USERID   TO SMSG-USERID                            
017600         MOVE OLOG-SOP-MSGTEXT  TO SMSG-SOP-MSGTEXT                       
017700                                                                          
017800         PERFORM S12-WRITE-SMSG                                           
017900       END-IF                                                             
018000                                                                          
018100       PERFORM S01-READ-OLOG                                              
018200     END-PERFORM                                                          
018300                                                                          
018400     PERFORM Z-FINIT                                                      
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019100 A-INIT SECTION.                                                          
019200                                                                          
019300     OPEN INPUT  OLOG                                                     
019400     OPEN OUTPUT ALOG, SMSG                                               
019500                                                                          
019600     ACCEPT TODAYS-DATE  FROM DATE                                        
019700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019800     .                                                                    
019900                                                                          
020000 Z-FINIT SECTION.                                                         
020100     CLOSE OLOG                                                           
020200           ALOG                                                           
020300           SMSG                                                           
020400                                                                          
020500     MOVE 'S' TO POSTSUM-OPKOD                                            
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 S01-READ-OLOG    SECTION.                                                
021100     READ OLOG INTO OLOG-AREA                                             
021200     AT END                                                               
021300        MOVE HIGH-VALUE TO OLOG-AREA                                      
021400        SET END-OF-OLOG TO TRUE                                           
021500                                                                          
021600     NOT AT END                                                           
021700        CONTINUE                                                          
021800     END-READ                                                             
021900     .                                                                    
022000                                                                          
022100 S11-WRITE-ALOG SECTION.                                                  
022200                                                                          
022300     WRITE ALOG-POST FROM ALOG-AREA                                       
022400                                                                          
022500     MOVE 'ALOG' TO POSTSUM-FDNAMN                                        
022600     MOVE 'W97041D2' TO POSTSUM-DDNAMN2                                   
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900                                                                          
023000 S12-WRITE-SMSG SECTION.                                                  
023100                                                                          
023200     WRITE SMSG-POST FROM SMSG-AREA                                       
023300                                                                          
023400     MOVE 'SMSG' TO POSTSUM-FDNAMN                                        
023500     MOVE 'W97041D3' TO POSTSUM-DDNAMN2                                   
023600     CALL POSTSUM USING POSTSUM-PARM                                      
023700     .                                                                    

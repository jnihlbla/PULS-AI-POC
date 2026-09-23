000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9802600.                                                
000300 AUTHOR.         KARIN JOHANSSON.                                         
000400 DATE-WRITTEN.   00/11/09 (R9802800) TILL CAR 01/04/04 (W9802600)         
000500                                                                          
000600*                                                                         
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM IS RUN WHEN A JOB CONTROLLED BY SOP ABENDS.         
000900*        IT SENDS CASE RECORDS TO SCOPUS VIA A VCOM API.                  
001000*                                                                         
001100*        02/04/19 - changes for VINST made                                
001200                                                                          
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- PARAMETER FILE                                             
002100     SELECT PARMFILE                   ASSIGN TO W98026D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400                                                                          
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  PARMFILE                                                             
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100 01  FILLER     PIC X(80).                                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W9802600'.            
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000 77  PARMFILE-EOF-SW             PIC X       VALUE 'N'.                   
004100     88  END-OF-PARMFILE                     VALUE 'J'.                   
004200     EJECT                                                                
004300 01  GENERAL-SUBPROGRAM.                                                  
004400*                                                                         
004500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004600     03  DSCONS                  PIC X(8)    VALUE 'DSCONS'.              
004700     03  DSSEND                  PIC X(8)    VALUE 'DSSEND'.              
004800     03  DSRLSE                  PIC X(8)    VALUE 'DSRLSE'.              
004900                                                                          
005000*    --- PARAMETERS TO ABEND                                              
005100                                                                          
005200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005400                                                                          
005500 01  ERRTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005700     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005900*01  -COPY W0028  -PRE VCOM-                                              
006000     EJECT                                                                
006100 01  PARM-AREA                   PIC X(80).                               
006200                                                                          
006300 01  WSYMBID                     PIC X(20).                               
006400 01  WSYMB-VALUE                 PIC X(60).                               
006500 01  WFILL                       PIC X(80).                               
006600 01  WFILL2                      PIC X(80).                               
006700                                                                          
006800 01  WIDPROCESS                  PIC X(10).                               
006900 01  WPAR-IDPROCESS              PIC X(10).                               
007000 01  WPRIO                       PIC X.                                   
007100 01  WTIMESTAMP                  PIC X(16).                               
007200 01  WABENDMSG                   PIC X(65).                               
007300 01  WSEVERITY                   PIC X(20).                               
007400 01  PTR                         PIC S9(9)   COMP SYNC.                   
007500 01  ANT-CHAR                    PIC S9(4)   COMP-3.                      
007600 01  RCODE                       PIC S9(4)   COMP.                        
007700     EJECT                                                                
007800 LINKAGE SECTION.                                                         
007900 01  PA-AREA.                                                             
008000     03 PA-LENGTH                PIC S9999 COMP SYNC.                     
008100     03 PA-GROUP                 PIC XXXX.                                
008200     EJECT                                                                
008300 PROCEDURE DIVISION USING PA-AREA.                                        
008400 MAIN SECTION.                                                            
008500                                                                          
008600     PERFORM A-INIT                                                       
008700     PERFORM B-GET-PARAMETERS                                             
008800     PERFORM UNTIL END-OF-PARMFILE                                        
008900       IF WIDPROCESS NOT = SPACE                                          
009000         PERFORM C-SEND-CASE-RECORDS                                      
009100       ELSE                                                               
009200         MOVE 4 TO RCODE                                                  
009300       END-IF                                                             
009400       PERFORM B-GET-PARAMETERS                                           
009500     END-PERFORM                                                          
009600                                                                          
009700     PERFORM Z-FINIT                                                      
009800                                                                          
009900     MOVE RCODE TO RETURN-CODE                                            
010000     GOBACK                                                               
010100     .                                                                    
010200     EJECT                                                                
010300 A-INIT SECTION.                                                          
010400                                                                          
010500     OPEN INPUT  PARMFILE                                                 
010600     MOVE ZERO TO RCODE                                                   
010700     .                                                                    
010800     EJECT                                                                
010900 B-GET-PARAMETERS  SECTION.                                               
011000                                                                          
011100     PERFORM S01-READ-PARMFILE                                            
011200     IF NOT END-OF-PARMFILE                                               
011300       MOVE PARM-AREA             TO WIDPROCESS                           
011400       PERFORM S01-READ-PARMFILE                                          
011500       MOVE PARM-AREA             TO WTIMESTAMP                           
011600       PERFORM S01-READ-PARMFILE                                          
011700       MOVE PARM-AREA             TO WPRIO                                
011800       PERFORM S01-READ-PARMFILE                                          
011900       MOVE PARM-AREA             TO WABENDMSG                            
012000       PERFORM S01-READ-PARMFILE                                          
012100       MOVE PARM-AREA             TO WPAR-IDPROCESS                       
012200                                                                          
012300       DISPLAY ' '                                                        
012400       DISPLAY 'IDPROCESS    ' WIDPROCESS                                 
012500       DISPLAY 'PARIDPROCESS ' WPAR-IDPROCESS                             
012600       DISPLAY 'ABENDMSG     ' WABENDMSG                                  
012700       DISPLAY 'TIMESTAMP    ' WTIMESTAMP                                 
012800       DISPLAY 'GROUP        ' PA-GROUP                                   
012900       DISPLAY 'PRIO         ' WPRIO                                      
013000     END-IF                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 C-SEND-CASE-RECORDS  SECTION.                                            
013400                                                                          
013500     IF PA-GROUP = 'PROD'                                                 
013600       MOVE 'W980Z1SE' TO VCOM-PARTNER                                    
013700     ELSE                                                                 
013800       MOVE 'W980Z1TT' TO VCOM-PARTNER                                    
013900     END-IF                                                               
014000*    MOVE 'W980'   TO VCOM-INITIATOR                                      
014100     MOVE 'INIT41' TO VCOM-INITIATOR                                      
014200     MOVE 'VVIS.LQ.AUTOCASE CRL' TO VCOM-SENDERTAG                        
014300                                                                          
014400     CALL  DSCONS  USING VCOM-RC                                          
014500                         VCOM-DISTID                                      
014600                         VCOM-SECUR                                       
014700                         VCOM-TIMEOUT                                     
014800                         VCOM-SENDERTAG                                   
014900                         VCOM-PARTNER                                     
015000                         VCOM-RECEIPT                                     
015100                         VCOM-PRIO                                        
015200                         VCOM-INITIATOR                                   
015300                                                                          
015400     DISPLAY ' '                                                          
015500     DISPLAY 'DSCONS RC         = ' VCOM-RC                               
015600     DISPLAY 'DSCONS DISTID     = ' VCOM-DISTID                           
015700     DISPLAY 'DSCONS SECUR      = ' VCOM-SECUR-DATA                       
015800     DISPLAY 'DSCONS TIMEOUT    = ' VCOM-TIMEOUT                          
015900     DISPLAY 'DSCONS SENDERTAG  = ' VCOM-SENDERTAG                        
016000     DISPLAY 'DSCONS INITIATOR  = ' VCOM-INITIATOR                        
016100     DISPLAY 'DSCONS PARTNER    = ' VCOM-PARTNER                          
016200     DISPLAY 'DSCONS PRIO       = ' VCOM-PRIO                             
016300                                                                          
016400     IF VCOM-RC NOT = ZERO                                                
016500       MOVE 8 TO RCODE                                                    
016600     ELSE                                                                 
016700       EVALUATE TRUE                                                      
016800         WHEN WPRIO = '4' OR '5' OR '6'                                   
016900           MOVE 'Major' TO WSEVERITY                                      
017000         WHEN WPRIO = '2' OR '3'                                          
017100           MOVE 'Minor' TO WSEVERITY                                      
017200         WHEN OTHER                                                       
017300           MOVE 'Minimal' TO WSEVERITY                                    
017400       END-EVALUATE                                                       
017500                                                                          
017600       MOVE SPACE TO VCOM-DATA                                            
017700       MOVE 1 TO PTR                                                      
017800                                                                          
017900       STRING                                                             
018000         'AUTOSR;'                                                        
018100         ';;;;'                                                           
018200         'New;'                                                           
018300         DELIMITED BY SIZE                                                
018400         INTO VCOM-DATA WITH POINTER PTR                                  
018500                                                                          
018600       STRING                                                             
018700*         -- CONTACT /SECURITY ID                                         
018800         'OS390WSOP;'    DELIMITED BY SIZE                                
018900*         -- RECEIVING PRG                                                
019000******** 'GOT PULS AUTO CASE BO;' DELIMITED BY SIZE                       
019100******** 'GOT PULS AUTO CASE 2ND;' DELIMITED BY SIZE                      
019200******** 'GOT PULS AUTO CASE 3RD;' DELIMITED BY SIZE                      
019210         'GOT PULS MA 3RD;' DELIMITED BY SIZE                             
019300*         -- BUSINESS IMPACT                                              
019400          WSEVERITY     DELIMITED BY SPACE                                
019500          ';'           DELIMITED BY SIZE                                 
019600*         -- SENDING SYSTEM                                               
019700          'SOP Parts;'  DELIMITED BY SIZE                                 
019800*         -- SUMMARY TEXT                                                 
019900          WABENDMSG     DELIMITED BY SIZE                                 
020000          ';'           DELIMITED BY SIZE                                 
020100*         -- RESOURCE NAME                                                
020200          WIDPROCESS    DELIMITED BY SPACE                                
020300          ';'           DELIMITED BY SIZE                                 
020400*         -- ACTION, ABEND, HW TYPE, PROBLEM TYPE, DESCR                  
020500*            HWDADDR, CHTYPE  NOT USED                                    
020600          ';;;;;;;'     DELIMITED BY SIZE                                 
020700*         -- DETECTED                                                     
020800          WTIMESTAMP    DELIMITED BY SIZE                                 
020900          ';'           DELIMITED BY SIZE                                 
021000*         -- SHEDULED READY, ACTUAL READY, NOTE  NOT USED                 
021100          ';;;'         DELIMITED BY SIZE                                 
021200*         -- INITIATOR                                                    
021300         'W0SOP01;'    DELIMITED BY SIZE                                  
021400*         -- REFERENCE, ASSET  NOT USED                                   
021500         ';;'           DELIMITED BY SIZE                                 
021600*         -- LINE-FEED. UTGÅTT, ERSATT MED CRL I SENDERTAG                
021700*        x'25'          DELIMITED BY SIZE                                 
021800                                                                          
021900          INTO VCOM-DATA                                                  
022000          WITH POINTER PTR                                                
022100*         WPAR-IDPROCESS DELIMITED BY SPACE                               
022200                                                                          
022300       SUBTRACT 1 FROM PTR GIVING VCOM-ACTLENGTH                          
022400                                                                          
022500       DISPLAY ' '                                                        
022600       DISPLAY VCOM-DATA (1:VCOM-ACTLENGTH)                               
022700       DISPLAY ' '                                                        
022800                                                                          
022900       CALL  DSSEND  USING VCOM-RC                                        
023000                           VCOM-DISTID                                    
023100                           VCOM-ACTLENGTH                                 
023200                           VCOM-DATA                                      
023300                                                                          
023400       DISPLAY ' '                                                        
023500       DISPLAY 'DSSEND RC         = ' VCOM-RC                             
023600                                                                          
023700       IF VCOM-RC NOT = ZERO                                              
023800         MOVE 8 TO RCODE                                                  
023900       END-IF                                                             
024000                                                                          
024100       MOVE +1   TO VCOM-RVALUE                                           
024200       CALL DSRLSE USING VCOM-RC                                          
024300                         VCOM-DISTID                                      
024400                         VCOM-RVALUE                                      
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 Z-FINIT SECTION.                                                         
024900                                                                          
025000     CLOSE PARMFILE                                                       
025100     .                                                                    
025200     EJECT                                                                
025300 S01-READ-PARMFILE SECTION.                                               
025400                                                                          
025500     READ PARMFILE INTO PARM-AREA                                         
025600     AT END                                                               
025700        SET END-OF-PARMFILE TO TRUE                                       
025800     END-READ                                                             
025900     .                                                                    

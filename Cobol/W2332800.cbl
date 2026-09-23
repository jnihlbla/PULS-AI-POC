000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2332800.                                                
000400 AUTHOR.         STENING INGER.                                           
000500 DATE-WRITTEN.   19/09/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CREATE EXTRACT FILE WITH DATA FROM HERKULES SYSTEM               
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401                                                                          
002403     SELECT W233271                    ASSIGN TO W23328D1.                
002405*          --- INPUT FILE FROM PGM W23327 - START RECORD                  
002406                                                                          
002407     SELECT W233272                    ASSIGN TO W23328D2.                
002408*          --- INPUT FILE FROM PGM W23327 - EXTRACT INFO                  
002409                                                                          
002430     SELECT W233281                    ASSIGN TO W23328D3.                
002431*          --- EXCTRACT FILE - HERCULES SYSTEM                            
002432                                                                          
002433     SELECT W233282                    ASSIGN TO W23328D4.                
002434*          --- ERROR FILE                                                 
002435                                                                          
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
003017 FILE SECTION.                                                            
003018     SKIP3                                                                
003019 FD  W233271                                                              
003020     RECORDING       F                                                    
003030     BLOCK CONTAINS  0.                                                   
003040                                                                          
003041*01  RECORD -COPY W23327A -PRE IN-START-     -L.                          
003060                                                                          
003061 FD  W233272                                                              
003062     RECORDING       F                                                    
003063     BLOCK CONTAINS  0.                                                   
003064                                                                          
003065*01  RECORD -COPY W23327B -PRE IN-           -L.                          
003300                                                                          
003400 FD  W233281                                                              
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  RECORD -COPY W23327  -PRE  OUT-         -L.                          
003801                                                                          
003810 FD  W233282                                                              
003820     RECORDING       F                                                    
003830     BLOCK CONTAINS  0.                                                   
003840                                                                          
003850 01  ERR-START-RECORD      PIC X(120).                                    
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W2332800'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004411 77  WS-START-IDPOST-NUM         PIC 9(03)   VALUE ZERO.                  
004412 77  WS-START-IDPOST             PIC 9(03)   VALUE ZERO.                  
004413 77  WS-START-KVPOST-TOT         PIC 9(03)   VALUE ZERO.                  
004420 77  WS-START-RECORD-OK          PIC X(01)   VALUE SPACE.                 
004430 01  WS-ERR-TEXT.                                                         
004432     05 FILLER                   PIC X(07)   VALUE 'REC NO'.              
004433     05 ERR-IDPOST               PIC 9(03).                               
004435     05 FILER                    PIC X(12)   VALUE ' REC TOT NO'.         
004436     05 ERR-KVPOST-TOT           PIC 9(03).                               
004437     05 FILLER                   PIC X(03)   VALUE ' - '.                 
004438     05 ERR-TEXT                 PIC X(65).                               
004439 01  ERR-TEXT1.                                                           
004440     05 FILLER                   PIC X(65)  VALUE                         
004441     ' NOO RECORDS ARE SENT FROM HERCULES SYSTEM'.                        
004442 01  ERR-TEXT2.                                                           
004443     05 FILLER                   PIC X(65)  VALUE                         
004444     ' THE RECORDS ARE NOT SENT IN SEQUENCE'.                             
004445 01  ERR-TEXT3.                                                           
004446     05 FILLER                   PIC X(35)   VALUE                        
004450        ' THE SUM OF RECORDS DOES NOT MATCH'.                             
004452     05 FILLER                   PIC X(28)   VALUE                        
004453        'THE TOTAL NUMBERS OF RECORDS'.                                   
004500                                                                          
004510 77  W233271-EOF-SW              PIC X       VALUE 'N'.                   
004520     88  END-OF-W233271                      VALUE 'J'.                   
004522 77  W233272-EOF-SW              PIC X       VALUE 'N'.                   
004523     88  END-OF-W233272                      VALUE 'J'.                   
004530     EJECT                                                                
004540 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004550 01  FILLER REDEFINES TODAYS-DATE.                                        
004560     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004570     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004580     03  TODAYS-DATE-DAY         PIC 9(2).                                
004590     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  FILLER                      PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006206*01  AREA -COPY W23327A    -PRE IN-START-                                 
006207     EJECT                                                                
006209 01  FILLER                      PIC X(24)   VALUE                        
006210                                 'IN-AREA        '.                       
006211*01  AREA -COPY W23327B    -PRE IN-                                       
006212     EJECT                                                                
006310 01  FILLER                      PIC X(24)   VALUE                        
006320                                 'OUT-AREA        '.                      
006350*01  AREA -COPY W23327     -PRE OUT-                                      
006360     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006800                                                                          
006900     PERFORM A-INIT                                                       
007000                                                                          
007001     PERFORM B-CHECK-START-RECORD                                         
007002                                                                          
007003     IF WS-START-RECORD-OK = YES                                          
007004        PERFORM S01-READ-W233272                                          
007012        PERFORM UNTIL END-OF-W233272                                      
007014           MOVE IN-KDPGMMRK     TO OUT-KDPGMMRK                           
007015           MOVE IN-IDLANDX2     TO OUT-IDLANDX2                           
007016           MOVE IN-IDFABRIK     TO OUT-IDFABRIK                           
007017           MOVE IN-IDARTNR      TO OUT-IDARTNR                            
007018           MOVE IN-DAAAVV       TO OUT-DAAAVV                             
007019           MOVE IN-VLPART       TO OUT-VLPART                             
007021           PERFORM S11A-WRITE-W233281                                     
007022           PERFORM S01-READ-W233272                                       
007030        END-PERFORM                                                       
008100     END-IF                                                               
008120                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO                   TO RETURN-CODE                           
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  W233271                                                  
008920                 W233272                                                  
009010     OPEN OUTPUT W233281                                                  
009020                 W233282                                                  
009100     SKIP2                                                                
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009510 B-CHECK-START-RECORD   SECTION.                                          
009520                                                                          
009531     MOVE YES                        TO WS-START-RECORD-OK                
009532     PERFORM S01-READ-W233271                                             
009533     IF END-OF-W233271                                                    
009534       MOVE ZERO                     TO ERR-IDPOST                        
009535       MOVE ZERO                     TO ERR-KVPOST-TOT                    
009536       MOVE ERR-TEXT1                TO ERR-TEXT                          
009538       PERFORM S11B-WRITE-W233282                                         
009539       MOVE NOO                      TO WS-START-RECORD-OK                
009540     ELSE                                                                 
009542       MOVE IN-START-IDPOST          TO WS-START-IDPOST                   
009543       MOVE IN-START-KVPOST-TOT      TO WS-START-KVPOST-TOT               
009544       PERFORM S01-READ-W233271                                           
009546       PERFORM UNTIL END-OF-W233271                                       
009550         MOVE IN-START-IDPOST        TO WS-START-IDPOST-NUM               
009551         IF WS-START-IDPOST-NUM NOT = WS-START-IDPOST + 1                 
009552           MOVE WS-START-IDPOST      TO ERR-IDPOST                        
009553           MOVE WS-START-KVPOST-TOT  TO ERR-KVPOST-TOT                    
009554           MOVE ERR-TEXT2            TO ERR-TEXT                          
009555           PERFORM S11B-WRITE-W233282                                     
009556           MOVE NOO                  TO WS-START-RECORD-OK                
009557         END-IF                                                           
009558         MOVE IN-START-IDPOST        TO WS-START-IDPOST                   
009559         MOVE IN-START-KVPOST-TOT    TO WS-START-KVPOST-TOT               
009560         PERFORM S01-READ-W233271                                         
009561       END-PERFORM                                                        
009562                                                                          
009564       IF WS-START-RECORD-OK = YES                                        
009565          IF WS-START-IDPOST NOT = WS-START-KVPOST-TOT                    
009566            MOVE WS-START-IDPOST     TO ERR-IDPOST                        
009567            MOVE WS-START-KVPOST-TOT TO ERR-KVPOST-TOT                    
009568            MOVE ERR-TEXT3           TO ERR-TEXT                          
009569            PERFORM S11B-WRITE-W233282                                    
009570            MOVE NOO                 TO WS-START-RECORD-OK                
009571          END-IF                                                          
009572       END-IF                                                             
009573     END-IF                                                               
009574     .                                                                    
009580     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W233271                                                        
009702           W233272                                                        
009710           W233281                                                        
009720           W233282                                                        
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-W233271  SECTION.                                               
010003                                                                          
010004     READ W233271  INTO IN-START-AREA                                     
010005     AT END                                                               
010006        MOVE HIGH-VALUE TO IN-START-AREA                                  
010007        SET END-OF-W233271 TO TRUE                                        
010008                                                                          
010009     NOT AT END                                                           
010010        MOVE 'W233271'  TO POSTSUM-FDNAMN                                 
010011        MOVE 'W23328D1' TO POSTSUM-DDNAMN2                                
010012*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
010013        MOVE SPACE     TO POSTSUM-TRANSTYP                                
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S01-READ-W233272  SECTION.                                               
010103                                                                          
010104     READ W233272  INTO IN-AREA                                           
010105     AT END                                                               
010106        MOVE HIGH-VALUE TO IN-AREA                                        
010107        SET END-OF-W233272 TO TRUE                                        
010108                                                                          
010109     NOT AT END                                                           
010110        MOVE 'W233272'  TO POSTSUM-FDNAMN                                 
010111        MOVE 'W23328D2' TO POSTSUM-DDNAMN2                                
010112*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
010113        MOVE SPACE     TO POSTSUM-TRANSTYP                                
010114        CALL POSTSUM USING POSTSUM-PARM                                   
010115     END-READ                                                             
010116     .                                                                    
010117     EJECT                                                                
010310 S11A-WRITE-W233281 SECTION.                                              
010320                                                                          
010330     WRITE OUT-RECORD FROM OUT-AREA                                       
010340                                                                          
010350     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010360     MOVE 'W233281'  TO POSTSUM-FDNAMN                                    
010370     MOVE 'W23328D3' TO POSTSUM-DDNAMN2                                   
010380     CALL POSTSUM USING POSTSUM-PARM                                      
010390     .                                                                    
010391     EJECT                                                                
010392 S11B-WRITE-W233282  SECTION.                                             
010393                                                                          
010394     WRITE ERR-START-RECORD FROM WS-ERR-TEXT                              
010395                                                                          
010396     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010397     MOVE 'W233282'  TO POSTSUM-FDNAMN                                    
010398     MOVE 'W23328D4' TO POSTSUM-DDNAMN2                                   
010399     CALL POSTSUM USING POSTSUM-PARM                                      
010400     .                                                                    
010401     EJECT                                                                
010410 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    

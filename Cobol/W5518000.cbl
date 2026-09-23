000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5518000.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/07/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM MATCHES WDL222 FILE WITH DIRECT SUPPLIERS           
001000*        IN WWLEV06                                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- WDL222 RECORDS                                             
002500     SELECT W55178                     ASSIGN TO W55180D1.                
002600     SKIP2                                                                
002700*          --- MATCHED DIRECT SUPPLIERS                                   
002800     SELECT W55180                     ASSIGN TO W55180D3.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W55178                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W55178      -L.                                                
003900     SKIP3                                                                
004000 FD  W55180                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  RECORD -COPY W55178 -PRE  UT-  -L.                                   
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W5518000'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  W55178-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W55178                       VALUE 'J'.                   
005400                                                                          
005500     EJECT                                                                
005600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES TODAYS-DATE.                                        
005800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006000     03  TODAYS-DATE-DAY         PIC 9(2).                                
006100     EJECT                                                                
006200 01  WS-VALID-DATE               PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES WS-VALID-DATE.                                      
006400     03  WS-VALID-YEAR           PIC 9(2).                                
006500     03  WS-VALID-MONTH          PIC 9(2).                                
006600     03  WS-VALID-DAY            PIC 9(2).                                
006700     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     SKIP2                                                                
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  ERROR-TEXT.                                                          
008000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  IN1-AREA-START              PIC X(24)   VALUE                        
008800                                 'IN1-AREA-START  '.                      
008900     SKIP2                                                                
009000                                                                          
009100*01  AREA -COPY W55178     -PRE IN1-                                      
009200     EJECT                                                                
009300 01  UT-AREA-START               PIC X(24)   VALUE                        
009400                                 'UT-AREA-START  '.                       
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W55178     -PRE UT-                                       
009800     EJECT                                                                
009900*  --- VALID DDGS                                                         
010000*01  -COPY WWLEV06                                                        
010100 PROCEDURE DIVISION.                                                      
010200 MAIN SECTION.                                                            
010300     SKIP2                                                                
010400                                                                          
010500     PERFORM A-INIT                                                       
010600     PERFORM S01-READ-W55178                                              
010700     PERFORM UNTIL END-OF-W55178                                          
010800        PERFORM B-MATCH-DIRLEV                                            
010900     END-PERFORM                                                          
011000                                                                          
011100     PERFORM Z-FINIT                                                      
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
010400                                                                          
011600 A-INIT SECTION.                                                          
011700                                                                          
011800     OPEN INPUT  W55178                                                   
011900                                                                          
012000     OPEN OUTPUT W55180                                                   
012100     SKIP2                                                                
012200     ACCEPT TODAYS-DATE  FROM DATE                                        
012300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012400     MOVE TODAYS-DATE-YEAR TO WS-VALID-YEAR                               
012500     MOVE '01'             TO WS-VALID-MONTH                              
012600     MOVE '01'             TO WS-VALID-DAY                                
012700     .                                                                    
012800     EJECT                                                                
012900 B-MATCH-DIRLEV SECTION.                                                  
013000     MOVE IN1-IDLEVNR      TO LEV06-IDLEVNR                               
013100     IF LEV06-DDGS                                                        
013200       PERFORM BA-MOVE-FIELDS                                             
013300     END-IF                                                               
013400     PERFORM S01-READ-W55178                                              
013500     .                                                                    
013600     EJECT                                                                
013700 BA-MOVE-FIELDS SECTION.                                                  
013800                                                                          
013900     IF IN1-IDPTYP = 'R33'                                                
014000        IF IN1-TIAVSDAT >= WS-VALID-DATE                                  
014100        AND IN1-TIAVSDAT <= TODAYS-DATE                                   
014200           MOVE IN1-IDARTNR   TO UT-IDARTNR                               
014300           MOVE IN1-IDPTYP    TO UT-IDPTYP                                
014400           MOVE IN1-IDLEVNR   TO UT-IDLEVNR                               
014500           MOVE IN1-KVAVIS    TO UT-KVAVIS                                
014600           MOVE IN1-TIAVSDAT  TO UT-TIAVSDAT                              
014700           PERFORM S11-WRITE-W55180                                       
014800        END-IF                                                            
014900     END-IF                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 Z-FINIT SECTION.                                                         
015300     CLOSE W55178                                                         
015400           W55180                                                         
015500     SKIP2                                                                
015600     MOVE 'S' TO POSTSUM-OPKOD                                            
015700     CALL POSTSUM USING POSTSUM-PARM                                      
015800     .                                                                    
015900     EJECT                                                                
016000 S01-READ-W55178  SECTION.                                                
016100     READ W55178 INTO IN1-AREA                                            
016200     AT END                                                               
016300        MOVE HIGH-VALUE TO IN1-AREA                                       
016400        SET END-OF-W55178 TO TRUE                                         
016500                                                                          
016600     NOT AT END                                                           
016700        MOVE 'W55178' TO POSTSUM-FDNAMN                                   
016800        MOVE 'W55180D1' TO POSTSUM-DDNAMN2                                
016900        MOVE IN1-IDPTYP TO POSTSUM-TRANSTYP                               
017000        CALL POSTSUM USING POSTSUM-PARM                                   
017100     END-READ                                                             
017200     .                                                                    
017300     EJECT                                                                
017400 S11-WRITE-W55180 SECTION.                                                
017500                                                                          
017600     WRITE UT-RECORD FROM UT-AREA                                         
017700                                                                          
017800     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
017900     MOVE 'W55180' TO POSTSUM-FDNAMN                                      
018000     MOVE 'W55180D3' TO POSTSUM-DDNAMN2                                   
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018300     EJECT                                                                
018400 S99-ABEND SECTION.                                                       
018500                                                                          
018600     SKIP2                                                                
018700     MOVE 'S' TO POSTSUM-OPKOD                                            
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     CALL ABEND USING RKOD-ABEND                                          
019000     .                                                                    

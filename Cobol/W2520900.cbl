000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2520900.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   19/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PART INFORMATION - PURCHASE PLANNING                             
001000*                                                                         
001100*    ABENDCODES:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INFIL                                                      
002400     SELECT W01184                     ASSIGN TO W25209D1.                
002500     SKIP2                                                                
002600*          --- UTFIL                                                      
002700     SELECT W25209                     ASSIGN TO W25209D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W01184                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W01184      -L.                                                
003800     SKIP3                                                                
003900 FD  W25209                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  RECORD -COPY W25209 -PRE  OUT-  -L.                                  
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2520900'.            
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000 77  IX1                         PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  MAX-IX1-DC                  PIC S9(9)   VALUE +6   COMP SYNC.        
005200 77  W-WRITE-W25209              PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W01184                       VALUE 'J'.                   
005600     EJECT                                                                
005700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES TODAYS-DATE.                                        
005900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006100     03  TODAYS-DATE-DAY         PIC 9(2).                                
006200     EJECT                                                                
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETERS TO ABEND                                              
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERROR-TEXT.                                                          
007500     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- VALID IDDC CODES                                                 
007900*                                                                         
008000*01  -COPY WWDC99                                                         
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  IN-AREA-START               PIC X(24)   VALUE                        
008700                                 'IN-AREA-START  '.                       
008800*01  AREA -COPY W01184     -PRE IN-                                       
008900     EJECT                                                                
009000 01  OUT-AREA-START              PIC X(24)   VALUE                        
009100                                 'OUT-AREA-START  '.                      
009200*01  AREA -COPY W25209     -PRE OUT- INGER                                
009300     EJECT                                                                
009400 PROCEDURE DIVISION.                                                      
009500 MAIN SECTION.                                                            
009600                                                                          
009700     PERFORM A-INIT                                                       
009800     PERFORM S01-READ-W01184                                              
009900     INITIALIZE OUT-AREA                                                  
010000     PERFORM S02-INFO-FROM-W01184                                         
010100     MOVE  IN-SLAG-IDARTNR        TO OUT-IDARTNR                          
010200                                                                          
010300     PERFORM UNTIL END-OF-W01184                                          
010400       IF IN-SLAG-IDARTNR NOT = OUT-IDARTNR                               
010500          IF W-WRITE-W25209 = YES                                         
010600             PERFORM S11-WRITE-W25209                                     
010700          END-IF                                                          
010800          MOVE NOO TO W-WRITE-W25209                                      
010900          INITIALIZE OUT-AREA                                             
011000       END-IF                                                             
011100                                                                          
011200       PERFORM S02-INFO-FROM-W01184                                       
011300                                                                          
011400       PERFORM S01-READ-W01184                                            
011500     END-PERFORM                                                          
011600                                                                          
011700     IF W-WRITE-W25209 = YES                                              
011800        PERFORM S11-WRITE-W25209                                          
011900     END-IF                                                               
012000                                                                          
012100     PERFORM Z-FINIT                                                      
012200                                                                          
012300     MOVE ZERO TO RETURN-CODE                                             
012400     GOBACK                                                               
012500     .                                                                    
012600     EJECT                                                                
012700 A-INIT SECTION.                                                          
012800                                                                          
012900     OPEN INPUT  W01184                                                   
013000                                                                          
013100     OPEN OUTPUT W25209                                                   
013200     SKIP2                                                                
013300     ACCEPT TODAYS-DATE  FROM DATE                                        
013400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013500     .                                                                    
013600     EJECT                                                                
013700 S02-INFO-FROM-W01184        SECTION.                                     
013800                                                                          
013900     MOVE  IN-SLAG-IDARTNR        TO OUT-IDARTNR                          
014000                                                                          
014100     MOVE IN-SLAG-IDDC            TO WS-IDDC                              
014200     MOVE +0                      TO IX1                                  
014300     IF NDC-US-RU                                                         
014400       MOVE +1                    TO IX1                                  
014500     ELSE                                                                 
014600       IF NDC-JP                                                          
014700         MOVE +2                  TO IX1                                  
014800       ELSE                                                               
014900         IF NDC-AU                                                        
015000           MOVE +3                TO IX1                                  
015100         ELSE                                                             
015200           IF NDC-IN                                                      
015300             MOVE +4              TO IX1                                  
015400           ELSE                                                           
015500             IF NDC-CN-71                                                 
015600               MOVE +5            TO IX1                                  
015700             ELSE                                                         
015800               IF NDC-CN-72                                               
015900                 MOVE +6          TO IX1                                  
016000               END-IF                                                     
016100             END-IF                                                       
016200           END-IF                                                         
016300         END-IF                                                           
016400       END-IF                                                             
016500     END-IF                                                               
016600                                                                          
016700     IF IX1 > +0                                                          
016800       MOVE YES                   TO W-WRITE-W25209                       
016900       IF IN-SLAG-IDLANDX2 = 'US'                                         
017000          MOVE IN-SLAG-DAPUBL     TO OUT-DAPUBL-US                        
017100       ELSE                                                               
017200          IF IN-SLAG-IDLANDX2 = 'CN'                                      
017300             MOVE IN-SLAG-DAPUBL  TO OUT-DAPUBL-CN                        
017400          END-IF                                                          
017500       END-IF                                                             
017600       MOVE IN-SLAG-IDANSK        TO OUT-IDANSK-SLAG    (IX1)             
017700       MOVE IN-SLAG-IDARTNR-EMBQ0 TO OUT-IDARTNR-EMBQ0-SLAG(IX1)          
017800       MOVE IN-SLAG-IDDC          TO OUT-IDDC-SLAG      (IX1)             
017900       MOVE IN-SLAG-IDINK         TO OUT-IDINK-SLAG     (IX1)             
018000       MOVE IN-SLAG-IDLEVNR       TO OUT-IDLEVNR-SLAG   (IX1)             
018100       MOVE IN-SLAG-KDAVT         TO OUT-KDAVT-SLAG     (IX1)             
018200       MOVE IN-SLAG-KDARTURS      TO OUT-KDARTURS-SLAG  (IX1)             
018300       MOVE IN-SLAG-KDMATRPR      TO OUT-KDMATRPR-SLAG  (IX1)             
018400       MOVE IN-SLAG-KVAKS-SDC     TO OUT-KVAKS-SDC-SLAG (IX1)             
018500       MOVE IN-SLAG-KVBEART       TO OUT-KVBEART-SLAG   (IX1)             
018600       MOVE IN-SLAG-KVLS          TO OUT-KVLS-SLAG      (IX1)             
018700       MOVE IN-SLAG-KVOKS-BULK    TO OUT-KVOKS-BULK-SLAG(IX1)             
018800       MOVE IN-SLAG-KVOKS-DAG     TO OUT-KVOKS-DAG-SLAG (IX1)             
018900       MOVE IN-SLAG-KVROS-BULK    TO OUT-KVROS-BULK-SLAG(IX1)             
019000       MOVE IN-SLAG-KVROS-DAG     TO OUT-KVROS-DAG-SLAG (IX1)             
019100       MOVE IN-SLAG-KVRESS        TO OUT-KVRESS-SLAG    (IX1)             
019200     END-IF                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 Z-FINIT SECTION.                                                         
019600     CLOSE W01184                                                         
019700           W25209                                                         
019800     SKIP2                                                                
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
020300 S01-READ-W01184  SECTION.                                                
020400                                                                          
020500     READ W01184 INTO IN-AREA                                             
020600     AT END                                                               
020700        MOVE HIGH-VALUE TO IN-AREA                                        
020800        SET END-OF-W01184 TO TRUE                                         
020900                                                                          
021000     NOT AT END                                                           
021100        MOVE 'W01184'   TO POSTSUM-FDNAMN                                 
021200        MOVE 'W25209D1' TO POSTSUM-DDNAMN2                                
021300*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
021400        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
021500        CALL POSTSUM USING POSTSUM-PARM                                   
021600     END-READ                                                             
021700     .                                                                    
021800     EJECT                                                                
021900 S11-WRITE-W25209 SECTION.                                                
022000                                                                          
022100     WRITE OUT-RECORD FROM OUT-AREA                                       
022200                                                                          
022300     MOVE 'OUT'      TO POSTSUM-TRANSTYP                                  
022400     MOVE 'W25209'   TO POSTSUM-FDNAMN                                    
022500     MOVE 'W25209D2' TO POSTSUM-DDNAMN2                                   
022600     CALL POSTSUM USING POSTSUM-PARM                                      
022700     .                                                                    
022800     EJECT                                                                
022900 S99-ABEND SECTION.                                                       
023000                                                                          
023100     SKIP2                                                                
023200     MOVE 'S' TO POSTSUM-OPKOD                                            
023300     CALL POSTSUM USING POSTSUM-PARM                                      
023400     CALL ABEND USING RKOD-ABEND                                          
023500     .                                                                    

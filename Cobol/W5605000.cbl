000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5605000.                                                
000300 AUTHOR.         BHAT ARCHANA.                                            
000400 DATE-WRITTEN.   20/02/20.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        MATCHES WDK6 AND WDK7 PARTS                                      
001000*                                                                         
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- WDK6 DATA                                                  
002500     SELECT W01160                     ASSIGN TO W56050D1.                
002600     SKIP2                                                                
002700*          --- WDK7 DATA                                                  
002800     SELECT W01184                     ASSIGN TO W56050D2.                
002900     SKIP2                                                                
003000*          --- MATCHED PARTS                                              
003100     SELECT W56050                     ASSIGN TO W56050D3.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W01160                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W01160      -L.                                                
004200     SKIP3                                                                
004300 FD  W01184                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W01184      -L.                                                
004800     SKIP3                                                                
004900 FD  W56050                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  RECORD -COPY W56050 -PRE  UT-  -L.                                   
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5605000'.            
005800 77  YES                         PIC X       VALUE 'J'.                   
005900 77  NOO                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W01160                       VALUE 'J'.                   
006300                                                                          
006400 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W01184                       VALUE 'J'.                   
006600     EJECT                                                                
006700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES TODAYS-DATE.                                        
006900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007100     03  TODAYS-DATE-DAY         PIC 9(2).                                
007200     EJECT                                                                
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     SKIP2                                                                
007800*    --- PARAMETERS TO ABEND                                              
007900                                                                          
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300     SKIP2                                                                
008400 01  ERROR-TEXT.                                                          
008500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200*01  -COPY WWDC99                                                         
009300     EJECT                                                                
009400 01  IN-AREA-START             PIC X(24)   VALUE                          
009500                                 'IN-AREA-START  '.                       
009600     SKIP2                                                                
009700                                                                          
009800 01  IN-AREA1.                                                            
009900*    03  -COPY W01160                                                     
010000     EJECT                                                                
010100 01  IN2-AREA-START             PIC X(24)   VALUE                         
010200                                 'IN2-AREA-START  '.                      
010300     SKIP2                                                                
010400                                                                          
010500 01  IN-AREA2.                                                            
010600*    03  -COPY W01184                                                     
010700     EJECT                                                                
010800 01  UT-AREA-START               PIC X(24)   VALUE                        
010900                                 'UT-AREA-START  '.                       
011000     SKIP2                                                                
011100                                                                          
011200 01  UT-AREA.                                                             
011300*    03  -COPY W56050   -PRE UT-                                          
011400     EJECT                                                                
011500                                                                          
011600 PROCEDURE DIVISION.                                                      
011700 MAIN SECTION.                                                            
011800     SKIP2                                                                
011900                                                                          
012000     PERFORM A-INIT                                                       
012100     PERFORM S01-READ-W01160                                              
012200     PERFORM S02-READ-W01184                                              
012300     PERFORM UNTIL END-OF-W01160 OR END-OF-W01184                         
012400       EVALUATE TRUE                                                      
012500         WHEN CLAG-IDARTNR = SLAG-IDARTNR                                 
012600           PERFORM UNTIL END-OF-W01184                                    
012700           OR SLAG-IDARTNR NOT = CLAG-IDARTNR                             
012800             PERFORM B-PROCESS                                            
012900             PERFORM S02-READ-W01184                                      
013000           END-PERFORM                                                    
013100           PERFORM S01-READ-W01160                                        
013200         WHEN CLAG-IDARTNR > SLAG-IDARTNR                                 
013300           PERFORM S02-READ-W01184                                        
013400         WHEN CLAG-IDARTNR < SLAG-IDARTNR                                 
013500           PERFORM S01-READ-W01160                                        
013600       END-EVALUATE                                                       
013700     END-PERFORM                                                          
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000                                                                          
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500 A-INIT SECTION.                                                          
014600                                                                          
014700     OPEN INPUT  W01160                                                   
014800                 W01184                                                   
014900                                                                          
015000     OPEN OUTPUT W56050                                                   
015100     SKIP2                                                                
015200     ACCEPT TODAYS-DATE  FROM DATE                                        
015300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015400     .                                                                    
015500     EJECT                                                                
015600 B-PROCESS SECTION.                                                       
015700                                                                          
015800     MOVE SLAG-IDDC       TO WS-IDDC                                      
015900     IF NDC-NA OR LDC-CN OR XDC-NON-VCC-OWNED                             
016000       MOVE CLAG-IDARTNR    TO UT-IDARTNR                                 
016100       MOVE CLAG-KDERS      TO UT-KDERS                                   
016200       MOVE CLAG-KDPSLLOC   TO UT-KDPSLLOC                                
016300       MOVE CLAG-KDPRODSL   TO UT-KDPRODSL                                
016400       MOVE CLAG-TIFINLV    TO UT-TIFINLV                                 
016500       MOVE SLAG-IDDC       TO UT-IDDC                                    
016600       MOVE SLAG-KVPB-REF   TO UT-KVPB-REF                                
016700       MOVE SLAG-KVEFRS     TO UT-KVEFRS                                  
016800       MOVE SLAG-KVLS       TO UT-KVLS                                    
016900       MOVE SLAG-PRAVCOST   TO UT-PRAVCOST                                
017000       PERFORM S11-WRITE-W56050                                           
017100     END-IF                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 Z-FINIT SECTION.                                                         
017500     CLOSE W01160                                                         
017600           W01184                                                         
017700           W56050                                                         
017800     SKIP2                                                                
017900     MOVE 'S' TO POSTSUM-OPKOD                                            
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018200     EJECT                                                                
018300 S01-READ-W01160  SECTION.                                                
018400     READ W01160 INTO IN-AREA1                                            
018500     AT END                                                               
018600        MOVE HIGH-VALUE TO IN-AREA1                                       
018700        SET END-OF-W01160 TO TRUE                                         
018800                                                                          
018900     NOT AT END                                                           
019000        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
019100        MOVE 'W56050D1' TO POSTSUM-DDNAMN2                                
019200        MOVE SPACE     TO POSTSUM-TRANSTYP                                
019300        CALL POSTSUM USING POSTSUM-PARM                                   
019400     END-READ                                                             
019500     .                                                                    
019600     EJECT                                                                
019700 S02-READ-W01184  SECTION.                                                
019800     READ W01184 INTO IN-AREA2                                            
019900     AT END                                                               
020000        MOVE HIGH-VALUE TO IN-AREA2                                       
020100        SET END-OF-W01184 TO TRUE                                         
020200                                                                          
020300     NOT AT END                                                           
020400        MOVE 'W01184' TO POSTSUM-FDNAMN                                   
020500        MOVE 'W56050D2' TO POSTSUM-DDNAMN2                                
020600        MOVE SPACE      TO POSTSUM-TRANSTYP                               
020700        CALL POSTSUM USING POSTSUM-PARM                                   
020800     END-READ                                                             
020900     .                                                                    
021000     EJECT                                                                
021100 S11-WRITE-W56050 SECTION.                                                
021200                                                                          
021300     WRITE UT-RECORD FROM UT-AREA                                         
021400                                                                          
021500     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
021600     MOVE 'W56050' TO POSTSUM-FDNAMN                                      
021700     MOVE 'W56050D3' TO POSTSUM-DDNAMN2                                   
021800     CALL POSTSUM USING POSTSUM-PARM                                      
021900     .                                                                    
022000     EJECT                                                                

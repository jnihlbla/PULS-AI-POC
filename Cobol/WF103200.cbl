000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF103200.                                                
000400 AUTHOR.         BHAT ARCHANA.                                            
000500 DATE-WRITTEN.   20/09/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        READS T01CUYE TABLE TO FETCH THE YEARLY CURRENCY RATES.          
001100* THE LAST RUN OF THE PROGRAM IS SAVED IN THE WF1032A                     
001110* FILE. THE RATES UPDATED/INSERTED AFTER THIS DATE ARE FETCHED.           
001120* THE CURRENT DATE IS UPDATED IN THE WF1032A FILE AFTER PROCESSING        
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
002400     SKIP2                                                                
002610*          --- DATE FILE FOR SELECTION                                    
002620     SELECT WF1032A                    ASSIGN TO WF1032D1.                
002630*          --- UPDATED DATE FILE                                          
002640     SELECT WF1032B                    ASSIGN TO WF1032D2.                
002650*          --- T01CUYE DATA                                               
002660     SELECT WF1032                     ASSIGN TO WF1032D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003710 FD  WF1032A                                                              
003720     RECORDING       F                                                    
003730     BLOCK CONTAINS  0.                                                   
003740                                                                          
003750 01  WF1032A-REC   PIC X(80).                                             
003760     EJECT                                                                
003770 FD  WF1032B                                                              
003780     RECORDING       F                                                    
003790     BLOCK CONTAINS  0.                                                   
003791                                                                          
003792 01  WF1032B-REC   PIC X(80).                                             
003793     EJECT                                                                
003794 FD  WF1032                                                               
003795     RECORDING       F                                                    
003796     BLOCK CONTAINS  0.                                                   
003797                                                                          
003798*01  RECORD -COPY WF1032 -PRE  UT02-  -L.                                 
003799     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'WF103200'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004400     EJECT                                                                
004410 77  WF1032A-EOF-SW               PIC X      VALUE 'N'.                   
004420     88  END-OF-WF1032A                      VALUE 'J'.                   
004430     EJECT                                                                
004500 01  GENERAL-SUBPROGRAMS.                                                 
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004900     SKIP2                                                                
005000*    --- PARAMETRAR TILL ABEND                                            
005100*                                                                         
005200 77  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
005210 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005300     EJECT                                                                
005400     SKIP2                                                                
005500 01  ERROR-TEXT.                                                          
005600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005900*    --- PARAMETRAR TILL POSTSUM                                          
006000*                                                                         
006100*01  -COPY W0005   -PRE  POSTSUM-                                         
006200     EJECT                                                                
006270 01  IN-AREA.                                                             
006271     03 IN-DATE                  PIC X(8).                                
006280     EJECT                                                                
006300 01  UT-AREA-START               PIC X(24)   VALUE                        
006400                                 'UT-AREA-START  '.                       
006500     SKIP2                                                                
006510 01  UT01-AREA                   PIC X(08).                               
006600                                                                          
006700*01  AREA -COPY WF1032     -PRE UT02-                                     
006800     EJECT                                                                
006900*    --- WORK-AREAS FOR DB2-SECTIONS                                      
007000 01  FILLER                       PIC X(16)  VALUE 'CUYE-TAB   '.         
007100*01  -COPY T01CUYE    -PRE CUYE-                                          
007200                                                                          
007300 01  FILLER                       PIC X(16)  VALUE 'CUYE-AREA'.           
007400       EXEC SQL INCLUDE T01CUYE  END-EXEC.                                
007500                                                                          
007600 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
007700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
007800                                                                          
007900*                        **** STATUS-CODE FROM DB2                        
008000 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
008100 01  DB2-WS.                                                              
008200   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
008300     88  LINES-FOUND                         VALUE +000.                  
008400     88  LINES-MISSING                       VALUE +100.                  
008500     88  RESOURCE-WRONG                      VALUE 904.                   
008600   03  GOOD-SQLCODES.                                                     
008700     05  GOOD-SQLCODE OCCURS 5                                            
008800         INDEXED BY SQLCODE-IX    PIC 999.                                
008900 PROCEDURE DIVISION.                                                      
009000 MAIN SECTION.                                                            
009100     SKIP2                                                                
009200                                                                          
009300     PERFORM A-INIT                                                       
009310     PERFORM S01-READ-WF1032A                                             
009400     PERFORM DB2-OPEN-CRS-CUYE                                            
009500     PERFORM DB2-FETCH-CRS-CUYE                                           
009600     PERFORM UNTIL LINES-MISSING                                          
009700                                                                          
009800       PERFORM B-MOVE-DATA                                                
009900       PERFORM DB2-FETCH-CRS-CUYE                                         
010000                                                                          
010100     END-PERFORM                                                          
010200     PERFORM DB2-CLOSE-CRS-CUYE                                           
010210                                                                          
010220     PERFORM S11-WRITE-WF1032B                                            
010230                                                                          
010400     PERFORM Z-FINIT                                                      
010500                                                                          
010600     MOVE ZERO TO RETURN-CODE                                             
010700     GOBACK                                                               
010800     .                                                                    
010900     EJECT                                                                
011000 A-INIT SECTION.                                                          
011100                                                                          
011200     OPEN INPUT  WF1032A                                                  
011210          OUTPUT WF1032B                                                  
011220                 WF1032                                                   
011300     SKIP2                                                                
011400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011570     MOVE FUNCTION CURRENT-DATE (1:8)  TO UT01-AREA                       
011600     .                                                                    
011700     EJECT                                                                
011800 B-MOVE-DATA SECTION.                                                     
011900                                                                          
012000     MOVE CUYE-IDLEGSEL        TO UT02-IDLEGSEL                           
012100     MOVE CUYE-KDVALISO        TO UT02-KDVALISO                           
012200     MOVE CUYE-DASTADAT        TO UT02-DASTADAT                           
012300     MOVE CUYE-PRKURS-NEW      TO UT02-PRKURS-NEW                         
012400     MOVE CUYE-REVALUTA-FROM   TO UT02-REVALUTA-FROM                      
012500     MOVE CUYE-REVALUTA-TO     TO UT02-REVALUTA-TO                        
012600     MOVE CUYE-DAREGDAT        TO UT02-DAREGDAT                           
012700     MOVE CUYE-DAUPPDAT        TO UT02-DAUPPDAT                           
012800     MOVE CUYE-DADELDAT        TO UT02-DADELDAT                           
012900     MOVE CUYE-IDUSER          TO UT02-IDUSER                             
013000                                                                          
013100     PERFORM S12-WRITE-WF1032                                             
013200     .                                                                    
013300     EJECT                                                                
013400 Z-FINIT SECTION.                                                         
013500     CLOSE WF1032                                                         
013510           WF1032A                                                        
013520           WF1032B                                                        
013600     SKIP2                                                                
013700     MOVE 'S' TO POSTSUM-OPKOD                                            
013800     CALL POSTSUM USING POSTSUM-PARM                                      
013900     .                                                                    
014000     EJECT                                                                
014010 S01-READ-WF1032A  SECTION.                                               
014020     SKIP2                                                                
014021*** READ THE DATE OF LAST RUN OF PROGRAM                                  
014030     READ WF1032A INTO IN-AREA                                            
014040     AT END                                                               
014050        SET END-OF-WF1032A TO TRUE                                        
014060                                                                          
014070     NOT AT END                                                           
014080        MOVE 'WF1032A' TO POSTSUM-FDNAMN                                  
014090        MOVE 'WF1032D1' TO POSTSUM-DDNAMN2                                
014091        MOVE SPACES    TO POSTSUM-TRANSTYP                                
014092        CALL POSTSUM USING POSTSUM-PARM                                   
014093     END-READ                                                             
014094     .                                                                    
014095     EJECT                                                                
014096 S11-WRITE-WF1032B SECTION.                                               
014097*** WRITE THE CURRENT DATE INTO THE FILE FOR THE NEXT RUN OF PGM          
014098                                                                          
014099     WRITE WF1032B-REC FROM UT01-AREA                                     
014100                                                                          
014101     MOVE 'WF1032B' TO POSTSUM-FDNAMN                                     
014102     MOVE 'WF1032D2' TO POSTSUM-DDNAMN2                                   
014103     CALL POSTSUM USING POSTSUM-PARM                                      
014104     .                                                                    
014105     EJECT                                                                
014110 S12-WRITE-WF1032 SECTION.                                                
014200                                                                          
014300     WRITE UT02-RECORD FROM UT02-AREA                                     
014400                                                                          
014500     MOVE 'WF1032' TO POSTSUM-FDNAMN                                      
014600     MOVE 'WF1032D3' TO POSTSUM-DDNAMN2                                   
014700     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
014900     EJECT                                                                
014910*** SELECT CURRENCY RATES THAT HAVE BEEN UPDATED/DELETED AFTER THE        
014920*** LAST RUN OF THE PROGRAM                                               
015000 DB2-OPEN-CRS-CUYE SECTION.                                               
015100     EXEC SQL DECLARE CUYE-CRS CURSOR FOR                                 
015200     SELECT   T01CUYE.IDLEGSEL,                                           
015300              T01CUYE.KDVALISO,                                           
015400              T01CUYE.DASTADAT,                                           
015500              T01CUYE.PRKURS_NEW,                                         
015600              T01CUYE.REVALUTA_FROM,                                      
015700              T01CUYE.REVALUTA_TO,                                        
015800              T01CUYE.DAREGDAT,                                           
015900              T01CUYE.DAUPPDAT,                                           
016000              T01CUYE.DADELDAT,                                           
016100              T01CUYE.IDUSER                                              
016200                                                                          
016300     FROM     T01CUYE                                                     
016400                                                                          
016500     WHERE    T01CUYE.DAUPPDAT > :IN-DATE                                 
016510        OR    T01CUYE.DADELDAT > :IN-DATE                                 
016530        OR    T01CUYE.DAREGDAT > :IN-DATE                                 
016600                                                                          
016700     ORDER BY T01CUYE.IDLEGSEL,                                           
016800              T01CUYE.KDVALISO,                                           
016900              T01CUYE.DASTADAT                                            
017000                                                                          
017100     FOR FETCH ONLY                                                       
017200     END-EXEC                                                             
017300                                                                          
017400     MOVE 000            TO GOOD-SQLCODES                                 
017500     EXEC SQL OPEN CUYE-CRS                                               
017600     END-EXEC                                                             
017700     MOVE SQLCODE        TO SQLCODE-WS                                    
017800     PERFORM DB2-STATUS-CHECK                                             
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200 DB2-FETCH-CRS-CUYE SECTION.                                              
018300     EXEC SQL FETCH CUYE-CRS INTO                                         
018400            :CUYE-IDLEGSEL,                                               
018500            :CUYE-KDVALISO,                                               
018600            :CUYE-DASTADAT,                                               
018700            :CUYE-PRKURS-NEW,                                             
018800            :CUYE-REVALUTA-FROM,                                          
018900            :CUYE-REVALUTA-TO,                                            
019000            :CUYE-DAREGDAT,                                               
019100            :CUYE-DAUPPDAT,                                               
019200            :CUYE-DADELDAT,                                               
019300            :CUYE-IDUSER                                                  
019400     END-EXEC                                                             
019500                                                                          
019600     MOVE 000100         TO GOOD-SQLCODES                                 
019700     MOVE SQLCODE        TO SQLCODE-WS                                    
019800     PERFORM DB2-STATUS-CHECK                                             
019900     .                                                                    
020000     EJECT                                                                
020100                                                                          
020200 DB2-CLOSE-CRS-CUYE SECTION.                                              
020300     EXEC SQL CLOSE CUYE-CRS                                              
020400     END-EXEC                                                             
020500     .                                                                    
020600     EJECT                                                                
020700                                                                          
020800 DB2-STATUS-CHECK SECTION.                                                
020900     SET SQLCODE-IX         TO 1                                          
021000     SEARCH GOOD-SQLCODE AT END                                           
021100           CALL ABEND USING RKOD-ABEND-DB2                                
021200        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
021300           CONTINUE                                                       
021400     END-SEARCH                                                           
021500     .                                                                    

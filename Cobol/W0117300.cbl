000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0117300.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/10/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER LEVERANSPLANEBASEN WDD9 MED SB.                            
001000*        SKAPAR FIL MED SUMMERAD KVBR PER ARTIKEL.                        
001100*        W01173   SUMMERAD KVBR FRÅN WDD902                               
001200*        XX                                                               
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- SUMMERAD KVBR PER ARTIKEL                                  
002900     SELECT W01173                     ASSIGN TO W01173D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W01173                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  POST -COPY W01173 -PRE  W01173-  -L.                                 
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W0117300'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  SW-FIRST-ARTIKEL            PIC X       VALUE 'J'.                   
005000     88  FIRST-ARTIKEL                       VALUE 'J'.                   
005100                                                                          
005200 01  ARBETSAREOR.                                                         
005300     03  WS-SPAR-IDARTNR         PIC S9(9)  VALUE ZERO.                   
005400     03  WS-SUM-KVBR             PIC S9(7)  VALUE ZERO.                   
005500     EJECT                                                                
005600*   --- VALID IDDC CODES                                                  
005700*01  -COPY WWDC99                                                         
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     SKIP2                                                                
007200*    --- PARAMETRAR TILL ABEND                                            
007300                                                                          
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007600     SKIP2                                                                
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  W01173-AREA-START           PIC X(24)   VALUE                        
008600                                 'W01173-AREA-START  '.                   
008700     SKIP2                                                                
008800                                                                          
008900*01  AREA -COPY W01173     -PRE W01173-                                   
009000     EJECT                                                                
009100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009200*                                                                         
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009900     03  W-IDLEVNR-X.                                                     
010000         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010600     SKIP2                                                                
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100 01  SSA2                        PIC X(64).                               
011200     EJECT                                                                
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011800     SKIP3                                                                
011900 01  DLI-IO-AREA.                                                         
012000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012100     SKIP3                                                                
012200     03  WLINLB01 REDEFINES IO-AREA.                                      
012300*        05  -COPY WDD901  -PRE WDD901-                                   
012400     SKIP3                                                                
012500     03  WLINLB02 REDEFINES IO-AREA.                                      
012600*        05  -COPY WDD902  -PRE WDD902-                                   
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900                                                                          
013000     EJECT                                                                
013100*01  -COPY W0008  -PRE WDD9-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING WDD9-PCB.                                      
013500     ENTRY 'DLITCBL' USING WDD9-PCB.                                      
013600                                                                          
013700     PERFORM A-INIT                                                       
013800     PERFORM IMS-GET-WDD9                                                 
013900     PERFORM UNTIL SEGMENT-SLUT                                           
014000        EVALUATE WDD9-SEG-NAME-FB                                         
014100           WHEN 'WDD901  '                                                
014300              IF FIRST-ARTIKEL                                            
014310                 MOVE WDD901-IDDC TO WS-IDDC                              
014400                 MOVE NEJ    TO SW-FIRST-ARTIKEL                          
014500              ELSE                                                        
014600                 PERFORM B-BEHANDLA-ARTIKEL                               
014710                 MOVE ZERO        TO WS-SPAR-IDARTNR                      
014720                                     WS-SUM-KVBR                          
014730                 MOVE WDD901-IDDC TO WS-IDDC                              
014800              END-IF                                                      
014900              MOVE WDD901-IDARTNR TO WS-SPAR-IDARTNR                      
015000           WHEN 'WDD902  '                                                
015100              PERFORM D-SUMMERA-KVBR                                      
015200         END-EVALUATE                                                     
015300         PERFORM IMS-GET-WDD9                                             
015400     END-PERFORM                                                          
015500     PERFORM B-BEHANDLA-ARTIKEL                                           
015600     PERFORM Z-FINIT                                                      
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100                                                                          
016200                                                                          
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN OUTPUT W01173                                                   
016600                                                                          
016700     ACCEPT DAGENS-DATUM  FROM DATE                                       
016800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017200                                                                          
017300 B-BEHANDLA-ARTIKEL SECTION.                                              
017400                                                                          
017500     IF CDC-SE                                                            
017510       IF WS-SUM-KVBR > 0                                                 
017600          MOVE WS-SPAR-IDARTNR   TO W01173-IDARTNR                        
017700          MOVE WS-SUM-KVBR       TO W01173-KVBR-TOT                       
017800          PERFORM S11-SKRIV-W01173                                        
017900       END-IF                                                             
017910     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
019100                                                                          
019200 D-SUMMERA-KVBR SECTION.                                                  
019300                                                                          
019400     COMPUTE WS-SUM-KVBR = WS-SUM-KVBR + WDD902-KVBR                      
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800                                                                          
019900 Z-FINIT SECTION.                                                         
020000     CLOSE W01173                                                         
020100     SKIP2                                                                
020200     MOVE 'S' TO POSTSUM-OPKOD                                            
020300     CALL POSTSUM USING POSTSUM-PARM                                      
020400     .                                                                    
020500     EJECT                                                                
020600 S11-SKRIV-W01173 SECTION.                                                
020700                                                                          
020800     WRITE W01173-POST FROM W01173-AREA                                   
020900                                                                          
021000     MOVE 'W01173' TO POSTSUM-FDNAMN                                      
021100     MOVE 'W01173D1' TO POSTSUM-DDNAMN2                                   
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
021400     EJECT                                                                
021500* --- IMS SEKTIONER ---                                                   
021600     SKIP3                                                                
021700     EJECT                                                                
021800 IMS-GET-WDD9   SECTION.                                                  
021900                                                                          
022000     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-AREA                           
022100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
022200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-STATUSKONTROLL SECTION.                                              
022700                                                                          
022800     SET STATUS-IX TO 1                                                   
022900     SEARCH GODK-STATUS                                                   
023000       AT END                                                             
023100         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
023200         DISPLAY FELTEXT                                                  
023300         CALL FELLOG                                                      
023400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023500         CONTINUE                                                         
023600     END-SEARCH                                                           
023700     .                                                                    

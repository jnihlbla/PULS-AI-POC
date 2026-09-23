000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W005WDL7.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   10/04/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBPROGRAM SOM SKÖTER UPPLÄGG AV NYA WDL7-SEGMENT                
001000*                                                                         
001100*                                                                         
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 DATA DIVISION.                                                           
001500                                                                          
001600 WORKING-STORAGE SECTION.                                                 
001700 77  IDPGM                       PIC X(8)   VALUE 'W005WDL7'.             
001800 77  JA                          PIC X      VALUE 'J'.                    
001900 77  NEJ                         PIC X      VALUE 'N'.                    
002000 77  FELTEXT                     PIC X(32)  VALUE SPACE.                  
003000 77  IX1                         PIC S9(3)  VALUE ZERO COMP-3.            
003100 77  IX2                         PIC S9(3)  VALUE ZERO COMP-3.            
003200 77  INDX                        PIC S9(3)  VALUE ZERO COMP-3.            
003300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
003400                                                                          
003500 01  DATUM-OMVANDLING.                                                    
003600     03  WS-FIRST-TIVV           PIC 9(2) VALUE ZERO.                     
003700     03  WS-INNEV-TIRP           PIC 9(2) VALUE ZERO.                     
003800     03  WS-KVVIPER              PIC 9(1) VALUE ZERO.                     
003900                                                                          
003910     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
003920     03  FILLER REDEFINES NAESTA-TIAARP.                                  
003930         05 NAESTA-TIAA          PIC  9(2).                               
003940         05 NAESTA-TIRP          PIC  9(2).                               
003950                                                                          
003960     03  LAST-WEEK-PER           PIC  9(2)   VALUE ZERO.                  
003970     03  WS-CURR-WEEK            PIC  9(2)   VALUE ZERO.                  
003971                                                                          
003972     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
003973     03  FILLER REDEFINES DAGENS-TIAARP.                                  
003974         05 DAGENS-TIAA          PIC  9(2).                               
003975         05 DAGENS-TIRP          PIC  9(2).                               
003976                                                                          
003980                                                                          
003990     03  WS-DAG-I-VECKA          PIC 9(1) VALUE ZERO.                     
003991                                                                          
004000 01  VECKA-I-PERIOD-TABELL.                                               
005000     03  VECKA-I-INNEV-PERIOD    OCCURS 5.                                
006000         05 AKTUELL-VECKA        PIC 9(2).                                
007000                                                                          
007100 01  FILLER                      PIC  X(16) VALUE 'WWDCKONS'.             
007200*01  -COPY WWDCKONS.                                                      
007300                                                                          
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900*                                                                         
008000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
008100 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
008200*01 -COPY WDATAREA                                                        
008300     SKIP2                                                                
008400* ARBETS-AREOR TILL IMS-SEKTIONERNA                                       
008500*                                                                         
008600 01  IMS-WS.                                                              
008700   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
008800                                                                          
008900   03  STATUS-WS                 PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009300                                                                          
009400   03  GODK-STATUSKODER.                                                  
009500     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
009600                                                                          
009700 01  NYCKLAR-TILL-DLI.                                                    
009800   03  W-IDARTNR-X.                                                       
009900     05  W-IDARTNR               PIC S9(9)   VALUE +0 COMP-3.             
010000                                                                          
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010300                                                                          
010400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDL701'.        
010500 01  DLI-IO-WDL701.                                                       
010600*  03   -COPY WDL701                                                      
010700                                                                          
010800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDL711'.        
010900 01  DLI-IO-WDL711.                                                       
011000*  03   -COPY WDL711                                                      
012000                                                                          
013000 01  FILLER                      PIC X(16)  VALUE 'IMS-FUNCTIONS'.        
014000*01    -COPY W0003                                                        
015000                                                                          
016000 LINKAGE SECTION.                                                         
016100                                                                          
016200*01  -COPY W005WDL7                                                       
016300                                                                          
016400*01  -COPY W0008 -PRE WDL7-                                               
016500     05  FILLER                  PIC X.                                   
016600                                                                          
016700 PROCEDURE DIVISION USING WDL7-W005WDL7 WDL7-PCB.                         
016800 MAIN SECTION.                                                            
016900                                                                          
017000     PERFORM A-INIT                                                       
017100                                                                          
017200     MOVE WDL7-IDARTNR     TO ART-IDARTNR                                 
017300     PERFORM IMS-ISRT-WDL701                                              
017400                                                                          
017500     PERFORM B-SAETT-GRUNDVAERDEN-WDL711                                  
017600     PERFORM IMS-ISRT-WDL711                                              
017700                                                                          
017800***COMMENTED UNTIL PROBLEMS WITH TO MUCH DATA IN WDL7 IS                  
017900*** SOLVED   2018-02-28                                                   
018000* WHEN 1 NDC-NA CREATES THEN ALL NDC-NA MUST EXIST                        
018100*    MOVE WC-NDC-US-RU TO DC-IDDC                                         
018200*    PERFORM IMS-ISRT-WDL711                                              
018300*    MOVE WC-NDC-US-LA TO DC-IDDC                                         
018400*    PERFORM IMS-ISRT-WDL711                                              
018500*    MOVE WC-NDC-US-SE TO DC-IDDC                                         
018600*    PERFORM IMS-ISRT-WDL711                                              
018700*    MOVE WC-NDC-US-CH TO DC-IDDC                                         
018800*    PERFORM IMS-ISRT-WDL711                                              
018900*    MOVE WC-NDC-US-JA TO DC-IDDC                                         
019000*    PERFORM IMS-ISRT-WDL711                                              
019100*    MOVE WC-NDC-CA      TO DC-IDDC                                       
019200*    PERFORM IMS-ISRT-WDL711                                              
019400                                                                          
019500     MOVE ZERO TO RETURN-CODE                                             
019600     GOBACK                                                               
019700     .                                                                    
019800     EJECT                                                                
019900                                                                          
020000  A-INIT SECTION.                                                         
030000                                                                          
040000     IF WDL7-IDARTNR NUMERIC AND WDL7-IDARTNR > ZERO                      
050000       MOVE WDL7-IDARTNR     TO W-IDARTNR                                 
060000     ELSE                                                                 
070000       MOVE 'FEL WDL7-IDARTNR ' TO FELTEXT                                
070100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
070200     END-IF                                                               
070300     IF WDL7-IDDC NOT > SPACE                                             
070400       MOVE 'FEL WDL7-IDDC    ' TO FELTEXT                                
070500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900                                                                          
071000  B-SAETT-GRUNDVAERDEN-WDL711 SECTION.                                    
072000                                                                          
073000      MOVE WDL7-IDDC         TO DC-IDDC                                   
074000      MOVE ZERO              TO DC-TIREFEFT                               
075000                                                                          
076000      MOVE +1                TO INDX                                      
077000      PERFORM UNTIL INDX     > 53                                         
078000        MOVE ZERO            TO DC-KVOI-RULL    (INDX)                    
079000                                DC-KVOI-REF-RULL (INDX)                   
080000                                DC-KVOI-CDC-RULL (INDX)                   
090000                                DC-KVOT-RULL    (INDX)                    
100000                                DC-KVOT-REF-RULL (INDX)                   
110000                                DC-KVOT-CDC-RULL (INDX)                   
120000        ADD +1               TO INDX                                      
130000      END-PERFORM                                                         
140000                                                                          
150000      PERFORM BA-1-VECKOR-I-INNEV                                         
160000      MOVE +1 TO INDX                                                     
170000      PERFORM UNTIL INDX > 5                                              
180000        MOVE AKTUELL-VECKA(INDX) TO DC-TIVV          (INDX)               
190000        MOVE ZERO TO                DC-KVOI-INNEV    (INDX)               
200000                                    DC-KVOI-REF-INNEV(INDX)               
210000                                    DC-KVOI-CDC-INNEV(INDX)               
220000                                    DC-KVOT-INNEV    (INDX)               
230000                                    DC-KVOT-REF-INNEV(INDX)               
240000                                    DC-KVOT-CDC-INNEV(INDX)               
240100                                    DC-KVOI-PP-INNEV (INDX)               
240200                                    DC-KVOT-PP-INNEV (INDX)               
250000        ADD +1 TO INDX                                                    
260000      END-PERFORM                                                         
480000      .                                                                   
490000                                                                          
500000  BA-1-VECKOR-I-INNEV SECTION.                                            
510000                                                                          
520000*    INNEVARANDE PERIOD, INNEVARANDE VECKA, VECKOR I PERIOD.              
530000      MOVE 'IDAG'       TO DAT-KDDATFORM                                  
540000                                                                          
550000      CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                     
560000                          DAT-O-TIDATUM DAT-KDSVAR                        
570000                                                                          
580000      IF DAT-KDSVAR-OK                                                    
590000        MOVE DAT-TIRP       TO WS-INNEV-TIRP                              
600000        MOVE DAT-KVVIPER    TO WS-KVVIPER                                 
600100        MOVE DAT-TID        TO WS-DAG-I-VECKA                             
600200        MOVE DAT-TIVV       TO WS-CURR-WEEK                               
600300        MOVE DAT-TIAARP     TO DAGENS-TIAARP                              
600400                               NAESTA-TIAARP                              
610000      END-IF                                                              
620000                                                                          
630000*    FÖRSTA VECKA I PERIOD.                                               
640000      MOVE 'AARP  '          TO DAT-KDDATFORM                             
650000      MOVE DAT-TIAARP        TO DAT-I-TIDATUM                             
660000                                                                          
670000      CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                     
680000                          DAT-O-TIDATUM DAT-KDSVAR                        
690000                                                                          
700000      IF DAT-KDSVAR-OK                                                    
710000        MOVE DAT-TIVV       TO WS-FIRST-TIVV                              
710100**** FIX FOR OI ON SUNDAYS IN THE LAST WEEK OF THE PERIOD                 
710101**********************************************************                
710110        COMPUTE LAST-WEEK-PER = WS-FIRST-TIVV + WS-KVVIPER - 1            
710120        IF LAST-WEEK-PER = WS-CURR-WEEK                                   
710130        AND WS-DAG-I-VECKA = 7                                            
710200          COMPUTE NAESTA-TIRP = DAGENS-TIRP + 1                           
710300          IF NAESTA-TIRP = +13                                            
710400            ADD +1 TO NAESTA-TIAA                                         
710500            MOVE +1 TO NAESTA-TIRP                                        
710510              MOVE 'AARP  '  TO DAT-KDDATFORM                             
710520              MOVE NAESTA-TIAARP TO DAT-I-TIDATUM                         
710530                                                                          
710540              CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM             
710550                                  DAT-O-TIDATUM DAT-KDSVAR                
710560                                                                          
710570              IF DAT-KDSVAR-OK                                            
710580                MOVE DAT-TIVV TO WS-FIRST-TIVV                            
710581                MOVE DAT-TIRP TO WS-INNEV-TIRP                            
710582                MOVE DAT-KVVIPER TO WS-KVVIPER                            
710590              END-IF                                                      
710600          END-IF                                                          
710601****************************************************************          
710610        END-IF                                                            
710700                                                                          
720000      END-IF                                                              
730000                                                                          
740000      IF WS-INNEV-TIRP = 01                                               
750000         MOVE 01 TO WS-FIRST-TIVV                                         
000000      END-IF                                                              
010000                                                                          
020000*    TABELL MED INNEVARANDE PERIODS VECKOR                                
040000      MOVE WS-FIRST-TIVV TO AKTUELL-VECKA(1)                              
050000      MOVE 2 TO INDX                                                      
060000      PERFORM UNTIL INDX > WS-KVVIPER                                     
070000         COMPUTE AKTUELL-VECKA(INDX) =                                    
080000                           AKTUELL-VECKA(INDX - 1) + 1                    
090000         ADD +1 TO INDX                                                   
100000      END-PERFORM                                                         
110000      .                                                                   
111000                                                                          
120000 IMS-ISRT-WDL701 SECTION.                                                 
130000                                                                          
131000     IF WDL7-DBD-NAME = 'WDL7'                                            
132000       MOVE 'WDL701   ' TO SSA1                                           
133000     ELSE                                                                 
140000       MOVE 'WLOIGA01 ' TO SSA1                                           
141000     END-IF                                                               
150000     MOVE '  II' TO GODK-STATUSKODER                                      
160000     CALL CBLTDLI USING ISRT WDL7-PCB DLI-IO-WDL701 SSA1                  
170000     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
180000     PERFORM IMS-STATUSKONTROLL                                           
190000     .                                                                    
200000                                                                          
210000 IMS-ISRT-WDL711 SECTION.                                                 
220000                                                                          
221000     IF WDL7-DBD-NAME = 'WDL7'                                            
221100       STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                       
221200          DELIMITED BY SIZE INTO SSA1                                     
222000       MOVE 'WDL711   ' TO SSA2                                           
223000     ELSE                                                                 
230000       STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                       
240000          DELIMITED BY SIZE INTO SSA1                                     
250000       MOVE 'WLOIGA11 ' TO SSA2                                           
251000     END-IF                                                               
260000     MOVE '  II' TO GODK-STATUSKODER                                      
270000     CALL CBLTDLI USING ISRT WDL7-PCB DLI-IO-WDL711 SSA1 SSA2             
280000     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
290000     PERFORM IMS-STATUSKONTROLL                                           
300000     .                                                                    
320000                                                                          
330000 IMS-STATUSKONTROLL SECTION.                                              
340000                                                                          
350000     SET STATUS-IX TO 1                                                   
360000     SEARCH GODK-STATUS                                                   
370000       AT END                                                             
380000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
390000         DELIMITED BY SIZE INTO FELTEXT                                   
400000         CALL FELLOG                                                      
410000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
420000         CONTINUE                                                         
430000     END-SEARCH                                                           
440000     .                                                                    

000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2719800.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   10/11/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPDATES TISTOREF FOR KDERS = X2, X5                              
001000*        WRITES OUTFILE FOR X2,X3,X5,X6 TO BE                             
001010*        HANDLED IN ROUTINE FOR "QUICK SUPERSESSIONS"                     
001100*        PROGRAM UPDATES WDK6                                             
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- TISTOREF                                                   
002200     SELECT W27197                     ASSIGN TO W27198D1.                
002300     EJECT                                                                
002400     SELECT W27198                     ASSIGN TO W27198D2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W27197                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W27197      -L.                                                
003500     EJECT                                                                
003600 FD  W27198                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W27197 -PRE  UT-  -L.                                     
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2719800'.            
004700 01  CHKP-VAR.                                                            
004800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005300 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000                                                                          
006100 77  W27197-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W27197                       VALUE 'J'.                   
006300                                                                          
006400     EJECT                                                                
006500 01  ARBETSAREOR.                                                         
006600                                                                          
006700     03 WS-ANTAL-W27197          PIC 9(8)   VALUE ZERO.                   
006800     03 WS-ANTAL-UPD-WDK6        PIC 9(8)   VALUE ZERO.                   
006810     03 WS-KDERS                 PIC 9(2)    VALUE ZERO.                  
006900     EJECT                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL POSTSUM                                          
007700*                                                                         
007800*01  -COPY W0005   -PRE  POSTSUM-                                         
007900     EJECT                                                                
008000 01  IN-AREA-START               PIC X(24)   VALUE                        
008100                                             'IN-AREA-START'.             
008200     SKIP2                                                                
008300                                                                          
008400*01  AREA -COPY W27197     -PRE IN-                                       
008500*                                                                         
008600     EJECT                                                                
008610 01  UT-AREA-START               PIC X(24)   VALUE                        
008620                                             'UT-AREA-START'.             
008630     SKIP2                                                                
008640                                                                          
008650*01  AREA -COPY W27197     -PRE UT-                                       
008660*                                                                         
008670     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000     03  W-IDARTNR-X.                                                     
009100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009200     03  W-KDSEGKEY-X.                                                    
009300         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
009400     SKIP2                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010100     88  IMS-EJ-OK                           VALUE 'XD'.                  
010200     SKIP2                                                                
010300 01  GODK-STATUSKODER.                                                    
010400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010500     SKIP3                                                                
010600 01  SSA1                        PIC X(64).                               
010700 01  SSA2                        PIC X(64).                               
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300                                                                          
011400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
011500 01  DLI-IO-WDK611.                                                       
011600*    03  -COPY WDK611                                                     
011700     EJECT                                                                
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000                                                                          
012100*01  -COPY W0009   -PRE MSG-                                              
012200                                                                          
012300*01  -COPY W0008  -PRE WDK6-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012700 MAIN SECTION.                                                            
012800     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
012900                                                                          
013000     SKIP2                                                                
013100     PERFORM A-INIT                                                       
013200     PERFORM S01-LAES-W27197                                              
013300     PERFORM UNTIL END-OF-W27197                                          
013400                                                                          
013500       IF CHKP-ANT > CHKP-MAX                                             
013600         PERFORM X-TAG-CHECKPOINT                                         
013700       END-IF                                                             
013800                                                                          
013900       PERFORM B-BEHANDLA-POSTER                                          
014000                                                                          
014100       PERFORM S01-LAES-W27197                                            
014200     END-PERFORM                                                          
014300                                                                          
014400                                                                          
014500     PERFORM Z-FINIT                                                      
014600                                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200     SKIP2                                                                
015300                                                                          
015400     PERFORM IMS-RESTART                                                  
015500                                                                          
015600     OPEN INPUT W27197                                                    
015700     OPEN OUTPUT W27198                                                   
015800     .                                                                    
015900     EJECT                                                                
016000 B-BEHANDLA-POSTER SECTION.                                               
016100                                                                          
016200     MOVE IN-IDARTNR TO W-IDARTNR                                         
016210                        UT-IDARTNR                                        
016300                                                                          
016400     PERFORM IMS-GHU-K611                                                 
016500     IF IN-TISTOREF NOT = CLAG-TISTOREF                                   
016600        MOVE IN-TISTOREF     TO CLAG-TISTOREF                             
016601        MOVE CLAG-KDERS      TO WS-KDERS                                  
016610                                                                          
016611*  WE ONLY UPDATE TISTOREF FOR KDERS X2,X5,                               
016612*  BUT WE WRITE OUTFILE FOR KDERS X2,X3,X5 AND X6                         
016620       IF WS-KDERS(2:1) = 2 OR 5                                          
016700         PERFORM IMS-REPL-WDK6                                            
016701         ADD +1              TO CHKP-ANT                                  
016702         ADD 1               TO WS-ANTAL-UPD-WDK6                         
016703       END-IF                                                             
016704       MOVE CLAG-TISTOREF    TO UT-TISTOREF                               
016705       MOVE CLAG-KDERS       TO UT-KDERS                                  
016710       PERFORM S02-SKRIV-W27198                                           
017000     END-IF                                                               
017100                                                                          
017200     .                                                                    
017300     EJECT                                                                
017400 Z-FINIT SECTION.                                                         
017500                                                                          
017600     CLOSE W27197                                                         
017610           W27198                                                         
017700     DISPLAY 'ANTAL W27197    : ' WS-ANTAL-W27197                         
017800     DISPLAY 'ANTAL REPL WDK6 : ' WS-ANTAL-UPD-WDK6                       
017900     .                                                                    
018000     EJECT                                                                
018100 S01-LAES-W27197  SECTION.                                                
018200     SKIP2                                                                
018300     READ W27197 INTO IN-AREA                                             
018400     AT END                                                               
018500        SET END-OF-W27197 TO TRUE                                         
018600                                                                          
018700     NOT AT END                                                           
018800        ADD 1                TO WS-ANTAL-W27197                           
018900     END-READ                                                             
019000     .                                                                    
019100     EJECT                                                                
019110 S02-SKRIV-W27198 SECTION.                                                
019111     SKIP2                                                                
019120     WRITE UT-POST FROM UT-AREA                                           
019130                                                                          
019140     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
019150     MOVE 'W27198' TO POSTSUM-FDNAMN                                      
019160     MOVE 'W27198D2' TO POSTSUM-DDNAMN2                                   
019170     CALL POSTSUM USING POSTSUM-PARM                                      
019180     .                                                                    
019190     EJECT                                                                
019200                                                                          
019300 X-TAG-CHECKPOINT   SECTION.                                              
019400                                                                          
019500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
019600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
019700     PERFORM IMS-CHECKPOINT                                               
019800     MOVE ZERO TO CHKP-ANT                                                
019900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
020000     .                                                                    
020100     EJECT                                                                
020200* --- IMS SEKTIONER ---                                                   
020300                                                                          
020400     EJECT                                                                
020500 IMS-GHU-K611 SECTION.                                                    
020600                                                                          
020700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
020800          DELIMITED BY SIZE INTO SSA1                                     
020900     MOVE 'WDK611  '         TO SSA2                                      
021000     MOVE '  ' TO GODK-STATUSKODER                                        
021100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
021200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021300     PERFORM IMS-STATUSKONTROLL                                           
021400     .                                                                    
021500     EJECT                                                                
021600 IMS-REPL-WDK6 SECTION.                                                   
021700                                                                          
021800     MOVE '  ' TO GODK-STATUSKODER                                        
021900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
022000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     .                                                                    
022300     EJECT                                                                
022400 IMS-RESTART SECTION.                                                     
022500     SKIP2                                                                
022600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022700     MOVE '  ' TO GODK-STATUSKODER                                        
022800     CALL CBLTDLI USING XRST MSG-PCB                                      
022900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023000                        CHKP-AREA-LENGTH CHKP-AREA                        
023100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023200     PERFORM IMS-STATUSKONTROLL                                           
023300     .                                                                    
023400     EJECT                                                                
023500 IMS-CHECKPOINT SECTION.                                                  
023600     SKIP2                                                                
023700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023800     MOVE '  XD' TO GODK-STATUSKODER                                      
023900     CALL CBLTDLI USING CHKP MSG-PCB                                      
024000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024100                        CHKP-AREA-LENGTH CHKP-AREA                        
024200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024300     PERFORM IMS-STATUSKONTROLL                                           
024400                                                                          
024500     IF IMS-EJ-OK                                                         
024600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024700       DISPLAY FELTEXT                                                    
024800       CALL FELLOG                                                        
024900     END-IF                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 IMS-STATUSKONTROLL SECTION.                                              
025300     SKIP2                                                                
025400     SET STATUS-IX TO 1                                                   
025500     SEARCH GODK-STATUS                                                   
025600       AT END                                                             
025700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025800           DELIMITED BY SIZE INTO FELTEXT                                 
025900         DISPLAY FELTEXT                                                  
026000         CALL FELLOG                                                      
026100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026200         CONTINUE                                                         
026300     END-SEARCH                                                           
026400     .                                                                    

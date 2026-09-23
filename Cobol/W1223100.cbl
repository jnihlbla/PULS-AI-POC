000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1223100.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   160519.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER W12207 SOM SKAPAS I PGM W12204.                 
000900*        BÅDE ARTIKLAR SOM SKA SKALAS/RENSAS WDK6 TAS BORT                
001000*        FRÅN WDF812.                                                     
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDF8.                                      
001300*                                                                         
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- ARTIKLAR SOM SKA RENSAS/SKALAS WDK6                        
002100     SELECT W12207                     ASSIGN TO W12231D1.                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W12207                                                               
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS  0.                                                   
002900                                                                          
003000*01  -COPY W12207      -L.                                                
003100                                                                          
003200                                                                          
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W1223100'.            
003800 01  CHKP-VAR.                                                            
003900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004400 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  W-DLET-WDF812               PIC 9(7)    VALUE ZERO.                  
004700                                                                          
004800 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004900 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005000                                                                          
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 77  W12207-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W12207                       VALUE 'J'.                   
005700                                                                          
005800                                                                          
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400                                                                          
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800                                                                          
006900                                                                          
007000 01  IN-AREA-START               PIC X(24)   VALUE                        
007100                                             'IN-AREA-START'.             
007200                                                                          
007300                                                                          
007400*01  AREA -COPY W12207     -PRE IN-                                       
007500*                                                                         
007600                                                                          
007700                                                                          
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-IDARTNR-X.                                                     
008200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     03  W-WDF8C1KY-MIN-X.                                                
008400         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
008500         05  W-IDSPRGRP-MIN      PIC  X(10)  VALUE SPACE.                 
008600     03  W-WDF8C1KY-MAX-X.                                                
008700         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
008800         05  W-IDSPRGRP-MAX      PIC  X(10)  VALUE SPACE.                 
008900     03  W-IDSPRGRP-X.                                                    
009000         05  W-IDSPRGRP          PIC  X(10)  VALUE SPACE.                 
009100                                                                          
009200                                                                          
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                       VALUE '  '.                  
009600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009900     88  IMS-EJ-OK                           VALUE 'XD'.                  
010000                                                                          
010100 01  GODK-STATUSKODER.                                                    
010200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300                                                                          
010400 01  ALL-SSA.                                                             
010500     03 SSA1                     PIC X(64).                               
010600     03 SSA2                     PIC X(64).                               
010700                                                                          
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000                                                                          
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF801'.                      
011300 01  DLI-IO-WDF801.                                                       
011400*    03  -COPY WDF801.                                                    
011500                                                                          
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF812'.                      
011700 01  DLI-IO-WDF812.                                                       
011800*    03  -COPY WDF812.                                                    
011900                                                                          
012000                                                                          
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF8C'.                       
012200 01  DLI-IO-WDF8C.                                                        
012300*    03  -COPY WDF8C1                                                     
012400                                                                          
012500                                                                          
012600 LINKAGE SECTION.                                                         
012700*01  -COPY W0009  -PRE MSG-                                               
012800                                                                          
012900*01  -COPY W0008  -PRE WDF8-                                              
013000     05  FILLER                  PIC X.                                   
013100                                                                          
013200*01  -COPY W0008  -PRE WDF8C-                                             
013300     05  FILLER                  PIC X.                                   
013400                                                                          
013500                                                                          
013600 PROCEDURE DIVISION  USING MSG-PCB WDF8-PCB WDF8C-PCB.                    
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB WDF8-PCB WDF8C-PCB.                    
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM S01-LAES-W12207                                              
014300     PERFORM UNTIL END-OF-W12207                                          
014400       IF IN-UTFIL-TYP = 'S' OR 'B'                                       
014500          MOVE LOW-VALUE  TO W-WDF8C1KY-MIN-X                             
014600          MOVE HIGH-VALUE TO W-WDF8C1KY-MAX-X                             
014700          MOVE IN-IDARTNR TO W-IDARTNR-MIN                                
014800                             W-IDARTNR-MAX                                
014900                             W-IDARTNR                                    
015000          PERFORM IMS-GU-WDF8C-FIRST                                      
015100          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                    
015200             MOVE SEQC-IDSPRGRP TO W-IDSPRGRP                             
015300             PERFORM IMS-GHU-WDF812                                       
015400*****        DISPLAY 'DLET ' W-IDARTNR                                    
015500             PERFORM IMS-DLET-WDF812                                      
                   ADD 1 TO W-DLET-WDF812                                       
015600             ADD +1 TO CHKP-ANT                                           
015700             PERFORM IMS-GN-WDF8C-NEXT                                    
015800          END-PERFORM                                                     
015900          IF CHKP-ANT > CHKP-MAX                                          
016000             PERFORM X-TAG-CHECKPOINT                                     
016100          END-IF                                                          
016200       END-IF                                                             
016300       PERFORM S01-LAES-W12207                                            
016400     END-PERFORM                                                          
016500                                                                          
016600     PERFORM Z-FINIT                                                      
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000                                                                          
017100                                                                          
017200 A-INIT SECTION.                                                          
017300     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
017400                                                                          
017500     PERFORM IMS-RESTART                                                  
017600     OPEN INPUT W12207                                                    
017700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017800     .                                                                    
017900                                                                          
018000                                                                          
018100 Z-FINIT SECTION.                                                         
018110     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
018200                                                                          
           DISPLAY 'ANTAL BORTTAGNA WDF812: ' W-DLET-WDF812                     
018300     CLOSE W12207                                                         
018400     MOVE 'S' TO POSTSUM-OPKOD                                            
018500     CALL POSTSUM USING POSTSUM-PARM                                      
018600     .                                                                    
018700                                                                          
018800                                                                          
018900 S01-LAES-W12207  SECTION.                                                
019000                                                                          
019100     READ W12207 INTO IN-AREA                                             
019200     AT END                                                               
019300        SET END-OF-W12207 TO TRUE                                         
019400                                                                          
019500     NOT AT END                                                           
019600        MOVE 'W12207'     TO POSTSUM-FDNAMN                               
019700        MOVE 'W12226D1'   TO POSTSUM-DDNAMN2                              
019800        MOVE IN-UTFIL-TYP TO POSTSUM-TRANSTYP                             
019900        CALL POSTSUM USING POSTSUM-PARM                                   
020000     END-READ                                                             
020100     .                                                                    
020200                                                                          
020300                                                                          
020400 X-TAG-CHECKPOINT SECTION.                                                
020500                                                                          
020600     PERFORM IMS-CHECKPOINT                                               
020700     MOVE ZERO TO CHKP-ANT                                                
020800     .                                                                    
020900                                                                          
021000                                                                          
021100* --- IMS SEKTIONER ---                                                   
021200                                                                          
021300 IMS-GU-WDF8C-FIRST SECTION.                                              
021310     MOVE 'GU-WDF8C-FIRST  ' TO CURRENT-IMS-SECTION                       
021400                                                                          
021500     MOVE SPACE             TO ALL-SSA                                    
021600     STRING 'WDF8C1  (WDF8C1KY>=' W-WDF8C1KY-MIN-X                        
021700                    '&WDF8C1KY<=' W-WDF8C1KY-MAX-X ')'                    
021800        DELIMITED BY SIZE INTO SSA1                                       
021900     MOVE '  GEGB'          TO GODK-STATUSKODER                           
022000     CALL CBLTDLI USING GU WDF8C-PCB DLI-IO-WDF8C SSA1                    
022100     MOVE WDF8C-STATUS-CODE TO STATUS-WS                                  
022200     PERFORM IMS-STATUSKONTROLL                                           
022300     .                                                                    
022400                                                                          
022500 IMS-GN-WDF8C-NEXT SECTION.                                               
022510     MOVE 'GN-WDF8C-NEXT   ' TO CURRENT-IMS-SECTION                       
022600                                                                          
022700     MOVE SPACE             TO ALL-SSA                                    
022800     STRING 'WDF8C1  (WDF8C1KY>=' W-WDF8C1KY-MIN-X                        
022900                    '&WDF8C1KY<=' W-WDF8C1KY-MAX-X ')'                    
023000        DELIMITED BY SIZE INTO SSA1                                       
023100     MOVE '  GEGB'          TO GODK-STATUSKODER                           
023200     CALL CBLTDLI USING GN WDF8C-PCB DLI-IO-WDF8C SSA1                    
023300     MOVE WDF8C-STATUS-CODE TO STATUS-WS                                  
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600                                                                          
024800 IMS-GHU-WDF812 SECTION.                                                  
024810     MOVE 'GHU-WDF812      ' TO CURRENT-IMS-SECTION                       
024900                                                                          
025000     MOVE SPACE             TO ALL-SSA                                    
025100     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
025200        DELIMITED BY SIZE INTO SSA1                                       
025300     STRING 'WDF812  (IDARTNR  =' W-IDARTNR-X ')'                         
025400        DELIMITED BY SIZE INTO SSA2                                       
025500     MOVE '  '              TO GODK-STATUSKODER                           
025600     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF812 SSA1 SSA2              
025700     MOVE WDF8-STATUS-CODE  TO STATUS-WS                                  
025800     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
026000                                                                          
026100 IMS-DLET-WDF812 SECTION.                                                 
026110     MOVE 'DLET-WDF812     ' TO CURRENT-IMS-SECTION                       
026200                                                                          
026300     MOVE SPACE             TO ALL-SSA                                    
026400     MOVE '  '              TO GODK-STATUSKODER                           
026500     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF812                       
026600     MOVE WDF8-STATUS-CODE  TO STATUS-WS                                  
026700     PERFORM IMS-STATUSKONTROLL                                           
026800     .                                                                    
026900                                                                          
027000 IMS-RESTART SECTION.                                                     
027100                                                                          
027200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027300     MOVE '  ' TO GODK-STATUSKODER                                        
027400     CALL CBLTDLI USING XRST MSG-PCB                                      
027500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027600                        CHKP-AREA-LENGTH CHKP-AREA                        
027700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000                                                                          
028100 IMS-CHECKPOINT SECTION.                                                  
028200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028300     MOVE '  XD' TO GODK-STATUSKODER                                      
028400     CALL CBLTDLI USING CHKP MSG-PCB                                      
028500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028600                        CHKP-AREA-LENGTH CHKP-AREA                        
028700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     IF IMS-EJ-OK                                                         
029000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
029100       DISPLAY FELTEXT                                                    
029200       CALL FELLOG                                                        
029300     END-IF                                                               
029400     .                                                                    
029500                                                                          
029600 IMS-STATUSKONTROLL SECTION.                                              
029700                                                                          
029800     SET STATUS-IX TO 1                                                   
029900     SEARCH GODK-STATUS                                                   
030000       AT END                                                             
030100         CALL FELLOG                                                      
030200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030300         CONTINUE                                                         
030400     END-SEARCH                                                           
030500     .                                                                    

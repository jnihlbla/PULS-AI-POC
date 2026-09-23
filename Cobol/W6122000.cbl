000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6122000.                                                
000300 AUTHOR.         EVA LUNDELL.                                             
000400 DATE-WRITTEN.   96/09/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER EN FIL MED POSTER                               
001000*        SOM SKA RENSAS FRÅN WDL6.                                        
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLINLC (WDL6).                             
001300*                                                                         
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*          --- INFIL MED POSTER SOM SKA RENSAS FRÅN WDL6                  
002200     SELECT W61215                     ASSIGN TO W61220D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  W61215                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W61215      -L.                                                
003300                                                                          
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W6122000'.            
004000 01  FELTEXT.                                                             
004100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-W61215-ANT               PIC S9(7)   VALUE ZERO COMP-3.           
004600 77  WS-DELETE                   PIC S9(7)   VALUE ZERO COMP-3.           
004700                                                                          
004800 01  CHKP-VAR.                                                            
004900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005400 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
005500                                                                          
005600 77  W61215-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W61215                       VALUE 'J'.                   
005800                                                                          
005900                                                                          
006000                                                                          
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400                                                                          
006500     EJECT                                                                
006600 01  IN-AREA-START               PIC X(24)   VALUE                        
006700                                             'IN-AREA-START'.             
006800                                                                          
006900*01  AREA -COPY W61215     -PRE IN-                                       
007000*                                                                         
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007300                                                                          
007400 01  NYCKLAR-TILL-DLI.                                                    
007500   03  W-IDARTNR-X.                                                       
007600     05  W-IDARTNR               PIC S9(9)   VALUE ZERO COMP-3.           
007700   03  W-DAINLEV-X.                                                       
007800     05  W-DAINLEV               PIC 9(16)   VALUE ZERO.                  
007900                                                                          
008000                                                                          
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008500     88  IMS-EJ-OK                           VALUE 'XD'.                  
008600                                                                          
008700                                                                          
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000                                                                          
009100                                                                          
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC01'.         
010000                                                                          
010100 01  DLI-IO-INLC01.                                                       
010200*  03  -COPY WDL601                                                       
010300                                                                          
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC11'.         
010600                                                                          
010700 01  DLI-IO-INLC11.                                                       
010800*  03  -COPY WDL611                                                       
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100*01  -COPY W0009   -PRE MSG-                                              
011200                                                                          
011300*01  -COPY W0008  -PRE INLC-                                              
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600 PROCEDURE DIVISION  USING MSG-PCB INLC-PCB.                              
011700 MAIN SECTION.                                                            
011800     ENTRY 'DLITCBL' USING MSG-PCB INLC-PCB.                              
011900                                                                          
012000     PERFORM A-INIT                                                       
012100     PERFORM S01-LAES-W61215                                              
012200                                                                          
012300     PERFORM UNTIL END-OF-W61215                                          
012400       IF CHKP-ANT > CHKP-MAX                                             
012500         PERFORM IMS-CHECKPOINT                                           
012600         MOVE ZERO TO CHKP-ANT                                            
012700         MOVE ZERO TO W-IDARTNR                                           
012800       END-IF                                                             
012900       IF IN-IDARTNR NOT = W-IDARTNR                                      
013000         MOVE IN-IDARTNR TO W-IDARTNR                                     
013100         PERFORM IMS-GET-INLC-ART                                         
013200       END-IF                                                             
013300       MOVE IN-DAINLEV TO W-DAINLEV                                       
013400       PERFORM IMS-GET-INLC-INL                                           
013500       IF SEGMENT-FINNS                                                   
013600         PERFORM IMS-DLET-INLC                                            
013700         ADD +1 TO CHKP-ANT                                               
013800         ADD +1 TO WS-DELETE                                              
013900       END-IF                                                             
014000       PERFORM S01-LAES-W61215                                            
014100     END-PERFORM                                                          
014200                                                                          
014300     PERFORM Z-FINIT                                                      
014400                                                                          
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800                                                                          
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     PERFORM IMS-RESTART                                                  
015300                                                                          
015400     OPEN INPUT W61215                                                    
015500     .                                                                    
015600                                                                          
015700     EJECT                                                                
015800 Z-FINIT SECTION.                                                         
015900                                                                          
016000     CLOSE W61215                                                         
016100                                                                          
016200     DISPLAY 'W61215-ANTAL = ' WS-W61215-ANT                              
016300     DISPLAY 'DELETE-ANTAL = ' WS-DELETE                                  
016400     .                                                                    
016500                                                                          
016600     EJECT                                                                
016700 S01-LAES-W61215  SECTION.                                                
016800                                                                          
016900     READ W61215 INTO IN-AREA                                             
017000     AT END                                                               
017100       SET END-OF-W61215 TO TRUE                                          
017200     NOT AT END                                                           
017300       ADD +1 TO WS-W61215-ANT                                            
017400     END-READ                                                             
017500     .                                                                    
017600                                                                          
017700     EJECT                                                                
017800* --- IMS SEKTIONER ---                                                   
017900                                                                          
018000 IMS-GET-INLC-ART SECTION.                                                
018100                                                                          
018200     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
018300          DELIMITED BY SIZE INTO SSA1                                     
018400     MOVE '  GE' TO GODK-STATUSKODER                                      
018500     CALL CBLTDLI USING GU INLC-PCB DLI-IO-INLC01 SSA1                    
018600     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
018700     PERFORM IMS-STATUSKONTROLL                                           
018800     .                                                                    
018900                                                                          
019000                                                                          
019100 IMS-GET-INLC-INL SECTION.                                                
019200                                                                          
019300     STRING 'WLINLC11(DAINLEV  =' W-DAINLEV-X ')'                         
019400          DELIMITED BY SIZE INTO SSA1                                     
019500     MOVE '  GE' TO GODK-STATUSKODER                                      
019600     CALL CBLTDLI USING GHNP INLC-PCB DLI-IO-INLC11 SSA1                  
019700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
019800     PERFORM IMS-STATUSKONTROLL                                           
019900     .                                                                    
020000                                                                          
020100                                                                          
020200 IMS-DLET-INLC SECTION.                                                   
020300                                                                          
020400     MOVE '  ' TO GODK-STATUSKODER                                        
020500     CALL CBLTDLI USING DLET INLC-PCB DLI-IO-INLC11                       
020600     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
020700     PERFORM IMS-STATUSKONTROLL                                           
020800     .                                                                    
020900                                                                          
021000     EJECT                                                                
021100 IMS-RESTART SECTION.                                                     
021200                                                                          
021300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021400     MOVE '  ' TO GODK-STATUSKODER                                        
021500     CALL CBLTDLI USING XRST MSG-PCB                                      
021600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021700                        CHKP-AREA-LENGTH CHKP-AREA                        
021800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021900     PERFORM IMS-STATUSKONTROLL                                           
022000     .                                                                    
022100                                                                          
022200                                                                          
022300 IMS-CHECKPOINT SECTION.                                                  
022400                                                                          
022500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022600     MOVE '  XD' TO GODK-STATUSKODER                                      
022700     CALL CBLTDLI USING CHKP MSG-PCB                                      
022800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022900                        CHKP-AREA-LENGTH CHKP-AREA                        
023000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023100     PERFORM IMS-STATUSKONTROLL                                           
023200                                                                          
023300     IF IMS-EJ-OK                                                         
023400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
023500       DISPLAY FELTEXT                                                    
023600       CALL FELLOG                                                        
023700     END-IF                                                               
023800     .                                                                    
023900                                                                          
024000     EJECT                                                                
024100 IMS-STATUSKONTROLL SECTION.                                              
024200                                                                          
024300     SET STATUS-IX TO 1                                                   
024400     SEARCH GODK-STATUS                                                   
024500       AT END                                                             
024600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024700           DELIMITED BY SIZE INTO FELTEXT                                 
024800         DISPLAY FELTEXT                                                  
024900         CALL FELLOG                                                      
025000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025100         CONTINUE                                                         
025200     END-SEARCH                                                           
025300     .                                                                    

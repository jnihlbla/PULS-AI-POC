000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0153300.                                                
000300 AUTHOR.         KJELL ANDRE.                                             
000400 DATE-WRITTEN.   96/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        RENSNING AV HÄNDELSE-TRANSAR                                     
001000*        GENERELLT PROGRAM FÖR ATT TA BORT TRANSAKTIONER FRÅN EN          
001100*        FIL-DATABAS (WDR3)                                               
001200*        PROGRAMMET LÄSER EN FIL MED NYCKLAR FÖR DE TRANSAR SOM           
001300*        SKA TAS BORT, OCH SOM SKAPATS AV ETT TIDIGARE PROGRAM            
001400*        SOM LÄST UT TRANSARNA TILL EN FIL.                               
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLFILC (WDR3)                              
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*          --- FIL MED TRANSAR SOM SKA RENSAS                             
002600     SELECT W01533                     ASSIGN TO W01533D1.                
002700                                                                          
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200                                                                          
003300 FD  W01533                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY WDR301        -L.                                              
003800                                                                          
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W0153300'.            
004500 01  CHKP-VAR.                                                            
004600     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004900     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005000     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005100     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  W-ANT-IN                    PIC S9(7)   VALUE ZERO COMP-3.           
005500 77  W-ANT-DEL                   PIC S9(7)   VALUE ZERO COMP-3.           
005600                                                                          
005700                                                                          
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100                                                                          
006200 77  W01533-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W01533                       VALUE 'J'.                   
006400                                                                          
006500                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900                                                                          
007000     EJECT                                                                
007100 01  RENS-AREA-START             PIC X(16)   VALUE                        
007200                                             'RENS-AREA-START'.           
007300                                                                          
007400*01  AREA -COPY WDR301  -L   -PRE RENS-                                   
007500*                                                                         
007600                                                                          
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-WDR301KY-X.                                                    
008200         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
008300                                                                          
008400                                                                          
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900                                                                          
009000                                                                          
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300                                                                          
009400                                                                          
009500 01  SSA1                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLFILC01'.                    
010300 01  DLI-IO-WLFILC01.                                                     
010400*    03  -COPY WDR301    -PRE FILC-                                       
010500                                                                          
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800*01  -COPY W0009  -PRE MSG-                                               
010900                                                                          
011000*01  -COPY W0008  -PRE FILC-                                              
011100     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011300 PROCEDURE DIVISION  USING MSG-PCB FILC-PCB.                              
011400 MAIN SECTION.                                                            
011500     ENTRY 'DLITCBL' USING MSG-PCB FILC-PCB.                              
011600                                                                          
011700     PERFORM A-INIT                                                       
011800     PERFORM S01-LAES-W01533                                              
011900     PERFORM UNTIL END-OF-W01533                                          
012000       IF CHKP-ANT > CHKP-MAX                                             
012100         PERFORM X-TAG-CHECKPOINT                                         
012200       END-IF                                                             
012300       PERFORM B-LAES-RENSA-TRANS                                         
012400                                                                          
012500       PERFORM S01-LAES-W01533                                            
012600     END-PERFORM                                                          
012700                                                                          
012800                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013600                                                                          
013700     PERFORM IMS-RESTART                                                  
013800                                                                          
013900     OPEN INPUT W01533                                                    
014000     .                                                                    
014100                                                                          
014200                                                                          
014300                                                                          
014400 B-LAES-RENSA-TRANS  SECTION.                                             
014500                                                                          
014600     MOVE RENS-AREA TO W-WDR301KY                                         
014700     PERFORM IMS-GET-FILC-FIL                                             
014800     IF SEGMENT-FINNS                                                     
014900       PERFORM IMS-DLET-FILC-FIL                                          
015000       ADD 1 TO CHKP-ANT                                                  
015100                W-ANT-DEL                                                 
015200     END-IF                                                               
015300     .                                                                    
015400                                                                          
015500                                                                          
015600 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W01533                                                         
015900                                                                          
016000     DISPLAY 'ANTAL LÄSTA POSTER = ' W-ANT-IN                             
016100     DISPLAY 'ANTAL DELETE       = ' W-ANT-DEL                            
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W01533  SECTION.                                                
016500                                                                          
016600     READ W01533 INTO RENS-AREA                                           
016700     AT END                                                               
016800        MOVE HIGH-VALUE TO RENS-AREA                                      
016900        MOVE JA TO W01533-EOF-SW                                          
017000     NOT AT END                                                           
017100        ADD +1 TO W-ANT-IN                                                
017200     END-READ                                                             
017300     .                                                                    
017400                                                                          
017500                                                                          
017600                                                                          
017700 X-TAG-CHECKPOINT   SECTION.                                              
017800                                                                          
017900     PERFORM IMS-CHECKPOINT                                               
018000     MOVE ZERO TO CHKP-ANT                                                
018100     .                                                                    
018200     EJECT                                                                
018300* --- IMS SEKTIONER ---                                                   
018400                                                                          
018500 IMS-GET-FILC-FIL SECTION.                                                
018600                                                                          
018700     STRING 'WDR301  (WDR301KY =' W-WDR301KY-X ')'                        
018800          DELIMITED BY SIZE INTO SSA1                                     
018900     MOVE '  GE' TO GODK-STATUSKODER                                      
019000     CALL CBLTDLI USING GHU FILC-PCB DLI-IO-WLFILC01 SSA1                 
019100     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
019200     PERFORM IMS-STATUSKONTROLL                                           
019300     .                                                                    
019400                                                                          
019500                                                                          
019600 IMS-DLET-FILC-FIL SECTION.                                               
019700                                                                          
019800     MOVE '  ' TO GODK-STATUSKODER                                        
019900     CALL CBLTDLI USING DLET FILC-PCB DLI-IO-WLFILC01                     
020000     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
020100     PERFORM IMS-STATUSKONTROLL                                           
020200     .                                                                    
020300                                                                          
020400     EJECT                                                                
020500 IMS-RESTART SECTION.                                                     
020600                                                                          
020700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020800     MOVE '  ' TO GODK-STATUSKODER                                        
020900     CALL CBLTDLI USING XRST MSG-PCB                                      
021000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021100                        CHKP-AREA-LENGTH CHKP-AREA                        
021200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021300     PERFORM IMS-STATUSKONTROLL                                           
021400     .                                                                    
021500                                                                          
021600                                                                          
021700 IMS-CHECKPOINT SECTION.                                                  
021800                                                                          
021900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022000     MOVE '  XD' TO GODK-STATUSKODER                                      
022100     CALL CBLTDLI USING CHKP MSG-PCB                                      
022200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022300                        CHKP-AREA-LENGTH CHKP-AREA                        
022400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022500     PERFORM IMS-STATUSKONTROLL                                           
022600                                                                          
022700     IF IMS-EJ-OK                                                         
022800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022900       DISPLAY FELTEXT                                                    
023000       CALL FELLOG                                                        
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 IMS-STATUSKONTROLL SECTION.                                              
023500                                                                          
023600     SET STATUS-IX TO 1                                                   
023700     SEARCH GODK-STATUS                                                   
023800       AT END                                                             
023900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024000           DELIMITED BY SIZE INTO FELTEXT                                 
024100         DISPLAY FELTEXT                                                  
024200         CALL FELLOG                                                      
024300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024400         CONTINUE                                                         
024500     END-SEARCH                                                           
024600     .                                                                    

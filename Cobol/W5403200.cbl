000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5403200.                                                
000400*AUTHOR.         KARL JOHAN HANSSON.                                      
000500*DATE-WRITTEN.   96/02/22.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FIL MED KDVTH OCH UPPDATERAR WDK6 (611-SEGMENTET)          
001100*        MED DETTA VÄRDE.                                                 
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK6                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- FIL MED NYA KDVTH-VÄRDEN.                                  
002500     SELECT W54030                     ASSIGN TO W54032D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000                                                                          
003100 FD  W54030                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400     SKIP2                                                                
003500*01  -COPY W54030      -L.                                                
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W5403200'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 01  CHKP-VAR.                                                            
004600 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004700 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004800 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005100 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
005200                                                                          
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W54030-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W54030                       VALUE 'J'.                   
005900                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
007000                                                                          
007100*01  AREA -COPY W54030     -PRE IN-                                       
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007400     SKIP3                                                                
007500 01  NYCKLAR-TILL-DLI.                                                    
007600     03  W-IDARTNR-X.                                                     
007700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007800     03  W-KDSEGKEY-X.                                                    
007900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008000     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     88  IMS-EJ-OK                           VALUE 'XD'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000     SKIP3                                                                
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
010300     SKIP3                                                                
010400     03  WDK601 REDEFINES IO-AREA.                                        
010500*        05  -COPY WDK601                                                 
010600     SKIP3                                                                
010700     03  WDK611 REDEFINES IO-AREA.                                        
010800*        05  -COPY WDK611                                                 
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011300                                                                          
011400*01  -COPY W0008  -PRE WDK6-                                              
011500     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
011800     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
011900                                                                          
012000                                                                          
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     PERFORM S01-LAES-W54030                                              
012400     PERFORM UNTIL END-OF-W54030                                          
012500       IF CHKP-ANT > CHKP-MAX                                             
012600         PERFORM B-TAG-CHECKPOINT                                         
012700       END-IF                                                             
012800                                                                          
012900       MOVE IN-IDARTNR    TO W-IDARTNR                                    
013000       PERFORM IMS-GHU-WDK611                                             
013100       IF SEGMENT-FINNS                                                   
013200         MOVE IN-KDVTH    TO CLAG-KDVTH                                   
013300         PERFORM IMS-REPL-WDK611                                          
013400         ADD +1           TO CHKP-ANT                                     
013500       END-IF                                                             
013600       PERFORM S01-LAES-W54030                                            
013700     END-PERFORM                                                          
013800                                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100                                                                          
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700                                                                          
014800     PERFORM IMS-RESTART                                                  
014900                                                                          
015000     OPEN INPUT W54030                                                    
015100                                                                          
015200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015300     .                                                                    
015400     SKIP2                                                                
015500 Z-FINIT SECTION.                                                         
015600                                                                          
015700     CLOSE W54030                                                         
015800                                                                          
015900     MOVE 'S' TO POSTSUM-OPKOD                                            
016000     CALL POSTSUM USING POSTSUM-PARM                                      
016100     .                                                                    
016200     SKIP2                                                                
016300 S01-LAES-W54030  SECTION.                                                
016400                                                                          
016500     READ W54030 INTO IN-AREA                                             
016600     AT END                                                               
016700        SET END-OF-W54030 TO TRUE                                         
016800     NOT AT END                                                           
016900        MOVE 'W54030'   TO POSTSUM-FDNAMN                                 
017000        MOVE 'W54032D1' TO POSTSUM-DDNAMN2                                
017100        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
017200        CALL POSTSUM USING POSTSUM-PARM                                   
017300     END-READ                                                             
017400     .                                                                    
017500     EJECT                                                                
017600 B-TAG-CHECKPOINT   SECTION.                                              
017700                                                                          
017900     PERFORM IMS-CHECKPOINT                                               
018000     MOVE ZERO TO CHKP-ANT                                                
018100     .                                                                    
018200     EJECT                                                                
018300* --- IMS SEKTIONER ---                                                   
018400                                                                          
018500 IMS-GHU-WDK611 SECTION.                                                  
018600                                                                          
018610     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018620          DELIMITED BY SIZE INTO SSA1                                     
018630     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
018640          DELIMITED BY SIZE INTO SSA2                                     
019000     MOVE '  GE' TO GODK-STATUSKODER                                      
019100     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA SSA1 SSA2                
019200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
019300     PERFORM IMS-STATUSKONTROLL                                           
019400     .                                                                    
019500     SKIP3                                                                
019600 IMS-REPL-WDK611 SECTION.                                                 
019700                                                                          
019800     MOVE '  ' TO GODK-STATUSKODER                                        
019900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA                         
020000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
020100     PERFORM IMS-STATUSKONTROLL                                           
020200     .                                                                    
020300     EJECT                                                                
020400 IMS-RESTART SECTION.                                                     
020500                                                                          
020600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020700     MOVE '  ' TO GODK-STATUSKODER                                        
020800     CALL CBLTDLI USING XRST MSG-PCB                                      
020900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021000                        CHKP-AREA-LENGTH CHKP-AREA                        
021100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021200     PERFORM IMS-STATUSKONTROLL                                           
021300     .                                                                    
021400     EJECT                                                                
021500 IMS-CHECKPOINT SECTION.                                                  
021600                                                                          
021700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021800     MOVE '  XD' TO GODK-STATUSKODER                                      
021900     CALL CBLTDLI USING CHKP MSG-PCB                                      
022000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022100                        CHKP-AREA-LENGTH CHKP-AREA                        
022200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022300     PERFORM IMS-STATUSKONTROLL                                           
022310                                                                          
022400     IF IMS-EJ-OK                                                         
022500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022600       DISPLAY FELTEXT                                                    
022700       CALL FELLOG                                                        
022800     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 IMS-STATUSKONTROLL SECTION.                                              
023300                                                                          
023400     SET STATUS-IX TO 1                                                   
023500     SEARCH GODK-STATUS                                                   
023600       AT END                                                             
023700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023800         DELIMITED BY SIZE INTO FELTEXT                                   
023900         DISPLAY FELTEXT                                                  
024000         CALL FELLOG                                                      
024100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024200         CONTINUE                                                         
024300     END-SEARCH                                                           
024400     .                                                                    

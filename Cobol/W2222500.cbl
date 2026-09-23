000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222500.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   99/12/07.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRAD PROGNOS PÅ WDK6                             
001100*        INGET ÅTERSTARTSREGISTER BEHÖVS                                  
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK6                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900****-------------------------------------------------------------         
002000*                                                                         
002100*    2016-03-16  E'TRACKER 10243132 CHINA EXPORT PROJECT.KRAV             
002200*                VID KDERS > 20 SÄTTES KVPB-SEP = 0.0                     
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- PROGNOS SOM SKA FÖRÄNDRAS                                  
003300     SELECT W22225                     ASSIGN TO W22225D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W22225                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W22221      -L.                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W2222500'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
005300     SKIP2                                                                
005400 01  CHKP-VAR.                                                            
005500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006000 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
006100                                                                          
006200                                                                          
006300                                                                          
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006700                                                                          
006800 77  W22225-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W22225                       VALUE 'J'.                   
007000                                                                          
007100     EJECT                                                                
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900*                                                                         
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008800 01  IN-AREA-START               PIC X(24)   VALUE                        
008900                                             'IN-AREA-START'.             
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W22221     -PRE IN-                                       
009300*                                                                         
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  NYCKLAR-TILL-DLI.                                                    
009800     03  W-IDARTNR-X.                                                     
009900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010000     03  W-IDDC-X.                                                        
010100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010900     88  IMS-EJ-OK                           VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100     EJECT                                                                
012200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
012300     SKIP3                                                                
012400 01  DLI-IO-WDK611.                                                       
012500*    03  -COPY WDK611                                                     
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900*01  -COPY W0009   -PRE MSG-                                              
013000     EJECT                                                                
013100*01  -COPY W0008  -PRE WDK6-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
013500     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
013600                                                                          
013700     PERFORM A-INIT                                                       
013800     PERFORM S01-LAES-W22225                                              
013900     PERFORM UNTIL END-OF-W22225                                          
014000                                                                          
014100       PERFORM B-BEHANDLA-POSTER                                          
014200                                                                          
014300       PERFORM S01-LAES-W22225                                            
014400                                                                          
014500     END-PERFORM                                                          
014600     PERFORM Z-FINIT                                                      
014700                                                                          
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP2                                                                
015400                                                                          
015500     ACCEPT DAGENS-DATUM FROM DATE                                        
015600                                                                          
015700     OPEN INPUT W22225                                                    
015800                                                                          
015900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016000     PERFORM IMS-RESTART                                                  
016100     .                                                                    
016200     EJECT                                                                
016300 B-BEHANDLA-POSTER SECTION.                                               
016400                                                                          
016500     MOVE IN-IDARTNR TO W-IDARTNR                                         
016600     PERFORM IMS-GHU-K611                                                 
016700                                                                          
016710     IF (CLAG-KDERS > 20) AND                                             
016711        (CLAG-IDDC-REF NOT = SPACE)                                       
016712                                                                          
016720       MOVE IN-KVPB-SEP      TO CLAG-KVPB-SEP                             
016721       PERFORM IMS-REPL-K611                                              
016722       ADD +1 TO CHKP-ANT                                                 
016723       IF CHKP-ANT > CHKP-MAX                                             
016724         PERFORM IMS-CHECKPOINT                                           
016725         MOVE +0 TO CHKP-ANT                                              
016726       END-IF                                                             
016730     ELSE                                                                 
016800       IF  CLAG-KVPB-SEP = IN-KVPB-SEP                                    
016900       AND CLAG-RVPROFEL = IN-RVPROFEL                                    
017000         CONTINUE                                                         
017100       ELSE                                                               
017200         MOVE IN-KVPB-SEP      TO CLAG-KVPB-SEP                           
017300         MOVE IN-RVPROFEL      TO CLAG-RVPROFEL                           
017400         IF IN-KVPB-SEP > 0 AND                                           
017500            CLAG-KDUART = 'P'                                             
017600             MOVE SPACE TO CLAG-KDUART                                    
017700         END-IF                                                           
017800         PERFORM IMS-REPL-K611                                            
017900         ADD +1 TO CHKP-ANT                                               
018000         IF CHKP-ANT > CHKP-MAX                                           
018100           PERFORM IMS-CHECKPOINT                                         
018200           MOVE +0 TO CHKP-ANT                                            
018300         END-IF                                                           
018400       END-IF                                                             
018410     END-IF                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 Z-FINIT SECTION.                                                         
018800                                                                          
018900     CLOSE W22225                                                         
019000     SKIP2                                                                
019100     MOVE 'S' TO POSTSUM-OPKOD                                            
019200     CALL POSTSUM USING POSTSUM-PARM                                      
019300                                                                          
019400     .                                                                    
019500     EJECT                                                                
019600 S01-LAES-W22225  SECTION.                                                
019700     SKIP2                                                                
019800     READ W22225 INTO IN-AREA                                             
019900     AT END                                                               
020000        SET END-OF-W22225 TO TRUE                                         
020100                                                                          
020200     NOT AT END                                                           
020300        MOVE 'W22225' TO POSTSUM-FDNAMN                                   
020400        MOVE 'W22225D1' TO POSTSUM-DDNAMN2                                
020500        CALL POSTSUM USING POSTSUM-PARM                                   
020600     END-READ                                                             
020700     .                                                                    
020800     EJECT                                                                
020900 IMS-GHU-K611 SECTION.                                                    
021000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021100          DELIMITED BY SIZE INTO SSA1                                     
021200     MOVE 'WDK611  '       TO SSA2                                        
021300     MOVE '  ' TO GODK-STATUSKODER                                        
021400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
021500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021600     PERFORM IMS-STATUSKONTROLL                                           
021700     .                                                                    
021800     EJECT                                                                
021900 IMS-REPL-K611 SECTION.                                                   
022000                                                                          
022100     MOVE '  ' TO GODK-STATUSKODER                                        
022200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
022300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     .                                                                    
022600     EJECT                                                                
022700 IMS-RESTART SECTION.                                                     
022800     SKIP2                                                                
022900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023000     MOVE '  ' TO GODK-STATUSKODER                                        
023100     CALL CBLTDLI USING XRST MSG-PCB                                      
023200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023300                        CHKP-AREA-LENGTH CHKP-AREA                        
023400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     .                                                                    
023700     EJECT                                                                
023800 IMS-CHECKPOINT SECTION.                                                  
023900     SKIP2                                                                
024000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024100     MOVE '  XD' TO GODK-STATUSKODER                                      
024200     CALL CBLTDLI USING CHKP MSG-PCB                                      
024300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024400                        CHKP-AREA-LENGTH CHKP-AREA                        
024500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024600     PERFORM IMS-STATUSKONTROLL                                           
024700                                                                          
024800     IF IMS-EJ-OK                                                         
024900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
025000       DISPLAY FELTEXT                                                    
025100       CALL FELLOG                                                        
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 IMS-STATUSKONTROLL SECTION.                                              
025600     SKIP2                                                                
025700     SET STATUS-IX TO 1                                                   
025800     SEARCH GODK-STATUS                                                   
025900       AT END                                                             
026000         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
026100         DISPLAY FELTEXT                                                  
026200         CALL FELLOG                                                      
026300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026400         CONTINUE                                                         
026500     END-SEARCH                                                           
026600     .                                                                    

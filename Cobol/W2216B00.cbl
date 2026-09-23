000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2216B00.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   11/11/11.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FIL W2216B MED ARTIKLAR SOM SKALL                          
001100*        DELETAS PÅ WDGX2248                                              
001200*        (FRÅN PGM W2216A - BMP MED ÅTERSTARTSPROBLEM                     
001300*        UTAN DENNA LÖSNING)                                              
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDGX2248 (WDR5)                            
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- INFIL LEVNR-ARTNR                                          
003000     SELECT W2216B                     ASSIGN TO W2216BD1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W2216B                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  -COPY W2216B      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2216B00'.            
004500                                                                          
004600 01  CHKP-VAR.                                                            
004700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005200 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000                                                                          
006100 77  W2216B-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W2216B                       VALUE 'J'.                   
006300     EJECT                                                                
006400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500 01  FILLER REDEFINES DAGENS-DATUM.                                       
006600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006900     SKIP3                                                                
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
008400*01  AREA -COPY W2216B     -PRE IN-                                       
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000     03  W-WDGXKEY-2247-X.                                                
009100         05  FILLER              PIC X(04)    VALUE '2247'.               
009200         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
009510     03  W-KY2248-X.                                                      
009520         05  W-IDDC-2248         PIC X(2)     VALUE SPACE.                
009530         05  W-IDLEVNR-2248      PIC X(5)     VALUE SPACE.                
009540         05  W-IDARTNR-2248      PIC S9(9)    VALUE ZERO COMP-3.          
009600     SKIP2                                                                
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                   PIC XX.                                  
009900     88  SEGMENT-FINNS                       VALUE '  '.                  
010000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010300     88  IMS-EJ-OK                           VALUE 'XD'.                  
010400     SKIP2                                                                
010500 01  GODK-STATUSKODER.                                                    
010600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010700     SKIP3                                                                
010800 01  SSA1                        PIC X(128).                              
010900 01  SSA2                        PIC X(128).                              
011000     EJECT                                                                
011100*    --- IMS FUNKTIONSKODER                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011310*********************************************************                 
011400*    ---  DLI INPUT-OUTPUT AREA                                           
011710*********************************************************                 
011720 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
011730 01  DLI-IO-WDGX2248.                                                     
011740*    03  -COPY  WDGX2248                                                  
012500     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE WDR5-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING MSG-PCB WDR5-PCB.                              
014100     ENTRY 'DLITCBL' USING MSG-PCB WDR5-PCB.                              
014200                                                                          
014300     SKIP2                                                                
014400     PERFORM A-INIT                                                       
014500     PERFORM S01-LAES-W2216B                                              
014600     PERFORM UNTIL END-OF-W2216B                                          
014700       IF CHKP-ANT > CHKP-MAX                                             
014800         PERFORM X-TAG-CHECKPOINT                                         
014900       END-IF                                                             
015000                                                                          
015100       PERFORM B-DELETE-WDGX2248                                          
015200                                                                          
015300       PERFORM S01-LAES-W2216B                                            
015400     END-PERFORM                                                          
015500                                                                          
015600                                                                          
015700     PERFORM Z-FINIT                                                      
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400     SKIP2                                                                
016500                                                                          
016600     PERFORM IMS-RESTART                                                  
016700                                                                          
016800     OPEN INPUT W2216B                                                    
017000                                                                          
017100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017200     .                                                                    
017300     EJECT                                                                
017400 B-DELETE-WDGX2248 SECTION.                                               
017500     SKIP2                                                                
017600     MOVE IN-IDDC    TO W-IDDC-2248                                       
017610     MOVE IN-IDLEVNR TO W-IDLEVNR-2248                                    
017700     MOVE IN-IDARTNR TO W-IDARTNR-2248                                    
017800                                                                          
017900     PERFORM IMS-GHU-WDGX2248                                             
018000                                                                          
018100     IF SEGMENT-FINNS                                                     
018200        PERFORM IMS-DLET-WDGX2248                                         
018300        ADD 1 TO CHKP-ANT                                                 
018400                                                                          
018500        MOVE 'WDR5'     TO POSTSUM-FDNAMN                                 
018600        MOVE 'WDGX2248' TO POSTSUM-DDNAMN2                                
018700        MOVE 'DLET'     TO POSTSUM-TRANSTYP                               
018800        CALL POSTSUM USING POSTSUM-PARM                                   
018900     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 Z-FINIT SECTION.                                                         
019300                                                                          
019500     CLOSE W2216B                                                         
019600     SKIP2                                                                
019700     MOVE 'S' TO POSTSUM-OPKOD                                            
019800     CALL POSTSUM USING POSTSUM-PARM                                      
019900     .                                                                    
020000     EJECT                                                                
020100 S01-LAES-W2216B  SECTION.                                                
020200     SKIP2                                                                
020300     READ W2216B INTO IN-AREA                                             
020400     AT END                                                               
020500        SET END-OF-W2216B TO TRUE                                         
020600                                                                          
020700     NOT AT END                                                           
020800        MOVE 'W2216B'   TO POSTSUM-FDNAMN                                 
020900        MOVE 'W2216BD1' TO POSTSUM-DDNAMN2                                
021000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
021100        CALL POSTSUM USING POSTSUM-PARM                                   
021400     END-READ                                                             
021500     .                                                                    
021600     EJECT                                                                
021700 X-TAG-CHECKPOINT   SECTION.                                              
021800                                                                          
021900* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
022000     PERFORM IMS-CHECKPOINT                                               
022100     MOVE ZERO TO CHKP-ANT                                                
022200     .                                                                    
022300     EJECT                                                                
022400* --- IMS SEKTIONER ---                                                   
022500     SKIP3                                                                
022600 IMS-GHU-WDGX2248 SECTION.                                                
022610     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X    ')'                 
022800          DELIMITED BY SIZE INTO SSA1                                     
022900     STRING 'WDGX2248(KY2248   =' W-KY2248-X ')'                          
023000          DELIMITED BY SIZE INTO SSA2                                     
023100     MOVE '  GE' TO GODK-STATUSKODER                                      
023200     CALL CBLTDLI USING GHU  WDR5-PCB DLI-IO-WDGX2248 SSA1 SSA2           
023300     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600     SKIP3                                                                
023700 IMS-DLET-WDGX2248 SECTION.                                               
023800                                                                          
023900     MOVE '  ' TO GODK-STATUSKODER                                        
024000     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDGX2248                     
024100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
024200     PERFORM IMS-STATUSKONTROLL                                           
024400     .                                                                    
024500     EJECT                                                                
024600 IMS-RESTART SECTION.                                                     
024700     SKIP2                                                                
024800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024900     MOVE '  ' TO GODK-STATUSKODER                                        
025000     CALL CBLTDLI USING XRST MSG-PCB                                      
025100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025200                        CHKP-AREA-LENGTH CHKP-AREA                        
025300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     EJECT                                                                
025700 IMS-CHECKPOINT SECTION.                                                  
025800     SKIP2                                                                
025900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026000     MOVE '  XD' TO GODK-STATUSKODER                                      
026100     CALL CBLTDLI USING CHKP MSG-PCB                                      
026200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026300                        CHKP-AREA-LENGTH CHKP-AREA                        
026400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026500     PERFORM IMS-STATUSKONTROLL                                           
026600                                                                          
026700     IF IMS-EJ-OK                                                         
026800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026900       DISPLAY FELTEXT                                                    
027000       CALL FELLOG                                                        
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 IMS-STATUSKONTROLL SECTION.                                              
027500     SKIP2                                                                
027600     SET STATUS-IX TO 1                                                   
027700     SEARCH GODK-STATUS                                                   
027800       AT END                                                             
027900         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
028000         DISPLAY FELTEXT                                                  
028100         CALL FELLOG                                                      
028200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028300         CONTINUE                                                         
028400     END-SEARCH                                                           
028500     .                                                                    

000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2216900.                                                
000301 AUTHOR.         JOHAN NIHLBLAD.                                          
000401 DATE-WRITTEN.   15/10/21.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701                                                                          
000801*    FUNCTION:                                                            
000901*        UPDATES KDANSKSEG ON WDK601                                      
001001*                                                                         
001101                                                                          
001201     SKIP3                                                                
001301 ENVIRONMENT DIVISION.                                                    
001401     SKIP2                                                                
001501 INPUT-OUTPUT SECTION.                                                    
001601                                                                          
001701 FILE-CONTROL.                                                            
001801     SKIP2                                                                
001901*          --- BUYERKOD SAMT REFILLKÖPTABELL                              
002001     SELECT W22168                     ASSIGN TO W22169D1.                
002101     EJECT                                                                
002201 DATA DIVISION.                                                           
002301     SKIP3                                                                
002401 FILE SECTION.                                                            
002501     SKIP3                                                                
002601 FD  W22168                                                               
002701     RECORDING       F                                                    
002801     BLOCK CONTAINS  0.                                                   
002901                                                                          
003001*01  -COPY W22168      -L.                                                
003101     EJECT                                                                
003201 WORKING-STORAGE SECTION.                                                 
003301                                                                          
003401                                                                          
003501*    -- CHECKED BY WY2000                                                 
003601 77  IDPGM                       PIC X(8)    VALUE 'W2216900'.            
003701 01  CHKP-VAR.                                                            
003801 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
003901 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004001 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004101 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004201 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004301 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004401 77  JA                          PIC X       VALUE 'J'.                   
004501 77  NEJ                         PIC X       VALUE 'N'.                   
004601     SKIP2                                                                
004701 01  FELTEXT.                                                             
004801     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004901     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005001                                                                          
005101 77  W22168-EOF-SW               PIC X       VALUE 'N'.                   
005201     88  END-OF-W22168                       VALUE 'J'.                   
005301                                                                          
005401     EJECT                                                                
005501 01  ARBETSAREOR.                                                         
005601                                                                          
005701     03 WS-ANTAL-W22168          PIC 9(8)   VALUE ZERO.                   
005801     03 WS-ANTAL-UPD-WDK6        PIC 9(8)   VALUE ZERO.                   
005901     EJECT                                                                
006001 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006101 01  FILLER REDEFINES DAGENS-DATUM.                                       
006201     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006301     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006401     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006501     EJECT                                                                
006601 01  DYNAMISKA-SUBPROGRAM.                                                
006701*                                                                         
006801     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006901     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007201*    --- PARAMETRAR TILL POSTSUM                                          
007301*                                                                         
007401*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007601 01  IN-AREA-START               PIC X(24)   VALUE                        
007701                                             'IN-AREA-START'.             
007801     SKIP2                                                                
007901                                                                          
008001*01  AREA -COPY W22168     -PRE IN-                                       
008101*                                                                         
008201     EJECT                                                                
008301 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008401     SKIP3                                                                
008501 01  NYCKLAR-TILL-DLI.                                                    
008601     03  W-IDARTNR-X.                                                     
008701         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008801     SKIP2                                                                
008901*    --- STATUS-KOD FRÅN IMS                                              
009001 01  STATUS-WS                   PIC XX.                                  
009101     88  SEGMENT-FINNS                       VALUE '  '.                  
009201     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009301     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009401     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009501     88  IMS-EJ-OK                           VALUE 'XD'.                  
009601     SKIP2                                                                
009701 01  GODK-STATUSKODER.                                                    
009801     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009901     SKIP3                                                                
010001 01  SSA1                        PIC X(64).                               
010101     EJECT                                                                
010201*    --- IMS FUNKTIONSKODER                                               
010301*01  -COPY W0003                                                          
010401     EJECT                                                                
010501*    ---  DLI INPUT-OUTPUT AREA                                           
010601                                                                          
010701 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
010801 01  DLI-IO-WDK601.                                                       
010901*    03  -COPY WDK601                                                     
011001     EJECT                                                                
012001                                                                          
012101 LINKAGE SECTION.                                                         
012201                                                                          
012301*01  -COPY W0009   -PRE MSG-                                              
012401                                                                          
012501*01  -COPY W0008  -PRE WDK6-                                              
012601     05  FILLER                  PIC X.                                   
012701     EJECT                                                                
012801 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012901 MAIN SECTION.                                                            
013001     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
013101                                                                          
013201     SKIP2                                                                
013301     PERFORM A-INIT                                                       
013401     PERFORM S01-LAES-W22168                                              
013501     PERFORM UNTIL END-OF-W22168                                          
013601                                                                          
013701       IF CHKP-ANT > CHKP-MAX                                             
013801         PERFORM X-TAG-CHECKPOINT                                         
013901       END-IF                                                             
014001                                                                          
014002       IF IN-FLANSKSEG-CHANGED = JA                                       
014101         PERFORM B-BEHANDLA-POSTER                                        
014102       END-IF                                                             
014201                                                                          
014301       PERFORM S01-LAES-W22168                                            
014401     END-PERFORM                                                          
014501                                                                          
014601                                                                          
014701     PERFORM Z-FINIT                                                      
014801                                                                          
014901     MOVE ZERO TO RETURN-CODE                                             
015001     GOBACK                                                               
015101     .                                                                    
015201     EJECT                                                                
015301 A-INIT SECTION.                                                          
015401     SKIP2                                                                
015501                                                                          
015601     PERFORM IMS-RESTART                                                  
015701                                                                          
015801     OPEN INPUT W22168                                                    
015901     .                                                                    
016001     EJECT                                                                
016101 B-BEHANDLA-POSTER SECTION.                                               
016201                                                                          
016301     MOVE IN-IDARTNR TO W-IDARTNR                                         
016401                                                                          
016501     PERFORM IMS-GHU-WDK601                                               
016601     IF SEGMENT-FINNS                                                     
016701       IF IN-KDANSKSEG NOT = ART-KDANSKSEG                                
016901                                                                          
017001          MOVE IN-KDANSKSEG  TO ART-KDANSKSEG                             
018001                                                                          
018900          PERFORM IMS-REPL-WDK6                                           
019000          ADD +1             TO CHKP-ANT                                  
019100          ADD 1              TO WS-ANTAL-UPD-WDK6                         
019200       END-IF                                                             
019301     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 Z-FINIT SECTION.                                                         
019700                                                                          
019801     CLOSE W22168                                                         
019901     DISPLAY 'ANTAL W22168    : ' WS-ANTAL-W22168                         
020000     DISPLAY 'ANTAL REPL WDK6 : ' WS-ANTAL-UPD-WDK6                       
020100     .                                                                    
020200     EJECT                                                                
020301 S01-LAES-W22168  SECTION.                                                
020400     SKIP2                                                                
020501     READ W22168 INTO IN-AREA                                             
020600     AT END                                                               
020701        SET END-OF-W22168 TO TRUE                                         
020800                                                                          
020900     NOT AT END                                                           
021001        ADD 1                TO WS-ANTAL-W22168                           
021100     END-READ                                                             
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500 X-TAG-CHECKPOINT   SECTION.                                              
021600                                                                          
021700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
021800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
021900     PERFORM IMS-CHECKPOINT                                               
022000     MOVE ZERO TO CHKP-ANT                                                
022100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
022200     .                                                                    
022300     EJECT                                                                
022400* --- IMS SEKTIONER ---                                                   
022500                                                                          
022600     EJECT                                                                
022701 IMS-GHU-WDK601 SECTION.                                                  
022800                                                                          
022900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023000          DELIMITED BY SIZE INTO SSA1                                     
023100     MOVE '  GE' TO GODK-STATUSKODER                                      
023201     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
023300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600     EJECT                                                                
023700 IMS-REPL-WDK6 SECTION.                                                   
023800                                                                          
023900     MOVE '  ' TO GODK-STATUSKODER                                        
024001     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK601                       
024100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024200     PERFORM IMS-STATUSKONTROLL                                           
024300     .                                                                    
024400     EJECT                                                                
024500 IMS-RESTART SECTION.                                                     
024600     SKIP2                                                                
024700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024800     MOVE '  ' TO GODK-STATUSKODER                                        
024900     CALL CBLTDLI USING XRST MSG-PCB                                      
025000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025100                        CHKP-AREA-LENGTH CHKP-AREA                        
025200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025300     PERFORM IMS-STATUSKONTROLL                                           
025400     .                                                                    
025500     EJECT                                                                
025600 IMS-CHECKPOINT SECTION.                                                  
025700     SKIP2                                                                
025800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025900     MOVE '  XD' TO GODK-STATUSKODER                                      
026000     CALL CBLTDLI USING CHKP MSG-PCB                                      
026100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026200                        CHKP-AREA-LENGTH CHKP-AREA                        
026300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026400     PERFORM IMS-STATUSKONTROLL                                           
026500                                                                          
026600     IF IMS-EJ-OK                                                         
026700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026800       DISPLAY FELTEXT                                                    
026900       CALL FELLOG                                                        
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 IMS-STATUSKONTROLL SECTION.                                              
027400     SKIP2                                                                
027500     SET STATUS-IX TO 1                                                   
027600     SEARCH GODK-STATUS                                                   
027700       AT END                                                             
027800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027900           DELIMITED BY SIZE INTO FELTEXT                                 
028000         DISPLAY FELTEXT                                                  
028100         CALL FELLOG                                                      
028200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028300         CONTINUE                                                         
028400     END-SEARCH                                                           
028500     .                                                                    

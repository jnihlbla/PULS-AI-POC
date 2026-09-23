000101 ID DIVISION.                                                             
000201     SKIP2                                                                
000301 PROGRAM-ID.     W2715300.                                                
000401*AUTHOR.         JOHAN NIHLBLAD.                                          
000501*DATE-WRITTEN.   19/11/18.                                                
000601                                                                          
000701*    REMARKS                                                              
000801*                                                                         
000901*    FUNKTION:                                                            
001001*        UPDATES FORECAST ON WDK7 FROM FORECAST                           
001101*        IN THE FUTUER(KVPB-JUST)                                         
001201*                                                                         
001301*        PROGRAMMET UPPDATERAR WDK7                                       
001401*                                                                         
001501*    ABENDKODER:                                                          
001601*        U0016 -  . . . .                                                 
001701*        U1000 -  . . . .                                                 
001801*                                                                         
001901                                                                          
002001     SKIP3                                                                
002101 ENVIRONMENT DIVISION.                                                    
002201     SKIP2                                                                
002301 INPUT-OUTPUT SECTION.                                                    
002401                                                                          
002501 FILE-CONTROL.                                                            
002601     SKIP2                                                                
002701*          --- PROGNOS SOM SKA FÖRÄNDRAS                                  
002801     SELECT W27153                     ASSIGN TO W27153D1.                
002901     EJECT                                                                
003001 DATA DIVISION.                                                           
003101     SKIP3                                                                
003201 FILE SECTION.                                                            
003301     SKIP3                                                                
003401 FD  W27153                                                               
003501     RECORDING       F                                                    
003601     BLOCK CONTAINS  0.                                                   
003701                                                                          
003801*01  -COPY W27153      -L.                                                
003901     EJECT                                                                
004001 WORKING-STORAGE SECTION.                                                 
004101     SKIP2                                                                
004201                                                                          
004301*    -- CHECKED BY WY2000                                                 
004401 77  IDPGM                       PIC X(8)    VALUE 'W2715300'.            
004501 77  JA                          PIC X       VALUE 'J'.                   
004601 77  NEJ                         PIC X       VALUE 'N'.                   
004701 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004801     SKIP2                                                                
004901 01  CHKP-VAR.                                                            
005001 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005101 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005201 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005301 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005401 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005501 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005601                                                                          
005701                                                                          
005801                                                                          
005901 01  FELTEXT.                                                             
006001     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006101     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201                                                                          
006301 77  W27153-EOF-SW               PIC X       VALUE 'N'.                   
006401     88  END-OF-W27153                       VALUE 'J'.                   
006501                                                                          
006801     EJECT                                                                
006901 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007001 01  FILLER REDEFINES DAGENS-DATUM.                                       
007101     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007201     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007301     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007401     EJECT                                                                
007501 01  DYNAMISKA-SUBPROGRAM.                                                
007601*                                                                         
007701     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007801     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008001     EJECT                                                                
008101*    --- PARAMETRAR TILL POSTSUM                                          
008201*                                                                         
008301*01  -COPY W0005   -PRE  POSTSUM-                                         
008401     EJECT                                                                
008501 01  IN-AREA-START               PIC X(24)   VALUE                        
008601                                             'IN-AREA-START'.             
008701     SKIP2                                                                
008801                                                                          
008901*01  AREA -COPY W27153     -PRE IN-                                       
009001*                                                                         
009101     EJECT                                                                
009201 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009301     SKIP3                                                                
009401 01  NYCKLAR-TILL-DLI.                                                    
009501     03  W-IDARTNR-X.                                                     
009601         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009701     03  W-IDDC-X.                                                        
009801         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009901     03  W-KDSEGKEY-X.                                                    
010001         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
010101                                                                          
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
011601 01  SSA3                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012300     SKIP3                                                                
012401 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
012501 01  DLI-IO-WDK701.                                                       
012601*    03  -COPY WDK701                                                     
012701     EJECT                                                                
012801                                                                          
012901 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
013001 01  DLI-IO-WDK711.                                                       
013101*    03  -COPY WDK711                                                     
013201     EJECT                                                                
013301                                                                          
013401 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK727'.                      
013501 01  DLI-IO-WDK727.                                                       
013601*    03  -COPY WDK727                                                     
013701     EJECT                                                                
013801                                                                          
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0009   -PRE MSG-                                              
014200     EJECT                                                                
014300*01  -COPY W0008  -PRE WDK7-                                              
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB.                              
014700     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB.                              
014800                                                                          
014900     PERFORM A-INIT                                                       
015001     PERFORM S01-LAES-W27153                                              
015101     PERFORM UNTIL END-OF-W27153                                          
015200                                                                          
015300       PERFORM B-BEHANDLA-POSTER                                          
015400                                                                          
015501       PERFORM S01-LAES-W27153                                            
015600                                                                          
015700     END-PERFORM                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500     SKIP2                                                                
016600                                                                          
016700     ACCEPT DAGENS-DATUM FROM DATE                                        
016800                                                                          
016900     PERFORM IMS-RESTART                                                  
017000                                                                          
017101     OPEN INPUT W27153                                                    
017200                                                                          
017300                                                                          
017400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017500     .                                                                    
017600     EJECT                                                                
017700 B-BEHANDLA-POSTER SECTION.                                               
017800                                                                          
017900     MOVE IN-IDARTNR TO W-IDARTNR                                         
018000     MOVE IN-IDDC    TO W-IDDC                                            
018401     IF CHKP-ANT > CHKP-MAX                                               
018501       PERFORM X-TAG-CHECKPOINT                                           
018601     END-IF                                                               
019001     PERFORM IMS-GHU-WDK711                                               
019100                                                                          
019201     IF IN-KVPB-JUST-1 > ZERO                                             
019302       MOVE IN-KVPB-JUST-1    TO SLAG-KVPB-REF                            
019303                                 SLAG-KVPB-HIST                           
019401       MOVE DAGENS-DATUM      TO SLAG-TIREFMPB                            
019402                                 SLAG-TIREFSTA                            
019403       MOVE 'A'               TO SLAG-KDREFSTA                            
019501       PERFORM IMS-REPL-WDK711                                            
019601       PERFORM IMS-GHNP-WDK727                                            
019701       IF IN-KVPB-JUST-2 > ZERO                                           
019901         MOVE IN-KVPB-JUST-2 TO PROG-KVPB-JUST(1)                         
020001         MOVE IN-TIPBJUST-2  TO PROG-TIPBJUST(1)                          
020101         MOVE ZERO           TO PROG-KVPB-JUST(2)                         
020201                                PROG-TIPBJUST(2)                          
020301         PERFORM IMS-REPL-WDK727                                          
020401         ADD +1 TO CHKP-ANT                                               
020501       ELSE                                                               
020601         PERFORM IMS-DLET-WDK727                                          
020701         ADD +1 TO CHKP-ANT                                               
020801       END-IF                                                             
020901     ELSE                                                                 
021001       PERFORM IMS-GHNP-WDK727                                            
021101       IF SEGMENT-FINNS                                                   
021201         PERFORM IMS-DLET-WDK727                                          
021301         ADD +1 TO CHKP-ANT                                               
021401       END-IF                                                             
021501     END-IF                                                               
023001     .                                                                    
023101     EJECT                                                                
023201 Z-FINIT SECTION.                                                         
023301                                                                          
023401     CLOSE W27153                                                         
023501     SKIP2                                                                
023601     MOVE 'S' TO POSTSUM-OPKOD                                            
023701     CALL POSTSUM USING POSTSUM-PARM                                      
023801                                                                          
023901     .                                                                    
024001     EJECT                                                                
024101 S01-LAES-W27153  SECTION.                                                
024201     SKIP2                                                                
024301     READ W27153 INTO IN-AREA                                             
024401     AT END                                                               
024501        SET END-OF-W27153 TO TRUE                                         
024601                                                                          
024701     NOT AT END                                                           
024801        MOVE 'W27153' TO POSTSUM-FDNAMN                                   
024901        MOVE 'W27153D1' TO POSTSUM-DDNAMN2                                
025001        CALL POSTSUM USING POSTSUM-PARM                                   
025101     END-READ                                                             
025201     .                                                                    
025301     EJECT                                                                
025401 X-TAG-CHECKPOINT   SECTION.                                              
025501                                                                          
025601* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
025701* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
025801     PERFORM IMS-CHECKPOINT                                               
025901     MOVE ZERO TO CHKP-ANT                                                
026001* --- LÄS OM DATABAS OM DET BEHÖVS                                        
026101     .                                                                    
026201     EJECT                                                                
026301* --- IMS SEKTIONER ---                                                   
026401     SKIP3                                                                
026501     EJECT                                                                
026601 IMS-RESTART SECTION.                                                     
026701     SKIP2                                                                
026801     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026901     MOVE '  ' TO GODK-STATUSKODER                                        
027001     CALL CBLTDLI USING XRST MSG-PCB                                      
027101                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027201                        CHKP-AREA-LENGTH CHKP-AREA                        
027301     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027401     PERFORM IMS-STATUSKONTROLL                                           
027501     .                                                                    
027601     EJECT                                                                
027701 IMS-CHECKPOINT SECTION.                                                  
027801     SKIP2                                                                
027901     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028001     MOVE '  XD' TO GODK-STATUSKODER                                      
028101     CALL CBLTDLI USING CHKP MSG-PCB                                      
028201                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028301                        CHKP-AREA-LENGTH CHKP-AREA                        
028401     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028501     PERFORM IMS-STATUSKONTROLL                                           
028601                                                                          
028701     IF IMS-EJ-OK                                                         
028801       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028901       DISPLAY FELTEXT                                                    
029001       CALL FELLOG                                                        
029101     END-IF                                                               
029201     .                                                                    
029301     EJECT                                                                
030401 IMS-GHU-WDK711 SECTION.                                                  
030501                                                                          
030601     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
030701          DELIMITED BY SIZE INTO SSA1                                     
030801     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
030901          DELIMITED BY SIZE INTO SSA2                                     
031001     MOVE '  ' TO GODK-STATUSKODER                                        
031101     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
031201     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
031301     PERFORM IMS-STATUSKONTROLL                                           
031401     .                                                                    
031501     SKIP3                                                                
031601 IMS-GHNP-WDK727 SECTION.                                                 
031701                                                                          
031801     MOVE 'WDK727   '      TO SSA1                                        
032501     MOVE '  GE' TO GODK-STATUSKODER                                      
032601     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK727 SSA1                  
032701     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
032801     PERFORM IMS-STATUSKONTROLL                                           
032901     .                                                                    
033001     EJECT                                                                
033101 IMS-REPL-WDK711 SECTION.                                                 
033201                                                                          
033301     MOVE '  ' TO GODK-STATUSKODER                                        
033401     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
033501     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
033601     PERFORM IMS-STATUSKONTROLL                                           
033701     .                                                                    
033801     EJECT                                                                
033901 IMS-REPL-WDK727 SECTION.                                                 
034001                                                                          
034101     MOVE '  ' TO GODK-STATUSKODER                                        
034201     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK727                       
034301     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
034401     PERFORM IMS-STATUSKONTROLL                                           
034501     .                                                                    
034601     EJECT                                                                
034701 IMS-DLET-WDK727 SECTION.                                                 
034801                                                                          
034901     MOVE '  ' TO GODK-STATUSKODER                                        
035001     CALL CBLTDLI USING DLET WDK7-PCB DLI-IO-WDK727                       
035101     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
035201     PERFORM IMS-STATUSKONTROLL                                           
035301     .                                                                    
035401     EJECT                                                                
035501 IMS-STATUSKONTROLL SECTION.                                              
035601     SKIP2                                                                
035701     SET STATUS-IX TO 1                                                   
035801     SEARCH GODK-STATUS                                                   
035901       AT END                                                             
036001         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
036101         DISPLAY FELTEXT                                                  
036201         CALL FELLOG                                                      
036301       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036401         CONTINUE                                                         
036501     END-SEARCH                                                           
037001     .                                                                    

000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5128N00.                                                
000301 AUTHOR.         BARSHARANI BISHOYE.                                      
000401 DATE-WRITTEN.   JAN 2020.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*                                                                         
000901*       -PROGRAM READS         WDG2, WDB6, WDK6                           
001001*                                                                         
001101*    ABENDKODER:                                                          
001201*        U0016 -  . . . .                                                 
001301*        U1000 -  . . . .                                                 
001401*                                                                         
001501                                                                          
001601 ENVIRONMENT DIVISION.                                                    
001701                                                                          
001801 INPUT-OUTPUT SECTION.                                                    
001901                                                                          
002001 FILE-CONTROL.                                                            
002101*          --- INPUT FROM W51262                                          
002201     SELECT W51280B                    ASSIGN TO W5128ND1.                
002301                                                                          
002401*          --- OUTPUT WITH CONVERTED CURRENCY                             
002501     SELECT W51280NA                   ASSIGN TO W5128ND2.                
002601                                                                          
002701     EJECT                                                                
002801                                                                          
002901 DATA DIVISION.                                                           
003001                                                                          
003101 FILE SECTION.                                                            
003201 FD  W51280B                                                              
003301     RECORDING       F                                                    
003401     BLOCK CONTAINS  0.                                                   
003501 01  IN-POST.                                                             
003601*    03  -COPY W51280    -PRE  IN-  -L.                                   
003701                                                                          
003801 FD  W51280NA                                                             
003901     RECORDING       F                                                    
004001     BLOCK CONTAINS  0.                                                   
004101*01  POST   -COPY W51281 -PRE  UT-  -L.                                   
004201                                                                          
004301 WORKING-STORAGE SECTION.                                                 
004401 77  IDPGM                        PIC X(8)    VALUE 'W5128N00'.           
004501 77  JA                           PIC X       VALUE 'J'.                  
004601 77  NEJ                          PIC X       VALUE 'N'.                  
004701 77  FELTEXT                      PIC X(80).                              
004801 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
004802 77  WS-MARKUP1                   PIC 9V9(3)  VALUE ZERO.                 
004901 77  W51280B-EOF-SW               PIC X       VALUE 'N'.                  
005001     88  END-OF-W51280B                       VALUE 'J'.                  
005101 77  WS-PRARTNTO-TOT              PIC S9(11)V9(2) VALUE +0                
005201                                                   COMP-3.                
005301 77  WS-PRARTNTO                  PIC S9(11)V9(2) VALUE +0                
005401                                                   COMP-3.                
005501 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
005601                                                   COMP-3.                
005701 77  W-REVALUTA                   PIC S9(5) COMP-3 VALUE ZERO.            
005801 77  WS-SAVE-KDVALISO             PIC X(3)         VALUE SPACE.           
005901 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
006001 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
006101 77  WS-DATE-DISPLAY              PIC 9(16)   VALUE ZERO.                 
006201 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
006301 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
006401 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
006601                                                                          
006701 01  DYNAMISKA-SUBPROGRAM.                                                
006801     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
006901     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
007001     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
007101     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
007201     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
007301                                                                          
007401 01  NYCKLAR-TILL-DLI.                                                    
007501     03  W-IDDC-X.                                                        
007601         05  W-IDDC               PIC X(2)    VALUE SPACE.                
007602     03  W-IDFKNGRP-X.                                                    
007603         05  W-IDFKNGRP           PIC S9(5)   VALUE ZERO COMP-3.          
007604     03  W-KDSEGKEY-X.                                                    
007605         05  W-KDSEGKEY           PIC X(1)    VALUE '1'.                  
007606     03  W-IDARTNR-X.                                                     
007607         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
007701                                                                          
007801*    --- PARAMETRAR TILL ABEND                                            
007901 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
008001 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
008101 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
008201     EJECT                                                                
008301                                                                          
008401*    --- PARAMETRAR TILL POSTSUM                                          
008501*01  -COPY W0005   -PRE  POSTSUM-                                         
008601     EJECT                                                                
008701*01  -COPY W510CURR                                                       
008801     EJECT                                                                
008901                                                                          
009001 01  IN-AREA-START               PIC X(24)   VALUE                        
009101                                 'IN-AREA-START  '.                       
009201                                                                          
009301*01  AREA -COPY W51280     -PRE IN-                                       
009401     EJECT                                                                
009501                                                                          
009601 01  UT-AREA-START               PIC X(24)   VALUE                        
009701                                 'UT-AREA-START  '.                       
009801                                                                          
009901*01  AREA -COPY W51281     -PRE UT-                                       
010001     EJECT                                                                
010101                                                                          
010201*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010301 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
010401                                                                          
010501     EJECT                                                                
010601                                                                          
010701*    --- STATUS-KOD FRÅN IMS                                              
010801 01  STATUS-WS                    PIC XX.                                 
010901     88  SEGMENT-FINNS                        VALUE '  '.                 
011001     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
011101                                                                          
011201 01  GODK-STATUSKODER.                                                    
011301     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011401                                                                          
011501 01  SSA1                         PIC X(64).                              
011502 01  SSA2                         PIC X(64).                              
011503 01  SSA3                         PIC X(64).                              
011601     EJECT                                                                
011701                                                                          
011801*    --- IMS FUNKTIONSKODER                                               
011901*01  -COPY W0003                                                          
012001     EJECT                                                                
012101                                                                          
012201*    ---  DLI INPUT-OUTPUT AREA                                           
012301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
012401 01  DLI-IO-WDB601.                                                       
012501*    03  -COPY WDB601                                                     
012601     EJECT                                                                
012701                                                                          
012801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
012901 01  DLI-IO-WDB617.                                                       
013001*    03  -COPY WDB617                                                     
014001     EJECT                                                                
014002 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB622'.                      
014003 01  DLI-IO-WDB622.                                                       
014004*    03  -COPY WDB622                                                     
014005     EJECT                                                                
014006                                                                          
014007 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
014008 01  DLI-IO-WDK601.                                                       
014009*    03  -COPY WDK601                                                     
014010     EJECT                                                                
014101 LINKAGE SECTION.                                                         
014201*01  -COPY W0008  -PRE 9305-                                              
014301     05  FILLER                  PIC X.                                   
014401*01  -COPY W0008  -PRE WDB6-                                              
014501     05  FILLER                  PIC X.                                   
014502*01  -COPY W0008  -PRE WDK6-                                              
014503     05  FILLER                  PIC X.                                   
014601                                                                          
014701     EJECT                                                                
014801                                                                          
014901 PROCEDURE DIVISION  USING 9305-PCB WDB6-PCB WDK6-PCB.                    
015001                                                                          
015101 MAIN SECTION.                                                            
015201     ENTRY 'DLITCBL' USING 9305-PCB WDB6-PCB WDK6-PCB.                    
015301                                                                          
015401     PERFORM A-INIT                                                       
015501                                                                          
015601     PERFORM S01-READ-W51280B                                             
015701     PERFORM UNTIL END-OF-W51280B                                         
015801       PERFORM B-CONVERT-CURRENCY                                         
015901       PERFORM S01-READ-W51280B                                           
016001     END-PERFORM                                                          
016101                                                                          
016201     PERFORM Z-FINIT                                                      
016301     MOVE ZERO TO RETURN-CODE                                             
016401     GOBACK                                                               
016501     .                                                                    
016601     EJECT                                                                
016701                                                                          
016801 A-INIT SECTION.                                                          
016901     OPEN INPUT  W51280B                                                  
017001     OPEN OUTPUT W51280NA                                                 
017101                                                                          
017201     MOVE  ZERO  TO  UT-IDLOPNRM                                          
017401     .                                                                    
017501     EJECT                                                                
017601                                                                          
017701 B-CONVERT-CURRENCY SECTION.                                              
017801                                                                          
017901     PERFORM BA-GET-EXCHRATE                                              
018001                                                                          
020702     COMPUTE WS-PRARTNTO   ROUNDED =                                      
020703             (IN-PRARTNTO * IN-KVAVIS) +                                  
020704             (IN-PRARTNTO * IN-KVAVIS * WS-MARKUP) +                      
020705             (IN-PRARTNTO * IN-KVAVIS * WS-MARKUP1)                       
020706                                                                          
020707     COMPUTE WS-PRARTNTO-TOT  ROUNDED =                                   
020708           ((IN-PRARTNTO * IN-KVAVIS) / (W-PRKURS / W-REVALUTA))          
020709         + ((IN-PRARTNTO * IN-KVAVIS * WS-MARKUP) /                       
020710             (W-PRKURS / W-REVALUTA))                                     
020720         + ((IN-PRARTNTO * IN-KVAVIS * WS-MARKUP1) /                      
020730             (W-PRKURS / W-REVALUTA))                                     
020740                                                                          
020801     MOVE IN-IDPTYP               TO UT-IDPTYP                            
020901     MOVE IN-IDARTNR              TO UT-IDARTNR                           
021001     MOVE IN-IDDC                 TO UT-IDDC                              
022001     MOVE IN-KVAVIS               TO UT-KVAVIS                            
022101     MOVE IN-PRARTNTO             TO UT-PRARTNTO                          
022201     MOVE WS-PRARTNTO             TO UT-PRARTNTO-SEK                      
022301     MOVE W-PRKURS                TO UT-PRKURS                            
022401     MOVE WS-PRARTNTO-TOT         TO UT-SUNTO-TOT                         
022501     MOVE IN-KDVALISO             TO UT-KDVALISO                          
022601     MOVE IN-KDTRADP              TO UT-KDTRADP                           
022701     MOVE IN-IDLEVNR              TO UT-IDLEVNR                           
022801     MOVE IN-IDFAKT               TO UT-IDFAKT                            
022901     COMPUTE WS-DATE-DISPLAY   = 9999999999999999                         
023001                                  - IN-DAINLEV                            
023101     MOVE WS-DATE-DISPLAY(1:8)     TO UT-DAREGDAT                         
023201     PERFORM S02-WRITE-W51280NA                                           
023301     .                                                                    
023401     EJECT                                                                
023501                                                                          
023601 BA-GET-EXCHRATE SECTION.                                                 
023701                                                                          
023801     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
023901                                  - IN-DAINLEV                            
024001     MOVE WS-ACTUAL-DATE          TO WS-ACTUAL-DATE-X                     
024101     MOVE WS-ACTUAL-DATE-X(3:4)   TO W-DATE-AAMM                          
024201                                                                          
024301     IF IN-KDVALISO = WS-SAVE-KDVALISO                                    
024401     AND W-DATE-AAMM = WS-SAVE-MONTH                                      
024501       CONTINUE                                                           
024601     ELSE                                                                 
024701       MOVE IN-KDVALISO           TO WS-SAVE-KDVALISO                     
024801                                                                          
024901       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
025001                                     WS-SAVE-MONTH                        
025101       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
025201       MOVE IN-KDVALISO           TO CURR-KDVALISO-ROW                    
025301       MOVE 'M'                   TO CURR-KDVALTYP                        
025401       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
025501       IF CURR-KDSVAR = ' '                                               
025601         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
025701         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
025801       ELSE                                                               
025901         MOVE 1                   TO W-PRKURS                             
026001         MOVE 1                   TO W-REVALUTA                           
026101       END-IF                                                             
026201                                                                          
026301       PERFORM BAA-GET-LANDING-COST                                       
026401     END-IF                                                               
026501     .                                                                    
026601     EJECT                                                                
026701 BAA-GET-LANDING-COST SECTION.                                            
026801                                                                          
026901     MOVE IN-IDDC                TO W-IDDC                                
027001     PERFORM IMS-GU-WDB601                                                
027101     IF SEGMENT-FINNS                                                     
027201       PERFORM IMS-GNP-WDB617                                             
027301       IF SEGMENT-FINNS                                                   
027501         IF PROC-TILANDCO >  WS-ACTUAL-DATE                               
027601           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
027701         ELSE                                                             
027801           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
027901         END-IF                                                           
028001       END-IF                                                             
028101     END-IF                                                               
028102                                                                          
028103     IF DCS-THAILAND                                                      
028104**** USE DEFAULT VALUE OF 30% AS IMPORT DUTY FOR THAILAND                 
028105       MOVE 0.30               TO WS-MARKUP1                              
028106     ELSE                                                                 
028107       MOVE ZEROS              TO WS-MARKUP1                              
028108     END-IF                                                               
028109                                                                          
028110     IF DCS-TAIWAN                                                        
028111       MOVE IN-IDARTNR         TO W-IDARTNR                               
028120       PERFORM IMS-GU-WDK601                                              
028130       IF SEGMENT-FINNS                                                   
028140         MOVE ART-IDFKNGRP     TO W-IDFKNGRP                              
028150**** FETCH THE LCF PER FUNCTION GROUP IF AVAILABLE                        
028160         PERFORM IMS-GU-WDB622                                            
028170         IF SEGMENT-FINNS                                                 
028180           IF FGAD-TILANDCO >  WS-ACTUAL-DATE                             
028190             MOVE FGAD-RELANDCO-FG-TO   TO WS-MARKUP                      
028200           ELSE                                                           
028201             MOVE FGAD-RELANDCO-FG-FROM TO WS-MARKUP                      
028202           END-IF                                                         
028203         END-IF                                                           
028204       END-IF                                                             
028205     END-IF                                                               
028206     .                                                                    
028301     EJECT                                                                
028400 Z-FINIT SECTION.                                                         
028501     CLOSE W51280B                                                        
028601           W51280NA                                                       
028700     MOVE 'S' TO POSTSUM-OPKOD                                            
028800     CALL POSTSUM USING POSTSUM-PARM                                      
028900     .                                                                    
029000     EJECT                                                                
029100* --- IMS SECTIONS ---                                                    
029200                                                                          
029301 S01-READ-W51280B SECTION.                                                
029401     READ W51280B INTO IN-AREA                                            
029500     AT END                                                               
029601        SET END-OF-W51280B TO TRUE                                        
029700                                                                          
029800     NOT AT END                                                           
029900        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
030001        MOVE 'W51280B'    TO POSTSUM-FDNAMN                               
030101        MOVE 'W5128ND1'   TO POSTSUM-DDNAMN2                              
030200        CALL POSTSUM USING POSTSUM-PARM                                   
030300     END-READ                                                             
030400     .                                                                    
030500                                                                          
030601 S02-WRITE-W51280NA SECTION.                                              
030700     WRITE UT-POST              FROM UT-AREA                              
030800                                                                          
030900     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
031001     MOVE 'W51280NA'             TO POSTSUM-FDNAMN                        
031101     MOVE 'W5128ND2'            TO POSTSUM-DDNAMN2                        
031200     CALL POSTSUM USING POSTSUM-PARM                                      
031300     .                                                                    
031400                                                                          
031501 IMS-GU-WDB601    SECTION.                                                
032001     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
032101          DELIMITED BY SIZE INTO SSA1                                     
032201     MOVE '  GE' TO GODK-STATUSKODER                                      
032301     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
032401     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032501     PERFORM IMS-STATUS-CONTROL                                           
032601     .                                                                    
032701     SKIP3                                                                
032801 IMS-GNP-WDB617 SECTION.                                                  
032901     MOVE 'WDB617   ' TO SSA1                                             
033001     MOVE '  GE'        TO GODK-STATUSKODER                               
033101     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
033201     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
033301     PERFORM IMS-STATUS-CONTROL                                           
033401     .                                                                    
033501     SKIP3                                                                
033601                                                                          
033602 IMS-GU-WDB622 SECTION.                                                   
033603                                                                          
033604     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
033605          DELIMITED BY SIZE INTO SSA1                                     
033606     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
033607          DELIMITED BY SIZE INTO SSA2                                     
033608     STRING 'WDB622  (IDFKNGRP =' W-IDFKNGRP-X ')'                        
033609          DELIMITED BY SIZE INTO SSA3                                     
033610     MOVE '  GE' TO GODK-STATUSKODER                                      
033620     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB622 SSA1 SSA2 SSA3          
033630     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
033640     PERFORM IMS-STATUS-CONTROL                                           
033650     .                                                                    
033660     SKIP3                                                                
033670 IMS-GU-WDK601   SECTION.                                                 
033680     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
033690          DELIMITED BY SIZE INTO SSA1                                     
033700     MOVE '  GE' TO GODK-STATUSKODER                                      
033701     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
033702     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033703     PERFORM IMS-STATUS-CONTROL                                           
033704     .                                                                    
033705     SKIP3                                                                
033706 IMS-STATUS-CONTROL SECTION.                                              
033801     SET STATUS-IX TO 1                                                   
033901     SEARCH GODK-STATUS                                                   
034001       AT END                                                             
034101         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034201           DELIMITED BY SIZE INTO FELTEXT                                 
034301         DISPLAY FELTEXT                                                  
034401         CALL FELLOG                                                      
034501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034601         CONTINUE                                                         
034701     END-SEARCH                                                           
034801     .                                                                    

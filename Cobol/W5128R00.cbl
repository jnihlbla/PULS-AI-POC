000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5128R00.                                                
000301 AUTHOR.         BARSHARANI BISHOYE.                                      
000401 DATE-WRITTEN.   21/05/2020.                                              
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
002201     SELECT W51280BC                   ASSIGN TO W5128RD1.                
002301                                                                          
002401*          --- OUTPUT WITH CONVERTED CURRENCY                             
002501     SELECT W51280R                    ASSIGN TO W5128RD2.                
002601                                                                          
002701     EJECT                                                                
002801                                                                          
002901 DATA DIVISION.                                                           
003001                                                                          
003101 FILE SECTION.                                                            
003201 FD  W51280BC                                                             
003301     RECORDING       F                                                    
003401     BLOCK CONTAINS  0.                                                   
003501 01  IN-POST.                                                             
003601*    03  -COPY W51280    -PRE  IN-  -L.                                   
003701                                                                          
003801 FD  W51280R                                                              
003901     RECORDING       F                                                    
004001     BLOCK CONTAINS  0.                                                   
004101*01  POST   -COPY W51281 -PRE  UT-  -L.                                   
004201                                                                          
004301 WORKING-STORAGE SECTION.                                                 
004401 77  IDPGM                        PIC X(8)    VALUE 'W5128R00'.           
004501 77  JA                           PIC X       VALUE 'J'.                  
004601 77  NEJ                          PIC X       VALUE 'N'.                  
004701 77  FELTEXT                      PIC X(80).                              
004801 77  WS-MARKUP                    PIC 9V9(3)  VALUE ZERO.                 
004802 77  WS-MARKUP1                   PIC 9V9(3)  VALUE ZERO.                 
004901 77  W51280BC-EOF-SW              PIC X       VALUE 'N'.                  
005001     88  END-OF-W51280BC                      VALUE 'J'.                  
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
007602     03  W-IDARTNR-X.                                                     
007603         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
007604     03  W-IDFKNGRP-X.                                                    
007605         05  W-IDFKNGRP           PIC S9(5)   VALUE ZERO COMP-3.          
007606     03  W-KDSEGKEY-X.                                                    
007607         05  W-KDSEGKEY           PIC X(1)    VALUE '1'.                  
007701                                                                          
007801*    --- PARAMETRAR TILL ABEND                                            
007901 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
008001 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
009001 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
009101     EJECT                                                                
009201                                                                          
009301*    --- PARAMETRAR TILL POSTSUM                                          
009401*01  -COPY W0005   -PRE  POSTSUM-                                         
009501     EJECT                                                                
009601*01  -COPY W510CURR                                                       
009701     EJECT                                                                
009800                                                                          
009900 01  IN-AREA-START               PIC X(24)   VALUE                        
010000                                 'IN-AREA-START  '.                       
010100                                                                          
010201*01  AREA -COPY W51280     -PRE IN-                                       
010300     EJECT                                                                
010400                                                                          
010500 01  UT-AREA-START               PIC X(24)   VALUE                        
010600                                 'UT-AREA-START  '.                       
010700                                                                          
010801*01  AREA -COPY W51281     -PRE UT-                                       
010900     EJECT                                                                
011000                                                                          
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
011300                                                                          
011400     EJECT                                                                
011500                                                                          
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                    PIC XX.                                 
011800     88  SEGMENT-FINNS                        VALUE '  '.                 
011900     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
012000                                                                          
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300                                                                          
012400 01  SSA1                         PIC X(64).                              
012500 01  SSA2                         PIC X(64).                              
012501 01  SSA3                         PIC X(64).                              
012502     EJECT                                                                
012601                                                                          
012701*    --- IMS FUNKTIONSKODER                                               
012801*01  -COPY W0003                                                          
012901     EJECT                                                                
013001                                                                          
013101*    ---  DLI INPUT-OUTPUT AREA                                           
013201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
013301 01  DLI-IO-WDB601.                                                       
013401*    03  -COPY WDB601                                                     
013501     EJECT                                                                
013601                                                                          
013701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
013801 01  DLI-IO-WDB617.                                                       
013901*    03  -COPY WDB617                                                     
014001     EJECT                                                                
014006                                                                          
014007 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB622'.                      
014008 01  DLI-IO-WDB622.                                                       
014009*    03  -COPY WDB622                                                     
014010     EJECT                                                                
014020                                                                          
014030 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
014040 01  DLI-IO-WDK601.                                                       
014050*    03  -COPY WDK601                                                     
014060     EJECT                                                                
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
015601     PERFORM S01-READ-W51280BC                                            
015701     PERFORM UNTIL END-OF-W51280BC                                        
015801       PERFORM B-CONVERT-CURRENCY                                         
015901       PERFORM S01-READ-W51280BC                                          
016001     END-PERFORM                                                          
016101                                                                          
016201     PERFORM Z-FINIT                                                      
016301     MOVE ZERO TO RETURN-CODE                                             
016401     GOBACK                                                               
016501     .                                                                    
016601     EJECT                                                                
016701                                                                          
016801 A-INIT SECTION.                                                          
016901     OPEN INPUT  W51280BC                                                 
017001     OPEN OUTPUT W51280R                                                  
017201     .                                                                    
017301     EJECT                                                                
017401                                                                          
017501 B-CONVERT-CURRENCY SECTION.                                              
017601                                                                          
017701     PERFORM BA-GET-EXCHRATE                                              
017801                                                                          
018702     COMPUTE WS-PRARTNTO   ROUNDED =                                      
018703             (IN-PRARTNTO * IN-KVAVIS) +                                  
018704             (IN-PRARTNTO * IN-KVAVIS * WS-MARKUP) +                      
018705             (IN-PRARTNTO * IN-KVAVIS * WS-MARKUP1)                       
018706                                                                          
018707     COMPUTE WS-PRARTNTO-TOT  ROUNDED =                                   
018708           ((IN-PRARTNTO * IN-KVAVIS) / (W-PRKURS / W-REVALUTA))          
018709         + ((IN-PRARTNTO * IN-KVAVIS * WS-MARKUP) /                       
018710             (W-PRKURS / W-REVALUTA))                                     
018720         + ((IN-PRARTNTO * IN-KVAVIS * WS-MARKUP1) /                      
018730             (W-PRKURS / W-REVALUTA))                                     
018740                                                                          
018801     MOVE IN-IDPTYP               TO UT-IDPTYP                            
018901     MOVE IN-IDARTNR              TO UT-IDARTNR                           
019001     MOVE IN-IDDC                 TO UT-IDDC                              
020001     MOVE IN-KVAVIS               TO UT-KVAVIS                            
020101     MOVE IN-PRARTNTO             TO UT-PRARTNTO                          
020201     MOVE WS-PRARTNTO             TO UT-PRARTNTO-SEK                      
020301     MOVE W-PRKURS                TO UT-PRKURS                            
020401     MOVE WS-PRARTNTO-TOT         TO UT-SUNTO-TOT                         
020501     MOVE IN-KDVALISO             TO UT-KDVALISO                          
020601     MOVE IN-KDTRADP              TO UT-KDTRADP                           
020701     MOVE IN-IDLEVNR              TO UT-IDLEVNR                           
020801     MOVE IN-IDFAKT               TO UT-IDFAKT                            
020901     MOVE IN-IDLOPNRM             TO UT-IDLOPNRM                          
021001     COMPUTE WS-DATE-DISPLAY   = 9999999999999999                         
022001                                  - IN-DAINLEV                            
022101     MOVE WS-DATE-DISPLAY(1:8)     TO UT-DAREGDAT                         
022201     PERFORM S02-WRITE-W51280R                                            
022301     .                                                                    
022401     EJECT                                                                
022501                                                                          
022601 BA-GET-EXCHRATE SECTION.                                                 
022701                                                                          
022801     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
022901                                  - IN-DAINLEV                            
023001     MOVE WS-ACTUAL-DATE             TO WS-ACTUAL-DATE-X                  
023101     MOVE WS-ACTUAL-DATE-X(3:4)      TO W-DATE-AAMM                       
023201                                                                          
023301     IF IN-KDVALISO = WS-SAVE-KDVALISO                                    
023401     AND W-DATE-AAMM = WS-SAVE-MONTH                                      
023501       CONTINUE                                                           
023601     ELSE                                                                 
023701       MOVE IN-KDVALISO           TO WS-SAVE-KDVALISO                     
023801       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
023901                                     WS-SAVE-MONTH                        
024001       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
024101       MOVE IN-KDVALISO           TO CURR-KDVALISO-ROW                    
024201       MOVE 'M'                   TO CURR-KDVALTYP                        
024301       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
024401       IF CURR-KDSVAR = ' '                                               
024501         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
024601         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
024701       ELSE                                                               
024801         MOVE 1                   TO W-PRKURS                             
024901         MOVE 1                   TO W-REVALUTA                           
025001       END-IF                                                             
025101                                                                          
025201                                                                          
025301       PERFORM BAA-GET-LANDING-COST                                       
025401     END-IF                                                               
025501     .                                                                    
025601     EJECT                                                                
025701 BAA-GET-LANDING-COST SECTION.                                            
025801                                                                          
025901     MOVE IN-IDDC                TO W-IDDC                                
026001     PERFORM IMS-GU-WDB601                                                
026101     IF SEGMENT-FINNS                                                     
026201       PERFORM IMS-GNP-WDB617                                             
026301       IF SEGMENT-FINNS                                                   
026501         IF PROC-TILANDCO >  WS-ACTUAL-DATE                               
026601           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
026701         ELSE                                                             
026801           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
026901         END-IF                                                           
027001       END-IF                                                             
027101     END-IF                                                               
027102                                                                          
027103     IF DCS-THAILAND                                                      
027104**** USE DEFAULT VALUE OF 30% AS IMPORT DUTY FOR THAILAND                 
027105       MOVE 0.30               TO WS-MARKUP1                              
027106     ELSE                                                                 
027107       MOVE ZEROS              TO WS-MARKUP1                              
027108     END-IF                                                               
027109                                                                          
027110     IF DCS-TAIWAN                                                        
027111       MOVE IN-IDARTNR         TO W-IDARTNR                               
027120       PERFORM IMS-GU-WDK601                                              
027130       IF SEGMENT-FINNS                                                   
027140         MOVE ART-IDFKNGRP     TO W-IDFKNGRP                              
027150**** FETCH THE LCF PER FUNCTION GROUP IF AVAILABLE                        
027160         PERFORM IMS-GU-WDB622                                            
027170         IF SEGMENT-FINNS                                                 
027180           IF FGAD-TILANDCO >  WS-ACTUAL-DATE                             
027190             MOVE FGAD-RELANDCO-FG-TO   TO WS-MARKUP                      
027200           ELSE                                                           
027201             MOVE FGAD-RELANDCO-FG-FROM TO WS-MARKUP                      
027202           END-IF                                                         
027203         END-IF                                                           
027204       END-IF                                                             
027205     END-IF                                                               
027206     .                                                                    
027301     EJECT                                                                
027401 Z-FINIT SECTION.                                                         
027501     CLOSE W51280BC                                                       
027601           W51280R                                                        
027701     MOVE 'S' TO POSTSUM-OPKOD                                            
027801     CALL POSTSUM USING POSTSUM-PARM                                      
027901     .                                                                    
028001     EJECT                                                                
028101* --- IMS SECTIONS ---                                                    
028201                                                                          
028301 S01-READ-W51280BC SECTION.                                               
028401     READ W51280BC INTO IN-AREA                                           
028501     AT END                                                               
028601        SET END-OF-W51280BC TO TRUE                                       
028701                                                                          
028801     NOT AT END                                                           
028901        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
029001        MOVE 'W51280BC'   TO POSTSUM-FDNAMN                               
029101        MOVE 'W5128RD1'   TO POSTSUM-DDNAMN2                              
029201        CALL POSTSUM USING POSTSUM-PARM                                   
029301     END-READ                                                             
029401     .                                                                    
029501                                                                          
029601 S02-WRITE-W51280R SECTION.                                               
029701     WRITE UT-POST              FROM UT-AREA                              
029801                                                                          
029901     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
030001     MOVE 'W51280R'             TO POSTSUM-FDNAMN                         
030101     MOVE 'W5128RD2'            TO POSTSUM-DDNAMN2                        
030201     CALL POSTSUM USING POSTSUM-PARM                                      
030301     .                                                                    
030401                                                                          
030501 IMS-GU-WDB601    SECTION.                                                
030601     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
030701          DELIMITED BY SIZE INTO SSA1                                     
030801     MOVE '  GE' TO GODK-STATUSKODER                                      
030901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
031001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032001     PERFORM IMS-STATUS-CONTROL                                           
032101     .                                                                    
032201     SKIP3                                                                
032301 IMS-GNP-WDB617 SECTION.                                                  
032401     MOVE 'WDB617   ' TO SSA1                                             
032501     MOVE '  GE'        TO GODK-STATUSKODER                               
032601     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
032701     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
032801     PERFORM IMS-STATUS-CONTROL                                           
032901     .                                                                    
033001     SKIP3                                                                
033101                                                                          
033102 IMS-GU-WDB622 SECTION.                                                   
033103                                                                          
033104     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
033105          DELIMITED BY SIZE INTO SSA1                                     
033106     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
033107          DELIMITED BY SIZE INTO SSA2                                     
033108     STRING 'WDB622  (IDFKNGRP =' W-IDFKNGRP-X ')'                        
033109          DELIMITED BY SIZE INTO SSA3                                     
033110     MOVE '  GE' TO GODK-STATUSKODER                                      
033120     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB622 SSA1 SSA2 SSA3          
033130     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
033140     PERFORM IMS-STATUS-CONTROL                                           
033150     .                                                                    
033160     SKIP3                                                                
033170 IMS-GU-WDK601   SECTION.                                                 
033180     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
033190          DELIMITED BY SIZE INTO SSA1                                     
033200     MOVE '  GE' TO GODK-STATUSKODER                                      
033201     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
033202     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033203     PERFORM IMS-STATUS-CONTROL                                           
033204     .                                                                    
033205     SKIP3                                                                
033206 IMS-STATUS-CONTROL SECTION.                                              
033301     SET STATUS-IX TO 1                                                   
033401     SEARCH GODK-STATUS                                                   
033501       AT END                                                             
033601         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033701           DELIMITED BY SIZE INTO FELTEXT                                 
033801         DISPLAY FELTEXT                                                  
033901         CALL FELLOG                                                      
034001       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034101         CONTINUE                                                         
034201     END-SEARCH                                                           
034301     .                                                                    

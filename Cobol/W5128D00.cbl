000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5128D00.                                                
000301 AUTHOR.         ARCHANA BHAT.                                            
000401 DATE-WRITTEN.   DEC 2019.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*                                                                         
000901*       -PROGRAM READS         WDG2                                       
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
002201     SELECT W51280B                    ASSIGN TO W5128DD1.                
002301                                                                          
002401*          --- OUTPUT WITH CONVERTED CURRENCY                             
002501     SELECT W51280F                    ASSIGN TO W5128DD2.                
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
003801 FD  W51280F                                                              
003901     RECORDING       F                                                    
004001     BLOCK CONTAINS  0.                                                   
004101*01  POST   -COPY W51281 -PRE  UT-  -L.                                   
004201                                                                          
004301 WORKING-STORAGE SECTION.                                                 
004401 77  IDPGM                        PIC X(8)    VALUE 'W5128D00'.           
004501 77  JA                           PIC X       VALUE 'J'.                  
004601 77  NEJ                          PIC X       VALUE 'N'.                  
004701 77  FELTEXT                      PIC X(80).                              
004801 77  W51280B-EOF-SW               PIC X       VALUE 'N'.                  
004901     88  END-OF-W51280B                       VALUE 'J'.                  
005001 77  WS-PRARTNTO-TOT              PIC S9(11)V9(2) VALUE +0                
005101                                                   COMP-3.                
005201 77  WS-PRARTNTO                  PIC S9(11)V9(2) VALUE +0                
005301                                                   COMP-3.                
005401 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
005501                                                   COMP-3.                
005601 77  W-REVALUTA                   PIC S9(5) COMP-3 VALUE ZERO.            
005701 77  WS-KDVALISO                  PIC X(3)         VALUE SPACE.           
005801 77  WS-PREV-KDTRADP              PIC X(4)         VALUE SPACE.           
005901 77  WS-SAVE-KDTRADP              PIC X(4)         VALUE SPACE.           
006001 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
006100 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
006200 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
006301 77  WS-DATE-DISPLAY              PIC 9(16)   VALUE ZERO.                 
006302 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
006303 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
006800                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
007100     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
007200     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
007300     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
007310     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
007400                                                                          
007500 01  NYCKLAR-TILL-DLI.                                                    
008501     03  W-IDDC-X.                                                        
008601         05  W-IDDC               PIC X(2)    VALUE SPACE.                
008701                                                                          
008801*    --- PARAMETRAR TILL ABEND                                            
008901 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
009001 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
009101 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
009201     EJECT                                                                
009301                                                                          
009401*    --- PARAMETRAR TILL POSTSUM                                          
009501*01  -COPY W0005   -PRE  POSTSUM-                                         
009601     EJECT                                                                
009602*01  -COPY W510CURR                                                       
009603     EJECT                                                                
009701                                                                          
009801 01  IN-AREA-START               PIC X(24)   VALUE                        
009901                                 'IN-AREA-START  '.                       
010001                                                                          
010101*01  AREA -COPY W51280     -PRE IN-                                       
010201     EJECT                                                                
010301                                                                          
010401 01  UT-AREA-START               PIC X(24)   VALUE                        
010501                                 'UT-AREA-START  '.                       
010601                                                                          
010701*01  AREA -COPY W51281     -PRE UT-                                       
010801     EJECT                                                                
010901                                                                          
011001*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011101 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
011201                                                                          
011301     EJECT                                                                
011401                                                                          
011501*    --- STATUS-KOD FRÅN IMS                                              
011601 01  STATUS-WS                    PIC XX.                                 
011701     88  SEGMENT-FINNS                        VALUE '  '.                 
011801     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
011901                                                                          
012001 01  GODK-STATUSKODER.                                                    
012101     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012201                                                                          
012301 01  SSA1                         PIC X(64).                              
012601     EJECT                                                                
012701                                                                          
012801*    --- IMS FUNKTIONSKODER                                               
012901*01  -COPY W0003                                                          
013001     EJECT                                                                
013101                                                                          
013201*    ---  DLI INPUT-OUTPUT AREA                                           
013701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
013801 01  DLI-IO-WDB601.                                                       
013901*    03  -COPY WDB601                                                     
014001     EJECT                                                                
014101 LINKAGE SECTION.                                                         
014201*01  -COPY W0008  -PRE 9305-                                              
014301     05  FILLER                  PIC X.                                   
014401*01  -COPY W0008  -PRE WDB6-                                              
014501     05  FILLER                  PIC X.                                   
014601                                                                          
014701     EJECT                                                                
014801                                                                          
014901 PROCEDURE DIVISION  USING 9305-PCB WDB6-PCB.                             
015001                                                                          
015101 MAIN SECTION.                                                            
015201     ENTRY 'DLITCBL' USING 9305-PCB WDB6-PCB.                             
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
017001     OPEN OUTPUT W51280F                                                  
017002                                                                          
017003     MOVE  ZERO  TO UT-IDLOPNRM                                           
017101     .                                                                    
017201     EJECT                                                                
017301                                                                          
017401 B-CONVERT-CURRENCY SECTION.                                              
017501                                                                          
017601     PERFORM BA-GET-EXCHRATE                                              
017701                                                                          
017801     COMPUTE WS-PRARTNTO   ROUNDED =                                      
017901             (IN-PRARTNTO * IN-KVAVIS)                                    
018001                                                                          
018101     COMPUTE WS-PRARTNTO-TOT  ROUNDED =                                   
019001            WS-PRARTNTO  / (W-PRKURS / W-REVALUTA)                        
020001                                                                          
020101     MOVE IN-IDPTYP               TO UT-IDPTYP                            
020201     MOVE IN-IDARTNR              TO UT-IDARTNR                           
020301     MOVE IN-IDDC                 TO UT-IDDC                              
020401     MOVE IN-KVAVIS               TO UT-KVAVIS                            
020501     MOVE IN-PRARTNTO             TO UT-PRARTNTO                          
020601     MOVE WS-PRARTNTO             TO UT-PRARTNTO-SEK                      
020701     MOVE W-PRKURS                TO UT-PRKURS                            
020801     MOVE WS-PRARTNTO-TOT         TO UT-SUNTO-TOT                         
020901     MOVE IN-KDVALISO             TO UT-KDVALISO                          
021001     MOVE IN-KDTRADP              TO UT-KDTRADP                           
021002     MOVE IN-IDLEVNR              TO UT-IDLEVNR                           
021003     MOVE IN-IDFAKT               TO UT-IDFAKT                            
022001     COMPUTE WS-DATE-DISPLAY   = 9999999999999999                         
022101                                  - IN-DAINLEV                            
022201     MOVE WS-DATE-DISPLAY(1:8)    TO UT-DAREGDAT                          
022301     PERFORM S02-WRITE-W51280F                                            
022401     .                                                                    
022501     EJECT                                                                
022601                                                                          
022701 BA-GET-EXCHRATE SECTION.                                                 
022801                                                                          
022901     IF IN-KDVALISO = SPACES                                              
023001       IF IN-KDTRADP NOT = WS-SAVE-KDTRADP                                
023101         MOVE IN-IDDC           TO W-IDDC                                 
023201         PERFORM IMS-GU-WDB601                                            
023301         IF SEGMENT-FINNS                                                 
023401            MOVE DCS-KDVALISO   TO WS-KDVALISO                            
023501         END-IF                                                           
023601       END-IF                                                             
023701     ELSE                                                                 
023801       MOVE IN-KDVALISO         TO WS-KDVALISO                            
023901     END-IF                                                               
024001                                                                          
024101     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
024201                                  - IN-DAINLEV                            
024301     MOVE WS-ACTUAL-DATE           TO WS-ACTUAL-DATE-X                    
024401     MOVE WS-ACTUAL-DATE-X(3:4)    TO W-DATE-AAMM                         
024501                                                                          
024600     IF IN-KDTRADP = WS-SAVE-KDTRADP                                      
024701     AND W-DATE-AAMM = WS-SAVE-MONTH                                      
024800       CONTINUE                                                           
024900     ELSE                                                                 
025000       MOVE IN-KDTRADP            TO WS-SAVE-KDTRADP                      
025202       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
025203                                     WS-SAVE-MONTH                        
025204       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
025205       MOVE WS-KDVALISO           TO CURR-KDVALISO-ROW                    
025206       MOVE 'M'                   TO CURR-KDVALTYP                        
025207       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
025208       IF CURR-KDSVAR = ' '                                               
025209         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
025210         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
025220       ELSE                                                               
025230         MOVE 1                   TO W-PRKURS                             
025240         MOVE 1                   TO W-REVALUTA                           
025250       END-IF                                                             
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026801 Z-FINIT SECTION.                                                         
026901     CLOSE W51280B                                                        
027001           W51280F                                                        
027101     MOVE 'S' TO POSTSUM-OPKOD                                            
027201     CALL POSTSUM USING POSTSUM-PARM                                      
027301     .                                                                    
027401     EJECT                                                                
027501* --- IMS SECTIONS ---                                                    
027601                                                                          
027701 S01-READ-W51280B SECTION.                                                
027801     READ W51280B INTO IN-AREA                                            
027901     AT END                                                               
028001        SET END-OF-W51280B TO TRUE                                        
028101                                                                          
028201     NOT AT END                                                           
028301        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
028401        MOVE 'W51280B'    TO POSTSUM-FDNAMN                               
028501        MOVE 'W5128DD1'   TO POSTSUM-DDNAMN2                              
028601        CALL POSTSUM USING POSTSUM-PARM                                   
028701     END-READ                                                             
028801     .                                                                    
028901                                                                          
029001 S02-WRITE-W51280F SECTION.                                               
029101     WRITE UT-POST              FROM UT-AREA                              
029201                                                                          
029301     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
029401     MOVE 'W51280F'             TO POSTSUM-FDNAMN                         
029501     MOVE 'W5128DD2'            TO POSTSUM-DDNAMN2                        
029601     CALL POSTSUM USING POSTSUM-PARM                                      
029701     .                                                                    
029801                                                                          
031301 IMS-GU-WDB601    SECTION.                                                
031401     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
031501          DELIMITED BY SIZE INTO SSA1                                     
031601     MOVE '  GE' TO GODK-STATUSKODER                                      
031701     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
031801     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031901     PERFORM IMS-STATUS-CONTROL                                           
032001     .                                                                    
033001     SKIP3                                                                
033101 IMS-STATUS-CONTROL SECTION.                                              
033201     SET STATUS-IX TO 1                                                   
033301     SEARCH GODK-STATUS                                                   
033401       AT END                                                             
033501         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033601           DELIMITED BY SIZE INTO FELTEXT                                 
033701         DISPLAY FELTEXT                                                  
033801         CALL FELLOG                                                      
033901       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034001         CONTINUE                                                         
034101     END-SEARCH                                                           
034201     .                                                                    

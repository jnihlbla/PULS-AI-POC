001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF107100.                                                
001400 AUTHOR.         ANDERS HENRIKSSON.                                       
001500 DATE-WRITTEN.   JUNE    2003.                                            
001600 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THE PGM                                                          
002110*        - READS FILE WITH NEW/CHANGED/DELETED VAT RECORDS                
002140*        - SENDS THE FOLLOWING VAT DATA TO VSS:                           
002170*            BY USING WZ01SEND (CARPARTS.VSS.RECVAT)                      
002510*                                                                         
002600                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003380*          --- VAT-RECORDS                                                
003390     SELECT WF1017                     ASSIGN TO WF1071D1.                
003391                                                                          
003400*          --- DISTRIBUTION-DATA                                          
003410     SELECT WF1071                     ASSIGN TO WF1071D2.                
003420                                                                          
003500     EJECT                                                                
003510                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
004097 FD  WF1017                                                               
004098     RECORDING       F                                                    
004099     BLOCK CONTAINS  0.                                                   
004100                                                                          
004110*01  -COPY WF10M17     -L.                                                
004111                                                                          
004112 FD  WF1071                                                               
004113     RECORDING       V                                                    
004114     BLOCK CONTAINS  0.                                                   
004115                                                                          
004116 01  WF1071-POST.                                                         
004117*    03  -COPY WF10VSS   -PRE UT-   -L.                                   
004118*    03  -COPY WF10M17   -PRE UT-   -L.                                   
004119     EJECT                                                                
004120                                                                          
004121     EJECT                                                                
004122                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'WF107100'.            
004581 77  WF1017-EOF-SW               PIC X       VALUE 'N'.                   
004582     88  END-OF-WF1017                       VALUE 'J'.                   
004583 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
004584     88  FIRST-TIME                          VALUE 'J'.                   
004585 77  JA                          PIC X       VALUE 'J'.                   
004586 77  NEJ                         PIC X       VALUE 'N'.                   
004590     EJECT                                                                
004600                                                                          
004700 01  WS-ATAB-COUNTRIES.                                                   
004900     03  WS-VSS                  PIC X(50)   VALUE                        
005000                                 'CARPARTS.VSS.RECVAT'.                   
005010                                                                          
005110 01  WS-IDLOPNR.                                                          
005120     03  WS-KVDAGAR              PIC 9(3)    VALUE ZERO.                  
005130     03  WS-TIME                 PIC 9(8)    VALUE ZERO.                  
005200 01  WS-TIMESTAMP.                                                        
005300     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
005310     03  FILLER                  PIC X       VALUE '-'.                   
005320     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
005330     03  FILLER                  PIC X       VALUE '-'.                   
005340     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
005350     03  FILLER                  PIC X       VALUE SPACE.                 
005360     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
005370     03  FILLER                  PIC X       VALUE ':'.                   
005380     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
005390     03  FILLER                  PIC X       VALUE ':'.                   
005391     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
005392     03  FILLER                  PIC X       VALUE '.'.                   
005393     03  WS-DECIMAL              PIC X(2)    VALUE SPACE.                 
005394     03  FILLER                  PIC X(4)    VALUE '0000'.                
005395 01  WS-BILLIT                   PIC X(6)    VALUE 'BILVAT'.              
005396                                                                          
005397 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
005398 01  WS-FIRST-DAY-OF-YEAR.                                                
005399     03  WS-NEW-YEAR             PIC X(4)    VALUE SPACE.                 
005400     03  FILLER                  PIC X(4)    VALUE '0101'.                
005401     EJECT                                                                
005402                                                                          
005403 01  ERRTEXT.                                                             
005404     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005405     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005406 01  KDRC-DISPLAY                PIC Z(5).                                
005407     EJECT                                                                
005410                                                                          
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006010                                                                          
006100 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006401     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006402     EJECT                                                                
006410                                                                          
006420*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006430                                                                          
006440 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006450 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006460 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006601     EJECT                                                                
006603                                                                          
006627*    --- AREOR FÖR KOMMUNIKATION                                          
006628 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
006629*01  -COPY WZ01SEND                                                       
006630     EJECT                                                                
009070                                                                          
009071 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
009072     SKIP3                                                                
009073*    -COPY WZ20DAYS                                                       
009074     EJECT                                                                
009075                                                                          
009082 01  UT17-AREA-START             PIC X(24)   VALUE                        
009090                                             'UT17-AREA-START'.           
009091 01  UT17-AREA.                                                           
009092*    03  -COPY WF10VSS        -PRE UT17-                                  
009100*    03  -COPY WF10M17        -PRE UT17-                                  
009200*                                                                         
009300     EJECT                                                                
009400                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB.                                       
010802                                                                          
010803 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010900                                                                          
011200     PERFORM A-INIT                                                       
011201                                                                          
011210     PERFORM B-EXECUTE                                                    
011212                                                                          
012510     PERFORM Z-FINIT                                                      
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013010                                                                          
013100 A-INIT SECTION.                                                          
013200     OPEN INPUT WF1017                                                    
013201     OPEN OUTPUT WF1071                                                   
013210                                                                          
013220     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
013230     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-NEW-YEAR                     
013240     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
013250     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
013260     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
013270     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
013280     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
013290     MOVE FUNCTION CURRENT-DATE (15:2) TO WS-DECIMAL                      
013291     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
013292                                                                          
013293     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
013294     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
013295     MOVE ZERO                        TO DAYS-KVDAYS                      
013296     MOVE SPACE                       TO DAYS-IDCALEND                    
013297     MOVE WS-FIRST-DAY-OF-YEAR        TO DAYS-TIDATE1                     
013298     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
013299     CALL WZ20DAYS USING                                                  
013300          DAYS-WZ20DAYS                                                   
013310     IF DAYS-KDRC = ZERO                                                  
013320       MOVE DAYS-KVDAYS               TO WS-KVDAGAR                       
013321*** JUSTERING FÖR ATT FÅ DAGENS NUMMER + 500                              
013322       COMPUTE WS-KVDAGAR = WS-KVDAGAR + 500                              
013330     END-IF                                                               
013340                                                                          
013350     MOVE WS-IDLOPNR   TO UT17-IDLOPNR                                    
013361     MOVE WS-TIMESTAMP TO UT17-TIMESTAMP                                  
013371     MOVE WS-BILLIT    TO UT17-BILLIT-TEXT                                
014200     .                                                                    
014350     EJECT                                                                
014360                                                                          
014370 B-EXECUTE SECTION.                                                       
014391     PERFORM S01-READ-WF1017                                              
014392                                                                          
014394     PERFORM UNTIL END-OF-WF1017                                          
014395       IF  UT17-IDLEGSEL = 'VCCS' AND                                     
014396          (UT17-IDLAND = 'EU'  OR                                         
014397           UT17-IDLAND = 'SE')                                            
014398         IF FIRST-TIME                                                    
014400           MOVE NEJ TO FIRST-TIME-SW                                      
014401           PERFORM S11-VAT-OPEN                                           
014402         END-IF                                                           
014410         PERFORM S12-VAT-PUT                                              
014411         PERFORM S02-WRITE-WF1071                                         
014412         PERFORM S01-READ-WF1017                                          
014413       ELSE                                                               
014414         PERFORM S01-READ-WF1017                                          
014415       END-IF                                                             
014420     END-PERFORM                                                          
014421     IF NOT FIRST-TIME                                                    
014423       PERFORM S13-VAT-CLOSE                                              
014424     END-IF                                                               
014425     .                                                                    
014426     EJECT                                                                
014427                                                                          
014500 Z-FINIT SECTION.                                                         
014700     CLOSE WF1017                                                         
014800     CLOSE WF1071                                                         
015000     .                                                                    
015101     EJECT                                                                
015102                                                                          
015438 S01-READ-WF1017  SECTION.                                                
015439     READ WF1017          INTO UT17-WF10M17                               
015440     AT END                                                               
015441        MOVE HIGH-VALUE   TO   UT17-WF10M17                               
015442        SET END-OF-WF1017 TO   TRUE                                       
015443     END-READ                                                             
015444     .                                                                    
015445                                                                          
015446 S02-WRITE-WF1071 SECTION.                                                
015449     WRITE WF1071-POST FROM UT17-AREA                                     
015451     .                                                                    
015452     EJECT                                                                
015453                                                                          
015455 S11-VAT-OPEN SECTION.                                                    
015461     MOVE WS-VSS                    TO SEND-ADDISPABS                     
015464     MOVE 'OPEN'                    TO SEND-KDFUNC                        
015465     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015466                         SEND-OPEN-AREA                                   
015467     IF SEND-KDRC > ZERO                                                  
015468       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015469       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015470       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015471       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015472     END-IF                                                               
015473     .                                                                    
015480                                                                          
015514 S12-VAT-PUT SECTION.                                                     
015515     MOVE 'PUT'                           TO SEND-KDFUNC                  
015516     MOVE LENGTH OF UT17-AREA             TO SEND-KVDLEN                  
015517     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015518                         SEND-KVDLEN                                      
015519                         UT17-AREA                                        
015520     IF SEND-KDRC > 1                                                     
015521       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015522       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015523       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015524       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015525     END-IF                                                               
015526     .                                                                    
015528                                                                          
015529 S13-VAT-CLOSE SECTION.                                                   
015530     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015531     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015532     .                                                                    

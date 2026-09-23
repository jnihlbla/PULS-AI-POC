000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6113700.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   FEB 2002.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAM FÖR ATT LÄGGA UPP WDM521 POSTER                          
002400*        FLYTTA GODS FRÅN SVS TILL CDC                                    
002500*                                                                         
002600*        PROGRAMMET UPPDATERAR WDM5                                       
002700*                                                                         
002800*    ABENDKODER:                                                          
002900*        U0016 -  . . . .                                                 
003000*        U1000 -  . . . .                                                 
003100*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*          ---                                                            
004100     SELECT W611IN                     ASSIGN TO W61137D1.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
005106     SKIP3                                                                
005113                                                                          
005180 FD  W611IN                                                               
005181     RECORDING       F                                                    
005182     BLOCK CONTAINS  0.                                                   
005190                                                                          
005191*01  POST -COPY W61136  -PRE IN-    -L.                                   
005192                                                                          
005200                                                                          
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005501                                                                          
005502*    -- CHECKED BY WY2000                                                 
005510 01  CHKP-VAR.                                                            
005520   03 CHKP-MSG-IO-AREA-LENGTH    PIC S9(9)   VALUE +32 COMP SYNC.         
005530   03 CHKP-MSG-IO-AREA           PIC X(32)   VALUE SPACE.                 
005540   03 CHKP-AREA-LENGTH           PIC S9(9)   VALUE +32 COMP SYNC.         
005550   03 CHKP-AREA                  PIC X(32)   VALUE SPACE.                 
005560   03 CHKP-ANT                   PIC S9(3)   VALUE +0.                    
005570   03 CHKP-MAX                   PIC S9(3)   VALUE +500.                  
005600 77  IDPGM                       PIC X(8)    VALUE 'W6113700'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005810                                                                          
005926                                                                          
006000 77  W611IN-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W611IN                       VALUE 'J'.                   
006200                                                                          
006700                                                                          
006800     EJECT                                                                
006900 01  ARBETSAREOR.                                                         
007000                                                                          
007100     03 WS-DAGENS-DATUM          PIC 9(8)   VALUE ZERO.                   
007200     03 WS-DADATTID              PIC 9(14)   VALUE ZERO.                  
007300     03 WS-ANTAL-POSTER          PIC 9(14)   VALUE ZERO.                  
009300                                                                          
015000                                                                          
015100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015200 01  FILLER REDEFINES DAGENS-DATUM.                                       
015300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015600                                                                          
015700 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
015800                                                                          
015900     EJECT                                                                
016000                                                                          
016100******************************************************************        
016200*      TABELLER                                                           
016300******************************************************************        
016400                                                                          
016500                                                                          
016600 01  TABENTRY-PARM.                                                       
016700                                                                          
016800     03  STEGLANGD                 PIC S9(9) COMP.                        
016900     03  POST-ANTAL                PIC S9(9) COMP.                        
017000     03  NYCKELLANGD               PIC S9(9) COMP.                        
017100                                                                          
017200 01  TAB-MAX                     PIC S9(9) COMP  VALUE ZERO.              
017300                                                                          
017400 01  RETURANTALTABELL.                                                    
017500     03  RETURANTAL OCCURS 5.                                             
017600        05  TAB-IDDC                  PIC  X(2).                          
017700        05  TAB-ANTAL                 PIC S9(7)    COMP-3.                
017800                                                                          
017900 01  DYNAMISKA-SUBPROGRAM.                                                
018000*                                                                         
018100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
018200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
018500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
018600     03  WARBKONV                PIC X(8)    VALUE 'WARBKONV'.            
018700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018800     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
018900     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
019000     SKIP2                                                                
019100*    --- PARAMETRAR TILL ABEND                                            
019200                                                                          
019300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019401     EJECT                                                                
019410                                                                          
019420*01  AREA -COPY W61136     -PRE IN-                                       
019430     EJECT                                                                
019500     SKIP2                                                                
019600 01  FELTEXT.                                                             
019700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019820     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
019830     03  FILLER                  PIC X       VALUE SPACE.                 
019840     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
019900     EJECT                                                                
020000*    --- PARAMETRAR TILL DATKORT                                          
020100*                                                                         
020200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'XXXXX'.               
020300     SKIP2                                                                
020400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
020500     SKIP2                                                                
020600*01  -COPY WDATKORT                                                       
020700     EJECT                                                                
020800*    --- PARAMETRAR TILL POSTSUM                                          
020900*                                                                         
021000*01  -COPY W0005   -PRE  POSTSUM-                                         
021100     EJECT                                                                
021400*01  -COPY WDATAREA                                                       
021500     EJECT                                                                
021600*******                      PARAMETRAR TILL WDAGKONV                     
021700*                                                                         
021800*01  -COPY WDAGAREA                                                       
025500     EJECT                                                                
025600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025700*                                                                         
025800     EJECT                                                                
025900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026000     SKIP3                                                                
026100 01  NYCKLAR-TILL-DLI.                                                    
026200     03  W-IDARTNR-X.                                                     
026300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026310                                                                          
026320     03  W-ADTRDEST-X.                                                    
026330         05  W-ADTRDEST          PIC X(3)    VALUE SPACE.                 
026340                                                                          
026350     03  W-IDTRPTNR-X.                                                    
026360         05  W-IDTRPTNR          PIC S9(5)   VALUE ZERO COMP-3.           
026370                                                                          
027200     SKIP2                                                                
027300*    --- STATUS-KOD FRÅN IMS                                              
027400 01  STATUS-WS                   PIC XX.                                  
027500     88  SEGMENT-FINNS                       VALUE '  '.                  
027600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027700     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
027800                                                   'GB'.                  
027810     88  IMS-EJ-OK                           VALUE 'XD'.                  
027900 01  STATUS-TEST                 PIC X       VALUE 'N'.                   
028000     SKIP2                                                                
028100 01  GODK-STATUSKODER.                                                    
028200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028300     SKIP3                                                                
028400 01  SSA1                        PIC X(64).                               
028500 01  SSA2                        PIC X(64).                               
028510 01  SSA3                        PIC X(64).                               
028600     EJECT                                                                
028700*    --- IMS FUNKTIONSKODER                                               
028800*01  -COPY W0003                                                          
028900     EJECT                                                                
029000******************************************************************        
029100*          DLI INPUT - OUTPUT AREA                                        
029200******************************************************************        
029300                                                                          
029400                                                                          
029500                                                                          
029600     EJECT                                                                
029700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDM521'.         
029800     SKIP3                                                                
029900 01  DLI-IO-WDM521.                                                       
030000*    03  -COPY WDM521                                                     
030500     EJECT                                                                
030800 LINKAGE SECTION.                                                         
030810*01  -COPY W0009   -PRE MSG-                                              
030900                                                                          
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE WDM5-                                              
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400 PROCEDURE DIVISION  USING MSG-PCB WDM5-PCB.                              
031500     ENTRY 'DLITCBL' USING MSG-PCB WDM5-PCB.                              
031600                                                                          
031700                                                                          
031800     PERFORM A-INIT                                                       
031900                                                                          
032001     PERFORM S01-LAES-W611IN                                              
033900                                                                          
034000     PERFORM UNTIL END-OF-W611IN                                          
034100                                                                          
034110       IF CHKP-ANT > CHKP-MAX                                             
034120         PERFORM X-TAG-CHECKPOINT                                         
034130       END-IF                                                             
034200       PERFORM B-BEARBETA                                                 
034210       PERFORM S01-LAES-W611IN                                            
035200     END-PERFORM                                                          
035300     PERFORM Z-FINIT                                                      
035400                                                                          
035500     MOVE ZERO TO RETURN-CODE                                             
035600     GOBACK                                                               
035700     .                                                                    
035800     EJECT                                                                
035900                                                                          
036000                                                                          
036100 A-INIT SECTION.                                                          
036110                                                                          
036120     PERFORM IMS-RESTART                                                  
036200                                                                          
036300     OPEN INPUT  W611IN                                                   
036400                                                                          
036500*                                  *** VILKEN SKA ANVÄNDAS ?              
036600*    CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036700*    MOVE D-AAR              TO DAGENS-DATUM-AAR                          
036800*    MOVE D-MAANAD           TO DAGENS-DATUM-MAANAD                       
036900*    MOVE D-DAG              TO DAGENS-DATUM-DAG                          
037000*    MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037100                                                                          
037200     ACCEPT DAGENS-DATUM FROM DATE                                        
037300     MOVE FUNCTION CURRENT-DATE(1:14)                                     
037400                             TO WS-DADATTID                               
039300     .                                                                    
039400     EJECT                                                                
039500 B-BEARBETA SECTION.                                                      
039600                                                                          
039930     MOVE 'CDC'              TO W-ADTRDEST                                
039940     MOVE 99999              TO W-IDTRPTNR                                
039950     MOVE SPACE              TO AVG-WDM521                                
039960     ADD 1                   TO WS-DADATTID                               
039991     MOVE WS-DADATTID        TO AVG-DADATTID                              
040000     MOVE IN-IDARTNR         TO AVG-IDARTNR                               
040001     MOVE IN-KVANTAL         TO AVG-KVANTAL                               
040002     MOVE 'N'                TO AVG-FLEJBOK                               
040020     MOVE WS-DAGENS-DATUM (2:6)                                           
040021                             TO AVG-TIUPPDAT                              
040022     PERFORM IMS-ISRT-M521                                                
040030     .                                                                    
040100     EJECT                                                                
056893                                                                          
056894                                                                          
056895                                                                          
056900 Z-FINIT SECTION.                                                         
057000     CLOSE W611IN                                                         
057100     DISPLAY 'ANTAL POSTER : ' WS-ANTAL-POSTER                            
057200     .                                                                    
057300     EJECT                                                                
057400                                                                          
057500                                                                          
057600 S01-LAES-W611IN SECTION.                                                 
057700     READ W611IN INTO IN-AREA                                             
057800     AT END                                                               
057900        SET END-OF-W611IN TO TRUE                                         
057910     NOT AT END                                                           
057920        ADD 1                TO WS-ANTAL-POSTER                           
058000                                                                          
058100     END-READ                                                             
058200     .                                                                    
058300     EJECT                                                                
058400                                                                          
058500                                                                          
058610 S99-ABEND SECTION.                                                       
058700                                                                          
058800     SKIP2                                                                
058900     MOVE 'S' TO POSTSUM-OPKOD                                            
059000     CALL POSTSUM USING POSTSUM-PARM                                      
059100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
059200     .                                                                    
059300     EJECT                                                                
059400                                                                          
059410                                                                          
059420 X-TAG-CHECKPOINT   SECTION.                                              
059430                                                                          
059440* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
059450* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
059460     PERFORM IMS-CHECKPOINT                                               
059470     MOVE ZERO TO CHKP-ANT                                                
059480* --- LÄS OM DATABAS OM DET BEHÖVS                                        
059490     .                                                                    
059491     EJECT                                                                
059492                                                                          
059500                                                                          
059600* --- IMS SEKTIONER --   &&&                                              
059700     SKIP3                                                                
059800                                                                          
059801 IMS-ISRT-M521 SECTION.                                                   
059802                                                                          
059803     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST-X ')'                        
059804          DELIMITED BY SIZE INTO SSA1                                     
059805     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
059806          DELIMITED BY SIZE INTO SSA2                                     
059807     MOVE 'WDM521 ' TO SSA3                                               
059808     MOVE '  II' TO GODK-STATUSKODER                                      
059809     CALL CBLTDLI USING ISRT WDM5-PCB DLI-IO-WDM521 SSA1 SSA2 SSA3        
059810     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
059811     PERFORM IMS-STATUSKONTROLL                                           
059812     .                                                                    
063254     EJECT                                                                
063255 IMS-CHECKPOINT SECTION.                                                  
063256     SKIP2                                                                
063257     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
063258     MOVE '  XD' TO GODK-STATUSKODER                                      
063259     CALL CBLTDLI USING CHKP MSG-PCB                                      
063260                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
063261                        CHKP-AREA-LENGTH CHKP-AREA                        
063262     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063263     PERFORM IMS-STATUSKONTROLL                                           
063264                                                                          
063265     IF IMS-EJ-OK                                                         
063266       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-TEXT           
063267       DISPLAY FELTEXT                                                    
063268       CALL FELLOG                                                        
063269     END-IF                                                               
063270     .                                                                    
063271     EJECT                                                                
063272 IMS-RESTART SECTION.                                                     
063273     SKIP2                                                                
063274     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
063275     MOVE '  ' TO GODK-STATUSKODER                                        
063276     CALL CBLTDLI USING XRST MSG-PCB                                      
063277                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
063278                        CHKP-AREA-LENGTH CHKP-AREA                        
063279     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063280     PERFORM IMS-STATUSKONTROLL                                           
063281     .                                                                    
063282     EJECT                                                                
063290 IMS-STATUSKONTROLL SECTION.                                              
063300                                                                          
063400     SET STATUS-IX TO 1                                                   
063500     SEARCH GODK-STATUS                                                   
063600       AT END                                                             
063700         MOVE STATUS-WS        TO FELTEXT-STATUS                          
063800         DISPLAY FELTEXT                                                  
063900         CALL FELLOG                                                      
064000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064100         CONTINUE                                                         
064200     END-SEARCH                                                           
064300     .                                                                    

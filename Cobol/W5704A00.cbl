000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5704A00.                                                
000301 AUTHOR.         ANDERS HENRIKSSON                                        
000401 DATE-WRITTEN.   NOV 2012.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*                                                                         
000901*       -PROGRAMMET LÄSER      WDG2                                       
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
002101*          --- INPUT FROM W57045                                          
002201     SELECT W57045A                    ASSIGN TO W5704AD1.                
002301                                                                          
002401*          --- OUTPUT WITH CNY KRW AED CURRENCY                           
002501     SELECT W57046A                    ASSIGN TO W5704AD2.                
002601                                                                          
002701     EJECT                                                                
002801                                                                          
002901 DATA DIVISION.                                                           
003001                                                                          
003101 FILE SECTION.                                                            
003201 FD  W57045A                                                              
003301     RECORDING       F                                                    
003401     BLOCK CONTAINS  0.                                                   
003501 01  IN-POST.                                                             
003601*    03  -COPY W57045    -PRE  IN-  -L.                                   
003701                                                                          
003801 FD  W57046A                                                              
003901     RECORDING       F                                                    
004001     BLOCK CONTAINS  0.                                                   
004101*01  POST   -COPY W57046 -PRE  UT-  -L.                                   
004201                                                                          
004301 WORKING-STORAGE SECTION.                                                 
004401 77  IDPGM                        PIC X(8)    VALUE 'W5704A00'.           
004501 77  JA                           PIC X       VALUE 'J'.                  
004601 77  NEJ                          PIC X       VALUE 'N'.                  
004701 77  FELTEXT                      PIC X(80).                              
004801 77  WS-MARKUP                    PIC 9V9(2)  VALUE ZERO.                 
004900 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
005000 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
005100 77  W57045A-EOF-SW               PIC X       VALUE 'N'.                  
005200     88  END-OF-W57045A                       VALUE 'J'.                  
005301 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
005401 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
005501 77  WS-SAVE-KDVALISO             PIC X(3)    VALUE SPACE.                
005601 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
005701 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
005702                                                   COMP-3.                
006000                                                                          
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
006300     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
006400     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
006500     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
006601     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
006700                                                                          
006801                                                                          
006901 01  NYCKLAR-TILL-DLI.                                                    
007001     03  W-IDDC-X.                                                        
007101         05  W-IDDC               PIC X(2)    VALUE SPACE.                
007201                                                                          
007301*    --- PARAMETRAR TILL ABEND                                            
007401 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
007501 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
007601 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
007701     EJECT                                                                
007801                                                                          
007901*    --- PARAMETRAR TILL POSTSUM                                          
008001*01  -COPY W0005   -PRE  POSTSUM-                                         
008101     EJECT                                                                
008201*01  -COPY W510CURR                                                       
008301     EJECT                                                                
008400                                                                          
008500 01  IN-AREA-START               PIC X(24)   VALUE                        
008600                                 'IN-AREA-START  '.                       
008700                                                                          
008800*01  AREA -COPY W57045     -PRE IN-                                       
008900     EJECT                                                                
009000                                                                          
009100 01  UT-AREA-START               PIC X(24)   VALUE                        
009200                                 'UT-AREA-START  '.                       
009300                                                                          
009400*01  AREA -COPY W57046     -PRE UT-                                       
009500     EJECT                                                                
009600                                                                          
009700*    --- STATUS-KOD FRÅN IMS                                              
009800 01  STATUS-WS                    PIC XX.                                 
009900     88  SEGMENT-FINNS                        VALUE '  '.                 
010000     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
010100                                                                          
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400                                                                          
010500 01  SSA1                         PIC X(64).                              
010600     EJECT                                                                
010701*    --- IMS FUNKTIONSKODER                                               
010801*01  -COPY W0003                                                          
010901      EJECT                                                               
011000*    ---  DLI INPUT-OUTPUT AREA                                           
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
011200 01  DLI-IO-WDB601.                                                       
011300*    03  -COPY WDB601                                                     
011400     EJECT                                                                
011500                                                                          
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
011700 01  DLI-IO-WDB617.                                                       
011800*    03  -COPY WDB617                                                     
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100*01  -COPY W0008  -PRE WDG2-                                              
012201     05  FILLER                  PIC X.                                   
012301*01  -COPY W0008  -PRE WDB6-                                              
012400     05  FILLER                  PIC X.                                   
012500                                                                          
012600     EJECT                                                                
012700                                                                          
012800 PROCEDURE DIVISION  USING WDG2-PCB WDB6-PCB.                             
012900                                                                          
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING WDG2-PCB WDB6-PCB.                             
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM S01-READ-W57045A                                             
013600     PERFORM UNTIL END-OF-W57045A                                         
013700       PERFORM B-CONVERT-CURRENCY-TO-CNY                                  
013800       PERFORM S01-READ-W57045A                                           
013900     END-PERFORM                                                          
014000                                                                          
014101     PERFORM Z-FINIT                                                      
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600                                                                          
014700 A-INIT SECTION.                                                          
014800     OPEN INPUT  W57045A                                                  
014900     OPEN OUTPUT W57046A                                                  
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 B-CONVERT-CURRENCY-TO-CNY SECTION.                                       
015500     PERFORM BA-GET-EXCHRATE                                              
015601     COMPUTE IN-PRARTNTO   ROUNDED =                                      
015701            ((IN-PRARTNTO * IN-KVAVIS) / W-PRKURS) +                      
015801            ((IN-PRARTNTO * IN-KVAVIS * WS-MARKUP) / W-PRKURS)            
015901     MOVE IN-IDARTNR              TO UT-IDARTNR                           
016001     MOVE IN-IDDC                 TO UT-IDDC                              
016101     MOVE IN-DAINLEV              TO UT-DAINLEV                           
016201     MOVE IN-KVAVIS               TO UT-KVAVIS                            
017001     MOVE IN-PRARTNTO             TO UT-PRARTNTO                          
018001     MOVE IN-KDVALISO             TO UT-KDVALISO                          
019001     MOVE IN-KDTRADP              TO UT-KDTRADP                           
020001     PERFORM S02-WRITE-W57046A                                            
020101     .                                                                    
020201     EJECT                                                                
020301                                                                          
020401 BA-GET-EXCHRATE SECTION.                                                 
020501                                                                          
020601     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
020701                                  - IN-DAINLEV                            
020801     MOVE WS-ACTUAL-DATE             TO WS-ACTUAL-DATE-X                  
020901     MOVE WS-ACTUAL-DATE-X(3:4)      TO W-DATE-AAMM                       
021001                                                                          
021101     IF IN-KDVALISO = WS-SAVE-KDVALISO                                    
021201     AND W-DATE-AAMM = WS-SAVE-MONTH                                      
021301       CONTINUE                                                           
021401     ELSE                                                                 
021501       MOVE IN-KDVALISO           TO WS-SAVE-KDVALISO                     
021601                                                                          
021701       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
021801                                     WS-SAVE-MONTH                        
021901       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
022001       MOVE IN-KDVALISO           TO CURR-KDVALISO-ROW                    
022101       MOVE 'M'                   TO CURR-KDVALTYP                        
022200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
022301       IF CURR-KDSVAR = ' '                                               
022401         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
022501       ELSE                                                               
022601         MOVE 1                   TO W-PRKURS                             
022701       END-IF                                                             
022801                                                                          
022901       PERFORM BAA-GET-LANDING-COST                                       
023001     END-IF                                                               
023101     .                                                                    
023201     EJECT                                                                
023301 BAA-GET-LANDING-COST SECTION.                                            
023401                                                                          
023501     MOVE IN-IDDC                TO W-IDDC                                
023601     PERFORM IMS-GU-WDB601                                                
023701     IF SEGMENT-FINNS                                                     
023801       PERFORM IMS-GNP-WDB617                                             
023901       IF SEGMENT-FINNS                                                   
025001         IF PROC-TILANDCO >  WS-ACTUAL-DATE                               
026001           MOVE PROC-RELANDCO-TO   TO WS-MARKUP                           
027001         ELSE                                                             
028001           MOVE PROC-RELANDCO-FROM TO WS-MARKUP                           
028101         END-IF                                                           
028201       END-IF                                                             
028301     END-IF                                                               
028401     .                                                                    
028501     EJECT                                                                
028601 Z-FINIT SECTION.                                                         
028700     CLOSE W57045A                                                        
028800     CLOSE W57046A                                                        
028900     MOVE 'S' TO POSTSUM-OPKOD                                            
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     .                                                                    
029201     EJECT                                                                
029301                                                                          
029401 S01-READ-W57045A SECTION.                                                
029501     READ W57045A INTO IN-AREA                                            
029601     AT END                                                               
029701        SET END-OF-W57045A TO TRUE                                        
029800                                                                          
029900     NOT AT END                                                           
030000        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
030100        MOVE 'W57045A'     TO POSTSUM-FDNAMN                              
030200        MOVE 'W5704AD1'   TO POSTSUM-DDNAMN2                              
030300        CALL POSTSUM USING POSTSUM-PARM                                   
030400     END-READ                                                             
030500     .                                                                    
030600                                                                          
030700 S02-WRITE-W57046A SECTION.                                               
030800     WRITE UT-POST              FROM UT-AREA                              
030900                                                                          
031001     MOVE 'UT'                 TO POSTSUM-TRANSTYP                        
031100     MOVE 'W57046A'              TO POSTSUM-FDNAMN                        
031200     MOVE 'W5704AD2'            TO POSTSUM-DDNAMN2                        
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     .                                                                    
031500                                                                          
031600 IMS-GU-WDB601    SECTION.                                                
031700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
031800          DELIMITED BY SIZE INTO SSA1                                     
031900     MOVE '  GE' TO GODK-STATUSKODER                                      
032000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
032100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032200     PERFORM IMS-STATUS-CONTROL                                           
032300     .                                                                    
032400     SKIP3                                                                
032500 IMS-GNP-WDB617 SECTION.                                                  
032600     MOVE 'WDB617   ' TO SSA1                                             
032700     MOVE '  GE'        TO GODK-STATUSKODER                               
032800     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB617 SSA1                   
032900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
033000     PERFORM IMS-STATUS-CONTROL                                           
033100     .                                                                    
033200     SKIP3                                                                
033300                                                                          
033400 IMS-STATUS-CONTROL SECTION.                                              
033500     SET STATUS-IX TO 1                                                   
033600     SEARCH GODK-STATUS                                                   
033700       AT END                                                             
033800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033900           DELIMITED BY SIZE INTO FELTEXT                                 
034000         DISPLAY FELTEXT                                                  
034100         CALL FELLOG                                                      
034200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034300         CONTINUE                                                         
034400     END-SEARCH                                                           
034500     .                                                                    

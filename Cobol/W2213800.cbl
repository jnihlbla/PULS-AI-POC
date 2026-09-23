000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2213800.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   03/02/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER HÄNDELSEBAS                                                
000900*        LEVNR SOM HAR ÄNDRAT AVSÄNDNINGS-VECKODAGAR                      
001000*        BEGÄR OMSPEC FÖR ARTIKLAR MED DESSA LEVNR                        
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDR3                                       
001300*        PROGRAMMET LÄSER      WDD9                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001801****************************************************************          
001810*    ÄNDRINGAR:                                                           
001820*    2013-01-22  E'TRACKER 10143273 CHINA  LOCAL SOURCING                 
001830*                                                                         
001840*                                                                         
001850*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- BEGÄRAN OMSPEC                                             
002800     SELECT W22131                     ASSIGN TO W22138D1.                
002900     SKIP2                                                                
003000*          --- HÄNDELSESEGM SOM SKALL TAS BORT                            
003100     SELECT W22139                     ASSIGN TO W22138D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W22131                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W2212204 -PRE  UT-  -L.                                   
004200     SKIP3                                                                
004300 FD  W22139                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY WDR301 -PRE  UT2-  -L.                                    
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W2213800'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500*01  -COPY WWDCKONS                                                       
005600                                                                          
005700 01  WS-FALT.                                                             
005800     03  WS-WDR301-DATA.                                                  
005900         05  WS-IDLEVNR-WDR3     PIC X(5).                                
006000         05  FILLER              PIC X(195).                              
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900*                                                                         
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007500     SKIP2                                                                
007600*    --- PARAMETRAR TILL ABEND                                            
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008100     SKIP2                                                                
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     EJECT                                                                
009000*01  -COPY WDATAREA                                                       
009100     EJECT                                                                
009200 01  UT-AREA-START               PIC X(24)   VALUE                        
009300                                 'UT-AREA-START  '.                       
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W2212204     -PRE UT-                                     
009700     EJECT                                                                
009800 01  UT2-AREA-START              PIC X(24)   VALUE                        
009900                                 'UT2-AREA-START  '.                      
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY WDR301     -PRE UT2-                                      
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000     03  W-WDR301KY-X.                                                    
011100         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
011500     03  W-WDD9A1KY-MIN-X.                                                
011600         05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                 
011700         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
011710         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
011720     03  W-WDD9A1KY-MAX-X.                                                
011730         05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                 
011740         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
011750         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
011800     03  W-IDDC-A1-X.                                                     
011900         05  W-IDDC-A1           PIC X(2)    VALUE '11'.                  
011910     03  W-IDLEVNR-X.                                                     
011920         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012000     03  W-WDD901KY-X.                                                    
012100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012300     03  W-WDD905KY-X.                                                    
012400         05  W-DAAVROP-AVS       PIC 9(6)    VALUE ZERO.                  
012500         05  W-TILEVDAG          PIC S9      VALUE ZERO COMP-3.           
012600     03  W-KDAVROP-X.                                                     
012700         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
012800     03  W-IDPGM-X.                                                       
012900         05  W-IDPGM             PIC X(8)    VALUE 'W2011100'.            
013000     SKIP2                                                                
013100*    --- STATUS-KOD FRÅN IMS                                              
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FINNS                       VALUE '  '.                  
013400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013700     SKIP2                                                                
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01  SSA1                        PIC X(128).                              
014200 01  SSA2                        PIC X(64).                               
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR301'.                      
014900 01  DLI-IO-WDR301.                                                       
015000*    03  -COPY WDR301                                                     
015100     EJECT                                                                
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD9A1'.                      
015300 01  DLI-IO-WDD9A1.                                                       
015400*    03  -COPY WDD9A1    -PRE SEK-                                        
015500     EJECT                                                                
015600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
015700 01  DLI-IO-WDD901.                                                       
015800*    03  -COPY WDD901                                                     
015900     SKIP3                                                                
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
016100 01  DLI-IO-WDD902.                                                       
016200*    03  -COPY WDD902                                                     
016300     EJECT                                                                
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
016500 01  DLI-IO-WDD905.                                                       
016600*    03  -COPY WDD905                                                     
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900                                                                          
017000                                                                          
017100*01  -COPY W0008  -PRE WDR3-                                              
017200     05  FILLER                  PIC X.                                   
017300                                                                          
017400*01  -COPY W0008  -PRE WDD9A-                                             
017500     05  FILLER                  PIC X.                                   
017600                                                                          
017700*01  -COPY W0008  -PRE WDD9-                                              
017800     05  FILLER                  PIC X.                                   
017900                                                                          
018000     EJECT                                                                
018100 PROCEDURE DIVISION  USING WDR3-PCB WDD9A-PCB WDD9-PCB.                   
018200 MAIN SECTION.                                                            
018300     ENTRY 'DLITCBL' USING WDR3-PCB WDD9A-PCB WDD9-PCB.                   
018400                                                                          
018500     PERFORM A-INIT                                                       
018600                                                                          
018700     PERFORM IMS-GET-WDR301                                               
018800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
019000       MOVE FIL-WDR301-DATA  TO WS-WDR301-DATA                            
019100       MOVE WS-IDLEVNR-WDR3  TO W-IDLEVNR                                 
019200                                W-IDLEVNR-MAX                             
019300                                W-IDLEVNR-MIN                             
019400       PERFORM IMS-GU-WDD9A1                                              
019500       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
019600          MOVE SEK-LEVA-IDARTNR TO W-IDARTNR                              
019610          MOVE WC-CDC-SE        TO W-IDDC                                 
019700          MOVE FIL-WDR301-DATA  TO WS-WDR301-DATA                         
019800          MOVE WS-IDLEVNR-WDR3  TO W-IDLEVNR                              
019900          PERFORM IMS-GU-WDD902                                           
020000          IF SEGMENT-FINNS                                                
020100             PERFORM IMS-GNP-WDD905                                       
020200             IF SEGMENT-FINNS                                             
020300                PERFORM B-SKAPA-OMSPEC                                    
020400             END-IF                                                       
020500          END-IF                                                          
020600                                                                          
020700          PERFORM IMS-GN-WDD9A1                                           
020800       END-PERFORM                                                        
020900       PERFORM C-SKAPA-DLET-POST                                          
021000                                                                          
021100       PERFORM IMS-GET-WDR301                                             
021200     END-PERFORM                                                          
021300                                                                          
021400     PERFORM Z-FINIT                                                      
021500                                                                          
021600     MOVE ZERO TO RETURN-CODE                                             
021700     GOBACK                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 A-INIT SECTION.                                                          
022100                                                                          
022200     OPEN OUTPUT W22131                                                   
022300                 W22139                                                   
022400                                                                          
022500     ACCEPT DAGENS-DATUM  FROM DATE                                       
022600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022610                                                                          
022620     MOVE LOW-VALUE  TO W-WDD9A1KY-MIN-X                                  
022630     MOVE HIGH-VALUE TO W-WDD9A1KY-MAX-X                                  
022640                                                                          
022700     .                                                                    
022800     EJECT                                                                
022900 B-SKAPA-OMSPEC SECTION.                                                  
023000                                                                          
023100     MOVE '2204'     TO UT-IDHTYP                                         
023200     MOVE W-IDARTNR  TO UT-IDARTNR                                        
023300     MOVE 19         TO UT-KDLPORS                                        
023400                                                                          
023500     PERFORM S11-SKRIV-W22131                                             
023600     .                                                                    
023700     EJECT                                                                
023800 C-SKAPA-DLET-POST SECTION.                                               
023900                                                                          
024000     MOVE FIL-WDR301 TO UT2-FIL-WDR301                                    
024100                                                                          
024200     PERFORM S12-SKRIV-W22139                                             
024300     .                                                                    
024400     EJECT                                                                
024500 Z-FINIT SECTION.                                                         
024600     CLOSE W22131                                                         
024700           W22139                                                         
024800     SKIP2                                                                
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S11-SKRIV-W22131 SECTION.                                                
025400                                                                          
025500     WRITE UT-POST FROM UT-AREA                                           
025600                                                                          
025700     MOVE UT-IDHTYP  TO POSTSUM-TRANSTYP                                  
025800     MOVE 'W22131'   TO POSTSUM-FDNAMN                                    
025900     MOVE 'W22138D1' TO POSTSUM-DDNAMN2                                   
026000     CALL POSTSUM USING POSTSUM-PARM                                      
026100     .                                                                    
026200     EJECT                                                                
026300 S12-SKRIV-W22139 SECTION.                                                
026400                                                                          
026500     WRITE UT2-POST FROM UT2-AREA                                         
026600                                                                          
026700     MOVE 'DLET'     TO POSTSUM-TRANSTYP                                  
026800     MOVE 'W22139'   TO POSTSUM-FDNAMN                                    
026900     MOVE 'W22138D2' TO POSTSUM-DDNAMN2                                   
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200     EJECT                                                                
027300 S99-ABEND SECTION.                                                       
027400                                                                          
027500     SKIP2                                                                
027600     MOVE 'S' TO POSTSUM-OPKOD                                            
027700     CALL POSTSUM USING POSTSUM-PARM                                      
027800     CALL ABEND USING RKOD-ABEND                                          
027900     .                                                                    
028000     EJECT                                                                
028100* --- IMS SEKTIONER ---                                                   
028200                                                                          
028300 IMS-GET-WDR301 SECTION.                                                  
028400                                                                          
028500*****STRING 'WDR301  (WDR301KY =' W-WDR301KY-X ')'                        
028600     STRING 'WDR301  (IDPGM    =' W-IDPGM-X ')'                           
028700          DELIMITED BY SIZE INTO SSA1                                     
028800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
028900     CALL CBLTDLI USING GN WDR3-PCB DLI-IO-WDR301 SSA1                    
029000     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
029100     PERFORM IMS-STATUSKONTROLL                                           
029200     .                                                                    
029300     EJECT                                                                
029400 IMS-GU-WDD9A1 SECTION.                                                   
029500                                                                          
029600     STRING 'WDD9A1  (WDD9A1KY>=' W-WDD9A1KY-MIN-X                        
029700                    '&WDD9A1KY<=' W-WDD9A1KY-MAX-X                        
029710                    '&IDDC     =' W-IDDC-A1-X ')'                         
029800          DELIMITED BY SIZE INTO SSA1                                     
029900     MOVE '  GE' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GU WDD9A-PCB DLI-IO-WDD9A1 SSA1                   
030100     MOVE WDD9A-STATUS-CODE TO STATUS-WS                                  
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     SKIP3                                                                
030500 IMS-GN-WDD9A1 SECTION.                                                   
030600                                                                          
030700     STRING 'WDD9A1  (WDD9A1KY>=' W-WDD9A1KY-MIN-X                        
030800                    '&WDD9A1KY<=' W-WDD9A1KY-MAX-X                        
030810                    '&IDDC     =' W-IDDC-A1-X ')'                         
030900          DELIMITED BY SIZE INTO SSA1                                     
031000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031100     CALL CBLTDLI USING GN WDD9A-PCB DLI-IO-WDD9A1 SSA1                   
031200     MOVE WDD9A-STATUS-CODE TO STATUS-WS                                  
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031500     EJECT                                                                
031600 IMS-GU-WDD902 SECTION.                                                   
031700                                                                          
031800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
031900          DELIMITED BY SIZE INTO SSA1                                     
032000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
032100          DELIMITED BY SIZE INTO SSA2                                     
032200     MOVE '  GE' TO GODK-STATUSKODER                                      
032300     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
032400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
032500     PERFORM IMS-STATUSKONTROLL                                           
032600     .                                                                    
032700     EJECT                                                                
032800 IMS-GNP-WDD905 SECTION.                                                  
032900                                                                          
033000     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
033100          DELIMITED BY SIZE INTO SSA1                                     
033200     MOVE '  GE' TO GODK-STATUSKODER                                      
033300     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
033400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
033500     PERFORM IMS-STATUSKONTROLL                                           
033600     .                                                                    
033700     EJECT                                                                
033800 IMS-STATUSKONTROLL SECTION.                                              
033900                                                                          
034000     SET STATUS-IX TO 1                                                   
034100     SEARCH GODK-STATUS                                                   
034200       AT END                                                             
034300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034400           DELIMITED BY SIZE INTO FELTEXT                                 
034500         DISPLAY FELTEXT                                                  
034600         CALL FELLOG                                                      
034700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034800         CONTINUE                                                         
034900     END-SEARCH                                                           
035000     .                                                                    

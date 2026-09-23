000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W4405200.                                            
000300 AUTHOR.             ANNELIE ENGLUND.                                     
000400 DATE-WRITTEN.       FEBR. 1991.                                          
000500     REMARKS.                                                             
000600*                                                                         
000700*    FUNKTION.                                                            
000800*             *SB*                                                        
000900*    LÄSER NER WDK9 TILL EN SEKV. FIL                                     
001000*                                                                         
001100*    UTFIL: W44052                                                        
001200*           W44066                                                        
001300*           W44074                                                        
001400*                                                                         
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900     SELECT W44052           ASSIGN TO      W44052D1.                     
002000     SELECT W44066           ASSIGN TO      W44052D2.                     
002100     SELECT W44074           ASSIGN TO      W44052D3.                     
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500     SKIP2                                                                
002600 FD  W44052                                                               
002700     LABEL RECORD STANDARD                                                
002800     RECORDING F                                                          
002900     BLOCK CONTAINS 0.                                                    
003000*01  W44052-POST -COPY W440013    -L                                      
003100     EJECT                                                                
003200 FD  W44066                                                               
003300     LABEL RECORD STANDARD                                                
003400     RECORDING F                                                          
003500     BLOCK CONTAINS 0.                                                    
003600*01  POST -COPY W440066   -PRE W44066-  -L.                               
003700     EJECT                                                                
003800 FD  W44074                                                               
003900     LABEL RECORD STANDARD                                                
004000     RECORDING F                                                          
004100     BLOCK CONTAINS 0.                                                    
004200*01  POST -COPY W440074   -PRE W44074-  -L.                               
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004900*      --- VALID IDDC CODES                                               
005000*                                                                         
005100*01    -COPY WWDCKONS                                                     
005200       EJECT                                                              
005300                                                                          
005400*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
005500                                                                          
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
005800   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
005900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
006000     EJECT                                                                
006100*    ---- PARAMETRAR TILL POSTSUM                                         
006200                                                                          
006300*01  -COPY W0005      -PRE POSTSUM-.                                      
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'UT-W44052'.           
006600     SKIP3                                                                
006700*01  AREA  -COPY W440013    -PRE UT-                                      
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'UT-W44066'.           
007000     SKIP3                                                                
007100*01  AREA -COPY W440066    -PRE U66-.                                     
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'UT-W44074'.           
007400     SKIP3                                                                
007500*01  AREA -COPY W440074    -PRE U74-.                                     
007600     EJECT                                                                
007700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS  '.            
007900                                                                          
008000*    ---- STATUSKOD FRÅN IMS                                              
008100                                                                          
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                      VALUE '  '.                   
008400     88  SEGMENT-SLUT                       VALUE 'GB'.                   
008500     SKIP3                                                                
008600 01  GODK-STATUSKODER.                                                    
008700   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
008800     SKIP3                                                                
008900 01  SSA1                        PIC X(32).                               
009000     EJECT                                                                
009100*01      -COPY W0003.                                                     
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE                         
009400                                            'DLI-IO-AREA'.                
009500 01  DLI-IO-AREA.                                                         
009600   03  IO-AREA                   PIC X(64).                               
009700                                                                          
009800*  03  POST -COPY WDK901             -RED IO-AREA.                        
009900     EJECT                                                                
010000*  03  POST -COPY WDK911             -RED IO-AREA.                        
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300     SKIP2                                                                
010400*    -COPY W0008 -PRE WDK9-.                                              
010500   05  FILLER             PIC X.                                          
010600     EJECT                                                                
010700 PROCEDURE DIVISION  USING WDK9-PCB.                                      
010800     ENTRY 'DLITCBL' USING WDK9-PCB.                                      
010900     SKIP2                                                                
011000     PERFORM A-INIT                                                       
011100                                                                          
011200     PERFORM IMS-GET-WDK9                                                 
011300     PERFORM UNTIL SEGMENT-SLUT                                           
011400       IF WDK9-SEG-NAME-FB = 'WDK901  '                                   
011500         IF UT-ART-SUTPO-TOT NOT = ZERO                                   
011600           PERFORM B-SKRIV-W44052-UTPOST                                  
011700         END-IF                                                           
011800         IF UT-ART-SUTPO-TOT < ZERO                                       
011810           DISPLAY 'ART MED -SUTPO-TOT: ' UT-ART-IDARTNR                  
011811                   ' ' UT-ART-SUTPO-TOT                                   
011820         END-IF                                                           
011900         MOVE ART-IDARTNR      TO UT-ART-IDARTNR                          
012000         MOVE WC-CDC-SE        TO UT-ART-IDDC                             
012100         MOVE ART-SUTPO-TOT    TO UT-ART-SUTPO-TOT                        
012200                                                                          
012300         PERFORM C-SKRIV-W44066-UTPOST                                    
012400         PERFORM D-SKRIV-W44074-UTPOST                                    
012500       END-IF                                                             
012600                                                                          
012700       IF WDK9-SEG-NAME-FB = 'WDK911  '                                   
012800         ADD ANT-SUTPO-PB      TO UT-ART-SUTPO-PB                         
012900         ADD ANT-SUTPO-EJPB    TO UT-ART-SUTPO-EJPB                       
012910         IF ANT-SUTPO-PB < ZERO                                           
012920           DISPLAY 'ART MED -SUTPO-PB: ' UT-ART-IDARTNR                   
012921                   ' ' ANT-SUTPO-PB                                       
012930         END-IF                                                           
012940         IF ANT-SUTPO-EJPB < ZERO                                         
012950           DISPLAY 'ART MED -SUTPO-EJPB: ' UT-ART-IDARTNR                 
012951                   ' ' ANT-SUTPO-EJPB                                     
012960         END-IF                                                           
013000       END-IF                                                             
013100                                                                          
013200       PERFORM IMS-GET-WDK9                                               
013300     END-PERFORM                                                          
013400                                                                          
013500     IF UT-ART-SUTPO-TOT > ZERO                                           
013600       PERFORM B-SKRIV-W44052-UTPOST                                      
013700     END-IF                                                               
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     OPEN OUTPUT W44052                                                   
014700                 W44066                                                   
014800                 W44074                                                   
014900                                                                          
015000     MOVE ZERO       TO UT-ART-IDARTNR                                    
015100                        UT-ART-IDDC                                       
015200                        UT-ART-SUTPO-TOT                                  
015300                        UT-ART-SUTPO-PB                                   
015400                        UT-ART-SUTPO-EJPB                                 
015500     MOVE 'W4405200' TO POSTSUM-PROGNAMN                                  
015600     .                                                                    
015700     EJECT                                                                
015800   B-SKRIV-W44052-UTPOST SECTION.                                         
015900                                                                          
016000     WRITE W44052-POST FROM UT-AREA                                       
016100                                                                          
016200     MOVE 'UT '      TO POSTSUM-TRANSTYP                                  
016300     MOVE 'W44052'   TO POSTSUM-FDNAMN                                    
016400     MOVE 'W44052D1' TO POSTSUM-DDNAMN2                                   
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600                                                                          
016700     MOVE ZERO       TO UT-ART-IDARTNR                                    
016800                        UT-ART-IDDC                                       
016900                        UT-ART-SUTPO-TOT                                  
017000                        UT-ART-SUTPO-PB                                   
017100                        UT-ART-SUTPO-EJPB                                 
017200     .                                                                    
017300     EJECT                                                                
017400   C-SKRIV-W44066-UTPOST SECTION.                                         
017500                                                                          
017600     MOVE  ART-IDARTNR           TO U66-IDARTNR                           
017700     MOVE  WC-CDC-SE             TO U66-IDDC                              
017800                                                                          
017900     MOVE  ART-KVOKS-BULK        TO U66-KVOKS-BULK                        
018000     MOVE  ART-KVOKS-DAG         TO U66-KVOKS-DAG                         
018100     MOVE  ART-KVOKS-VOR         TO U66-KVOKS-VOR                         
018200                                                                          
018300     MOVE  ART-KVPREAVB-BULK     TO U66-KVPREAVB-BULK                     
018400     MOVE  ART-KVPREAVB-DAG      TO U66-KVPREAVB-DAG                      
018500     MOVE  ART-KVPREAVB-VOR      TO U66-KVPREAVB-VOR                      
018600                                                                          
018700     MOVE  ART-KVPRERO-BULK      TO U66-KVPRERO-BULK                      
018800     MOVE  ART-KVPRERO-DAG       TO U66-KVPRERO-DAG                       
018900                                                                          
019000     WRITE W44066-POST         FROM U66-AREA                              
019100                                                                          
019200     MOVE 'U66 '                 TO POSTSUM-TRANSTYP                      
019300     MOVE 'W44066'               TO POSTSUM-FDNAMN                        
019400     MOVE 'W44052D2'             TO POSTSUM-DDNAMN2                       
019500     CALL  POSTSUM            USING POSTSUM-PARM                          
019600     .                                                                    
019700     EJECT                                                                
019800   D-SKRIV-W44074-UTPOST SECTION.                                         
019900                                                                          
020000     MOVE  ART-IDARTNR       TO U74-IDARTNR                               
020100     MOVE  ART-KVOFFERT      TO U74-KVOFFERT                              
020200                                                                          
020300     WRITE W44074-POST     FROM U74-AREA                                  
020400                                                                          
020500     MOVE 'U74 '             TO POSTSUM-TRANSTYP                          
020600     MOVE 'W44074  '         TO POSTSUM-FDNAMN                            
020700     MOVE 'W44052D3'         TO POSTSUM-DDNAMN2                           
020800     CALL  POSTSUM        USING POSTSUM-PARM                              
020900     .                                                                    
021000     EJECT                                                                
021100 Z-FINIT SECTION.                                                         
021200                                                                          
021300     CLOSE W44052                                                         
021400           W44066                                                         
021500           W44074                                                         
021600     MOVE 'S' TO POSTSUM-OPKOD                                            
021700     CALL POSTSUM USING POSTSUM-PARM                                      
021800     .                                                                    
021900     EJECT                                                                
022000*    ---- IMS SEKTIONER                                                   
022100 IMS-GET-WDK9 SECTION.                                                    
022200                                                                          
022300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022400     CALL CBLTDLI USING GN WDK9-PCB DLI-IO-AREA                           
022500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
022600     PERFORM IMS-STATUSKONTROLL.                                          
022700     SKIP3                                                                
022800 IMS-STATUSKONTROLL SECTION.                                              
022900                                                                          
023000     SET STATUS-IX TO 1                                                   
023100     SEARCH GODK-STATUS                                                   
023200     AT END                                                               
023300     STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                               
023400     DELIMITED BY SIZE INTO FELTEXT                                       
023500     CALL FELLOG                                                          
023600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
023700     END-SEARCH                                                           
023800     .                                                                    

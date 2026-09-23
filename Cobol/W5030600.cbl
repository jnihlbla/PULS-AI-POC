000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5030600.                                                
000400 AUTHOR.         HÅKAN JOHANSSON.                                         
000500 DATE-WRITTEN.   SEPTEMBER 1989.                                          
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        VISAR NOLLAD ARTIKEL                                             
001200*        KAN LOSSA NOLLAD ARTIKEL MED PF11                                
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W5T306                                              
001700*        MID:         W5I30601                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W5O30601                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002610                                                                          
002700*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(8)    VALUE 'W5030600'.            
003000 77  JA                          PIC X(1)    VALUE 'J'.                   
003100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003200                                                                          
003300 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
003400 77  WS-NY-NYCKEL                PIC X(1)    VALUE 'N'.                   
003500 77  CL-INDEX                    PIC S9(9)   VALUE +0 COMP SYNC.          
003600 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +155 COMP SYNC.          
003700 77  WS-RAKNARE                  PIC 99    VALUE ZERO.                    
003710                                                                          
003720 01  DYNAMISKA-SUBPROGRAM.                                                
003730     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003740     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003750                                                                          
003800 01  DAGENS-DATUM                PIC S9(6).                               
003900                                                                          
004000 01   WS-IDTRANS                  PIC X(4).                               
004100     88  GODKAEND-BILD               VALUE '5301'                         
004200                                           '5302'                         
004300                                           '5306'.                        
004400     EJECT                                                                
004500 01  FILLER                      PIC X(10)   VALUE 'DLINYCKLAR'.          
004600 01  NYCKLAR-TILL-DLI.                                                    
004700*                                                                         
004800     03  W-WDGXKEY-ROT-X.                                                 
004900         05  FILLER              PIC X(4)    VALUE '5115'.                
005000         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
005100*                                                                         
005200     03  W-WDGXKEY-X.                                                     
005300         05  W-IDDC-UTR          PIC  X(2)   VALUE SPACE.                 
005400         05  W-IDARTNR-UTR       PIC S9(9)   VALUE ZERO COMP-3.           
005500         05  FILLER              PIC  X(8)   VALUE LOW-VALUE.             
005600*                                                                         
005610     03  W-WDGXKEY-MIN-X.                                                 
005620         05  W-IDDC-UTR-MIN      PIC  X(2)   VALUE SPACE.                 
005630         05  W-IDARTNR-UTR-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
005640         05  FILLER              PIC  X(8)   VALUE LOW-VALUE.             
005650*                                                                         
005660     03  W-WDGXKEY-MAX-X.                                                 
005670         05  W-IDDC-UTR-MAX     PIC  X(2) VALUE SPACE.                    
005680         05  W-IDARTNR-UTR-MAX  PIC S9(9) VALUE +999999999 COMP-3.        
005690         05  FILLER             PIC  X(8) VALUE HIGH-VALUE.               
005691*                                                                         
005700     03  W-IDARTNR-X.                                                     
005800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
005900*                                                                         
006000     03  W-KDSEGKEY-X.                                                    
006100         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
006200                                                                          
006210     03  W-IDDC-X.                                                        
006220         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
006230                                                                          
006240     03  W-IDDC-B6-X.                                                     
006250         05 W-IDDC-B6            PIC X(2).                                
006260                                                                          
006300     EJECT                                                                
006301 01  GENERELLA-SUBPROGRAM.                                                
006302     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006303*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006304*01 -COPY WMSGINIT                                                        
006340     EJECT                                                                
006400                                                                          
006500 01  FILLER                      PIC X(9)    VALUE 'FELTEXTER'.           
006600 01  W-FEL-1.                                                             
006700     03  FEL-1-SVE               PIC X(31)                                
006800             VALUE 'ARTIKELN EJ NOLLAD             '.                     
006900     03  FEL-1-ENG               PIC X(31)                                
007000             VALUE 'PART NUMBER REPORTED WITH ZERO '.                     
007100 01  FILLER REDEFINES W-FEL-1.                                            
007200     03  FEL-1 OCCURS 2          PIC X(31).                               
007300     SKIP2                                                                
007400 01  W-FEL-2.                                                             
007500     03  FEL-2-SVE               PIC X(17)                                
007600                             VALUE 'FEL NYCKEL       '.                   
007700     03  FEL-2-ENG               PIC X(17)                                
007800                             VALUE 'WRONG KEY'.                           
007900 01  FILLER REDEFINES W-FEL-2.                                            
008000     03  FEL-2 OCCURS 2          PIC X(17).                               
008100     EJECT                                                                
008200                                                                          
008300 01  W-MED-1.                                                             
008400     03  MED-1-SVE               PIC X(20)                                
008500           VALUE 'UPPDATERING UTFÖRD  '.                                  
008600     03  MED-1-ENG               PIC X(20)                                
008700           VALUE 'UPDATE DONE '.                                          
008800 01  FILLER REDEFINES W-MED-1.                                            
008900     03  MED-1 OCCURS 2          PIC X(20).                               
009000*****************************************************************         
009100*                                                               *         
009200*                AREOR FÖR MFS OCH SKÄRMHANTERING               *         
009300*                                                               *         
009400*****************************************************************         
009500     SKIP2                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
009700     SKIP2                                                                
009800*01  MID -COPY W5I30601                                                   
010000     EJECT                                                                
010100*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400*    03  MOD -COPY W5O30601    -RED MSG-AREA                              
010600     EJECT                                                                
010700*01  -COPY WMFSAREA                                                       
010900     EJECT                                                                
011000*****************************************************************         
011100*                                                                         
011200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011300*                                                                         
011400 01  IMS-WS.                                                              
011500     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
011600     SKIP3                                                                
011700*                                                                         
011800     03  STATUS-WS               PIC X(2).                                
011900         88  SEGMENT-FINNS                   VALUE '  '.                  
012000         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
012100     SKIP3                                                                
012200     03  GODK-STATUSKODER.                                                
012300         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
012400     SKIP3                                                                
012500 01    SSA1                      PIC X(64).                               
012600 01    SSA2                      PIC X(64).                               
012700 01    SSA3                      PIC X(64).                               
012800     EJECT                                                                
012900*                            IMS FUNKTIONSKODER                           
013000*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*                            DLI INPUT-OUTPUT AREA                        
013400 01  FILLER                      PIC X(7)    VALUE 'IOAREA '.             
013500 01  DLI-IO-AREA.                                                         
013600*                                                                         
013700     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
013800     SKIP2                                                                
013900                                                                          
014000*03  WLXXEF11 -COPY WDGX5116  -RED IO-AREA                                
014200     EJECT                                                                
014300                                                                          
014310 01  DLI-IO-AREA2.                                                        
014320*                                                                         
014330     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
014340     SKIP2                                                                
014400*03  WLARTC11 -COPY WDK611              -RED IO-AREA2                     
014600     EJECT                                                                
014700                                                                          
014710 01  DLI-IO-AREA3.                                                        
014720*                                                                         
014730     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
014740     SKIP2                                                                
014750*03  WLARTS11 -COPY WDK711              -RED IO-AREA3                     
014751                                                                          
014752 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014753 01   DLI-IO-AREA-B601.                                                   
014754*     03  -COPY WDB601                                                    
014755                                                                          
014760     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900*01  -COPY W0009    -PRE MSG-                                             
015000     EJECT                                                                
015400*01  -COPY W0008    -PRE USEA-                                            
015600         05  FILLER              PIC X(1).                                
015700                                                                          
015720*01  -COPY W0008    -PRE ARTREG-                                          
015730         05  FILLER              PIC X(1).                                
015740     EJECT                                                                
016000*01  -COPY W0008    -PRE XXEF-                                            
016200         05  FILLER              PIC X(1).                                
016400                                                                          
016420*01  -COPY W0008    -PRE ARTS-                                            
016430         05  FILLER              PIC X(1).                                
016431                                                                          
016432*01  -COPY W0008    -PRE WDB6-                                            
016433         05  FILLER              PIC X(1).                                
016440     EJECT                                                                
016500 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
016510                          ARTREG-PCB XXEF-PCB ARTS-PCB WDB6-PCB.          
016520 MAIN SECTION.                                                            
016600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
016610                          ARTREG-PCB XXEF-PCB ARTS-PCB WDB6-PCB.          
016800                                                                          
016900     PERFORM IMS-GET-MSG                                                  
017000     IF SEGMENT-FINNS                                                     
017100        PERFORM A-INIT                                                    
017200        PERFORM B-BEHANDLA-INPUT                                          
017210        PERFORM E-RENSA-HANDELSER                                         
017300        IF WS-IDARTNR NUMERIC AND                                         
017310          (DCS-KDDC > SPACE AND NOT DCS-DDC)                              
017400           IF WS-NY-NYCKEL = JA                                           
017500             OR NOT MFS-UPDATE                                            
017600               PERFORM  C-VISA-NOLLAD-ARTIKEL                             
017700           ELSE                                                           
017800               IF MFS-IDTRANS = '5306'                                    
017900                   PERFORM D-RADERA-NOLLAD-ARTIKEL                        
018000               END-IF                                                     
018100           END-IF                                                         
018200        ELSE                                                              
018400               MOVE FEL-2 (CL-INDEX) TO MOD-TEMFSFEL                      
018800        END-IF                                                            
018900        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
019000        PERFORM IMS-INSERT-MSG                                            
019100     END-IF                                                               
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 A-INIT SECTION.                                                          
019800                                                                          
019900     IF MSG-DUBBLA-TRANSKODER                                             
020000         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30601               
020100         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
020200         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
020300         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
020400     ELSE                                                                 
020500         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30601                
020600         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
020700         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
020800         MOVE SPACE TO MFS-KDTRTYP                                        
020900     END-IF                                                               
021000                                                                          
021100     MOVE LOW-VALUE  TO MSG-AREA                                          
021200     MOVE 'W5O30601' TO MFS-IDMOD                                         
021300     MOVE '5306'     TO MOD-IDTRANS                                       
021400                                                                          
021500     IF ENGLISH-TEXT                                                      
021600         MOVE 2 TO CL-INDEX                                               
021700         MOVE 'N' TO MFS-KDHUVOMR                                         
021800     ELSE                                                                 
021900         MOVE 1 TO CL-INDEX                                               
022000     END-IF                                                               
022100                                                                          
022200     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
022300                                                                          
022400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
022410                             MOD-IDDC-IN                                  
022500                             MOD-TEMFSFEL                                 
022600                             MOD-TEMFSINF                                 
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 B-BEHANDLA-INPUT SECTION.                                                
023200                                                                          
023202     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023203     MOVE '001'             TO MSGI-KDCALL                                
023204     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023205     MOVE '5306'            TO MSGI-IDTRANS                               
023206     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023207                                                                          
023208     IF MFS-IDTRANS = '5306'                                              
023209     OR (MID-IDARTNR-IN NUMERIC                                           
023210     AND MID-IDARTNR-IN > ZERO)                                           
023211         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
023212     END-IF                                                               
023213     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023214     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
023215     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
023220                                                                          
023230     IF (MFS-IDTRANS = '5306'                                             
023300     AND MID-IDARTNR-IN = ALL '+')                                        
023400     OR (MFS-IDTRANS NOT = '5306'                                         
023500     AND WS-IDARTNR = ZERO)                                               
023600         MOVE NEJ TO WS-NY-NYCKEL                                         
023700     ELSE                                                                 
023900         MOVE JA TO WS-NY-NYCKEL                                          
024000     END-IF                                                               
024100                                                                          
024120     MOVE MSGI-IDDC       TO W-IDDC-B6                                    
024130     PERFORM IMS-GU-WDB601                                                
024190                                                                          
024200     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
024300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
024400     INSPECT MOD-IDARTNR-UT REPLACING LEADING '+' BY SPACE                
024500                                                                          
024510     MOVE W-IDDC-B6 TO MOD-IDDC-UT                                        
024520     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
024530     INSPECT MOD-IDDC-UT REPLACING LEADING '+' BY SPACE                   
024540                                                                          
024600     ACCEPT DAGENS-DATUM FROM DATE                                        
024700     .                                                                    
024800     EJECT                                                                
024900                                                                          
025000 C-VISA-NOLLAD-ARTIKEL SECTION.                                           
025200                                                                          
025300     MOVE W-IDDC-B6    TO W-IDDC-UTR                                      
025400     MOVE WS-IDARTNR   TO W-IDARTNR-UTR                                   
025500     PERFORM IMS-GU-ART-UTREDNSALDO                                       
025600                                                                          
025700     IF SEGMENT-FINNS                                                     
025800        IF 5116-IDPRODNR = ZERO                                           
025900            MOVE SPACE TO MOD-KDORDKL                                     
026000        ELSE                                                              
026100            MOVE 5116-IDPRODNR TO MOD-IDPRODNR                            
026200            MOVE 5116-KDORDKL  TO MOD-KDORDKL                             
026300        END-IF                                                            
026400        MOVE 5116-IDPW      TO MOD-IDPW                                   
026500        MOVE 5116-TIUPPDAT  TO MOD-TIUPPDAT                               
026600        MOVE 5116-TIUPPTID  TO MOD-TIUPPTID                               
026700        INSPECT MOD-TIUPPTID REPLACING FIRST SPACE BY ':'                 
026800     ELSE                                                                 
027000             MOVE FEL-1 (CL-INDEX) TO MOD-TEMFSFEL                        
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027800 D-RADERA-NOLLAD-ARTIKEL SECTION.                                         
028000                                                                          
028100     MOVE WS-IDARTNR   TO W-IDARTNR-UTR  W-IDARTNR                        
028200     MOVE W-IDDC-B6    TO W-IDDC-UTR                                      
028300     PERFORM IMS-GHU-ART-UTREDNSALDO                                      
028400     IF SEGMENT-FINNS                                                     
028500       PERFORM IMS-DELETE-ART-UTREDNSALDO                                 
028510       IF DCS-CDC OR DCS-CDC-TR                                           
028600         PERFORM IMS-LAES-ART-WDK6                                        
028610         IF SEGMENT-FINNS                                                 
028700           MOVE +0 TO CLAG-KVUTRS                                         
028800           PERFORM IMS-REPL-WDK6-SEGM                                     
028801         END-IF                                                           
028810       ELSE                                                               
028820         PERFORM IMS-LAES-ART-WDK7                                        
028821         IF SEGMENT-FINNS                                                 
028830           MOVE +0 TO SLAG-KVUTRS                                         
028840           PERFORM IMS-REPL-WDK7-SEGM                                     
028841         END-IF                                                           
028850       END-IF                                                             
028900       MOVE MED-1 (CL-INDEX) TO MOD-TEMFSINF                              
029000     ELSE                                                                 
029200       MOVE FEL-1 (CL-INDEX) TO MOD-TEMFSFEL                              
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
030100                                                                          
030110 E-RENSA-HANDELSER SECTION.                                               
030111* RENSAR BORT ÄLDRE  HÄNDELSER OM UTREDNINGSSALDO ÄR NOLL PÅ              
030112* WDK6 ALT. WDK7                                                          
030113     IF DCS-KDDC = SPACE OR DCS-DDC                                       
030114       CONTINUE                                                           
030115     ELSE                                                                 
030116       MOVE DCS-IDDC            TO W-IDDC-UTR-MIN                         
030117                                   W-IDDC-UTR-MAX                         
030118                                   W-IDDC                                 
030119       MOVE 1                   TO WS-RAKNARE                             
030120                                                                          
030121       PERFORM UNTIL WS-RAKNARE > 10                                      
030122         PERFORM IMS-GHN-ART-UTREDNSALDO                                  
030123         IF SEGMENT-FINNS                                                 
030124           MOVE 5116-IDARTNR       TO W-IDARTNR                           
030125           IF DCS-CDC OR DCS-CDC-TR                                       
030126             PERFORM IMS-LAES-ART-WDK6                                    
030127             IF SEGMENT-FINNS                                             
030128               IF CLAG-KVUTRS = 0                                         
030129                 PERFORM IMS-DELETE-ART-UTREDNSALDO                       
030130               END-IF                                                     
030131             END-IF                                                       
030132           ELSE                                                           
030133             PERFORM IMS-LAES-ART-WDK7                                    
030134             IF SEGMENT-FINNS                                             
030135               IF SLAG-KVUTRS = 0                                         
030136                 PERFORM IMS-DELETE-ART-UTREDNSALDO                       
030137               END-IF                                                     
030138             END-IF                                                       
030139           END-IF                                                         
030140         ELSE                                                             
030141           MOVE 11                 TO WS-RAKNARE                          
030142         END-IF                                                           
030143       ADD +1                      TO WS-RAKNARE                          
030144       END-PERFORM                                                        
030145     END-IF                                                               
030146     .                                                                    
030147     EJECT                                                                
030150* IMS SEKTIONER                                                           
030200 IMS-GET-MSG SECTION.                                                     
030300     MOVE '  QC' TO GODK-STATUSKODER                                      
030400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800     SKIP3                                                                
030900 IMS-INSERT-MSG SECTION.                                                  
031000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031100     MOVE SPACE TO GODK-STATUSKODER                                       
031200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031600     EJECT                                                                
031700                                                                          
031800 IMS-GHN-ART-UTREDNSALDO SECTION.                                         
031900                                                                          
032000     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
032100             DELIMITED BY SIZE INTO SSA1                                  
032210     STRING  'WLXXEF11(WDGXKEY >=' W-WDGXKEY-MIN-X                        
032220                     '&WDGXKEY <=' W-WDGXKEY-MAX-X ')'                    
032300             DELIMITED BY SIZE INTO SSA2                                  
032400     MOVE '  GE' TO GODK-STATUSKODER                                      
032500     CALL CBLTDLI USING GHN XXEF-PCB DLI-IO-AREA SSA1 SSA2                
032600     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSKONTROLL                                           
032800     .                                                                    
032900     EJECT                                                                
033000                                                                          
033010 IMS-GU-ART-UTREDNSALDO SECTION.                                          
033020                                                                          
033030     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
033040             DELIMITED BY SIZE INTO SSA1                                  
033050     STRING 'WLXXEF11(WDGXKEY  =' W-WDGXKEY-X ')'                         
033060             DELIMITED BY SIZE INTO SSA2                                  
033070     MOVE '  GE' TO GODK-STATUSKODER                                      
033080     CALL CBLTDLI USING GU XXEF-PCB DLI-IO-AREA SSA1 SSA2                 
033090     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
033091     PERFORM IMS-STATUSKONTROLL                                           
033092     .                                                                    
033093     EJECT                                                                
033094                                                                          
033100 IMS-GHU-ART-UTREDNSALDO SECTION.                                         
033200                                                                          
033300     STRING 'WLXXEF01(WDGXKEY  =' W-WDGXKEY-ROT-X ')'                     
033400             DELIMITED BY SIZE INTO SSA1                                  
033500     STRING 'WLXXEF11(WDGXKEY  =' W-WDGXKEY-X ')'                         
033600             DELIMITED BY SIZE INTO SSA2                                  
033700     MOVE '  GE' TO GODK-STATUSKODER                                      
033800     CALL CBLTDLI USING GHU XXEF-PCB DLI-IO-AREA SSA1 SSA2                
033900     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200     SKIP2                                                                
034300 IMS-DELETE-ART-UTREDNSALDO SECTION.                                      
034400                                                                          
034500     MOVE '  ' TO GODK-STATUSKODER                                        
034600     CALL CBLTDLI USING DLET XXEF-PCB DLI-IO-AREA                         
034700     MOVE XXEF-STATUS-CODE TO STATUS-WS                                   
034800     PERFORM IMS-STATUSKONTROLL                                           
034900     .                                                                    
035000     EJECT                                                                
035100                                                                          
035200 IMS-LAES-ART-WDK6 SECTION.                                               
035300                                                                          
035400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
035500            DELIMITED BY SIZE INTO SSA1                                   
035700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
035800            DELIMITED BY SIZE INTO SSA2                                   
035900     MOVE '  GE' TO GODK-STATUSKODER                                      
036000     CALL CBLTDLI USING GHU ARTREG-PCB IO-AREA2 SSA1 SSA2                 
036100     MOVE ARTREG-STATUS-CODE TO STATUS-WS                                 
036200     PERFORM IMS-STATUSKONTROLL                                           
036300     .                                                                    
036400     SKIP2                                                                
036500                                                                          
036600 IMS-REPL-WDK6-SEGM SECTION.                                              
036700                                                                          
036800     MOVE '  ' TO GODK-STATUSKODER                                        
036900     CALL CBLTDLI USING REPL ARTREG-PCB IO-AREA2                          
037000     MOVE ARTREG-STATUS-CODE TO STATUS-WS                                 
037100     PERFORM IMS-STATUSKONTROLL                                           
037200     .                                                                    
037300     EJECT                                                                
037310 IMS-LAES-ART-WDK7 SECTION.                                               
037320                                                                          
037330     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
037340            DELIMITED BY SIZE INTO SSA1                                   
037350     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
037360            DELIMITED BY SIZE INTO SSA2                                   
037370     MOVE '  GE' TO GODK-STATUSKODER                                      
037380     CALL CBLTDLI USING GHU ARTS-PCB IO-AREA3 SSA1 SSA2                   
037390     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
037391     PERFORM IMS-STATUSKONTROLL                                           
037392     .                                                                    
037393     SKIP2                                                                
037394                                                                          
037395 IMS-REPL-WDK7-SEGM SECTION.                                              
037396                                                                          
037397     MOVE '  ' TO GODK-STATUSKODER                                        
037398     CALL CBLTDLI USING REPL ARTS-PCB IO-AREA3                            
037399     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
037400     PERFORM IMS-STATUSKONTROLL                                           
037401     .                                                                    
037402     EJECT                                                                
037403 IMS-GU-WDB601    SECTION.                                                
037404     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
037405          DELIMITED BY SIZE INTO SSA1                                     
037406     MOVE '  GE' TO GODK-STATUSKODER                                      
037407     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
037408     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
037409     PERFORM IMS-STATUSKONTROLL                                           
037410     IF SEGMENT-SAKNAS                                                    
037411         MOVE SPACE TO DCS-KDDC                                           
037412     END-IF                                                               
037413     .                                                                    
037420                                                                          
037500 IMS-STATUSKONTROLL SECTION.                                              
037600     SKIP2                                                                
037700     SET STATUS-IX TO 1                                                   
037800     SEARCH GODK-STATUS                                                   
037810       AT END                                                             
037820         CALL FELLOG                                                      
037900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
038000     END-SEARCH                                                           
038100     .                                                                    

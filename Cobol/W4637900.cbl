001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4637900.                                                
001200 AUTHOR.         GERRY CARMICHAEL.                                        
001300 DATE-WRITTEN.   98/08/24.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        SB.                                                              
001810*        SKAPAR FIL FÖR RENSNING AV MJUKVARUORDERRADER SOM                
001820*        ÄR ÄLDRE ÄN 30 + 10 DAGAR (PIE RENSAR EFTER 30).                 
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDQ3   SB                                  
002020*                              WDE4                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003320*          --- SOFTWARE ORDER FÖR RENSNING .                              
003330     SELECT W46379                     ASSIGN TO W46379D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003930 FD  W46379                                                               
003940     RECORDING       F                                                    
003950     BLOCK CONTAINS  0.                                                   
003960                                                                          
003970*01  POST -COPY W463VG3 -PRE  VG3- -L.                                    
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004201                                                                          
004202*    -COPY WY2000W1                                                       
004203     SKIP3                                                                
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W4637900'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  WS-IDDISTR                  PIC 9(5).                                
004700 77  WS-IDKUNDNR                 PIC 9(7).                                
004701 77  WS-IDORDNR7                 PIC 9(7).                                
004710 77  WS-IDARTNR                  PIC 9(9).                                
004800     EJECT                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005320 01  WS-TIORDREG-RENS            PIC 9(6)    VALUE ZERO.                  
005400     EJECT                                                                
005410*      --- VALID IDDC CODES                                               
005420*                                                                         
005430*01    -COPY WWDC99                                                       
005440       EJECT                                                              
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006101     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006110     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006120     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL DATKORT                                          
007400*                                                                         
007500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W46379'.              
007600     SKIP2                                                                
007700 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
007800     SKIP2                                                                
007900*01  -COPY WDATKORT                                                       
008001     EJECT                                                                
008002*    --- PARAMETRAR TILL POSTSUM                                          
008003*                                                                         
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008103     EJECT                                                                
008105*                                                                         
008106 01  FILLER                      PIC X(16) VALUE 'CIA-AREA'.              
008107     SKIP3                                                                
008108*01  -COPY W009CIA                                                        
008109     EJECT                                                                
008110*01  -COPY WDAGAREA                                                       
008201     EJECT                                                                
008202                                                                          
008203 01  VG3-AREA-START              PIC X(24)   VALUE                        
008204                                 'VG3-AREA-START '.                       
008313     SKIP2                                                                
008320*01  AREA -COPY W463VG3    -PRE VG3-                                      
008330     EJECT                                                                
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009001     03  W-WDQ301KY-X.                                                    
009010         05  W-WDQ301KY          PIC X(12)    VALUE SPACE.                
009020     03  W-WDE4KEY-X.                                                     
009030         05  W-IDDISTR           PIC S9(5)    COMP-3 VALUE ZERO.          
009040         05  W-IDKUNDNR          PIC S9(7)    COMP-3 VALUE ZERO.          
009050         05  W-IDORDNR5          PIC 9(5)            VALUE ZERO.          
009060         05  FILLER              PIC X(5)            VALUE SPACE.         
009070         05  W-IDPRODNR          PIC S9(7)    COMP-3 VALUE ZERO.          
009090         05  W-IDPLKLST          PIC S9(3)    COMP-3 VALUE ZERO.          
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009510     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNKTIONSKODER                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3'.                        
010802 01  DLI-IO-WDQ3.                                                         
010810*    03  -COPY WDQ301                                                     
010811     EJECT                                                                
010820 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
010830 01  DLI-IO-WDE401.                                                       
010840*    03  -COPY WDE401                                                     
011100     EJECT                                                                
011110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
011120 01  DLI-IO-WDE411.                                                       
011130*    03  -COPY WDE411                                                     
011180     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300                                                                          
011401     EJECT                                                                
011402*01  -COPY W0008  -PRE WDQ3-                                              
011410     05  FILLER                  PIC X.                                   
011420     EJECT                                                                
011430*01  -COPY W0008  -PRE WDE4-                                              
011440     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING WDQ3-PCB WDE4-PCB.                             
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING WDQ3-PCB WDE4-PCB.                             
011700                                                                          
011900                                                                          
012000     PERFORM A-INIT                                                       
012100                                                                          
012201     PERFORM IMS-GET-WDQ3                                                 
012202     PERFORM UNTIL SEGMENT-SLUT                                           
012203       EVALUATE WDQ3-SEG-NAME-FB                                          
012204         WHEN 'WDQ301'                                                    
012205           MOVE ODEL-IDDC      TO WS-IDDC                                 
012216           IF CDC-SE AND                                                  
012217              (ODEL-IDLEVNR = '1441 ' OR                                  
012218              ODEL-IDLEVNR = 'BP2TW') AND                                 
012219              ODEL-KDODELSTA = 'U'                                        
012220              PERFORM B-KOLLA-OM-RENSNING                                 
012221           END-IF                                                         
012222       END-EVALUATE                                                       
012223       PERFORM IMS-GET-WDQ3                                               
012230     END-PERFORM                                                          
012300     PERFORM Z-FINIT                                                      
012400                                                                          
012500     MOVE ZERO TO RETURN-CODE                                             
012600     GOBACK                                                               
012700     .                                                                    
012800     EJECT                                                                
012900 A-INIT SECTION.                                                          
013101                                                                          
013110     OPEN OUTPUT W46379                                                   
013200                                                                          
013300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013400     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
013500     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
013600     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013711     PERFORM AA-BERAKNA-TESTDATUM                                         
013720     .                                                                    
013730     EJECT                                                                
013731 AA-BERAKNA-TESTDATUM SECTION.                                            
013732                                                                          
013764     MOVE 003 TO DAG-KDCALL.                                              
013765     MOVE DAGENS-DATUM TO DAG-TIAAMMDD-TOM                                
013766     MOVE 40           TO DAG-KVKALDAG                                    
013767     IF   DAGENS-DATUM > 500000                                           
013768         MOVE 19 TO DAG-TISEKEL-TOM                                       
013769     ELSE                                                                 
013770         MOVE 20 TO DAG-TISEKEL-TOM                                       
013771     END-IF                                                               
013772                                                                          
013773     CALL WDAGKONV USING                                                  
013774          DAG-KDCALL,                                                     
013775          DAG-DATUM-AREA,                                                 
013776          DAG-KDSVAR                                                      
013777                                                                          
013778     IF DAG-KDSVAR = SPACE                                                
013779        MOVE DAG-TIAAMMDD-FOM TO WS-TIORDREG-RENS                         
013780     ELSE                                                                 
013781        MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                            
013782        MOVE '*** FEL FRÅN DAGKONV ***' TO FELTEXT-STR                    
013783        PERFORM S99-ABEND                                                 
013784     END-IF                                                               
013785     .                                                                    
013786     EJECT                                                                
014010 B-KOLLA-OM-RENSNING SECTION.                                             
014020                                                                          
014030     MOVE ODEL-IDDISTR  TO W-IDDISTR                                      
014031                           WS-IDDISTR                                     
014040     MOVE ODEL-IDKUNDNR TO W-IDKUNDNR                                     
014041                           WS-IDKUNDNR                                    
014050     MOVE ODEL-IDORDNR7 TO W-IDORDNR5                                     
014051                           WS-IDORDNR7                                    
014060     MOVE ODEL-IDPRODNR TO W-IDPRODNR                                     
014070     MOVE ODEL-IDPLKLST TO W-IDPLKLST                                     
014080                                                                          
014090     PERFORM IMS-GET-WDE4-KORD                                            
014091     PERFORM UNTIL SEGMENT-SAKNAS                                         
014092       PERFORM IMS-GET-WDE4-ORAD                                          
014093       IF    SEGMENT-FINNS                                                
014094         IF ORAD-KVBEART NOT = ORAD-KVLEVART AND                          
014095            ORAD-FLDIRLEV = JA               AND                          
014096            ORAD-IDSYSTEM = 'VDI '           AND                          
014097            ORAD-KDRADSTA < +4               AND                          
014098            ORAD-IDBIL    > SPACE                                         
014099            MOVE ODEL-TIREGDAT         TO TMP1-YYMMDD                     
014100            MOVE WS-TIORDREG-RENS      TO TMP2-YYMMDD                     
014101            PERFORM WY2000P1                                              
014102            IF TMP1-YYMMDD <= TMP2-YYMMDD                                 
014103              MOVE 'REN'               TO VG3-IDPTYP                      
014104              MOVE WS-IDDISTR (2:4)    TO VG3-IDDISTR                     
014105              MOVE WS-IDKUNDNR(2:6)    TO VG3-IDKUNDNR                    
014106              MOVE WS-IDORDNR7         TO VG3-IDORDNR7                    
014107              MOVE KORD-IDPRODNR       TO VG3-IDPRODNR                    
014108              MOVE ORAD-IDPURAD        TO VG3-IDRADNR                     
014109                                                                          
014110              MOVE 'VO '               TO CIA-IDARTPRE-IN                 
014111              MOVE ORAD-IDARTNR        TO CIA-IDARTBET-IN                 
014112                                          WS-IDARTNR                      
014113              CALL W009CIA USING CIA-W009CIA                              
014114              IF CIA-KDSVAR   NOT   =  'F'                                
014115                 MOVE CIA-IDARTPRE-UT  TO VG3-IDARTPRE                    
014116                 MOVE CIA-IDARTBET-UT  TO VG3-IDARTBET                    
014117              ELSE                                                        
014118                MOVE ORAD-IDARTNR      TO WS-IDARTNR                      
014119               STRING 'ARTIKEL ' WS-IDARTNR ' FUNKAR EJ I W009CIA'        
014120                DELIMITED BY SIZE INTO FELTEXT-STR                        
014121                MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                    
014122                PERFORM S99-ABEND                                         
014123              END-IF                                                      
014124                                                                          
014125              MOVE '83'                TO VG3-KDORDBEK                    
014161              PERFORM S11-SKRIV-W46379                                    
014190            END-IF                                                        
014192         END-IF                                                           
014193       END-IF                                                             
014194     END-PERFORM                                                          
014195     .                                                                    
014196     EJECT                                                                
014225 Z-FINIT SECTION.                                                         
014230     CLOSE W46379                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014601     EJECT                                                                
014810 S11-SKRIV-W46379 SECTION.                                                
014820                                                                          
014830     WRITE VG3-POST FROM VG3-AREA                                         
014840                                                                          
014850     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
014860     MOVE 'W46379' TO POSTSUM-FDNAMN                                      
014870     MOVE 'W46379D1' TO POSTSUM-DDNAMN2                                   
014880     CALL POSTSUM USING POSTSUM-PARM                                      
014890     .                                                                    
014891     EJECT                                                                
014900 S99-ABEND SECTION.                                                       
015000                                                                          
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SEKTIONER ---                                                   
015600     SKIP3                                                                
015701     EJECT                                                                
015702 IMS-GET-WDQ3   SECTION.                                                  
015703                                                                          
015704     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-WDQ3                           
015705     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
015706     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015707     PERFORM IMS-STATUSKONTROLL                                           
015710     .                                                                    
015720     EJECT                                                                
015730 IMS-GET-WDE4-KORD SECTION.                                               
015740                                                                          
015750     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
015760          DELIMITED BY SIZE INTO SSA1                                     
015770     MOVE '  GE' TO GODK-STATUSKODER                                      
015780     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
015790     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
015791     PERFORM IMS-STATUSKONTROLL                                           
015792     .                                                                    
015793     EJECT                                                                
015794 IMS-GET-WDE4-ORAD SECTION.                                               
015795                                                                          
015796     MOVE 'WDE411' TO SSA1                                                
015798     MOVE '  GE' TO GODK-STATUSKODER                                      
015799     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
015800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
015801     PERFORM IMS-STATUSKONTROLL                                           
015802     .                                                                    
015820     EJECT                                                                
015900 IMS-STATUSKONTROLL SECTION.                                              
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GODK-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO FELTEXT                                 
016600         DISPLAY FELTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
017110     EJECT                                                                
017300*    -COPY WY2000P1                                                       

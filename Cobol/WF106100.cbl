000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BMP-DB2-PGM               
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF106100.                                                
000600 AUTHOR.         ANDERS HENRIKSSON                                        
000700 DATE-WRITTEN.   JUN 2006.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*   PGM                                                                   
001100*   -SELECTS ROWS FROM YESTERDAYS INVOICING READING                       
001200*      T01DHEA                                                            
001300*      T01DLIN                                                            
001400*      T01CURR                                                            
001410*      T01PDEV                                                            
001500                                                                          
001600*   -INSERT SELECTED ROWS IN                                              
001700*      T01SDEV                                                            
002200*                                                                         
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000 FILE-CONTROL.                                                            
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300                                                                          
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                     PIC X(8)    VALUE 'WF106100'.              
003600                                                                          
003700*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01CURR                       
003710 77  YES                         PIC X       VALUE 'Y'.                   
003720 77  NOO                         PIC X       VALUE 'N'.                   
003730                                                                          
003900 01  WS-CURRENT-DATE           PIC X(8)  VALUE SPACE.                     
004000 01  WS-KDVALISO               PIC X(3)  VALUE SPACE.                     
004100 01  WS-RUNDATUM               PIC X(8)  VALUE SPACE.                     
004200 01  WS-DASTADAT               PIC X(8)  VALUE SPACE.                     
004300 77  WS-IX                     PIC S9(9) VALUE +0    COMP SYNC.           
004400 77  WS-VALUE-MIN              PIC S9(11)V9(5) COMP-3 VALUE ZERO.         
004500 77  WS-VALUE-MAX              PIC S9(11)V9(5) COMP-3 VALUE ZERO.         
004600 77  WS-REARTRAB               PIC S9(3)       COMP-3 VALUE 75.           
004700                                                                          
004800 77  BUILD-PAY-SW                PIC X       VALUE SPACE.                 
004900     88  BUILD-PAY-JA                        VALUE 'Y'.                   
005000     88  BUILD-PAY-NEJ                       VALUE 'N'.                   
005100     EJECT                                                                
005200                                                                          
005300 77  BUILD-DEL-SW                PIC X       VALUE SPACE.                 
005400     88  BUILD-DEL-JA                        VALUE 'Y'.                   
005500     88  BUILD-DEL-NEJ                       VALUE 'N'.                   
005510     EJECT                                                                
005520                                                                          
005530 77  BUILD-DIS-SW                PIC X       VALUE SPACE.                 
005540     88  BUILD-DIS-JA                        VALUE 'Y'.                   
005550     88  BUILD-DIS-NEJ                       VALUE 'N'.                   
005560     EJECT                                                                
005570                                                                          
005580 77  BUILD-NTO-SW                PIC X       VALUE SPACE.                 
005590     88  BUILD-NTO-JA                        VALUE 'Y'.                   
005591     88  BUILD-NTO-NEJ                       VALUE 'N'.                   
005592     EJECT                                                                
005593                                                                          
005594 77  BUILD-NTOH-SW               PIC X       VALUE SPACE.                 
005595     88  BUILD-NTOH-JA                       VALUE 'Y'.                   
005596     88  BUILD-NTOH-NEJ                      VALUE 'N'.                   
005597     EJECT                                                                
005598                                                                          
005599 77  BUILD-SUN-SW                PIC X       VALUE SPACE.                 
005600     88  BUILD-SUN-JA                        VALUE 'Y'.                   
005601     88  BUILD-SUN-NEJ                       VALUE 'N'.                   
005602     EJECT                                                                
005603                                                                          
005604 77  BUILD-SUNH-SW               PIC X       VALUE SPACE.                 
005605     88  BUILD-SUNH-JA                       VALUE 'Y'.                   
005606     88  BUILD-SUNH-NEJ                      VALUE 'N'.                   
005607     EJECT                                                                
005608                                                                          
005610 01  ERRTEXT.                                                             
005700     03  FILLER                PIC X(8)    VALUE 'ERRTEXT'.               
005800     03  ERRTEXT-STR           PIC X(72)   VALUE SPACE.                   
005900 01  KDRC-DISPLAY              PIC Z(5).                                  
006000     EJECT                                                                
006100                                                                          
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300     03  ABEND                 PIC X(8)    VALUE 'ABEND   '.              
006500     EJECT                                                                
006600                                                                          
006700 01  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.              
006800     SKIP3                                                                
006900*    -COPY WZ20DAYS                                                       
007000     EJECT                                                                
007100                                                                          
007200*    --- PARAMETRAR TILL ABEND                                            
007300*                                                                         
007400 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
007500 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
007600     EJECT                                                                
007700                                                                          
009300*        WORK-AREAS FOR DB2-SECTIONS                                      
009400*                                                                         
009500 01  FILLER                    PIC X(16)   VALUE 'DB2-LSEL   '.           
009600*01  -COPY T01LSEL        -PRE LSEL-                                      
009700     EJECT                                                                
009800                                                                          
009810 01  FILLER                    PIC X(16)   VALUE 'DB2-DHEA   '.           
009820*01  -COPY T01DHEA        -PRE DHEA-                                      
009830     EJECT                                                                
009840                                                                          
009900 01  FILLER                    PIC X(16)   VALUE 'DB2-DLIN   '.           
010000*01  -COPY T01DLIN        -PRE DLIN-                                      
010100     EJECT                                                                
010200                                                                          
010300 01  FILLER                    PIC X(16)   VALUE 'DB2-CURR   '.           
010400*01  -COPY T01CURR        -PRE CURR-                                      
010500     EJECT                                                                
010600                                                                          
010610 01  FILLER                    PIC X(16)   VALUE 'DB2-PDEV   '.           
010620*01  -COPY T01PDEV        -PRE PDEV-                                      
010630     EJECT                                                                
010640                                                                          
010650 01  FILLER                    PIC X(16)   VALUE 'DB2-SDEV   '.           
010660*01  -COPY T01SDEV        -PRE SDEV-                                      
010670     EJECT                                                                
010680                                                                          
010700 01  FILLER                    PIC X(16)   VALUE 'LSEL-AREA'.             
010800       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
010900                                                                          
010910 01  FILLER                    PIC X(16)   VALUE 'DHEA-AREA'.             
010920       EXEC SQL INCLUDE T01DHEA END-EXEC.                                 
010930                                                                          
011000 01  FILLER                    PIC X(16)   VALUE 'DLIN-AREA'.             
011100       EXEC SQL INCLUDE T01DLIN END-EXEC.                                 
011200                                                                          
011300 01  FILLER                    PIC X(16)   VALUE 'CURR-AREA'.             
011400       EXEC SQL INCLUDE T01CURR END-EXEC.                                 
011500                                                                          
011510 01  FILLER                    PIC X(16)   VALUE 'PDEV-AREA'.             
011520       EXEC SQL INCLUDE T01PDEV END-EXEC.                                 
011530                                                                          
011540 01  FILLER                    PIC X(16)   VALUE 'SDEV-AREA'.             
011550       EXEC SQL INCLUDE T01SDEV END-EXEC.                                 
011560                                                                          
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
011900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012000*                        **** STATUS-CODE FROM DB2                        
012100                                                                          
012200 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
012300 01  DB2-WS.                                                              
012400   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
012610     88  CURSOR-OK                         VALUE 000.                     
012620     88  LINES-FOUND                       VALUE 000.                     
012630     88  LINES-MISSING                     VALUE 100.                     
012640     88  RESOURCE-WRONG                    VALUE 904.                     
012650                                                                          
012700   03  GOOD-SQLCODES.                                                     
012800     05  GOOD-SQLCODE OCCURS 5                                            
012900         INDEXED BY SQLCODE-IX PIC 999.                                   
013000     EJECT                                                                
013100                                                                          
013200 LINKAGE SECTION.                                                         
013300*01  -COPY W0009   -PRE MSG-                                              
013400     EJECT                                                                
013500                                                                          
013600 PROCEDURE DIVISION  USING MSG-PCB.                                       
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING MSG-PCB.                                       
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM B-EXECUTE                                                    
014300                                                                          
014400     PERFORM Z-FINISH                                                     
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 A-INIT SECTION.                                                          
015100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
015200     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
015300     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
015400     MOVE 1                           TO DAYS-KVDAYS                      
015500     MOVE ' '                         TO DAYS-IDCALEND                    
015600     MOVE SPACE                       TO DAYS-TIDATE1                     
015700     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
015800     CALL WZ20DAYS USING                                                  
015900          DAYS-WZ20DAYS                                                   
016000     IF DAYS-KDRC = ZERO                                                  
016100       MOVE DAYS-TIDATE1              TO WS-RUNDATUM                      
016200     END-IF                                                               
016300     .                                                                    
016400     EJECT                                                                
016500                                                                          
016600 B-EXECUTE SECTION.                                                       
016610     MOVE ZERO TO WS-IX                                                   
016700     PERFORM DB2-OPEN-T01PDEV                                             
016800     PERFORM DB2-FETCH-T01PDEV                                            
016900     PERFORM UNTIL LINES-MISSING                                          
017910**** PAYMENT TERMS MISSING CHECK                                          
017920       IF PDEV-FLPAYTE = 'J'                                              
018000         PERFORM DB2-OPEN-CRS-DHEA-PAY                                    
018100         PERFORM DB2-FETCH-CRS-DHEA-PAY                                   
018200         PERFORM UNTIL LINES-MISSING                                      
018400           PERFORM BA-BUILD-OUTPUT-DHEA-PAY                               
018600           PERFORM DB2-FETCH-CRS-DHEA-PAY                                 
018700         END-PERFORM                                                      
018800         PERFORM DB2-CLOSE-CRS-DHEA-PAY                                   
018810       END-IF                                                             
018900                                                                          
018910**** DELIVERY TERMS MISSING CHECK                                         
018920       IF PDEV-FLDELTE = 'J'                                              
019000         PERFORM DB2-OPEN-CRS-DLIN-DEL                                    
019100         PERFORM DB2-FETCH-CRS-DLIN-DEL                                   
019200         PERFORM UNTIL LINES-MISSING                                      
019400           PERFORM BA-BUILD-OUTPUT-DLIN-DEL                               
019600           PERFORM DB2-FETCH-CRS-DLIN-DEL                                 
019700         END-PERFORM                                                      
019800         PERFORM DB2-CLOSE-CRS-DLIN-DEL                                   
019810       END-IF                                                             
019900                                                                          
019910**** DISCOUNT TO HIGH CHECK                                               
019920       IF PDEV-REARTRAB = ZERO                                            
019930         CONTINUE                                                         
019940       ELSE                                                               
020000         PERFORM DB2-OPEN-CRS-DLIN-DIS                                    
020100         PERFORM DB2-FETCH-CRS-DLIN-DIS                                   
020200         PERFORM UNTIL LINES-MISSING                                      
020400           PERFORM BA-BUILD-OUTPUT-DLIN-DIS                               
020600           PERFORM DB2-FETCH-CRS-DLIN-DIS                                 
020700         END-PERFORM                                                      
020800         PERFORM DB2-CLOSE-CRS-DLIN-DIS                                   
020900       END-IF                                                             
021100                                                                          
021101**** NET PRICE TO LOW CHECK                                               
021110       PERFORM DB2-SELECT-MAX-T01CURR                                     
021200       PERFORM DB2-OPEN-CRS-CURR-DLIN                                     
021300       PERFORM DB2-FETCH-CRS-CURR-DLIN                                    
021400       PERFORM UNTIL LINES-MISSING                                        
021410         IF PDEV-PRARTNTO-MIN = ZERO                                      
021420           CONTINUE                                                       
021430         ELSE                                                             
021500           COMPUTE WS-VALUE-MIN ROUNDED =                                 
021600                   PDEV-PRARTNTO-MIN /                                    
021700                  (CURR-PRKURS / CURR-REVALUTA)                           
022100           PERFORM DB2-OPEN-CRS-DLIN-PRARTNTO-1                           
022200           PERFORM DB2-FETCH-CRS-DLIN-PRARTNTO-1                          
022300           PERFORM UNTIL LINES-MISSING                                    
022500             PERFORM BA-BUILD-OUTPUT-DLIN-PRARTNTO                        
022700             PERFORM DB2-FETCH-CRS-DLIN-PRARTNTO-1                        
022800           END-PERFORM                                                    
022900           PERFORM DB2-CLOSE-CRS-DLIN-PRARTNTO-1                          
022910         END-IF                                                           
023000                                                                          
023001**** NET PRICE TO HIGH CHECK                                              
023002         IF PDEV-PRARTNTO-MAX = ZERO                                      
023003           CONTINUE                                                       
023004         ELSE                                                             
023005           COMPUTE WS-VALUE-MAX ROUNDED =                                 
023006                   PDEV-PRARTNTO-MAX /                                    
023007                  (CURR-PRKURS / CURR-REVALUTA)                           
023010           PERFORM DB2-OPEN-CRS-DLIN-PRARTNTO-2                           
023020           PERFORM DB2-FETCH-CRS-DLIN-PRARTNTO-2                          
023030           PERFORM UNTIL LINES-MISSING                                    
023050             PERFORM BB-BUILD-OUTPUT-DLIN-PRARTNTO                        
023070             PERFORM DB2-FETCH-CRS-DLIN-PRARTNTO-2                        
023080           END-PERFORM                                                    
023090           PERFORM DB2-CLOSE-CRS-DLIN-PRARTNTO-2                          
023091                                                                          
023100           PERFORM DB2-FETCH-CRS-CURR-DLIN                                
023110         END-IF                                                           
023200       END-PERFORM                                                        
023300       PERFORM DB2-CLOSE-CRS-CURR-DLIN                                    
023400                                                                          
023410**** NET VALUE TO LOW CHECK                                               
023500       PERFORM DB2-OPEN-CRS-CURR-DLIN                                     
023600       PERFORM DB2-FETCH-CRS-CURR-DLIN                                    
023700       PERFORM UNTIL LINES-MISSING                                        
023710         IF PDEV-SUNTO-MIN = ZERO                                         
023720           CONTINUE                                                       
023730         ELSE                                                             
023800           COMPUTE WS-VALUE-MIN ROUNDED =                                 
023900                   PDEV-SUNTO-MIN /                                       
024000                  (CURR-PRKURS / CURR-REVALUTA)                           
024400           PERFORM DB2-OPEN-CRS-DLIN-SUNTO-1                              
024500           PERFORM DB2-FETCH-CRS-DLIN-SUNTO-1                             
024600           PERFORM UNTIL LINES-MISSING                                    
024800             PERFORM BA-BUILD-OUTPUT-DLIN-SUNTO                           
025000             PERFORM DB2-FETCH-CRS-DLIN-SUNTO-1                           
025100           END-PERFORM                                                    
025200           PERFORM DB2-CLOSE-CRS-DLIN-SUNTO-1                             
025210         END-IF                                                           
025300                                                                          
025301**** NET VALUE TO HIGH CHECK                                              
025302         IF PDEV-SUNTO-MAX = ZERO                                         
025303           CONTINUE                                                       
025304         ELSE                                                             
025305           COMPUTE WS-VALUE-MAX ROUNDED =                                 
025306                   PDEV-SUNTO-MAX /                                       
025307                  (CURR-PRKURS / CURR-REVALUTA)                           
025310           PERFORM DB2-OPEN-CRS-DLIN-SUNTO-2                              
025320           PERFORM DB2-FETCH-CRS-DLIN-SUNTO-2                             
025330           PERFORM UNTIL LINES-MISSING                                    
025350             PERFORM BB-BUILD-OUTPUT-DLIN-SUNTO                           
025370             PERFORM DB2-FETCH-CRS-DLIN-SUNTO-2                           
025380           END-PERFORM                                                    
025390           PERFORM DB2-CLOSE-CRS-DLIN-SUNTO-2                             
025391         END-IF                                                           
025392                                                                          
025400         PERFORM DB2-FETCH-CRS-CURR-DLIN                                  
025500       END-PERFORM                                                        
025600       PERFORM DB2-CLOSE-CRS-CURR-DLIN                                    
025601                                                                          
025610       PERFORM DB2-FETCH-T01PDEV                                          
025700     END-PERFORM                                                          
025800     PERFORM DB2-CLOSE-T01PDEV                                            
025810                                                                          
025820**** SOFTWARE CHINA                                                       
026110     PERFORM DB2-OPEN-CRS-DLIN-SOFT                                       
026120     PERFORM DB2-FETCH-CRS-DLIN-SOFT                                      
026130     PERFORM UNTIL LINES-MISSING                                          
026140       PERFORM BC-BUILD-OUTPUT-DLIN-SOFT                                  
026150       PERFORM DB2-FETCH-CRS-DLIN-SOFT                                    
026160     END-PERFORM                                                          
026170     PERFORM DB2-CLOSE-CRS-DLIN-SOFT                                      
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700 BA-BUILD-OUTPUT-DHEA-PAY      SECTION.                                   
026701     MOVE YES TO BUILD-PAY-SW                                             
026702     IF PDEV-FLSOFT = 'J'                                                 
026703       MOVE YES TO BUILD-PAY-SW                                           
027651     ELSE                                                                 
027652       IF DHEA-FLSOFT = 'J'                                               
027653         MOVE NOO TO BUILD-PAY-SW                                         
027654       ELSE                                                               
027655         MOVE YES TO BUILD-PAY-SW                                         
027657       END-IF                                                             
027660     END-IF                                                               
027670                                                                          
027671     IF BUILD-PAY-JA                                                      
027672       IF PDEV-FLFREE = 'J'                                               
027673         MOVE YES TO BUILD-PAY-SW                                         
027674       ELSE                                                               
027675         IF DHEA-FLFREE = 'J'                                             
027676           MOVE NOO TO BUILD-PAY-SW                                       
027677         ELSE                                                             
027678           MOVE YES TO BUILD-PAY-SW                                       
027679         END-IF                                                           
027680       END-IF                                                             
027681     END-IF                                                               
027682                                                                          
027683     IF BUILD-PAY-JA                                                      
027684       IF PDEV-FLINVOIC = 'J'                                             
027685         IF DHEA-KDFINDOC = 'INV'                                         
027686           MOVE YES TO BUILD-PAY-SW                                       
027687         ELSE                                                             
027688           MOVE NOO TO BUILD-PAY-SW                                       
027689         END-IF                                                           
027690       ELSE                                                               
027691         MOVE YES TO BUILD-PAY-SW                                         
027693       END-IF                                                             
027694     END-IF                                                               
027695                                                                          
027696     IF BUILD-PAY-JA                                                      
027697       PERFORM BAA-BUILD-OUTPUT-DHEA-PAY                                  
027698     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
027910 BAA-BUILD-OUTPUT-DHEA-PAY     SECTION.                                   
027911     ADD +1 TO WS-IX                                                      
027912     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
027913     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
027914     MOVE WS-IX                 TO SDEV-IDLOPNR                           
027915     MOVE 'PAYMENT TERMS MISS'  TO SDEV-BETEXT                            
027916     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
027917     MOVE SPACE                 TO SDEV-IDARTNR-FINANCE                   
027918     MOVE SPACE                 TO SDEV-BEART                             
027919     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
027920     MOVE SPACE                 TO SDEV-IDEXCUST-1                        
027921     MOVE SPACE                 TO SDEV-IDEXCUST-2                        
027922     MOVE SPACE                 TO SDEV-IDREF                             
027923     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
027924     MOVE DHEA-FLSOFT           TO SDEV-FLSOFT                            
027925     MOVE DHEA-FLFREE           TO SDEV-FLFREE                            
027926     MOVE ZERO                  TO SDEV-REARTRAB                          
027927     MOVE ZERO                  TO SDEV-PRARTNTO                          
027928     MOVE ZERO                  TO SDEV-SUNTO                             
027929     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
027930     MOVE ZERO                  TO SDEV-PRARTNTO-SEK                      
027931     MOVE ZERO                  TO SDEV-SUNTO-SEK                         
027932     MOVE ZERO                  TO SDEV-KVLEVART                          
027933     MOVE ZERO                  TO SDEV-IDSTATNR                          
027934     MOVE SPACE                 TO SDEV-IDLEVNR                           
027935     MOVE SPACE                 TO SDEV-DAFINDOC                          
027936                                                                          
027937     PERFORM DB2-INSERT-T01SDEV                                           
027938     .                                                                    
027940     EJECT                                                                
027950                                                                          
028000 BA-BUILD-OUTPUT-DLIN-DEL      SECTION.                                   
028001     MOVE YES TO BUILD-DEL-SW                                             
028002     IF PDEV-FLSOFT = 'J'                                                 
028003       MOVE YES TO BUILD-DEL-SW                                           
028004     ELSE                                                                 
028005       IF DHEA-FLSOFT = 'J'                                               
028006         MOVE NOO TO BUILD-DEL-SW                                         
028007       ELSE                                                               
028008         MOVE YES TO BUILD-DEL-SW                                         
028009       END-IF                                                             
028010     END-IF                                                               
028011                                                                          
028012     IF BUILD-DEL-JA                                                      
028013       IF PDEV-FLFREE = 'J'                                               
028014         MOVE YES TO BUILD-DEL-SW                                         
028015       ELSE                                                               
028016         IF DHEA-FLFREE = 'J'                                             
028017           MOVE NOO TO BUILD-DEL-SW                                       
028018         ELSE                                                             
028019           MOVE YES TO BUILD-DEL-SW                                       
028020         END-IF                                                           
028021       END-IF                                                             
028022     END-IF                                                               
028023                                                                          
028024     IF BUILD-DEL-JA                                                      
028025       IF PDEV-FLINVOIC = 'J'                                             
028026         IF DHEA-KDFINDOC = 'INV'                                         
028027           MOVE YES TO BUILD-DEL-SW                                       
028028         ELSE                                                             
028029           MOVE NOO TO BUILD-DEL-SW                                       
028030         END-IF                                                           
028031       ELSE                                                               
028032         MOVE YES TO BUILD-DEL-SW                                         
028033       END-IF                                                             
028034     END-IF                                                               
028035                                                                          
028036     IF BUILD-DEL-JA                                                      
028037       PERFORM BAA-BUILD-OUTPUT-DLIN-DEL                                  
028038     END-IF                                                               
028040     .                                                                    
028100     EJECT                                                                
028200                                                                          
028300 BAA-BUILD-OUTPUT-DLIN-DEL      SECTION.                                  
028400     ADD +1 TO WS-IX                                                      
028910     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
028920     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
028930     MOVE WS-IX                 TO SDEV-IDLOPNR                           
028940     MOVE 'DELIVERY TERMS MISS' TO SDEV-BETEXT                            
028950     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
028960     MOVE SPACE                 TO SDEV-IDARTNR-FINANCE                   
028970     MOVE SPACE                 TO SDEV-BEART                             
028980     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
028990     MOVE SPACE                 TO SDEV-IDEXCUST-1                        
028991     MOVE SPACE                 TO SDEV-IDEXCUST-2                        
028992     MOVE SPACE                 TO SDEV-IDREF                             
028993     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
028994     MOVE DHEA-FLSOFT           TO SDEV-FLSOFT                            
028995     MOVE DHEA-FLFREE           TO SDEV-FLFREE                            
028996     MOVE ZERO                  TO SDEV-REARTRAB                          
028997     MOVE ZERO                  TO SDEV-PRARTNTO                          
028998     MOVE ZERO                  TO SDEV-SUNTO                             
028999     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
029000     MOVE ZERO                  TO SDEV-PRARTNTO-SEK                      
029001     MOVE ZERO                  TO SDEV-SUNTO-SEK                         
029002     MOVE ZERO                  TO SDEV-KVLEVART                          
029003     MOVE ZERO                  TO SDEV-IDSTATNR                          
029004     MOVE SPACE                 TO SDEV-IDLEVNR                           
029005     MOVE SPACE                 TO SDEV-DAFINDOC                          
029006                                                                          
029007     PERFORM DB2-INSERT-T01SDEV                                           
029010     .                                                                    
029100     EJECT                                                                
029200                                                                          
029300 BA-BUILD-OUTPUT-DLIN-DIS      SECTION.                                   
029301     MOVE YES TO BUILD-DIS-SW                                             
029310     IF PDEV-FLSOFT = 'J'                                                 
029320       MOVE YES TO BUILD-DIS-SW                                           
029330     ELSE                                                                 
029340       IF DLIN-FLSOFT = 'J'                                               
029350         MOVE NOO TO BUILD-DIS-SW                                         
029360       ELSE                                                               
029370         MOVE YES TO BUILD-DIS-SW                                         
029380       END-IF                                                             
029390     END-IF                                                               
029391                                                                          
029392     IF BUILD-DIS-JA                                                      
029393       IF PDEV-FLFREE = 'J'                                               
029394         MOVE YES TO BUILD-DIS-SW                                         
029395       ELSE                                                               
029396         IF DLIN-FLFREE = 'J'                                             
029397           MOVE NOO TO BUILD-DIS-SW                                       
029398         ELSE                                                             
029399           MOVE YES TO BUILD-DIS-SW                                       
029400         END-IF                                                           
029401       END-IF                                                             
029402     END-IF                                                               
029403                                                                          
029404     IF BUILD-DIS-JA                                                      
029405       IF PDEV-FLINVOIC = 'J'                                             
029406         IF DHEA-KDFINDOC = 'INV'                                         
029407           MOVE YES TO BUILD-DIS-SW                                       
029408         ELSE                                                             
029409           MOVE NOO TO BUILD-DIS-SW                                       
029410         END-IF                                                           
029411       ELSE                                                               
029412         MOVE YES TO BUILD-DIS-SW                                         
029413       END-IF                                                             
029414     END-IF                                                               
029415                                                                          
029416     IF BUILD-DIS-JA                                                      
029417       IF PDEV-FLSERV = 'J'                                               
029419         MOVE YES TO BUILD-DIS-SW                                         
029423       ELSE                                                               
029424         IF DLIN-BEART = SPACE                                            
029425           MOVE NOO TO BUILD-DIS-SW                                       
029426         ELSE                                                             
029427           MOVE YES TO BUILD-DIS-SW                                       
029428         END-IF                                                           
029429       END-IF                                                             
029430     END-IF                                                               
029431                                                                          
029432     IF BUILD-DIS-JA                                                      
029433       PERFORM BAA-BUILD-OUTPUT-DLIN-DIS                                  
029434     END-IF                                                               
029440     .                                                                    
029500     EJECT                                                                
029600                                                                          
030100 BAA-BUILD-OUTPUT-DLIN-DIS      SECTION.                                  
030200     ADD +1 TO WS-IX                                                      
030210     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
030220     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
030230     MOVE WS-IX                 TO SDEV-IDLOPNR                           
030240     MOVE 'DISCOUNT IS HIGH'    TO SDEV-BETEXT                            
030250     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
030260     MOVE DLIN-IDARTNR-FINANCE  TO SDEV-IDARTNR-FINANCE                   
030270     MOVE DLIN-BEART            TO SDEV-BEART                             
030280     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
030290     MOVE DLIN-IDEXCUST-1       TO SDEV-IDEXCUST-1                        
030291     MOVE DLIN-IDEXCUST-2       TO SDEV-IDEXCUST-2                        
030292     MOVE DLIN-IDREF            TO SDEV-IDREF                             
030293     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
030294     MOVE DLIN-FLSOFT           TO SDEV-FLSOFT                            
030295     MOVE DLIN-FLFREE           TO SDEV-FLFREE                            
030296     MOVE DLIN-REARTRAB         TO SDEV-REARTRAB                          
030297     MOVE DLIN-PRARTNTO         TO SDEV-PRARTNTO                          
030298     MOVE DLIN-SUNTO            TO SDEV-SUNTO                             
030299     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
030300     MOVE ZERO                  TO SDEV-PRARTNTO-SEK                      
030301     MOVE ZERO                  TO SDEV-SUNTO-SEK                         
030302     MOVE ZERO                  TO SDEV-KVLEVART                          
030303     MOVE ZERO                  TO SDEV-IDSTATNR                          
030304     MOVE SPACE                 TO SDEV-IDLEVNR                           
030305     MOVE SPACE                 TO SDEV-DAFINDOC                          
030306                                                                          
030307     PERFORM DB2-INSERT-T01SDEV                                           
030308     .                                                                    
030400     EJECT                                                                
030500                                                                          
030600 BA-BUILD-OUTPUT-DLIN-PRARTNTO SECTION.                                   
030601     MOVE YES TO BUILD-NTO-SW                                             
030610     IF PDEV-FLSOFT = 'J'                                                 
030620       MOVE YES TO BUILD-NTO-SW                                           
030630     ELSE                                                                 
030640       IF DLIN-FLSOFT = 'J'                                               
030650         MOVE NOO TO BUILD-NTO-SW                                         
030660       ELSE                                                               
030670         MOVE YES TO BUILD-NTO-SW                                         
030680       END-IF                                                             
030690     END-IF                                                               
030691                                                                          
030692     IF BUILD-NTO-JA                                                      
030693       IF PDEV-FLFREE = 'J'                                               
030694         MOVE YES TO BUILD-NTO-SW                                         
030695       ELSE                                                               
030696         IF DLIN-FLFREE = 'J'                                             
030697           MOVE NOO TO BUILD-NTO-SW                                       
030698         ELSE                                                             
030699           MOVE YES TO BUILD-NTO-SW                                       
030700         END-IF                                                           
030701       END-IF                                                             
030702     END-IF                                                               
030703                                                                          
030704     IF BUILD-NTO-JA                                                      
030705       IF PDEV-FLINVOIC = 'J'                                             
030706         IF DHEA-KDFINDOC = 'INV'                                         
030707           MOVE YES TO BUILD-NTO-SW                                       
030708         ELSE                                                             
030709           MOVE NOO TO BUILD-NTO-SW                                       
030710         END-IF                                                           
030711       ELSE                                                               
030712         MOVE YES TO BUILD-NTO-SW                                         
030713       END-IF                                                             
030714     END-IF                                                               
030715                                                                          
030716     IF BUILD-NTO-JA                                                      
030717       IF PDEV-FLSERV = 'J'                                               
030718         MOVE YES TO BUILD-NTO-SW                                         
030719       ELSE                                                               
030720         IF DLIN-BEART = SPACE                                            
030721           MOVE NOO TO BUILD-NTO-SW                                       
030722         ELSE                                                             
030723           MOVE YES TO BUILD-NTO-SW                                       
030724         END-IF                                                           
030725       END-IF                                                             
030726     END-IF                                                               
030727                                                                          
030728     IF BUILD-NTO-JA                                                      
030729       PERFORM BAA-BUILD-OUTPUT-DLIN-PRARTNTO                             
030731     END-IF                                                               
030740     .                                                                    
030800     EJECT                                                                
030900                                                                          
031200 BAA-BUILD-OUTPUT-DLIN-PRARTNTO SECTION.                                  
031300     ADD +1 TO WS-IX                                                      
031510     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
031520     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
031530     MOVE WS-IX                 TO SDEV-IDLOPNR                           
031540     MOVE 'NET PRICE IS LOW'    TO SDEV-BETEXT                            
031550     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
031560     MOVE DLIN-IDARTNR-FINANCE  TO SDEV-IDARTNR-FINANCE                   
031570     MOVE DLIN-BEART            TO SDEV-BEART                             
031580     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
031590     MOVE DLIN-IDEXCUST-1       TO SDEV-IDEXCUST-1                        
031594     MOVE DLIN-IDEXCUST-2       TO SDEV-IDEXCUST-2                        
031595     MOVE DLIN-IDREF            TO SDEV-IDREF                             
031596     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
031597     MOVE DLIN-FLSOFT           TO SDEV-FLSOFT                            
031598     MOVE DLIN-FLFREE           TO SDEV-FLFREE                            
031599     MOVE DLIN-REARTRAB         TO SDEV-REARTRAB                          
031600     MOVE DLIN-PRARTNTO         TO SDEV-PRARTNTO                          
031601     MOVE DLIN-SUNTO            TO SDEV-SUNTO                             
031602     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
031603     COMPUTE SDEV-PRARTNTO-SEK ROUNDED = DLIN-PRARTNTO *                  
031604                                (CURR-PRKURS / CURR-REVALUTA)             
031605     COMPUTE SDEV-SUNTO-SEK ROUNDED    = DLIN-SUNTO *                     
031606                                (CURR-PRKURS / CURR-REVALUTA)             
031607     MOVE ZERO                  TO SDEV-KVLEVART                          
031608     MOVE ZERO                  TO SDEV-IDSTATNR                          
031609     MOVE SPACE                 TO SDEV-IDLEVNR                           
031610     MOVE SPACE                 TO SDEV-DAFINDOC                          
031611                                                                          
031612     PERFORM DB2-INSERT-T01SDEV                                           
031620     .                                                                    
031700     EJECT                                                                
031800                                                                          
031801 BB-BUILD-OUTPUT-DLIN-PRARTNTO SECTION.                                   
031802     MOVE YES TO BUILD-NTOH-SW                                            
031803     IF PDEV-FLSOFT = 'J'                                                 
031804       MOVE YES TO BUILD-NTOH-SW                                          
031805     ELSE                                                                 
031806       IF DLIN-FLSOFT = 'J'                                               
031807         MOVE NOO TO BUILD-NTOH-SW                                        
031808       ELSE                                                               
031809         MOVE YES TO BUILD-NTOH-SW                                        
031810       END-IF                                                             
031811     END-IF                                                               
031812                                                                          
031813     IF BUILD-NTOH-JA                                                     
031814       IF PDEV-FLFREE = 'J'                                               
031815         MOVE YES TO BUILD-NTOH-SW                                        
031816       ELSE                                                               
031817         IF DLIN-FLFREE = 'J'                                             
031818           MOVE NOO TO BUILD-NTOH-SW                                      
031819         ELSE                                                             
031820           MOVE YES TO BUILD-NTOH-SW                                      
031821         END-IF                                                           
031822       END-IF                                                             
031823     END-IF                                                               
031824                                                                          
031825     IF BUILD-NTO-JA                                                      
031826       IF PDEV-FLINVOIC = 'J'                                             
031827         IF DHEA-KDFINDOC = 'INV'                                         
031828           MOVE YES TO BUILD-NTOH-SW                                      
031829         ELSE                                                             
031830           MOVE NOO TO BUILD-NTOH-SW                                      
031831         END-IF                                                           
031832       ELSE                                                               
031833         MOVE YES TO BUILD-NTOH-SW                                        
031834       END-IF                                                             
031835     END-IF                                                               
031836                                                                          
031837     IF BUILD-NTOH-JA                                                     
031838       IF PDEV-FLSERV = 'J'                                               
031839         MOVE YES TO BUILD-NTOH-SW                                        
031840       ELSE                                                               
031841         IF DLIN-BEART = SPACE                                            
031842           MOVE NOO TO BUILD-NTOH-SW                                      
031843         ELSE                                                             
031844           MOVE YES TO BUILD-NTOH-SW                                      
031845         END-IF                                                           
031846       END-IF                                                             
031847     END-IF                                                               
031848                                                                          
031849     IF BUILD-NTOH-JA                                                     
031850       PERFORM BBB-BUILD-OUTPUT-DLIN-PRARTNTO                             
031851     END-IF                                                               
031852     .                                                                    
031853     EJECT                                                                
031854                                                                          
031855 BBB-BUILD-OUTPUT-DLIN-PRARTNTO SECTION.                                  
031856     ADD +1 TO WS-IX                                                      
031857     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
031858     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
031859     MOVE WS-IX                 TO SDEV-IDLOPNR                           
031860     MOVE 'NET PRICE IS HIGH'   TO SDEV-BETEXT                            
031861     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
031870     MOVE DLIN-IDARTNR-FINANCE  TO SDEV-IDARTNR-FINANCE                   
031880     MOVE DLIN-BEART            TO SDEV-BEART                             
031890     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
031891     MOVE DLIN-IDEXCUST-1       TO SDEV-IDEXCUST-1                        
031892     MOVE DLIN-IDEXCUST-2       TO SDEV-IDEXCUST-2                        
031893     MOVE DLIN-IDREF            TO SDEV-IDREF                             
031894     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
031895     MOVE DLIN-FLSOFT           TO SDEV-FLSOFT                            
031896     MOVE DLIN-FLFREE           TO SDEV-FLFREE                            
031897     MOVE DLIN-REARTRAB         TO SDEV-REARTRAB                          
031898     MOVE DLIN-PRARTNTO         TO SDEV-PRARTNTO                          
031899     MOVE DLIN-SUNTO            TO SDEV-SUNTO                             
031900     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
031901     COMPUTE SDEV-PRARTNTO-SEK ROUNDED = DLIN-PRARTNTO *                  
031902                                (CURR-PRKURS / CURR-REVALUTA)             
031903     COMPUTE SDEV-SUNTO-SEK  ROUNDED   = DLIN-SUNTO *                     
031904                                (CURR-PRKURS / CURR-REVALUTA)             
031905     MOVE ZERO                  TO SDEV-KVLEVART                          
031906     MOVE ZERO                  TO SDEV-IDSTATNR                          
031907     MOVE SPACE                 TO SDEV-IDLEVNR                           
031908     MOVE SPACE                 TO SDEV-DAFINDOC                          
031909                                                                          
031910     PERFORM DB2-INSERT-T01SDEV                                           
031911     .                                                                    
031912     EJECT                                                                
031913                                                                          
031914 BA-BUILD-OUTPUT-DLIN-SUNTO    SECTION.                                   
031915     MOVE YES TO BUILD-SUN-SW                                             
031916     IF PDEV-FLSOFT = 'J'                                                 
031917       MOVE YES TO BUILD-SUN-SW                                           
031918     ELSE                                                                 
031919       IF DLIN-FLSOFT = 'J'                                               
031920         MOVE NOO TO BUILD-SUN-SW                                         
031921       ELSE                                                               
031922         MOVE YES TO BUILD-SUN-SW                                         
031923       END-IF                                                             
031924     END-IF                                                               
031925                                                                          
031926     IF BUILD-SUN-JA                                                      
031927       IF PDEV-FLFREE = 'J'                                               
031928         MOVE YES TO BUILD-SUN-SW                                         
031929       ELSE                                                               
031930         IF DLIN-FLFREE = 'J'                                             
031931           MOVE NOO TO BUILD-SUN-SW                                       
031932         ELSE                                                             
031933           MOVE YES TO BUILD-SUN-SW                                       
031934         END-IF                                                           
031935       END-IF                                                             
031936     END-IF                                                               
031937                                                                          
031938     IF BUILD-SUN-JA                                                      
031939       IF PDEV-FLINVOIC = 'J'                                             
031940         IF DHEA-KDFINDOC = 'INV'                                         
031941           MOVE YES TO BUILD-SUN-SW                                       
031942         ELSE                                                             
031943           MOVE NOO TO BUILD-SUN-SW                                       
031944         END-IF                                                           
031945       ELSE                                                               
031946         MOVE YES TO BUILD-SUN-SW                                         
031947       END-IF                                                             
031948     END-IF                                                               
031949                                                                          
031950     IF BUILD-SUN-JA                                                      
031951       IF PDEV-FLSERV = 'J'                                               
031952         MOVE YES TO BUILD-SUN-SW                                         
031953       ELSE                                                               
031954         IF DLIN-BEART = SPACE                                            
031955           MOVE NOO TO BUILD-SUN-SW                                       
031956         ELSE                                                             
031957           MOVE YES TO BUILD-SUN-SW                                       
031958         END-IF                                                           
031959       END-IF                                                             
031960     END-IF                                                               
031961                                                                          
031962     IF BUILD-SUN-JA                                                      
031963       PERFORM BAA-BUILD-OUTPUT-DLIN-SUNTO                                
031964     END-IF                                                               
031965     .                                                                    
031966     EJECT                                                                
031967                                                                          
032100 BAA-BUILD-OUTPUT-DLIN-SUNTO    SECTION.                                  
032200     ADD +1 TO WS-IX                                                      
032810     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
032820     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
032830     MOVE WS-IX                 TO SDEV-IDLOPNR                           
032840     MOVE 'NET VALUE IS LOW'    TO SDEV-BETEXT                            
032850     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
032860     MOVE DLIN-IDARTNR-FINANCE  TO SDEV-IDARTNR-FINANCE                   
032870     MOVE DLIN-BEART            TO SDEV-BEART                             
032880     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
032890     MOVE DLIN-IDEXCUST-1       TO SDEV-IDEXCUST-1                        
032891     MOVE DLIN-IDEXCUST-2       TO SDEV-IDEXCUST-2                        
032892     MOVE DLIN-IDREF            TO SDEV-IDREF                             
032893     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
032894     MOVE DLIN-FLSOFT           TO SDEV-FLSOFT                            
032895     MOVE DLIN-FLFREE           TO SDEV-FLFREE                            
032896     MOVE DLIN-REARTRAB         TO SDEV-REARTRAB                          
032897     MOVE DLIN-PRARTNTO         TO SDEV-PRARTNTO                          
032898     MOVE DLIN-SUNTO            TO SDEV-SUNTO                             
032899     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
032900     COMPUTE SDEV-PRARTNTO-SEK ROUNDED = DLIN-PRARTNTO *                  
032901                                (CURR-PRKURS / CURR-REVALUTA)             
032902     COMPUTE SDEV-SUNTO-SEK  ROUNDED   = DLIN-SUNTO *                     
032903                                (CURR-PRKURS / CURR-REVALUTA)             
032904     MOVE ZERO                  TO SDEV-KVLEVART                          
032905     MOVE ZERO                  TO SDEV-IDSTATNR                          
032906     MOVE SPACE                 TO SDEV-IDLEVNR                           
032907     MOVE SPACE                 TO SDEV-DAFINDOC                          
032908                                                                          
032909     PERFORM DB2-INSERT-T01SDEV                                           
032910     .                                                                    
033000     EJECT                                                                
033100                                                                          
033110 BB-BUILD-OUTPUT-DLIN-SUNTO    SECTION.                                   
033111     MOVE YES TO BUILD-SUNH-SW                                            
033120     IF PDEV-FLSOFT = 'J'                                                 
033130       MOVE YES TO BUILD-SUNH-SW                                          
033140     ELSE                                                                 
033150       IF DLIN-FLSOFT = 'J'                                               
033160         MOVE NOO TO BUILD-SUNH-SW                                        
033170       ELSE                                                               
033180         MOVE YES TO BUILD-SUNH-SW                                        
033190       END-IF                                                             
033191     END-IF                                                               
033192                                                                          
033193     IF BUILD-SUNH-JA                                                     
033194       IF PDEV-FLFREE = 'J'                                               
033195         MOVE YES TO BUILD-SUNH-SW                                        
033196       ELSE                                                               
033197         IF DLIN-FLFREE = 'J'                                             
033198           MOVE NOO TO BUILD-SUNH-SW                                      
033199         ELSE                                                             
033200           MOVE YES TO BUILD-SUNH-SW                                      
033201         END-IF                                                           
033202       END-IF                                                             
033203     END-IF                                                               
033204                                                                          
033205     IF BUILD-SUNH-JA                                                     
033206       IF PDEV-FLINVOIC = 'J'                                             
033207         IF DHEA-KDFINDOC = 'INV'                                         
033208           MOVE YES TO BUILD-SUNH-SW                                      
033209         ELSE                                                             
033210           MOVE NOO TO BUILD-SUNH-SW                                      
033211         END-IF                                                           
033212       ELSE                                                               
033213         MOVE YES TO BUILD-SUNH-SW                                        
033214       END-IF                                                             
033215     END-IF                                                               
033216                                                                          
033217     IF BUILD-SUNH-JA                                                     
033218       IF PDEV-FLSERV = 'J'                                               
033219         MOVE YES TO BUILD-SUNH-SW                                        
033220       ELSE                                                               
033221         IF DLIN-BEART = SPACE                                            
033222           MOVE NOO TO BUILD-SUNH-SW                                      
033223         ELSE                                                             
033224           MOVE YES TO BUILD-SUNH-SW                                      
033225         END-IF                                                           
033226       END-IF                                                             
033227     END-IF                                                               
033228                                                                          
033229     IF BUILD-SUNH-JA                                                     
033230       PERFORM BBB-BUILD-OUTPUT-DLIN-SUNTO                                
033231     END-IF                                                               
033232     .                                                                    
033233     EJECT                                                                
033234                                                                          
033240 BBB-BUILD-OUTPUT-DLIN-SUNTO    SECTION.                                  
033250     ADD +1 TO WS-IX                                                      
033300     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
033400     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
033500     MOVE WS-IX                 TO SDEV-IDLOPNR                           
033600     MOVE 'NET VALUE IS HIGH'   TO SDEV-BETEXT                            
033700     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
033800     MOVE DLIN-IDARTNR-FINANCE  TO SDEV-IDARTNR-FINANCE                   
033900     MOVE DLIN-BEART            TO SDEV-BEART                             
034000     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
034100     MOVE DLIN-IDEXCUST-1       TO SDEV-IDEXCUST-1                        
034200     MOVE DLIN-IDEXCUST-2       TO SDEV-IDEXCUST-2                        
034300     MOVE DLIN-IDREF            TO SDEV-IDREF                             
034400     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
034410     MOVE DLIN-FLSOFT           TO SDEV-FLSOFT                            
034420     MOVE DLIN-FLFREE           TO SDEV-FLFREE                            
034430     MOVE DLIN-REARTRAB         TO SDEV-REARTRAB                          
034440     MOVE DLIN-PRARTNTO         TO SDEV-PRARTNTO                          
034450     MOVE DLIN-SUNTO            TO SDEV-SUNTO                             
034460     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
034461     COMPUTE SDEV-PRARTNTO-SEK ROUNDED = DLIN-PRARTNTO *                  
034462                                (CURR-PRKURS / CURR-REVALUTA)             
034463     COMPUTE SDEV-SUNTO-SEK ROUNDED    = DLIN-SUNTO *                     
034464                                (CURR-PRKURS / CURR-REVALUTA)             
034465     MOVE ZERO                  TO SDEV-KVLEVART                          
034466     MOVE ZERO                  TO SDEV-IDSTATNR                          
034467     MOVE SPACE                 TO SDEV-IDLEVNR                           
034468     MOVE SPACE                 TO SDEV-DAFINDOC                          
034469                                                                          
034470     PERFORM DB2-INSERT-T01SDEV                                           
034490     .                                                                    
034491     EJECT                                                                
034492                                                                          
034493 BC-BUILD-OUTPUT-DLIN-SOFT     SECTION.                                   
034494     ADD +1 TO WS-IX                                                      
034495     MOVE DHEA-IDLEGSEL         TO SDEV-IDLEGSEL                          
034496     MOVE WS-RUNDATUM           TO SDEV-DAREGDAT                          
034497     MOVE WS-IX                 TO SDEV-IDLOPNR                           
034498     MOVE 'SOFTWARE TO CHINA'   TO SDEV-BETEXT                            
034499     MOVE DHEA-IDFINDOC         TO SDEV-IDFINDOC                          
034500     MOVE DLIN-IDARTNR-FINANCE  TO SDEV-IDARTNR-FINANCE                   
034510     MOVE DLIN-BEART            TO SDEV-BEART                             
034511     MOVE DHEA-IDPARTNR         TO SDEV-IDPARTNR                          
034512     MOVE DLIN-IDEXCUST-1       TO SDEV-IDEXCUST-1                        
034513     MOVE DLIN-IDEXCUST-2       TO SDEV-IDEXCUST-2                        
034514     MOVE DLIN-IDREF            TO SDEV-IDREF                             
034515     MOVE DHEA-KDFINDOC         TO SDEV-KDFINDOC                          
034516     MOVE DLIN-FLSOFT           TO SDEV-FLSOFT                            
034517     MOVE DLIN-FLFREE           TO SDEV-FLFREE                            
034518     MOVE DLIN-REARTRAB         TO SDEV-REARTRAB                          
034519     MOVE DLIN-PRARTNTO         TO SDEV-PRARTNTO                          
034520     MOVE DLIN-SUNTO            TO SDEV-SUNTO                             
034521     MOVE DLIN-KVLEVART         TO SDEV-KVLEVART                          
034522     MOVE DLIN-IDSTATNR         TO SDEV-IDSTATNR                          
034523     MOVE DHEA-KDVALISO         TO SDEV-KDVALISO                          
034524     MOVE DHEA-DAFINDOC         TO SDEV-DAFINDOC                          
034525     MOVE DHEA-IDLEVNR          TO SDEV-IDLEVNR                           
034526     COMPUTE SDEV-PRARTNTO-SEK ROUNDED = DLIN-PRARTNTO *                  
034527                                (CURR-PRKURS / CURR-REVALUTA)             
034528     COMPUTE SDEV-SUNTO-SEK ROUNDED    = DLIN-SUNTO *                     
034529                                (CURR-PRKURS / CURR-REVALUTA)             
034530     MOVE DLIN-KVLEVART         TO SDEV-KVLEVART                          
034531     MOVE DLIN-IDSTATNR         TO SDEV-IDSTATNR                          
034532     MOVE DLIN-IDLEVNR          TO SDEV-IDLEVNR                           
034533     MOVE DHEA-DAFINDOC         TO SDEV-DAFINDOC                          
034534                                                                          
034535     PERFORM DB2-INSERT-T01SDEV                                           
034536     .                                                                    
034537     EJECT                                                                
034538                                                                          
034540 Z-FINISH SECTION.                                                        
034600     CONTINUE                                                             
034700     .                                                                    
034800     EJECT                                                                
034900                                                                          
039900* --- DB2 SECTIONS  ---                                                   
040000*                                                                         
040010 DB2-OPEN-T01PDEV SECTION.                                                
040020     EXEC SQL DECLARE PDEV-CRS CURSOR FOR                                 
040030     SELECT   IDLEGSEL                                                    
040040             ,KDBEHX                                                      
040050             ,FLPAYTE                                                     
040060             ,FLDELTE                                                     
040070             ,REARTRAB                                                    
040080             ,PRARTNTO_MIN                                                
040090             ,PRARTNTO_MAX                                                
040091             ,SUNTO_MIN                                                   
040092             ,SUNTO_MAX                                                   
040093             ,FLSOFT                                                      
040094             ,FLFREE                                                      
040095             ,FLSERV                                                      
040096             ,FLINVOIC                                                    
040097                                                                          
040098     FROM     T01PDEV                                                     
040099                                                                          
040100     WHERE    KDBEHX   = 'S'                                              
040101                                                                          
040102     ORDER BY IDLEGSEL                                                    
040103                                                                          
040104     FOR FETCH ONLY                                                       
040105     END-EXEC                                                             
040106                                                                          
040107     MOVE 000            TO GOOD-SQLCODES                                 
040108     EXEC SQL OPEN PDEV-CRS                                               
040109     END-EXEC                                                             
040110     MOVE SQLCODE        TO SQLCODE-WS                                    
040111     PERFORM DB2-STATUS-CHECK                                             
040112     .                                                                    
040113     EJECT                                                                
040114                                                                          
040115 DB2-FETCH-T01PDEV SECTION.                                               
040116     EXEC SQL FETCH PDEV-CRS INTO                                         
040117            :PDEV-IDLEGSEL                                                
040118           ,:PDEV-KDBEHX                                                  
040119           ,:PDEV-FLPAYTE                                                 
040120           ,:PDEV-FLDELTE                                                 
040121           ,:PDEV-REARTRAB                                                
040122           ,:PDEV-PRARTNTO-MIN                                            
040123           ,:PDEV-PRARTNTO-MAX                                            
040124           ,:PDEV-SUNTO-MIN                                               
040125           ,:PDEV-SUNTO-MAX                                               
040126           ,:PDEV-FLSOFT                                                  
040127           ,:PDEV-FLFREE                                                  
040128           ,:PDEV-FLSERV                                                  
040129           ,:PDEV-FLINVOIC                                                
040130     END-EXEC                                                             
040131                                                                          
040132     MOVE 000100         TO GOOD-SQLCODES                                 
040133     MOVE SQLCODE        TO SQLCODE-WS                                    
040134     PERFORM DB2-STATUS-CHECK                                             
040135     .                                                                    
040136     EJECT                                                                
040137                                                                          
040138 DB2-CLOSE-T01PDEV SECTION.                                               
040139     EXEC SQL CLOSE PDEV-CRS                                              
040140     END-EXEC                                                             
040141     .                                                                    
040142     EJECT                                                                
040143                                                                          
040150****** TERMS OF PAYMENT CHECK ********                                    
040200*                                                                         
040300 DB2-OPEN-CRS-DHEA-PAY SECTION.                                           
040400     EXEC SQL DECLARE DHEA-CRS CURSOR FOR                                 
040500     SELECT   IDLEGSEL                                                    
040600             ,IDFINDOC                                                    
040610             ,IDPARTNR                                                    
040620             ,KDFINDOC                                                    
040630             ,FLSOFT                                                      
040640             ,FLFREE                                                      
040650             ,KDVALISO                                                    
040700                                                                          
040800     FROM     T01DHEA                                                     
040900                                                                          
041000     WHERE    IDLEGSEL = :PDEV-IDLEGSEL                                   
041100     AND      DAEXDAT  = :WS-RUNDATUM                                     
041200     AND      BEBETVIL = ' '                                              
041500                                                                          
041600     ORDER BY IDLEGSEL                                                    
041700             ,IDFINDOC                                                    
041800                                                                          
041900     FOR FETCH ONLY                                                       
042000     END-EXEC                                                             
042100                                                                          
042200     MOVE 000            TO GOOD-SQLCODES                                 
042300     EXEC SQL OPEN DHEA-CRS                                               
042400     END-EXEC                                                             
042500     MOVE SQLCODE        TO SQLCODE-WS                                    
042600     PERFORM DB2-STATUS-CHECK                                             
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000 DB2-FETCH-CRS-DHEA-PAY SECTION.                                          
043100     EXEC SQL FETCH DHEA-CRS INTO                                         
043200            :DHEA-IDLEGSEL                                                
043300           ,:DHEA-IDFINDOC                                                
043310           ,:DHEA-IDPARTNR                                                
043320           ,:DHEA-KDFINDOC                                                
043330           ,:DHEA-FLSOFT                                                  
043340           ,:DHEA-FLFREE                                                  
043350           ,:DHEA-KDVALISO                                                
043400     END-EXEC                                                             
043500                                                                          
043600     MOVE 000100         TO GOOD-SQLCODES                                 
043700     MOVE SQLCODE        TO SQLCODE-WS                                    
043800     PERFORM DB2-STATUS-CHECK                                             
043900     .                                                                    
044000     EJECT                                                                
044100                                                                          
044200 DB2-CLOSE-CRS-DHEA-PAY SECTION.                                          
044300     EXEC SQL CLOSE DHEA-CRS                                              
044400     END-EXEC                                                             
044500     .                                                                    
044600     EJECT                                                                
044700                                                                          
044850****** TERMS OF DELIVERY CHECK ********                                   
044900*                                                                         
045000 DB2-OPEN-CRS-DLIN-DEL SECTION.                                           
045100     EXEC SQL DECLARE DLIN-CRS CURSOR FOR                                 
045200     SELECT   DISTINCT(B.IDFINDOC)                                        
045300             ,B.IDLEGSEL                                                  
045310             ,B.KDVALISO                                                  
045320             ,B.IDPARTNR                                                  
045330             ,B.KDFINDOC                                                  
045340             ,B.FLSOFT                                                    
045350             ,B.FLFREE                                                    
045400                                                                          
045500     FROM     T01DLIN A                                                   
045600             ,T01DHEA B                                                   
045700                                                                          
045800     WHERE    A.IDLEGSEL = :PDEV-IDLEGSEL                                 
045900     AND      A.DAEXDAT  = :WS-RUNDATUM                                   
046000     AND      A.BELEVVIL = ' '                                            
046300     AND      A.IDLEGSEL      = B.IDLEGSEL                                
046400     AND      A.DAEXDAT       = B.DAEXDAT                                 
046500     AND      A.TIEXTID       = B.TIEXTID                                 
046600     AND      B.TIEXTID       = A.TIEXTID                                 
046700     AND      B.KDVALISO      = A.KDVALISO                                
046800     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
046900     AND      B.IDLEVNR       = A.IDLEVNR                                 
047000     AND      B.IDPARTNR      = A.IDPARTNR                                
047100     AND      B.KDFINDOC      = A.KDFINDOC                                
047200     AND      B.FLSOFT        = A.FLSOFT                                  
047300     AND      B.FLFREE        = A.FLFREE                                  
047400     AND      B.FLPRIV        = A.FLPRIV                                  
047500     AND      B.IDBREAK_1     = A.IDBREAK_1                               
047600     AND      B.IDBREAK_2     = A.IDBREAK_2                               
047700                                                                          
047800     ORDER BY B.IDLEGSEL                                                  
047900             ,B.IDFINDOC                                                  
048000                                                                          
048100     FOR FETCH ONLY                                                       
048200     END-EXEC                                                             
048300                                                                          
048400     MOVE 000            TO GOOD-SQLCODES                                 
048500     EXEC SQL OPEN DLIN-CRS                                               
048600     END-EXEC                                                             
048700     MOVE SQLCODE        TO SQLCODE-WS                                    
048800     PERFORM DB2-STATUS-CHECK                                             
048900     .                                                                    
049000     EJECT                                                                
049100                                                                          
049200 DB2-FETCH-CRS-DLIN-DEL SECTION.                                          
049300     EXEC SQL FETCH DLIN-CRS INTO                                         
049400            :DHEA-IDFINDOC                                                
049500           ,:DHEA-IDLEGSEL                                                
049510           ,:DHEA-KDVALISO                                                
049520           ,:DHEA-IDPARTNR                                                
049530           ,:DHEA-KDFINDOC                                                
049540           ,:DHEA-FLSOFT                                                  
049550           ,:DHEA-FLFREE                                                  
049600     END-EXEC                                                             
049700                                                                          
049800     MOVE 000100         TO GOOD-SQLCODES                                 
049900     MOVE SQLCODE        TO SQLCODE-WS                                    
050000     PERFORM DB2-STATUS-CHECK                                             
050100     .                                                                    
050200     EJECT                                                                
050300                                                                          
050400 DB2-CLOSE-CRS-DLIN-DEL SECTION.                                          
050500     EXEC SQL CLOSE DLIN-CRS                                              
050600     END-EXEC                                                             
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000****** TERMS OF DISCOUNT CHECK ********                                   
051100*                                                                         
051200 DB2-OPEN-CRS-DLIN-DIS SECTION.                                           
051300     EXEC SQL DECLARE DLIN2-CRS CURSOR FOR                                
051400     SELECT   B.IDLEGSEL                                                  
051500             ,B.IDFINDOC                                                  
051600             ,A.IDARTNR_FINANCE                                           
051610             ,A.BEART                                                     
051620             ,B.IDPARTNR                                                  
051700             ,A.IDEXCUST_1                                                
051710             ,A.IDEXCUST_2                                                
051720             ,A.IDREF                                                     
051721             ,B.KDFINDOC                                                  
051722             ,A.FLSOFT                                                    
051723             ,A.FLFREE                                                    
051730             ,A.REARTRAB                                                  
051740             ,A.PRARTNTO                                                  
051750             ,A.SUNTO                                                     
051760             ,B.KDVALISO                                                  
051800                                                                          
051900     FROM     T01DLIN A                                                   
052000             ,T01DHEA B                                                   
052100                                                                          
052200     WHERE    A.IDLEGSEL      = :PDEV-IDLEGSEL                            
052300     AND      A.DAEXDAT       = :WS-RUNDATUM                              
052600     AND      A.REARTRAB      > :PDEV-REARTRAB                            
052700     AND      A.IDLEGSEL      = B.IDLEGSEL                                
052800     AND      A.DAEXDAT       = B.DAEXDAT                                 
052900     AND      A.TIEXTID       = B.TIEXTID                                 
053000     AND      B.TIEXTID       = A.TIEXTID                                 
053100     AND      B.KDVALISO      = A.KDVALISO                                
053200     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
053300     AND      B.IDLEVNR       = A.IDLEVNR                                 
053400     AND      B.IDPARTNR      = A.IDPARTNR                                
053500     AND      B.KDFINDOC      = A.KDFINDOC                                
053600     AND      B.FLSOFT        = A.FLSOFT                                  
053700     AND      B.FLFREE        = A.FLFREE                                  
053800     AND      B.FLPRIV        = A.FLPRIV                                  
053900     AND      B.IDBREAK_1     = A.IDBREAK_1                               
054000     AND      B.IDBREAK_2     = A.IDBREAK_2                               
054100                                                                          
054200     ORDER BY B.IDLEGSEL                                                  
054300             ,B.IDFINDOC                                                  
054400                                                                          
054500     FOR FETCH ONLY                                                       
054600     END-EXEC                                                             
054700                                                                          
054800     MOVE 000            TO GOOD-SQLCODES                                 
054900     EXEC SQL OPEN DLIN2-CRS                                              
055000     END-EXEC                                                             
055100     MOVE SQLCODE        TO SQLCODE-WS                                    
055200     PERFORM DB2-STATUS-CHECK                                             
055300     .                                                                    
055400     EJECT                                                                
055500                                                                          
055600 DB2-FETCH-CRS-DLIN-DIS SECTION.                                          
055700     EXEC SQL FETCH DLIN2-CRS INTO                                        
055800            :DHEA-IDLEGSEL                                                
055900           ,:DHEA-IDFINDOC                                                
056000           ,:DLIN-IDARTNR-FINANCE                                         
056100           ,:DLIN-BEART                                                   
056110           ,:DHEA-IDPARTNR                                                
056120           ,:DLIN-IDEXCUST-1                                              
056130           ,:DLIN-IDEXCUST-2                                              
056140           ,:DLIN-IDREF                                                   
056150           ,:DHEA-KDFINDOC                                                
056160           ,:DLIN-FLSOFT                                                  
056170           ,:DLIN-FLFREE                                                  
056180           ,:DLIN-REARTRAB                                                
056190           ,:DLIN-PRARTNTO                                                
056191           ,:DLIN-SUNTO                                                   
056192           ,:DHEA-KDVALISO                                                
056200     END-EXEC                                                             
056300                                                                          
056400     MOVE 000100         TO GOOD-SQLCODES                                 
056500     MOVE SQLCODE        TO SQLCODE-WS                                    
056600     PERFORM DB2-STATUS-CHECK                                             
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 DB2-CLOSE-CRS-DLIN-DIS SECTION.                                          
057100     EXEC SQL CLOSE DLIN2-CRS                                             
057200     END-EXEC                                                             
057300     .                                                                    
057400     EJECT                                                                
057500                                                                          
057600****** TERMS OF NETPRICE CHECK ********                                   
057700*                                                                         
057800 DB2-OPEN-CRS-DLIN-PRARTNTO-1 SECTION.                                    
057900     EXEC SQL DECLARE DLIN31-CRS CURSOR FOR                               
058000     SELECT   B.IDLEGSEL                                                  
058100             ,B.IDFINDOC                                                  
058200             ,A.IDARTNR_FINANCE                                           
058300             ,A.BEART                                                     
058400             ,B.IDPARTNR                                                  
058410             ,A.IDEXCUST_1                                                
058420             ,A.IDEXCUST_2                                                
058430             ,A.IDREF                                                     
058440             ,B.KDFINDOC                                                  
058450             ,A.FLSOFT                                                    
058460             ,A.FLFREE                                                    
058470             ,A.REARTRAB                                                  
058480             ,A.PRARTNTO                                                  
058490             ,A.SUNTO                                                     
058491             ,B.KDVALISO                                                  
058500                                                                          
058600     FROM     T01DLIN A                                                   
058700             ,T01DHEA B                                                   
058800                                                                          
058900     WHERE    A.IDLEGSEL      = :PDEV-IDLEGSEL                            
059000     AND      A.DAEXDAT       = :WS-RUNDATUM                              
059100     AND      A.PRARTNTO      < :WS-VALUE-MIN                             
059200     AND      A.KDVALISO      = :WS-KDVALISO                              
059600     AND      A.IDLEGSEL      = B.IDLEGSEL                                
059700     AND      A.DAEXDAT       = B.DAEXDAT                                 
059800     AND      A.TIEXTID       = B.TIEXTID                                 
059900     AND      B.TIEXTID       = A.TIEXTID                                 
060000     AND      B.KDVALISO      = A.KDVALISO                                
060100     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
060200     AND      B.IDLEVNR       = A.IDLEVNR                                 
060300     AND      B.IDPARTNR      = A.IDPARTNR                                
060400     AND      B.KDFINDOC      = A.KDFINDOC                                
060500     AND      B.FLSOFT        = A.FLSOFT                                  
060600     AND      B.FLFREE        = A.FLFREE                                  
060700     AND      B.FLPRIV        = A.FLPRIV                                  
060800     AND      B.IDBREAK_1     = A.IDBREAK_1                               
060900     AND      B.IDBREAK_2     = A.IDBREAK_2                               
061000                                                                          
061100     ORDER BY B.IDLEGSEL                                                  
061200             ,B.IDFINDOC                                                  
061300                                                                          
061400     FOR FETCH ONLY                                                       
061500     END-EXEC                                                             
061600                                                                          
061700     MOVE 000            TO GOOD-SQLCODES                                 
061800     EXEC SQL OPEN DLIN31-CRS                                             
061900     END-EXEC                                                             
062000     MOVE SQLCODE        TO SQLCODE-WS                                    
062100     PERFORM DB2-STATUS-CHECK                                             
062200     .                                                                    
062300     EJECT                                                                
062400                                                                          
062500 DB2-FETCH-CRS-DLIN-PRARTNTO-1 SECTION.                                   
062600     EXEC SQL FETCH DLIN31-CRS INTO                                       
062700            :DHEA-IDLEGSEL                                                
062800           ,:DHEA-IDFINDOC                                                
062900           ,:DLIN-IDARTNR-FINANCE                                         
063000           ,:DLIN-BEART                                                   
063100           ,:DHEA-IDPARTNR                                                
063110           ,:DLIN-IDEXCUST-1                                              
063120           ,:DLIN-IDEXCUST-2                                              
063130           ,:DLIN-IDREF                                                   
063140           ,:DHEA-KDFINDOC                                                
063150           ,:DLIN-FLSOFT                                                  
063160           ,:DLIN-FLFREE                                                  
063170           ,:DLIN-REARTRAB                                                
063180           ,:DLIN-PRARTNTO                                                
063190           ,:DLIN-SUNTO                                                   
063191           ,:DHEA-KDVALISO                                                
063200     END-EXEC                                                             
063300                                                                          
063400     MOVE 000100         TO GOOD-SQLCODES                                 
063500     MOVE SQLCODE        TO SQLCODE-WS                                    
063600     PERFORM DB2-STATUS-CHECK                                             
063700     .                                                                    
063800     EJECT                                                                
063900                                                                          
064000 DB2-CLOSE-CRS-DLIN-PRARTNTO-1 SECTION.                                   
064100     EXEC SQL CLOSE DLIN31-CRS                                            
064200     END-EXEC                                                             
064300     .                                                                    
064400     EJECT                                                                
064500                                                                          
064510 DB2-OPEN-CRS-DLIN-PRARTNTO-2 SECTION.                                    
064520     EXEC SQL DECLARE DLIN32-CRS CURSOR FOR                               
064530     SELECT   B.IDLEGSEL                                                  
064540             ,B.IDFINDOC                                                  
064550             ,A.IDARTNR_FINANCE                                           
064560             ,A.BEART                                                     
064570             ,B.IDPARTNR                                                  
064580             ,A.IDEXCUST_1                                                
064590             ,A.IDEXCUST_2                                                
064591             ,A.IDREF                                                     
064592             ,B.KDFINDOC                                                  
064593             ,A.FLSOFT                                                    
064594             ,A.FLFREE                                                    
064595             ,A.REARTRAB                                                  
064596             ,A.PRARTNTO                                                  
064597             ,A.SUNTO                                                     
064598             ,B.KDVALISO                                                  
064599                                                                          
064600     FROM     T01DLIN A                                                   
064601             ,T01DHEA B                                                   
064602                                                                          
064603     WHERE    A.IDLEGSEL      = :PDEV-IDLEGSEL                            
064604     AND      A.DAEXDAT       = :WS-RUNDATUM                              
064605     AND      A.PRARTNTO      > :WS-VALUE-MAX                             
064606     AND      A.KDVALISO      = :WS-KDVALISO                              
064607     AND      A.IDLEGSEL      = B.IDLEGSEL                                
064608     AND      A.DAEXDAT       = B.DAEXDAT                                 
064609     AND      A.TIEXTID       = B.TIEXTID                                 
064610     AND      B.TIEXTID       = A.TIEXTID                                 
064611     AND      B.KDVALISO      = A.KDVALISO                                
064612     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
064613     AND      B.IDLEVNR       = A.IDLEVNR                                 
064614     AND      B.IDPARTNR      = A.IDPARTNR                                
064615     AND      B.KDFINDOC      = A.KDFINDOC                                
064616     AND      B.FLSOFT        = A.FLSOFT                                  
064617     AND      B.FLFREE        = A.FLFREE                                  
064618     AND      B.FLPRIV        = A.FLPRIV                                  
064619     AND      B.IDBREAK_1     = A.IDBREAK_1                               
064620     AND      B.IDBREAK_2     = A.IDBREAK_2                               
064621                                                                          
064622     ORDER BY B.IDLEGSEL                                                  
064623             ,B.IDFINDOC                                                  
064624                                                                          
064625     FOR FETCH ONLY                                                       
064626     END-EXEC                                                             
064627                                                                          
064628     MOVE 000            TO GOOD-SQLCODES                                 
064629     EXEC SQL OPEN DLIN32-CRS                                             
064630     END-EXEC                                                             
064631     MOVE SQLCODE        TO SQLCODE-WS                                    
064632     PERFORM DB2-STATUS-CHECK                                             
064633     .                                                                    
064634     EJECT                                                                
064635                                                                          
064636 DB2-FETCH-CRS-DLIN-PRARTNTO-2 SECTION.                                   
064637     EXEC SQL FETCH DLIN32-CRS INTO                                       
064638            :DHEA-IDLEGSEL                                                
064639           ,:DHEA-IDFINDOC                                                
064640           ,:DLIN-IDARTNR-FINANCE                                         
064641           ,:DLIN-BEART                                                   
064642           ,:DHEA-IDPARTNR                                                
064643           ,:DLIN-IDEXCUST-1                                              
064644           ,:DLIN-IDEXCUST-2                                              
064645           ,:DLIN-IDREF                                                   
064646           ,:DHEA-KDFINDOC                                                
064647           ,:DLIN-FLSOFT                                                  
064648           ,:DLIN-FLFREE                                                  
064649           ,:DLIN-REARTRAB                                                
064650           ,:DLIN-PRARTNTO                                                
064651           ,:DLIN-SUNTO                                                   
064652           ,:DHEA-KDVALISO                                                
064653     END-EXEC                                                             
064654                                                                          
064655     MOVE 000100         TO GOOD-SQLCODES                                 
064656     MOVE SQLCODE        TO SQLCODE-WS                                    
064657     PERFORM DB2-STATUS-CHECK                                             
064658     .                                                                    
064659     EJECT                                                                
064660                                                                          
064661 DB2-CLOSE-CRS-DLIN-PRARTNTO-2 SECTION.                                   
064662     EXEC SQL CLOSE DLIN32-CRS                                            
064663     END-EXEC                                                             
064664     .                                                                    
064665     EJECT                                                                
064666                                                                          
064670****** TERMS OF NETPRICE CHECK ********                                   
064700*                                                                         
064800 DB2-OPEN-CRS-DLIN-SUNTO-1  SECTION.                                      
064900     EXEC SQL DECLARE DLIN41-CRS CURSOR FOR                               
065000     SELECT   B.IDLEGSEL                                                  
065100             ,B.IDFINDOC                                                  
065200             ,A.IDARTNR_FINANCE                                           
065300             ,A.BEART                                                     
065400             ,B.IDPARTNR                                                  
065410             ,A.IDEXCUST_1                                                
065420             ,A.IDEXCUST_2                                                
065430             ,A.IDREF                                                     
065440             ,B.KDFINDOC                                                  
065450             ,A.FLSOFT                                                    
065460             ,A.FLFREE                                                    
065470             ,A.REARTRAB                                                  
065480             ,A.PRARTNTO                                                  
065490             ,A.SUNTO                                                     
065491             ,B.KDVALISO                                                  
065500                                                                          
065600     FROM     T01DLIN A                                                   
065700             ,T01DHEA B                                                   
065800                                                                          
065900     WHERE    A.IDLEGSEL      = :PDEV-IDLEGSEL                            
066000     AND      A.DAEXDAT       = :WS-RUNDATUM                              
066100     AND      A.SUNTO         < :WS-VALUE-MIN                             
066300     AND      A.KDVALISO      = :WS-KDVALISO                              
066600     AND      A.IDLEGSEL      = B.IDLEGSEL                                
066700     AND      A.DAEXDAT       = B.DAEXDAT                                 
066800     AND      A.TIEXTID       = B.TIEXTID                                 
066900     AND      B.TIEXTID       = A.TIEXTID                                 
067000     AND      B.KDVALISO      = A.KDVALISO                                
067100     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
067200     AND      B.IDLEVNR       = A.IDLEVNR                                 
067300     AND      B.IDPARTNR      = A.IDPARTNR                                
067400     AND      B.KDFINDOC      = A.KDFINDOC                                
067500     AND      B.FLSOFT        = A.FLSOFT                                  
067600     AND      B.FLFREE        = A.FLFREE                                  
067700     AND      B.FLPRIV        = A.FLPRIV                                  
067800     AND      B.IDBREAK_1     = A.IDBREAK_1                               
067900     AND      B.IDBREAK_2     = A.IDBREAK_2                               
068000                                                                          
068100     ORDER BY B.IDLEGSEL                                                  
068200             ,B.IDFINDOC                                                  
068300                                                                          
068400     FOR FETCH ONLY                                                       
068500     END-EXEC                                                             
068600                                                                          
068700     MOVE 000            TO GOOD-SQLCODES                                 
068800     EXEC SQL OPEN DLIN41-CRS                                             
068900     END-EXEC                                                             
069000     MOVE SQLCODE        TO SQLCODE-WS                                    
069100     PERFORM DB2-STATUS-CHECK                                             
069200     .                                                                    
069300     EJECT                                                                
069400                                                                          
069500 DB2-FETCH-CRS-DLIN-SUNTO-1  SECTION.                                     
069600     EXEC SQL FETCH DLIN41-CRS INTO                                       
069700            :DHEA-IDLEGSEL                                                
069800           ,:DHEA-IDFINDOC                                                
069900           ,:DLIN-IDARTNR-FINANCE                                         
070000           ,:DLIN-BEART                                                   
070100           ,:DHEA-IDPARTNR                                                
070110           ,:DLIN-IDEXCUST-1                                              
070120           ,:DLIN-IDEXCUST-2                                              
070130           ,:DLIN-IDREF                                                   
070140           ,:DHEA-KDFINDOC                                                
070150           ,:DLIN-FLSOFT                                                  
070160           ,:DLIN-FLFREE                                                  
070170           ,:DLIN-REARTRAB                                                
070180           ,:DLIN-PRARTNTO                                                
070190           ,:DLIN-SUNTO                                                   
070191           ,:DHEA-KDVALISO                                                
070200     END-EXEC                                                             
070300                                                                          
070400     MOVE 000100         TO GOOD-SQLCODES                                 
070500     MOVE SQLCODE        TO SQLCODE-WS                                    
070600     PERFORM DB2-STATUS-CHECK                                             
070700     .                                                                    
070800     EJECT                                                                
070900                                                                          
071000 DB2-CLOSE-CRS-DLIN-SUNTO-1  SECTION.                                     
071100     EXEC SQL CLOSE DLIN41-CRS                                            
071200     END-EXEC                                                             
071300     .                                                                    
071400     EJECT                                                                
071500                                                                          
071510 DB2-OPEN-CRS-DLIN-SUNTO-2  SECTION.                                      
071520     EXEC SQL DECLARE DLIN42-CRS CURSOR FOR                               
071530     SELECT   B.IDLEGSEL                                                  
071540             ,B.IDFINDOC                                                  
071550             ,A.IDARTNR_FINANCE                                           
071560             ,A.BEART                                                     
071570             ,B.IDPARTNR                                                  
071580             ,A.IDEXCUST_1                                                
071590             ,A.IDEXCUST_2                                                
071591             ,A.IDREF                                                     
071592             ,B.KDFINDOC                                                  
071593             ,A.FLSOFT                                                    
071594             ,A.FLFREE                                                    
071595             ,A.REARTRAB                                                  
071596             ,A.PRARTNTO                                                  
071597             ,A.SUNTO                                                     
071598             ,B.KDVALISO                                                  
071599                                                                          
071600     FROM     T01DLIN A                                                   
071601             ,T01DHEA B                                                   
071602                                                                          
071603     WHERE    A.IDLEGSEL      = :PDEV-IDLEGSEL                            
071604     AND      A.DAEXDAT       = :WS-RUNDATUM                              
071606     AND      A.SUNTO         > :WS-VALUE-MAX                             
071607     AND      A.KDVALISO      = :WS-KDVALISO                              
071608     AND      A.IDLEGSEL      = B.IDLEGSEL                                
071609     AND      A.DAEXDAT       = B.DAEXDAT                                 
071610     AND      A.TIEXTID       = B.TIEXTID                                 
071611     AND      B.TIEXTID       = A.TIEXTID                                 
071612     AND      B.KDVALISO      = A.KDVALISO                                
071613     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
071614     AND      B.IDLEVNR       = A.IDLEVNR                                 
071615     AND      B.IDPARTNR      = A.IDPARTNR                                
071616     AND      B.KDFINDOC      = A.KDFINDOC                                
071617     AND      B.FLSOFT        = A.FLSOFT                                  
071618     AND      B.FLFREE        = A.FLFREE                                  
071619     AND      B.FLPRIV        = A.FLPRIV                                  
071620     AND      B.IDBREAK_1     = A.IDBREAK_1                               
071621     AND      B.IDBREAK_2     = A.IDBREAK_2                               
071622                                                                          
071623     ORDER BY B.IDLEGSEL                                                  
071624             ,B.IDFINDOC                                                  
071625                                                                          
071626     FOR FETCH ONLY                                                       
071627     END-EXEC                                                             
071628                                                                          
071629     MOVE 000            TO GOOD-SQLCODES                                 
071630     EXEC SQL OPEN DLIN42-CRS                                             
071631     END-EXEC                                                             
071632     MOVE SQLCODE        TO SQLCODE-WS                                    
071633     PERFORM DB2-STATUS-CHECK                                             
071634     .                                                                    
071635     EJECT                                                                
071636                                                                          
071637 DB2-FETCH-CRS-DLIN-SUNTO-2  SECTION.                                     
071638     EXEC SQL FETCH DLIN42-CRS INTO                                       
071639            :DHEA-IDLEGSEL                                                
071640           ,:DHEA-IDFINDOC                                                
071641           ,:DLIN-IDARTNR-FINANCE                                         
071642           ,:DLIN-BEART                                                   
071643           ,:DHEA-IDPARTNR                                                
071644           ,:DLIN-IDEXCUST-1                                              
071645           ,:DLIN-IDEXCUST-2                                              
071646           ,:DLIN-IDREF                                                   
071647           ,:DHEA-KDFINDOC                                                
071648           ,:DLIN-FLSOFT                                                  
071649           ,:DLIN-FLFREE                                                  
071650           ,:DLIN-REARTRAB                                                
071651           ,:DLIN-PRARTNTO                                                
071652           ,:DLIN-SUNTO                                                   
071653           ,:DHEA-KDVALISO                                                
071654     END-EXEC                                                             
071655                                                                          
071656     MOVE 000100         TO GOOD-SQLCODES                                 
071657     MOVE SQLCODE        TO SQLCODE-WS                                    
071658     PERFORM DB2-STATUS-CHECK                                             
071659     .                                                                    
071660     EJECT                                                                
071661                                                                          
071662 DB2-CLOSE-CRS-DLIN-SUNTO-2  SECTION.                                     
071663     EXEC SQL CLOSE DLIN42-CRS                                            
071664     END-EXEC                                                             
071665     .                                                                    
071666     EJECT                                                                
071667                                                                          
071670****** SELECT RIGHT KDVALISO *********                                    
071700*                                                                         
071800 DB2-OPEN-CRS-CURR-DLIN     SECTION.                                      
071900     EXEC SQL DECLARE CURR-CRS CURSOR FOR                                 
072000     SELECT   DISTINCT(A.KDVALISO)                                        
072100             ,B.PRKURS                                                    
072200             ,B.REVALUTA                                                  
072300                                                                          
072400     FROM     T01DLIN A                                                   
072500             ,T01CURR B                                                   
072600                                                                          
072700     WHERE    A.IDLEGSEL = :PDEV-IDLEGSEL                                 
072800     AND      A.DAEXDAT  = :WS-RUNDATUM                                   
073100     AND      A.IDLEGSEL = B.IDLEGSEL                                     
073200     AND      A.KDVALISO = B.KDVALISO                                     
073300     AND      B.DASTADAT = :WS-DASTADAT                                   
073400                                                                          
073500     FOR FETCH ONLY                                                       
073600     END-EXEC                                                             
073700                                                                          
073800     MOVE 000            TO GOOD-SQLCODES                                 
073900     EXEC SQL OPEN CURR-CRS                                               
074000     END-EXEC                                                             
074100     MOVE SQLCODE        TO SQLCODE-WS                                    
074200     PERFORM DB2-STATUS-CHECK                                             
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 DB2-FETCH-CRS-CURR-DLIN     SECTION.                                     
074700     EXEC SQL FETCH CURR-CRS INTO                                         
074800            :WS-KDVALISO                                                  
074900           ,:CURR-PRKURS                                                  
075000           ,:CURR-REVALUTA                                                
075100     END-EXEC                                                             
075200                                                                          
075300     MOVE 000100         TO GOOD-SQLCODES                                 
075400     MOVE SQLCODE        TO SQLCODE-WS                                    
075500     PERFORM DB2-STATUS-CHECK                                             
075600     .                                                                    
075700     EJECT                                                                
075800                                                                          
075900 DB2-CLOSE-CRS-CURR-DLIN     SECTION.                                     
076000     EXEC SQL CLOSE CURR-CRS                                              
076100     END-EXEC                                                             
076200     .                                                                    
076300     EJECT                                                                
076400                                                                          
076500****** SELECT RIGHT KDVALISO *********                                    
076600*                                                                         
076700 DB2-SELECT-MAX-T01CURR SECTION.                                          
076800     MOVE 000            TO GOOD-SQLCODES                                 
076900                                                                          
077000     EXEC SQL                                                             
077100     SELECT   MAX(T01CURR.DASTADAT)                                       
077200                                                                          
077300     INTO     :WS-DASTADAT                                                
077400                                                                          
077500     FROM     T01CURR                                                     
077600                                                                          
077700     WHERE    T01CURR.IDLEGSEL = :PDEV-IDLEGSEL                           
077800       AND   (T01CURR.DASTADAT < :WS-RUNDATUM                             
077900        OR    T01CURR.DASTADAT = :WS-RUNDATUM)                            
078000     END-EXEC                                                             
078100                                                                          
078200     MOVE SQLCODE        TO SQLCODE-WS                                    
078300     PERFORM DB2-STATUS-CHECK                                             
078400     .                                                                    
078500                                                                          
078510****** INSERT VALUES IN T01SDEV ********                                  
078520*                                                                         
078521 DB2-INSERT-T01SDEV SECTION.                                              
078522     MOVE 000     TO GOOD-SQLCODES                                        
078523     EXEC SQL                                                             
078524        INSERT INTO T01SDEV                                               
078525          (IDLEGSEL                                                       
078526          ,DAREGDAT                                                       
078527          ,IDLOPNR                                                        
078528          ,BETEXT                                                         
078529          ,IDFINDOC                                                       
078530          ,IDARTNR_FINANCE                                                
078531          ,BEART                                                          
078532          ,IDPARTNR                                                       
078533          ,IDEXCUST_1                                                     
078534          ,IDEXCUST_2                                                     
078535          ,IDREF                                                          
078536          ,KDFINDOC                                                       
078537          ,FLSOFT                                                         
078538          ,FLFREE                                                         
078539          ,REARTRAB                                                       
078540          ,PRARTNTO                                                       
078541          ,SUNTO                                                          
078542          ,KDVALISO                                                       
078543          ,PRARTNTO_SEK                                                   
078544          ,SUNTO_SEK                                                      
078545          ,KVLEVART                                                       
078546          ,IDSTATNR                                                       
078547          ,IDLEVNR                                                        
078548          ,DAFINDOC)                                                      
078549        VALUES(:SDEV-IDLEGSEL                                             
078550              ,:SDEV-DAREGDAT                                             
078551              ,:SDEV-IDLOPNR                                              
078552              ,:SDEV-BETEXT                                               
078553              ,:SDEV-IDFINDOC                                             
078554              ,:SDEV-IDARTNR-FINANCE                                      
078555              ,:SDEV-BEART                                                
078556              ,:SDEV-IDPARTNR                                             
078557              ,:SDEV-IDEXCUST-1                                           
078558              ,:SDEV-IDEXCUST-2                                           
078559              ,:SDEV-IDREF                                                
078560              ,:SDEV-KDFINDOC                                             
078561              ,:SDEV-FLSOFT                                               
078562              ,:SDEV-FLFREE                                               
078563              ,:SDEV-REARTRAB                                             
078564              ,:SDEV-PRARTNTO                                             
078565              ,:SDEV-SUNTO                                                
078566              ,:SDEV-KDVALISO                                             
078567              ,:SDEV-PRARTNTO-SEK                                         
078568              ,:SDEV-SUNTO-SEK                                            
078569              ,:SDEV-KVLEVART                                             
078570              ,:SDEV-IDSTATNR                                             
078571              ,:SDEV-IDLEVNR                                              
078572              ,:SDEV-DAFINDOC)                                            
078573     END-EXEC                                                             
078574     MOVE SQLCODE TO SQLCODE-WS                                           
078575     PERFORM DB2-STATUS-CHECK                                             
078576     .                                                                    
078577     EJECT                                                                
078578                                                                          
078647 DB2-OPEN-CRS-DLIN-SOFT SECTION.                                          
078648     EXEC SQL DECLARE DLIN-CRS-SOFT CURSOR FOR                            
078649     SELECT   B.IDLEGSEL                                                  
078650             ,B.IDFINDOC                                                  
078651             ,A.IDARTNR_FINANCE                                           
078652             ,A.BEART                                                     
078653             ,B.IDPARTNR                                                  
078654             ,A.IDEXCUST_1                                                
078655             ,A.IDEXCUST_2                                                
078656             ,A.IDREF                                                     
078657             ,B.KDFINDOC                                                  
078658             ,A.FLSOFT                                                    
078659             ,A.FLFREE                                                    
078660             ,A.REARTRAB                                                  
078661             ,A.PRARTNTO                                                  
078662             ,A.SUNTO                                                     
078663             ,B.KDVALISO                                                  
078664             ,B.DAFINDOC                                                  
078665             ,A.KVLEVART                                                  
078666             ,A.IDSTATNR                                                  
078667             ,A.IDLEVNR                                                   
078668                                                                          
078669     FROM     T01DLIN A                                                   
078670             ,T01DHEA B                                                   
078671                                                                          
078672     WHERE    A.IDLEGSEL      = 'VCCS'                                    
078673     AND      A.DAEXDAT       = :WS-RUNDATUM                              
078674     AND    ( A.FLSOFT        = 'J'                                       
078675     OR       A.FLSOFT        = 'Y')                                      
078676     AND      A.IDPARTNR = '352343'                                       
078677     AND      A.IDLEGSEL      = B.IDLEGSEL                                
078678     AND      A.DAEXDAT       = B.DAEXDAT                                 
078679     AND      A.TIEXTID       = B.TIEXTID                                 
078680     AND      B.TIEXTID       = A.TIEXTID                                 
078681     AND      B.KDVALISO      = A.KDVALISO                                
078682     AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                           
078683     AND      B.IDLEVNR       = A.IDLEVNR                                 
078684     AND      B.IDPARTNR      = A.IDPARTNR                                
078685     AND      B.KDFINDOC      = A.KDFINDOC                                
078686     AND      B.FLSOFT        = A.FLSOFT                                  
078687     AND      B.FLFREE        = A.FLFREE                                  
078688     AND      B.FLPRIV        = A.FLPRIV                                  
078689     AND      B.IDBREAK_1     = A.IDBREAK_1                               
078690     AND      B.IDBREAK_2     = A.IDBREAK_2                               
078691                                                                          
078692     ORDER BY B.IDLEGSEL                                                  
078693             ,B.IDFINDOC                                                  
078694                                                                          
078695     FOR FETCH ONLY                                                       
078696     END-EXEC                                                             
078697                                                                          
078698     MOVE 000            TO GOOD-SQLCODES                                 
078699     EXEC SQL OPEN DLIN-CRS-SOFT                                          
078700     END-EXEC                                                             
078701     MOVE SQLCODE        TO SQLCODE-WS                                    
078702     PERFORM DB2-STATUS-CHECK                                             
078703     .                                                                    
078704     EJECT                                                                
078705                                                                          
078706 DB2-FETCH-CRS-DLIN-SOFT SECTION.                                         
078707     EXEC SQL FETCH DLIN-CRS-SOFT INTO                                    
078708            :DHEA-IDLEGSEL                                                
078709           ,:DHEA-IDFINDOC                                                
078710           ,:DLIN-IDARTNR-FINANCE                                         
078711           ,:DLIN-BEART                                                   
078712           ,:DHEA-IDPARTNR                                                
078713           ,:DLIN-IDEXCUST-1                                              
078714           ,:DLIN-IDEXCUST-2                                              
078715           ,:DLIN-IDREF                                                   
078716           ,:DHEA-KDFINDOC                                                
078717           ,:DLIN-FLSOFT                                                  
078718           ,:DLIN-FLFREE                                                  
078719           ,:DLIN-REARTRAB                                                
078720           ,:DLIN-PRARTNTO                                                
078721           ,:DLIN-SUNTO                                                   
078722           ,:DHEA-KDVALISO                                                
078723           ,:DHEA-DAFINDOC                                                
078724           ,:DLIN-KVLEVART                                                
078725           ,:DLIN-IDSTATNR                                                
078726           ,:DLIN-IDLEVNR                                                 
078727     END-EXEC                                                             
078728                                                                          
078729     MOVE 000100         TO GOOD-SQLCODES                                 
078730     MOVE SQLCODE        TO SQLCODE-WS                                    
078731     PERFORM DB2-STATUS-CHECK                                             
078732     .                                                                    
078733     EJECT                                                                
078734                                                                          
078735 DB2-CLOSE-CRS-DLIN-SOFT SECTION.                                         
078736     EXEC SQL CLOSE DLIN-CRS-SOFT                                         
078737     END-EXEC                                                             
078738     .                                                                    
078739     EJECT                                                                
078740                                                                          
078741 DB2-STATUS-CHECK SECTION.                                                
078750     SET SQLCODE-IX         TO 1                                          
078800     SEARCH GOOD-SQLCODE AT END                                           
078900           CALL ABEND USING RKOD-ABEND-DB2                                
079000        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
079100           CONTINUE                                                       
079200     END-SEARCH                                                           
079300     .                                                                    

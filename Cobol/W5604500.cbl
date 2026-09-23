000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5604500.                                    
000400*AUTHOR.                     BO HAMMARIN, GDC-GROUP.                      
000500*DATE-WRITTEN.               MARS 1997.                                   
000600*REMARKS.                                                                 
000700*           PROGRAMMET GENERERAR LISTOR ENLIGT NEDAN:                     
000800*                                                                         
000900*           D2 INNEHÅLLER JUSTERINGSRAPPORT TILL NDC-NA (D&P)             
001000*                                                                         
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001700     SELECT  W5604B          ASSIGN UT-S-W56045D1.                        
001800                                                                          
001900* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
002000     SELECT  W56045          ASSIGN UT-S-W56045D2.                        
002100                                                                          
002200* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
002300     SELECT  SORTER          ASSIGN UT-S-W56045DS.                        
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W5604B                                                               
002900     RECORDING F                                                          
003000     BLOCK 0 RECORDS.                                                     
003100*01  IN-POST-B -COPY W51322P   -L.                                        
003200     SKIP2                                                                
003300                                                                          
003400 FD  W56045                                                               
003500     RECORDING V                                                          
003600     BLOCK 0 RECORDS.                                                     
003700 01  LIST-POST           PIC X(124).                                      
003800     SKIP2                                                                
003900                                                                          
003910                                                                          
003920 SD  SORTER.                                                              
003930                                                                          
004000*01  POST     -COPY W51322P   -PRE SORT-.                                 
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                         PIC X(8) VALUE 'W5604500'.             
004700 77  JA                            PIC X    VALUE 'J'.                    
004800 77  NEJ                           PIC X    VALUE 'N'.                    
004900                                                                          
005000 01  W-SUB-PROG.                                                          
005100     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
005200     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005300     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
005400                                                                          
005500 01  KONSTANTER.                                                          
005600     03  ENRAD           PIC 9       VALUE 1.                             
005700     03  TVARAD          PIC 9       VALUE 2.                             
005800     03  RADMAX          PIC S9(3)   VALUE +58  COMP-3.                   
005900                                                                          
005910 01  CURR-IDDC           PIC X(2)    VALUE SPACE.                         
005911                                                                          
005912 01  W001-DAP.                                                            
005920     03  FILLER                  PIC X(165)  VALUE SPACE.                 
006000                                                                          
006100 01  KAT-TEXTER.                                                          
006200     03  KATTEXT-30      PIC X(50)   VALUE                                
006300         'INVESTIGATION QUANTITY UPDATED'.                                
006400     03  KATTEXT-31      PIC X(50)   VALUE                                
006500         'INVESTIGATION QUANTITY UPDATED'.                                
006600     03  KATTEXT-32      PIC X(50)   VALUE                                
006700         'INVESTIGATION QUANTITY ADJUSTED'.                               
006800     03  KATTEXT-33      PIC X(50)   VALUE                                
006900         'INVESTIGATION QUANTITY UPDATED'.                                
007000                                                                          
007100 01  VARIABLER.                                                           
007200     03  SPAR-SORTBGP    PIC 9       VALUE 0.                             
007600     03  WS-ADARTADR     PIC 9(9)    VALUE 0.                             
007700     03  EOF             PIC X       VALUE 'N'.                           
007800     03  SPAR-KAT        PIC S9(3)   COMP-3 VALUE +0.                     
008200                                                                          
008300 01  SUBPROGRAM.                                                          
008400     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
008500     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
008600     EJECT                                                                
008700 01  FILLER.                                                              
008800*    03  -COPY WDATKORT.                                                  
008900     EJECT                                                                
009000 01  FILLER.                                                              
009100*    03  -COPY W0005  -PRE POSTSUM-.                                      
009200 01  INFIL-TRANSID.                                                       
009300     03  FILLER              PIC X(6)        VALUE 'W5604B'.              
009400     03  FILLER              PIC X(8)        VALUE 'W56045D1'.            
009500     03  INFIL-IDPTYP        PIC 9(4)        VALUE ZERO.                  
009600     EJECT                                                                
009700*    --- VALID IDDC CODES                                                 
009800*                                                                         
009900*01  -COPY WWDC99                                                         
010000     EJECT                                                                
010100 01  W-AREA.                                                              
010200*    03  AREA   -COPY W51322P  -PRE W9-                                   
010300     EJECT                                                                
010400****************************************************************          
010500*         HÄR FÖLJER RADER FÖR JUSTERINGSRAPPORT PER ANSK      *          
010600*         JR=JUSTERINGSRAPPORTRING                             *          
010700****************************************************************          
010800 01  FILLER               PIC X(16)      VALUE  'JRJRJRJRJR'.             
010900                                                                          
011800 01  JR-RUBRIK1.                                                          
011900     03  FILLER         PIC X(2)         VALUE SPACE.                     
012000     03  JR-RUB1-VCXX   PIC X(4)         VALUE SPACE.                     
012100     03  FILLER         PIC X(26)                                         
012200         VALUE '                  W56045-9'.                              
012300     03  JR-RUB1-IDDC   PIC XX           VALUE SPACE.                     
012400     03  FILLER         PIC X(19)                                         
012500         VALUE ' ADJUSTMENT REPORT.'.                                     
012600     03  FILLER         PIC X(36)        VALUE SPACE.                     
012700     03  JR-DAT1        PIC B99.                                          
012800     03  JR-DAT2        PIC B99.                                          
012900     03  JR-DAT3        PIC B99.                                          
013200     SKIP2                                                                
013300 01  JR-RUBRIK2.                                                          
013400     03  FILLER         PIC X(108)                                        
013500         VALUE '     PARTNO  DC  DESCRIPTION     VENDOR   AREA            
013600-              ' LPC  SORT      BUYER   ADJ.LS   ADJ.QTY    ADJ.VA        
013700-              'LUE'.                                                     
013800     SKIP2                                                                
013900 01  JR-RAD.                                                              
014000     03  JR-IDARTNR     PIC Z(11).                                        
014100     03  FILLER         PIC XX           VALUE SPACE.                     
014200     03  JR-IDDC        PIC X(2).                                         
014300     03  FILLER         PIC XX           VALUE SPACE.                     
014400     03  JR-BEART       PIC X(15).                                        
014500     03  JR-IDLEVNR     PIC X(7).                                         
014600     03  FILLER         PIC X(3)         VALUE SPACE.                     
014700     03  JR-ADLAGOMR    PIC ZZ99.                                         
014800     03  FILLER         PIC X(2)         VALUE SPACE.                     
014900     03  JR-KDPSLLOC    PIC Z(6).                                         
015000     03  FILLER         PIC X(4)         VALUE SPACE.                     
015100     03  JR-KDSORT      PIC X(2).                                         
015200     03  JR-IDANSKNR    PIC Z(11)-.                                       
015300     03  JR-KVLS        PIC Z(7)9-.                                       
015400     03  JR-KVJUSTKV    PIC Z(8)9-.                                       
015500     03  JR-JUST-VARDE  PIC Z(8)9.99-    BLANK WHEN ZERO.                 
015600     SKIP2                                                                
015700 01  BLANKRAD           PIC X            VALUE SPACE.                     
015800     EJECT                                                                
015900 PROCEDURE DIVISION.                                                      
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     SORT SORTER ON ASCENDING KEY SORT-W51322P-SORTDEL                    
016400                                                                          
016500     INPUT  PROCEDURE A-INPUT                                             
016600     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
016700     IF SORT-RETURN > +0                                                  
016800       DISPLAY ' W5604B SORT-FEL '                                        
016900       MOVE +16 TO ABEND-CODE                                             
017000       CALL ABEND USING ABEND-CODE                                        
017100     ELSE                                                                 
017200       PERFORM C-AVSLUTA                                                  
017300       MOVE +0 TO RETURN-CODE                                             
017400     END-IF                                                               
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900                                                                          
018000     OPEN INPUT  W5604B                                                   
018100          OUTPUT W56045                                                   
018200                                                                          
018300     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
018400     .                                                                    
018500     EJECT                                                                
018600 A-INPUT                  SECTION.                                        
018700                                                                          
018800     PERFORM S01-LAS-INFIL                                                
018900     PERFORM UNTIL EOF = JA                                               
019000       PERFORM S02-RELEASE-SORT                                           
019100       PERFORM S01-LAS-INFIL                                              
019200     END-PERFORM                                                          
019210     MOVE D-AAR        TO JR-DAT3                                         
019220     MOVE D-MAANAD     TO JR-DAT1                                         
019230     MOVE D-DAG        TO JR-DAT2                                         
019300     .                                                                    
019400     EJECT                                                                
019500 B-SKAPA-LISTOR SECTION.                                                  
019600                                                                          
019700     MOVE NEJ    TO EOF                                                   
019800     PERFORM S03-RETURN-SORT                                              
019900     PERFORM UNTIL EOF = JA                                               
019910       IF W9-IDDC NOT = CURR-IDDC                                         
019920          MOVE W9-IDDC  TO CURR-IDDC                                      
019930          PERFORM S10-SKRIV-DAP1                                          
019940          PERFORM S11-SKRIV-DAP2                                          
019941          PERFORM S04-SKRIV-RUBRIK                                        
019950       END-IF                                                             
020000       PERFORM BF-LISTA9                                                  
020100*                              **  JUSTERINGSRAPPORT **                   
020200       PERFORM S03-RETURN-SORT                                            
020300     END-PERFORM                                                          
020400     .                                                                    
020500     EJECT                                                                
020600 BF-LISTA9  SECTION.                                                      
020700******************************************************************        
020800*         JUSTERINGSRAPPORT         LISTA 9                      *        
020900******************************************************************        
021000                                                                          
021100     MOVE W9-IDARTNR           TO JR-IDARTNR                              
021200     MOVE W9-IDDC              TO JR-IDDC                                 
021300                                  WS-IDDC                                 
021400     MOVE W9-IDANSKNR          TO JR-IDANSKNR                             
021500     MOVE W9-BEART             TO JR-BEART                                
021600     MOVE W9-IDLEVNR           TO JR-IDLEVNR                              
021700     MOVE W9-ADLAGOMR          TO JR-ADLAGOMR                             
021800     MOVE W9-KDSORT            TO JR-KDSORT                               
021900     MOVE W9-KDPSLLOC          TO JR-KDPSLLOC                             
022000     MOVE W9-KVLS              TO JR-KVLS                                 
022100     MOVE W9-KVJUSTKV          TO JR-KVJUSTKV                             
022200     MOVE W9-JUST-VAERDE       TO JR-JUST-VARDE                           
022300                                                                          
026800                                                                          
026900     WRITE LIST-POST FROM JR-RAD                                          
027500     .                                                                    
027600     EJECT                                                                
027700 C-AVSLUTA  SECTION.                                                      
027800                                                                          
027900     CLOSE W5604B                                                         
028000           W56045                                                         
028100     .                                                                    
028200     SKIP3                                                                
028300 S01-LAS-INFIL SECTION.                                                   
028400                                                                          
028500     READ W5604B INTO W-AREA                                              
028600     AT END                                                               
028700         MOVE JA TO EOF                                                   
028800     NOT AT END                                                           
028900         MOVE W9-IDLISTA       TO INFIL-IDPTYP                            
029000         MOVE INFIL-TRANSID    TO POSTSUM-TRANSID                         
029100         CALL POSTSUM USING POSTSUM-PARM                                  
029200     END-READ                                                             
029300     .                                                                    
029400     SKIP3                                                                
029500 S02-RELEASE-SORT SECTION.                                                
029600                                                                          
029700     RELEASE SORT-POST FROM W-AREA                                        
029800     .                                                                    
029900     SKIP3                                                                
030000 S03-RETURN-SORT SECTION.                                                 
030100                                                                          
030200     RETURN SORTER INTO W-AREA                                            
030300     AT END                                                               
030400         MOVE JA TO EOF                                                   
030500     END-RETURN                                                           
030600     .                                                                    
030610                                                                          
030700 S04-SKRIV-RUBRIK SECTION.                                                
030710                                                                          
030711     IF NDC-CA                                                            
030712        MOVE 'VCL '            TO JR-RUB1-VCXX                            
030713     ELSE                                                                 
030714        MOVE 'VCNA'            TO JR-RUB1-VCXX                            
030715     END-IF                                                               
030716     MOVE W9-IDDC              TO JR-RUB1-IDDC                            
030717                                                                          
030720     WRITE LIST-POST  FROM JR-RUBRIK1                                     
030721     WRITE LIST-POST  FROM BLANKRAD                                       
030722     WRITE LIST-POST  FROM JR-RUBRIK2                                     
030723     WRITE LIST-POST  FROM BLANKRAD                                       
030724                                                                          
030725     .                                                                    
030730 S10-SKRIV-DAP1 SECTION.                                                  
030800                                                                          
030900     MOVE ' ¤DAPW56045' TO W001-DAP                                       
031000     WRITE LIST-POST FROM W001-DAP                                        
031100                                                                          
031200     MOVE SPACE TO W001-DAP                                               
031300     .                                                                    
031400                                                                          
031500 S11-SKRIV-DAP2 SECTION.                                                  
031600                                                                          
031700     STRING ' ¤DAP' CURR-IDDC                                             
031800            DELIMITED BY SIZE INTO W001-DAP                               
031900     WRITE LIST-POST FROM W001-DAP                                        
032000                                                                          
032100     MOVE SPACE TO W001-DAP                                               
032200     .                                                                    

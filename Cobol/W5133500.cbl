000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5133500.                                    
000400*AUTHOR.                     KARL JOHAN HANSSON.                          
000500*DATE-WRITTEN.               MAJ  1999.                                   
000600*REMARKS.                                                                 
000700*      PROGRAMMET SKAPAR LISTOR TILL JAPAN OCH AUSTRALIEN                 
000800*      INNEHÅLLANDE JUSTERINGSRAPPORT PER ANSKAFFARE.                     
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001500     SELECT  W51335          ASSIGN UT-S-W51335D1.                        
001600                                                                          
001700* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
001800     SELECT  LISTA9-61       ASSIGN UT-S-W51335D2.                        
001900     SELECT  LISTA9-62       ASSIGN UT-S-W51335D3.                        
001901* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
001910     SELECT  UTFIL           ASSIGN UT-S-W51335D4.                        
002000                                                                          
002100* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
002200     SELECT  SORTER          ASSIGN UT-S-W51335DS.                        
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W51335                                                               
002800     RECORDING F                                                          
002900     BLOCK 0 RECORDS.                                                     
003000                                                                          
003100*01  IN-POSTER -COPY W51335    -L.                                        
003200     EJECT                                                                
003300 FD  LISTA9-61                                                            
003400     RECORDING F                                                          
003500     BLOCK 0 RECORDS.                                                     
003600                                                                          
003700 01  LISTA-9-61          PIC X(110).                                      
003800     EJECT                                                                
003900 FD  LISTA9-62                                                            
004000     RECORDING F                                                          
004100     BLOCK 0 RECORDS.                                                     
004200                                                                          
004300 01  LISTA-9-62          PIC X(110).                                      
004400     EJECT                                                                
004410 FD  UTFIL                                                                
004420     RECORDING F                                                          
004430     BLOCK 0 RECORDS.                                                     
004440                                                                          
004450 01  UTFILEN             PIC X(121).                                      
004460     EJECT                                                                
004500 SD  SORTER.                                                              
004600                                                                          
004700*01  POST     -COPY W51335    -PRE SORT-.                                 
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                         PIC X(8) VALUE 'W5133500'.             
005300 77  JA                            PIC X    VALUE 'J'.                    
005400 77  NEJ                           PIC X    VALUE 'N'.                    
005500                                                                          
005600 01  W-SUB-PROG.                                                          
005700     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
005800     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005900     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
006000                                                                          
006100 01  ARBETSFAELT.                                                         
006200     03  RADMAX          PIC S9(3)   VALUE +42  COMP-3.                   
006300     03  EOF             PIC X       VALUE 'N'.                           
006400                                                                          
006500 01  SUBPROGRAM.                                                          
006600     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
006700     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
006800     EJECT                                                                
006810*   --- VALID IDDC CODES                                                  
006820*                                                                         
006830*01  -COPY WWDC99                                                         
006840*                                                                         
006900 01  FILLER.                                                              
007000*    03  -COPY WDATKORT.                                                  
007100     EJECT                                                                
007200 01  FILLER.                                                              
007300*    03  -COPY W0005  -PRE POSTSUM-.                                      
007400     EJECT                                                                
007500 01  W-AREA.                                                              
007600*    03  AREA   -COPY W51335   -PRE W9-                                   
007700     EJECT                                                                
007800****************************************************************          
007900*         HÄR FÖLJER RADER FÖR LISTA3 'ARTIKLAR FÖR VILKA      *          
008000*         UTREDNINGSSALDO UPPDATERATS'                         *          
008100****************************************************************          
008200 01  FILLER               PIC X(16)       VALUE ALL 'U'.                  
008300                                                                          
008400 01  BLANKRAD           PIC X        VALUE SPACE.                         
008500                                                                          
008600 01  JR-HJELP-AREA.                                                       
008700     03  JR-SIDOR         PIC S9(5)        VALUE +0  COMP-3.              
008800     03  JR-RADER         PIC S9(5)        VALUE +99 COMP-3.              
008900     03  OLD-IDDC         PIC X(2)         VALUE SPACE.                   
009000     SKIP2                                                                
009100 01  JR-RUBRIK1.                                                          
009200     03  FILLER           PIC X(2)           VALUE SPACE.                 
009300     03  FILLER           PIC X(39)                                       
009400         VALUE 'VOLVO CAR CORPORATION, CUSTOMER SERVICE'.                 
009420     03  FILLER           PIC X(10)  VALUE '  W51335-9'.                  
009500     03  JR-RUB1-IDDC     PIC XX             VALUE SPACE.                 
009600     03  FILLER           PIC X(19)                                       
009700         VALUE ' ADJUSTMENT REPORT.'.                                     
009800     03  FILLER           PIC X(17)          VALUE SPACE.                 
009900     03  JR-AAR           PIC B99.                                        
010000     03  JR-MAN           PIC B99.                                        
010100     03  JR-DAG           PIC B99.                                        
010200     03  FILLER           PIC X(6)           VALUE ' PAGE '.              
010300     03  JR-SIDA          PIC Z(4).                                       
010400     SKIP2                                                                
010410 01  RUBRIK-ONDEMAND.                                                     
010420     03  FILLER           PIC X(9)  VALUE ' W51335-9'.                    
010430     03  ON-RUB1-IDDC     PIC XX             VALUE SPACE.                 
010440     03  FILLER           PIC X(9)   VALUE SPACE.                         
010450     03  FILLER           PIC X(4)   VALUE 'VCCS'.                        
010460     03  FILLER           PIC X(16)  VALUE SPACE.                         
010470     03  FILLER           PIC X(40)                                       
010480                 VALUE 'ADJUSTMENT REPORT                       '.        
010490     03  FILLER           PIC X(10)  VALUE SPACE.                         
010491     03  FILLER           PIC X(4)   VALUE 'DC: '.                        
010492     03  ON-RUB1-IDDC2    PIC X(2).                                       
010493     03  FILLER           PIC X(4)   VALUE SPACE.                         
010494     03  FILLER           PIC X(6)   VALUE 'DATE: '.                      
010495     03  ON-AAR           PIC 99.                                         
010496     03  ON-MAN           PIC 99.                                         
010497     03  ON-DAG           PIC 99.                                         
010498                                                                          
010500 01  JR-RUBRIK2.                                                          
010510     03  FILLER           PIC X(47) VALUE                                 
010520               '     PARTNO  DC  DESCRIPTION   SUPPLIER    AREA'.         
010530     03  FILLER           PIC X(37) VALUE                                 
010540               '  P-CODE  ADJ.TYPE  PROCURER  ADJ. LS'.                   
010550     03  FILLER           PIC X(23) VALUE                                 
010560               '  ADJ QTY    ADJ. VALUE'.                                 
011000                                                                          
011100 01  JR-RAD.                                                              
011200     03  JR-IDARTNR       PIC Z(11).                                      
011300     03  FILLER           PIC XX VALUE SPACE.                             
011400     03  JR-IDDC          PIC X(2).                                       
011500     03  FILLER           PIC XX VALUE SPACE.                             
011600     03  JR-BEART         PIC X(15).                                      
011700     03  FILLER           PIC X(2).                                       
011710     03  JR-IDLEVNR       PIC X(5).                                       
011800     03  FILLER           PIC X(3) VALUE SPACE.                           
011900     03  JR-ADLAGOMR      PIC ZZ99.                                       
012100     03  JR-KDPRODSL      PIC Z(7).                                       
012300     03  JR-KDINVKAT      PIC Z(9).                                       
013000     03  JR-IDANSKNR      PIC Z(11)-.                                     
013100     03  JR-KVLS          PIC Z(9)9-.                                     
013200     03  JR-KVJUSTKV      PIC Z(7)9-.                                     
013300     03  JR-JUST-VARDE    PIC Z(9)9.99-  BLANK WHEN ZERO.                 
013400     EJECT                                                                
013500 PROCEDURE DIVISION.                                                      
013600                                                                          
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     SORT SORTER ON ASCENDING  KEY SORT-IDDC                              
013910                    DESCENDING KEY SORT-SORTVAERDE                        
013920                    ASCENDING  KEY SORT-IDARTNR                           
013930                                   SORT-IDANSKNR                          
014000                 USING W51335                                             
014100                                                                          
014200     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
014300     IF SORT-RETURN > +0                                                  
014400       DISPLAY ' W51335 SORT-FEL '                                        
014500       MOVE +16 TO ABEND-CODE                                             
014600       CALL ABEND USING ABEND-CODE                                        
014700     ELSE                                                                 
014800       PERFORM C-AVSLUTA                                                  
014900       MOVE +0 TO RETURN-CODE                                             
015000     END-IF                                                               
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500                                                                          
015600     OPEN OUTPUT LISTA9-61                                                
015700                 LISTA9-62                                                
015710                 UTFIL                                                    
015800                                                                          
015900     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
016000     MOVE D-AAR           TO JR-AAR                                       
016100     MOVE D-MAANAD        TO JR-MAN                                       
016200     MOVE D-DAG           TO JR-DAG                                       
016201     MOVE D-AAR           TO ON-AAR                                       
016202     MOVE D-MAANAD        TO ON-MAN                                       
016203     MOVE D-DAG           TO ON-DAG                                       
016204                                                                          
016210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016300     .                                                                    
016400     EJECT                                                                
016500 B-SKAPA-LISTOR SECTION.                                                  
016600                                                                          
016700     PERFORM S03-RETURN-SORT                                              
016800     PERFORM UNTIL EOF = JA                                               
016900       PERFORM BA-LISTA9                                                  
017000       PERFORM S03-RETURN-SORT                                            
017100     END-PERFORM                                                          
017200     .                                                                    
017300     EJECT                                                                
017400 BA-LISTA9  SECTION.                                                      
017500******************************************************************        
017600*         JUSTERINGSRAPPORT         LISTA 9                      *        
017700******************************************************************        
017800                                                                          
017900     MOVE W9-IDARTNR           TO JR-IDARTNR                              
018000     MOVE W9-IDDC              TO JR-IDDC                                 
018010                                  WS-IDDC                                 
018100     MOVE W9-IDANSKNR          TO JR-IDANSKNR                             
018200     MOVE W9-BEART             TO JR-BEART                                
018300     MOVE W9-IDLEVNR           TO JR-IDLEVNR                              
018400     MOVE W9-ADLAGOMR          TO JR-ADLAGOMR                             
018600     MOVE W9-KDINVKAT          TO JR-KDINVKAT                             
019300     MOVE W9-KDPRODSL          TO JR-KDPRODSL                             
019400     MOVE W9-KVLS              TO JR-KVLS                                 
019500     MOVE W9-KVJUSTKV          TO JR-KVJUSTKV                             
019600     MOVE W9-SUARTSTD-JUST     TO JR-JUST-VARDE                           
019700                                                                          
019800     IF W9-IDDC NOT = OLD-IDDC                                            
019900       MOVE W9-IDDC            TO OLD-IDDC                                
019910                                  ON-RUB1-IDDC                            
019920                                  ON-RUB1-IDDC2                           
020000       ADD +99                 TO JR-RADER                                
020010       MOVE +0                 TO JR-SIDOR                                
020020       WRITE UTFILEN    FROM  RUBRIK-ONDEMAND AFTER PAGE                  
020030       WRITE UTFILEN    FROM  JR-RUBRIK2                                  
020100     END-IF                                                               
020200     MOVE W9-IDDC              TO JR-RUB1-IDDC                            
020300                                                                          
020400     IF JR-RADER > RADMAX                                                 
020500       ADD +1                  TO JR-SIDOR                                
020600       MOVE JR-SIDOR           TO JR-SIDA                                 
020700       EVALUATE TRUE                                                      
020800         WHEN NDC-JP                                                      
020900           WRITE LISTA-9-61 FROM  JR-RUBRIK1 AFTER PAGE                   
021000           WRITE LISTA-9-61 FROM  JR-RUBRIK2 AFTER 2                      
021100           WRITE LISTA-9-61 FROM  BLANKRAD AFTER 1                        
021200         WHEN NDC-AU                                                      
021300           WRITE LISTA-9-62 FROM  JR-RUBRIK1 AFTER PAGE                   
021400           WRITE LISTA-9-62 FROM  JR-RUBRIK2 AFTER 2                      
021500           WRITE LISTA-9-62 FROM  BLANKRAD AFTER 1                        
021600       END-EVALUATE                                                       
021700       MOVE +8 TO JR-RADER                                                
021800     END-IF                                                               
021900                                                                          
022000     EVALUATE TRUE                                                        
022100       WHEN NDC-JP                                                        
022110           WRITE LISTA-9-61 FROM JR-RAD AFTER 1                           
022200       WHEN NDC-AU                                                        
022210           WRITE LISTA-9-62 FROM JR-RAD AFTER 1                           
022300     END-EVALUATE                                                         
022310     WRITE UTFILEN    FROM JR-RAD                                         
022400     ADD +1 TO JR-RADER                                                   
022500     .                                                                    
022600     SKIP3                                                                
022700 C-AVSLUTA  SECTION.                                                      
022800                                                                          
022900     CLOSE LISTA9-61                                                      
023000           LISTA9-62                                                      
023001           UTFIL                                                          
023010                                                                          
023020     MOVE 'S'     TO POSTSUM-OPKOD                                        
023030     CALL POSTSUM USING POSTSUM-PARM                                      
023100     .                                                                    
023200     SKIP3                                                                
023300 S03-RETURN-SORT SECTION.                                                 
023400                                                                          
023500     RETURN SORTER INTO W-AREA                                            
023510     AT END                                                               
023520         MOVE JA TO EOF                                                   
023530     NOT AT END                                                           
023600       MOVE 'W51335'   TO POSTSUM-FDNAMN                                  
023601       MOVE 'W51335D1' TO POSTSUM-DDNAMN2                                 
023602       MOVE W9-IDDC    TO POSTSUM-TRANSTYP                                
023603       CALL POSTSUM USING POSTSUM-PARM                                    
023800     END-RETURN                                                           
023900     .                                                                    

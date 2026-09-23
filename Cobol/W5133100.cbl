000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5133100.                                    
000400*AUTHOR.                     KARL JOHAN HANSSON.                          
000500*DATE-WRITTEN.               MAJ  1999.                                   
000600*REMARKS.                                                                 
000700*      PROGRAMMET SKAPAR LISTOR TILL CDC                                  
000800*      INNEHÅLLANDE JUSTERINGSRAPPORT PER ANSKAFFARE.                     
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001500     SELECT  W51331          ASSIGN UT-S-W51331D1.                        
001600                                                                          
001700* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
001800     SELECT  LISTA9          ASSIGN UT-S-W51331D2.                        
001900                                                                          
001910* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
001920     SELECT  SORTER          ASSIGN UT-S-W51331DS.                        
001930     EJECT                                                                
001940 DATA DIVISION.                                                           
001950 FILE SECTION.                                                            
001960     SKIP3                                                                
001970 FD  W51331                                                               
001980     RECORDING F                                                          
001990     BLOCK 0 RECORDS.                                                     
002000                                                                          
002100*01  IN-POSTER -COPY W51335    -L.                                        
002200     EJECT                                                                
002300 FD  LISTA9                                                               
002400     RECORDING F                                                          
002500     BLOCK 0 RECORDS.                                                     
002600                                                                          
002700 01  LISTA-9             PIC X(121).                                      
002800     EJECT                                                                
002900 SD  SORTER.                                                              
003000                                                                          
003100*01  POST     -COPY W51335    -PRE SORT-.                                 
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                   PIC X(8)    VALUE 'W5133100'.                
003700 77  JA                      PIC X       VALUE 'J'.                       
003800 77  NEJ                     PIC X       VALUE 'N'.                       
003900                                                                          
003910 01  FORSTA-POST             PIC X       VALUE 'J'.                       
003920                                                                          
004000 01  W-SUB-PROG.                                                          
004100     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
004200     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
004300     03  ABEND-CODE          PIC S9(4)   VALUE +0 COMP SYNC.              
004400                                                                          
004500 01  ARBETSFAELT.                                                         
004600     03  RADMAX              PIC S9(3)   VALUE +42  COMP-3.               
004700     03  EOF                 PIC X       VALUE 'N'.                       
004800                                                                          
004900 01  SUBPROGRAM.                                                          
005000     03  WDATUM              PIC X(6)    VALUE 'WDATUM'.                  
005100     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
005200     EJECT                                                                
005300*   --- VALID IDDC CODES                                                  
005400*                                                                         
005500*01  -COPY WWDC99                                                         
005600*                                                                         
005700 01  FILLER.                                                              
005800*    03  -COPY WDATKORT.                                                  
005900     EJECT                                                                
006000 01  FILLER.                                                              
006100*    03  -COPY W0005  -PRE POSTSUM-.                                      
006200     EJECT                                                                
006300 01  W-AREA.                                                              
006400*    03  AREA   -COPY W51335   -PRE IN-                                   
006500     EJECT                                                                
006600****************************************************************          
006700*         HÄR FÖLJER RADER FÖR LISTA3 'ARTIKLAR FÖR VILKA      *          
006800*         UTREDNINGSSALDO UPPDATERATS'                         *          
006900****************************************************************          
007000 01  FILLER               PIC X(16)        VALUE ALL 'U'.                 
007100                                                                          
007200 01  BLANKRAD             PIC X            VALUE SPACE.                   
007300                                                                          
007400 01  JR-HJELP-AREA.                                                       
007500     03  JR-SIDOR         PIC S9(5)        VALUE +0  COMP-3.              
007600     03  JR-RADER         PIC S9(5)        VALUE +99 COMP-3.              
007700     SKIP2                                                                
007710*                                                                         
007800 01  JR-RUBRIK1.                                                          
007900     03  FILLER           PIC X(11)  VALUE ' W51331-911'.                 
008200     03  FILLER           PIC X(9)   VALUE SPACE.                         
008300     03  FILLER           PIC X(4)   VALUE 'VCCS'.                        
008500     03  FILLER           PIC X(16)  VALUE SPACE.                         
008510     03  FILLER           PIC X(40)                                       
008520                 VALUE 'ADJUSTMENT REPORT                       '.        
008530     03  FILLER           PIC X(10)  VALUE SPACE.                         
008540     03  FILLER           PIC X(6)   VALUE 'DC: 11'.                      
008550     03  FILLER           PIC X(4)   VALUE SPACE.                         
008560     03  FILLER           PIC X(6)   VALUE 'DATE: '.                      
008600     03  JR-AAR           PIC 99.                                         
008700     03  JR-MAN           PIC 99.                                         
008800     03  JR-DAG           PIC 99.                                         
009100                                                                          
009236                                                                          
009240 01  JR-RUBRIK2.                                                          
009300     03  FILLER           PIC X(47) VALUE                                 
009400               '     PARTNO  DC  DESCRIPTION   SUPPLIER    AREA'.         
009500     03  FILLER           PIC X(37) VALUE                                 
009600               '  P-CODE  ADJ.TYPE  PROCURER  ADJ. LS'.                   
009700     03  FILLER           PIC X(23) VALUE                                 
009800               '  ADJ QTY    ADJ. VALUE'.                                 
009900                                                                          
010000 01  JR-RAD.                                                              
010100     03  JR-IDARTNR       PIC Z(11).                                      
010200     03  FILLER           PIC XX VALUE SPACE.                             
010300     03  JR-IDDC          PIC X(2).                                       
010400     03  FILLER           PIC XX VALUE SPACE.                             
010500     03  JR-BEART         PIC X(15).                                      
010600     03  FILLER           PIC X(2) VALUE SPACE.                           
010610     03  JR-IDLEVNR       PIC X(5).                                       
010700     03  FILLER           PIC X(3) VALUE SPACE.                           
010800     03  JR-ADLAGOMR      PIC ZZ99.                                       
010900     03  JR-KDPRODSL      PIC Z(7).                                       
011000     03  JR-KDINVKAT      PIC Z(9).                                       
011100     03  JR-IDANSKNR      PIC Z(11)-.                                     
011200     03  JR-KVLS          PIC Z(9)9-.                                     
011300     03  JR-KVJUSTKV      PIC Z(7)9-.                                     
011400     03  JR-JUST-VARDE    PIC Z(9)9.99-  BLANK WHEN ZERO.                 
011500     EJECT                                                                
011600 PROCEDURE DIVISION.                                                      
011700                                                                          
011800     PERFORM A-INIT                                                       
011900                                                                          
012000     SORT SORTER ON DESCENDING KEY SORT-SORTVAERDE                        
012100                    ASCENDING  KEY SORT-IDARTNR                           
012200                                   SORT-IDANSKNR                          
012300                 USING W51331                                             
012400                                                                          
012500     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
012600     IF SORT-RETURN > +0                                                  
012700       DISPLAY ' W51331 SORT-FEL '                                        
012800       MOVE +16 TO ABEND-CODE                                             
012900       CALL ABEND USING ABEND-CODE                                        
013000     ELSE                                                                 
013100       PERFORM C-AVSLUTA                                                  
013200       MOVE +0 TO RETURN-CODE                                             
013300     END-IF                                                               
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT SECTION.                                                          
013800                                                                          
013900     OPEN OUTPUT LISTA9                                                   
014000                                                                          
014100     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
014200     MOVE D-AAR           TO JR-AAR                                       
014300     MOVE D-MAANAD        TO JR-MAN                                       
014400     MOVE D-DAG           TO JR-DAG                                       
014500                                                                          
014600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014700     .                                                                    
014800     EJECT                                                                
014900 B-SKAPA-LISTOR SECTION.                                                  
015000                                                                          
015100     PERFORM S03-RETURN-SORT                                              
015200     PERFORM UNTIL EOF = JA                                               
015300       PERFORM BA-LISTA9                                                  
015400       PERFORM S03-RETURN-SORT                                            
015500     END-PERFORM                                                          
015600     .                                                                    
015700     EJECT                                                                
015800 BA-LISTA9  SECTION.                                                      
015900******************************************************************        
016000*         JUSTERINGSRAPPORT         LISTA 9                      *        
016100******************************************************************        
016200                                                                          
016300     MOVE IN-IDARTNR           TO JR-IDARTNR                              
016400     MOVE IN-IDDC              TO JR-IDDC                                 
016500                                  WS-IDDC                                 
016600     MOVE IN-IDANSKNR          TO JR-IDANSKNR                             
016700     MOVE IN-BEART             TO JR-BEART                                
016800     MOVE IN-IDLEVNR           TO JR-IDLEVNR                              
016900     MOVE IN-ADLAGOMR          TO JR-ADLAGOMR                             
017000     MOVE IN-KDINVKAT          TO JR-KDINVKAT                             
017100     MOVE IN-KDPRODSL          TO JR-KDPRODSL                             
017200     MOVE IN-KVLS              TO JR-KVLS                                 
017300     MOVE IN-KVJUSTKV          TO JR-KVJUSTKV                             
017400     MOVE IN-SUARTSTD-JUST     TO JR-JUST-VARDE                           
017500                                                                          
017600*    IF JR-RADER > RADMAX                                                 
017700*      ADD +1                  TO JR-SIDOR                                
017800*      MOVE JR-SIDOR           TO JR-SIDA                                 
017810       IF FORSTA-POST = 'J'                                               
017900         WRITE LISTA-9    FROM  JR-RUBRIK1 AFTER PAGE                     
017901         WRITE LISTA-9    FROM  JR-RUBRIK2 AFTER 2                        
017902         WRITE LISTA-9    FROM  BLANKRAD AFTER 1                          
017910         MOVE 'N'               TO FORSTA-POST                            
017920       END-IF                                                             
018200*      MOVE +8 TO JR-RADER                                                
018300*    END-IF                                                               
018400                                                                          
018500     WRITE LISTA-9    FROM JR-RAD AFTER 1                                 
018600*    ADD +1 TO JR-RADER                                                   
018700     .                                                                    
018800     SKIP3                                                                
018900 C-AVSLUTA  SECTION.                                                      
019000                                                                          
019100     CLOSE LISTA9                                                         
019200                                                                          
019300     MOVE 'S'     TO POSTSUM-OPKOD                                        
019400     CALL POSTSUM USING POSTSUM-PARM                                      
019500     .                                                                    
019600     SKIP3                                                                
019700 S03-RETURN-SORT SECTION.                                                 
019800                                                                          
019900     RETURN SORTER INTO W-AREA                                            
020000     AT END                                                               
020100         MOVE JA TO EOF                                                   
020200     NOT AT END                                                           
020300       MOVE 'W51331'   TO POSTSUM-FDNAMN                                  
020400       MOVE 'W51331D1' TO POSTSUM-DDNAMN2                                 
020500       MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                
020600       CALL POSTSUM USING POSTSUM-PARM                                    
020700     END-RETURN                                                           
020800     .                                                                    

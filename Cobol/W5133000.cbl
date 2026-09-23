000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5133000.                                    
000400*AUTHOR.                     KARL JOHAN HANSSON                           
000500*DATE-WRITTEN.               MAJ  1999.                                   
000600*REMARKS.                                                                 
000700*      PROGRAMMET SKAPAR LISTOR TILL CDC                                  
000800*      INNEHÅLLANDE ARTIKLAR MED 'UTREDNINGSSALDO UPPDATERAT'.            
000810*      (KATEGORI 2, 8 OCH 11)                                             
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001500     SELECT  W51330          ASSIGN UT-S-W51330D1.                        
001600                                                                          
001700* - - - - - - - - - - - - - - - - - - - - - - UT FIL TILL ONDEMAND        
001800     SELECT  LISTA3          ASSIGN UT-S-W51330D2.                        
001900                                                                          
002000* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
002100     SELECT  SORTER          ASSIGN UT-S-W51330DS.                        
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD  W51330                                                               
002700     RECORDING F                                                          
002800     BLOCK 0 RECORDS.                                                     
002900                                                                          
003000*01  IN-POSTER -COPY W51334    -L.                                        
003100     EJECT                                                                
003200 FD  LISTA3                                                               
003300     RECORDING F                                                          
003400     BLOCK 0 RECORDS.                                                     
003500                                                                          
003600 01  LISTA-3             PIC X(121).                                      
003700     EJECT                                                                
003800 SD  SORTER.                                                              
003900                                                                          
004000*01  POST     -COPY W51334    -PRE SORT-.                                 
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                         PIC X(8) VALUE 'W5133000'.             
004600 77  JA                            PIC X    VALUE 'J'.                    
004700 77  NEJ                           PIC X    VALUE 'N'.                    
004800                                                                          
004810 01  FIRST-TIME                    PIC X    VALUE 'J'.                    
004820                                                                          
004900 01  W-SUB-PROG.                                                          
005000     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
005100     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005200     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
005300                                                                          
005400 01  KONSTANTER.                                                          
005500     03  RADMAX          PIC S9(3)   VALUE +42  COMP-3.                   
005600                                                                          
005700 01  KAT-TEXTER.                                                          
005800     03  KATTEXT-2       PIC X(50)   VALUE                                
005900         'TYPE 2, UPDATED ON QUEUE 5302'.                                 
005910     03  KATTEXT-8       PIC X(50)   VALUE                                
005920         'TYPE 8, ALREADY ADJUSTED'.                                      
006000     03  KATTEXT-11      PIC X(50)   VALUE                                
006010         '        PARTS ALREADY IN QUEUE 5302'.                           
006600                                                                          
006700 01  VARIABLER.                                                           
006800     03  SPAR-SORTBGP    PIC 9       VALUE 0.                             
006900     03  WS-ADARTADR     PIC 9(9)    VALUE 0.                             
007000     03  EOF             PIC X       VALUE 'N'.                           
007100     03  SPAR-KAT        PIC S9(3)   COMP-3 VALUE +0.                     
007200                                                                          
007300 01  SUBPROGRAM.                                                          
007400     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
007500     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
007600     EJECT                                                                
007700*   --- VALID IDDC CODES                                                  
007800*                                                                         
007900*01  -COPY WWDC99                                                         
008000*                                                                         
008100 01  FILLER.                                                              
008200*    03  -COPY WDATKORT.                                                  
008300     EJECT                                                                
008400 01  FILLER.                                                              
008500*    03  -COPY W0005  -PRE POSTSUM-.                                      
008600 01  INFIL-TRANSID.                                                       
008700     03  FILLER              PIC X(6)        VALUE 'W51330'.              
008800     03  FILLER              PIC X(8)        VALUE 'W51330D1'.            
008900     03  INFIL-IDPTYP        PIC 9(4)        VALUE ZERO.                  
009000     EJECT                                                                
009100 01  INAREA.                                                              
009200     03  AREA   -COPY W51334   -PRE IN-                                   
009300     EJECT                                                                
009400 01  FILLER               PIC X(16)       VALUE ALL 'HJELP-AREA'.         
009500                                                                          
009600 01  HJELP-AREA.                                                          
009700     03  SIDOR          PIC S9(5)        VALUE  +0 COMP-3.                
009800     03  RADER          PIC S9(5)        VALUE +99 COMP-3.                
009900     SKIP2                                                                
010000 01  RUBRIK1.                                                             
010100     03  FILLER         PIC X(11)                                         
010300         VALUE  ' W51330-003'.                                            
010400     03  FILLER         PIC X(9)      VALUE SPACE.                        
010401     03  FILLER         PIC X(4)      VALUE 'VCCS'.                       
010402     03  FILLER         PIC X(16)      VALUE SPACE.                       
010410     03  FILLER         PIC X(40)                                         
010500         VALUE  'PARTS PUNCHED WITH PHYSICAL DEVIATION   '.               
010600     03  FILLER         PIC X(10)      VALUE SPACE.                       
010700     03  FILLER         PIC X(6)      VALUE 'DC: 11'.                     
010710     03  FILLER         PIC X(4)      VALUE SPACE.                        
010720     03  FILLER         PIC X(6)      VALUE 'DATE: '.                     
010800     03  RUB1-AAR       PIC 99.                                           
010900     03  RUB1-MAN       PIC 99.                                           
011000     03  RUB1-DAG       PIC 99.                                           
011300     SKIP2                                                                
011400 01  RUBRIK2.                                                             
011500     03  FILLER         PIC X(10)     VALUE SPACE.                        
011900     03  RUB2-KATTEXT   PIC X(50).                                        
012000     SKIP2                                                                
012100 01  RUBRIK3.                                                             
012200     03  FILLER         PIC X(18)     VALUE SPACE.                        
012300     03  FILLER         PIC X(50)     VALUE                               
012400        'PART   ADJ.    ADJ.    ADV.   AVIS     AK   STOCK '.             
012500     03  FILLER         PIC X(40)     VALUE                               
012600        '    EFR     INV       P-  DESCRIPTION   '.                       
012700                                                                          
012800 01  RUBRIK4.                                                             
012900     03  FILLER         PIC X(5)      VALUE SPACE.                        
013000     03  FILLER         PIC X(50)     VALUE                               
013100        'PARTNO     ADRESS   DATE    QTY     DATE    QTY   '.             
013200     03  FILLER         PIC X(40)     VALUE                               
013300        ' QTY     QTY     QTY     QTY     CODE   '.                       
013400                                                                          
013500 01  RAD.                                                                 
013600     03  FILLER         PIC X(2)   VALUE SPACE.                           
013700     03  RAD-IDARTNR    PIC Z(9).                                         
013800     03  FILLER         PIC X(2)   VALUE SPACE.                           
013900     03  RAD-ADARTADR   PIC Z(9).                                         
014000     03  RAD-TIJUSTDA   PIC Z(7).                                         
014100     03  RAD-KVJUSTKV   PIC Z(7)-.                                        
014200     03  RAD-TIAVIDAT   PIC Z(8).                                         
014300     03  RAD-KVAVIS     PIC Z(7).                                         
014400     03  RAD-KVAKS      PIC Z(7)-.                                        
014500     03  RAD-KVLS       PIC Z(7)-.                                        
014600     03  RAD-KVEFRS     PIC Z(7)-.                                        
014700     03  RAD-KVUTRS     PIC Z(7)-.                                        
014800     03  FILLER         PIC X(5)     VALUE SPACE.                         
014900     03  RAD-KDPRODSL   PIC Z(3).                                         
015000     03  FILLER         PIC X(2)     VALUE SPACE.                         
015100     03  RAD-BEART      PIC X(25).                                        
015200     EJECT                                                                
015300 PROCEDURE DIVISION.                                                      
015400                                                                          
015500     PERFORM A-INIT                                                       
015600                                                                          
015700     SORT SORTER ON ASCENDING KEY SORT-KDSORT1                            
015800                                  SORT-KDINVKAT                           
015900                                  SORT-ADARTADR                           
016000                                                                          
016100     INPUT  PROCEDURE A-SKAPA-SORTBEGR-LISTA-3                            
016200     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
016300     IF SORT-RETURN > +0                                                  
016400       DISPLAY ' W51330 SORT-FEL '                                        
016500       MOVE +16 TO ABEND-CODE                                             
016600       CALL ABEND USING ABEND-CODE                                        
016700     ELSE                                                                 
016800       PERFORM C-AVSLUTA                                                  
016900       MOVE +0 TO RETURN-CODE                                             
017000     END-IF                                                               
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500                                                                          
017600     OPEN INPUT  W51330                                                   
017700          OUTPUT LISTA3                                                   
017800                                                                          
017900     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
018000     MOVE D-AAR           TO RUB1-AAR                                     
018100     MOVE D-MAANAD        TO RUB1-MAN                                     
018200     MOVE D-DAG           TO RUB1-DAG                                     
018300                                                                          
018400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018500     .                                                                    
018600     EJECT                                                                
018700 A-SKAPA-SORTBEGR-LISTA-3 SECTION.                                        
018800                                                                          
018900     PERFORM S01-LAS-INFIL                                                
019000     PERFORM UNTIL EOF = JA                                               
019100       IF IN-KDINVKAT = +8                                                
019200         MOVE IN-ADARTADR         TO WS-ADARTADR                          
019300         EVALUATE WS-ADARTADR(1:2)                                        
019400           WHEN 10       MOVE 1   TO IN-KDSORT1                           
019500           WHEN 20       MOVE 2   TO IN-KDSORT1                           
019600           WHEN 21       MOVE 2   TO IN-KDSORT1                           
019700           WHEN 40       MOVE 2   TO IN-KDSORT1                           
019800           WHEN 30       MOVE 3   TO IN-KDSORT1                           
019900           WHEN 35       MOVE 3   TO IN-KDSORT1                           
020000           WHEN 71       MOVE 3   TO IN-KDSORT1                           
020100           WHEN 72       MOVE 3   TO IN-KDSORT1                           
020200           WHEN 73       MOVE 3   TO IN-KDSORT1                           
020300           WHEN 90       MOVE 4   TO IN-KDSORT1                           
020400           WHEN 91       MOVE 4   TO IN-KDSORT1                           
020500           WHEN 92       MOVE 4   TO IN-KDSORT1                           
020600           WHEN OTHER    MOVE 5   TO IN-KDSORT1                           
020700         END-EVALUATE                                                     
020800       ELSE                                                               
020900         MOVE 0                   TO IN-KDSORT1                           
021000       END-IF                                                             
021100       PERFORM S02-RELEASE-SORT                                           
021200       PERFORM S01-LAS-INFIL                                              
021300     END-PERFORM                                                          
021400     .                                                                    
021500     EJECT                                                                
021600 B-SKAPA-LISTOR SECTION.                                                  
021700                                                                          
021800     MOVE NEJ    TO EOF                                                   
021900     PERFORM S03-RETURN-SORT                                              
022000     PERFORM UNTIL EOF = JA                                               
022100       PERFORM BB-UTREDNINGSSALDO-LISTA                                   
022200       PERFORM S03-RETURN-SORT                                            
022300     END-PERFORM                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 BB-UTREDNINGSSALDO-LISTA SECTION.                                        
022700                                                                          
022800******************************************************************        
022900*    LISTA3-*  'ARTIKLAR FÖR VILKA UTREDNINGSSALDO UPPDATERATS'  *        
023000******************************************************************        
023100                                                                          
023200     MOVE IN-ADARTADR       TO RAD-ADARTADR                               
023300     MOVE IN-IDARTNR        TO RAD-IDARTNR                                
023400     MOVE IN-TIJUSTDA       TO RAD-TIJUSTDA                               
023500     MOVE IN-KVJUSTKV       TO RAD-KVJUSTKV                               
023600     MOVE IN-TIAVIDAT       TO RAD-TIAVIDAT                               
023700     MOVE IN-KVAVIS         TO RAD-KVAVIS                                 
023800     MOVE IN-KVAKS          TO RAD-KVAKS                                  
023900     MOVE IN-KVLS           TO RAD-KVLS                                   
024000     MOVE IN-KVEFRS         TO RAD-KVEFRS                                 
024100     MOVE IN-KVUTRS         TO RAD-KVUTRS                                 
024200     MOVE IN-KDPRODSL       TO RAD-KDPRODSL                               
024300     MOVE IN-BEART          TO RAD-BEART                                  
024400     MOVE IN-IDDC           TO WS-IDDC                                    
024500*    IF RADER > RADMAX OR                                                 
024600     IF SPAR-KAT     NOT = IN-KDINVKAT OR                                 
024700       SPAR-SORTBGP NOT = IN-KDSORT1                                      
024800       PERFORM BBH-RUBRIKER                                               
024810       MOVE IN-KDINVKAT    TO SPAR-KAT                                    
024900       MOVE IN-KDSORT1     TO SPAR-SORTBGP                                
025000     END-IF                                                               
025100                                                                          
025200     WRITE LISTA-3 FROM RAD AFTER 2                                       
025300*    ADD +2 TO RADER                                                      
025400     .                                                                    
025500     EJECT                                                                
025600 BBH-RUBRIKER SECTION.                                                    
025700******************************************************************        
025800*      GENERERAR RUBRIKER FÖR LISTA3                             *        
025900******************************************************************        
026000*    ADD +1              TO SIDOR                                         
026100*    MOVE SIDOR          TO RUB1-SIDA                                     
026110     IF FIRST-TIME = 'J'                                                  
026200       WRITE LISTA-3 FROM RUBRIK1 AFTER PAGE                              
026300       MOVE 'N' TO FIRST-TIME                                             
026400     END-IF                                                               
026500                                                                          
026600     EVALUATE IN-KDINVKAT                                                 
027300       WHEN +2                                                            
027400         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
027410       WHEN +8                                                            
027420         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
027500       WHEN +11                                                           
027600         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
027700     END-EVALUATE                                                         
027800                                                                          
027900     WRITE LISTA-3 FROM RUBRIK2 AFTER 2                                   
028000     WRITE LISTA-3 FROM RUBRIK3 AFTER 2                                   
028100     WRITE LISTA-3 FROM RUBRIK4 AFTER 1                                   
028200*    MOVE +8 TO RADER                                                     
028300     .                                                                    
028400     EJECT                                                                
028500 C-AVSLUTA  SECTION.                                                      
028600                                                                          
028700     CLOSE W51330                                                         
028800           LISTA3                                                         
028900     MOVE 'S'     TO POSTSUM-OPKOD                                        
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     .                                                                    
029200     SKIP3                                                                
029300 S01-LAS-INFIL SECTION.                                                   
029400                                                                          
029500     READ W51330 INTO IN-AREA                                             
029600     AT END                                                               
029700         MOVE JA TO EOF                                                   
029800     NOT AT END                                                           
029900         MOVE '003'            TO INFIL-IDPTYP                            
030000         MOVE INFIL-TRANSID    TO POSTSUM-TRANSID                         
030100         CALL POSTSUM USING POSTSUM-PARM                                  
030200     END-READ                                                             
030300     .                                                                    
030400     SKIP3                                                                
030500 S02-RELEASE-SORT SECTION.                                                
030600                                                                          
030700     RELEASE SORT-POST FROM IN-AREA                                       
030800     MOVE 'W51330'   TO POSTSUM-FDNAMN                                    
030900     MOVE 'SORTIN'   TO POSTSUM-DDNAMN2                                   
031000     MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                  
031100     CALL POSTSUM USING POSTSUM-PARM                                      
031200     .                                                                    
031300     SKIP3                                                                
031400 S03-RETURN-SORT SECTION.                                                 
031500                                                                          
031600     RETURN SORTER INTO IN-AREA                                           
031700     AT END                                                               
031800       MOVE JA TO EOF                                                     
031900     NOT AT END                                                           
032000       MOVE 'W51330'   TO POSTSUM-FDNAMN                                  
032100       MOVE 'SORTUT'   TO POSTSUM-DDNAMN2                                 
032200       MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                
032300       CALL POSTSUM USING POSTSUM-PARM                                    
032400     END-RETURN                                                           
032500     .                                                                    

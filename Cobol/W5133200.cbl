000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5133200.                                    
000400*AUTHOR.                     KARL JOHAN HANSSON                           
000500*DATE-WRITTEN.               MAJ  1999.                                   
000600*REMARKS.                                                                 
000700*      PROGRAMMET SKAPAR LISTOR TILL SDC EUROPA                           
000800*      INNEHÅLLANDE ARTIKLAR MED 'UTREDNINGSSALDO UPPDATERAT'.            
000900*      (KATEGORI 2, 8 OCH 11)                                             
001000     EJECT                                                                
001100 ENVIRONMENT DIVISION.                                                    
001200                                                                          
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001600     SELECT  W51332          ASSIGN UT-S-W51332D1.                        
001700                                                                          
001800* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
001900     SELECT  LISTA3-21       ASSIGN UT-S-W51332D2.                        
002000     SELECT  LISTA3-23       ASSIGN UT-S-W51332D3.                        
002100     SELECT  LISTA3-24       ASSIGN UT-S-W51332D4.                        
002200     SELECT  LISTA3-25       ASSIGN UT-S-W51332D5.                        
002300     SELECT  LISTA3-26       ASSIGN UT-S-W51332D6.                        
002400     SELECT  LISTA3-91       ASSIGN UT-S-W51332D8.                        
002500     SELECT  LISTA3-91-OND   ASSIGN UT-S-W51332D9.                        
002700* - - - - - - - - - - - - - - - - - - - - - - - - UTFILEN.                
002800     SELECT  UTFIL           ASSIGN UT-S-W51332D7.                        
002900                                                                          
003000* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
003100     SELECT  SORTER          ASSIGN UT-S-W51332DS.                        
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W51332                                                               
003700     RECORDING F                                                          
003800     BLOCK 0 RECORDS.                                                     
003900                                                                          
004000*01  IN-POSTER -COPY W51334    -L.                                        
004100     EJECT                                                                
004200 FD  LISTA3-21                                                            
004300     RECORDING F                                                          
004400     BLOCK 0 RECORDS.                                                     
004500                                                                          
004600 01  LISTA-3-21          PIC X(121).                                      
004700     SKIP2                                                                
004800 FD  LISTA3-23                                                            
004900     RECORDING F                                                          
005000     BLOCK 0 RECORDS.                                                     
005100                                                                          
005200 01  LISTA-3-23          PIC X(121).                                      
005300     SKIP2                                                                
005400 FD  LISTA3-24                                                            
005500     RECORDING F                                                          
005600     BLOCK 0 RECORDS.                                                     
005700                                                                          
005800 01  LISTA-3-24          PIC X(121).                                      
005900     SKIP2                                                                
006000 FD  LISTA3-25                                                            
006100     RECORDING F                                                          
006200     BLOCK 0 RECORDS.                                                     
006300                                                                          
006400 01  LISTA-3-25          PIC X(121).                                      
006500     SKIP2                                                                
006600 FD  LISTA3-26                                                            
006700     RECORDING F                                                          
006800     BLOCK 0 RECORDS.                                                     
006900                                                                          
007000 01  LISTA-3-26          PIC X(121).                                      
007100     EJECT                                                                
007200 FD  UTFIL                                                                
007300     RECORDING F                                                          
007400     BLOCK 0 RECORDS.                                                     
007500                                                                          
007600 01  UTFILEN             PIC X(121).                                      
007700     EJECT                                                                
007800 FD  LISTA3-91                                                            
007900     RECORDING F                                                          
008000     BLOCK 0 RECORDS.                                                     
008100                                                                          
008200 01  LISTA-3-91          PIC X(121).                                      
008300     EJECT                                                                
008400 FD  LISTA3-91-OND                                                        
008500     RECORDING F                                                          
008600     BLOCK 0 RECORDS.                                                     
008700                                                                          
008800 01  LISTA-3-91-OND      PIC X(121).                                      
008900     EJECT                                                                
009600 SD  SORTER.                                                              
009700                                                                          
009800*01  POST     -COPY W51334    -PRE SORT-.                                 
009900     EJECT                                                                
010000 WORKING-STORAGE SECTION.                                                 
010100                                                                          
010300 77  IDPGM                         PIC X(8) VALUE 'W5133200'.             
010400 77  JA                            PIC X    VALUE 'J'.                    
010500 77  NEJ                           PIC X    VALUE 'N'.                    
010600                                                                          
010700 01  W-IDDC                        PIC XX   VALUE SPACE.                  
010800 01  FORSTA-POST                   PIC XX   VALUE 'N'.                    
010900                                                                          
011000 01  W-SUB-PROG.                                                          
011100     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
011200     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
011300     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
011400                                                                          
011500 01  KONSTANTER.                                                          
011600     03  RADMAX          PIC S9(3)   VALUE +42  COMP-3.                   
011700                                                                          
011800 01  KAT-TEXTER.                                                          
011900     03  KATTEXT-2       PIC X(50)   VALUE                                
012000         'TYPE 2, UPDATED ON QUEUE 5302'.                                 
012100     03  KATTEXT-8       PIC X(50)   VALUE                                
012200         'TYPE 8, ALREADY ADJUSTED'.                                      
012300     03  KATTEXT-11      PIC X(50)   VALUE                                
012400         '        PARTS ALREADY IN QUEUE 5302'.                           
012500                                                                          
012600 01  VARIABLER.                                                           
012700     03  SPAR-SORTBGP-21 PIC 9       VALUE 0.                             
012800     03  SPAR-SORTBGP-23 PIC 9       VALUE 0.                             
012900     03  SPAR-SORTBGP-24 PIC 9       VALUE 0.                             
013000     03  SPAR-SORTBGP-25 PIC 9       VALUE 0.                             
013100     03  SPAR-SORTBGP-26 PIC 9       VALUE 0.                             
013200     03  SPAR-SORTBGP-91 PIC 9       VALUE 0.                             
013400     03  WS-ADARTADR     PIC 9(9)    VALUE 0.                             
013500     03  EOF             PIC X       VALUE 'N'.                           
013600     03  SPAR-KAT-21     PIC S9(3)   COMP-3 VALUE +0.                     
013700     03  SPAR-KAT-23     PIC S9(3)   COMP-3 VALUE +0.                     
013800     03  SPAR-KAT-24     PIC S9(3)   COMP-3 VALUE +0.                     
013900     03  SPAR-KAT-25     PIC S9(3)   COMP-3 VALUE +0.                     
014000     03  SPAR-KAT-26     PIC S9(3)   COMP-3 VALUE +0.                     
014100     03  SPAR-KAT-91     PIC S9(3)   COMP-3 VALUE +0.                     
014300                                                                          
014400 01  SUBPROGRAM.                                                          
014500     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
014600     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
014700     EJECT                                                                
014800*   --- VALID IDDC CODES                                                  
014900*                                                                         
015000*01  -COPY WWDC99                                                         
015100*                                                                         
015200 01  FILLER.                                                              
015300*    03  -COPY WDATKORT.                                                  
015400     EJECT                                                                
015500 01  FILLER.                                                              
015600*    03  -COPY W0005  -PRE POSTSUM-.                                      
015700 01  INFIL-TRANSID.                                                       
015800     03  FILLER              PIC X(6)        VALUE 'W51332'.              
015900     03  FILLER              PIC X(8)        VALUE 'W51332D1'.            
016000     03  INFIL-IDPTYP        PIC 9(4)        VALUE ZERO.                  
016100     EJECT                                                                
016200 01  WORK-AREA.                                                           
016300     03  AREA   -COPY W51334   -PRE ARB-                                  
016400     EJECT                                                                
016500 01  FILLER               PIC X(16)       VALUE ALL 'HJELP-AREA'.         
016600                                                                          
016700 01  HJELP-AREA.                                                          
016800     03  SIDOR-21       PIC S9(5)        VALUE  +0 COMP-3.                
016900     03  SIDOR-23       PIC S9(5)        VALUE  +0 COMP-3.                
017000     03  SIDOR-24       PIC S9(5)        VALUE  +0 COMP-3.                
017100     03  SIDOR-25       PIC S9(5)        VALUE  +0 COMP-3.                
017200     03  SIDOR-26       PIC S9(5)        VALUE  +0 COMP-3.                
017300     03  SIDOR-91       PIC S9(5)        VALUE  +0 COMP-3.                
017500     03  RADER-21       PIC S9(5)        VALUE +99 COMP-3.                
017600     03  RADER-23       PIC S9(5)        VALUE +99 COMP-3.                
017700     03  RADER-24       PIC S9(5)        VALUE +99 COMP-3.                
017800     03  RADER-25       PIC S9(5)        VALUE +99 COMP-3.                
017900     03  RADER-26       PIC S9(5)        VALUE +99 COMP-3.                
018000     03  RADER-91       PIC S9(5)        VALUE +99 COMP-3.                
018200     SKIP2                                                                
018300 01  RUBRIK-ONDEMAND.                                                     
018400     03  RUBON1-FILLER  PIC X(9)  VALUE                                   
018500                                ' W51332-3'.                              
018600     03  RUBON1-IDDC1   PIC XX              VALUE SPACE.                  
018700     03  FILLER         PIC X(9)            VALUE SPACE.                  
018800     03  FILLER         PIC X(4)      VALUE 'VCCS'.                       
018900     03  FILLER         PIC X(16)           VALUE SPACE.                  
019000     03  FILLER         PIC X(40)                                         
019100         VALUE  'PARTS PUNCHED WITH PHYSICAL DEVIATION  '.                
019200     03  FILLER         PIC X(10)           VALUE SPACE.                  
019300     03  FILLER         PIC X(4)      VALUE 'DC: '.                       
019400     03  RUBON1-IDDC2   PIC X(2).                                         
019500     03  FILLER         PIC X(4)            VALUE SPACE.                  
019600     03  FILLER         PIC X(6)      VALUE 'DATE: '.                     
019700     03  RUBON1-AAR     PIC 99.                                           
019800     03  RUBON1-MAN     PIC 99.                                           
019900     03  RUBON1-DAG     PIC 99.                                           
020000                                                                          
020100 01  RUBRIK1.                                                             
020200     03  FILLER         PIC X(2)            VALUE SPACE.                  
020300     03  FILLER         PIC X(31)                                         
020400         VALUE  'VOLVO CAR PARTS        W51332-3'.                        
020500     03  RUB1-IDDC1     PIC XX              VALUE SPACE.                  
020600     03  FILLER         PIC X(39)                                         
020700         VALUE  '  PARTS PUNCHED WITH PHYSICAL DEVIATION'.                
020800     03  FILLER         PIC X(2)      VALUE SPACE.                        
020900     03  FILLER         PIC X(3)      VALUE 'DC'.                         
021000     03  RUB1-IDDC2     PIC X(3).                                         
021100     03  RUB1-AAR       PIC B99.                                          
021200     03  RUB1-MAN       PIC B99.                                          
021300     03  RUB1-DAG       PIC B99.                                          
021400     03  FILLER         PIC X(7)      VALUE '  PAGE '.                    
021500     03  RUB1-SIDA      PIC Z(4).                                         
021600                                                                          
021700 01  RUBRIK2.                                                             
021800     03  FILLER         PIC X(10)     VALUE SPACE.                        
021900     03  RUB2-KATTEXT   PIC X(50).                                        
022000                                                                          
022100 01  RUBRIK3.                                                             
022200     03  FILLER         PIC X(18)     VALUE SPACE.                        
022300     03  FILLER         PIC X(50)     VALUE                               
022400        'PART   ADJ.    ADJ.    ADV.   AVIS     AK   STOCK '.             
022500     03  FILLER         PIC X(40)     VALUE                               
022600        '    EFR     INV       P-  DESCRIPTION   '.                       
022700                                                                          
022800 01  RUBRIK4.                                                             
022900     03  FILLER         PIC X(5)      VALUE SPACE.                        
023000     03  FILLER         PIC X(50)     VALUE                               
023100        'PARTNO     ADRESS   DATE    QTY     DATE    QTY   '.             
023200     03  FILLER         PIC X(40)     VALUE                               
023300        ' QTY     QTY     QTY     QTY     CODE   '.                       
023400                                                                          
023500 01  RAD.                                                                 
023600     03  FILLER         PIC X(2)   VALUE SPACE.                           
023700     03  RAD-IDARTNR    PIC Z(9).                                         
023800     03  FILLER         PIC X(2)   VALUE SPACE.                           
023900     03  RAD-ADARTADR   PIC Z(9).                                         
024000     03  RAD-TIJUSTDA   PIC Z(7).                                         
024100     03  RAD-KVJUSTKV   PIC Z(7)-.                                        
024200     03  RAD-TIAVIDAT   PIC Z(8).                                         
024300     03  RAD-KVAVIS     PIC Z(7).                                         
024400     03  RAD-KVAKS      PIC Z(7)-.                                        
024500     03  RAD-KVLS       PIC Z(7)-.                                        
024600     03  RAD-KVEFRS     PIC Z(7)-.                                        
024700     03  RAD-KVUTRS     PIC Z(7)-.                                        
024800     03  FILLER         PIC X(5)     VALUE SPACE.                         
024900     03  RAD-KDPRODSL   PIC Z(3).                                         
025000     03  FILLER         PIC X(2)     VALUE SPACE.                         
025100     03  RAD-BEART      PIC X(25).                                        
025200                                                                          
025300 01  BLANKRAD           PIC X        VALUE SPACE.                         
025400     EJECT                                                                
025500 PROCEDURE DIVISION.                                                      
025600                                                                          
025700     PERFORM A-INIT                                                       
025800                                                                          
025900     SORT SORTER ON ASCENDING KEY SORT-IDDC                               
026000                                  SORT-KDSORT1                            
026100                                  SORT-KDINVKAT                           
026200                                  SORT-ADARTADR                           
026300                                                                          
026400     INPUT  PROCEDURE A-SKAPA-SORTBEGR-LISTA-3                            
026500     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
026600     IF SORT-RETURN > +0                                                  
026700       DISPLAY ' W51332 SORT-FEL '                                        
026800       MOVE +16 TO ABEND-CODE                                             
026900       CALL ABEND USING ABEND-CODE                                        
027000     ELSE                                                                 
027100       PERFORM C-AVSLUTA                                                  
027200       MOVE +0 TO RETURN-CODE                                             
027300     END-IF                                                               
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800                                                                          
027900     OPEN INPUT  W51332                                                   
028000          OUTPUT LISTA3-21                                                
028100                 LISTA3-23                                                
028200                 LISTA3-24                                                
028300                 LISTA3-25                                                
028400                 LISTA3-26                                                
028500                 UTFIL                                                    
028600                 LISTA3-91                                                
028700                 LISTA3-91-OND                                            
028900                                                                          
029000     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
029100     MOVE D-AAR           TO RUB1-AAR                                     
029200     MOVE D-MAANAD        TO RUB1-MAN                                     
029300     MOVE D-DAG           TO RUB1-DAG                                     
029400     MOVE D-AAR           TO RUBON1-AAR                                   
029500     MOVE D-MAANAD        TO RUBON1-MAN                                   
029600     MOVE D-DAG           TO RUBON1-DAG                                   
029700                                                                          
029800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029900     .                                                                    
030000     EJECT                                                                
030100 A-SKAPA-SORTBEGR-LISTA-3 SECTION.                                        
030200                                                                          
030300     PERFORM S01-LAS-INFIL                                                
030400     PERFORM UNTIL EOF = JA                                               
030500       IF ARB-KDINVKAT = +8                                               
030600         MOVE ARB-ADARTADR        TO WS-ADARTADR                          
030700         EVALUATE WS-ADARTADR(1:2)                                        
030800           WHEN 10       MOVE 1   TO ARB-KDSORT1                          
030900           WHEN 20       MOVE 2   TO ARB-KDSORT1                          
031000           WHEN 21       MOVE 2   TO ARB-KDSORT1                          
031100           WHEN 40       MOVE 2   TO ARB-KDSORT1                          
031200           WHEN 30       MOVE 3   TO ARB-KDSORT1                          
031300           WHEN 35       MOVE 3   TO ARB-KDSORT1                          
031400           WHEN 71       MOVE 3   TO ARB-KDSORT1                          
031500           WHEN 72       MOVE 3   TO ARB-KDSORT1                          
031600           WHEN 73       MOVE 3   TO ARB-KDSORT1                          
031700           WHEN 90       MOVE 4   TO ARB-KDSORT1                          
031800           WHEN 91       MOVE 4   TO ARB-KDSORT1                          
031900           WHEN 92       MOVE 4   TO ARB-KDSORT1                          
032000           WHEN OTHER    MOVE 5   TO ARB-KDSORT1                          
032100         END-EVALUATE                                                     
032200       ELSE                                                               
032300         MOVE 0                   TO ARB-KDSORT1                          
032400       END-IF                                                             
032500       PERFORM S02-RELEASE-SORT                                           
032600       PERFORM S01-LAS-INFIL                                              
032700     END-PERFORM                                                          
032800     .                                                                    
032900     EJECT                                                                
033000 B-SKAPA-LISTOR SECTION.                                                  
033100                                                                          
033200     MOVE NEJ    TO EOF                                                   
033300     PERFORM S03-RETURN-SORT                                              
033400     PERFORM UNTIL EOF = JA                                               
033500       PERFORM BB-UTREDNINGSSALDO-LISTA                                   
033600       PERFORM S03-RETURN-SORT                                            
033700     END-PERFORM                                                          
033800     .                                                                    
033900     EJECT                                                                
034000 BB-UTREDNINGSSALDO-LISTA SECTION.                                        
034100                                                                          
034200******************************************************************        
034300*    LISTA3-*  'ARTIKLAR FÖR VILKA UTREDNINGSSALDO UPPDATERATS'  *        
034400******************************************************************        
034500                                                                          
034600     MOVE ARB-ADARTADR      TO RAD-ADARTADR                               
034700                               WS-ADARTADR                                
034800     MOVE ARB-IDARTNR       TO RAD-IDARTNR                                
034900     MOVE ARB-TIJUSTDA      TO RAD-TIJUSTDA                               
035000     MOVE ARB-KVJUSTKV      TO RAD-KVJUSTKV                               
035100     MOVE ARB-TIAVIDAT      TO RAD-TIAVIDAT                               
035200     MOVE ARB-KVAVIS        TO RAD-KVAVIS                                 
035300     MOVE ARB-KVAKS         TO RAD-KVAKS                                  
035400     MOVE ARB-KVLS          TO RAD-KVLS                                   
035500     MOVE ARB-KVEFRS        TO RAD-KVEFRS                                 
035600     MOVE ARB-KVUTRS        TO RAD-KVUTRS                                 
035700     MOVE ARB-KDPRODSL      TO RAD-KDPRODSL                               
035800     MOVE ARB-BEART         TO RAD-BEART                                  
035900     MOVE ARB-IDDC          TO WS-IDDC                                    
036000                                                                          
036100     IF W-IDDC NOT = ARB-IDDC                                             
036200       MOVE 'J'             TO FORSTA-POST                                
036300       MOVE ARB-IDDC        TO W-IDDC                                     
036400     ELSE                                                                 
036500       MOVE 'N'             TO FORSTA-POST                                
036600     END-IF                                                               
036700                                                                          
036800     EVALUATE TRUE                                                        
036900       WHEN SDC-NL                                                        
038000         IF RADER-21 > RADMAX OR                                          
038100            SPAR-KAT-21     NOT = ARB-KDINVKAT OR                         
038200            SPAR-SORTBGP-21 NOT = ARB-KDSORT1                             
038300            PERFORM BBH-RUBRIKER-21                                       
038400            MOVE ARB-KDINVKAT   TO SPAR-KAT-21                            
038500            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-21                        
038600         END-IF                                                           
038700                                                                          
039400         WRITE LISTA-3-21 FROM RAD AFTER 1                                
039500         WRITE UTFILEN    FROM RAD AFTER 1                                
039600         ADD +1 TO RADER-21                                               
040700                                                                          
040800         WRITE LISTA-3-23 FROM RAD AFTER 1                                
040900         WRITE UTFILEN    FROM RAD AFTER 1                                
041000         ADD +1 TO RADER-23                                               
042400       WHEN SDC-ES                                                        
042500         IF RADER-24 > RADMAX OR                                          
042600            SPAR-KAT-24     NOT = ARB-KDINVKAT OR                         
042700            SPAR-SORTBGP-24 NOT = ARB-KDSORT1                             
042800            PERFORM BBJ-RUBRIKER-24                                       
042900            MOVE ARB-KDINVKAT   TO SPAR-KAT-24                            
043000            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-24                        
043100         END-IF                                                           
043200                                                                          
043300         WRITE LISTA-3-24 FROM RAD AFTER 1                                
043400         WRITE UTFILEN    FROM RAD AFTER 1                                
043500         ADD +1 TO RADER-24                                               
043600       WHEN SDC-IT                                                        
043700         IF RADER-25 > RADMAX OR                                          
043800            SPAR-KAT-25     NOT = ARB-KDINVKAT OR                         
043900            SPAR-SORTBGP-25 NOT = ARB-KDSORT1                             
044000            PERFORM BBK-RUBRIKER-25                                       
044100            MOVE ARB-KDINVKAT   TO SPAR-KAT-25                            
044200            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-25                        
044300         END-IF                                                           
044400                                                                          
044500         WRITE LISTA-3-25 FROM RAD AFTER 1                                
044600         WRITE UTFILEN    FROM RAD AFTER 1                                
044700         ADD +1 TO RADER-25                                               
044800       WHEN SDC-AT                                                        
044900         IF RADER-26 > RADMAX OR                                          
045000            SPAR-KAT-26     NOT = ARB-KDINVKAT OR                         
045100            SPAR-SORTBGP-26 NOT = ARB-KDSORT1                             
045200            PERFORM BBL-RUBRIKER-26                                       
045300            MOVE ARB-KDINVKAT   TO SPAR-KAT-26                            
045400            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-26                        
045500         END-IF                                                           
045600                                                                          
045700         WRITE LISTA-3-26 FROM RAD AFTER 1                                
045800         WRITE UTFILEN    FROM RAD AFTER 1                                
045900         ADD +1 TO RADER-26                                               
045910       WHEN SDC-NL-ET                                                     
045940         IF RADER-91 > RADMAX OR                                          
045950            SPAR-KAT-91     NOT = ARB-KDINVKAT OR                         
045960            SPAR-SORTBGP-91 NOT = ARB-KDSORT1                             
045970            PERFORM BBH-RUBRIKER-91                                       
045980            MOVE ARB-KDINVKAT   TO SPAR-KAT-91                            
045990            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-91                        
045991         END-IF                                                           
045992                                                                          
046003         WRITE LISTA-3-91 FROM RAD AFTER 1                                
046004         WRITE LISTA-3-91-OND FROM RAD AFTER 1                            
046005         ADD +1 TO RADER-91                                               
046020     END-EVALUATE                                                         
046100     .                                                                    
046200     EJECT                                                                
046300 BBH-RUBRIKER-21 SECTION.                                                 
046400******************************************************************        
046500*      GENERERAR RUBRIKER FÖR LISTA3 SDC 21                      *        
046600******************************************************************        
046700     ADD +1              TO SIDOR-21                                      
046800     MOVE SIDOR-21       TO RUB1-SIDA                                     
046900     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
047000                            RUB1-IDDC2                                    
047100     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
047200                            RUBON1-IDDC2                                  
047300     WRITE LISTA-3-21 FROM RUBRIK1 AFTER PAGE                             
047400     IF FORSTA-POST = 'J'                                                 
047500       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
047600     END-IF                                                               
047700                                                                          
047800     EVALUATE ARB-KDINVKAT                                                
047900       WHEN +2                                                            
048000         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
048100       WHEN +8                                                            
048200         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
048300       WHEN +11                                                           
048400         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
048500     END-EVALUATE                                                         
048600                                                                          
048700     WRITE LISTA-3-21 FROM RUBRIK2 AFTER 2                                
048800     WRITE LISTA-3-21 FROM RUBRIK3 AFTER 2                                
048900     WRITE LISTA-3-21 FROM RUBRIK4 AFTER 1                                
049000     WRITE LISTA-3-21 FROM BLANKRAD    AFTER 1                            
049100     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
049200     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
049300     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
049400     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
049500     MOVE +9 TO RADER-21                                                  
049600     .                                                                    
049700     EJECT                                                                
049800 BBI-RUBRIKER-23 SECTION.                                                 
049900******************************************************************        
050000*      GENERERAR RUBRIKER FÖR LISTA3 SDC 23                      *        
050100******************************************************************        
050200     ADD +1              TO SIDOR-23                                      
050300     MOVE SIDOR-23       TO RUB1-SIDA                                     
050400     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
050500                            RUB1-IDDC2                                    
050600     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
050700                            RUBON1-IDDC2                                  
050800     WRITE LISTA-3-23 FROM RUBRIK1 AFTER PAGE                             
050900     IF FORSTA-POST = 'J'                                                 
051000       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
051100     END-IF                                                               
051200                                                                          
051300     EVALUATE ARB-KDINVKAT                                                
051400       WHEN +2                                                            
051500         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
051600       WHEN +8                                                            
051700         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
051800       WHEN +11                                                           
051900         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
052000     END-EVALUATE                                                         
052100                                                                          
052200     WRITE LISTA-3-23 FROM RUBRIK2 AFTER 2                                
052300     WRITE LISTA-3-23 FROM RUBRIK3 AFTER 2                                
052400     WRITE LISTA-3-23 FROM RUBRIK4 AFTER 1                                
052500     WRITE LISTA-3-23 FROM BLANKRAD    AFTER 1                            
052600     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
052700     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
052800     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
052900     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
053000     MOVE +9 TO RADER-23                                                  
053100     .                                                                    
053200     EJECT                                                                
053300 BBJ-RUBRIKER-24 SECTION.                                                 
053400******************************************************************        
053500*      GENERERAR RUBRIKER FÖR LISTA3 SDC 24                      *        
053600******************************************************************        
053700     ADD +1              TO SIDOR-24                                      
053800     MOVE SIDOR-24       TO RUB1-SIDA                                     
053900     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
054000                            RUB1-IDDC2                                    
054100     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
054200                            RUBON1-IDDC2                                  
054300     WRITE LISTA-3-24 FROM RUBRIK1 AFTER PAGE                             
054400     IF FORSTA-POST = 'J'                                                 
054500       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
054600     END-IF                                                               
054700                                                                          
054800     EVALUATE ARB-KDINVKAT                                                
054900       WHEN +2                                                            
055000         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
055100       WHEN +8                                                            
055200         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
055300       WHEN +11                                                           
055400         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
055500     END-EVALUATE                                                         
055600                                                                          
055700     WRITE LISTA-3-24 FROM RUBRIK2 AFTER 2                                
055800     WRITE LISTA-3-24 FROM RUBRIK3 AFTER 2                                
055900     WRITE LISTA-3-24 FROM RUBRIK4 AFTER 1                                
056000     WRITE LISTA-3-24 FROM BLANKRAD    AFTER 1                            
056100     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
056200     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
056300     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
056400     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
056500     MOVE +9 TO RADER-24                                                  
056600     .                                                                    
056700     EJECT                                                                
056800 BBK-RUBRIKER-25 SECTION.                                                 
056900******************************************************************        
057000*      GENERERAR RUBRIKER FÖR LISTA3 SDC 25                      *        
057100******************************************************************        
057200     ADD +1              TO SIDOR-25                                      
057300     MOVE SIDOR-25       TO RUB1-SIDA                                     
057400     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
057500                            RUB1-IDDC2                                    
057600     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
057700                            RUBON1-IDDC2                                  
057800     WRITE LISTA-3-25 FROM RUBRIK1 AFTER PAGE                             
057900     IF FORSTA-POST = 'J'                                                 
058000       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
058100     END-IF                                                               
058200                                                                          
058300     EVALUATE ARB-KDINVKAT                                                
058400       WHEN +2                                                            
058500         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
058600       WHEN +8                                                            
058700         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
058800       WHEN +11                                                           
058900         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
059000     END-EVALUATE                                                         
059100                                                                          
059200     WRITE LISTA-3-25 FROM RUBRIK2 AFTER 2                                
059300     WRITE LISTA-3-25 FROM RUBRIK3 AFTER 2                                
059400     WRITE LISTA-3-25 FROM RUBRIK4 AFTER 1                                
059500     WRITE LISTA-3-25 FROM BLANKRAD    AFTER 1                            
059600     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
059700     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
059800     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
059900     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
060000     MOVE +9 TO RADER-25                                                  
060100     .                                                                    
060200     EJECT                                                                
060300 BBL-RUBRIKER-26 SECTION.                                                 
060400******************************************************************        
060500*      GENERERAR RUBRIKER FÖR LISTA3 SDC 26                      *        
060600******************************************************************        
060700     ADD +1              TO SIDOR-26                                      
060800     MOVE SIDOR-26       TO RUB1-SIDA                                     
060900     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
061000                            RUB1-IDDC2                                    
061100     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
061200                            RUBON1-IDDC2                                  
061300     WRITE LISTA-3-26 FROM RUBRIK1 AFTER PAGE                             
061400     IF FORSTA-POST = 'J'                                                 
061500       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
061600     END-IF                                                               
061700                                                                          
061800     EVALUATE ARB-KDINVKAT                                                
061900       WHEN +2                                                            
062000         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
062100       WHEN +8                                                            
062200         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
062300       WHEN +11                                                           
062400         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
062500     END-EVALUATE                                                         
062600                                                                          
062700     WRITE LISTA-3-26 FROM RUBRIK2 AFTER 2                                
062800     WRITE LISTA-3-26 FROM RUBRIK3 AFTER 2                                
062900     WRITE LISTA-3-26 FROM RUBRIK4 AFTER 1                                
063000     WRITE LISTA-3-26 FROM BLANKRAD    AFTER 1                            
063100     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
063200     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
063300     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
063400     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
063500     MOVE +9 TO RADER-26                                                  
063600     .                                                                    
063700     EJECT                                                                
063800 BBH-RUBRIKER-91 SECTION.                                                 
063900******************************************************************        
064000*      GENERERAR RUBRIKER FÖR LISTA3 SDC 91                      *        
064100******************************************************************        
064200     ADD +1              TO SIDOR-91                                      
064300     MOVE SIDOR-91       TO RUB1-SIDA                                     
064400     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
064500     MOVE 'ET'           TO RUB1-IDDC2                                    
064600     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
064700     MOVE 'ET'           TO RUBON1-IDDC2                                  
064800     WRITE LISTA-3-91 FROM RUBRIK1 AFTER PAGE                             
064900     WRITE LISTA-3-91-OND FROM RUBRIK-ONDEMAND AFTER PAGE                 
065000                                                                          
065100     EVALUATE ARB-KDINVKAT                                                
065200       WHEN +2                                                            
065300         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
065400       WHEN +8                                                            
065500         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
065600       WHEN +11                                                           
065700         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
065800     END-EVALUATE                                                         
065900                                                                          
066000     WRITE LISTA-3-91 FROM RUBRIK2 AFTER 2                                
066100     WRITE LISTA-3-91 FROM RUBRIK3 AFTER 2                                
066200     WRITE LISTA-3-91 FROM RUBRIK4 AFTER 1                                
066300     WRITE LISTA-3-91 FROM BLANKRAD    AFTER 1                            
066400     WRITE LISTA-3-91-OND FROM RUBRIK2 AFTER 2                            
066500     WRITE LISTA-3-91-OND FROM RUBRIK3 AFTER 2                            
066600     WRITE LISTA-3-91-OND FROM RUBRIK4 AFTER 1                            
066700     WRITE LISTA-3-91-OND FROM BLANKRAD    AFTER 1                        
066800     MOVE +9 TO RADER-91                                                  
066900     .                                                                    
070500     EJECT                                                                
070600 C-AVSLUTA  SECTION.                                                      
070700                                                                          
070800     CLOSE W51332                                                         
070900           LISTA3-21                                                      
071000           LISTA3-23                                                      
071100           LISTA3-24                                                      
071200           LISTA3-25                                                      
071300           LISTA3-26                                                      
071400           UTFIL                                                          
071500           LISTA3-91                                                      
071600           LISTA3-91-OND                                                  
071800     MOVE 'S'     TO POSTSUM-OPKOD                                        
071900     CALL POSTSUM USING POSTSUM-PARM                                      
072000     .                                                                    
072100     SKIP3                                                                
072200 S01-LAS-INFIL SECTION.                                                   
072300                                                                          
072400     READ W51332 INTO ARB-AREA                                            
072500     AT END                                                               
072600         MOVE JA TO EOF                                                   
072700     NOT AT END                                                           
072800         MOVE '003'            TO INFIL-IDPTYP                            
072900         MOVE INFIL-TRANSID    TO POSTSUM-TRANSID                         
073000         CALL POSTSUM USING POSTSUM-PARM                                  
073100     END-READ                                                             
073200     .                                                                    
073300     SKIP3                                                                
073400 S02-RELEASE-SORT SECTION.                                                
073500                                                                          
073600     RELEASE SORT-POST FROM ARB-AREA                                      
073700     MOVE 'W51332'   TO POSTSUM-FDNAMN                                    
073800     MOVE 'SORTIN'   TO POSTSUM-DDNAMN2                                   
073900     MOVE ARB-IDDC   TO POSTSUM-TRANSTYP                                  
074000     CALL POSTSUM USING POSTSUM-PARM                                      
074100     .                                                                    
074200     SKIP3                                                                
074300 S03-RETURN-SORT SECTION.                                                 
074400                                                                          
074500     RETURN SORTER INTO ARB-AREA                                          
074600     AT END                                                               
074700       MOVE JA TO EOF                                                     
074800     NOT AT END                                                           
074900       MOVE 'W51332'   TO POSTSUM-FDNAMN                                  
075000       MOVE 'SORTUT'   TO POSTSUM-DDNAMN2                                 
075100       MOVE ARB-IDDC   TO POSTSUM-TRANSTYP                                
075200       CALL POSTSUM USING POSTSUM-PARM                                    
075300     END-RETURN                                                           
075400     .                                                                    

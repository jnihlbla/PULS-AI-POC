000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5133400.                                    
000400*AUTHOR.                     KARL JOHAN HANSSON                           
000500*DATE-WRITTEN.               MAJ  1999.                                   
000600*REMARKS.                                                                 
000700*      PROGRAMMET SKAPAR LISTOR TILL JAPAN OCH AUSTRALIEN                 
001300*      INNEHÅLLANDE ARTIKLAR MED 'UTREDNINGSSALDO UPPDATERAT'.            
001400*      (KATEGORI 2, 8 OCH 11)                                             
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
003000     SELECT  W51334          ASSIGN UT-S-W51334D1.                        
003100                                                                          
003200* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
003800     SELECT  LISTA3-61       ASSIGN UT-S-W51334D2.                        
003900     SELECT  LISTA3-62       ASSIGN UT-S-W51334D3.                        
004800                                                                          
004801* - - - - - - - - - - - - - - - - - - - - - - - - UTFIL.                  
004810     SELECT  UTFIL           ASSIGN UT-S-W51334D4.                        
004820                                                                          
004900* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
005000     SELECT  SORTER          ASSIGN UT-S-W51334DS.                        
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300 FILE SECTION.                                                            
005400     SKIP3                                                                
005500 FD  W51334                                                               
005600     RECORDING F                                                          
005700     BLOCK 0 RECORDS.                                                     
005800                                                                          
005900*01  IN-POSTER -COPY W51334    -L.                                        
006000     EJECT                                                                
009000     SKIP2                                                                
009100 FD  LISTA3-61                                                            
009200     RECORDING F                                                          
009300     BLOCK 0 RECORDS.                                                     
009400                                                                          
009500 01  LISTA-3-61          PIC X(121).                                      
009600     SKIP2                                                                
009700 FD  LISTA3-62                                                            
009800     RECORDING F                                                          
009900     BLOCK 0 RECORDS.                                                     
010000                                                                          
010100 01  LISTA-3-62          PIC X(121).                                      
015000     EJECT                                                                
015010 FD  UTFIL                                                                
015020     RECORDING F                                                          
015030     BLOCK 0 RECORDS.                                                     
015040                                                                          
015050 01  UTFILEN             PIC X(121).                                      
015060     EJECT                                                                
015100 SD  SORTER.                                                              
015200                                                                          
015300*01  POST     -COPY W51334    -PRE SORT-.                                 
015400     EJECT                                                                
015500 WORKING-STORAGE SECTION.                                                 
015600                                                                          
015700*    -- CHECKED BY WY2000                                                 
015800 77  IDPGM                         PIC X(8) VALUE 'W5133400'.             
015900 77  JA                            PIC X    VALUE 'J'.                    
016000 77  NEJ                           PIC X    VALUE 'N'.                    
016100                                                                          
016110 01  W-IDDC                        PIC XX   VALUE SPACE.                  
016120 01  FORSTA-POST                   PIC XX   VALUE 'N'.                    
016130                                                                          
016200 01  W-SUB-PROG.                                                          
016300     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
016400     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
016500     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
016600                                                                          
016700 01  KONSTANTER.                                                          
017000     03  RADMAX          PIC S9(3)   VALUE +42  COMP-3.                   
017100                                                                          
017200 01  KAT-TEXTER.                                                          
017300     03  KATTEXT-2       PIC X(50)   VALUE                                
017400         'TYPE 2, UPDATED ON QUEUE 5302'.                                 
017500     03  KATTEXT-8       PIC X(50)   VALUE                                
017600         'TYPE 8, ALREADY ADJUSTED'.                                      
017700     03  KATTEXT-11      PIC X(50)   VALUE                                
017800         '        PARTS ALREADY IN QUEUE 5302'.                           
018100                                                                          
018200 01  VARIABLER.                                                           
018800     03  SPAR-SORTBGP-61 PIC 9       VALUE 0.                             
018900     03  SPAR-SORTBGP-62 PIC 9       VALUE 0.                             
019000     03  WS-ADARTADR     PIC 9(9)    VALUE 0.                             
019100     03  EOF             PIC X       VALUE 'N'.                           
019700     03  SPAR-KAT-61     PIC S9(3)   COMP-3 VALUE +0.                     
019800     03  SPAR-KAT-62     PIC S9(3)   COMP-3 VALUE +0.                     
020000                                                                          
020100 01  SUBPROGRAM.                                                          
020200     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
020300     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
020400     EJECT                                                                
020410*   --- VALID IDDC CODES                                                  
020420*                                                                         
020430*01  -COPY WWDC99                                                         
020440*                                                                         
020500 01  FILLER.                                                              
020600*    03  -COPY WDATKORT.                                                  
020700     EJECT                                                                
020800 01  FILLER.                                                              
020900*    03  -COPY W0005  -PRE POSTSUM-.                                      
021000 01  INFIL-TRANSID.                                                       
021100     03  FILLER              PIC X(6)        VALUE 'W51334'.              
021200     03  FILLER              PIC X(8)        VALUE 'W51334D1'.            
021300     03  INFIL-IDPTYP        PIC 9(4)        VALUE ZERO.                  
021400     EJECT                                                                
021500 01  WORK-AREA.                                                           
021600     03  AREA   -COPY W51334   -PRE ARB-                                  
021700     EJECT                                                                
022400 01  FILLER               PIC X(16)       VALUE ALL 'HJELP-AREA'.         
022500                                                                          
022600 01  HJELP-AREA.                                                          
023700     03  SIDOR-61       PIC S9(5)        VALUE  +0 COMP-3.                
023800     03  RADER-61       PIC S9(5)        VALUE +99 COMP-3.                
023900     03  SIDOR-62       PIC S9(5)        VALUE  +0 COMP-3.                
024000     03  RADER-62       PIC S9(5)        VALUE +99 COMP-3.                
024100     SKIP2                                                                
024110 01  RUBRIK-ONDEMAND.                                                     
024120     03  RUBON1-FILLER  PIC X(9)  VALUE                                   
024130                                ' W51334-3'.                              
024140     03  RUBON1-IDDC1   PIC XX              VALUE SPACE.                  
024150     03  FILLER         PIC X(9)            VALUE SPACE.                  
024160     03  FILLER         PIC X(4)      VALUE 'VCCS'.                       
024170     03  FILLER         PIC X(16)           VALUE SPACE.                  
024180     03  FILLER         PIC X(40)                                         
024190         VALUE  'PARTS PUNCHED WITH PHYSICAL DEVIATION  '.                
024191     03  FILLER         PIC X(10)           VALUE SPACE.                  
024192     03  FILLER         PIC X(4)      VALUE 'DC: '.                       
024193     03  RUBON1-IDDC2   PIC X(2).                                         
024194     03  FILLER         PIC X(4)            VALUE SPACE.                  
024195     03  FILLER         PIC X(6)      VALUE 'DATE: '.                     
024196     03  RUBON1-AAR     PIC 99.                                           
024197     03  RUBON1-MAN     PIC 99.                                           
024198     03  RUBON1-DAG     PIC 99.                                           
024199                                                                          
024200 01  RUBRIK1.                                                             
024300     03  FILLER         PIC X(2)            VALUE SPACE.                  
024400     03  FILLER         PIC X(31)                                         
024500         VALUE  'VOLVO CAR PARTS        W51334-3'.                        
024600     03  RUB1-IDDC1     PIC XX              VALUE SPACE.                  
024700     03  FILLER         PIC X(39)                                         
024800         VALUE  '  PARTS PUNCHED WITH PHYSICAL DEVIATION'.                
024900     03  FILLER         PIC X(2)      VALUE SPACE.                        
025000     03  FILLER         PIC X(3)      VALUE 'DC'.                         
025100     03  RUB1-IDDC2     PIC X(3).                                         
025200     03  RUB1-AAR       PIC B99.                                          
025300     03  RUB1-MAN       PIC B99.                                          
025400     03  RUB1-DAG       PIC B99.                                          
025500     03  FILLER         PIC X(7)      VALUE '  PAGE '.                    
025600     03  RUB1-SIDA      PIC Z(4).                                         
025700     SKIP2                                                                
025800 01  RUBRIK2.                                                             
025900     03  FILLER         PIC X(10)     VALUE SPACE.                        
026300     03  RUB2-KATTEXT   PIC X(50).                                        
026400     SKIP2                                                                
026500 01  RUBRIK3.                                                             
026600     03  FILLER         PIC X(18)     VALUE SPACE.                        
026700     03  FILLER         PIC X(50)     VALUE                               
026800        'PART   ADJ.    ADJ.    ADV.   AVIS     AK   STOCK '.             
026900     03  FILLER         PIC X(40)     VALUE                               
027000        '    EFR     INV       P-  DESCRIPTION   '.                       
027100                                                                          
027200 01  RUBRIK4.                                                             
027300     03  FILLER         PIC X(5)      VALUE SPACE.                        
027400     03  FILLER         PIC X(50)     VALUE                               
027500        'PARTNO     ADRESS   DATE    QTY     DATE    QTY   '.             
027600     03  FILLER         PIC X(40)     VALUE                               
027700        ' QTY     QTY     QTY     QTY     CODE   '.                       
027800                                                                          
027900 01  RAD.                                                                 
028000     03  FILLER         PIC X(2)   VALUE SPACE.                           
028100     03  RAD-IDARTNR    PIC Z(9).                                         
028200     03  FILLER         PIC X(2)   VALUE SPACE.                           
028300     03  RAD-ADARTADR   PIC Z(9).                                         
028400     03  RAD-TIJUSTDA   PIC Z(7).                                         
028500     03  RAD-KVJUSTKV   PIC Z(7)-.                                        
028600     03  RAD-TIAVIDAT   PIC Z(8).                                         
028700     03  RAD-KVAVIS     PIC Z(7).                                         
028800     03  RAD-KVAKS      PIC Z(7)-.                                        
028900     03  RAD-KVLS       PIC Z(7)-.                                        
029000     03  RAD-KVEFRS     PIC Z(7)-.                                        
029100     03  RAD-KVUTRS     PIC Z(7)-.                                        
029200     03  FILLER         PIC X(5)     VALUE SPACE.                         
029300     03  RAD-KDPRODSL   PIC Z(3).                                         
029400     03  FILLER         PIC X(2)     VALUE SPACE.                         
029500     03  RAD-BEART      PIC X(25).                                        
029600                                                                          
029700 01  BLANKRAD           PIC X        VALUE SPACE.                         
035500     EJECT                                                                
035600 PROCEDURE DIVISION.                                                      
035700                                                                          
035800     PERFORM A-INIT                                                       
035900                                                                          
036000     SORT SORTER ON ASCENDING KEY SORT-IDDC                               
036001                                  SORT-KDSORT1                            
036010                                  SORT-KDINVKAT                           
036020                                  SORT-ADARTADR                           
036100                                                                          
036200     INPUT  PROCEDURE A-SKAPA-SORTBEGR-LISTA-3                            
036300     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
036400     IF SORT-RETURN > +0                                                  
036500       DISPLAY ' W51334 SORT-FEL '                                        
036600       MOVE +16 TO ABEND-CODE                                             
036700       CALL ABEND USING ABEND-CODE                                        
036800     ELSE                                                                 
036900       PERFORM C-AVSLUTA                                                  
037000       MOVE +0 TO RETURN-CODE                                             
037100     END-IF                                                               
037200     GOBACK                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 A-INIT SECTION.                                                          
037600                                                                          
037700     OPEN INPUT  W51334                                                   
037800          OUTPUT LISTA3-61                                                
038400                 LISTA3-62                                                
038600                 UTFIL                                                    
038700     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
038800     MOVE D-AAR           TO RUB1-AAR                                     
038900     MOVE D-MAANAD        TO RUB1-MAN                                     
039000     MOVE D-DAG           TO RUB1-DAG                                     
039001     MOVE D-AAR           TO RUBON1-AAR                                   
039002     MOVE D-MAANAD        TO RUBON1-MAN                                   
039003     MOVE D-DAG           TO RUBON1-DAG                                   
039004                                                                          
039005                                                                          
039010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039100     .                                                                    
039200     EJECT                                                                
039300 A-SKAPA-SORTBEGR-LISTA-3 SECTION.                                        
039400                                                                          
039500     PERFORM S01-LAS-INFIL                                                
039600     PERFORM UNTIL EOF = JA                                               
040400       IF ARB-KDINVKAT = +8                                               
040500         MOVE ARB-ADARTADR        TO WS-ADARTADR                          
040600         EVALUATE WS-ADARTADR(1:2)                                        
040700           WHEN 10       MOVE 1   TO ARB-KDSORT1                          
040800           WHEN 20       MOVE 2   TO ARB-KDSORT1                          
040900           WHEN 21       MOVE 2   TO ARB-KDSORT1                          
041000           WHEN 40       MOVE 2   TO ARB-KDSORT1                          
041100           WHEN 30       MOVE 3   TO ARB-KDSORT1                          
041200           WHEN 35       MOVE 3   TO ARB-KDSORT1                          
041300           WHEN 71       MOVE 3   TO ARB-KDSORT1                          
041400           WHEN 72       MOVE 3   TO ARB-KDSORT1                          
041500           WHEN 73       MOVE 3   TO ARB-KDSORT1                          
041600           WHEN 90       MOVE 4   TO ARB-KDSORT1                          
041700           WHEN 91       MOVE 4   TO ARB-KDSORT1                          
041800           WHEN 92       MOVE 4   TO ARB-KDSORT1                          
041900           WHEN OTHER    MOVE 5   TO ARB-KDSORT1                          
042000         END-EVALUATE                                                     
042100       ELSE                                                               
042200         MOVE 0                   TO ARB-KDSORT1                          
042300       END-IF                                                             
042500       PERFORM S02-RELEASE-SORT                                           
042700       PERFORM S01-LAS-INFIL                                              
042800     END-PERFORM                                                          
042900     .                                                                    
043000     EJECT                                                                
043100 B-SKAPA-LISTOR SECTION.                                                  
043200                                                                          
043300     MOVE NEJ    TO EOF                                                   
043400     PERFORM S03-RETURN-SORT                                              
043500     PERFORM UNTIL EOF = JA                                               
043700       PERFORM BB-UTREDNINGSSALDO-LISTA                                   
044300       PERFORM S03-RETURN-SORT                                            
044400     END-PERFORM                                                          
044500     .                                                                    
044600     EJECT                                                                
044700 BB-UTREDNINGSSALDO-LISTA SECTION.                                        
044710                                                                          
044800******************************************************************        
044900*    LISTA3-*  'ARTIKLAR FÖR VILKA UTREDNINGSSALDO UPPDATERATS'  *        
045000******************************************************************        
045100                                                                          
045200     MOVE ARB-ADARTADR      TO RAD-ADARTADR                               
045300     MOVE ARB-IDARTNR       TO RAD-IDARTNR                                
045400     MOVE ARB-TIJUSTDA      TO RAD-TIJUSTDA                               
045500     MOVE ARB-KVJUSTKV      TO RAD-KVJUSTKV                               
045600     MOVE ARB-TIAVIDAT      TO RAD-TIAVIDAT                               
045700     MOVE ARB-KVAVIS        TO RAD-KVAVIS                                 
045800     MOVE ARB-KVAKS         TO RAD-KVAKS                                  
045900     MOVE ARB-KVLS          TO RAD-KVLS                                   
046000     MOVE ARB-KVEFRS        TO RAD-KVEFRS                                 
046100     MOVE ARB-KVUTRS        TO RAD-KVUTRS                                 
046200     MOVE ARB-KDPRODSL      TO RAD-KDPRODSL                               
046300     MOVE ARB-BEART         TO RAD-BEART                                  
046310     MOVE ARB-IDDC          TO WS-IDDC                                    
046400                                                                          
046401     IF W-IDDC NOT = ARB-IDDC                                             
046402       MOVE 'J'             TO FORSTA-POST                                
046403       MOVE ARB-IDDC        TO W-IDDC                                     
046404     ELSE                                                                 
046405       MOVE 'N'             TO FORSTA-POST                                
046406     END-IF                                                               
046407                                                                          
046410                                                                          
046500     EVALUATE TRUE                                                        
051600       WHEN NDC-JP                                                        
051700         IF RADER-61 > RADMAX OR                                          
051800            SPAR-KAT-61     NOT = ARB-KDINVKAT OR                         
051900            SPAR-SORTBGP-61 NOT = ARB-KDSORT1                             
052000            PERFORM BBH-RUBRIKER-61                                       
052010            MOVE ARB-KDINVKAT   TO SPAR-KAT-61                            
052100            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-61                        
052200         END-IF                                                           
052300                                                                          
052400         WRITE LISTA-3-61 FROM RAD AFTER 1                                
052410         WRITE UTFILEN    FROM RAD AFTER 1                                
052500         ADD +1 TO RADER-61                                               
052600       WHEN NDC-AU                                                        
052700         IF RADER-62 > RADMAX OR                                          
052800            SPAR-KAT-62     NOT = ARB-KDINVKAT OR                         
052900            SPAR-SORTBGP-62 NOT = ARB-KDSORT1                             
053000            PERFORM BBI-RUBRIKER-62                                       
053010            MOVE ARB-KDINVKAT   TO SPAR-KAT-62                            
053100            MOVE ARB-KDSORT1    TO SPAR-SORTBGP-62                        
053200         END-IF                                                           
053300                                                                          
053400         WRITE LISTA-3-62 FROM RAD AFTER 1                                
053410         WRITE UTFILEN    FROM RAD AFTER 1                                
053500         ADD +1 TO RADER-62                                               
053600     END-EVALUATE                                                         
053700     .                                                                    
053800     EJECT                                                                
070100 BBH-RUBRIKER-61 SECTION.                                                 
070200******************************************************************        
070300*      GENERERAR RUBRIKER FÖR LISTA3 NDC 61                      *        
070400******************************************************************        
070500     ADD +1              TO SIDOR-61                                      
070600     MOVE SIDOR-61       TO RUB1-SIDA                                     
070700     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
070800                            RUB1-IDDC2                                    
070810     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
070820                            RUBON1-IDDC2                                  
070900     WRITE LISTA-3-61 FROM RUBRIK1 AFTER PAGE                             
070910                                                                          
071000     IF FORSTA-POST = 'J'                                                 
071100       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
071110     END-IF                                                               
071200                                                                          
071300     EVALUATE ARB-KDINVKAT                                                
071400       WHEN +2                                                            
071500         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
071600       WHEN +8                                                            
071700         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
071800       WHEN +11                                                           
071900         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
072400     END-EVALUATE                                                         
072500                                                                          
072600     WRITE LISTA-3-61 FROM RUBRIK2 AFTER 2                                
072700     WRITE LISTA-3-61 FROM RUBRIK3 AFTER 2                                
072800     WRITE LISTA-3-61 FROM RUBRIK4 AFTER 1                                
072900     WRITE LISTA-3-61 FROM BLANKRAD    AFTER 1                            
072910     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
072920     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
072930     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
072940     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
073000     MOVE +9 TO RADER-61                                                  
073100     .                                                                    
073200     EJECT                                                                
073300 BBI-RUBRIKER-62 SECTION.                                                 
073400******************************************************************        
073500*      GENERERAR RUBRIKER FÖR LISTA3 NDC 62                      *        
073600******************************************************************        
073700     ADD +1              TO SIDOR-62                                      
073800     MOVE SIDOR-62       TO RUB1-SIDA                                     
073900     MOVE ARB-IDDC       TO RUB1-IDDC1                                    
074000                            RUB1-IDDC2                                    
074010     MOVE ARB-IDDC       TO RUBON1-IDDC1                                  
074020                            RUBON1-IDDC2                                  
074100     WRITE LISTA-3-62 FROM RUBRIK1 AFTER PAGE                             
074400                                                                          
074410     IF FORSTA-POST = 'J'                                                 
074420       WRITE UTFILEN    FROM RUBRIK-ONDEMAND AFTER PAGE                   
074430     END-IF                                                               
074440                                                                          
074500     EVALUATE ARB-KDINVKAT                                                
074510       WHEN +2                                                            
074520         MOVE KATTEXT-2  TO RUB2-KATTEXT                                  
074530       WHEN +8                                                            
074540         MOVE KATTEXT-8  TO RUB2-KATTEXT                                  
074550       WHEN +11                                                           
074560         MOVE KATTEXT-11 TO RUB2-KATTEXT                                  
075600     END-EVALUATE                                                         
075700                                                                          
075800     WRITE LISTA-3-62 FROM RUBRIK2 AFTER 2                                
075900     WRITE LISTA-3-62 FROM RUBRIK3 AFTER 2                                
076000     WRITE LISTA-3-62 FROM RUBRIK4 AFTER 1                                
076100     WRITE LISTA-3-62 FROM BLANKRAD    AFTER 1                            
076110     WRITE UTFILEN    FROM RUBRIK2 AFTER 2                                
076120     WRITE UTFILEN    FROM RUBRIK3 AFTER 2                                
076130     WRITE UTFILEN    FROM RUBRIK4 AFTER 1                                
076140     WRITE UTFILEN    FROM BLANKRAD    AFTER 1                            
076200     MOVE +9 TO RADER-62                                                  
076300     .                                                                    
085300     EJECT                                                                
085400 C-AVSLUTA  SECTION.                                                      
085500                                                                          
085600     CLOSE W51334                                                         
086200           LISTA3-61                                                      
086300           LISTA3-62                                                      
086310           UTFIL                                                          
086400     MOVE 'S'     TO POSTSUM-OPKOD                                        
086410     CALL POSTSUM USING POSTSUM-PARM                                      
086500     .                                                                    
086600     SKIP3                                                                
086700 S01-LAS-INFIL SECTION.                                                   
086800                                                                          
086900     READ W51334 INTO ARB-AREA                                            
087000     AT END                                                               
087100         MOVE JA TO EOF                                                   
087200     NOT AT END                                                           
087300         MOVE '003'            TO INFIL-IDPTYP                            
087400         MOVE INFIL-TRANSID    TO POSTSUM-TRANSID                         
087500         CALL POSTSUM USING POSTSUM-PARM                                  
087600     END-READ                                                             
087700     .                                                                    
087800     SKIP3                                                                
087900 S02-RELEASE-SORT SECTION.                                                
088000                                                                          
088100     RELEASE SORT-POST FROM ARB-AREA                                      
088110     MOVE 'W51334'   TO POSTSUM-FDNAMN                                    
088120     MOVE 'SORTIN'   TO POSTSUM-DDNAMN2                                   
088130     MOVE ARB-IDDC   TO POSTSUM-TRANSTYP                                  
088140     CALL POSTSUM USING POSTSUM-PARM                                      
088200     .                                                                    
088300     SKIP3                                                                
088400 S03-RETURN-SORT SECTION.                                                 
088500                                                                          
088600     RETURN SORTER INTO ARB-AREA                                          
088700     AT END                                                               
088800       MOVE JA TO EOF                                                     
088810     NOT AT END                                                           
088820       MOVE 'W51334'   TO POSTSUM-FDNAMN                                  
088830       MOVE 'SORTUT'   TO POSTSUM-DDNAMN2                                 
088840       MOVE ARB-IDDC   TO POSTSUM-TRANSTYP                                
088850       CALL POSTSUM USING POSTSUM-PARM                                    
088900     END-RETURN                                                           
089000     .                                                                    

000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.                 W5604300.                                    
000400*AUTHOR.                     GUN LÖFGREN                                  
000500*DATE-WRITTEN.               FEBRUARI 1997.                               
000600*REMARKS.                                                                 
000700*           PROGRAMMET GENERERAR LISTOR ENLIGT NEDAN:                     
000800*           (FÖR KDINVKAT 2,8 OCH 11)                                     
000900*                                                                         
001000*           FIL W56043 TILL D&P INNEHÅLLER SAMTLIGA NDC-NA                
001100*                                                                         
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700* - - - - - - - - - - - - - - - - - - - - - - - - INFIL.                  
001800     SELECT  W5604A          ASSIGN UT-S-W56043D1.                        
001900                                                                          
002000* - - - - - - - - - - - - - - - - - - - - - - - - LISTOR.                 
002100     SELECT  W56043          ASSIGN UT-S-W56043D2.                        
002200                                                                          
002300* - - - - - - - - - - - - - - - - - - - - - - - - SORT.                   
002400     SELECT  SORTER          ASSIGN UT-S-W56043DS.                        
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W5604A                                                               
003000     RECORDING F                                                          
003100     BLOCK 0 RECORDS.                                                     
003200                                                                          
003300*01  IN-POST-A -COPY W51322N   -L.                                        
003400     SKIP3                                                                
003500 FD  W56043                                                               
003600     RECORDING  V                                                         
003700     BLOCK CONTAINS  0 RECORDS.                                           
003800 01  LIST-POST          PIC X(124).                                       
003900     SKIP2                                                                
004000                                                                          
004100     SKIP2                                                                
004200 SD  SORTER.                                                              
004300                                                                          
004400*01  POST     -COPY W51322N   -PRE SORT-.                                 
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                         PIC X(8) VALUE 'W5604300'.             
005000 77  JA                            PIC X    VALUE 'J'.                    
005100 77  NEJ                           PIC X    VALUE 'N'.                    
005200                                                                          
005300 01  W-SUB-PROG.                                                          
005400     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
005500     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005600     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
005700                                                                          
005800 01  KONSTANTER.                                                          
005900     03  ENRAD           PIC 9       VALUE 1.                             
006000     03  TVARAD          PIC 9       VALUE 2.                             
006100     03  RADMAX          PIC S9(3)   VALUE +58  COMP-3.                   
006200                                                                          
006300 01  KAT-TEXTER.                                                          
006400     03  KATTEXT-OVR     PIC X(50)   VALUE                                
006500         'INVESTIGATION QUANTITY UPDATED'.                                
006600     03  KATTEXT-8       PIC X(50)   VALUE                                
006700         'INVESTIGATION QUANTITY ADJUSTED'.                               
006800                                                                          
006900 01  VARIABLER.                                                           
007000     03  CURR-IDDC       PIC X(2)    VALUE SPACE.                         
007100     03  SPAR-SORTBGP    PIC 9       VALUE 0.                             
007200     03  WS-ADARTADR     PIC 9(9)    VALUE 0.                             
007300     03  EOF             PIC X       VALUE 'N'.                           
007400     03  SPAR-KAT        PIC S9(3)   COMP-3 VALUE +0.                     
007500                                                                          
007600 01  SUBPROGRAM.                                                          
007700     03  WDATUM              PIC X(6)        VALUE 'WDATUM'.              
007800     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM'.             
007900     EJECT                                                                
008400 01  FILLER.                                                              
008500*    03  -COPY WDATKORT.                                                  
008600     EJECT                                                                
008700 01  FILLER.                                                              
008800*    03  -COPY W0005  -PRE POSTSUM-.                                      
008900 01  INFIL-TRANSID.                                                       
009000     03  FILLER              PIC X(6)        VALUE 'W5604A'.              
009100     03  FILLER              PIC X(8)        VALUE 'W56043D1'.            
009200     03  INFIL-IDPTYP        PIC 9(4)        VALUE ZERO.                  
009300     EJECT                                                                
009400 01  W-AREA.                                                              
009500*    03  W3     -COPY W51322N  -PRE W3-.                                  
009600     EJECT                                                                
009700****************************************************************          
009800*         HÄR FÖLJER RADER FÖR LISTA3 'ARTIKLAR FÖR VILKA      *          
009900*         UTREDNINGSSALDO UPPDATERATS'                         *          
010000****************************************************************          
010010                                                                          
010020 01  W001-DAP.                                                            
010030     03  FILLER                  PIC X(165)  VALUE SPACE.                 
010040                                                                          
010100 01  FILLER               PIC X(16)       VALUE ALL 'U'.                  
010200                                                                          
010600     SKIP2                                                                
010700 01  UTR-RUBRIK1.                                                         
010800     03  FILLER         PIC X(2)     VALUE SPACE.                         
010900     03  FILLER         PIC X(31)                                         
011000         VALUE  'VCNA                   W56043-3'.                        
011100     03  UTR-RUB1-IDDC  PIC XX       VALUE SPACE.                         
011200     03  FILLER         PIC X(39)                                         
011300         VALUE  '  PARTS PUNCHED WITH PHYSICAL DEVIATION'.                
011400     03  FILLER         PIC X(2)     VALUE SPACE.                         
011500     03  FILLER         PIC X(3)     VALUE 'DC'.                          
011600     03  UTR-IDDC       PIC X(3).                                         
011700     03  UTR-DAT1       PIC B99.                                          
011800     03  UTR-DAT2       PIC B99.                                          
011900     03  UTR-DAT3       PIC B99.                                          
012200     SKIP2                                                                
012300 01  UTR-RUBRIK11.                                                        
012400     03  FILLER         PIC X(2)     VALUE SPACE.                         
012500     03  FILLER         PIC X(4)     VALUE 'CAT '.                        
012600     03  UTR-KDINVKAT   PIC 9(2).                                         
012700     03  FILLER         PIC X(2)     VALUE SPACE.                         
012800     03  UTR-KATTEXT    PIC X(50).                                        
012900     SKIP2                                                                
013000 01  UTR-RUBRIK2.                                                         
013100     03  FILLER         PIC X(18)    VALUE SPACE.                         
013200     03  FILLER         PIC X(50)    VALUE                                
013300        'PART   ADJ.    ADJ.                    AK   STOCK '.             
013400     03  FILLER         PIC X(40)    VALUE                                
013500        '    EFR  INVEST.     LPC  DESCRIPTION   '.                       
013600                                                                          
013700 01  UTR-RUBRIK3.                                                         
013800     03  FILLER         PIC X(5)     VALUE SPACE.                         
013900     03  FILLER         PIC X(50)    VALUE                                
014000        'PARTNO     ADRESS   DATE    QTY                   '.             
014100     03  FILLER         PIC X(40)    VALUE                                
014200        ' QTY     QTY     QTY BALANCE            '.                       
014300                                                                          
014400 01  UTR-RAD.                                                             
014500     03  FILLER         PIC X(2)     VALUE SPACE.                         
014600     03  UTR-IDARTNR    PIC Z(9).                                         
014700     03  FILLER         PIC X(2)     VALUE SPACE.                         
014800     03  UTR-ADARTADR   PIC Z(9).                                         
014900     03  UTR-TIJUSTDA   PIC Z(7).                                         
015000     03  UTR-KVJUSTKV   PIC Z(7)-.                                        
015100     03  FILLER         PIC X(15)    VALUE SPACE.                         
015200     03  UTR-KVAKS      PIC Z(7)-.                                        
015300     03  UTR-KVLS       PIC Z(7)-.                                        
015400     03  UTR-KVEFRS     PIC Z(7)-.                                        
015500     03  UTR-KVUTRS     PIC Z(7)-.                                        
015600     03  FILLER         PIC X(5)     VALUE SPACE.                         
015700     03  UTR-KDPRODSL   PIC Z(3).                                         
015800     03  FILLER         PIC X(2)     VALUE SPACE.                         
015900     03  UTR-BEART      PIC X(25).                                        
016000     EJECT                                                                
016100 01  BLANKRAD           PIC X        VALUE SPACE.                         
016200     EJECT                                                                
016300 PROCEDURE DIVISION.                                                      
016400                                                                          
016500     PERFORM A-INIT                                                       
016600                                                                          
016700     SORT SORTER ON ASCENDING KEY SORT-IDDC                               
016800                                  SORT-KDSORT1                            
016900                                  SORT-KDINVKAT                           
017000                                  SORT-ADARTADR                           
017100                                  SORT-IDARTNR                            
017200                                  SORT-KDPSLLOC                           
017300                                                                          
017400     INPUT  PROCEDURE A-INPUT                                             
017500     OUTPUT PROCEDURE B-SKAPA-LISTOR                                      
017600     IF SORT-RETURN > +0                                                  
017700       DISPLAY ' W5604A SORT-FEL '                                        
017800       MOVE +16 TO ABEND-CODE                                             
017900       CALL ABEND USING ABEND-CODE                                        
018000     ELSE                                                                 
018100       PERFORM C-AVSLUTA                                                  
018200       MOVE +0 TO RETURN-CODE                                             
018300     END-IF                                                               
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     OPEN INPUT  W5604A                                                   
019000          OUTPUT W56043                                                   
019100                                                                          
019200     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
019300     .                                                                    
019400     EJECT                                                                
019500 A-INPUT                  SECTION.                                        
019600                                                                          
019700     PERFORM S01-LAS-INFIL                                                
019800     PERFORM UNTIL EOF = JA                                               
019900       IF W3-KDINVKAT = +8                                                
020000         MOVE W3-ADARTADR         TO WS-ADARTADR                          
020100         EVALUATE WS-ADARTADR(1:2)                                        
020200           WHEN 10       MOVE 1   TO W3-KDSORT1                           
020300           WHEN 20       MOVE 2   TO W3-KDSORT1                           
020400           WHEN 21       MOVE 2   TO W3-KDSORT1                           
020500           WHEN 40       MOVE 2   TO W3-KDSORT1                           
020600           WHEN 30       MOVE 3   TO W3-KDSORT1                           
020700           WHEN 35       MOVE 3   TO W3-KDSORT1                           
020800           WHEN 71       MOVE 3   TO W3-KDSORT1                           
020900           WHEN 72       MOVE 3   TO W3-KDSORT1                           
021000           WHEN 73       MOVE 3   TO W3-KDSORT1                           
021100           WHEN 90       MOVE 4   TO W3-KDSORT1                           
021200           WHEN 91       MOVE 4   TO W3-KDSORT1                           
021300           WHEN 92       MOVE 4   TO W3-KDSORT1                           
021400           WHEN OTHER    MOVE 5   TO W3-KDSORT1                           
021500         END-EVALUATE                                                     
021600       ELSE                                                               
021700           MOVE 0                 TO W3-KDSORT1                           
021800       END-IF                                                             
021900       PERFORM S02-RELEASE-SORT                                           
022000       PERFORM S01-LAS-INFIL                                              
022100     END-PERFORM                                                          
022200     .                                                                    
022300     EJECT                                                                
022400 B-SKAPA-LISTOR SECTION.                                                  
022500                                                                          
022600     MOVE NEJ    TO EOF                                                   
022700     PERFORM S03-RETURN-SORT                                              
022800     PERFORM UNTIL EOF = JA                                               
022900       IF W3-IDDC NOT = CURR-IDDC                                         
022901          MOVE W3-IDDC  TO CURR-IDDC                                      
022904          PERFORM S10-SKRIV-DAP1                                          
022905          PERFORM S11-SKRIV-DAP2                                          
022910          PERFORM S04-RUBRIKER                                            
022930       END-IF                                                             
022940                                                                          
023000       PERFORM BB-UTREDNINGSSALDO-LISTA                                   
023100*                              **  LISTA MED ARTIKLAR DÄR      **         
023200*                              **  UTREDNINGSSALDO UPPDATERATS **         
023300       PERFORM S03-RETURN-SORT                                            
023400     END-PERFORM                                                          
023500     .                                                                    
023600     EJECT                                                                
023700 BB-UTREDNINGSSALDO-LISTA SECTION.                                        
023800******************************************************************        
023900*    LISTA3-*  'ARTIKLAR FÖR VILKA UTREDNINGSSALDO UPPDATERATS'  *        
024000******************************************************************        
024100                                                                          
024200     MOVE W3-ADARTADR       TO UTR-ADARTADR                               
024300     MOVE W3-IDARTNR        TO UTR-IDARTNR                                
024400     MOVE W3-TIJUSTDA       TO UTR-TIJUSTDA                               
024500     MOVE W3-KVJUSTKV       TO UTR-KVJUSTKV                               
024600     MOVE W3-KVAKS          TO UTR-KVAKS                                  
024700     MOVE W3-KVLS           TO UTR-KVLS                                   
024800     MOVE W3-KVEFRS         TO UTR-KVEFRS                                 
024900     MOVE W3-KVUTRS         TO UTR-KVUTRS                                 
025000     MOVE W3-KDPSLLOC       TO UTR-KDPRODSL                               
025100     MOVE W3-BEART          TO UTR-BEART                                  
025200                                                                          
025600*    IF UTR-RADER > RADMAX OR                                             
025700*       SPAR-KAT     NOT = W3-KDINVKAT OR                                 
025800*       SPAR-SORTBGP NOT = W3-KDSORT1                                     
025900*       PERFORM S04-RUBRIKER                                              
026000*       MOVE W3-KDSORT1  TO SPAR-SORTBGP                                  
026100*    END-IF                                                               
026200                                                                          
026300     WRITE LIST-POST FROM UTR-RAD                                         
029600     .                                                                    
029700     EJECT                                                                
032600                                                                          
041100 C-AVSLUTA  SECTION.                                                      
041200                                                                          
041300     CLOSE W5604A                                                         
041400           W56043                                                         
041800     .                                                                    
041900     SKIP3                                                                
042000 S01-LAS-INFIL SECTION.                                                   
042100                                                                          
042200     READ W5604A INTO W-AREA                                              
042300     AT END                                                               
042400         MOVE JA TO EOF                                                   
042500     NOT AT END                                                           
042600         MOVE '003'            TO INFIL-IDPTYP                            
042700         MOVE INFIL-TRANSID    TO POSTSUM-TRANSID                         
042800         CALL POSTSUM USING POSTSUM-PARM                                  
042900     END-READ                                                             
043000     .                                                                    
043100     SKIP3                                                                
043200 S02-RELEASE-SORT SECTION.                                                
043300                                                                          
043400     RELEASE SORT-POST FROM W-AREA                                        
043500     .                                                                    
043600     SKIP3                                                                
043700 S03-RETURN-SORT SECTION.                                                 
043800                                                                          
043900     RETURN SORTER INTO W-AREA                                            
044000     AT END                                                               
044100         MOVE JA TO EOF                                                   
044200     END-RETURN                                                           
044300     .                                                                    
044310 S04-RUBRIKER SECTION.                                                    
044320******************************************************************        
044330*      GENERERAR RUBRIKER FÖR LISTA                              *        
044340******************************************************************        
044370     MOVE W3-IDDC       TO UTR-IDDC                                       
044380                           UTR-RUB1-IDDC                                  
044390     MOVE D-AAR         TO UTR-DAT3                                       
044391     MOVE D-MAANAD      TO UTR-DAT1                                       
044392     MOVE D-DAG         TO UTR-DAT2                                       
044393     WRITE LIST-POST  FROM UTR-RUBRIK1                                    
044394     MOVE W3-KDINVKAT   TO SPAR-KAT                                       
044395     MOVE SPAR-KAT      TO UTR-KDINVKAT                                   
044396                                                                          
044397     IF SPAR-KAT = +8                                                     
044398       MOVE KATTEXT-8   TO UTR-KATTEXT                                    
044399     ELSE                                                                 
044400       MOVE KATTEXT-OVR TO UTR-KATTEXT                                    
044401     END-IF                                                               
044402                                                                          
044403     WRITE LIST-POST FROM UTR-RUBRIK11                                    
044404     WRITE LIST-POST FROM BLANKRAD                                        
044405     WRITE LIST-POST FROM UTR-RUBRIK2                                     
044406     WRITE LIST-POST FROM BLANKRAD                                        
044407     WRITE LIST-POST FROM UTR-RUBRIK3                                     
044408     WRITE LIST-POST FROM BLANKRAD                                        
044410     .                                                                    
044411     EJECT                                                                
044420 S10-SKRIV-DAP1 SECTION.                                                  
044500                                                                          
044600     MOVE ' ¤DAPW56043' TO W001-DAP                                       
044700     WRITE LIST-POST FROM W001-DAP                                        
044800                                                                          
044900     MOVE SPACE TO W001-DAP                                               
045000     .                                                                    
045100                                                                          
045200 S11-SKRIV-DAP2 SECTION.                                                  
045300                                                                          
045400     STRING ' ¤DAP' CURR-IDDC                                             
045500            DELIMITED BY SIZE INTO W001-DAP                               
045600     WRITE LIST-POST FROM W001-DAP                                        
045700                                                                          
045800     MOVE SPACE TO W001-DAP                                               
045900     .                                                                    

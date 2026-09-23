000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0927400.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   06/03/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        TAR EMOT FIL FRÅN NAP                                            
001000*        SPARAR POSTER (NAPIN) VARS INF TID EJ UPPNÅDD                    
001100*        SÄNDER POSTER TILL W09278(985,940,984,941)                       
001200*        SKRIVER FIL MED LARMPOSTER                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- FIL FRÅN NAP                                               
002700     SELECT NAPIN                      ASSIGN TO W09274D1.                
002800     SKIP2                                                                
002900*          --- REG SPARADE POSTER                                         
003000     SELECT W09274I                    ASSIGN TO W09274D2.                
003100     SKIP2                                                                
003200*          --- REG SPARADE POSTER UT                                      
003300     SELECT W09274U                    ASSIGN TO W09274D3.                
003400     SKIP2                                                                
003500*          --- FIL TILL W09278                                            
003600     SELECT W09275                     ASSIGN TO W09274D4.                
003700     SKIP2                                                                
003800*          --- LARMPOSTER                                                 
003900     SELECT W09277                     ASSIGN TO W09274D5.                
004000     SKIP2                                                                
004100*          --- SORTERINGSFIL                                              
004200     SELECT SORTFIL                    ASSIGN TO W09274DS.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  NAPIN                                                                
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY NAPIN         -L.                                              
005300                                                                          
005400     SKIP3                                                                
005500 FD  W09274I                                                              
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  -COPY NAPIN         -L.                                              
006000     SKIP3                                                                
006100 FD  W09274U                                                              
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  REGUT-POST -COPY NAPIN      -L.                                      
006600                                                                          
006700     SKIP3                                                                
006800 FD  W09275                                                               
006900     RECORDING       V                                                    
007000     BLOCK CONTAINS  0.                                                   
007100                                                                          
007200 01  UT-POST            PIC X(58).                                        
007300                                                                          
007400*01  POST -COPY A311G940 -PRE  UTO-  -L.                                  
007500                                                                          
007600*01  POST -COPY A311G941 -PRE  UTA-  -L.                                  
007700                                                                          
007800*01  POST -COPY A310G984 -PRE  UTI-  -L.                                  
007900                                                                          
008000*01  POST -COPY A310G985 -PRE  UTP-  -L.                                  
008100     SKIP3                                                                
008200 FD  W09277                                                               
008300     RECORDING       F                                                    
008400     BLOCK CONTAINS  0.                                                   
008500                                                                          
008600*01  LARM-POST -COPY NAPIN      -L.                                       
008700                                                                          
008800     SKIP2                                                                
008900 SD  SORTFIL.                                                             
009000                                                                          
009100 01  SORT-POST.                                                           
009200     03  SORT-IDPTYP             PIC 9(1).                                
009300     03  SORT-IDARTNR            PIC S9(9) COMP-3.                        
009400     03  SORT-DATA               PIC X(58).                               
009500                                                                          
009600*03  POST940 -COPY A311G940 -PRE  SORT- REDEFINES SORT-DATA               
009700     SKIP2                                                                
009800*03  POST941 -COPY A311G941 -PRE  SORT- REDEFINES SORT-DATA               
009900     SKIP2                                                                
010000*03  POST984 -COPY A310G984 -PRE  SORT- REDEFINES SORT-DATA               
010100     SKIP2                                                                
010200*03  POST985 -COPY A310G985 -PRE  SORT- REDEFINES SORT-DATA               
010300     EJECT                                                                
010400 WORKING-STORAGE SECTION.                                                 
010500                                                                          
010600 77  IDPGM                       PIC X(8)    VALUE 'W0927400'.            
010700 77  JA                          PIC X       VALUE 'J'.                   
010800 77  NEJ                         PIC X       VALUE 'N'.                   
010900                                                                          
011000 77  NAPIN-EOF-SW                PIC X       VALUE 'N'.                   
011100     88  END-OF-NAPIN                        VALUE 'J'.                   
011200                                                                          
011300 77  W09274I-EOF-SW              PIC X       VALUE 'N'.                   
011400     88  END-OF-W09274I                      VALUE 'J'.                   
011500                                                                          
011600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
011700     88  END-OF-SORTFIL                      VALUE 'J'.                   
011800                                                                          
011900 77  SW-ANN                      PIC X       VALUE 'N'.                   
012000                                                                          
012100 01  DIV.                                                                 
012200     03  DD-6V-DATUM             PIC 9(6).                                
012300     03  W-DATUM8                PIC 9(8).                                
012400     03  FILLER  REDEFINES W-DATUM8.                                      
012500         05  FILLER              PIC 9(2).                                
012600         05  W-DATUM6            PIC 9(6).                                
012700     03  W-BESTNR12              PIC 9(12).                               
012800     03  FILLER  REDEFINES W-BESTNR12.                                    
012900         05  W-BESTPREF          PIC 9(3).                                
013000         05  W-BESTLNR           PIC 9(6).                                
013100         05  W-BESTSUFF          PIC 9(3).                                
013200     03  FILLER  REDEFINES W-BESTNR12.                                    
013300         05  W-BESTNR12-00       PIC 9(2).                                
013400         05  W-BESTNR10          PIC X(10).                               
013500     03  TABELL  OCCURS 500.                                              
013600         05  TAB-OBJECT-ID       PIC X(10).                               
013700         05  TAB-ORDERED-PROD    PIC X(18).                               
013800     03  IX-TAB                  PIC S9(3)   COMP-3  VALUE 1.             
013900     03  TAB-MAX                 PIC S9(3)   COMP-3  VALUE ZERO.          
014000     03  WS-BESTPRIS             PIC 9(12)   COMP-3  VALUE ZERO.          
014100     EJECT                                                                
014200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014300 01  FILLER REDEFINES DAGENS-DATUM.                                       
014400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014700 01  NEXTDAY-DATUM               PIC 9(6)    VALUE ZERO.                  
014800     EJECT                                                                
014900 01  DYNAMISKA-SUBPROGRAM.                                                
015000*                                                                         
015100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
015600     SKIP2                                                                
015700*    --- PARAMETRAR TILL ABEND                                            
015800                                                                          
015900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016200     SKIP2                                                                
016300 01  FELTEXT.                                                             
016400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
016500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL DATKORT                                          
016800*                                                                         
016900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W09274'.              
017000     SKIP2                                                                
017100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
017200     SKIP2                                                                
017300*01  -COPY WDATKORT                                                       
017400     EJECT                                                                
017500*    --- PARAMETRAR TILL POSTSUM                                          
017600*                                                                         
017700*01  -COPY W0005   -PRE  POSTSUM-                                         
017800     EJECT                                                                
017900*01  -COPY WDATAREA                                                       
018000     EJECT                                                                
018100*01  -COPY WORKAREA                                                       
018200     EJECT                                                                
018300 01  IN-AREA-START               PIC X(24)   VALUE                        
018400                                 'IN-AREA-START  '.                       
018500     SKIP2                                                                
018600 01  IN-AREA.                                                             
018700     03  IN-AREA-0.                                                       
018800*   05  FILLER -COPY NAPIN  -PRE IN-                                      
018900     EJECT                                                                
019000 01  REGIN-AREA-START            PIC X(24)   VALUE                        
019100                                 'REGIN-AREA-START  '.                    
019200     SKIP2                                                                
019300 01  REGIN-AREA.                                                          
019400     03  REGIN-AREA-0.                                                    
019500                                                                          
019600*   05  FILLER -COPY NAPIN  -PRE REGIN-                                   
019700     EJECT                                                                
019800 01  REGUT-AREA-START            PIC X(24)   VALUE                        
019900                                 'REGUT-AREA-START  '.                    
020000     SKIP2                                                                
020100 01  REGUT-AREA.                                                          
020200     03  REGUT-AREA-0.                                                    
020300                                                                          
020400*   05  FILLER -COPY NAPIN  -PRE REGUT-                                   
020500     EJECT                                                                
020600 01  UT-AREA-START               PIC X(24)   VALUE                        
020700                                 'UT-AREA-START  '.                       
020800     SKIP2                                                                
020900 01  UT-AREA.                                                             
021000     03  UT-AREA-0.                                                       
021100       05  UT-PT                 PIC S9(3)  COMP-3.                       
021200       05  FILLER                PIC X(51).                               
021300                                                                          
021400*   03  FILLER -COPY A311G940  -PRE UTO-  -RED  UT-AREA-0                 
021500     SKIP3                                                                
021600*   03  FILLER -COPY A311G941  -PRE UTA-  -RED  UT-AREA-0                 
021700     SKIP3                                                                
021800*   03  FILLER -COPY A310G984  -PRE UTI-  -RED  UT-AREA-0                 
021900     SKIP3                                                                
022000*   03  FILLER -COPY A310G985  -PRE UTP-  -RED  UT-AREA-0                 
022100     EJECT                                                                
022200 01  LARM-AREA-START             PIC X(24)   VALUE                        
022300                                 'LARM-AREA-START  '.                     
022400     SKIP2                                                                
022500 01  LARM-AREA.                                                           
022600     03  LARM-AREA-0.                                                     
022700*   05  FILLER -COPY NAPIN  -PRE LARM-                                    
022800                                                                          
022900     EJECT                                                                
023000 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
023100                                  'SORTWS-AREA-START  '.                  
023200     SKIP2                                                                
023300 01  SORTWS-AREA.                                                         
023400         05  SORTWS-IDPTYP       PIC 9(1).                                
023500         05  SORTWS-IDARTNR      PIC S9(9) COMP-3.                        
023600         05  SORTWS-POST         PIC X(58).                               
023700                                                                          
023800*   05  FILLER -COPY A311G940  -PRE SORTWSO- -RED SORTWS-POST             
023900      SKIP3                                                               
024000*   05  FILLER -COPY A311G941  -PRE SORTWSA- -RED SORTWS-POST             
024100      SKIP3                                                               
024200*   05  FILLER -COPY A310G984  -PRE SORTWSI- -RED SORTWS-POST             
024300      SKIP3                                                               
024400*   05  FILLER -COPY A310G985  -PRE SORTWSP- -RED SORTWS-POST             
024500                                                                          
024600 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
024700     EJECT                                                                
024800 PROCEDURE DIVISION.                                                      
024900 MAIN SECTION.                                                            
025000     SKIP2                                                                
025100                                                                          
025200     PERFORM A-INIT                                                       
025300                                                                          
025400     SORT SORTFIL ASCENDING KEY SORT-IDPTYP                               
025500                                SORT-IDARTNR                              
025600                  INPUT  PROCEDURE B-SORT-INPUT                           
025700                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
025800                                                                          
025900     IF SORT-RETURN NOT = 0                                               
026000       MOVE SORT-RETURN TO SORT-RETURN-X                                  
026100       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
026200       DELIMITED BY SIZE INTO FELTEXT-STR                                 
026300       DISPLAY FELTEXT                                                    
026400       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
026500       PERFORM S99-ABEND                                                  
026600     ELSE                                                                 
026700       PERFORM Z-FINIT                                                    
026800                                                                          
026900       MOVE ZERO TO RETURN-CODE                                           
027000       GOBACK                                                             
027100     END-IF                                                               
027200                                                                          
027300     .                                                                    
027400     EJECT                                                                
027500 A-INIT SECTION.                                                          
027600                                                                          
027700     OPEN INPUT  NAPIN                                                    
027800                 W09274I                                                  
027900                                                                          
028000     OPEN OUTPUT W09274U                                                  
028100                 W09275                                                   
028200                 W09277                                                   
028300     SKIP2                                                                
028400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028500     MOVE D-AAR         TO DAGENS-DATUM-AAR                               
028600     MOVE D-MAANAD      TO DAGENS-DATUM-MAANAD                            
028700     MOVE D-DAG         TO DAGENS-DATUM-DAG                               
028800     MOVE IDPGM         TO POSTSUM-PROGNAMN                               
028900     MOVE DAGENS-DATUM  TO NEXTDAY-DATUM                                  
029000     ADD  +1            TO NEXTDAY-DATUM                                  
029100                                                                          
029200     MOVE 002           TO WORK-KDCALL                                    
029300     MOVE 11            TO WORK-IDDC                                      
029400     MOVE DAGENS-DATUM  TO WORK-TIAAMMDD-FOM                              
029500     MOVE ZERO          TO WORK-TIAAMMDD-TOM                              
029600     MOVE 2             TO WORK-KVWORKD                                   
029700                                                                          
029800     CALL WORKDAY USING WORK-KDCALL                                       
029900                        WORK-DATE-AREA                                    
030000                        WORK-KDSVAR                                       
030100                                                                          
030200     IF WORK-KDSVAR-OK                                                    
030300        MOVE WORK-TIAAMMDD-TOM          TO NEXTDAY-DATUM                  
030400     ELSE                                                                 
030500        MOVE DAGENS-DATUM               TO NEXTDAY-DATUM                  
030600     END-IF                                                               
030700     DISPLAY 'WORKDAY ' WORK-TIAAMMDD-TOM ' '                             
030800                        WORK-TIAAMMDD-NEXT-WORKDAY                        
030900                                                                          
031000     MOVE 002           TO WORK-KDCALL                                    
031100     MOVE 11            TO WORK-IDDC                                      
031200     MOVE DAGENS-DATUM  TO WORK-TIAAMMDD-FOM                              
031300     MOVE ZERO          TO WORK-TIAAMMDD-TOM                              
031400     MOVE 36            TO WORK-KVWORKD                                   
031500                                                                          
031600     CALL WORKDAY USING WORK-KDCALL                                       
031700                        WORK-DATE-AREA                                    
031800                        WORK-KDSVAR                                       
031900                                                                          
032000     IF WORK-KDSVAR-OK                                                    
032100        MOVE WORK-TIAAMMDD-TOM  TO DD-6V-DATUM                            
032200     ELSE                                                                 
032300        MOVE DAGENS-DATUM       TO DD-6V-DATUM                            
032400     END-IF                                                               
032500     DISPLAY '6 VECKOR ' WORK-TIAAMMDD-TOM ' '                            
032600     .                                                                    
032700     EJECT                                                                
032800 B-SORT-INPUT  SECTION.                                                   
032900                                                                          
033000     PERFORM S01-LAES-NAPIN                                               
033100     PERFORM UNTIL END-OF-NAPIN                                           
033200                                                                          
033300       INSPECT IN-NAP-ORDERED-PROD                                        
033400               REPLACING LEADING SPACE BY ZERO                            
033500                                                                          
033600*******IF (IN-NAP-STATUS = 'C' OR 'L') OR                                 
033700       IF (IN-NAP-ITM-RELEASED NOT = 'X')                                 
033800**********IF IN-NAP-STATUS = 'C' OR 'L' OR                                
033900          IF IN-NAP-ITM-RELEASED = SPACE OR 'C'                           
034000             PERFORM BD-SKAPA-941                                         
034100             PERFORM S31-SORT-RELEASE                                     
034200             MOVE IN-NAP-OBJECT-ID TO  TAB-OBJECT-ID (IX-TAB)             
034300             MOVE IN-NAP-ORDERED-PROD TO                                  
034400                                 TAB-ORDERED-PROD (IX-TAB)                
034500             MOVE IX-TAB TO TAB-MAX                                       
034600             ADD +1 TO IX-TAB                                             
034700          END-IF                                                          
034800       ELSE                                                               
034900**        ITM-RELEASED = X = AKTIV  (STATUS = R ?)                        
035000          IF IN-NAP-PRICE > ZERO                                          
035100            PERFORM BA-SKAPA-985                                          
035200            PERFORM S31-SORT-RELEASE                                      
035300          END-IF                                                          
035400                                                                          
035500          IF IN-NAP-PUR-GROUP > SPACE                                     
035600             PERFORM BE-SKAPA-984                                         
035700             PERFORM S31-SORT-RELEASE                                     
035800          END-IF                                                          
035900                                                                          
036000          MOVE IN-NAP-VPER-START  TO W-DATUM8                             
036100          IF W-DATUM6 <= NEXTDAY-DATUM                                    
036200             PERFORM BC-SKAPA-940                                         
036300             PERFORM S31-SORT-RELEASE                                     
036400             IF IN-NAP-VPER-END > ZERO AND                                
036500                IN-NAP-VPER-END < 20991231                                
036600                MOVE 'J'   TO IN-FLEND-TEST                               
036700                MOVE 'J'   TO IN-FLLARM-TEST                              
036800                MOVE SPACE TO IN-FLSTART-TEST                             
036900                MOVE IN-NAP-IN       TO REGUT-AREA                        
037000                PERFORM  S10-SKRIV-W09274U                                
037100             END-IF                                                       
037200          ELSE                                                            
037300             MOVE 'JA'  TO IN-FLSTART-TEST                                
037400             IF IN-NAP-VPER-END > ZERO AND                                
037500                IN-NAP-VPER-END < 20991231                                
037600                MOVE 'J'   TO IN-FLEND-TEST                               
037700                MOVE 'J'   TO IN-FLLARM-TEST                              
037800             ELSE                                                         
037900                MOVE SPACE TO IN-FLEND-TEST                               
038000                MOVE SPACE TO IN-FLLARM-TEST                              
038100             END-IF                                                       
038200             MOVE IN-NAP-IN       TO REGUT-AREA                           
038300             PERFORM  S10-SKRIV-W09274U                                   
038400          END-IF                                                          
038500       END-IF                                                             
038600                                                                          
038700       PERFORM S01-LAES-NAPIN                                             
038800     END-PERFORM                                                          
038900                                                                          
039000     PERFORM S02-LAES-W09274I                                             
039100     PERFORM UNTIL END-OF-W09274I                                         
039200       PERFORM BF-TEST-ANNULLERAD                                         
039300       IF SW-ANN = JA                                                     
039400          CONTINUE                                                        
039500       ELSE                                                               
039600          INSPECT REGIN-NAP-ORDERED-PROD                                  
039700                  REPLACING LEADING SPACE BY ZERO                         
039800          MOVE REGIN-NAP-VPER-START  TO W-DATUM8                          
039900          IF REGIN-FLSTART-TEST = 'J' AND                                 
040000             W-DATUM6 <= NEXTDAY-DATUM                                    
040100                MOVE REGIN-NAP-IN   TO IN-NAP-IN                          
040200                PERFORM BC-SKAPA-940                                      
040300                PERFORM S31-SORT-RELEASE                                  
040400                IF IN-NAP-VPER-END > ZERO AND                             
040500                   IN-NAP-VPER-END < 20991231                             
040600                   MOVE 'J'   TO IN-FLEND-TEST                            
040700                   MOVE 'J'   TO IN-FLLARM-TEST                           
040800                   MOVE SPACE TO IN-FLSTART-TEST                          
040900                   MOVE IN-NAP-IN        TO REGUT-AREA                    
041000                   PERFORM  S10-SKRIV-W09274U                             
041100                END-IF                                                    
041200          ELSE                                                            
041300             MOVE REGIN-NAP-VPER-END    TO W-DATUM8                       
041400             IF REGIN-FLLARM-TEST = 'J' AND                               
041500                W-DATUM6 <= DD-6V-DATUM                                   
041600*   *****         SKAPA LARMFIL                                           
041700                  MOVE REGIN-NAP-IN TO LARM-NAP-IN                        
041800                  PERFORM S12-SKRIV-W09277                                
041900                  MOVE SPACE TO REGIN-FLLARM-TEST                         
042000                  MOVE REGIN-NAP-IN     TO REGUT-AREA                     
042100                  PERFORM  S10-SKRIV-W09274U                              
042200             ELSE                                                         
042300                IF REGIN-FLEND-TEST = 'J' AND                             
042400                  W-DATUM6 <= NEXTDAY-DATUM                               
042500                  MOVE REGIN-NAP-IN   TO IN-NAP-IN                        
042600                  PERFORM BD-SKAPA-941                                    
042700                  PERFORM S31-SORT-RELEASE                                
042800                ELSE                                                      
042900                  MOVE REGIN-NAP-IN     TO REGUT-AREA                     
043000                  PERFORM  S10-SKRIV-W09274U                              
043100                END-IF                                                    
043200             END-IF                                                       
043300          END-IF                                                          
043400       END-IF                                                             
043500                                                                          
043600       PERFORM S02-LAES-W09274I                                           
043700     END-PERFORM                                                          
043800     .                                                                    
043900     EJECT                                                                
044000 BA-SKAPA-985         SECTION.                                            
044100     SKIP2                                                                
044200     MOVE SPACE                TO SORTWSP-A310T985                        
044300     MOVE 985                  TO SORTWSP-PT                              
044400     MOVE '1002 '              TO SORTWSP-GSDB-FORB                       
044500     MOVE IN-NAP-IDARTNR       TO SORTWSP-ARTNR                           
044600     MOVE IN-NAP-VENDOR        TO SORTWSP-GSDB-LEV                        
044700     MOVE ZERO                 TO SORTWSP-PRISSKR-BEST                    
044800     PERFORM BAA-KONV-PRICE                                               
044900     PERFORM BAB-KONV-PRICE-UNIT-KDENH                                    
045000*****PERFORM BAC-KONV-UNIT-KDSORT                                         
045100     MOVE SPACE                TO SORTWSP-KDSORT                          
045200     MOVE '0'                  TO SORTWSP-KDPRIS-TYP                      
045300     MOVE ZERO                 TO SORTWSP-KDVAL-BEST                      
045400     MOVE IN-NAP-VPER-START    TO W-DATUM8                                
045500     MOVE W-DATUM6             TO SORTWSP-DATUM-BESTPRIS                  
045600     MOVE ZERO                 TO SORTWSP-INKNR                           
045700     MOVE SPACE                TO SORTWSP-BESTTYP                         
045800     MOVE IN-NAP-CURRENCY      TO SORTWSP-KDVAL-ISO                       
045900     MOVE 1002                 TO SORTWSP-FORBNR                          
046000     MOVE ZERO                 TO SORTWSP-LEVNUM                          
046100     MOVE 1                    TO SORTWS-IDPTYP                           
046200     MOVE IN-NAP-IDARTNR       TO SORTWS-IDARTNR                          
046300     MOVE SPACE                TO SORTWSP-KDFPKPRI                        
046400     .                                                                    
046500     EJECT                                                                
046600 BAA-KONV-PRICE            SECTION.                                       
046700     SKIP2                                                                
046800     MOVE IN-NAP-PRICE         TO SORTWSP-BESTPRIS                        
046900*    ALLTID 2 DEC ?  GBP ?                                                
047000*    I 985-POSTEN ALLTID HELTAL VARAV 2 DEC (GBP 3 DEC !)                 
047100     IF IN-NAP-CURRENCY = 'GBP'                                           
047200        COMPUTE SORTWSP-BESTPRIS = SORTWSP-BESTPRIS * 10                  
047300     END-IF                                                               
047400*    DÅ VI BARA FÅR 2 DEC, MEN W55312 RÄKNAR MED 3 DEC FÖR GBP            
047500     .                                                                    
047600     EJECT                                                                
047700 BAB-KONV-PRICE-UNIT-KDENH SECTION.                                       
047800     SKIP2                                                                
047900***  KONVERTERA  KDENH (KDANTENH)                                         
048000     IF IN-NAP-PRICE-UNIT = 1                                             
048100        MOVE '1'               TO SORTWSP-KDENH-BEST                      
048200     ELSE                                                                 
048300        MOVE 'X'               TO SORTWSP-KDENH-BEST                      
048400        IF IN-NAP-PRICE-UNIT = 10                                         
048500           MOVE '7'            TO SORTWSP-KDENH-BEST                      
048600        END-IF                                                            
048700        IF IN-NAP-PRICE-UNIT = 100                                        
048800           MOVE '2'            TO SORTWSP-KDENH-BEST                      
048900        END-IF                                                            
049000        IF IN-NAP-PRICE-UNIT = 144                                        
049100           MOVE '8'            TO SORTWSP-KDENH-BEST                      
049200        END-IF                                                            
049300        IF IN-NAP-PRICE-UNIT = 1000                                       
049400           MOVE '3'            TO SORTWSP-KDENH-BEST                      
049500        END-IF                                                            
049600        IF SORTWSP-KDENH-BEST = 'X'                                       
049700           COMPUTE WS-BESTPRIS ROUNDED =                                  
049800                   SORTWSP-BESTPRIS * 1000 / IN-NAP-PRICE-UNIT            
049900           MOVE WS-BESTPRIS    TO SORTWSP-BESTPRIS                        
050000           MOVE '3'            TO SORTWSP-KDENH-BEST                      
050100        END-IF                                                            
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 BAC-KONV-UNIT-KDSORT SECTION.                                            
050600     SKIP2                                                                
050700*    OBS  KDSORT ANVÄNDS INTE I 985 POSTERNA                              
050800***  KONVERTERA KDSORT                                                    
050900     IF IN-NAP-UNIT = 'PCE'                                               
051000        MOVE 'ST'              TO SORTWSP-KDSORT                          
051100     ELSE                                                                 
051200        MOVE IN-NAP-UNIT       TO SORTWSP-KDSORT                          
051300        IF IN-NAP-UNIT = 'PAR'                                            
051400           MOVE 'PA'           TO SORTWSP-KDSORT                          
051500        END-IF                                                            
051600        IF IN-NAP-UNIT = 'MMT'                                            
051610           MOVE 'MM'           TO SORTWSP-KDSORT                          
051620        END-IF                                                            
051700        IF IN-NAP-UNIT = 'MTR'                                            
051800           MOVE 'M '           TO SORTWSP-KDSORT                          
051900        END-IF                                                            
052000        IF IN-NAP-UNIT = 'MTK'                                            
052100           MOVE 'M2'           TO SORTWSP-KDSORT                          
052200        END-IF                                                            
052300        IF IN-NAP-UNIT = 'MTQ'                                            
052400           MOVE 'M3'           TO SORTWSP-KDSORT                          
052500        END-IF                                                            
052600        IF IN-NAP-UNIT = 'MLT'                                            
052700           MOVE 'ML'           TO SORTWSP-KDSORT                          
052800        END-IF                                                            
052900        IF IN-NAP-UNIT = 'LTR'                                            
053000           MOVE 'L '           TO SORTWSP-KDSORT                          
053100        END-IF                                                            
053200        IF IN-NAP-UNIT = 'GRM'                                            
053300           MOVE 'G '           TO SORTWSP-KDSORT                          
053400        END-IF                                                            
053500        IF IN-NAP-UNIT = 'KGM'                                            
053600           MOVE 'KG'           TO SORTWSP-KDSORT                          
053700        END-IF                                                            
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 BC-SKAPA-940         SECTION.                                            
054200     SKIP2                                                                
054300     MOVE SPACE                TO SORTWSO-A311T940                        
054400     MOVE 940                  TO SORTWSO-PT                              
054500     MOVE IN-NAP-IDARTNR       TO SORTWSO-ARTNR                           
054600     MOVE IN-NAP-VENDOR        TO SORTWSO-GSDB-LEV                        
054610                                  SORTWSO-IDLEVNR-SHIP                    
054700     MOVE '1'                  TO SORTWSO-LEVSUFF                         
054800     MOVE '1002 '              TO SORTWSO-GSDB-FORB                       
054900     MOVE IN-NAP-OBJECT-ID     TO W-BESTNR10                              
055000     MOVE ZERO                 TO W-BESTNR12-00                           
055100     MOVE W-BESTPREF           TO SORTWSO-BESTPREF                        
055200     MOVE W-BESTLNR            TO SORTWSO-BESTLNR                         
055300     MOVE W-BESTSUFF           TO SORTWSO-BESTSUFF                        
055400*****MOVE DAGENS-DATUM         TO SORTWSO-DATUM-UTSKR                     
055500     MOVE IN-NAP-VPER-START    TO W-DATUM8                                
055600     MOVE W-DATUM6             TO SORTWSO-DATUM-UTSKR                     
055700     MOVE ZERO                 TO SORTWSO-ANT-BESTANN                     
055800     MOVE ZERO                 TO SORTWSO-BESTPRIS                        
055900     MOVE ZERO                 TO SORTWSO-KDVAL-BEST                      
056000     MOVE ZERO                 TO SORTWSO-KDENH-BEST                      
056100     MOVE SPACE                TO SORTWSO-KDSORT                          
056200     MOVE SPACE                TO SORTWSO-KDVAL-ISO                       
056300     MOVE 1002                 TO SORTWSO-FORBNR                          
056400     MOVE ZERO                 TO SORTWSO-LEVNUM                          
056500     MOVE 2                    TO SORTWS-IDPTYP                           
056600     MOVE IN-NAP-IDARTNR       TO SORTWS-IDARTNR                          
056700     .                                                                    
056800     EJECT                                                                
056900 BD-SKAPA-941         SECTION.                                            
057000     SKIP2                                                                
057100     MOVE SPACE                TO SORTWSA-A311T941                        
057200     MOVE 941                  TO SORTWSA-PT                              
057300     MOVE IN-NAP-IDARTNR       TO SORTWSA-ARTNR                           
057400     MOVE IN-NAP-VENDOR        TO SORTWSA-GSDB-LEV                        
057410                                  SORTWSA-IDLEVNR-SHIP                    
057500     MOVE '1'                  TO SORTWSA-LEVSUFF                         
057600     MOVE '1002 '              TO SORTWSA-GSDB-FORB                       
057700     MOVE IN-NAP-OBJECT-ID     TO W-BESTNR10                              
057800     MOVE ZERO                 TO W-BESTNR12-00                           
057900     MOVE W-BESTPREF           TO SORTWSA-BESTPREF                        
058000     MOVE W-BESTLNR            TO SORTWSA-BESTLNR                         
058100     MOVE W-BESTSUFF           TO SORTWSA-BESTSUFF                        
058200     MOVE DAGENS-DATUM         TO SORTWSA-DATUM-UTSKR                     
058300     MOVE 999999999            TO SORTWSA-ANT-BESTANN                     
058400     MOVE 1002                 TO SORTWSA-FORBNR                          
058500     MOVE ZERO                 TO SORTWSA-LEVNUM                          
058600     MOVE 3                    TO SORTWS-IDPTYP                           
058700     MOVE IN-NAP-IDARTNR       TO SORTWS-IDARTNR                          
058800     .                                                                    
058900     EJECT                                                                
059000 BE-SKAPA-984         SECTION.                                            
059100     SKIP2                                                                
059200     MOVE SPACE                TO SORTWSI-A310T984                        
059300     MOVE 984                  TO SORTWSI-PT                              
059400     MOVE '1002 '              TO SORTWSI-GSDB-FORB                       
059500     MOVE IN-NAP-IDARTNR       TO SORTWSI-ARTNR                           
059600     MOVE IN-NAP-PUR-GROUP     TO SORTWSI-IDINK                           
059700     MOVE ZERO                 TO SORTWSI-INKNR-NY                        
059800     MOVE ZERO                 TO SORTWSI-INKNR-GAM                       
059900     MOVE 1002                 TO SORTWSI-FORBNR                          
060000     MOVE 4                    TO SORTWS-IDPTYP                           
060100     MOVE IN-NAP-IDARTNR       TO SORTWS-IDARTNR                          
060200     .                                                                    
060300     EJECT                                                                
060400 BF-TEST-ANNULLERAD   SECTION.                                            
060500     SKIP2                                                                
060600     MOVE +1  TO IX-TAB                                                   
060700     MOVE NEJ TO SW-ANN                                                   
060800     PERFORM UNTIL IX-TAB > TAB-MAX OR                                    
060900        SW-ANN = JA                                                       
061000        IF REGIN-NAP-OBJECT-ID = TAB-OBJECT-ID (IX-TAB) AND               
061100           REGIN-NAP-ORDERED-PROD = TAB-ORDERED-PROD (IX-TAB)             
061200           MOVE JA TO SW-ANN                                              
061300        END-IF                                                            
061400        ADD +1 TO IX-TAB                                                  
061500     END-PERFORM                                                          
061600     .                                                                    
061700     EJECT                                                                
061800 C-SORT-OUTPUT SECTION.                                                   
061900     SKIP2                                                                
062000     PERFORM S32-SORT-RETURN                                              
062100     PERFORM UNTIL END-OF-SORTFIL                                         
062200       MOVE SORTWS-POST TO UT-AREA                                        
062300       PERFORM S11-SKRIV-W09275                                           
062400       PERFORM S32-SORT-RETURN                                            
062500     END-PERFORM                                                          
062600     .                                                                    
062700     EJECT                                                                
062800 Z-FINIT SECTION.                                                         
062900     CLOSE NAPIN                                                          
063000           W09274I                                                        
063100           W09274U                                                        
063200           W09275                                                         
063300           W09277                                                         
063400     SKIP2                                                                
063500     MOVE 'S' TO POSTSUM-OPKOD                                            
063600     CALL POSTSUM USING POSTSUM-PARM                                      
063700     .                                                                    
063800     EJECT                                                                
063900 S01-LAES-NAPIN SECTION.                                                  
064000     READ NAPIN INTO IN-AREA                                              
064100     AT END                                                               
064200        MOVE HIGH-VALUE TO IN-AREA                                        
064300        SET END-OF-NAPIN TO TRUE                                          
064400                                                                          
064500     NOT AT END                                                           
064600        MOVE 'NAP '     TO POSTSUM-FDNAMN                                 
064700        MOVE 'W09274D1' TO POSTSUM-DDNAMN2                                
064800        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
064900        CALL POSTSUM USING POSTSUM-PARM                                   
065000     END-READ                                                             
065100     .                                                                    
065200     EJECT                                                                
065300 S02-LAES-W09274I SECTION.                                                
065400     READ W09274I INTO REGIN-AREA                                         
065500     AT END                                                               
065600        MOVE HIGH-VALUE TO REGIN-AREA                                     
065700        SET END-OF-W09274I TO TRUE                                        
065800                                                                          
065900     NOT AT END                                                           
066000        MOVE 'W09274'   TO POSTSUM-FDNAMN                                 
066100        MOVE 'W09274D2' TO POSTSUM-DDNAMN2                                
066200        MOVE 'REGI'     TO POSTSUM-TRANSTYP                               
066300        CALL POSTSUM USING POSTSUM-PARM                                   
066400     END-READ                                                             
066500     .                                                                    
066600     EJECT                                                                
066700 S10-SKRIV-W09274U SECTION.                                               
066800                                                                          
066900     WRITE REGUT-POST FROM REGUT-AREA                                     
067000                                                                          
067100     MOVE 'REGU'     TO POSTSUM-TRANSTYP                                  
067200     MOVE 'W09274'   TO POSTSUM-FDNAMN                                    
067300     MOVE 'W09274D3' TO POSTSUM-DDNAMN2                                   
067400     CALL POSTSUM USING POSTSUM-PARM                                      
067500     .                                                                    
067600     EJECT                                                                
067700 S11-SKRIV-W09275 SECTION.                                                
067800                                                                          
067900     WRITE UT-POST FROM UT-AREA                                           
068000                                                                          
068100     MOVE 'UTPT'     TO POSTSUM-TRANSTYP                                  
068200     IF UT-PT = 940                                                       
068300        MOVE '940'   TO POSTSUM-TRANSTYP                                  
068400     END-IF                                                               
068500     IF UT-PT = 941                                                       
068600        MOVE '941'   TO POSTSUM-TRANSTYP                                  
068700     END-IF                                                               
068800     IF UT-PT = 984                                                       
068900        MOVE '984'   TO POSTSUM-TRANSTYP                                  
069000     END-IF                                                               
069100     IF UT-PT = 985                                                       
069200        MOVE '985'   TO POSTSUM-TRANSTYP                                  
069300     END-IF                                                               
069400     MOVE 'W09275'   TO POSTSUM-FDNAMN                                    
069500     MOVE 'W09274D4' TO POSTSUM-DDNAMN2                                   
069600     CALL POSTSUM USING POSTSUM-PARM                                      
069700     .                                                                    
069800     EJECT                                                                
069900 S12-SKRIV-W09277 SECTION.                                                
070000                                                                          
070100     WRITE LARM-POST FROM LARM-AREA                                       
070200                                                                          
070300     MOVE 'LARM'     TO POSTSUM-TRANSTYP                                  
070400     MOVE 'W09277'   TO POSTSUM-FDNAMN                                    
070500     MOVE 'W09274D5' TO POSTSUM-DDNAMN2                                   
070600     CALL POSTSUM USING POSTSUM-PARM                                      
070700     .                                                                    
070800     EJECT                                                                
070900 S31-SORT-RELEASE  SECTION.                                               
071000                                                                          
071100     RELEASE SORT-POST FROM SORTWS-AREA                                   
071200     .                                                                    
071300     EJECT                                                                
071400 S32-SORT-RETURN  SECTION.                                                
071500                                                                          
071600     RETURN SORTFIL INTO SORTWS-AREA                                      
071700     AT END                                                               
071800         SET END-OF-SORTFIL TO TRUE                                       
071900     .                                                                    
072000     EJECT                                                                
072100 S99-ABEND SECTION.                                                       
072200                                                                          
072300     SKIP2                                                                
072400     MOVE 'S' TO POSTSUM-OPKOD                                            
072500     CALL POSTSUM USING POSTSUM-PARM                                      
072600     CALL ABEND USING RKOD-ABEND                                          
072700     .                                                                    

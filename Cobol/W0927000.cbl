000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0927000.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   03/12/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        TAR EMOT FIL FRÅN EPIC                                           
001000*        SPARAR POSTER (BUY,BPA) VARS INF TID EJ UPPNÅDD                  
001100*        SÄNDER POSTER TILL W09278(985,940,984,941)                       
001200*        SÄNDER POSTER MED STANDARDPRISER (SSK,POB)                       
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FIL FRÅN EPIC                                              
002800     SELECT EPICSE                     ASSIGN TO W09270D1.                
002900     SKIP2                                                                
003000*          --- REG SPARADE POSTER                                         
003100     SELECT W09270I                    ASSIGN TO W09270D2.                
003200     SKIP2                                                                
003300*          --- REG SPARADE POSTER UT                                      
003400     SELECT W09270U                    ASSIGN TO W09270D3.                
003500     SKIP2                                                                
003600*          --- FIL TILL W09278                                            
003700     SELECT W09271                     ASSIGN TO W09270D4.                
003800     SKIP2                                                                
003900*          --- STANDARDPRISER                                             
004000     SELECT W09272                     ASSIGN TO W09270D5.                
004100     SKIP2                                                                
004200*          --- SORTERINGSFIL                                              
004300     SELECT SORTFIL                    ASSIGN TO W09270DS.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP3                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  EPICSE                                                               
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300 01  FILLER              PIC X(82).                                       
005400                                                                          
005500*01  -COPY IS061OOP      -L.                                              
005600                                                                          
005700*01  -COPY IS061BPA      -L.                                              
005800                                                                          
005900*01  -COPY IS061POB      -L.                                              
006000                                                                          
006100*01  -COPY IS061SSK      -L.                                              
006200                                                                          
006300*01  -COPY IS061BUY      -L.                                              
006400     SKIP3                                                                
006500 FD  W09270I                                                              
006600     RECORDING       V                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900 01  FILLER              PIC X(82).                                       
007000                                                                          
007100*01  -COPY IS061BPA      -L.                                              
007200                                                                          
007300*01  -COPY IS061BUY      -L.                                              
007400     SKIP3                                                                
007500 FD  W09270U                                                              
007600     RECORDING       V                                                    
007700     BLOCK CONTAINS  0.                                                   
007800                                                                          
007900 01  FILLER              PIC X(82).                                       
008000                                                                          
008100 01  REGUT-POST        PIC X(79).                                         
008200                                                                          
008300*01  POST -COPY IS061BPA -PRE  REGUTO-  -L.                               
008400                                                                          
008500*01  POST -COPY IS061BUY -PRE  REGUTI-  -L.                               
008600     SKIP3                                                                
008700 FD  W09271                                                               
008800     RECORDING       V                                                    
008900     BLOCK CONTAINS  0.                                                   
009000                                                                          
009100 01  UT-POST            PIC X(58).                                        
009200                                                                          
009300*01  POST -COPY A311G940 -PRE  UTO-  -L.                                  
009400                                                                          
009500*01  POST -COPY A311G941 -PRE  UTA-  -L.                                  
009600                                                                          
009700*01  POST -COPY A310G984 -PRE  UTI-  -L.                                  
009800                                                                          
009900*01  POST -COPY A310G985 -PRE  UTP-  -L.                                  
010000     SKIP3                                                                
010100 FD  W09272                                                               
010200     RECORDING       V                                                    
010300     BLOCK CONTAINS  0.                                                   
010400                                                                          
010500 01  STD-POST           PIC X(53).                                        
010600                                                                          
010700*01  POST -COPY IS061SSK -PRE  STDS-  -L.                                 
010800                                                                          
010900*01  POST -COPY IS061POB -PRE  STDA-  -L.                                 
011000     SKIP2                                                                
011100 SD  SORTFIL.                                                             
011200                                                                          
011300 01  SORT-POST.                                                           
011400     03  SORT-IDPTYP             PIC 9(1).                                
011500     03  SORT-IDARTNR            PIC S9(9) COMP-3.                        
011600     03  SORT-DATA               PIC X(58).                               
011700                                                                          
011800*03  POST940 -COPY A311G940 -PRE  SORT- REDEFINES SORT-DATA               
011900     SKIP2                                                                
012000*03  POST941 -COPY A311G941 -PRE  SORT- REDEFINES SORT-DATA               
012100     SKIP2                                                                
012200*03  POST984 -COPY A310G984 -PRE  SORT- REDEFINES SORT-DATA               
012300     SKIP2                                                                
012400*03  POST985 -COPY A310G985 -PRE  SORT- REDEFINES SORT-DATA               
012500     EJECT                                                                
012600 WORKING-STORAGE SECTION.                                                 
012700                                                                          
012800 77  IDPGM                       PIC X(8)    VALUE 'W0927000'.            
012900 77  JA                          PIC X       VALUE 'J'.                   
013000 77  NEJ                         PIC X       VALUE 'N'.                   
013100                                                                          
013200 77  EPICSE-EOF-SW               PIC X       VALUE 'N'.                   
013300     88  END-OF-EPICSE                       VALUE 'J'.                   
013400                                                                          
013500 77  W09270I-EOF-SW              PIC X       VALUE 'N'.                   
013600     88  END-OF-W09270I                      VALUE 'J'.                   
013700                                                                          
013800 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
013900     88  END-OF-SORTFIL                      VALUE 'J'.                   
014000                                                                          
014100 01  DIV.                                                                 
014200     03  BPA                     PIC X(3)    VALUE 'BPA'.                 
014300     03  OOP                     PIC X(3)    VALUE 'OOP'.                 
014400     03  BUY                     PIC X(3)    VALUE 'BUY'.                 
014500     03  POB                     PIC X(3)    VALUE 'POB'.                 
014600     03  SSK                     PIC X(3)    VALUE 'SSK'.                 
014700     03  W-DATUM8                PIC 9(8).                                
014800     03  FILLER  REDEFINES W-DATUM8.                                      
014900         05  FILLER              PIC 9(2).                                
015000         05  W-DATUM6            PIC 9(6).                                
015100     03  W-BESTNR12              PIC 9(12).                               
015200     03  FILLER  REDEFINES W-BESTNR12.                                    
015300         05  W-BESTPREF          PIC 9(3).                                
015400         05  W-BESTLNR           PIC 9(6).                                
015500         05  W-BESTSUFF          PIC 9(3).                                
015600     EJECT                                                                
015700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015800 01  FILLER REDEFINES DAGENS-DATUM.                                       
015900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016200 01  NEXTDAY-DATUM               PIC 9(6)    VALUE ZERO.                  
016300     EJECT                                                                
016400 01  DYNAMISKA-SUBPROGRAM.                                                
016500*                                                                         
016600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
016800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017000     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
017100     SKIP2                                                                
017200*    --- PARAMETRAR TILL ABEND                                            
017300                                                                          
017400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017700     SKIP2                                                                
017800 01  FELTEXT.                                                             
017900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL DATKORT                                          
018300*                                                                         
018400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W09270'.              
018500     SKIP2                                                                
018600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
018700     SKIP2                                                                
018800*01  -COPY WDATKORT                                                       
018900     EJECT                                                                
019000*    --- PARAMETRAR TILL POSTSUM                                          
019100*                                                                         
019200*01  -COPY W0005   -PRE  POSTSUM-                                         
019300     EJECT                                                                
019400*01  -COPY WDATAREA                                                       
019500     EJECT                                                                
019600*01  -COPY WORKAREA                                                       
019700     EJECT                                                                
019800 01  IN-AREA-START               PIC X(24)   VALUE                        
019900                                 'IN-AREA-START  '.                       
020000     SKIP2                                                                
020100 01  IN-AREA.                                                             
020200     03  IN-AREA-0.                                                       
020300       05  IN-RT                 PIC X(3).                                
020400       05  FILLER                PIC X(79).                               
020500*   03  FILLER -COPY IS061OOP  -PRE INP-  -RED  IN-AREA-0                 
020600     SKIP3                                                                
020700*   03  FILLER -COPY IS061BPA  -PRE INO-  -RED  IN-AREA-0                 
020800     SKIP3                                                                
020900*   03  FILLER -COPY IS061POB  -PRE INA-  -RED  IN-AREA-0                 
021000     SKIP3                                                                
021100*   03  FILLER -COPY IS061SSK  -PRE INS-  -RED  IN-AREA-0                 
021200     SKIP3                                                                
021300*   03  FILLER -COPY IS061BUY  -PRE INI-  -RED  IN-AREA-0                 
021400     EJECT                                                                
021500 01  REGIN-AREA-START            PIC X(24)   VALUE                        
021600                                 'REGIN-AREA-START  '.                    
021700     SKIP2                                                                
021800 01  REGIN-AREA.                                                          
021900     03  REGIN-AREA-0.                                                    
022000       05  REGIN-RT              PIC X(3).                                
022100       05  FILLER                PIC X(76).                               
022200                                                                          
022300*   03  FILLER -COPY IS061BUY  -PRE REGINI-  -RED  REGIN-AREA-0           
022400     SKIP3                                                                
022500*   03  FILLER -COPY IS061BPA  -PRE REGINO-  -RED  REGIN-AREA-0           
022600     EJECT                                                                
022700 01  REGUT-AREA-START            PIC X(24)   VALUE                        
022800                                 'REGUT-AREA-START  '.                    
022900     SKIP2                                                                
023000 01  REGUT-AREA.                                                          
023100     03  REGUT-AREA-0.                                                    
023200       05  REGUT-RT              PIC X(3).                                
023300       05  FILLER                PIC X(76).                               
023400                                                                          
023500*   03  FILLER -COPY IS061BUY  -PRE REGUTI-  -RED  REGUT-AREA-0           
023600     SKIP3                                                                
023700*   03  FILLER -COPY IS061BPA  -PRE REGUTO-  -RED  REGUT-AREA-0           
023800     EJECT                                                                
023900 01  UT-AREA-START               PIC X(24)   VALUE                        
024000                                 'UT-AREA-START  '.                       
024100     SKIP2                                                                
024200 01  UT-AREA.                                                             
024300     03  UT-AREA-0.                                                       
024400       05  UT-PT                 PIC S9(3)  COMP-3.                       
024500       05  FILLER                PIC X(51).                               
024600                                                                          
024700*   03  FILLER -COPY A311G940  -PRE UTO-  -RED  UT-AREA-0                 
024800     SKIP3                                                                
024900*   03  FILLER -COPY A311G941  -PRE UTA-  -RED  UT-AREA-0                 
025000     SKIP3                                                                
025100*   03  FILLER -COPY A310G984  -PRE UTI-  -RED  UT-AREA-0                 
025200     SKIP3                                                                
025300*   03  FILLER -COPY A310G985  -PRE UTP-  -RED  UT-AREA-0                 
025400     EJECT                                                                
025500 01  STD-AREA-START              PIC X(24)   VALUE                        
025600                                 'STD-AREA-START  '.                      
025700     SKIP2                                                                
025800 01  STD-AREA.                                                            
025900     03  STD-AREA-0.                                                      
026000       05  STD-RT                PIC X(3).                                
026100       05  FILLER                PIC X(51).                               
026200                                                                          
026300*   03  FILLER -COPY IS061SSK  -PRE STDS-  -RED  STD-AREA-0               
026400     SKIP3                                                                
026500*   03  FILLER -COPY IS061POB  -PRE STDA-  -RED  STD-AREA-0               
026600     EJECT                                                                
026700 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
026800                                  'SORTWS-AREA-START  '.                  
026900     SKIP2                                                                
027000 01  SORTWS-AREA.                                                         
027100         05  SORTWS-IDPTYP       PIC 9(1).                                
027200         05  SORTWS-IDARTNR      PIC S9(9) COMP-3.                        
027300         05  SORTWS-POST         PIC X(58).                               
027400                                                                          
027500*   05  FILLER -COPY A311G940  -PRE SORTWSO- -RED SORTWS-POST             
027600      SKIP3                                                               
027700*   05  FILLER -COPY A311G941  -PRE SORTWSA- -RED SORTWS-POST             
027800      SKIP3                                                               
027900*   05  FILLER -COPY A310G984  -PRE SORTWSI- -RED SORTWS-POST             
028000      SKIP3                                                               
028100*   05  FILLER -COPY A310G985  -PRE SORTWSP- -RED SORTWS-POST             
028200                                                                          
028300 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
028400     EJECT                                                                
028500 PROCEDURE DIVISION.                                                      
028600 MAIN SECTION.                                                            
028700     SKIP2                                                                
028800                                                                          
028900     PERFORM A-INIT                                                       
029000                                                                          
029100     SORT SORTFIL ASCENDING KEY SORT-IDPTYP                               
029200                                SORT-IDARTNR                              
029300                  INPUT  PROCEDURE B-SORT-INPUT                           
029400                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
029500                                                                          
029600     IF SORT-RETURN NOT = 0                                               
029700       MOVE SORT-RETURN TO SORT-RETURN-X                                  
029800       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
029900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
030000       DISPLAY FELTEXT                                                    
030100       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
030200       PERFORM S99-ABEND                                                  
030300     ELSE                                                                 
030400       PERFORM Z-FINIT                                                    
030500                                                                          
030600       MOVE ZERO TO RETURN-CODE                                           
030700       GOBACK                                                             
030800     END-IF                                                               
030900                                                                          
031000     .                                                                    
031100     EJECT                                                                
031200 A-INIT SECTION.                                                          
031300                                                                          
031400     OPEN INPUT  EPICSE                                                   
031500                 W09270I                                                  
031600                                                                          
031700     OPEN OUTPUT W09270U                                                  
031800                 W09271                                                   
031900                 W09272                                                   
032000     SKIP2                                                                
032100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
032200     MOVE D-AAR         TO DAGENS-DATUM-AAR                               
032300     MOVE D-MAANAD      TO DAGENS-DATUM-MAANAD                            
032400     MOVE D-DAG         TO DAGENS-DATUM-DAG                               
032500     MOVE IDPGM         TO POSTSUM-PROGNAMN                               
032600     MOVE DAGENS-DATUM  TO NEXTDAY-DATUM                                  
032700     ADD  +1            TO NEXTDAY-DATUM                                  
032800                                                                          
032900     MOVE 002           TO WORK-KDCALL                                    
033000     MOVE 11            TO WORK-IDDC                                      
033100     MOVE DAGENS-DATUM  TO WORK-TIAAMMDD-FOM                              
033200     MOVE ZERO          TO WORK-TIAAMMDD-TOM                              
033300     MOVE 2             TO WORK-KVWORKD                                   
033400                                                                          
033500     CALL WORKDAY USING WORK-KDCALL                                       
033600                        WORK-DATE-AREA                                    
033700                        WORK-KDSVAR                                       
033800                                                                          
033900     IF WORK-KDSVAR-OK                                                    
034000        MOVE WORK-TIAAMMDD-TOM          TO NEXTDAY-DATUM                  
034100** ??   MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO NEXTDAY-DATUM                  
034200     END-IF                                                               
034300     DISPLAY 'WORKDAY ' WORK-TIAAMMDD-TOM ' '                             
034400                        WORK-TIAAMMDD-NEXT-WORKDAY                        
034500     .                                                                    
034600     EJECT                                                                
034700 B-SORT-INPUT  SECTION.                                                   
034800                                                                          
034900     PERFORM S01-LAES-EPICSE                                              
035000     PERFORM UNTIL END-OF-EPICSE                                          
035100       IF IN-RT = OOP                                                     
035200         PERFORM BA-SKAPA-985-AV-OOP                                      
035300         PERFORM S31-SORT-RELEASE                                         
035400       END-IF                                                             
035500                                                                          
035600       IF IN-RT = BPA                                                     
035700          IF INO-ORDERDATE-END > '00000000'                               
035800             MOVE INO-ORDERDATE-END  TO W-DATUM8                          
035900             IF W-DATUM6 <= NEXTDAY-DATUM                                 
036000***             AND INO-ORDTYPE = 'PROD'                                  
036100                PERFORM BD-SKAPA-941-AV-BPA                               
036200                PERFORM S31-SORT-RELEASE                                  
036300             ELSE                                                         
036400                MOVE INO-IS061BPA  TO REGUT-AREA                          
036500                PERFORM  S10-SKRIV-W09270U                                
036600             END-IF                                                       
036700          ELSE                                                            
036800             MOVE INO-ORDERDATE-FROM TO W-DATUM8                          
036900             IF W-DATUM6 <= NEXTDAY-DATUM AND                             
037000                INO-ORDTYPE = 'PROD'                                      
037100                PERFORM BC-SKAPA-940-AV-BPA                               
037200                PERFORM S31-SORT-RELEASE                                  
037300             ELSE                                                         
037400                MOVE INO-IS061BPA  TO REGUT-AREA                          
037500                PERFORM  S10-SKRIV-W09270U                                
037600             END-IF                                                       
037700          END-IF                                                          
037800       END-IF                                                             
037900                                                                          
038000       IF IN-RT = BUY                                                     
038100          MOVE INI-DATE-VALID-FR TO W-DATUM8                              
038200          IF W-DATUM6 <= NEXTDAY-DATUM                                    
038300             PERFORM BE-SKAPA-984-AV-BUY                                  
038400             PERFORM S31-SORT-RELEASE                                     
038500          ELSE                                                            
038600             MOVE INI-IS061BUY  TO REGUT-AREA                             
038700             PERFORM  S10-SKRIV-W09270U                                   
038800          END-IF                                                          
038900       END-IF                                                             
039000                                                                          
039100       IF IN-RT = SSK                                                     
039200          MOVE INS-IS061SSK  TO STD-AREA                                  
039300          PERFORM  S12-SKRIV-W09272                                       
039400       END-IF                                                             
039500       IF IN-RT = POB                                                     
039600          MOVE INA-IS061POB  TO STD-AREA                                  
039700          PERFORM  S12-SKRIV-W09272                                       
039800       END-IF                                                             
039900       PERFORM S01-LAES-EPICSE                                            
040000     END-PERFORM                                                          
040100                                                                          
040200     PERFORM S02-LAES-W09270I                                             
040300     PERFORM UNTIL END-OF-W09270I                                         
040400       IF REGIN-RT = BPA                                                  
040500          IF REGINO-ORDERDATE-END > '00000000'                            
040600             MOVE REGINO-ORDERDATE-END  TO W-DATUM8                       
040700             IF W-DATUM6 <= NEXTDAY-DATUM                                 
040800***             AND REGINO-ORDTYPE = 'PROD'                               
040900                MOVE REGINO-IS061BPA TO INO-IS061BPA                      
041000                PERFORM BD-SKAPA-941-AV-BPA                               
041100                PERFORM S31-SORT-RELEASE                                  
041200             ELSE                                                         
041300                MOVE REGINO-IS061BPA  TO REGUT-AREA                       
041400                PERFORM  S10-SKRIV-W09270U                                
041500             END-IF                                                       
041600          ELSE                                                            
041700             MOVE REGINO-ORDERDATE-FROM TO W-DATUM8                       
041800             IF W-DATUM6 <= NEXTDAY-DATUM AND                             
041900                REGINO-ORDTYPE = 'PROD'                                   
042000                MOVE REGINO-IS061BPA TO INO-IS061BPA                      
042100                PERFORM BC-SKAPA-940-AV-BPA                               
042200                PERFORM S31-SORT-RELEASE                                  
042300             ELSE                                                         
042400                MOVE REGINO-IS061BPA  TO REGUT-AREA                       
042500                PERFORM  S10-SKRIV-W09270U                                
042600             END-IF                                                       
042700          END-IF                                                          
042800       END-IF                                                             
042900                                                                          
043000       IF REGIN-RT = BUY                                                  
043100          MOVE REGINI-DATE-VALID-FR TO W-DATUM8                           
043200          IF W-DATUM6 <= NEXTDAY-DATUM                                    
043300             MOVE REGINI-IS061BUY TO INI-IS061BUY                         
043400             PERFORM BE-SKAPA-984-AV-BUY                                  
043500             PERFORM S31-SORT-RELEASE                                     
043600          ELSE                                                            
043700             MOVE REGINI-IS061BUY  TO REGUT-AREA                          
043800             PERFORM  S10-SKRIV-W09270U                                   
043900          END-IF                                                          
044000       END-IF                                                             
044100       PERFORM S02-LAES-W09270I                                           
044200     END-PERFORM                                                          
044300     .                                                                    
044400     EJECT                                                                
044500 BA-SKAPA-985-AV-OOP  SECTION.                                            
044600     SKIP2                                                                
044700     MOVE SPACE                TO SORTWSP-A310T985                        
044800     MOVE 985                  TO SORTWSP-PT                              
044900     MOVE '1002 '              TO SORTWSP-GSDB-FORB                       
045000     MOVE INP-PARTNO           TO SORTWSP-ARTNR                           
045100     MOVE INP-SUPPLIER-ID      TO SORTWSP-GSDB-LEV                        
045200     COMPUTE SORTWSP-PRISSKR-BEST =                                       
045300             INP-ORDER-PRICE-SEK / 100                                    
045400     MOVE INP-ORDER-PRICE-LOC  TO SORTWSP-BESTPRIS                        
045500     MOVE INP-PRICE-UNIT       TO SORTWSP-KDENH-BEST                      
045600     MOVE INP-UOM              TO SORTWSP-KDSORT                          
045700     MOVE '0'                  TO SORTWSP-KDPRIS-TYP                      
045800     MOVE ZERO                 TO SORTWSP-KDVAL-BEST                      
045900     MOVE INP-EFFECTIVE-DATE   TO W-DATUM8                                
046000     MOVE W-DATUM6             TO SORTWSP-DATUM-BESTPRIS                  
046100     MOVE ZERO                 TO SORTWSP-INKNR                           
046200     MOVE SPACE                TO SORTWSP-BESTTYP                         
046300     MOVE INP-LOC-CURR         TO SORTWSP-KDVAL-ISO                       
046400     MOVE 1002                 TO SORTWSP-FORBNR                          
046500     MOVE ZERO                 TO SORTWSP-LEVNUM                          
046600     MOVE 1                    TO SORTWS-IDPTYP                           
046700     MOVE INP-PARTNO           TO SORTWS-IDARTNR                          
046800     MOVE INP-PACK-TYPE-CODE   TO SORTWSP-KDFPKPRI                        
046900     .                                                                    
047000     EJECT                                                                
047100*BX-VALIDERA-DAG      SECTION.                                            
047200     SKIP2                                                                
047300*    MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
047400*    MOVE AAMMDD TO DAT-I-TIDATUM                                         
047500                                                                          
047600*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
047700*                    DAT-O-TIDATUM DAT-KDSVAR                             
047800                                                                          
047900*    IF DAT-KDSVAR-OK                                                     
048000*      ......                                                             
048100*    ELSE                                                                 
048200*      .........                                                          
048300*    END-IF                                                               
048400*    .                                                                    
048500     EJECT                                                                
048600 BC-SKAPA-940-AV-BPA  SECTION.                                            
048700     SKIP2                                                                
048800     MOVE SPACE                TO SORTWSO-A311T940                        
048900     MOVE 940                  TO SORTWSO-PT                              
049000     MOVE INO-PARTNO           TO SORTWSO-ARTNR                           
049100     MOVE INO-SUPPLIER-ID      TO SORTWSO-GSDB-LEV                        
049200     MOVE INO-SUPPLIER-SHIP    TO SORTWSO-IDLEVNR-SHIP                    
049300     MOVE '1'                  TO SORTWSO-LEVSUFF                         
049400     MOVE '1002 '              TO SORTWSO-GSDB-FORB                       
049500     MOVE INO-ORDERNO          TO W-BESTNR12                              
049600     MOVE W-BESTPREF           TO SORTWSO-BESTPREF                        
049700     MOVE W-BESTLNR            TO SORTWSO-BESTLNR                         
049800     MOVE W-BESTSUFF           TO SORTWSO-BESTSUFF                        
049900     IF INO-ORDERDATE-FROM > '00000000'                                   
050000        MOVE INO-ORDERDATE-FROM   TO W-DATUM8                             
050100        MOVE W-DATUM6          TO SORTWSO-DATUM-UTSKR                     
050200     ELSE                                                                 
050300        MOVE DAGENS-DATUM      TO SORTWSO-DATUM-UTSKR                     
050400     END-IF                                                               
050500     MOVE ZERO                 TO SORTWSO-ANT-BESTANN                     
050600     MOVE ZERO                 TO SORTWSO-BESTPRIS                        
050700     MOVE ZERO                 TO SORTWSO-KDVAL-BEST                      
050800     MOVE ZERO                 TO SORTWSO-KDENH-BEST                      
050900     MOVE SPACE                TO SORTWSO-KDSORT                          
051000     MOVE SPACE                TO SORTWSO-KDVAL-ISO                       
051100     MOVE 1002                 TO SORTWSO-FORBNR                          
051200     MOVE ZERO                 TO SORTWSO-LEVNUM                          
051300     MOVE 2                    TO SORTWS-IDPTYP                           
051400     MOVE INO-PARTNO           TO SORTWS-IDARTNR                          
051500     .                                                                    
051600     EJECT                                                                
051700 BD-SKAPA-941-AV-BPA  SECTION.                                            
051800     SKIP2                                                                
051900     MOVE SPACE                TO SORTWSA-A311T941                        
052000     MOVE 941                  TO SORTWSA-PT                              
052100     MOVE INO-PARTNO           TO SORTWSA-ARTNR                           
052200     MOVE INO-SUPPLIER-ID      TO SORTWSA-GSDB-LEV                        
052210     MOVE INO-SUPPLIER-SHIP    TO SORTWSA-IDLEVNR-SHIP                    
052300     MOVE '1'                  TO SORTWSA-LEVSUFF                         
052400     MOVE '1002 '              TO SORTWSA-GSDB-FORB                       
052500     MOVE INO-ORDERNO          TO W-BESTNR12                              
052600     MOVE W-BESTPREF           TO SORTWSA-BESTPREF                        
052700     MOVE W-BESTLNR            TO SORTWSA-BESTLNR                         
052800     MOVE W-BESTSUFF           TO SORTWSA-BESTSUFF                        
052900     MOVE DAGENS-DATUM         TO SORTWSA-DATUM-UTSKR                     
053000     MOVE 999999999            TO SORTWSA-ANT-BESTANN                     
053100     MOVE 1002                 TO SORTWSA-FORBNR                          
053200     MOVE ZERO                 TO SORTWSA-LEVNUM                          
053300     MOVE 3                    TO SORTWS-IDPTYP                           
053400     MOVE INO-PARTNO           TO SORTWS-IDARTNR                          
053500     .                                                                    
053600     EJECT                                                                
053700 BE-SKAPA-984-AV-BUY  SECTION.                                            
053800     SKIP2                                                                
053900     MOVE SPACE                TO SORTWSI-A310T984                        
054000     MOVE 984                  TO SORTWSI-PT                              
054100     MOVE '1002 '              TO SORTWSI-GSDB-FORB                       
054200     MOVE INI-PARTNO           TO SORTWSI-ARTNR                           
054300     MOVE INI-BUYER-CODE       TO SORTWSI-IDINK                           
054400     MOVE ZERO                 TO SORTWSI-INKNR-NY                        
054500     MOVE ZERO                 TO SORTWSI-INKNR-GAM                       
054600     MOVE 1002                 TO SORTWSI-FORBNR                          
054700     MOVE 4                    TO SORTWS-IDPTYP                           
054800     MOVE INI-PARTNO           TO SORTWS-IDARTNR                          
054900     .                                                                    
055000     EJECT                                                                
055100 C-SORT-OUTPUT SECTION.                                                   
055200     SKIP2                                                                
055300     PERFORM S32-SORT-RETURN                                              
055400     PERFORM UNTIL END-OF-SORTFIL                                         
055500       MOVE SORTWS-POST TO UT-AREA                                        
055600       PERFORM S11-SKRIV-W09271                                           
055700       PERFORM S32-SORT-RETURN                                            
055800     END-PERFORM                                                          
055900     .                                                                    
056000     EJECT                                                                
056100 Z-FINIT SECTION.                                                         
056200     CLOSE EPICSE                                                         
056300           W09270I                                                        
056400           W09270U                                                        
056500           W09271                                                         
056600           W09272                                                         
056700     SKIP2                                                                
056800     MOVE 'S' TO POSTSUM-OPKOD                                            
056900     CALL POSTSUM USING POSTSUM-PARM                                      
057000     .                                                                    
057100     EJECT                                                                
057200 S01-LAES-EPICSE  SECTION.                                                
057300     READ EPICSE INTO IN-AREA                                             
057400     AT END                                                               
057500        MOVE HIGH-VALUE TO IN-AREA                                        
057600        SET END-OF-EPICSE TO TRUE                                         
057700                                                                          
057800     NOT AT END                                                           
057900        MOVE 'EPICSE'   TO POSTSUM-FDNAMN                                 
058000        MOVE 'W09270D1' TO POSTSUM-DDNAMN2                                
058100        MOVE IN-RT      TO POSTSUM-TRANSTYP                               
058200        CALL POSTSUM USING POSTSUM-PARM                                   
058300     END-READ                                                             
058400     .                                                                    
058500     EJECT                                                                
058600 S02-LAES-W09270I SECTION.                                                
058700     READ W09270I INTO REGIN-AREA                                         
058800     AT END                                                               
058900        MOVE HIGH-VALUE TO REGIN-AREA                                     
059000        SET END-OF-W09270I TO TRUE                                        
059100                                                                          
059200     NOT AT END                                                           
059300        MOVE 'W09270'   TO POSTSUM-FDNAMN                                 
059400        MOVE 'W09270D2' TO POSTSUM-DDNAMN2                                
059500        MOVE REGIN-RT   TO POSTSUM-TRANSTYP                               
059600        CALL POSTSUM USING POSTSUM-PARM                                   
059700     END-READ                                                             
059800     .                                                                    
059900     EJECT                                                                
060000 S10-SKRIV-W09270U SECTION.                                               
060100                                                                          
060200     WRITE REGUT-POST FROM REGUT-AREA                                     
060300                                                                          
060400     MOVE REGUT-RT   TO POSTSUM-TRANSTYP                                  
060500     MOVE 'W09270'   TO POSTSUM-FDNAMN                                    
060600     MOVE 'W09270D3' TO POSTSUM-DDNAMN2                                   
060700     CALL POSTSUM USING POSTSUM-PARM                                      
060800     .                                                                    
060900     EJECT                                                                
061000 S11-SKRIV-W09271 SECTION.                                                
061100                                                                          
061200     WRITE UT-POST FROM UT-AREA                                           
061300                                                                          
061400     MOVE 'UTPT'     TO POSTSUM-TRANSTYP                                  
061500     IF UT-PT = 940                                                       
061600        MOVE '940'   TO POSTSUM-TRANSTYP                                  
061700     END-IF                                                               
061800     IF UT-PT = 941                                                       
061900        MOVE '941'   TO POSTSUM-TRANSTYP                                  
062000     END-IF                                                               
062100     IF UT-PT = 984                                                       
062200        MOVE '984'   TO POSTSUM-TRANSTYP                                  
062300     END-IF                                                               
062400     IF UT-PT = 985                                                       
062500        MOVE '985'   TO POSTSUM-TRANSTYP                                  
062600     END-IF                                                               
062700     MOVE 'W09271'   TO POSTSUM-FDNAMN                                    
062800     MOVE 'W09270D4' TO POSTSUM-DDNAMN2                                   
062900     CALL POSTSUM USING POSTSUM-PARM                                      
063000     .                                                                    
063100     EJECT                                                                
063200 S12-SKRIV-W09272 SECTION.                                                
063300                                                                          
063400     WRITE STD-POST FROM STD-AREA                                         
063500                                                                          
063600     MOVE STD-RT     TO POSTSUM-TRANSTYP                                  
063700     MOVE 'W09272'   TO POSTSUM-FDNAMN                                    
063800     MOVE 'W09270D5' TO POSTSUM-DDNAMN2                                   
063900     CALL POSTSUM USING POSTSUM-PARM                                      
064000     .                                                                    
064100     EJECT                                                                
064200 S31-SORT-RELEASE  SECTION.                                               
064300                                                                          
064400     RELEASE SORT-POST FROM SORTWS-AREA                                   
064500     .                                                                    
064600     EJECT                                                                
064700 S32-SORT-RETURN  SECTION.                                                
064800                                                                          
064900     RETURN SORTFIL INTO SORTWS-AREA                                      
065000     AT END                                                               
065100         SET END-OF-SORTFIL TO TRUE                                       
065200     .                                                                    
065300     EJECT                                                                
065400 S99-ABEND SECTION.                                                       
065500                                                                          
065600     SKIP2                                                                
065700     MOVE 'S' TO POSTSUM-OPKOD                                            
065800     CALL POSTSUM USING POSTSUM-PARM                                      
065900     CALL ABEND USING RKOD-ABEND                                          
066000     .                                                                    

000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3304000.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  NOVEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*                                                                         
000900*    FUNKTION:                                                            
001000*    HÄMTAR STATISTIK (AF-OMFATTNINGAR) SUMMERAR S-URVALEN.               
001100*    MATCHAR URVALSKÖ MOT SEKUNDÄR REGISTER OCH INFO-FILEN.               
001200*    PLOCKAR DE POSTER SOM BEHÖVS FÖR VARJE URVAL.                        
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*    --- INFILER:                                                         
002100     SELECT W33015                       ASSIGN TO W33040D1.              
002200     SELECT W33022                       ASSIGN TO W33040D9.              
002300     SELECT W33019                       ASSIGN TO W33040D2.              
002400     SELECT W33031                       ASSIGN TO W33040D3.              
002500*    --- UTFILER:                                                         
002600* P-LISTOR P/PLV/PPV                                                      
002700     SELECT W33041                       ASSIGN TO W33040D4.              
002800* S-LISTOR PÅ FSGVÄRDE                                                    
002900     SELECT W33053                       ASSIGN TO W33040D5.              
003000     SELECT W33057                       ASSIGN TO W33040D6.              
003100     SELECT W33061                       ASSIGN TO W33040D7.              
003200* A1/A2-LISTOR                                                            
003300     SELECT W33047                       ASSIGN TO W33040D8.              
003400* S-LISTOR PÅ ANTAL                                                       
003500     SELECT W33054                       ASSIGN TO W33040DA.              
003600     SELECT W33058                       ASSIGN TO W33040DB.              
003700     SELECT W33062                       ASSIGN TO W33040DC.              
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W33015                                                               
004400     LABEL RECORD   STANDARD                                              
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800*01  POST -COPY W33014      -L -PRE I15-.                                 
004900     EJECT                                                                
005000 FD  W33022                                                               
005100     LABEL RECORD   STANDARD                                              
005200     RECORDING      F                                                     
005300     BLOCK CONTAINS 0.                                                    
005400     SKIP2                                                                
005500*01  POST -COPY W33014  -L -PRE I22-.                                     
005600     EJECT                                                                
005700 FD  W33019                                                               
005800     LABEL RECORD   STANDARD                                              
005900     RECORDING      F                                                     
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200*01  POST -COPY W33019  -L -PRE I19-.                                     
006300     EJECT                                                                
006400 FD  W33031                                                               
006500     LABEL RECORD   STANDARD                                              
006600     RECORDING      V                                                     
006700     BLOCK CONTAINS 0.                                                    
006800     SKIP2                                                                
006900*01  POST -COPY W3303103  -L -PRE I3103-.                                 
007000     SKIP2                                                                
007100*01  POST -COPY W3303102  -L -PRE I3102-.                                 
007200     SKIP2                                                                
007300*01  POST -COPY W3303104  -L -PRE I3104-.                                 
007400     EJECT                                                                
007500 FD  W33041                                                               
007600     LABEL RECORD   STANDARD                                              
007700     RECORDING      F                                                     
007800     BLOCK CONTAINS 0.                                                    
007900     SKIP2                                                                
008000*01  POST  -COPY W33041  -L -PRE U41-.                                    
008100     EJECT                                                                
008200 FD  W33053                                                               
008300     LABEL RECORD   STANDARD                                              
008400     RECORDING      F                                                     
008500     BLOCK CONTAINS 0.                                                    
008600     SKIP2                                                                
008700*01  POST -COPY W33053  -L -PRE U53-.                                     
008800     EJECT                                                                
008900 FD  W33054                                                               
009000     LABEL RECORD   STANDARD                                              
009100     RECORDING      F                                                     
009200     BLOCK CONTAINS 0.                                                    
009300     SKIP2                                                                
009400*01  POST -COPY W33053  -L -PRE U54-.                                     
009500     EJECT                                                                
009600 FD  W33057                                                               
009700     LABEL RECORD   STANDARD                                              
009800     RECORDING      F                                                     
009900     BLOCK CONTAINS 0.                                                    
010000     SKIP2                                                                
010100*01  POST  -COPY W33057  -L -PRE U57-.                                    
010200     EJECT                                                                
010300 FD  W33058                                                               
010400     LABEL RECORD   STANDARD                                              
010500     RECORDING      F                                                     
010600     BLOCK CONTAINS 0.                                                    
010700     SKIP2                                                                
010800*01  POST  -COPY W33057  -L -PRE U58-.                                    
010900     EJECT                                                                
011000 FD  W33061                                                               
011100     LABEL RECORD   STANDARD                                              
011200     RECORDING      F                                                     
011300     BLOCK CONTAINS 0.                                                    
011400     SKIP2                                                                
011500*01  POST  -COPY W33061 -L -PRE U61-.                                     
011600     EJECT                                                                
011700 FD  W33062                                                               
011800     LABEL RECORD   STANDARD                                              
011900     RECORDING      F                                                     
012000     BLOCK CONTAINS 0.                                                    
012100     SKIP2                                                                
012200*01  POST  -COPY W33061 -L -PRE U62-.                                     
012300     EJECT                                                                
012400 FD  W33047                                                               
012500     LABEL RECORD   STANDARD                                              
012600     RECORDING      F                                                     
012700     BLOCK CONTAINS 0.                                                    
012800     SKIP2                                                                
012900*01  POST  -COPY W33041  -L -PRE U47-.                                    
013000     EJECT                                                                
013100 WORKING-STORAGE SECTION.                                                 
013200     SKIP2                                                                
013201                                                                          
013210*    -- CHECKED BY WY2000                                                 
013300 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3304000'.            
013400 77  JA                          PIC X(1)    VALUE 'J'.                   
013500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
013600 77  SEKUNDAR-EOF                PIC X(1)    VALUE 'N'.                   
013700 77  SEKUNDAR-II-EOF             PIC X(1)    VALUE 'N'.                   
013800 77  INFO-EOF                    PIC X(1)    VALUE 'N'.                   
013900 77  URVAL-EOF                   PIC X(1)    VALUE 'N'.                   
014000 77  P-LV-PV                     PIC X(1)    VALUE 'N'.                   
014100 77  OVR                         PIC X(1)    VALUE 'N'.                   
014200 77  SW-IFYLLT                   PIC X(1)    VALUE 'N'.                   
014300 77  SW-TRAEFF                   PIC X(1)    VALUE 'N'.                   
014400 77  IX                          PIC S9(4)   VALUE +1  COMP SYNC.         
014500 77  TAB-IX                      PIC S9(4)   VALUE +1  COMP SYNC.         
014600 77  FELKOD                      PIC S9(4)   VALUE +0  COMP SYNC.         
014700 77  TOT-RAKNARE-SEKU            PIC S9(9)   VALUE +0  COMP.              
014800 77  TOT-RAKNARE-II-SEKU         PIC S9(9)   VALUE +0  COMP.              
014900 77  TOT-RAKNARE-INFO            PIC S9(9)   VALUE +0  COMP.              
015000 77  TOT-RAKNARE-URV             PIC S9(9)   VALUE +0  COMP.              
015100 77  TOT-RAKNARE-41              PIC S9(9)   VALUE +0  COMP.              
015200 77  TOT-RAKNARE-53              PIC S9(9)   VALUE +0  COMP.              
015300 77  TOT-RAKNARE-57              PIC S9(9)   VALUE +0  COMP.              
015400 77  TOT-RAKNARE-61              PIC S9(9)   VALUE +0  COMP.              
015500 77  TOT-RAKNARE-47              PIC S9(9)   VALUE +0  COMP.              
015600 77  TOT-RAKNARE-54              PIC S9(9)   VALUE +0  COMP.              
015700 77  TOT-RAKNARE-58              PIC S9(9)   VALUE +0  COMP.              
015800 77  TOT-RAKNARE-62              PIC S9(9)   VALUE +0  COMP.              
015910                                                                          
015920 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
015930 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
015940 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
015950 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
015960                                                                          
016000     EJECT                                                                
016100                                                                          
016200 01  DYNAMISKA-SUBPROGRAM.                                                
016300*                                                                         
016400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016600     SKIP2                                                                
016700*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
016800*                                                                         
016900 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
017000*01  -COPY W0005 -PRE  POSTSUM-                                           
017100     EJECT                                                                
017200*                                                                         
017300 01  FILLER                     PIC X(16)  VALUE 'TABELL-AREOR'.          
017400 01  TABELL.                                                              
017500     03  TAB-RAD OCCURS 100.                                              
017600        05  TAB-AREA.                                                     
017700          07  FILLER                     PIC X(28).                       
017800          07  TAB-IDPTYP                 PIC X(3).                        
017900          07  FILLER                     PIC X(1).                        
018000          07  TAB-IDTRANS                PIC X(4).                        
018100          07  FILLER                     PIC X(2579).                     
018200*01  AREA  -PRE T2-  -COPY W3303102.                                      
018300     EJECT                                                                
018400*01  AREA  -PRE T3-  -COPY W3303103.                                      
018500     EJECT                                                                
018600*01  AREA  -PRE T4-  -COPY W3303104.                                      
018700     EJECT                                                                
018800 01  T4TABELL.                                                            
018900     03  T4TAB-RAD OCCURS 100.                                            
019000*05  AREA  -PRE T4TAB-  -COPY W33053.                                     
019100     EJECT                                                                
019200*01  AREA  -PRE T453-  -COPY W33053.                                      
019300     EJECT                                                                
019400*01  AREA  -PRE T454-  -COPY W33053.                                      
019500     EJECT                                                                
019600*01  AREA  -PRE T457-  -COPY W33057.                                      
019700     EJECT                                                                
019800*01  AREA  -PRE T458-  -COPY W33057.                                      
019900     EJECT                                                                
020000*01  AREA  -PRE T461-  -COPY W33061.                                      
020100     EJECT                                                                
020200*01  AREA  -PRE T462-  -COPY W33061.                                      
020300     EJECT                                                                
020400*********************************'INFILER'******************              
020500 01  FILLER                       PIC X(16)  VALUE 'SEKUND-REG'.          
020600*                                                                         
020700*01  AREA -COPY W33014  -PRE I15-.                                        
020800     EJECT                                                                
020900 01  FILLER                 PIC X(16)  VALUE 'SEKUND-REG-II'.             
021000*                                                                         
021100*01  AREA -COPY W33014  -PRE I22-.                                        
021200     EJECT                                                                
021300 01  FILLER                       PIC X(16)  VALUE 'INFO-FIL'.            
021400*01  AREA -COPY W33019  -PRE I19-.                                        
021500     EJECT                                                                
021600*                                                                         
021700 01  FILLER                       PIC X(16)  VALUE '31-FILEN'.            
021800 01  I31-AREA.                                                            
021900   03  FILLER                     PIC X(32).                              
022000   03  I31-IDTRANS                PIC X(4).                               
022100   03  FILLER                     PIC X(2579).                            
022200     SKIP2                                                                
022300*01  AREA  -PRE I3102- -COPY W3303102  -RED I31-AREA.                     
022400    EJECT                                                                 
022500*01  AREA  -PRE I3103- -COPY W3303103  -RED I31-AREA.                     
022600    EJECT                                                                 
022700*01  AREA  -PRE I3104- -COPY W3303104  -RED I31-AREA.                     
022800    EJECT                                                                 
022900 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
023000*01  AREA  -COPY W33041  -PRE U41-.                                       
023100     EJECT                                                                
023200*01  AREA  -COPY W33053  -PRE U53-.                                       
023300    EJECT                                                                 
023400*01  AREA  -COPY W33057  -PRE U57-.                                       
023500     EJECT                                                                
023600*01  AREA  -COPY W33061  -PRE U61-.                                       
023700     EJECT                                                                
023800 PROCEDURE DIVISION.                                                      
023900     SKIP2                                                                
024000 STYR SECTION.                                                            
024100     PERFORM A-INIT                                                       
024200     PERFORM S01-LAS-SEKUNDAR-REGISTER                                    
024300     PERFORM S09-LAS-SEKUNDAR-II-REGISTER                                 
024400     PERFORM S04-LAS-INFOFIL                                              
024500     PERFORM B-SKAPA-TAB-SKRIV-FILER                                      
024600     PERFORM UNTIL SEKUNDAR-EOF = JA AND SEKUNDAR-II-EOF = JA             
024700        IF INFO-EOF = JA                                                  
024800           MOVE +16                TO FELKOD                              
024900           CALL ABEND USING FELKOD                                        
025000        ELSE                                                              
025100           IF I19-IDARTNR = I15-IDARTNR                                   
025200             PERFORM C-MATCHA-TABELL-SKRIV-FILER                          
025300             PERFORM S01-LAS-SEKUNDAR-REGISTER                            
025400           ELSE                                                           
025500             IF I19-IDARTNR = I22-IDARTNR                                 
025600               PERFORM D-MATCHA-TABELL-SKRIV-FILER                        
025700               PERFORM S09-LAS-SEKUNDAR-II-REGISTER                       
025800             ELSE                                                         
025900               PERFORM S04-LAS-INFOFIL                                    
026000             END-IF                                                       
026100           END-IF                                                         
026200        END-IF                                                            
026300     END-PERFORM                                                          
026400     PERFORM E-TOEM-TABELL-SKRIV-FILER                                    
026500     PERFORM Z-FINIT                                                      
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 A-INIT SECTION.                                                          
027010     MOVE 'A-INIT'   TO WS-SEKTION                                        
027100     SKIP2                                                                
027200     OPEN INPUT  W33015                                                   
027300                 W33022                                                   
027400                 W33019                                                   
027500                 W33031                                                   
027600     OPEN OUTPUT W33041                                                   
027700                 W33053                                                   
027800                 W33057                                                   
027900                 W33061                                                   
028000                 W33047                                                   
028100                 W33054                                                   
028200                 W33058                                                   
028300                 W33062                                                   
028400                                                                          
028500     INITIALIZE TABELL                                                    
028600                T4TABELL                                                  
028700                T453-AREA                                                 
028800                T457-AREA                                                 
028900                T461-AREA                                                 
029000                                                                          
029100     MOVE +0 TO TOT-RAKNARE-SEKU                                          
029200     MOVE +0 TO TOT-RAKNARE-II-SEKU                                       
029300     MOVE +0 TO TOT-RAKNARE-INFO                                          
029400     MOVE +0 TO TOT-RAKNARE-URV                                           
029500     MOVE +0 TO TOT-RAKNARE-41                                            
029600     MOVE +0 TO TOT-RAKNARE-53                                            
029700     MOVE +0 TO TOT-RAKNARE-57                                            
029800     MOVE +0 TO TOT-RAKNARE-61                                            
029900     MOVE +0 TO TOT-RAKNARE-47                                            
030000     MOVE +0 TO TOT-RAKNARE-54                                            
030100     MOVE +0 TO TOT-RAKNARE-58                                            
030200     MOVE +0 TO TOT-RAKNARE-62                                            
030300     .                                                                    
030400     EJECT                                                                
030500 B-SKAPA-TAB-SKRIV-FILER SECTION.                                         
030510     MOVE 'B-SKAPA-TAB-SKRIV-FILER' TO WS-SEKTION                         
030600     SKIP2                                                                
030700     PERFORM S03-LAS-URVALSFIL                                            
030800     MOVE +1 TO TAB-IX                                                    
030900     PERFORM UNTIL URVAL-EOF = JA OR TAB-IX > +100                        
031000        IF I31-IDTRANS = '3202'                                           
031100           MOVE I3102-AREA            TO TAB-AREA (TAB-IX)                
031200        ELSE                                                              
031300           IF I31-IDTRANS = '3203'                                        
031400              MOVE I3103-AREA         TO TAB-AREA (TAB-IX)                
031500           ELSE                                                           
031600              MOVE I3104-AREA         TO TAB-AREA (TAB-IX)                
031700              MOVE I3104-001-GRUPP    TO                                  
031800                                      T4TAB-001-GRUPP (TAB-IX)            
031900           END-IF                                                         
032000        END-IF                                                            
032100        IF I3102-IDPTYP = 'PLV' OR 'PPV'                                  
032200          MOVE JA TO P-LV-PV                                              
032300        ELSE                                                              
032400          MOVE JA TO OVR                                                  
032500        END-IF                                                            
032600        IF I3103-IDPTYP = 'PLV' OR 'PPV'                                  
032700          MOVE JA TO P-LV-PV                                              
032800        ELSE                                                              
032900          MOVE JA TO OVR                                                  
033000        END-IF                                                            
033100        PERFORM S03-LAS-URVALSFIL                                         
033200        ADD +1                        TO TAB-IX                           
033300     END-PERFORM                                                          
033400     IF P-LV-PV = NEJ                                                     
033500       MOVE +999999999 TO I22-IDARTNR                                     
033600       MOVE JA TO SEKUNDAR-II-EOF                                         
033700     END-IF                                                               
033800     IF OVR = NEJ                                                         
033900       MOVE +999999999 TO I15-IDARTNR                                     
034000       MOVE JA TO SEKUNDAR-EOF                                            
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 C-MATCHA-TABELL-SKRIV-FILER SECTION.                                     
034410     MOVE 'C-MATCHA-TABELL-SKRIV-FILER' TO WS-SEKTION                     
034500     SKIP2                                                                
034600     MOVE +1 TO TAB-IX                                                    
034700     PERFORM UNTIL TAB-IDTRANS (TAB-IX) = SPACE OR                        
034800     TAB-IX > +100                                                        
034900        IF TAB-IDTRANS (TAB-IX) = '3202'                                  
035000           IF TAB-IDPTYP(TAB-IX) NOT = 'PPV'                              
035100             IF TAB-IDPTYP(TAB-IX) NOT = 'PLV'                            
035200               MOVE TAB-AREA (TAB-IX)   TO T2-AREA                        
035300               PERFORM CA-TESTA-3202-URVAL                                
035400             END-IF                                                       
035500           END-IF                                                         
035600        ELSE                                                              
035700           IF TAB-IDTRANS (TAB-IX) = '3203'                               
035800              IF TAB-IDPTYP(TAB-IX) NOT = 'PPV'                           
035900                IF TAB-IDPTYP(TAB-IX) NOT = 'PLV'                         
036000                  MOVE TAB-AREA (TAB-IX)   TO T3-AREA                     
036100                  PERFORM CB-TESTA-3203-URVAL                             
036200                END-IF                                                    
036300              END-IF                                                      
036400           ELSE                                                           
036500              IF TAB-IDTRANS (TAB-IX) = '3204'                            
036600                 MOVE TAB-AREA (TAB-IX)   TO T4-AREA                      
036700                 PERFORM CC-TESTA-3204-URVAL                              
036800              END-IF                                                      
036900           END-IF                                                         
037000        END-IF                                                            
037100        ADD +1 TO TAB-IX                                                  
037200     END-PERFORM                                                          
037300     .                                                                    
037400     EJECT                                                                
037500 CA-TESTA-3202-URVAL     SECTION.                                         
037510     MOVE 'CA-TESTA-3202-URVAL' TO WS-SEKTION                             
037600     SKIP2                                                                
037700* SW-IFYLLT TÄNDS OM NÅGOT FINNS PÅ URVALSFILERNA                         
037800* SW-TRAEFF TÄNDS OM DESSA STÄMMER MED INFOFILEN                          
037900     MOVE NEJ                   TO SW-IFYLLT                              
038000     MOVE NEJ                   TO SW-TRAEFF                              
038100     PERFORM CAA-KOLLA-KONC-DISTR-MARKN                                   
038200     IF SW-IFYLLT = JA AND SW-TRAEFF = JA                                 
038300     OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                               
038400        MOVE NEJ                TO SW-IFYLLT                              
038500        MOVE NEJ                TO SW-TRAEFF                              
038600        PERFORM CAB-KOLLA-IDANSK                                          
038700        IF SW-IFYLLT = JA AND SW-TRAEFF = JA                              
038800        OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                            
038900           MOVE NEJ             TO SW-IFYLLT                              
039000           MOVE NEJ             TO SW-TRAEFF                              
039100           PERFORM CAC-KOLLA-KDVVKL                                       
039200           IF SW-IFYLLT = JA AND SW-TRAEFF = JA                           
039300           OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                         
039400              MOVE NEJ          TO SW-IFYLLT                              
039500              MOVE NEJ          TO SW-TRAEFF                              
039600              PERFORM CAD-KOLLA-IDLKTO                                    
039700              IF SW-IFYLLT = JA AND SW-TRAEFF = JA                        
039800              OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                      
039900                 MOVE NEJ       TO SW-IFYLLT                              
040000                 MOVE NEJ       TO SW-TRAEFF                              
040100                 PERFORM CAE-KOLLA-IDLEVNR                                
040200                 IF SW-IFYLLT = JA AND SW-TRAEFF = JA                     
040300                 OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                   
040400                    MOVE NEJ       TO SW-IFYLLT                           
040500                    MOVE NEJ       TO SW-TRAEFF                           
040600                    PERFORM CAF-KOLLA-KDPRODSL                            
040700                    IF SW-IFYLLT = JA AND SW-TRAEFF = JA                  
040800                    OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                
040900                       MOVE NEJ       TO SW-IFYLLT                        
041000                       MOVE NEJ       TO SW-TRAEFF                        
041100                       PERFORM CAG-KOLLA-IDFKNGRP                         
041200                       IF SW-IFYLLT = JA AND SW-TRAEFF = JA               
041300                       OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ             
041400                          MOVE T2-001-GRUPP          TO                   
041500                                                  U41-001-GRUPP           
041600                          MOVE +5                 TO U41-IDGTYP           
041700                          PERFORM S10-FLYTTA-T-U41-AREA                   
041800                          IF U41-IDPTYP = 'P1 '                           
041900                             PERFORM S02-SKRIV-41-FIL                     
042000                          ELSE                                            
042100                             PERFORM S08-SKRIV-47-FIL                     
042200                          END-IF                                          
042300                       END-IF                                             
042400                    END-IF                                                
042500                 END-IF                                                   
042600              END-IF                                                      
042700           END-IF                                                         
042800        END-IF                                                            
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 CAA-KOLLA-KONC-DISTR-MARKN SECTION.                                      
043210     MOVE 'CAA-KOLLA-KONC-DISTR-MARKN' TO WS-SEKTION                      
043300     SKIP2                                                                
043400                                                                          
043500     IF T2-IDKONCNR (1) > ZERO          OR                                
043600        T2-IDKONCNR (2) > ZERO          OR                                
043700        T2-IDKONCNR (3) > ZERO          OR                                
043800        T2-IDKONCNR (4) > ZERO          OR                                
043900        T2-IDKONCNR (5) > ZERO          OR                                
044000        T2-IDKONCNR (6) > ZERO          OR                                
044100        T2-IDKONCNR (7) > ZERO          OR                                
044200        T2-IDKONCNR (8) > ZERO                                            
044300        MOVE JA                         TO SW-IFYLLT                      
044400*FIX PGA SPLITT 930601 KAN TAS BORT 950701 (TVÅ ÅR)                       
044500**** FÖR ATT INTE FÅ MED SIG KONCNR 0 SOM ALLA GAMLA                      
044600**** LV DISTRIKT HAR.                                                     
044700        IF SW-IFYLLT = JA AND I15-IDKONCNR = +0                           
044800          MOVE +999 TO I15-IDKONCNR                                       
044900        END-IF                                                            
045000*FIX                                                                      
045100        IF T2-IDKONCNR (1) = I15-IDKONCNR     OR                          
045200           T2-IDKONCNR (2) = I15-IDKONCNR     OR                          
045300           T2-IDKONCNR (3) = I15-IDKONCNR     OR                          
045400           T2-IDKONCNR (4) = I15-IDKONCNR     OR                          
045500           T2-IDKONCNR (5) = I15-IDKONCNR     OR                          
045600           T2-IDKONCNR (6) = I15-IDKONCNR     OR                          
045700           T2-IDKONCNR (7) = I15-IDKONCNR     OR                          
045800           T2-IDKONCNR (8) = I15-IDKONCNR                                 
045900           MOVE JA                      TO SW-TRAEFF                      
046000        END-IF                                                            
046100     ELSE                                                                 
046200        IF T2-IDDISTR-TOM (1) > ZERO  OR                                  
046300           T2-IDDISTR-TOM (2) > ZERO  OR                                  
046400           T2-IDDISTR-TOM (3) > ZERO  OR                                  
046500           T2-IDDISTR-TOM (4) > ZERO                                      
046600           MOVE JA                    TO SW-IFYLLT                        
046700           IF (T2-IDDISTR-TOM (1) NOT < I15-IDDISTR                       
046800              AND I15-IDDISTR NOT < T2-IDDISTR-FOM (1)) OR                
046900              (T2-IDDISTR-TOM (2) NOT < I15-IDDISTR                       
047000              AND I15-IDDISTR NOT < T2-IDDISTR-FOM (2)) OR                
047100              (T2-IDDISTR-TOM (3) NOT < I15-IDDISTR                       
047200              AND I15-IDDISTR NOT < T2-IDDISTR-FOM (3)) OR                
047300              (T2-IDDISTR-TOM (4) NOT < I15-IDDISTR                       
047400              AND I15-IDDISTR NOT < T2-IDDISTR-FOM (4))                   
047500              MOVE JA                 TO SW-TRAEFF                        
047600           END-IF                                                         
047700        ELSE                                                              
047800           IF T2-KDMARK-BUDG-TOM (1) > ZERO  OR                           
047900              T2-KDMARK-BUDG-TOM (2) > ZERO  OR                           
048000              T2-KDMARK-BUDG-TOM (3) > ZERO  OR                           
048100              T2-KDMARK-BUDG-TOM (4) > ZERO  OR                           
048200              T2-KDMARK-BUDG-TOM (5) > ZERO  OR                           
048300              T2-KDMARK-BUDG-TOM (6) > ZERO  OR                           
048400              T2-KDMARK-BUDG-TOM (7) > ZERO  OR                           
048500              T2-KDMARK-BUDG-TOM (8) > ZERO                               
048600              MOVE JA                    TO SW-IFYLLT                     
048700              IF (T2-KDMARK-BUDG-TOM (1) NOT <                            
048800                 I15-KDMARK-BUDG                                          
048900                 AND I15-KDMARK-BUDG NOT <                                
049000                 T2-KDMARK-BUDG-FOM (1))     OR                           
049100                 (T2-KDMARK-BUDG-TOM (2) NOT <                            
049200                 I15-KDMARK-BUDG                                          
049300                 AND I15-KDMARK-BUDG NOT <                                
049400                 T2-KDMARK-BUDG-FOM (2))     OR                           
049500                 (T2-KDMARK-BUDG-TOM (3) NOT <                            
049600                 I15-KDMARK-BUDG                                          
049700                 AND I15-KDMARK-BUDG NOT <                                
049800                 T2-KDMARK-BUDG-FOM (3))     OR                           
049900                 (T2-KDMARK-BUDG-TOM (4) NOT <                            
050000                 I15-KDMARK-BUDG                                          
050100                 AND I15-KDMARK-BUDG NOT <                                
050200                 T2-KDMARK-BUDG-FOM (4))     OR                           
050300                 (T2-KDMARK-BUDG-TOM (5) NOT <                            
050400                 I15-KDMARK-BUDG                                          
050500                 AND I15-KDMARK-BUDG NOT <                                
050600                 T2-KDMARK-BUDG-FOM (5))     OR                           
050700                 (T2-KDMARK-BUDG-TOM (6) NOT <                            
050800                 I15-KDMARK-BUDG                                          
050900                 AND I15-KDMARK-BUDG NOT <                                
051000                 T2-KDMARK-BUDG-FOM (6))     OR                           
051100                 (T2-KDMARK-BUDG-TOM (7) NOT <                            
051200                 I15-KDMARK-BUDG                                          
051300                 AND I15-KDMARK-BUDG NOT <                                
051400                 T2-KDMARK-BUDG-FOM (7))     OR                           
051500                 (T2-KDMARK-BUDG-TOM (8) NOT <                            
051600                 I15-KDMARK-BUDG                                          
051700                 AND I15-KDMARK-BUDG NOT <                                
051800                 T2-KDMARK-BUDG-FOM (8))                                  
051900                 MOVE JA                 TO SW-TRAEFF                     
052000              END-IF                                                      
052100           END-IF                                                         
052200        END-IF                                                            
052300     END-IF                                                               
052400     .                                                                    
052500     EJECT                                                                
052600 CAB-KOLLA-IDANSK           SECTION.                                      
052610     MOVE 'CAB-KOLLA-IDANSK' TO WS-SEKTION                                
052700     SKIP2                                                                
052800                                                                          
052900     IF T2-IDANSK-TOM (1) > ZERO  OR                                      
053000        T2-IDANSK-TOM (2) > ZERO  OR                                      
053100        T2-IDANSK-TOM (3) > ZERO  OR                                      
053200        T2-IDANSK-TOM (4) > ZERO                                          
053300        MOVE JA                       TO SW-IFYLLT                        
053400     END-IF                                                               
053500     IF SW-IFYLLT = JA                                                    
053600        IF (T2-IDANSK-TOM (1) NOT < I19-IDANSK                            
053700           AND I19-IDANSK NOT < T2-IDANSK-FOM (1)) OR                     
053800           (T2-IDANSK-TOM (2) NOT < I19-IDANSK                            
053900           AND I19-IDANSK NOT < T2-IDANSK-FOM (2)) OR                     
054000           (T2-IDANSK-TOM (3) NOT < I19-IDANSK                            
054100           AND I19-IDANSK NOT < T2-IDANSK-FOM (3)) OR                     
054200           (T2-IDANSK-TOM (4) NOT < I19-IDANSK                            
054300           AND I19-IDANSK NOT < T2-IDANSK-FOM (4))                        
054400           MOVE JA                    TO SW-TRAEFF                        
054500        END-IF                                                            
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 CAC-KOLLA-KDVVKL           SECTION.                                      
054910     MOVE 'CAC-KOLLA-KDVVKL' TO WS-SEKTION                                
055000     SKIP2                                                                
055100     IF T2-KDVVKL (1) > ZERO  OR                                          
055200        T2-KDVVKL (2) > ZERO  OR                                          
055300        T2-KDVVKL (3) > ZERO  OR                                          
055400        T2-KDVVKL (4) > ZERO  OR                                          
055500        T2-KDVVKL (5) > ZERO                                              
055600        MOVE JA                        TO SW-IFYLLT                       
055700     END-IF                                                               
055800     IF SW-IFYLLT = JA                                                    
055900       IF T2-KDVVKL (1) > ZERO AND                                        
056000          T2-KDVVKL (1) = I19-KDVVKL                                      
056100            MOVE JA                     TO SW-TRAEFF                      
056200       ELSE                                                               
056300         IF T2-KDVVKL (2) > ZERO AND                                      
056400            T2-KDVVKL (2) = I19-KDVVKL                                    
056500              MOVE JA                     TO SW-TRAEFF                    
056600         ELSE                                                             
056700           IF T2-KDVVKL (3) > ZERO AND                                    
056800              T2-KDVVKL (3) = I19-KDVVKL                                  
056900                MOVE JA                     TO SW-TRAEFF                  
057000           ELSE                                                           
057100             IF T2-KDVVKL (4) > ZERO AND                                  
057200                T2-KDVVKL (4) = I19-KDVVKL                                
057300                  MOVE JA                     TO SW-TRAEFF                
057400             ELSE                                                         
057500               IF T2-KDVVKL (5) > ZERO AND                                
057600                  T2-KDVVKL (5) = I19-KDVVKL                              
057700                    MOVE JA                     TO SW-TRAEFF              
057800               END-IF                                                     
057900             END-IF                                                       
058000           END-IF                                                         
058100         END-IF                                                           
058200       END-IF                                                             
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 CAD-KOLLA-IDLKTO           SECTION.                                      
058610     MOVE 'CAD-KOLLA-KDLKTO' TO WS-SEKTION                                
058700     SKIP2                                                                
058800     IF T2-IDLKTO (1) > ZERO           OR                                 
058900        T2-IDLKTO (2) > ZERO           OR                                 
059000        T2-IDLKTO (3) > ZERO           OR                                 
059100        T2-IDLKTO (4) > ZERO           OR                                 
059200        T2-IDLKTO (5) > ZERO           OR                                 
059300        T2-IDLKTO (6) > ZERO           OR                                 
059400        T2-IDLKTO (7) > ZERO           OR                                 
059500        T2-IDLKTO (8) > ZERO                                              
059600        MOVE JA                        TO SW-IFYLLT                       
059700     END-IF                                                               
059800     IF SW-IFYLLT = JA                                                    
059900        IF T2-IDLKTO (1) = I19-IDLKTO  OR                                 
060000           T2-IDLKTO (2) = I19-IDLKTO  OR                                 
060100           T2-IDLKTO (3) = I19-IDLKTO  OR                                 
060200           T2-IDLKTO (4) = I19-IDLKTO  OR                                 
060300           T2-IDLKTO (5) = I19-IDLKTO  OR                                 
060400           T2-IDLKTO (6) = I19-IDLKTO  OR                                 
060500           T2-IDLKTO (7) = I19-IDLKTO  OR                                 
060600           T2-IDLKTO (8) = I19-IDLKTO                                     
060700           MOVE JA                     TO SW-TRAEFF                       
060800        END-IF                                                            
060900     END-IF                                                               
061000     .                                                                    
061100     EJECT                                                                
061200 CAE-KOLLA-IDLEVNR          SECTION.                                      
061210     MOVE 'CAE-KOLLA-IDLEVNR' TO WS-SEKTION                               
061300     SKIP2                                                                
061400     IF T2-IDLEVNR (1) NOT = SPACE     OR                                 
061500        T2-IDLEVNR (2) NOT = SPACE     OR                                 
061600        T2-IDLEVNR (3) NOT = SPACE     OR                                 
061700        T2-IDLEVNR (4) NOT = SPACE     OR                                 
061800        T2-IDLEVNR (5) NOT = SPACE     OR                                 
061900        T2-IDLEVNR (6) NOT = SPACE     OR                                 
062000        T2-IDLEVNR (7) NOT = SPACE     OR                                 
062100        T2-IDLEVNR (8) NOT = SPACE                                        
062200        MOVE JA                        TO SW-IFYLLT                       
062300     END-IF                                                               
062400                                                                          
062500*---- DET HAR VISAT SIG ATT VISSA ARTNR HAR LEVNR 0                       
062600*---- OM MAN DÅ GÖR URVAL LEVERANTÖR                                      
062700*---- OCH DÅ GÖR URVAL PÅ PÅ MINDRE ÄN 8 LEVERANTÖRER                     
062800*---- SÅ FÅR MAN MED SIG ALLA ARTIKLAR MED LEVNR 0                        
062900*-->>> DETTA SPÄRRAS BORT I CAEA  <<<<                                    
063000                                                                          
063100     IF SW-IFYLLT = JA                                                    
063200        PERFORM CAEA-LEVNR-NOLL-KOLL                                      
063300        IF T2-IDLEVNR (1) = I19-IDLEVNR  OR                               
063400           T2-IDLEVNR (2) = I19-IDLEVNR  OR                               
063500           T2-IDLEVNR (3) = I19-IDLEVNR  OR                               
063600           T2-IDLEVNR (4) = I19-IDLEVNR  OR                               
063700           T2-IDLEVNR (5) = I19-IDLEVNR  OR                               
063800           T2-IDLEVNR (6) = I19-IDLEVNR  OR                               
063900           T2-IDLEVNR (7) = I19-IDLEVNR  OR                               
064000           T2-IDLEVNR (8) = I19-IDLEVNR                                   
064100           MOVE JA                     TO SW-TRAEFF                       
064200        END-IF                                                            
064300     END-IF                                                               
064400     .                                                                    
064500     EJECT                                                                
064600 CAEA-LEVNR-NOLL-KOLL  SECTION.                                           
064610     MOVE 'CAEA-LEVNR-NOLL-KOLL' TO WS-SEKTION                            
064700     SKIP2                                                                
064800     IF T2-IDLEVNR (1) NOT = SPACE                                        
064900       CONTINUE                                                           
065000     ELSE                                                                 
065100       MOVE '99999' TO  T2-IDLEVNR (1)                                    
065200     END-IF                                                               
065300                                                                          
065400     IF T2-IDLEVNR (2) NOT = SPACE                                        
065500       CONTINUE                                                           
065600     ELSE                                                                 
065700       MOVE '99999' TO  T2-IDLEVNR (2)                                    
065800     END-IF                                                               
065900                                                                          
066000     IF T2-IDLEVNR (3) NOT = SPACE                                        
066100       CONTINUE                                                           
066200     ELSE                                                                 
066300       MOVE '99999' TO  T2-IDLEVNR (3)                                    
066400     END-IF                                                               
066500                                                                          
066600     IF T2-IDLEVNR (4) NOT = SPACE                                        
066700       CONTINUE                                                           
066800     ELSE                                                                 
066900       MOVE '99999' TO  T2-IDLEVNR (4)                                    
067000     END-IF                                                               
067100                                                                          
067200     IF T2-IDLEVNR (5) NOT = SPACE                                        
067300       CONTINUE                                                           
067400     ELSE                                                                 
067500       MOVE '99999' TO  T2-IDLEVNR (5)                                    
067600     END-IF                                                               
067700                                                                          
067800     IF T2-IDLEVNR (6) NOT = SPACE                                        
067900       CONTINUE                                                           
068000     ELSE                                                                 
068100       MOVE '99999' TO  T2-IDLEVNR (6)                                    
068200     END-IF                                                               
068300                                                                          
068400     IF T2-IDLEVNR (7) NOT = SPACE                                        
068500       CONTINUE                                                           
068600     ELSE                                                                 
068700       MOVE '99999' TO  T2-IDLEVNR (7)                                    
068800     END-IF                                                               
068900                                                                          
069000     IF T2-IDLEVNR (8) NOT = SPACE                                        
069100       CONTINUE                                                           
069200     ELSE                                                                 
069300       MOVE '99999' TO  T2-IDLEVNR (8)                                    
069400     END-IF                                                               
069500     .                                                                    
069600     EJECT                                                                
069700 CAF-KOLLA-KDPRODSL         SECTION.                                      
069710     MOVE 'CAF-KOLLA-KDPRODSL' TO WS-SEKTION                              
069800     SKIP2                                                                
069900     IF T2-KDPRODSL (1) > ZERO         OR                                 
070000        T2-KDPRODSL (2) > ZERO         OR                                 
070100        T2-KDPRODSL (3) > ZERO         OR                                 
070200        T2-KDPRODSL (4) > ZERO         OR                                 
070300        T2-KDPRODSL (5) > ZERO         OR                                 
070400        T2-KDPRODSL (6) > ZERO         OR                                 
070500        T2-KDPRODSL (7) > ZERO                                            
070600        MOVE JA                        TO SW-IFYLLT                       
070700     END-IF                                                               
070800     IF SW-IFYLLT = JA                                                    
070900        IF T2-KDPRODSL (1) = I19-KDPRODSL OR                              
071000           T2-KDPRODSL (2) = I19-KDPRODSL OR                              
071100           T2-KDPRODSL (3) = I19-KDPRODSL OR                              
071200           T2-KDPRODSL (4) = I19-KDPRODSL OR                              
071300           T2-KDPRODSL (5) = I19-KDPRODSL OR                              
071400           T2-KDPRODSL (6) = I19-KDPRODSL OR                              
071500           T2-KDPRODSL (7) = I19-KDPRODSL                                 
071600           MOVE JA                     TO SW-TRAEFF                       
071700        END-IF                                                            
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 CAG-KOLLA-IDFKNGRP         SECTION.                                      
072110     MOVE 'CAG-KOLLA-IDFKNGRP' TO WS-SEKTION                              
072200     SKIP2                                                                
072300     MOVE +1                            TO IX                             
072400     PERFORM UNTIL IX > +99  OR                                           
072500     SW-IFYLLT = JA AND SW-TRAEFF = JA                                    
072600        IF T2-IDFKNGRP-TOM (IX) > ZERO                                    
072700           MOVE JA                    TO SW-IFYLLT                        
072800           IF  T2-IDFKNGRP-TOM (IX) NOT < I19-IDFKNGRP                    
072900           AND I19-IDFKNGRP NOT < T2-IDFKNGRP-FOM (IX)                    
073000              MOVE JA                 TO SW-TRAEFF                        
073100           END-IF                                                         
073200        END-IF                                                            
073300        ADD  +1                         TO IX                             
073400     END-PERFORM                                                          
073500     .                                                                    
073600     EJECT                                                                
073700 CB-TESTA-3203-URVAL     SECTION.                                         
073710     MOVE 'CB-TESTA-3203-URVAL' TO WS-SEKTION                             
073800     SKIP2                                                                
073900* SW-IFYLLT TÄNDS OM NÅGOT FINNS PÅ URVALSFILERNA                         
074000* SW-TRAEFF TÄNDS OM DESSA STÄMMER MED INFOFILEN                          
074100     MOVE NEJ                   TO SW-IFYLLT                              
074200     MOVE NEJ                   TO SW-TRAEFF                              
074300     PERFORM CBA-KOLLA-KONC-DISTR-MARKN                                   
074400     IF SW-IFYLLT = JA AND SW-TRAEFF = JA                                 
074500     OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                               
074600        PERFORM CBB-KOLLA-IDARTNR-EV-SKRIV                                
074700     END-IF                                                               
074800     .                                                                    
074900     EJECT                                                                
075000 CBA-KOLLA-KONC-DISTR-MARKN SECTION.                                      
075010     MOVE 'CBA-KOLLA-KONC-DISTR-MARKN' TO WS-SEKTION                      
075100     SKIP2                                                                
075200                                                                          
075300     IF T3-IDKONCNR (1) > ZERO          OR                                
075400        T3-IDKONCNR (2) > ZERO          OR                                
075500        T3-IDKONCNR (3) > ZERO          OR                                
075600        T3-IDKONCNR (4) > ZERO          OR                                
075700        T3-IDKONCNR (5) > ZERO          OR                                
075800        T3-IDKONCNR (6) > ZERO          OR                                
075900        T3-IDKONCNR (7) > ZERO          OR                                
076000        T3-IDKONCNR (8) > ZERO                                            
076100        MOVE JA                         TO SW-IFYLLT                      
076200*FIX PGA SPLITT 930601 KAN TAS BORT 950701 (TVÅ ÅR)                       
076300**** FÖR ATT INTE FÅ MED SIG KONCNR 0 SOM ALLA GAMLA                      
076400**** LV DISTRIKT HAR.                                                     
076500        IF SW-IFYLLT = JA AND I15-IDKONCNR = +0                           
076600          MOVE +999 TO I15-IDKONCNR                                       
076700        END-IF                                                            
076800*FIX                                                                      
076900        IF T3-IDKONCNR (1) = I15-IDKONCNR     OR                          
077000           T3-IDKONCNR (2) = I15-IDKONCNR     OR                          
077100           T3-IDKONCNR (3) = I15-IDKONCNR     OR                          
077200           T3-IDKONCNR (4) = I15-IDKONCNR     OR                          
077300           T3-IDKONCNR (5) = I15-IDKONCNR     OR                          
077400           T3-IDKONCNR (6) = I15-IDKONCNR     OR                          
077500           T3-IDKONCNR (7) = I15-IDKONCNR     OR                          
077600           T3-IDKONCNR (8) = I15-IDKONCNR                                 
077700           MOVE JA                      TO SW-TRAEFF                      
077800        END-IF                                                            
077900     ELSE                                                                 
078000        IF T3-IDDISTR-TOM (1) > ZERO  OR                                  
078100           T3-IDDISTR-TOM (2) > ZERO  OR                                  
078200           T3-IDDISTR-TOM (3) > ZERO  OR                                  
078300           T3-IDDISTR-TOM (4) > ZERO                                      
078400           MOVE JA                    TO SW-IFYLLT                        
078500           IF (T3-IDDISTR-TOM (1) NOT < I15-IDDISTR                       
078600              AND I15-IDDISTR NOT < T3-IDDISTR-FOM (1)) OR                
078700              (T3-IDDISTR-TOM (2) NOT < I15-IDDISTR                       
078800              AND I15-IDDISTR NOT < T3-IDDISTR-FOM (2)) OR                
078900              (T3-IDDISTR-TOM (3) NOT < I15-IDDISTR                       
079000              AND I15-IDDISTR NOT < T3-IDDISTR-FOM (3)) OR                
079100              (T3-IDDISTR-TOM (4) NOT < I15-IDDISTR                       
079200              AND I15-IDDISTR NOT < T3-IDDISTR-FOM (4))                   
079300              MOVE JA                 TO SW-TRAEFF                        
079400           END-IF                                                         
079500        ELSE                                                              
079600           IF T3-KDMARK-BUDG-TOM (1) > ZERO  OR                           
079700              T3-KDMARK-BUDG-TOM (2) > ZERO  OR                           
079800              T3-KDMARK-BUDG-TOM (3) > ZERO  OR                           
079900              T3-KDMARK-BUDG-TOM (4) > ZERO  OR                           
080000              T3-KDMARK-BUDG-TOM (5) > ZERO  OR                           
080100              T3-KDMARK-BUDG-TOM (6) > ZERO  OR                           
080200              T3-KDMARK-BUDG-TOM (7) > ZERO  OR                           
080300              T3-KDMARK-BUDG-TOM (8) > ZERO                               
080400              MOVE JA                    TO SW-IFYLLT                     
080500              IF (T3-KDMARK-BUDG-TOM (1) NOT <                            
080600                 I15-KDMARK-BUDG                                          
080700                 AND I15-KDMARK-BUDG NOT <                                
080800                 T3-KDMARK-BUDG-FOM (1))     OR                           
080900                 (T3-KDMARK-BUDG-TOM (2) NOT <                            
081000                 I15-KDMARK-BUDG                                          
081100                 AND I15-KDMARK-BUDG NOT <                                
081200                 T3-KDMARK-BUDG-FOM (2))     OR                           
081300                 (T3-KDMARK-BUDG-TOM (3) NOT <                            
081400                 I15-KDMARK-BUDG                                          
081500                 AND I15-KDMARK-BUDG NOT <                                
081600                 T3-KDMARK-BUDG-FOM (3))     OR                           
081700                 (T3-KDMARK-BUDG-TOM (4) NOT <                            
081800                 I15-KDMARK-BUDG                                          
081900                 AND I15-KDMARK-BUDG NOT <                                
082000                 T3-KDMARK-BUDG-FOM (4))     OR                           
082100                 (T3-KDMARK-BUDG-TOM (5) NOT <                            
082200                 I15-KDMARK-BUDG                                          
082300                 AND I15-KDMARK-BUDG NOT <                                
082400                 T3-KDMARK-BUDG-FOM (5))     OR                           
082500                 (T3-KDMARK-BUDG-TOM (6) NOT <                            
082600                 I15-KDMARK-BUDG                                          
082700                 AND I15-KDMARK-BUDG NOT <                                
082800                 T3-KDMARK-BUDG-FOM (6))     OR                           
082900                 (T3-KDMARK-BUDG-TOM (7) NOT <                            
083000                 I15-KDMARK-BUDG                                          
083100                 AND I15-KDMARK-BUDG NOT <                                
083200                 T3-KDMARK-BUDG-FOM (7))     OR                           
083300                 (T3-KDMARK-BUDG-TOM (8) NOT <                            
083400                 I15-KDMARK-BUDG                                          
083500                 AND I15-KDMARK-BUDG NOT <                                
083600                 T3-KDMARK-BUDG-FOM (8))                                  
083700                 MOVE JA                 TO SW-TRAEFF                     
083800              END-IF                                                      
083900           END-IF                                                         
084000        END-IF                                                            
084100     END-IF                                                               
084200     .                                                                    
084300     EJECT                                                                
084400 CBB-KOLLA-IDARTNR-EV-SKRIV SECTION.                                      
084410     MOVE 'CBB-KOLLA-IDARTNR-EV-SKRIV' TO WS-SEKTION                      
084500     SKIP2                                                                
084600     MOVE +1                       TO IX                                  
084700     MOVE NEJ                      TO SW-TRAEFF                           
084800     PERFORM UNTIL IX > +500 OR                                           
084900     SW-TRAEFF = JA                                                       
085000        IF T3-IDARTNR (IX) = I19-IDARTNR                                  
085100           MOVE JA                  TO SW-TRAEFF                          
085200           MOVE T3-001-GRUPP          TO                                  
085300                                       U41-001-GRUPP                      
085400           MOVE +5                  TO U41-IDGTYP                         
085500           PERFORM S10-FLYTTA-T-U41-AREA                                  
085600           IF U41-IDPTYP = 'P1 '                                          
085700              PERFORM S02-SKRIV-41-FIL                                    
085800           ELSE                                                           
085900              PERFORM S08-SKRIV-47-FIL                                    
086000           END-IF                                                         
086100        END-IF                                                            
086200        ADD  +1                        TO IX                              
086300     END-PERFORM                                                          
086400     .                                                                    
086500     EJECT                                                                
086600 CC-TESTA-3204-URVAL     SECTION.                                         
086610     MOVE 'CC-TESTA-3204-URVAL' TO WS-SEKTION                             
086700     SKIP2                                                                
086800     MOVE NEJ                        TO SW-TRAEFF                         
086900     MOVE NEJ                        TO SW-IFYLLT                         
087000     PERFORM CCA-KOLLA-KONC-DISTR-MARKN                                   
087100     IF SW-IFYLLT = JA AND SW-TRAEFF = JA                                 
087200        MOVE NEJ                TO SW-IFYLLT                              
087300        MOVE NEJ                TO SW-TRAEFF                              
087400        PERFORM CCB-KOLLA-IDANSK                                          
087500       IF SW-IFYLLT = JA AND SW-TRAEFF = JA                               
087600       OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                             
087700          MOVE NEJ                        TO SW-TRAEFF                    
087800          MOVE NEJ                        TO SW-IFYLLT                    
087900          PERFORM CCC-KOLLA-IDFKNGRP                                      
088000          IF SW-IFYLLT = JA AND SW-TRAEFF = JA                            
088100          OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                          
088200             MOVE NEJ                        TO SW-TRAEFF                 
088300             MOVE NEJ                        TO SW-IFYLLT                 
088400             PERFORM CCD-KOLLA-KDPRODSL                                   
088500             IF SW-IFYLLT = JA AND SW-TRAEFF = JA                         
088600             OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                       
088700                MOVE NEJ                        TO SW-TRAEFF              
088800                MOVE NEJ                        TO SW-IFYLLT              
088900                PERFORM CCE-KOLLA-IDLEVNR                                 
089000                IF SW-IFYLLT = JA AND SW-TRAEFF = JA                      
089100                OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                    
089200                  IF T4-IDPTYP = 'S1 ' OR 'S2 ' OR 'S3 '                  
089300                    PERFORM CCF-BEHANDLA-3104-URVAL                       
089400                  ELSE                                                    
089500*T4-IDPTYP = 'S1A' OR 'S2A' OR 'S3A'                                      
089600                    PERFORM CCG-BEHANDLA-3104-URVAL                       
089700                  END-IF                                                  
089800                END-IF                                                    
089900             END-IF                                                       
090000          END-IF                                                          
090100       END-IF                                                             
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 CCA-KOLLA-KONC-DISTR-MARKN SECTION.                                      
090510     MOVE 'CCA-KOLLA-KONC-DISTR-MARKN' TO WS-SEKTION                      
090600     SKIP2                                                                
090700     IF T4-IDKONCNR (1) > ZERO          OR                                
090800        T4-IDKONCNR (2) > ZERO          OR                                
090900        T4-IDKONCNR (3) > ZERO          OR                                
091000        T4-IDKONCNR (4) > ZERO          OR                                
091100        T4-IDKONCNR (5) > ZERO          OR                                
091200        T4-IDKONCNR (6) > ZERO          OR                                
091300        T4-IDKONCNR (7) > ZERO          OR                                
091400        T4-IDKONCNR (8) > ZERO                                            
091500        MOVE JA                         TO SW-IFYLLT                      
091600*FIX PGA SPLITT 930601 KAN TAS BORT 950701 (TVÅ ÅR)                       
091700**** FÖR ATT INTE FÅ MED SIG KONCNR 0 SOM ALLA GAMLA                      
091800**** LV DISTRIKT HAR.                                                     
091900        IF SW-IFYLLT = JA AND I15-IDKONCNR = +0                           
092000          MOVE +999 TO I15-IDKONCNR                                       
092100        END-IF                                                            
092200*FIX                                                                      
092300        IF T4-IDKONCNR (1) = I15-IDKONCNR     OR                          
092400           T4-IDKONCNR (2) = I15-IDKONCNR     OR                          
092500           T4-IDKONCNR (3) = I15-IDKONCNR     OR                          
092600           T4-IDKONCNR (4) = I15-IDKONCNR     OR                          
092700           T4-IDKONCNR (5) = I15-IDKONCNR     OR                          
092800           T4-IDKONCNR (6) = I15-IDKONCNR     OR                          
092900           T4-IDKONCNR (7) = I15-IDKONCNR     OR                          
093000           T4-IDKONCNR (8) = I15-IDKONCNR                                 
093100           MOVE JA                      TO SW-TRAEFF                      
093200        END-IF                                                            
093300     ELSE                                                                 
093400        IF T4-IDDISTR-TOM (1) > ZERO  OR                                  
093500           T4-IDDISTR-TOM (2) > ZERO  OR                                  
093600           T4-IDDISTR-TOM (3) > ZERO  OR                                  
093700           T4-IDDISTR-TOM (4) > ZERO                                      
093800           MOVE JA                    TO SW-IFYLLT                        
093900           IF (T4-IDDISTR-TOM (1) NOT < I15-IDDISTR                       
094000              AND I15-IDDISTR NOT < T4-IDDISTR-FOM (1)) OR                
094100              (T4-IDDISTR-TOM (2) NOT < I15-IDDISTR                       
094200              AND I15-IDDISTR NOT < T4-IDDISTR-FOM (2)) OR                
094300              (T4-IDDISTR-TOM (3) NOT < I15-IDDISTR                       
094400              AND I15-IDDISTR NOT < T4-IDDISTR-FOM (3)) OR                
094500              (T4-IDDISTR-TOM (4) NOT < I15-IDDISTR                       
094600              AND I15-IDDISTR NOT < T4-IDDISTR-FOM (4))                   
094700              MOVE JA                 TO SW-TRAEFF                        
094800           END-IF                                                         
094900        ELSE                                                              
095000           IF T4-KDMARK-BUDG-TOM (1) > ZERO  OR                           
095100              T4-KDMARK-BUDG-TOM (2) > ZERO  OR                           
095200              T4-KDMARK-BUDG-TOM (3) > ZERO  OR                           
095300              T4-KDMARK-BUDG-TOM (4) > ZERO                               
095400              MOVE JA                    TO SW-IFYLLT                     
095500              IF (T4-KDMARK-BUDG-TOM (1) NOT <                            
095600                 I15-KDMARK-BUDG                                          
095700                 AND I15-KDMARK-BUDG NOT <                                
095800                 T4-KDMARK-BUDG-FOM (1))     OR                           
095900                 (T4-KDMARK-BUDG-TOM (2) NOT <                            
096000                 I15-KDMARK-BUDG                                          
096100                 AND I15-KDMARK-BUDG NOT <                                
096200                 T4-KDMARK-BUDG-FOM (2))     OR                           
096300                 (T4-KDMARK-BUDG-TOM (3) NOT <                            
096400                 I15-KDMARK-BUDG                                          
096500                 AND I15-KDMARK-BUDG NOT <                                
096600                 T4-KDMARK-BUDG-FOM (3))     OR                           
096700                 (T4-KDMARK-BUDG-TOM (4) NOT <                            
096800                 I15-KDMARK-BUDG                                          
096900                 AND I15-KDMARK-BUDG NOT <                                
097000                 T4-KDMARK-BUDG-FOM (4))                                  
097100                 MOVE JA                 TO SW-TRAEFF                     
097200              END-IF                                                      
097300           END-IF                                                         
097400        END-IF                                                            
097500     END-IF                                                               
097600     .                                                                    
097700     EJECT                                                                
097800 CCC-KOLLA-IDFKNGRP SECTION.                                              
097810     MOVE 'CCC-KOLLA-IDFKNGRP' TO WS-SEKTION                              
097900     SKIP2                                                                
098000     IF T4-IDFKNGRP-TOM  > ZERO                                           
098100        MOVE JA                      TO SW-IFYLLT                         
098200        IF  T4-IDFKNGRP-TOM NOT <                                         
098300        I19-IDFKNGRP                                                      
098400        AND I19-IDFKNGRP NOT <                                            
098500        T4-IDFKNGRP-FOM                                                   
098600           MOVE JA                   TO SW-TRAEFF                         
098700        END-IF                                                            
098800     END-IF                                                               
098900     .                                                                    
099000     EJECT                                                                
099100 CCB-KOLLA-IDANSK           SECTION.                                      
099110     MOVE 'CCB-KOLLA-IDANSK' TO WS-SEKTION                                
099200     SKIP2                                                                
099300                                                                          
099400     IF T4-IDANSK-TOM (1) > ZERO  OR                                      
099500        T4-IDANSK-TOM (2) > ZERO  OR                                      
099600        T4-IDANSK-TOM (3) > ZERO  OR                                      
099700        T4-IDANSK-TOM (4) > ZERO                                          
099800        MOVE JA                       TO SW-IFYLLT                        
099900     END-IF                                                               
100000     IF SW-IFYLLT = JA                                                    
100100        IF (T4-IDANSK-TOM (1) NOT < I19-IDANSK                            
100200           AND I19-IDANSK NOT < T4-IDANSK-FOM (1)) OR                     
100300           (T4-IDANSK-TOM (2) NOT < I19-IDANSK                            
100400           AND I19-IDANSK NOT < T4-IDANSK-FOM (2)) OR                     
100500           (T4-IDANSK-TOM (3) NOT < I19-IDANSK                            
100600           AND I19-IDANSK NOT < T4-IDANSK-FOM (3)) OR                     
100700           (T4-IDANSK-TOM (4) NOT < I19-IDANSK                            
100800           AND I19-IDANSK NOT < T4-IDANSK-FOM (4))                        
100900           MOVE JA                    TO SW-TRAEFF                        
101000        END-IF                                                            
101100     END-IF                                                               
101200     .                                                                    
101300     EJECT                                                                
101400 CCD-KOLLA-KDPRODSL SECTION.                                              
101410     MOVE 'CCD-KOLLA-KDPRODSL' TO WS-SEKTION                              
101500     SKIP2                                                                
101600     IF T4-KDPRODSL (1) > ZERO       OR                                   
101700        T4-KDPRODSL (2) > ZERO       OR                                   
101800        T4-KDPRODSL (3) > ZERO       OR                                   
101900        T4-KDPRODSL (4) > ZERO                                            
102000        MOVE JA                      TO SW-IFYLLT                         
102100     END-IF                                                               
102200     IF SW-IFYLLT = JA                                                    
102300        IF  T4-KDPRODSL (1) = I19-KDPRODSL OR                             
102400            T4-KDPRODSL (2) = I19-KDPRODSL OR                             
102500            T4-KDPRODSL (3) = I19-KDPRODSL OR                             
102600            T4-KDPRODSL (4) = I19-KDPRODSL                                
102700           MOVE JA                   TO SW-TRAEFF                         
102800        END-IF                                                            
102900     END-IF                                                               
103000     .                                                                    
103100     EJECT                                                                
103200 CCE-KOLLA-IDLEVNR SECTION.                                               
103210     MOVE 'CCE-KOLLA-IDLEVNR' TO WS-SEKTION                               
103300     SKIP2                                                                
103400                                                                          
103500     IF T4-IDLEVNR (1) NOT = SPACE   OR                                   
103600        T4-IDLEVNR (2) NOT = SPACE   OR                                   
103700        T4-IDLEVNR (3) NOT = SPACE   OR                                   
103800        T4-IDLEVNR (4) NOT = SPACE   OR                                   
103900        T4-IDLEVNR (5) NOT = SPACE   OR                                   
104000        T4-IDLEVNR (6) NOT = SPACE   OR                                   
104100        T4-IDLEVNR (7) NOT = SPACE   OR                                   
104200        T4-IDLEVNR (8) NOT = SPACE                                        
104300        MOVE JA                      TO SW-IFYLLT                         
104400     END-IF                                                               
104500                                                                          
104600*---- DET HAR VISAT SIG ATT VISSA ARTNR HAR LEVNR 0                       
104700*---- OM MAN DÅ GÖR URVAL LEVERANTÖR                                      
104800*---- OCH DÅ GÖR URVAL PÅ PÅ MINDRE ÄN 8 LEVERANTÖRER                     
104900*---- SÅ FÅR MAN MED SIG ALLA ARTIKLAR MED LEVNR 0                        
105000*-->>> DETTA SPÄRRAS BORT I CCEA  <<<<                                    
105100                                                                          
105200     IF SW-IFYLLT = JA                                                    
105300        PERFORM CCEA-LEVNR-NOLL-KOLL                                      
105400        IF  T4-IDLEVNR (1) = I19-IDLEVNR  OR                              
105500            T4-IDLEVNR (2) = I19-IDLEVNR  OR                              
105600            T4-IDLEVNR (3) = I19-IDLEVNR  OR                              
105700            T4-IDLEVNR (4) = I19-IDLEVNR  OR                              
105800            T4-IDLEVNR (5) = I19-IDLEVNR  OR                              
105900            T4-IDLEVNR (6) = I19-IDLEVNR  OR                              
106000            T4-IDLEVNR (7) = I19-IDLEVNR  OR                              
106100            T4-IDLEVNR (8) = I19-IDLEVNR                                  
106200            MOVE JA                  TO SW-TRAEFF                         
106300        END-IF                                                            
106400     END-IF                                                               
106500     .                                                                    
106600     EJECT                                                                
106700 CCEA-LEVNR-NOLL-KOLL  SECTION.                                           
106710     MOVE 'CCEA-LEVNR-NOLL-KOLL' TO WS-SEKTION                            
106800     SKIP2                                                                
106900     IF T4-IDLEVNR (1) NOT = SPACE                                        
107000       CONTINUE                                                           
107100     ELSE                                                                 
107200       MOVE '99999' TO  T4-IDLEVNR (1)                                    
107300     END-IF                                                               
107400                                                                          
107500     IF T4-IDLEVNR (2) NOT = SPACE                                        
107600       CONTINUE                                                           
107700     ELSE                                                                 
107800       MOVE '99999' TO  T4-IDLEVNR (2)                                    
107900     END-IF                                                               
108000                                                                          
108100     IF T4-IDLEVNR (3) NOT = SPACE                                        
108200       CONTINUE                                                           
108300     ELSE                                                                 
108400       MOVE '99999' TO  T4-IDLEVNR (3)                                    
108500     END-IF                                                               
108600                                                                          
108700     IF T4-IDLEVNR (4) NOT = SPACE                                        
108800       CONTINUE                                                           
108900     ELSE                                                                 
109000       MOVE '99999' TO  T4-IDLEVNR (4)                                    
109100     END-IF                                                               
109200                                                                          
109300     IF T4-IDLEVNR (5) NOT = SPACE                                        
109400       CONTINUE                                                           
109500     ELSE                                                                 
109600       MOVE '99999' TO  T4-IDLEVNR (5)                                    
109700     END-IF                                                               
109800                                                                          
109900     IF T4-IDLEVNR (6) NOT = SPACE                                        
110000       CONTINUE                                                           
110100     ELSE                                                                 
110200       MOVE '99999' TO  T4-IDLEVNR (6)                                    
110300     END-IF                                                               
110400                                                                          
110500     IF T4-IDLEVNR (7) NOT = SPACE                                        
110600       CONTINUE                                                           
110700     ELSE                                                                 
110800       MOVE '99999' TO  T4-IDLEVNR (7)                                    
110900     END-IF                                                               
111000                                                                          
111100     IF T4-IDLEVNR (8) NOT = SPACE                                        
111200       CONTINUE                                                           
111300     ELSE                                                                 
111400       MOVE '99999' TO  T4-IDLEVNR (8)                                    
111500     END-IF                                                               
111600     .                                                                    
111700     EJECT                                                                
111800 CCF-BEHANDLA-3104-URVAL    SECTION.                                      
111810     MOVE 'CCF-BEHANDLA-3104-URVAL' TO WS-SEKTION                         
111900     SKIP2                                                                
112000     IF T4-IDPTYP          = 'S1 '                                        
112100        MOVE T4TAB-AREA (TAB-IX)   TO T453-AREA                           
112200        IF T453-IDARTNR = I15-IDARTNR                                     
112300           PERFORM CCFA-ADDERA-FAELT-T453                                 
112400        ELSE                                                              
112500           IF T453-IDARTNR  > ZERO                                        
112600               PERFORM S05-SKRIV-53-FIL                                   
112700               PERFORM CCFB-NOLLSTAELL-FAELT-T453                         
112800           END-IF                                                         
112900           IF T4-KDSVAR = JA                                              
113000             MOVE ZERO                  TO T453-KDPRODSL                  
113100           ELSE                                                           
113200             MOVE I19-KDPRODSL          TO T453-KDPRODSL                  
113300           END-IF                                                         
113400           MOVE I19-IDARTNR           TO T453-IDARTNR                     
113500           MOVE I19-BEART-SVE         TO T453-BEART-SVE                   
113600           MOVE I19-IDFKNGRP          TO T453-IDFKNGRP                    
113700           PERFORM CCFA-ADDERA-FAELT-T453                                 
113800        END-IF                                                            
113900        MOVE T453-AREA               TO T4TAB-AREA (TAB-IX)               
114000     ELSE                                                                 
114100        IF T4-IDPTYP          = 'S2 '                                     
114200           MOVE T4TAB-AREA (TAB-IX)   TO T457-AREA                        
114300           IF I15-IDARTNR  = T457-IDARTNR                                 
114400              PERFORM CCFC-ADDERA-FAELT-T457                              
114500           ELSE                                                           
114600              IF T457-IDARTNR  > ZERO                                     
114700                 PERFORM S06-SKRIV-57-FIL                                 
114800                 PERFORM CCFD-NOLLSTAELL-FAELT-T457                       
114900              END-IF                                                      
115000              IF T4-KDSVAR = JA                                           
115100                MOVE ZERO                  TO T457-KDPRODSL               
115200              ELSE                                                        
115300                MOVE I19-KDPRODSL          TO T457-KDPRODSL               
115400              END-IF                                                      
115500              MOVE I19-IDARTNR        TO T457-IDARTNR                     
115600              MOVE I19-BEART-SVE      TO T457-BEART-SVE                   
115700              MOVE I19-IDFKNGRP       TO T457-IDFKNGRP                    
115800              PERFORM CCFC-ADDERA-FAELT-T457                              
115900           END-IF                                                         
116000           MOVE T457-AREA             TO T4TAB-AREA (TAB-IX)              
116100        ELSE                                                              
116200           MOVE T4TAB-AREA (TAB-IX)   TO T461-AREA                        
116300           IF T461-IDARTNR  =                                             
116400           I15-IDARTNR                                                    
116500               PERFORM CCFE-ADDERA-FAELT-T461                             
116600           ELSE                                                           
116700              IF T461-IDARTNR  > ZERO                                     
116800                 PERFORM S07-SKRIV-61-FIL                                 
116900                 PERFORM CCFF-NOLLSTAELL-FAELT-T461                       
117000              END-IF                                                      
117100              IF T4-KDSVAR = JA                                           
117200                MOVE ZERO             TO T461-KDPRODSL                    
117300              ELSE                                                        
117400                MOVE I19-KDPRODSL     TO T461-KDPRODSL                    
117500              END-IF                                                      
117600              MOVE I19-IDARTNR        TO T461-IDARTNR                     
117700              MOVE I19-BEART-SVE      TO T461-BEART-SVE                   
117800              MOVE I19-IDFKNGRP       TO T461-IDFKNGRP                    
117900              PERFORM CCFE-ADDERA-FAELT-T461                              
118000           END-IF                                                         
118100           MOVE T461-AREA              TO T4TAB-AREA (TAB-IX)             
118200        END-IF                                                            
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600 CCFA-ADDERA-FAELT-T453      SECTION.                                     
118610     MOVE 'CCFA-ADDERA-FAELT-T453' TO WS-SEKTION                          
118700     SKIP2                                                                
118800     COMPUTE T453-SUARTFSG-RAAR =                                         
118900     T453-SUARTFSG-RAAR  + I15-SUARTFSG-RAAR                              
119000     COMPUTE T453-SULEVANT-RAAR =                                         
119100     T453-SULEVANT-RAAR  + I15-SULEVANT-RAAR                              
119200     COMPUTE T453-SUARTSJK-RAAR =                                         
119300     T453-SUARTSJK-RAAR  + I15-SUARTSJK-RAAR                              
119400     COMPUTE T453-SULEVANT-FRAAR  =                                       
119500     T453-SULEVANT-FRAAR  + I15-SULEVANT-FRAAR                            
119600     .                                                                    
119700     EJECT                                                                
119800 CCFB-NOLLSTAELL-FAELT-T453   SECTION.                                    
119810     MOVE 'CCFB-NOLLSTAELL-T453' TO WS-SEKTION                            
119900     SKIP2                                                                
120000     MOVE ZERO                    TO T453-SUARTFSG-RAAR                   
120100                                     T453-SULEVANT-RAAR                   
120200                                     T453-SUARTSJK-RAAR                   
120300                                     T453-SULEVANT-FRAAR                  
120400     .                                                                    
120500     EJECT                                                                
120600 CCFC-ADDERA-FAELT-T457      SECTION.                                     
120610     MOVE 'CCFC-ADDERA-FAELT-T457' TO WS-SEKTION                          
120700     SKIP2                                                                
120800     COMPUTE T457-SUARTFSG-RAAR  =                                        
120900     T457-SUARTFSG-RAAR  + I15-SUARTFSG-RAAR                              
121000     COMPUTE T457-SULEVANT-RAAR  =                                        
121100     T457-SULEVANT-RAAR  + I15-SULEVANT-RAAR                              
121200     COMPUTE T457-SUARTSJK-RAAR  =                                        
121300     T457-SUARTSJK-RAAR  + I15-SUARTSJK-RAAR                              
121400     COMPUTE T457-SULEVANT-FRAAR =                                        
121500     T457-SULEVANT-FRAAR  + I15-SULEVANT-FRAAR                            
121600     .                                                                    
121700     EJECT                                                                
121800 CCFD-NOLLSTAELL-FAELT-T457   SECTION.                                    
121810     MOVE 'CCFD-NOLLSTAELL-FAELT-T457' TO WS-SEKTION                      
121900     SKIP2                                                                
122000     MOVE ZERO                    TO T457-SUARTFSG-RAAR                   
122100                                     T457-SULEVANT-RAAR                   
122200                                     T457-SUARTSJK-RAAR                   
122300                                     T457-SULEVANT-FRAAR                  
122400     .                                                                    
122500     EJECT                                                                
122600 CCFE-ADDERA-FAELT-T461      SECTION.                                     
122610     MOVE 'CCFE-ADDERA-FAELT-T461' TO WS-SEKTION                          
122700     SKIP2                                                                
122800     COMPUTE T461-SUARTFSG-RAAR  =                                        
122900     T461-SUARTFSG-RAAR  + I15-SUARTFSG-RAAR                              
123000     COMPUTE T461-SULEVANT-RAAR  =                                        
123100     T461-SULEVANT-RAAR  + I15-SULEVANT-RAAR                              
123200     COMPUTE T461-SUARTSJK-RAAR  =                                        
123300     T461-SUARTSJK-RAAR  + I15-SUARTSJK-RAAR                              
123400     COMPUTE T461-SULEVANT-FRAAR =                                        
123500     T461-SULEVANT-FRAAR  + I15-SULEVANT-FRAAR                            
123600     .                                                                    
123700     EJECT                                                                
123800 CCFF-NOLLSTAELL-FAELT-T461   SECTION.                                    
123810     MOVE 'CCFF-NOLLSTAELL-FAELT-T461' TO WS-SEKTION                      
123900     SKIP2                                                                
124000     MOVE ZERO                    TO T461-SUARTFSG-RAAR                   
124100                                     T461-SULEVANT-RAAR                   
124200                                     T461-SUARTSJK-RAAR                   
124300                                     T461-SULEVANT-FRAAR                  
124400     .                                                                    
124500     EJECT                                                                
124600 CCG-BEHANDLA-3104-URVAL    SECTION.                                      
124610     MOVE 'CCG-BEHANDLA-3104-URVAL' TO WS-SEKTION                         
124700     SKIP2                                                                
124800     IF T4-IDPTYP          = 'S1A'                                        
124900        MOVE T4TAB-AREA (TAB-IX)   TO T454-AREA                           
125000        IF T454-IDARTNR = I15-IDARTNR                                     
125100           PERFORM CCGA-ADDERA-FAELT-T454                                 
125200        ELSE                                                              
125300           IF T454-IDARTNR  > ZERO                                        
125400               PERFORM S12-SKRIV-54-FIL                                   
125500               PERFORM CCGB-NOLLSTAELL-FAELT-T453                         
125600           END-IF                                                         
125700           IF T4-KDSVAR = JA                                              
125800             MOVE ZERO             TO T454-KDPRODSL                       
125900           ELSE                                                           
126000             MOVE I19-KDPRODSL     TO T454-KDPRODSL                       
126100           END-IF                                                         
126200           MOVE I19-IDARTNR           TO T454-IDARTNR                     
126300           MOVE I19-BEART-SVE         TO T454-BEART-SVE                   
126400           MOVE I19-IDFKNGRP          TO T454-IDFKNGRP                    
126500           PERFORM CCGA-ADDERA-FAELT-T454                                 
126600        END-IF                                                            
126700        MOVE T454-AREA               TO T4TAB-AREA (TAB-IX)               
126800     ELSE                                                                 
126900        IF T4-IDPTYP          = 'S2A'                                     
127000           MOVE T4TAB-AREA (TAB-IX)   TO T458-AREA                        
127100           IF I15-IDARTNR  = T458-IDARTNR                                 
127200              PERFORM CCGC-ADDERA-FAELT-T458                              
127300           ELSE                                                           
127400              IF T458-IDARTNR  > ZERO                                     
127500                 PERFORM S13-SKRIV-58-FIL                                 
127600                 PERFORM CCGD-NOLLSTAELL-FAELT-T458                       
127700              END-IF                                                      
127800              IF T4-KDSVAR = JA                                           
127900                MOVE ZERO             TO T458-KDPRODSL                    
128000              ELSE                                                        
128100                MOVE I19-KDPRODSL     TO T458-KDPRODSL                    
128200              END-IF                                                      
128300              MOVE I19-IDARTNR        TO T458-IDARTNR                     
128400              MOVE I19-BEART-SVE      TO T458-BEART-SVE                   
128500              MOVE I19-IDFKNGRP       TO T458-IDFKNGRP                    
128600              PERFORM CCGC-ADDERA-FAELT-T458                              
128700           END-IF                                                         
128800           MOVE T458-AREA             TO T4TAB-AREA (TAB-IX)              
128900        ELSE                                                              
129000           MOVE T4TAB-AREA (TAB-IX)   TO T462-AREA                        
129100           IF T462-IDARTNR = I15-IDARTNR                                  
129200               PERFORM CCGE-ADDERA-FAELT-T462                             
129300           ELSE                                                           
129400              IF T462-IDARTNR  > ZERO                                     
129500                 PERFORM S14-SKRIV-62-FIL                                 
129600                 PERFORM CCGF-NOLLSTAELL-FAELT-T462                       
129700              END-IF                                                      
129800              IF T4-KDSVAR = JA                                           
129900                MOVE ZERO             TO T462-KDPRODSL                    
130000              ELSE                                                        
130100                MOVE I19-KDPRODSL     TO T462-KDPRODSL                    
130200              END-IF                                                      
130300              MOVE I19-IDARTNR        TO T462-IDARTNR                     
130400              MOVE I19-BEART-SVE      TO T462-BEART-SVE                   
130500              MOVE I19-IDFKNGRP       TO T462-IDFKNGRP                    
130600              PERFORM CCGE-ADDERA-FAELT-T462                              
130700           END-IF                                                         
130800           MOVE T462-AREA              TO T4TAB-AREA (TAB-IX)             
130900        END-IF                                                            
131000     END-IF                                                               
131100     .                                                                    
131200     EJECT                                                                
131300 CCGA-ADDERA-FAELT-T454     SECTION.                                      
131310     MOVE 'CCGA-ADDERA-FAELT-T454' TO WS-SEKTION                          
131400     SKIP2                                                                
131500     COMPUTE T454-SUARTFSG-RAAR =                                         
131600     T454-SUARTFSG-RAAR  + I15-SUARTFSG-RAAR                              
131700     COMPUTE T454-SULEVANT-RAAR =                                         
131800     T454-SULEVANT-RAAR  + I15-SULEVANT-RAAR                              
131900     COMPUTE T454-SUARTSJK-RAAR =                                         
132000     T454-SUARTSJK-RAAR  + I15-SUARTSJK-RAAR                              
132100     COMPUTE T454-SULEVANT-FRAAR  =                                       
132200     T454-SULEVANT-FRAAR  + I15-SULEVANT-FRAAR                            
132300     .                                                                    
132400     EJECT                                                                
132500 CCGB-NOLLSTAELL-FAELT-T453 SECTION.                                      
132510     MOVE 'CCGB-NOLLSTAELL-FAELT-T453' TO WS-SEKTION                      
132600     SKIP2                                                                
132700     MOVE ZERO                    TO T454-SUARTFSG-RAAR                   
132800                                     T454-SULEVANT-RAAR                   
132900                                     T454-SUARTSJK-RAAR                   
133000                                     T454-SULEVANT-FRAAR                  
133100     .                                                                    
133200     EJECT                                                                
133300 CCGC-ADDERA-FAELT-T458     SECTION.                                      
133310     MOVE 'CCGC-ADDERA-FAELT-T458' TO WS-SEKTION                          
133400     SKIP2                                                                
133500     COMPUTE T458-SUARTFSG-RAAR  =                                        
133600     T458-SUARTFSG-RAAR  + I15-SUARTFSG-RAAR                              
133700     COMPUTE T458-SULEVANT-RAAR  =                                        
133800     T458-SULEVANT-RAAR  + I15-SULEVANT-RAAR                              
133900     COMPUTE T458-SUARTSJK-RAAR  =                                        
134000     T458-SUARTSJK-RAAR  + I15-SUARTSJK-RAAR                              
134100     COMPUTE T458-SULEVANT-FRAAR =                                        
134200     T458-SULEVANT-FRAAR  + I15-SULEVANT-FRAAR                            
134300     .                                                                    
134400     EJECT                                                                
134500 CCGD-NOLLSTAELL-FAELT-T458 SECTION.                                      
134510     MOVE 'CCGD-NOLLSTAELL-FAELT-T458' TO WS-SEKTION                      
134600     SKIP2                                                                
134700     MOVE ZERO                    TO T458-SUARTFSG-RAAR                   
134800                                     T458-SULEVANT-RAAR                   
134900                                     T458-SUARTSJK-RAAR                   
135000                                     T458-SULEVANT-FRAAR                  
135100     .                                                                    
135200     EJECT                                                                
135300 CCGE-ADDERA-FAELT-T462     SECTION.                                      
135310     MOVE 'CCGE-ADDERA-FAELT-T462' TO WS-SEKTION                          
135400     SKIP2                                                                
135500     COMPUTE T462-SUARTFSG-RAAR  =                                        
135600     T462-SUARTFSG-RAAR  + I15-SUARTFSG-RAAR                              
135700     COMPUTE T462-SULEVANT-RAAR  =                                        
135800     T462-SULEVANT-RAAR  + I15-SULEVANT-RAAR                              
135900     COMPUTE T462-SUARTSJK-RAAR  =                                        
136000     T462-SUARTSJK-RAAR  + I15-SUARTSJK-RAAR                              
136100     COMPUTE T462-SULEVANT-FRAAR =                                        
136200     T462-SULEVANT-FRAAR  + I15-SULEVANT-FRAAR                            
136300     .                                                                    
136400     EJECT                                                                
136500 CCGF-NOLLSTAELL-FAELT-T462 SECTION.                                      
136510     MOVE 'CCGF-NOLLSTAELL-FAELT-T462' TO WS-SEKTION                      
136600     SKIP2                                                                
136700     MOVE ZERO                    TO T462-SUARTFSG-RAAR                   
136800                                     T462-SULEVANT-RAAR                   
136900                                     T462-SUARTSJK-RAAR                   
137000                                     T462-SULEVANT-FRAAR                  
137100     .                                                                    
137200     EJECT                                                                
137300 D-MATCHA-TABELL-SKRIV-FILER SECTION.                                     
137310     MOVE 'D-MATCHA-TABELL-SKRIV-FILER' TO WS-SEKTION                     
137400     SKIP2                                                                
137500     MOVE +1 TO TAB-IX                                                    
137600     PERFORM UNTIL TAB-IDTRANS (TAB-IX) = SPACE OR                        
137700     TAB-IX > +100                                                        
137800        IF TAB-IDTRANS (TAB-IX) = '3202'                                  
137900           IF TAB-IDPTYP(TAB-IX) =  'PPV'                                 
138000              MOVE TAB-AREA (TAB-IX)   TO T2-AREA                         
138100                                                                          
138200*-------HÄR KOLLAS ATT ARTIKELN ÄR GODKÄND MED AVSEENDE PÅ                
138300*-------OM LISTAN SOM VALTS ÄR EN LV- RESP. PV-LISTA                      
138800             PERFORM DA-TESTA-3202-URVAL                                  
139000           END-IF                                                         
139100        ELSE                                                              
139200           IF TAB-IDTRANS (TAB-IX) = '3203'                               
139300              IF TAB-IDPTYP(TAB-IX) =  'PPV'                              
139400                MOVE TAB-AREA (TAB-IX)   TO T3-AREA                       
139500                                                                          
139600*-------HÄR KOLLAS ATT ARTIKELN ÄR GODKÄND MED AVSEENDE PÅ                
139700*-------OM LISTAN SOM VALTS ÄR EN LV- RESP. PV-LISTA                      
140100                IF   TAB-IDPTYP(TAB-IX) =  'PPV'                          
140200                  PERFORM DB-TESTA-3203-URVAL                             
140300                END-IF                                                    
140400              END-IF                                                      
140500           END-IF                                                         
140600        END-IF                                                            
140700        ADD +1 TO TAB-IX                                                  
140800     END-PERFORM                                                          
140900     .                                                                    
141000     EJECT                                                                
141100 DA-TESTA-3202-URVAL     SECTION.                                         
141110     MOVE 'DA-TESTA-3202-URVAL' TO WS-SEKTION                             
141200     SKIP2                                                                
141300* SW-IFYLLT TÄNDS OM NÅGOT FINNS PÅ URVALSFILERNA                         
141400* SW-TRAEFF TÄNDS OM DESSA STÄMMER MED INFOFILEN                          
141500     MOVE NEJ                   TO SW-IFYLLT                              
141600     MOVE NEJ                   TO SW-TRAEFF                              
141700     PERFORM DAB-KOLLA-IDANSK                                             
141800     IF SW-IFYLLT = JA AND SW-TRAEFF = JA                                 
141900     OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                               
142000        MOVE NEJ             TO SW-IFYLLT                                 
142100        MOVE NEJ             TO SW-TRAEFF                                 
142200        PERFORM DAC-KOLLA-KDVVKL                                          
142300        IF SW-IFYLLT = JA AND SW-TRAEFF = JA                              
142400        OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                            
142500           MOVE NEJ          TO SW-IFYLLT                                 
142600           MOVE NEJ          TO SW-TRAEFF                                 
142700           PERFORM DAD-KOLLA-IDLKTO                                       
142800           IF SW-IFYLLT = JA AND SW-TRAEFF = JA                           
142900           OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                         
143000              MOVE NEJ       TO SW-IFYLLT                                 
143100              MOVE NEJ       TO SW-TRAEFF                                 
143200              PERFORM DAE-KOLLA-IDLEVNR                                   
143300              IF SW-IFYLLT = JA AND SW-TRAEFF = JA                        
143400              OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                      
143500                 MOVE NEJ       TO SW-IFYLLT                              
143600                 MOVE NEJ       TO SW-TRAEFF                              
143700                 PERFORM DAF-KOLLA-KDPRODSL                               
143800                 IF SW-IFYLLT = JA AND SW-TRAEFF = JA                     
143900                 OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                   
144000                    MOVE NEJ       TO SW-IFYLLT                           
144100                    MOVE NEJ       TO SW-TRAEFF                           
144200                    PERFORM DAG-KOLLA-IDFKNGRP                            
144300                    IF SW-IFYLLT = JA AND SW-TRAEFF = JA                  
144400                    OR SW-IFYLLT = NEJ AND SW-TRAEFF = NEJ                
144500                       MOVE T2-001-GRUPP          TO                      
144600                                               U41-001-GRUPP              
144700                       MOVE +5                 TO U41-IDGTYP              
144800                       PERFORM S11-FLYTTA-T-U41-AREA                      
144900                       IF U41-IDPTYP = 'PLV' OR 'PPV'                     
145000                         PERFORM S02-SKRIV-41-FIL                         
145100                       END-IF                                             
145200                    END-IF                                                
145300                 END-IF                                                   
145400              END-IF                                                      
145500           END-IF                                                         
145600        END-IF                                                            
145700     END-IF                                                               
145800     .                                                                    
145900     EJECT                                                                
146000 DAB-KOLLA-IDANSK           SECTION.                                      
146010     MOVE 'DAB-KOLLA-IDANSK' TO WS-SEKTION                                
146100     SKIP2                                                                
146200                                                                          
146300     IF T2-IDANSK-TOM (1) > ZERO  OR                                      
146400        T2-IDANSK-TOM (2) > ZERO  OR                                      
146500        T2-IDANSK-TOM (3) > ZERO  OR                                      
146600        T2-IDANSK-TOM (4) > ZERO                                          
146700        MOVE JA                       TO SW-IFYLLT                        
146800     END-IF                                                               
146900     IF SW-IFYLLT = JA                                                    
147000        IF (T2-IDANSK-TOM (1) NOT < I19-IDANSK                            
147100           AND I19-IDANSK NOT < T2-IDANSK-FOM (1)) OR                     
147200           (T2-IDANSK-TOM (2) NOT < I19-IDANSK                            
147300           AND I19-IDANSK NOT < T2-IDANSK-FOM (2)) OR                     
147400           (T2-IDANSK-TOM (3) NOT < I19-IDANSK                            
147500           AND I19-IDANSK NOT < T2-IDANSK-FOM (3)) OR                     
147600           (T2-IDANSK-TOM (4) NOT < I19-IDANSK                            
147700           AND I19-IDANSK NOT < T2-IDANSK-FOM (4))                        
147800           MOVE JA                    TO SW-TRAEFF                        
147900        END-IF                                                            
148000     END-IF                                                               
148100     .                                                                    
148200     EJECT                                                                
148300 DAC-KOLLA-KDVVKL           SECTION.                                      
148310     MOVE 'DAC-KOLLA-KDVVKL' TO WS-SEKTION                                
148400     SKIP2                                                                
148500     IF T2-KDVVKL (1) > ZERO  OR                                          
148600        T2-KDVVKL (2) > ZERO  OR                                          
148700        T2-KDVVKL (3) > ZERO  OR                                          
148800        T2-KDVVKL (4) > ZERO  OR                                          
148900        T2-KDVVKL (5) > ZERO                                              
149000        MOVE JA                        TO SW-IFYLLT                       
149100     END-IF                                                               
149200     IF SW-IFYLLT = JA                                                    
149300        IF T2-KDVVKL (1) > ZERO AND                                       
149400           T2-KDVVKL (1) = I19-KDVVKL                                     
149500             MOVE JA                     TO SW-TRAEFF                     
149600        ELSE                                                              
149700          IF T2-KDVVKL (2) > ZERO AND                                     
149800             T2-KDVVKL (2) = I19-KDVVKL                                   
149900               MOVE JA                     TO SW-TRAEFF                   
150000          ELSE                                                            
150100            IF T2-KDVVKL (3) > ZERO AND                                   
150200               T2-KDVVKL (3) = I19-KDVVKL                                 
150300                 MOVE JA                     TO SW-TRAEFF                 
150400            ELSE                                                          
150500              IF T2-KDVVKL (4) > ZERO AND                                 
150600                 T2-KDVVKL (4) = I19-KDVVKL                               
150700                   MOVE JA                     TO SW-TRAEFF               
150800              ELSE                                                        
150900                IF T2-KDVVKL (5) > ZERO AND                               
151000                   T2-KDVVKL (5) = I19-KDVVKL                             
151100                     MOVE JA                     TO SW-TRAEFF             
151200                END-IF                                                    
151300              END-IF                                                      
151400            END-IF                                                        
151500          END-IF                                                          
151600        END-IF                                                            
151700     END-IF                                                               
151800     .                                                                    
151900     EJECT                                                                
152000 DAD-KOLLA-IDLKTO           SECTION.                                      
152010     MOVE 'DAD-KOLLA-IDLKTO' TO WS-SEKTION                                
152100     SKIP2                                                                
152200     IF T2-IDLKTO (1) > ZERO           OR                                 
152300        T2-IDLKTO (2) > ZERO           OR                                 
152400        T2-IDLKTO (3) > ZERO           OR                                 
152500        T2-IDLKTO (4) > ZERO           OR                                 
152600        T2-IDLKTO (5) > ZERO           OR                                 
152700        T2-IDLKTO (6) > ZERO           OR                                 
152800        T2-IDLKTO (7) > ZERO           OR                                 
152900        T2-IDLKTO (8) > ZERO                                              
153000        MOVE JA                        TO SW-IFYLLT                       
153100     END-IF                                                               
153200     IF SW-IFYLLT = JA                                                    
153300        IF T2-IDLKTO (1) = I19-IDLKTO  OR                                 
153400           T2-IDLKTO (2) = I19-IDLKTO  OR                                 
153500           T2-IDLKTO (3) = I19-IDLKTO  OR                                 
153600           T2-IDLKTO (4) = I19-IDLKTO  OR                                 
153700           T2-IDLKTO (5) = I19-IDLKTO  OR                                 
153800           T2-IDLKTO (6) = I19-IDLKTO  OR                                 
153900           T2-IDLKTO (7) = I19-IDLKTO  OR                                 
154000           T2-IDLKTO (8) = I19-IDLKTO                                     
154100           MOVE JA                     TO SW-TRAEFF                       
154200        END-IF                                                            
154300     END-IF                                                               
154400     .                                                                    
154500     EJECT                                                                
154600 DAE-KOLLA-IDLEVNR          SECTION.                                      
154610     MOVE 'DAE-KOLLA-IDLEVNR' TO WS-SEKTION                               
154700     SKIP2                                                                
154800     IF T2-IDLEVNR (1) NOT = SPACE     OR                                 
154900        T2-IDLEVNR (2) NOT = SPACE     OR                                 
155000        T2-IDLEVNR (3) NOT = SPACE     OR                                 
155100        T2-IDLEVNR (4) NOT = SPACE     OR                                 
155200        T2-IDLEVNR (5) NOT = SPACE     OR                                 
155300        T2-IDLEVNR (6) NOT = SPACE     OR                                 
155400        T2-IDLEVNR (7) NOT = SPACE     OR                                 
155500        T2-IDLEVNR (8) NOT = SPACE                                        
155600        MOVE JA                        TO SW-IFYLLT                       
155700     END-IF                                                               
155800                                                                          
155900*---- DET HAR VISAT SIG ATT VISSA ARTNR HAR LEVNR 0                       
156000*---- OM MAN DÅ GÖR URVAL LEVERANTÖR                                      
156100*---- OCH DÅ GÖR URVAL PÅ PÅ MINDRE ÄN 8 LEVERANTÖRER                     
156200*---- SÅ FÅR MAN MED SIG ALLA ARTIKLAR MED LEVNR 0                        
156300*-->>> DETTA SPÄRRAS BORT I DAEA  <<<<                                    
161600                                                                          
161700     IF SW-IFYLLT = JA                                                    
161710        PERFORM DAEA-LEVNR-NOLL-KOLL                                      
161800        IF T2-IDLEVNR (1) = I19-IDLEVNR  OR                               
161900           T2-IDLEVNR (2) = I19-IDLEVNR  OR                               
162000           T2-IDLEVNR (3) = I19-IDLEVNR  OR                               
162100           T2-IDLEVNR (4) = I19-IDLEVNR  OR                               
162200           T2-IDLEVNR (5) = I19-IDLEVNR  OR                               
162300           T2-IDLEVNR (6) = I19-IDLEVNR  OR                               
162400           T2-IDLEVNR (7) = I19-IDLEVNR  OR                               
162500           T2-IDLEVNR (8) = I19-IDLEVNR                                   
162600           MOVE JA                     TO SW-TRAEFF                       
162700        END-IF                                                            
162800     END-IF                                                               
162900     .                                                                    
163000     EJECT                                                                
163010 DAEA-LEVNR-NOLL-KOLL  SECTION.                                           
163011     MOVE 'DAEA-LEVNR-NOLL-KOLL' TO WS-SEKTION                            
163020     SKIP2                                                                
163030     IF T2-IDLEVNR (1) NOT = SPACE                                        
163040       CONTINUE                                                           
163050     ELSE                                                                 
163060       MOVE '99999' TO  T2-IDLEVNR (1)                                    
163070     END-IF                                                               
163080                                                                          
163090     IF T2-IDLEVNR (2) NOT = SPACE                                        
163091       CONTINUE                                                           
163092     ELSE                                                                 
163093       MOVE '99999' TO  T2-IDLEVNR (2)                                    
163094     END-IF                                                               
163095                                                                          
163096     IF T2-IDLEVNR (3) NOT = SPACE                                        
163097       CONTINUE                                                           
163098     ELSE                                                                 
163099       MOVE '99999' TO  T2-IDLEVNR (3)                                    
163100     END-IF                                                               
163101                                                                          
163102     IF T2-IDLEVNR (4) NOT = SPACE                                        
163103       CONTINUE                                                           
163104     ELSE                                                                 
163105       MOVE '99999' TO  T2-IDLEVNR (4)                                    
163106     END-IF                                                               
163107                                                                          
163108     IF T2-IDLEVNR (5) NOT = SPACE                                        
163109       CONTINUE                                                           
163110     ELSE                                                                 
163111       MOVE '99999' TO  T2-IDLEVNR (5)                                    
163112     END-IF                                                               
163113                                                                          
163114     IF T2-IDLEVNR (6) NOT = SPACE                                        
163115       CONTINUE                                                           
163116     ELSE                                                                 
163117       MOVE '99999' TO  T2-IDLEVNR (6)                                    
163118     END-IF                                                               
163119                                                                          
163120     IF T2-IDLEVNR (7) NOT = SPACE                                        
163121       CONTINUE                                                           
163122     ELSE                                                                 
163123       MOVE '99999' TO  T2-IDLEVNR (7)                                    
163124     END-IF                                                               
163125                                                                          
163126     IF T2-IDLEVNR (8) NOT = SPACE                                        
163127       CONTINUE                                                           
163128     ELSE                                                                 
163129       MOVE '99999' TO  T2-IDLEVNR (8)                                    
163130     END-IF                                                               
163131     .                                                                    
163132     EJECT                                                                
163140 DAF-KOLLA-KDPRODSL         SECTION.                                      
163150     MOVE 'DAF-KOLLA-KDPRODSL' TO WS-SEKTION                              
163200     SKIP2                                                                
163300     IF T2-KDPRODSL (1) > ZERO         OR                                 
163400        T2-KDPRODSL (2) > ZERO         OR                                 
163500        T2-KDPRODSL (3) > ZERO         OR                                 
163600        T2-KDPRODSL (4) > ZERO         OR                                 
163700        T2-KDPRODSL (5) > ZERO         OR                                 
163800        T2-KDPRODSL (6) > ZERO         OR                                 
163900        T2-KDPRODSL (7) > ZERO                                            
164000        MOVE JA                        TO SW-IFYLLT                       
164100     END-IF                                                               
164200     IF SW-IFYLLT = JA                                                    
164300        IF T2-KDPRODSL (1) = I19-KDPRODSL OR                              
164400           T2-KDPRODSL (2) = I19-KDPRODSL OR                              
164500           T2-KDPRODSL (3) = I19-KDPRODSL OR                              
164600           T2-KDPRODSL (4) = I19-KDPRODSL OR                              
164700           T2-KDPRODSL (5) = I19-KDPRODSL OR                              
164800           T2-KDPRODSL (6) = I19-KDPRODSL OR                              
164900           T2-KDPRODSL (7) = I19-KDPRODSL                                 
165000           MOVE JA                     TO SW-TRAEFF                       
165100        END-IF                                                            
165200     END-IF                                                               
165300     .                                                                    
165400     EJECT                                                                
165500 DAG-KOLLA-IDFKNGRP         SECTION.                                      
165510     MOVE 'DAG-KOLLA-IDFKNGRP' TO WS-SEKTION                              
165600     SKIP2                                                                
165700     MOVE +1                            TO IX                             
165800     PERFORM UNTIL IX > +99  OR                                           
165900     SW-IFYLLT = JA AND SW-TRAEFF = JA                                    
166000        IF T2-IDFKNGRP-TOM (IX) > ZERO                                    
166100           MOVE JA                    TO SW-IFYLLT                        
166200           IF  T2-IDFKNGRP-TOM (IX) NOT < I19-IDFKNGRP                    
166300           AND I19-IDFKNGRP NOT < T2-IDFKNGRP-FOM (IX)                    
166400              MOVE JA                 TO SW-TRAEFF                        
166500           END-IF                                                         
166600        END-IF                                                            
166700        ADD  +1                         TO IX                             
166800     END-PERFORM                                                          
166900     .                                                                    
167000     EJECT                                                                
167100 DB-TESTA-3203-URVAL     SECTION.                                         
167110     MOVE 'DB-TESTA-3203-URVAL' TO WS-SEKTION                             
167200     MOVE NEJ                   TO SW-IFYLLT                              
167300     MOVE +1                       TO IX                                  
167400     MOVE NEJ                      TO SW-TRAEFF                           
167500     PERFORM UNTIL IX > +500 OR                                           
167600     SW-TRAEFF = JA                                                       
167700        IF T3-IDARTNR (IX) = I19-IDARTNR                                  
167800           MOVE JA                  TO SW-TRAEFF                          
167900           MOVE T3-001-GRUPP          TO                                  
168000                                       U41-001-GRUPP                      
168100           MOVE +5                  TO U41-IDGTYP                         
168200           PERFORM S11-FLYTTA-T-U41-AREA                                  
168300           IF U41-IDPTYP = 'PLV' OR 'PPV'                                 
168400              PERFORM S02-SKRIV-41-FIL                                    
168500           END-IF                                                         
168600        END-IF                                                            
168700        ADD  +1                        TO IX                              
168800     END-PERFORM                                                          
168900     .                                                                    
169000     EJECT                                                                
169100 E-TOEM-TABELL-SKRIV-FILER SECTION.                                       
169110     MOVE 'E-TOEM-TABELL-SKRIV-FILER' TO WS-SEKTION                       
169200     SKIP2                                                                
169300     MOVE +1 TO TAB-IX                                                    
169400     PERFORM UNTIL TAB-IDTRANS (TAB-IX) = SPACE OR                        
169500     TAB-IX > +100                                                        
169600        IF TAB-IDTRANS (TAB-IX) = '3204'                                  
169700           PERFORM EA-TESTA-3204-URVAL                                    
169800        END-IF                                                            
169900        ADD +1 TO TAB-IX                                                  
170000     END-PERFORM                                                          
170100     .                                                                    
170200     EJECT                                                                
170300 EA-TESTA-3204-URVAL     SECTION.                                         
170310     MOVE 'EA-TESTA-3204-URVAL' TO WS-SEKTION                             
170400     SKIP2                                                                
170500     IF T4TAB-IDPTYP (TAB-IX) = 'S1 '                                     
170600        MOVE T4TAB-AREA (TAB-IX)   TO T453-AREA                           
170700        IF T453-IDARTNR  > ZERO                                           
170800            PERFORM S05-SKRIV-53-FIL                                      
170900        END-IF                                                            
171000     ELSE                                                                 
171100        IF T4TAB-IDPTYP (TAB-IX) = 'S2 '                                  
171200           MOVE T4TAB-AREA (TAB-IX)   TO T457-AREA                        
171300           IF T457-IDARTNR  > ZERO                                        
171400               PERFORM S06-SKRIV-57-FIL                                   
171500           END-IF                                                         
171600        ELSE                                                              
171700           IF T4TAB-IDPTYP (TAB-IX) = 'S3 '                               
171800             MOVE T4TAB-AREA (TAB-IX)   TO T461-AREA                      
171900             IF T461-IDARTNR  > ZERO                                      
172000                 PERFORM S07-SKRIV-61-FIL                                 
172100             END-IF                                                       
172200           END-IF                                                         
172300        END-IF                                                            
172400     END-IF                                                               
172500     IF T4TAB-IDPTYP (TAB-IX) = 'S1A'                                     
172600        MOVE T4TAB-AREA (TAB-IX)   TO T454-AREA                           
172700        IF T454-IDARTNR  > ZERO                                           
172800            PERFORM S12-SKRIV-54-FIL                                      
172900        END-IF                                                            
173000     ELSE                                                                 
173100        IF T4TAB-IDPTYP (TAB-IX) = 'S2A'                                  
173200           MOVE T4TAB-AREA (TAB-IX)   TO T458-AREA                        
173300           IF T458-IDARTNR  > ZERO                                        
173400               PERFORM S13-SKRIV-58-FIL                                   
173500           END-IF                                                         
173600        ELSE                                                              
173700          IF T4TAB-IDPTYP (TAB-IX) = 'S3A'                                
173800             MOVE T4TAB-AREA (TAB-IX)   TO T462-AREA                      
173900             IF T462-IDARTNR  > ZERO                                      
174000                 PERFORM S14-SKRIV-62-FIL                                 
174100             END-IF                                                       
174200          END-IF                                                          
174300        END-IF                                                            
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700 S01-LAS-SEKUNDAR-REGISTER SECTION.                                       
174710     MOVE 'S01-LAS-SEKUNDAR-REGISTER' TO WS-FIL-SEKTION                   
174800     SKIP2                                                                
174900     READ W33015 INTO I15-AREA                                            
175000     AT END                                                               
175100       MOVE JA                     TO SEKUNDAR-EOF                        
175200       MOVE +999999999 TO I15-IDARTNR                                     
175300     END-READ                                                             
175400                                                                          
175500     IF SEKUNDAR-EOF = NEJ                                                
175600       ADD +1 TO TOT-RAKNARE-SEKU                                         
175700     END-IF                                                               
175800     .                                                                    
175900     EJECT                                                                
176000 S02-SKRIV-41-FIL         SECTION.                                        
176010     MOVE 'S02-SKRIV-41-FIL' TO WS-FIL-SEKTION                            
176100     SKIP2                                                                
176200     WRITE U41-POST FROM U41-AREA                                         
176300     ADD +1 TO TOT-RAKNARE-41                                             
176400     .                                                                    
176500     EJECT                                                                
176600 S03-LAS-URVALSFIL SECTION.                                               
176610     MOVE 'S03-LAS-URVALFIL' TO WS-FIL-SEKTION                            
176700     SKIP2                                                                
176800     READ W33031 INTO I31-AREA                                            
176900     AT END                                                               
177000       MOVE JA                    TO URVAL-EOF                            
177100     END-READ                                                             
177200                                                                          
177300     IF URVAL-EOF = NEJ                                                   
177400       ADD +1 TO TOT-RAKNARE-URV                                          
177500     END-IF                                                               
177600     .                                                                    
177700     EJECT                                                                
177800 S04-LAS-INFOFIL SECTION.                                                 
177810     MOVE 'S04-LAS-INFOFIL' TO WS-FIL-SEKTION                             
177900     SKIP2                                                                
178000     READ W33019 INTO I19-AREA                                            
178100     AT END                                                               
178200       MOVE JA                        TO INFO-EOF                         
178300     END-READ                                                             
178400                                                                          
178500     IF INFO-EOF = NEJ                                                    
178600       ADD +1 TO TOT-RAKNARE-INFO                                         
178700     END-IF                                                               
178800     .                                                                    
178900     EJECT                                                                
179000 S05-SKRIV-53-FIL     SECTION.                                            
179010     MOVE 'S05-SKRIV-53-FIL' TO WS-FIL-SEKTION                            
179100     SKIP2                                                                
179200     MOVE +5                     TO T453-IDGTYP                           
179300     WRITE U53-POST FROM T453-AREA                                        
179400     ADD +1 TO TOT-RAKNARE-53                                             
179500     .                                                                    
179600     EJECT                                                                
179700 S06-SKRIV-57-FIL     SECTION.                                            
179710     MOVE 'S06-SKRIV-57-FIL' TO WS-FIL-SEKTION                            
179800     SKIP2                                                                
179900     MOVE +5                     TO T457-IDGTYP                           
180000     WRITE U57-POST FROM T457-AREA                                        
180100     ADD +1 TO TOT-RAKNARE-57                                             
180200     .                                                                    
180300     EJECT                                                                
180400 S07-SKRIV-61-FIL     SECTION.                                            
180410     MOVE 'S07-SKRIV-61-FIL' TO WS-FIL-SEKTION                            
180500     SKIP2                                                                
180600     MOVE +5                     TO T461-IDGTYP                           
180700     WRITE U61-POST FROM T461-AREA                                        
180800     ADD +1 TO TOT-RAKNARE-61                                             
180900     .                                                                    
181000     EJECT                                                                
181100 S08-SKRIV-47-FIL         SECTION.                                        
181110     MOVE 'S08-SKRIV-47-FIL' TO WS-FIL-SEKTION                            
181200     SKIP2                                                                
181300     WRITE U47-POST FROM U41-AREA                                         
181400     ADD +1 TO TOT-RAKNARE-47                                             
181500     .                                                                    
181600     EJECT                                                                
181700 S09-LAS-SEKUNDAR-II-REGISTER SECTION.                                    
181710     MOVE 'S09-LAS-SEKUNDAR-II-REGISTER' TO WS-FIL-SEKTION                
181800     SKIP2                                                                
181900     READ W33022 INTO I22-AREA                                            
182000     AT END                                                               
182100       MOVE JA TO SEKUNDAR-II-EOF                                         
182200       MOVE +999999999 TO I22-IDARTNR                                     
182300     END-READ                                                             
182400                                                                          
182500     IF SEKUNDAR-II-EOF = NEJ                                             
182600       ADD +1 TO TOT-RAKNARE-II-SEKU                                      
182700     END-IF                                                               
182800     .                                                                    
182900     EJECT                                                                
183000 S10-FLYTTA-T-U41-AREA SECTION.                                           
183010     MOVE 'S10-FLYTTA-T-U41-AREA' TO WS-FIL-SEKTION                       
183100     SKIP2                                                                
183200     MOVE I19-IDARTNR                   TO U41-IDARTNR                    
183300     MOVE I19-BEART-SVE                 TO U41-BEART-SVE                  
183400     MOVE I19-KDPRODSL                  TO U41-KDPRODSL                   
183500     MOVE I19-BEPRODSL                  TO U41-BEPRODSL                   
183600     MOVE I19-IDFKNGRP                  TO U41-IDFKNGRP                   
183700     MOVE I19-BEFKNGRP                  TO U41-BEFKNGRP                   
183800     MOVE I15-IDDISTR                   TO U41-IDDISTR                    
183900     MOVE I15-IDKONCNR                  TO U41-IDKONCNR                   
184000     MOVE I15-KDMARK-BUDG               TO U41-KDMARK-BUDG                
184100     MOVE I15-BEMARK-BUDG               TO U41-BEMARK-BUDG                
184200     MOVE I15-SUARTFSG-PER              TO U41-SUARTFSG-PER               
184300     MOVE I15-SUARTFSG-AAR              TO U41-SUARTFSG-AAR               
184400     MOVE I15-SUARTFSG-FAAR             TO U41-SUARTFSG-FAAR              
184500     MOVE I15-SUARTFSG-RAAR             TO U41-SUARTFSG-RAAR              
184600     MOVE I15-SUARTFSG-FRAAR            TO U41-SUARTFSG-FRAAR             
184700     MOVE I15-SULEVANT-PER              TO U41-SULEVANT-PER               
184800     MOVE I15-SULEVANT-AAR              TO U41-SULEVANT-AAR               
184900     MOVE I15-SULEVANT-FAAR             TO U41-SULEVANT-FAAR              
185000     MOVE I15-SULEVANT-RAAR             TO U41-SULEVANT-RAAR              
185100     MOVE I15-SULEVANT-FRAAR            TO U41-SULEVANT-FRAAR             
185200     MOVE I15-SUARTSJK-PER              TO U41-SUARTSJK-PER               
185300     MOVE I15-SUARTSJK-AAR              TO U41-SUARTSJK-AAR               
185400     MOVE I15-SUARTSJK-FAAR             TO U41-SUARTSJK-FAAR              
185500     MOVE I15-SUARTSJK-RAAR             TO U41-SUARTSJK-RAAR              
185600     MOVE I15-SUARTSJK-FRAAR            TO U41-SUARTSJK-FRAAR             
185700     .                                                                    
185800     EJECT                                                                
185900 S11-FLYTTA-T-U41-AREA SECTION.                                           
185910     MOVE 'S11-FLYTTA-T-U41-AREA' TO WS-FIL-SEKTION                       
186000*--------------------------------FÖR P-LV-PV-LISTAN.                      
186100     SKIP2                                                                
186200     MOVE I19-IDARTNR                   TO U41-IDARTNR                    
186300     MOVE I19-BEART-SVE                 TO U41-BEART-SVE                  
186400     MOVE I19-KDPRODSL                  TO U41-KDPRODSL                   
186500     MOVE I19-BEPRODSL                  TO U41-BEPRODSL                   
186600     MOVE I19-IDFKNGRP                  TO U41-IDFKNGRP                   
186700     MOVE I19-BEFKNGRP                  TO U41-BEFKNGRP                   
186800     MOVE I22-IDDISTR                   TO U41-IDDISTR                    
186900     MOVE I22-IDKONCNR                  TO U41-IDKONCNR                   
187000     MOVE I22-KDMARK-BUDG               TO U41-KDMARK-BUDG                
187100     MOVE I22-BEMARK-BUDG               TO U41-BEMARK-BUDG                
187200     MOVE I22-SUARTFSG-PER              TO U41-SUARTFSG-PER               
187300     MOVE I22-SUARTFSG-AAR              TO U41-SUARTFSG-AAR               
187400     MOVE I22-SUARTFSG-FAAR             TO U41-SUARTFSG-FAAR              
187500     MOVE I22-SUARTFSG-RAAR             TO U41-SUARTFSG-RAAR              
187600     MOVE I22-SUARTFSG-FRAAR            TO U41-SUARTFSG-FRAAR             
187700     MOVE I22-SULEVANT-PER              TO U41-SULEVANT-PER               
187800     MOVE I22-SULEVANT-AAR              TO U41-SULEVANT-AAR               
187900     MOVE I22-SULEVANT-FAAR             TO U41-SULEVANT-FAAR              
188000     MOVE I22-SULEVANT-RAAR             TO U41-SULEVANT-RAAR              
188100     MOVE I22-SULEVANT-FRAAR            TO U41-SULEVANT-FRAAR             
188200     MOVE I22-SUARTSJK-PER              TO U41-SUARTSJK-PER               
188300     MOVE I22-SUARTSJK-AAR              TO U41-SUARTSJK-AAR               
188400     MOVE I22-SUARTSJK-FAAR             TO U41-SUARTSJK-FAAR              
188500     MOVE I22-SUARTSJK-RAAR             TO U41-SUARTSJK-RAAR              
188600     MOVE I22-SUARTSJK-FRAAR            TO U41-SUARTSJK-FRAAR             
188700     .                                                                    
188800     EJECT                                                                
188900 S12-SKRIV-54-FIL     SECTION.                                            
188910     MOVE 'S12-SKRIV-54-FIL' TO WS-FIL-SEKTION                            
189000     SKIP2                                                                
189100     MOVE +5                     TO T454-IDGTYP                           
189200     WRITE U54-POST FROM T454-AREA                                        
189300     ADD +1 TO TOT-RAKNARE-54                                             
189400     .                                                                    
189500                                                                          
189600 S13-SKRIV-58-FIL     SECTION.                                            
189610     MOVE 'S13-SKRIV-58-FIL' TO WS-FIL-SEKTION                            
189700     SKIP2                                                                
189800     MOVE +5                     TO T458-IDGTYP                           
189900     WRITE U58-POST FROM T458-AREA                                        
190000     ADD +1 TO TOT-RAKNARE-58                                             
190100     .                                                                    
190200                                                                          
190300 S14-SKRIV-62-FIL     SECTION.                                            
190310     MOVE 'S14-SKRIV-62-FIL' TO WS-FIL-SEKTION                            
190400     SKIP2                                                                
190500     MOVE +5                     TO T462-IDGTYP                           
190600     WRITE U62-POST FROM T462-AREA                                        
190700     ADD +1 TO TOT-RAKNARE-62                                             
190800     .                                                                    
190900     EJECT                                                                
191000 Z-FINIT SECTION.                                                         
191010     MOVE 'Z-FINIT' TO WS-FIL-SEKTION                                     
191100     SKIP2                                                                
191200     CLOSE W33015                                                         
191300           W33022                                                         
191400           W33019                                                         
191500           W33031                                                         
191600           W33041                                                         
191700           W33053                                                         
191800           W33057                                                         
191900           W33061                                                         
192000           W33047                                                         
192100           W33054                                                         
192200           W33058                                                         
192300           W33062                                                         
192400                                                                          
192500     MOVE PROGRAM-NAMN           TO POSTSUM-PROGNAMN                      
192600     MOVE 'T'                    TO POSTSUM-OPKOD                         
192700                                                                          
192800     MOVE 'SEK1'                 TO POSTSUM-TRANSTYP                      
192900     MOVE 'W33015'               TO POSTSUM-FDNAMN                        
193000     MOVE 'W33040D1'             TO POSTSUM-DDNAMN2                       
193100     MOVE TOT-RAKNARE-SEKU       TO POSTSUM-TOTTRANS                      
193200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
193300                                                                          
193400     MOVE 'SEK2'                 TO POSTSUM-TRANSTYP                      
193500     MOVE 'W33022'               TO POSTSUM-FDNAMN                        
193600     MOVE 'W33040D9'             TO POSTSUM-DDNAMN2                       
193700     MOVE TOT-RAKNARE-II-SEKU    TO POSTSUM-TOTTRANS                      
193800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
193900                                                                          
194000     MOVE 'INFO'                 TO POSTSUM-TRANSTYP                      
194100     MOVE 'W33019'               TO POSTSUM-FDNAMN                        
194200     MOVE 'W33040D2'             TO POSTSUM-DDNAMN2                       
194300     MOVE TOT-RAKNARE-INFO       TO POSTSUM-TOTTRANS                      
194400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
194500                                                                          
194600     MOVE ' URV'                 TO POSTSUM-TRANSTYP                      
194700     MOVE 'W33031'               TO POSTSUM-FDNAMN                        
194800     MOVE 'W33040D3'             TO POSTSUM-DDNAMN2                       
194900     MOVE TOT-RAKNARE-URV        TO POSTSUM-TOTTRANS                      
195000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
195100                                                                          
195200     MOVE '  41'                 TO POSTSUM-TRANSTYP                      
195300     MOVE 'W33041'               TO POSTSUM-FDNAMN                        
195400     MOVE 'W33040D4'             TO POSTSUM-DDNAMN2                       
195500     MOVE TOT-RAKNARE-41         TO POSTSUM-TOTTRANS                      
195600     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
195700                                                                          
195800     MOVE '  53'                 TO POSTSUM-TRANSTYP                      
195900     MOVE 'W33053'               TO POSTSUM-FDNAMN                        
196000     MOVE 'W33040D5'             TO POSTSUM-DDNAMN2                       
196100     MOVE TOT-RAKNARE-53         TO POSTSUM-TOTTRANS                      
196200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
196300                                                                          
196400     MOVE '  57'                 TO POSTSUM-TRANSTYP                      
196500     MOVE 'W33057'               TO POSTSUM-FDNAMN                        
196600     MOVE 'W33040D6'             TO POSTSUM-DDNAMN2                       
196700     MOVE TOT-RAKNARE-57         TO POSTSUM-TOTTRANS                      
196800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
196900                                                                          
197000     MOVE '  61'                 TO POSTSUM-TRANSTYP                      
197100     MOVE 'W33061'               TO POSTSUM-FDNAMN                        
197200     MOVE 'W33040D7'             TO POSTSUM-DDNAMN2                       
197300     MOVE TOT-RAKNARE-61         TO POSTSUM-TOTTRANS                      
197400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
197500                                                                          
197600     MOVE '  47'                 TO POSTSUM-TRANSTYP                      
197700     MOVE 'W33047'               TO POSTSUM-FDNAMN                        
197800     MOVE 'W33040D8'             TO POSTSUM-DDNAMN2                       
197900     MOVE TOT-RAKNARE-47         TO POSTSUM-TOTTRANS                      
198000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
198100                                                                          
198200     MOVE '  54'                 TO POSTSUM-TRANSTYP                      
198300     MOVE 'W33054'               TO POSTSUM-FDNAMN                        
198400     MOVE 'W33040DA'             TO POSTSUM-DDNAMN2                       
198500     MOVE TOT-RAKNARE-54         TO POSTSUM-TOTTRANS                      
198600     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
198700                                                                          
198800     MOVE '  58'                 TO POSTSUM-TRANSTYP                      
198900     MOVE 'W33058'               TO POSTSUM-FDNAMN                        
199000     MOVE 'W33040DB'             TO POSTSUM-DDNAMN2                       
199100     MOVE TOT-RAKNARE-58         TO POSTSUM-TOTTRANS                      
199200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
199300                                                                          
199400     MOVE '  62'                 TO POSTSUM-TRANSTYP                      
199500     MOVE 'W33062'               TO POSTSUM-FDNAMN                        
199600     MOVE 'W33040DC'             TO POSTSUM-DDNAMN2                       
199700     MOVE TOT-RAKNARE-62         TO POSTSUM-TOTTRANS                      
199800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
199900                                                                          
200000     MOVE 'S'                    TO POSTSUM-OPKOD                         
200100     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
200200     .                                                                    

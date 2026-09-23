000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3301200.                                                 
000400 AUTHOR.        RONNY STENHOLM.                                           
000500     DATE-WRITTEN.  DEC 1989.                                             
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        UPPDATERA PRIMÄWREGISTRET TILL ARTIKELSTATISTIKEN.               
001000*        LÄSER EN FIL (W33011) MED SÅLDA ARTIKLAR SORTERADE PÅ            
001100*        ARTIKEL, VECKA, DISTRIKT OCH POSTYP MED DEN                      
001200*        UPPDATERAS PRIMÄWREGISTRET TILL ARTIKELSTATISTIKEN.              
001300*        PRIMÄWREGISTRET INNEHÅLLER TVÅ ÅRS FÖRSÄLJNING.                  
001400*        VECKOR ÄLDRE ÄN TVÅ ÅR TAS BORT.                                 
001500*        TRANSAKTIONER MED POSTTYP 200 INNEBÄR JUSTERINGAR                
001600*        PÅ ANTINGEN SÅLD KVANTITET OCH/ELLER SUMMA ELLER                 
001700*        JUSTERING PÅ ARTIKELNS SJÄLVKOSTNADSPRIS.                        
001800*        DESSA JUSTERINGAR KONTROLLERAS OCH VID FEL SKRIVS                
001900*        FELAKTIGA POSTER UT PÅ EN FELFIL TILL UT-TRATTEN.                
002000*    SUBPROGRAM:                                                          
002100*        WDATKONV - UTFÖR KONVERTERING AV ÅÅMMDD TILL                     
002200*                   ÅÅVV                                                  
002300*     WY2000                                                              
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*    --- INFILER:                                                         
003200*           --- UPPDATERINGSTRANSAR:                                      
003300     SELECT W33011-TRANS                 ASSIGN TO W33012D1.              
003400*           --- GAMMALT REGISTER:                                         
003500     SELECT W33013-INREG                 ASSIGN TO W33012D2.              
003600     SKIP2                                                                
003700*    --- UTFILER:                                                         
003800*           --- UPPDATERAT REGISTER:                                      
003900     SELECT W33013-UTREG                 ASSIGN TO W33012D3.              
004000     SKIP2                                                                
004100*--- RAPPORTFILER:                                                        
004200*           --- FELFIL                                                    
004300     SELECT W33012-FEL                   ASSIGN TO W33012D4.              
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  W33011-TRANS                                                         
005000     LABEL RECORD   STANDARD                                              
005100     RECORDING      V                                                     
005200     BLOCK CONTAINS 0.                                                    
005300     SKIP2                                                                
005400*    -COPY W330110C  -L.                                                  
005600     SKIP2                                                                
005700*    -COPY W330200C  -L.                                                  
005900     EJECT                                                                
006000 FD  W33013-INREG                                                         
006100     LABEL RECORD   STANDARD                                              
006200     RECORDING      V                                                     
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP2                                                                
006500*    -COPY W330310   -L.                                                  
006700     SKIP2                                                                
006800*    -COPY W330300   -L.                                                  
007000     SKIP2                                                                
007100*    -COPY W330320   -L.                                                  
007300     SKIP2                                                                
007400*    -COPY W330330   -L.                                                  
007600     SKIP2                                                                
007700*    -COPY W330340   -L.                                                  
007900     SKIP2                                                                
008000*    -COPY W330350   -L.                                                  
008200     EJECT                                                                
008300 FD  W33013-UTREG                                                         
008400     LABEL RECORD   STANDARD                                              
008500     RECORDING      V                                                     
008600     BLOCK CONTAINS 0.                                                    
008700     SKIP2                                                                
008800*01  POST -COPY W330310  -PRE UT310-  -L.                                 
009000     SKIP2                                                                
009100*01  POST -COPY W330300  -PRE UT300-  -L.                                 
009300     SKIP2                                                                
009400*01  POST -COPY W330320  -PRE UT320-  -L.                                 
009600     SKIP2                                                                
009700*01  POST -COPY W330330  -PRE UT330-  -L.                                 
009900     SKIP2                                                                
010000*01  POST -COPY W330340  -PRE UT340-  -L.                                 
010200     SKIP2                                                                
010300*01  POST -COPY W330350  -PRE UT350-  -L.                                 
010500     EJECT                                                                
010600 FD  W33012-FEL                                                           
010700     LABEL RECORD    STANDARD                                             
010800     RECORDING       V                                                    
010900     BLOCK CONTAINS 0.                                                    
011000 01  FEL-POST.                                                            
011100*    03  -COPY W092W001                                                   
011300     03  FEL-DEL                PIC X(76).                                
011400     EJECT                                                                
011500     SKIP2                                                                
011600     EJECT                                                                
011700 WORKING-STORAGE SECTION.                                                 
011800     SKIP2                                                                
011810     SKIP3                                                                
011900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3301200'.            
012000*    --- FLAGGOR                                                          
012100 77  TRANS-EOF-SW                PIC X(1)    VALUE 'N'.                   
012200     88  END-OF-TRANS                        VALUE 'J'.                   
012300 77  INREG-EOF-SW                PIC X(1)    VALUE 'N'.                   
012400     88  END-OF-INREG                        VALUE 'J'.                   
012500 77  RATT-DIST                   PIC X(1)    VALUE 'N'.                   
012600 77  JA                          PIC X(1)    VALUE 'J'.                   
012700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
012800 77  NY-ART-AAVV                 PIC X(1)    VALUE 'N'.                   
012900 77  NY-ART-INTRANS              PIC X(1)    VALUE 'N'.                   
013000 77  JUSTERING-GODKAND           PIC X(1)    VALUE 'N'.                   
013100 77  SPAR-IDARTNR                PIC S9(9) VALUE ZERO COMP-3.             
013200 77  NY-IDARTNR                  PIC S9(9) VALUE ZERO COMP-3.             
013300 77  NY-DAFSGVV                  PIC S9(7) VALUE ZERO COMP-3.             
013400 77  TEST-SULEVANT               PIC S9(9) VALUE ZERO COMP-3.             
013500 77  TEST-SUARTFSG          PIC S9(9)V9(2) VALUE ZERO COMP-3.             
013600 77  TOT-RAKNARE-TRANS           PIC S9(9)   VALUE +0 COMP.               
013700 77  TOT-RAKNARE-INREG           PIC S9(9)   VALUE +0 COMP.               
013800 77  TOT-RAKNARE-FELFIL          PIC S9(9)   VALUE +0 COMP.               
013900 77  TOT-RAKNARE-300             PIC S9(9)   VALUE +0 COMP.               
014000 77  TOT-RAKNARE-310             PIC S9(9)   VALUE +0 COMP.               
014100 77  TOT-RAKNARE-320             PIC S9(9)   VALUE +0 COMP.               
014200 77  TOT-RAKNARE-330             PIC S9(9)   VALUE +0 COMP.               
014300 77  TOT-RAKNARE-340             PIC S9(9)   VALUE +0 COMP.               
014400 77  TOT-RAKNARE-350             PIC S9(9)   VALUE +0 COMP.               
014500     SKIP3                                                                
014600 01  FIRST-TIAAVV                PIC 9(7).                                
014700 01  FILLER     REDEFINES FIRST-TIAAVV.                                   
014800     03  FILLER                  PIC 9.                                   
014900     03  FIRST-AA                PIC 9(4).                                
015000     03  FIRST-VV                PIC 9(2).                                
015100                                                                          
015200 01  OMV-AARP                    PIC 9(6).                                
015300 01  FILLER     REDEFINES OMV-AARP.                                       
015400     03  OMV-AA                  PIC 9(4).                                
015500     03  OMV-RP                  PIC 9(2).                                
015600                                                                          
015700 01  DAGENS-AARP                 PIC 9(4).                                
015800 01  FILLER     REDEFINES DAGENS-AARP.                                    
015900     03  DAGENS-AA               PIC 9(2).                                
016000     03  DAGENS-RP               PIC 9(2).                                
016100                                                                          
016200 01  DAGENS-AAVV.                                                         
016300     03  DAGENS-AA-VV            PIC 9(2).                                
016400     03  DAGENS-VV               PIC 9(2).                                
016500                                                                          
016600 01  DAGENS-DATUM                PIC 9(6).                                
016700 01  FILLER     REDEFINES DAGENS-DATUM.                                   
016800     03  DAGENS-AAR              PIC 9(2).                                
016900     03  DAGENS-MAANAD           PIC 9(2).                                
017000     03  DAGENS-DAG              PIC 9(2).                                
017100     EJECT                                                                
017200 01  PACKA-UPP-KORT.                                                      
017300   03  P-UPP-IDPTYP               PIC X(3).                               
017400   03  P-UPP-IDARTNR              PIC 9(9).                               
017500   03  P-UPP-DAFSGVV              PIC 9(7).                               
017600   03  P-UPP-IDDISTR              PIC 9(5).                               
017700   03  P-UPP-KVLEVART             PIC S9(7).                              
017800   03  P-UPP-PRARTNTO             PIC S9(7)V9(2).                         
017900   03  P-UPP-PRARTSJK             PIC S9(7)V9(2).                         
018000   03  P-UPP-IDUSER               PIC X(8).                               
018100     SKIP3                                                                
018200 01  WS-AAVVD.                                                            
018300*                                                                         
018400     03  WS-AAVV                 PIC 9(4)    VALUE ZERO.                  
018500     03  FILLER                  PIC 9(1)    VALUE ZERO.                  
018600     EJECT                                                                
018700 01  DYNAMISKA-SUBPROGRAM.                                                
018800*                                                                         
018900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
019100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
019300     SKIP2                                                                
019400*    --- PARAMETRAR TILL ABEND                                            
019500                                                                          
019600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019700     SKIP2                                                                
019800*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
019900*                                                                         
020000*01  -COPY W0005 -PRE  POSTSUM-                                           
020200     EJECT                                                                
020300*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
020400*                                                                         
020500*01  -COPY WDATAREA                                                       
020700     EJECT                                                                
020800*- - - - - - - - - - - - - - - - DATKORT                                  
020900*                                                                         
021000 01  FILLER                       PIC X(16)  VALUE 'DATKORT'.             
021100 01  DATKORT-PROGNAMN             PIC X(6)   VALUE SPACE.                 
021200 01  DATKORT-ID                   PIC X(6)   VALUE 'WDATUM'.              
021300*01  -COPY WDATKORT.                                                      
021500     EJECT                                                                
021600 01  IN11-AREA-START           PIC X(24)   VALUE                          
021700                                            'IN11-AREA-START  '.          
021800     SKIP3                                                                
021900 01  TRANS-AREA                  PIC X(88).                               
022000*01  FILLER  -PRE IN110-  -COPY W330110C -RED TRANS-AREA                  
022200     SKIP3                                                                
022300*01  FILLER  -PRE IN200-  -COPY W330200C -RED TRANS-AREA                  
022500     EJECT                                                                
022600 01  IN13-AREA-START           PIC X(24)   VALUE                          
022700                                            'IN13-AREA-START  '.          
022800     SKIP3                                                                
022900 01  IN13-AREA.                                                           
023000     03  IN13-IDPTYP              PIC X(3).                               
023100     03  IN13-IDARTNR             PIC S9(9) VALUE ZERO COMP-3.            
023200     03  IN13-DAFSGVV             PIC  9(6) VALUE ZERO .                  
023300     03  FILLER                   PIC X(14).                              
023400     SKIP2                                                                
023500 01  IN310-AREA-START           PIC X(24)   VALUE                         
023600                                           'IN310-AREA-START  '.          
023700     SKIP3                                                                
023800 01  IN31-AREA                  PIC X(28).                                
023900*01  FILLER  -PRE IN310-  -COPY W330310 -RED IN31-AREA                    
024100     SKIP3                                                                
024200 01  IN30-AREA                  PIC X(19).                                
024300*01  FILLER  -PRE IN300-  -COPY W330300 -RED IN30-AREA                    
024500     EJECT                                                                
024600 01  IN32-AREA                  PIC X(14).                                
024700*01  FILLER  -PRE IN320-  -COPY W330320 -RED IN32-AREA                    
024900     EJECT                                                                
025000 01  IN33-AREA                  PIC X(14).                                
025100*01  FILLER  -PRE IN330-  -COPY W330330 -RED IN33-AREA                    
025300     EJECT                                                                
025400 01  IN34-AREA                  PIC X(14).                                
025500*01  FILLER  -PRE IN340-  -COPY W330340 -RED IN34-AREA                    
025700     EJECT                                                                
025800 01  IN35-AREA                  PIC X(14).                                
025900*01  FILLER  -PRE IN350-  -COPY W330350 -RED IN35-AREA                    
026100     EJECT                                                                
026200 01  WS310-AREA-START           PIC X(24)   VALUE                         
026300                                           'WS310-AREA-START  '.          
026400     SKIP3                                                                
026500 01  WS31-AREA                  PIC X(28).                                
026600*01  FILLER  -PRE WS310-  -COPY W330310 -RED WS31-AREA                    
026800     SKIP3                                                                
026900 01  WS30-AREA                  PIC X(19).                                
027000*01  FILLER  -PRE WS300-  -COPY W330300 -RED WS30-AREA                    
027200     EJECT                                                                
027300 01  WS32-AREA                  PIC X(14).                                
027400*01  FILLER  -PRE WS320-  -COPY W330320 -RED WS32-AREA                    
027600     EJECT                                                                
027700 01  WS33-AREA                  PIC X(14).                                
027800*01  FILLER  -PRE WS330-  -COPY W330330 -RED WS33-AREA                    
028000     EJECT                                                                
028100 01  WS34-AREA                  PIC X(14).                                
028200*01  FILLER  -PRE WS340-  -COPY W330340 -RED WS34-AREA                    
028400     EJECT                                                                
028500 01  WS35-AREA                  PIC X(14).                                
028600*01  FILLER  -PRE WS350-  -COPY W330350 -RED WS35-AREA                    
028800     EJECT                                                                
028900 01  FEL-AREA-START            PIC X(24)   VALUE                          
029000                                            'FEL-AREA-START   '.          
029100 01  FEL-AREA.                                                            
029200*    03  FILLER  -COPY W092W001  -PRE W092-.                              
029400     03 FEL-KORT               PIC X(76).                                 
029500                                                                          
029600     EJECT                                                                
029700 PROCEDURE DIVISION.                                                      
029800     SKIP2                                                                
029900 STYR SECTION.                                                            
030000     PERFORM A-INIT                                                       
030100     PERFORM S03-LAS-13-FIL                                               
030200     PERFORM S01-HAEMTA-POSTGRUPP13                                       
030300     PERFORM S02-LAS-11-FIL                                               
030400     PERFORM UNTIL END-OF-TRANS AND END-OF-INREG                          
030500       IF NOT END-OF-INREG                                                
030600         PERFORM S06-GAMMAL-VECKA-BORT                                    
030700         PERFORM B-HITTA-RAETT-13GRUPP                                    
030800       END-IF                                                             
030900       IF END-OF-INREG                                                    
031000         PERFORM S04-SKRIV-SISTA-POST                                     
031100       END-IF                                                             
031200       IF NOT END-OF-TRANS                                                
031300         IF IN110-IDPTYP = 200                                            
031400           IF IN200-IDDISTR = 0                                           
031500             PERFORM C-JUSTERA-POST-300                                   
031600           END-IF                                                         
031700           IF IN200-IDPTYP = 200 AND                                      
031800              IN200-IDDISTR > 0                                           
031900                                                                          
032000             IF IN300-IDARTNR = IN200-IDARTNR     OR                      
032100              IN200-IDARTNR = NY-IDARTNR          OR                      
032110              IN200-IDARTNR = WS300-IDARTNR                               
032200                                                                          
032300               IF IN300-DAFSGVV = IN200-DAFSGVV   OR                      
032400                  IN200-DAFSGVV = NY-DAFSGVV      OR                      
032410                  IN200-DAFSGVV = WS300-DAFSGVV                           
032500                                                                          
032600                 PERFORM D-JUSTERA-POST-310                               
032700                 PERFORM S02-LAS-11-FIL                                   
032800               ELSE                                                       
032900                 IF IN300-DAFSGVV < IN200-DAFSGVV                         
033000                   CONTINUE                                               
033100                 ELSE                                                     
033200                   MOVE 316 TO W092-IDFELKODX                             
033300                   PERFORM S25-SKRIV-PA-FELFIL                            
033400                   PERFORM S02-LAS-11-FIL                                 
033500                 END-IF                                                   
033600               END-IF                                                     
033700             ELSE                                                         
033800               IF IN300-IDARTNR < IN200-IDARTNR                           
033900                 CONTINUE                                                 
034000               ELSE                                                       
034100                 MOVE 316 TO W092-IDFELKODX                               
034200                 PERFORM S25-SKRIV-PA-FELFIL                              
034300                 PERFORM S02-LAS-11-FIL                                   
034400               END-IF                                                     
034500             END-IF                                                       
034600           END-IF                                                         
034700           MOVE ZERO TO NY-IDARTNR                                        
034800           MOVE ZERO TO NY-DAFSGVV                                        
034900           PERFORM S99-SKRIV-FIL-13                                       
035000         ELSE                                                             
035100           IF IN110-IDARTNR = IN300-IDARTNR AND                           
035200              IN110-DAFSGVV = IN300-DAFSGVV                               
035300           OR IN110-IDARTNR = WS300-IDARTNR AND                           
035400              IN110-DAFSGVV = WS300-DAFSGVV                               
035500             IF IN110-IDARTNR = IN300-IDARTNR AND                         
035600                IN110-DAFSGVV = IN300-DAFSGVV AND                         
035700                IN110-IDDISTR  = IN310-IDDISTR                            
035800               PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                      
035900               PERFORM I-JUSTERA-310POST-MED-110POST                      
036000               PERFORM J-JUSTERA-320-330-340-350                          
036100             ELSE                                                         
036200               PERFORM F-SKAPA-NY-310POST                                 
036300               PERFORM G-SKAPA-SUMMAPOSTER                                
036400             END-IF                                                       
036500           ELSE                                                           
036600             PERFORM E-SKAPA-NY-300POST                                   
036700             PERFORM F-SKAPA-NY-310POST                                   
036800             PERFORM G-SKAPA-SUMMAPOSTER                                  
036900           END-IF                                                         
037000           PERFORM H-KOLLA-JUST-NYTT-DISTR                                
037100           PERFORM S99-SKRIV-FIL-13                                       
037200         END-IF                                                           
037300       END-IF                                                             
037400     END-PERFORM                                                          
037500     PERFORM Z-FINIT                                                      
037600     MOVE ZERO TO RETURN-CODE                                             
037700     GOBACK                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 A-INIT SECTION.                                                          
038100     SKIP2                                                                
038200     OPEN INPUT W33011-TRANS                                              
038300                W33013-INREG                                              
038400     SKIP2                                                                
038500     OPEN OUTPUT W33013-UTREG                                             
038600                 W33012-FEL                                               
038700                                                                          
038800     MOVE ZERO TO FIRST-TIAAVV                                            
038900                                                                          
039000     MOVE ZERO TO TOT-RAKNARE-TRANS                                       
039100                  TOT-RAKNARE-INREG                                       
039200                  TOT-RAKNARE-FELFIL                                      
039300                  TOT-RAKNARE-300                                         
039400                  TOT-RAKNARE-310                                         
039500                  TOT-RAKNARE-320                                         
039600                  TOT-RAKNARE-330                                         
039700                  TOT-RAKNARE-340                                         
039800                  TOT-RAKNARE-350                                         
039900                  WS300-IDARTNR                                           
040000                  WS300-DAFSGVV                                           
040100                                                                          
040200     MOVE PROGRAM-NAMN TO DATKORT-PROGNAMN                                
040300                                                                          
040400     CALL DATKORT USING DATKORT-PROGNAMN DATKORT-ID DATUMKORT             
040500     MOVE D-AAR    TO DAGENS-AAR                                          
040600                      DAGENS-AA-VV                                        
040700     MOVE D-MAANAD TO DAGENS-MAANAD                                       
040800     MOVE D-DAG    TO DAGENS-DAG                                          
040900     MOVE D-VECKA  TO DAGENS-VV                                           
041000     MOVE ZERO TO IN300-IDARTNR                                           
041100                  IN300-DAFSGVV                                           
041200                  IN310-IDDISTR                                           
041300     PERFORM AA-OMV-DAGENS-DATUM-TILL-AARP                                
041400     PERFORM AB-HAMTA-FIRST-VECKA                                         
041500     .                                                                    
041600     EJECT                                                                
041700 AA-OMV-DAGENS-DATUM-TILL-AARP SECTION.                                   
041800     SKIP2                                                                
041900     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
042000     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
042100                                                                          
042200     CALL WDATKONV USING DAT-KDDATFORM                                    
042300                         DAT-I-TIDATUM                                    
042400                         DAT-O-TIDATUM                                    
042500                         DAT-KDSVAR                                       
042600                                                                          
042700     MOVE DAT-TIAARP TO DAGENS-AARP                                       
042800     .                                                                    
042900                                                                          
043000 AB-HAMTA-FIRST-VECKA SECTION.                                            
043100     SKIP2                                                                
043200     MOVE DAGENS-AARP   TO OMV-AARP                                       
043210                                                                          
043220     IF OMV-AA  <  70                                                     
043230       THEN                                                               
043240       COMPUTE  OMV-AA = OMV-AA + 2000                                    
043250     ELSE                                                                 
043254       IF   OMV-AA  < 100                                                 
043260         COMPUTE  OMV-AA = OMV-AA + 1900                                  
043270       END-IF                                                             
043271     END-IF                                                               
043280                                                                          
043300     SUBTRACT 2 FROM OMV-AA                                               
043400     ADD +1 TO OMV-RP                                                     
043500     IF OMV-RP = 13                                                       
043600        MOVE 1 TO OMV-RP                                                  
043700        ADD +1 TO OMV-AA                                                  
043800     END-IF                                                               
043900                                                                          
044000     MOVE OMV-AARP      TO DAT-I-TIDATUM                                  
044100     MOVE 'AARP'        TO DAT-KDDATFORM                                  
044200                                                                          
044300      CALL WDATKONV USING DAT-KDDATFORM                                   
044400                          DAT-I-TIDATUM                                   
044500                          DAT-O-TIDATUM                                   
044600                          DAT-KDSVAR                                      
044700                                                                          
044800     MOVE DAT-TIAA-VECKA  TO FIRST-AA                                     
044900     MOVE DAT-TIVV        TO FIRST-VV                                     
044910                                                                          
044920     IF FIRST-AA <  70                                                    
044930     THEN                                                                 
044940       COMPUTE FIRST-AA = FIRST-AA + 2000                                 
044950     ELSE                                                                 
044954       IF FIRST-AA  <  100                                                
044955       THEN                                                               
044960         COMPUTE FIRST-AA = FIRST-AA + 1900                               
044961       END-IF                                                             
044970     END-IF                                                               
044980                                                                          
045000************ FIX FIX FIX 910119 SE/PF FÖR ATT KLARA ÅRSSKIFTE             
045100************ DÅ WDATKONV GER FEL VECKA                                    
045200     IF FIRST-AA = 1998 AND FIRST-VV = 53                                 
045300        MOVE 1999           TO FIRST-AA                                   
045400        MOVE 01           TO FIRST-VV                                     
045500     END-IF                                                               
045600************ FIX FIX FIX 910119 SE/PF FÖR ATT KLARA ÅRSSKIFTE             
045700     .                                                                    
045800     EJECT                                                                
045900 B-HITTA-RAETT-13GRUPP   SECTION.                                         
046000     SKIP2                                                                
046100     PERFORM UNTIL IN300-IDARTNR >= IN110-IDARTNR OR END-OF-INREG         
046200       PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                              
046300       PERFORM S99-SKRIV-FIL-13                                           
046400       PERFORM S01-HAEMTA-POSTGRUPP13                                     
046500       PERFORM S06-GAMMAL-VECKA-BORT                                      
046600     END-PERFORM                                                          
046700     IF NOT END-OF-INREG                                                  
046800       MOVE IN300-IDARTNR TO SPAR-IDARTNR                                 
046900       IF IN300-IDARTNR = IN110-IDARTNR                                   
047000         PERFORM UNTIL IN300-DAFSGVV NOT < IN110-DAFSGVV OR               
047100                       SPAR-IDARTNR NOT = IN300-IDARTNR  OR               
047200                       END-OF-INREG                                       
047300           PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                          
047400           PERFORM S99-SKRIV-FIL-13                                       
047500           PERFORM S01-HAEMTA-POSTGRUPP13                                 
047600           PERFORM S06-GAMMAL-VECKA-BORT                                  
047700         END-PERFORM                                                      
047800       END-IF                                                             
047900     END-IF                                                               
048000     IF NOT END-OF-INREG                                                  
048100       MOVE NEJ TO RATT-DIST                                              
048200       PERFORM UNTIL RATT-DIST = JA                                       
048300         IF IN300-IDARTNR = IN110-IDARTNR AND                             
048400              IN300-DAFSGVV = IN110-DAFSGVV AND                           
048500              IN310-IDDISTR = IN110-IDDISTR                               
048600              MOVE JA TO RATT-DIST                                        
048700         ELSE                                                             
048800           IF IN300-IDARTNR = IN110-IDARTNR AND                           
048900              IN300-DAFSGVV = IN110-DAFSGVV AND                           
049000              IN310-IDDISTR < IN110-IDDISTR                               
049100               PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                      
049200               PERFORM S99-SKRIV-FIL-13                                   
049300               PERFORM S01-HAEMTA-POSTGRUPP13                             
049400               PERFORM S06-GAMMAL-VECKA-BORT                              
049500           ELSE                                                           
049600             MOVE JA TO RATT-DIST                                         
049700           END-IF                                                         
049800         END-IF                                                           
049900       END-PERFORM                                                        
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 C-JUSTERA-POST-300   SECTION.                                            
050400     SKIP2                                                                
050500     MOVE NEJ TO NY-ART-AAVV                                              
050600     IF IN300-IDARTNR = IN200-IDARTNR AND                                 
050700          IN300-DAFSGVV = IN200-DAFSGVV                                   
050800                                                                          
050900*--------------JUSTERING AV SJK                                           
051000*--------------OBS!! SJK FLYTTAS TILL IN300-.... PGA                      
051100*--------------DET KAN KOMMA EN 200POST TILL SOM                          
051200*--------------JUSTERAR 310POSTEN DÅ FLYTTAS SAMTLIGA                     
051300*--------------300 - 350 POSTERNA TILL WS300-350                          
051400       MOVE IN200-PRARTSJK TO IN300-PRARTSJK                              
051500       PERFORM S02-LAS-11-FIL                                             
051600       IF IN200-IDPTYP = 200 AND                                          
051700          IN200-IDARTNR = IN300-IDARTNR AND                               
051800          IN200-DAFSGVV = IN300-DAFSGVV                                   
051900         PERFORM  B-HITTA-RAETT-13GRUPP                                   
052000       ELSE                                                               
052100         PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                            
052200       END-IF                                                             
052300     ELSE                                                                 
052400                                                                          
052500*-----------NY ART AAVV SJK                                               
052600       MOVE 300 TO WS300-IDPTYP                                           
052700       MOVE IN200-IDARTNR TO WS300-IDARTNR                                
052800       MOVE IN200-DAFSGVV TO WS300-DAFSGVV                                
052900       MOVE IN200-PRARTSJK TO WS300-PRARTSJK                              
053000       PERFORM S30-SKRIV-300POST                                          
053100       MOVE ZERO TO WS300-IDPTYP                                          
053200       MOVE JA TO NY-ART-AAVV                                             
053300       MOVE IN200-IDARTNR TO NY-IDARTNR                                   
053400       MOVE IN200-DAFSGVV TO NY-DAFSGVV                                   
053500       PERFORM S02-LAS-11-FIL                                             
053600     END-IF                                                               
053700                                                                          
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100 D-JUSTERA-POST-310   SECTION.                                            
054200     SKIP2                                                                
054300     IF IN300-IDARTNR = IN200-IDARTNR AND                                 
054400        IN300-DAFSGVV = IN200-DAFSGVV AND                                 
054500        IN310-IDDISTR = IN200-IDDISTR AND                                 
054600        NY-ART-AAVV = NEJ                                                 
054700       PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                              
054800       PERFORM S20-JUSTERA-DISTR-KVANT                                    
054900     ELSE                                                                 
055000*-------------HÄR GER VILLKOREN UTSKRIFT PÅ FELFIL'                       
055100       IF ( NY-ART-AAVV = JA AND                                          
055200          NY-IDARTNR = IN200-IDARTNR AND                                  
055300          NY-DAFSGVV = IN200-DAFSGVV ) OR                                 
055400          ( IN200-IDARTNR = IN300-IDARTNR AND                             
055500          IN200-DAFSGVV = IN300-DAFSGVV )                                 
055600                                                                          
055700                                                                          
055800         IF IN200-KVLEVART < ZERO                                         
055900           MOVE 315 TO W092-IDFELKODX                                     
056000           PERFORM S25-SKRIV-PA-FELFIL                                    
056100         ELSE                                                             
056200           IF IN200-PRARTNTO < ZERO                                       
056300             MOVE 315 TO W092-IDFELKODX                                   
056400             PERFORM S25-SKRIV-PA-FELFIL                                  
056500           ELSE                                                           
056501***XXFÖR ATT FÅ MED 300POST OM NYTT DIST ÄR DET 1:A TILL 300POSTEN        
056502             IF IN200-IDDISTR < IN310-IDDISTR                             
056510               MOVE IN300-IDPTYP TO WS300-IDPTYP                          
056520               MOVE IN300-IDARTNR TO WS300-IDARTNR                        
056530               MOVE IN300-DAFSGVV TO WS300-DAFSGVV                        
056540               MOVE IN300-PRARTSJK TO WS300-PRARTSJK                      
056550               IF WS300-IDPTYP = 300                                      
056560                 PERFORM S30-SKRIV-300POST                                
056570                 MOVE ZERO TO WS300-IDPTYP                                
056580                 MOVE ZERO TO IN300-IDPTYP                                
056590               END-IF                                                     
056591             END-IF                                                       
056600***XX                                                                     
056610             MOVE 310 TO WS310-IDPTYP                                     
056700             MOVE IN200-IDDISTR TO WS310-IDDISTR                          
056800             MOVE IN200-KVLEVART TO WS310-SULEVANT                        
056900             MOVE IN200-PRARTNTO TO WS310-SUARTFSG                        
057000             MOVE ZERO TO WS310-SULEVANT-DO                               
057100                          WS310-SUARTFSG-DO                               
057200             PERFORM S31-SKRIV-310POST                                    
057300             MOVE ZERO TO WS310-IDPTYP                                    
057400           END-IF                                                         
057500         END-IF                                                           
057600       ELSE                                                               
057610**XXX FÖR ATT FÅ MED JUSTERINGEN OM DET ÄR ETT NYTT DIST                  
057620**XXX OCH DET SISTA FÖRE NÄSTA 300P                                       
057633         IF WS300-IDARTNR = IN200-IDARTNR                                 
057640            AND WS300-DAFSGVV = IN200-DAFSGVV                             
057650           MOVE 310 TO WS310-IDPTYP                                       
057660           MOVE IN200-IDDISTR TO WS310-IDDISTR                            
057670           MOVE IN200-KVLEVART TO WS310-SULEVANT                          
057680           MOVE IN200-PRARTNTO TO WS310-SUARTFSG                          
057690           MOVE ZERO TO WS310-SULEVANT-DO                                 
057691                        WS310-SUARTFSG-DO                                 
057692           PERFORM S31-SKRIV-310POST                                      
057693           MOVE ZERO TO WS310-IDPTYP                                      
057694         ELSE                                                             
057700           MOVE 316 TO W092-IDFELKODX                                     
057800           PERFORM S25-SKRIV-PA-FELFIL                                    
057910         END-IF                                                           
057911**XXX                                                                     
057920       END-IF                                                             
058000     END-IF                                                               
058100     MOVE NEJ TO NY-ART-AAVV                                              
058200     .                                                                    
058300     EJECT                                                                
058400 E-SKAPA-NY-300POST   SECTION.                                            
058500     SKIP2                                                                
058600     MOVE 300 TO WS300-IDPTYP                                             
058700     MOVE IN110-IDARTNR TO WS300-IDARTNR                                  
058800     MOVE IN110-DAFSGVV TO WS300-DAFSGVV                                  
058900     MOVE IN110-PRARTSJK TO WS300-PRARTSJK                                
059000     .                                                                    
059100                                                                          
059200 F-SKAPA-NY-310POST   SECTION.                                            
059300     SKIP2                                                                
059400     MOVE 310 TO WS310-IDPTYP                                             
059500     MOVE IN110-IDDISTR TO WS310-IDDISTR                                  
059600     MOVE IN110-SULEVANT TO WS310-SULEVANT                                
059700     MOVE IN110-SUARTFSG TO WS310-SUARTFSG                                
059800     MOVE IN110-SULEVANT-DO TO WS310-SULEVANT-DO                          
059900     MOVE IN110-SUARTFSG-DO TO WS310-SUARTFSG-DO                          
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 G-SKAPA-SUMMAPOSTER  SECTION.                                            
060400     SKIP2                                                                
060500     IF IN110-SULEVANT-RAB > ZERO OR                                      
060600        IN110-SUARTFSG-RAB > ZERO                                         
060700       MOVE 320 TO WS320-IDPTYP                                           
060800       MOVE IN110-SULEVANT-RAB TO WS320-SULEVANT-RAB                      
060900       MOVE IN110-SUARTFSG-RAB TO WS320-SUARTFSG-RAB                      
061000     END-IF                                                               
061100     IF IN110-SULEVANT-SPEC > ZERO OR                                     
061200        IN110-SUARTFSG-SPEC > ZERO                                        
061300       MOVE 330 TO WS330-IDPTYP                                           
061400       MOVE IN110-SULEVANT-SPEC TO WS330-SULEVANT-SPEC                    
061500       MOVE IN110-SUARTFSG-SPEC TO WS330-SUARTFSG-SPEC                    
061600     END-IF                                                               
061700     IF IN110-SULEVANT-MAN > ZERO OR                                      
061800        IN110-SUARTFSG-MAN > ZERO                                         
061900       MOVE 340 TO WS340-IDPTYP                                           
062000       MOVE IN110-SULEVANT-MAN TO WS340-SULEVANT-MAN                      
062100       MOVE IN110-SUARTFSG-MAN TO WS340-SUARTFSG-MAN                      
062200     END-IF                                                               
062300     IF IN110-SULEVANT-KRE > ZERO OR                                      
062400        IN110-SUARTFSG-KRE > ZERO                                         
062500       MOVE 350 TO WS350-IDPTYP                                           
062600       MOVE IN110-SULEVANT-KRE TO WS350-SULEVANT-KRE                      
062700       MOVE IN110-SUARTFSG-KRE TO WS350-SUARTFSG-KRE                      
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100                                                                          
063200 H-KOLLA-JUST-NYTT-DISTR  SECTION.                                        
063300     SKIP2                                                                
063400     PERFORM S02-LAS-11-FIL                                               
063500*----                                                                     
063600*----HÄR KAN TVÅ JUSTERINGSPOSTER KOMMA EFTER VARANDRA                    
063700*----SOM BÅDA JUSTERAR AKTUELL 300-310POST                                
063800*----                                                                     
063900     IF IN200-IDPTYP = 200                                                
064000       PERFORM UNTIL IN110-IDARTNR > WS300-IDARTNR OR                     
064100          IN110-DAFSGVV > WS300-DAFSGVV OR END-OF-TRANS                   
064200           PERFORM S20-JUSTERA-DISTR-KVANT                                
064300           PERFORM S02-LAS-11-FIL                                         
064400       END-PERFORM                                                        
064500     END-IF                                                               
064600     .                                                                    
064700                                                                          
064800 I-JUSTERA-310POST-MED-110POST SECTION.                                   
064900     SKIP2                                                                
065000     ADD IN110-SULEVANT TO WS310-SULEVANT                                 
065100     ADD IN110-SUARTFSG TO WS310-SUARTFSG                                 
065200     ADD IN110-SULEVANT-DO TO WS310-SULEVANT-DO                           
065300     ADD IN110-SUARTFSG-DO TO WS310-SUARTFSG-DO                           
065400     .                                                                    
065500     EJECT                                                                
065600 J-JUSTERA-320-330-340-350 SECTION.                                       
065700     SKIP2                                                                
065800     IF IN110-SULEVANT-RAB > ZERO OR                                      
065900        IN110-SUARTFSG-RAB > ZERO                                         
066000       IF WS320-IDPTYP = 320                                              
066100         ADD IN110-SULEVANT-RAB TO WS320-SULEVANT-RAB                     
066200         ADD IN110-SUARTFSG-RAB TO WS320-SUARTFSG-RAB                     
066300       ELSE                                                               
066400         MOVE 320 TO WS320-IDPTYP                                         
066500         MOVE IN110-SULEVANT-RAB TO WS320-SULEVANT-RAB                    
066600         MOVE IN110-SUARTFSG-RAB TO WS320-SUARTFSG-RAB                    
066700       END-IF                                                             
066800     END-IF                                                               
066900     IF IN110-SULEVANT-SPEC > ZERO OR                                     
067000        IN110-SUARTFSG-SPEC > ZERO                                        
067100       IF WS330-IDPTYP = 330                                              
067200         ADD IN110-SULEVANT-SPEC TO WS330-SULEVANT-SPEC                   
067300         ADD IN110-SUARTFSG-SPEC TO WS330-SUARTFSG-SPEC                   
067400       ELSE                                                               
067500         MOVE 330 TO WS330-IDPTYP                                         
067600         MOVE IN110-SULEVANT-SPEC TO WS330-SULEVANT-SPEC                  
067700         MOVE IN110-SUARTFSG-SPEC TO WS330-SUARTFSG-SPEC                  
067800       END-IF                                                             
067900     END-IF                                                               
068000     IF IN110-SULEVANT-MAN > ZERO OR                                      
068100        IN110-SUARTFSG-MAN > ZERO                                         
068200       IF WS340-IDPTYP = 340                                              
068300         ADD IN110-SULEVANT-MAN TO WS340-SULEVANT-MAN                     
068400         ADD IN110-SUARTFSG-MAN TO WS340-SUARTFSG-MAN                     
068500       ELSE                                                               
068600         MOVE 340 TO WS340-IDPTYP                                         
068700         MOVE IN110-SULEVANT-MAN TO WS340-SULEVANT-MAN                    
068800         MOVE IN110-SUARTFSG-MAN TO WS340-SUARTFSG-MAN                    
068900       END-IF                                                             
069000     END-IF                                                               
069100     IF IN110-SULEVANT-KRE > ZERO OR                                      
069200        IN110-SUARTFSG-KRE > ZERO                                         
069300       IF WS350-IDPTYP = 350                                              
069400         ADD IN110-SULEVANT-KRE TO WS350-SULEVANT-KRE                     
069500         ADD IN110-SUARTFSG-KRE TO WS350-SUARTFSG-KRE                     
069600       ELSE                                                               
069700         MOVE 350 TO WS350-IDPTYP                                         
069800         MOVE IN110-SULEVANT-KRE TO WS350-SULEVANT-KRE                    
069900         MOVE IN110-SUARTFSG-KRE TO WS350-SUARTFSG-KRE                    
070000       END-IF                                                             
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 Z-FINIT SECTION.                                                         
070500     SKIP2                                                                
070600     CLOSE W33011-TRANS                                                   
070700           W33013-INREG                                                   
070800           W33013-UTREG                                                   
070900           W33012-FEL                                                     
071000     SKIP2                                                                
071100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
071200     MOVE 'T' TO POSTSUM-OPKOD                                            
071300                                                                          
071400     MOVE 'IN  '                 TO  POSTSUM-TRANSTYP                     
071500     MOVE 'W33011'               TO  POSTSUM-FDNAMN                       
071600     MOVE 'W33012D1'             TO  POSTSUM-DDNAMN2                      
071700     MOVE TOT-RAKNARE-TRANS      TO  POSTSUM-TOTTRANS                     
071800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
071900                                                                          
072000     MOVE 'IN  '                 TO  POSTSUM-TRANSTYP                     
072100     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
072200     MOVE 'W33012D2'             TO  POSTSUM-DDNAMN2                      
072300     MOVE TOT-RAKNARE-INREG      TO  POSTSUM-TOTTRANS                     
072400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
072500                                                                          
072600     MOVE '300 '                 TO  POSTSUM-TRANSTYP                     
072700     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
072800     MOVE 'W33012D3'             TO  POSTSUM-DDNAMN2                      
072900     MOVE TOT-RAKNARE-300        TO  POSTSUM-TOTTRANS                     
073000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
073100                                                                          
073200     MOVE '310 '                 TO  POSTSUM-TRANSTYP                     
073300     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
073400     MOVE 'W33012D3'             TO  POSTSUM-DDNAMN2                      
073500     MOVE TOT-RAKNARE-310        TO  POSTSUM-TOTTRANS                     
073600     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
073700                                                                          
073800     MOVE '320 '                 TO  POSTSUM-TRANSTYP                     
073900     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
074000     MOVE 'W33012D3'             TO  POSTSUM-DDNAMN2                      
074100     MOVE TOT-RAKNARE-320        TO  POSTSUM-TOTTRANS                     
074200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
074300                                                                          
074400     MOVE '330 '                 TO  POSTSUM-TRANSTYP                     
074500     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
074600     MOVE 'W33012D3'             TO  POSTSUM-DDNAMN2                      
074700     MOVE TOT-RAKNARE-330        TO  POSTSUM-TOTTRANS                     
074800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
074900                                                                          
075000     MOVE '340 '                 TO  POSTSUM-TRANSTYP                     
075100     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
075200     MOVE 'W33012D3'             TO  POSTSUM-DDNAMN2                      
075300     MOVE TOT-RAKNARE-340        TO  POSTSUM-TOTTRANS                     
075400     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
075500                                                                          
075600     MOVE '350 '                 TO  POSTSUM-TRANSTYP                     
075700     MOVE 'W33013'               TO  POSTSUM-FDNAMN                       
075800     MOVE 'W33012D3'             TO  POSTSUM-DDNAMN2                      
075900     MOVE TOT-RAKNARE-350        TO  POSTSUM-TOTTRANS                     
076000     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
076100                                                                          
076200     MOVE 'FEL '                 TO  POSTSUM-TRANSTYP                     
076300     MOVE 'W33012'               TO  POSTSUM-FDNAMN                       
076400     MOVE 'W33012D4'             TO  POSTSUM-DDNAMN2                      
076500     MOVE TOT-RAKNARE-FELFIL     TO  POSTSUM-TOTTRANS                     
076600     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
076700                                                                          
076800                                                                          
076900     MOVE 'S' TO POSTSUM-OPKOD                                            
077000     CALL POSTSUM USING POSTSUM-PARM                                      
077100     .                                                                    
077200     EJECT                                                                
077300 S01-HAEMTA-POSTGRUPP13  SECTION.                                         
077400     SKIP2                                                                
077500     IF IN13-IDPTYP = 300                                                 
077600       MOVE IN13-AREA TO IN30-AREA                                        
077700       IF NOT END-OF-INREG                                                
077800         PERFORM S03-LAS-13-FIL                                           
077900       END-IF                                                             
078000     END-IF                                                               
078100     IF IN13-IDPTYP = 310                                                 
078200       MOVE IN13-AREA TO IN31-AREA                                        
078300       IF NOT END-OF-INREG                                                
078400         PERFORM S03-LAS-13-FIL                                           
078500       END-IF                                                             
078600     END-IF                                                               
078700                                                                          
078800     PERFORM UNTIL IN13-IDPTYP = 300 OR 310 OR END-OF-INREG               
078900                                                                          
079000       IF IN13-IDPTYP = 320                                               
079100         MOVE IN13-AREA TO IN32-AREA                                      
079200       END-IF                                                             
079300                                                                          
079400       IF IN13-IDPTYP = 330                                               
079500         MOVE IN13-AREA TO IN33-AREA                                      
079600       END-IF                                                             
079700                                                                          
079800       IF IN13-IDPTYP = 340                                               
079900         MOVE IN13-AREA TO IN34-AREA                                      
080000       END-IF                                                             
080100                                                                          
080200       IF IN13-IDPTYP = 350                                               
080300         MOVE IN13-AREA TO IN35-AREA                                      
080400       END-IF                                                             
080500       PERFORM S03-LAS-13-FIL                                             
080600     END-PERFORM                                                          
080700     .                                                                    
080800     EJECT                                                                
080900                                                                          
081000 S02-LAS-11-FIL  SECTION.                                                 
081100     SKIP2                                                                
081200     READ W33011-TRANS INTO TRANS-AREA                                    
081300     AT END                                                               
081400       SET END-OF-TRANS TO TRUE                                           
081500       MOVE +999999999 TO IN110-IDARTNR                                   
081600       MOVE +99999     TO IN110-DAFSGVV                                   
081700                          IN110-IDDISTR                                   
081800     END-READ                                                             
081900     IF NOT END-OF-TRANS                                                  
082000       ADD +1 TO TOT-RAKNARE-TRANS                                        
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400                                                                          
082500 S03-LAS-13-FIL   SECTION.                                                
082600     SKIP2                                                                
082700     READ W33013-INREG INTO IN13-AREA                                     
082800     AT END                                                               
082900       SET END-OF-INREG TO TRUE                                           
083000       MOVE +999999999 TO IN300-IDARTNR                                   
083100       MOVE +99999     TO IN300-DAFSGVV                                   
083200                          IN310-IDDISTR                                   
083300     END-READ                                                             
083400     IF NOT END-OF-INREG                                                  
083500       ADD +1 TO TOT-RAKNARE-INREG                                        
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
084000 S04-SKRIV-SISTA-POST SECTION.                                            
084100     SKIP2                                                                
084200     IF IN300-IDARTNR < IN110-IDARTNR                                     
084300       PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                              
084400       PERFORM S99-SKRIV-FIL-13                                           
084500       PERFORM S05-ATERSTAELL-INREGPOST                                   
084600     ELSE                                                                 
084700       IF IN300-IDARTNR = IN110-IDARTNR AND                               
084800          IN300-DAFSGVV < IN110-DAFSGVV                                   
084900         PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                            
085000         PERFORM S99-SKRIV-FIL-13                                         
085100         PERFORM S05-ATERSTAELL-INREGPOST                                 
085200       ELSE                                                               
085300         IF IN300-IDARTNR = IN110-IDARTNR AND                             
085400            IN300-DAFSGVV = IN110-DAFSGVV AND                             
085500            IN310-IDDISTR < IN110-IDDISTR                                 
085600           PERFORM S98-FLYTTA-INAREA-TILL-WSAREA                          
085700           PERFORM S99-SKRIV-FIL-13                                       
085800           PERFORM S05-ATERSTAELL-INREGPOST                               
085900                                                                          
086000         END-IF                                                           
086100       END-IF                                                             
086200     END-IF                                                               
086300     .                                                                    
086400     EJECT                                                                
086500 S05-ATERSTAELL-INREGPOST SECTION.                                        
086600     SKIP2                                                                
086700     MOVE +999999999 TO IN300-IDARTNR                                     
086800     MOVE +99999 TO IN300-DAFSGVV                                         
086900                    IN310-IDDISTR                                         
087000     .                                                                    
087100     EJECT                                                                
087200 S06-GAMMAL-VECKA-BORT SECTION.                                           
087300     SKIP2                                                                
087400     IF NOT END-OF-INREG                                                  
087500       IF IN300-IDPTYP = 300                                              
087600         PERFORM UNTIL (IN300-IDPTYP = 300 AND                            
087700                       IN300-DAFSGVV NOT < FIRST-TIAAVV) OR               
                             END-OF-INREG                                       
087800           PERFORM S01-HAEMTA-POSTGRUPP13                                 
087900         END-PERFORM                                                      
088000       END-IF                                                             
088100     END-IF.                                                              
088200     EJECT                                                                
088300                                                                          
088400 S20-JUSTERA-DISTR-KVANT SECTION.                                         
088500     SKIP2                                                                
088600     IF WS310-IDDISTR = IN200-IDDISTR                                     
088700       MOVE JA TO JUSTERING-GODKAND                                       
088800       IF IN200-KVLEVART NOT = ZERO                                       
088900         IF IN200-KVLEVART NOT < ZERO                                     
089000           ADD IN200-KVLEVART TO WS310-SULEVANT                           
089100         ELSE                                                             
089200           MOVE WS310-SULEVANT TO TEST-SULEVANT                           
089300           ADD IN200-KVLEVART TO TEST-SULEVANT                            
089400           IF TEST-SULEVANT NOT < ZERO                                    
089500             MOVE TEST-SULEVANT TO WS310-SULEVANT                         
089600           ELSE                                                           
089700             MOVE 313 TO W092-IDFELKODX                                   
089800             PERFORM S25-SKRIV-PA-FELFIL                                  
089900             MOVE NEJ TO JUSTERING-GODKAND                                
090000           END-IF                                                         
090100         END-IF                                                           
090200       END-IF                                                             
090300     END-IF                                                               
090400     IF JUSTERING-GODKAND = JA AND                                        
090500        IN200-PRARTNTO NOT = ZERO                                         
090600       IF IN200-PRARTNTO NOT < ZERO                                       
090700         ADD IN200-PRARTNTO TO WS310-SUARTFSG                             
090800       ELSE                                                               
090900         MOVE WS310-SUARTFSG TO TEST-SUARTFSG                             
091000         ADD IN200-PRARTNTO TO TEST-SUARTFSG                              
091100         IF TEST-SUARTFSG NOT < ZERO                                      
091200           MOVE TEST-SUARTFSG TO WS310-SUARTFSG                           
091300         ELSE                                                             
091400           MOVE 314 TO W092-IDFELKODX                                     
091500           PERFORM S25-SKRIV-PA-FELFIL                                    
091600           SUBTRACT IN200-KVLEVART FROM WS310-SULEVANT                    
091700         END-IF                                                           
091800       END-IF                                                             
091900     ELSE                                                                 
092000       IF WS310-IDPTYP NOT = 310                                          
092100         MOVE 310            TO WS310-IDPTYP                              
092200         MOVE IN200-IDDISTR  TO WS310-IDDISTR                             
092300         MOVE IN200-KVLEVART TO WS310-SULEVANT                            
092400         MOVE IN200-PRARTNTO TO WS310-SUARTFSG                            
092500         MOVE ZERO           TO WS310-SUARTFSG-DO                         
092600                                WS310-SUARTFSG-DO                         
092700                                TEST-SULEVANT                             
092800                                TEST-SUARTFSG                             
092900       END-IF                                                             
093000     END-IF                                                               
093100     .                                                                    
093200     EJECT                                                                
093300 S25-SKRIV-PA-FELFIL  SECTION.                                            
093400     SKIP2                                                                
093500     MOVE IN200-IDARTNR  TO W092-SORTBGP                                  
093600     MOVE IN200-IDPTYP   TO W092-IDPTYP                                   
093700     MOVE IN200-IDDISTR  TO W092-IDDISTR                                  
093800                                                                          
093900     MOVE ZERO           TO W092-IDKUNDNR                                 
094000                            W092-KDCLAGER                                 
094100                            W092-KDFRAKT                                  
094200                            W092-IDORDNR                                  
094300                            W092-KDORDKL                                  
094400                            W092-KDFELMRK                                 
094500     MOVE SPACE          TO W092-FILLER2                                  
094600                                                                          
094700     MOVE '200'          TO P-UPP-IDPTYP                                  
094800     MOVE IN200-IDARTNR  TO P-UPP-IDARTNR                                 
094900     MOVE IN200-IDDISTR  TO P-UPP-IDDISTR                                 
095000     MOVE IN200-DAFSGVV  TO P-UPP-DAFSGVV                                 
095100     MOVE IN200-KVLEVART TO P-UPP-KVLEVART                                
095200     MOVE IN200-PRARTNTO TO P-UPP-PRARTNTO                                
095300     MOVE IN200-PRARTSJK TO P-UPP-PRARTSJK                                
095400     MOVE IN200-IDUSER   TO P-UPP-IDUSER                                  
095500                                                                          
095600     MOVE PACKA-UPP-KORT TO FEL-KORT                                      
095700     WRITE FEL-POST FROM FEL-AREA                                         
095800                                                                          
095900     ADD +1 TO TOT-RAKNARE-FELFIL                                         
096000     .                                                                    
096100     EJECT                                                                
096200 S30-SKRIV-300POST  SECTION.                                              
096300     SKIP2                                                                
096400     WRITE UT300-POST FROM WS30-AREA                                      
096500     ADD +1 TO TOT-RAKNARE-300                                            
096600     .                                                                    
096700                                                                          
096800 S31-SKRIV-310POST  SECTION.                                              
096900     SKIP2                                                                
097000     WRITE UT310-POST FROM WS31-AREA                                      
097100     ADD +1 TO TOT-RAKNARE-310                                            
097200     .                                                                    
097300                                                                          
097400 S32-SKRIV-320POST  SECTION.                                              
097500     SKIP2                                                                
097600     WRITE UT320-POST FROM WS32-AREA                                      
097700     ADD +1 TO TOT-RAKNARE-320                                            
097800     .                                                                    
097900     EJECT                                                                
098000                                                                          
098100 S33-SKRIV-330POST  SECTION.                                              
098200     SKIP2                                                                
098300     WRITE UT330-POST FROM WS33-AREA                                      
098400     ADD +1 TO TOT-RAKNARE-330                                            
098500     .                                                                    
098600                                                                          
098700 S34-SKRIV-340POST  SECTION.                                              
098800     SKIP2                                                                
098900     WRITE UT340-POST FROM WS34-AREA                                      
099000     ADD +1 TO TOT-RAKNARE-340                                            
099100     .                                                                    
099200                                                                          
099300 S35-SKRIV-350POST  SECTION.                                              
099400     SKIP2                                                                
099500     WRITE UT350-POST FROM WS35-AREA                                      
099600     ADD +1 TO TOT-RAKNARE-350                                            
099700     .                                                                    
099800     EJECT                                                                
099900                                                                          
100000 S98-FLYTTA-INAREA-TILL-WSAREA SECTION.                                   
100100     SKIP2                                                                
100200     IF IN300-IDPTYP = 300                                                
100300       MOVE IN300-IDPTYP   TO WS300-IDPTYP                                
100400       MOVE IN300-IDARTNR  TO WS300-IDARTNR                               
100500       MOVE IN300-DAFSGVV  TO WS300-DAFSGVV                               
100600       MOVE IN300-PRARTSJK TO WS300-PRARTSJK                              
100700       MOVE ZERO           TO IN300-IDPTYP                                
100800     ELSE                                                                 
100900       MOVE ZERO           TO WS300-IDPTYP                                
101000                              WS300-PRARTSJK                              
101100     END-IF                                                               
101200                                                                          
101300     IF IN310-IDPTYP = 310                                                
101400       MOVE IN310-IDPTYP      TO WS310-IDPTYP                             
101500       MOVE IN310-IDDISTR     TO WS310-IDDISTR                            
101600       MOVE IN310-SULEVANT    TO WS310-SULEVANT                           
101700       MOVE IN310-SUARTFSG    TO WS310-SUARTFSG                           
101800       MOVE IN310-SULEVANT-DO TO WS310-SULEVANT-DO                        
101900       MOVE IN310-SUARTFSG-DO TO WS310-SUARTFSG-DO                        
102000       MOVE ZERO              TO IN310-IDPTYP                             
102100     ELSE                                                                 
102200       MOVE ZERO    TO WS310-IDDISTR                                      
102300                       WS310-SULEVANT                                     
102400                       WS310-SUARTFSG                                     
102500                       WS310-SULEVANT-DO                                  
102600                       WS310-SUARTFSG-DO                                  
102700     END-IF                                                               
102800                                                                          
102900     IF IN320-IDPTYP = 320                                                
103000       MOVE IN320-IDPTYP       TO WS320-IDPTYP                            
103100       MOVE IN320-SULEVANT-RAB TO WS320-SULEVANT-RAB                      
103200       MOVE IN320-SUARTFSG-RAB TO WS320-SUARTFSG-RAB                      
103300       MOVE ZERO               TO IN320-IDPTYP                            
103400     ELSE                                                                 
103500       MOVE ZERO TO WS320-SULEVANT-RAB                                    
103600                    WS320-SUARTFSG-RAB                                    
103700     END-IF                                                               
103800                                                                          
103900     IF IN330-IDPTYP = 330                                                
104000       MOVE IN330-IDPTYP        TO WS330-IDPTYP                           
104100       MOVE IN330-SULEVANT-SPEC TO WS330-SULEVANT-SPEC                    
104200       MOVE IN330-SUARTFSG-SPEC TO WS330-SUARTFSG-SPEC                    
104300       MOVE ZERO                TO IN330-IDPTYP                           
104400     ELSE                                                                 
104500       MOVE ZERO TO WS330-SULEVANT-SPEC                                   
104600                    WS330-SUARTFSG-SPEC                                   
104700     END-IF                                                               
104800                                                                          
104900     IF IN340-IDPTYP = 340                                                
105000       MOVE IN340-IDPTYP       TO WS340-IDPTYP                            
105100       MOVE IN340-SULEVANT-MAN TO WS340-SULEVANT-MAN                      
105200       MOVE IN340-SUARTFSG-MAN TO WS340-SUARTFSG-MAN                      
105300       MOVE ZERO               TO IN340-IDPTYP                            
105400     ELSE                                                                 
105500       MOVE ZERO TO WS340-SULEVANT-MAN                                    
105600                    WS340-SUARTFSG-MAN                                    
105700     END-IF                                                               
105800                                                                          
105900     IF IN350-IDPTYP = 350                                                
106000       MOVE IN350-IDPTYP TO WS350-IDPTYP                                  
106100       MOVE IN350-SULEVANT-KRE TO WS350-SULEVANT-KRE                      
106200       MOVE IN350-SUARTFSG-KRE TO WS350-SUARTFSG-KRE                      
106300       MOVE ZERO TO IN350-IDPTYP                                          
106400     ELSE                                                                 
106500       MOVE ZERO TO WS350-SULEVANT-KRE                                    
106600                    WS350-SUARTFSG-KRE                                    
106700     END-IF                                                               
106800     .                                                                    
106900     EJECT                                                                
107000                                                                          
107100 S99-SKRIV-FIL-13  SECTION.                                               
107200     SKIP2                                                                
107300     IF WS300-IDPTYP = 300                                                
107400       PERFORM S30-SKRIV-300POST                                          
107500       MOVE ZERO TO WS300-IDPTYP                                          
107600     END-IF                                                               
107700                                                                          
107800     IF WS310-IDPTYP = 310                                                
107900       PERFORM S31-SKRIV-310POST                                          
108000       MOVE ZERO TO WS310-IDPTYP                                          
108100     END-IF                                                               
108200                                                                          
108300     IF WS320-IDPTYP = 320                                                
108400       PERFORM S32-SKRIV-320POST                                          
108500       MOVE ZERO TO WS320-IDPTYP                                          
108600     END-IF                                                               
108700                                                                          
108800     IF WS330-IDPTYP = 330                                                
108900       PERFORM S33-SKRIV-330POST                                          
109000       MOVE ZERO TO WS330-IDPTYP                                          
109100     END-IF                                                               
109200                                                                          
109300     IF WS340-IDPTYP = 340                                                
109400       PERFORM S34-SKRIV-340POST                                          
109500       MOVE ZERO TO WS340-IDPTYP                                          
109600     END-IF                                                               
109700                                                                          
109800     IF WS350-IDPTYP = 350                                                
109900       PERFORM S35-SKRIV-350POST                                          
110000       MOVE ZERO TO WS350-IDPTYP                                          
110100     END-IF                                                               
110200     .                                                                    
110300     EJECT                                                                
110400                                                                          
110410     EJECT                                                                

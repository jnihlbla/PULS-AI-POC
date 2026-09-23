000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2246300.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   13/10/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DETTA PROGRAM ÄR DELVIS EN KOPIA PÅ W2216A00 (W2218100).         
000900*        PROGRAMMET LÄSER EN LEVERANTÖR PÅ WDGX2206. MED DENNA            
001000*        SOM NYCKEL +DC HÄMTAS SAMTLIGA ARTIKLAR MED DENNA LEV.           
001100*        PÅ WDGX2248.                                                     
001200*        H-TYP-ROT  WDR5 ÄR BYTT MOT 2247+J.IN-PARAMETER TILLAGD          
001300*        I JCL'N OM DET ÄR VECKA ELLER PERIOD-KÖRNING.(VEC/PER).          
001400*                                                                         
001500*        2 UTFILER SKAPAS,EN FÖR ARTIKLAR SOM SKALL ÖVERFÖRAS             
001600*        VIA EDI OCH EN SOM SKAPAR DELETE POSTER FÖR W2246400             
001700*        SOM DELETAR WDGX2248 SEGMENT.                                    
001800*                                                                         
001900*        PROGRAMMET LÄSER      WDR2                                       
002000*        PROGRAMMET LÄSER      WDR5                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600*    2013-10-17  E-TRACKER: 10205391                                      
002700*                                                                         
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700*          --- PARAMETERKORT IN - SYSIN CARD FRÅN JCL                     
003800     SELECT PARMIN                     ASSIGN TO W22463D1.                
003900     SKIP2                                                                
004000*          --- ARTIKLAR TILL EDI                                          
004100     SELECT W22463                     ASSIGN TO W22463D2.                
004200     SKIP2                                                                
004300*          --- DELETE-POSTER WDGX2248                                     
004400     SELECT W22464                     ASSIGN TO W22463D3.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP2                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  PARMIN                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400 01  FILLER                 PIC X(80).                                    
005500     SKIP3                                                                
005600 FD  W22463                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  POST -COPY W2216A -PRE  UT63-  -L.                                   
006100     SKIP3                                                                
006200 FD  W22464                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600*01  POST -COPY W22464 -PRE  UT64-  -L.                                   
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900                                                                          
007000 77  IDPGM                       PIC X(8)    VALUE 'W2246300'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
007400 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
007600     EJECT                                                                
007610 01  DAGENS-DAGNR                PIC 9(1) VALUE ZERO.                     
007620                                                                          
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     EJECT                                                                
008210 01  FILLER                      PIC X(16)   VALUE 'WWDC99 '.             
008220*01  -COPY WWDC99                                                         
008230     EJECT                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400*                                                                         
008500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008810     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009010     EJECT                                                                
009020*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
009030                                                                          
009040*01  -COPY WDATAREA.                                                      
009050     EJECT                                                                
009100*    --- PARAMETRAR TILL ABEND                                            
009200                                                                          
009300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009600     SKIP2                                                                
009700 01  FELTEXT.                                                             
009800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL DATKORT                                          
010200*                                                                         
010300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22463'.              
010400     SKIP2                                                                
010500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010600     SKIP2                                                                
010700*01  -COPY WDATKORT                                                       
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL POSTSUM                                          
011000*                                                                         
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200     EJECT                                                                
011300*---------------------------------------------------- IN-FILER            
011400 01  PARM-AREA.                                                           
011500     03 KORTTYP                 PIC X(03)   VALUE SPACE.                  
011510     03  PARM-IDDC-FOM          PIC X(02)   VALUE SPACE.                  
011520     03  PARM-IDDC-TOM          PIC X(02)   VALUE SPACE.                  
011600     03 FILLER                  PIC X(73)   VALUE SPACE.                  
011700                                                                          
011800*---------------------------------------------------- UT-FILER            
011900 01  UT63-AREA-START             PIC X(24)   VALUE                        
012000                                 'UT63-AREA-START  '.                     
012100     SKIP2                                                                
012200                                                                          
012300*01  AREA -COPY W2216A     -PRE UT63-                                     
012400     EJECT                                                                
012500                                                                          
012600 01  UT64-AREA-START             PIC X(24)   VALUE                        
012700                                 'UT64-AREA-START  '.                     
012800     SKIP2                                                                
012900                                                                          
013000*01  AREA -COPY W22464     -PRE UT64-                                     
013100*----------------------------------------------------                     
013200     EJECT                                                                
013300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013400*                                                                         
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900     03  W-WDGXKEY-2205-X.                                                
014000         05  W-IDHTYP            PIC X(04)    VALUE '2205'.               
014100         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
014600                                                                          
014610     03  W-IDDC-MIN-X.                                                    
014620         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
014630                                                                          
014640     03  W-IDDC-MAX-X.                                                    
014650         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
014660                                                                          
014700     03  W-WDGXKEY-2247-X.                                                
014800         05  W-IDHTYP            PIC X(04)    VALUE '2247'.               
014900         05  W-FLLEVPLP          PIC X        VALUE 'J'.                  
015000         05  FILLER              PIC X(25)    VALUE LOW-VALUE.            
015100                                                                          
015200     03  W-KY2248-X.                                                      
015300         05  W-IDDC-2248         PIC X(02)    VALUE SPACE.                
015400         05  W-IDLEVNR-2248      PIC X(05)    VALUE SPACE.                
015500         05  W-IDARTNR-2248      PIC S9(9)    VALUE ZERO COMP-3.          
015600                                                                          
015700     03  W-KY2248-MIN-X.                                                  
015800         05  W-IDDC-2248-MIN     PIC X(2)     VALUE SPACE.                
015900         05  W-IDLEVNR-2248-MIN  PIC X(5)     VALUE SPACE.                
016000         05  W-IDARTNR-2248-MIN  PIC S9(9)    VALUE ZERO COMP-3.          
016100                                                                          
016200     03  W-KY2248-MAX-X.                                                  
016300         05  W-IDDC-2248-MAX     PIC X(2)     VALUE SPACE.                
016400         05  W-IDLEVNR-2248-MAX  PIC X(5)     VALUE SPACE.                
016500         05  W-IDARTNR-2248-MAX  PIC S9(9)                                
016600                                         VALUE +999999999 COMP-3.         
016700                                                                          
016800     SKIP2                                                                
016900*    --- STATUS-KOD FRÅN IMS                                              
017000 01  STATUS-WS                   PIC XX.                                  
017100     88  SEGMENT-FINNS                       VALUE '  '.                  
017200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017400     SKIP2                                                                
017500 01  GODK-STATUSKODER.                                                    
017600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017700     SKIP3                                                                
017800 01  SSA1                        PIC X(64).                               
017900 01  SSA2                        PIC X(64).                               
018000     EJECT                                                                
018100*    --- IMS FUNKTIONSKODER                                               
018200*01  -COPY W0003                                                          
018300     EJECT                                                                
018400*    ---  DLI INPUT-OUTPUT AREA                                           
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR201'.                      
018600 01  DLI-IO-WDR201.                                                       
018700*    03  -COPY WDGX01                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
019000 01  DLI-IO-WDGX2206.                                                     
019100*    03  -COPY WDGX2206                                                   
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
019400 01  DLI-IO-WDR501.                                                       
019500*    03  -COPY WDGX2247                                                   
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
019800 01  DLI-IO-WDGX2248.                                                     
019900*    03  -COPY WDGX2248                                                   
020000     EJECT                                                                
020100 LINKAGE SECTION.                                                         
020200                                                                          
020300                                                                          
020400*01  -COPY W0008  -PRE 2205-                                              
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008  -PRE 2247-                                              
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000 PROCEDURE DIVISION  USING 2205-PCB 2247-PCB.                             
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING 2205-PCB 2247-PCB.                             
021300                                                                          
021400                                                                          
021500     PERFORM A-INIT                                                       
021600                                                                          
021700     PERFORM IMS-GU-WDR201-2205                                           
021701                                                                          
021710     MOVE PARM-IDDC-FOM TO W-IDDC-MIN                                     
021720     MOVE PARM-IDDC-TOM TO W-IDDC-MAX                                     
021800     PERFORM IMS-GNP-WDGX2206                                             
021900                                                                          
022000     PERFORM UNTIL SEGMENT-SAKNAS                                         
022100        IF (KORTTYP      = 'VEC' AND                                      
022200            2206-FLLEVVB = JA)                                            
022220            MOVE 2206-IDDC            TO WS-IDDC                          
022273                                                                          
022274            IF (2206-KDVECKOSL = 'D')                                     
022275           AND ((NDC-CN AND DAGENS-DAGNR = 1) OR                          
022276                (NDC-US AND DAGENS-DAGNR = 2))                            
022277               PERFORM B-LAES-WDGX2248-SKRIV-UTPOST                       
022280            ELSE                                                          
022281               IF (2206-KDVECKOSL= 'V')                                   
022282              AND ((NDC-CN AND DAGENS-DAGNR = 4) OR                       
022283                   (NDC-US AND DAGENS-DAGNR = 5))                         
022284                 PERFORM B-LAES-WDGX2248-SKRIV-UTPOST                     
022288               END-IF                                                     
022289            END-IF                                                        
022290        END-IF                                                            
022300                                                                          
022400        IF (KORTTYP      = 'PER' AND                                      
022500            2206-FLLEVPLP = JA)                                           
022700              PERFORM B-LAES-WDGX2248-SKRIV-UTPOST                        
022900        END-IF                                                            
023000                                                                          
023100        PERFORM IMS-GNP-WDGX2206                                          
023200     END-PERFORM                                                          
023300                                                                          
023400                                                                          
023500     PERFORM Z-FINIT                                                      
023600                                                                          
023700     MOVE ZERO TO RETURN-CODE                                             
023800     GOBACK                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200     MOVE 'A-INIT '  TO CURRENT-SECTION                                   
024300                                                                          
024400     OPEN INPUT  PARMIN                                                   
024500                                                                          
024600     OPEN OUTPUT W22463                                                   
024700                 W22464                                                   
024800                                                                          
024810     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
024820     CALL WDATKONV USING DAT-KDDATFORM                                    
024830                         DAT-I-TIDATUM                                    
024840                         DAT-O-TIDATUM                                    
024850                         DAT-KDSVAR                                       
024860                                                                          
024870     MOVE DAT-TID     TO DAGENS-DAGNR                                     
024880                                                                          
024900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025000     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
025100     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
025200     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
025300                                                                          
025400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025500                                                                          
025600     PERFORM S01-LAES-PARMIN                                              
025700     .                                                                    
025800     EJECT                                                                
025900 B-LAES-WDGX2248-SKRIV-UTPOST  SECTION.                                   
026000     MOVE 'B-LAES-WDGX2248-SKRIV-UTPOST '  TO CURRENT-SECTION             
026100                                                                          
026200     MOVE 2206-IDDC             TO W-IDDC-2248-MIN                        
026300                                   W-IDDC-2248-MAX                        
026400     MOVE 2206-IDLEVNR          TO W-IDLEVNR-2248-MIN                     
026500                                   W-IDLEVNR-2248-MAX                     
026610     MOVE SPACE                 TO UT64-IDDC                              
026620     MOVE SPACE                 TO UT64-IDLEVNR                           
026700                                                                          
026800     PERFORM IMS-GU-WDR501-2247                                           
026900     PERFORM IMS-GNP-WDGX2248-MIN-MAX                                     
027000     PERFORM UNTIL SEGMENT-SAKNAS                                         
027100                                                                          
027200        IF 2206-KDEDI = 'O' OR 'F' OR 'E'                                 
027300          MOVE 2248-IDDC     TO UT63-IDDC                                 
027400          MOVE 2248-IDLEVNR  TO UT63-IDLEVNR                              
027500          MOVE 2248-IDARTNR  TO UT63-IDARTNR                              
027600          MOVE 2248-KVDAGAR  TO UT63-KVDAGAR                              
027700          MOVE 2248-KVBEART  TO UT63-KVBEART                              
027800                                                                          
027900          PERFORM S11-SKRIV-W22463                                        
028000        END-IF                                                            
028100                                                                          
028200        PERFORM BA-DELETE-WDGX2248                                        
028400        PERFORM IMS-GNP-WDGX2248-MIN-MAX                                  
028500     END-PERFORM                                                          
028600                                                                          
028700     .                                                                    
028800     EJECT                                                                
028900 BA-DELETE-WDGX2248   SECTION.                                            
029000     MOVE 'BA-DELETE-WDGX2248 '  TO CURRENT-SECTION                       
029100                                                                          
029200*--- DELETE/REPLACE SKER I BMP W2246400.                                  
029300                                                                          
029500     IF 2206-IDLEVNR  = UT64-IDLEVNR AND                                  
029600        2206-IDDC     = UT64-IDDC                                         
029700                                                                          
029800        MOVE ZERO     TO UT64-IDOVERFNR                                   
029900     ELSE                                                                 
030000       MOVE 2206-IDOVERFNR  TO UT64-IDOVERFNR                             
030100       ADD +1               TO UT64-IDOVERFNR                             
030200     END-IF                                                               
030600                                                                          
030700     MOVE 2248-IDDC     TO UT64-IDDC                                      
030800     MOVE 2248-IDLEVNR  TO UT64-IDLEVNR                                   
030900     MOVE 2248-IDARTNR  TO UT64-IDARTNR                                   
031000                                                                          
031100     PERFORM S12-SKRIV-W22464                                             
031200                                                                          
031300     .                                                                    
031400     EJECT                                                                
031500 Z-FINIT SECTION.                                                         
031600     CLOSE PARMIN                                                         
031610           W22463                                                         
031700           W22464                                                         
031800     SKIP2                                                                
031900     MOVE 'S' TO POSTSUM-OPKOD                                            
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
032200     EJECT                                                                
032300 S01-LAES-PARMIN  SECTION.                                                
032400     MOVE 'S01-LAES-PARMIN '  TO CURRENT-SECTION                          
032500                                                                          
032600*--- LÄS DATAKORT FRÅN JCL OM DET ÄR VECKO- ELLER PERIODKÖRNING           
032700                                                                          
032800     READ PARMIN  INTO PARM-AREA                                          
032900                                                                          
033000     AT END                                                               
033100        MOVE '***   PARM SAKNAS   ***'      TO FELTEXT-STR                
033200        PERFORM S99-ABEND                                                 
033300     NOT AT END                                                           
033400        IF KORTTYP NOT = 'VEC' AND                                        
033500           KORTTYP NOT = 'PER'                                            
033600           MOVE  '***   PARM FELAKTIG ***'  TO FELTEXT-STR                
033700           PERFORM S99-ABEND                                              
033800        END-IF                                                            
033900                                                                          
034000        MOVE 'PARM'                       TO POSTSUM-TRANSTYP             
034100        MOVE 'W22463'                     TO POSTSUM-FDNAMN               
034200        MOVE 'W22463D1'                   TO POSTSUM-DDNAMN2              
034300        CALL POSTSUM USING POSTSUM-PARM                                   
034400     END-READ                                                             
034500                                                                          
034600     .                                                                    
034700     EJECT                                                                
034800 S11-SKRIV-W22463 SECTION.                                                
034900     MOVE 'S11-SKRIV-W22463  '  TO CURRENT-SECTION                        
035000                                                                          
035100     WRITE UT63-POST FROM UT63-AREA                                       
035200                                                                          
035300     MOVE 'EDI'       TO POSTSUM-TRANSTYP                                 
035400     MOVE 'W22463' TO POSTSUM-FDNAMN                                      
035500     MOVE 'W22463D2' TO POSTSUM-DDNAMN2                                   
035600     CALL POSTSUM USING POSTSUM-PARM                                      
035700     .                                                                    
035800     EJECT                                                                
035900 S12-SKRIV-W22464 SECTION.                                                
036000     MOVE 'S12-SKRIV-W22464  '  TO CURRENT-SECTION                        
036100                                                                          
036200     WRITE UT64-POST FROM UT64-AREA                                       
036300                                                                          
036400     MOVE 'DLET'      TO POSTSUM-TRANSTYP                                 
036500     MOVE 'W22464' TO POSTSUM-FDNAMN                                      
036600     MOVE 'W22463D3' TO POSTSUM-DDNAMN2                                   
036700     CALL POSTSUM USING POSTSUM-PARM                                      
036800     .                                                                    
036900     EJECT                                                                
037000 S99-ABEND SECTION.                                                       
037100     MOVE 'S99-ABEND '  TO CURRENT-SECTION                                
037200                                                                          
037300     DISPLAY FELTEXT                                                      
037400     DISPLAY '* *'                                                        
037500     DISPLAY '* *  PROGRAM ABEND'                                         
037600     DISPLAY '* *'                                                        
037700     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * * * '           
037800     DISPLAY '* *'                                                        
037900                                                                          
038000     SKIP2                                                                
038100     MOVE 'S' TO POSTSUM-OPKOD                                            
038200     CALL POSTSUM USING POSTSUM-PARM                                      
038300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
038400     .                                                                    
038500     EJECT                                                                
038600* --- IMS SEKTIONER ---                                                   
038700                                                                          
038800     EJECT                                                                
038900 IMS-GU-WDR201-2205 SECTION.                                              
039000     MOVE 'IMS-GU-WDR201-2205 '     TO DBS-SECTION                        
039100                                                                          
039200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
039300          DELIMITED BY SIZE INTO SSA1                                     
039400     MOVE '  '   TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GU 2205-PCB DLI-IO-WDR201   SSA1                  
039600     MOVE 2205-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000 IMS-GNP-WDGX2206   SECTION.                                              
040100     MOVE 'IMS-GNP-WDGX2206  '      TO DBS-SECTION                        
040200                                                                          
040300     STRING 'WDGX2206(IDDC    >=' W-IDDC-MIN-X                            
040310                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
040320          DELIMITED BY SIZE INTO SSA1                                     
040400     MOVE '  GE'      TO GODK-STATUSKODER                                 
040500     CALL CBLTDLI USING GNP 2205-PCB DLI-IO-WDGX2206 SSA1                 
040600     MOVE 2205-STATUS-CODE TO STATUS-WS                                   
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     EJECT                                                                
041000 IMS-GU-WDR501-2247 SECTION.                                              
041100     MOVE 'IMS-GU-WDR501-2247 '   TO DBS-SECTION                          
041200                                                                          
041300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X ')'                    
041400          DELIMITED BY SIZE INTO SSA1                                     
041500     MOVE '  '   TO GODK-STATUSKODER                                      
041600     CALL CBLTDLI USING GU 2247-PCB DLI-IO-WDR501 SSA1                    
041700     MOVE 2247-STATUS-CODE TO STATUS-WS                                   
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000     EJECT                                                                
042100 IMS-GNP-WDGX2248-MIN-MAX  SECTION.                                       
042200     MOVE 'IMS-GNP-WDGX2248-MIN-MAX '  TO DBS-SECTION                     
042300                                                                          
042400     STRING 'WDGX2248(KY2248  >=' W-KY2248-MIN-X                          
042500                    '&KY2248  <=' W-KY2248-MAX-X ')'                      
042600          DELIMITED BY SIZE INTO SSA1                                     
042700     MOVE '  GE' TO GODK-STATUSKODER                                      
042800     CALL CBLTDLI USING GNP 2247-PCB DLI-IO-WDGX2248 SSA1                 
042900     MOVE 2247-STATUS-CODE TO STATUS-WS                                   
043000     PERFORM IMS-STATUSKONTROLL                                           
043100     .                                                                    
043200     EJECT                                                                
043300 IMS-STATUSKONTROLL SECTION.                                              
043400                                                                          
043500     SET STATUS-IX TO 1                                                   
043600     SEARCH GODK-STATUS                                                   
043700       AT END                                                             
043800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
043900           DELIMITED BY SIZE INTO FELTEXT                                 
044000         DISPLAY FELTEXT                                                  
044100         CALL FELLOG                                                      
044200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044300         CONTINUE                                                         
044400     END-SEARCH                                                           
044500     .                                                                    

000100 ID DIVISION.                                                             
000200     SKIP3                                                                
000300 PROGRAM-ID.             W1145400.                                        
000400 AUTHOR.                 PAH  KOPIA W11450                                
000500     DATE-WRITTEN.       APR  2006.                                       
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            IMS BMP/BATCH HUVUDPROGRAM.                                  
001100*                                                                         
001200*            TÖMNING  NYPON - SAP  NAP                                    
001300*                                                                         
001400*            PROGRAMMET STYRS AV HÄNDELSETRANSAR (WDGX1142) SOM           
001500*            BETAS AV.                                                    
001600*            PLOCKAR INFO FRÅN ARTIKELREGISTREN (WDK6, WDD2)              
001700*                              WDD3 (BENÄMNING).                          
001800*                                                                         
001900***---------------------------------------------------------------        
002000*** PROGRAMÄNDRINGAR                                                      
002100***---------------------------------------------------------------        
002200*            E-TRACKER 10249768 SKICKA BRANDON PG25 TILL SRM(NAP)         
002300*            E-TRACKER 10277114 NEW MAIL ADDRESSES                        
002400*                                                                         
002500*                                                                         
002600*                                                                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP3                                                                
003400* UTFILER:                                                                
003500*                        TRANSAR   TILL NAP                               
003600     SELECT  W11454      ASSIGN    UT-S-W11454D2.                         
003700*                        MAIL DATA TILL NAP                               
003800     SELECT  W11455      ASSIGN    UT-S-W11454D3.                         
003900*                        MAIL ADR  TILL NAP                               
004000     SELECT  W11456      ASSIGN    UT-S-W11454D4.                         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W11454                                                               
004700     LABEL RECORD STANDARD                                                
004800     RECORDING      f                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP3                                                                
005100*01  UTPOST        -COPY NAPUT    -L                                      
005200     SKIP3                                                                
005300 FD  W11455                                                               
005400     LABEL RECORD STANDARD                                                
005500     RECORDING      F                                                     
005600     BLOCK CONTAINS 0.                                                    
005700     SKIP3                                                                
005800*01  UTPOST2       -COPY W11455   -L                                      
005900     SKIP3                                                                
006000 FD  W11456                                                               
006100     LABEL RECORD STANDARD                                                
006200     RECORDING      F                                                     
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP3                                                                
006500 01  UT3-POST          PIC X(80).                                         
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900*    -COPY WY2000W1                                                       
007000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1145400'.               
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  IX                          PIC 9(3)    VALUE ZERO.                  
007400 77  IX-MAX                      PIC 9(3)    VALUE ZERO.                  
007500 77  KOLL-IX                     PIC 9(3)    VALUE ZERO.                  
007600 77  RAEKN                       PIC S9(3)   VALUE +0    COMP-3.          
007700 77  WS-KVPROG                   PIC 9(9)    VALUE ZERO.                  
007800 77  WS-IDHANDLR                 PIC 9(3)    VALUE ZERO.                  
007900 77  SW-TRAFF                    PIC X       VALUE 'N'.                   
008000                                                                          
008100 01  ARTIKELNR.                                                           
008200     03 BLANKA-X                 PIC X(11) VALUE SPACE.                   
008300     03 ARTNR-ALFA               PIC X(9).                                
008400     03 ARTNR-NUM REDEFINES ARTNR-ALFA PIC 9(9).                          
008500                                                                          
008600 01  WS-IDUSER.                                                           
008700     03 WS-IDMAIL                PIC X(9).                                
008800     03 FILLER                   PIC X(51).                               
008900                                                                          
009000 01  KOLL-IDUSER                 PIC X(9).                                
009100 01  FILLER REDEFINES KOLL-IDUSER.                                        
009200     03  KOLL-TKN                PIC X  OCCURS 9.                         
009300                                                                          
009400 01  MAIL-RAD.                                                            
009500     03 FILLER                   PIC X(5) VALUE 'DEST '.                  
009600     03 MAIL-ADRESS              PIC X(60).                               
009700                                                                          
009800 01  SPAR-AREA.                                                           
009900     03  SPAR-ARTC-IDANSK        PIC 9(3).                                
010000     03  SPAR-ARTC-IDLEVNR       PIC X(5).                                
010100     03  SPAR-ARTC-KDHF          PIC S9                  COMP-3.          
010200     03  SPAR-ARTC-IDINK         PIC 9(3).                                
010300     03  SPAR-ARTG-IDINK         PIC 9(3).                                
010400     03  SPAR-KDSORT             PIC X(2).                                
010500     03  SPAR-KDPRODSL           PIC 9(2).                                
010600     03  SPAR-IDFKNGRP           PIC 9(4).                                
010700     03  SPAR-IDINK              PIC X(3).                                
010710     03  SPAR-IDPROJ             PIC X(4).                                
010800     EJECT                                                                
010900*01   -COPY WWKONLEV                                                      
011000     EJECT                                                                
011100*01   -COPY WWPRODSL                                                      
011200     EJECT                                                                
011300 01  DAGENS-DATUM.                                                        
011400     03  AAMMDD                  PIC 9(6)    VALUE ZERO.                  
011500     03  AAVV                    PIC 9(4)    VALUE ZERO.                  
011600     03  FILLER    REDEFINES AAVV.                                        
011700         05  AA                  PIC 9(2).                                
011800         05  VV                  PIC 9(2).                                
011900     SKIP3                                                                
012000 01  W-ANSK-TEL.                                                          
012100     03  W-IDANSK                PIC 9(3).                                
012200     03  FILLER                  PIC X.                                   
012300     03  W-EXTTEL.                                                        
012400         05  W-IDTFN             PIC X(16).                               
012500     EJECT                                                                
012600*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
012700     SKIP3                                                                
012800 01  DYNAMISKA-SUBPROGRAM.                                                
012900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
013200   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
013300   03  W980SOP                   PIC X(8)    VALUE 'W980SOP '.            
013400     EJECT                                                                
013500*    ---- PARAMETRAR TILL W980SOP                                         
013600                                                                          
013700*01  -COPY WSOPAREA.                                                      
013800     EJECT                                                                
013900*    ---- PARAMETRAR TILL WDATKONV                                        
014000                                                                          
014100*01  -COPY WDATAREA.                                                      
014200     EJECT                                                                
014300*    ---- PARAMETRAR TILL POSTSUM                                         
014400                                                                          
014500*01  -COPY W0005       -PRE POSTSUM-.                                     
014600     EJECT                                                                
014700*01  AREA     -COPY NAPUT      -PRE UT-                                   
014800     EJECT                                                                
014900*01  AREA     -COPY W11455     -PRE UT2-                                  
015000     EJECT                                                                
015100 01  UT3-AREA                  PIC X(80).                                 
015200     EJECT                                                                
015300*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
015400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015500     SKIP3                                                                
015600*    ---- STATUSKOD FRÅN IMS                                              
015700 01  STATUS-WS                   PIC XX.                                  
015800     88  SEGMENT-FINNS                       VALUE '  '.                  
015900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016000     88  SEGMENT-SLUT                        VALUE 'GE'.                  
016100     88  IMS-EJ-OK                           VALUE 'XD'.                  
016200     SKIP3                                                                
016300 01  GODK-STATUSKODER.                                                    
016400   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
016500     SKIP3                                                                
016600 01  SSA1                        PIC X(64).                               
016700 01  SSA2                        PIC X(64).                               
016800     EJECT                                                                
016900*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
017000 01  NYCKLAR-TILL-DLI.                                                    
017100   03  W-IDARTNR-X.                                                       
017200     05  W-IDARTNR               PIC S9(9)                COMP-3.         
017300   03  W-WDGXKEY-1141.                                                    
017400     05  FILLER                  PIC X(4)     VALUE '1141'.               
017500     05  FILLER                  PIC X(26)    VALUE LOW-VALUE.            
017600   03  W-KDARBTYP-X.                                                      
017700     05  W-KDARBTYP              PIC X(08)    VALUE 'INK '.               
017800   03  W-IDPERSON-X.                                                      
017900     05  W-IDPERSON              PIC S9(3)    VALUE ZERO COMP-3.          
018000 01  W-IDSKYLT-X.                                                         
018100     03  W-IDSKYLT               PIC  X(3)   VALUE 'GB '.                 
018200     EJECT                                                                
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500 01  DLI-IO-AREA.                                                         
018600   03 IO-AREA                    PIC X(900)  VALUE SPACE.                 
018700     SKIP3                                                                
018800*  03  WLXXAV11 -COPY WDGX1142 -PRE HTR-  -RED IO-AREA.                   
018900     EJECT                                                                
019000*  03  WLARTC01 -COPY WDK601              -RED IO-AREA.                   
019100     EJECT                                                                
019200*  03  WLARTC11 -COPY WDK611              -RED IO-AREA.                   
019300     EJECT                                                                
019400 01  DLI-IO-AREA-2.                                                       
019500   03 IO-AREA-2                  PIC X(600)  VALUE SPACE.                 
019600     SKIP3                                                                
019700*  03  WLARTG01 -COPY WDD201 -PRE ARTG- -RED IO-AREA-2.                   
019800     EJECT                                                                
019900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
020000 01  DLI-IO-WDD301.                                                       
020100*    03  -COPY WDD301                                                     
020200     EJECT                                                                
020300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
020400 01  DLI-IO-WDD311.                                                       
020500*    03  -COPY WDD311                                                     
020600     EJECT                                                                
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
020800 01  DLI-IO-WDP311.                                                       
020900*    03  -COPY WDP311                                                     
021000     EJECT                                                                
021100 LINKAGE SECTION.                                                         
021200     SKIP3                                                                
021300*01      -COPY W0009     -PRE MSG-                                        
021400     EJECT                                                                
021500*01      -COPY W0008     -PRE HTR-                                        
021600      05 FILLER          PIC X.                                           
021700     EJECT                                                                
021800*01      -COPY W0008     -PRE ARTC-                                       
021900      05 FILLER          PIC X.                                           
022000     EJECT                                                                
022100*01      -COPY W0008     -PRE ARTG-                                       
022200      05 FILLER          PIC X.                                           
022300     EJECT                                                                
022400*01      -COPY W0008     -PRE WDD3-                                       
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01      -COPY W0008     -PRE WDP3-                                       
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000 PROCEDURE DIVISION USING MSG-PCB HTR-PCB ARTC-PCB                        
023100                        ARTG-PCB WDD3-PCB WDP3-PCB.                       
023200     ENTRY 'DLITCBL' USING MSG-PCB HTR-PCB ARTC-PCB                       
023300                        ARTG-PCB WDD3-PCB WDP3-PCB.                       
023400                                                                          
023500     PERFORM A-INIT                                                       
023600     PERFORM IMS-GU-HTR-1141-ROT                                          
023700     PERFORM IMS-GHNP-HTR-1142                                            
023800                                                                          
023900     PERFORM UNTIL SEGMENT-SLUT OR RAEKN > +499                           
024000                                                                          
024100        MOVE HTR-1142-IDARTNR TO W-IDARTNR                                
024200        PERFORM IMS-GU-ARTG-ROT                                           
024300                                                                          
024400        IF SEGMENT-FINNS                                                  
024500           PERFORM IMS-GU-ARTC-ROT                                        
024600           IF SEGMENT-FINNS                                               
024700              MOVE ART-KDSORT   TO SPAR-KDSORT                            
024800              MOVE ART-KDPRODSL TO SPAR-KDPRODSL                          
024900              MOVE ART-IDFKNGRP TO SPAR-IDFKNGRP                          
025000              PERFORM IMS-GNP-ARTC11                                      
025100              IF SEGMENT-FINNS                                            
025200                 MOVE CLAG-IDINK  TO SPAR-IDINK                           
025300                                     SPAR-ARTC-IDINK                      
025310                 MOVE CLAG-IDPROJ TO SPAR-IDPROJ                          
025400              END-IF                                                      
025500           END-IF                                                         
025600                                                                          
025700           PERFORM AB-KOLLA-IDINK                                         
025800                                                                          
025900           PERFORM B-SKAPA-SKRIV-UTPOST                                   
026000        ELSE                                                              
026100           DISPLAY ' *** FINNS EJ PÅ NYPON : ' W-IDARTNR                  
026200        END-IF                                                            
026300                                                                          
026400        PERFORM IMS-GHNP-HTR-1142                                         
026500     END-PERFORM                                                          
026600                                                                          
026700     PERFORM Z-FINIT                                                      
026800                                                                          
026900     IF SEGMENT-SLUT                                                      
027000        CONTINUE                                                          
027100     ELSE                                                                 
027200        PERFORM ZZ-STARTA-NYTT-JOBB-VIA-SOP                               
027300     END-IF                                                               
027400                                                                          
027500     MOVE ZERO TO RETURN-CODE                                             
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 A-INIT SECTION.                                                          
028000                                                                          
028100     MOVE ZERO    TO RAEKN                                                
028200     MOVE SPACE   TO UT2-AREA                                             
028300                                                                          
028400     MOVE 'IDAG'  TO DAT-KDDATFORM                                        
028500     MOVE  ZERO   TO DAT-I-TIDATUM                                        
028600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
028700                         DAT-O-TIDATUM DAT-KDSVAR                         
028800     IF DAT-KDSVAR-OK                                                     
028900       MOVE DAT-TIAAMMDD    TO  AAMMDD                                    
029000       MOVE DAT-TIAA-VECKA  TO  AA                                        
029100       MOVE DAT-TIVV        TO  VV                                        
029200     END-IF                                                               
029300                                                                          
029400     OPEN OUTPUT W11454 W11455 W11456                                     
029500     MOVE 'W11454'   TO  POSTSUM-PROGNAMN                                 
029600                         POSTSUM-FDNAMN                                   
029700     MOVE 'W11454D2' TO  POSTSUM-DDNAMN2                                  
029800     .                                                                    
029900     EJECT                                                                
030000 AB-KOLLA-IDINK SECTION.                                                  
030100                                                                          
030200     IF CLAG-IDINK (1:3) NUMERIC                                          
030300        MOVE CLAG-IDINK (1:3)    TO SPAR-ARTC-IDINK                       
030400     ELSE                                                                 
030500        IF CLAG-IDINK (2:3) NUMERIC                                       
030600           MOVE CLAG-IDINK (2:3) TO SPAR-ARTC-IDINK                       
030700        ELSE                                                              
030800            MOVE ZERO             TO SPAR-ARTC-IDINK                      
030900        END-IF                                                            
031000     END-IF                                                               
031100                                                                          
031200     IF ARTG-ART-IDINK (1:3) NUMERIC                                      
031300        MOVE ARTG-ART-IDINK (1:3) TO SPAR-ARTG-IDINK                      
031400     ELSE                                                                 
031500        IF ARTG-ART-IDINK (2:3) NUMERIC                                   
031600           MOVE ARTG-ART-IDINK (2:3) TO SPAR-ARTG-IDINK                   
031700        ELSE                                                              
031800            MOVE ZERO             TO SPAR-ARTG-IDINK                      
031900        END-IF                                                            
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 B-SKAPA-SKRIV-UTPOST SECTION.                                            
032400                                                                          
032500     MOVE SPACE             TO UT-NAP-COMMON-AREA                         
032600     MOVE 'PULS'            TO UT-NAP-IDRT                                
032700     MOVE ZERO              TO UT-NAP-ZERO                                
032800     MOVE W-IDARTNR         TO UT-NAP-IDARTNR                             
032900     INSPECT UT-NAP-MATNR REPLACING LEADING ZEROES BY SPACE               
032910     IF SPAR-KDPRODSL = 18                                                
033000       MOVE 'HALB'          TO UT-NAP-MTART                               
033001     ELSE                                                                 
033010       MOVE 'NLAG'          TO UT-NAP-MTART                               
033020     END-IF                                                               
033100     MOVE 'N'               TO UT-NAP-MBRSH                               
033200*******                                                                   
033300     IF SPAR-KDSORT = 'ST' OR 'SA' OR 'SW' OR 'TM' OR 'HW'                
033400                       OR 'PA'                                            
033500        MOVE 'PCE'          TO UT-NAP-MEINS                               
033600     ELSE                                                                 
033700        IF SPAR-KDSORT = 'MM'                                             
033800           MOVE 'MMT'       TO UT-NAP-MEINS                               
033900        END-IF                                                            
034000        IF SPAR-KDSORT = 'M'                                              
034100           MOVE 'MTR'       TO UT-NAP-MEINS                               
034200        END-IF                                                            
034300        IF SPAR-KDSORT = 'M2'                                             
034400           MOVE 'MTK'       TO UT-NAP-MEINS                               
034500        END-IF                                                            
034600        IF SPAR-KDSORT = 'M3'                                             
034700           MOVE 'MTQ'       TO UT-NAP-MEINS                               
034800        END-IF                                                            
034900        IF SPAR-KDSORT = 'ML'                                             
035000           MOVE 'MLT'       TO UT-NAP-MEINS                               
035100        END-IF                                                            
035200        IF SPAR-KDSORT = 'L'                                              
035300           MOVE 'LTR'       TO UT-NAP-MEINS                               
035400        END-IF                                                            
035500        IF SPAR-KDSORT = 'G'                                              
035600           MOVE 'GRM'       TO UT-NAP-MEINS                               
035700        END-IF                                                            
035800        IF SPAR-KDSORT = 'KG'                                             
035900           MOVE 'KGM'       TO UT-NAP-MEINS                               
036000        END-IF                                                            
036100        IF SPAR-KDSORT = 'C2'                                             
036200           MOVE 'C2 '       TO UT-NAP-MEINS                               
036300        END-IF                                                            
036400     END-IF                                                               
036500*******                                                                   
036600     MOVE 'KG'              TO UT-NAP-GEWEI                               
036700                                                                          
036800     MOVE 'GB'              TO W-IDSKYLT                                  
036900     PERFORM IMS-GU-D311                                                  
037000     IF SEGMENT-FINNS and text-beart not = space                          
037100        MOVE TEXT-BEART     TO UT-NAP-MAKTX                               
037200     ELSE                                                                 
037300        MOVE 'S'            TO W-IDSKYLT                                  
037400        PERFORM IMS-GU-D311                                               
037500        IF SEGMENT-FINNS AND TEXT-BEART NOT = SPACE                       
037600           MOVE TEXT-BEART  TO UT-NAP-MAKTX                               
037700        ELSE                                                              
037800           MOVE 'missing'   TO UT-NAP-MAKTX                               
037900        END-IF                                                            
038000     END-IF                                                               
038100     MOVE 'EN'              TO UT-NAP-SPRAS-ISO                           
038200                                                                          
038300     MOVE 'BP2TW'           TO UT-NAP-WERKS                               
038400     MOVE 'ND'              TO UT-NAP-DISMM                               
038500**   IDINK = 4XX                                                          
038600     MOVE SPAR-KDPRODSL     TO TEST-KDPRODSL                              
038700     IF KDPRODSL-VOLVO-EMB                                                
038800        MOVE 'EN00'         TO UT-NAP-MATKL                               
038900     ELSE                                                                 
039000        IF KDPRODSL-TOOLS                                                 
039100           MOVE '01  '      TO UT-NAP-MATKL                               
039200        ELSE                                                              
039300           IF KDPRODSL-CHEMICAL                                           
039400              MOVE 'EM01'   TO UT-NAP-MATKL                               
039500           END-IF                                                         
039600        END-IF                                                            
039700     END-IF                                                               
039800     IF SPAR-KDPRODSL = 19 AND SPAR-IDFKNGRP = 1983                       
039900        MOVE 'EM01'         TO UT-NAP-MATKL                               
040000     END-IF                                                               
040100     IF KDPRODSL-BRANDON                                                  
040200        MOVE 'CX04'         TO UT-NAP-MATKL                               
040300     END-IF                                                               
040400**   IDINK = 4XX                                                          
040500     MOVE SPACE             TO UT-NAP-MFRN                                
040600     MOVE SPACE             TO UT-NAP-MFRPN                               
040700     MOVE 'S'               TO UT-NAP-VPRSV                               
040701     MOVE SPACE             TO UT-NAP-ZEINR                               
040702                                                                          
040703*SAP DEMAND, INFO ON PROJECT FOR TOOLS. (DM ORDERING)                     
040710     IF SPAR-KDPRODSL = 18                                                
040720       MOVE SPAR-IDPROJ     TO UT-NAP-ZEINR                               
040810     END-IF                                                               
040820                                                                          
040900     MOVE SPACE             TO UT-NAP-TDLINE                              
041000     MOVE 'EN'              TO UT-NAP-SPRAS-ISO-X                         
041100                                                                          
041200     IF UT2-AREA   = SPACE                                                
041300        PERFORM S03-SKRIV-UTPOST3                                         
041400     END-IF                                                               
041500                                                                          
041600     MOVE SPACE              TO UT2-AREA                                  
041700     MOVE W-IDARTNR          TO UT2-IDARTNR                               
041800     MOVE ARTG-ART-KVPROG    TO UT2-KVPROG                                
041900     MOVE UT-NAP-MAKTX(1:25) TO UT2-BEART                                 
042000                                                                          
042100     PERFORM S01-SKRIV-UTPOST                                             
042101                                                                          
042102*--- TOOLS SKALL EJ SKICKAS PÅ MAIL TILL INKÖP, SE NP W114SA              
042110     IF SPAR-KDPRODSL = 18                                                
042120       CONTINUE                                                           
042130     ELSE                                                                 
042200       PERFORM S02-SKRIV-UTPOST2                                          
042210     END-IF                                                               
042300****                                                                      
042400     PERFORM IMS-DLET-HTR-1142                                            
042500     ADD +1    TO RAEKN                                                   
042600     .                                                                    
042700     EJECT                                                                
042800 Z-FINIT   SECTION.                                                       
042900                                                                          
043000     CLOSE W11454 W11455 W11456                                           
043100                                                                          
043200     MOVE 'S' TO POSTSUM-OPKOD                                            
043300     CALL POSTSUM USING POSTSUM-PARM                                      
043400     .                                                                    
043500     EJECT                                                                
043600 ZZ-STARTA-NYTT-JOBB-VIA-SOP SECTION.                                     
043700                                                                          
043800     MOVE   SPACE   TO  SOP-DDPREFIX                                      
043900     MOVE    'O'    TO  SOP-SOPFUNC                                       
044000     MOVE  'W114D6' TO  SOP-PROC-NAME                                     
044100     MOVE   ZERO    TO  SOP-ACTPASS-DATE                                  
044200                                                                          
044300     CALL W980SOP USING SOP-PARM-AREA                                     
044400                                                                          
044500     IF SOP-RETCODE > +8                                                  
044600       CALL FELLOG                                                        
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 S01-SKRIV-UTPOST SECTION.                                                
045100                                                                          
045200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
045300     MOVE 'W11454D2' TO POSTSUM-DDNAMN2                                   
045400     CALL POSTSUM USING POSTSUM-PARM                                      
045500     WRITE UTPOST FROM ut-area                                            
045600     .                                                                    
045700     SKIP3                                                                
045800 S02-SKRIV-UTPOST2 SECTION.                                               
045900                                                                          
046000     MOVE 'MAIL'     TO POSTSUM-TRANSTYP                                  
046100     MOVE 'W11454D3' TO POSTSUM-DDNAMN2                                   
046200     CALL POSTSUM USING POSTSUM-PARM                                      
046300     WRITE UTPOST2 FROM UT2-AREA                                          
046400     .                                                                    
046500     SKIP3                                                                
046600 S03-SKRIV-UTPOST3 SECTION.                                               
046700                                                                          
046800     MOVE 'ADR '     TO POSTSUM-TRANSTYP                                  
046900     MOVE 'W11454D4' TO POSTSUM-DDNAMN2                                   
047000     CALL POSTSUM USING POSTSUM-PARM                                      
047100                                                                          
047200     MOVE ')SEND'                TO UT3-AREA                              
047300     WRITE UT3-POST FROM UT3-AREA                                         
047400     MOVE 'TITLE NYA ART. NAP        ' TO UT3-AREA                        
047500     WRITE UT3-POST FROM UT3-AREA                                         
047600     MOVE 'OPTION FORCE'         TO UT3-AREA                              
047700     WRITE UT3-POST FROM UT3-AREA                                         
047800     MOVE 'WSYST@VOLVOCARS.COM ' TO MAIL-ADRESS                           
047900     MOVE 'INK '                 TO W-KDARBTYP                            
048000     MOVE 499                    TO W-IDPERSON                            
048100     PERFORM IMS-GET-WDP3-IDANSK                                          
048200     IF SEGMENT-SAKNAS                                                    
048300        DISPLAY ' IDINK  SAKNAS PÅ P311: ' '499'                          
048400     ELSE                                                                 
048500        MOVE PERS-IDMAIL         TO MAIL-ADRESS                           
048600     END-IF                                                               
048700     MOVE MAIL-RAD               TO UT3-AREA                              
048800     WRITE UT3-POST FROM UT3-AREA                                         
048900     MOVE 'MEMO SEND'            TO UT3-AREA                              
049000     WRITE UT3-POST FROM UT3-AREA                                         
049100     MOVE ')END'                 TO UT3-AREA                              
049200     WRITE UT3-POST FROM UT3-AREA                                         
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-GU-HTR-1141-ROT SECTION.                                             
049600     STRING 'WLXXAV01(WDGXKEY  =' W-WDGXKEY-1141 ')'                      
049700            DELIMITED BY SIZE INTO SSA1                                   
049800     MOVE '  ' TO GODK-STATUSKODER                                        
049900     CALL CBLTDLI USING GU HTR-PCB DLI-IO-AREA SSA1                       
050000     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     SKIP3                                                                
050400 IMS-GHNP-HTR-1142 SECTION.                                               
050500     MOVE 'WLXXAV11(KDSEGKEY =5)'  TO SSA1                                
050600     MOVE '  GE' TO GODK-STATUSKODER                                      
050700     CALL CBLTDLI USING GHNP HTR-PCB DLI-IO-AREA SSA1                     
050800     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
050900     PERFORM IMS-STATUSKONTROLL                                           
051000     .                                                                    
051100     SKIP3                                                                
051200 IMS-DLET-HTR-1142 SECTION.                                               
051300     MOVE '  ' TO GODK-STATUSKODER                                        
051400     CALL CBLTDLI USING DLET HTR-PCB DLI-IO-AREA                          
051500     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
051600     PERFORM IMS-STATUSKONTROLL                                           
051700     .                                                                    
051800     EJECT                                                                
051900 IMS-GU-ARTC-ROT SECTION.                                                 
052000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
052100            DELIMITED BY SIZE INTO SSA1                                   
052200     MOVE '  ' TO GODK-STATUSKODER                                        
052300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
052400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052700     SKIP3                                                                
052800 IMS-GNP-ARTC11 SECTION.                                                  
052900     MOVE  'WLARTC11 ' TO SSA1                                            
053000     MOVE '  GE'   TO GODK-STATUSKODER                                    
053100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
053200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053300     PERFORM IMS-STATUSKONTROLL                                           
053400     .                                                                    
053500     SKIP3                                                                
053600 IMS-GU-ARTG-ROT SECTION.                                                 
053700     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
053800            DELIMITED BY SIZE INTO SSA1                                   
053900     MOVE '  GE' TO GODK-STATUSKODER                                      
054000     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA-2 SSA1                    
054100     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     .                                                                    
054400     EJECT                                                                
054500 IMS-GU-D311 SECTION.                                                     
054600                                                                          
054700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
054800            DELIMITED BY SIZE INTO SSA1                                   
054900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
055000            DELIMITED BY SIZE INTO SSA2                                   
055100     MOVE '  GE' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
055300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     EJECT                                                                
055700 IMS-GET-WDP3-IDANSK   SECTION.                                           
055800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
055900          DELIMITED BY SIZE INTO SSA1                                     
056000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
056100          DELIMITED BY SIZE INTO SSA2                                     
056200     MOVE '  GE' TO GODK-STATUSKODER                                      
056300     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
056400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700     EJECT                                                                
056800 IMS-STATUSKONTROLL SECTION.                                              
056900     SET STATUS-IX TO 1                                                   
057000     SEARCH GODK-STATUS                                                   
057100       AT END CALL FELLOG                                                 
057200       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
057300     END-SEARCH                                                           
057400     .                                                                    
057500     EJECT                                                                
057600*    -COPY WY2000P1                                                       

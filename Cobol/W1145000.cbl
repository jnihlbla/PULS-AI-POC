000100 ID DIVISION.                                                             
000200     SKIP3                                                                
000300 PROGRAM-ID.             W1145000.                                        
000400 AUTHOR.                 INGRID DANIELSSON/LOTTA L                        
000500     DATE-WRITTEN.       JULI 1988.                                       
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            IMS BMP/BATCH HUVUDPROGRAM.                                  
001100*                                                                         
001200*            TÖMNING  NYPON - INKÖP PV                                    
001300*                                                                         
001400*            PROGRAMMET STYRS AV HÄNDELSETRANSAR (WDGX1142) SOM           
001500*            BETAS AV.                                                    
001600*            PLOCKAR INFO FRÅN ARTIKELREGISTREN (WDK6, WDD2)              
001700*                              WDP3 (ANSK-INFO/WANSNAME).                 
001800*                                                                         
001900*                                                                         
002000***------------------------------------------------------------           
002100*ÄNDRINGAR:                                                               
002200*2015-12-30  E'TRACKER 10243132 CHINA EXPORT 2015                         
002300*                                                                         
002400*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP3                                                                
003200* UTFILER:                                                                
003300*                        TRANSAR TILL INKÖP PV                            
003400     SELECT  W11450      ASSIGN    UT-S-W11450D2.                         
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W11450                                                               
004100     LABEL RECORD STANDARD                                                
004200     RECORDING      V                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP3                                                                
004500*01  UTPOST        -COPY T335R301 -L                                      
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -COPY WY2000W1                                                       
005000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1145000'.               
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  IX                          PIC 9(3)    VALUE ZERO.                  
005400 77  IX-MAX                      PIC 9(3)    VALUE ZERO.                  
005500 77  KOLL-IX                     PIC 9(3)    VALUE ZERO.                  
005600 77  RAEKN                       PIC S9(3)   VALUE +0    COMP-3.          
005700 77  WS-KVPROG                   PIC 9(9)    VALUE ZERO.                  
005800 77  WS-IDHANDLR                 PIC 9(3)    VALUE ZERO.                  
005900 77  SW-TRAFF                    PIC X       VALUE 'N'.                   
006000                                                                          
006100 01  ARTIKELNR.                                                           
006200     03 BLANKA-X                 PIC X(11) VALUE SPACE.                   
006300     03 ARTNR-ALFA               PIC X(9).                                
006400     03 ARTNR-NUM REDEFINES ARTNR-ALFA PIC 9(9).                          
006500                                                                          
006600 01  WS-IDUSER.                                                           
006700     03 WS-IDMAIL                PIC X(9).                                
006800     03 FILLER                   PIC X(51).                               
006900                                                                          
007000 01  KOLL-IDUSER                 PIC X(9).                                
007100 01  FILLER REDEFINES KOLL-IDUSER.                                        
007200     03  KOLL-TKN                PIC X  OCCURS 9.                         
007300                                                                          
007400 01  SPAR-AREA.                                                           
007500     03  SPAR-ARTC-IDANSK        PIC 9(3).                                
007600     03  SPAR-ARTC-IDLEVNR       PIC X(5).                                
007700     03  SPAR-ARTC-KDHF          PIC S9                  COMP-3.          
007800     03  SPAR-ARTC-IDINK         PIC 9(3).                                
007900     03  SPAR-ARTG-IDINK         PIC 9(3).                                
008000     03  SPAR-KDSORT             PIC X(2).                                
008100     03  SPAR-FLPISK             PIC X(1).                                
008200     03  SPAR-IDPROJ             PIC X(4).                                
008300     03  SPAR-KDERS              PIC 9(2).                                
008400     EJECT                                                                
008500*01   -COPY WWKONLEV                                                      
008600     EJECT                                                                
008700 01  DAGENS-DATUM.                                                        
008800     03  AAMMDD                  PIC 9(6)    VALUE ZERO.                  
008900     03  AAVV                    PIC 9(4)    VALUE ZERO.                  
009000     03  FILLER    REDEFINES AAVV.                                        
009100         05  AA                  PIC 9(2).                                
009200         05  VV                  PIC 9(2).                                
009300     SKIP3                                                                
009400 01  W-ANSK-TEL.                                                          
009500     03  W-IDANSK                PIC 9(3).                                
009600     03  FILLER                  PIC X.                                   
009700     03  W-EXTTEL.                                                        
009800         05  W-IDTFN             PIC X(16).                               
009900     EJECT                                                                
010000*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
010100     SKIP3                                                                
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
010400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
010500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010600   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010700   03  W980SOP                   PIC X(8)    VALUE 'W980SOP '.            
010800     EJECT                                                                
010900*    ---- PARAMETRAR TILL W980SOP                                         
011000                                                                          
011100*01  -COPY WSOPAREA.                                                      
011200     EJECT                                                                
011300*    ---- PARAMETRAR TILL WDATKONV                                        
011400                                                                          
011500*01  -COPY WDATAREA.                                                      
011600     EJECT                                                                
011700*    ---- PARAMETRAR TILL POSTSUM                                         
011800                                                                          
011900*01  -COPY W0005       -PRE POSTSUM-.                                     
012000     EJECT                                                                
012100*01  AREA     -COPY T335R301   -PRE UT-                                   
012200     EJECT                                                                
012300*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500     SKIP3                                                                
012600*    ---- STATUSKOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FINNS                       VALUE '  '.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     88  SEGMENT-SLUT                        VALUE 'GE'.                  
013100     88  IMS-EJ-OK                           VALUE 'XD'.                  
013200     SKIP3                                                                
013300 01  GODK-STATUSKODER.                                                    
013400   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
014000 01  NYCKLAR-TILL-DLI.                                                    
014100   03  W-IDARTNR-X.                                                       
014200     05  W-IDARTNR               PIC S9(9)                COMP-3.         
014300   03  W-WDGXKEY-1141.                                                    
014400     05  FILLER                  PIC X(4)     VALUE '1141'.               
014500     05  FILLER                  PIC X(26)    VALUE LOW-VALUE.            
014600   03  W-KDARBTYP-X.                                                      
014700     05  W-KDARBTYP              PIC X(08)    VALUE 'ANSK'.               
014800   03  W-IDPERSON-X.                                                      
014900     05  W-IDPERSON              PIC S9(3)    VALUE ZERO COMP-3.          
015000     EJECT                                                                
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300 01  DLI-IO-AREA.                                                         
015400   03 IO-AREA                    PIC X(900)  VALUE SPACE.                 
015500     SKIP3                                                                
015600*  03  WLXXAV11 -COPY WDGX1142 -PRE HTR-  -RED IO-AREA.                   
015700     EJECT                                                                
015800*  03  WLARTC01 -COPY WDK601              -RED IO-AREA.                   
015900     EJECT                                                                
016000*  03  WLARTC11 -COPY WDK611              -RED IO-AREA.                   
016100     EJECT                                                                
016200 01  DLI-IO-AREA-2.                                                       
016300   03 IO-AREA-2                  PIC X(600)  VALUE SPACE.                 
016400     SKIP3                                                                
016500*  03  WLARTG01 -COPY WDD201 -PRE ARTG- -RED IO-AREA-2.                   
016600     EJECT                                                                
016700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
016800 01  DLI-IO-WDP311.                                                       
016900*    03  -COPY WDP311                                                     
017000     EJECT                                                                
017100 LINKAGE SECTION.                                                         
017200     SKIP3                                                                
017300*01      -COPY W0009     -PRE MSG-                                        
017400     EJECT                                                                
017500*01      -COPY W0008     -PRE HTR-                                        
017600      05 FILLER          PIC X.                                           
017700     EJECT                                                                
017800*01      -COPY W0008     -PRE ARTC-                                       
017900      05 FILLER          PIC X.                                           
018000     EJECT                                                                
018100*01      -COPY W0008     -PRE ARTG-                                       
018200      05 FILLER          PIC X.                                           
018300     EJECT                                                                
018400*01      -COPY W0008     -PRE WDP3-                                       
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700 PROCEDURE DIVISION USING MSG-PCB HTR-PCB ARTC-PCB                        
018800                        ARTG-PCB WDP3-PCB.                                
018900     ENTRY 'DLITCBL' USING MSG-PCB HTR-PCB ARTC-PCB                       
019000                        ARTG-PCB WDP3-PCB.                                
019100                                                                          
019200     PERFORM A-INIT                                                       
019300     PERFORM IMS-GU-HTR-1141-ROT                                          
019400     PERFORM IMS-GHNP-HTR-1142                                            
019500                                                                          
019600     PERFORM UNTIL SEGMENT-SLUT OR RAEKN > +499                           
019700                                                                          
019800        MOVE HTR-1142-IDARTNR TO W-IDARTNR                                
019900        PERFORM IMS-GU-ARTG-ROT                                           
020000                                                                          
020100        IF SEGMENT-FINNS                                                  
020200           PERFORM IMS-GU-ARTC-ROT                                        
020300           IF SEGMENT-FINNS                                               
020400              MOVE ART-IDLEVNR  TO SPAR-ARTC-IDLEVNR                      
020500                                   WS-IDLEVNR-KONCERN                     
020600              MOVE ART-KDSORT   TO SPAR-KDSORT                            
020700              MOVE ART-KDERS-UTG TO SPAR-KDERS                            
020800              MOVE ZERO TO SPAR-ARTC-IDANSK                               
020900                           SPAR-ARTC-KDHF                                 
021000              PERFORM IMS-GNP-ARTC11                                      
021100              IF SEGMENT-FINNS                                            
021200                 MOVE CLAG-IDANSK TO SPAR-ARTC-IDANSK                     
021300                 MOVE CLAG-KDHF   TO SPAR-ARTC-KDHF                       
021400                 MOVE CLAG-KDERS  TO SPAR-KDERS                           
021500              END-IF                                                      
021600           END-IF                                                         
021700                                                                          
021800           PERFORM AB-KOLLA-IDINK                                         
021900           IF KONCERN-LEV                                                 
022000           OR (SPAR-ARTC-KDHF > 0)                                        
022100           OR (WS-IDHANDLR = 0)                                           
022200              DISPLAY  'FEL '  W-IDARTNR                                  
022300           ELSE                                                           
022400              PERFORM B-SKAPA-SKRIV-UTPOST                                
022500           END-IF                                                         
022600        ELSE                                                              
022700           DISPLAY ' *** FINNS EJ PÅ NYPON : ' W-IDARTNR                  
022800        END-IF                                                            
022900                                                                          
023000        PERFORM IMS-GHNP-HTR-1142                                         
023100     END-PERFORM                                                          
023200                                                                          
023300     PERFORM Z-FINIT                                                      
023400                                                                          
023500     IF SEGMENT-SLUT                                                      
023600        CONTINUE                                                          
023700     ELSE                                                                 
023800        PERFORM ZZ-STARTA-NYTT-JOBB-VIA-SOP                               
023900     END-IF                                                               
024000                                                                          
024100     MOVE ZERO TO RETURN-CODE                                             
024200     GOBACK                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 A-INIT SECTION.                                                          
024600                                                                          
024700     MOVE ZERO TO RAEKN                                                   
024800                                                                          
024900     MOVE 'IDAG'  TO DAT-KDDATFORM                                        
025000     MOVE  ZERO   TO DAT-I-TIDATUM                                        
025100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
025200                         DAT-O-TIDATUM DAT-KDSVAR                         
025300     IF DAT-KDSVAR-OK                                                     
025400       MOVE DAT-TIAAMMDD    TO  AAMMDD                                    
025500       MOVE DAT-TIAA-VECKA  TO  AA                                        
025600       MOVE DAT-TIVV        TO  VV                                        
025700     END-IF                                                               
025800                                                                          
025900     OPEN OUTPUT W11450                                                   
026000     MOVE 'W11450'   TO  POSTSUM-PROGNAMN                                 
026100                         POSTSUM-FDNAMN                                   
026200     MOVE 'W11450D2' TO  POSTSUM-DDNAMN2                                  
026300     .                                                                    
026400     EJECT                                                                
026500 AB-KOLLA-IDINK SECTION.                                                  
026600                                                                          
026700     IF CLAG-IDINK (1:3) NUMERIC                                          
026800        MOVE CLAG-IDINK (1:3)    TO SPAR-ARTC-IDINK                       
026900     ELSE                                                                 
027000        IF CLAG-IDINK (2:3) NUMERIC                                       
027100           MOVE CLAG-IDINK (2:3) TO SPAR-ARTC-IDINK                       
027200        ELSE                                                              
027300            MOVE ZERO             TO SPAR-ARTC-IDINK                      
027400        END-IF                                                            
027500     END-IF                                                               
027600                                                                          
027700     IF ARTG-ART-IDINK (1:3) NUMERIC                                      
027800        MOVE ARTG-ART-IDINK (1:3) TO SPAR-ARTG-IDINK                      
027900     ELSE                                                                 
028000        IF ARTG-ART-IDINK (2:3) NUMERIC                                   
028100           MOVE ARTG-ART-IDINK (2:3) TO SPAR-ARTG-IDINK                   
028200        ELSE                                                              
028300            MOVE ZERO             TO SPAR-ARTG-IDINK                      
028400        END-IF                                                            
028500     END-IF                                                               
028600                                                                          
028700                                                                          
028800     MOVE ZERO TO WS-IDHANDLR                                             
028900     IF (SPAR-ARTC-IDINK > +99  AND < +790) OR                            
029000        (SPAR-ARTC-IDINK > +799 AND < +987) OR                            
029100        (SPAR-ARTC-IDINK > +987 AND < +1000)                              
029200         MOVE SPAR-ARTC-IDINK TO WS-IDHANDLR                              
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 B-SKAPA-SKRIV-UTPOST SECTION.                                            
029700                                                                          
029800     MOVE '301'       TO UT-IDRT                                          
029900     MOVE SPACE       TO UT-COMMON-AREA                                   
030000                         UT-NP-AREA                                       
030100     MOVE WS-IDHANDLR TO UT-IDHANDLR                                      
030200     MOVE W-IDARTNR TO ARTNR-NUM                                          
030300     INSPECT ARTNR-ALFA REPLACING LEADING ZEROES BY SPACE                 
030400     MOVE ARTIKELNR TO UT-IDPITEM                                         
030500     MOVE 'PR'        TO UT-CDTYPE-REQ                                    
030600     MOVE 'V'         TO UT-CD-IDPITEM                                    
030700     MOVE 'VCC '      TO UT-IDPORG                                        
030800     MOVE ARTG-ART-TEANSINK TO UT-TXNOTES-REQ1                            
030900     MOVE SPAR-KDSORT TO UT-NP-CDUOM                                      
031000*******                                                                   
031100     IF SPAR-KDSORT = 'ST' OR 'SA' OR 'SW' OR 'TM' OR 'HW'                
031200                       OR 'PA'                                            
031300        MOVE 'PCE'          TO UT-NP-CDUOM                                
031400     ELSE                                                                 
031500        IF SPAR-KDSORT = 'MM'                                             
031600           MOVE 'MMT'       TO UT-NP-CDUOM                                
031700        END-IF                                                            
031800        IF SPAR-KDSORT = 'M'                                              
031900           MOVE 'MTR'       TO UT-NP-CDUOM                                
032000        END-IF                                                            
032100        IF SPAR-KDSORT = 'M2'                                             
032200           MOVE 'MTK'       TO UT-NP-CDUOM                                
032300        END-IF                                                            
032400        IF SPAR-KDSORT = 'M3'                                             
032500           MOVE 'MTQ'       TO UT-NP-CDUOM                                
032600        END-IF                                                            
032700        IF SPAR-KDSORT = 'ML'                                             
032800           MOVE 'MLT'       TO UT-NP-CDUOM                                
032900        END-IF                                                            
033000        IF SPAR-KDSORT = 'L'                                              
033100           MOVE 'LTR'       TO UT-NP-CDUOM                                
033200        END-IF                                                            
033300        IF SPAR-KDSORT = 'G'                                              
033400           MOVE 'GRM'       TO UT-NP-CDUOM                                
033500        END-IF                                                            
033600        IF SPAR-KDSORT = 'KG'                                             
033700           MOVE 'KGM'       TO UT-NP-CDUOM                                
033800        END-IF                                                            
033900        IF SPAR-KDSORT = 'C2'                                             
034000           MOVE 'C2 '       TO UT-NP-CDUOM                                
034100        END-IF                                                            
034200     END-IF                                                               
034300*******                                                                   
034400                                                                          
034500     IF SPAR-ARTC-IDANSK > 0                                              
034600        MOVE SPAR-ARTC-IDANSK TO W-IDANSK                                 
034700        MOVE W-IDANSK TO W-IDPERSON                                       
034800        PERFORM IMS-GET-WDP3-IDANSK                                       
034900        IF SEGMENT-SAKNAS                                                 
035000           DISPLAY ' IDANSK SAKNAS PÅ P311: ' W-IDANSK                    
035100        ELSE                                                              
035200           MOVE PERS-IDNAMN TO UT-NMHANDLR-ISSUER                         
035300           MOVE PERS-IDTFN  TO UT-IDPHONE-ISSUER                          
035400           MOVE PERS-IDAVD  TO UT-IDSECTN-ISSUER                          
035500           MOVE PERS-IDMAIL TO WS-IDUSER                                  
035600           PERFORM BA-KOLLA-IDUSER                                        
035700        END-IF                                                            
035800     END-IF                                                               
035900                                                                          
036000     IF SPAR-KDERS > 10                                                   
036100        MOVE AAMMDD TO UT-NP-TIPROD-DATE                                  
036200        MOVE ZERO   TO UT-NP-QTPITEM-YEAR                                 
036300                                                                          
036400                                                                          
036500***  IF ARTG-ART-KDRESBED = 'E' OR 'U'                                    
036600***     MOVE AAMMDD TO UT-NP-TIPROD-DATE                                  
036700***     MOVE ZERO   TO UT-NP-QTPITEM-YEAR                                 
036800     ELSE                                                                 
036900        IF ARTG-ART-KVPROG = ZERO                                         
037010          IF ARTG-ART-TILEVBEG = ZERO                                     
037020            MOVE ARTG-ART-TILEVBEG TO UT-NP-TIPROD-DATE                   
037030          ELSE                                                            
037031            MOVE AAMMDD TO UT-NP-TIPROD-DATE                              
037040          END-IF                                                          
037100        ELSE                                                              
037200           MOVE ARTG-ART-TILEVBEG TO UT-NP-TIPROD-DATE                    
037300        END-IF                                                            
037400        MOVE ZERO              TO WS-KVPROG                               
037500        MOVE ARTG-ART-KVPROG   TO WS-KVPROG                               
037600        MOVE WS-KVPROG         TO UT-NP-QTPITEM-YEAR                      
037700     END-IF                                                               
037800                                                                          
037900     MOVE 'N'               TO UT-FLPLANT-BUYER                           
038000     MOVE 'Y'               TO UT-FLCOBL-ALLOWED                          
038100                               UT-NP-FLCOBL-REQUIRED                      
038200     MOVE 'BP2TW'           TO UT-NP-IDUSER                               
038300     MOVE ZERO              TO UT-IDCONSIG                                
038400                               UT-NP-QTPITEM-ORDER                        
038500                               UT-NP-TIPRE-DEL-DATE-1                     
038600                               UT-NP-QTPRE-DEL-1                          
038700                               UT-NP-TIPRE-DEL-2                          
038800                               UT-NP-TIPRE-DEL-DATE-2                     
038900                               UT-NP-QTPRE-DEL-2                          
039000                               UT-NP-TIPRE-DEL-3                          
039100                               UT-NP-TIPRE-DEL-DATE-3                     
039200                               UT-NP-QTPRE-DEL-3                          
039300                               UT-NP-TISAMPLE                             
039400                               UT-NP-TISAMPLE-DATE                        
039500                               UT-NP-QTSAMPLE                             
039600                                                                          
039700*******fix bodil 060510                                                   
039800**** IF ARTG-ART-TILEVBEG > 041231                                        
039900**** OR ARTG-ART-TILEVBEG = 0                                             
040000        PERFORM S01-SKRIV-UTPOST                                          
040100        PERFORM IMS-DLET-HTR-1142                                         
040200        ADD +1 TO RAEKN                                                   
040300**** END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 BA-KOLLA-IDUSER SECTION.                                                 
040700                                                                          
040800     MOVE NEJ TO SW-TRAFF                                                 
040900     MOVE WS-IDMAIL TO KOLL-IDUSER                                        
041000     MOVE 1 TO IX                                                         
041100     MOVE 9 TO IX-MAX                                                     
041200     PERFORM UNTIL IX > IX-MAX                                            
041300        IF KOLL-TKN(IX) = '@'                                             
041400           MOVE IX TO KOLL-IX                                             
041500           MOVE 10 TO IX                                                  
041600           MOVE JA TO SW-TRAFF                                            
041700        ELSE                                                              
041800           ADD 1 TO IX                                                    
041900        END-IF                                                            
042000     END-PERFORM                                                          
042100                                                                          
042200     IF SW-TRAFF = NEJ                                                    
042300        MOVE KOLL-IDUSER(1:8) TO UT-IDUSERID-ISSUER                       
042400     ELSE                                                                 
042500       ADD -1 TO KOLL-IX                                                  
042600                                                                          
042700       IF KOLL-IX = 1                                                     
042800        MOVE KOLL-IDUSER(1:1) TO UT-IDUSERID-ISSUER                       
042900       ELSE                                                               
043000        IF KOLL-IX = 2                                                    
043100         MOVE KOLL-IDUSER(1:2) TO UT-IDUSERID-ISSUER                      
043200        ELSE                                                              
043300         IF KOLL-IX = 3                                                   
043400          MOVE KOLL-IDUSER(1:3) TO UT-IDUSERID-ISSUER                     
043500         ELSE                                                             
043600          IF KOLL-IX = 4                                                  
043700           MOVE KOLL-IDUSER(1:4) TO UT-IDUSERID-ISSUER                    
043800          ELSE                                                            
043900           IF KOLL-IX = 5                                                 
044000            MOVE KOLL-IDUSER(1:5) TO UT-IDUSERID-ISSUER                   
044100           ELSE                                                           
044200            IF KOLL-IX = 6                                                
044300             MOVE KOLL-IDUSER(1:6) TO UT-IDUSERID-ISSUER                  
044400            ELSE                                                          
044500             IF KOLL-IX = 7                                               
044600              MOVE KOLL-IDUSER(1:7) TO UT-IDUSERID-ISSUER                 
044700             ELSE                                                         
044800              IF KOLL-IX = 8                                              
044900               MOVE KOLL-IDUSER(1:8) TO UT-IDUSERID-ISSUER                
045000              END-IF                                                      
045100             END-IF                                                       
045200            END-IF                                                        
045300           END-IF                                                         
045400          END-IF                                                          
045500         END-IF                                                           
045600        END-IF                                                            
045700       END-IF                                                             
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 Z-FINIT   SECTION.                                                       
046200                                                                          
046300     CLOSE W11450                                                         
046400                                                                          
046500     MOVE 'S' TO POSTSUM-OPKOD                                            
046600     CALL POSTSUM USING POSTSUM-PARM                                      
046700     .                                                                    
046800     EJECT                                                                
046900 ZZ-STARTA-NYTT-JOBB-VIA-SOP SECTION.                                     
047000                                                                          
047100     MOVE   SPACE   TO  SOP-DDPREFIX                                      
047200     MOVE    'O'    TO  SOP-SOPFUNC                                       
047300     MOVE  'W114S2' TO  SOP-PROC-NAME                                     
047400     MOVE   ZERO    TO  SOP-ACTPASS-DATE                                  
047500                                                                          
047600     CALL W980SOP USING SOP-PARM-AREA                                     
047700                                                                          
047800     IF SOP-RETCODE > +8                                                  
047900       CALL FELLOG                                                        
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 S01-SKRIV-UTPOST SECTION.                                                
048400                                                                          
048500     MOVE SPACE TO POSTSUM-TRANSTYP                                       
048600     CALL POSTSUM USING POSTSUM-PARM                                      
048700     WRITE UTPOST FROM ut-area                                            
048800     .                                                                    
048900     EJECT                                                                
049000 IMS-GU-HTR-1141-ROT SECTION.                                             
049100     STRING 'WLXXAV01(WDGXKEY  =' W-WDGXKEY-1141 ')'                      
049200            DELIMITED BY SIZE INTO SSA1                                   
049300     MOVE '  ' TO GODK-STATUSKODER                                        
049400     CALL CBLTDLI USING GU HTR-PCB DLI-IO-AREA SSA1                       
049500     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
049600     PERFORM IMS-STATUSKONTROLL                                           
049700     .                                                                    
049800     SKIP3                                                                
049900 IMS-GHNP-HTR-1142 SECTION.                                               
050000     MOVE 'WLXXAV11(KDSEGKEY =1)'  TO SSA1                                
050100     MOVE '  GE' TO GODK-STATUSKODER                                      
050200     CALL CBLTDLI USING GHNP HTR-PCB DLI-IO-AREA SSA1                     
050300     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     .                                                                    
050600     SKIP3                                                                
050700 IMS-DLET-HTR-1142 SECTION.                                               
050800     MOVE '  ' TO GODK-STATUSKODER                                        
050900     CALL CBLTDLI USING DLET HTR-PCB DLI-IO-AREA                          
051000     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     EJECT                                                                
051400 IMS-GU-ARTC-ROT SECTION.                                                 
051500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
051600            DELIMITED BY SIZE INTO SSA1                                   
051700     MOVE '  ' TO GODK-STATUSKODER                                        
051800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
051900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     SKIP3                                                                
052300 IMS-GNP-ARTC11 SECTION.                                                  
052400     MOVE  'WLARTC11 ' TO SSA1                                            
052500     MOVE '  GE'   TO GODK-STATUSKODER                                    
052600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
052700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052800     PERFORM IMS-STATUSKONTROLL                                           
052900     .                                                                    
053000     SKIP3                                                                
053100 IMS-GU-ARTG-ROT SECTION.                                                 
053200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
053300            DELIMITED BY SIZE INTO SSA1                                   
053400     MOVE '  GE' TO GODK-STATUSKODER                                      
053500     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA-2 SSA1                    
053600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
053700     PERFORM IMS-STATUSKONTROLL                                           
053800     .                                                                    
053900     EJECT                                                                
054000 IMS-GET-WDP3-IDANSK SECTION.                                             
054100     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
054200          DELIMITED BY SIZE INTO SSA1                                     
054300     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
054400          DELIMITED BY SIZE INTO SSA2                                     
054500     MOVE '  GE' TO GODK-STATUSKODER                                      
054600     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-WDP311 SSA1 SSA2              
054700     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     SKIP3                                                                
055100 IMS-STATUSKONTROLL SECTION.                                              
055200     SET STATUS-IX TO 1                                                   
055300     SEARCH GODK-STATUS                                                   
055400       AT END CALL FELLOG                                                 
055500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
055600     END-SEARCH                                                           
055700     .                                                                    
055800     EJECT                                                                
055900*    -COPY WY2000P1                                                       

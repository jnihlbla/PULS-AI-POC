000400 ID DIVISION.                                                             
000500                                                                          
000600 PROGRAM-ID.             W4255000.                                        
001000*AUTHOR.                 KARL JOHAN                                       
001100*DATE-WRITTEN.           AUG  1982.                                       
001200                                                                          
001900*    FUNKTION:                                                            
002000*            PROGRAMMET LÄSER FILEN W42599 (KONKATENERAD)                 
002100*            OCH KOMPLETTERAR DESS POSTER MED                             
002200*            INFORMATION FRÅN ARTIKELREGISTRET (WDK6).                    
002300*                                                                         
002400*            PROGRAMMET SKRIVER EFTER HAND UT ALLA INLÄSTA                
002500*            POSTER PÅ FILEN W42521.                                      
002600*                                                                         
002700*    SUBPROGRAM:                                                          
002910*            RANDOMKEY  - BERÄKNAR RANDOMISERAD NYCKEL                    
003000*            POSTSUM                                                      
003100     EJECT                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300 INPUT-OUTPUT SECTION.                                                    
003400 FILE-CONTROL.                                                            
003500     SKIP3                                                                
003600*------------------------------------------------ INFIL.                  
003700     SELECT  W42599          ASSIGN UT-S-W42550D1.                        
003800     SKIP3                                                                
003900*------------------------------------------------ UTFILER.                
004000     SELECT  W42521          ASSIGN UT-S-W42550D2.                        
004100     SKIP3                                                                
004200*------------------------------------------------ SORTERINGSFIL           
004300*                                                 PÅ RANDOMKEY            
004400     SELECT  SORTFIL         ASSIGN UT-S-DUMMY.                           
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700 FILE SECTION.                                                            
004800     SKIP2                                                                
004900 FD  W42599                                                               
005000     RECORDING      F                                                     
005100     BLOCK CONTAINS 0.                                                    
005200                                                                          
005300*01  99-POST -COPY W425SU8    -L                                          
005500     SKIP3                                                                
005600 FD  W42521                                                               
005700     RECORDING      F                                                     
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006000*01  21-POST -COPY W425SU8    -L                                          
006200     EJECT                                                                
006300 SD  SORTFIL                                                              
006400                .                                                         
006500                                                                          
006600 01  SD-AREA.                                                             
006700     03  SD-RANDOMKEY           PIC X(4).                                 
006800     03  SD-IDARTNR             PIC S9(9)   COMP-3.                       
006900*    03  SD-POST -COPY W425SU8    -L                                      
007100     EJECT                                                                
007200 WORKING-STORAGE SECTION.                                                 
007210                                                                          
007300*    -- CHECKED BY WY2000                                                 
007900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4255000'.            
008000                                                                          
008100 01  KONSTANTER.                                                          
008200     03  JA                      PIC X       VALUE 'J'.                   
008300     03  NEJ                     PIC X       VALUE 'N'.                   
008400     03  WDK6                    PIC X(4)    VALUE 'WDK6'.                
008600                                                                          
008700 01  EOF.                                                                 
008800     03  W42599-EOF              PIC X.                                   
008900     03  SORTFIL-EOF             PIC X.                                   
009000                                                                          
009100 01  W-ART.                                                               
009200     03  W-ARTIKEL               PIC 9(9).                                
009300     03  W-RED-ART               REDEFINES  W-ARTIKEL.                    
009400         05  W-ARTIKEL-1-8       PIC 9(8).                                
009500         05  FILLER              PIC 9(1).                                
009600                                                                          
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010000     03  W015RAND                PIC X(8)    VALUE 'W015RAND'.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010110     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010120     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010130     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
010131     EJECT                                                                
010140*--PARAMETRAR TILL SUBPROGRAM POSTSUM                                     
010160                                                                          
010170*01  -COPY W0005      -PRE POSTSUM-.                                      
010180     EJECT                                                                
010190*--PARAMETRAR TILL SUBPROGRAM W400ARTU                                    
010191                                                                          
010192*01  -COPY W400ARTU                                                       
010193     EJECT                                                                
010200                                                                          
010300 01  RETURNKODER.                                                         
010400     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16 COMP.              
010500                                                                          
010600 01  ARBETSAREOR.                                                         
010700     03  WS-IDARTNR              PIC S9(9)             COMP-3.            
011500                                                                          
011600 01  PACKA-UPP.                                                           
011700     03  W-KDARTHNT              PIC 9(7).                                
011800     03  FILLER REDEFINES W-KDARTHNT.                                     
011900         05  W-FIKTIV-KVANT      PIC 9(4).                                
012000         05  FILLER              PIC 9(3).                                
012300*                                                                         
012800     EJECT                                                                
014500 01  INFIL-TRANSID.                                                       
014600     03  FILLER              PIC X(6)  VALUE 'W42599'.                    
014700     03  FILLER              PIC X(8)  VALUE 'W42550D1'.                  
014800     03  INFIL-TRANSTYP      PIC X(4)  VALUE '    '.                      
014900                                                                          
015000 01  UTFIL-TRANSID.                                                       
015100     03  FILLER              PIC X(6)  VALUE 'W42521'.                    
015200     03  FILLER              PIC X(8)  VALUE 'W42550D2'.                  
015300     03  UTFIL-TRANSTYP      PIC X(4)  VALUE '    '.                      
015400                                                                          
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16) VALUE 'INPOST-AREA '.          
015700 01  RANDOM-POST.                                                         
015800     03  RANDOM-KEY              PIC X(4).                                
015900     03  RANDOM-IDARTNR          PIC S9(9)    COMP-3.                     
016000*    03  INPOST-AREA -COPY W425SU8    -L                                  
016200     SKIP3                                                                
016300     03  SU2-AREA REDEFINES INPOST-AREA.                                  
016400*        05  POST     -COPY W425SU2    -PRE SU2-                          
016600     EJECT                                                                
016700     03  SU3-AREA REDEFINES INPOST-AREA.                                  
016800*        05  POST     -COPY W425SU3     -PRE SU3-                         
017000     EJECT                                                                
017100     03  PSU8-AREA REDEFINES INPOST-AREA.                                 
017200*        05  POST     -COPY W425PSU8    -PRE PSU8-                        
017400     EJECT                                                                
017410     03  PSUH-AREA REDEFINES INPOST-AREA.                                 
017420*        05  POST     -COPY W425PSUH    -PRE PSUH-                        
017430     EJECT                                                                
017500     03  PSU9-AREA REDEFINES INPOST-AREA.                                 
017600*        05  POST     -COPY W425PSU9    -PRE PSU9-                        
017800     EJECT                                                                
017900 01  SU8-AREA.                                                            
018000*        05  POST     -COPY W425SU8     -PRE SU8-                         
018200     EJECT                                                                
018300 01  SUH-AREA.                                                            
018400*        05  POST     -COPY W425SUH     -PRE SUH-                         
018600     EJECT                                                                
018610 01  SU9-AREA.                                                            
018620*        05  POST     -COPY W425SU9     -PRE SU9-                         
018630     EJECT                                                                
018700 01  NYCKLAR-TILL-DLI.                                                    
018800                                                                          
018900   03  W-IDARTNR-X.                                                       
019000     05  W-IDARTNR               PIC S9(9)                COMP-3.         
019010     EJECT                                                                
019070*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
019080*                                                                         
019090 01  IMS-WS.                                                              
019091   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
019092     SKIP3                                                                
019093*                            *** STATUSKOD FRÅN IMS                       
019094   03  STATUS-WS                 PIC XX.                                  
019095     88  SEGMENT-FINNS                       VALUE '  '.                  
019096     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019099     SKIP3                                                                
019100   03  GODK-STATUSKODER.                                                  
019101     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019102     SKIP3                                                                
019103   03  SSA1                      PIC X(64).                               
019104   03  SSA2                      PIC X(64).                               
019105     EJECT                                                                
019106*01  -COPY W0003                                                          
019107     EJECT                                                                
019108 01  FILLER                   PIC X(16) VALUE 'DLI-IO-AREA-K601'.         
019109 01  DLI-IO-AREA-K601.                                                    
019110     03  WLARTC01.                                                        
019113*        05   -COPY WDK601                                                
019114     EJECT                                                                
019116 01  FILLER                   PIC X(16) VALUE 'DLI-IO-AREA-K611'.         
019117 01  DLI-IO-AREA-K611.                                                    
019118     03  WLARTC11.                                                        
019119*        05   -COPY WDK611                                                
019120     EJECT                                                                
020100 LINKAGE SECTION.                                                         
020110*01  -COPY W0008 -PRE ARTC-.                                              
020120         05  FILLER      PIC X(1).                                        
020130     EJECT                                                                
020600 PROCEDURE DIVISION USING ARTC-PCB.                                       
020700     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
021100                                                                          
021200     PERFORM A-INITIERA                                                   
021300                                                                          
021400     SORT SORTFIL                                                         
021500          ASCENDING KEY SD-RANDOMKEY SD-IDARTNR                           
021600          INPUT  PROCEDURE  B-LAGG-IN-RANDOMKEY                           
021700          OUTPUT PROCEDURE  C-BEARBETA                                    
021800                                                                          
021900     IF SORT-RETURN > ZERO                                                
022000       DISPLAY '***** W4255000: FEL PÅ SORTEN'                            
022100       PERFORM X-ABEND                                                    
022200     END-IF                                                               
022300     PERFORM Z-AVSLUTA                                                    
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INITIERA SECTION.                                                      
022900                                                                          
023000      OPEN INPUT  W42599                                                  
023100           OUTPUT W42521                                                  
023200                                                                          
023300      MOVE 'W4255000'            TO POSTSUM-PROGNAMN                      
023400      MOVE +30000                TO SORT-FILE-SIZE                        
023500      MOVE NEJ                   TO W42599-EOF                            
023600                                    SORTFIL-EOF                           
023700      MOVE ZERO                  TO WS-IDARTNR                            
023800     .                                                                    
023900     EJECT                                                                
024000 B-LAGG-IN-RANDOMKEY SECTION.                                             
024100                                                                          
024200     PERFORM S01-LAS-INFIL                                                
024300                                                                          
024400     PERFORM UNTIL                                                        
024500      NOT ( W42599-EOF = NEJ )                                            
025500       IF SU2-IDPTYP = 'SU2' OR 'SU3' OR 'SU8' OR 'SU9' OR 'SUH'          
025600         PERFORM BA-SKAPA-W015RAND                                        
025700       ELSE                                                               
025800         PERFORM S02-SKRIV-POST-21                                        
025900       END-IF                                                             
026100       PERFORM S01-LAS-INFIL                                              
026200     END-PERFORM                                                          
026300     .                                                                    
026400     SKIP3                                                                
026500 BA-SKAPA-W015RAND SECTION.                                               
026600                                                                          
026700     IF SU2-IDPTYP = 'SU2' OR 'SU3'                                       
026800        MOVE SU3-IDARTNR           TO WS-IDARTNR                          
026900     ELSE                                                                 
027000       IF SU2-IDPTYP = 'SU8'                                              
027100          MOVE PSU8-IDARTNR      TO W-ARTIKEL                             
027200          MOVE W-ARTIKEL-1-8     TO WS-IDARTNR                            
027300       ELSE                                                               
027400         IF SU2-IDPTYP = 'SUH'                                            
027500           MOVE PSUH-IDARTNR     TO W-ARTIKEL                             
027510           MOVE W-ARTIKEL-1-8    TO WS-IDARTNR                            
027600         ELSE                                                             
027720           IF SU2-IDPTYP = 'SU9'                                          
027730              MOVE PSU9-IDARTNR TO WS-IDARTNR                             
027740           END-IF                                                         
027741         END-IF                                                           
027750       END-IF                                                             
027800     END-IF                                                               
027900     MOVE WS-IDARTNR    TO SD-IDARTNR                                     
028000                                                                          
028100     CALL W015RAND USING WS-IDARTNR SD-RANDOMKEY WDK6                     
028200     MOVE INPOST-AREA TO SD-POST                                          
028300     RELEASE SD-AREA                                                      
028400     .                                                                    
028500     EJECT                                                                
028600 C-BEARBETA SECTION.                                                      
028700                                                                          
028800     PERFORM S03-RETURN-SORTFIL                                           
028900     MOVE ZERO TO WS-IDARTNR                                              
029000                                                                          
029100     PERFORM UNTIL SORTFIL-EOF = JA                                       
029300       IF RANDOM-IDARTNR NOT = WS-IDARTNR                                 
029400         PERFORM S04-LAS-ARTREG-INFO                                      
029500         MOVE RANDOM-IDARTNR TO WS-IDARTNR                                
029600       END-IF                                                             
029700       IF SU2-IDPTYP = 'SU2'                                              
029800         IF ART-KDERS-UTG = +0                                            
029900         AND CLAG-KDERS NUMERIC                                           
031600           MOVE CLAG-KDERS    TO SU2-KDERS                                
031605         ELSE                                                             
031614           MOVE ART-KDERS-UTG TO SU2-KDERS                                
031700         END-IF                                                           
032400       ELSE                                                               
032500         IF SU2-IDPTYP = 'SU3'                                            
032600           IF CLAG-KVQPACK-1 NUMERIC                                      
032700             MOVE CLAG-KVQPACK-1 TO SU3-KVQPACK-1                         
032800           ELSE                                                           
032900             MOVE +0 TO SU3-KVQPACK-1                                     
033000           END-IF                                                         
033100           IF CLAG-KDARTHNT NUMERIC                                       
033200             MOVE CLAG-KDARTHNT  TO W-KDARTHNT                            
033300             MOVE W-FIKTIV-KVANT TO SU3-FIKTIV-KVANT                      
033400           ELSE                                                           
033500             MOVE +0 TO SU3-FIKTIV-KVANT                                  
033600           END-IF                                                         
033700         ELSE                                                             
033800           IF SU2-IDPTYP = 'SU8'                                          
034000             MOVE NEJ TO SU8-KDVIP                                        
034100           ELSE                                                           
034200             IF SU2-IDPTYP = 'SU9'                                        
034400               MOVE ART-KDSORT   TO PSU9-KDSORT                           
034500               MOVE CLAG-KDVVKL  TO PSU9-KDVVKL                           
034600               MOVE ART-KDPRODSL TO PSU9-KDPRODSL                         
034700               IF  CLAG-KVQPACK-1 NUMERIC                                 
034800               AND CLAG-KVQPACK-1 NOT = ZERO                              
034900                 MOVE CLAG-KVQPACK-1 TO PSU9-KVQPACK                      
035000               ELSE                                                       
035100                 IF CLAG-KDARTHNT NUMERIC                                 
035200                   MOVE CLAG-KDARTHNT TO W-KDARTHNT                       
035300                   MOVE W-FIKTIV-KVANT  TO PSU9-KVQPACK                   
035400                 ELSE                                                     
035500                   MOVE +0 TO PSU9-KVQPACK                                
035600                 END-IF                                                   
035700               END-IF                                                     
035800             END-IF                                                       
035900           END-IF                                                         
036000         END-IF                                                           
036100       END-IF                                                             
036200       PERFORM S02-SKRIV-POST-21                                          
036300       PERFORM S03-RETURN-SORTFIL                                         
036400     END-PERFORM                                                          
036500     .                                                                    
036600     EJECT                                                                
043500 X-ABEND   SECTION.                                                       
043600                                                                          
043700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
043800     .                                                                    
043900     SKIP3                                                                
044000 Z-AVSLUTA SECTION.                                                       
044100                                                                          
044200     CLOSE W42599                                                         
044300           W42521                                                         
044400                                                                          
044500      MOVE 'S'                   TO POSTSUM-OPKOD                         
044600     CALL POSTSUM USING POSTSUM-PARM                                      
044700     .                                                                    
044800     EJECT                                                                
044900 S01-LAS-INFIL SECTION.                                                   
045000                                                                          
045100     READ W42599 INTO INPOST-AREA                                         
045200                AT END                                                    
045300                    MOVE JA      TO W42599-EOF                            
045400     END-READ                                                             
045500                                                                          
045600     IF W42599-EOF = NEJ                                                  
045700        MOVE SU2-IDPTYP        TO INFIL-TRANSTYP                          
045800        MOVE INFIL-TRANSID     TO POSTSUM-TRANSID                         
045900       CALL POSTSUM USING POSTSUM-PARM                                    
046000     END-IF                                                               
046100     .                                                                    
046200     SKIP3                                                                
046300 S02-SKRIV-POST-21 SECTION.                                               
046400                                                                          
046500     IF SU2-IDPTYP = 'SU8'                                                
046610       MOVE ZERO              TO PSU8-REBPRIS                             
046700       MOVE PSU8-W425PSU8     TO SU8-W425SU8-CTX                          
046710       PERFORM S02A-OMVANDLA-KDARTURS                                     
046720       WRITE 21-POST FROM SU8-AREA                                        
046730     ELSE                                                                 
046740                                                                          
046900       IF SU2-IDPTYP = 'SUH'                                              
047100         MOVE PSUH-W425PSUH  TO SUH-W425SUH                               
047110         WRITE 21-POST FROM SUH-AREA                                      
047112       ELSE                                                               
047114                                                                          
047115         IF SU2-IDPTYP = 'SU9'                                            
047116           MOVE ZERO         TO PSU9-REBPRIS                              
047117           MOVE PSU9-W425PSU9-CTX TO SU9-W425SU9-CTX                      
047118           WRITE 21-POST FROM INPOST-AREA                                 
047119         ELSE                                                             
047120                                                                          
047121           WRITE 21-POST FROM INPOST-AREA                                 
047130         END-IF                                                           
047140       END-IF                                                             
047200     END-IF                                                               
047300                                                                          
047400     MOVE SU2-IDPTYP         TO INFIL-TRANSTYP                            
047500     MOVE UTFIL-TRANSID      TO POSTSUM-TRANSID                           
047600     CALL POSTSUM USING POSTSUM-PARM                                      
047700     .                                                                    
047900     SKIP2                                                                
047920 S02A-OMVANDLA-KDARTURS SECTION.                                          
047921                                                                          
047930     MOVE PSU8-KDARTURS      TO ARTU-KDARTURS                             
047931     MOVE PSU8-IDDISTR       TO ARTU-IDDISTR                              
047932     MOVE SPACE              TO ARTU-IDDC                                 
047933                                                                          
047934     CALL W400ARTU USING ARTU-W400ARTU                                    
047935                                                                          
047936     MOVE ARTU-KDARTURS-NUM  TO SU8-KDARTURS-NUM                          
047940     .                                                                    
047950     EJECT                                                                
048000 S03-RETURN-SORTFIL SECTION.                                              
048100                                                                          
048200     RETURN SORTFIL INTO RANDOM-POST                                      
048300                   AT END                                                 
048400                   MOVE JA       TO SORTFIL-EOF                           
048500       END-RETURN                                                         
048600     .                                                                    
048700     EJECT                                                                
048800 S04-LAS-ARTREG-INFO SECTION.                                             
048900                                                                          
049000     MOVE RANDOM-IDARTNR        TO W-IDARTNR                              
049100     PERFORM IMS-GET-ARTIKELREG-WDK601                                    
049200     IF SEGMENT-FINNS                                                     
049260       IF ART-KDERS-UTG > ZERO                                            
049280         MOVE ZERO                 TO CLAG-KDERS                          
049290         MOVE ZERO                 TO CLAG-KDVVKL                         
049291         MOVE ZERO                 TO CLAG-KVQPACK-1                      
049292         MOVE ZERO                 TO CLAG-KDARTHNT                       
049293       ELSE                                                               
049294         PERFORM IMS-GET-ARTIKELREG-WDK611                                
049302       END-IF                                                             
049303     ELSE                                                                 
049305       MOVE ZERO                 TO ART-KDPRODSL                          
049306       MOVE SPACE                TO ART-KDSORT                            
049307       MOVE ZERO                 TO ART-KDERS-UTG                         
049309       MOVE ZERO                 TO CLAG-KDERS                            
049310       MOVE ZERO                 TO CLAG-KDVVKL                           
049311       MOVE ZERO                 TO CLAG-KVQPACK-1                        
049312       MOVE ZERO                 TO CLAG-KDARTHNT                         
049314     END-IF                                                               
049320     .                                                                    
049400* IMS SEKTIONER                   ******                                  
049500                                                                          
049600 IMS-GET-ARTIKELREG-WDK601 SECTION.                                       
049700                                                                          
049800     STRING  'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                        
049900             DELIMITED BY SIZE INTO    SSA1                               
050000     MOVE    '  GE'              TO    GODK-STATUSKODER                   
050100     CALL    CBLTDLI             USING GU                                 
050200                                       ARTC-PCB                           
050300                                       DLI-IO-AREA-K601                   
050400                                       SSA1                               
050500     MOVE    ARTC-STATUS-CODE    TO    STATUS-WS                          
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     .                                                                    
050800     SKIP2                                                                
050900 IMS-GET-ARTIKELREG-WDK611 SECTION.                                       
051000                                                                          
051100     MOVE    'WLARTC11 '    TO    SSA1                                    
051200     MOVE    '  '           TO    GODK-STATUSKODER                        
051300     CALL    CBLTDLI        USING GNP                                     
051400                                  ARTC-PCB                                
051500                                  DLI-IO-AREA-K611                        
051600                                  SSA1                                    
051700     MOVE    ARTC-STATUS-CODE    TO    STATUS-WS                          
051800     PERFORM IMS-STATUSKONTROLL                                           
051900     .                                                                    
052000     EJECT                                                                
053500 IMS-STATUSKONTROLL SECTION.                                              
053600                                                                          
053700     SET STATUS-IX TO 1                                                   
053800                                                                          
053900     SEARCH GODK-STATUS AT END CALL FELLOG                                
054000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054100         CONTINUE                                                         
054200     END-SEARCH                                                           
054300     .                                                                    

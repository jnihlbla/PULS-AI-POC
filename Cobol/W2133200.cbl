000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2133200.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-WRITTEN.               NOV 1978.                                    
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*    FUNKTION.                                                            
000800*        PROGRAMMET UTFÖR LEVERANTÖRSBYTE.                                
000900*        UPPDATERAR SEGMENT 11 I ARTIKELREGISTRET.                        
001000*        I LEVERANSPLANEREGISTRET SKER OCKSÅ VISS UPPDATERING.            
001100*    SUBPROGRAM.                                                          
001200*        W2133210    SKÖTER ALL  UPPDATERING MOT ARTIKELREGISTRET         
001300*                    OCH LEVERANTÖRSREGISTRET OCH HÄNDELSEBASER           
001400*                    VIA UTFILER SOM UPPDATERAR BASERNA I                 
001500*                    EFTERFÖLJANDE BMP-PROGRAM (W21334 OCH W21336)        
001600*                                                                         
001700*                                                                         
001800******************************************************************        
001900* ÄNDRINGAR:                                                              
002000* 2015-12-07  E'TRACKER 10243132 CHINA EXPORT 2015                        
002100*                       LEV.BYTE EXT-TO-REF + REF-TO-EXT ÄR OK.           
002200*                       LEV.BYTE REF-TO-REF ÄR EJ OK.                     
002300*                                                                         
002400* 2021-AUG-30 STORY-2230520 /1)SUPPLIER 1000 NOT VALID ANYMORE,           
002500*             SO DELETING THE VALIDATION.                                 
002600*             2)REMOVED (IMSART-FLAGGA-KVPB-JUST-XDC = JA) CHECK          
002700*                                                                         
002800*                                                                         
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*--------------------------------------- TRANSAKTIONER FÖR                
003500*                                        LEVERANTÖRSBYTE                  
003600*                                        INPUT                            
003700     SELECT W21331 ASSIGN UT-S-W21332D1.                                  
003800*                                                                         
003900*--------------------------------------- TRANSAKTIONER FÖR                
004000*                                        FELLISTA                         
004100*                                        OUTPUT                           
004200     SELECT W21333 ASSIGN UT-S-W21332D2.                                  
004300*                                                                         
004400*--------------------------------------- TRANSAKTIONER TILL               
004500*                                        EKONOMI PÅ ARTIKLAR SOM          
004600*                                        BYTER TILL REFILL CDC            
004700*                                        OUTPUT                           
004800     SELECT W21350 ASSIGN UT-S-W21332D6.                                  
004900*                                                                         
005000     EJECT                                                                
005100 DATA DIVISION.                                                           
005200 FILE SECTION.                                                            
005300     SKIP2                                                                
005400 FD  W21331                                                               
005500     RECORDING F                                                          
005600     BLOCK 0                                                              
005700     LABEL RECORD STANDARD.                                               
005800*01  -COPY W213R01     -L                                                 
005900     SKIP3                                                                
006000 FD  W21333                                                               
006100     RECORDING V                                                          
006200     BLOCK 0                                                              
006300     LABEL RECORD STANDARD.                                               
006400 01  U33-POST.                                                            
006500*05   -COPY W092W001    -L                                                
006600     05  FILER               PIC X(80).                                   
006700     SKIP3                                                                
006800 FD  W21350                                                               
006900     RECORDING        F                                                   
007000     BLOCK CONTAINS   0.                                                  
007100                                                                          
007200*01  POST -COPY W21350 -PRE  UT50-  -L.                                   
007300     EJECT                                                                
007400 WORKING-STORAGE SECTION.                                                 
007500     SKIP2                                                                
007600                                                                          
007700*    -- CHECKED BY WY2000                                                 
007800 01  W.                                                                   
007900     05  W-PROGNAMN          PIC X(6)    VALUE 'W21332'.                  
008000     05  W-GAMMALT-IDLEVNR   PIC X(5).                                    
008100     05  W-GAMMALT-IDLEVNR-SHIP PIC X(5) VALUE SPACE.                     
008200     05  W-KOD               PIC X(4).                                    
008300     05  W-KVAVROP           PIC S9(7)               COMP-3.              
008400     05  W-KVBR              PIC S9(7)               COMP-3.              
008500     05  W-IDLEVNR           PIC X(5).                                    
008600     05  W-GAMMALT-IDDC-REF  PIC X(2)    VALUE SPACE.                     
008700     05  WS-EXT-TO-REF       PIC X       VALUE SPACE.                     
008800     05  WS-REF-TO-EXT       PIC X       VALUE SPACE.                     
008900     05  WS-REF-TO-REF       PIC X       VALUE SPACE.                     
009000     05  WS-REF-DC-OK        PIC X       VALUE 'N'.                       
009100     05  IX                  PIC 9(2)    VALUE ZERO.                      
009200                                                                          
009300 77  WS-IDLEVNR              PIC X(5).                                    
009400 77  WS-IDLEVNR-NUM          PIC X(5).                                    
009500 77   INDATA-SW              PIC X       VALUE 'J'.                       
009600     88  INDATA-OK           VALUE 'J'.                                   
009700     88  INDATA-FEL          VALUE 'N'.                                   
009800                                                                          
009900                                                                          
010000 77  SW-SATSBEORDR-FINNS     PIC X(1)    VALUE SPACE.                     
010100     88  SATSBEORDR-FINNS                VALUE 'J'.                       
010200                                                                          
010300     SKIP1                                                                
010400 01  FILLER                  PIC X(16)   VALUE 'BYTES-OBJEKT'.            
010500     SKIP2                                                                
010600 01  KONSTANTER.                                                          
010700     05  JA                  PIC X       VALUE 'J'.                       
010800     05  NEJ                 PIC X       VALUE 'N'.                       
010900     05  CURRENT-SECTION     PIC X(30)   VALUE SPACE.                     
011000     SKIP1                                                                
011100     05  OPPNA-FILER         PIC S9(3)   VALUE +001  COMP-3.              
011200     05  LAES-ARTIKELDATA    PIC S9(3)   VALUE +101  COMP-3.              
011300     05  UPPD-ARTIKELDATA    PIC S9(3)   VALUE +102  COMP-3.              
011400     05  LAES-LEVERANTORSDATA                                             
011500                             PIC S9(3)   VALUE +201  COMP-3.              
011600     05  LAES-ART-INFO-PLAN  PIC S9(3)   VALUE +301  COMP-3.              
011700     05  LAES-LEV-INFO-PLAN  PIC S9(3)   VALUE +302  COMP-3.              
011800     05  DELETE-SEGM3-4-PLAN PIC S9(3)   VALUE +303  COMP-3.              
011900     05  LAES-AVROP          PIC S9(3)   VALUE +304  COMP-3.              
012000     05  DELETE-AVROP        PIC S9(3)   VALUE +305  COMP-3.              
012100     05  REPLACE-AVROP       PIC S9(3)   VALUE +306  COMP-3.              
012200     05  LAES-SATSORDERNR    PIC S9(3)   VALUE +307  COMP-3.              
012300     05  LAES-DC-SEGMENT     PIC S9(3)   VALUE +401  COMP-3.              
012400     05  INSERT-WDGX2204     PIC S9(3)   VALUE +402  COMP-3.              
012500     05  INSERT-WDGX2214     PIC S9(3)   VALUE +403  COMP-3.              
012600     05  INSERT-WDGX2302     PIC S9(3)   VALUE +404  COMP-3.              
012700     05  STANG-FILER         PIC S9(3)   VALUE +999  COMP-3.              
012800     SKIP2                                                                
012900 01  SW.                                                                  
013000     05  SW-W21331-EOF       PIC X       VALUE 'N'.                       
013100     SKIP2                                                                
013200*                                                                         
013300*01  -COPY WWPRODSL                                                       
013400*                                                                         
013500*    --- VALID IDDC CODES                                                 
013600*                                                                         
013700*01  -COPY WWDCKONS                                                       
013800*01  -COPY WWDC99                                                         
013900     EJECT                                                                
014000 01  DYNAMISKA-SUBPROGRAM.                                                
014100     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
014200     05  W2133210            PIC X(8)    VALUE 'W2133210'.                
014300     05  W222PBTO            PIC X(8)    VALUE 'W222PBTO'.                
014400     EJECT                                                                
014500*--------------------------------------- AREA FÖR CALL AV                 
014600*                                        KVPB-PLAN                        
014700*01  -COPY W222PBTO                                                       
014800     EJECT                                                                
014900*--------------------------------------- AREA FÖR SVENSKA                 
015000*                                        LEVERANTÖRER                     
015100*    -COPY WWLEV02                                                        
015200     EJECT                                                                
015300*--------------------------------------- AREA FÖR TIVELAKTIGA             
015400*                                        LEVERANTÖRER                     
015500*    -COPY WWLEV03                                                        
015600     EJECT                                                                
015700*--------------------------------------- AREA FÖR W21331-POST             
015800*                                        POSTTYP = R01                    
015900*01  AREA  -COPY W213R01     -PRE I31R01-                                 
016000     EJECT                                                                
016100*--------------------------------------- AREA FÖR W21333-POST             
016200*                                        FELLISTA                         
016300*01  AREA  -COPY W092W001    -PRE U33-                                    
016400     04  U33-TRANS           PIC X(80).                                   
016500*    04  TRANS1  -COPY W213R01T  -PRE  U331- -RED U33-TRANS               
016600     EJECT                                                                
016700*--------------------------------------- AREA FÖR W21350-POST             
016800*                                        ARTIKLAR BYTE EXT-TO-REF         
016900*                                        REFILL PÅ CDC                    
017000*01  AREA  -COPY W21350      -PRE UT50-                                   
017100     EJECT                                                                
017200*--------------------------------------- LÄNKAREA MOT IMS-SUBPGM          
017300*                                        ARTIKELREGISTRET                 
017400*01  -COPY W213L321    -PRE IMSART-                                       
017500     EJECT                                                                
017600*--------------------------------------- LÄNKAREA MOT IMS-SUBPGM          
017700*                                        LEVERANTÖRSREGISTRET             
017800*01  -COPY W213L322    -PRE IMSLEV-                                       
017900     EJECT                                                                
018000*-------------------------------------- LÄNKAREA MOT IMS-SUBPGM           
018100*                                       LEVERANSPLANEREGISTER             
018200*01  -COPY  W213L323     -PRE  IMSPLAN-                                   
018300     EJECT                                                                
018400*--------------------------------------- LÄNKAREA MOT IMS-SUBPGM          
018500*                                        DC-REG (LAES WDB601)             
018600*01  -COPY W213L324   -PRE  IMSDC-                                        
018700     EJECT                                                                
018800*--------------------------------------- LÄNKAREA MOT IMS-SUBPGM          
018900*                                        INSERT MOT HÄNDELSEREGIST        
019000*01  -COPY W213L325   -PRE  IMSC3-                                        
019100     EJECT                                                                
019200*--------------------------------------- PARAMETRAR TILL POSTSUM          
019300*01  -COPY W0005        -PRE POSTSUM-                                     
019400     EJECT                                                                
019500 LINKAGE SECTION.                                                         
019600     SKIP2                                                                
019700*--------------------------------------- PCB TILL LEVERANTÖRSREG          
019800*01  -COPY W0008     -PRE LEVA-                                           
019900     05  FILLER                    PIC X.                                 
020000     EJECT                                                                
020100*--------------------------------------- PCB TILL ARTREG                  
020200*01  -COPY W0008     -PRE ARTC-                                           
020300     05  FILLER                    PIC X.                                 
020400     EJECT                                                                
020500*01  -COPY W0008    -PRE INLB-                                            
020600     05  FILLER                    PIC X.                                 
020700     EJECT                                                                
020800*01  -COPY W0008    -PRE WDB6-                                            
020900     05  FILLER                    PIC X.                                 
021000     EJECT                                                                
021100*01  -COPY W0008    -PRE WDK7-                                            
021200     05  FILLER                    PIC X.                                 
021300     EJECT                                                                
021400 01  PBTO-ARTC-PCB                 PIC X.                                 
021500 01  PBTO-WDK7-PCB                 PIC X.                                 
021600 01  PBTO-ARTM-PCB                 PIC X.                                 
021700 01  PBTO-2501-PCB                 PIC X.                                 
021800 01  PBTO-WDB6R-PCB                PIC X.                                 
021900 01  PBTO-WDK7R-PCB                PIC X.                                 
022000 01  PBTO-WDB6-PCB                 PIC X.                                 
022100 01  PBTO-WDD7-PCB                 PIC X.                                 
022200 01  PBTO-WDK7E-PCB                PIC X.                                 
022300 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
022400 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
022500 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
022600 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
022700 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
022800 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
022900 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
023000 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
023100     EJECT                                                                
023200 PROCEDURE DIVISION USING LEVA-PCB ARTC-PCB                               
023300                          INLB-PCB WDB6-PCB WDK7-PCB                      
023400                          PBTO-ARTC-PCB                                   
023500                          PBTO-WDK7-PCB                                   
023600                          PBTO-ARTM-PCB                                   
023700                          PBTO-2501-PCB                                   
023800                          PBTO-WDB6R-PCB                                  
023900                          PBTO-WDK7R-PCB                                  
024000                          PBTO-WDB6-PCB                                   
024100                          PBTO-WDD7-PCB                                   
024200                          PBTO-WDK7E-PCB                                  
024300                          PBTO-W222-UTIL-WDK6-PCB                         
024400                          PBTO-W222-UTIL-WDK7-PCB                         
024500                          PBTO-W222-UTIL-WDB6-PCB                         
024600                          PBTO-W222-UTUP-WDK7-PCB                         
024700                          PBTO-W222-UTUP-WDB6-PCB                         
024800                          PBTO-W222-UTUP-UTIL-WDK6-PCB                    
024900                          PBTO-W222-UTUP-UTIL-WDK7-PCB                    
025000                          PBTO-W222-UTUP-UTIL-WDB6-PCB                    
025100                          .                                               
025200                                                                          
025300     ENTRY 'DLITCBL' USING LEVA-PCB ARTC-PCB                              
025400                          INLB-PCB WDB6-PCB WDK7-PCB                      
025500                          PBTO-ARTC-PCB                                   
025600                          PBTO-WDK7-PCB                                   
025700                          PBTO-ARTM-PCB                                   
025800                          PBTO-2501-PCB                                   
025900                          PBTO-WDB6R-PCB                                  
026000                          PBTO-WDK7R-PCB                                  
026100                          PBTO-WDB6-PCB                                   
026200                          PBTO-WDD7-PCB                                   
026300                          PBTO-WDK7E-PCB                                  
026400                          PBTO-W222-UTIL-WDK6-PCB                         
026500                          PBTO-W222-UTIL-WDK7-PCB                         
026600                          PBTO-W222-UTIL-WDB6-PCB                         
026700                          PBTO-W222-UTUP-WDK7-PCB                         
026800                          PBTO-W222-UTUP-WDB6-PCB                         
026900                          PBTO-W222-UTUP-UTIL-WDK6-PCB                    
027000                          PBTO-W222-UTUP-UTIL-WDK7-PCB                    
027100                          PBTO-W222-UTUP-UTIL-WDB6-PCB                    
027200                          .                                               
027300     SKIP2                                                                
027400     PERFORM A-INITIERING                                                 
027500     PERFORM B-LAS-TRANS                                                  
027600                                                                          
027700     PERFORM UNTIL SW-W21331-EOF = JA                                     
027800       PERFORM C-LAS-ARTIKEL-INFO                                         
027900       IF IMSART-ANROP-OK                                                 
028000         PERFORM D-LAS-LEVERANTORS-INFO                                   
028100         IF IMSLEV-ANROP-OK                                               
028200           MOVE IMSART-IDLEVNR      TO W-GAMMALT-IDLEVNR                  
028300           MOVE IMSART-IDLEVNR-SHIP TO W-GAMMALT-IDLEVNR-SHIP             
028400           MOVE IMSART-IDDC-REF     TO W-GAMMALT-IDDC-REF                 
028500           MOVE IMSART-KDPRODSL     TO TEST-KDPRODSL                      
028600           IF (IMSART-IDLEVNR = '1000 ' AND                               
028700               I31R01-IDLEVNR = '1002 ')                                  
028800           OR (IMSART-IDLEVNR = '1002 ' AND                               
028900               I31R01-IDLEVNR = '1000 ')                                  
029000           OR (I31R01-IDLEVNR = '1002 ' AND                               
029100               KDPRODSL-LOCAL)                                            
029200             MOVE '003' TO U33-IDFELKODX                                  
029300             PERFORM E-SKRIV-FELMEDDELANDE                                
029400           ELSE                                                           
029500             MOVE IMSLEV-IDLEVNR TO WS-IDLEVNR-NUM                        
029600             INSPECT WS-IDLEVNR-NUM REPLACING                             
029700                                          ALL SPACE BY ZERO               
029800             IF WS-IDLEVNR-NUM NUMERIC                                    
029900               AND (IMSLEV-IDLEVNR-MOTSV NOT = SPACE)                     
030000               MOVE NEJ TO INDATA-SW                                      
030100               MOVE '002' TO U33-IDFELKODX                                
030200               PERFORM E-SKRIV-FELMEDDELANDE                              
030300             ELSE                                                         
030400               MOVE NEJ TO WS-EXT-TO-REF                                  
030500                           WS-REF-TO-EXT                                  
030600                           WS-REF-TO-REF                                  
030700                           WS-REF-DC-OK                                   
030800               PERFORM I-LAS-DC-LEVNR-INFO                                
030900               IF IMSDC-ANROP-OK                                          
031000                 IF W-GAMMALT-IDDC-REF = SPACE                            
031100                   MOVE JA  TO WS-EXT-TO-REF                              
031200                 ELSE                                                     
031300                   MOVE JA  TO WS-REF-TO-REF                              
031400                 END-IF                                                   
031500                                                                          
031600                 MOVE IMSDC-IDDC-REF TO WS-IDDC                           
031710                 IF NDC                                                   
031800                   MOVE JA    TO WS-REF-DC-OK                             
031900                 END-IF                                                   
032000               ELSE                                                       
032100                 IF W-GAMMALT-IDDC-REF NOT = SPACE                        
032200                   MOVE JA  TO WS-REF-TO-EXT                              
032300                 END-IF                                                   
032400               END-IF                                                     
032500                                                                          
032600               IF (WS-REF-TO-REF = JA) OR                                 
032700                  (WS-EXT-TO-REF = JA AND WS-REF-DC-OK = NEJ)             
032800                                                                          
032900                 MOVE '999' TO U33-IDFELKODX                              
033000                 PERFORM E-SKRIV-FELMEDDELANDE                            
033100               ELSE                                                       
033200                 IF (W-GAMMALT-IDLEVNR NOT = I31R01-IDLEVNR)              
033300                 OR (W-GAMMALT-IDLEVNR-SHIP                               
033400                                      NOT = I31R01-IDLEVNR-SHIP)          
033500                 OR (I31R01-IDSYSTEM = 'W212' OR 'W201')                  
033600                   IF WS-REF-TO-EXT = JA                                  
033700                     PERFORM F-UPPDATERA-ARTREG                           
033800                     PERFORM G-UPPDATERA-HAENDELSE-REG                    
033900                   ELSE                                                   
034000                     PERFORM F-UPPDATERA-ARTREG                           
034100                     PERFORM H-BEHANDLA-LEVPLAN                           
034200                     PERFORM G-UPPDATERA-HAENDELSE-REG                    
034300                     IF WS-EXT-TO-REF = JA                                
034400                       PERFORM J-SKRIV-BYTE-REFILL-ART                    
034500                     END-IF                                               
034600                   END-IF                                                 
034700                 END-IF                                                   
034800               END-IF                                                     
034900             END-IF                                                       
035000           END-IF                                                         
035100         ELSE                                                             
035200           MOVE '002' TO U33-IDFELKODX                                    
035300           PERFORM E-SKRIV-FELMEDDELANDE                                  
035400         END-IF                                                           
035500       ELSE                                                               
035600         MOVE '001' TO U33-IDFELKODX                                      
035700         PERFORM E-SKRIV-FELMEDDELANDE                                    
035800       END-IF                                                             
035900       PERFORM B-LAS-TRANS                                                
036000     END-PERFORM                                                          
036100                                                                          
036200     PERFORM Z-AVSLUTNING                                                 
036300     MOVE ZERO TO RETURN-CODE                                             
036400     GOBACK                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 A-INITIERING SECTION.                                                    
036800******************************************************************        
036900*    ÖPPNA ALLA FILER                                            *        
037000*    INITIERA POSTSUM                                            *        
037100******************************************************************        
037200     SKIP2                                                                
037300     OPEN INPUT W21331                                                    
037400          OUTPUT W21333                                                   
037500                 W21350                                                   
037600     SKIP1                                                                
037700     MOVE W-PROGNAMN TO POSTSUM-PROGNAMN                                  
037800     MOVE 'R01' TO POSTSUM-TRANSTYP                                       
037900     MOVE ZERO TO U33-AREA                                                
038000                                                                          
038100     MOVE OPPNA-FILER      TO IMSART-KDCALL                               
038200     CALL W2133210 USING IMSART-W213L321                                  
038300                                 LEVA-PCB                                 
038400                                 ARTC-PCB                                 
038500                                 INLB-PCB                                 
038600                                 WDB6-PCB                                 
038700                                 WDK7-PCB                                 
038800     .                                                                    
038900     EJECT                                                                
039000 B-LAS-TRANS SECTION.                                                     
039100     MOVE 'B-LAS-TRANS '  TO CURRENT-SECTION                              
039200******************************************************************        
039300*    LÄSER W21331 OCH ÖKAR UPP POSTRÄKNAREN                      *        
039400******************************************************************        
039500     SKIP2                                                                
039600     READ W21331 INTO I31R01-AREA                                         
039700       AT END MOVE JA TO SW-W21331-EOF                                    
039800     END-READ                                                             
039900     IF SW-W21331-EOF = NEJ                                               
040000       MOVE 'INFIL' TO POSTSUM-FDNAMN                                     
040100       MOVE 'W21332D1' TO POSTSUM-DDNAMN2                                 
040200       CALL POSTSUM USING POSTSUM-PARM                                    
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 C-LAS-ARTIKEL-INFO SECTION.                                              
040700     MOVE 'C-LAS-ARTIKEL-INFO '  TO CURRENT-SECTION                       
040800******************************************************************        
040900*    LÄSER SEGMENTEN 1, 30 OCH 60 I ARTIKELREGISTRET.            *        
041000*    IMSART-ANROP-FEL SÄTTS DÅ SEGMENT 1 SAKNAS ÖVRIGA           *        
041100*    SEGMENT SKALL ALLTID FINNAS.                                *        
041200******************************************************************        
041300     SKIP2                                                                
041400     MOVE I31R01-IDARTNR TO IMSART-IDARTNR                                
041500     MOVE LAES-ARTIKELDATA TO IMSART-KDCALL                               
041600     CALL W2133210 USING IMSART-W213L321                                  
041700                                 LEVA-PCB                                 
041800                                 ARTC-PCB                                 
041900                                 INLB-PCB                                 
042000                                 WDB6-PCB                                 
042100                                 WDK7-PCB                                 
042200     .                                                                    
042300     EJECT                                                                
042400 D-LAS-LEVERANTORS-INFO SECTION.                                          
042500     MOVE 'D-LAS-LEVERANTORS-INFO '  TO CURRENT-SECTION                   
042600******************************************************************        
042700*    LÄSER ROTSEGMENTET OCH FÖRSTA W213AA07 PÅ LEVERANTÖRS-      *        
042800*    REGISTRET. IMSLEV-ANROP-FEL SÄTTS DÅ ROTSEGMENTET SAKNAS,   *        
042900*    DÅ W213AA07 SAKNAS SÄTTS KDATTENT = 0                       *        
043000******************************************************************        
043100     SKIP2                                                                
043200     MOVE I31R01-IDLEVNR       TO IMSLEV-IDLEVNR                          
043300     MOVE I31R01-IDLEVNR-SHIP  TO IMSLEV-IDLEVNR-SHIP                     
043400     MOVE LAES-LEVERANTORSDATA TO IMSLEV-KDCALL                           
043500     CALL W2133210 USING IMSLEV-W213L322                                  
043600                                 LEVA-PCB                                 
043700                                 ARTC-PCB                                 
043800                                 INLB-PCB                                 
043900                                 WDB6-PCB                                 
044000                                 WDK7-PCB                                 
044100     .                                                                    
044200     EJECT                                                                
044300 E-SKRIV-FELMEDDELANDE SECTION.                                           
044400     MOVE 'E-SKRIV-FELMEDDELANDE '  TO CURRENT-SECTION                    
044500******************************************************************        
044600*    SKAPAR OCH SKRIVER FELMEDDELANDE-POSTER                     *        
044700******************************************************************        
044800     SKIP2                                                                
044900     MOVE I31R01-IDARTNR TO U33-SORTBGP                                   
045000     MOVE I31R01-IDARTNR TO U331-IDARTNR                                  
045100     MOVE I31R01-IDLEVNR TO U331-IDLEVNR                                  
045200     MOVE I31R01-IDSYSTEM TO U331-IDSYSTEM                                
045300     MOVE 'R01' TO U33-IDPTYP                                             
045400                   U331-IDPTYP                                            
045500     SKIP1                                                                
045600     WRITE U33-POST FROM U33-AREA                                         
045700     SKIP1                                                                
045800     MOVE 'FELFIL' TO POSTSUM-FDNAMN                                      
045900     MOVE 'W21332D2' TO POSTSUM-DDNAMN2                                   
046000     CALL POSTSUM USING POSTSUM-PARM                                      
046100     MOVE ZERO TO U33-AREA                                                
046200     .                                                                    
046300     EJECT                                                                
046400 F-UPPDATERA-ARTREG SECTION.                                              
046500     MOVE 'F-UPPDATERA-ARTREG '  TO CURRENT-SECTION                       
046600******************************************************************        
046700*    UPPDATERA SEGMENT 30 OCH 60 MED INFO                        *        
046800*    OM NYA LEVERANTÖREN.                                        *        
046900******************************************************************        
047000     SKIP2                                                                
047100     IF IMSART-FLMANGK = NEJ                                              
047200       MOVE IMSLEV-KDGK TO IMSART-KDGK                                    
047300     END-IF                                                               
047400     SKIP1                                                                
047500     IF IMSART-KDGK = 2                                                   
047600       MOVE IMSLEV-KVDAGAR-TTC2 TO IMSART-KVDAGAR-TT                      
047700     ELSE                                                                 
047800       MOVE IMSLEV-KVDAGAR-TTC1 TO IMSART-KVDAGAR-TT                      
047900     END-IF                                                               
048000     SKIP1                                                                
048100     IF IMSART-KDHF > ZERO AND IMSART-FLMANLT = NEJ                       
048200       CONTINUE                                                           
048300     ELSE                                                                 
048400       IF IMSART-FLMANLT = NEJ                                            
048500         MOVE IMSLEV-KVVECKOR-LT TO IMSART-KVVECKOR-LT                    
048600       END-IF                                                             
048700     END-IF                                                               
048800     SKIP1                                                                
048900     IF IMSART-FLMANAT = NEJ                                              
049000       MOVE IMSLEV-KVVECKOR-AT TO IMSART-KVVECKOR-AT                      
049100     END-IF                                                               
049200                                                                          
049300     IF IMSART-KDAVT = 0                                                  
049400     OR IMSART-KDAVT = 2                                                  
049500     OR IMSART-KDAVT = 3                                                  
049600       IF IMSLEV-KDLEVTYP = +2                                            
049700         MOVE +0 TO IMSART-KDAVT                                          
049800       ELSE                                                               
049900         MOVE IMSLEV-KDLEVTYP TO IMSART-KDAVT                             
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300     SKIP1                                                                
050400     IF WS-EXT-TO-REF = JA                                                
050500       IF IMSART-IDPLANGR-AG > ZERO AND < 9                               
050600                                                                          
050700         MOVE IMSLEV-IDANSK-PG (IMSART-IDPLANGR-AG)                       
050800                               TO IMSART-IDANSK                           
050900       END-IF                                                             
051000     ELSE                                                                 
051100       IF IMSART-IDPLANGR-AG > ZERO AND < 9                               
051200       AND I31R01-IDLEVNR NOT = SPACE AND NOT = '9996 '                   
051300                                      AND NOT = '9997 '                   
051400                                      AND NOT = '9998 '                   
051500                                      AND NOT = '9999 '                   
051600         MOVE IMSLEV-IDANSK-PG (IMSART-IDPLANGR-AG)                       
051700                               TO IMSART-IDANSK                           
051800       END-IF                                                             
051900     END-IF                                                               
052000     SKIP1                                                                
052100     IF I31R01-IDLEVNR = '1002 '                                          
052200       MOVE 1 TO IMSART-KDKSP                                             
052300     END-IF                                                               
052400     SKIP1                                                                
052500     IF IMSART-KDKSP > 1 AND IMSART-IDLEVNR NOT = I31R01-IDLEVNR          
052600       MOVE ZERO TO IMSART-KDKSP                                          
052700     END-IF                                                               
052800     SKIP1                                                                
052900     IF IMSART-IDLEVNR = '1002 '                                          
053000       MOVE ZERO TO IMSART-KDKSP                                          
053100     END-IF                                                               
053200     SKIP1                                                                
053300     IF  IMSART-KDLPSP = 5                                                
053400       MOVE  ZERO TO IMSART-KDLPSP                                        
053500     END-IF                                                               
053600     SKIP1                                                                
053700     IF WS-EXT-TO-REF = JA                                                
053800       PERFORM FA-UPPDATERA-ARTREG-REF                                    
053900     ELSE                                                                 
054000       MOVE SPACE           TO IMSART-IDDC-REF                            
054100       MOVE SPACE           TO IMSART-IDLANDX2                            
054200                                                                          
054300       MOVE ZERO            TO IMSART-KVPB-PLAN                           
054400       MOVE 1 TO IX                                                       
054500       PERFORM UNTIL IX  > 12                                             
054600         MOVE ZERO          TO IMSART-RESEASON-PLAN (IX)                  
054700                                                                          
054800         ADD 1  TO IX                                                     
054900       END-PERFORM                                                        
055000     END-IF                                                               
055100                                                                          
055200     SKIP1                                                                
055300*-------------------------------------- SKRIVER TRANS-FILER               
055400*                                       NYREGISTRERINGAR                  
055500     MOVE UPPD-ARTIKELDATA    TO IMSART-KDCALL                            
055600     MOVE I31R01-IDLEVNR      TO IMSART-IDLEVNR                           
055700     IF WS-EXT-TO-REF = JA                                                
055800       MOVE I31R01-IDLEVNR    TO IMSART-IDLEVNR-SHIP                      
055900     ELSE                                                                 
056000       MOVE I31R01-IDLEVNR-SHIP TO IMSART-IDLEVNR-SHIP                    
056100     END-IF                                                               
056200     CALL W2133210 USING IMSART-W213L321                                  
056300                                 LEVA-PCB                                 
056400                                 ARTC-PCB                                 
056500                                 INLB-PCB                                 
056600                                 WDB6-PCB                                 
056700                                 WDK7-PCB                                 
056800     SKIP1                                                                
056900     .                                                                    
057000     EJECT                                                                
057100 FA-UPPDATERA-ARTREG-REF  SECTION.                                        
057200     MOVE 'FA-UPPDATERA-ARTREG-REF '  TO CURRENT-SECTION                  
057300                                                                          
057400     MOVE IMSDC-IDDC-REF  TO IMSART-IDDC-REF                              
057500     MOVE IMSDC-IDLANDX2  TO IMSART-IDLANDX2                              
057600                                                                          
057700     IF IMSART-KDERS > 20                                                 
057800       MOVE ZERO            TO IMSART-KVPB-PLAN                           
057900       MOVE 1 TO IX                                                       
058000       PERFORM UNTIL IX  > 12                                             
058100         MOVE 1.00          TO IMSART-RESEASON-PLAN (IX)                  
058200                                                                          
058300         ADD 1  TO IX                                                     
058400       END-PERFORM                                                        
058500     ELSE                                                                 
058600*---                                         MASKINELL KVPB-PLAN          
058700       MOVE IMSART-IDARTNR        TO PBTO-IDARTNR                         
058800       CALL W222PBTO USING PBTO-W222PBTO                                  
058900                           PBTO-ARTC-PCB                                  
059000                           PBTO-WDK7-PCB                                  
059100                           PBTO-ARTM-PCB                                  
059200                           PBTO-2501-PCB                                  
059300                           PBTO-WDB6R-PCB                                 
059400                           PBTO-WDK7R-PCB                                 
059500                           PBTO-WDB6-PCB                                  
059600                           PBTO-WDD7-PCB                                  
059700                           PBTO-WDK7E-PCB                                 
059800                           PBTO-W222-UTIL-WDK6-PCB                        
059900                           PBTO-W222-UTIL-WDK7-PCB                        
060000                           PBTO-W222-UTIL-WDB6-PCB                        
060100                           PBTO-W222-UTUP-WDK7-PCB                        
060200                           PBTO-W222-UTUP-WDB6-PCB                        
060300                           PBTO-W222-UTUP-UTIL-WDK6-PCB                   
060400                           PBTO-W222-UTUP-UTIL-WDK7-PCB                   
060500                           PBTO-W222-UTUP-UTIL-WDB6-PCB                   
060600                                                                          
060700       IF PBTO-KDSVAR = JA                                                
060800          MOVE PBTO-KVPB-PLAN     TO IMSART-KVPB-PLAN                     
060900          MOVE 1 TO IX                                                    
061000          PERFORM UNTIL IX  > 12                                          
061100            MOVE PBTO-RESEASON-PLAN (IX)                                  
061200                                  TO IMSART-RESEASON-PLAN (IX)            
061300                                                                          
061400            ADD 1  TO IX                                                  
061500          END-PERFORM                                                     
061600       ELSE                                                               
061700         MOVE ZERO                TO IMSART-KVPB-PLAN                     
061800         MOVE 1 TO IX                                                     
061900         PERFORM UNTIL IX  > 12                                           
062000           MOVE 1.00              TO IMSART-RESEASON-PLAN (IX)            
062100                                                                          
062200           ADD 1  TO IX                                                   
062300         END-PERFORM                                                      
062400       END-IF                                                             
062500     END-IF                                                               
062600     .                                                                    
062700     EJECT                                                                
062800 G-UPPDATERA-HAENDELSE-REG SECTION.                                       
062900     MOVE 'G-UPPDATERA-HAENDELSE-REG '  TO CURRENT-SECTION                
063000******************************************************************        
063100*    LÄGG UPP POSTERNA 2213 OCH 2204 PÅ HÄNDELSEREGISTRET,       *        
063200*    OM DET ÄR EN SATS LÄGGS ÄVEN POST 2302 UPP.                 *        
063300******************************************************************        
063400     SKIP2                                                                
063500*--------------------------------------- POST 2213                        
063600     IF WS-EXT-TO-REF = JA                                                
063700       CONTINUE                                                           
063800     ELSE                                                                 
063900       MOVE I31R01-IDARTNR    TO IMSC3-IDARTNR                            
064000       MOVE INSERT-WDGX2214   TO IMSC3-KDCALL                             
064100                                                                          
064200       CALL W2133210 USING IMSC3-W213L325                                 
064300                           LEVA-PCB                                       
064400                           ARTC-PCB                                       
064500                           INLB-PCB                                       
064600                           WDB6-PCB                                       
064700                           WDK7-PCB                                       
064800     END-IF                                                               
064900     SKIP1                                                                
065000*--------------------------------------- POST 2204                        
065100     IF IMSART-PRARTSTD NOT = ZERO                                        
065200**** DET SKALL INTE SKAPAS LEVERANSFÖRSLAG FÖR ARTIKLAR MED               
065300**** PRODUKTSLAG 3X ELLER 6X MED ORSAK KDLPORS 11                         
065400       MOVE I31R01-IDARTNR    TO IMSC3-IDARTNR                            
065500       MOVE 11                TO IMSC3-KDLPORS                            
065600                                                                          
065700       IF WS-EXT-TO-REF = JA                                              
065800         MOVE IMSDC-IDDC-REF  TO IMSC3-IDDC                               
065900       ELSE                                                               
066000         MOVE WC-CDC-SE       TO IMSC3-IDDC                               
066100       END-IF                                                             
066200                                                                          
066300       MOVE INSERT-WDGX2204   TO IMSC3-KDCALL                             
066400                                                                          
066500       CALL W2133210 USING IMSC3-W213L325                                 
066600                           LEVA-PCB                                       
066700                           ARTC-PCB                                       
066800                           INLB-PCB                                       
066900                           WDB6-PCB                                       
067000                           WDK7-PCB                                       
067100                                                                          
067200                                                                          
067300       IF WS-REF-TO-EXT = JA                                              
067400         MOVE W-GAMMALT-IDDC-REF  TO IMSC3-IDDC                           
067500                                                                          
067600         MOVE INSERT-WDGX2204     TO IMSC3-KDCALL                         
067700                                                                          
067800         CALL W2133210 USING IMSC3-W213L325                               
067900                             LEVA-PCB                                     
068000                             ARTC-PCB                                     
068100                             INLB-PCB                                     
068200                             WDB6-PCB                                     
068300                             WDK7-PCB                                     
068400       END-IF                                                             
068500                                                                          
068600                                                                          
068700     END-IF                                                               
068800     SKIP1                                                                
068900     IF  I31R01-IDLEVNR = '1000 '                                         
069000     OR  I31R01-IDLEVNR = '1002 '                                         
069100     OR  W-GAMMALT-IDLEVNR = '1000 '                                      
069200     OR  W-GAMMALT-IDLEVNR = '1002 '                                      
069300       MOVE I31R01-IDARTNR    TO IMSC3-IDARTNR                            
069400       MOVE I31R01-IDLEVNR    TO IMSC3-IDLEVNR                            
069500       MOVE INSERT-WDGX2302   TO IMSC3-KDCALL                             
069600                                                                          
069700       CALL W2133210 USING IMSC3-W213L325                                 
069800                            LEVA-PCB                                      
069900                            ARTC-PCB                                      
070000                            INLB-PCB                                      
070100                            WDB6-PCB                                      
070200                            WDK7-PCB                                      
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 H-BEHANDLA-LEVPLAN SECTION.                                              
070700     MOVE 'H-BEHANDLA-LEVPLAN '  TO CURRENT-SECTION                       
070800******************************************************************        
070900*    LEVERANSPLANEN ÄNDRAS FÖR ALLA LEVERANTÖRER UTOM DEN NYA.   *        
071000*    SEGMENT 4 OCH VISSA AV SEGMENT 5 DELETAS.                   *        
071100******************************************************************        
071200     SKIP2                                                                
071300     MOVE I31R01-IDARTNR TO IMSPLAN-IDARTNR                               
071400     MOVE LAES-ART-INFO-PLAN TO IMSPLAN-KDCALL                            
071500     CALL W2133210 USING IMSPLAN-W213L323                                 
071600                                 LEVA-PCB                                 
071700                                 ARTC-PCB                                 
071800                                 INLB-PCB                                 
071900                                 WDB6-PCB                                 
072000                                 WDK7-PCB                                 
072100     SKIP1                                                                
072200     IF IMSPLAN-ANROP-OK                                                  
072300       PERFORM HA-LAS-LEVINFO                                             
072400       PERFORM UNTIL IMSPLAN-ANROP-FEL                                    
072500         MOVE IMSPLAN-KVBR TO W-KVBR                                      
072600         IF IMSPLAN-IDLEVNR NOT = I31R01-IDLEVNR                          
072700           MOVE DELETE-SEGM3-4-PLAN TO IMSPLAN-KDCALL                     
072800           CALL W2133210 USING IMSPLAN-W213L323                           
072900                                 LEVA-PCB                                 
073000                                 ARTC-PCB                                 
073100                                 INLB-PCB                                 
073200                                 WDB6-PCB                                 
073300                                 WDK7-PCB                                 
073400                                                                          
073500           IF WS-EXT-TO-REF = JA                                          
073600             PERFORM HE-BEHANDLA-AVROP                                    
073700           ELSE                                                           
073800             IF IMSART-KDLPSP NOT = 3                                     
073900               PERFORM HE-BEHANDLA-AVROP                                  
074000             END-IF                                                       
074100           END-IF                                                         
074200         END-IF                                                           
074300         PERFORM HA-LAS-LEVINFO                                           
074400       END-PERFORM                                                        
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 HA-LAS-LEVINFO SECTION.                                                  
074900     MOVE 'HA-LAS-LEVINFO  '  TO CURRENT-SECTION                          
075000******************************************************************        
075100*    LÄSER LEVERANTÖRSINFO SEGMENT 2  LEVPLANREG                 *        
075200******************************************************************        
075300     SKIP2                                                                
075400     MOVE LAES-LEV-INFO-PLAN TO IMSPLAN-KDCALL                            
075500     CALL W2133210 USING IMSPLAN-W213L323                                 
075600                                LEVA-PCB                                  
075700                                ARTC-PCB                                  
075800                                INLB-PCB                                  
075900                                WDB6-PCB                                  
076000                                WDK7-PCB                                  
076100     SKIP3                                                                
076200     .                                                                    
076300 HB-LAS-AVROP SECTION.                                                    
076400     MOVE 'HB-LAS-AVROP '  TO CURRENT-SECTION                             
076500******************************************************************        
076600*    LÄSER AVROP                 SEGMENT 5                       *        
076700******************************************************************        
076800     SKIP2                                                                
076900     MOVE LAES-AVROP TO IMSPLAN-KDCALL                                    
077000     CALL W2133210 USING IMSPLAN-W213L323                                 
077100                                LEVA-PCB                                  
077200                                ARTC-PCB                                  
077300                                INLB-PCB                                  
077400                                WDB6-PCB                                  
077500                                WDK7-PCB                                  
077600     .                                                                    
077700     EJECT                                                                
077800 HC-BEHANDLA-AVBOKNINGSINFO SECTION.                                      
077900     MOVE 'HC-BEHANDLA-AVBOKNINGSINFO '  TO CURRENT-SECTION               
078000******************************************************************        
078100*    SUMMAN AV AVROPSKVANTITERNA SKALL VARA MINDRE ELLER LIKA MED*        
078200*    BESTÄLLNINGSREST, OM SUMMAN BLIR STÖRRE JUSTERAS DEN SISTA  *        
078300*    POSTEN SÅ ATT DET BLIR LIKHET                               *        
078400******************************************************************        
078500     SKIP2                                                                
078600     IF W-KVAVROP < W-KVBR                                                
078700       ADD IMSPLAN-KVAVROP TO W-KVAVROP                                   
078800       IF W-KVAVROP > W-KVBR                                              
078900         COMPUTE IMSPLAN-KVAVROP = IMSPLAN-KVAVROP -                      
079000                    (W-KVAVROP - W-KVBR)                                  
079100         MOVE REPLACE-AVROP TO IMSPLAN-KDCALL                             
079200         CALL W2133210 USING IMSPLAN-W213L323                             
079300                                 LEVA-PCB                                 
079400                                 ARTC-PCB                                 
079500                                 INLB-PCB                                 
079600                                 WDB6-PCB                                 
079700                                 WDK7-PCB                                 
079800       END-IF                                                             
079900     ELSE                                                                 
080000       MOVE DELETE-AVROP TO IMSPLAN-KDCALL                                
080100       CALL W2133210 USING IMSPLAN-W213L323                               
080200                                 LEVA-PCB                                 
080300                                 ARTC-PCB                                 
080400                                 INLB-PCB                                 
080500                                 WDB6-PCB                                 
080600                                 WDK7-PCB                                 
080700     END-IF                                                               
080800     SKIP3                                                                
080900     .                                                                    
081000 HD-LAES-SATSORDERNR SECTION.                                             
081100     MOVE 'HD-LAES-SATSORDERNR '  TO CURRENT-SECTION                      
081200******************************************************************        
081300*    LÄSER SATSORDERNR SEGMENT 7. OM SEGMENT SAKNAS SÄTTS        *        
081400*    IMSLEV-ANROP-FEL.                                           *        
081500* PL-2016-06-16 SATSORDER FINNS BARA FÖR LEVNR 1002.             *        
081600******************************************************************        
081700     SKIP2                                                                
081800     MOVE LAES-SATSORDERNR TO IMSPLAN-KDCALL                              
081900     CALL W2133210 USING IMSPLAN-W213L323                                 
082000                                 LEVA-PCB                                 
082100                                 ARTC-PCB                                 
082200                                 INLB-PCB                                 
082300                                 WDB6-PCB                                 
082400                                 WDK7-PCB                                 
082500     .                                                                    
082600     EJECT                                                                
082700 HE-BEHANDLA-AVROP   SECTION.                                             
082800     MOVE 'HE-BEHANDLA-AVROP '  TO CURRENT-SECTION                        
082900                                                                          
083000     PERFORM HB-LAS-AVROP                                                 
083100     MOVE ZERO TO W-KVAVROP                                               
083200     PERFORM UNTIL IMSPLAN-ANROP-FEL                                      
083300       IF IMSPLAN-KDAVROP = 1                                             
083400         MOVE DELETE-AVROP TO IMSPLAN-KDCALL                              
083500         CALL W2133210 USING IMSPLAN-W213L323                             
083600                                         LEVA-PCB                         
083700                                         ARTC-PCB                         
083800                                         INLB-PCB                         
083900                                         WDB6-PCB                         
084000                                         WDK7-PCB                         
084100       ELSE                                                               
084200         IF IMSPLAN-KDAVROP = 2                                           
084300           MOVE SPACE TO SW-SATSBEORDR-FINNS                              
084400           IF IMSPLAN-IDLEVNR = '1002'                                    
084500             PERFORM HD-LAES-SATSORDERNR                                  
084600             IF IMSPLAN-ANROP-OK                                          
084700               MOVE JA TO SW-SATSBEORDR-FINNS                             
084800             END-IF                                                       
084900           END-IF                                                         
085000           IF SATSBEORDR-FINNS                                            
085100             CONTINUE                                                     
085200           ELSE                                                           
085300             PERFORM HC-BEHANDLA-AVBOKNINGSINFO                           
085400           END-IF                                                         
085500         END-IF                                                           
085600       END-IF                                                             
085700       PERFORM HB-LAS-AVROP                                               
085800     END-PERFORM                                                          
085900                                                                          
086000     .                                                                    
086100     EJECT                                                                
086200 I-LAS-DC-LEVNR-INFO SECTION.                                             
086300     MOVE 'I-LAS-DC-LEVNR-INFO '  TO CURRENT-SECTION                      
086400******************************************************************        
086500*    LÄSER WDB601                IDLEVNR = DC                    *        
086600*    FINNS IDLEVNR PÅ B6 SÅ ÄR DET ETT REFILL-DC,EJ LOKAL LEV    *        
086700******************************************************************        
086800                                                                          
086900     MOVE I31R01-IDLEVNR        TO IMSDC-IDLEVNR                          
087000     MOVE LAES-DC-SEGMENT       TO IMSDC-KDCALL                           
087100     CALL W2133210 USING IMSDC-W213L324                                   
087200                                LEVA-PCB                                  
087300                                ARTC-PCB                                  
087400                                INLB-PCB                                  
087500                                WDB6-PCB                                  
087600                                WDK7-PCB                                  
087700     .                                                                    
087800     EJECT                                                                
087900 J-SKRIV-BYTE-REFILL-ART  SECTION.                                        
088000     MOVE 'J-SKRIV-BYTE-REFILL-ART '  TO CURRENT-SECTION                  
088100                                                                          
088200     MOVE I31R01-IDARTNR TO UT50-IDARTNR                                  
088300     MOVE I31R01-IDLEVNR TO UT50-IDLEVNR                                  
088400     MOVE WC-CDC-SE      TO UT50-IDDC                                     
088500                                                                          
088600     WRITE UT50-POST FROM UT50-AREA                                       
088700                                                                          
088800     MOVE 'REF'      TO POSTSUM-TRANSTYP                                  
088900     MOVE 'W21350'   TO POSTSUM-FDNAMN                                    
089000     MOVE 'W21332D6' TO POSTSUM-DDNAMN2                                   
089100     CALL POSTSUM USING POSTSUM-PARM                                      
089200                                                                          
089300     .                                                                    
089400     EJECT                                                                
089500 Z-AVSLUTNING SECTION.                                                    
089600******************************************************************        
089700*    STÄNGER ALLA FILER                                          *        
089800*    SKRIVER POSTSUMS RÄKNEVERK                                  *        
089900******************************************************************        
090000     SKIP2                                                                
090100     CLOSE W21331                                                         
090200           W21333                                                         
090300           W21350                                                         
090400     SKIP1                                                                
090500     MOVE 'S' TO POSTSUM-OPKOD                                            
090600     CALL POSTSUM USING POSTSUM-PARM                                      
090700                                                                          
090800     MOVE STANG-FILER      TO IMSART-KDCALL                               
090900     CALL W2133210 USING IMSART-W213L321                                  
091000                                 LEVA-PCB                                 
091100                                 ARTC-PCB                                 
091200                                 INLB-PCB                                 
091300                                 WDB6-PCB                                 
091400                                 WDK7-PCB                                 
091500     .                                                                    

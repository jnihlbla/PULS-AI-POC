000100 ID DIVISION.                                                             
000201 PROGRAM-ID.                 W2133210.                                    
000301 AUTHOR.                     IDK, GÖTEBORG.                               
000401 DATE-WRITTEN.               NOV 1978.                                    
000501                                                                          
000601*    FUNKTION.                                                            
000701*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2133200.                      
000801*        SUBPROGRAMMET SKÖTER OM SAMTLIGA UPPDATERINGAR                   
000901*        MOT ARTIKELREGISTRET (LOGISKT WLARTC),                           
001001*        LEVERANSPLANEREGISTRET (LOGISKT W213AC) OCH                      
001101*        LEVERANTÖRSREGISTRET (LOGISKT W213AA)                            
001201*                                                                         
001301*        OBS  UPPDATERINGARNA SKER NUMERA VIA UTFILER                     
001401*             SOM UPPDATERAR BASERNA I BMP-PGM W21334                     
001501*                                                                         
001601*             ÄVEN HÄNDELSEBASERNA UPPDATERAS PÅ DETTA SÄTT               
001701*             BMP W21336                                                  
001801*                                                                         
002001******************************************************************        
002101* ÄNDRINGAR:                                                              
002201* 2015-12-09  E'TRACKER 10243132 CHINA EXPORT 2015                        
002301*                                                                         
002401*                                                                         
002501*                                                                         
002600 ENVIRONMENT DIVISION.                                                    
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*--------------------------------------- TRANSAKTIONER FÖR                
003100*                                        UPPDATERING AV                   
003201*                                        WDK6,WDD9,WDD6 OCH WDD2          
003308*                                        WDK7                             
003408     SELECT W21334 ASSIGN UT-S-W21332D4.                                  
003508*                                                                         
003608*--------------------------------------- TRANSAKTIONER FÖR                
003708*                                        UPPDATERING AV                   
003808*                                        HÄNDELSEBASER                    
003908     SELECT W21332 ASSIGN UT-S-W21332D5.                                  
004008*                                                                         
004108     SKIP3                                                                
004208 DATA DIVISION.                                                           
004308 FILE SECTION.                                                            
004408     SKIP2                                                                
004508 FD  W21334                                                               
004608     RECORDING F                                                          
004708     BLOCK 0                                                              
004808     LABEL RECORD STANDARD.                                               
004908 01  U34-POST.                                                            
005008*05   -COPY W21334      -L                                                
005108     SKIP3                                                                
005208 FD  W21332                                                               
005308     RECORDING F                                                          
005408     BLOCK 0                                                              
005508     LABEL RECORD STANDARD.                                               
005608 01  U32-POST.                                                            
005708*05   -COPY W2132213    -L                                                
005808     EJECT                                                                
005908 WORKING-STORAGE SECTION.                                                 
006008     SKIP2                                                                
006108                                                                          
006208*    -- CHECKED BY WY2000                                                 
006308 01  KONSTANTER.                                                          
006408     05  JA                  PIC X       VALUE 'J'.                       
006508     05  NEJ                 PIC X       VALUE 'N'.                       
006608     05  CURRENT-SECTION     PIC X(30)   VALUE SPACE.                     
006708     05  DBS-SECTION         PIC X(30)   VALUE SPACE.                     
006800     05  MAX-ANT-ANSKAFFARE  PIC S9(9)   VALUE +8    COMP SYNC.           
006900     SKIP1                                                                
007000     05  OPEN-FILES          PIC S9(3)   VALUE +001  COMP-3.              
007100     05  LAES-ARTIKELDATA    PIC S9(3)   VALUE +101  COMP-3.              
007200     05  UPPD-ARTIKELDATA    PIC S9(3)   VALUE +102  COMP-3.              
007300     05  LAES-LEVERANTORSDATA                                             
007400                             PIC S9(3)   VALUE +201  COMP-3.              
007500     05  LAES-ART-INFO-PLAN  PIC S9(3)   VALUE +301  COMP-3.              
007600     05  LAES-LEV-INFO-PLAN  PIC S9(3)   VALUE +302  COMP-3.              
007700     05  DELETE-SEGM3-4-PLAN PIC S9(3)   VALUE +303  COMP-3.              
007800     05  LAES-AVROP          PIC S9(3)   VALUE +304  COMP-3.              
007900     05  DELETE-AVROP        PIC S9(3)   VALUE +305  COMP-3.              
008000     05  REPLACE-AVROP       PIC S9(3)   VALUE +306  COMP-3.              
008100     05  LAES-SATSORDERNR    PIC S9(3)   VALUE +307  COMP-3.              
008214     05  LAES-DC-SEGMENT     PIC S9(3)   VALUE +401  COMP-3.              
008300     05  INSERT-WDGX2204     PIC S9(3)   VALUE +402  COMP-3.              
008400     05  INSERT-WDGX2214     PIC S9(3)   VALUE +403  COMP-3.              
008500     05  INSERT-WDGX2302     PIC S9(3)   VALUE +404  COMP-3.              
008701     05  CLOSE-FILES         PIC S9(3)   VALUE +999  COMP-3.              
008801                                                                          
008901*01  -COPY WWDCKONS                                                       
009001     SKIP2                                                                
009101*--------------------------------------- AREA FÖR W21334-POST             
009201*                                                                         
009301*01  AREA  -COPY W21334      -PRE U34-                                    
009401     SKIP2                                                                
009501*--------------------------------------- AREA FÖR W21332-POST             
009601*                                                                         
009701 01  U32-AREA                 PIC X(14).                                  
009801     SKIP2                                                                
009901*01  AREA  -COPY W2132213    -PRE 2213-   -RED U32-AREA                   
010001     EJECT                                                                
010101*01  AREA  -COPY W2132204    -PRE 2204-   -RED U32-AREA                   
010201     EJECT                                                                
010301*01  AREA  -COPY W2132302    -PRE 2302-   -RED U32-AREA                   
010401     EJECT                                                                
010501*--------------------------------------- NYCKLAR                          
010601 01  W-IDARTNR-X.                                                         
010701     05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.               
010702                                                                          
010703 01  W-IDDC-X.                                                            
010704     05  W-IDDC              PIC X(2)    VALUE '11'.                      
010706                                                                          
010707 01  W-IDDC-REF-X.                                                        
010708     05  W-IDDC-REF          PIC X(2)    VALUE '11'.                      
010709                                                                          
010801 01  W-WDD901KY-X.                                                        
010901     05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.               
011001     05  W-IDDC-D9           PIC  X(2)   VALUE SPACE.                     
011101 01  W-IDLEVNR-X.                                                         
011201     05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                     
011301                                                                          
011401 01  W-IDLEVNR-B6-X.                                                      
011501     05  W-IDLEVNR-B6        PIC  X(5)   VALUE SPACE.                     
011601                                                                          
011701 01  W-WDD905KY-X.                                                        
011801     03  W-DAAVROP-X.                                                     
011901         05  W-DAAVROP       PIC  9(6)   VALUE ZERO.                      
012001     03  W-TILEVDAG-X.                                                    
012101         05  W-TILEVDAG      PIC  S9     VALUE ZERO COMP-3.               
012102                                                                          
012103 01  W-KDAVROP-X.                                                         
012104     03  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.               
012107                                                                          
012201 01  W-KDERS-0-X.                                                         
012301     05  W-KDERS-0           PIC S9(3)   VALUE ZERO  COMP-3.              
012401                                                                          
012501 01  W.                                                                   
012601     05  IX                  PIC S9(9)   VALUE +0    COMP SYNC.           
012701     SKIP2                                                                
012801 01  DYNAMISKA-SUBPROGRAM.                                                
012901     05  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
013001     05  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
013101     EJECT                                                                
013201*--------------------------------------- ARBETSAREOR TILL                 
013301*                                        IMS-SEKTIONERNA                  
013401 01  IMS-WS.                                                              
013501     05  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
013601     SKIP2                                                                
013701*--------------------------------------- STATUSKOD FRÅN IMS               
013801     05  STATUS-WS           PIC X(2).                                    
013901       88  SEGMENT-FINNS                 VALUE '  '.                      
014001       88  SEGMENT-SAKNAS                VALUE 'GE'.                      
014101     SKIP2                                                                
014201     05  SSA1                PIC X(64).                                   
014301     05  SSA2                PIC X(64).                                   
014401     05  SSA3                PIC X(64).                                   
014501     05  SSA4                PIC X(64).                                   
014601     SKIP2                                                                
014701     05  GODK-STATUSKODER.                                                
014801         10  GODK-STATUS     OCCURS 10                                    
014901                             INDEXED BY STATUS-IX                         
015001                             PIC X(2).                                    
015101     SKIP2                                                                
015201*01  -COPY W0003                                                          
015301     EJECT                                                                
015601*--------------------------------------- IO-AREA TILL SEGMENT 1           
015701*                                        ARTIKELREGISTRET                 
015702                                                                          
015703 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-01'.            
015704     SKIP2                                                                
015705 01  DLI-IO-AREA-01.                                                      
015706*    03  WLARTC01  -COPY WDK601                                           
015901     EJECT                                                                
016201*--------------------------------------- IO-AREA TILL SEGMENT 30          
016301*                                        ARTIKELREGISTRET                 
016302                                                                          
016303 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA-11'.            
016304     SKIP2                                                                
016305 01  DLI-IO-AREA-11.                                                      
016401*    03  WLARTC11  -COPY WDK611                                           
016501     EJECT                                                                
016801*--------------------------------------- IO-AREA TILL SEGMENT 1           
016901*                                        LEVERANTÖRSREGISTRET             
016902                                                                          
016903 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF101'.             
016904     SKIP2                                                                
016905 01  DLI-IO-WDF101.                                                       
017001*    03  WLLEVA01  -COPY WDF101     -PRE LEV01-                           
017101     EJECT                                                                
017201*-------------------------------------- IO-AREA TILL SEGMENT 2            
017301*                            WLINLB11   LEVERANSPLANEREGISTER             
017302                                                                          
017303 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD902'.             
017304     SKIP2                                                                
017305 01  DLI-IO-WDD902.                                                       
017306*    03  W213AC02  -COPY WDD902     -PRE PLAN02-                          
017501     EJECT                                                                
017601*-------------------------------------- IO-AREA TILL SEGMENT 5            
017701*                            WLINLB23   LEVERANSPLANEREGISTER             
017702                                                                          
017703 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD905'.             
017704     SKIP2                                                                
017705 01  DLI-IO-WDD905.                                                       
017801*    03  W213AC05  -COPY WDD905     -PRE  PLAN05-                         
017901     EJECT                                                                
018001*-------------------------------------- IO-AREA TILL SEGMENT 7            
018101*                            WLINLB32   LEVERANSPLANEREGISTER             
018102                                                                          
018103 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD907'.             
018104     SKIP2                                                                
018105 01  DLI-IO-WDD907.                                                       
018201*    03  W213AC07  -COPY WDD907     -PRE  PLAN07-                         
018301     EJECT                                                                
018302*-------------------------------------- IO-AREA TILL SEGMENT 1            
018303*                            WLINLB01   LEVERANSPLANEREGISTER             
018304                                                                          
018305 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
018306 01  DLI-IO-WDD901.                                                       
018307*    03  -COPY WDD901   -PRE PLAN01-                                      
018308     EJECT                                                                
018310*-------------------------------------- IO-AREA TILL SEGMENT 4            
018320*                            WLINLB22   LEVERANSPLANEREGISTER             
018330                                                                          
018340 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
018350 01  DLI-IO-WDD904.                                                       
018360*    03  -COPY WDD904   -PRE PLAN04-                                      
018370     EJECT                                                                
018380*--------------------------------------------------------------           
018401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
018501 01  DLI-IO-WDB601.                                                       
018601*    03  -COPY WDB601                                                     
018801     EJECT                                                                
018802*--------------------------------------------------------------           
018803 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
018804 01  DLI-IO-WDK701.                                                       
018805*    03  -COPY WDK701                                                     
018806     EJECT                                                                
018807 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018808 01  DLI-IO-WDK711.                                                       
018809*    03  -COPY WDK711                                                     
018810     EJECT                                                                
018830 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK727'.                      
018840 01  DLI-IO-WDK727.                                                       
018850*    03  -COPY WDK727                                                     
018860     EJECT                                                                
018901 LINKAGE SECTION.                                                         
019001 01  LINK-AREA               PIC X(200).                                  
019101     SKIP2                                                                
019201*--------------------------------------- LÄNKAREA TILL ARTIKELREG         
019301*01  ARTLINK -COPY W213L321     -PRE LINK- -RED  LINK-AREA                
019401     EJECT                                                                
019501*--------------------------------------- LÄNKAREA TILL LEVERAN-           
019601*                                        TÖRSREGISTRET                    
019701*01  LEVLINK -COPY W213L322     -PRE LINK2- -RED LINK-AREA                
019801     EJECT                                                                
019901*--------------------------------------- LÄNKAREA TILL LEVERANS-          
020001*                                        PLANEREGISTER                    
020101*01  PLANLINK -COPY W213L323    -PRE LINK3-  -RED LINK-AREA               
020201     EJECT                                                                
020202*01  C2LINK   -COPY W213L325    -PRE LINK5-  -RED LINK-AREA               
020203     EJECT                                                                
020301*--------------------------------------- LÄNKAREA TILL WDB601 DC-         
020401*                                        LEVERANTÖRNR-REGISTER            
020501*01  DCLINK   -COPY W213L324    -PRE LINK4-  -RED LINK-AREA               
020601     EJECT                                                                
020901*--------------------------------------- PCB TILL LEVERANTÖRSREG          
021001*01  -COPY W0008   -PRE LEVA-                                             
021101         05  FILLER          PIC X.                                       
021201     EJECT                                                                
021301*--------------------------------------- PCB TILL ARTREG                  
021401*01  -COPY W0008   -PRE ARTC-                                             
021501         05  FILLER          PIC X.                                       
021601     EJECT                                                                
021701*01  -COPY W0008   -PRE INLB-                                             
021801         05  FILLER          PIC X.                                       
021901     EJECT                                                                
022001*01  -COPY W0008   -PRE WDB6-                                             
022101         05  FILLER          PIC X.                                       
022201     EJECT                                                                
022203*01  -COPY W0008   -PRE WDK7-                                             
022204         05  FILLER          PIC X.                                       
022205     EJECT                                                                
022301 PROCEDURE DIVISION USING LINK-AREA LEVA-PCB ARTC-PCB                     
022401                    INLB-PCB WDB6-PCB WDK7-PCB.                           
022501     SKIP2                                                                
022601     EVALUATE LINK-KDCALL                                                 
022701         WHEN OPEN-FILES                                                  
022801                PERFORM P-OPPNA-FILER                                     
022901         WHEN LAES-ARTIKELDATA                                            
023001                PERFORM A-LAES-ARTIKELDATA                                
023101         WHEN UPPD-ARTIKELDATA                                            
023201                PERFORM B-UPPD-ARTIKELDATA                                
023301         WHEN LAES-LEVERANTORSDATA                                        
023401                PERFORM D-LAES-LEVERANTORSDATA                            
023501         WHEN LAES-ART-INFO-PLAN                                          
023601                PERFORM E-LAES-ART-INFO-PLAN                              
023701         WHEN LAES-LEV-INFO-PLAN                                          
023801                PERFORM F-LAES-LEV-INFO-PLAN                              
023901         WHEN DELETE-SEGM3-4-PLAN                                         
024001                PERFORM G-DELETE-SEGM3-4-PLAN                             
024101         WHEN LAES-AVROP                                                  
024201                PERFORM H-LAES-AVROP                                      
024301         WHEN DELETE-AVROP                                                
024401                PERFORM I-DELETE-AVROP                                    
024501         WHEN REPLACE-AVROP                                               
024601                PERFORM J-REPLACE-AVROP                                   
024714         WHEN LAES-DC-SEGMENT                                             
024801                PERFORM K-LAES-DC-LEVNR-INFO                              
024901         WHEN LAES-SATSORDERNR                                            
025001                PERFORM L-LAES-SATSORDERNR                                
025101         WHEN INSERT-WDGX2204                                             
025201                PERFORM M-ISRT-WDGX2204                                   
025301         WHEN INSERT-WDGX2214                                             
025401                PERFORM N-ISRT-WDGX2214                                   
025501         WHEN INSERT-WDGX2302                                             
025601                PERFORM O-ISRT-WDGX2302                                   
025701         WHEN CLOSE-FILES                                                 
025801                PERFORM Q-STANG-FILER                                     
025901         WHEN OTHER                                                       
026001                MOVE NEJ TO LINK-FLJANEJ-ANROP                            
026101     END-EVALUATE                                                         
026201     MOVE +0 TO RETURN-CODE                                               
026301     GOBACK                                                               
026401     .                                                                    
026501     EJECT                                                                
026601 A-LAES-ARTIKELDATA SECTION.                                              
026602     MOVE 'A-LAES-ARTIKELDATA ' TO CURRENT-SECTION                        
026701******************************************************************        
026801*    LÄSER SEGMENT 01 OCH 11.    OM SEGMENT 1 SAKNAS             *        
026901*    SÄTTS FLJANEJ-ANROP = NEJ                                   *        
026902*    LEVBYTE EXT-TO-REF NOT OK IF JUST-PB EXIST FOR REFILL-XDC'S *        
027001******************************************************************        
027101     SKIP2                                                                
027201     MOVE LINK-IDARTNR TO W-IDARTNR                                       
027301     MOVE LOW-VALUE TO LINK-IOAREA-ARTIKELDATA                            
027401     MOVE NEJ TO LINK-FLJANEJ-ANROP                                       
027501                                                                          
027601     PERFORM IMS-GET-WLARTC01                                             
027701     IF SEGMENT-FINNS                                                     
027801       MOVE ART-IDLEVNR      TO LINK-IDLEVNR                              
027901       MOVE ART-KDPRODSL     TO LINK-KDPRODSL                             
028001                                                                          
028101       PERFORM IMS-GET-WLARTC11                                           
028201       MOVE CLAG-KDHF         TO LINK-KDHF                                
028301       MOVE CLAG-FLMANAT      TO LINK-FLMANAT                             
028401       MOVE CLAG-FLMANLT      TO LINK-FLMANLT                             
028501       MOVE CLAG-IDANSK       TO LINK-IDANSK                              
028601       MOVE CLAG-KVVECKOR-LT  TO LINK-KVVECKOR-LT                         
028701       MOVE CLAG-KVVECKOR-AT  TO LINK-KVVECKOR-AT                         
028801       MOVE CLAG-IDPLANGR-AG  TO LINK-IDPLANGR-AG                         
028901       MOVE CLAG-KDAVT        TO LINK-KDAVT                               
029001       MOVE CLAG-KDKSP        TO LINK-KDKSP                               
029101       MOVE CLAG-KDLPSP       TO LINK-KDLPSP                              
029201       MOVE CLAG-FLMANGK      TO LINK-FLMANGK                             
029301       MOVE CLAG-KDGK         TO LINK-KDGK                                
029401       MOVE CLAG-KDLTK        TO LINK-KDLTK                               
029501       MOVE CLAG-PRARTSTD     TO LINK-PRARTSTD                            
029601       MOVE CLAG-IDLEVNR-SHIP TO LINK-IDLEVNR-SHIP                        
029714       MOVE CLAG-IDDC-REF     TO LINK-IDDC-REF                            
029715       MOVE CLAG-KDERS        TO LINK-KDERS                               
029716                                                                          
029718       PERFORM AA-CHECK-PBREF-JUST-FOR-XDC                                
029780                                                                          
029801       MOVE JA                TO LINK-FLJANEJ-ANROP                       
029901     END-IF                                                               
030001     .                                                                    
030101     EJECT                                                                
030102 AA-CHECK-PBREF-JUST-FOR-XDC  SECTION.                                    
030103     MOVE 'AA-CHECK-PBREF-JUST-FOR-XDC' TO CURRENT-SECTION                
030104                                                                          
030105     MOVE NEJ TO LINK-FLAGGA-KVPB-JUST-XDC                                
030106                                                                          
030107     PERFORM IMS-GU-WDK701                                                
030108                                                                          
030109     IF SEGMENT-FINNS                                                     
030110       PERFORM IMS-GNP-WDK711-REF                                         
030111       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
030112                    (LINK-FLAGGA-KVPB-JUST-XDC = JA)                      
030113                                                                          
030115         MOVE SLAG-IDDC   TO W-IDDC                                       
030116         PERFORM IMS-GNP-WDK727                                           
030117                                                                          
030118         IF SEGMENT-FINNS                                                 
030120           IF (PROG-KVPB-JUST(1) > ZERO) OR                               
030121              (PROG-KVPB-JUST(2) > ZERO)                                  
030122                                                                          
030124              MOVE JA  TO LINK-FLAGGA-KVPB-JUST-XDC                       
030125           END-IF                                                         
030126         END-IF                                                           
030127                                                                          
030128         PERFORM IMS-GNP-WDK711-REF                                       
030129                                                                          
030130       END-PERFORM                                                        
030131     END-IF                                                               
030132                                                                          
030133     .                                                                    
030140     EJECT                                                                
030201 B-UPPD-ARTIKELDATA SECTION.                                              
030202     MOVE 'B-UPPD-ARTIKELDATA '  TO CURRENT-SECTION                       
030301******************************************************************        
030401*    UPPDATERAR INFORMATION OM DEN NYA LEVERANTÖREN PÅ           *        
030506*    SEGMENT 01 OCH 11.                                          *        
030606*---                                                             *        
030706*    VID BYTE FRÅN EXTERN LEVERANTÖR TILL REFILL PÅ CDC:         *        
030808*    UPPDATERA ÄVEN WDD201 OCH WDK712.                           *        
030906*    LÄGG UPP REFILL-SEGMENT FÖR CDC PÅ WDK629.                  *        
030907*---                                                             *        
030908*    VID BYTE FRÅN REFILL TILL EXTERN LEVERANTÖR PÅ CDC:         *        
030909*    UPPDATERA WDK611 OCH TAG BORT WDK629.                       *        
030910*    TAG BORT REFILL-ORDERN FÖR CDC PÅ WDE301.                   *        
031006******************************************************************        
031106     SKIP2                                                                
031206     PERFORM IMS-GET-GHU-WLARTC01                                         
031306     PERFORM S01-NOLLA-U34-AREA                                           
031406     MOVE REPL         TO U34-ATGARD                                      
031506     MOVE 'WDK601'     TO U34-IDSEGM                                      
031606     MOVE ART-IDARTNR  TO U34-IDARTNR                                     
031706     MOVE LINK-IDLEVNR TO U34-IDLEVNR                                     
031806     WRITE U34-POST  FROM U34-AREA                                        
031906*****PERFORM IMS-REPLACE-ARTC-01                                          
032006                                                                          
032106     PERFORM IMS-GET-WLARTC11-FIRST                                       
032206                                                                          
032306     PERFORM S01-NOLLA-U34-AREA                                           
032406     MOVE REPL             TO U34-ATGARD                                  
032506     MOVE 'WDK611'         TO U34-IDSEGM                                  
032606                                                                          
032706     MOVE LINK-KDGK        TO U34-KDGK                                    
032806     MOVE LINK-IDARTNR     TO W-IDARTNR        U34-IDARTNR                
032906     MOVE LINK-KVDAGAR-TT  TO U34-KVDAGAR-TT                              
033006     MOVE LINK-KVVECKOR-LT TO U34-KVVECKOR-LT                             
033106     MOVE LINK-KVVECKOR-AT TO U34-KVVECKOR-AT                             
033206     MOVE LINK-KDAVT       TO U34-KDAVT                                   
033306     MOVE LINK-KDKSP       TO U34-KDKSP                                   
033406     MOVE LINK-IDANSK      TO U34-IDANSK                                  
033506     MOVE LINK-IDPLANGR-AG TO U34-IDPLANGR-AG                             
033606     MOVE LINK-KDLPSP      TO U34-KDLPSP                                  
033706     MOVE LINK-IDLEVNR-SHIP TO U34-IDLEVNR-SHIP                           
033806     MOVE LINK-IDDC-REF    TO U34-IDDC-REF                                
033915     MOVE LINK-IDLANDX2    TO U34-IDLANDX2                                
034007     WRITE U34-POST  FROM U34-AREA                                        
034107*****PERFORM IMS-REPLACE-ARTC-11                                          
034207                                                                          
034307     IF CLAG-IDDC-REF = SPACE                                             
034407     AND (LINK-IDDC-REF NOT = SPACE)                                      
035207                                                                          
035307       PERFORM S01-NOLLA-U34-AREA                                         
035407       MOVE ISRT             TO U34-ATGARD                                
035507       MOVE 'WDK629'         TO U34-IDSEGM                                
035608       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
035708       MOVE LINK-IDDC-REF    TO U34-IDDC-REF                              
035911       MOVE LINK-KVPB-PLAN   TO U34-KVPB-PLAN                             
036011                                                                          
036111       MOVE +1 TO IX                                                      
036211       PERFORM UNTIL IX  > +12                                            
036311         MOVE LINK-RESEASON-PLAN (IX)                                     
036411                                TO U34-RESEASON-PLAN (IX)                 
036511                                                                          
036611         ADD +1 TO IX                                                     
036711       END-PERFORM                                                        
036811                                                                          
036911       WRITE U34-POST  FROM U34-AREA                                      
037015                                                                          
037115       PERFORM S01-NOLLA-U34-AREA                                         
037215       MOVE REPL             TO U34-ATGARD                                
037315       MOVE 'WDD201'         TO U34-IDSEGM                                
037415       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
037515                                                                          
037615       WRITE U34-POST  FROM U34-AREA                                      
037715                                                                          
037815       PERFORM S01-NOLLA-U34-AREA                                         
037915       MOVE DLET             TO U34-ATGARD                                
038015       MOVE 'WDG202'         TO U34-IDSEGM                                
038115       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
038215                                                                          
038315       WRITE U34-POST  FROM U34-AREA                                      
038316                                                                          
038317       PERFORM S01-NOLLA-U34-AREA                                         
038318       MOVE REPL             TO U34-ATGARD                                
038319       MOVE 'WDK626'         TO U34-IDSEGM                                
038320       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
038321                                                                          
038330       WRITE U34-POST  FROM U34-AREA                                      
038411     END-IF                                                               
038412                                                                          
038413     IF (CLAG-IDDC-REF NOT = SPACE)                                       
038414     AND LINK-IDDC-REF = SPACE                                            
038415                                                                          
038416       PERFORM S01-NOLLA-U34-AREA                                         
038417       MOVE DLET             TO U34-ATGARD                                
038418       MOVE 'WDE301'         TO U34-IDSEGM                                
038419       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
038420                                                                          
038421       WRITE U34-POST  FROM U34-AREA                                      
038422                                                                          
038423       PERFORM S01-NOLLA-U34-AREA                                         
038424       MOVE DLET             TO U34-ATGARD                                
038425       MOVE 'WDK629'         TO U34-IDSEGM                                
038426       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
038427       MOVE CLAG-IDDC-REF    TO U34-IDDC-REF                              
038428                                                                          
038429       WRITE U34-POST  FROM U34-AREA                                      
038430                                                                          
038431       PERFORM S01-NOLLA-U34-AREA                                         
038432       MOVE REPL             TO U34-ATGARD                                
038433       MOVE 'WDK626'         TO U34-IDSEGM                                
038434       MOVE LINK-IDARTNR     TO U34-IDARTNR                               
038436                                                                          
038437       WRITE U34-POST  FROM U34-AREA                                      
038440     END-IF                                                               
038511     .                                                                    
038611     EJECT                                                                
038711 D-LAES-LEVERANTORSDATA SECTION.                                          
038712     MOVE 'D-LAES-LEVERANTORSDATA '  TO CURRENT-SECTION                   
038811******************************************************************        
038911*    LÄSER SEGMENT 1 OCH 7. OM SEGMENT 1 SAKNAS SÄTTS            *        
039011*    FLJANEJ-ANROP = NEJ                                         *        
039111******************************************************************        
039211     SKIP2                                                                
039311     MOVE LINK2-IDLEVNR      TO W-IDLEVNR                                 
039411     MOVE LOW-VALUE TO LINK2-IOAREA-LEVERANTOR                            
039511     MOVE NEJ TO LINK2-FLJANEJ-ANROP                                      
039611                                                                          
039711     PERFORM IMS-GET-WLLEVA01                                             
039811     IF SEGMENT-FINNS                                                     
039911                                                                          
040011       MOVE LEV01-LEV-KDLEVTYP      TO LINK2-KDLEVTYP                     
040111       MOVE LEV01-LEV-IDLEVNR-MOTSV TO LINK2-IDLEVNR-MOTSV                
040211       MOVE 1 TO IX                                                       
040311       PERFORM UNTIL IX > MAX-ANT-ANSKAFFARE                              
040411         MOVE LEV01-LEV-IDANSK-PG(IX) TO LINK2-IDANSK-PG(IX)              
040511         ADD 1 TO IX                                                      
040611       END-PERFORM                                                        
040711*                                                                         
040811* GET THE BELOW DATA FROM SHIP-SUPPLIER INSTEAD OF MFG-SUPPLIER           
040911       MOVE LINK2-IDLEVNR-SHIP TO W-IDLEVNR                               
041011       PERFORM IMS-GET-WLLEVA01                                           
041111       IF SEGMENT-FINNS                                                   
041211         MOVE JA TO LINK2-FLJANEJ-ANROP                                   
041311         MOVE LEV01-LEV-KDGK          TO LINK2-KDGK                       
041411         MOVE LEV01-LEV-KVDAGAR-TTC1  TO LINK2-KVDAGAR-TTC1               
041511         MOVE LEV01-LEV-KVDAGAR-TTC2  TO LINK2-KVDAGAR-TTC2               
041611         MOVE LEV01-LEV-KVVECKOR-LT   TO LINK2-KVVECKOR-LT                
041711         MOVE LEV01-LEV-KVVECKOR-AT   TO LINK2-KVVECKOR-AT                
041811       END-IF                                                             
041911     END-IF                                                               
042011     .                                                                    
042111     EJECT                                                                
042211 E-LAES-ART-INFO-PLAN SECTION.                                            
042212     MOVE 'E-LAES-ART-INFO-PLAN '  TO CURRENT-SECTION                     
042311*****************************************************************         
042411*    LÄSER SEGMENT 1 I LEVERANSPLANEREGISTER                    *         
042511*****************************************************************         
042611     SKIP2                                                                
042711     MOVE LINK3-IDARTNR TO W-IDARTNR                                      
042811     MOVE LOW-VALUE     TO LINK3-IOAREA                                   
042911     MOVE NEJ           TO LINK3-FLJANEJ-ANROP                            
043011                                                                          
043111     MOVE W-IDARTNR     TO W-IDARTNR-D9                                   
043211     MOVE WC-CDC-SE     TO W-IDDC-D9                                      
043311     PERFORM IMS-GET-WLINLB01                                             
043411     IF  SEGMENT-FINNS                                                    
043511       MOVE JA TO LINK3-FLJANEJ-ANROP                                     
043611     END-IF                                                               
043711     .                                                                    
043811     EJECT                                                                
043911 F-LAES-LEV-INFO-PLAN SECTION.                                            
043912     MOVE 'F-LAES-LEV-INFO-PLAN '  TO CURRENT-SECTION                     
044011*****************************************************************         
044111*    LÄSER SEGMENT 2 I LEVERANSPLANEREGISTER                    *         
044211*****************************************************************         
044311     SKIP2                                                                
044411     MOVE LINK3-IDARTNR TO W-IDARTNR                                      
044511     MOVE LOW-VALUE     TO LINK3-IOAREA                                   
044611     MOVE NEJ           TO LINK3-FLJANEJ-ANROP                            
044711                                                                          
044811     PERFORM IMS-GET-WLINLB11                                             
044911     IF  SEGMENT-FINNS                                                    
045011       MOVE JA             TO LINK3-FLJANEJ-ANROP                         
045111       MOVE PLAN02-KVBR    TO LINK3-KVBR                                  
045211       MOVE PLAN02-IDLEVNR TO LINK3-IDLEVNR                               
045311     END-IF                                                               
045411     .                                                                    
045511     EJECT                                                                
045611 G-DELETE-SEGM3-4-PLAN SECTION.                                           
045612     MOVE 'G-DELETE-SEGM3-4-PLAN '  TO CURRENT-SECTION                    
045711*****************************************************************         
045811*    DELETAR SEGMENT 3 OCH 4 I LEVERANSPLANEREGISTER            *         
045911*****************************************************************         
046011     SKIP2                                                                
046111     PERFORM S01-NOLLA-U34-AREA                                           
046211     MOVE DLET          TO U34-ATGARD                                     
046311     MOVE LINK3-IDARTNR TO W-IDARTNR U34-IDARTNR                          
046411     MOVE LINK3-IDLEVNR TO W-IDLEVNR U34-IDLEVNR                          
046511                                                                          
046611     PERFORM IMS-GET-WLINLB22                                             
046711     IF  SEGMENT-FINNS                                                    
046811        MOVE 'WDD904'   TO U34-IDSEGM                                     
046911        WRITE U34-POST  FROM U34-AREA                                     
047011*******PERFORM IMS-DELETE-INLB                                            
047111     END-IF                                                               
047211     .                                                                    
047311     EJECT                                                                
047411 H-LAES-AVROP SECTION.                                                    
047412     MOVE 'H-LAES-AVROP '  TO CURRENT-SECTION                             
047511*****************************************************************         
047611*    LÄSER SEGMENT 5       I LEVERANSPLANEREGISTER              *         
047711*****************************************************************         
047811     SKIP2                                                                
047911     MOVE LINK3-IDARTNR TO W-IDARTNR                                      
048011     MOVE LINK3-IDLEVNR TO W-IDLEVNR                                      
048111     MOVE NEJ           TO LINK3-FLJANEJ-ANROP                            
048211     MOVE LOW-VALUE     TO LINK3-IOAREA                                   
048311                                                                          
048411     PERFORM IMS-GET-WLINLB23                                             
048511     IF  SEGMENT-FINNS                                                    
048611       MOVE PLAN05-KVAVROP     TO LINK3-KVAVROP                           
048711       MOVE PLAN05-DAAVROP-AVS TO LINK3-DAAVROP-AVS                       
048811       MOVE PLAN05-TILEVDAG    TO LINK3-TILEVDAG                          
048911       MOVE PLAN05-KDAVROP     TO LINK3-KDAVROP                           
049011       MOVE JA                 TO LINK3-FLJANEJ-ANROP                     
049111     END-IF                                                               
049211     .                                                                    
049311     EJECT                                                                
049411 I-DELETE-AVROP SECTION.                                                  
049412     MOVE 'I-DELETE-AVROP '   TO CURRENT-SECTION                          
049511*****************************************************************         
049611*    DELETAR SEGMENT 5 I LEVERANSPLANEREGISTER                  *         
049711*****************************************************************         
049811     SKIP2                                                                
049911     PERFORM S01-NOLLA-U34-AREA                                           
050011     MOVE DLET              TO U34-ATGARD                                 
050111     MOVE 'WDD905'          TO U34-IDSEGM                                 
050211     MOVE LINK3-IDARTNR     TO U34-IDARTNR                                
050311     MOVE LINK3-IDLEVNR     TO U34-IDLEVNR                                
050411     MOVE LINK3-DAAVROP-AVS TO U34-DAAVROP-AVS                            
050511     MOVE LINK3-TILEVDAG    TO U34-TILEVDAG                               
050611     MOVE LINK3-KDAVROP     TO U34-KDAVROP                                
050711                                                                          
050811*****INGEN POSITIONERINGS-LÄSNING BEHÖVS ***                              
050911*****PERFORM IMS-GET-WLINLB23-KVAL                                        
051011                                                                          
051111     WRITE U34-POST  FROM U34-AREA                                        
051211*****PERFORM IMS-DELETE-INLB                                              
051311     MOVE JA TO LINK3-FLJANEJ-ANROP                                       
051411     .                                                                    
051511     EJECT                                                                
051611 J-REPLACE-AVROP SECTION.                                                 
051612     MOVE 'J-REPLACE-AVROP '  TO CURRENT-SECTION                          
051711*****************************************************************         
051811*    ÄNDRAR SEGMENT 5 I LEVERANSPLANEREGISTER                   *         
051911*****************************************************************         
052011     SKIP2                                                                
052111     PERFORM S01-NOLLA-U34-AREA                                           
052211     MOVE REPL              TO U34-ATGARD                                 
052311     MOVE 'WDD905'          TO U34-IDSEGM                                 
052411     MOVE LINK3-IDARTNR     TO U34-IDARTNR                                
052511     MOVE LINK3-IDLEVNR     TO U34-IDLEVNR                                
052611     MOVE LINK3-DAAVROP-AVS TO U34-DAAVROP-AVS                            
052711     MOVE LINK3-TILEVDAG    TO U34-TILEVDAG                               
052811     MOVE LINK3-KDAVROP     TO U34-KDAVROP                                
052911                                                                          
053011*****INGEN POSITIONERINGS-LÄSNING BEHÖVS ***                              
053111*****PERFORM IMS-GET-WLINLB23-KVAL                                        
053211     MOVE LINK3-KVAVROP TO PLAN05-KVAVROP  U34-KVAVROP                    
053311                                                                          
053411     WRITE U34-POST  FROM U34-AREA                                        
053511*****PERFORM IMS-REPLACE-INLB                                             
053611     EJECT                                                                
053711     .                                                                    
053812 K-LAES-DC-LEVNR-INFO SECTION.                                            
053911     MOVE 'K-LAES-DC-LEVNR-INFO '  TO CURRENT-SECTION                     
054011*****************************************************************         
054111*    KOLLAR OM LEVERANTÖREN ÄR ETT REFILL-DC.                   *         
054211*****************************************************************         
054311     SKIP2                                                                
054411     MOVE LINK4-IDLEVNR TO W-IDLEVNR-B6                                   
054511     MOVE LOW-VALUE     TO LINK4-IOAREA                                   
054611     MOVE NEJ           TO LINK4-FLJANEJ-ANROP                            
054711                                                                          
054811     PERFORM IMS-GU-WDB601                                                
054911     IF SEGMENT-FINNS                                                     
054912       MOVE JA TO LINK4-FLJANEJ-ANROP                                     
055111       MOVE DCS-IDDC       TO LINK4-IDDC-REF                              
055211       MOVE DCS-IDLANDX2   TO LINK4-IDLANDX2                              
055311     ELSE                                                                 
055411       MOVE SPACE          TO LINK4-IDDC-REF                              
055511                              LINK4-IDLANDX2                              
055611     END-IF                                                               
055911     .                                                                    
056011     EJECT                                                                
056211 L-LAES-SATSORDERNR SECTION.                                              
056311*****************************************************************         
056411*    LÄSER SEGMENT 7 LEVERANSPLANEREG. OM SEGMENTET SAKNAS      *         
056511*    SÄTTS LINK3-FLJANEJ-ANROP TILL NEJ.                        *         
056611*    (MEDFÖR ATT IDORDNR I LÄNKAREAN INTE ALLS ANVÄNDS)         *         
056711*****************************************************************         
056811     SKIP2                                                                
056911     MOVE LINK3-IDARTNR TO W-IDARTNR                                      
057011     MOVE LINK3-IDLEVNR TO W-IDLEVNR                                      
057111     MOVE LINK3-DAAVROP-AVS TO W-DAAVROP                                  
057211     MOVE LINK3-TILEVDAG    TO W-TILEVDAG                                 
057212     MOVE LINK3-KDAVROP     TO W-KDAVROP                                  
057311                                                                          
057411     PERFORM IMS-GET-WLINLB32                                             
057511     IF SEGMENT-FINNS                                                     
057611       MOVE JA TO LINK3-FLJANEJ-ANROP                                     
057711     ELSE                                                                 
057811       MOVE NEJ TO LINK3-FLJANEJ-ANROP                                    
057911     END-IF                                                               
058011     MOVE ZERO TO LINK3-IDORDNR                                           
058111     .                                                                    
058211     EJECT                                                                
058311 M-ISRT-WDGX2204 SECTION.                                                 
058312     MOVE 'M-ISRT-WDGX2204 '  TO CURRENT-SECTION                          
058411     SKIP3                                                                
058511     MOVE SPACE             TO U32-AREA                                   
058611     MOVE LINK5-IDARTNR     TO 2204-IDARTNR                               
058711     MOVE LINK5-KDLPORS     TO 2204-KDLPORS                               
058811     MOVE '2204'            TO 2204-IDHTYP                                
058913     MOVE LINK5-IDDC        TO 2204-IDDC                                  
059013                                                                          
059113     WRITE U32-POST FROM U32-AREA                                         
059213     .                                                                    
059313     EJECT                                                                
059413 N-ISRT-WDGX2214 SECTION.                                                 
059414     MOVE 'N-ISRT-WDGX2214 '  TO CURRENT-SECTION                          
059513     SKIP3                                                                
059613     MOVE SPACE             TO U32-AREA                                   
059713     MOVE LINK5-IDARTNR     TO 2213-IDARTNR                               
059813     MOVE '2213'            TO 2213-IDHTYP                                
059913                                                                          
060013     WRITE U32-POST FROM U32-AREA                                         
060113     .                                                                    
060213     EJECT                                                                
060313 O-ISRT-WDGX2302 SECTION.                                                 
060314     MOVE 'O-ISRT-WDGX2302 '  TO CURRENT-SECTION                          
060413     SKIP3                                                                
060513     MOVE SPACE          TO  U32-AREA                                     
060613     MOVE LINK5-IDARTNR  TO 2302-IDARTNR-SATS                             
060713     MOVE LINK5-IDLEVNR  TO 2302-IDLEVNR                                  
060813     MOVE '2301'         TO 2302-IDHTYP                                   
060913                                                                          
061013     WRITE U32-POST FROM U32-AREA                                         
061213     .                                                                    
061313     EJECT                                                                
061413 P-OPPNA-FILER   SECTION.                                                 
061414     MOVE 'P-OPPNA-FILER '  TO CURRENT-SECTION                            
061513     SKIP3                                                                
061613     OPEN OUTPUT W21334                                                   
061713                 W21332                                                   
061813     .                                                                    
061913     EJECT                                                                
062013 Q-STANG-FILER   SECTION.                                                 
062014     MOVE 'Q-STANG-FILER '  TO CURRENT-SECTION                            
062113     SKIP3                                                                
062213     CLOSE       W21334                                                   
062313                 W21332                                                   
062413     .                                                                    
062513     EJECT                                                                
062613 S01-NOLLA-U34-AREA SECTION.                                              
062713     SKIP3                                                                
062813     MOVE SPACE TO U34-AREA                                               
062913                   U34-IDLEVNR                                            
062914                   U34-IDDC-REF                                           
062915                   U34-IDLANDX2                                           
063013     MOVE ZERO  TO U34-IDARTNR                                            
063113                   U34-DAAVROP-AVS                                        
063213                   U34-TILEVDAG                                           
063313                   U34-KVAVROP                                            
063413                   U34-KDGK                                               
063513                   U34-KVDAGAR-TT                                         
063613                   U34-KVVECKOR-LT                                        
063713                   U34-KVVECKOR-AT                                        
063813                   U34-KDAVT                                              
063913                   U34-KDKSP                                              
064013                   U34-IDANSK                                             
064113                   U34-IDPLANGR-AG                                        
064213                   U34-KDLPSP                                             
064214                   U34-KDAVROP                                            
064215                   U34-KVPB-PLAN                                          
064216                                                                          
064217     MOVE +1 TO IX                                                        
064218     PERFORM UNTIL IX  > +12                                              
064219       MOVE ZERO    TO U34-RESEASON-PLAN (IX)                             
064221                                                                          
064222       ADD +1 TO IX                                                       
064223     END-PERFORM                                                          
064230     .                                                                    
064313     EJECT                                                                
064413******************************************************************        
064513*                                                                *        
064613*    I M S   S E C T I O N E R                                   *        
064713*                                                                *        
064813******************************************************************        
064913     SKIP3                                                                
065113 IMS-GET-GHU-WLARTC01 SECTION.                                            
065114     MOVE 'IMS-GET-GHU-WLARTC01 '  TO DBS-SECTION                         
065213     SKIP1                                                                
065313     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
065413              DELIMITED BY SIZE INTO SSA1                                 
065513     MOVE '  ' TO GODK-STATUSKODER                                        
065613     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-01 SSA1                  
065713     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
065813     PERFORM IMS-STATUSKONTROLL                                           
065913     SKIP3                                                                
066013     .                                                                    
066113 IMS-GET-WLARTC01 SECTION.                                                
066114     MOVE 'IMS-GET-WLARTC01 '  TO DBS-SECTION                             
066213     SKIP1                                                                
066313     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X '&KDERS    ='               
066413              W-KDERS-0-X ')'                                             
066513              DELIMITED BY SIZE INTO SSA1                                 
066613     MOVE '  GE' TO GODK-STATUSKODER                                      
066713     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
066813     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066913     PERFORM IMS-STATUSKONTROLL                                           
067013     SKIP3                                                                
067113     .                                                                    
067213 IMS-GET-WLARTC11 SECTION.                                                
067214     MOVE 'IMS-GET-WLARTC11 '  TO DBS-SECTION                             
067313     SKIP1                                                                
067413     MOVE 'WLARTC11 ' TO SSA2                                             
067513     MOVE '  ' TO GODK-STATUSKODER                                        
067613     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA2                 
067713     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
067813     PERFORM IMS-STATUSKONTROLL                                           
067913     SKIP3                                                                
068013     .                                                                    
068113 IMS-GET-WLARTC11-FIRST SECTION.                                          
068114     MOVE 'IMS-GET-WLARTC11-FIRST '  TO DBS-SECTION                       
068213     SKIP1                                                                
068313     MOVE 'WLARTC11*F' TO SSA2                                            
068413     MOVE '  ' TO GODK-STATUSKODER                                        
068513     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA2                 
068613     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
068713     PERFORM IMS-STATUSKONTROLL                                           
068813     .                                                                    
068913     EJECT                                                                
068914 IMS-GU-WDK701 SECTION.                                                   
068915     MOVE 'IMS-GU-WDK701 '  TO DBS-SECTION                                
068916                                                                          
068919     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
068920          DELIMITED BY SIZE INTO SSA1                                     
068941     MOVE '  GE' TO GODK-STATUSKODER                                      
068942     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701  SSA1                   
068943     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
068944     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069010     EJECT                                                                
069011 IMS-GNP-WDK711-REF  SECTION.                                             
069012     MOVE 'IMS-GNP-WDK711-REF ' TO DBS-SECTION                            
069013                                                                          
069014     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
069015          DELIMITED BY SIZE INTO SSA1                                     
069018     MOVE '  GE'       TO GODK-STATUSKODER                                
069019     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
069020     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
069021     PERFORM IMS-STATUSKONTROLL                                           
069022     .                                                                    
069023     SKIP3                                                                
069024 IMS-GNP-WDK727 SECTION.                                                  
069025     MOVE 'IMS-GNP-WDK727 '  TO DBS-SECTION                               
069026                                                                          
069027     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
069028          DELIMITED BY SIZE INTO SSA1                                     
069029     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
069030          DELIMITED BY SIZE INTO SSA2                                     
069031     MOVE 'WDK727  '        TO SSA3                                       
069032     MOVE '  GE'            TO GODK-STATUSKODER                           
069033     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1 SSA2              
069034                                                        SSA3              
069035     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
069036     PERFORM IMS-STATUSKONTROLL                                           
069037     .                                                                    
069038     EJECT                                                                
069039 IMS-GET-WLLEVA01 SECTION.                                                
069040     MOVE 'IMS-GET-WLLEVA01 '  TO DBS-SECTION                             
069113     SKIP1                                                                
069213     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
069313              DELIMITED BY SIZE INTO SSA1                                 
069413     MOVE '  GE' TO GODK-STATUSKODER                                      
069513     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WDF101 SSA1                    
069613     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
069713     PERFORM IMS-STATUSKONTROLL                                           
069813     .                                                                    
069913     EJECT                                                                
070013 IMS-GET-WLINLB11 SECTION.                                                
070014     MOVE 'IMS-GET-WLINLB11 '  TO DBS-SECTION                             
070113     SKIP1                                                                
070213     MOVE 'WLINLB11 ' TO SSA1                                             
070313     MOVE '  GE' TO GODK-STATUSKODER                                      
070413     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-WDD902 SSA1                  
070513     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
070613     PERFORM IMS-STATUSKONTROLL                                           
070713     SKIP3                                                                
070813     .                                                                    
072013 IMS-GET-WLINLB01 SECTION.                                                
072014     MOVE 'IMS-GET-WLINLB01 ' TO DBS-SECTION                              
072113     SKIP1                                                                
072213     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
072313            DELIMITED BY SIZE INTO SSA1                                   
072413     MOVE '  GE' TO GODK-STATUSKODER                                      
072513     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-WDD901 SSA1                   
072613     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
072713     PERFORM IMS-STATUSKONTROLL                                           
072813     .                                                                    
072913     EJECT                                                                
073013 IMS-GET-WLINLB22 SECTION.                                                
073014     MOVE 'IMS-GET-WLINLB22 '  TO DBS-SECTION                             
073113     SKIP1                                                                
073213     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
073313            DELIMITED BY SIZE INTO SSA1                                   
073413     MOVE 'WLINLB22' TO SSA2                                              
073513     MOVE '  GE' TO GODK-STATUSKODER                                      
073613     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-WDD904 SSA1 SSA2             
073713     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
073813     PERFORM IMS-STATUSKONTROLL                                           
073913     .                                                                    
074013     EJECT                                                                
074113 IMS-GET-WLINLB23 SECTION.                                                
074114     MOVE 'IMS-GET-WLINLB23 '  TO DBS-SECTION                             
074213     SKIP1                                                                
074313     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
074413            DELIMITED BY SIZE INTO SSA1                                   
074513     MOVE 'WLINLB23 ' TO SSA2                                             
074613     MOVE '  GE' TO GODK-STATUSKODER                                      
074713     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-WDD905 SSA1 SSA2             
074813     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
074913     PERFORM IMS-STATUSKONTROLL                                           
075013     .                                                                    
075113     EJECT                                                                
075213*IMS-GET-WLINLB23-KVAL SECTION.                                           
075214*    MOVE 'IMS-GET-WLINLB23-KVAL '  TO DBS-SECTION                        
075313*    SKIP1                                                                
075413*    STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
075513*           DELIMITED BY SIZE INTO SSA1                                   
075613*    STRING 'WLINLB23*F(WDD905KY =' W-WDD905KY-X ')'                      
075713*           DELIMITED BY SIZE INTO SSA2                                   
075813*    MOVE '  GE' TO GODK-STATUSKODER                                      
075913*    CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-WDD905 SSA1 SSA2             
076013*    MOVE INLB-STATUS-CODE TO STATUS-WS                                   
076113*    PERFORM IMS-STATUSKONTROLL                                           
076213*                                                                         
076313*    .                                                                    
076413     EJECT                                                                
076513 IMS-GET-WLINLB32 SECTION.                                                
076514     MOVE 'IMS-GET-WLINLB32 '  TO DBS-SECTION                             
076613     SKIP1                                                                
076713     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
076813            DELIMITED BY SIZE INTO SSA1                                   
076914     STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X                            
076916                      '&KDAVROP  =' W-KDAVROP-X ')'                       
077013            DELIMITED BY SIZE INTO SSA2                                   
077113     MOVE 'WLINLB32 '  TO SSA3                                            
077213     MOVE '  GE' TO GODK-STATUSKODER                                      
077313     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-WDD907 SSA1 SSA2 SSA3         
077413     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
077513     PERFORM IMS-STATUSKONTROLL                                           
077613     .                                                                    
077713     EJECT                                                                
077813 IMS-GU-WDB601 SECTION.                                                   
077913     MOVE 'IMS-GU-WDB601 '  TO DBS-SECTION                                
078013                                                                          
078113     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-B6-X ')'                      
078213          DELIMITED BY SIZE INTO SSA1                                     
078313     MOVE '  GE' TO GODK-STATUSKODER                                      
078413     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
078513     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
078613     PERFORM IMS-STATUSKONTROLL                                           
078713     .                                                                    
078813     EJECT                                                                
078913 IMS-STATUSKONTROLL SECTION.                                              
079013     SKIP1                                                                
079113     SET STATUS-IX TO 1                                                   
079213     SEARCH GODK-STATUS AT END CALL FELLOG                                
079313       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079413       CONTINUE                                                           
079511     END-SEARCH                                                           
080001     .                                                                    

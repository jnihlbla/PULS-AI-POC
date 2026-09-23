000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2214010.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-WRITTEN.               JAN 1978.                                    
000500     SKIP3                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2214000 OCH SKÖTER            
000900*        SAMTLIGA IMS-CALL MOT DATABASERNA:                               
001000*                                         FYSISKT        LOGISKT          
001100*                 ARTIKELREG              WDK6           WLARTC           
001200*                 LEVPLAN                 WDD9           WLINLB           
001300*                 ARTIKELREG              WDK9           WLARTM           
001400*                 ARTIKELREG              WDK7           ----             
001500*                (LEV.PL.FÖRSLAGSKÖ       WDD6  I PGM W2214A00)           
001600*                 PROGRAMMET LÄSER        WDB6                            
001700*                                                                         
001800*    ÄNDRAD FUNKTION:                                                     
001900*        ETRACKER 4820410. DO NOT INCLUDE OVERSTOCK AT MICRO-LDC          
002000*        TILLKOMMER WDB601-LÄSNING.                                       
002100*                                                                         
002200*        2015-11-24                                                       
002300*        E'TRACKER 10243132 KINA EXPORT 2015                              
002400*                                                                         
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP1                                                                
003200*    -COPY WY2000W3                                                       
003300     SKIP3                                                                
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600*    -COPY WY2000W9                                                       
003700     SKIP3                                                                
003800*01  -COPY WWDCKONS                                                       
003900                                                                          
004000 01  RKOD                    PIC S9(4)  VALUE +0     COMP SYNC.           
004100                                                                          
004200 01  FELTEXT.                                                             
004300     03  FILLER              PIC X(8)    VALUE 'FELTEXT'.                 
004400     03  FELTEXT-STR         PIC X(72)   VALUE SPACE.                     
004500                                                                          
004600 01  CURRENT-SECTION         PIC X(32)   VALUE SPACE.                     
004610 01  DBS-SECTION             PIC X(32)   VALUE SPACE.                     
004700                                                                          
004800                                                                          
004900 01  W-DAGENS-AAAAMMDD       PIC 9(8)    VALUE ZERO.                      
004910 01  WS-DAYS-TIDATE1-AAVVD   PIC 9(5)    VALUE ZERO.                      
005000                                                                          
005100 01  FILLER                  PIC X(16)   VALUE 'DIVERSE'.                 
005200 01  DIVERSE.                                                             
005300     03  WS-KVFRYSTIPLUS1    PIC S9(3)   VALUE +0   COMP-3.               
005400     03  WS-TIFINLV-AAVV     PIC S9(5)   VALUE +0   COMP-3.               
005500     03  WS-ANT-VECKOR       PIC S9(3)   VALUE +0   COMP-3.               
005600     03  W-KVPB-SDC-TOT      PIC S9(6)V9(1)          COMP-3.              
005700     03  W-KVPB-SDC-EJ-DIR   PIC S9(6)V9(1)          COMP-3.              
005800     03  W-TILLG-SDC         PIC S9(7)               COMP-3.              
005900     03  W-OVERLAGER-SDC     PIC S9(7)               COMP-3.              
006000     03  WS-PRARTBES         PIC X     VALUE 'N'.                         
006100     03  W-KVOKS             PIC S9(7) VALUE ZERO    COMP-3.              
006200     03  W-KDPRODSL          PIC 9(2).                                    
006300     03  FILLER REDEFINES W-KDPRODSL.                                     
006400         05  FILLER          PIC 9.                                       
006500         05  W-IDPROD        PIC 9.                                       
006600     03  DAGENS-AAVV         PIC 9(4)     VALUE ZERO.                     
006700     03  FILLER REDEFINES DAGENS-AAVV.                                    
006800         05  DAGENS-AA       PIC 9(2).                                    
006900         05  DAGENS-VV       PIC 9(2).                                    
007000     03  TIFINLV-AAVVD       PIC 9(5)     VALUE ZERO.                     
007100     03  FILLER REDEFINES TIFINLV-AAVVD.                                  
007200         05  TIFINLV-AA      PIC 9(2).                                    
007300         05  TIFINLV-VV      PIC 9(2).                                    
007400         05  FILLER          PIC 9(1).                                    
007500     03  VECKO-DIFF          PIC 9(5)     VALUE ZERO.                     
007600     03  SUM-RETUR           PIC S9(7)    VALUE ZERO COMP-3.              
007700     03  WS-DAAVROP-AVS      PIC 9(6).                                    
007800     03  FILLER  REDEFINES WS-DAAVROP-AVS.                                
007900         05  WS-DAAVROP-SS   PIC 9(2).                                    
008000         05  WS-DAAVROP-AAVV PIC 9(4).                                    
008500     03  DEFINITIV           PIC S9      VALUE +1  COMP-3.                
008600                                                                          
008700 01  WS-TIAAVV               PIC 9(04)   VALUE ZERO.                      
008800     SKIP1                                                                
008900*                                                                         
009000*01    -COPY WWPRODSL                                                     
009100*                                                                         
009200 01  SUBPROGRAM.                                                          
009300     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
009400     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
009500     03  ABEND               PIC X(8)    VALUE 'ABEND '.                  
009600     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
009700     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
009710     03  WZ20DAYS            PIC X(8)    VALUE 'WZ20DAYS'.                
009800     03  WINTSOR             PIC X(8)    VALUE 'WINTSOR'.                 
009900                                                                          
010000 01  SW-OK               PIC X(1)    VALUE 'J'.                           
010100 77  ROT-INLB-SW         PIC X(1)    VALUE 'N'.                           
010200     88  ROT-INLB-FINNS              VALUE 'J'.                           
010300     88  ROT-INLB-SAKNAS             VALUE 'N'.                           
010400     SKIP1                                                                
010500*      --- VALID IDDC CODES                                               
010600*                                                                         
010700*01    -COPY WWDC99                                                       
010800       EJECT                                                              
010900                                                                          
011000 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
011100*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
011200*- - - - - - - - - - - - - OCCURS-MAX SÄTTS TILL VERKLIGT ANTAL           
011300*- - - - - - - - - - - - - I N- SEKTIONEN.                                
011400 01  IDDC-INDEX-WS.                                                       
011500     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
011600                                                                          
011700     03 WDCIX            PIC S9(3)   VALUE +0   COMP SYNC.                
011800     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
011900                                                                          
012000 01  IDDC-TABELL.                                                         
012100     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
012200                INDEXED BY DCIX.                                          
012300        05 T-DCS.                                                         
012400          07 T-DCS-IDDC           PIC X(2).                               
012500          07 T-DCS-KDDC           PIC X(2).                               
012600          07 T-DCS-FLOVRLAGBER    PIC X.                                  
012700          07 T-DCS-IDLEVNR-DC     PIC X(5).                               
012800                                                                          
012900                                                                          
013000 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
013100*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
013200 01  TABENTRY-PARM.                                                       
013300     03  STEGLANGD               PIC S9(9) COMP.                          
013400     03  ANTAL                   PIC S9(9) COMP.                          
013500     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
013600                                                                          
013700                                                                          
013800*- - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                       
013900 01  WDATAREA                PIC X(8)    VALUE 'WDATAREA'.                
014000*01  -COPY WDATAREA.                                                      
014100     EJECT                                                                
014103*- - - - - - - - - - - - - PARAMETRAR TILL WZ20DAYS                       
014104                                                                          
014105 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
014106*   -COPY WZ20DAYS                                                        
014120     EJECT                                                                
014200 01  IMS-WS.                                                              
014300     03  FILLER              PIC X(8)    VALUE 'IMS-WS'.                  
014400     03  IX                  PIC S9(9)   VALUE +0    COMP SYNC.           
014500     03  IX-DAG              PIC S9(3)   VALUE +0    COMP SYNC.           
014600     SKIP3                                                                
014700     03  STATUS-WS           PIC X(2).                                    
014800         88  SEGMENT-FINNS               VALUE '  '.                      
014900         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
015000         88  SEGMENT-SLUT                VALUE 'GB'.                      
015100     SKIP3                                                                
015200     03  SSA1                PIC X(128).                                  
015300     03  SSA2                PIC X(64).                                   
015400     03  SSA3                PIC X(64).                                   
015500     03  SSA4                PIC X(64).                                   
015600     SKIP3                                                                
015700 01  KONSTANTER.                                                          
015800     03  JA                  PIC X       VALUE 'J'.                       
015900     03  OCH                 PIC X       VALUE '&'.                       
016000     03  NEJ                 PIC X       VALUE 'N'.                       
016100     03  AVTAL-FINNS         PIC X(1)    VALUE 'N'.                       
016200     03  LAES-ARTIKEL-DATA   PIC S9(3)   VALUE +401  COMP-3.              
016300     03  LAES-LEVERANTOER    PIC S9(3)   VALUE +402  COMP-3.              
016400     03  LAES-OMSPEC         PIC S9(3)   VALUE +403  COMP-3.              
016500     03  LAES-ARTIKEL        PIC S9(3)   VALUE +404  COMP-3.              
016510*-- ALLA UPPDATERINGAR FLYTTADE TILL BMP W2214A00.                        
016600*--  03  UPPDAT-CLAG         PIC S9(3)   VALUE +405  COMP-3.              
016700*--  03  BORTTAG-OMSPEC      PIC S9(3)   VALUE +410  COMP-3.              
016800*--  03  BORTTAG-KOPPLING-LP PIC S9(3)   VALUE +413  COMP-3.              
016900     SKIP1                                                                
017000     03  LAES-AVROP-FIRST    PIC S9(3)   VALUE +420  COMP-3.              
017100     03  LAES-AVROP-NEXT     PIC S9(3)   VALUE +421  COMP-3.              
017200     03  LAES-AVROP-KVAL     PIC S9(3)   VALUE +422  COMP-3.              
017300*--  03  UPPDAT-AVROP        PIC S9(3)   VALUE +423  COMP-3.              
017400*--  03  BORTTAG-AVROP       PIC S9(3)   VALUE +424  COMP-3.              
017500*--  03  NYUPPL-AVROP        PIC S9(3)   VALUE +425  COMP-3.              
017600     03  LAES-AVROP-LEV-F    PIC S9(3)   VALUE +426  COMP-3.              
017700     03  LAES-AVROP-LEV-N    PIC S9(3)   VALUE +427  COMP-3.              
017800     03  LAES-WDB6-DC-INFO   PIC S9(3)   VALUE +428  COMP-3.              
017900     SKIP3                                                                
018000 01  W-IDARTNR-X.                                                         
018100     03  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
018200 01  W-IDARTNR-SATS-X.                                                    
018300     03  W-IDARTNR-SATS      PIC S9(9)   VALUE ZERO  COMP-3.              
018400 01  W-IDLEVNR-X.                                                         
018500     03  W-IDLEVNR           PIC X(5)    VALUE SPACE.                     
018600 01  W-KDAVROP-X.                                                         
018700     03  W-KDAVROP           PIC S9(1)   VALUE ZERO  COMP-3.              
018800 01  W-DAPRLIST-X.                                                        
018900     03  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                      
019000 01  W-DAXLEVSP-X.                                                        
019100     03  W-DAXLEVSP          PIC  9(6)   VALUE ZERO.                      
019200 01  W-TISPECST-X.                                                        
019300     03  W-TISPECST          PIC S9(5)   VALUE ZERO  COMP-3.              
019400 01  W-WDD901KY-X.                                                        
019500     03  W-IDARTNR-D9        PIC  S9(9)   VALUE ZERO  COMP-3.             
019600     03  W-IDDC-D9           PIC  X(2)    VALUE SPACE.                    
019700 01  W-WDD905KY-X.                                                        
019800     03  W-DAAVROP-X.                                                     
019900         05  W-DAAVROP       PIC  9(6)    VALUE ZERO.                     
020000     03  W-TILEVDAG-X.                                                    
020100         05  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.              
020200 01  W-DABEHOV-X-MIN.                                                     
020300     03  W-ANT-DABEHOV-MIN   PIC 9(6)   VALUE ZERO.                       
020400 01  W-DABEHOV-X-MAX.                                                     
020500     03  W-ANT-DABEHOV-MAX   PIC 9(6)   VALUE ZERO.                       
020600 01  W-ANT-TIBEHOV-MIN       PIC S9(5) COMP-3 VALUE ZERO.                 
020700 01  W-ANT-TIBEHOV-MAX       PIC S9(5) COMP-3 VALUE ZERO.                 
020800 01  W-IDPTYP-X.                                                          
020900     03  W-IDPTYP            PIC X(3)    VALUE '310'.                     
021000                                                                          
022200     SKIP3                                                                
022300 01  W-IDDC-B6-X.                                                         
022400     03  W-IDDC-B6           PIC X(2)  VALUE SPACE.                       
022500                                                                          
022510 01  W-IDDC-REF-X.                                                        
022520     03  W-IDDC-REF          PIC X(2)   VALUE '11'.                       
022530                                                                          
022560                                                                          
022600 01  GODK-STATUSKODER.                                                    
022700     03  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC X(2).             
022800     EJECT                                                                
022900*    -COPY W0003.                                                         
023000     EJECT                                                                
023100 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK601'.          
023200     SKIP2                                                                
023300 01  DLI-IO-WDK601.                                                       
023400*    03  WLARTC01 -COPY WDK601                                            
023500     EJECT                                                                
023600 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK611'.          
023700     SKIP2                                                                
023800 01  DLI-IO-WDK611.                                                       
023900*    03  WLARTC11 -COPY WDK611                                            
024000     EJECT                                                                
024010 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK621'.          
024100 01  DLI-IO-WDK621.                                                       
024200*    03  WLARTC21 -COPY WDK621                                            
024300     EJECT                                                                
024320 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK623'.          
024330 01  DLI-IO-WDK623.                                                       
024340*    03  WLARTC23 -COPY WDK623                                            
024350     EJECT                                                                
024351 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDD901'.          
024352 01  DLI-IO-WDD901.                                                       
024353*    03  WLINLB01 -COPY WDD901  -PRE WDD9ART-                             
024354     EJECT                                                                
024355 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDD902'.          
024356 01  DLI-IO-WDD902.                                                       
024357*    03 WLINLB11 -COPY WDD902   -PRE LEV-                                 
024360     EJECT                                                                
024380 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDD904'.          
024390 01  DLI-IO-WDD904.                                                       
024391*    03 WLINLB22 -COPY WDD904   -PRE OMSPEC-                              
024393     EJECT                                                                
024394 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDD905'.          
024395 01  DLI-IO-WDD905.                                                       
024396*    03 WLINLB23 -COPY WDD905   -PRE AVROP-                               
024397     EJECT                                                                
024399 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK701'.          
024400 01  DLI-IO-WDK701.                                                       
024410*    03 WDK701   -COPY WDK701                                             
024420     EJECT                                                                
024440 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK711'.          
024450 01  DLI-IO-WDK711.                                                       
024460*    03 WDK711   -COPY WDK711                                             
024470     EJECT                                                                
024800 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK901'.          
024900 01  DLI-IO-WDK901.                                                       
025600*    03 WLARTM01 -COPY WDK901   -PRE ARTM-                                
025700     EJECT                                                                
025710 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDK911'.          
025720 01  DLI-IO-WDK911.                                                       
025800*    03 WLARTM11 -COPY WDK911   -PRE ARTM-                                
025900     EJECT                                                                
026301 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDL201'.          
026302 01  DLI-IO-WDL201.                                                       
026303*    03 WLINLE01 -COPY WDL201   -PRE INLE-                                
026310     EJECT                                                                
026320 01  FILLER                  PIC  X(16)   VALUE 'DLI-IO-WDL221'.          
026330 01  DLI-IO-WDL221.                                                       
026600*    03 WLINLE21 -COPY WDL221                                             
026700     EJECT                                                                
026800 01  DLI-IO-AREA-F1          PIC X(100).                                  
026900     SKIP2                                                                
027000*01  WLLEVA01 -COPY WDF101      -PRE F1-      -RED DLI-IO-AREA-F1         
027100     EJECT                                                                
027400 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-AREA-WDB6'.         
027500 01  DLI-IO-AREA-B6.                                                      
027600     SKIP2                                                                
027700*    03  -COPY WDB601                                                     
027800     EJECT                                                                
027900 LINKAGE SECTION.                                                         
028000     SKIP1                                                                
028100*01  AREA -COPY W221L401   -PRE LINK-                                     
028200     EJECT                                                                
028300*01  AREA -COPY W221L402   -PRE LINK2- -RED LINK-AREA                     
028400     EJECT                                                                
028500*    -COPY W0008 -PRE WDK6-                                               
028600          05  FILLER                     PIC X.                           
028700     EJECT                                                                
028800*    -COPY W0008 -PRE WDD9-                                               
028900          05  FILLER                     PIC X(7).                        
029000          05  WDD9-KEY-02-IDLEVNR        PIC X(5).                        
029100     EJECT                                                                
029200*    -COPY W0008 -PRE WDK7-                                               
029300          05  FILLER                     PIC X.                           
029400     EJECT                                                                
029500*    -COPY W0008 -PRE ARTM-                                               
029600          05  FILLER                     PIC X.                           
029700     EJECT                                                                
029800*    -COPY W0008 -PRE WDL2-                                               
029900          05  FILLER                     PIC X.                           
030000     EJECT                                                                
030100*    -COPY W0008 -PRE WDF1-                                               
030200          05  FILLER                     PIC X.                           
030300     EJECT                                                                
030700*    -COPY W0008 -PRE WDB6-                                               
030800          05  FILLER                     PIC X.                           
030900     EJECT                                                                
031000 PROCEDURE DIVISION USING LINK-AREA  WDK6-PCB WDD9-PCB WDK7-PCB           
031100                            ARTM-PCB WDL2-PCB WDF1-PCB WDB6-PCB.          
031300     SKIP3                                                                
031400     EVALUATE LINK-KDCALL                                                 
031500         WHEN LAES-ARTIKEL-DATA                                           
031600             PERFORM A-LAES-ARTIKEL-DATA                                  
031700         WHEN LAES-LEVERANTOER                                            
031800             PERFORM B-LAES-LEVERANTOER                                   
031900         WHEN LAES-OMSPEC                                                 
032000             PERFORM C-LAES-OMSPEC                                        
032100         WHEN LAES-ARTIKEL                                                
032200             PERFORM D-LAES-ARTIKEL                                       
032700         WHEN LAES-AVROP-FIRST                                            
032800             PERFORM M-LAES-AVROP-FIRST                                   
032900         WHEN LAES-AVROP-NEXT                                             
033000             PERFORM N-LAES-AVROP-NEXT                                    
033100         WHEN LAES-AVROP-KVAL                                             
033200             PERFORM O-LAES-AVROP-KVAL                                    
034100         WHEN LAES-AVROP-LEV-F                                            
034200             PERFORM U-LAES-AVROP-LEV-F                                   
034300         WHEN LAES-AVROP-LEV-N                                            
034400             PERFORM V-LAES-AVROP-LEV-N                                   
034500                                                                          
034600         WHEN LAES-WDB6-DC-INFO                                           
034700             PERFORM Z-LAES-WDB6-DC-INFO                                  
034800                                                                          
034900         WHEN OTHER                                                       
035000         MOVE 30 TO RKOD                                                  
035100         DISPLAY '*** W2214010, KDCALL-VÄRDE FELAKTIGT'                   
035200         CALL ABEND USING RKOD                                            
035300     END-EVALUATE                                                         
035400     SKIP1                                                                
035500     MOVE ZERO TO RETURN-CODE                                             
035600     GOBACK                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 A-LAES-ARTIKEL-DATA SECTION.                                             
035910     MOVE 'A-LAES-ARTIKEL-DATA '  TO CURRENT-SECTION                      
036000     SKIP3                                                                
036100     MOVE NEJ            TO ROT-INLB-SW                                   
036200     MOVE NEJ TO LINK-FLJANEJ-ANROP                                       
036300                                                                          
036400     PERFORM IMS-GN-ARTIKEL-WDK601                                        
036500     MOVE JA TO SW-OK                                                     
036600     IF SEGMENT-FINNS                                                     
036700        MOVE ART-KDPRODSL        TO TEST-KDPRODSL                         
036800        IF (KDPRODSL-WHEELS AND ART-IDFKNGRP = 3955) OR                   
036900           (ART-IDLEVNR  = '1446' AND ART-IDFKNGRP = 1968) OR             
037000           (ART-KDSORT   = 'SW')                                          
037100           MOVE NEJ TO SW-OK                                              
037200        ELSE                                                              
037300          PERFORM S10-KOLLA-LEVNR-REFILL                                  
037400        END-IF                                                            
037500     END-IF                                                               
037600                                                                          
037700     PERFORM UNTIL SEGMENT-SLUT OR                                        
037800        (ART-KDERS-UTG = ZERO AND KDPRODSL-VOLVO-BIMA AND                 
037900         SW-OK = JA)                                                      
038000         PERFORM IMS-GN-ARTIKEL-WDK601                                    
038100         MOVE JA TO SW-OK                                                 
038200         IF SEGMENT-FINNS                                                 
038300            MOVE ART-KDPRODSL    TO TEST-KDPRODSL                         
038400            IF (KDPRODSL-WHEELS AND ART-IDFKNGRP = 3955) OR               
038500               (ART-IDLEVNR  = '1446' AND ART-IDFKNGRP = 1968) OR         
038600               (ART-KDSORT   = 'SW')                                      
038700***            OBS  DELAR TILL RADIOAPPARATER SKALL EJ MED ***            
038800***            OBS  MONTERINGSANVISNINGAR     SKALL EJ MED ***            
038900***            OBS  SOFTVARA                  SKALL EJ MED ***            
039000***            OBS  REFILL-ARTIKEL TILL CDC   SKALL EJ MED ***            
039100               MOVE NEJ TO SW-OK                                          
039200            ELSE                                                          
039300              PERFORM S10-KOLLA-LEVNR-REFILL                              
039400            END-IF                                                        
039500         END-IF                                                           
039600     END-PERFORM                                                          
039700                                                                          
039800     IF SEGMENT-FINNS                                                     
039900     AND ART-KDERS-UTG = ZERO                                             
040000     AND KDPRODSL-VOLVO-BIMA                                              
040100         MOVE JA           TO LINK-FLJANEJ-ANROP                          
040200         MOVE ART-TIFINLV  TO LINK-TIFINLV                                
040300         MOVE ART-IDARTNR  TO LINK-IDARTNR W-IDARTNR                      
040400         MOVE ART-IDLEVNR  TO LINK-IDLEVNR                                
040500         MOVE ART-IDFKNGRP TO LINK-IDFKNGRP                               
040600         MOVE ART-KDPRODSL TO LINK-KDPRODSL                               
040700                              W-KDPRODSL                                  
040800         MOVE W-IDPROD     TO LINK-IDPROD                                 
040900         MOVE ART-KDERS-UTG TO LINK-KDERS-UTG                             
041000     SKIP1                                                                
041100         PERFORM IMS-GNP-CLAG-WDK611                                      
041200         IF SEGMENT-FINNS                                                 
041300             MOVE CLAG-BEFT          TO LINK-BEFT                         
041400             MOVE CLAG-FLAVRART      TO LINK-FLAVRART                     
041500             MOVE CLAG-FLJIT         TO LINK-FLJIT                        
041600             MOVE CLAG-FLMANBK       TO LINK-FLMANBK                      
041700             MOVE CLAG-FLMANAT       TO LINK-FLMANAT                      
041800             MOVE CLAG-FLMANKP       TO LINK-FLMANKP                      
041900             MOVE CLAG-IDANSK        TO LINK-IDANSK                       
042000             MOVE CLAG-KDHF          TO LINK-KDHF                         
042100             MOVE CLAG-KDLPSP        TO LINK-KDLPSP                       
042200             MOVE CLAG-KVDAGAR-INLEV TO LINK-KVDAGAR-INLEV                
042300             MOVE CLAG-KVDAGAR-TT    TO LINK-KVDAGAR-TT                   
042400             MOVE CLAG-KVDAGAR-FFH   TO LINK-KVDAGAR-FFH                  
042500             MOVE CLAG-KVLAAN        TO LINK-KVLAAN                       
042600             MOVE CLAG-KVPALL        TO LINK-KVPALL                       
042700             MOVE CLAG-KVVECKOR-AT   TO LINK-KVVECKOR-AT                  
042800             MOVE CLAG-KVVECKOR-BT   TO LINK-KVVECKOR-BT                  
042900             MOVE CLAG-KVVECKOR-FT   TO LINK-KVVECKOR-FT                  
043000             MOVE CLAG-KVVECKOR-LT   TO LINK-KVVECKOR-LT                  
043100             MOVE CLAG-TIOMSPEC      TO LINK-TIOMSPEC                     
043200     SKIP1                                                                
043300             MOVE CLAG-FLMANQ        TO LINK-FLMANQ                       
043400             MOVE CLAG-KDAVT         TO LINK-KDAVT                        
043500             MOVE CLAG-KDKSP         TO LINK-KDKSP                        
043600             MOVE CLAG-KDVVKL        TO LINK-KDVVKL                       
043700             MOVE CLAG-KVAP          TO LINK-KVAP                         
043800             MOVE CLAG-KVBK          TO LINK-KVBK                         
043900             MOVE CLAG-KVKP          TO LINK-KVKP                         
044000             MOVE CLAG-KVOVERF       TO LINK-KVOVERF                      
044100             MOVE CLAG-KVQ           TO LINK-KVQ                          
044200             MOVE CLAG-KVQ-JUST      TO LINK-KVQ-JUST                     
044300                                                                          
044500             IF CLAG-KVSLUTKP > 0                                         
046420               MOVE 'YYWWD'           TO DAYS-KDDATFMT1                   
046430               MOVE 'YYMMDD'          TO DAYS-KDDATFMT2                   
046440               MOVE LINK-TIAAVVD-AKT  TO WS-DAYS-TIDATE1-AAVVD            
046450**** DAGNR SÄTTS = 5                                                      
046460               ADD +5                 TO WS-DAYS-TIDATE1-AAVVD            
046470               MOVE WS-DAYS-TIDATE1-AAVVD   TO DAYS-TIDATE1               
046480               MOVE 0                 TO DAYS-KVDAYS                      
046490               MOVE SPACE             TO DAYS-TIDATE2                     
046491                                         DAYS-IDCALEND                    
046492               CALL WZ20DAYS USING DAYS-WZ20DAYS                          
046494                                                                          
046495               IF DAYS-KDRC = 8                                           
046496                 MOVE 32 TO RKOD                                          
046497                 MOVE 'FEL VID ANROP TILL WZ20DAYS '                      
046498                                          TO FELTEXT-STR                  
046500                 DISPLAY FELTEXT                                          
046501                 CALL ABEND USING RKOD                                    
046502               ELSE                                                       
046503                 MOVE CLAG-TISLUTKP     TO TMP1-YYMMDD                    
046504                 MOVE DAYS-TIDATE2(1:6) TO TMP2-YYMMDD                    
046506                 PERFORM WY2000P1                                         
046507                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
046508                   MOVE +0 TO LINK-KVSLUTKP                               
046509                 ELSE                                                     
046510                   MOVE CLAG-KVSLUTKP TO LINK-KVSLUTKP                    
046511                 END-IF                                                   
046514               END-IF                                                     
046520             ELSE                                                         
046600               MOVE CLAG-KVSLUTKP TO LINK-KVSLUTKP                        
046700             END-IF                                                       
046850                                                                          
046900             MOVE CLAG-TIBESRPT      TO LINK-TIBESRPT                     
047000             MOVE CLAG-TIBESRPT-PAAM TO LINK-TIBESRPT-PAAM                
047100             MOVE CLAG-TILPSP        TO LINK-TILPSP                       
047200             MOVE CLAG-TIQJUST       TO LINK-TIQJUST                      
047300             MOVE CLAG-IDINK         TO LINK-IDINK                        
047400             MOVE CLAG-KDLEVPLF      TO LINK-KDLEVPLF                     
047500             MOVE CLAG-KDFREKKL      TO LINK-KDFREKKL                     
047600             MOVE CLAG-KDPRISKL      TO LINK-KDPRISKL                     
047700             MOVE 1                  TO LINK-KVANTAL-CLAGER               
047800             MOVE SPACE              TO LINK-FLFSP (1)                    
047900             MOVE CLAG-FLMANPB       TO LINK-FLMANPB (1)                  
048000             MOVE CLAG-FLMPB         TO LINK-FLMPB (1)                    
048100             MOVE CLAG-KVPB-SATS     TO LINK-KVPB-SATS (1)                
048200             MOVE CLAG-KVPB-SEP      TO LINK-KVPB-SEP (1)                 
048300             MOVE CLAG-KVPB-TPO      TO LINK-KVPB-TPO (1)                 
048400             MOVE CLAG-REDIRLEV      TO LINK-REDIRLEV (1)                 
048500             MOVE CLAG-TISLJUST      TO LINK-TISLJUST (1)                 
048600             MOVE CLAG-KVMAD-SEP     TO LINK-KVMAD-SEP (1)                
048700             MOVE CLAG-KVMAD-TOT     TO LINK-KVMAD-TOT (1)                
048800             MOVE CLAG-KVMP          TO LINK-KVMP (1)                     
048900             MOVE CLAG-KVPB-VESL     TO LINK-KVPB-VESL (1)                
049000             MOVE CLAG-RESLJUST      TO LINK-RESLJUST (1)                 
049100             MOVE CLAG-RVPROFEL      TO LINK-RVPROFEL (1)                 
049200             MOVE CLAG-RVPROURS      TO LINK-RVPROURS (1)                 
049300             MOVE CLAG-TILEVDAG (1)  TO LINK-TILEVDAG (1)                 
049400             MOVE CLAG-TILEVDAG (2)  TO LINK-TILEVDAG (2)                 
049500             MOVE CLAG-TILEVDAG (3)  TO LINK-TILEVDAG (3)                 
049600             MOVE CLAG-TILEVDAG (4)  TO LINK-TILEVDAG (4)                 
049700             MOVE CLAG-TILEVDAG (5)  TO LINK-TILEVDAG (5)                 
049800             MOVE SPACE TO LINK-FLFSP (2)                                 
049900                          LINK-FLMANPB (2)                                
050000                          LINK-FLMPB (2)                                  
050100             MOVE ZERO TO LINK-KVPB-SATS (2)                              
050200                          LINK-KVPB-SEP (2)                               
050300                          LINK-KVPB-TPO (2)                               
050400                          LINK-REDIRLEV (2)                               
050500                          LINK-TISLJUST (2)                               
050600                          LINK-KVMAD-SEP (2)                              
050700                          LINK-KVMAD-TOT (2)                              
050800                          LINK-KVMP (2)                                   
050900                          LINK-KVPB-VESL (2)                              
051000                          LINK-RESLJUST (2)                               
051100                          LINK-RVPROFEL (2)                               
051200                          LINK-RVPROURS (2)                               
051300                                                                          
051400             MOVE CLAG-IDLKTO     TO LINK-IDLKTO                          
051500             MOVE CLAG-PRARTSTD   TO LINK-PRARTSTD                        
051600                                                                          
051700             PERFORM AB-HAEMTA-PRARTBES                                   
051800                                                                          
051900             MOVE CLAG-KDUART     TO LINK-KDUART                          
052000             MOVE CLAG-TILTK      TO LINK-TILTK                           
052100             MOVE CLAG-KVQPACK-1  TO LINK-KVQPACK-1                       
052200             MOVE CLAG-KDGK       TO LINK-KDGK                            
052300             MOVE CLAG-KDLTK      TO LINK-KDLTK                           
052400             MOVE CLAG-KVFRYSTI   TO LINK-KVFRYSTI                        
052500             MOVE CLAG-KDOPPLAN   TO LINK-KDOPPLAN                        
052600             MOVE CLAG-FLTPO1     TO LINK-FLTPO1                          
052700             MOVE CLAG-KDERS      TO LINK-KDERS (1)                       
052800             MOVE CLAG-KVAKS-CDC  TO LINK-KVAKS (1)                       
052801             ADD  CLAG-KVAKS-PAV  TO LINK-KVAKS (1)                       
052802             ADD  CLAG-KVAKS-T    TO LINK-KVAKS (1)                       
052810                                                                          
052900             IF LINK-KVAKS (1) > ZERO                                     
053000                PERFORM AA-JUSTERA-MED-RETURER                            
053100             END-IF                                                       
053310                                                                          
053400             MOVE CLAG-KVLS       TO LINK-KVLS (1)                        
053500             MOVE CLAG-KVRESS     TO LINK-KVRESS (1)                      
053600             MOVE CLAG-KVROS      TO LINK-KVROS (1)                       
053700             MOVE CLAG-KVSLAGER   TO LINK-KVSLAGER (1)                    
053800             MOVE CLAG-KVRETUR    TO LINK-KVRETUR (1)                     
053900             MOVE CLAG-TIDISPIN   TO LINK-TIDISPIN (1)                    
054000             MOVE ZERO TO LINK-KDERS (2)                                  
054100                          LINK-KVAKS (2)                                  
054200                          LINK-KVLS (2)                                   
054300                          LINK-KVRESS (2)                                 
054400                          LINK-KVROS (2)                                  
054500                          LINK-KVSLAGER (2)                               
054600                          LINK-KVRETUR (2)                                
054700                          LINK-TIDISPIN (2)                               
054800             MOVE CLAG-DAPBPLAN   TO LINK-DAPBPLAN                        
054900             MOVE CLAG-DASEASON   TO LINK-DASEASON                        
055000             MOVE CLAG-KVPB-PLAN  TO LINK-KVPB-PLAN                       
055100             MOVE CLAG-ADLAGOMR   TO LINK-ADLAGOMR                        
055200             MOVE CLAG-FLNYBER    TO LINK-FLNYBER                         
055300             MOVE CLAG-KVULOAD    TO LINK-KVULOAD                         
055400             MOVE CLAG-PRORDSK    TO LINK-PRORDSK                         
055500             MOVE CLAG-VLARTNTO   TO LINK-VLARTNTO                        
055600             MOVE CLAG-KVEOQ      TO LINK-KVEOQ                           
055700             MOVE CLAG-KVSLAGER-OPT      TO LINK-KVSLAGER-OPT             
055800             MOVE CLAG-RESEASON-PLAN (1) TO LINK-RESEASON-PLAN (1)        
055900             MOVE CLAG-RESEASON-PLAN (2) TO LINK-RESEASON-PLAN (2)        
056000             MOVE CLAG-RESEASON-PLAN (3) TO LINK-RESEASON-PLAN (3)        
056100             MOVE CLAG-RESEASON-PLAN (4) TO LINK-RESEASON-PLAN (4)        
056200             MOVE CLAG-RESEASON-PLAN (5) TO LINK-RESEASON-PLAN (5)        
056300             MOVE CLAG-RESEASON-PLAN (6) TO LINK-RESEASON-PLAN (6)        
056400             MOVE CLAG-RESEASON-PLAN (7) TO LINK-RESEASON-PLAN (7)        
056500             MOVE CLAG-RESEASON-PLAN (8) TO LINK-RESEASON-PLAN (8)        
056600             MOVE CLAG-RESEASON-PLAN (9) TO LINK-RESEASON-PLAN (9)        
056700           MOVE CLAG-RESEASON-PLAN (10) TO LINK-RESEASON-PLAN (10)        
056800           MOVE CLAG-RESEASON-PLAN (11) TO LINK-RESEASON-PLAN (11)        
056900           MOVE CLAG-RESEASON-PLAN (12) TO LINK-RESEASON-PLAN (12)        
057000           IF CLAG-KVVECKOR-LVAR NUMERIC                                  
057100              MOVE CLAG-KVVECKOR-LVAR   TO LINK-KVVECKOR-LVAR             
057200           ELSE                                                           
057300              MOVE ZERO                 TO LINK-KVVECKOR-LVAR             
057400           END-IF                                                         
057500         END-IF                                                           
057600                                                                          
057700         MOVE NEJ TO AVTAL-FINNS                                          
057800         PERFORM IMS-GNP-AVTAL-WDK623                                     
057900         PERFORM UNTIL NOT(                                               
058000            SEGMENT-FINNS AND AVTAL-FINNS = NEJ)                          
058100            IF LINK-IDLEVNR = AVT-IDLEVNR-AVT                             
058200               MOVE AVT-IDAVTAL TO LINK-IDAVTAL                           
058300               MOVE JA TO AVTAL-FINNS                                     
058400            ELSE                                                          
058500               PERFORM IMS-GNP-AVTAL-WDK623                               
058600            END-IF                                                        
058700         END-PERFORM                                                      
058800         IF AVTAL-FINNS = NEJ                                             
058900            MOVE ZERO TO LINK-IDAVTAL                                     
059000         END-IF                                                           
059100                                                                          
059200     MOVE ZERO              TO   W-KVPB-SDC-TOT                           
059300                                 W-KVPB-SDC-EJ-DIR                        
059400                                 W-TILLG-SDC                              
059500                                 W-OVERLAGER-SDC                          
059600                                                                          
059700     MOVE LINK-IDARTNR  TO W-IDARTNR                                      
059800     PERFORM IMS-GET-WDK701-SDC                                           
059900                                                                          
060000     IF SEGMENT-FINNS                                                     
060010******* SKALL BARA LÄSA DE MED IDDC-REF = 11                              
060100        PERFORM IMS-GNP-WDK711-SDC-REF                                    
060200                                                                          
060300        PERFORM UNTIL SEGMENT-SAKNAS                                      
060500           ADD SLAG-KVPB-REF     TO   W-KVPB-SDC-TOT                      
060600           IF SLAG-FLCDCBEH = JA                                          
060700               ADD SLAG-KVPB-REF TO   W-KVPB-SDC-EJ-DIR                   
060800           END-IF                                                         
061000           MOVE ZERO           TO   W-TILLG-SDC                           
061100           ADD SLAG-KVLS       TO   W-TILLG-SDC                           
061200           ADD SLAG-KVBEART    TO   W-TILLG-SDC                           
061300           ADD SLAG-KVAKS-SDC  TO   W-TILLG-SDC                           
061400           ADD SLAG-KVAKS-PAV  TO   W-TILLG-SDC                           
061500           COMPUTE W-KVOKS = SLAG-KVOKS-BULK                              
061600                           + SLAG-KVOKS-DAG                               
061700******* FIX FÖR NEGATIVA KVOKS                                            
061800           IF W-KVOKS > 0                                                 
061900             SUBTRACT W-KVOKS FROM W-TILLG-SDC                            
062000           END-IF                                                         
062100                                                                          
062200           MOVE SLAG-IDDC TO WS-IDDC                                      
062300           IF NDC                                                         
062400              CONTINUE                                                    
062500           ELSE                                                           
062600             SET DCIX TO +1                                               
062700             SEARCH DC-TAB                                                
062800                AT END                                                    
062900                   MOVE NEJ TO DCS-TRAEFF                                 
063000                WHEN T-DCS-IDDC (DCIX) = SLAG-IDDC                        
063100                   MOVE JA  TO DCS-TRAEFF                                 
063200                   CONTINUE                                               
063300             END-SEARCH                                                   
063400                                                                          
063500             IF DCS-TRAEFF = JA                                           
063600*              - KOLLA OM SDC-LAGERKODENS första POS är 'S' (SDC)         
063700               IF T-DCS-KDDC(DCIX)(1:1) = 'S'                             
063800*                - KOLLA OM ÖVERLAGERBERÄKNING PÅ SDC SKA GÖRAS           
063900                 IF T-DCS-FLOVRLAGBER(DCIX) = NEJ                         
064000                   CONTINUE                                               
064100                 ELSE                                                     
064200                   IF SLAG-KVREFOVL < W-TILLG-SDC                         
064300                      COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC           
064400                                              + W-TILLG-SDC               
064500                                              - SLAG-KVREFOVL             
064600                   END-IF                                                 
064700                 END-IF                                                   
064800               END-IF                                                     
064900             ELSE                                                         
065000*              -- SDC ÄR EJ REGISTRERAT PÅ WDB6. HOPPA !                  
065100*              -- Detta borde egentligen aldrig inträffa'                 
065200               DISPLAY 'IDDC ' SLAG-IDDC ' EJ REG PÅ WDB6'                
065300               CONTINUE                                                   
065400             END-IF                                                       
065500           END-IF                                                         
065600           MOVE LINK-TIAAVV-AKT TO DAGENS-AAVV                            
065700           MOVE ART-TIFINLV     TO TIFINLV-AAVVD                          
065800           MOVE DAGENS-AA       TO TMP1-YY                                
065900           MOVE TIFINLV-AA      TO TMP2-YY                                
066000           PERFORM WY2000P9                                               
066100           COMPUTE VECKO-DIFF = (TMP1-YY - TMP2-YY) * 52                  
066200                              + DAGENS-VV - TIFINLV-VV                    
066300           IF VECKO-DIFF < 52                                             
066400              MOVE ZERO TO W-OVERLAGER-SDC                                
066500***           DISPLAY '          ÖVERLAG NOLLAS. VECKODIFF < 52'          
066600           END-IF                                                         
066710           PERFORM IMS-GNP-WDK711-SDC-REF                                 
066800        END-PERFORM                                                       
066900     END-IF                                                               
067000                                                                          
067100     MOVE W-OVERLAGER-SDC TO LINK-KVLS-SDC-OVER                           
067200                                                                          
067300     COMPUTE W-KVPB-SDC-TOT ROUNDED = W-KVPB-SDC-TOT                      
067400                                                                          
067500     MOVE W-KVPB-SDC-TOT  TO LINK-KVPB-SDC-TOT                            
067600                                                                          
067700     COMPUTE W-KVPB-SDC-EJ-DIR ROUNDED = W-KVPB-SDC-EJ-DIR                
067800                                                                          
067900     MOVE W-KVPB-SDC-EJ-DIR TO LINK-KVPB-SDC-EJ-DIR                       
068000                                                                          
068100         MOVE ZERO          TO WS-KVFRYSTIPLUS1                           
068200         MOVE ZERO          TO WS-TIFINLV-AAVV                            
068300         MOVE LINK-IDARTNR  TO W-IDARTNR                                  
068400         PERFORM IMS-GU-ARTM-ART-WDK901                                   
068500         IF SEGMENT-FINNS                                                 
068600            MOVE ARTM-ART-SUTPO-TOT                                       
068700                               TO LINK-SUTPO-TOT (1)                      
068800*** FIX FÖR NEGATIVA KVOKS                                                
068900            IF ARTM-ART-KVOKS-BULK > 0                                    
069000              MOVE ARTM-ART-KVOKS-BULK                                    
069100                               TO LINK-KVOKS-BULK (1)                     
069200            ELSE                                                          
069300              MOVE +0 TO LINK-KVOKS-BULK (1)                              
069400            END-IF                                                        
069500            IF ARTM-ART-KVOKS-DAG  > 0                                    
069600              MOVE ARTM-ART-KVOKS-DAG                                     
069700                               TO LINK-KVOKS-DAG (1)                      
069800            ELSE                                                          
069900              MOVE +0 TO LINK-KVOKS-DAG (1)                               
070000            END-IF                                                        
070100            IF ARTM-ART-KVOKS-VOR  > 0                                    
070200              MOVE ARTM-ART-KVOKS-VOR                                     
070300                               TO LINK-KVOKS-VOR (1)                      
070400            ELSE                                                          
070500              MOVE +0 TO LINK-KVOKS-VOR (1)                               
070600            END-IF                                                        
070700                                                                          
070800            COMPUTE WS-KVFRYSTIPLUS1 = LINK-KVFRYSTI + 1                  
070900            COMPUTE WS-KVFRYSTIPLUS1 = WS-KVFRYSTIPLUS1 * -1              
071000            COMPUTE WS-TIFINLV-AAVV  = LINK-TIFINLV / 10                  
071100            CALL W009VADD USING WS-TIFINLV-AAVV WS-KVFRYSTIPLUS1          
071200                                                                          
071300            MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                           
071400            MOVE LINK-TIAAVV-AKT   TO TMP2-YYWW                           
071500            PERFORM WY2000P3                                              
071600            IF TMP1-YYWW > TMP2-YYWW                                      
071700               CONTINUE                                                   
071800            ELSE                                                          
071900*** LÄSER TPO-BEHOV FROM NÄSTA VECKA OCH 18 VECKOR FRAMÅT                 
072000*** DESSA TPO-BEHOV LIGGER TILL GRUND FÖR BERÄKNING AV KVPB-TPO           
072100               MOVE LINK-TIAAVV-AKT TO W-ANT-TIBEHOV-MIN                  
072200               MOVE +1              TO WS-ANT-VECKOR                      
072300               CALL W009VADD USING W-ANT-TIBEHOV-MIN WS-ANT-VECKOR        
072400               MOVE +17               TO WS-ANT-VECKOR                    
072500               MOVE W-ANT-TIBEHOV-MIN TO W-ANT-TIBEHOV-MAX                
072600               CALL W009VADD USING W-ANT-TIBEHOV-MAX WS-ANT-VECKOR        
072700                                                                          
072800               MOVE W-ANT-TIBEHOV-MIN TO W-ANT-DABEHOV-MIN                
072900               IF W-ANT-TIBEHOV-MIN NOT = ZERO                            
073000                 IF W-ANT-TIBEHOV-MIN < 5000                              
073100                   MOVE 20            TO W-ANT-DABEHOV-MIN (1:2)          
073200                 ELSE                                                     
073300                   IF W-ANT-TIBEHOV-MIN < 9999                            
073400                     MOVE 19          TO W-ANT-DABEHOV-MIN (1:2)          
073500                   ELSE                                                   
073600                     MOVE 999999      TO W-ANT-DABEHOV-MIN                
073700                   END-IF                                                 
073800                 END-IF                                                   
073900               END-IF                                                     
074000                                                                          
074100               MOVE W-ANT-TIBEHOV-MAX TO W-ANT-DABEHOV-MAX                
074200               IF W-ANT-TIBEHOV-MAX NOT = ZERO                            
074300                 IF W-ANT-TIBEHOV-MAX < 5000                              
074400                   MOVE 20            TO W-ANT-DABEHOV-MAX (1:2)          
074500                 ELSE                                                     
074600                   IF W-ANT-TIBEHOV-MAX < 9999                            
074700                     MOVE 19          TO W-ANT-DABEHOV-MAX (1:2)          
074800                   ELSE                                                   
074900                     MOVE 999999      TO W-ANT-DABEHOV-MAX                
075000                   END-IF                                                 
075100                 END-IF                                                   
075200               END-IF                                                     
075300                                                                          
075400               MOVE ZERO     TO LINK-SUTPO-PB (1)                         
075500               PERFORM IMS-GNP-ARTM-ANT-WDK911                            
075600               PERFORM UNTIL SEGMENT-SAKNAS                               
075700                  COMPUTE LINK-SUTPO-PB (1) =                             
075800                          LINK-SUTPO-PB (1) + ARTM-ANT-SUTPO-PB           
075900                  PERFORM IMS-GNP-ARTM-ANT-WDK911                         
076000               END-PERFORM                                                
076100                                                                          
076200               MOVE ZERO     TO LINK-SUTPO-PB (2)                         
076300                                                                          
076400            END-IF                                                        
076500         ELSE                                                             
076600            MOVE ZERO          TO LINK-SUTPO-TOT  (1)                     
076700            MOVE ZERO          TO LINK-SUTPO-PB   (1)                     
076800            MOVE ZERO          TO LINK-KVOKS-BULK (1)                     
076900            MOVE ZERO          TO LINK-KVOKS-DAG  (1)                     
077000            MOVE ZERO          TO LINK-KVOKS-VOR  (1)                     
077100            MOVE ZERO          TO LINK-SUTPO-TOT  (2)                     
077200            MOVE ZERO          TO LINK-SUTPO-PB   (2)                     
077300            MOVE ZERO          TO LINK-KVOKS-BULK (2)                     
077400            MOVE ZERO          TO LINK-KVOKS-DAG  (2)                     
077500            MOVE ZERO          TO LINK-KVOKS-VOR  (2)                     
077600         END-IF                                                           
077700     END-IF                                                               
077800     SKIP1                                                                
077900     MOVE ZERO TO LINK-TIXLEVSP                                           
078000     SKIP1                                                                
078100                  LINK-KDLPORS-TAB (1)                                    
078200                  LINK-KDLPORS-TAB (2)                                    
078300                  LINK-KDLPORS-TAB (3)                                    
078400                  LINK-KVBEST-PL                                          
078500     SKIP1                                                                
078600     .                                                                    
078700     EJECT                                                                
078800 AA-JUSTERA-MED-RETURER SECTION.                                          
078810     MOVE 'AA-JUSTERA-MED-RETURER '  TO CURRENT-SECTION                   
078900     SKIP3                                                                
079000     PERFORM IMS-GU-INLE01-WDL201                                         
079100     IF SEGMENT-FINNS                                                     
079200        MOVE ZERO TO SUM-RETUR                                            
079300        MOVE 310  TO W-IDPTYP                                             
079400        PERFORM IMS-GNP-INLE21-WDL221                                     
079500        PERFORM UNTIL SEGMENT-SAKNAS                                      
079600           IF MOT-KDRT = 7 OR 77                                          
079700              COMPUTE SUM-RETUR = SUM-RETUR +                             
079800                      MOT-KVAVIS - MOT-KVANTMOT                           
079900           END-IF                                                         
080000           PERFORM IMS-GNP-INLE21-WDL221                                  
080100        END-PERFORM                                                       
080200        IF SUM-RETUR < ZERO                                               
080300           MOVE ZERO TO SUM-RETUR                                         
080400        END-IF                                                            
080500        SUBTRACT SUM-RETUR           FROM LINK-KVAKS (1)                  
080600        IF LINK-KVAKS (1) < ZERO                                          
080700           MOVE ZERO TO LINK-KVAKS (1)                                    
080800        END-IF                                                            
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200 AB-HAEMTA-PRARTBES SECTION.                                              
081210     MOVE 'AB-HAEMTA-PRARTBES '  TO CURRENT-SECTION                       
081300                                                                          
081400     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-AAAAMMDD                 
081500     COMPUTE W-DAPRLIST = 99999999 - W-DAGENS-AAAAMMDD                    
081600     PERFORM IMS-GNP-PRL-WDK621                                           
081700     IF SEGMENT-SAKNAS                                                    
081800        MOVE CLAG-PRARTSTD       TO LINK-PRARTBES                         
081900     ELSE                                                                 
082000       MOVE NEJ                 TO WS-PRARTBES                            
082100       PERFORM UNTIL  SEGMENT-SAKNAS                                      
082200         IF PRL-SUINLEV-PR > ZERO                                         
082300           MOVE PRL-PRARTBES-PR  TO LINK-PRARTBES                         
082400           SET SEGMENT-SAKNAS TO TRUE                                     
082500         ELSE                                                             
082600           IF WS-PRARTBES = NEJ                                           
082700             MOVE PRL-PRARTBES-PR TO LINK-PRARTBES                        
082800             MOVE JA              TO WS-PRARTBES                          
082900           END-IF                                                         
083000           PERFORM IMS-GNP-PRL-WDK621                                     
083100         END-IF                                                           
083200       END-PERFORM                                                        
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 B-LAES-LEVERANTOER SECTION.                                              
083610     MOVE 'B-LAES-LEVERANTOER '  TO CURRENT-SECTION                       
083700     SKIP3                                                                
083800     MOVE LINK-IDARTNR TO W-IDARTNR                                       
083900                          W-IDARTNR-D9                                    
084000     MOVE WC-CDC-SE    TO W-IDDC-D9                                       
084100     IF ROT-INLB-SAKNAS                                                   
084200         PERFORM IMS-GU-ROT-LP-WDD901                                     
084300         IF SEGMENT-FINNS                                                 
084400            MOVE JA      TO ROT-INLB-SW                                   
084500         ELSE                                                             
084600            MOVE NEJ     TO ROT-INLB-SW                                   
084700            MOVE NEJ     TO LINK-FLJANEJ-ANROP                            
084800            MOVE SPACE   TO LINK-IDLEVNR                                  
084900            MOVE ZERO    TO LINK-KVBR                                     
085000         END-IF                                                           
085100     END-IF                                                               
085200     IF ROT-INLB-FINNS                                                    
085300         PERFORM IMS-GNP-LEVERANTOER-WDD902                               
085400         IF SEGMENT-FINNS                                                 
085500             MOVE JA TO LINK-FLJANEJ-ANROP                                
085600             MOVE LEV-IDLEVNR TO LINK-IDLEVNR                             
085700             MOVE LEV-KVBR TO LINK-KVBR                                   
085800         ELSE                                                             
085900             MOVE NEJ TO LINK-FLJANEJ-ANROP                               
086000             MOVE SPACE  TO LINK-IDLEVNR                                  
086100             MOVE ZERO   TO LINK-KVBR                                     
086200         END-IF                                                           
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600 C-LAES-OMSPEC SECTION.                                                   
086610     MOVE 'C-LAES-OMSPEC ' TO CURRENT-SECTION                             
086700     SKIP3                                                                
086800     IF ROT-INLB-FINNS                                                    
086900         MOVE LINK-IDLEVNR TO W-IDLEVNR                                   
087000         PERFORM IMS-GNP-OMSPEC-WDD904                                    
087100         IF SEGMENT-FINNS                                                 
087200             MOVE OMSPEC-KVBEST-PL TO LINK-KVBEST-PL                      
087300             MOVE OMSPEC-KDLPORS-TAB (1) TO LINK-KDLPORS-TAB (1)          
087400             MOVE OMSPEC-KDLPORS-TAB (2) TO LINK-KDLPORS-TAB (2)          
087500             MOVE OMSPEC-KDLPORS-TAB (3) TO LINK-KDLPORS-TAB (3)          
087600         END-IF                                                           
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000 D-LAES-ARTIKEL SECTION.                                                  
088010     MOVE 'D-LAES-ARTIKEL '  TO CURRENT-SECTION                           
088100     SKIP3                                                                
088200     MOVE LINK-IDARTNR TO W-IDARTNR                                       
088300     PERFORM IMS-GU-ARTIKEL-KVAL-WDK601                                   
088400     MOVE JA TO LINK-FLJANEJ-ANROP                                        
088500     .                                                                    
088600     EJECT                                                                
100900 M-LAES-AVROP-FIRST SECTION.                                              
100910     MOVE 'M-LAES-AVROP-FIRST '  TO CURRENT-SECTION                       
101000     SKIP1                                                                
101100     IF ROT-INLB-FINNS                                                    
101200         MOVE LINK2-KDAVROP TO W-KDAVROP                                  
101300         MOVE NEJ TO LINK2-FLJANEJ-ANROP                                  
101400         PERFORM IMS-GNP-AVROP-WDD905-FIRST                               
101500         SKIP1                                                            
101600         IF SEGMENT-FINNS                                                 
101700             MOVE JA TO LINK2-FLJANEJ-ANROP                               
101800             MOVE AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                     
101900             MOVE WS-DAAVROP-AAVV   TO LINK2-TIAVROP-AVS                  
102000***      IF AVROP-TIAVRDAT-INL > ZERO                                     
102100             MOVE AVROP-TIAVRDAT-INL   TO DAT-I-TIDATUM                   
102200             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
102300             CALL WDATKONV USING          DAT-KDDATFORM                   
102400                                          DAT-I-TIDATUM                   
102500                                          DAT-O-TIDATUM                   
102600                                          DAT-KDSVAR                      
102700             IF DAT-KDSVAR-FEL                                            
102800               MOVE 32 TO RKOD                                            
102900               MOVE 'FEL VID ANROP TILL DATKONV 1' TO FELTEXT-STR         
103000               DISPLAY FELTEXT                                            
103100               CALL ABEND USING RKOD                                      
103200             ELSE                                                         
103300               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
103400               MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL               
103500             END-IF                                                       
103600***       ELSE                                                            
103700***            MOVE ZERO               TO LINK2-TIAVROP-INL               
103800***       END-IF                                                          
103900***       IF AVROP-TIAVRDAT-DISP > ZERO                                   
104000             MOVE AVROP-TIAVRDAT-DISP  TO DAT-I-TIDATUM                   
104100             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
104200             CALL WDATKONV USING          DAT-KDDATFORM                   
104300                                          DAT-I-TIDATUM                   
104400                                          DAT-O-TIDATUM                   
104500                                          DAT-KDSVAR                      
104600             IF DAT-KDSVAR-FEL                                            
104700               MOVE 32 TO RKOD                                            
104800               MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT-STR         
104900               DISPLAY FELTEXT                                            
105000               CALL ABEND USING RKOD                                      
105100             ELSE                                                         
105200               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
105300               MOVE WS-TIAAVV          TO LINK2-TIAVROP-DISP              
105400             END-IF                                                       
105500***       ELSE                                                            
105600***            MOVE ZERO               TO LINK2-TIAVROP-DISP              
105700***       END-IF                                                          
105800             MOVE AVROP-KVAVROP   TO LINK2-KVAVROP                        
105900             MOVE WDD9-KEY-02-IDLEVNR TO LINK2-IDLEVNR                    
106000         ELSE                                                             
106100             MOVE NEJ TO LINK2-FLJANEJ-ANROP                              
106200             MOVE ZERO            TO     LINK2-TIAVROP-AVS                
106300                                         LINK2-TIAVROP-INL                
106400                                         LINK2-TIAVROP-DISP               
106500                                         LINK2-KVAVROP                    
106600             MOVE SPACE           TO     LINK2-IDLEVNR                    
106700         END-IF                                                           
106800     ELSE                                                                 
106900         MOVE NEJ TO LINK2-FLJANEJ-ANROP                                  
107000         MOVE ZERO                TO LINK2-TIAVROP-AVS                    
107100                                     LINK2-TIAVROP-INL                    
107200                                     LINK2-TIAVROP-DISP                   
107300                                     LINK2-KVAVROP                        
107400         MOVE SPACE               TO LINK2-IDLEVNR                        
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 N-LAES-AVROP-NEXT  SECTION.                                              
107810     MOVE 'N-LAES-AVROP-NEXT '  TO CURRENT-SECTION                        
107900     SKIP1                                                                
108000     IF ROT-INLB-FINNS                                                    
108100         MOVE LINK2-KDAVROP TO W-KDAVROP                                  
108200         PERFORM IMS-GNP-AVROP-WDD905-NEXT                                
108300         SKIP1                                                            
108400         IF SEGMENT-FINNS                                                 
108500             MOVE JA TO LINK2-FLJANEJ-ANROP                               
108600             MOVE AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                     
108700             MOVE WS-DAAVROP-AAVV   TO LINK2-TIAVROP-AVS                  
108800***      IF AVROP-TIAVRDAT-INL > ZERO                                     
108900             MOVE AVROP-TIAVRDAT-INL   TO DAT-I-TIDATUM                   
109000             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
109100             CALL WDATKONV USING          DAT-KDDATFORM                   
109200                                          DAT-I-TIDATUM                   
109300                                          DAT-O-TIDATUM                   
109400                                          DAT-KDSVAR                      
109500             IF DAT-KDSVAR-FEL                                            
109600               MOVE 32 TO RKOD                                            
109700               MOVE 'FEL VID ANROP TILL DATKONV 3' TO FELTEXT-STR         
109800               DISPLAY FELTEXT                                            
109900               CALL ABEND USING RKOD                                      
110000             ELSE                                                         
110100               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
110200               MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL               
110300             END-IF                                                       
110400***       ELSE                                                            
110500***            MOVE ZERO               TO LINK2-TIAVROP-INL               
110600***       END-IF                                                          
110700***       IF AVROP-TIAVRDAT-DISP > ZERO                                   
110800             MOVE AVROP-TIAVRDAT-DISP  TO DAT-I-TIDATUM                   
110900             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
111000             CALL WDATKONV USING          DAT-KDDATFORM                   
111100                                          DAT-I-TIDATUM                   
111200                                          DAT-O-TIDATUM                   
111300                                          DAT-KDSVAR                      
111400             IF DAT-KDSVAR-FEL                                            
111500               MOVE 32 TO RKOD                                            
111600               MOVE 'FEL VID ANROP TILL DATKONV 4' TO FELTEXT-STR         
111700               DISPLAY FELTEXT                                            
111800               CALL ABEND USING RKOD                                      
111900             ELSE                                                         
112000               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
112100               MOVE WS-TIAAVV          TO LINK2-TIAVROP-DISP              
112200             END-IF                                                       
112300***       ELSE                                                            
112400***            MOVE ZERO               TO LINK2-TIAVROP-DISP              
112500***       END-IF                                                          
112600             MOVE AVROP-KVAVROP   TO LINK2-KVAVROP                        
112700             MOVE WDD9-KEY-02-IDLEVNR TO LINK2-IDLEVNR                    
112800         ELSE                                                             
112900             MOVE NEJ TO LINK2-FLJANEJ-ANROP                              
113000             MOVE ZERO            TO     LINK2-TIAVROP-AVS                
113100                                         LINK2-TIAVROP-INL                
113200                                         LINK2-TIAVROP-DISP               
113300                                         LINK2-KVAVROP                    
113400             MOVE SPACE           TO     LINK2-IDLEVNR                    
113500         END-IF                                                           
113600     ELSE                                                                 
113700         MOVE NEJ TO LINK2-FLJANEJ-ANROP                                  
113800         MOVE ZERO                TO LINK2-TIAVROP-AVS                    
113900                                     LINK2-TIAVROP-INL                    
114000                                     LINK2-TIAVROP-DISP                   
114100                                     LINK2-KVAVROP                        
114200         MOVE SPACE               TO LINK2-IDLEVNR                        
114300     END-IF                                                               
114400     .                                                                    
114500     EJECT                                                                
114600 O-LAES-AVROP-KVAL  SECTION.                                              
114610     MOVE 'O-LAES-AVROP-KVAL '  TO CURRENT-SECTION                        
114700     SKIP1                                                                
114800     IF ROT-INLB-FINNS                                                    
114900         MOVE LINK2-IDLEVNR TO W-IDLEVNR                                  
115000         MOVE LINK2-TIAVROP-AVS TO WS-DAAVROP-AAVV                        
115100         IF WS-DAAVROP-AAVV > 5000                                        
115200            MOVE 19         TO WS-DAAVROP-SS                              
115300         ELSE                                                             
115400            MOVE 20         TO WS-DAAVROP-SS                              
115500         END-IF                                                           
115600         MOVE WS-DAAVROP-AVS    TO W-DAAVROP                              
115700         MOVE LINK2-KDAVROP TO W-KDAVROP                                  
115800         PERFORM IMS-GNP-AVROP-WDD905-KVAL                                
115900         SKIP1                                                            
116000         IF SEGMENT-FINNS                                                 
116100             MOVE JA TO LINK2-FLJANEJ-ANROP                               
116200             MOVE AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                     
116300             MOVE WS-DAAVROP-AAVV   TO LINK2-TIAVROP-AVS                  
116400***      IF AVROP-TIAVRDAT-INL > ZERO                                     
116500             MOVE AVROP-TIAVRDAT-INL   TO DAT-I-TIDATUM                   
116600             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
116700             CALL WDATKONV USING          DAT-KDDATFORM                   
116800                                          DAT-I-TIDATUM                   
116900                                          DAT-O-TIDATUM                   
117000                                          DAT-KDSVAR                      
117100             IF DAT-KDSVAR-FEL                                            
117200               MOVE 32 TO RKOD                                            
117300               MOVE 'FEL VID ANROP TILL DATKONV 5' TO FELTEXT-STR         
117400               DISPLAY FELTEXT                                            
117500               CALL ABEND USING RKOD                                      
117600             ELSE                                                         
117700               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
117800               MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL               
117900             END-IF                                                       
118000***       ELSE                                                            
118100***            MOVE ZERO               TO LINK2-TIAVROP-INL               
118200***       END-IF                                                          
118300***       IF AVROP-TIAVRDAT-DISP > ZERO                                   
118400             MOVE AVROP-TIAVRDAT-DISP  TO DAT-I-TIDATUM                   
118500             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
118600             CALL WDATKONV USING          DAT-KDDATFORM                   
118700                                          DAT-I-TIDATUM                   
118800                                          DAT-O-TIDATUM                   
118900                                          DAT-KDSVAR                      
119000             IF DAT-KDSVAR-FEL                                            
119100               MOVE 32 TO RKOD                                            
119200               MOVE 'FEL VID ANROP TILL DATKONV 6' TO FELTEXT-STR         
119300               DISPLAY FELTEXT                                            
119400               CALL ABEND USING RKOD                                      
119500             ELSE                                                         
119600               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
119700               MOVE WS-TIAAVV          TO LINK2-TIAVROP-DISP              
119800             END-IF                                                       
119900***       ELSE                                                            
120000***            MOVE ZERO               TO LINK2-TIAVROP-DISP              
120100***       END-IF                                                          
120200             MOVE AVROP-KVAVROP   TO LINK2-KVAVROP                        
120300         ELSE                                                             
120400             MOVE NEJ TO LINK2-FLJANEJ-ANROP                              
120500             MOVE ZERO            TO LINK2-TIAVROP-AVS                    
120600                                         LINK2-TIAVROP-INL                
120700                                         LINK2-TIAVROP-DISP               
120800                                         LINK2-KVAVROP                    
120900         END-IF                                                           
121000     ELSE                                                                 
121100       MOVE NEJ TO LINK2-FLJANEJ-ANROP                                    
121200       MOVE ZERO                  TO LINK2-TIAVROP-AVS                    
121300                                   LINK2-TIAVROP-INL                      
121400                                   LINK2-TIAVROP-DISP                     
121500                                   LINK2-KVAVROP                          
121600     END-IF                                                               
121700     .                                                                    
121800     EJECT                                                                
134600 U-LAES-AVROP-LEV-F SECTION.                                              
134610     MOVE 'U-LAES-AVROP-LEV-F '  TO CURRENT-SECTION                       
134700     SKIP1                                                                
134800     IF ROT-INLB-FINNS                                                    
134900         MOVE LINK2-IDLEVNR TO W-IDLEVNR                                  
135000         MOVE LINK2-KDAVROP TO W-KDAVROP                                  
135100         PERFORM IMS-GNP-AVROP-KVAL-LEV-D905-F                            
135200         SKIP1                                                            
135300         IF SEGMENT-FINNS                                                 
135400             MOVE JA TO LINK2-FLJANEJ-ANROP                               
135500             MOVE AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                     
135600             MOVE WS-DAAVROP-AAVV   TO LINK2-TIAVROP-AVS                  
135700***      IF AVROP-TIAVRDAT-INL > ZERO                                     
135800             MOVE AVROP-TIAVRDAT-INL   TO DAT-I-TIDATUM                   
135900             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
136000             CALL WDATKONV USING          DAT-KDDATFORM                   
136100                                          DAT-I-TIDATUM                   
136200                                          DAT-O-TIDATUM                   
136300                                          DAT-KDSVAR                      
136400             IF DAT-KDSVAR-FEL                                            
136500               MOVE 32 TO RKOD                                            
136600               MOVE 'FEL VID ANROP TILL DATKONV 9' TO FELTEXT-STR         
136700               DISPLAY FELTEXT                                            
136800               CALL ABEND USING RKOD                                      
136900             ELSE                                                         
137000               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
137100               MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL               
137200             END-IF                                                       
137300***       ELSE                                                            
137400***            MOVE ZERO               TO LINK2-TIAVROP-INL               
137500***       END-IF                                                          
137600***       IF AVROP-TIAVRDAT-DISP > ZERO                                   
137700             MOVE AVROP-TIAVRDAT-DISP  TO DAT-I-TIDATUM                   
137800             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
137900             CALL WDATKONV USING          DAT-KDDATFORM                   
138000                                          DAT-I-TIDATUM                   
138100                                          DAT-O-TIDATUM                   
138200                                          DAT-KDSVAR                      
138300             IF DAT-KDSVAR-FEL                                            
138400               MOVE 32 TO RKOD                                            
138500               MOVE 'FEL VID ANROP TILL DATKONV A' TO FELTEXT-STR         
138600               DISPLAY FELTEXT                                            
138700               CALL ABEND USING RKOD                                      
138800             ELSE                                                         
138900               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
139000               MOVE WS-TIAAVV          TO LINK2-TIAVROP-DISP              
139100             END-IF                                                       
139200***      ELSE                                                             
139300***            MOVE ZERO               TO LINK2-TIAVROP-DISP              
139400***      END-IF                                                           
139500***      IF AVROP-TIAVRDAT-INL > ZERO                                     
139600             MOVE AVROP-TIAVRDAT-INL   TO DAT-I-TIDATUM                   
139700             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
139800             CALL WDATKONV USING          DAT-KDDATFORM                   
139900                                          DAT-I-TIDATUM                   
140000                                          DAT-O-TIDATUM                   
140100                                          DAT-KDSVAR                      
140200             IF DAT-KDSVAR-FEL                                            
140300               MOVE 32 TO RKOD                                            
140400               MOVE 'FEL VID ANROP TILL DATKONV B' TO FELTEXT-STR         
140500               DISPLAY FELTEXT                                            
140600               CALL ABEND USING RKOD                                      
140700             ELSE                                                         
140800               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
140900               MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL               
141000             END-IF                                                       
141100***       ELSE                                                            
141200***            MOVE ZERO               TO LINK2-TIAVROP-INL               
141300***       END-IF                                                          
141400***       IF AVROP-TIAVRDAT-DISP > ZERO                                   
141500             MOVE AVROP-TIAVRDAT-DISP  TO DAT-I-TIDATUM                   
141600             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
141700             CALL WDATKONV USING          DAT-KDDATFORM                   
141800                                          DAT-I-TIDATUM                   
141900                                          DAT-O-TIDATUM                   
142000                                          DAT-KDSVAR                      
142100             IF DAT-KDSVAR-FEL                                            
142200               MOVE 32 TO RKOD                                            
142300               MOVE 'FEL VID ANROP TILL DATKONV C' TO FELTEXT-STR         
142400               DISPLAY FELTEXT                                            
142500               CALL ABEND USING RKOD                                      
142600             ELSE                                                         
142700               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
142800               MOVE WS-TIAAVV          TO LINK2-TIAVROP-DISP              
142900             END-IF                                                       
143000***       ELSE                                                            
143100***            MOVE ZERO               TO LINK2-TIAVROP-DISP              
143200***       END-IF                                                          
143300             MOVE AVROP-KVAVROP   TO LINK2-KVAVROP                        
143400         ELSE                                                             
143500             MOVE NEJ TO LINK2-FLJANEJ-ANROP                              
143600             MOVE ZERO            TO LINK2-TIAVROP-AVS                    
143700                                         LINK2-TIAVROP-INL                
143800                                         LINK2-TIAVROP-DISP               
143900                                         LINK2-KVAVROP                    
144000         END-IF                                                           
144100     ELSE                                                                 
144200         MOVE NEJ TO LINK2-FLJANEJ-ANROP                                  
144300         MOVE ZERO                TO LINK2-TIAVROP-AVS                    
144400                                     LINK2-TIAVROP-INL                    
144500                                     LINK2-TIAVROP-DISP                   
144600                                     LINK2-KVAVROP                        
144700     END-IF                                                               
144800     .                                                                    
144900     EJECT                                                                
145000 V-LAES-AVROP-LEV-N SECTION.                                              
145010     MOVE 'V-LAES-AVROP-LEV-N '  TO CURRENT-SECTION                       
145100     SKIP1                                                                
145200     IF ROT-INLB-FINNS                                                    
145300         MOVE LINK2-IDLEVNR TO W-IDLEVNR                                  
145400         MOVE LINK2-KDAVROP TO W-KDAVROP                                  
145500         PERFORM IMS-GNP-AVROP-KVAL-LEV-D905-N                            
145600         SKIP1                                                            
145700         IF SEGMENT-FINNS                                                 
145800             MOVE JA TO LINK2-FLJANEJ-ANROP                               
145900             MOVE AVROP-DAAVROP-AVS TO WS-DAAVROP-AVS                     
146000             MOVE WS-DAAVROP-AAVV   TO LINK2-TIAVROP-AVS                  
146100***      IF AVROP-TIAVRDAT-INL > ZERO                                     
146200             MOVE AVROP-TIAVRDAT-INL   TO DAT-I-TIDATUM                   
146300             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
146400             CALL WDATKONV USING          DAT-KDDATFORM                   
146500                                          DAT-I-TIDATUM                   
146600                                          DAT-O-TIDATUM                   
146700                                          DAT-KDSVAR                      
146800             IF DAT-KDSVAR-FEL                                            
146900               MOVE 32 TO RKOD                                            
147000               MOVE 'FEL VID ANROP TILL DATKONV D' TO FELTEXT-STR         
147100               DISPLAY FELTEXT                                            
147200               CALL ABEND USING RKOD                                      
147300             ELSE                                                         
147400               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
147500               MOVE WS-TIAAVV          TO LINK2-TIAVROP-INL               
147600             END-IF                                                       
147700***       ELSE                                                            
147800***            MOVE ZERO               TO LINK2-TIAVROP-INL               
147900***       END-IF                                                          
148000***       IF AVROP-TIAVRDAT-DISP > ZERO                                   
148100             MOVE AVROP-TIAVRDAT-DISP  TO DAT-I-TIDATUM                   
148200             MOVE 'AAMMDD'             TO DAT-KDDATFORM                   
148300             CALL WDATKONV USING          DAT-KDDATFORM                   
148400                                          DAT-I-TIDATUM                   
148500                                          DAT-O-TIDATUM                   
148600                                          DAT-KDSVAR                      
148700             IF DAT-KDSVAR-FEL                                            
148800               MOVE 32 TO RKOD                                            
148900               MOVE 'FEL VID ANROP TILL DATKONV E' TO FELTEXT-STR         
149000               DISPLAY FELTEXT                                            
149100               CALL ABEND USING RKOD                                      
149200             ELSE                                                         
149300               MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                       
149400               MOVE WS-TIAAVV          TO LINK2-TIAVROP-DISP              
149500             END-IF                                                       
149600***       ELSE                                                            
149700***            MOVE ZERO               TO LINK2-TIAVROP-DISP              
149800***       END-IF                                                          
149900             MOVE AVROP-KVAVROP   TO LINK2-KVAVROP                        
150000         ELSE                                                             
150100             MOVE NEJ TO LINK2-FLJANEJ-ANROP                              
150200             MOVE ZERO            TO LINK2-TIAVROP-AVS                    
150300                                         LINK2-TIAVROP-INL                
150400                                         LINK2-TIAVROP-DISP               
150500                                         LINK2-KVAVROP                    
150600         END-IF                                                           
150700     ELSE                                                                 
150800         MOVE NEJ TO LINK2-FLJANEJ-ANROP                                  
150900         MOVE ZERO                TO LINK2-TIAVROP-AVS                    
151000                                     LINK2-TIAVROP-INL                    
151100                                     LINK2-TIAVROP-DISP                   
151200                                     LINK2-KVAVROP                        
151300     END-IF                                                               
151400     .                                                                    
151500     EJECT                                                                
151600*W-                                                                       
151700*X-                                                                       
151800*Y-                                                                       
151900 Z-LAES-WDB6-DC-INFO  SECTION.                                            
151910     MOVE 'Z-LAES-WDB6-DC-INFO '  TO CURRENT-SECTION                      
152000     SKIP2                                                                
152100* FYLLER DC-TABELLEN MED ALLA DATABAS-RECORD FRÅN WDB6                    
152200* FÖR ATT SLIPPA BAS-LÄSNING FÖR VARJE ART I INFILEN.                     
152300     SET DCIX TO +1                                                       
152400     PERFORM IMS-GN-WDB601                                                
152500     PERFORM UNTIL SEGMENT-SLUT                                           
152600                                                                          
152700       IF DCIX <= DC-MAX                                                  
152800         Move DCS-IDDC To T-DCS-IDDC(DCIX)                                
152900         Move DCS-KDDC To T-DCS-KDDC(DCIX)                                
153000         Move DCS-FLOVRLAGBER To T-DCS-FLOVRLAGBER(DCIX)                  
153100         MOVE DCS-IDLEVNR-DC  TO T-DCS-IDLEVNR-DC(DCIX)                   
153200         SET DCIX UP BY +1                                                
153300         PERFORM IMS-GN-WDB601                                            
153400       ELSE                                                               
153500                                                                          
153600           MOVE 35 TO RKOD                                                
153700           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT-STR               
153800*          -- OBS. DETTA ÄR FÖRSTA CALL MOT W2214010                      
153900*          -- INGEN ARTIKEL I W2214000 ÄR ÄNNU PROCESSAD                  
154000           DISPLAY FELTEXT                                                
154100           CALL ABEND USING RKOD                                          
154200       END-IF                                                             
154300     END-PERFORM                                                          
154400                                                                          
154500*****  --- SÄTTER TAKET PÅ TABELLEN                                       
154600     SET DCIX  DOWN BY +1                                                 
154700     SET DC-MAX TO DCIX                                                   
154800                                                                          
154900*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
155000     MOVE DC-MAX                  TO ANTAL                                
155100     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
155200     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
155300*                                                                         
155400     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
155500                  T-DCS-IDDC(1) NYCKELLANGD                               
155600                                                                          
155700     DISPLAY 'Alla IDDC på WDB6 laddade i DC-TAB'                         
155800     DISPLAY 'Antal = ' DC-MAX                                            
155900     DISPLAY '----TOP-----'                                               
156000     SET DCIX TO +1                                                       
156100     PERFORM UNTIL DCIX > DC-MAX                                          
156200       SET WDCIX TO DCIX                                                  
156300       DISPLAY  '  TAB-RAD ' WDCIX ' IDDC '  T-DCS-IDDC(DCIX)             
156400       SET DCIX UP BY +1                                                  
156500     END-PERFORM                                                          
156600     DISPLAY '----END-----'                                               
156700     .                                                                    
156800     EJECT                                                                
156900 S10-KOLLA-LEVNR-REFILL  SECTION.                                         
157000     MOVE 'S10-KOLLA-LEVNR-REFILL '  TO CURRENT-SECTION                   
157100                                                                          
157200     SET DCIX TO +1                                                       
157300     SEARCH DC-TAB                                                        
157400        AT END                                                            
157500           MOVE NEJ TO DCS-TRAEFF                                         
157600        WHEN T-DCS-IDLEVNR-DC (DCIX) = ART-IDLEVNR                        
157700           MOVE JA  TO DCS-TRAEFF                                         
157800           CONTINUE                                                       
157900     END-SEARCH                                                           
158000                                                                          
158100     IF DCS-TRAEFF = JA                                                   
158200       MOVE NEJ TO SW-OK                                                  
158300     END-IF                                                               
158400                                                                          
158500     .                                                                    
158600     EJECT                                                                
158700 IMS-GN-WDB601    SECTION.                                                
158710     MOVE 'IMS-GN-WDB601 '  TO DBS-SECTION                                
158720                                                                          
158800     STRING 'WDB601   '                                                   
158900          DELIMITED BY SIZE INTO SSA1                                     
159000     MOVE '  GB' TO GODK-STATUSKODER                                      
159100     CALL CBLTDLI USING GN WDB6-PCB  DLI-IO-AREA-B6 SSA1                  
159200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
159300     PERFORM IMS-STATUSKONTROLL                                           
159400     .                                                                    
159500     EJECT                                                                
159600 IMS-GN-ARTIKEL-WDK601 SECTION.                                           
159620     MOVE 'IMS-GN-ARTIKEL-WDK601'  TO DBS-SECTION                         
159700     SKIP1                                                                
159800     MOVE 'WDK601 ' TO SSA1                                               
159900     MOVE '  GB' TO GODK-STATUSKODER                                      
160000     CALL CBLTDLI USING GN   WDK6-PCB DLI-IO-WDK601 SSA1                  
160100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
160200     PERFORM IMS-STATUSKONTROLL                                           
160300     SKIP3                                                                
160400     .                                                                    
160410     EJECT                                                                
160500 IMS-GNP-CLAG-WDK611 SECTION.                                             
160520     MOVE 'IMS-GNP-CLAG-WDK611 '  TO DBS-SECTION                          
160600     SKIP1                                                                
160700     STRING 'WDK611  (KDSEGKEY =1)'                                       
160800            DELIMITED BY SIZE INTO SSA1                                   
160900     MOVE '  ' TO GODK-STATUSKODER                                        
161000     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK611 SSA1                  
161100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
161200     PERFORM IMS-STATUSKONTROLL                                           
161300     SKIP3                                                                
161400     .                                                                    
161410     EJECT                                                                
161500 IMS-GNP-AVTAL-WDK623 SECTION.                                            
161520     MOVE ' IMS-GNP-AVTAL-WDK623 '  TO DBS-SECTION                        
161600     SKIP1                                                                
161700     STRING 'WDK611  (KDSEGKEY =1)'                                       
161800            DELIMITED BY SIZE INTO SSA1                                   
161900     MOVE 'WDK623 ' TO SSA2                                               
162000     MOVE '  GE' TO GODK-STATUSKODER                                      
162100     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK623 SSA1 SSA2             
162200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162300     PERFORM IMS-STATUSKONTROLL                                           
162400     .                                                                    
162500     EJECT                                                                
163600 IMS-GNP-PRL-WDK621   SECTION.                                            
163620     MOVE 'IMS-GNP-PRL-WDK621 '  TO DBS-SECTION                           
163700     SKIP1                                                                
163800     STRING 'WDK621  (DAPRLIST=>' W-DAPRLIST-X                            
163900                    '&IDLEVNR  =' ART-IDLEVNR ')'                         
164000            DELIMITED BY SIZE INTO SSA1                                   
164100     MOVE '  GE'      TO GODK-STATUSKODER                                 
164200     CALL CBLTDLI USING GNP  WDK6-PCB DLI-IO-WDK621 SSA1                  
164300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     SKIP3                                                                
164600     .                                                                    
164700     EJECT                                                                
164800 IMS-GNP-LEVERANTOER-WDD902 SECTION.                                      
164820     MOVE 'IMS-GNP-LEVERANTOER-WDD902 '  TO DBS-SECTION                   
164900     SKIP1                                                                
165000     MOVE 'WDD902 ' TO SSA1                                               
165100     MOVE '  GE' TO GODK-STATUSKODER                                      
165200     CALL CBLTDLI USING GNP  WDD9-PCB DLI-IO-WDD902 SSA1                  
165300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
165400     PERFORM IMS-STATUSKONTROLL                                           
165500     .                                                                    
165600     EJECT                                                                
165700 IMS-GNP-OMSPEC-WDD904 SECTION.                                           
165720     MOVE 'IMS-GNP-OMSPEC-WDD904 '  TO DBS-SECTION                        
165800     SKIP1                                                                
165900     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
166000            DELIMITED BY SIZE INTO SSA1                                   
166100     MOVE 'WDD904 ' TO SSA2                                               
166200     MOVE '  GE' TO GODK-STATUSKODER                                      
166310     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD904 SSA1 SSA2              
166400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
166500     PERFORM IMS-STATUSKONTROLL                                           
166600     SKIP3                                                                
166700     .                                                                    
166710     EJECT                                                                
166800 IMS-GNP-AVROP-WDD905-FIRST SECTION.                                      
166820     MOVE 'IMS-GNP-AVROP-WDD905-FIRST '  TO DBS-SECTION                   
166900     SKIP1                                                                
167000     MOVE 'WDD902  *F'  TO SSA1                                           
167100     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
167200            DELIMITED BY SIZE INTO SSA2                                   
167300     MOVE '  GE' TO GODK-STATUSKODER                                      
167410     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
167500     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
167600     PERFORM IMS-STATUSKONTROLL                                           
167700     .                                                                    
167800     EJECT                                                                
167900 IMS-GNP-AVROP-WDD905-NEXT  SECTION.                                      
167920     MOVE 'IMS-GNP-AVROP-WDD905-NEXT '  TO DBS-SECTION                    
168000     SKIP1                                                                
168100     MOVE 'WDD902 ' TO SSA1                                               
168200     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
168300            DELIMITED BY SIZE INTO SSA2                                   
168400     MOVE '  GE' TO GODK-STATUSKODER                                      
168510     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
168600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     SKIP3                                                                
168900     .                                                                    
168910     EJECT                                                                
169000 IMS-GNP-AVROP-WDD905-KVAL SECTION.                                       
169020     MOVE 'IMS-GNP-AVROP-WDD905-KVAL '  TO DBS-SECTION                    
169100     SKIP1                                                                
169200     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
169300            DELIMITED BY SIZE INTO SSA1                                   
169400     STRING 'WDD905  *F(DAAVROP  =' W-DAAVROP-X                           
169500                      '&KDAVROP  =' W-KDAVROP-X ')'                       
169600            DELIMITED BY SIZE INTO SSA2                                   
169700     MOVE '  GE' TO GODK-STATUSKODER                                      
169810     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
169900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
170000     PERFORM IMS-STATUSKONTROLL                                           
170100     SKIP3                                                                
170200     .                                                                    
170210     EJECT                                                                
170300 IMS-GNP-AVROP-KVAL-LEV-D905-F SECTION.                                   
170320     MOVE 'IMS-GNP-AVROP-KVAL-LEV-D905-F '  TO DBS-SECTION                
170400     SKIP1                                                                
170500     STRING 'WDD902  *F(IDLEVNR  =' W-IDLEVNR-X ')'                       
170600            DELIMITED BY SIZE INTO SSA1                                   
170700     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
170800            DELIMITED BY SIZE INTO SSA2                                   
170900     MOVE '  GE' TO GODK-STATUSKODER                                      
171000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
171100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSKONTROLL                                           
171300     .                                                                    
171400     EJECT                                                                
171500 IMS-GNP-AVROP-KVAL-LEV-D905-N SECTION.                                   
171520     MOVE 'IMS-GNP-AVROP-KVAL-LEV-D905-N '  TO DBS-SECTION                
171600     SKIP1                                                                
171700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
171800            DELIMITED BY SIZE INTO SSA1                                   
171900     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
172000            DELIMITED BY SIZE INTO SSA2                                   
172100     MOVE '  GE' TO GODK-STATUSKODER                                      
172200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
172300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
172400     PERFORM IMS-STATUSKONTROLL                                           
172500     .                                                                    
172600     EJECT                                                                
172700 IMS-GU-ARTIKEL-KVAL-WDK601 SECTION.                                      
172720     MOVE 'IMS-GU-ARTIKEL-KVAL-WDK601'  TO DBS-SECTION                    
172800     SKIP1                                                                
172900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
173000            DELIMITED BY SIZE INTO SSA1                                   
173100     MOVE '  '   TO GODK-STATUSKODER                                      
173200     CALL CBLTDLI USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                  
173300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
173400     PERFORM IMS-STATUSKONTROLL                                           
173500     .                                                                    
173600     EJECT                                                                
173700 IMS-GU-ROT-LP-WDD901 SECTION.                                            
173720     MOVE 'IMS-GU-ROT-LP-WDD901 '  TO DBS-SECTION                         
173800     SKIP1                                                                
173900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
174000            DELIMITED BY SIZE INTO SSA1                                   
174100     MOVE '  GE' TO GODK-STATUSKODER                                      
174210     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD901 SSA1                   
174300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
174400     PERFORM IMS-STATUSKONTROLL                                           
174500     SKIP3                                                                
174600     .                                                                    
174700     EJECT                                                                
178600 IMS-GU-ARTM-ART-WDK901  SECTION.                                         
178620     MOVE 'IMS-GU-ARTM-ART-WDK901 '  TO DBS-SECTION                       
178700     SKIP1                                                                
178800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
178900            DELIMITED BY SIZE INTO SSA1                                   
179000     MOVE '  GE' TO GODK-STATUSKODER                                      
179100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-WDK901 SSA1                    
179200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
179300     PERFORM IMS-STATUSKONTROLL                                           
179400     .                                                                    
179500     EJECT                                                                
179600 IMS-GNP-ARTM-ANT-WDK911  SECTION.                                        
179620     MOVE 'IMS-GNP-ARTM-ANT-WDK911 '  TO DBS-SECTION                      
179700     SKIP1                                                                
179800     STRING 'WLARTM11(DABEHOV >=' W-DABEHOV-X-MIN                         
179900                    '&DABEHOV <=' W-DABEHOV-X-MAX ')'                     
180000            DELIMITED BY SIZE INTO SSA1                                   
180100     MOVE '  GE' TO GODK-STATUSKODER                                      
180200     CALL CBLTDLI USING GNP ARTM-PCB DLI-IO-WDK911 SSA1                   
180300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
180400     PERFORM IMS-STATUSKONTROLL                                           
180500     .                                                                    
180600     EJECT                                                                
180700 IMS-GET-WDK701-SDC    SECTION.                                           
180710     MOVE 'IMS-GET-WDK701-SDC '  TO DBS-SECTION                           
180800                                                                          
180900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
181000          DELIMITED BY SIZE INTO SSA1                                     
181100     MOVE '  GE' TO GODK-STATUSKODER                                      
181200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
181300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
181400     PERFORM IMS-STATUSKONTROLL                                           
181500     .                                                                    
181600     SKIP2                                                                
182510 IMS-GNP-WDK711-SDC-REF  SECTION.                                         
182520     MOVE 'IMS-GNP-WDK711-SDC-REF' TO DBS-SECTION                         
182521                                                                          
182530     STRING 'WDK711  (IDDCREF  =' W-IDDC-REF-X ')'                        
182531     DELIMITED BY SIZE INTO SSA1                                          
182550     MOVE '  GE' TO GODK-STATUSKODER                                      
182560     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
182570     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
182580     PERFORM IMS-STATUSKONTROLL                                           
182590     .                                                                    
182591     EJECT                                                                
182600 IMS-GU-INLE01-WDL201 SECTION.                                            
182610     MOVE 'IMS-GU-INLE01-WDL201 '  TO DBS-SECTION                         
182700                                                                          
182800     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
182900          DELIMITED BY SIZE INTO SSA1                                     
183000     MOVE '  GE' TO GODK-STATUSKODER                                      
183100     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
183200     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
183300     PERFORM IMS-STATUSKONTROLL                                           
183400     .                                                                    
183500     EJECT                                                                
183600 IMS-GNP-INLE21-WDL221 SECTION.                                           
183610     MOVE 'IMS-GNP-INLE21-WDL221 '  TO DBS-SECTION                        
183700                                                                          
183910     MOVE 'WDL211   ' TO SSA1                                             
184000     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
184100          DELIMITED BY SIZE INTO SSA2                                     
184200     MOVE '  GE' TO GODK-STATUSKODER                                      
184300     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
184400     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
184500     PERFORM IMS-STATUSKONTROLL                                           
184600     .                                                                    
184700     EJECT                                                                
184800 IMS-GET-LEVA01-WDF101 SECTION.                                           
184810     MOVE 'IMS-GET-LEVA01-WDF101 '  TO DBS-SECTION                        
184900                                                                          
185000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
185100          DELIMITED BY SIZE INTO SSA1                                     
185200     MOVE '  GE' TO GODK-STATUSKODER                                      
185300     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
185400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
185500     PERFORM IMS-STATUSKONTROLL                                           
185600     .                                                                    
185700     EJECT                                                                
187700 IMS-STATUSKONTROLL SECTION.                                              
187800     SKIP1                                                                
187900     SET STATUS-IX TO 1                                                   
188000     SEARCH GODK-STATUS AT END                                            
188100     DISPLAY IMS-WS                                                       
188200     CALL FELLOG                                                          
188300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
188400     CONTINUE                                                             
188500     END-SEARCH                                                           
188600     .                                                                    
188800     EJECT                                                                
188900*    -COPY WY2000P9                                                       
189000     EJECT                                                                
189100*    -COPY WY2000P1                                                       
189200     EJECT                                                                
189300*    -COPY WY2000P3                                                       

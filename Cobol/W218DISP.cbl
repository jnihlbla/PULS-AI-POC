000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W218DISP.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   92/03/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BERÄKNAR OCH UPDATERAR TIDISPIN                                  
000900*                                                                         
001000*         LÄSER      WLARTC      (WDK6)                                   
001100*                    WLARTS      (WDK7)                                   
001200*                    WLINLB      (WDD9)                                   
001300*                                                                         
001400*         UPPDATERAR WLARTC      (WDK611)                                 
001500*                                                                         
001600*                                                                         
001700*                                                                         
001800*        SVARSKODER TILL HUVUDPROGRAMMET.                                 
001900*                                                                         
002000*        DISP-KDSVAR = 1    ARTIKEL SAKNAS                                
002100*        DISP-KDSVAR = 2    KDERS-UTG > 0                                 
002200*        DISP-KDSVAR = 3    WDK611 UPPDATERAD                             
002300*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000                                                                          
003100                                                                          
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -COPY WY2000W2                                                       
004000                                                                          
004100*    -COPY WY2000W1                                                       
004200     SKIP3                                                                
004300 77  IDPGM                       PIC X(8)    VALUE 'W218DISP'.            
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  AKTIV                       PIC X       VALUE 'A'.                   
004800 77  PASSIV                      PIC X       VALUE 'P'.                   
004900                                                                          
005000*01  -COPY WWDCKONS                                                       
005100                                                                          
005200*    ---- ARBETSFÄLT                                                      
005300 77  Y2K-IX                      PIC  9(2)   VALUE 0.                     
005400 77  LB-IX                       PIC  9(2)   VALUE 0.                     
005500 77  LB-MAX                      PIC  9(2)   VALUE 0.                     
005600 77  AVROP-TEST-IX               PIC  9(1)   VALUE 0.                     
005700 77  AVROP-TEST-IX-MAX           PIC  9(1)   VALUE 0.                     
005800 77  AVROP-IX                    PIC  9(2)   VALUE 0.                     
005900 77  AVROP-MAX                   PIC  9(2)   VALUE 0.                     
006000 77  LB-30                       PIC  9(2)   VALUE 0.                     
006100 77  AVROP-15                    PIC  9(2)   VALUE 0.                     
006200                                                                          
006300 77  WS-IDARTNR                  PIC S9(9)   VALUE 0  COMP-3.             
006400 77  WS-LB-ANTAL                 PIC S9(7)   VALUE 0  COMP-3.             
006500 77  WS-LB-DISP-AAMMDD           PIC 9(6)    VALUE 0.                     
006600 77  WS-SISTA-DISP               PIC  9(7)   VALUE 0.                     
006700 77  WS-AVROP-ANTAL              PIC S9(7)   VALUE 0  COMP-3.             
006800 77  WS-AVROP-SUM-ANTAL          PIC S9(7)   VALUE 0  COMP-3.             
006900 77  WS-LB-SUM-ANTAL             PIC S9(7)   VALUE 0  COMP-3.             
007000 77  WS-AVROP-DAT-TIPP           PIC  9(3)   VALUE 0  COMP-3.             
007100 77  WS-AVROP-DISP-AAMMDD        PIC  9(6)   VALUE 0.                     
007200 77  WS-AVROP-DISP-AAMMDD-S3     PIC S9(6)   VALUE 0  COMP-3.             
007300 77  WS-AVROP-INL-AAMMDD         PIC  9(6)   VALUE 0.                     
007400 77  WS-UPPNADD-DISPDAT          PIC  9(7)   VALUE 0  COMP-3.             
007500                                                                          
007600 77  WS-PB-TOT                   PIC S9(9)V9(1) VALUE 0  COMP-3.          
007700 77  WS-VECKOBEHOV               PIC S9(9)V9(1) VALUE 0  COMP-3.          
007800 77  WS-KVAKS-TOT                PIC S9(7)      VALUE 0  COMP-3.          
007900 77  WS-KVPB-REF-SUM             PIC S9(9)V9(3) VALUE 0  COMP-3.          
008000 77  WS-KVPB-REF-WEEK-SUM        PIC S9(9)V9(3) VALUE 0  COMP-3.          
008100 77  WS-KVPB-REF-PROCPER         PIC S9(9)V9(1) VALUE 0  COMP-3.          
008200                                                                          
008300 77  UT-TIDISPIN                 PIC S9(7)      VALUE 0  COMP-3.          
008400                                                                          
008500 77  WS-ANTAL-PLUSVECKOR         PIC S9(3)      VALUE 0 COMP-3.           
008600 77  WS-DAGENS-DATUM-16V         PIC 9(6)       VALUE 0.                  
008700 77  WS-DAGENS-DATUM-16V-PACK    PIC 9(6)       VALUE 0.                  
008800 77  WS-IN-DATUM                 PIC 9(6)       VALUE 0.                  
008900 77  WS-UT-DATUM                 PIC 9(6)       VALUE 0.                  
009000                                                                          
009100 77  WS-TIFINLV-AAMMDD           PIC 9(6)       VALUE 0.                  
009200                                                                          
009300 01  ARBETSAREOR.                                                         
009400     03 WS-KDPRODSL              PIC 9(3)      VALUE ZERO.                
009500     03 WS-KDPRODSL-DELAR REDEFINES WS-KDPRODSL.                          
009600        05 WS-KDPRODSL-1-2       PIC 9(2).                                
009700        05 WS-KDPRODSL-3         PIC 9(1).                                
009800     03 WS-IDPROD                PIC S9(3)     VALUE ZERO COMP-3.         
009900******************************************************************        
010000*    SWITCHAR                                                             
010100******************************************************************        
010200                                                                          
010300 01   SWITCHAR.                                                           
010400     03  BERAKNA-TIDISPIN        PIC X(1)    VALUE 'N'.                   
010500     03  UPPDATERA-TIDISPIN      PIC X(1)    VALUE 'N'.                   
010600     03  BEHOV-TACKT             PIC X(1)    VALUE 'N'.                   
010700     03  BAD-DILIVER             PIC X(1)    VALUE 'N'.                   
010800     03  DILIVER-LONG-AWAY       PIC X(1)    VALUE 'N'.                   
010900     03  BEFORE-LAST-LEVBESK     PIC X(1)    VALUE 'N'.                   
011000     03  LEVERANSBESKED-FINNS    PIC X(1)    VALUE 'N'.                   
011100     03  FL-REST-AV-LEVTAB-TOM   PIC X(1)    VALUE 'N'.                   
011200     03  AVROP-FINNS             PIC X(1)    VALUE 'N'.                   
011300     03  FL2228-SEGMENT-FINNS    PIC X(1)    VALUE 'N'.                   
011400     03  FL2228-BAS-SLUT         PIC X(1)    VALUE 'N'.                   
011500     03  FL902-SEGMENT-SAKNAS    PIC X(1)    VALUE 'N'.                   
011600     03  FL905-SEGMENT-SAKNAS    PIC X(1)    VALUE 'N'.                   
011700     03  FL924-SEGMENT-SAKNAS    PIC X(1)    VALUE 'N'.                   
011800*SWITCHAR                                                                 
011900                                                                          
012000 77  ALLT-SW                     PIC X(1)    VALUE 'J'.                   
012100     88 ALLT-OK                              VALUE 'J'.                   
012200                                                                          
012300 01  DATUM.                                                               
012400     03  WS-TIAAVVD-GRP.                                                  
012500         05  WS-TIAAVV-GRP.                                               
012600            07 WS-TIAA-VECKA         PIC 9(2).                            
012700            07 WS-TIIVV              PIC 9(2).                            
012800         05  WS-TIAAVV               REDEFINES WS-TIAAVV-GRP              
012900                                     PIC 9(4).                            
013000         05  WS-TID                  PIC 9.                               
013100     03 WS-TIAAVVD-GRP-R             REDEFINES WS-TIAAVVD-GRP             
013200                                     PIC 9(5).                            
013300                                                                          
013400                                                                          
013500 01  -COPY W200W001                                                       
013600                                                                          
013700******************************************************************        
013800*      TABELLER                                                           
013900******************************************************************        
014000                                                                          
014100                                                                          
014200 01  TABENTRY-PARM.                                                       
014300                                                                          
014400     03  STEGLANGD                 PIC S9(9) COMP.                        
014500     03  POST-ANTAL                PIC S9(9) COMP.                        
014600     03  NYCKELLANGD               PIC S9(9) COMP.                        
014700                                                                          
014800*01  TAB-MAX                     PIC S9(9) COMP  VALUE 1200.              
014900                                                                          
015000 01  LEVERANSBESKEDTABELL.                                                
015100     03  LEVERANSBESKED OCCURS 30.                                        
015200        05  LEV-TAB-DISP              PIC  S9(7)    COMP-3.               
015300        05  LEV-TAB-ANTAL             PIC  S9(7)    COMP-3.               
015400                                                                          
015500 01  AVROP-TEST-TABELL.                                                   
015600     03  TEST-AVROP OCCURS 3.                                             
015700        05 AVROP-TESTTAB-DISP     PIC  S9(7)    COMP-3.                   
015800        05 AVROP-TESTTAB-ANTAL    PIC  S9(7)    COMP-3.                   
015900        05 AVROP-TESTTAB-DAT-TIPP PIC  S9(3)    COMP-3.                   
016000                                                                          
016100 01  AVROP-TABELL.                                                        
016200     03  AVROP OCCURS 15.                                                 
016300        05 AVROP-TAB-DISP         PIC  S9(7)    COMP-3.                   
016400        05 AVROP-TAB-ANTAL        PIC  S9(7)    COMP-3.                   
016500        05 AVROP-TAB-DAT-TIPP     PIC S9(3)     COMP-3.                   
016600                                                                          
016700******************************************************************        
016800*       ARBETSFÄLT                                                        
016900******************************************************************        
017000                                                                          
017100 01  FILLER             PIC X(16) VALUE 'WDATAREA  '.                     
017200*01  RDARAREA    -COPY WDATAREA.                                          
017300     SKIP2                                                                
017400*                                                                         
017500 01   ARBETSFALT.                                                         
017600                                                                          
017700     03  DAGENS-DATUM.                                                    
017800       05 WS-TIAA-DAT               PIC 9(2) VALUE ZERO.                  
017900       05 WS-TIMM-DAT               PIC 9(2) VALUE ZERO.                  
018000       05 WS-TIVV-DAT               PIC 9(2) VALUE ZERO.                  
018100                                                                          
018200                                                                          
018300     03 DAGENS-DATUM-NUM            PIC 9(6) VALUE ZERO.                  
018400                                                                          
018500     03 DAGENS-DATUM-PACK         PIC S9(7) VALUE ZERO COMP-3.            
018600                                                                          
018700     03 DAGENS-DATUM-MINUS-5        PIC 9(6) VALUE ZERO.                  
018800                                                                          
018900     03  W009VADD-AREA.                                                   
019000       05 DATUM-AAVV               PIC S9(5) COMP-3.                      
019100       05 ANTAL                    PIC S9(3) COMP-3.                      
019200                                                                          
019300                                                                          
019400******************************************************************        
019500*    DYNAMISKA SUBPROGRAN                                                 
019600******************************************************************        
019700     SKIP2                                                                
019800 01  DYNAMISKA-SUBPROGRAM.                                                
019900     03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.          
020000     03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.          
020100     03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.          
020200     03  W009VADD                  PIC X(8)    VALUE 'W009VADD'.          
020300     03  WINTSOR                   PIC X(8)    VALUE 'WINTSOR '.          
020400     03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.          
020500     03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY'.           
020600                                                                          
020700                                                                          
020800******************************************************************        
020900*          PARAMETRAR TILL POSTSUM                                        
021000******************************************************************        
021100                                                                          
021200 01  FILLER                      PIC X(16)   VALUE 'POSTSUM'.             
021300                                                                          
021400*01  -COPY W0005      -PRE POSTSUM-                                       
021500                                                                          
021600     EJECT                                                                
021700******************************************************************        
021800*          PARAMETRAR TILL WORKDAY                                        
021900******************************************************************        
022000                                                                          
022100 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
022200                                                                          
022300*01  -COPY WORKAREA                                                       
022400                                                                          
022500     EJECT                                                                
022600******************************************************************        
022700*          PARAMETRAR TILL DATUMKORT                                      
022800******************************************************************        
022900                                                                          
023000 01  DATUMKORT-ID                PIC X(6)   VALUE 'WDATUM'.               
023100                                                                          
023200*01  -COPY WDATKORT                                                       
023300                                                                          
023400******************************************************************        
023500*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
023600******************************************************************        
023700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023800     SKIP2                                                                
023900*    ---- STATUSKOD FRÅN IMS                                              
024000                                                                          
024100                                                                          
024200 01  STATUS-WS                   PIC XX.                                  
024300     88  SEGMENT-FINNS                       VALUE '  '.                  
024400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024500     88  IMS-EJ-OK                           VALUE 'XD'.                  
024600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024700     SKIP2                                                                
024800 01  GODK-STATUSKODER.                                                    
024900   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
025000     SKIP2                                                                
025100 01  SSA1                        PIC X(64).                               
025200 01  SSA2                        PIC X(64).                               
025300 01  SSA3                        PIC X(64).                               
025400     SKIP2                                                                
025500******************************************************************        
025600*         NYCKLAR OCH SÖKFÄLT TILL DLI                                    
025700******************************************************************        
025800                                                                          
025900 01  FILLER                      PIC X(16) VALUE 'DLI-NYCKLAR'.           
026000                                                                          
026100 01  NYCKLAR-TILL-DLI.                                                    
026200                                                                          
026300*                                                                         
026400     03  W-IDARTNR-X.                                                     
026500       05  W-IDARTNR          PIC S9(9)  COMP-3.                          
026600*                                                                         
026700     03  W-KDSEGKEY-X.                                                    
026800       05  W-KDSEGKEY         PIC X.                                      
026900                                                                          
027000*                                                                         
027100     03  W-IDDC-X.                                                        
027200       05  W-IDDC             PIC X(2)   VALUE SPACE.                     
027300                                                                          
027400*                                                                         
027500     03  W-WDD901KY-X.                                                    
027600       05  W-IDARTNR-D9       PIC S9(9)  COMP-3.                          
027700       05  W-IDDC-D9          PIC  X(2) VALUE SPACE.                      
027800                                                                          
027900*                                                                         
028000     03  W-IDLEVNR-X.                                                     
028100       05  W-IDLEVNR           PIC  X(5) VALUE SPACE.                     
028200                                                                          
028300*                                                                         
028400     03  W-KDAVROP-X.                                                     
028500       05  W-KDAVROP          PIC S9      VALUE +2  COMP-3.               
028600                                                                          
028700*                                                                         
028800     03  W-TILEVBESK-X.                                                   
028900       05  W-TILEVBSK         PIC S9(7)   COMP-3.                         
029000*                                                                         
029100     03  W-2241KEY-X.                                                     
029200       05  W-IDHTYP           PIC X(4)    VALUE '2241'.                   
029300       05  FILLER             PIC X(26)   VALUE LOW-VALUE.                
029400*                                                                         
029500     03  W-2242KEY-X.                                                     
029600       05  W-2242-IDARTNR     PIC S9(9)  VALUE ZERO  COMP-3.              
029700       05  W-LOW-VALUE        PIC X(4)   VALUE LOW-VALUE.                 
029800     EJECT                                                                
029900                                                                          
030000*01  -COPY W0003                                                          
030100     EJECT                                                                
030200******************************************************************        
030300*          DLI INPUT - OUTPUT AREA                                        
030400******************************************************************        
030500                                                                          
030600 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
030700 01  DLI-IO-ARTC01.                                                       
030800*    03  -COPY WDK601                                                     
030900     EJECT                                                                
031000                                                                          
031100 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
031200 01  DLI-IO-ARTC11.                                                       
031300*    03  -COPY WDK611                                                     
031400     EJECT                                                                
031500                                                                          
031600 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
031700 01  DLI-IO-ARTS01.                                                       
031800*    03  -COPY WDK701                                                     
031900     EJECT                                                                
032000                                                                          
032100 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
032200 01  DLI-IO-ARTS11.                                                       
032300*    03  -COPY WDK711                                                     
032400     EJECT                                                                
032500                                                                          
032600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB01'.                      
032700 01  DLI-IO-INLB01.                                                       
032800*    03  -COPY WDD901                                                     
032900     EJECT                                                                
033000                                                                          
033100 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB11'.                      
033200 01  DLI-IO-INLB11.                                                       
033300*    03  -COPY WDD902  -PRE INLB11-                                       
033400     EJECT                                                                
033500                                                                          
033600 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB23'.                      
033700 01  DLI-IO-INLB23.                                                       
033800*    03  -COPY WDD905  -PRE INLB23-                                       
033900     EJECT                                                                
034000                                                                          
034100 01  FILLER         PIC X(24) VALUE 'DLI-IO-INLB24'.                      
034200 01  DLI-IO-INLB24.                                                       
034300*    03  -COPY WDD924  -PRE INLB24-                                       
034400     EJECT                                                                
034500                                                                          
034600 01  FILLER         PIC X(24) VALUE 'DLI-IO-GX2241'.                      
034700 01  DLI-IO-GX2241.                                                       
034800     03  IO-GX2241      PIC X(30) VALUE SPACE.                            
034900     EJECT                                                                
035000                                                                          
035100 01  FILLER         PIC X(24) VALUE 'DLI-IO-GX2242'.                      
035200 01  DLI-IO-GX2242.                                                       
035300*    03  -COPY WDGX2242                                                   
035400     EJECT                                                                
035500                                                                          
035600******************************************************************        
035700*    LINKAGE SECTION                                                      
035800******************************************************************        
035900 LINKAGE SECTION.                                                         
036000                                                                          
036100*01  -COPY W218DISP                                                       
036200                                                                          
036300*01      -COPY W0008     -PRE ARTC-                                       
036400        05 FILLER     PIC X.                                              
036500     EJECT                                                                
036600                                                                          
036700*01      -COPY W0008     -PRE ARTS-                                       
036800        05 FILLER     PIC X.                                              
036900     EJECT                                                                
037000                                                                          
037100*01      -COPY W0008     -PRE OIGA-                                       
037200        05 FILLER     PIC X.                                              
037300     EJECT                                                                
037400                                                                          
037500*01      -COPY W0008     -PRE INLB-                                       
037600        05 FILLER     PIC X.                                              
037700     EJECT                                                                
037800                                                                          
037900*01      -COPY W0008     -PRE XXCT-                                       
038000        05 FILLER     PIC X.                                              
038100     EJECT                                                                
038200                                                                          
038300 PROCEDURE DIVISION  USING DISP-W218DISP ARTC-PCB ARTS-PCB                
038400                                         OIGA-PCB INLB-PCB                
038500                                         XXCT-PCB.                        
038600 MAIN SECTION.                                                            
038700                                                                          
038800     IF ALLT-OK                                                           
038900        PERFORM A-INIT                                                    
039000        MOVE DISP-IDARTNR TO W-IDARTNR                                    
039100        PERFORM C-SKALL-TIDISPIN-BERAKNAS                                 
039200        IF BERAKNA-TIDISPIN = JA                                          
039300           PERFORM D-BERAKNA-TIDISPIN                                     
039400        END-IF                                                            
039500        IF UPPDATERA-TIDISPIN = JA                                        
039600           PERFORM E-UPPDATERA-CLAG                                       
039700        END-IF                                                            
039800     END-IF                                                               
039900     MOVE ZERO TO RETURN-CODE                                             
040000     GOBACK                                                               
040100     .                                                                    
040200                                                                          
040300 A-INIT SECTION.                                                          
040400     SKIP2                                                                
040500                                                                          
040600     MOVE JA             TO BERAKNA-TIDISPIN                              
040700                            UPPDATERA-TIDISPIN                            
040800                                                                          
040900     MOVE NEJ            TO BEHOV-TACKT                                   
041000                            BAD-DILIVER                                   
041100                            LEVERANSBESKED-FINNS                          
041200                            AVROP-FINNS                                   
041300                                                                          
041400     MOVE ZERO           TO UT-TIDISPIN                                   
041500                                                                          
041600     ACCEPT DAGENS-DATUM FROM DATE                                        
041700     MOVE DAGENS-DATUM      TO DAGENS-DATUM-NUM                           
041800     MOVE DAGENS-DATUM-NUM  TO DAGENS-DATUM-PACK                          
041900                                                                          
042000          MOVE 3                   TO WORK-KDCALL                         
042100          MOVE '11'                TO WORK-IDDC                           
042200          MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                   
042300          MOVE +6                  TO WORK-KVWORKD                        
042400          CALL WORKDAY USING  WORK-KDCALL                                 
042500               WORK-DATE-AREA WORK-KDSVAR                                 
042600          MOVE WORK-TIAAMMDD-FOM   TO DAGENS-DATUM-MINUS-5                
042700     .                                                                    
042800                                                                          
042900     EJECT                                                                
043000                                                                          
043100                                                                          
043200 C-SKALL-TIDISPIN-BERAKNAS SECTION.                                       
043300                                                                          
043400* KONTROLERA OM DISPDATUM SKALL BERÄKNAS ELLER EJ.                        
043500* FINNS SEGMENT PÅ WDK6 OCH KDERS-UTG = 0, LÄSES BASEN                    
043600* OCH TESTER UTFÖRS PÅ OM TIDISPIN SKALL BERÄKNAS                         
043700* GENOM KEDJAN LEVERANSBESKED-AVROP-FRYSTID/1:A INLEV                     
043800* ELLER OM TIDISPIN SKALL FÅ ETT ANNAT VÄRDE DIREKT.                      
043900                                                                          
044000                                                                          
044100     PERFORM IMS-GET-WLARTC01                                             
044200     IF SEGMENT-FINNS                                                     
044300                                                                          
044400        IF ART-KDERS-UTG > +0                                             
044500           MOVE NEJ       TO BERAKNA-TIDISPIN                             
044600           MOVE NEJ       TO UPPDATERA-TIDISPIN                           
044700           MOVE 2         TO DISP-KDSVAR                                  
044800        ELSE                                                              
044900           MOVE ART-TIFINLV           TO DAT-I-TIDATUM                    
045000           MOVE 'AAVVD'               TO DAT-KDDATFORM                    
045100           CALL WDATKONV       USING DAT-KDDATFORM                        
045200                                     DAT-I-TIDATUM                        
045300                                     DAT-O-TIDATUM                        
045400                                     DAT-KDSVAR                           
045500           IF DAT-KDSVAR-OK                                               
045600              MOVE DAT-TIAAMMDD      TO WS-TIFINLV-AAMMDD                 
045700           END-IF                                                         
045800*LÄSA WDK611                                                              
045900           PERFORM IMS-GET-WLARTC11                                       
046000           IF WS-IDPROD < 1 OR > 9                                        
046100              MOVE 3 TO WS-IDPROD                                         
046200           END-IF                                                         
046300           PERFORM S12-SUMMERA-AKS                                        
046400           PERFORM S13-BEHANDLA-KDPRODSL                                  
046500                                                                          
046600                                                                          
046700           IF CLAG-KDERS > +10                                            
046800              MOVE NEJ TO BERAKNA-TIDISPIN                                
046900           ELSE                                                           
047000              IF ART-TIFINLV = +99991                                     
047100                 MOVE ZERO        TO UT-TIDISPIN                          
047200                 MOVE NEJ         TO BERAKNA-TIDISPIN                     
047300              ELSE                                                        
047400                 MOVE WS-TIFINLV-AAMMDD   TO TMP1-YYMMDD                  
047500                 MOVE DAGENS-DATUM-PACK   TO TMP2-YYMMDD                  
047600                 PERFORM WY2000P1                                         
047700                 IF TMP1-YYMMDD >= TMP2-YYMMDD                            
047800                    IF CLAG-PRARTSTD = 0                                  
047900                       MOVE WS-TIFINLV-AAMMDD TO UT-TIDISPIN              
048000                       MOVE NEJ         TO BERAKNA-TIDISPIN               
048100                    ELSE                                                  
048200                       IF CLAG-KDGK = 0 OR CLAG-KDLTK = 0                 
048300                          MOVE WS-TIFINLV-AAMMDD TO                       
048400                                          UT-TIDISPIN                     
048500                          MOVE NEJ TO BERAKNA-TIDISPIN                    
048600                       ELSE                                               
048700                          CONTINUE                                        
048800                       END-IF                                             
048900                    END-IF                                                
049000                 ELSE                                                     
049100                    IF CLAG-PRARTSTD = 0                                  
049200                       IF CLAG-KDGK = 0 OR CLAG-KDLTK = 0                 
049300                          MOVE DAGENS-DATUM-NUM TO UT-TIDISPIN            
049400                          MOVE NEJ TO BERAKNA-TIDISPIN                    
049500                       ELSE                                               
049600                          PERFORM S06-SKAPA-TIDISPIN-M-FT                 
049700                          MOVE NEJ TO BERAKNA-TIDISPIN                    
049800                       END-IF                                             
049900                    ELSE                                                  
050000                       IF CLAG-KDGK = 0 OR CLAG-KDLTK = 0                 
050100                          MOVE ZERO TO UT-TIDISPIN                        
050200                          MOVE NEJ TO BERAKNA-TIDISPIN                    
050300                       END-IF                                             
050400                    END-IF                                                
050500                 END-IF                                                   
050600              END-IF                                                      
050700           END-IF                                                         
050800           IF WS-KVAKS-TOT > 0 AND                                        
050900              WS-KVAKS-TOT > WS-VECKOBEHOV                                
051000              MOVE NEJ TO BERAKNA-TIDISPIN                                
051100              MOVE NEJ TO UPPDATERA-TIDISPIN                              
051200              IF DISP-IDPGM = 'W2180200'                                  
051300                 PERFORM S11-EV-TRANS-TILL-2242                           
051400              END-IF                                                      
051500           END-IF                                                         
051600        END-IF                                                            
051700     ELSE                                                                 
051800        MOVE NEJ       TO BERAKNA-TIDISPIN                                
051900        MOVE NEJ       TO UPPDATERA-TIDISPIN                              
052000        MOVE 1         TO DISP-KDSVAR                                     
052100     END-IF                                                               
052200                                                                          
052300     .                                                                    
052400     EJECT                                                                
052500                                                                          
052600                                                                          
052700                                                                          
052800 D-BERAKNA-TIDISPIN SECTION.                                              
052900                                                                          
053000* "HUVUDSLINGA" FÖR BERÄKNING AV DISPONIBELDATUM .                        
053100                                                                          
053200     PERFORM S05-DAGENS-DAT-PLUS-16V                                      
053300                                                                          
053400     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
053500     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
053600     PERFORM IMS-GU-WLINLB01                                              
053700     IF SEGMENT-FINNS                                                     
053800        PERFORM DA-KONTROLERA-LEVERANSBESKED                              
053900        IF LEVERANSBESKED-FINNS = JA                                      
054000           PERFORM DB-BERAKNA-LEVERANSBESKED                              
054100           IF BEHOV-TACKT = JA                                            
054200              PERFORM S01-SKAPA-TIDISPIN-MED-LEVBESK                      
054300           ELSE                                                           
054400              PERFORM DC-KONTR-AVROP-E-LEVBESK                            
054500              IF AVROP-FINNS = JA                                         
054600                 PERFORM DE-BERAKNA-AVROP                                 
054700                 IF BEHOV-TACKT = JA                                      
054800                    PERFORM S02-SKAPA-TIDISPIN-MED-AVROP                  
054900                 ELSE                                                     
055000                    PERFORM S03-SKAPA-TIDISPIN-M-FT-1INLEV                
055100                 END-IF                                                   
055200              ELSE                                                        
055300                 PERFORM S03-SKAPA-TIDISPIN-M-FT-1INLEV                   
055400              END-IF                                                      
055500           END-IF                                                         
055600        ELSE                                                              
055700           PERFORM DD-KONTROLERA-AVROP                                    
055800           IF AVROP-FINNS = JA                                            
055900              PERFORM DE-BERAKNA-AVROP                                    
056000              IF BEHOV-TACKT = JA                                         
056100                 PERFORM S02-SKAPA-TIDISPIN-MED-AVROP                     
056200              ELSE                                                        
056300                 PERFORM S03-SKAPA-TIDISPIN-M-FT-1INLEV                   
056400              END-IF                                                      
056500           ELSE                                                           
056600              PERFORM S03-SKAPA-TIDISPIN-M-FT-1INLEV                      
056700           END-IF                                                         
056800        END-IF                                                            
056900     ELSE                                                                 
057000        PERFORM S03-SKAPA-TIDISPIN-M-FT-1INLEV                            
057100     END-IF                                                               
057200     .                                                                    
057300   EJECT                                                                  
057400                                                                          
057500 DA-KONTROLERA-LEVERANSBESKED SECTION.                                    
057600                                                                          
057700     SKIP2                                                                
057800*    LÄS DE 30 FÖRSTA LEVERANSBESKEDEN OAVSETT LEVERANTÖR I               
057900*    WDD9. LÄGGER ANKOMSTDATUM OCH ANTAL I TABELL FÖR                     
058000*    SORTERING I ANKOMSTORDNING:                                          
058100                                                                          
058200     MOVE NEJ     TO LEVERANSBESKED-FINNS                                 
058300     MOVE NEJ     TO DILIVER-LONG-AWAY                                    
058400     MOVE NEJ     TO FL902-SEGMENT-SAKNAS                                 
058500     MOVE NEJ     TO FL924-SEGMENT-SAKNAS                                 
058600     MOVE ZERO    TO WS-LB-DISP-AAMMDD                                    
058700     MOVE ZERO    TO WS-LB-ANTAL                                          
058800     MOVE ZERO    TO LB-MAX                                               
058900                                                                          
059000* ***TÖMMA TABELL                                                         
059100                                                                          
059200     MOVE 1 TO LB-IX                                                      
059300     PERFORM UNTIL LB-IX > 30                                             
059400        MOVE ZERO TO LEV-TAB-DISP(LB-IX)                                  
059500        MOVE ZERO TO LEV-TAB-ANTAL(LB-IX)                                 
059600        ADD 1 TO LB-IX                                                    
059700     END-PERFORM                                                          
059800                                                                          
059900     MOVE 1 TO LB-IX                                                      
060000                                                                          
060100     PERFORM IMS-GNP-WLINLB11-F                                           
060200     IF SEGMENT-SAKNAS                                                    
060300        MOVE JA TO FL902-SEGMENT-SAKNAS                                   
060400     END-IF                                                               
060500     PERFORM UNTIL FL902-SEGMENT-SAKNAS = JA                              
060600        MOVE NEJ TO FL924-SEGMENT-SAKNAS                                  
060700        MOVE NEJ     TO DILIVER-LONG-AWAY                                 
060800        MOVE INLB11-IDLEVNR TO W-IDLEVNR                                  
060900        PERFORM IMS-GNP-WLINLB24                                          
061000        IF SEGMENT-SAKNAS                                                 
061100           MOVE JA TO FL924-SEGMENT-SAKNAS                                
061200        END-IF                                                            
061300        PERFORM UNTIL FL924-SEGMENT-SAKNAS = JA OR                        
061400                      LB-IX > 30        OR                                
061500                      DILIVER-LONG-AWAY = JA                              
061600           MOVE INLB24-LEV-TILEVBSK-DISP                                  
061700                               TO WS-LB-DISP-AAMMDD                       
061800           IF INLB24-LEV-KVAVIS-BSKURS    > 0                             
061900              MOVE INLB24-LEV-KVAVIS-BSKURS                               
062000                                  TO WS-LB-ANTAL                          
062100           END-IF                                                         
062200                                                                          
062300                                                                          
062400* LEVERANSBESKED MED DISPDAT BARA FÖR C1                                  
062500                                                                          
062600           MOVE WS-LB-DISP-AAMMDD TO TMP1-YYMMDD                          
062700           MOVE DAGENS-DATUM-PACK TO TMP2-YYMMDD                          
062800           PERFORM WY2000P1                                               
062900           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
063000              MOVE WS-LB-DISP-AAMMDD TO TMP1-YYMMDD                       
063100              MOVE WS-DAGENS-DATUM-16V-PACK  TO TMP2-YYMMDD               
063200              IF TMP1-YYMMDD  < TMP2-YYMMDD                               
063300                 MOVE NEJ TO DILIVER-LONG-AWAY                            
063400                 MOVE WS-LB-DISP-AAMMDD TO                                
063500                            LEV-TAB-DISP(LB-IX)                           
063600                 MOVE WS-LB-ANTAL    TO                                   
063700                            LEV-TAB-ANTAL(LB-IX)                          
063800                 ADD 1               TO LB-IX                             
063900              ELSE                                                        
064000                 MOVE JA TO DILIVER-LONG-AWAY                             
064100              END-IF                                                      
064200           END-IF                                                         
064300                                                                          
064400           PERFORM IMS-GNP-WLINLB24                                       
064500           IF SEGMENT-SAKNAS                                              
064600              MOVE JA  TO FL924-SEGMENT-SAKNAS                            
064700           END-IF                                                         
064800        END-PERFORM                                                       
064900        PERFORM IMS-GNP-WLINLB11-N                                        
065000        IF SEGMENT-SAKNAS                                                 
065100           MOVE JA TO FL902-SEGMENT-SAKNAS                                
065200        END-IF                                                            
065300     END-PERFORM                                                          
065400     IF LB-IX > 1                                                         
065500        MOVE JA               TO LEVERANSBESKED-FINNS                     
065600     END-IF                                                               
065700                                                                          
065800     COMPUTE LB-MAX = LB-IX - 1                                           
065900                                                                          
066000     MOVE 1 TO LB-30                                                      
066100     PERFORM UNTIL LB-30 > 30                                             
066200        ADD 1 TO LB-30                                                    
066300     END-PERFORM                                                          
066400     .                                                                    
066500    EJECT                                                                 
066600                                                                          
066700                                                                          
066800                                                                          
066900   DB-BERAKNA-LEVERANSBESKED SECTION.                                     
067000                                                                          
067100* SORTERA LEVERANSBESKEDTABELLEN MED WINTSORT.                            
067200*                                                                         
067300* LÄS SOM MEST DE TIO FÖRSTA LEVERANSBESKEDEN I TABELLEN                  
067400* KONTROLERA FÖR VART OCH ETT AV DEN OM DET SAMMANLAGGDA ANTALET          
067500* ARTIKLAR MOTSVARAR VECKOBEHOVET, HÄNSYN TAS TILL AKTUELL                
067600* PLANERINGSPERIOD.                                                       
067700* OM BEHOVET TÄCKS AVBRYTS BEHANDLINGEN, OM INTE                          
067800* SPARAS SISTA UPPGIFTEN.                                                 
067900                                                                          
068000************* FIX FÖR ATT KLARA SEKELSKIFTET *************                
068100************* ÅR < 50 BLIR ÅR + 50           *************                
068200************* ÅR > 50 BLIR ÅR - 50           *************                
068300     MOVE 1                     TO Y2K-IX                                 
068400     PERFORM UNTIL Y2K-IX > LB-MAX                                        
068500       MOVE LEV-TAB-DISP (Y2K-IX) TO TMP1-YYMMDD                          
068600       PERFORM WY2000P1                                                   
068700       MOVE TMP1-YYMMDD           TO LEV-TAB-DISP (Y2K-IX)                
068800       ADD 1                      TO Y2K-IX                               
068900     END-PERFORM                                                          
069000                                                                          
069100*****SORTERING                                                            
069200     MOVE +8                         TO STEGLANGD                         
069300     MOVE LB-MAX                     TO POST-ANTAL                        
069400     MOVE +4                         TO NYCKELLANGD                       
069500     CALL WINTSOR USING LEVERANSBESKEDTABELL STEGLANGD                    
069600                        POST-ANTAL                                        
069700          LEV-TAB-DISP(1) NYCKELLANGD                                     
069800                                                                          
069900************* FIX FÖR ATT KLARA SEKELSKIFTET *************                
070000************* ÅTERSTÄLLER DATUMEN            *************                
070100     MOVE 1                     TO Y2K-IX                                 
070200     PERFORM UNTIL Y2K-IX > LB-MAX                                        
070300       MOVE LEV-TAB-DISP (Y2K-IX) TO TMP1-YYMMDD                          
070400       PERFORM WY2000P1                                                   
070500       MOVE TMP1-YYMMDD           TO LEV-TAB-DISP (Y2K-IX)                
070600       ADD 1                      TO Y2K-IX                               
070700     END-PERFORM                                                          
070800                                                                          
070900     MOVE 1 TO LB-30                                                      
071000     PERFORM UNTIL LB-30 > 30                                             
071100        ADD 1 TO LB-30                                                    
071200     END-PERFORM                                                          
071300                                                                          
071400     MOVE ZERO              TO WS-LB-SUM-ANTAL                            
071500     MOVE ZERO              TO WS-AVROP-SUM-ANTAL                         
071600     MOVE NEJ               TO BEHOV-TACKT                                
071700     MOVE NEJ               TO FL-REST-AV-LEVTAB-TOM                      
071800     MOVE ZERO              TO WS-SISTA-DISP                              
071900     PERFORM S08-BERAKNA-PERIODBEHOV                                      
072000     MOVE 1 TO LB-IX                                                      
072100     PERFORM UNTIL LB-IX > 10 OR BEHOV-TACKT = JA  OR                     
072200                            FL-REST-AV-LEVTAB-TOM = JA                    
072300        IF LEV-TAB-DISP(LB-IX)  = +0   AND                                
072400           LEV-TAB-ANTAL(LB-IX) = +0                                      
072500           MOVE JA       TO FL-REST-AV-LEVTAB-TOM                         
072600        ELSE                                                              
072700           MOVE LEV-TAB-DISP(LB-IX)      TO DAT-I-TIDATUM                 
072800           MOVE 'AAMMDD '                TO DAT-KDDATFORM                 
072900           CALL WDATKONV                 USING DAT-KDDATFORM              
073000                                               DAT-I-TIDATUM              
073100                                               DAT-O-TIDATUM              
073200                                               DAT-KDSVAR                 
073300                                                                          
073400           COMPUTE WS-VECKOBEHOV = WS-PB-TOT / 4.33                       
073500                                                                          
073600           IF CLAG-KDVVKL = 5                                             
073700              COMPUTE WS-VECKOBEHOV = WS-VECKOBEHOV * 0.5                 
073800           END-IF                                                         
073900           COMPUTE WS-LB-SUM-ANTAL =                                      
074000                     WS-LB-SUM-ANTAL + LEV-TAB-ANTAL(LB-IX)               
074100           IF WS-LB-SUM-ANTAL < WS-VECKOBEHOV                             
074200              ADD 1 TO LB-IX                                              
074300           ELSE                                                           
074400              MOVE JA TO BEHOV-TACKT                                      
074500              MOVE LEV-TAB-DISP(LB-IX)                                    
074600                              TO WS-UPPNADD-DISPDAT                       
074700           END-IF                                                         
074800        END-IF                                                            
074900     END-PERFORM                                                          
075000     IF BEHOV-TACKT = NEJ                                                 
075100        MOVE LEV-TAB-DISP(LB-MAX)  TO WS-SISTA-DISP                       
075200     END-IF                                                               
075300       .                                                                  
075400      EJECT                                                               
075500                                                                          
075600                                                                          
075700 DC-KONTR-AVROP-E-LEVBESK SECTION.                                        
075800                                                                          
075900                                                                          
076000* ANVÄNDS OM VECKOBEHOV EJ TÄCKS AV LEVERANSBESKED.                       
076100*                                                                         
076200* FÖR ÖVER UPPNÅDD SUMMA OCH SISTA DISPDATUM FRÅN DB-SECTION.             
076300* LÄS TRE AVROP FÖR VARJE LEVERANTÖR SOM HAR STÖRRE                       
076400* DISPONIBELDATUM ÄN DEN PÅ SISTA LEVERANSBESKEDET                        
076500* OCH INTE LIGGER LÄNGRE BORT ÄN 16 VECKOR                                
076600* FRÅN DAGENS DATUM. LÄGG AVROPEN I EN SORTERINGSTABELL.                  
076700*                                                                         
076800     MOVE WS-LB-SUM-ANTAL       TO WS-AVROP-SUM-ANTAL                     
076900     MOVE NEJ     TO BEFORE-LAST-LEVBESK                                  
077000     MOVE NEJ     TO DILIVER-LONG-AWAY                                    
077100     MOVE NEJ     TO FL902-SEGMENT-SAKNAS                                 
077200     MOVE NEJ     TO FL905-SEGMENT-SAKNAS                                 
077300     MOVE NEJ     TO AVROP-FINNS                                          
077400                                                                          
077500     MOVE 1 TO AVROP-TEST-IX                                              
077600     PERFORM UNTIL AVROP-TEST-IX > 3                                      
077700        MOVE ZERO TO AVROP-TESTTAB-DISP(AVROP-TEST-IX)                    
077800        MOVE ZERO TO AVROP-TESTTAB-ANTAL(AVROP-TEST-IX)                   
077900        MOVE ZERO TO AVROP-TESTTAB-DAT-TIPP(AVROP-TEST-IX)                
078000        ADD 1 TO AVROP-TEST-IX                                            
078100     END-PERFORM                                                          
078200                                                                          
078300     MOVE 1 TO AVROP-IX                                                   
078400     PERFORM UNTIL AVROP-IX > 15                                          
078500       MOVE ZERO TO AVROP-TAB-DISP(AVROP-IX)                              
078600       MOVE ZERO TO AVROP-TAB-ANTAL(AVROP-IX)                             
078700       MOVE ZERO TO AVROP-TAB-DAT-TIPP(AVROP-IX)                          
078800       ADD 1 TO AVROP-IX                                                  
078900     END-PERFORM                                                          
079000                                                                          
079100     MOVE 1 TO AVROP-TEST-IX                                              
079200     MOVE 1 TO AVROP-IX                                                   
079300*    LÄSA WDD9                                                            
079400                                                                          
079500     PERFORM IMS-GNP-WLINLB11-F                                           
079600     IF SEGMENT-SAKNAS                                                    
079700        MOVE JA TO FL902-SEGMENT-SAKNAS                                   
079800     END-IF                                                               
079900     PERFORM UNTIL FL902-SEGMENT-SAKNAS = JA                              
080000        MOVE INLB11-IDLEVNR TO W-IDLEVNR                                  
080100        MOVE 1 TO AVROP-TEST-IX                                           
080200        MOVE NEJ TO FL905-SEGMENT-SAKNAS                                  
080300        PERFORM IMS-GNP-WLINLB23                                          
080400        IF SEGMENT-SAKNAS                                                 
080500           MOVE JA TO FL905-SEGMENT-SAKNAS                                
080600         END-IF                                                           
080700        PERFORM UNTIL FL905-SEGMENT-SAKNAS = JA OR                        
080800                      AVROP-TEST-IX > 3                                   
080900                                                                          
081000           MOVE INLB23-TIAVRDAT-DISP TO DAT-I-TIDATUM                     
081100           MOVE 'AAMMDD'             TO DAT-KDDATFORM                     
081200           CALL WDATKONV             USING DAT-KDDATFORM                  
081300                                     DAT-I-TIDATUM                        
081400                                     DAT-O-TIDATUM                        
081500                                     DAT-KDSVAR                           
081600           IF DAT-KDSVAR-OK                                               
081700              MOVE DAT-TIAAMMDD      TO WS-AVROP-DISP-AAMMDD              
081800              MOVE DAT-TIRP          TO WS-AVROP-DAT-TIPP                 
081900           END-IF                                                         
082000                                                                          
082100           MOVE WS-AVROP-DISP-AAMMDD   TO TMP1-YYMMDD                     
082200           MOVE WS-SISTA-DISP          TO TMP2-YYMMDD                     
082300           PERFORM WY2000P1                                               
082400           IF TMP1-YYMMDD <= TMP2-YYMMDD                                  
082500              MOVE JA TO BEFORE-LAST-LEVBESK                              
082600           END-IF                                                         
082700           MOVE WS-AVROP-DISP-AAMMDD     TO TMP1-YYMMDD                   
082800           MOVE WS-DAGENS-DATUM-16V-PACK TO TMP2-YYMMDD                   
082900           PERFORM WY2000P1                                               
083000           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
083100              MOVE JA TO DILIVER-LONG-AWAY                                
083200           END-IF                                                         
083300           IF BEFORE-LAST-LEVBESK = NEJ                                   
083400              IF DILIVER-LONG-AWAY = NEJ                                  
083500                 MOVE JA            TO AVROP-FINNS                        
083600                 MOVE WS-AVROP-DISP-AAMMDD                                
083700                  TO AVROP-TESTTAB-DISP(AVROP-TEST-IX)                    
083800                 MOVE WS-AVROP-DAT-TIPP                                   
083900                    TO AVROP-TESTTAB-DAT-TIPP(AVROP-TEST-IX)              
084000                 MOVE INLB23-KVAVROP                                      
084100                     TO AVROP-TESTTAB-ANTAL(AVROP-TEST-IX)                
084200                 MOVE NEJ TO DILIVER-LONG-AWAY                            
084300                 ADD 1  TO AVROP-TEST-IX                                  
084400              END-IF                                                      
084500           END-IF                                                         
084600           PERFORM IMS-GNP-WLINLB23                                       
084700           IF SEGMENT-SAKNAS                                              
084800              MOVE JA TO FL905-SEGMENT-SAKNAS                             
084900           END-IF                                                         
085000           MOVE NEJ TO DILIVER-LONG-AWAY                                  
085100           MOVE NEJ TO BEFORE-LAST-LEVBESK                                
085200        END-PERFORM                                                       
085300        MOVE NEJ TO FL905-SEGMENT-SAKNAS                                  
085400        COMPUTE AVROP-TEST-IX-MAX = AVROP-TEST-IX - 1                     
085500                                                                          
085600        MOVE 1 TO AVROP-TEST-IX                                           
085700        PERFORM UNTIL AVROP-TEST-IX > AVROP-TEST-IX-MAX                   
085800           MOVE AVROP-TESTTAB-DISP(AVROP-TEST-IX)                         
085900                        TO AVROP-TAB-DISP(AVROP-IX)                       
086000           MOVE AVROP-TESTTAB-ANTAL(AVROP-TEST-IX)                        
086100                        TO AVROP-TAB-ANTAL(AVROP-IX)                      
086200           MOVE AVROP-TESTTAB-DAT-TIPP(AVROP-TEST-IX)                     
086300                        TO AVROP-TAB-DAT-TIPP(AVROP-IX)                   
086400           ADD 1 TO AVROP-TEST-IX                                         
086500           ADD 1 TO AVROP-IX                                              
086600        END-PERFORM                                                       
086700        PERFORM IMS-GNP-WLINLB11-N                                        
086800        IF SEGMENT-SAKNAS                                                 
086900           MOVE JA TO FL902-SEGMENT-SAKNAS                                
087000        END-IF                                                            
087100     END-PERFORM                                                          
087200                                                                          
087300     COMPUTE AVROP-MAX = AVROP-IX - 1                                     
087400                                                                          
087500     MOVE 1 TO AVROP-15                                                   
087600     PERFORM UNTIL AVROP-15 > 15                                          
087700        ADD 1 TO AVROP-15                                                 
087800     END-PERFORM                                                          
087900     .                                                                    
088000    EJECT                                                                 
088100                                                                          
088200                                                                          
088300 DD-KONTROLERA-AVROP SECTION.                                             
088400                                                                          
088500* ANVÄNDS OM GODKÄNDA LEVERANSBESKED EJ FANNS.                            
088600*                                                                         
088700* LÄS TRE AVROP FRÅN ALLA LEVERANTÖRER. AVROPEN FÅR INTE LIGGA            
088800* LÄNGRE FRAM ÄN 16 VECKOR. HAR EN LEVERANTÖR AVROP BLAND DE TRE,         
088900* SOM ÄR PASSERADE, LÄSES NÄSTA LEVERANTÖR. AVROPEN LÄGGS I               
089000* EN TABELL FÖR SORTERING.                                                
089100                                                                          
089200    SKIP2                                                                 
089300                                                                          
089400     MOVE ZERO    TO WS-AVROP-SUM-ANTAL                                   
089500     MOVE NEJ     TO AVROP-FINNS                                          
089600     MOVE NEJ     TO BAD-DILIVER                                          
089700     MOVE NEJ     TO DILIVER-LONG-AWAY                                    
089800     MOVE NEJ     TO FL902-SEGMENT-SAKNAS                                 
089900     MOVE NEJ     TO FL905-SEGMENT-SAKNAS                                 
090000                                                                          
090100     MOVE 1 TO AVROP-TEST-IX                                              
090200     PERFORM UNTIL AVROP-TEST-IX > 3                                      
090300        MOVE ZERO TO AVROP-TESTTAB-DISP(AVROP-TEST-IX)                    
090400        MOVE ZERO TO AVROP-TESTTAB-ANTAL(AVROP-TEST-IX)                   
090500        MOVE ZERO TO AVROP-TESTTAB-DAT-TIPP(AVROP-TEST-IX)                
090600        ADD 1 TO AVROP-TEST-IX                                            
090700     END-PERFORM                                                          
090800                                                                          
090900     MOVE 1 TO AVROP-IX                                                   
091000     PERFORM UNTIL AVROP-IX > 15                                          
091100       MOVE ZERO TO AVROP-TAB-DISP(AVROP-IX)                              
091200       MOVE ZERO TO AVROP-TAB-ANTAL(AVROP-IX)                             
091300       MOVE ZERO TO AVROP-TAB-DAT-TIPP(AVROP-IX)                          
091400       ADD 1 TO AVROP-IX                                                  
091500     END-PERFORM                                                          
091600                                                                          
091700     MOVE 1 TO AVROP-TEST-IX                                              
091800     MOVE 1 TO AVROP-IX                                                   
091900*    LÄSA WDD9                                                            
092000                                                                          
092100     PERFORM IMS-GNP-WLINLB11-F                                           
092200     IF SEGMENT-SAKNAS                                                    
092300        MOVE JA TO FL902-SEGMENT-SAKNAS                                   
092400     END-IF                                                               
092500     PERFORM UNTIL FL902-SEGMENT-SAKNAS = JA                              
092600        MOVE 1 TO AVROP-TEST-IX                                           
092700        MOVE NEJ     TO BAD-DILIVER                                       
092800        MOVE NEJ TO FL905-SEGMENT-SAKNAS                                  
092900        MOVE INLB11-IDLEVNR TO W-IDLEVNR                                  
093000        PERFORM IMS-GNP-WLINLB23                                          
093100        IF SEGMENT-SAKNAS                                                 
093200           MOVE JA TO FL905-SEGMENT-SAKNAS                                
093300        END-IF                                                            
093400        PERFORM UNTIL FL905-SEGMENT-SAKNAS = JA OR                        
093500                      AVROP-TEST-IX > 3 OR                                
093600                      BAD-DILIVER = JA                                    
093700           MOVE NEJ TO DILIVER-LONG-AWAY                                  
093800           MOVE INLB23-TIAVRDAT-INL  TO WS-AVROP-INL-AAMMDD               
093900                                                                          
094000           MOVE INLB23-TIAVRDAT-DISP TO DAT-I-TIDATUM                     
094100           MOVE 'AAMMDD'             TO DAT-KDDATFORM                     
094200           CALL WDATKONV             USING DAT-KDDATFORM                  
094300                                     DAT-I-TIDATUM                        
094400                                     DAT-O-TIDATUM                        
094500                                     DAT-KDSVAR                           
094600           IF DAT-KDSVAR-OK                                               
094700              MOVE DAT-TIAAMMDD      TO WS-AVROP-DISP-AAMMDD              
094800              MOVE DAT-TIRP          TO WS-AVROP-DAT-TIPP                 
094900           END-IF                                                         
095000              MOVE WS-AVROP-INL-AAMMDD   TO TMP1-YYMMDD                   
095100*******       MOVE DAGENS-DATUM-PACK     TO TMP2-YYMMDD                   
095200              MOVE DAGENS-DATUM-MINUS-5  TO TMP2-YYMMDD                   
095300              PERFORM WY2000P1                                            
095400              IF TMP1-YYMMDD <= TMP2-YYMMDD                               
095500                 MOVE JA TO BAD-DILIVER                                   
095600              END-IF                                                      
095700              MOVE WS-AVROP-DISP-AAMMDD     TO TMP1-YYMMDD                
095800              MOVE WS-DAGENS-DATUM-16V-PACK TO TMP2-YYMMDD                
095900              PERFORM WY2000P1                                            
096000              IF TMP1-YYMMDD > TMP2-YYMMDD                                
096100                 MOVE JA TO DILIVER-LONG-AWAY                             
096200              END-IF                                                      
096300              MOVE WS-AVROP-INL-AAMMDD   TO TMP1-YYMMDD                   
096400              MOVE DAGENS-DATUM-PACK     TO TMP2-YYMMDD                   
096500              PERFORM WY2000P1                                            
096600              IF BAD-DILIVER = NEJ AND                                    
096700                 TMP1-YYMMDD > TMP2-YYMMDD                                
096800                 IF DILIVER-LONG-AWAY = NEJ                               
096900                    MOVE JA            TO AVROP-FINNS                     
097000                    MOVE WS-AVROP-DISP-AAMMDD                             
097100                       TO AVROP-TESTTAB-DISP(AVROP-TEST-IX)               
097200                    MOVE WS-AVROP-DAT-TIPP                                
097300                       TO AVROP-TESTTAB-DAT-TIPP(AVROP-TEST-IX)           
097400                    MOVE INLB23-KVAVROP                                   
097500                       TO AVROP-TESTTAB-ANTAL(AVROP-TEST-IX)              
097600                    MOVE JA TO DILIVER-LONG-AWAY                          
097700                    ADD 1  TO AVROP-TEST-IX                               
097800                 END-IF                                                   
097900              END-IF                                                      
098000           PERFORM IMS-GNP-WLINLB23                                       
098100           IF SEGMENT-SAKNAS                                              
098200              MOVE JA TO FL905-SEGMENT-SAKNAS                             
098300           END-IF                                                         
098400        END-PERFORM                                                       
098500        COMPUTE AVROP-TEST-IX-MAX = AVROP-TEST-IX - 1                     
098600                                                                          
098700        IF BAD-DILIVER = NEJ                                              
098800           MOVE 1 TO AVROP-TEST-IX                                        
098900           PERFORM UNTIL AVROP-TEST-IX > AVROP-TEST-IX-MAX                
099000              MOVE AVROP-TESTTAB-DISP(AVROP-TEST-IX)                      
099100                           TO AVROP-TAB-DISP(AVROP-IX)                    
099200              MOVE AVROP-TESTTAB-ANTAL(AVROP-TEST-IX)                     
099300                           TO AVROP-TAB-ANTAL(AVROP-IX)                   
099400              MOVE AVROP-TESTTAB-DAT-TIPP(AVROP-TEST-IX)                  
099500                           TO AVROP-TAB-DAT-TIPP(AVROP-IX)                
099600              ADD 1 TO AVROP-TEST-IX                                      
099700              ADD 1 TO AVROP-IX                                           
099800           END-PERFORM                                                    
099900        END-IF                                                            
100000        PERFORM IMS-GNP-WLINLB11-N                                        
100100        IF SEGMENT-SAKNAS                                                 
100200           MOVE JA TO FL902-SEGMENT-SAKNAS                                
100300        END-IF                                                            
100400     END-PERFORM                                                          
100500                                                                          
100600     COMPUTE AVROP-MAX = AVROP-IX - 1                                     
100700                                                                          
100800     MOVE 1 TO AVROP-15                                                   
100900     PERFORM UNTIL AVROP-15 > 15                                          
101000        ADD 1 TO AVROP-15                                                 
101100     END-PERFORM                                                          
101200     .                                                                    
101300    EJECT                                                                 
101400                                                                          
101500   DE-BERAKNA-AVROP SECTION.                                              
101600                                                                          
101700* ANVÄNDS BÅDE EFTER DC- SECTION OCH EFTER DD-SECTION.                    
101800*                                                                         
101900* LÄS DE 10 FÖRSTA AVROPEN UR DEN SORTERADE TABELLEN.                     
102000* KONTROLERA FÖR VART OCH ETT AV DEM OM VECKOBEHOVET TÄCKS                
102100* ELLER EJ. HÄNSYN TAS TILL AKTUELL PLANNERINGSPERIOD.                    
102200* NÄR BEHOVET ÄR TÄCKT AVBRYTS BEHANDLINGEN.                              
102300                                                                          
102400************* FIX FÖR ATT KLARA SEKELSKIFTET *************                
102500************* ÅR < 50 BLIR ÅR + 50           *************                
102600************* ÅR > 50 BLIR ÅR - 50           *************                
102700     MOVE 1                     TO Y2K-IX                                 
102800     PERFORM UNTIL Y2K-IX > AVROP-MAX                                     
102900       MOVE AVROP-TAB-DISP (Y2K-IX) TO TMP1-YYMMDD                        
103000       PERFORM WY2000P1                                                   
103100       MOVE TMP1-YYMMDD           TO AVROP-TAB-DISP (Y2K-IX)              
103200       ADD 1                      TO Y2K-IX                               
103300     END-PERFORM                                                          
103400                                                                          
103500     MOVE +10                        TO STEGLANGD                         
103600     MOVE AVROP-MAX                  TO POST-ANTAL                        
103700     MOVE +4                         TO NYCKELLANGD                       
103800     CALL WINTSOR USING AVROP-TABELL STEGLANGD POST-ANTAL                 
103900          AVROP-TAB-DISP(1) NYCKELLANGD                                   
104000                                                                          
104100************* FIX FÖR ATT KLARA SEKELSKIFTET *************                
104200************* ÅTERSTÄLLER DATUMEN            *************                
104300     MOVE 1                     TO Y2K-IX                                 
104400     PERFORM UNTIL Y2K-IX > AVROP-MAX                                     
104500       MOVE AVROP-TAB-DISP (Y2K-IX) TO TMP1-YYMMDD                        
104600       PERFORM WY2000P1                                                   
104700       MOVE TMP1-YYMMDD           TO AVROP-TAB-DISP (Y2K-IX)              
104800       ADD 1                      TO Y2K-IX                               
104900     END-PERFORM                                                          
105000                                                                          
105100     MOVE 1 TO AVROP-15                                                   
105200     PERFORM UNTIL AVROP-15 > 15                                          
105300        ADD 1 TO AVROP-15                                                 
105400     END-PERFORM                                                          
105500     MOVE ZERO              TO WS-PB-TOT                                  
105600     MOVE ZERO              TO WS-VECKOBEHOV                              
105700     MOVE NEJ               TO BEHOV-TACKT                                
105800     PERFORM S08-BERAKNA-PERIODBEHOV                                      
105900     MOVE 1 TO AVROP-IX                                                   
106000     PERFORM UNTIL AVROP-IX > 10 OR BEHOV-TACKT = JA                      
106100        IF AVROP-TAB-DISP(AVROP-IX) = +0   AND                            
106200          AVROP-TAB-ANTAL(AVROP-IX) = +0                                  
106300           ADD 1 TO AVROP-IX                                              
106400        ELSE                                                              
106500           PERFORM S09-BERAKNA-VECKOBEHOV-1                               
106600           COMPUTE WS-AVROP-SUM-ANTAL =                                   
106700               WS-AVROP-SUM-ANTAL + AVROP-TAB-ANTAL(AVROP-IX)             
106800           IF WS-AVROP-SUM-ANTAL < WS-VECKOBEHOV                          
106900              ADD 1                  TO AVROP-IX                          
107000           ELSE                                                           
107100              MOVE JA TO BEHOV-TACKT                                      
107200              MOVE AVROP-TAB-DISP(AVROP-IX)                               
107300                    TO WS-UPPNADD-DISPDAT                                 
107400           END-IF                                                         
107500        END-IF                                                            
107600     END-PERFORM                                                          
107700     .                                                                    
107800      EJECT                                                               
107900                                                                          
108000 E-UPPDATERA-CLAG SECTION.                                                
108100                                                                          
108200* UPPDATERAR WDK611 MED TIDISPIN SOM BERÄKNATS                            
108300* D-SECTION ELLER TAGITS FRAM I C-SECTION.                                
108400                                                                          
108500                                                                          
108600     IF CLAG-KDERS > +10                                                  
108700        MOVE ZERO               TO CLAG-TIDISPIN                          
108800     ELSE                                                                 
108900        MOVE UT-TIDISPIN        TO CLAG-TIDISPIN                          
109000     END-IF                                                               
109100     PERFORM IMS-REPL-WLARTC11                                            
109200     MOVE 3 TO DISP-KDSVAR                                                
109300                                                                          
109400     .                                                                    
109500     EJECT                                                                
109600                                                                          
109700                                                                          
109800 S01-SKAPA-TIDISPIN-MED-LEVBESK SECTION.                                  
109900                                                                          
110000* ANVÄNDS FÖR ATT BERÄKNA TIDISPIN DÅ AKTUELLT                            
110100* VECKOBEHOV TÄCKTS AV LEVERANSBESKED.                                    
110200                                                                          
110300   SKIP2                                                                  
110400                                                                          
110500*C1                                                                       
110600     MOVE WS-UPPNADD-DISPDAT TO UT-TIDISPIN                               
110700     .                                                                    
110800     EJECT                                                                
110900                                                                          
111000 S02-SKAPA-TIDISPIN-MED-AVROP SECTION.                                    
111100                                                                          
111200* ANVÄNDS FÖR ATT BERÄKNA TIDISPIN DÅ AKTUELLT                            
111300* VECKOBEHOV TÄCKTS AV LEVERANSBESKED OCH AVROP ELLER                     
111400* BARA MED AVROP.                                                         
111500   SKIP2                                                                  
111600                                                                          
111700     IF CLAG-KDGK = 1                                                     
111800*C1                                                                       
111900        MOVE WS-UPPNADD-DISPDAT       TO UT-TIDISPIN                      
112000     ELSE                                                                 
112100        IF CLAG-KDGK = 2                                                  
112200*C1                                                                       
112300           MOVE WS-UPPNADD-DISPDAT TO WS-IN-DATUM                         
112400           MOVE 1               TO WS-ANTAL-PLUSVECKOR                    
112500           PERFORM S04-BERAKNA-PLUSVECKOR                                 
112600           MOVE WS-UT-DATUM     TO UT-TIDISPIN                            
112700        END-IF                                                            
112800     END-IF                                                               
112900     .                                                                    
113000                                                                          
113100     EJECT                                                                
113200                                                                          
113300 S03-SKAPA-TIDISPIN-M-FT-1INLEV SECTION.                                  
113400                                                                          
113500*    "VÄXEL" I KEDJAN LEVERANSBESKED-AVROP-FRYSTID/1:A INLEV.             
113600*    OM BEHOV EJ BLIVIT TÄCKT GENOM LEVERANSBESKED-AVROP                  
113700*    SKALL TIDISPIN SÄTTAS MED HJÄLP AV 1:A INLVEV OM                     
113800*    TIFINLEV >= DAGENS DATUM ANNARS MED HJÄLP AV FRYSTID,                
113900*    I BÅDA FALLEN TAS HÄNSYN TILL VÄRDENA PÅ KDGK OCH KDLTK.             
114000                                                                          
114100     MOVE WS-TIFINLV-AAMMDD   TO TMP1-YYMMDD                              
114200     MOVE DAGENS-DATUM-PACK   TO TMP2-YYMMDD                              
114300     PERFORM WY2000P1                                                     
114400     IF TMP1-YYMMDD >= TMP2-YYMMDD                                        
114500        PERFORM S07-SKAPA-TIDISPIN-M-TIFINLEV                             
114600     ELSE                                                                 
114700        PERFORM S06-SKAPA-TIDISPIN-M-FT                                   
114800     END-IF.                                                              
114900                                                                          
115000     EJECT                                                                
115100                                                                          
115200 S04-BERAKNA-PLUSVECKOR SECTION.                                          
115300                                                                          
115400* ANVÄNDS VID OMRÄKNING FRÅN AAMMDD TILL AAVVD                            
115500   SKIP2                                                                  
115600     MOVE WS-IN-DATUM         TO DAT-I-TIDATUM                            
115700     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
115800     CALL WDATKONV       USING DAT-KDDATFORM                              
115900                               DAT-I-TIDATUM                              
116000                               DAT-O-TIDATUM                              
116100                               DAT-KDSVAR                                 
116200     IF DAT-KDSVAR-OK                                                     
116300        MOVE DAT-TIAAVVD      TO WS-TIAAVVD-GRP-R                         
116400        MOVE WS-TIAAVV         TO DATUM-AAVV                              
116500        MOVE WS-ANTAL-PLUSVECKOR TO ANTAL                                 
116600        CALL W009VADD USING DATUM-AAVV ANTAL                              
116700        MOVE DATUM-AAVV            TO WS-TIAAVV                           
116800        MOVE WS-TIAAVVD-GRP-R      TO DAT-I-TIDATUM                       
116900        MOVE 'AAVVD'               TO DAT-KDDATFORM                       
117000        CALL WDATKONV       USING DAT-KDDATFORM                           
117100                                  DAT-I-TIDATUM                           
117200                                  DAT-O-TIDATUM                           
117300                                  DAT-KDSVAR                              
117400        IF DAT-KDSVAR-OK                                                  
117500           MOVE DAT-TIAAMMDD           TO WS-UT-DATUM                     
117600        END-IF                                                            
117700     END-IF                                                               
117800     .                                                                    
117900                                                                          
118000  S05-DAGENS-DAT-PLUS-16V SECTION.                                        
118100                                                                          
118200* PLUSSAR 16 HELA VECKOR TILL DAGENS DATUM                                
118300                                                                          
118400     MOVE DAGENS-DATUM        TO WS-IN-DATUM                              
118500     MOVE +17                 TO WS-ANTAL-PLUSVECKOR                      
118600     PERFORM S04-BERAKNA-PLUSVECKOR                                       
118700     MOVE WS-UT-DATUM         TO WS-DAGENS-DATUM-16V                      
118800     MOVE WS-DAGENS-DATUM-16V TO WS-DAGENS-DATUM-16V-PACK                 
118900     .                                                                    
119000     EJECT                                                                
119100                                                                          
119200  S06-SKAPA-TIDISPIN-M-FT SECTION.                                        
119300                                                                          
119400* ANVÄNDS FÖR ATT BERÄKNA TIDISPIN DÅ AKTUELLT                            
119500* VECKOBEHOV EJ TÄCKTS AV VARE SIG BARA LEVERANSBESKED, LEVE-             
119600* RANSBESKED OCH AVROP ELLER BARA AVROP.                                  
119700* ANVÄNDS ÄVEN DÅ LEVERANSBESKED OCH ELLER AVROP INTE                     
119800* FINNS.                                                                  
119900                                                                          
120000     IF CLAG-KDGK = 1                                                     
120100*C1                                                                       
120200        MOVE DAGENS-DATUM          TO WS-IN-DATUM                         
120300        MOVE CLAG-KVVECKOR-FT      TO WS-ANTAL-PLUSVECKOR                 
120400        PERFORM S04-BERAKNA-PLUSVECKOR                                    
120500        MOVE WS-UT-DATUM         TO UT-TIDISPIN                           
120600     ELSE                                                                 
120700        IF CLAG-KDGK = 2                                                  
120800*C1                                                                       
120900           MOVE DAGENS-DATUM      TO WS-IN-DATUM                          
121000           MOVE CLAG-KVVECKOR-FT  TO WS-ANTAL-PLUSVECKOR                  
121100           PERFORM S04-BERAKNA-PLUSVECKOR                                 
121200           MOVE WS-UT-DATUM       TO WS-IN-DATUM                          
121300           MOVE +1                TO WS-ANTAL-PLUSVECKOR                  
121400           PERFORM S04-BERAKNA-PLUSVECKOR                                 
121500           MOVE WS-UT-DATUM       TO UT-TIDISPIN                          
121600*C1                                                                       
121700        END-IF                                                            
121800     END-IF                                                               
121900     .                                                                    
122000     EJECT                                                                
122100                                                                          
122200  S07-SKAPA-TIDISPIN-M-TIFINLEV SECTION.                                  
122300                                                                          
122400* ANVÄNDS FÖR ATT BERÄKNA TIDISPIN DÅ AKTUELLT                            
122500* VECKOBEHOV EJ TÄCKTS AV VARE SIG BARA LEVERANSBESKED, LEVE-             
122600* RANSBESKED OCH AVROP ELLER BARA AVROP, OCH *** OCH FÖRSTA               
122700* INLEVERANS ÄR STÖRRE ÄN DAGENS DATUM****.                               
122800                                                                          
122900     IF CLAG-KDGK = 1                                                     
123000*C1                                                                       
123100        MOVE WS-TIFINLV-AAMMDD     TO UT-TIDISPIN                         
123200     ELSE                                                                 
123300        IF CLAG-KDGK = 2                                                  
123400*C1                                                                       
123500           MOVE WS-TIFINLV-AAMMDD TO WS-IN-DATUM                          
123600           MOVE +1                TO WS-ANTAL-PLUSVECKOR                  
123700           PERFORM S04-BERAKNA-PLUSVECKOR                                 
123800           MOVE WS-UT-DATUM       TO UT-TIDISPIN                          
123900        END-IF                                                            
124000     END-IF                                                               
124100     .                                                                    
124200     EJECT                                                                
124300                                                                          
124400 S08-BERAKNA-PERIODBEHOV SECTION.                                         
124500                                                                          
124600     MOVE ZERO              TO WS-PB-TOT                                  
124700                               WS-KVPB-REF-SUM                            
124800                               WS-KVPB-REF-WEEK-SUM                       
124900                               WS-KVPB-REF-PROCPER                        
125000                                                                          
125100     PERFORM IMS-GU-ARTS01                                                
125200     IF SEGMENT-FINNS                                                     
125300        PERFORM IMS-GNP-ARTS11                                            
125400        PERFORM UNTIL SEGMENT-SAKNAS                                      
125500           IF SLAG-KDREFSTA = AKTIV                                       
125600              MOVE SLAG-IDDC TO W-IDDC                                    
125700              IF SLAG-FLREFILL = JA                                       
125800                 COMPUTE WS-KVPB-REF-SUM =                                
125900                         WS-KVPB-REF-SUM + SLAG-KVPB-REF                  
126000              END-IF                                                      
126100           END-IF                                                         
126200           PERFORM IMS-GNP-ARTS11                                         
126300        END-PERFORM                                                       
126400                                                                          
126500        COMPUTE WS-KVPB-REF-WEEK-SUM =                                    
126600                WS-KVPB-REF-SUM / 4.33                                    
126700                                                                          
126800        COMPUTE WS-KVPB-REF-PROCPER ROUNDED =                             
126900                WS-KVPB-REF-SUM                                           
127000     END-IF                                                               
127100                                                                          
127200     COMPUTE WS-PB-TOT =                                                  
127300             CLAG-KVPB-SEP        +                                       
127400             CLAG-KVPB-TPO        +                                       
127500             WS-KVPB-REF-PROCPER                                          
127600     .                                                                    
127700     EJECT                                                                
127800                                                                          
127900                                                                          
128000 S09-BERAKNA-VECKOBEHOV-1 SECTION.                                        
128100                                                                          
128200     MOVE ZERO              TO WS-VECKOBEHOV                              
128300                                                                          
128400     COMPUTE WS-VECKOBEHOV = WS-PB-TOT / 4.33                             
128500                                                                          
128600     IF CLAG-KDVVKL = 5                                                   
128700        COMPUTE WS-VECKOBEHOV = WS-VECKOBEHOV * 0.5                       
128800     END-IF                                                               
128900     .                                                                    
129000     EJECT                                                                
129100                                                                          
129200 S11-EV-TRANS-TILL-2242 SECTION.                                          
129300                                                                          
129400     IF WS-KVAKS-TOT > 0                                                  
129500        PERFORM IMS-GHU-XXCT01                                            
129600                                                                          
129700        IF SEGMENT-SAKNAS                                                 
129800           MOVE W-2241KEY-X     TO DLI-IO-GX2242                          
129900           PERFORM IMS-ISRT-XXCT01                                        
130000           PERFORM IMS-GHU-XXCT01                                         
130100        END-IF                                                            
130200        MOVE SPACE                  TO 2242-WDGX2242                      
130300        MOVE DISP-IDARTNR           TO W-2242-IDARTNR                     
130400                                       2242-IDARTNR                       
130500        PERFORM IMS-ISRT-XXCT11                                           
130600        MOVE '7'                     TO DISP-KDSVAR                       
130700     END-IF                                                               
130800     .                                                                    
130900     EJECT                                                                
131000                                                                          
131100                                                                          
131200 S12-SUMMERA-AKS SECTION.                                                 
131300                                                                          
131400     COMPUTE WS-KVAKS-TOT =                                               
131500                CLAG-KVAKS-CDC  +                                         
131600                CLAG-KVAKS-T    +                                         
131700                CLAG-KVAKS-PAV                                            
131800     .                                                                    
131900     EJECT                                                                
132000                                                                          
132100                                                                          
132200 S13-BEHANDLA-KDPRODSL SECTION.                                           
132300                                                                          
132400     MOVE ART-KDPRODSL  TO WS-KDPRODSL                                    
132500     MOVE WS-KDPRODSL-3 TO WS-IDPROD                                      
132600     .                                                                    
132700                                                                          
132800     EJECT                                                                
132900                                                                          
133000                                                                          
133100******************************************************************        
133200* IMS SEKTIONER                                                           
133300******************************************************************        
133400     SKIP2                                                                
133500 IMS-GET-WLARTC01 SECTION.                                                
133600                                                                          
133700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X  ')'                        
133800            DELIMITED BY SIZE INTO SSA1                                   
133900     MOVE '  GE' TO GODK-STATUSKODER                                      
134000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
134100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
134200     PERFORM IMS-STATUSKONTROLL                                           
134300     .                                                                    
134400     EJECT                                                                
134500                                                                          
134600 IMS-GET-WLARTC11 SECTION.                                                
134700                                                                          
134800     MOVE 'WLARTC11(KDSEGKEY =1)'    TO SSA1                              
134900     MOVE '  ' TO GODK-STATUSKODER                                        
135000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC11 SSA1                  
135100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
135200     PERFORM IMS-STATUSKONTROLL                                           
135300     .                                                                    
135400     EJECT                                                                
135500                                                                          
135600 IMS-GU-ARTS01 SECTION.                                                   
135700                                                                          
135800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X  ')'                        
135900            DELIMITED BY SIZE INTO SSA1                                   
136000     MOVE '  GE' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS01 SSA1                    
136200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     EJECT                                                                
136600                                                                          
136700 IMS-GNP-ARTS11 SECTION.                                                  
136800                                                                          
136900     MOVE 'WLARTS11 '   TO SSA1                                           
137000     MOVE '  GE' TO GODK-STATUSKODER                                      
137100     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-ARTS11 SSA1                   
137200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
137300     PERFORM IMS-STATUSKONTROLL                                           
137400     .                                                                    
137500     EJECT                                                                
137600                                                                          
137700 IMS-GU-WLINLB01 SECTION.                                                 
137800* LÄSA ROTEN PÅ SÖKT ARTIKELNUMMER/DC I WDD9                              
137900                                                                          
138000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X  ')'                       
138100            DELIMITED BY SIZE INTO SSA1                                   
138200     MOVE '  GE' TO GODK-STATUSKODER                                      
138300     CALL CBLTDLI USING GU INLB-PCB DLI-IO-INLB01 SSA1                    
138400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     .                                                                    
138700     EJECT                                                                
138800                                                                          
138900 IMS-GNP-WLINLB11-F SECTION.                                              
139000*    NÄR AVROP SKALL BÖRJA LÄSAS I DD-SECTION                             
139100                                                                          
139200     MOVE   'WLINLB11*F ' TO SSA1                                         
139300     MOVE '  GE' TO GODK-STATUSKODER                                      
139400     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB11 SSA1                   
139500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
139600     PERFORM IMS-STATUSKONTROLL                                           
139700     .                                                                    
139800     EJECT                                                                
139900                                                                          
140000 IMS-GNP-WLINLB11-N SECTION.                                              
140100*    NÄR NY LEVERANTÖR SKALL LÄSAS I DD-SECTION                           
140200                                                                          
140300     STRING 'WLINLB11(IDLEVNR  >' W-IDLEVNR-X ')'                         
140400            DELIMITED BY SIZE INTO SSA1                                   
140500     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB11 SSA1                   
140600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     EJECT                                                                
141000                                                                          
141100  IMS-GNP-WLINLB23 SECTION.                                               
141200* LÄSA NÄSTA AVROP FÖR EN LEVERANTÖR KVAL: KDAVROP =2                     
141300                                                                          
141400     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
141500            DELIMITED BY SIZE INTO SSA1                                   
141600     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
141700            DELIMITED BY SIZE INTO SSA2                                   
141800     MOVE '  GE' TO GODK-STATUSKODER                                      
141900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB23 SSA1 SSA2              
142000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
142100     PERFORM IMS-STATUSKONTROLL                                           
142200     .                                                                    
142300     EJECT                                                                
142400                                                                          
142500                                                                          
142600 IMS-GNP-WLINLB24 SECTION.                                                
142700*    NÄR NYTT LBESK FÖR EN LEVERANTÖR SKALL LÄSAS I DD-SECTION            
142800                                                                          
142900     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
143000            DELIMITED BY SIZE INTO SSA1                                   
143100     MOVE   'WLINLB24 ' TO SSA2                                           
143200     MOVE '  GE' TO GODK-STATUSKODER                                      
143300     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-INLB24 SSA1 SSA2              
143400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
143500     PERFORM IMS-STATUSKONTROLL                                           
143600     .                                                                    
143700     EJECT                                                                
143800 IMS-REPL-WLARTC11 SECTION.                                               
143900                                                                          
144000     MOVE '  ' TO GODK-STATUSKODER                                        
144100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
144200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     EJECT                                                                
144600 IMS-GHU-XXCT01 SECTION.                                                  
144700*                                                                         
144800     STRING 'WLXXCT01(WDG3KEY  =' W-2241KEY-X ')'                         
144900             DELIMITED BY SIZE INTO SSA1                                  
145000     MOVE '  GE' TO GODK-STATUSKODER                                      
145100     CALL CBLTDLI USING GHU XXCT-PCB DLI-IO-GX2242 SSA1                   
145200     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     .                                                                    
145500     EJECT                                                                
145600 IMS-ISRT-XXCT01 SECTION.                                                 
145700*                                                                         
145800     MOVE 'WLXXCT01' TO SSA1                                              
145900     MOVE '  ' TO GODK-STATUSKODER                                        
146000     CALL CBLTDLI USING ISRT XXCT-PCB DLI-IO-GX2241 SSA1                  
146100     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
146200     PERFORM IMS-STATUSKONTROLL                                           
146300     .                                                                    
146400     EJECT                                                                
146500 IMS-ISRT-XXCT11 SECTION.                                                 
146600*                                                                         
146700     MOVE 'WLXXCT11' TO SSA1                                              
146800     MOVE '  ' TO GODK-STATUSKODER                                        
146900     CALL CBLTDLI USING ISRT XXCT-PCB DLI-IO-GX2242 SSA1                  
147000     MOVE XXCT-STATUS-CODE TO STATUS-WS                                   
147100     PERFORM IMS-STATUSKONTROLL                                           
147200     .                                                                    
147300     EJECT                                                                
147400 IMS-STATUSKONTROLL SECTION.                                              
147500     SKIP2                                                                
147600                                                                          
147700     SET STATUS-IX TO 1                                                   
147800     SEARCH GODK-STATUS                                                   
147900       AT END CALL FELLOG                                                 
148000       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
148100     END-SEARCH                                                           
148200     .                                                                    
148300     EJECT                                                                
148400*    -COPY WY2000P1                                                       
148500     EJECT                                                                

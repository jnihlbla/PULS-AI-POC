000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2010600.                                                
000300 AUTHOR.         ANN JORDEBO.                                             
000400 DATE-WRITTEN.   DECEMBER 1989.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700**   FUNKTION.   TP-PROGRAM FÖR ANSKAFFNING (LEVERANSBESKED)              
000800*                                                                         
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T106                                              
001400*                     W2T106U                                             
001500*        MID:         W2I10601                                            
001600*    UTDATA.                                                              
001700*        MOD:         W2O10601                                            
001800*                                                                         
001900*   ÄNDRINGAR:                                                            
002000*    STORT ÄT. NY BILDLAYOUT, NY FUNKTION (RADVIS UPPDATERING)            
002100*              NYA BERÄKNINGAR INST 23/10 92 SK                           
002200*                                                                         
002300*    SDC ÄT.   ETT LEVERANSBESKED KAN ANTINGEN HA BARA CDC ELLER          
002400*              BARA ST SOM MOTTAGNINGSPLATS, INTE BÅDA SOM                
002500*              TIDIGARE.                                                  
002600*              VID MASKINELL GENERERING AV LEVERANSBESKED LÄGGS           
002700*              ANTAL PÅ CDC VID GK = 1 OCH PÅ ST VID GK = 2.              
002800*              VID MANUELL ÄNDRING MOTTAGANDE LAGER, LÄGGS                
002900*              ANTAL PÅ MOTTAGANDE LAGER.  24/11 94 SK                    
003000*                                                                         
003100*   ÄNDRINGAR:                                                            
003200*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
003300*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003400*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003500*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003600*                                                                         
003700*                                                                         
003800*      2014-05-15  E-TRACKER 10232898                                     
003900*                  VID ÄNDRING AV PLAN. INLEV-VECKA PÅ REDAN              
004000*                  REGISTRERAD RAD, SKALL DATUM MAXIMALT KUNNA            
004100*                  SÄTTAS TILL 2 ÅR I FRAMTIDEN.                          
004200*                                                                         
004300*    SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP3                                                                
004600 DATA DIVISION.                                                           
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900*    -COPY WY2000W1                                                       
005000                                                                          
005100*    -COPY WY2000W2                                                       
005200                                                                          
005300***************************************************************           
005400*     W O R K I N G   S T O R A G E   S E C T I O N                       
005500***************************************************************           
005600 77  IDPGM           PIC X(8)    VALUE 'W2010600'.                        
005700 77  JA              PIC X       VALUE 'J'.                               
005800 77  NEJ             PIC X       VALUE 'N'.                               
005900 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
006000                                                                          
006100                                                                          
006200 77  IDARTNR-WS      PIC X(9).                                            
006300 77  IDLEVNR-WS      PIC X(5).                                            
006400 77  IDLEVNR-SHIP-WS PIC X(5).                                            
006500 77  WS-IDLEVNR-8    PIC X(8)    VALUE SPACE.                             
006600 77  WS-IDLEVNR-ANNAN PIC X(5).                                           
006700 77  WS-SHIP-ANNAN    PIC X(5).                                           
006800 77  IDLEVNR-AKT     PIC X(5).                                            
006900 77  IX              PIC S9(2) VALUE ZERO.                                
007000 77  RAPPORT-MAX     PIC S9(2) VALUE +9.                                  
007100 77  TOMRAD-RAK      PIC S9(2) VALUE ZERO.                                
007200 77  TRAFF           PIC X(1)  VALUE SPACE.                               
007300 77  W-IDLEVNR-TEMP  PIC X(5)  VALUE SPACE.                               
007400                                                                          
007500 77  IDTRANS-WS      PIC X(4).                                            
007600     88 EGEN-BILD                VALUE '2106'.                            
007700     88 GODK-MID                 VALUE '4275' '4277'.                     
007800                                                                          
007900 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
008000     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
008100     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
008200                                                                          
008300     SKIP2                                                                
008400***************************************************************           
008500*              A R B E T S F Ä L T                                        
008600***************************************************************           
008700 01  ARBETSFAELT.                                                         
008800   03  FL-ANNAN-LEV              PIC X VALUE 'N'.                         
008900   03  FL-ANNAN-SHIP             PIC X VALUE 'N'.                         
009000   03  KVAVIS-WS           PIC S9(7) OCCURS 9.                            
009100   03  TILEVBSK-INL-C1-AAMMDD-WS PIC S9(6) COMP-3 OCCURS 9.               
009200   03  TILEVBSK-AVS-AAMMDD-WS    PIC S9(6) COMP-3 OCCURS 9.               
009300   03  TILEVBSK-RAD-AAMMDD-WS    PIC S9(6) COMP-3 OCCURS 9.               
009400   03  TILEVBSK-AVS-SPAR         PIC S9(6) VALUE ZERO.                    
009500   03  TIBORT-WS                 PIC S9(6) VALUE ZERO.                    
009600                                                                          
009700   03  HUVUDLEV-WS               PIC X(5) VALUE SPACE.                    
009800                                                                          
009900                                                                          
010000   03  DAGENS-TID        PIC 9(8)    VALUE ZERO.                          
010100   03  FILLER    REDEFINES DAGENS-TID.                                    
010200       05  DAGENS-HHMMSS PIC 9(6).                                        
010300       05  FILLER        PIC 9(2).                                        
010400   03  DAGENS-DATUM      PIC 9(6) VALUE ZERO.                             
010500   03  HJAELP-DATUM      PIC S9(6) VALUE ZERO.                            
010600   03  FILLER    REDEFINES HJAELP-DATUM.                                  
010700       05 HJAELP-AAR     PIC S9(2).                                       
010800       05 HJAELP-MAN     PIC S9(2).                                       
010900       05 HJAELP-DAG     PIC S9(2).                                       
011000   03  WS-DALEVBSK-AVS   PIC 9(8).                                        
011100   03  FILLER  REDEFINES WS-DALEVBSK-AVS.                                 
011200       05  WS-DALEVBSK-SS     PIC 9(2).                                   
011300       05  WS-DALEVBSK-AAMMDD PIC 9(6).                                   
011400                                                                          
011500   03  KVDAGAR-INLEV     PIC S9(3) VALUE ZERO COMP-3.                     
011600   03  KVDAGAR-TTC1      PIC S9(3) VALUE ZERO COMP-3.                     
011700                                                                          
011800   03  TEST-RAD          PIC X(40) VALUE '*******************'.           
011900 01  DISPDAT-TABELL.                                                      
012000   03  DISPONIBELDATUM OCCURS 9.                                          
012100     05 GAMMAL-TILEVBSK-DISP-C1 PIC S9(7) COMP-3.                         
012200***************************************************************           
012300*                S W I T C H A R                                          
012400***************************************************************           
012500 01  SWITCHAR.                                                            
012600   03  NYCKEL-SW                          PIC X   VALUE 'J'.              
012700     88  NYCKEL-OK                                VALUE 'J'.              
012800     88  NYCKEL-FEL                               VALUE 'N'.              
012900     88  NYCKEL-SAKNAS                            VALUE 'S'.              
013000   03  INDATA-SW                          PIC X   VALUE 'J'.              
013100     88  INDATA-OK                                VALUE 'J'.              
013200   03  ENTER-OCH-INGET-INDATA-SW          PIC X   VALUE 'J'.              
013300     88  ENTER-OCH-INGET-INDATA                   VALUE 'J'.              
013400   03  MIN-MAX-SW                         PIC X   VALUE 'J'.              
013500     88  MIN-MAX-OK                               VALUE 'J'.              
013600   03  INLB-ROT-SW                        PIC X   VALUE ' '.              
013700     88  INLB-ROT-FINNS                           VALUE 'J'.              
013800     88  INLB-ROT-SAKNAS                          VALUE 'N'.              
013900   03  INLB-LEV-SW                        PIC X   VALUE ' '.              
014000     88  INLB-LEV-FINNS                           VALUE 'J'.              
014100     88  INLB-LEV-SAKNAS                          VALUE 'N'.              
014200   03  NY-PLAN-SW                         PIC X   VALUE 'N'.              
014300     88  NY-PLAN                                  VALUE 'J'.              
014400   03  GAMLA-PLANER-SW                    PIC X   VALUE 'N'.              
014500     88  GAMLA-PLANER                             VALUE 'J'.              
014600     SKIP2                                                                
014700***************************************************************           
014800*            N Y C K L A R   T I L L   D L I                              
014900***************************************************************           
015000 01  NYCKLAR-TILL-DLI.                                                    
015100    03    W-IDARTNR-X.                                                    
015200       05 W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                 
015300    03    W-IDLEVNR-X.                                                    
015400       05 W-IDLEVNR       PIC X(5)    VALUE SPACE.                        
015500    03    W-IDLEVNR-SHIP-X.                                               
015600       05 W-IDLEVNR-SHIP  PIC X(5)    VALUE SPACE.                        
015700    03    W-DALEVBSK-X.                                                   
015800       05 W-DALEVBSK      PIC  9(8)   VALUE ZERO.                         
015900    03    W-KDCLAGER-X.                                                   
016000       05 W-KDCLAGER      PIC S9(1)   VALUE ZERO  COMP-3.                 
016100    03    W-IDLEVBSK-X.                                                   
016200       05 W-IDLEVBSK      PIC S9(1)   VALUE ZERO  COMP-3.                 
016300    03    W-WDD901KY-X.                                                   
016400       05 W-IDARTNR-INLB  PIC S9(9)   VALUE ZERO  COMP-3.                 
016500       05 W-IDDC-INLB     PIC X(2)    VALUE SPACE.                        
016600                                                                          
016700    03    W-2227KEY-X.                                                    
016800       05 W-IDHTYP        PIC X(4)    VALUE '2227'.                       
016900       05 FILLER          PIC X(26)   VALUE LOW-VALUE.                    
017000     EJECT                                                                
017100*      --- VALID IDDC CODES                                               
017200*                                                                         
017300*01    -COPY WWDCKONS                                                     
017400       EJECT                                                              
017500***************************************************************           
017600*      D Y N A M I S K A   S U B P R O G R A M                            
017700***************************************************************           
017800 01    DYNAMISKA-SUBPROGRAM.                                              
017900       03 CBLTDLI         PIC X(8) VALUE 'CBLTDLI '.                      
018000       03 FELLOG          PIC X(8) VALUE 'FELLOG  '.                      
018100       03 WDATKONV        PIC X(8) VALUE 'WDATKONV'.                      
018200       03 WORKDAY         PIC X(8) VALUE 'WORKDAY '.                      
018300       03 W005INIT        PIC X(8) VALUE 'W005INIT'.                      
018400       03 WMEDKONV        PIC X(8) VALUE 'WMEDKONV'.                      
018410                                                                          
018420*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018430*01 -COPY WMEDAREA                                                        
018440     SKIP3                                                                
018500***************************************************************           
018600*            M E D D E L A N D E N                                        
018700***************************************************************           
018800 01      FELMEDDELANDEN.                                                  
018900   03    FEL-1           PIC X(40)                                        
019000                VALUE 'NYCKLAR FEL                             '.         
019100   03    FEL-2           PIC X(40)                                        
019200                VALUE 'FÖR UPPDATERING TRYCK PF11              '.         
019300   03    FEL-3           PIC X(40)                                        
019400                VALUE 'ARTIKELN FÖRAVISERAD, ÄNDRING EJ MÖJLIG '.         
019500   03    FEL-4           PIC X(40)                                        
019600                VALUE 'ANGE LEVNR                              '.         
019700   03    FEL-5           PIC X(40)                                        
019800                VALUE 'UPPLYSTA FÄLT FEL                       '.         
019900   03    FEL-6           PIC X(40)                                        
020000                VALUE 'GK=3, ANGE INLEV-DAG FÖR BÅDA LAGREN    '.         
020100   03    FEL-7           PIC X(40)                                        
020200                VALUE 'GK=0, ANGE RÄTT GK PÅ 2132-BILDEN FÖRST '.         
020300   03    FEL-8           PIC X(40)                                        
020400                VALUE 'LEVERANTÖREN FINNS EJ I LEV.REGISTRET   '.         
020500   03    FEL-9           PIC X(40)                                        
020600                VALUE 'ARTIKELN FINNS EJ I LEV.PLAN REGISTRET  '.         
020700   03    FEL-10          PIC X(40)                                        
020800                VALUE 'ARTIKELN FINNS EJ I ARTIKELREGISTRET    '.         
020900   03    FEL-11          PIC X(40)                                        
021000                VALUE 'ARTIKELN UTGÅNGEN, UPPDAT EJ TILLÅTEN   '.         
021100   03    FEL-12          PIC X(40)                                        
021200                VALUE 'LEVERANSBESKED FINNS EJ PÅ LEVERANTÖREN '.         
021300   03    FEL-13          PIC X(40)                                        
021400                VALUE 'ANGE CDC ELLER ST                       '.         
021500   03    FEL-14          PIC X(40)                                        
021600                VALUE 'PF11 OCH INGET INDATA                   '.         
021700   03    FEL-15          PIC X(40)                                        
021800                VALUE 'EJ ÄNDRING/BORTTAG OCH NYUPPL SAMTIDIGT '.         
021900   03    FEL-16          PIC X(40)                                        
022000                VALUE 'LEVERANSBESKED FINNS REDAN              '.         
022100   03    FEL-17          PIC X(40)                                        
022200                VALUE 'ANNAN LEVERANTÖR FINNS                  '.         
022300   03    FEL-18          PIC X(40)                                        
022400                VALUE 'ANNAN LEVERANTÖR FINNS, ANGE LEVNR      '.         
022500   03    FEL-19          PIC X(40)                                        
022600                VALUE 'HUVUDLEVERANTÖR = 0                     '.         
022700   03    FEL-20          PIC X(40)                                        
022800                VALUE 'ÄR DATUM  EXT. LEV.BESK.INF.  OK ?      '.         
022900   03    FEL-21          PIC X(40)                                        
023000                VALUE 'ÄNDRAD AVS.VECKA FINNS REDAN            '.         
023100   03    FEL-22          PIC X(40)                                        
023200                VALUE 'OBEHÖRIG ANVÄNDARE '.                              
023300     EJECT                                                                
023400 01  MEDDELANDEN.                                                         
023500   03    MED-1           PIC X(40)                                        
023600                VALUE 'UPPDATERING GJORD                       '.         
023700   03    MED-2           PIC X(40)                                        
023800                VALUE 'FÖR MER INFORMATION, TRYCK ENTER        '.         
023801                                                                          
023802 01  MESSAGE-CODES.                                                       
023830     03 INF-REFILL-PART          PIC X(3)    VALUE '434'.                 
023840     EJECT                                                                
023900*****************************************************************         
024000*        T P - A R E O R                                                  
024100*****************************************************************         
024200 01  FILLER  PIC X(16)   VALUE '    TP-AREAOR   '.                        
024300     SKIP2                                                                
024400*01      MID -COPY W2I10601.                                              
024500     EJECT                                                                
024600*01      -COPY WMSGAREA                                                   
024700     EJECT                                                                
024800*  03    MOD -COPY W2O10601 -RED MSG-AREA.                                
024900     EJECT                                                                
025000*01  -COPY WMFSAREA.                                                      
025100     EJECT                                                                
025200***************************************************************           
025300*       C O P Y T E X T E R    (DYNAMISKA ANROP)                          
025400***************************************************************           
025500 01  FILLER             PIC X(16) VALUE 'WDATAREA     '.                  
025600*01   -COPY WDATAREA.                                                     
025700     EJECT                                                                
025800 01  FILLER             PIC X(16) VALUE 'WMSGINIT     '.                  
025900*                    **** PARAMETRAR TILL W005INIT                        
026000*01   -COPY WMSGINIT.                                                     
026100     EJECT                                                                
026200 01  FILLER             PIC X(16) VALUE 'WORKAREA     '.                  
026300*01   -COPY WORKAREA.                                                     
026400     EJECT                                                                
026500***************************************************************           
026600*    A R B E T S A R E O R   I M S - S E K T I O N E R N A                
026700***************************************************************           
026800 01  IMS-WS.                                                              
026900   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
027000     SKIP3                                                                
027100*****                    **** STATUS-KOD FRÅN IMS                         
027200   03    STATUS-WS       PIC XX.                                          
027300         88  SEGMENT-FINNS       VALUE '  '.                              
027400         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
027500         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
027600     SKIP3                                                                
027700   03    GODK-STATUSKODER.                                                
027800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027900     SKIP3                                                                
028000 01      SSA1            PIC X(64).                                       
028100 01      SSA2            PIC X(64).                                       
028200 01      SSA3            PIC X(64).                                       
028300     SKIP3                                                                
028400***************************************************************           
028500*          I M S   F U N K T I O N S K O D E R                            
028600***************************************************************           
028700*01      -COPY W0003                                                      
028800     EJECT                                                                
028900***************************************************************           
029000*                  I O - A R E O R                                        
029100***************************************************************           
029200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-01'.          
029300     SKIP3                                                                
029400 01  DLI-IO-AREA-01.                                                      
029500     03  IO-AREA-01               PIC X(150) VALUE SPACE.                 
029600     03  WLARTC01 REDEFINES IO-AREA-01.                                   
029700*        05  -COPY WDK601                                                 
029800     SKIP3                                                                
029900                                                                          
030000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-11'.        
030100     SKIP3                                                                
030200 01  DLI-IO-AREA-11.                                                      
030300     03  IO-AREA-11               PIC X(900) VALUE SPACE.                 
030400     03  WLARTC11 REDEFINES IO-AREA-11.                                   
030500*        05  -COPY WDK611                                                 
030600     EJECT                                                                
030700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-23'.        
030800     SKIP3                                                                
030900 01  DLI-IO-AREA-23.                                                      
031000     03  IO-AREA-23               PIC X(800) VALUE SPACE.                 
031100     03  WLARTC23 REDEFINES IO-AREA-23.                                   
031200*        05  -COPY WDK623                                                 
031300     EJECT                                                                
031400 01  FILLER              PIC X(16)   VALUE 'DLI-IO-AREA-2'.               
031500 01      DLI-IO-AREA-2.                                                   
031600     03  IO-AREA-2       PIC X(200)  VALUE SPACE.                         
031700     SKIP2                                                                
031800*    03  WLINLB01 -COPY WDD901 -PRE INLB-     -RED IO-AREA-2.             
031900     EJECT                                                                
032000*    03  WLINLB11 -COPY WDD902 -PRE INLB-     -RED IO-AREA-2.             
032100     EJECT                                                                
032200*    03  WLINLB24 -COPY WDD924 -PRE INLB-     -RED IO-AREA-2.             
032300     EJECT                                                                
032400*    03  WLINLB25 -COPY WDD925 -PRE INLB-     -RED IO-AREA-2.             
032500     EJECT                                                                
032600     03  WLLEVA01 -COPY WDF101 -PRE LEVA-     -RED IO-AREA-2.             
032700     EJECT                                                                
032800*    03  FILLER   -COPY WDD924 -PRE WINLB-                                
032900     EJECT                                                                
033000                                                                          
033100 01  FILLER              PIC X(16) VALUE 'IO-AREA-3 '.                    
033200 01      DLI-IO-AREA-3.                                                   
033300     03  IO-AREA-3         PIC X(20)  VALUE SPACE.                        
033400     SKIP2                                                                
033500*    03  WLXXBW01 INGEN COPYTEXT  TOM ROT                                 
033600     EJECT                                                                
033700*    03  WLXXBW11 -COPY WDGX2228              -RED IO-AREA-3.             
033800     EJECT                                                                
033900 01  FILLER              PIC X(16)   VALUE 'DLI-IO-AREA-A'.               
034000 01      DLI-IO-AREA-A.                                                   
034100     03  IO-AREA-A       PIC X(200)  VALUE SPACE.                         
034200     SKIP2                                                                
034300*    03  WLINLB24 -COPY WDD924 -PRE INLBA-    -RED IO-AREA-A.             
034400     EJECT                                                                
034500                                                                          
034600***************************************************************           
034700*             L I N K A G E   S E C T I O N                               
034800***************************************************************           
034900 LINKAGE SECTION.                                                         
035000     SKIP2                                                                
035100*01  -COPY W0009     -PRE MSG-                                            
035200     EJECT                                                                
035300*01  -COPY W0008     -PRE USEA-                                           
035400         05  FILLER           PIC X.                                      
035500     EJECT                                                                
035600*01  -COPY W0008     -PRE ARTC-                                           
035700         05  FILLER           PIC X.                                      
035800     EJECT                                                                
035900*01  -COPY W0008     -PRE INLB-                                           
036000         05  KFBA-IDARTNR     PIC S9(9) COMP-3.                           
036100         05  KFBA-IDDC        PIC X(2).                                   
036200         05  KFBA-IDLEVNR     PIC X(5).                                   
036300     EJECT                                                                
036400*01  -COPY W0008     -PRE LEVA-                                           
036500         05  FILLER           PIC X.                                      
036600     EJECT                                                                
036700*01  -COPY W0008     -PRE XXBW-                                           
036800         05  FILLER           PIC X.                                      
036900     EJECT                                                                
037000*01  -COPY W0008     -PRE INLBA-                                          
037100         05  FILLER           PIC X.                                      
037200     EJECT                                                                
037300 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
037400                   ARTC-PCB INLB-PCB LEVA-PCB XXBW-PCB INLBA-PCB.         
037500 MAIN SECTION.                                                            
037600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
037700                   ARTC-PCB INLB-PCB LEVA-PCB XXBW-PCB INLBA-PCB.         
037800                                                                          
037900     PERFORM IMS-GET-MSG                                                  
038000     IF SEGMENT-FINNS                                                     
038100        PERFORM A-INIT                                                    
038200*       CALL FELLOG                                                       
038300        PERFORM B-KOLLA-NYCKLAR                                           
038400                                                                          
038500        PERFORM SEC-URITY                                                 
038600        IF PASSED-SECURITY-CHECK                                          
038700          IF NYCKEL-OK                                                    
038800             IF MFS-UPDATE                                                
038900                PERFORM C-INDATA-KONTROLL                                 
039000                IF INDATA-OK                                              
039100                   PERFORM D-KOLLA-MOT-BAS                                
039200                   IF INDATA-OK                                           
039300                      PERFORM E-UPPDATERA-VISA-BILD                       
039400                   ELSE                                                   
039500                      PERFORM F-VISA-EFTER-INDATAFEL                      
039600                   END-IF                                                 
039700                ELSE                                                      
039800                   PERFORM F-VISA-EFTER-INDATAFEL                         
039900                END-IF                                                    
040000                CONTINUE                                                  
040100             ELSE                                                         
040200                PERFORM J-BEHANDLA-ENTER-OCH-INDATA                       
040300                IF ENTER-OCH-INGET-INDATA                                 
040400                   IF MFS-FIRST                                           
040500                      PERFORM G-VISA-FOERSTA-SIDAN                        
040600                   ELSE                                                   
040700                      IF MFS-NEXT                                         
040800                         PERFORM H-VISA-NAESTA-SIDA                       
040900                      ELSE                                                
041000                         IF MFS-ENTER                                     
041100                            PERFORM I-VISA-SAMMA-SIDA                     
041200                            MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL          
041300                         END-IF                                           
041400                      END-IF                                              
041500                   END-IF                                                 
041600                END-IF                                                    
041700             END-IF                                                       
041800          ELSE                                                            
041900             IF NYCKEL-SAKNAS                                             
042000                PERFORM MFS-TOM-SIDA                                      
042100             ELSE                                                         
042200                MOVE FEL-1 TO MOD-TEMFSFEL                                
042300*¤¤¤¤¤¤¤¤¤¤     NYCKLAR FEL                                               
042400                PERFORM MFS-TOM-SIDA                                      
042500             END-IF                                                       
042600          END-IF                                                          
042700          IF NYCKEL-FEL AND NOT EGEN-BILD                                 
042800             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                       
042900                                     MOD-IDLEVNR-UT                       
043000                                     MOD-IDLEVNR-SHIP-UT                  
043100          ELSE                                                            
043200             MOVE IDARTNR-WS     TO MOD-IDARTNR-UT                        
043300             MOVE IDLEVNR-WS     TO MOD-IDLEVNR-UT                        
043400**           MOVE W-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                   
043500             INSPECT MOD-IDARTNR-UT REPLACING                             
043600                                    LEADING ZERO BY SPACE                 
043700          END-IF                                                          
043800        ELSE                                                              
043900*         --- ANVÄNDARE EJ BEHÖRIG PÅ LEVERANTÖR-NIVÅ                     
044000          CONTINUE                                                        
044100        END-IF                                                            
044200                                                                          
044300        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O10601 + 4                     
044400        PERFORM IMS-INSERT-MSG                                            
044500     END-IF                                                               
044600     MOVE ZERO TO RETURN-CODE                                             
044700     GOBACK                                                               
044800     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 A-INIT SECTION.                                                          
045200     SKIP2                                                                
045300     IF MSG-DUBBLA-TRANSKODER                                             
045400        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10601                
045500        MOVE MSG-IDTRANS-2        TO MFS-IDTRANS IDTRANS-WS               
045600        MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                         
045700        MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                          
045800        MOVE MSG-IDPFK            TO MFS-IDPFK                            
045900     ELSE                                                                 
046000        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I10601                
046100        MOVE MSG-IDTRANS-1        TO MFS-IDTRANS IDTRANS-WS               
046200        MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                         
046300        MOVE ' '                  TO MFS-KDTRTYP                          
046400                                     MFS-IDPFK                            
046500     END-IF                                                               
046600                                                                          
046700     MOVE LOW-VALUE               TO MOD-W2O10601                         
046800     MOVE 'W2O10601'              TO MFS-IDMOD                            
046900     MOVE '2106'                  TO MOD-IDTRANS                          
047000                                                                          
047100     ACCEPT DAGENS-DATUM FROM DATE                                        
047200     ACCEPT DAGENS-TID   FROM TIME                                        
047300                                                                          
047400     MOVE '8'                     TO MFS-IDPFK                            
047500                                                                          
047600     IF NOT (EGEN-BILD OR GODK-MID)                                       
047700        MOVE ALL '+' TO MID-IDLEVNR-IN                                    
047800        MOVE SPACE   TO MID-IDLEVNR-UT                                    
047900     END-IF                                                               
048000                                                                          
048100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
048200     MOVE '001'             TO MSGI-KDCALL                                
048300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
048400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
048500     MOVE '2106'            TO MSGI-IDTRANS                               
048600                                                                          
048700     IF MFS-IDTRANS = '2106' OR '4275' OR '4277'                          
048800       IF MID-IDARTNR-IN = ALL '+'                                        
048900***      MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                            
049000         IF MID-IDLEVNR-IN = ALL '+'                                      
049100           CONTINUE                                                       
049200         ELSE                                                             
049300           MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                            
049400         END-IF                                                           
049500       ELSE                                                               
049600         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
049700         IF MID-IDLEVNR-IN = ALL '+'                                      
049800           MOVE SPACE          TO MSGI-IDLEVNR                            
049900           CONTINUE                                                       
050000         ELSE                                                             
050100           MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                            
050200         END-IF                                                           
050300       END-IF                                                             
050400     ELSE                                                                 
050500       MOVE SPACE          TO MSGI-IDLEVNR                                
050600       IF MFS-IDTRANS(1:2) NOT = '42'                                     
050700         IF MID-IDARTNR-IN NUMERIC                                        
050800         AND MID-IDARTNR-IN > ZERO                                        
050900           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
051000         END-IF                                                           
051100         IF MID-IDLEVNR-IN NOT = SPACE                                    
051200           MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                            
051300         END-IF                                                           
051400       END-IF                                                             
051500     END-IF                                                               
051600                                                                          
051700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
051800     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
051900     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
052000     MOVE MSGI-IDLEVNR   TO IDLEVNR-WS                                    
052100                                                                          
052200     IF MID-IDARTNR-IN = ALL '+'                                          
052300     AND MID-IDLEVNR-IN = ALL '+'                                         
052400           CONTINUE                                                       
052500     ELSE                                                                 
052600        MOVE '7' TO MFS-IDPFK                                             
052700        MOVE SPACE TO MFS-KDTRTYP                                         
052800     END-IF                                                               
052900                                                                          
053000     MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-IN                       
053100                                     MOD-IDLEVNR-IN                       
053200                                                                          
053300     IF NOT EGEN-BILD                                                     
053400        MOVE ' '                  TO MFS-KDTRTYP                          
053500        MOVE '7'                  TO MFS-IDPFK                            
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900                                                                          
054000 B-KOLLA-NYCKLAR SECTION.                                                 
054100     MOVE 'B-KOLLA-NYCKLAR '  TO CURRENT-SECTION                          
054200     SKIP2                                                                
054300     IF (IDARTNR-WS NUMERIC AND IDARTNR-WS > 0)                           
054400        MOVE IDARTNR-WS TO W-IDARTNR                                      
054500                           W-IDARTNR-INLB                                 
054600        MOVE WC-CDC-SE  TO W-IDDC-INLB                                    
054700        MOVE IDLEVNR-WS TO W-IDLEVNR                                      
054800        PERFORM IMS-GET-ART                                               
054900****    ¤ GU-WDK601                                                       
055000        IF SEGMENT-FINNS                                                  
055100          IF (NOT EGEN-BILD) OR                                           
055200             (MFS-UPDATE AND MID-IDLEVNR-RAD(9)= ALL '+')                 
055300               MOVE ART-IDLEVNR    TO W-IDLEVNR                           
055400                                      IDLEVNR-WS                          
055500          END-IF                                                          
055600        END-IF                                                            
055700        PERFORM IMS-GET-ART-INLB                                          
055800****    ¤ GU-WDD901 '                                                     
055900        IF SEGMENT-FINNS                                                  
056000           MOVE JA TO INLB-ROT-SW                                         
056100           IF W-IDLEVNR  NOT = SPACE                                      
056200              PERFORM IMS-GET-LEV-INLB                                    
056300****          ¤ GU-WDD901-WDD902                                          
056400              IF SEGMENT-FINNS                                            
056500                 MOVE JA TO INLB-LEV-SW                                   
056600              ELSE                                                        
056700                 PERFORM IMS-GET-LEV-LEVA                                 
056800****             ¤ GU-WDF101                                              
056900                 IF SEGMENT-FINNS                                         
057000                    MOVE NEJ TO INLB-LEV-SW                               
057100                 ELSE                                                     
057200                    MOVE FEL-8 TO MOD-TEMFSINF                            
057300*¤¤¤¤               LEVERANTÖREN FINNS EJ I LEV.REGISTRET                 
057400                    MOVE NEJ   TO NYCKEL-SW                               
057500                 END-IF                                                   
057600              END-IF                                                      
057700           ELSE                                                           
057800              PERFORM IMS-GET-LEV-OKVAL-INLB                              
057900****          ¤ GNP-WDD901-902                                            
058000              IF SEGMENT-FINNS                                            
058100                 PERFORM IMS-GET-LEV-NEXT-INLB                            
058200****             ¤ GNP-WDD902                                             
058300                 IF SEGMENT-FINNS                                         
058400                    PERFORM IMS-GET-ART                                   
058500****                GU-WDK601                                             
058600                    MOVE ART-IDLEVNR TO MOD-IDLEVNR-UT                    
058700                    MOVE ART-IDLEVNR TO W-IDLEVNR                         
058800                                        IDLEVNR-WS                        
058900                    PERFORM IMS-GET-LEV-INLB                              
059000****                ¤ GU-WDD901-902                                       
059100                    IF SEGMENT-FINNS                                      
059200                       MOVE JA TO INLB-LEV-SW                             
059300                    ELSE                                                  
059400                       MOVE 'S'   TO NYCKEL-SW                            
059500                       MOVE FEL-18 TO MOD-TEMFSFEL                        
059600*¤¤¤¤                  ANNAN LEVERANTÖR FINNS, ANGE LEVNR                 
059700                    END-IF                                                
059800                 ELSE                                                     
059900                    MOVE INLB-IDLEVNR TO W-IDLEVNR                        
060000                                         IDLEVNR-WS                       
060100                    MOVE JA TO INLB-LEV-SW                                
060200                    PERFORM IMS-GET-LEV-INLB                              
060300****                ¤ GU-WDD901-902                                       
060400                 END-IF                                                   
060500              ELSE                                                        
060600                 PERFORM IMS-GET-ART                                      
060700***              GU-WDK601                                                
060800                 MOVE ART-IDLEVNR TO MOD-IDLEVNR-UT                       
060900                 MOVE ART-IDLEVNR TO W-IDLEVNR                            
061000                                     IDLEVNR-WS                           
061100                 MOVE FEL-4 TO MOD-TEMFSINF                               
061200*¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤ ANGE LEVNR                                               
061300                 MOVE 'S'   TO NYCKEL-SW                                  
061400              END-IF                                                      
061500           END-IF                                                         
061600        ELSE                                                              
061700           PERFORM IMS-GET-ART                                            
061800****       ¤ GU-WDK601                                                    
061900           IF SEGMENT-FINNS                                               
062000              IF ART-KDERS-UTG > 0                                        
062100                 MOVE FEL-11 TO MOD-TEMFSINF                              
062200*¤¤¤¤            ARTIKELN UTGÅNGEN, UPPDAT EJ TILLÅTEN                    
062300                 MOVE 'S'    TO NYCKEL-SW                                 
062400              ELSE                                                        
062500                 IF NOT EGEN-BILD                                         
062600                    MOVE ART-IDLEVNR    TO W-IDLEVNR                      
062700                                           IDLEVNR-WS                     
062800                 END-IF                                                   
062900                 IF W-IDLEVNR  NOT = SPACE                                
063000                    PERFORM IMS-GET-LEV-LEVA                              
063100****                ¤ GU-WDF101                                           
063200                    IF SEGMENT-FINNS                                      
063300                       MOVE NEJ    TO INLB-ROT-SW                         
063400                                      INLB-LEV-SW                         
063500                    ELSE                                                  
063600                       MOVE FEL-8 TO MOD-TEMFSINF                         
063700*¤¤¤¤                  LEVERANTÖREN FINNS EJ I LEV.REGISTRET              
063800                       MOVE 'S'   TO NYCKEL-SW                            
063900                    END-IF                                                
064000                 ELSE                                                     
064100                    IF W-IDLEVNR = SPACE AND                              
064200                     (ART-IDLEVNR NOT = SPACE)                            
064300                       MOVE ART-IDLEVNR TO W-IDLEVNR                      
064400                                           IDLEVNR-WS                     
064500                       MOVE NEJ         TO INLB-ROT-SW                    
064600                                           INLB-LEV-SW                    
064700                    ELSE                                                  
064800                       MOVE ART-IDLEVNR TO MOD-IDLEVNR-UT                 
064900                                           W-IDLEVNR                      
065000                                           IDLEVNR-WS                     
065100                       MOVE 'S' TO NYCKEL-SW                              
065200                       MOVE FEL-19 TO MOD-TEMFSINF                        
065300                    END-IF                                                
065400                 END-IF                                                   
065500              END-IF                                                      
065600           ELSE                                                           
065700              MOVE FEL-10 TO MOD-TEMFSINF                                 
065800*¤¤¤¤¤¤¤      ARTIKELN FINNS EJ I ARTIKELREGISTRET                        
065900              MOVE 'S'    TO NYCKEL-SW                                    
066000           END-IF                                                         
066100        END-IF                                                            
066200        IF NYCKEL-OK                                                      
066300           MOVE W-IDLEVNR         TO MSGI-IDLEVNR                         
066400           MOVE '001'             TO MSGI-KDCALL                          
066500           MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                          
066600           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
066700           PERFORM BA-GET-LEV-SHIP                                        
066800        END-IF                                                            
066900     ELSE                                                                 
067000        MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                              
067100        MOVE NEJ             TO NYCKEL-SW                                 
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600 BA-GET-LEV-SHIP   SECTION.                                               
067700     MOVE 'BA-GET-LEV-SHIP '  TO CURRENT-SECTION                          
067800                                                                          
067900     PERFORM IMS-GET-ART                                                  
067910     PERFORM IMS-GET-CLAG                                                 
067920     IF SEGMENT-FINNS                                                     
068000        IF ART-IDLEVNR = W-IDLEVNR                                        
068300         MOVE CLAG-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                    
068400       END-IF                                                             
068410       IF CLAG-IDDC-REF NOT = SPACE                                       
068420          MOVE INF-REFILL-PART  TO MED-IDMFSFEL                           
068430          CALL WMEDKONV      USING MED-WMEDAREA                           
068440          MOVE MED-MFSFEL       TO MOD-TEMFSFEL                           
068450       END-IF                                                             
068500     ELSE                                                                 
068600       PERFORM IMS-GET-ART                                                
068700       PERFORM IMS-GNP-ARTC23                                             
068800       MOVE NEJ TO TRAFF                                                  
068900       PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                         
069000         IF W-IDLEVNR = AVT-IDLEVNR-AVT                                   
069100           MOVE AVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP-UT                   
069200           MOVE JA TO TRAFF                                               
069300         END-IF                                                           
069400         PERFORM IMS-GNP-ARTC23                                           
069500       END-PERFORM                                                        
069600       IF TRAFF = NEJ                                                     
069700         MOVE W-IDLEVNR         TO MOD-IDLEVNR-SHIP-UT                    
069800       END-IF                                                             
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200                                                                          
070300 C-INDATA-KONTROLL SECTION.                                               
070400     MOVE 'C-INDATA-KONTROLL '  TO CURRENT-SECTION                        
070500     SKIP2                                                                
070600     PERFORM CA-FINNS-INDATA-KONTROLL                                     
070700     IF INDATA-OK                                                         
070800        PERFORM CB-FORMELL-INDATA-KONTROLL                                
070900        IF INDATA-OK                                                      
071000           PERFORM CC-KONTROLERA-UPPDATERINGSTYP                          
071100           IF INDATA-OK                                                   
071200              PERFORM CD-RELATIONSKONTROLL                                
071300              IF INDATA-OK                                                
071400                 CONTINUE                                                 
071500              ELSE                                                        
071600                 MOVE FEL-5 TO MOD-TEMFSFEL                               
071700*¤¤¤¤¤¤¤¤        UPPLYSTA FÄLT FEL                                        
071800              END-IF                                                      
071900           ELSE                                                           
072000              MOVE FEL-15 TO MOD-TEMFSFEL                                 
072100*¤¤¤¤¤        EJ ÄNDRING/BORTTAG OCH NYUPPL SAMTIDIGT                     
072200           END-IF                                                         
072300        ELSE                                                              
072400           MOVE FEL-5 TO MOD-TEMFSFEL                                     
072500*¤¤¤¤¤¤¤¤  UPPLYSTA FÄLT FEL                                              
072600        END-IF                                                            
072700     ELSE                                                                 
072800        MOVE FEL-14 TO MOD-TEMFSFEL                                       
072900*¤¤¤¤   PF11 OCH INGET INDATA                                             
073000     END-IF                                                               
073100     .                                                                    
073200     EJECT                                                                
073300                                                                          
073400 CA-FINNS-INDATA-KONTROLL SECTION.                                        
073500     MOVE 'CA-FINNS-INDATA-KONTROLL'  TO CURRENT-SECTION                  
073600                                                                          
073700     MOVE +1 TO IX                                                        
073800     MOVE ZERO TO TOMRAD-RAK                                              
073900     PERFORM UNTIL IX > 9                                                 
074000        IF IX < 9                                                         
074100           IF  MID-KDCMD(IX)           = ALL '+'                          
074200           AND MID-TILEVBSK-RAD(IX)    = ALL '+'                          
074300           AND MID-KVAVIS(IX)          = ALL '+'                          
074400           AND MID-TILEVBSK-INL-C1(IX) = ALL '+'                          
074500               ADD +1 TO TOMRAD-RAK                                       
074600           END-IF                                                         
074700        ELSE                                                              
074800           IF  MID-KDCMD(IX)           = 'N'                              
074900           AND MID-TILEVBSK-AVS(IX)    = ALL '+'                          
075000           AND MID-KVAVIS(IX)          = ALL '+'                          
075100           AND MID-TILEVBSK-INL-C1(IX) = ALL '+'                          
075200           AND MID-IDLEVNR-RAD    (IX) = ALL '+'                          
075300               ADD +1 TO TOMRAD-RAK                                       
075400           END-IF                                                         
075500        END-IF                                                            
075600        ADD +1 TO IX                                                      
075700     END-PERFORM                                                          
075800                                                                          
075900     IF TOMRAD-RAK = 9                                                    
076000     AND MID-TELEVBSK-EXT        = ALL '+'                                
076100     AND MID-TELEVBSK-EXT2       = ALL '+'                                
076200     AND MID-TELEVBSK-EXT3       = ALL '+'                                
076300     AND MID-TELEVBSK-EXT4       = ALL '+'                                
076400     AND MID-TIBORT              = ALL '+'                                
076500        MOVE NEJ TO INDATA-SW                                             
076600        MOVE +1 TO IX                                                     
076700        PERFORM UNTIL IX > 9                                              
076800           IF IX < 9                                                      
076900              MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN(IX)                    
077000                                      MOD-KVAVIS-IN(IX)                   
077100                                      MOD-TILEVBSK-INL-C1-IN(IX)          
077200                                      MOD-TILEVBSK-RAD-IN(IX)             
077300           ELSE                                                           
077400              MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-AVS-NY                 
077500                                      MOD-KVAVIS-NY                       
077600                                      MOD-TILEVBSK-INL-C1-NY              
077700                                      MOD-IDLEVNR-NY                      
077800                                      MOD-IDLEVNR-SHIP-NY                 
077900           END-IF                                                         
078000           ADD +1 TO IX                                                   
078100        END-PERFORM                                                       
078200        MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT                          
078300                                MOD-TELEVBSK-EXT2                         
078400                                MOD-TELEVBSK-EXT3                         
078500                                MOD-TELEVBSK-EXT4                         
078600                                MOD-TIBORT                                
078700     END-IF                                                               
078800                                                                          
078900     .                                                                    
079000     EJECT                                                                
079100                                                                          
079200 CB-FORMELL-INDATA-KONTROLL SECTION.                                      
079300     MOVE 'CB-FORMELL-INDATA-KONTROLL'  TO CURRENT-SECTION                
079400     SKIP2                                                                
079500                                                                          
079600*KONTROLLERA INDATA PÅ VISNINGSRADER                                      
079700                                                                          
079800                                                                          
079900     MOVE +1 TO IX                                                        
080000     PERFORM UNTIL IX > 9                                                 
080100        MOVE ZERO TO KVAVIS-WS (IX)                                       
080200                     TILEVBSK-AVS-AAMMDD-WS (IX)                          
080300                     TILEVBSK-RAD-AAMMDD-WS (IX)                          
080400                     TILEVBSK-INL-C1-AAMMDD-WS (IX)                       
080500        ADD +1 TO IX                                                      
080600     END-PERFORM                                                          
080700                                                                          
080800     MOVE +1 TO IX                                                        
080900     PERFORM UNTIL IX > 9                                                 
081000                                                                          
081100**KDCMD                                                                   
081200                                                                          
081300        IF IX < 9                                                         
081400* * *      PÅ RAD 1 - 8 ÄR REPLACE, DELETE OCH OIFYLLT OK                 
081500           IF MID-KDCMD(IX) = ALL '+'                                     
081600              MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN(IX)                    
081700              MOVE MFS-ALFA-FAELT-RAETT TO                                
081800                                  MOD-KDCMD-IN-ATTR(IX)                   
081900           ELSE                                                           
082000              IF MID-KDCMD-DELETE(IX) OR                                  
082100                 MID-KDCMD-REPLACE(IX)                                    
082200                 MOVE MFS-ALFA-FAELT-RAETT TO                             
082300                                     MOD-KDCMD-IN-ATTR(IX)                
082400              ELSE                                                        
082500                 MOVE NEJ TO INDATA-SW                                    
082600                 MOVE MFS-ALFA-FAELT-FEL   TO                             
082700                                     MOD-KDCMD-IN-ATTR(IX)                
082800              END-IF                                                      
082900              MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN(IX)                  
083000           END-IF                                                         
083100        ELSE                                                              
083200           CONTINUE                                                       
083300* * *      PÅ RAD 9  ÄR KDCMD ALLTID = N                                  
083400        END-IF                                                            
083500                                                                          
083600                                                                          
083700****TILEVBSK-AVS                                                          
083800                                                                          
083900* * *   VID NYUPPLÄGG KOLLAS OM AVSÄNDNINGSVECKA KAN GODKÄNNAS            
084000                                                                          
084100                                                                          
084200        IF MID-TILEVBSK-AVS(IX) = ALL '+'                                 
084300           IF IX < 9                                                      
084400              MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-AVS-UT(IX)             
084500              MOVE MFS-NUM-FAELT-RAETT TO                                 
084600                       MOD-TILEVBSK-AVS-UT-ATTR(IX)                       
084700           ELSE                                                           
084800              MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-AVS-NY                 
084900              MOVE MFS-NUM-FAELT-RAETT TO                                 
085000                       MOD-TILEVBSK-AVS-NY-ATTR                           
085100           END-IF                                                         
085200        ELSE                                                              
085300           IF IX < 9                                                      
085400*                                           KONTROLL AV INDATUM           
085500*                                           GÖRS EJ DÅ DETTA ÄR           
085600*                                           ETT VÄRDE SOM KOMMER          
085700*                                           FRÅN BAS OCH ÄR ÅTER-         
085800*                                           INLÄST UTAN ATT KUNNA         
085900*                                           ÄNDRAS                        
086000              MOVE MFS-NUM-FAELT-RAETT TO                                 
086100                               MOD-TILEVBSK-AVS-UT-ATTR(IX)               
086200              MOVE MFS-ROER-EJ-FAELT TO MOD-TILEVBSK-AVS-UT(IX)           
086300              MOVE MID-TILEVBSK-AVS(IX) TO DAT-I-TIDATUM                  
086400              PERFORM S21-DATUMKONV-TILL-AAMMDD                           
086500              IF DAT-KDSVAR-OK                                            
086600                 MOVE DAT-TIAAMMDD TO TILEVBSK-AVS-AAMMDD-WS(IX)          
086700              END-IF                                                      
086800           ELSE                                                           
086900*             IX = 9 ***                                                  
087000              MOVE MID-TILEVBSK-AVS(IX) TO DAT-I-TIDATUM                  
087100              PERFORM S21-DATUMKONV-TILL-AAMMDD                           
087200              IF DAT-KDSVAR-OK                                            
087300                 MOVE DAT-TIAAMMDD TO TILEVBSK-AVS-AAMMDD-WS(IX)          
087400                 PERFORM CBA-KOLLA-MIN-OCH-MAX-GRAENSER                   
087500                 IF MIN-MAX-OK                                            
087600                    MOVE MFS-NUM-FAELT-RAETT TO                           
087700                             MOD-TILEVBSK-AVS-NY-ATTR                     
087800                 ELSE                                                     
087900                    MOVE NEJ TO INDATA-SW                                 
088000                    MOVE MFS-NUM-FAELT-FEL TO                             
088100                             MOD-TILEVBSK-AVS-NY-ATTR                     
088200                 END-IF                                                   
088300              ELSE                                                        
088400                 MOVE NEJ TO INDATA-SW                                    
088500                 MOVE MFS-NUM-FAELT-FEL   TO                              
088600                          MOD-TILEVBSK-AVS-NY-ATTR                        
088700              END-IF                                                      
088800           END-IF                                                         
088900           MOVE MFS-ROER-EJ-FAELT TO MOD-TILEVBSK-AVS-NY                  
089000        END-IF                                                            
089100                                                                          
089200****TILEVBSK-RAD                                                          
089300                                                                          
089400* * *   VID ÄNDRING   KOLLAS OM AVSÄNDNINGSVECKA KAN GODKÄNNAS            
089500*       OBS ENDAST IX 1 - 8                                               
089600                                                                          
089700        IF MID-TILEVBSK-RAD(IX) = ALL '+'                                 
089800           IF IX < 9                                                      
089900              MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-RAD-IN(IX)             
090000              MOVE MFS-NUM-FAELT-RAETT TO                                 
090100                       MOD-TILEVBSK-RAD-IN-ATTR(IX)                       
090200           END-IF                                                         
090300        ELSE                                                              
090400           IF IX < 9                                                      
090500              MOVE MID-TILEVBSK-RAD(IX) TO DAT-I-TIDATUM                  
090600              PERFORM S21-DATUMKONV-TILL-AAMMDD                           
090700              IF DAT-KDSVAR-OK                                            
090800                 MOVE DAT-TIAAMMDD TO TILEVBSK-RAD-AAMMDD-WS(IX)          
090900                 PERFORM CBA-KOLLA-MIN-OCH-MAX-GRAENSER                   
091000                 IF MIN-MAX-OK                                            
091100                    MOVE MFS-NUM-FAELT-RAETT TO                           
091200                               MOD-TILEVBSK-RAD-IN-ATTR(IX)               
091300                    MOVE MFS-ROER-EJ-FAELT TO                             
091400                               MOD-TILEVBSK-RAD-IN(IX)                    
091500                 ELSE                                                     
091600                    MOVE NEJ TO INDATA-SW                                 
091700                    MOVE MFS-NUM-FAELT-FEL TO                             
091800                             MOD-TILEVBSK-RAD-IN-ATTR(IX)                 
091900                 END-IF                                                   
092000              ELSE                                                        
092100                 MOVE NEJ TO INDATA-SW                                    
092200                 MOVE MFS-NUM-FAELT-FEL   TO                              
092300                          MOD-TILEVBSK-RAD-IN-ATTR(IX)                    
092400              END-IF                                                      
092500           END-IF                                                         
092600           MOVE MFS-ROER-EJ-FAELT TO MOD-TILEVBSK-RAD-IN(IX)              
092700        END-IF                                                            
092800                                                                          
092900                                                                          
093000*****KVAVIS                                                               
093100        IF MID-KVAVIS(IX) = ALL '+'                                       
093200           IF IX < 9                                                      
093300              MOVE MFS-RENSA-FAELT TO MOD-KVAVIS-IN(IX)                   
093400              MOVE MFS-NUM-FAELT-RAETT TO                                 
093500                           MOD-KVAVIS-IN-ATTR(IX)                         
093600           ELSE                                                           
093700              MOVE MFS-RENSA-FAELT TO MOD-KVAVIS-NY                       
093800              MOVE MFS-NUM-FAELT-RAETT TO                                 
093900                           MOD-KVAVIS-NY-ATTR                             
094000           END-IF                                                         
094100        ELSE                                                              
094200           IF MID-KVAVIS(IX) NUMERIC                                      
094300              MOVE MID-KVAVIS(IX) TO KVAVIS-WS(IX)                        
094400              IF KVAVIS-WS(IX)  > 0                                       
094500                 IF IX < 9                                                
094600                    MOVE MFS-NUM-FAELT-RAETT TO                           
094700                                 MOD-KVAVIS-IN-ATTR(IX)                   
094800                 ELSE                                                     
094900                    MOVE MFS-NUM-FAELT-RAETT TO                           
095000                                 MOD-KVAVIS-NY-ATTR                       
095100                 END-IF                                                   
095200              ELSE                                                        
095300                 MOVE NEJ TO INDATA-SW                                    
095400                 IF IX < 9                                                
095500                    MOVE MFS-NUM-FAELT-FEL   TO                           
095600                                 MOD-KVAVIS-IN-ATTR(IX)                   
095700                 ELSE                                                     
095800                    MOVE MFS-NUM-FAELT-FEL   TO                           
095900                                 MOD-KVAVIS-NY-ATTR                       
096000                 END-IF                                                   
096100              END-IF                                                      
096200           ELSE                                                           
096300              MOVE NEJ TO INDATA-SW                                       
096400              IF IX < 9                                                   
096500                 MOVE MFS-NUM-FAELT-FEL   TO                              
096600                                 MOD-KVAVIS-IN-ATTR(IX)                   
096700              ELSE                                                        
096800                 MOVE MFS-NUM-FAELT-FEL   TO                              
096900                                 MOD-KVAVIS-NY-ATTR                       
097000              END-IF                                                      
097100           END-IF                                                         
097200           IF IX < 9                                                      
097300              MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVIS-IN(IX)                 
097400           ELSE                                                           
097500              MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVIS-NY                     
097600           END-IF                                                         
097700        END-IF                                                            
097800                                                                          
097900****TILEVBSK-INL-C1 (CDC)                                                 
098000                                                                          
098100        IF MID-TILEVBSK-INL-C1(IX) = ALL '+'                              
098200           IF IX < 9                                                      
098300              MOVE MFS-RENSA-FAELT TO                                     
098400                          MOD-TILEVBSK-INL-C1-IN(IX)                      
098500              MOVE MFS-NUM-FAELT-RAETT TO                                 
098600                       MOD-TILEVBSK-INL-C1-IN-ATTR(IX)                    
098700           ELSE                                                           
098800              MOVE MFS-RENSA-FAELT TO                                     
098900                          MOD-TILEVBSK-INL-C1-NY                          
099000              MOVE MFS-NUM-FAELT-RAETT TO                                 
099100                       MOD-TILEVBSK-INL-C1-NY-ATTR                        
099200           END-IF                                                         
099300        ELSE                                                              
099400          MOVE MID-TILEVBSK-INL-C1(IX) TO DAT-I-TIDATUM                   
099500          PERFORM S21-DATUMKONV-TILL-AAMMDD                               
099600          IF DAT-KDSVAR-OK                                                
099700            MOVE DAT-TIAAMMDD TO TILEVBSK-INL-C1-AAMMDD-WS(IX)            
099800            MOVE TILEVBSK-INL-C1-AAMMDD-WS(IX) TO TMP1-YYMMDD             
099900            IF TILEVBSK-RAD-AAMMDD-WS(IX ) > ZERO                         
100000              MOVE TILEVBSK-RAD-AAMMDD-WS (IX) TO TMP2-YYMMDD             
100100            ELSE                                                          
100200              MOVE TILEVBSK-AVS-AAMMDD-WS (IX) TO TMP2-YYMMDD             
100300            END-IF                                                        
100400            PERFORM WY2000P1                                              
100500            IF TMP1-YYMMDD < TMP2-YYMMDD                                  
100600              MOVE NEJ TO INDATA-SW                                       
100700              IF IX < 9                                                   
100800                MOVE MFS-NUM-FAELT-FEL TO                                 
100900                                   MOD-TILEVBSK-INL-C1-IN-ATTR(IX)        
101000              ELSE                                                        
101100                MOVE MFS-NUM-FAELT-FEL TO                                 
101200                                   MOD-TILEVBSK-INL-C1-NY-ATTR            
101300              END-IF                                                      
101400            ELSE                                                          
101500              IF IX < 9                                                   
101600                PERFORM CBC-KOLLA-MIN-MAX-GRAENS-3AR                      
101700                IF MIN-MAX-OK                                             
101800                  MOVE MFS-NUM-FAELT-RAETT TO                             
101900                                   MOD-TILEVBSK-INL-C1-IN-ATTR(IX)        
102000                ELSE                                                      
102100                  MOVE NEJ TO INDATA-SW                                   
102200                  MOVE MFS-NUM-FAELT-FEL TO                               
102300                                   MOD-TILEVBSK-INL-C1-IN-ATTR(IX)        
102400                END-IF                                                    
102500              ELSE                                                        
102600                PERFORM CBB-KOLLA-MIN-OCH-MAX-GRAENSER                    
102700                IF MIN-MAX-OK                                             
102800                  MOVE MFS-NUM-FAELT-RAETT TO                             
102900                                   MOD-TILEVBSK-INL-C1-NY-ATTR            
103000                ELSE                                                      
103100                  MOVE NEJ TO INDATA-SW                                   
103200                  MOVE MFS-NUM-FAELT-FEL TO                               
103300                                   MOD-TILEVBSK-INL-C1-NY-ATTR            
103400                END-IF                                                    
103500              END-IF                                                      
103600            END-IF                                                        
103700           ELSE                                                           
103800             IF IX < 9                                                    
103900               MOVE MFS-NUM-FAELT-FEL   TO                                
104000                                   MOD-TILEVBSK-INL-C1-IN-ATTR(IX)        
104100             ELSE                                                         
104200               MOVE MFS-NUM-FAELT-FEL   TO                                
104300                                   MOD-TILEVBSK-INL-C1-NY-ATTR            
104400             END-IF                                                       
104500             MOVE NEJ TO INDATA-SW                                        
104600           END-IF                                                         
104700           IF IX < 9                                                      
104800              MOVE MFS-ROER-EJ-FAELT TO                                   
104900                          MOD-TILEVBSK-INL-C1-IN(IX)                      
105000           ELSE                                                           
105100              MOVE MFS-ROER-EJ-FAELT TO                                   
105200                          MOD-TILEVBSK-INL-C1-NY                          
105300           END-IF                                                         
105400        END-IF                                                            
105500                                                                          
105600*****IDLEVNR                                                              
105700        IF IX = 9                                                         
105800           IF MID-IDLEVNR-RAD(IX) = ALL '+'                               
105900              MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-NY                      
106000              MOVE MFS-ALFA-FAELT-RAETT TO                                
106100                              MOD-IDLEVNR-NY-ATTR                         
106200           ELSE                                                           
106300              MOVE MID-IDLEVNR-RAD(IX) TO IDLEVNR-WS                      
106400              IF IDLEVNR-WS NOT = SPACE                                   
106500                 MOVE MFS-ALFA-FAELT-RAETT TO                             
106600                              MOD-IDLEVNR-NY-ATTR                         
106700                 IF IDLEVNR-WS NOT = W-IDLEVNR                            
106800                    MOVE JA TO FL-ANNAN-LEV                               
106900                    MOVE IDLEVNR-WS TO WS-IDLEVNR-ANNAN                   
107000                 END-IF                                                   
107100                 MOVE W-IDLEVNR  TO W-IDLEVNR-TEMP                        
107200                 MOVE IDLEVNR-WS  TO W-IDLEVNR                            
107300                 PERFORM IMS-GET-LEV-LEVA                                 
107400                 IF SEGMENT-SAKNAS                                        
107500                   MOVE NEJ TO INDATA-SW                                  
107600                   MOVE MFS-ALFA-FAELT-FEL   TO                           
107700                                MOD-IDLEVNR-NY-ATTR                       
107800                 END-IF                                                   
107900                 MOVE W-IDLEVNR-TEMP  TO W-IDLEVNR                        
108000              ELSE                                                        
108100                 MOVE NEJ TO INDATA-SW                                    
108200                 MOVE MFS-ALFA-FAELT-FEL   TO                             
108300                              MOD-IDLEVNR-NY-ATTR                         
108400              END-IF                                                      
108500              IF IX < 9                                                   
108600                 MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-RAD(IX)            
108700              ELSE                                                        
108800                 MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-NY                 
108900              END-IF                                                      
109000           END-IF                                                         
109100                                                                          
109200*****IDLEVNR-SHIP                                                         
109300           IF MID-IDLEVNR-SHIP = ALL '+'                                  
109400             MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-SHIP-NY            
109500             MOVE MFS-ALFA-FAELT-RAETT  TO                                
109600                                      MOD-IDLEVNR-SHIP-NY-ATTR            
109700           ELSE                                                           
109800              IF MID-IDLEVNR-SHIP  NOT = SPACE                            
109900                MOVE MID-IDLEVNR-SHIP     TO IDLEVNR-SHIP-WS              
110000                MOVE MFS-ALFA-FAELT-RAETT TO                              
110100                                    MOD-IDLEVNR-SHIP-NY-ATTR              
110200                IF IDLEVNR-SHIP-WS NOT = W-IDLEVNR                        
110300                  MOVE JA                 TO FL-ANNAN-SHIP                
110400                  MOVE IDLEVNR-SHIP-WS    TO WS-SHIP-ANNAN                
110500                END-IF                                                    
110600                MOVE IDLEVNR-SHIP-WS      TO W-IDLEVNR-SHIP               
110700                PERFORM IMS-GET-LEV-LEVA-SHIP                             
110800                IF SEGMENT-SAKNAS                                         
110900                  MOVE NEJ TO INDATA-SW                                   
111000                  MOVE MFS-ALFA-FAELT-FEL TO                              
111100                                     MOD-IDLEVNR-SHIP-NY-ATTR             
111200                 END-IF                                                   
111300***           ELSE                                                        
111400***              MOVE NEJ TO INDATA-SW                                    
111500***              MOVE MFS-ALFA-FAELT-FEL  TO                              
111600***                                  MOD-IDLEVNR-SHIP-NY-ATTR             
111700              END-IF                                                      
111800           END-IF                                                         
111900**                                                                        
112000           IF INDATA-OK                                                   
112100             IF (MID-IDLEVNR-RAD(IX) = ALL '+' AND                        
112200                 IDLEVNR-SHIP-WS     > SPACES)                            
112300               MOVE NEJ TO INDATA-SW                                      
112400               MOVE MFS-ALFA-FAELT-FEL TO                                 
112500                                      MOD-IDLEVNR-SHIP-NY-ATTR            
112600             END-IF                                                       
112700           END-IF                                                         
112800                                                                          
112900        END-IF                                                            
113000        ADD +1 TO IX                                                      
113100     END-PERFORM                                                          
113200                                                                          
113300                                                                          
113400                                                                          
113500*TELEVBSK-EXT                                                             
113600                                                                          
113700     IF MID-TELEVBSK-EXT = ALL '+' OR SPACE                               
113800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT-ATTR                
113900     ELSE                                                                 
114000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT-ATTR                
114100     END-IF                                                               
114200                                                                          
114300     IF MID-TELEVBSK-EXT2 = ALL '+' OR SPACE                              
114400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT2-ATTR               
114500     ELSE                                                                 
114600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT2-ATTR               
114700     END-IF                                                               
114800                                                                          
114900     IF MID-TELEVBSK-EXT3 = ALL '+' OR SPACE                              
115000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT3-ATTR               
115100     ELSE                                                                 
115200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT3-ATTR               
115300     END-IF                                                               
115400                                                                          
115500     IF MID-TELEVBSK-EXT4 = ALL '+' OR SPACE                              
115600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT4-ATTR               
115700     ELSE                                                                 
115800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TELEVBSK-EXT4-ATTR               
115900     END-IF                                                               
116000                                                                          
116100*TIBORT                                                                   
116200                                                                          
116300     IF MID-TIBORT = ALL '+'                                              
116400        MOVE MFS-NUM-FAELT-RAETT TO MOD-TIBORT-ATTR                       
116500     ELSE                                                                 
116600        MOVE MID-TIBORT TO DAT-I-TIDATUM                                  
116700        PERFORM S21-DATUMKONV-TILL-AAMMDD                                 
116800        IF DAT-KDSVAR-OK                                                  
116900           MOVE DAGENS-DATUM   TO TMP1-YYMMDD                             
117000           MOVE DAT-TIAAMMDD   TO TMP2-YYMMDD                             
117100           PERFORM WY2000P1                                               
117200           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
117300              MOVE MFS-NUM-FAELT-FEL   TO MOD-TIBORT-ATTR                 
117400              MOVE NEJ TO INDATA-SW                                       
117500           ELSE                                                           
117600              MOVE MFS-NUM-FAELT-RAETT TO MOD-TIBORT-ATTR                 
117700              MOVE DAT-TIAAMMDD TO TIBORT-WS                              
117800           END-IF                                                         
117900        ELSE                                                              
118000           MOVE MFS-NUM-FAELT-FEL   TO MOD-TIBORT-ATTR                    
118100           MOVE NEJ TO INDATA-SW                                          
118200        END-IF                                                            
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600                                                                          
118700 CBA-KOLLA-MIN-OCH-MAX-GRAENSER SECTION.                                  
118800     MOVE 'CBA-KOLLA-MIN-OCH-MAX-GRAENSER' TO CURRENT-SECTION             
118900     SKIP2                                                                
119000*    AVSÄNDNINGSVECKAN FÅR EJ VARA MINDRE ÄN DAGENS DATUM - 5 V           
119100*    (25 ARBETSDAGAR) ELLER STÖRRE ÄN DAGENS DATUM + 3 ÅR                 
119200                                                                          
119300     MOVE 3            TO WORK-KDCALL                                     
119400     MOVE -25          TO WORK-KVWORKD                                    
119500     MOVE WC-CDC-SE    TO WORK-IDDC                                       
119600     MOVE DAGENS-DATUM TO WORK-TIAAMMDD-TOM                               
119700     CALL WORKDAY   USING WORK-KDCALL,                                    
119800                          WORK-DATE-AREA,                                 
119900                          WORK-KDSVAR                                     
120000     IF WORK-KDSVAR-OK                                                    
120100        IF TILEVBSK-RAD-AAMMDD-WS(IX) > ZERO                              
120200          MOVE TILEVBSK-RAD-AAMMDD-WS(IX) TO TMP1-YYMMDD                  
120300        ELSE                                                              
120400          MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO TMP1-YYMMDD                  
120500        END-IF                                                            
120600        MOVE WORK-TIAAMMDD-FOM            TO TMP2-YYMMDD                  
120700        PERFORM WY2000P1                                                  
120800        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
120900           MOVE NEJ TO MIN-MAX-SW                                         
121000        END-IF                                                            
121100     ELSE                                                                 
121200        MOVE NEJ TO MIN-MAX-SW                                            
121300     END-IF                                                               
121400                                                                          
121500     MOVE DAGENS-DATUM TO HJAELP-DATUM                                    
121600     IF TILEVBSK-RAD-AAMMDD-WS(IX) > ZERO                                 
121700       MOVE TILEVBSK-RAD-AAMMDD-WS(IX) TO TMP1-YYMMDD                     
121800     ELSE                                                                 
121900       MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO TMP1-YYMMDD                     
122000     END-IF                                                               
122100     MOVE HJAELP-DATUM                 TO TMP2-YYMMDD                     
122200     PERFORM WY2000P1                                                     
122300****** LÄGG PÅ 3 ÅR PÅ DAGENS DATUM                                       
122400     ADD 30000 TO TMP2-YYMMDD                                             
122500     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
122600        MOVE NEJ TO MIN-MAX-SW                                            
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000                                                                          
123100 CBB-KOLLA-MIN-OCH-MAX-GRAENSER SECTION.                                  
123200     MOVE 'CBB-KOLLA-MIN-OCH-MAX-GRAENSER' TO CURRENT-SECTION             
123300                                                                          
123400*    INLEVERANSVECKAN FÅR EJ VARA MINDRE ÄN DAGENS DATUM - 5 V            
123500*    (25 ARBETSDAGAR) ELLER STÖRRE ÄN DAGENS DATUM + 3 ÅR                 
123600                                                                          
123700     MOVE 3            TO WORK-KDCALL                                     
123800     MOVE -25          TO WORK-KVWORKD                                    
123900     MOVE WC-CDC-SE    TO WORK-IDDC                                       
124000     MOVE DAGENS-DATUM TO WORK-TIAAMMDD-TOM                               
124100     CALL WORKDAY   USING WORK-KDCALL,                                    
124200                          WORK-DATE-AREA,                                 
124300                          WORK-KDSVAR                                     
124400     IF WORK-KDSVAR-OK                                                    
124500        IF TILEVBSK-INL-C1-AAMMDD-WS(IX) > ZERO                           
124600          MOVE TILEVBSK-INL-C1-AAMMDD-WS(IX) TO TMP1-YYMMDD               
124700        ELSE                                                              
124800          MOVE TILEVBSK-RAD-AAMMDD-WS(IX)    TO TMP1-YYMMDD               
124900        END-IF                                                            
125000        MOVE WORK-TIAAMMDD-FOM               TO TMP2-YYMMDD               
125100        PERFORM WY2000P1                                                  
125200        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
125300           MOVE NEJ TO MIN-MAX-SW                                         
125400        END-IF                                                            
125500     ELSE                                                                 
125600        MOVE NEJ TO MIN-MAX-SW                                            
125700     END-IF                                                               
125800                                                                          
125900     MOVE DAGENS-DATUM TO HJAELP-DATUM                                    
126000     IF TILEVBSK-INL-C1-AAMMDD-WS(IX) > ZERO                              
126100       MOVE TILEVBSK-INL-C1-AAMMDD-WS(IX) TO TMP1-YYMMDD                  
126200     ELSE                                                                 
126300       MOVE TILEVBSK-RAD-AAMMDD-WS(IX)    TO TMP1-YYMMDD                  
126400     END-IF                                                               
126500     MOVE HJAELP-DATUM                    TO TMP2-YYMMDD                  
126600     PERFORM WY2000P1                                                     
126700****** LÄGG PÅ 3 ÅR PÅ DAGENS DATUM                                       
126800     ADD 30000 TO TMP2-YYMMDD                                             
126900     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
127000        MOVE NEJ TO MIN-MAX-SW                                            
127100     END-IF                                                               
127200     .                                                                    
127300     EJECT                                                                
127400 CBC-KOLLA-MIN-MAX-GRAENS-3AR SECTION.                                    
127500     MOVE 'CBC-KOLLA-MIN-MAX-GRAENS-3AR' TO CURRENT-SECTION               
127600                                                                          
127700*    INLEVERANSVECKAN FÅR EJ VARA STÖRRE ÄN DAGENS DATUM + 3 ÅR           
127800                                                                          
127900     MOVE JA   TO MIN-MAX-SW                                              
128000                                                                          
128100     MOVE DAGENS-DATUM TO HJAELP-DATUM                                    
128200     IF TILEVBSK-INL-C1-AAMMDD-WS(IX) > ZERO                              
128300       MOVE TILEVBSK-INL-C1-AAMMDD-WS(IX) TO TMP1-YYMMDD                  
128400     ELSE                                                                 
128500       MOVE TILEVBSK-RAD-AAMMDD-WS(IX)    TO TMP1-YYMMDD                  
128600     END-IF                                                               
128700     MOVE HJAELP-DATUM                    TO TMP2-YYMMDD                  
128800     PERFORM WY2000P1                                                     
128900****** LÄGG PÅ 3 ÅR PÅ DAGENS DATUM                                       
129000     ADD 30000 TO TMP2-YYMMDD                                             
129100     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
129200        MOVE NEJ TO MIN-MAX-SW                                            
129300     END-IF                                                               
129400                                                                          
129500     .                                                                    
129600     EJECT                                                                
129700 CC-KONTROLERA-UPPDATERINGSTYP SECTION.                                   
129800                                                                          
129900     MOVE +1 TO IX                                                        
130000     PERFORM UNTIL IX > 9                                                 
130100        IF IX < 9                                                         
130200           IF MID-KDCMD(IX)               NOT = ALL '+'  OR               
130300              MID-TILEVBSK-RAD(IX)        NOT =  ALL '+' OR               
130400              MID-KVAVIS(IX)              NOT =  ALL '+' OR               
130500              MID-TILEVBSK-INL-C1(IX)     NOT =  ALL '+'                  
130600              MOVE JA TO GAMLA-PLANER-SW                                  
130700           END-IF                                                         
130800        ELSE                                                              
130900           IF IX = 9                                                      
131000              IF MID-TILEVBSK-AVS(IX)      NOT =  ALL '+' OR              
131100                 MID-KVAVIS(IX)            NOT =  ALL '+' OR              
131200                 MID-TILEVBSK-INL-C1(IX)   NOT =  ALL '+' OR              
131300                 MID-IDLEVNR-RAD    (IX)   NOT =  ALL '+' OR              
131400                 MID-IDLEVNR-SHIP          NOT =  ALL '+'                 
131500                 MOVE JA TO NY-PLAN-SW                                    
131600              END-IF                                                      
131700           END-IF                                                         
131800         END-IF                                                           
131900        ADD +1 TO IX                                                      
132000     END-PERFORM                                                          
132100                                                                          
132200     IF NY-PLAN AND GAMLA-PLANER                                          
132300        MOVE NEJ TO INDATA-SW                                             
132400        MOVE +1 TO IX                                                     
132500        PERFORM UNTIL IX > 9                                              
132600           IF IX < 9                                                      
132700              IF MID-TILEVBSK-AVS(IX)      NOT =  ALL '+'                 
132800                                                                          
132900                 IF MID-KDCMD(IX)  NOT = ALL '+'                          
133000                    MOVE MFS-ALFA-FAELT-FEL TO                            
133100                               MOD-KDCMD-IN-ATTR(IX)                      
133200                 ELSE                                                     
133300                    MOVE MFS-OPEN-ALPHA-NOMOD TO                          
133400                               MOD-KDCMD-IN-ATTR(IX)                      
133500                 END-IF                                                   
133600                                                                          
133700                 IF MID-TILEVBSK-RAD(IX) NOT = ALL '+'                    
133800                    MOVE MFS-NUM-FAELT-FEL TO                             
133900                               MOD-TILEVBSK-RAD-IN-ATTR(IX)               
134000                 ELSE                                                     
134100                    MOVE MFS-OPEN-NUM-NOMOD TO                            
134200                               MOD-TILEVBSK-RAD-IN-ATTR(IX)               
134300                 END-IF                                                   
134400                                                                          
134500                 IF MID-KVAVIS(IX) NOT = ALL '+'                          
134600                    MOVE MFS-NUM-FAELT-FEL TO                             
134700                               MOD-KVAVIS-IN-ATTR(IX)                     
134800                 ELSE                                                     
134900                    MOVE MFS-OPEN-NUM-NOMOD TO                            
135000                               MOD-KVAVIS-IN-ATTR(IX)                     
135100                 END-IF                                                   
135200                                                                          
135300                 IF MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                 
135400                    MOVE MFS-NUM-FAELT-FEL TO                             
135500                               MOD-TILEVBSK-INL-C1-IN-ATTR(IX)            
135600                 ELSE                                                     
135700                    MOVE MFS-OPEN-NUM-NOMOD TO                            
135800                               MOD-TILEVBSK-INL-C1-IN-ATTR(IX)            
135900                 END-IF                                                   
136000                                                                          
136100              ELSE                                                        
136200                 MOVE MFS-FORMATETS-ATTR TO                               
136300                               MOD-KDCMD-IN-ATTR(IX)                      
136400                               MOD-TILEVBSK-RAD-IN-ATTR(IX)               
136500                               MOD-KVAVIS-IN-ATTR(IX)                     
136600                               MOD-TILEVBSK-INL-C1-IN-ATTR(IX)            
136700              END-IF                                                      
136800              MOVE MFS-ROER-EJ-FAELT TO                                   
136900                                     MOD-KDCMD-IN(IX)                     
137000                                     MOD-TILEVBSK-RAD-IN(IX)              
137100                                     MOD-KVAVIS-IN(IX)                    
137200                                     MOD-TILEVBSK-INL-C1-IN(IX)           
137300                                                                          
137400              MOVE MFS-ADD-LAES-IN-FAELT TO                               
137500                               MOD-TILEVBSK-AVS-UT-ATTR(IX)               
137600              MOVE MFS-ROER-EJ-FAELT TO                                   
137700                               MOD-TILEVBSK-AVS-UT(IX)                    
137800           ELSE                                                           
137900*             * OBS IX = 9 *                                              
138000              IF MID-TILEVBSK-AVS(IX) NOT = ALL '+'                       
138100                 MOVE MFS-NUM-FAELT-FEL TO                                
138200                               MOD-TILEVBSK-AVS-NY-ATTR                   
138300              ELSE                                                        
138400                 MOVE MFS-NUM-FAELT-RAETT TO                              
138500                               MOD-TILEVBSK-AVS-NY-ATTR                   
138600              END-IF                                                      
138700              IF MID-KVAVIS(IX) NOT = ALL '+'                             
138800                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-NY-ATTR             
138900              ELSE                                                        
139000                 MOVE MFS-NUM-FAELT-RAETT TO MOD-KVAVIS-NY-ATTR           
139100              END-IF                                                      
139200              IF MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                    
139300                 MOVE MFS-NUM-FAELT-FEL TO                                
139400                               MOD-TILEVBSK-INL-C1-NY-ATTR                
139500              ELSE                                                        
139600                 MOVE MFS-NUM-FAELT-RAETT TO                              
139700                               MOD-TILEVBSK-INL-C1-NY-ATTR                
139800              END-IF                                                      
139900              IF MID-IDLEVNR-RAD(IX) NOT = ALL '+'                        
140000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-NY-ATTR           
140100              ELSE                                                        
140200                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-NY-ATTR         
140300              END-IF                                                      
140400              IF MID-IDLEVNR-SHIP     NOT = ALL '+'                       
140500                 MOVE MFS-ALFA-FAELT-FEL TO                               
140600                                        MOD-IDLEVNR-SHIP-NY-ATTR          
140700              ELSE                                                        
140800                 MOVE MFS-ALFA-FAELT-RAETT TO                             
140900                                        MOD-IDLEVNR-SHIP-NY-ATTR          
141000              END-IF                                                      
141100              MOVE MFS-ROER-EJ-FAELT TO                                   
141200                               MOD-TILEVBSK-AVS-NY                        
141300                               MOD-KVAVIS-NY                              
141400                               MOD-TILEVBSK-INL-C1-NY                     
141500                               MOD-IDLEVNR-NY                             
141600                               MOD-IDLEVNR-SHIP-NY                        
141700           END-IF                                                         
141800           ADD +1 TO IX                                                   
141900        END-PERFORM                                                       
142000     END-IF                                                               
142100     .                                                                    
142200     EJECT                                                                
142300                                                                          
142400 CD-RELATIONSKONTROLL SECTION.                                            
142500     SKIP2                                                                
142600     IF NY-PLAN                                                           
142700        IF MID-TILEVBSK-AVS (9) = ALL '+'                                 
142800           MOVE NEJ TO INDATA-SW                                          
142900           MOVE MFS-NUM-FAELT-FEL TO                                      
143000                       MOD-TILEVBSK-AVS-NY-ATTR                           
143100        END-IF                                                            
143200        IF  MID-KVAVIS (9)          = ALL '+'                             
143300           MOVE MFS-NUM-FAELT-FEL TO                                      
143400                    MOD-KVAVIS-NY-ATTR                                    
143500           MOVE NEJ TO INDATA-SW                                          
143600        END-IF                                                            
143700     END-IF                                                               
143800     IF MID-TELEVBSK-EXT = ALL '+' OR SPACE                               
143900        CONTINUE                                                          
144000     ELSE                                                                 
144100        IF MID-TIBORT = ALL '+'                                           
144200           MOVE MFS-NUM-FAELT-FEL TO                                      
144300                       MOD-TIBORT-ATTR                                    
144400           MOVE NEJ TO INDATA-SW                                          
144500        END-IF                                                            
144600     END-IF                                                               
144700     IF MID-TELEVBSK-EXT2 = ALL '+' OR SPACE                              
144800        CONTINUE                                                          
144900     ELSE                                                                 
145000        IF MID-TIBORT = ALL '+'                                           
145100           MOVE MFS-NUM-FAELT-FEL TO                                      
145200                       MOD-TIBORT-ATTR                                    
145300           MOVE FEL-20            TO MOD-TEMFSINF                         
145400           MOVE NEJ TO INDATA-SW                                          
145500        END-IF                                                            
145600     END-IF                                                               
145700     IF MID-TELEVBSK-EXT3 = ALL '+' OR SPACE                              
145800        CONTINUE                                                          
145900     ELSE                                                                 
146000        IF MID-TIBORT = ALL '+'                                           
146100           MOVE MFS-NUM-FAELT-FEL TO                                      
146200                       MOD-TIBORT-ATTR                                    
146300           MOVE FEL-20            TO MOD-TEMFSINF                         
146400           MOVE NEJ TO INDATA-SW                                          
146500        END-IF                                                            
146600     END-IF                                                               
146700     IF MID-TELEVBSK-EXT4 = ALL '+' OR SPACE                              
146800        CONTINUE                                                          
146900     ELSE                                                                 
147000        IF MID-TIBORT = ALL '+'                                           
147100           MOVE MFS-NUM-FAELT-FEL TO                                      
147200                       MOD-TIBORT-ATTR                                    
147300           MOVE FEL-20            TO MOD-TEMFSINF                         
147400           MOVE NEJ TO INDATA-SW                                          
147500        END-IF                                                            
147600     END-IF                                                               
147700                                                                          
147800     IF GAMLA-PLANER                                                      
147900        MOVE +1 TO IX                                                     
148000        PERFORM UNTIL IX > 8                                              
148100           IF MID-KVAVIS(IX)           = ALL '+'                          
148200           AND MID-TILEVBSK-INL-C1(IX) = ALL '+'                          
148300              CONTINUE                                                    
148400           ELSE                                                           
148500              IF MID-KDCMD(IX)         = ALL '+'                          
148600                 MOVE MFS-ALFA-FAELT-FEL TO                               
148700                          MOD-KDCMD-IN-ATTR(IX)                           
148800                 MOVE NEJ TO INDATA-SW                                    
148900              END-IF                                                      
149000           END-IF                                                         
149100                                                                          
149200           IF MID-KDCMD-REPLACE(IX)                                       
149300              IF  MID-KVAVIS(IX)          = ALL '+'                       
149400              AND MID-TILEVBSK-INL-C1(IX) = ALL '+'                       
149500              AND MID-TILEVBSK-RAD   (IX) = ALL '+'                       
149600                 MOVE MFS-NUM-FAELT-FEL TO                                
149700                          MOD-KVAVIS-IN-ATTR(IX)                          
149800                          MOD-TILEVBSK-INL-C1-IN-ATTR(IX)                 
149900                          MOD-TILEVBSK-RAD-IN-ATTR(IX)                    
150000                 MOVE NEJ TO INDATA-SW                                    
150100              END-IF                                                      
150200           END-IF                                                         
150300                                                                          
150400           IF MID-KDCMD-DELETE(IX)                                        
150500              IF  MID-KVAVIS(IX)          NOT = ALL '+'                   
150600              OR  MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                   
150700              OR  MID-TILEVBSK-RAD   (IX) NOT = ALL '+'                   
150800                 MOVE NEJ TO INDATA-SW                                    
150900                 IF  MID-KVAVIS(IX)          NOT = ALL '+'                
151000                    MOVE MFS-NUM-FAELT-FEL TO                             
151100                             MOD-KVAVIS-IN-ATTR(IX)                       
151200                 END-IF                                                   
151300                 IF  MID-TILEVBSK-INL-C1(IX)  NOT = ALL '+'               
151400                    MOVE MFS-NUM-FAELT-FEL TO                             
151500                          MOD-TILEVBSK-INL-C1-IN-ATTR(IX)                 
151600                 END-IF                                                   
151700                 IF  MID-TILEVBSK-RAD   (IX)  NOT = ALL '+'               
151800                    MOVE MFS-NUM-FAELT-FEL TO                             
151900                          MOD-TILEVBSK-RAD-IN-ATTR(IX)                    
152000                 END-IF                                                   
152100              END-IF                                                      
152200           END-IF                                                         
152300                                                                          
152400           ADD +1 TO IX                                                   
152500        END-PERFORM                                                       
152600     END-IF                                                               
152700                                                                          
152800     .                                                                    
152900     EJECT                                                                
153000                                                                          
153100 D-KOLLA-MOT-BAS SECTION.                                                 
153200     MOVE 'D-KOLLA-MOT-BAS '  TO CURRENT-SECTION                          
153300     SKIP2                                                                
153400     PERFORM DA-SAMLA-DATA                                                
153500     IF INDATA-OK                                                         
153600       IF NY-PLAN                                                         
153700          PERFORM DB-KOLLA-NY-PLAN                                        
153800       ELSE                                                               
153900          IF GAMLA-PLANER                                                 
154000             PERFORM DC-KOLLA-GAMLA-PLANER                                
154100          END-IF                                                          
154200       END-IF                                                             
154300     END-IF                                                               
154400     .                                                                    
154500     EJECT                                                                
154600                                                                          
154700 DA-SAMLA-DATA SECTION.                                                   
154800     MOVE 'DA-SAMLA-DATA '  TO CURRENT-SECTION                            
154900*                                                                         
155000     IF NY-PLAN                                                           
155100       IF MID-IDLEVNR-RAD (9) = ALL '+' AND                               
155200          MID-IDLEVNR-SHIP = ALL '+'                                      
155300         PERFORM IMS-GET-ART                                              
155400         IF ART-IDLEVNR > SPACE                                           
155500           MOVE ART-IDLEVNR         TO W-IDLEVNR                          
155600                                       W-IDLEVNR-SHIP                     
155700           PERFORM IMS-GET-CLAG                                           
155800           IF SEGMENT-FINNS AND CLAG-IDLEVNR-SHIP > SPACE                 
155900             MOVE CLAG-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                     
156000           END-IF                                                         
156100         ELSE                                                             
156200           MOVE NEJ TO INDATA-SW                                          
156300           MOVE FEL-4  TO MOD-TEMFSFEL                                    
156400         END-IF                                                           
156500       ELSE                                                               
156600         IF MID-IDLEVNR-RAD (9) > SPACES AND                              
156700            MID-IDLEVNR-SHIP = ALL '+'                                    
156800           MOVE MID-IDLEVNR-RAD (9)   TO W-IDLEVNR                        
156900                                         W-IDLEVNR-SHIP                   
157000           IF ART-IDLEVNR = IDLEVNR-WS                                    
157100             PERFORM IMS-GET-CLAG                                         
157200             IF SEGMENT-FINNS AND CLAG-IDLEVNR-SHIP > SPACE               
157300               MOVE CLAG-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                   
157400             END-IF                                                       
157500           ELSE                                                           
157600             PERFORM IMS-GET-ART                                          
157700             PERFORM IMS-GNP-ARTC23                                       
157800             MOVE NEJ TO TRAFF                                            
157900             PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                   
158000               IF IDLEVNR-WS = AVT-IDLEVNR-AVT                            
158100                 MOVE AVT-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                  
158200                 MOVE JA TO TRAFF                                         
158300               END-IF                                                     
158400               PERFORM IMS-GNP-ARTC23                                     
158500             END-PERFORM                                                  
158600**           IF TRAFF = NEJ                                               
158700**             MOVE W-IDLEVNR         TO W-IDLEVNR-SHIP                   
158800**           END-IF                                                       
158900           END-IF                                                         
159000         ELSE                                                             
159100           IF MID-IDLEVNR-RAD (9) > SPACES AND                            
159200              MID-IDLEVNR-SHIP > SPACES                                   
159300             MOVE MID-IDLEVNR-SHIP    TO W-IDLEVNR-SHIP                   
159400             MOVE MID-IDLEVNR-RAD (9) TO W-IDLEVNR                        
159500           END-IF                                                         
159600         END-IF                                                           
159700       END-IF                                                             
159800     ELSE                                                                 
159900**     TO FETCH IDLEVNR-SHIP WHEN CMD IS 'R' OR 'Ä'                       
160000       MOVE +1 TO IX                                                      
160100       PERFORM UNTIL IX > 8                                               
160200         IF MID-KDCMD-REPLACE(IX)                                         
160300           MOVE MID-IDLEVNR-RAD (IX)       TO W-IDLEVNR                   
160400                                              W-IDLEVNR-SHIP              
160500         END-IF                                                           
160600         ADD +1 TO IX                                                     
160700       END-PERFORM                                                        
160800       PERFORM IMS-GET-ART                                                
160900       IF SEGMENT-FINNS                                                   
161000         IF ART-IDLEVNR = W-IDLEVNR                                       
161100           PERFORM IMS-GET-CLAG                                           
161200           IF SEGMENT-FINNS AND CLAG-IDLEVNR-SHIP > SPACE                 
161300             MOVE CLAG-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                     
161400           END-IF                                                         
161500         ELSE                                                             
161600           PERFORM IMS-GNP-ARTC23                                         
161700           MOVE NEJ TO TRAFF                                              
161800           PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                     
161900             IF W-IDLEVNR = AVT-IDLEVNR-AVT                               
162000               MOVE AVT-IDLEVNR-SHIP TO W-IDLEVNR-SHIP                    
162100               MOVE JA TO TRAFF                                           
162200             END-IF                                                       
162300             PERFORM IMS-GNP-ARTC23                                       
162400           END-PERFORM                                                    
162500**         IF TRAFF = NEJ                                                 
162600**           MOVE W-IDLEVNR         TO W-IDLEVNR-SHIP                     
162700**         END-IF                                                         
162800         END-IF                                                           
162900       END-IF                                                             
163000     END-IF                                                               
163100*                                                                         
163200     PERFORM IMS-GET-LEV-LEVA-SHIP                                        
163300     IF SEGMENT-FINNS                                                     
163400        COMPUTE KVDAGAR-TTC1 = LEVA-LEV-KVDAGAR-TTC1 + 1                  
163500     END-IF                                                               
163600     PERFORM IMS-GET-ART                                                  
163700**** ¤ GU-WDK601                                                          
163800     MOVE ART-IDLEVNR TO HUVUDLEV-WS                                      
163900     PERFORM IMS-GET-CLAG                                                 
164000**** ¤ GNP-WDK611                                                         
164100     IF SEGMENT-FINNS                                                     
164200*       LÄGGER TILL EN DAG EFTERSOM WORKDAY RÄKNAR                        
164300*       FR O M OCH T O M, DVS MAN FÖRLORAR EN DAG                         
164400                                                                          
164500        COMPUTE KVDAGAR-INLEV = CLAG-KVDAGAR-INLEV + 1                    
164600     END-IF                                                               
164700                                                                          
164800                                                                          
164900     MOVE +1 TO IX                                                        
165000     PERFORM UNTIL IX > 9                                                 
165100        MOVE ZERO TO GAMMAL-TILEVBSK-DISP-C1 (IX)                         
165200        ADD +1 TO IX                                                      
165300     END-PERFORM                                                          
165400                                                                          
165500     .                                                                    
165600     EJECT                                                                
165700                                                                          
165800                                                                          
165900 DB-KOLLA-NY-PLAN SECTION.                                                
166000     MOVE 'DB-KOLLA-NY-PLAN '   TO CURRENT-SECTION                        
166100                                                                          
166200     MOVE +9  TO IX                                                       
166300                                                                          
166400     IF INLB-ROT-FINNS                                                    
166500        IF INLB-LEV-FINNS                                                 
166600           PERFORM IMS-GET-LEV-INLB                                       
166700*          ¤ GU-WDD901-902                                                
166800           IF SEGMENT-FINNS                                               
166900             MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO WS-DALEVBSK-AAMMDD        
167000             IF WS-DALEVBSK-AAMMDD > 500000                               
167100                MOVE 19                      TO WS-DALEVBSK-SS            
167200             ELSE                                                         
167300                MOVE 20                      TO WS-DALEVBSK-SS            
167400             END-IF                                                       
167500             MOVE WS-DALEVBSK-AVS            TO W-DALEVBSK                
167600             PERFORM IMS-GET-LEVBESK-INLB                                 
167700*          ¤ GHNP-WDD924                                                  
167800           END-IF                                                         
167900           IF SEGMENT-FINNS                                               
168000              MOVE NEJ TO INDATA-SW                                       
168100              MOVE FEL-16 TO MOD-TEMFSFEL                                 
168200*              LEVERANSBESKED FINNS REDAN                                 
168300                                                                          
168400              IF MID-TILEVBSK-AVS(IX) NOT = ALL '+'                       
168500                 MOVE MFS-NUM-FAELT-FEL TO                                
168600                            MOD-TILEVBSK-AVS-NY-ATTR                      
168700              END-IF                                                      
168800              IF MID-KVAVIS(IX) NOT = ALL '+'                             
168900                 MOVE MFS-NUM-FAELT-FEL TO                                
169000                         MOD-KVAVIS-NY-ATTR                               
169100              END-IF                                                      
169200              IF MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                    
169300                 MOVE MFS-NUM-FAELT-FEL TO                                
169400                         MOD-TILEVBSK-INL-C1-NY-ATTR                      
169500              END-IF                                                      
169600              IF MID-IDLEVNR-RAD (IX) NOT = ALL '+'                       
169700                 MOVE MFS-ALFA-FAELT-FEL TO                               
169800                         MOD-IDLEVNR-NY-ATTR                              
169900              END-IF                                                      
170000              IF MID-IDLEVNR-SHIP     NOT = ALL '+'                       
170100                 MOVE MFS-ALFA-FAELT-FEL TO                               
170200                         MOD-IDLEVNR-SHIP-NY-ATTR                         
170300              END-IF                                                      
170400           END-IF                                                         
170500        END-IF                                                            
170600     END-IF                                                               
170700     .                                                                    
170800     EJECT                                                                
170900                                                                          
171000 DC-KOLLA-GAMLA-PLANER SECTION.                                           
171100     MOVE 'DC-KOLLA-GAMLA-PLANER '  TO CURRENT-SECTION                    
171200                                                                          
171300     IF INLB-ROT-FINNS                                                    
171400       IF INLB-LEV-FINNS                                                  
171500         MOVE +1 TO IX                                                    
171600         PERFORM UNTIL IX > 8                                             
171700           IF MID-KDCMD-REPLACE(IX) OR MID-KDCMD-DELETE(IX)               
171800             MOVE MID-IDLEVNR-RAD (IX) TO W-IDLEVNR                       
171900             PERFORM IMS-GET-LEV-INLB                                     
172000***          ¤ GU-WDD901-902                                              
172100             IF SEGMENT-FINNS                                             
172200               MOVE TILEVBSK-AVS-AAMMDD-WS(IX)                            
172300                                       TO WS-DALEVBSK-AAMMDD              
172400               IF WS-DALEVBSK-AAMMDD > 500000                             
172500                 MOVE 19               TO WS-DALEVBSK-SS                  
172600               ELSE                                                       
172700                 MOVE 20               TO WS-DALEVBSK-SS                  
172800               END-IF                                                     
172900               MOVE WS-DALEVBSK-AVS    TO W-DALEVBSK                      
173000               PERFORM IMS-GET-LEVBESK-INLB                               
173100*              ¤ GHNP-WDD924                                              
173200             END-IF                                                       
173300                                                                          
173400             IF SEGMENT-FINNS                                             
173500               MOVE INLB-LEV-TILEVBSK-DISP    TO                          
173600                         GAMMAL-TILEVBSK-DISP-C1 (IX)                     
173700                                                                          
173800* ARTIKELN                                                                
173900* FÖRAVISERAD                                                             
174000* MAN FÅR TA BORT MEN INTE ÄNDRA FÖRAVISERAD                              
174100                                                                          
174200               IF MID-KDCMD-REPLACE(IX)                                   
174300                 IF INLB-LEV-FLFORAVI    = JA                             
174400                   MOVE NEJ TO INDATA-SW                                  
174500                   MOVE FEL-3 TO MOD-TEMFSFEL                             
174600*¤¤¤¤¤¤¤¤¤¤¤¤¤     ARTIKELN FÖRAVISERAD                                   
174700                   IF MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'               
174800                     MOVE MFS-NUM-FAELT-FEL TO                            
174900                                  MOD-TILEVBSK-INL-C1-IN-ATTR(IX)         
175000                   END-IF                                                 
175100                 END-IF                                                   
175200               END-IF                                                     
175300             ELSE                                                         
175400               IF MID-KDCMD-REPLACE(IX) OR                                
175500                                    MID-KDCMD-DELETE(IX)                  
175600                 MOVE NEJ TO INDATA-SW                                    
175700                 MOVE FEL-12  TO MOD-TEMFSFEL                             
175800*¤¤¤¤¤           LEVERANSBESKED FINNS EJ PÅ LEVERANTÖREN                  
175900                 MOVE MFS-NUM-FAELT-FEL TO                                
176000                            MOD-TILEVBSK-AVS-UT-ATTR(IX)                  
176100               END-IF                                                     
176200             END-IF                                                       
176300           END-IF                                                         
176400           ADD +1 TO IX                                                   
176500         END-PERFORM                                                      
176600       END-IF                                                             
176700     END-IF                                                               
176800     .                                                                    
176900     EJECT                                                                
177000                                                                          
177100 E-UPPDATERA-VISA-BILD SECTION.                                           
177200     MOVE 'E-UPPDATERA-VISA-BILD '   TO CURRENT-SECTION                   
177300     SKIP2                                                                
177400     IF INLB-ROT-FINNS AND INLB-LEV-FINNS                                 
177500        CONTINUE                                                          
177600     ELSE                                                                 
177700        PERFORM EA-ISRT-ART-LEV-SEGMENT                                   
177800     END-IF                                                               
177900                                                                          
178000     IF FL-ANNAN-LEV = JA                                                 
178100*       KOLL OM LEV FINNS, ANNARS ISRT                                    
178200        MOVE WS-IDLEVNR-ANNAN TO W-IDLEVNR                                
178300        PERFORM IMS-GET-LEV-INLB                                          
178400        IF SEGMENT-SAKNAS                                                 
178500           MOVE W-IDLEVNR TO INLB-IDLEVNR                                 
178600           MOVE ZERO      TO INLB-KVBR                                    
178700                             INLB-TILEVPL                                 
178800           PERFORM IMS-ISRT-LEV-INLB                                      
178900****       ¤ ISRT-WDD901-WDD902                                           
179000                                                                          
179100           MOVE JA TO INLB-LEV-SW                                         
179200        END-IF                                                            
179300     END-IF                                                               
179400                                                                          
179500     IF FL-ANNAN-SHIP = JA                                                
179600*       KOLL OM LEV FINNS, ANNARS ISRT                                    
179700        MOVE WS-SHIP-ANNAN TO W-IDLEVNR-SHIP                              
179800        PERFORM IMS-GET-LEV-INLB-SHIP                                     
179900        IF SEGMENT-SAKNAS                                                 
180000           MOVE W-IDLEVNR-SHIP TO INLB-IDLEVNR                            
180100           MOVE ZERO           TO INLB-KVBR                               
180200                                  INLB-TILEVPL                            
180300           PERFORM IMS-ISRT-LEV-INLB                                      
180400****       ¤ ISRT-WDD901-WDD902                                           
180500                                                                          
180600           MOVE JA TO INLB-LEV-SW                                         
180700        END-IF                                                            
180800     END-IF                                                               
180900                                                                          
181000     IF NY-PLAN                                                           
181100        PERFORM EB-UPPDATERA-NY-PLAN                                      
181200     ELSE                                                                 
181300        IF GAMLA-PLANER                                                   
181400           PERFORM EC-UPPDATERA-GAMLA-PLANER                              
181500        END-IF                                                            
181600     END-IF                                                               
181700     PERFORM ED-TA-HAND-OM-INFOTEXTERNA                                   
181800     PERFORM EE-VISA-BILD                                                 
181900     MOVE MED-1 TO MOD-TEMFSINF                                           
182000*¤¤  UPPDATERING GJORD                                                    
182100     .                                                                    
182200     EJECT                                                                
182300                                                                          
182400 EA-ISRT-ART-LEV-SEGMENT SECTION.                                         
182500     MOVE 'EA-ISRT-ART-LEV-SEGMENT ' TO CURRENT-SECTION                   
182600     SKIP2                                                                
182700     MOVE W-IDARTNR       TO W-IDARTNR-INLB                               
182800     MOVE WC-CDC-SE       TO W-IDDC-INLB                                  
182900     PERFORM IMS-GET-LEV-INLB                                             
183000     IF SEGMENT-FINNS                                                     
183100        MOVE JA           TO INLB-LEV-SW                                  
183200     ELSE                                                                 
183300        MOVE INLB-LEV-WDD924 TO WINLB-LEV-WDD924                          
183400        IF INLB-ROT-SAKNAS                                                
183500           MOVE W-IDARTNR TO INLB-IDARTNR                                 
183600           MOVE WC-CDC-SE TO INLB-IDDC                                    
183700           PERFORM IMS-ISRT-ROT-INLB                                      
183800****    ¤ ISRT -WDD901                                                    
183900           MOVE JA        TO INLB-ROT-SW                                  
184000        END-IF                                                            
184100        MOVE W-IDLEVNR    TO INLB-IDLEVNR                                 
184200        MOVE ZERO         TO INLB-KVBR                                    
184300                             INLB-TILEVPL                                 
184400        PERFORM IMS-ISRT-LEV-INLB                                         
184500**** ¤ ISRT-WDD901-WDD902                                                 
184600        MOVE JA           TO INLB-LEV-SW                                  
184700*    PERFORM IMS-GET-LEV-INLB                                             
184800**** ¤ GU-WDD901-WDD902                                                   
184900        MOVE WINLB-LEV-WDD924 TO INLB-LEV-WDD924                          
185000     END-IF                                                               
185100     .                                                                    
185200     EJECT                                                                
185300                                                                          
185400                                                                          
185500 EB-UPPDATERA-NY-PLAN SECTION.                                            
185600     MOVE 'EB-UPPDATERA-NY-PLAN ' TO CURRENT-SECTION                      
185700                                                                          
185800     MOVE 9  TO IX                                                        
185900     IF INLB-ROT-FINNS AND INLB-LEV-FINNS                                 
186000        PERFORM IMS-GET-LEV-INLB                                          
186100***     ¤ GU-WDD901-WDD902                                                
186200        IF SEGMENT-FINNS                                                  
186300           MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO WS-DALEVBSK-AAMMDD          
186400           IF WS-DALEVBSK-AAMMDD > 500000                                 
186500              MOVE 19                      TO WS-DALEVBSK-SS              
186600           ELSE                                                           
186700              MOVE 20                      TO WS-DALEVBSK-SS              
186800           END-IF                                                         
186900           MOVE WS-DALEVBSK-AVS            TO W-DALEVBSK                  
187000           PERFORM IMS-GET-LEVBESK-INLB                                   
187100***        ¤GHNP-WDD924                                                   
187200           IF SEGMENT-FINNS                                               
187300              CONTINUE                                                    
187400           ELSE                                                           
187500              PERFORM EBA-BERAEKNA-VECKOR                                 
187600              MOVE NEJ TO INLB-LEV-FLSENLEV                               
187700                          INLB-LEV-FLFORAVI                               
187800              PERFORM S10-SKAPA-EV-TIDISPIN-TRANS                         
187900              PERFORM IMS-ISRT-LEVBESK-INLB                               
188000***           ¤ ISRT-WDD901-902-924                                       
188100              MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-AVS-NY                 
188200                                      MOD-KVAVIS-NY                       
188300                                      MOD-TILEVBSK-INL-C1-NY              
188400                                      MOD-IDLEVNR-NY                      
188500                                      MOD-IDLEVNR-SHIP-NY                 
188600           END-IF                                                         
188700        ELSE                                                              
188800           PERFORM EBA-BERAEKNA-VECKOR                                    
188900           MOVE NEJ TO INLB-LEV-FLSENLEV                                  
189000                       INLB-LEV-FLFORAVI                                  
189100           PERFORM S10-SKAPA-EV-TIDISPIN-TRANS                            
189200           PERFORM IMS-ISRT-LEVBESK-INLB                                  
189300***        ¤ ISRT-WDD901-902-924                                          
189400        END-IF                                                            
189500     END-IF                                                               
189600     .                                                                    
189700     EJECT                                                                
189800                                                                          
189900 EBA-BERAEKNA-VECKOR SECTION.                                             
190000     MOVE 'EBA-BERAEKNA-VECKOR ' TO CURRENT-SECTION                       
190100     SKIP2                                                                
190200                                                                          
190300     MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO WS-DALEVBSK-AAMMDD                
190400     IF WS-DALEVBSK-AAMMDD > 500000                                       
190500        MOVE 19                      TO WS-DALEVBSK-SS                    
190600     ELSE                                                                 
190700        MOVE 20                      TO WS-DALEVBSK-SS                    
190800     END-IF                                                               
190900     MOVE WS-DALEVBSK-AVS            TO INLB-LEV-DALEVBSK-AVS             
191000     MOVE ZERO                TO INLB-LEV-TILEVBSK-INL                    
191100                                 INLB-LEV-TILEVBSK-DISP                   
191200                                 INLB-LEV-KVAVIS-BSKURS                   
191300                                 INLB-LEV-KVAVIS-BSKKVAR                  
191400                                                                          
191500     IF MID-TILEVBSK-INL-C1(IX) = ALL '+'                                 
191600           PERFORM R3-RAK-MASKINELLT                                      
191700     ELSE                                                                 
191800        PERFORM R1-RAK-INLC1-DISPC1                                       
191900           MOVE MID-KVAVIS(IX)    TO INLB-LEV-KVAVIS-BSKURS               
192000                                     INLB-LEV-KVAVIS-BSKKVAR              
192100     END-IF                                                               
192200                                                                          
192300     MOVE DAGENS-DATUM           TO INLB-LEV-TIREGDAT                     
192400     MOVE DAGENS-HHMMSS          TO INLB-LEV-TIREGTID                     
192500     .                                                                    
192600     EJECT                                                                
192700                                                                          
192800 EC-UPPDATERA-GAMLA-PLANER SECTION.                                       
192900     MOVE 'EC-UPPDATERA-GAMLA-PLANER'  TO CURRENT-SECTION                 
193000                                                                          
193100     MOVE +1 TO IX                                                        
193200     PERFORM UNTIL IX > 8                                                 
193300        IF MID-KDCMD-REPLACE(IX) OR MID-KDCMD-DELETE(IX)                  
193400           MOVE MID-IDLEVNR-RAD (IX)       TO W-IDLEVNR                   
193500           PERFORM IMS-GET-LEV-INLB                                       
193600****       ¤ GU-WDD901-902                                                
193700           IF SEGMENT-FINNS                                               
193800             MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO WS-DALEVBSK-AAMMDD        
193900             IF WS-DALEVBSK-AAMMDD > 500000                               
194000                MOVE 19                      TO WS-DALEVBSK-SS            
194100             ELSE                                                         
194200                MOVE 20                      TO WS-DALEVBSK-SS            
194300             END-IF                                                       
194400             MOVE WS-DALEVBSK-AVS            TO W-DALEVBSK                
194500             PERFORM IMS-GET-LEVBESK-INLB                                 
194600****       ¤ GHNP-WDD924                                                  
194700           END-IF                                                         
194800           IF SEGMENT-FINNS                                               
194900              IF MID-KDCMD-REPLACE(IX)                                    
195000               IF MID-TILEVBSK-RAD(IX) NOT = ALL '+' AND                  
195100                  MID-TILEVBSK-RAD(IX) NOT = MID-TILEVBSK-AVS(IX)         
195200                 PERFORM ECB-NYCKEL-TILEVBSK-ANDRAD                       
195300               ELSE                                                       
195400                 PERFORM ECA-BEHANDLA-AENDRING                            
195500                 PERFORM S10-SKAPA-EV-TIDISPIN-TRANS                      
195600                 PERFORM IMS-REPL-INLB                                    
195700****             ¤ REPL-WDD924                                            
195800               END-IF                                                     
195900              ELSE                                                        
196000                 IF MID-KDCMD-DELETE(IX)                                  
196100                       PERFORM S10-SKAPA-EV-TIDISPIN-TRANS                
196200                       PERFORM IMS-DLET-INLB                              
196300****                   ¤ DLET-WDD924                                      
196400                 END-IF                                                   
196500              END-IF                                                      
196600           END-IF                                                         
196700        END-IF                                                            
196800        ADD +1 TO IX                                                      
196900     END-PERFORM                                                          
197000     .                                                                    
197100     EJECT                                                                
197200                                                                          
197300 ECA-BEHANDLA-AENDRING SECTION.                                           
197400     MOVE 'ECA-BEHANDLA-AENDRING ' TO CURRENT-SECTION                     
197500                                                                          
197600     IF MID-KVAVIS(IX)          NOT = ALL '+' AND                         
197700        MID-TILEVBSK-INL-C1(IX)     = ALL '+'                             
197800                                                                          
197900        PERFORM ECA1-ANDRAT-ANTAL                                         
198000     END-IF                                                               
198100                                                                          
198200     IF MID-KVAVIS(IX)          NOT = ALL '+' AND                         
198300        MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                             
198400                                                                          
198500        PERFORM ECA2-ANDR-ANT-ANDR-DAG-C1                                 
198600     END-IF                                                               
198700                                                                          
198800     IF MID-KVAVIS(IX)              = ALL '+' AND                         
198900        MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                             
199000                                                                          
199100        PERFORM ECA4-ANDRAD-DAG-C1                                        
199200     END-IF                                                               
199300                                                                          
199400     MOVE DAGENS-DATUM             TO INLB-LEV-TIREGDAT                   
199500     MOVE DAGENS-HHMMSS            TO INLB-LEV-TIREGTID                   
199600     .                                                                    
199700     EJECT                                                                
199800                                                                          
199900                                                                          
200000 ECA1-ANDRAT-ANTAL SECTION.                                               
200100     MOVE 'ECA1-ANDRAT-ANTAL' TO CURRENT-SECTION                          
200200                                                                          
200300     IF INLB-LEV-KVAVIS-BSKKVAR    > 0 AND                                
200400        INLB-LEV-KVAVIS-BSKURS     > 0                                    
200500                                                                          
200600        MOVE MID-KVAVIS(IX) TO INLB-LEV-KVAVIS-BSKKVAR                    
200700                               INLB-LEV-KVAVIS-BSKURS                     
200800     END-IF                                                               
200900     .                                                                    
201000     EJECT                                                                
201100                                                                          
201200                                                                          
201300 ECA2-ANDR-ANT-ANDR-DAG-C1 SECTION.                                       
201400     MOVE 'ECA2-ANDR-ANT-ANDR-DAG-C1'  TO CURRENT-SECTION                 
201500                                                                          
201600     IF INLB-LEV-KVAVIS-BSKKVAR    > 0 AND                                
201700        INLB-LEV-KVAVIS-BSKURS     > 0                                    
201800*       SAMMA LAGER                                                       
201900        PERFORM R1-RAK-INLC1-DISPC1                                       
202000        MOVE MID-KVAVIS(IX) TO INLB-LEV-KVAVIS-BSKKVAR                    
202100                               INLB-LEV-KVAVIS-BSKURS                     
202200     ELSE                                                                 
202300        IF INLB-LEV-KVAVIS-BSKKVAR    = 0 AND                             
202400           INLB-LEV-KVAVIS-BSKURS     = 0                                 
202500*          ANDRAT LAGER                                                   
202600           PERFORM R1-RAK-INLC1-DISPC1                                    
202700           MOVE MID-KVAVIS(IX) TO INLB-LEV-KVAVIS-BSKKVAR                 
202800                                  INLB-LEV-KVAVIS-BSKURS                  
202900        END-IF                                                            
203000     END-IF                                                               
203100     .                                                                    
203200     EJECT                                                                
203300                                                                          
203400                                                                          
203500 ECA4-ANDRAD-DAG-C1 SECTION.                                              
203600     MOVE 'ECA4-ANDRAD-DAG-C1 '  TO CURRENT-SECTION                       
203700                                                                          
203800     IF INLB-LEV-KVAVIS-BSKKVAR    > 0 AND                                
203900        INLB-LEV-KVAVIS-BSKURS     > 0                                    
204000*       SAMMA LAGER                                                       
204100        PERFORM R1-RAK-INLC1-DISPC1                                       
204200     ELSE                                                                 
204300        IF INLB-LEV-KVAVIS-BSKKVAR    = 0 AND                             
204400           INLB-LEV-KVAVIS-BSKURS     = 0                                 
204500*ÄNDRAT LAGER                                                             
204600           PERFORM R1-RAK-INLC1-DISPC1                                    
204700        END-IF                                                            
204800     END-IF                                                               
204900     .                                                                    
205000     EJECT                                                                
205100                                                                          
205200                                                                          
205300 ECB-NYCKEL-TILEVBSK-ANDRAD SECTION.                                      
205400     MOVE 'ECB-NYCKEL-TILEVBSK-ANDRAD'  TO CURRENT-SECTION                
205500                                                                          
205600         MOVE INLB-WLINLB24  TO INLBA-WLINLB24                            
205700                                                                          
205800         PERFORM ECBA-BERAEKNA-VECKOR                                     
205900*        MOVE NEJ TO INLBA-LEV-FLSENLEV                                   
206000*                    INLBA-LEV-FLFORAVI                                   
206100*?*      PERFORM S10-SKAPA-EV-TIDISPIN-TRANS                              
206200         PERFORM IMS-ISRT-LEVBESK-INLBA                                   
206300***      ¤ ISRT-WDD901-902-924                                            
206400                                                                          
206500         IF SEGMENT-FINNS-REDAN                                           
206600            MOVE FEL-21 TO MOD-TEMFSFEL                                   
206700            MOVE FEL-21 TO MOD-TEMFSINF                                   
206800***         MEDD  SEGMENT FINNS REDAN                                     
206900         ELSE                                                             
207000            PERFORM S10-SKAPA-EV-TIDISPIN-TRANS                           
207100            PERFORM IMS-DLET-INLB                                         
207200****        ¤ DLET-WDD924                                                 
207300         END-IF                                                           
207400     .                                                                    
207500     EJECT                                                                
207600                                                                          
207700                                                                          
207800 ECBA-BERAEKNA-VECKOR       SECTION.                                      
207900     MOVE 'ECBA-BERAEKNA-VECKOR '  TO CURRENT-SECTION                     
208000                                                                          
208100     MOVE TILEVBSK-RAD-AAMMDD-WS(IX) TO WS-DALEVBSK-AAMMDD                
208200     IF WS-DALEVBSK-AAMMDD > 500000                                       
208300        MOVE 19                      TO WS-DALEVBSK-SS                    
208400     ELSE                                                                 
208500        MOVE 20                      TO WS-DALEVBSK-SS                    
208600     END-IF                                                               
208700     MOVE WS-DALEVBSK-AVS            TO INLBA-LEV-DALEVBSK-AVS            
208800***  MOVE ZERO                       TO INLBA-LEV-TILEVBSK-INL            
208900***                                     INLBA-LEV-TILEVBSK-DISP           
209000***                                     INLBA-LEV-KVAVIS-BSKURS           
209100***                                     INLBA-LEV-KVAVIS-BSKKVAR          
209200                                                                          
209300     IF MID-TILEVBSK-INL-C1(IX) = ALL '+'                                 
209400        PERFORM R3A-RAK-MASKINELLT                                        
209500     ELSE                                                                 
209600        PERFORM R1A-RAK-INLC1-DISPC1                                      
209700        IF MID-KVAVIS (IX) > ZERO                                         
209800           MOVE MID-KVAVIS(IX)       TO INLBA-LEV-KVAVIS-BSKURS           
209900                                        INLBA-LEV-KVAVIS-BSKKVAR          
210000        END-IF                                                            
210100     END-IF                                                               
210200                                                                          
210300     MOVE DAGENS-DATUM             TO INLBA-LEV-TIREGDAT                  
210400     MOVE DAGENS-HHMMSS            TO INLBA-LEV-TIREGTID                  
210500     .                                                                    
210600     EJECT                                                                
210700                                                                          
210800                                                                          
210900 ED-TA-HAND-OM-INFOTEXTERNA  SECTION.                                     
211000     MOVE 'ED-TA-HAND-OM-INFOTEXTERNA'  TO CURRENT-SECTION                
211100     SKIP2                                                                
211200     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
211300     PERFORM EDA-EXTERNTEXTER                                             
211400     MOVE MID-DALEVBSK-FOERSTA-DOLD  TO W-DALEVBSK                        
211500     MOVE MID-IDLEVNR-FOERSTA-DOLD   TO W-IDLEVNR                         
211600     PERFORM MFS-SLAECK-MOD-PA-TEXTRAD                                    
211700     .                                                                    
211800     EJECT                                                                
211900                                                                          
212000                                                                          
212100 EDA-EXTERNTEXTER SECTION.                                                
212200     MOVE 'EDA-EXTERNTEXTER '   TO CURRENT-SECTION                        
212300                                                                          
212400     IF  MID-TELEVBSK-EXT = ALL '+'                                       
212500     AND MID-TIBORT       = ALL '+'                                       
212600        CONTINUE                                                          
212700     ELSE                                                                 
212800        MOVE +2 TO W-IDLEVBSK                                             
212900        PERFORM IMS-GET-INFO-INLB-KVAL                                    
213000****    ¤ GHU-WDD925                                                      
213100        IF SEGMENT-FINNS                                                  
213200           IF MID-TELEVBSK-EXT = ALL '+'                                  
213300              MOVE TIBORT-WS     TO INLB-INFO-TIBORT                      
213400*¤¤¤          MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                        
213500*¤¤¤                                    MOD-TELEVBSK-EXT                  
213600              PERFORM IMS-REPL-INLB                                       
213700****          ¤ REPL-WDD925                                               
213800           ELSE                                                           
213900              IF MID-TELEVBSK-EXT = SPACE                                 
214000                 PERFORM IMS-DLET-INLB                                    
214100****             ¤ DLET-'WDD925'                                          
214200                 MOVE +4 TO W-IDLEVBSK                                    
214300                 PERFORM IMS-GET-INFO-INLB-KVAL                           
214400                 IF SEGMENT-FINNS                                         
214500                    PERFORM IMS-DLET-INLB                                 
214600                 END-IF                                                   
214700                 MOVE +5 TO W-IDLEVBSK                                    
214800                 PERFORM IMS-GET-INFO-INLB-KVAL                           
214900                 IF SEGMENT-FINNS                                         
215000                    PERFORM IMS-DLET-INLB                                 
215100                 END-IF                                                   
215200                 MOVE +6 TO W-IDLEVBSK                                    
215300                 PERFORM IMS-GET-INFO-INLB-KVAL                           
215400                 IF SEGMENT-FINNS                                         
215500                    PERFORM IMS-DLET-INLB                                 
215600                 END-IF                                                   
215700                 MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT                 
215800                                         MOD-TELEVBSK-EXT2                
215900                                         MOD-TELEVBSK-EXT3                
216000                                         MOD-TELEVBSK-EXT4                
216100                                         MOD-TIBORT                       
216200              ELSE                                                        
216300                 MOVE MID-TELEVBSK-EXT  TO INLB-INFO-TELEVBSK             
216400                 MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT             
216500                 MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID             
216600                 MOVE TIBORT-WS         TO INLB-INFO-TIBORT               
216700*¤¤¤             MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT               
216800*¤¤¤                                       MOD-TIBORT                     
216900                 PERFORM IMS-REPL-INLB                                    
217000****             ¤ REPL-'WDD925'                                          
217100              END-IF                                                      
217200           END-IF                                                         
217300        ELSE                                                              
217400           IF MID-TELEVBSK-EXT = ALL '+' OR SPACE                         
217500              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT                    
217600                                      MOD-TIBORT                          
217700           ELSE                                                           
217800              MOVE 2 TO INLB-INFO-IDLEVBSK                                
217900              MOVE MID-TELEVBSK-EXT  TO INLB-INFO-TELEVBSK                
218000              MOVE TIBORT-WS         TO INLB-INFO-TIBORT                  
218100              MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT                
218200              MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID                
218300              PERFORM IMS-ISRT-INFO-INLB                                  
218400****          ¤ ISRT-WDD901-902-925                                       
218500              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT                  
218600                                        MOD-TIBORT                        
218700           END-IF                                                         
218800        END-IF                                                            
218900     END-IF                                                               
219000                                                                          
219100     IF  MID-TELEVBSK-EXT2 = ALL '+'                                      
219200     AND MID-TIBORT        = ALL '+'                                      
219300        CONTINUE                                                          
219400     ELSE                                                                 
219500        MOVE +4 TO W-IDLEVBSK                                             
219600        PERFORM IMS-GET-INFO-INLB-KVAL                                    
219700****    ¤ GHU-WDD925                                                      
219800        IF SEGMENT-FINNS                                                  
219900           IF MID-TELEVBSK-EXT2 = ALL '+'                                 
220000              MOVE TIBORT-WS     TO INLB-INFO-TIBORT                      
220100*¤¤¤          MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                        
220200*¤¤¤                                    MOD-TELEVBSK-EXT2                 
220300              PERFORM IMS-REPL-INLB                                       
220400****          ¤ REPL-WDD925                                               
220500           ELSE                                                           
220600              IF MID-TELEVBSK-EXT2 = SPACE                                
220700                 PERFORM IMS-DLET-INLB                                    
220800****             ¤ DLET-'WDD925'                                          
220900                 MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT2                
221000*?*                                      MOD-TIBORT                       
221100              ELSE                                                        
221200                 MOVE MID-TELEVBSK-EXT2 TO INLB-INFO-TELEVBSK             
221300                 MOVE TIBORT-WS         TO INLB-INFO-TIBORT               
221400                 MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT             
221500                 MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID             
221600*¤¤¤             MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT2              
221700*¤¤¤                                       MOD-TIBORT                     
221800                 PERFORM IMS-REPL-INLB                                    
221900****             ¤ REPL-'WDD925'                                          
222000              END-IF                                                      
222100           END-IF                                                         
222200        ELSE                                                              
222300           IF MID-TELEVBSK-EXT2 = ALL '+' OR SPACE                        
222400              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT2                   
222500*?*                                   MOD-TIBORT                          
222600           ELSE                                                           
222700              MOVE 4 TO INLB-INFO-IDLEVBSK                                
222800              MOVE MID-TELEVBSK-EXT2 TO INLB-INFO-TELEVBSK                
222900              MOVE TIBORT-WS         TO INLB-INFO-TIBORT                  
223000              MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT                
223100              MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID                
223200              PERFORM IMS-ISRT-INFO-INLB                                  
223300****          ¤ ISRT-WDD901-902-925                                       
223400              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT2                 
223500                                        MOD-TIBORT                        
223600           END-IF                                                         
223700        END-IF                                                            
223800     END-IF                                                               
223900                                                                          
224000     IF  MID-TELEVBSK-EXT3 = ALL '+'                                      
224100     AND MID-TIBORT        = ALL '+'                                      
224200        CONTINUE                                                          
224300     ELSE                                                                 
224400        MOVE +5 TO W-IDLEVBSK                                             
224500        PERFORM IMS-GET-INFO-INLB-KVAL                                    
224600****    ¤ GHU-WDD925                                                      
224700        IF SEGMENT-FINNS                                                  
224800           IF MID-TELEVBSK-EXT3 = ALL '+'                                 
224900              MOVE TIBORT-WS         TO INLB-INFO-TIBORT                  
225000*¤¤¤          MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                        
225100*¤¤¤                                    MOD-TELEVBSK-EXT3                 
225200              PERFORM IMS-REPL-INLB                                       
225300****          ¤ REPL-WDD925                                               
225400           ELSE                                                           
225500              IF MID-TELEVBSK-EXT3 = SPACE                                
225600                 PERFORM IMS-DLET-INLB                                    
225700****             ¤ DLET-'WDD925'                                          
225800                 MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT3                
225900*?*                                      MOD-TIBORT                       
226000              ELSE                                                        
226100                 MOVE MID-TELEVBSK-EXT3 TO INLB-INFO-TELEVBSK             
226200                 MOVE TIBORT-WS         TO INLB-INFO-TIBORT               
226300                 MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT             
226400                 MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID             
226500*¤¤¤             MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT3              
226600*¤¤¤                                       MOD-TIBORT                     
226700                 PERFORM IMS-REPL-INLB                                    
226800****             ¤ REPL-'WDD925'                                          
226900              END-IF                                                      
227000           END-IF                                                         
227100        ELSE                                                              
227200           IF MID-TELEVBSK-EXT3 = ALL '+' OR SPACE                        
227300              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT3                   
227400*?*                                   MOD-TIBORT                          
227500           ELSE                                                           
227600              MOVE 5 TO INLB-INFO-IDLEVBSK                                
227700              MOVE MID-TELEVBSK-EXT3 TO INLB-INFO-TELEVBSK                
227800              MOVE TIBORT-WS         TO INLB-INFO-TIBORT                  
227900              MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT                
228000              MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID                
228100              PERFORM IMS-ISRT-INFO-INLB                                  
228200****          ¤ ISRT-WDD901-902-925                                       
228300              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT3                 
228400                                        MOD-TIBORT                        
228500           END-IF                                                         
228600        END-IF                                                            
228700     END-IF                                                               
228800                                                                          
228900     IF  MID-TELEVBSK-EXT4 = ALL '+'                                      
229000     AND MID-TIBORT        = ALL '+'                                      
229100        CONTINUE                                                          
229200     ELSE                                                                 
229300        MOVE +6 TO W-IDLEVBSK                                             
229400        PERFORM IMS-GET-INFO-INLB-KVAL                                    
229500****    ¤ GHU-WDD925                                                      
229600        IF SEGMENT-FINNS                                                  
229700           IF MID-TELEVBSK-EXT4 = ALL '+'                                 
229800              MOVE TIBORT-WS         TO INLB-INFO-TIBORT                  
229900*¤¤¤          MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                        
230000*¤¤¤                                    MOD-TELEVBSK-EXT4                 
230100              PERFORM IMS-REPL-INLB                                       
230200****          ¤ REPL-WDD925                                               
230300           ELSE                                                           
230400              IF MID-TELEVBSK-EXT4 = SPACE                                
230500                 PERFORM IMS-DLET-INLB                                    
230600****             ¤ DLET-'WDD925'                                          
230700                 MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT4                
230800*?*                                      MOD-TIBORT                       
230900              ELSE                                                        
231000                 MOVE MID-TELEVBSK-EXT4 TO INLB-INFO-TELEVBSK             
231100                 MOVE TIBORT-WS         TO INLB-INFO-TIBORT               
231200                 MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT             
231300                 MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID             
231400*¤¤¤             MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT4              
231500*¤¤¤                                       MOD-TIBORT                     
231600                 PERFORM IMS-REPL-INLB                                    
231700****             ¤ REPL-'WDD925'                                          
231800              END-IF                                                      
231900           END-IF                                                         
232000        ELSE                                                              
232100           IF MID-TELEVBSK-EXT4 = ALL '+' OR SPACE                        
232200              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT4                   
232300*?*                                   MOD-TIBORT                          
232400           ELSE                                                           
232500              MOVE 6 TO INLB-INFO-IDLEVBSK                                
232600              MOVE MID-TELEVBSK-EXT4 TO INLB-INFO-TELEVBSK                
232700              MOVE TIBORT-WS         TO INLB-INFO-TIBORT                  
232800              MOVE DAGENS-DATUM      TO INLB-INFO-TIREGDAT                
232900              MOVE DAGENS-HHMMSS     TO INLB-INFO-TIREGTID                
233000              PERFORM IMS-ISRT-INFO-INLB                                  
233100****          ¤ ISRT-WDD901-902-925                                       
233200              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT4                 
233300                                        MOD-TIBORT                        
233400           END-IF                                                         
233500        END-IF                                                            
233600     END-IF                                                               
233700     .                                                                    
233800     EJECT                                                                
233900                                                                          
234000                                                                          
234100 EE-VISA-BILD SECTION.                                                    
234200     MOVE 'EE-VISA-BILD '  TO CURRENT-SECTION                             
234300                                                                          
234400     IF GAMLA-PLANER                                                      
234500        MOVE MID-DALEVBSK-FOERSTA-DOLD     TO W-DALEVBSK                  
234600     ELSE                                                                 
234700        IF NY-PLAN                                                        
234800           MOVE TILEVBSK-AVS-AAMMDD-WS (9) TO WS-DALEVBSK-AAMMDD          
234900           IF WS-DALEVBSK-AAMMDD > 500000                                 
235000              MOVE 19                      TO WS-DALEVBSK-SS              
235100           ELSE                                                           
235200              MOVE 20                      TO WS-DALEVBSK-SS              
235300           END-IF                                                         
235400           MOVE WS-DALEVBSK-AVS            TO W-DALEVBSK                  
235500        END-IF                                                            
235600     END-IF                                                               
235700        MOVE IDLEVNR-WS                    TO W-IDLEVNR                   
235800     PERFORM IMS-GET-ART-INLB                                             
235900     PERFORM IMS-GET-LEVBESK-2OKVAL-INLB                                  
236000****    ¤ GNP WDD924                                                      
236100     IF SEGMENT-FINNS                                                     
236200        MOVE KFBA-IDLEVNR                  TO IDLEVNR-AKT                 
236300        PERFORM L1-LISTA-LEVBESK                                          
236400        PERFORM MFS-RENSA-INFAELT-NYRAD                                   
236500     ELSE                                                                 
236600        PERFORM MFS-TOM-SIDA                                              
236700     END-IF                                                               
236800     MOVE IDLEVNR-WS TO W-IDLEVNR                                         
236900     PERFORM S2-LAES-OCH-BEH-EXTINFO                                      
237000     PERFORM S4-LAES-OCH-BEH-EXTINFO2                                     
237100     PERFORM S5-LAES-OCH-BEH-EXTINFO3                                     
237200     PERFORM S6-LAES-OCH-BEH-EXTINFO4                                     
237300     .                                                                    
237400     EJECT                                                                
237500                                                                          
237600 F-VISA-EFTER-INDATAFEL SECTION.                                          
237700     MOVE 'F-VISA-EFTER-INDATAFEL '  TO CURRENT-SECTION                   
237800     SKIP2                                                                
237900     MOVE MID-DALEVBSK-FOERSTA-DOLD TO                                    
238000                           MOD-DALEVBSK-FOERSTA-DOLD                      
238100     MOVE MID-IDLEVNR-FOERSTA-DOLD  TO                                    
238200                           MOD-IDLEVNR-FOERSTA-DOLD                       
238300     MOVE MID-DALEVBSK-NAESTA-DOLD TO                                     
238400                           MOD-DALEVBSK-NAESTA-DOLD                       
238500     MOVE MID-IDLEVNR-NAESTA-DOLD  TO                                     
238600                           MOD-IDLEVNR-NAESTA-DOLD                        
238700     IF INLB-ROT-FINNS AND INLB-LEV-FINNS                                 
238800*       MOVE MID-DALEVBSK-FOERSTA-DOLD TO W-DALEVBSK                      
238900*       MOVE MID-IDLEVNR-FOERSTA-DOLD  TO W-IDLEVNR                       
239000        PERFORM IMS-GET-LEV-INLB                                          
239100****    ¤ GU WDD901-902                                                   
239200*       PERFORM IMS-GET-LEVBESK-GE-INLB                                   
239300****    ¤ GNP >=WDD924                                                    
239400        PERFORM MFS-ROER-EJ-UTFALT-PA-LISTRAD                             
239500*       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                              
239600        IF  MID-TELEVBSK-EXT = ALL '+'                                    
239700        AND MID-TIBORT       = ALL '+'                                    
239800           PERFORM S2-LAES-OCH-BEH-EXTINFO                                
239900        ELSE                                                              
240000           IF MID-TELEVBSK-EXT = ALL '+'                                  
240100              MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT                  
240200           ELSE                                                           
240300              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT                  
240400           END-IF                                                         
240500           IF MID-TIBORT       = ALL '+'                                  
240600              MOVE MFS-RENSA-FAELT   TO MOD-TIBORT                        
240700           ELSE                                                           
240800              MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                        
240900           END-IF                                                         
241000        END-IF                                                            
241100        IF  MID-TELEVBSK-EXT2 = ALL '+'                                   
241200        AND MID-TIBORT        = ALL '+'                                   
241300           PERFORM S4-LAES-OCH-BEH-EXTINFO2                               
241400        ELSE                                                              
241500           IF MID-TELEVBSK-EXT2 = ALL '+'                                 
241600              MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT2                 
241700           ELSE                                                           
241800              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT2                 
241900           END-IF                                                         
242000        END-IF                                                            
242100        IF  MID-TELEVBSK-EXT3 = ALL '+'                                   
242200        AND MID-TIBORT        = ALL '+'                                   
242300           PERFORM S5-LAES-OCH-BEH-EXTINFO3                               
242400        ELSE                                                              
242500           IF MID-TELEVBSK-EXT3 = ALL '+'                                 
242600              MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT3                 
242700           ELSE                                                           
242800              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT3                 
242900           END-IF                                                         
243000        END-IF                                                            
243100        IF  MID-TELEVBSK-EXT4 = ALL '+'                                   
243200        AND MID-TIBORT        = ALL '+'                                   
243300           PERFORM S6-LAES-OCH-BEH-EXTINFO4                               
243400        ELSE                                                              
243500           IF MID-TELEVBSK-EXT4 = ALL '+'                                 
243600              MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT4                 
243700           ELSE                                                           
243800              MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT4                 
243900           END-IF                                                         
244000        END-IF                                                            
244100     ELSE                                                                 
244200        MOVE +1 TO IX                                                     
244300        PERFORM UNTIL IX > 8                                              
244400          PERFORM MFS-RENSA-UTFALT-PA-LISTRAD                             
244500        ADD +1 TO IX                                                      
244600        END-PERFORM                                                       
244700                                                                          
244800        IF MID-TELEVBSK-EXT = ALL '+'                                     
244900           MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT                     
245000        ELSE                                                              
245100           MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT                     
245200        END-IF                                                            
245300        IF MID-TELEVBSK-EXT2 = ALL '+'                                    
245400           MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT2                    
245500        ELSE                                                              
245600           MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT2                    
245700        END-IF                                                            
245800        IF MID-TIBORT       = ALL '+'                                     
245900           MOVE MFS-RENSA-FAELT   TO MOD-TIBORT                           
246000        ELSE                                                              
246100           MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                           
246200        END-IF                                                            
246300        IF MID-TELEVBSK-EXT3 = ALL '+'                                    
246400           MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT3                    
246500        ELSE                                                              
246600           MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT3                    
246700        END-IF                                                            
246800        IF MID-TELEVBSK-EXT4 = ALL '+'                                    
246900           MOVE MFS-RENSA-FAELT   TO MOD-TELEVBSK-EXT4                    
247000        ELSE                                                              
247100           MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT4                    
247200        END-IF                                                            
247300     END-IF                                                               
247400*    MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
247500     .                                                                    
247600     EJECT                                                                
247700                                                                          
247800 J-BEHANDLA-ENTER-OCH-INDATA SECTION.                                     
247900     MOVE 'J-BEHANDLA-ENTER-OCH-INDATA'  TO CURRENT-SECTION               
248000                                                                          
248100                                                                          
248200     IF MID-IDARTNR-IN = ALL '+'                                          
248300        IF EGEN-BILD                                                      
248400           MOVE ZERO TO TOMRAD-RAK                                        
248500           MOVE +1 TO IX                                                  
248600           PERFORM UNTIL IX > 9                                           
248700              IF IX < 9                                                   
248800                 IF  MID-KDCMD (IX)           = ALL '+'                   
248900                 AND MID-TILEVBSK-RAD   (IX)  = ALL '+'                   
249000                 AND MID-KVAVIS (IX)          = ALL '+'                   
249100                 AND MID-TILEVBSK-INL-C1(IX)  = ALL '+'                   
249200                     ADD +1 TO TOMRAD-RAK                                 
249300                 END-IF                                                   
249400              ELSE                                                        
249500                 IF  MID-TILEVBSK-AVS (IX)    = ALL '+'                   
249600                 AND MID-KVAVIS (IX)          = ALL '+'                   
249700                 AND MID-TILEVBSK-INL-C1(IX)  = ALL '+'                   
249800                 AND MID-IDLEVNR-RAD    (IX)  = ALL '+'                   
249900                     ADD +1 TO TOMRAD-RAK                                 
250000                 END-IF                                                   
250100             END-IF                                                       
250200             ADD +1 TO IX                                                 
250300           END-PERFORM                                                    
250400           IF  TOMRAD-RAK = 9                                             
250500           AND MID-TELEVBSK-EXT      = ALL '+'                            
250600           AND MID-TELEVBSK-EXT2     = ALL '+'                            
250700           AND MID-TELEVBSK-EXT3     = ALL '+'                            
250800           AND MID-TELEVBSK-EXT4     = ALL '+'                            
250900           AND MID-TIBORT            = ALL '+'                            
251000               CONTINUE                                                   
251100           ELSE                                                           
251200               MOVE NEJ TO ENTER-OCH-INGET-INDATA-SW                      
251300               MOVE FEL-2 TO MOD-TEMFSFEL                                 
251400*¤¤¤¤¤¤¤¤¤¤¤¤¤ FÖR UPPDATERING TRYCK PF11                                 
251500               PERFORM MFS-ROER-EJ-UTFALT-PA-LISTRAD                      
251600               PERFORM MFS-SPARA-INMATNINGSRADER                          
251700               MOVE MID-DALEVBSK-FOERSTA-DOLD TO                          
251800                                     MOD-DALEVBSK-FOERSTA-DOLD            
251900               MOVE MID-IDLEVNR-FOERSTA-DOLD  TO                          
252000                                     MOD-IDLEVNR-FOERSTA-DOLD             
252100               MOVE MID-DALEVBSK-NAESTA-DOLD TO                           
252200                                     MOD-DALEVBSK-NAESTA-DOLD             
252300               MOVE MID-IDLEVNR-NAESTA-DOLD  TO                           
252400                                     MOD-IDLEVNR-NAESTA-DOLD              
252500           END-IF                                                         
252600        END-IF                                                            
252700     END-IF                                                               
252800     .                                                                    
252900     EJECT                                                                
253000                                                                          
253100 G-VISA-FOERSTA-SIDAN SECTION.                                            
253200     MOVE 'G-VISA-FOERSTA-SIDAN '  TO CURRENT-SECTION                     
253300     SKIP2                                                                
253400     IF INLB-ROT-FINNS AND INLB-LEV-FINNS                                 
253500        PERFORM IMS-GET-ART-INLB                                          
253600        PERFORM IMS-GET-LEVBESK-2OKVAL-INLB                               
253700****    ¤ GNP WDD924                                                      
253800        IF SEGMENT-FINNS                                                  
253900           MOVE KFBA-IDLEVNR  TO IDLEVNR-AKT                              
254000           PERFORM L1-LISTA-LEVBESK                                       
254100           PERFORM MFS-RENSA-INFAELT-NYRAD                                
254200        ELSE                                                              
254300           MOVE FEL-12 TO MOD-TEMFSFEL                                    
254400*¤¤¤¤¤     LEVERANSBESKED FINNS EJ PÅ LEVERANTÖREN                        
254500           PERFORM MFS-TOM-SIDA                                           
254600        END-IF                                                            
254700        PERFORM IMS-GET-ART-INLB                                          
254800        IF SEGMENT-FINNS                                                  
254900           PERFORM IMS-GET-LEV-OKVAL-INLB                                 
255000           IF SEGMENT-FINNS                                               
255100              IF INLB-IDLEVNR = W-IDLEVNR                                 
255200                 PERFORM IMS-GET-LEV-NEXT-INLB                            
255300                 IF SEGMENT-FINNS                                         
255400                    MOVE FEL-17 TO MOD-TEMFSINF                           
255500*¤¤¤                ANNAN LEVERANTÖR FINNS                                
255600                 END-IF                                                   
255700              ELSE                                                        
255800                 MOVE FEL-17 TO MOD-TEMFSINF                              
255900*¤¤¤             ANNAN LEVERANTÖR FINNS                                   
256000              END-IF                                                      
256100           ELSE                                                           
256200              PERFORM MFS-TOM-SIDA                                        
256300           END-IF                                                         
256400        ELSE                                                              
256500           PERFORM MFS-TOM-SIDA                                           
256600        END-IF                                                            
256700        MOVE IDLEVNR-WS TO W-IDLEVNR                                      
256800        PERFORM S2-LAES-OCH-BEH-EXTINFO                                   
256900        PERFORM S4-LAES-OCH-BEH-EXTINFO2                                  
257000        PERFORM S5-LAES-OCH-BEH-EXTINFO3                                  
257100        PERFORM S6-LAES-OCH-BEH-EXTINFO4                                  
257200     ELSE                                                                 
257300        PERFORM MFS-TOM-SIDA                                              
257400     END-IF                                                               
257500     .                                                                    
257600     EJECT                                                                
257700                                                                          
257800 H-VISA-NAESTA-SIDA SECTION.                                              
257900     MOVE 'H-VISA-NAESTA-SIDA '  TO CURRENT-SECTION                       
258000     SKIP2                                                                
258100     IF INLB-ROT-FINNS AND INLB-LEV-FINNS                                 
258200        PERFORM IMS-GET-ART-INLB                                          
258300        IF MID-DALEVBSK-NAESTA-DOLD = 0                                   
258400           PERFORM IMS-GET-LEVBESK-2OKVAL-INLB                            
258500****       ¤ GNP WDD924                                                   
258600        ELSE                                                              
258700           MOVE MID-DALEVBSK-NAESTA-DOLD TO W-DALEVBSK                    
258800           MOVE MID-IDLEVNR-NAESTA-DOLD  TO W-IDLEVNR                     
258900           PERFORM IMS-GET-LEVBESK-GE-GE-INLB                             
259000****       ¤ GNP >=WDD924                                                 
259100        END-IF                                                            
259200        IF SEGMENT-FINNS                                                  
259300           MOVE KFBA-IDLEVNR      TO IDLEVNR-AKT                          
259400           PERFORM L1-LISTA-LEVBESK                                       
259500           MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                           
259600                                     MOD-TELEVBSK-EXT                     
259700                                     MOD-TELEVBSK-EXT2                    
259800                                     MOD-TELEVBSK-EXT3                    
259900                                     MOD-TELEVBSK-EXT4                    
260000           PERFORM MFS-RENSA-INFAELT-NYRAD                                
260100        ELSE                                                              
260200           MOVE FEL-12 TO MOD-TEMFSFEL                                    
260300*¤¤¤¤¤     LEVERANSBESKED FINNS EJ PÅ LEVERANTÖREN                        
260400           PERFORM MFS-TOM-SIDA                                           
260500        END-IF                                                            
260600        PERFORM IMS-GET-ART-INLB                                          
260700        IF SEGMENT-FINNS                                                  
260800           PERFORM IMS-GET-LEV-OKVAL-INLB                                 
260900           IF SEGMENT-FINNS                                               
261000              IF INLB-IDLEVNR = W-IDLEVNR                                 
261100                 PERFORM IMS-GET-LEV-NEXT-INLB                            
261200                 IF SEGMENT-FINNS                                         
261300                    MOVE FEL-17 TO MOD-TEMFSINF                           
261400*¤¤¤                ANNAN LEVERANTÖR FINNS                                
261500                 END-IF                                                   
261600              ELSE                                                        
261700                 MOVE FEL-17 TO MOD-TEMFSINF                              
261800*¤¤¤             ANNAN LEVERANTÖR FINNS                                   
261900              END-IF                                                      
262000           END-IF                                                         
262100        ELSE                                                              
262200           PERFORM MFS-TOM-SIDA                                           
262300        END-IF                                                            
262400        MOVE IDLEVNR-WS TO W-IDLEVNR                                      
262500        PERFORM S2-LAES-OCH-BEH-EXTINFO                                   
262600        PERFORM S4-LAES-OCH-BEH-EXTINFO2                                  
262700        PERFORM S5-LAES-OCH-BEH-EXTINFO3                                  
262800        PERFORM S6-LAES-OCH-BEH-EXTINFO4                                  
262900     ELSE                                                                 
263000        PERFORM MFS-TOM-SIDA                                              
263100     END-IF                                                               
263200     .                                                                    
263300     EJECT                                                                
263400                                                                          
263500 I-VISA-SAMMA-SIDA SECTION.                                               
263600     MOVE 'I-VISA-SAMMA-SIDA '  TO CURRENT-SECTION                        
263700     SKIP2                                                                
263800*************************************************************             
263900* OBS! SEKTIONEN ANVÄNDS INTE IDAG, UTAN ÄR EN FÖRBEREDELSE *             
264000*      FÖR INFÖRANDE AV NY STANDARD                         *             
264100*************************************************************             
264200     IF INLB-ROT-FINNS AND INLB-LEV-FINNS                                 
264300        PERFORM IMS-GET-ART-INLB                                          
264400        IF MID-DALEVBSK-FOERSTA-DOLD = 0                                  
264500           PERFORM IMS-GET-LEVBESK-2OKVAL-INLB                            
264600****       ¤ GNP WDD924                                                   
264700        ELSE                                                              
264800***********PERFORM IMS-GET-LEV-INLB                                       
264900***********¤ GU WDD901 902                                                
265000           MOVE MID-DALEVBSK-FOERSTA-DOLD TO W-DALEVBSK                   
265100           MOVE MID-IDLEVNR-FOERSTA-DOLD  TO W-IDLEVNR                    
265200           PERFORM IMS-GET-LEVBESK-GE-GE-INLB                             
265300****       ¤ GNP >=WDD924                                                 
265400        END-IF                                                            
265500        IF SEGMENT-FINNS                                                  
265600           MOVE KFBA-IDLEVNR      TO IDLEVNR-AKT                          
265700           PERFORM L1-LISTA-LEVBESK                                       
265800           MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT                     
265900                                     MOD-TELEVBSK-EXT2                    
266000                                     MOD-TELEVBSK-EXT3                    
266100                                     MOD-TELEVBSK-EXT4                    
266200                                     MOD-TIBORT                           
266300           PERFORM MFS-RENSA-INFAELT-NYRAD                                
266400        ELSE                                                              
266500           PERFORM MFS-TOM-SIDA                                           
266600           PERFORM MFS-RENSA-INFAELT-NYRAD                                
266700        END-IF                                                            
266800     ELSE                                                                 
266900        PERFORM MFS-TOM-SIDA                                              
267000        PERFORM MFS-RENSA-INFAELT-NYRAD                                   
267100        MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                              
267200                                MOD-TEMFSINF                              
267300     END-IF                                                               
267400     .                                                                    
267500     EJECT                                                                
267600                                                                          
267700 L1-LISTA-LEVBESK SECTION.                                                
267800     MOVE 'L1-LISTA-LEVBESK '  TO CURRENT-SECTION                         
267900     SKIP2                                                                
268000     IF SEGMENT-FINNS                                                     
268100        MOVE INLB-LEV-DALEVBSK-AVS TO                                     
268200                  MOD-DALEVBSK-FOERSTA-DOLD                               
268300        MOVE IDLEVNR-AKT           TO                                     
268400                  MOD-IDLEVNR-FOERSTA-DOLD                                
268500        MOVE +1 TO IX                                                     
268600        PERFORM UNTIL IX > 8                                              
268700           IF SEGMENT-FINNS                                               
268800                 PERFORM L12-EN-RAD                                       
268900                 PERFORM IMS-GET-LEVBESK-NEXT-INLB                        
269000****             ¤ GNP WDD924                                             
269100                 IF SEGMENT-FINNS                                         
269200                    MOVE KFBA-IDLEVNR TO IDLEVNR-AKT                      
269300                 END-IF                                                   
269400           ELSE                                                           
269500              PERFORM MFS-RENSA-UTFALT-PA-LISTRAD                         
269600              PERFORM MFS-RENSA-INFALT-PA-LISTRAD                         
269700           END-IF                                                         
269800           ADD +1 TO IX                                                   
269900        END-PERFORM                                                       
270000        IF SEGMENT-FINNS                                                  
270100          MOVE INLB-LEV-DALEVBSK-AVS TO                                   
270200                         MOD-DALEVBSK-NAESTA-DOLD                         
270300          MOVE IDLEVNR-AKT           TO                                   
270400                         MOD-IDLEVNR-NAESTA-DOLD                          
270500          MOVE MED-2                 TO MOD-TEMFSINF                      
270600*¤¤       FÖR MER INFORMATION, TRYCK ENTER                                
270700        ELSE                                                              
270800          MOVE ZERO        TO MOD-DALEVBSK-NAESTA-DOLD                    
270900          MOVE SPACE       TO MOD-IDLEVNR-NAESTA-DOLD                     
271000          MOVE MFS-RENSA-FAELT       TO MOD-TEMFSINF                      
271100        END-IF                                                            
271200     END-IF                                                               
271300     .                                                                    
271400     EJECT                                                                
271500                                                                          
271600 L12-EN-RAD SECTION.                                                      
271700     MOVE 'L12-EN-RAD '  TO CURRENT-SECTION                               
271800     SKIP2                                                                
271900     MOVE INLB-LEV-DALEVBSK-AVS TO WS-DALEVBSK-AVS                        
272000     MOVE WS-DALEVBSK-AAMMDD    TO DAT-I-TIDATUM                          
272100     PERFORM S22-DATUMKONV-TILL-AAVVD                                     
272200     IF DAT-KDSVAR-OK                                                     
272300        MOVE DAT-TIAAVVD TO MOD-TILEVBSK-AVS-UT(IX)                       
272400     ELSE                                                                 
272500        MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-AVS-UT(IX)                   
272600     END-IF                                                               
272700     IF INLB-LEV-KVAVIS-BSKKVAR    > 0                                    
272800        MOVE INLB-LEV-KVAVIS-BSKKVAR    TO MOD-KVAVIS-UT(IX)              
272900     END-IF                                                               
273000     IF INLB-LEV-TILEVBSK-INL    = 0                                      
273100        MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-INL-C1-UT(IX)                
273200     ELSE                                                                 
273300        MOVE INLB-LEV-TILEVBSK-INL    TO DAT-I-TIDATUM                    
273400        PERFORM S22-DATUMKONV-TILL-AAVVD                                  
273500        IF DAT-KDSVAR-OK                                                  
273600           MOVE DAT-TIAAVVD TO MOD-TILEVBSK-INL-C1-UT(IX)                 
273700        ELSE                                                              
273800           MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-INL-C1-UT(IX)             
273900        END-IF                                                            
274000     END-IF                                                               
274100     IF INLB-LEV-TILEVBSK-DISP    = 0                                     
274200        MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-DISP-C1-UT(IX)               
274300     ELSE                                                                 
274400        MOVE INLB-LEV-TILEVBSK-DISP    TO DAT-I-TIDATUM                   
274500        PERFORM S22-DATUMKONV-TILL-AAVVD                                  
274600        IF DAT-KDSVAR-OK                                                  
274700           MOVE DAT-TIAAVVD TO MOD-TILEVBSK-DISP-C1-UT(IX)                
274800        ELSE                                                              
274900           MOVE MFS-RENSA-FAELT TO MOD-TILEVBSK-DISP-C1-UT(IX)            
275000        END-IF                                                            
275100     END-IF                                                               
275200     MOVE INLB-LEV-FLFORAVI    TO MOD-FLFORAVI-C1-UT(IX)                  
275300                                                                          
275400     MOVE IDLEVNR-AKT          TO MOD-IDLEVNR-RAD (IX)                    
275500*                                                                         
275600     IF IX < 9                                                            
275700        PERFORM MFS-OEPPNA-LISTRAD                                        
275800        PERFORM MFS-RENSA-INFALT-PA-LISTRAD                               
275900     END-IF                                                               
276000     .                                                                    
276100     EJECT                                                                
276200                                                                          
276300 R1-RAK-INLC1-DISPC1   SECTION.                                           
276400     MOVE 'R1-RAK-INLC1-DISPC1 ' TO CURRENT-SECTION                       
276500     SKIP2                                                                
276600     MOVE 2                    TO WORK-KDCALL                             
276700     MOVE WC-CDC-SE            TO WORK-IDDC                               
276800     MOVE TILEVBSK-INL-C1-AAMMDD-WS(IX) TO                                
276900                                  INLB-LEV-TILEVBSK-INL                   
277000                                  WORK-TIAAMMDD-FOM                       
277100     MOVE KVDAGAR-INLEV        TO WORK-KVWORKD                            
277200     CALL WORKDAY  USING WORK-KDCALL,                                     
277300                         WORK-DATE-AREA,                                  
277400                         WORK-KDSVAR                                      
277500     IF WORK-KDSVAR-OK                                                    
277600        MOVE WORK-TIAAMMDD-TOM TO INLB-LEV-TILEVBSK-DISP                  
277700     END-IF                                                               
277800     .                                                                    
277900     SKIP3                                                                
278000                                                                          
278100 R1A-RAK-INLC1-DISPC1   SECTION.                                          
278200     MOVE 'R1A-RAK-INLC1-DISPC1 '  TO CURRENT-SECTION                     
278300     SKIP2                                                                
278400     MOVE 2                    TO WORK-KDCALL                             
278500     MOVE WC-CDC-SE            TO WORK-IDDC                               
278600     MOVE TILEVBSK-INL-C1-AAMMDD-WS(IX) TO                                
278700                                  INLBA-LEV-TILEVBSK-INL                  
278800                                  WORK-TIAAMMDD-FOM                       
278900     MOVE KVDAGAR-INLEV        TO WORK-KVWORKD                            
279000     CALL WORKDAY  USING WORK-KDCALL,                                     
279100                         WORK-DATE-AREA,                                  
279200                         WORK-KDSVAR                                      
279300     IF WORK-KDSVAR-OK                                                    
279400        MOVE WORK-TIAAMMDD-TOM TO INLBA-LEV-TILEVBSK-DISP                 
279500     END-IF                                                               
279600     .                                                                    
279700     EJECT                                                                
279800                                                                          
279900                                                                          
280000 R3-RAK-MASKINELLT SECTION.                                               
280100     MOVE 'R3-RAK-MASKINELLT '  TO CURRENT-SECTION                        
280200     SKIP2                                                                
280300     PERFORM R4-RAK-AVS-INLC1-DISPC1                                      
280400                                                                          
280500     PERFORM R9-BEHANDLA-ANTAL                                            
280600     .                                                                    
280700     SKIP3                                                                
280800                                                                          
280900                                                                          
281000 R3A-RAK-MASKINELLT SECTION.                                              
281100     MOVE 'R3A-RAK-MASKINELLT '  TO CURRENT-SECTION                       
281200     SKIP2                                                                
281300     PERFORM R4A-RAK-AVS-INLC1-DISPC1                                     
281400                                                                          
281500     PERFORM R9A-BEHANDLA-ANTAL                                           
281600     .                                                                    
281700     EJECT                                                                
281800                                                                          
281900                                                                          
282000 R4-RAK-AVS-INLC1-DISPC1 SECTION.                                         
282100     MOVE 'R4-RAK-AVS-INLC1-DISPC1'  TO CURRENT-SECTION                   
282200     SKIP2                                                                
282300     MOVE 2                       TO WORK-KDCALL                          
282400     MOVE WC-CDC-SE               TO WORK-IDDC                            
282500     MOVE TILEVBSK-AVS-AAMMDD-WS(IX) TO WORK-TIAAMMDD-FOM                 
282600     MOVE KVDAGAR-TTC1            TO WORK-KVWORKD                         
282700     CALL WORKDAY  USING WORK-KDCALL,                                     
282800                         WORK-DATE-AREA,                                  
282900                         WORK-KDSVAR                                      
283000     IF WORK-KDSVAR-OK                                                    
283100        MOVE WORK-TIAAMMDD-TOM    TO INLB-LEV-TILEVBSK-INL                
283200                                     WORK-TIAAMMDD-FOM                    
283300        MOVE KVDAGAR-INLEV        TO WORK-KVWORKD                         
283400        CALL WORKDAY  USING WORK-KDCALL,                                  
283500                            WORK-DATE-AREA,                               
283600                            WORK-KDSVAR                                   
283700        IF WORK-KDSVAR-OK                                                 
283800           MOVE WORK-TIAAMMDD-TOM TO INLB-LEV-TILEVBSK-DISP               
283900        END-IF                                                            
284000     END-IF                                                               
284100     .                                                                    
284200     EJECT                                                                
284300                                                                          
284400                                                                          
284500 R4A-RAK-AVS-INLC1-DISPC1 SECTION.                                        
284600     MOVE 'R4A-RAK-AVS-INLC1-DISPC1' TO CURRENT-SECTION                   
284700     SKIP2                                                                
284800     MOVE 2                       TO WORK-KDCALL                          
284900     MOVE WC-CDC-SE               TO WORK-IDDC                            
285000     MOVE TILEVBSK-RAD-AAMMDD-WS(IX) TO WORK-TIAAMMDD-FOM                 
285100     MOVE KVDAGAR-TTC1            TO WORK-KVWORKD                         
285200     CALL WORKDAY  USING WORK-KDCALL,                                     
285300                         WORK-DATE-AREA,                                  
285400                         WORK-KDSVAR                                      
285500     IF WORK-KDSVAR-OK                                                    
285600        MOVE WORK-TIAAMMDD-TOM    TO INLBA-LEV-TILEVBSK-INL               
285700                                     WORK-TIAAMMDD-FOM                    
285800        MOVE KVDAGAR-INLEV        TO WORK-KVWORKD                         
285900        CALL WORKDAY  USING WORK-KDCALL,                                  
286000                            WORK-DATE-AREA,                               
286100                            WORK-KDSVAR                                   
286200        IF WORK-KDSVAR-OK                                                 
286300           MOVE WORK-TIAAMMDD-TOM TO INLBA-LEV-TILEVBSK-DISP              
286400        END-IF                                                            
286500     END-IF                                                               
286600     .                                                                    
286700     EJECT                                                                
286800                                                                          
286900 R9-BEHANDLA-ANTAL SECTION.                                               
287000     MOVE 'R9-BEHANDLA-ANTAL '  TO CURRENT-SECTION                        
287100                                                                          
287200        IF INLB-LEV-TILEVBSK-INL    > 0                                   
287300           MOVE MID-KVAVIS(IX) TO INLB-LEV-KVAVIS-BSKURS                  
287400                                  INLB-LEV-KVAVIS-BSKKVAR                 
287500        END-IF                                                            
287600     .                                                                    
287700     SKIP3                                                                
287800                                                                          
287900 R9A-BEHANDLA-ANTAL SECTION.                                              
288000     MOVE 'R9A-BEHANDLA-ANTAL '  TO CURRENT-SECTION                       
288100                                                                          
288200     IF INLBA-LEV-TILEVBSK-INL    > 0                                     
288300        IF MID-KVAVIS (IX) > ZERO                                         
288400           MOVE MID-KVAVIS(IX) TO INLBA-LEV-KVAVIS-BSKURS                 
288500                                  INLBA-LEV-KVAVIS-BSKKVAR                
288600        END-IF                                                            
288700     END-IF                                                               
288800     .                                                                    
288900     EJECT                                                                
289000                                                                          
289100 S2-LAES-OCH-BEH-EXTINFO SECTION.                                         
289200     MOVE 'S2-LAES-OCH-BEH-EXTINFO'  TO CURRENT-SECTION                   
289300     SKIP2                                                                
289400     PERFORM IMS-GET-LEV-INLB                                             
289500**** ¤ GU WDD901-902                                                      
289600     IF SEGMENT-FINNS                                                     
289700        MOVE +2 TO W-IDLEVBSK                                             
289800        PERFORM IMS-GET-INFO-INLB                                         
289900******* ¤ GNU WDD925                                                      
290000        IF SEGMENT-FINNS                                                  
290100           MOVE DAGENS-DATUM       TO TMP1-YYMMDD                         
290200           MOVE INLB-INFO-TIBORT   TO TMP2-YYMMDD                         
290300           PERFORM WY2000P1                                               
290400           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
290500              PERFORM IMS-DLET-INLB                                       
290600*******       ¤ DLET WDD925                                               
290700              MOVE +4 TO W-IDLEVBSK                                       
290800              PERFORM IMS-GET-INFO-INLB                                   
290900              IF SEGMENT-FINNS                                            
291000                 PERFORM IMS-DLET-INLB                                    
291100              END-IF                                                      
291200              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT                    
291300                                      MOD-TELEVBSK-EXT2                   
291400                                      MOD-TIBORT                          
291500           ELSE                                                           
291600              MOVE INLB-INFO-TELEVBSK TO MOD-TELEVBSK-EXT                 
291700              MOVE INLB-INFO-TIBORT   TO DAT-I-TIDATUM                    
291800              PERFORM S22-DATUMKONV-TILL-AAVVD                            
291900              IF DAT-KDSVAR-OK                                            
292000                 MOVE DAT-TIAAVVD TO MOD-TIBORT                           
292100              END-IF                                                      
292200           END-IF                                                         
292300        ELSE                                                              
292400           MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT                       
292500                                   MOD-TIBORT                             
292600        END-IF                                                            
292700     END-IF                                                               
292800     .                                                                    
292900     EJECT                                                                
293000                                                                          
293100 S4-LAES-OCH-BEH-EXTINFO2 SECTION.                                        
293200     MOVE 'S4-LAES-OCH-BEH-EXTINFO2'  TO CURRENT-SECTION                  
293300     SKIP2                                                                
293400     PERFORM IMS-GET-LEV-INLB                                             
293500**** ¤ GU WDD901-902                                                      
293600     IF SEGMENT-FINNS                                                     
293700        MOVE +4 TO W-IDLEVBSK                                             
293800        PERFORM IMS-GET-INFO-INLB                                         
293900******* ¤ GNU WDD925                                                      
294000        IF SEGMENT-FINNS                                                  
294100           MOVE DAGENS-DATUM       TO TMP1-YYMMDD                         
294200           MOVE INLB-INFO-TIBORT   TO TMP2-YYMMDD                         
294300           PERFORM WY2000P1                                               
294400           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
294500              PERFORM IMS-DLET-INLB                                       
294600*******       ¤ DLET WDD925                                               
294700              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT2                   
294800              IF MOD-TELEVBSK-EXT <= SPACE                                
294900                 MOVE MFS-RENSA-FAELT TO MOD-TIBORT                       
295000              END-IF                                                      
295100           ELSE                                                           
295200              MOVE INLB-INFO-TELEVBSK TO MOD-TELEVBSK-EXT2                
295300              MOVE INLB-INFO-TIBORT   TO DAT-I-TIDATUM                    
295400              PERFORM S22-DATUMKONV-TILL-AAVVD                            
295500              IF DAT-KDSVAR-OK                                            
295600                 IF MOD-TIBORT NUMERIC AND MOD-TIBORT > ZERO              
295700                    MOVE MOD-TIBORT         TO TMP1-YYWWD                 
295800                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
295900                    PERFORM WY2000P2                                      
296000                    IF TMP1-YYWWD > TMP2-YYWWD                            
296100                       MOVE DAT-TIAAVVD TO MOD-TIBORT                     
296200                    END-IF                                                
296300                 ELSE                                                     
296400                    MOVE DAT-TIAAVVD TO MOD-TIBORT                        
296500                 END-IF                                                   
296600              END-IF                                                      
296700           END-IF                                                         
296800        ELSE                                                              
296900           MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT2                      
297000           IF MOD-TELEVBSK-EXT <= SPACE                                   
297100              MOVE MFS-RENSA-FAELT TO MOD-TIBORT                          
297200           END-IF                                                         
297300        END-IF                                                            
297400     END-IF                                                               
297500     .                                                                    
297600     EJECT                                                                
297700                                                                          
297800 S5-LAES-OCH-BEH-EXTINFO3 SECTION.                                        
297900     MOVE 'S5-LAES-OCH-BEH-EXTINFO3'  TO CURRENT-SECTION                  
298000     SKIP2                                                                
298100     PERFORM IMS-GET-LEV-INLB                                             
298200**** ¤ GU WDD901-902                                                      
298300     IF SEGMENT-FINNS                                                     
298400        MOVE +5 TO W-IDLEVBSK                                             
298500        PERFORM IMS-GET-INFO-INLB                                         
298600******* ¤ GNU WDD925                                                      
298700        IF SEGMENT-FINNS                                                  
298800           MOVE DAGENS-DATUM       TO TMP1-YYMMDD                         
298900           MOVE INLB-INFO-TIBORT   TO TMP2-YYMMDD                         
299000           PERFORM WY2000P1                                               
299100           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
299200              PERFORM IMS-DLET-INLB                                       
299300*******       ¤ DLET WDD925                                               
299400              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT3                   
299500              IF MOD-TELEVBSK-EXT <= SPACE                                
299600                 MOVE MFS-RENSA-FAELT TO MOD-TIBORT                       
299700              END-IF                                                      
299800           ELSE                                                           
299900              MOVE INLB-INFO-TELEVBSK TO MOD-TELEVBSK-EXT3                
300000              MOVE INLB-INFO-TIBORT   TO DAT-I-TIDATUM                    
300100              PERFORM S22-DATUMKONV-TILL-AAVVD                            
300200              IF DAT-KDSVAR-OK                                            
300300                 IF MOD-TIBORT NUMERIC AND MOD-TIBORT > ZERO              
300400                    MOVE MOD-TIBORT         TO TMP1-YYWWD                 
300500                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
300600                    PERFORM WY2000P2                                      
300700                    IF TMP1-YYWWD > TMP2-YYWWD                            
300800                       MOVE DAT-TIAAVVD TO MOD-TIBORT                     
300900                    END-IF                                                
301000                 ELSE                                                     
301100                    MOVE DAT-TIAAVVD TO MOD-TIBORT                        
301200                 END-IF                                                   
301300              END-IF                                                      
301400           END-IF                                                         
301500        ELSE                                                              
301600           MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT3                      
301700           IF MOD-TELEVBSK-EXT <= SPACE                                   
301800              MOVE MFS-RENSA-FAELT TO MOD-TIBORT                          
301900           END-IF                                                         
302000        END-IF                                                            
302100     END-IF                                                               
302200     .                                                                    
302300     EJECT                                                                
302400                                                                          
302500 S6-LAES-OCH-BEH-EXTINFO4 SECTION.                                        
302600     MOVE 'S6-LAES-OCH-BEH-EXTINFO4 '  TO CURRENT-SECTION                 
302700     SKIP2                                                                
302800     PERFORM IMS-GET-LEV-INLB                                             
302900**** ¤ GU WDD901-902                                                      
303000     IF SEGMENT-FINNS                                                     
303100        MOVE +6 TO W-IDLEVBSK                                             
303200        PERFORM IMS-GET-INFO-INLB                                         
303300******* ¤ GNU WDD925                                                      
303400        IF SEGMENT-FINNS                                                  
303500           MOVE DAGENS-DATUM       TO TMP1-YYMMDD                         
303600           MOVE INLB-INFO-TIBORT   TO TMP2-YYMMDD                         
303700           PERFORM WY2000P1                                               
303800           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
303900              PERFORM IMS-DLET-INLB                                       
304000*******       ¤ DLET WDD925                                               
304100              MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT4                   
304200              IF MOD-TELEVBSK-EXT <= SPACE                                
304300                 MOVE MFS-RENSA-FAELT TO MOD-TIBORT                       
304400              END-IF                                                      
304500           ELSE                                                           
304600              MOVE INLB-INFO-TELEVBSK TO MOD-TELEVBSK-EXT4                
304700              MOVE INLB-INFO-TIBORT   TO DAT-I-TIDATUM                    
304800              PERFORM S22-DATUMKONV-TILL-AAVVD                            
304900              IF DAT-KDSVAR-OK                                            
305000                 IF MOD-TIBORT NUMERIC AND MOD-TIBORT > ZERO              
305100                    MOVE MOD-TIBORT         TO TMP1-YYWWD                 
305200                    MOVE DAT-TIAAVVD        TO TMP2-YYWWD                 
305300                    PERFORM WY2000P2                                      
305400                    IF TMP1-YYWWD > TMP2-YYWWD                            
305500                       MOVE DAT-TIAAVVD TO MOD-TIBORT                     
305600                    END-IF                                                
305700                 ELSE                                                     
305800                    MOVE DAT-TIAAVVD TO MOD-TIBORT                        
305900                 END-IF                                                   
306000              END-IF                                                      
306100           END-IF                                                         
306200        ELSE                                                              
306300           MOVE MFS-RENSA-FAELT TO MOD-TELEVBSK-EXT4                      
306400           IF MOD-TELEVBSK-EXT <= SPACE                                   
306500              MOVE MFS-RENSA-FAELT TO MOD-TIBORT                          
306600           END-IF                                                         
306700        END-IF                                                            
306800     END-IF                                                               
306900     .                                                                    
307000     EJECT                                                                
307100                                                                          
307200                                                                          
307300 S10-SKAPA-EV-TIDISPIN-TRANS SECTION.                                     
307400     MOVE 'S10-SKAPA-EV-TIDISPIN-TRANS'  TO CURRENT-SECTION               
307500                                                                          
307600     IF NY-PLAN                                                           
307700        MOVE 9  TO IX                                                     
307800     END-IF                                                               
307900*    IF (GAMMAL-TILEVBSK-DISP-C1 (IX) NOT =                               
308000*                      INLB-LEV-TILEVBSK-DISP   )                         
308100*       AND INLB-LEV-TILEVBSK-DISP    < CLAG-TIDISPIN                     
308200        MOVE W-IDARTNR  TO  2228-IDARTNR                                  
308300        MOVE LOW-VALUE  TO  2228-LOW-VALUE                                
308400        MOVE SPACE      TO  2228-FILLER                                   
308500        PERFORM IMS-ISRT-2228                                             
308600*    END-IF                                                               
308700     .                                                                    
308800     EJECT                                                                
308900                                                                          
309000                                                                          
309100 S21-DATUMKONV-TILL-AAMMDD SECTION.                                       
309200     MOVE 'S21-DATUMKONV-TILL-AAMMDD' TO CURRENT-SECTION                  
309300                                                                          
309400     MOVE 'AAVVD '          TO DAT-KDDATFORM                              
309500     CALL WDATKONV USING DAT-KDDATFORM,                                   
309600                         DAT-I-TIDATUM,                                   
309700                         DAT-O-TIDATUM,                                   
309800                         DAT-KDSVAR                                       
309900     .                                                                    
310000     EJECT                                                                
310100                                                                          
310200 S22-DATUMKONV-TILL-AAVVD  SECTION.                                       
310300     MOVE 'S22-DATUMKONV-TILL-AAVVD' TO CURRENT-SECTION                   
310400                                                                          
310500     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
310600     CALL WDATKONV USING DAT-KDDATFORM,                                   
310700                         DAT-I-TIDATUM,                                   
310800                         DAT-O-TIDATUM,                                   
310900                         DAT-KDSVAR                                       
311000     .                                                                    
311100     EJECT                                                                
311200                                                                          
311300 SEC-URITY   SECTION.                                                     
311400     MOVE 'SEC-URITY  '  TO CURRENT-SECTION                               
311500     SKIP2                                                                
311600*    --- KOLLA BEHÖRIGHET PÅ LEVERANTÖRSNIVÅ                              
311700     PERFORM IMS-GET-ART                                                  
311800     IF  SEGMENT-FINNS                                                    
311900       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
312000       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
312100       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
312200*        --- BEHÖRIG ANVÄNDARE                                            
312300         SET PASSED-SECURITY-CHECK TO TRUE                                
312400       ELSE                                                               
312500*        --- OBEHÖRIG ANVÄNDARE                                           
312600         PERFORM MFS-TOM-SIDA                                             
312700         MOVE FEL-22    TO MOD-TEMFSFEL                                   
312800       END-IF                                                             
312900     END-IF                                                               
313000     .                                                                    
313100     EJECT                                                                
313200                                                                          
313300 MFS-ROER-EJ-UTFALT-PA-LISTRAD SECTION.                                   
313400                                                                          
313500     MOVE +1 TO IX                                                        
313600     PERFORM UNTIL IX > 8                                                 
313700        MOVE MFS-ROER-EJ-FAELT TO                                         
313800                                  MOD-TILEVBSK-AVS-UT(IX)                 
313900                                  MOD-KVAVIS-UT(IX)                       
314000                                  MOD-TILEVBSK-INL-C1-UT(IX)              
314100                                  MOD-TILEVBSK-DISP-C1-UT(IX)             
314200                                  MOD-FLFORAVI-C1-UT(IX)                  
314300                                  MOD-IDLEVNR-RAD(IX)                     
314400                                  MOD-IDLEVNR-SHIP-NY                     
314500        ADD +1 TO IX                                                      
314600     END-PERFORM                                                          
314700     .                                                                    
314800     EJECT                                                                
314900                                                                          
315000 MFS-RENSA-UTFALT-PA-LISTRAD SECTION.                                     
315100                                                                          
315200     MOVE MFS-RENSA-FAELT TO                                              
315300                    MOD-TILEVBSK-AVS-UT(IX)                               
315400                    MOD-KVAVIS-UT(IX)                                     
315500                    MOD-TILEVBSK-INL-C1-UT(IX)                            
315600                    MOD-TILEVBSK-DISP-C1-UT(IX)                           
315700                    MOD-FLFORAVI-C1-UT(IX)                                
315800                    MOD-IDLEVNR-RAD(IX)                                   
315900                    MOD-IDLEVNR-SHIP-NY                                   
316000     .                                                                    
316100     EJECT                                                                
316200                                                                          
316300 MFS-OEPPNA-LISTRAD SECTION.                                              
316400                                                                          
316500     MOVE MFS-ADD-LAES-IN-FAELT    TO                                     
316600                         MOD-TILEVBSK-AVS-UT-ATTR(IX)                     
316700     MOVE MFS-OPEN-ALPHA-NOMOD TO                                         
316800                         MOD-KDCMD-IN-ATTR(IX)                            
316900     MOVE MFS-OPEN-NUM-NOMOD TO                                           
317000                         MOD-KVAVIS-IN-ATTR(IX)                           
317100                         MOD-TILEVBSK-INL-C1-IN-ATTR(IX)                  
317200                         MOD-TILEVBSK-RAD-IN-ATTR(IX)                     
317300     MOVE MFS-ADD-LAES-IN-FAELT    TO                                     
317400                         MOD-IDLEVNR-RAD-ATTR(IX)                         
317500                         MOD-IDLEVNR-SHIP-NY-ATTR                         
317600     .                                                                    
317700     EJECT                                                                
317800                                                                          
317900 MFS-RENSA-INFALT-PA-LISTRAD SECTION.                                     
318000                                                                          
318100     MOVE MFS-RENSA-FAELT TO                                              
318200                         MOD-KDCMD-IN(IX)                                 
318300                         MOD-KVAVIS-IN(IX)                                
318400                         MOD-TILEVBSK-INL-C1-IN(IX)                       
318500                         MOD-TILEVBSK-RAD-IN(IX)                          
318600     .                                                                    
318700     EJECT                                                                
318800                                                                          
318900 MFS-RENSA-INFAELT-NYRAD SECTION.                                         
319000     SKIP2                                                                
319100                                                                          
319200     MOVE MFS-RENSA-FAELT    TO                                           
319300                                MOD-TILEVBSK-AVS-NY                       
319400                                MOD-KVAVIS-NY                             
319500                                MOD-TILEVBSK-INL-C1-NY                    
319600                                MOD-IDLEVNR-NY                            
319700                                MOD-IDLEVNR-SHIP-NY                       
319800     MOVE MFS-FORMATETS-ATTR TO                                           
319900                                MOD-TILEVBSK-AVS-NY-ATTR                  
320000                                MOD-KVAVIS-NY-ATTR                        
320100                                MOD-TILEVBSK-INL-C1-NY-ATTR               
320200                                MOD-IDLEVNR-NY-ATTR                       
320300                                MOD-IDLEVNR-SHIP-NY-ATTR                  
320400                                                                          
320500     .                                                                    
320600     EJECT                                                                
320700                                                                          
320800 MFS-SLAECK-MOD-PA-TEXTRAD SECTION.                                       
320900     SKIP2                                                                
321000                                                                          
321100     MOVE MFS-FORMATETS-ATTR TO                                           
321200                                MOD-TELEVBSK-EXT-ATTR                     
321300                                MOD-TELEVBSK-EXT2-ATTR                    
321400                                MOD-TELEVBSK-EXT3-ATTR                    
321500                                MOD-TELEVBSK-EXT4-ATTR                    
321600                                MOD-TIBORT-ATTR                           
321700     .                                                                    
321800     EJECT                                                                
321900                                                                          
322000 MFS-SPARA-INMATNINGSRADER SECTION.                                       
322100                                                                          
322200     MOVE +1 TO IX                                                        
322300     PERFORM UNTIL IX > 9                                                 
322400        IF IX < 9                                                         
322500                                                                          
322600                                                                          
322700           IF MID-TILEVBSK-AVS(IX) NOT = ALL '+'                          
322800              MOVE MFS-ADD-LAES-IN-FAELT TO                               
322900                                  MOD-TILEVBSK-AVS-UT-ATTR(IX)            
323000                                                                          
323100              MOVE MFS-ROER-EJ-FAELT TO                                   
323200                                  MOD-TILEVBSK-AVS-UT(IX)                 
323300                                                                          
323400                                                                          
323500              IF MID-KDCMD(IX) NOT = ALL '+'                              
323600                 MOVE MFS-OEPPNA-ALFA-FAELT TO                            
323700                                  MOD-KDCMD-IN-ATTR(IX)                   
323800                 MOVE MFS-ROER-EJ-FAELT TO                                
323900                                  MOD-KDCMD-IN(IX)                        
324000              ELSE                                                        
324100                 MOVE MFS-OEPPNA-ALFA-FAELT TO                            
324200                                  MOD-KDCMD-IN-ATTR(IX)                   
324300                 MOVE MFS-RENSA-FAELT TO                                  
324400                                  MOD-KDCMD-IN(IX)                        
324500              END-IF                                                      
324600                                                                          
324700                                                                          
324800              IF MID-KVAVIS(IX) NOT = ALL '+'                             
324900                 MOVE MFS-OEPPNA-NUM-FAELT TO                             
325000                                  MOD-KVAVIS-IN-ATTR(IX)                  
325100                 MOVE MFS-ROER-EJ-FAELT TO                                
325200                                  MOD-KVAVIS-IN(IX)                       
325300              ELSE                                                        
325400                 MOVE MFS-OEPPNA-NUM-FAELT TO                             
325500                                  MOD-KVAVIS-IN-ATTR(IX)                  
325600                 MOVE MFS-RENSA-FAELT TO                                  
325700                                  MOD-KVAVIS-IN(IX)                       
325800              END-IF                                                      
325900                                                                          
326000                                                                          
326100              IF MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                    
326200                 MOVE MFS-OEPPNA-NUM-FAELT TO                             
326300                                  MOD-TILEVBSK-INL-C1-IN-ATTR(IX)         
326400                 MOVE MFS-ROER-EJ-FAELT TO                                
326500                                  MOD-TILEVBSK-INL-C1-IN(IX)              
326600              ELSE                                                        
326700                 MOVE MFS-OEPPNA-NUM-FAELT TO                             
326800                                  MOD-TILEVBSK-INL-C1-IN-ATTR(IX)         
326900                 MOVE MFS-RENSA-FAELT TO                                  
327000                                  MOD-TILEVBSK-INL-C1-IN(IX)              
327100              END-IF                                                      
327200                                                                          
327300              IF MID-TILEVBSK-RAD   (IX) NOT = ALL '+'                    
327400                 MOVE MFS-OEPPNA-NUM-FAELT TO                             
327500                                  MOD-TILEVBSK-RAD-IN-ATTR(IX)            
327600                 MOVE MFS-ROER-EJ-FAELT TO                                
327700                                  MOD-TILEVBSK-RAD-IN(IX)                 
327800              ELSE                                                        
327900                 MOVE MFS-OEPPNA-NUM-FAELT TO                             
328000                                  MOD-TILEVBSK-RAD-IN-ATTR(IX)            
328100                 MOVE MFS-RENSA-FAELT TO                                  
328200                                  MOD-TILEVBSK-RAD-IN(IX)                 
328300              END-IF                                                      
328400                                                                          
328500           ELSE                                                           
328600                                                                          
328700              MOVE MFS-FORMATETS-ATTR TO                                  
328800                                  MOD-TILEVBSK-AVS-UT-ATTR(IX)            
328900                                                                          
329000              MOVE MFS-RENSA-FAELT TO                                     
329100                                  MOD-TILEVBSK-AVS-UT(IX)                 
329200                                                                          
329300                                                                          
329400              MOVE MFS-FORMATETS-ATTR TO                                  
329500                                  MOD-KDCMD-IN-ATTR(IX)                   
329600                                  MOD-KVAVIS-IN-ATTR(IX)                  
329700                                  MOD-TILEVBSK-INL-C1-IN-ATTR(IX)         
329800                                  MOD-TILEVBSK-RAD-IN-ATTR(IX)            
329900              MOVE MFS-RENSA-FAELT TO                                     
330000                                  MOD-KDCMD-IN(IX)                        
330100                                  MOD-KVAVIS-IN(IX)                       
330200                                  MOD-TILEVBSK-INL-C1-IN(IX)              
330300                                  MOD-TILEVBSK-RAD-IN(IX)                 
330400           END-IF                                                         
330500        ELSE                                                              
330600           IF MID-TILEVBSK-AVS(IX) NOT = ALL '+'                          
330700              MOVE MFS-OEPPNA-NUM-FAELT TO                                
330800                               MOD-TILEVBSK-AVS-NY-ATTR                   
330900              MOVE MFS-ROER-EJ-FAELT TO                                   
331000                                  MOD-TILEVBSK-AVS-NY                     
331100           ELSE                                                           
331200              MOVE MFS-FORMATETS-ATTR TO                                  
331300                               MOD-TILEVBSK-AVS-NY-ATTR                   
331400              MOVE MFS-RENSA-FAELT TO                                     
331500                                  MOD-TILEVBSK-AVS-NY                     
331600           END-IF                                                         
331700                                                                          
331800           IF MID-KVAVIS(IX) NOT = ALL '+'                                
331900              MOVE MFS-OEPPNA-NUM-FAELT TO                                
332000                               MOD-KVAVIS-NY-ATTR                         
332100              MOVE MFS-ROER-EJ-FAELT TO                                   
332200                                  MOD-KVAVIS-NY                           
332300           ELSE                                                           
332400              MOVE MFS-FORMATETS-ATTR TO                                  
332500                               MOD-KVAVIS-NY-ATTR                         
332600              MOVE MFS-RENSA-FAELT TO                                     
332700                                  MOD-KVAVIS-NY                           
332800           END-IF                                                         
332900                                                                          
333000           IF MID-TILEVBSK-INL-C1(IX) NOT = ALL '+'                       
333100              MOVE MFS-OEPPNA-NUM-FAELT TO                                
333200                               MOD-TILEVBSK-INL-C1-NY-ATTR                
333300              MOVE MFS-ROER-EJ-FAELT TO                                   
333400                                  MOD-TILEVBSK-INL-C1-NY                  
333500           ELSE                                                           
333600              MOVE MFS-FORMATETS-ATTR TO                                  
333700                               MOD-TILEVBSK-INL-C1-NY-ATTR                
333800              MOVE MFS-RENSA-FAELT TO                                     
333900                                  MOD-TILEVBSK-INL-C1-NY                  
334000           END-IF                                                         
334100                                                                          
334200           IF MID-IDLEVNR-RAD(IX) NOT = ALL '+'                           
334300              MOVE MFS-OEPPNA-ALFA-FAELT TO                               
334400                               MOD-IDLEVNR-NY-ATTR                        
334500              MOVE MFS-ROER-EJ-FAELT TO                                   
334600                               MOD-IDLEVNR-NY                             
334700           ELSE                                                           
334800              MOVE MFS-FORMATETS-ATTR TO                                  
334900                               MOD-IDLEVNR-NY-ATTR                        
335000              MOVE MFS-RENSA-FAELT TO                                     
335100                               MOD-IDLEVNR-NY                             
335200           END-IF                                                         
335300                                                                          
335400           IF MID-IDLEVNR-SHIP    NOT = ALL '+'                           
335500              MOVE MFS-OEPPNA-ALFA-FAELT TO                               
335600                               MOD-IDLEVNR-SHIP-NY-ATTR                   
335700              MOVE MFS-ROER-EJ-FAELT TO                                   
335800                               MOD-IDLEVNR-SHIP-NY                        
335900           ELSE                                                           
336000              MOVE MFS-FORMATETS-ATTR TO                                  
336100                               MOD-IDLEVNR-SHIP-NY-ATTR                   
336200              MOVE MFS-RENSA-FAELT TO                                     
336300                               MOD-IDLEVNR-SHIP-NY                        
336400           END-IF                                                         
336500                                                                          
336600        END-IF                                                            
336700        ADD +1 TO IX                                                      
336800     END-PERFORM                                                          
336900                                                                          
337000     IF MID-TELEVBSK-EXT  NOT = ALL '+'                                   
337100     OR MID-TELEVBSK-EXT2 NOT = ALL '+'                                   
337200     OR MID-TELEVBSK-EXT3 NOT = ALL '+'                                   
337300     OR MID-TELEVBSK-EXT4 NOT = ALL '+'                                   
337400     OR MID-TIBORT        NOT = ALL '+'                                   
337500        MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT                        
337600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVBSK-EXT-ATTR               
337700        MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT2                       
337800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVBSK-EXT2-ATTR              
337900        MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT3                       
338000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVBSK-EXT3-ATTR              
338100        MOVE MFS-ROER-EJ-FAELT TO MOD-TELEVBSK-EXT4                       
338200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVBSK-EXT4-ATTR              
338300        MOVE MFS-ROER-EJ-FAELT TO MOD-TIBORT                              
338400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIBORT-ATTR                     
338500     END-IF                                                               
338600     .                                                                    
338700     EJECT                                                                
338800                                                                          
338900 MFS-TOM-SIDA SECTION.                                                    
339000     SKIP2                                                                
339100                                                                          
339200     MOVE +1 TO IX                                                        
339300     PERFORM UNTIL IX > 8                                                 
339400        MOVE MFS-RENSA-FAELT TO                                           
339500                       MOD-KDCMD-IN(IX)                                   
339600                       MOD-TILEVBSK-AVS-UT(IX)                            
339700                       MOD-TILEVBSK-RAD-IN(IX)                            
339800                       MOD-KVAVIS-IN(IX)                                  
339900                       MOD-KVAVIS-UT(IX)                                  
340000                       MOD-TILEVBSK-INL-C1-UT(IX)                         
340100                       MOD-TILEVBSK-INL-C1-IN(IX)                         
340200                       MOD-TILEVBSK-DISP-C1-UT(IX)                        
340300                       MOD-FLFORAVI-C1-UT(IX)                             
340400                       MOD-IDLEVNR-RAD(IX)                                
340500                       MOD-IDLEVNR-SHIP-NY                                
340600        MOVE MFS-FORMATETS-ATTR TO                                        
340700                       MOD-KDCMD-IN-ATTR(IX)                              
340800                       MOD-TILEVBSK-AVS-UT-ATTR(IX)                       
340900                       MOD-TILEVBSK-RAD-IN-ATTR(IX)                       
341000                       MOD-KVAVIS-IN-ATTR(IX)                             
341100                       MOD-TILEVBSK-INL-C1-IN-ATTR(IX)                    
341200                       MOD-IDLEVNR-RAD-ATTR(IX)                           
341300                       MOD-IDLEVNR-SHIP-NY-ATTR                           
341400        ADD +1 TO IX                                                      
341500     END-PERFORM                                                          
341600                                                                          
341700     MOVE MFS-RENSA-FAELT   TO                                            
341800                    MOD-KVAVIS-NY                                         
341900                    MOD-TILEVBSK-AVS-NY                                   
342000                    MOD-TILEVBSK-INL-C1-NY                                
342100                    MOD-IDLEVNR-NY                                        
342200                    MOD-IDLEVNR-SHIP-NY                                   
342300     MOVE MFS-FORMATETS-ATTR TO                                           
342400                    MOD-TILEVBSK-AVS-NY-ATTR                              
342500                    MOD-KVAVIS-NY-ATTR                                    
342600                    MOD-TILEVBSK-INL-C1-NY-ATTR                           
342700                    MOD-IDLEVNR-NY-ATTR                                   
342800                    MOD-IDLEVNR-SHIP-NY-ATTR                              
342900     MOVE MFS-RENSA-FAELT    TO MOD-TELEVBSK-EXT                          
343000                                MOD-TELEVBSK-EXT2                         
343100                                MOD-TELEVBSK-EXT3                         
343200                                MOD-TELEVBSK-EXT4                         
343300                                MOD-TIBORT                                
343400     .                                                                    
343500     EJECT                                                                
343600                                                                          
343700                                                                          
343800***************************************************************           
343900*    IMS-SEKTIONER                                                        
344000***************************************************************           
344100                                                                          
344200 IMS-GET-MSG SECTION.                                                     
344300     MOVE '  QC' TO GODK-STATUSKODER                                      
344400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
344500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
344600     PERFORM IMS-STATUSKONTROLL                                           
344700     .                                                                    
344800                                                                          
344900 IMS-INSERT-MSG SECTION.                                                  
345000     IF ENGLISH-TEXT                                                      
345100        MOVE 'N' TO MFS-KDHUVOMR                                          
345200     END-IF                                                               
345300     IF MSGI-IDLAND-SPR = 'GB'                                            
345400        MOVE 'N' TO MFS-KDHUVOMR                                          
345500     END-IF                                                               
345600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
345700     MOVE SPACE TO GODK-STATUSKODER                                       
345800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
345900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
346000     PERFORM IMS-STATUSKONTROLL                                           
346100     .                                                                    
346200                                                                          
346300                                                                          
346400 IMS-GET-ART-INLB SECTION.                                                
346500     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
346600            DELIMITED BY SIZE INTO SSA1                                   
346700     MOVE '  GE' TO GODK-STATUSKODER                                      
346800     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA-2 SSA1                    
346900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
347000     PERFORM IMS-STATUSKONTROLL                                           
347100     .                                                                    
347200                                                                          
347300 IMS-GET-LEV-INLB SECTION.                                                
347400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
347500            DELIMITED BY SIZE INTO SSA1                                   
347600     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
347700            DELIMITED BY SIZE INTO SSA2                                   
347800     MOVE '  GE' TO GODK-STATUSKODER                                      
347900     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA-2 SSA1 SSA2               
348000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
348100     PERFORM IMS-STATUSKONTROLL                                           
348200     .                                                                    
348300                                                                          
348400 IMS-GET-LEV-INLB-SHIP SECTION.                                           
348500     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
348600            DELIMITED BY SIZE INTO SSA1                                   
348700     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
348800            DELIMITED BY SIZE INTO SSA2                                   
348900     MOVE '  GE' TO GODK-STATUSKODER                                      
349000     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA-2 SSA1 SSA2               
349100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
349200     PERFORM IMS-STATUSKONTROLL                                           
349300     .                                                                    
349400                                                                          
349500 IMS-GET-LEV-OKVAL-INLB SECTION.                                          
349600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
349700            DELIMITED BY SIZE INTO SSA1                                   
349800     MOVE 'WLINLB11 ' TO SSA2                                             
349900     MOVE '  GE' TO GODK-STATUSKODER                                      
350000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1 SSA2              
350100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
350200     PERFORM IMS-STATUSKONTROLL                                           
350300     .                                                                    
350400                                                                          
350500 IMS-GET-LEV-NEXT-INLB SECTION.                                           
350600     MOVE 'WLINLB11 ' TO SSA1                                             
350700     MOVE '  GE' TO GODK-STATUSKODER                                      
350800     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1                   
350900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
351000     PERFORM IMS-STATUSKONTROLL                                           
351100     .                                                                    
351200                                                                          
351300 IMS-ISRT-LEV-INLB SECTION.                                               
351400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
351500            DELIMITED BY SIZE INTO SSA1                                   
351600     MOVE 'WLINLB11 ' TO SSA2                                             
351700     MOVE '  '   TO GODK-STATUSKODER                                      
351800     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA-2 SSA1 SSA2             
351900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
352000     PERFORM IMS-STATUSKONTROLL                                           
352100     .                                                                    
352200                                                                          
352300 IMS-GET-LEVBESK-INLB SECTION.                                            
352400     STRING 'WLINLB24(DALEVBSK =' W-DALEVBSK-X ')'                        
352500            DELIMITED BY SIZE INTO SSA1                                   
352600     MOVE '  GE' TO GODK-STATUSKODER                                      
352700     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA-2 SSA1                  
352800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
352900     PERFORM IMS-STATUSKONTROLL                                           
353000     .                                                                    
353100                                                                          
353200 IMS-GET-LEVBESK-OKVAL-INLB SECTION.                                      
353300     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
353400            DELIMITED BY SIZE INTO SSA1                                   
353500     MOVE 'WLINLB24 ' TO SSA2                                             
353600     MOVE '  GE' TO GODK-STATUSKODER                                      
353700     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1 SSA2              
353800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
353900     PERFORM IMS-STATUSKONTROLL                                           
354000     .                                                                    
354100                                                                          
354200 IMS-GET-LEVBESK-2OKVAL-INLB SECTION.                                     
354300     STRING 'WLINLB11*F  '                                                
354400            DELIMITED BY SIZE INTO SSA1                                   
354500     MOVE 'WLINLB24 ' TO SSA2                                             
354600     MOVE '  GE' TO GODK-STATUSKODER                                      
354700     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1 SSA2              
354800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
354900     PERFORM IMS-STATUSKONTROLL                                           
355000     .                                                                    
355100                                                                          
355200 IMS-GET-LEVBESK-GE-INLB SECTION.                                         
355300     STRING 'WLINLB24(DALEVBSK>=' W-DALEVBSK-X ')'                        
355400            DELIMITED BY SIZE INTO SSA1                                   
355500     MOVE '  GE' TO GODK-STATUSKODER                                      
355600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1                   
355700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
355800     PERFORM IMS-STATUSKONTROLL                                           
355900     .                                                                    
356000                                                                          
356100 IMS-GET-LEVBESK-GE-GE-INLB SECTION.                                      
356200     STRING 'WLINLB11(IDLEVNR >=' W-IDLEVNR-X ')'                         
356300            DELIMITED BY SIZE INTO SSA1                                   
356400     STRING 'WLINLB24(DALEVBSK>=' W-DALEVBSK-X ')'                        
356500            DELIMITED BY SIZE INTO SSA2                                   
356600     MOVE '  GE' TO GODK-STATUSKODER                                      
356700     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1 SSA2              
356800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
356900     PERFORM IMS-STATUSKONTROLL                                           
357000     .                                                                    
357100                                                                          
357200 IMS-GET-LEVBESK-NEXT-INLB SECTION.                                       
357300     MOVE 'WLINLB24 ' TO SSA1                                             
357400     MOVE '  GE' TO GODK-STATUSKODER                                      
357500     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA-2 SSA1                   
357600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
357700     PERFORM IMS-STATUSKONTROLL                                           
357800     .                                                                    
357900                                                                          
358000 IMS-ISRT-LEVBESK-INLB SECTION.                                           
358100     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
358200            DELIMITED BY SIZE INTO SSA1                                   
358300     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
358400            DELIMITED BY SIZE INTO SSA2                                   
358500     MOVE 'WLINLB24 ' TO SSA3                                             
358600     MOVE '  '   TO GODK-STATUSKODER                                      
358700     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA-2                       
358800     SSA1 SSA2 SSA3                                                       
358900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
359000     PERFORM IMS-STATUSKONTROLL                                           
359100     .                                                                    
359200                                                                          
359300 IMS-ISRT-LEVBESK-INLBA SECTION.                                          
359400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
359500            DELIMITED BY SIZE INTO SSA1                                   
359600     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
359700            DELIMITED BY SIZE INTO SSA2                                   
359800     MOVE 'WLINLB24 ' TO SSA3                                             
359900     MOVE '  II'   TO GODK-STATUSKODER                                    
360000     CALL CBLTDLI USING ISRT INLBA-PCB DLI-IO-AREA-A                      
360100     SSA1 SSA2 SSA3                                                       
360200     MOVE INLBA-STATUS-CODE TO STATUS-WS                                  
360300     PERFORM IMS-STATUSKONTROLL                                           
360400     .                                                                    
360500                                                                          
360600 IMS-REPL-INLB SECTION.                                                   
360700     MOVE '  '   TO GODK-STATUSKODER                                      
360800     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA-2                       
360900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
361000     PERFORM IMS-STATUSKONTROLL                                           
361100     .                                                                    
361200                                                                          
361300 IMS-DLET-INLB SECTION.                                                   
361400     MOVE '  '   TO GODK-STATUSKODER                                      
361500     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA-2                       
361600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
361700     PERFORM IMS-STATUSKONTROLL                                           
361800     .                                                                    
361900                                                                          
362000 IMS-GET-INFO-INLB SECTION.                                               
362100     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-X ')'                        
362200            DELIMITED BY SIZE INTO SSA1                                   
362300     MOVE '  GE' TO GODK-STATUSKODER                                      
362400     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA-2 SSA1                   
362500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
362600     PERFORM IMS-STATUSKONTROLL                                           
362700     .                                                                    
362800                                                                          
362900 IMS-GET-INFO-INLB-KVAL SECTION.                                          
363000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
363100            DELIMITED BY SIZE INTO SSA1                                   
363200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
363300            DELIMITED BY SIZE INTO SSA2                                   
363400     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-X ')'                        
363500            DELIMITED BY SIZE INTO SSA3                                   
363600     MOVE '  GE' TO GODK-STATUSKODER                                      
363700     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA-2 SSA1 SSA2 SSA3         
363800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
363900     PERFORM IMS-STATUSKONTROLL                                           
364000     .                                                                    
364100                                                                          
364200 IMS-ISRT-INFO-INLB SECTION.                                              
364300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
364400            DELIMITED BY SIZE INTO SSA1                                   
364500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
364600            DELIMITED BY SIZE INTO SSA2                                   
364700     MOVE 'WLINLB25 ' TO SSA3                                             
364800     MOVE '  '   TO GODK-STATUSKODER                                      
364900     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA-2                       
365000     SSA1 SSA2 SSA3                                                       
365100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
365200     PERFORM IMS-STATUSKONTROLL                                           
365300     .                                                                    
365400                                                                          
365500 IMS-ISRT-ROT-INLB SECTION.                                               
365600     MOVE 'WLINLB01 ' TO SSA1                                             
365700     MOVE '  '   TO GODK-STATUSKODER                                      
365800     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA-2 SSA1                  
365900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
366000     PERFORM IMS-STATUSKONTROLL                                           
366100     .                                                                    
366200                                                                          
366300 IMS-GET-LEV-LEVA SECTION.                                                
366400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
366500            DELIMITED BY SIZE INTO SSA1                                   
366600     MOVE '  GE' TO GODK-STATUSKODER                                      
366700     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA-2 SSA1                    
366800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
366900     PERFORM IMS-STATUSKONTROLL                                           
367000     .                                                                    
367100                                                                          
367200 IMS-GET-LEV-LEVA-SHIP SECTION.                                           
367300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-SHIP-X ')'                    
367400            DELIMITED BY SIZE INTO SSA1                                   
367500     MOVE '  GE' TO GODK-STATUSKODER                                      
367600     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA-2 SSA1                    
367700     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
367800     PERFORM IMS-STATUSKONTROLL                                           
367900     .                                                                    
368000                                                                          
368100 IMS-GET-ART SECTION.                                                     
368200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
368300            DELIMITED BY SIZE INTO SSA1                                   
368400     MOVE '  GE' TO GODK-STATUSKODER                                      
368500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
368600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
368700     PERFORM IMS-STATUSKONTROLL                                           
368800     .                                                                    
368900                                                                          
369000 IMS-GET-CLAG SECTION.                                                    
369100     MOVE 'WLARTC11 ' TO SSA1                                             
369200     MOVE '  GE'   TO GODK-STATUSKODER                                    
369300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
369400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
369500     PERFORM IMS-STATUSKONTROLL                                           
369600     .                                                                    
369700                                                                          
369800 IMS-GNP-ARTC23 SECTION.                                                  
369900     MOVE 'WLARTC11 ' TO SSA1                                             
370000     MOVE 'WLARTC23 ' TO SSA2                                             
370100     MOVE '  GE' TO GODK-STATUSKODER                                      
370200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-23 SSA1 SSA2             
370300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
370400     PERFORM IMS-STATUSKONTROLL                                           
370500     .                                                                    
370600 IMS-ISRT-2228 SECTION.                                                   
370700     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
370800            DELIMITED BY SIZE INTO SSA1                                   
370900     MOVE   'WLXXBW11 ' TO SSA2                                           
371000     MOVE '  II' TO GODK-STATUSKODER                                      
371100     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA-3 SSA1 SSA2             
371200     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
371300     PERFORM IMS-STATUSKONTROLL                                           
371400     .                                                                    
371500                                                                          
371600                                                                          
371700 IMS-STATUSKONTROLL SECTION.                                              
371800     SET STATUS-IX TO 1                                                   
371900     SEARCH GODK-STATUS AT END CALL FELLOG                                
372000        WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                           
372100           CONTINUE                                                       
372200     END-SEARCH                                                           
372300     .                                                                    
372400                                                                          
372500     EJECT                                                                
372600*    -COPY WY2000P1                                                       
372700     EJECT                                                                
372800*    -COPY WY2000P2                                                       

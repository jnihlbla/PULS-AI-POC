000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2141000.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   94/11/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    LARMRAPPORT                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        W2141000 LÄSER WDK6, SKAPAR EN POST PÅ FILEN                     
001200*        W21410 MED NEJ OM EN TIDIGARE LAGERBRIST ÅTERSTÄLLTS             
001300*        (LAGERSALDO => SÄKERHETSLAGER) OCH MED JA OM BRIST I             
001400*        LAGRET UPPSTÅTT (LAGERSALDO < SÄKERHETSLAGER).                   
001500*        FILEN SKA SENARE I PROGRAM W2141200 UPPDATERA WDK6.              
001600*                                                                         
001700*                                                                         
001800*        OM FLLARM-BUF UPPDATERAS MED JA SKAPAS                           
001900*        EN LARMPOST PER ARTIKEL PÅ W21411.                               
002000*                                                                         
002100*                                                                         
002200*                                                                         
002300*                                                                         
002400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002500*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- ARTIKELFIL WDK6                                            
003600     SELECT W01160                     ASSIGN TO W21410D1.                
003700     SKIP2                                                                
003800*          --- LARMPOSTER                                                 
003900     SELECT W21411                     ASSIGN TO W21410D2.                
004000     SKIP2                                                                
004100*          --- UPPDATERINGSPOSTER FLLARM-BUF                              
004200     SELECT W21410                     ASSIGN TO W21410D3.                
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP2                                                                
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W01160                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  -COPY W01160      -L.                                                
005300     SKIP3                                                                
005400 FD  W21411                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  POST -COPY W2141101 -PRE  W21411-  -L.                               
005900 FD  W21410                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W21410 -PRE  W21410-  -L.                                 
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700*    -COPY WY2000W2                                                       
006800     SKIP3                                                                
006900 77  IDPGM                       PIC X(8)    VALUE 'W2141000'.            
007000 77  PROGRAM-NAMN                PIC X(6)    VALUE 'W21410'.              
007010 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
007020 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007210 77  W-CREATE-ALERT              PIC X(1)    VALUE SPACE.                 
007220 77  W-SLAG-KVPB-REF             PIC 9(9)V9(2) VALUE ZERO COMP-3.         
007230 01  WS-DAGENS-AAAAMMDD          PIC 9(8).                                
007240     EJECT                                                                
007310*    -COPY WWDC99                                                         
007320     EJECT                                                                
007400 01  -COPY WWDCKONS.                                                      
007500                                                                          
007600 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W01160                       VALUE 'J'.                   
007800     EJECT                                                                
007900******************************************************************        
008000*    ALLMÄNNA ARBETSAREOR                                                 
008100******************************************************************        
008200                                                                          
008300 01  FILLER                  PIC X(16)  VALUE 'WS-FALT        '.          
008400                                                                          
008500 01  WS-FALT.                                                             
008600     05  WS-BEART-SVE         PIC X(25).                                  
008700     05  WS-LAGERTILLG        PIC S9(9)               COMP-3.             
008800     05  WS-DDATUM            PIC 9(5).                                   
008900     05  FILLER              REDEFINES WS-DDATUM.                         
009000         10  WS-DDATUMAAVV-X.                                             
009100             15  WS-DDATUMAA PIC 9(2).                                    
009200             15  WS-DDATUMVV PIC 9(2).                                    
009300         10  WS-DDATUMAAVV   REDEFINES WS-DDATUMAAVV-X                    
009400                             PIC 9(4).                                    
009500         10  WS-DDATUMD      PIC 9.                                       
009600     SKIP3                                                                
009700 01  WS-TIRODAT              PIC S9(5).                                   
009800 01  FILLER  REDEFINES WS-TIRODAT.                                        
009900     03  AA1                 PIC 9.                                       
010000     03  AA2                 PIC 9.                                       
010100     03  FILLER              PIC 9(3).                                    
010200     SKIP3                                                                
010300 01  WS-AARDEL               PIC 99.                                      
010400 01  FILLER  REDEFINES WS-AARDEL.                                         
010500     03  AARDEL1             PIC 9.                                       
010600     03  FILLER              PIC 9.                                       
010700     SKIP3                                                                
010800                                                                          
010900******************************************************************        
011000*    SWITCHAR.                                                            
011100******************************************************************        
011200                                                                          
011300 01  FILLER                  PIC X(16)  VALUE 'SWITCHAR       '.          
011400                                                                          
011500 01  SW.                                                                  
011600     05  SW-FORSTA-ARTIKEL      PIC X       VALUE 'J'.                    
011700     05  SW-SKRIVA-LARMRAPPORT  PIC X       VALUE 'N'.                    
011800     EJECT                                                                
011900                                                                          
012000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012100 01  FILLER REDEFINES DAGENS-DATUM.                                       
012200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012500     EJECT                                                                
012600*                                                                         
012700*01  -COPY WWPRODSL                                                       
012800*                                                                         
012900 01  DYNAMISKA-SUBPROGRAM.                                                
013000*                                                                         
013100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
013400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013600     SKIP2                                                                
013700*    --- PARAMETRAR TILL ABEND                                            
013800                                                                          
013900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014100     SKIP2                                                                
014200 01  FELTEXT.                                                             
014300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014500     EJECT                                                                
014600*    ----PARAMETRAR TILL DATKORT                                          
014700                                                                          
014800 01  FILLER                  PIC X(16)  VALUE 'DATUMKORT      '.          
014900 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
015000*01  -COPY WDATKORT                                                       
015100     EJECT                                                                
015200                                                                          
015300*    --- PARAMETRAR TILL POSTSUM                                          
015400*01  -COPY W0005   -PRE  POSTSUM-                                         
015500     EJECT                                                                
015600 01  W01160-AREA-START           PIC X(24)   VALUE                        
015700                                 'W01160-AREA-START  '.                   
015800     SKIP2                                                                
015900                                                                          
016000*01  AREA -COPY W01160     -PRE W01160-                                   
016100     EJECT                                                                
016200 01  W21411-AREA-START           PIC X(24)   VALUE                        
016300                                 'W21411-AREA-START  '.                   
016400     SKIP2                                                                
016500                                                                          
016600*01  AREA -COPY W2141101   -PRE W21411-                                   
016700     EJECT                                                                
016800 01  W21410-AREA-START           PIC X(24)   VALUE                        
016900                                 'W21410-AREA-START  '.                   
017000     SKIP2                                                                
017100                                                                          
017200*01  AREA -COPY W21410     -PRE W21410-                                   
017300     EJECT                                                                
017400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017500*                                                                         
017600     EJECT                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017800     SKIP3                                                                
017900 01  NYCKLAR-TILL-DLI.                                                    
018000     03  W-IDARTNR-X.                                                     
018100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018200     03  W-KDSEGKEY-X.                                                    
018300         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
018400     03  W-IDSKYLT-X.                                                     
018500        05 W-IDSKYLT            PIC X(3)   VALUE 'S  '.                   
018600     EJECT                                                                
018700     SKIP2                                                                
018800*    --- STATUS-KOD FRÅN IMS                                              
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019300     SKIP2                                                                
019400 01  GODK-STATUSKODER.                                                    
019500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019600     SKIP3                                                                
019700 01  SSA1                        PIC X(64).                               
019800 01  SSA2                        PIC X(64).                               
019900     EJECT                                                                
020000*    --- IMS FUNKTIONSKODER                                               
020100*01  -COPY W0003                                                          
020200     EJECT                                                                
020300*    ---  DLI INPUT-OUTPUT AREA                                           
020400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020500     SKIP3                                                                
020600 01  DLI-IO-AREA.                                                         
020700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
020800     SKIP3                                                                
020900*    03  WLBENA11 -COPY WDD311 -PRE BENA-  -RED IO-AREA.                  
021000                                                                          
021100                                                                          
021200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-01'.          
021300     SKIP3                                                                
021400 01  DLI-IO-AREA-01.                                                      
021500     03  IO-AREA-01               PIC X(150) VALUE SPACE.                 
021600     03  WLARTC01 REDEFINES IO-AREA-01.                                   
021700*        05  -COPY WDK601                                                 
021800     SKIP3                                                                
021900                                                                          
022000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-11'.        
022100     SKIP3                                                                
022200 01  DLI-IO-AREA-11.                                                      
022300     03  IO-AREA-11               PIC X(900) VALUE SPACE.                 
022400     03  WLARTC11 REDEFINES IO-AREA-11.                                   
022500*        05  -COPY WDK611                                                 
022600     EJECT                                                                
022610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
022620 01  DLI-IO-WDK701.                                                       
022630*    03  -COPY WDK701                                                     
022640     EJECT                                                                
022650 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
022660 01  DLI-IO-WDK711.                                                       
022670*    03  -COPY WDK711                                                     
022680     EJECT                                                                
022700 LINKAGE SECTION.                                                         
022800                                                                          
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE BENA-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008  -PRE ARTC-                                              
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023510*01  -COPY W0008  -PRE WDK7-                                              
023520     05  FILLER                  PIC X.                                   
023530     EJECT                                                                
023600 PROCEDURE DIVISION  USING BENA-PCB ARTC-PCB WDK7-PCB.                    
023700 MAIN SECTION.                                                            
023800     ENTRY 'DLITCBL' USING BENA-PCB ARTC-PCB WDK7-PCB.                    
023900                                                                          
024000                                                                          
024100     PERFORM A-INIT                                                       
024200     PERFORM S01-LAES-W01160                                              
024300                                                                          
024400     PERFORM UNTIL END-OF-W01160                                          
024500        PERFORM B-BEHANDLA-ARTIKEL                                        
024600        PERFORM C-NOLLSTALL                                               
024700        PERFORM S01-LAES-W01160                                           
024800     END-PERFORM                                                          
024900     PERFORM Z-FINIT                                                      
025000                                                                          
025100     MOVE ZERO TO RETURN-CODE                                             
025200     GOBACK                                                               
025300     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600                                                                          
025700 A-INIT SECTION.                                                          
025800                                                                          
025900     OPEN INPUT  W01160                                                   
026000                                                                          
026100     OPEN OUTPUT W21411                                                   
026200                 W21410                                                   
026300                                                                          
026310     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
026320                                                                          
026400     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
026500                                                                          
026600     MOVE D-AAR TO WS-DDATUMAA                                            
026700     MOVE D-VECKA TO WS-DDATUMVV                                          
026800     MOVE D-DAGNR TO WS-DDATUMD                                           
026900                                                                          
027000     MOVE D-AAR TO WS-AARDEL                                              
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400                                                                          
027500 B-BEHANDLA-ARTIKEL SECTION.                                              
027600     SKIP2                                                                
027700     MOVE NEJ TO SW-SKRIVA-LARMRAPPORT                                    
027800     MOVE W01160-CLAG-IDARTNR TO W-IDARTNR                                
027900     IF W01160-CLAG-KDERS-UTG = ZERO                                      
028000        IF W01160-CLAG-KDERS = ZERO                                       
028100           PERFORM BA-SKALL-FLLARM-BUF-UPPDATERAS                         
028200           IF SW-SKRIVA-LARMRAPPORT = JA                                  
028300              PERFORM BB-SKAPA-LARM                                       
028400           END-IF                                                         
028500        END-IF                                                            
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900                                                                          
029000 BA-SKALL-FLLARM-BUF-UPPDATERAS SECTION.                                  
029100                                                                          
029110     PERFORM BAC-CHECK-PB-TOT                                             
029120     IF W-CREATE-ALERT = JA                                               
029400        IF W01160-CLAG-FLLARM-BUF = JA                                    
029500           PERFORM BAA-TESTA-EV-NEJ-FLLARM-BUF                            
029600        ELSE                                                              
029700           PERFORM BAB-TESTA-EV-JA-FLLARM-BUF                             
029800        END-IF                                                            
029910     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300 BAA-TESTA-EV-NEJ-FLLARM-BUF SECTION.                                     
030400                                                                          
030500     PERFORM S30-BER-LAGERTILLG                                           
030600     IF W01160-CLAG-KVSLAGER <= WS-LAGERTILLG                             
030700        PERFORM BAAA-UPPDATERA-MED-NEJ                                    
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 BAAA-UPPDATERA-MED-NEJ SECTION.                                          
031300                                                                          
031400                                                                          
031500     PERFORM IMS-GET-ART                                                  
031600     IF SEGMENT-FINNS                                                     
031700        MOVE ART-KDPRODSL        TO TEST-KDPRODSL                         
031800        IF ART-KDERS-UTG = 0 AND KDPRODSL-VOLVO-BIMA                      
031900           MOVE W-IDARTNR TO W21410-LARM-IDARTNR                          
032000           MOVE NEJ       TO W21410-LARM-FLLARM-BUF                       
032100           MOVE WC-CDC-SE TO W21410-LARM-IDDC                             
032200           MOVE SPACE     TO W21410-LARM-IDLEVNR                          
032300           PERFORM S12-SKRIV-W21410                                       
032400        END-IF                                                            
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900 BAB-TESTA-EV-JA-FLLARM-BUF SECTION.                                      
033000                                                                          
033100     IF W01160-CLAG-KVUTRS = ZERO                                         
033200           PERFORM S30-BER-LAGERTILLG                                     
033300           IF W01160-CLAG-KVSLAGER = 0                                    
033400              CONTINUE                                                    
033500           ELSE                                                           
033600              IF W01160-CLAG-KVSLAGER > WS-LAGERTILLG                     
033700                 MOVE W01160-CLAG-TIFINLV TO TMP1-YYWWD                   
033800                 MOVE WS-DDATUM           TO TMP2-YYWWD                   
033900                 PERFORM WY2000P2                                         
034000                 IF TMP1-YYWWD + 10 >= TMP2-YYWWD                         
034100                    CONTINUE                                              
034200                 ELSE                                                     
034300                    IF W01160-CLAG-KDKSP = 0                              
034400                       PERFORM BABA-UPPDATERA-MED-JA                      
034500                    END-IF                                                
034600                 END-IF                                                   
034700              END-IF                                                      
034800           END-IF                                                         
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200                                                                          
035300 BABA-UPPDATERA-MED-JA SECTION.                                           
035400     SKIP2                                                                
035500     PERFORM IMS-GET-ART                                                  
035600     IF SEGMENT-FINNS                                                     
035700        MOVE ART-KDPRODSL        TO TEST-KDPRODSL                         
035800        IF ART-KDERS-UTG = 0 AND KDPRODSL-VOLVO-BIMA                      
035900           MOVE W-IDARTNR TO W21410-LARM-IDARTNR                          
036000           MOVE JA        TO W21410-LARM-FLLARM-BUF                       
036100           MOVE WC-CDC-SE TO W21410-LARM-IDDC                             
036200           MOVE SPACE     TO W21410-LARM-IDLEVNR                          
036300           PERFORM S12-SKRIV-W21410                                       
036400                                                                          
036500           IF KDPRODSL-BIMA                                               
036600*             --- ÖVRIGA PRODUKTSLAG LARMAS MOT BILD 2171                 
036700*             --- TRANS SKICKAS TILL DISPATCHER MOT 2191 I W21412         
036800              MOVE JA TO SW-SKRIVA-LARMRAPPORT                            
036900           END-IF                                                         
037000        END-IF                                                            
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037410 BAC-CHECK-PB-TOT SECTION.                                                
037420     MOVE 'BAC-CHECK-PB-TOT          ' TO CURRENT-SECTION                 
037430                                                                          
037440     MOVE JA                          TO W-CREATE-ALERT                   
037450     MOVE +0                          TO W-SLAG-KVPB-REF                  
037460     IF W01160-CLAG-DAPBPLAN > WS-DAGENS-AAAAMMDD                         
037470        IF W01160-CLAG-KVPB-PLAN > 5.0                                    
037480           MOVE NEJ                   TO W-CREATE-ALERT                   
037490        END-IF                                                            
037491     ELSE                                                                 
037492        MOVE W01160-CLAG-IDARTNR      TO W-IDARTNR                        
037493        PERFORM IMS-GU-WDK701                                             
037494        IF SEGMENT-FINNS                                                  
037495           PERFORM IMS-GNP-WDK711                                         
037496           PERFORM UNTIL SEGMENT-SAKNAS                                   
037497           MOVE SLAG-IDDC             TO WS-IDDC                          
037499              IF SLAG-IDDC-REF = WC-CDC-SE                                
037501                 ADD SLAG-KVPB-REF    TO W-SLAG-KVPB-REF                  
037502              END-IF                                                      
037503              PERFORM IMS-GNP-WDK711                                      
037504           END-PERFORM                                                    
037505        END-IF                                                            
037517        IF (W01160-CLAG-KVPB-SEP  +                                       
037518            W01160-CLAG-KVPB-SATS +                                       
037519            W-SLAG-KVPB-REF) = 0 OR > 5.0                                 
037520            MOVE NEJ                  TO W-CREATE-ALERT                   
037522        END-IF                                                            
037523     END-IF                                                               
037525     .                                                                    
037526 BB-SKAPA-LARM SECTION.                                                   
037600                                                                          
037700     PERFORM BBA-BEART-FRAN-WDD3                                          
037800     PERFORM BBB-FLYTTA-TILL-UTFIL                                        
037900     PERFORM S11-SKRIV-W21411                                             
038000     .                                                                    
038100     EJECT                                                                
038200                                                                          
038300 BBA-BEART-FRAN-WDD3 SECTION.                                             
038400                                                                          
038500     PERFORM IMS-GET-BENA11-BSEQ                                          
038600     IF SEGMENT-FINNS                                                     
038700        MOVE BENA-TEXT-BEART TO WS-BEART-SVE                              
038800     ELSE                                                                 
038900        MOVE 'BENAMNING SAKNAS     ' TO WS-BEART-SVE                      
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 BBB-FLYTTA-TILL-UTFIL SECTION.                                           
039500                                                                          
039600     MOVE W01160-CLAG-IDFTG TO W21411-IDFTG                               
039700     MOVE W01160-CLAG-IDLEVNR TO W21411-IDLEVNR                           
039800     MOVE W01160-CLAG-IDARTNR TO W21411-IDARTNR                           
039900     MOVE W01160-CLAG-IDANSK TO W21411-IDANSK                             
040000     MOVE W01160-CLAG-KDVVKL TO W21411-KDVVKL                             
040100     MOVE W01160-CLAG-KDGK TO W21411-KDGK                                 
040200     MOVE W01160-CLAG-KDLTK TO W21411-KDLTK                               
040300     MOVE WS-BEART-SVE    TO W21411-BEART-SVE                             
040400     .                                                                    
040500     EJECT                                                                
040600                                                                          
040700                                                                          
040800                                                                          
040900 C-NOLLSTALL SECTION.                                                     
041000                                                                          
041100     MOVE ZERO TO WS-LAGERTILLG                                           
041200     .                                                                    
041300     EJECT                                                                
041400******************************************************************        
041500*                                                                *        
041600*    AVSLUTNING                                                  *        
041700*    STÄNG FIL, SKRIV UT POSTSUMS RÄKNEVERK                      *        
041800*                                                                *        
041900******************************************************************        
042000                                                                          
042100 Z-FINIT SECTION.                                                         
042200     CLOSE W01160                                                         
042300           W21411                                                         
042400           W21410                                                         
042500     SKIP2                                                                
042600     MOVE 'S' TO POSTSUM-OPKOD                                            
042700     CALL POSTSUM USING POSTSUM-PARM                                      
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100                                                                          
043200 S01-LAES-W01160  SECTION.                                                
043300     READ W01160 INTO W01160-AREA                                         
043400     AT END                                                               
043500        SET END-OF-W01160 TO TRUE                                         
043600                                                                          
043700     NOT AT END                                                           
043800        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
043900        MOVE 'W21410D1' TO POSTSUM-DDNAMN2                                
044000        CALL POSTSUM USING POSTSUM-PARM                                   
044100     END-READ                                                             
044200     .                                                                    
044300     EJECT                                                                
044400                                                                          
044500                                                                          
044600 S11-SKRIV-W21411 SECTION.                                                
044700                                                                          
044800     WRITE W21411-POST FROM W21411-AREA                                   
044900                                                                          
045000     MOVE 'W21411' TO POSTSUM-FDNAMN                                      
045100     MOVE 'W21410D2' TO POSTSUM-DDNAMN2                                   
045200     CALL POSTSUM USING POSTSUM-PARM                                      
045300     .                                                                    
045400     EJECT                                                                
045500                                                                          
045600                                                                          
045700 S12-SKRIV-W21410 SECTION.                                                
045800                                                                          
045900     WRITE W21410-POST FROM W21410-AREA                                   
046000                                                                          
046100     MOVE 'W21410' TO POSTSUM-FDNAMN                                      
046200     MOVE 'W21410D3' TO POSTSUM-DDNAMN2                                   
046300     CALL POSTSUM USING POSTSUM-PARM                                      
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
046700                                                                          
046800******************************************************************        
046900*                                                                *        
047000*    BER LAGERTILLG                                              *        
047100*    BERÄKNA LAGERTILLGÅNG PER C-LAGER                           *        
047200*                                                                *        
047300******************************************************************        
047400                                                                          
047500 S30-BER-LAGERTILLG SECTION.                                              
047600                                                                          
047700     COMPUTE WS-LAGERTILLG      =                                         
047800          W01160-CLAG-KVLS      +                                         
047900          W01160-CLAG-KVAKS-CDC +                                         
048000          W01160-CLAG-KVAKS-PAV +                                         
048100          W01160-CLAG-KVAKS-T   -                                         
048200          W01160-CLAG-KVRESS    -                                         
048300          W01160-CLAG-KVROS                                               
048400     .                                                                    
048500     EJECT                                                                
048600                                                                          
048700                                                                          
048800*S31-AENDRA-TIRODAT SECTION.                                              
048900*    IF W01160-CLAG-TIRODAT > +0                                          
049000*       MOVE W01160-CLAG-TIRODAT TO WS-TIRODAT                            
049100*       IF AA2 <= K-AAR                            ?                      
049200*            MOVE  AARDEL1 TO AA1                  ?                      
049300*       ELSE                                       ?                      
049400*           COMPUTE AA1 = AARDEL1 - 1              ?                      
049500*       END-IF                                     ?                      
049600*       MOVE WS-TIRODAT TO W01160-CLAG-TIRODAT                            
049700*     END-IF                                                              
049800*    .                                                                    
049900     EJECT                                                                
050000                                                                          
050100                                                                          
050200* --- IMS SEKTIONER ---                                                   
050300                                                                          
050400                                                                          
050410 IMS-GU-WDK701 SECTION.                                                   
050420     MOVE 'IMS-GU-WDK701   '    TO DBS-SECTION                            
050430                                                                          
050440     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
050450          DELIMITED BY SIZE   INTO SSA1                                   
050460     MOVE '  GE' TO GODK-STATUSKODER                                      
050470     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
050480     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
050490     PERFORM IMS-STATUSKONTROLL                                           
050491     .                                                                    
050492                                                                          
050493 IMS-GNP-WDK711 SECTION.                                                  
050494     MOVE 'IMS-GNP-WDK711  '  TO DBS-SECTION                              
050495                                                                          
050496     MOVE 'WDK711 '        TO SSA1                                        
050497     MOVE '  GE'           TO GODK-STATUSKODER                            
050498     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
050499     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
050500     PERFORM IMS-STATUSKONTROLL                                           
050501     .                                                                    
050502                                                                          
050510 IMS-GET-BENA11-BSEQ  SECTION.                                            
050520     MOVE 'IMS-GET-BENA11-BSEQ' TO DBS-SECTION                            
050530                                                                          
050700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
050800            DELIMITED BY SIZE INTO SSA1                                   
050900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
051000            DELIMITED BY SIZE INTO SSA2                                   
051100     MOVE '  GE' TO GODK-STATUSKODER                                      
051200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
051300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800                                                                          
051900 IMS-GET-ART SECTION.                                                     
051910     MOVE 'IMS-GET-ART        ' TO DBS-SECTION                            
052000                                                                          
052100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X  ')'                        
052200          DELIMITED BY SIZE INTO SSA1                                     
052300     MOVE '  GE' TO GODK-STATUSKODER                                      
052400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
052500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSKONTROLL                                           
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000                                                                          
053100 IMS-STATUSKONTROLL SECTION.                                              
053200                                                                          
053300     SET STATUS-IX TO 1                                                   
053400     SEARCH GODK-STATUS                                                   
053500       AT END                                                             
053600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
053700         DISPLAY FELTEXT                                                  
053800         CALL FELLOG                                                      
053900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054000         CONTINUE                                                         
054100     END-SEARCH                                                           
054200     .                                                                    
054300     EJECT                                                                
054400*    -COPY WY2000P2                                                       

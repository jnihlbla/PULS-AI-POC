000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.         W2366000.                                            
000400*AUTHOR.             HENRIK ARONSSON.                                     
000500*DATE-WRITTEN.       FEB 1992.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        SKAPAR EN FIL MED ARTIKLAR/LEVERANTÖR SOM                        
001200*        HAR FÖRSENADE INLEVERANSER.                                      
001300*        FÖR VARJE SÅDAN ARTIKEL/LEVERANTÖR SKAPAS :                      
001400*        * EN POST PER FÖRSENAD INLEVERANS    (IDPTYP = SEN)              
001500*        * EN "SLUTPOST" MED NÄSTA INLEVERANS (IDPTYP = NST)              
001600*                                                                         
001700*        DESSUTOM SKAPAS EN FIL MED INFO FRÅN                             
001800*        WDD924 (LEVERANSBESKED).                                         
001900*                                                                         
002000*        PROGRAMMET LÄSER      WDD9 MED SB                                
002100*                                                                         
002200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002300*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
002400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002500*        PROGRAMMET LÄSER      WLORDQ (WDA5A)                             
002600*        PROGRAMMET LÄSER      WLINLE (WDL2)                              
002800*        PROGRAMMET LÄSER      WDF5                                       
002900*                                                                         
003000*    ABENDKODER:                                                          
003100*        U0016 -  . . . .                                                 
003200*        U1000 -  . . . .                                                 
003300*                                                                         
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     SKIP2                                                                
004100*          --- FÖRSENADE INLEVERANSER MED DIV. INFO                       
004200     SELECT W23660                   ASSIGN TO W23660D1.                  
004300*                                                                         
004400*          --- WDD924-INFO (LEVERANSBESKED)                               
004500     SELECT W23661                   ASSIGN TO W23660D2.                  
004600*                                                                         
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W23660                                                               
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*01  POST -COPY W23660   -L  -PRE W23660-                                 
005700     SKIP3                                                                
005800 FD  W23661                                                               
005900     RECORDING      F                                                     
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200*01  POST -COPY W23661   -L  -PRE W23661-                                 
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500     SKIP2                                                                
006600*    -COPY WY2000W9                                                       
006700     SKIP3                                                                
006800*    -COPY WY2000W1                                                       
006900     SKIP3                                                                
007000 77  IDPGM                       PIC X(8)    VALUE 'W2366000'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  TRAFF                       PIC X       VALUE 'N'.                   
007400 77  W-IDDC-CURR                 PIC X(2)    VALUE SPACE.                 
007600 77  WS-IDARTNR-8                PIC 9(8)    VALUE ZERO.                  
007700 77  WS-LEV-TILEVBSK-INL-1       PIC S9(7)   VALUE ZERO COMP-3.           
007800 77  WS-LEV-TILEVBSK-INL-2       PIC S9(7)   VALUE ZERO COMP-3.           
007900                                                                          
008000 77  SW-NY-ART                   PIC X       VALUE SPACE.                 
008100 77  SW-FORSENAD-INLEV-FUNNEN    PIC X       VALUE SPACE.                 
008200 77  SW-NAESTA-INLEV-FUNNEN      PIC X       VALUE SPACE.                 
008300 01  W-DAAVROP-AVS               PIC 9(6).                                
008400 01  FILLER REDEFINES W-DAAVROP-AVS.                                      
008500     03  FILLER                  PIC 9(2).                                
008600     03  W-TIAVROP-AVS           PIC 9(4).                                
008700 01  W-DALEVBSK-AVS              PIC 9(8).                                
008800 01  FILLER REDEFINES W-DALEVBSK-AVS.                                     
008900     03  FILLER                  PIC 9(2).                                
009000     03  W-DALEVBSK-AAMMDD       PIC 9(6).                                
009100*                                                                         
009200 01  FILLER                      PIC X(8)    VALUE 'SPARFÄLT'.            
009300                                                                          
009400 01  SPAR-FAELT.                                                          
009500     03  SPAR-IDLEVNR            PIC X(5).                                
009600     03  SPAR-IDFTG              PIC 9(2).                                
009700     03  SPAR-IDANSK             PIC S9(3)   COMP-3.                      
009800     03  SPAR-BEART              PIC X(25)   VALUE SPACE.                 
009900     03  SPAR-IDAVINR            PIC S9(7)   VALUE ZERO COMP-3.           
010000     03  SPAR-TIAVSDAT           PIC S9(7)   VALUE ZERO COMP-3.           
010200     03  SPAR-ANTAL-RO-RADER     PIC S9(7)   VALUE ZERO COMP-3.           
010300     03  SPAR-KVSLAGER           PIC S9(9)   VALUE ZERO COMP-3.           
010400     03  SPAR-KVLS               PIC S9(9)   VALUE ZERO COMP-3.           
010500     03  SPAR-KVAKS              PIC S9(9)   VALUE ZERO COMP-3.           
010600     03  SPAR-KVROS              PIC S9(9)   VALUE ZERO COMP-3.           
010700     03  SPAR-KVRESS             PIC S9(9)   VALUE ZERO COMP-3.           
010800     03  SPAR-KDAVRPRIO          PIC 9       VALUE ZERO.                  
010900     03  SPAR-KVAVROP-NAESTA     PIC S9(7)   VALUE ZERO COMP-3.           
011000     03  SPAR-TIAVROP-AVS-NAESTA PIC S9(7)   VALUE ZERO COMP-3.           
011100     03  SPAR-DISPONIBELT        PIC S9(7)   VALUE ZERO COMP-3.           
011200     03  SPAR-KVOKS-BULK         PIC S9(9)   VALUE ZERO COMP-3.           
011300     03  SPAR-KVOKS-DAG          PIC S9(9)   VALUE ZERO COMP-3.           
011400     03  SPAR-KVOKS-VOR          PIC S9(9)   VALUE ZERO COMP-3.           
011800                                                                          
011900 01  WS-TIAAVV                   PIC 9(4)    VALUE ZERO.                  
012000 01  DATUM-0AAVV                 PIC 9(5).                                
012100 01  FILLER REDEFINES DATUM-0AAVV.                                        
012200     03  FILLER                  PIC 9.                                   
012300     03  DATUM-AA                PIC 9(2).                                
012400     03  DATUM-VV                PIC 9(2).                                
012500 01  DATUM-TIAAVVD.                                                       
012600     03  DATUM-TIAAVV            PIC 9(4)    VALUE ZERO.                  
012700     03  FILLER                  PIC 9(1)    VALUE ZERO.                  
012800*      --- VALID IDDC CODES                                               
012900*                                                                         
013000*01    -COPY WWDC99                                                       
013100*01    -COPY WWDCKONS                                                     
013200       EJECT                                                              
013300                                                                          
013400 01  DYNAMISKA-SUBPROGRAM.                                                
013500*                                                                         
013600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
014300     EJECT                                                                
014400                                                                          
014500*    --- PARAMETRAR TILL ABEND                                            
014600                                                                          
014700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014900                                                                          
015000 01  FELTEXT.                                                             
015100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015300                                                                          
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL POSTSUM                                          
015600*                                                                         
015700*01  -COPY W0005     -PRE  POSTSUM-                                       
015800     EJECT                                                                
015900*    --- AREA FÖR WDATKONV                                                
016000*                                                                         
016100 01  FILLER                      PIC X(8)    VALUE 'WDATAREA'.            
016200*01  -COPY WDATAREA                                                       
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL DATKORT                                          
016500*                                                                         
016600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016700*01  -COPY WDATKORT                                                       
016800     EJECT                                                                
016900                                                                          
017000 01  UT-AREA-START               PIC X(24)   VALUE                        
017100                                 'UT-AREA-START  '.                       
017200     SKIP2                                                                
017300 01  UT-AREA.                                                             
017400     03  FILLER                  PIC X(200).                              
017500*01  FILLER -COPY W23660     -PRE UT-     -RED  UT-AREA                   
017600     EJECT                                                                
017700 01  UT2-AREA-START              PIC X(24)   VALUE                        
017800                                 'UT2-AREA-START '.                       
017900     SKIP2                                                                
018000 01  UT2-AREA.                                                            
018100     03  FILLER                  PIC X(200).                              
018200*01  FILLER -COPY W23661     -PRE UT2-    -RED  UT2-AREA                  
018300     EJECT                                                                
018400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018500*                                                                         
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018800     SKIP3                                                                
018900 01  NYCKLAR-TILL-DLI.                                                    
019000     03  W-IDARTNR-X.                                                     
019100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019200                                                                          
019600     03  W-IDSKYLT-X.                                                     
019700         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
019800                                                                          
019900     03  W-DAINLEV-X.                                                     
020000         05 W-DAINLEV            PIC 9(16)  VALUE ZERO.                   
020100                                                                          
020200     03  W-WDA5A1KY-MIN-X.                                                
020300         05 W-IDARTNR-MIN        PIC S9(9)   VALUE ZERO COMP-3.           
020400         05 W-IDDC-MIN           PIC X(02)   VALUE SPACE.                 
020500         05 FILLER               PIC X(33)   VALUE LOW-VALUE.             
021100                                                                          
021200     03  W-WDA5A1KY-MAX-X.                                                
021300         05 W-IDARTNR-MAX        PIC S9(9)   VALUE ZERO COMP-3.           
021400         05 W-IDDC-MAX           PIC X(02)   VALUE SPACE.                 
021410         05 FILLER               PIC X(33)   VALUE HIGH-VALUE.            
022100                                                                          
022200     03  W-IDLEVNR-X.                                                     
022300         05 W-IDLEVNR-F5         PIC X(5)    VALUE SPACE.                 
022400     SKIP2                                                                
022500*    --- STATUS-KOD FRÅN IMS                                              
022600 01  STATUS-WS                   PIC XX.                                  
022700     88  SEGMENT-FINNS                       VALUE '  '.                  
022800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023000     88  BASEN-SLUT                          VALUE 'GB'.                  
023100     SKIP2                                                                
023200 01  GODK-STATUSKODER.                                                    
023300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023400     SKIP3                                                                
023500 01  SSA1                        PIC X(128).                              
023600 01  SSA2                        PIC X(64).                               
023700     EJECT                                                                
023800*    --- IMS FUNKTIONSKODER                                               
023900*01  -COPY W0003                                                          
024000     EJECT                                                                
024100*    ---  DLI INPUT-OUTPUT AREA                                           
024200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
024300     SKIP3                                                                
024400 01  DLI-IO-AREA.                                                         
024500     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
024600     SKIP3                                                                
024700     03  WDD901   REDEFINES IO-AREA.                                      
024800*        05  -COPY WDD901  -PRE WDD901-                                   
024900     SKIP3                                                                
025000     03  WDD902   REDEFINES IO-AREA.                                      
025100*        05  -COPY WDD902  -PRE WDD902-                                   
025200     SKIP3                                                                
025600     03  WDD905   REDEFINES IO-AREA.                                      
025700*        05  -COPY WDD905  -PRE WDD905-                                   
025800     SKIP3                                                                
025900     03  WDD924   REDEFINES IO-AREA.                                      
026000*        05  -COPY WDD924  -PRE WDD924-                                   
026100     SKIP3                                                                
026200     03  WLARTM01 REDEFINES IO-AREA.                                      
026300*        05  -COPY WDK901  -PRE ARTM01-                                   
026400     SKIP3                                                                
026500     03  WLBENA11 REDEFINES IO-AREA.                                      
026600*        05  -COPY WDD311  -PRE BENA11-                                   
026700     EJECT                                                                
026800     03  WLINLE01 REDEFINES IO-AREA.                                      
026900*        05  -COPY WDL201  -PRE INLE01-                                   
027000     EJECT                                                                
027100     03  WLINLE11 REDEFINES IO-AREA.                                      
027200*        05  -COPY WDL211  -PRE INLE11-                                   
027300     EJECT                                                                
027400     03  WLINLE21 REDEFINES IO-AREA.                                      
027500*        05  -COPY WDL221  -PRE INLE21-                                   
027600     EJECT                                                                
027700     03  WLORDQ01 REDEFINES IO-AREA.                                      
027800*        05  -COPY WDA5A1  -PRE ORDQ01-                                   
027900     EJECT                                                                
028300 01  DLI-IO-AREA-01.                                                      
028400     03  IO-AREA-01              PIC X(200)  VALUE SPACE.                 
028500     SKIP3                                                                
028600     03  WLARTC01 REDEFINES IO-AREA-01.                                   
028700*        05  -COPY WDK601                                                 
028800     EJECT                                                                
028900 01  DLI-IO-AREA-11.                                                      
029000     03  IO-AREA-11              PIC X(900)  VALUE SPACE.                 
029100     SKIP3                                                                
029200     03  WLARTC11 REDEFINES IO-AREA-11.                                   
029300*        05  -COPY WDK611                                                 
029400     EJECT                                                                
029500*    ---  DLI CROSS-INDEX  AREA                                           
029600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-CIX01'.        
029700     SKIP3                                                                
029800 01  DLI-IO-WDF501.                                                       
029900*    03  -COPY WDF501                                                     
030000     EJECT                                                                
030100*    ---  DLI CROSS-INDEX  AREA                                           
030200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-CIX11'.        
030300     SKIP3                                                                
030400 01  DLI-IO-WDF502.                                                       
030500*    03  -COPY WDF502                                                     
030600     EJECT                                                                
030700*                                                                         
030800 LINKAGE SECTION.                                                         
030900*01  -COPY W0008 -PRE WDD9-                                               
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008 -PRE ARTC-                                               
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008 -PRE ARTM-                                               
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008 -PRE BENA-                                               
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008 -PRE ORDQ-                                               
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008 -PRE INLE-                                               
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
033000*01  -COPY W0008 -PRE WDF5-                                               
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300 PROCEDURE DIVISION USING  WDD9-PCB ARTC-PCB ARTM-PCB                     
033400                  BENA-PCB ORDQ-PCB INLE-PCB WDF5-PCB.                    
033500     ENTRY 'DLITCBL' USING WDD9-PCB ARTC-PCB ARTM-PCB                     
033600                  BENA-PCB ORDQ-PCB INLE-PCB WDF5-PCB.                    
033700                                                                          
033800     PERFORM A-INIT                                                       
033900                                                                          
034000     PERFORM IMS-GN-WDD9                                                  
034100     PERFORM UNTIL (SEGMENT-SAKNAS OR                                     
034200                    BASEN-SLUT)                                           
034300       EVALUATE WDD9-SEG-NAME-FB                                          
034400         WHEN 'WDD901  '                                                  
034500           PERFORM B-MOVE-WDD901                                          
034600           MOVE WDD901-IDDC TO W-IDDC-CURR                                
034700         WHEN 'WDD902  '                                                  
034710           IF W-IDDC-CURR = WC-CDC-SE                                     
034800              PERFORM C-MOVE-WDD902                                       
034810           END-IF                                                         
035100         WHEN 'WDD905  '                                                  
035110           IF W-IDDC-CURR = WC-CDC-SE                                     
035200              PERFORM E-MOVE-WDD905                                       
035210           END-IF                                                         
035300         WHEN 'WDD924  '                                                  
035310           IF W-IDDC-CURR = WC-CDC-SE                                     
035400              PERFORM F-MOVE-WDD924                                       
035410           END-IF                                                         
035500       END-EVALUATE                                                       
035600       PERFORM IMS-GN-WDD9                                                
035700     END-PERFORM                                                          
035800     PERFORM Z-FINIT                                                      
035900     MOVE ZERO TO RETURN-CODE                                             
036000     GOBACK                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 A-INIT SECTION.                                                          
036400                                                                          
036500     OPEN OUTPUT W23660                                                   
036600                 W23661                                                   
036700                                                                          
036800     MOVE NEJ  TO SW-NY-ART                                               
036900                  SW-FORSENAD-INLEV-FUNNEN                                
037000     MOVE ZERO TO W-IDARTNR                                               
037100     MOVE SPACE TO SPAR-IDLEVNR                                           
037200                                                                          
037300     MOVE WC-CDC-SE TO W-IDDC-MIN                                         
037400                       W-IDDC-MAX                                         
037500                                                                          
037600     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
037700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037800     .                                                                    
037900     EJECT                                                                
038000 B-MOVE-WDD901 SECTION.                                                   
038100                                                                          
038200     MOVE JA             TO SW-NY-ART                                     
038300     MOVE WDD901-IDARTNR TO W-IDARTNR                                     
038400                                                                          
038500     MOVE ZERO           TO SPAR-KVAVROP-NAESTA                           
038600                            SPAR-TIAVROP-AVS-NAESTA                       
038900     .                                                                    
039000     EJECT                                                                
039100 C-MOVE-WDD902 SECTION.                                                   
039200                                                                          
039300     IF SW-FORSENAD-INLEV-FUNNEN = JA                                     
039400       IF SW-NAESTA-INLEV-FUNNEN = NEJ                                    
039500* -----  INGEN NÄSTA LEVERANS HITTAD FÖR                                  
039600* -----  FÖREGÅENDE ARTIKEL/LEVERANTÖR. SKAPA NOLL-POST                   
039700                                                                          
039800         MOVE JA   TO SW-NAESTA-INLEV-FUNNEN                              
039900         MOVE ZERO TO SPAR-KVAVROP-NAESTA                                 
040000                      SPAR-TIAVROP-AVS-NAESTA                             
040100         PERFORM S11-SKAPA-SKRIV-NASTA-LEV-POST                           
040200       END-IF                                                             
040300     END-IF                                                               
040400                                                                          
040500     MOVE NEJ            TO SW-FORSENAD-INLEV-FUNNEN                      
040600                            SW-NAESTA-INLEV-FUNNEN                        
040700     MOVE WDD902-IDLEVNR TO SPAR-IDLEVNR                                  
040800     .                                                                    
040900     EJECT                                                                
041600 E-MOVE-WDD905 SECTION.                                                   
041700                                                                          
041800     IF WDD905-KDAVROP = 2                                                
041900* ÄR DETTA OK ????                                                        
042000*       AND WDD905-TIAVRDAT-INL > ZERO                                    
042100* ÄR DETTA OK ????                                                        
042200       MOVE WDD905-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
042300       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
042400       CALL WDATKONV USING          DAT-KDDATFORM                         
042500                                    DAT-I-TIDATUM                         
042600                                    DAT-O-TIDATUM                         
042700                                    DAT-KDSVAR                            
042800       IF DAT-KDSVAR-FEL                                                  
042900         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT-STR               
043000         CALL FELLOG                                                      
043100       ELSE                                                               
043200         MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                             
043300         MOVE WS-TIAAVV          TO DATUM-0AAVV                           
043400       END-IF                                                             
043500                                                                          
043600* ---- KONTROLLERA FÖRSENADE INLEVERANSER.                                
043700* ---- KONTROLL SKER MOT PLANERAD INLEVERANSVECKA                         
043800* ---- MEN POST SKAPAS MED PLANERAD AVSÄNDNINGSVECKA                      
043900                                                                          
044000       IF D-AAR = DATUM-AA                                                
044100         IF DATUM-VV NOT > D-VECKA                                        
044200           MOVE JA TO SW-FORSENAD-INLEV-FUNNEN                            
044300           PERFORM EA-SKAPA-SKRIV-FORS-INLEV-POST                         
044400         END-IF                                                           
044500       ELSE                                                               
044600         MOVE D-AAR      TO TMP1-YY                                       
044700         MOVE DATUM-AA   TO TMP2-YY                                       
044800         PERFORM WY2000P9                                                 
044900         IF TMP1-YY > TMP2-YY                                             
045000           MOVE JA TO SW-FORSENAD-INLEV-FUNNEN                            
045100           PERFORM EA-SKAPA-SKRIV-FORS-INLEV-POST                         
045200         END-IF                                                           
045300       END-IF                                                             
045400                                                                          
045500* ---- KONTROLLERA NÄSTA LEVERANS                                         
045600* ---- (PLANERAD AVSÄNDNINGSVECKA)                                        
045700                                                                          
045800       IF (SW-FORSENAD-INLEV-FUNNEN = JA) AND                             
045900          (SW-NAESTA-INLEV-FUNNEN = NEJ)                                  
046000         IF D-AAR = DATUM-AA                                              
046100           IF DATUM-VV > D-VECKA                                          
046200             MOVE JA                 TO SW-NAESTA-INLEV-FUNNEN            
046300             MOVE WDD905-KVAVROP     TO SPAR-KVAVROP-NAESTA               
046400             MOVE WDD905-DAAVROP-AVS TO W-DAAVROP-AVS                     
046500             MOVE W-TIAVROP-AVS      TO SPAR-TIAVROP-AVS-NAESTA           
046600             PERFORM S11-SKAPA-SKRIV-NASTA-LEV-POST                       
046700           END-IF                                                         
046800         ELSE                                                             
046900           MOVE D-AAR      TO TMP1-YY                                     
047000           MOVE DATUM-AA   TO TMP2-YY                                     
047100           PERFORM WY2000P9                                               
047200           IF TMP1-YY < TMP2-YY                                           
047300             MOVE JA                 TO SW-NAESTA-INLEV-FUNNEN            
047400             MOVE WDD905-KVAVROP     TO SPAR-KVAVROP-NAESTA               
047500             MOVE WDD905-DAAVROP-AVS TO W-DAAVROP-AVS                     
047600             MOVE W-TIAVROP-AVS      TO SPAR-TIAVROP-AVS-NAESTA           
047700             PERFORM S11-SKAPA-SKRIV-NASTA-LEV-POST                       
047800           END-IF                                                         
047900         END-IF                                                           
048000       END-IF                                                             
048100                                                                          
048200     END-IF                                                               
048300                                                                          
048400     .                                                                    
048500     EJECT                                                                
048600 EA-SKAPA-SKRIV-FORS-INLEV-POST SECTION.                                  
048700**************************************************************            
048800* SKAPAR EN POST MED FÖRSENAD INLEVERANS (PLANERAD AVS.VECKA)*            
048900**************************************************************            
049000                                                                          
049100     MOVE 'SEN'                   TO UT-IDPTYP                            
049200                                                                          
049300     MOVE ZERO                    TO UT-KVAVROP-NAESTA                    
049400                                     UT-TIAVROP-AVS-NAESTA                
049500                                                                          
049600     MOVE WDD905-KVAVROP          TO UT-KVAVROP-FORSENAT                  
049700     MOVE WDD905-DAAVROP-AVS      TO W-DAAVROP-AVS                        
049800     MOVE W-TIAVROP-AVS           TO UT-TIAVROP-AVS-FORSENAT              
049900     MOVE SPAR-IDLEVNR            TO UT-IDLEVNR                           
050000     MOVE W-IDARTNR               TO UT-IDARTNR                           
050100                                                                          
050200     IF SW-NY-ART = JA                                                    
050300       MOVE NEJ TO SW-NY-ART                                              
050400       PERFORM EAA-HAEMTA-ARTC-BENA-INFO                                  
050500       PERFORM EAB-HAEMTA-INLE-ORDQ-INFO                                  
050700     END-IF                                                               
050800                                                                          
050900     MOVE SPAR-IDFTG          TO UT-IDFTG                                 
051000     MOVE SPAR-IDANSK         TO UT-IDANSK                                
051100     MOVE SPAR-BEART          TO UT-BEART                                 
051200     MOVE SPAR-KDAVRPRIO      TO UT-KDAVRPRIO                             
051600     MOVE SPAR-IDAVINR        TO UT-IDAVINR                               
051700     MOVE SPAR-TIAVSDAT       TO UT-TIAVSDAT                              
051900     MOVE SPAR-ANTAL-RO-RADER TO UT-ANTAL-RO-RADER                        
052300                                                                          
052400     PERFORM EAD-HAMTA-CROSS                                              
052500                                                                          
052600     PERFORM S01-SKRIV-UTPOST-W23660                                      
052700     .                                                                    
052800     EJECT                                                                
052900 EAA-HAEMTA-ARTC-BENA-INFO SECTION.                                       
053000*************************************************                         
053100* HÄMTA INFO FRÅN ARTIKELREG SAMT BENÄMNINGSREG *                         
053200*************************************************                         
053300                                                                          
053400     PERFORM IMS-GU-ARTC01                                                
053500     MOVE ART-IDFTG     TO SPAR-IDFTG                                     
053600                                                                          
053700     PERFORM IMS-GNP-ARTC11                                               
053800     MOVE CLAG-IDANSK   TO SPAR-IDANSK                                    
053900                                                                          
054000     MOVE ZERO TO SPAR-KVSLAGER                                           
054100                  SPAR-KVRESS                                             
054200                  SPAR-KVROS                                              
054300                  SPAR-KVAKS                                              
054400                  SPAR-KVLS                                               
054500                  SPAR-KDAVRPRIO                                          
054600                                                                          
054700       ADD CLAG-KVAKS-CDC  TO SPAR-KVAKS                                  
054800       ADD CLAG-KVAKS-PAV  TO SPAR-KVAKS                                  
054900       ADD CLAG-KVAKS-T    TO SPAR-KVAKS                                  
055000       ADD CLAG-KVLS       TO SPAR-KVLS                                   
055100       ADD CLAG-KVROS      TO SPAR-KVROS                                  
055200       ADD CLAG-KVSLAGER   TO SPAR-KVSLAGER                               
055300       ADD CLAG-KVRESS     TO SPAR-KVRESS                                 
055400                                                                          
055500     PERFORM IMS-GU-ARTM01                                                
055600     IF SEGMENT-FINNS                                                     
055700       COMPUTE SPAR-KVOKS-BULK  = ARTM01-ART-KVOKS-BULK                   
055800                                                                          
055900       COMPUTE SPAR-KVOKS-DAG   = ARTM01-ART-KVOKS-DAG                    
056000                                                                          
056100       COMPUTE SPAR-KVOKS-VOR   = ARTM01-ART-KVOKS-VOR                    
056200                                                                          
056300     ELSE                                                                 
056400       MOVE ZERO TO SPAR-KVOKS-BULK                                       
056500                    SPAR-KVOKS-DAG                                        
056600                    SPAR-KVOKS-VOR                                        
056700     END-IF                                                               
056800                                                                          
056900     COMPUTE SPAR-DISPONIBELT ROUNDED = SPAR-KVLS       -                 
057000                                        SPAR-KVRESS     -                 
057100                                        SPAR-KVOKS-BULK -                 
057200                                        SPAR-KVOKS-DAG  -                 
057300                                        SPAR-KVOKS-VOR                    
057400                                                                          
057500     IF SPAR-KVROS > (SPAR-DISPONIBELT + SPAR-KVAKS)                      
057600       MOVE 1 TO SPAR-KDAVRPRIO                                           
057700     ELSE                                                                 
057800       EVALUATE TRUE                                                      
057900         WHEN SPAR-KVSLAGER > (SPAR-DISPONIBELT + SPAR-KVAKS)             
058000           MOVE 2    TO SPAR-KDAVRPRIO                                    
058100         WHEN OTHER                                                       
058200           MOVE 3    TO SPAR-KDAVRPRIO                                    
058300       END-EVALUATE                                                       
058400     END-IF                                                               
058500                                                                          
058600     PERFORM IMS-GU-BENA11-BSEQ                                           
058700     IF SEGMENT-FINNS                                                     
058800       MOVE BENA11-TEXT-BEART TO SPAR-BEART                               
058900     ELSE                                                                 
059000       MOVE SPACE             TO SPAR-BEART                               
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 EAB-HAEMTA-INLE-ORDQ-INFO SECTION.                                       
059500********************************************************                  
059600* HÄMTA INFO FRÅN INLEV-HISTORIK-REG SAMT ORDERRAD-REG *                  
059700********************************************************                  
059800                                                                          
059900* -- HÄMTA SENASTE INLEVERANS                                             
060000                                                                          
060100     MOVE ZERO TO SPAR-IDAVINR                                            
060200                  SPAR-TIAVSDAT                                           
060400                                                                          
060500     MOVE NEJ TO TRAFF                                                    
060600     PERFORM IMS-GU-INLE01                                                
060700     IF SEGMENT-FINNS                                                     
060800       PERFORM IMS-GNP-INLE11                                             
060900       PERFORM UNTIL SEGMENT-SAKNAS OR TRAFF = JA                         
061000         MOVE INLE11-INL-DAINLEV TO W-DAINLEV                             
061100         PERFORM IMS-GNP-INLE21                                           
061200         IF SEGMENT-FINNS                                                 
061300           IF INLE21-MOT-IDLEVNR  = SPAR-IDLEVNR AND                      
061400             (INLE21-MOT-KDRT NOT = 7  AND 8  AND                         
061500                                    71        AND                         
061600                                    77 AND 88) AND                        
061700             (INLE21-MOT-IDPTYP   = 'R31' OR 'R32')                       
061800               MOVE JA TO TRAFF                                           
061900           ELSE                                                           
062000             PERFORM IMS-GNP-INLE11                                       
062100           END-IF                                                         
062200         ELSE                                                             
062300           PERFORM IMS-GNP-INLE11                                         
062400         END-IF                                                           
062500       END-PERFORM                                                        
062600                                                                          
062700       IF TRAFF = JA                                                      
062800         MOVE INLE21-MOT-IDAVINR    TO SPAR-IDAVINR                       
062900         MOVE INLE21-MOT-TIAVIDAT   TO SPAR-TIAVSDAT                      
063500       END-IF                                                             
063600                                                                          
063700     END-IF                                                               
063800                                                                          
063900* -- HÄMTA ANTAL RESTORDER                                                
064000                                                                          
064100     MOVE ZERO TO SPAR-ANTAL-RO-RADER                                     
064200                                                                          
064300     IF SPAR-KVROS > ZERO                                                 
064400       MOVE LOW-VALUE  TO W-WDA5A1KY-MIN-X                                
064500       MOVE HIGH-VALUE TO W-WDA5A1KY-MAX-X                                
064600       MOVE W-IDARTNR  TO W-IDARTNR-MIN                                   
064700                          W-IDARTNR-MAX                                   
064800       PERFORM IMS-GU-ORDQ01                                              
064900       PERFORM UNTIL SEGMENT-SAKNAS                                       
065000         IF ORDQ01-SEQA-KDSTARAD = 2                                      
065100           ADD 1 TO SPAR-ANTAL-RO-RADER                                   
065200         END-IF                                                           
065300         PERFORM IMS-GN-ORDQ01                                            
065400       END-PERFORM                                                        
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
067600 EAD-HAMTA-CROSS SECTION.                                                 
067700                                                                          
067800     MOVE W-IDARTNR    TO W-IDARTNR                                       
067900     MOVE SPAR-IDLEVNR TO W-IDLEVNR-F5                                    
068000     PERFORM IMS-GET-WDF502                                               
068100     IF SEGMENT-FINNS                                                     
068200       MOVE XLEV-BELEVART TO UT-BELEVART                                  
068300     ELSE                                                                 
068400       MOVE SPACE      TO UT-BELEVART                                     
068500     END-IF                                                               
068600     .                                                                    
068700     EJECT                                                                
068800 F-MOVE-WDD924 SECTION.                                                   
068900                                                                          
069000     IF SW-FORSENAD-INLEV-FUNNEN = JA                                     
069100                                                                          
069200       MOVE SPAR-IDFTG          TO UT2-IDFTG                              
069300       MOVE SPAR-IDANSK         TO UT2-IDANSK                             
069400       MOVE SPAR-IDLEVNR        TO UT2-IDLEVNR                            
069500       MOVE W-IDARTNR           TO UT2-IDARTNR                            
069600       MOVE SPAR-KDAVRPRIO      TO UT2-KDAVRPRIO                          
069700       MOVE SPAR-ANTAL-RO-RADER TO UT2-ANTAL-RO-RADER                     
069800                                                                          
069900       MOVE 'AAMMDD'                TO DAT-KDDATFORM                      
070000       MOVE WDD924-LEV-DALEVBSK-AVS TO W-DALEVBSK-AVS                     
070100       MOVE W-DALEVBSK-AAMMDD       TO DAT-I-TIDATUM                      
070200       CALL WDATKONV USING DAT-KDDATFORM                                  
070300                           DAT-I-TIDATUM                                  
070400                           DAT-O-TIDATUM                                  
070500                           DAT-KDSVAR                                     
070600       IF DAT-KDSVAR-OK                                                   
070700         MOVE DAT-TIAAVVD TO DATUM-TIAAVVD                                
070800         MOVE DATUM-TIAAVV TO UT2-TILEVBSK-AVS                            
070900       ELSE                                                               
071000         MOVE ZERO         TO UT2-TILEVBSK-AVS                            
071100       END-IF                                                             
071200                                                                          
071300*----- TA TIDIGASTE LEV.BES VARS DATUM EJ ÄR NOLL                         
071400*----- KONTROLLERA SEDAN ATT ANTAL EJ ÄR NOLL                             
071500                                                                          
071600       MOVE WDD924-LEV-TILEVBSK-INL    TO WS-LEV-TILEVBSK-INL-1           
071700       MOVE ZERO                       TO WS-LEV-TILEVBSK-INL-2           
071800                                                                          
071900       IF WS-LEV-TILEVBSK-INL-1 = ZERO                                    
072000         MOVE +9999999 TO WS-LEV-TILEVBSK-INL-1                           
072100       END-IF                                                             
072200                                                                          
072300       IF WS-LEV-TILEVBSK-INL-2 = ZERO                                    
072400         MOVE +9999999 TO WS-LEV-TILEVBSK-INL-2                           
072500       END-IF                                                             
072600                                                                          
072700       MOVE WS-LEV-TILEVBSK-INL-1   TO TMP1-YYMMDD                        
072800       MOVE WS-LEV-TILEVBSK-INL-2   TO TMP2-YYMMDD                        
072900       PERFORM WY2000P1                                                   
073000       IF TMP1-YYMMDD <= TMP2-YYMMDD                                      
073100         IF  WDD924-LEV-KVAVIS-BSKKVAR    > ZERO                          
073200           MOVE WDD924-LEV-KVAVIS-BSKKVAR    TO UT2-KVAVIS-BSKKVAR        
073300         ELSE                                                             
073400           MOVE ZERO                         TO UT2-KVAVIS-BSKKVAR        
073500         END-IF                                                           
073600       ELSE                                                               
073700         IF  WDD924-LEV-KVAVIS-BSKKVAR    > ZERO                          
073800           MOVE WDD924-LEV-KVAVIS-BSKKVAR    TO UT2-KVAVIS-BSKKVAR        
073900         ELSE                                                             
074000           MOVE ZERO                         TO UT2-KVAVIS-BSKKVAR        
074100         END-IF                                                           
074200       END-IF                                                             
074300                                                                          
074400       PERFORM S02-SKRIV-UTPOST-W23661                                    
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 S01-SKRIV-UTPOST-W23660  SECTION.                                        
074900                                                                          
075000     WRITE W23660-POST FROM UT-AREA                                       
075100     MOVE 'W23660'   TO POSTSUM-FDNAMN                                    
075200     MOVE 'W23660D1' TO POSTSUM-DDNAMN2                                   
075300     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
075400     CALL POSTSUM USING POSTSUM-PARM                                      
075500     .                                                                    
075600     EJECT                                                                
075700 S02-SKRIV-UTPOST-W23661  SECTION.                                        
075800                                                                          
075900     WRITE W23661-POST FROM UT2-AREA                                      
076000     MOVE 'W23661'   TO POSTSUM-FDNAMN                                    
076100     MOVE 'W23660D2' TO POSTSUM-DDNAMN2                                   
076200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
076300     CALL POSTSUM USING POSTSUM-PARM                                      
076400     .                                                                    
076500     EJECT                                                                
076600 S11-SKAPA-SKRIV-NASTA-LEV-POST SECTION.                                  
076700**************************************************************            
076800* SKAPAR POST MED NÄSTA LEVERANS (AVS-VECKA) SOM "SISTA-POST"*            
076900**************************************************************            
077000                                                                          
077100* -- EFTERSOM POSTEN IFYLLD VID UTSKRIFT AV "SEN" POST                    
077200* -- ÄNDRAS ENDAST NÖDVÄNDIG DATA                                         
077300                                                                          
077400     MOVE 'NST'                   TO UT-IDPTYP                            
077500                                                                          
077600     MOVE ZERO                    TO UT-KVAVROP-FORSENAT                  
077700                                     UT-TIAVROP-AVS-FORSENAT              
077800                                                                          
077900     MOVE SPAR-KVAVROP-NAESTA     TO UT-KVAVROP-NAESTA                    
078000     MOVE SPAR-TIAVROP-AVS-NAESTA TO UT-TIAVROP-AVS-NAESTA                
078100     MOVE SPAR-IDLEVNR            TO UT-IDLEVNR                           
078200                                                                          
078300     PERFORM S01-SKRIV-UTPOST-W23660                                      
078400     .                                                                    
078500     EJECT                                                                
078600 Z-FINIT   SECTION.                                                       
078700                                                                          
078800     IF SW-FORSENAD-INLEV-FUNNEN = JA                                     
078900       IF SW-NAESTA-INLEV-FUNNEN = NEJ                                    
079000* -----  INGEN NÄSTA LEVERANS HITTAD FÖR                                  
079100* -----  SISTA ARTIKEL/LEVERANTÖR. SKAPA NOLL-POST                        
079200                                                                          
079300         MOVE JA   TO SW-NAESTA-INLEV-FUNNEN                              
079400         MOVE ZERO TO SPAR-KVAVROP-NAESTA                                 
079500                      SPAR-TIAVROP-AVS-NAESTA                             
079600         PERFORM S11-SKAPA-SKRIV-NASTA-LEV-POST                           
079700       END-IF                                                             
079800     END-IF                                                               
079900                                                                          
080000     CLOSE W23660                                                         
080100           W23661                                                         
080200                                                                          
080300     MOVE 'S' TO POSTSUM-OPKOD                                            
080400     CALL POSTSUM USING POSTSUM-PARM                                      
080500     .                                                                    
080600     EJECT                                                                
080700* --- IMS SEKTIONER ---                                                   
080800                                                                          
080900 IMS-GN-WDD9 SECTION.                                                     
081000                                                                          
081100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
081200     CALL CBLTDLI USING GN WDD9-PCB IO-AREA                               
081300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
081400     PERFORM IMS-STATUSKONTROLL                                           
081500     .                                                                    
081600     EJECT                                                                
081700 IMS-GU-ARTC01 SECTION.                                                   
081800                                                                          
081900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
082000     DELIMITED BY SIZE INTO SSA1                                          
082100     MOVE '  ' TO GODK-STATUSKODER                                        
082200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
082300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     SKIP3                                                                
082700 IMS-GNP-ARTC11 SECTION.                                                  
082800                                                                          
082900     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
083000     MOVE '  ' TO GODK-STATUSKODER                                        
083100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
083200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     EJECT                                                                
083600 IMS-GU-ARTM01 SECTION.                                                   
083700                                                                          
083800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
083900     DELIMITED BY SIZE INTO SSA1                                          
084000     MOVE '  GE' TO GODK-STATUSKODER                                      
084100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA SSA1                      
084200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
084300     PERFORM IMS-STATUSKONTROLL                                           
084400     .                                                                    
084500     EJECT                                                                
084600 IMS-GU-BENA11-BSEQ SECTION.                                              
084700                                                                          
084800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
084900            DELIMITED BY SIZE INTO SSA1                                   
085000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
085100            DELIMITED BY SIZE INTO SSA2                                   
085200     MOVE '  ' TO GODK-STATUSKODER                                        
085300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
085400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
085500     PERFORM IMS-STATUSKONTROLL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 IMS-GU-INLE01 SECTION.                                                   
085900                                                                          
086000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
086100             DELIMITED BY SIZE INTO SSA1                                  
086200     MOVE '  GE' TO GODK-STATUSKODER                                      
086300     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA SSA1                      
086400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     SKIP3                                                                
086800 IMS-GNP-INLE11 SECTION.                                                  
086900                                                                          
087000     MOVE 'WLINLE11' TO SSA1                                              
087100     MOVE '  GE' TO GODK-STATUSKODER                                      
087200     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1                     
087300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
087400     PERFORM IMS-STATUSKONTROLL                                           
087500     .                                                                    
087600     SKIP3                                                                
087700 IMS-GNP-INLE21 SECTION.                                                  
087800                                                                          
087900     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
088000             DELIMITED BY SIZE INTO SSA1                                  
088100     MOVE 'WLINLE21' TO SSA2                                              
088200     MOVE '  GE' TO GODK-STATUSKODER                                      
088300     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
088400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700     EJECT                                                                
090800 IMS-GN-ORDQ01 SECTION.                                                   
090900                                                                          
091000     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
091100                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X ')'                    
091200             DELIMITED BY SIZE INTO SSA1                                  
091300     MOVE '  GE' TO GODK-STATUSKODER                                      
091400     CALL CBLTDLI USING GN ORDQ-PCB DLI-IO-AREA SSA1                      
091500     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     SKIP3                                                                
091900 IMS-GU-ORDQ01 SECTION.                                                   
092000                                                                          
092100     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
092200                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X ')'                    
092300             DELIMITED BY SIZE INTO SSA1                                  
092400     MOVE '  GE' TO GODK-STATUSKODER                                      
092500     CALL CBLTDLI USING GU ORDQ-PCB DLI-IO-AREA SSA1                      
092600     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
092700     PERFORM IMS-STATUSKONTROLL                                           
092800     .                                                                    
092900     EJECT                                                                
093000 IMS-GET-WDF502 SECTION.                                                  
093100                                                                          
093200     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
093300          DELIMITED BY SIZE INTO SSA1                                     
093400     STRING 'WDF502  *L(IDLEVNR  =' W-IDLEVNR-X ')'                       
093500          DELIMITED BY SIZE INTO SSA2                                     
093600     MOVE '  GE' TO GODK-STATUSKODER                                      
093700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
093800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100     EJECT                                                                
094200 IMS-STATUSKONTROLL  SECTION.                                             
094300                                                                          
094400     SET STATUS-IX TO 1                                                   
094500     SEARCH GODK-STATUS AT END CALL FELLOG                                
094600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
094700     CONTINUE                                                             
094800     END-SEARCH                                                           
094900     .                                                                    
095000     EJECT                                                                
095100*    -COPY WY2000P1                                                       
095200     EJECT                                                                
095300*    -COPY WY2000P9                                                       

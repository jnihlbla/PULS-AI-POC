000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2215900.                                                
000300 AUTHOR.         EGHOLT CONNY.                                            
000400 DATE-WRITTEN.   2007-01-02.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        Uppdaterar WDD6 med leveransplaneförslag, som primärt            
000900*        - skapats av W2215000 i W221J023. CDC                            
001000*        - skapats av W2241400 i W224V1 för Kina.                         
001100*                                                                         
001200*        WDD6 innehåller en "kö" av obehandlade leveransplane-            
001300*        förslag, som dels skapats i veckobatchen, dels av                
001400*        bild 2135 (egna förslag).                                        
001500*        - Kina: veckobatch W224V1 och on-line 2403/W224OMSP.             
001600*                                                                         
001700*        Förslagen på WDD6 administreras på bild 2147 som skickar         
001800*        valda förslag till 2103 där de behandlas och tas bort.           
001900*        De tas även bort av W2215600 och W2214000.                       
002000*        - Kina: administreras på bild 2447 och 2403.Förslagen            
002100*          tas bort av W2245600 och W2241200 i W224V1.                    
002200*                                                                         
002300*        Samma pgm används för både CDC och Kina.                         
002400*        INFIL CDC:  W221.W200V1.W22159                                   
002500*        INFIL KINA: W224.W224V1.W22416                                   
002600*                                                                         
002700*        PROGRAMMET  UPPDATERAR WDD6                                      
002800*                                                                         
002900*        (kod förberedd för konvertering till BMP)                        
003000*                                                                         
003100*    ABENDKODER:                                                          
003200*        U0016 -  . . . .                                                 
003300*        U1000 -  . . . .                                                 
003400*                                                                         
003500*                                                                         
003600**** CHANGES:                                                             
003700*    2013-02-25 E-TRACKER 10143273 LOCAL SOURCING CHINA                   
003800*                                  OMGJORD TILL BMP.                      
003900*                                                                         
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP2                                                                
004400 INPUT-OUTPUT Section.                                                    
004500                                                                          
004600 FILE-CONTROL.                                                            
004700     SKIP2                                                                
005300 DATA DIVISION.                                                           
005400     SKIP2                                                                
005700 FILE Section.                                                            
005800     SKIP3                                                                
006500                                                                          
006600 WORKING-STORAGE Section.                                                 
006700                                                                          
006800 77  IDPGM                       Pic X(8)    Value 'W2215900'.            
006900 01  CHKP-VAR.                                                            
007000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
007100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
007200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
007300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
007400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
007500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
007510     03 GSAM-PCB-LENGTH          PIC S9(9)   VALUE +48 COMP SYNC.         
                                                                                
007520 77  TYPE-OF-RUN                 PIC X       VALUE 'N'.                   
007530     88  NORMAL-RUN                          VALUE 'N'.                   
007540     88  RESTART-RUN                         VALUE 'R'.                   
                                                                                
007600 77  JA                          Pic X       Value 'J'.                   
007700 77  OCH                         Pic X       Value '&'.                   
007800 77  NEJ                         Pic X       Value 'N'.                   
007900 77  WS-ANT                      PIC 9(3)    VALUE ZERO.                  
008000                                                                          
008100 77  W22159-EOF-SW               Pic X       Value 'N'.                   
008200     88  END-OF-W22159                       Value 'J'.                   
008300                                                                          
008400 77  PROPOSAL-SW                 Pic X       Value 'J'.                   
008500     88  VALID-PROPOSAL                      Value 'J'.                   
008600     88  DELETE-PROPOSAL                     Value 'N'.                   
008700                                                                          
008800 77  OMSPEC-SW                   PIC X       VALUE ' '.                   
008900     88  OMSPEC-FINNS                        Value 'J'.                   
009000     88  OMSPEC-SAKNAS                       Value 'N'.                   
009100     EJECT                                                                
009200 01  DAGENS-DATUM                Pic 9(6)    Value Zero.                  
009300 01  Filler Redefines DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        Pic 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     Pic 9(2).                                
009600     03  DAGENS-DATUM-DAG        Pic 9(2).                                
009700                                                                          
009800 01  W-DAGENS-AAAAVV             PIC 9(6)    VALUE 200000.                
009900 01  FILLER REDEFINES W-DAGENS-AAAAVV.                                    
010000     03 FILLER                   PIC 9(2).                                
010100     03 W-DAGENS-VECKA           PIC 9(4).                                
010200 01  W-DAAVROP-SHP               PIC 9(6)    VALUE 200000.                
010300 01  FILLER REDEFINES W-DAAVROP-SHP.                                      
010400     03 FILLER                   PIC 9(2).                                
010500     03 W-DAAVROP-SHP-AAVV       PIC 9(4).                                
010600 01  W-DAAVROP-LIM               PIC 9(6)    VALUE 200000.                
010700 01  FILLER REDEFINES W-DAAVROP-LIM.                                      
010800     03 FILLER                   PIC 9(2).                                
010900     03 W-DAAVROP-LIM-AAVV       PIC 9(4).                                
011000 01  W-KVVECKOR-LT               PIC 9(3).                                
011100     EJECT                                                                
011200                                                                          
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400*                                                                         
011500     03  ABEND                   Pic X(8)    Value 'ABEND'.               
011600     03  CBLTDLI                 Pic X(8)    Value 'CBLTDLI '.            
011700     03  FELLOG                  Pic X(8)    Value 'FELLOG  '.            
011800     03  POSTSUM                 Pic X(8)    Value 'POSTSUM'.             
011900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012000     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
012100     SKIP2                                                                
012200*    --- PARAMETRAR TILL ABEND                                            
012300                                                                          
012400 77  RKOD-ABEND                  Pic S9(4)   Comp Value +0.               
012500 77  RKOD-ABEND-UTAN-DUMP        Pic S9(4)   Comp Value +16.              
012600 77  RKOD-ABEND-MED-DUMP         Pic S9(4)   Comp Value +1000.            
012700     SKIP2                                                                
012800 01  FELTEXT.                                                             
012900     03  Filler                  Pic X(8)    Value 'FELTEXT'.             
013000     03  FELTEXT-STR             Pic X(72)   Value Space.                 
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL POSTSUM                                          
013300*                                                                         
013400*01  -COPY W0005   -PRE  POSTSUM-                                         
013500     EJECT                                                                
013600**** VARIABLER TILL WDATKONV****                                          
013700*01  -COPY WDATAREA.                                                      
013800                                                                          
013900**** VARIABLER TILL W009VADD****                                          
014000 01  W009VADD-AAVV               PIC S9(5)    COMP-3.                     
014100 01  W009VADD-ANTAL              PIC S9(3)    COMP-3.                     
014200     EJECT                                                                
014300                                                                          
014400*01  -COPY WWDCKONS                                                       
014500*01  -COPY WWDC99                                                         
014600                                                                          
014700 01  IN-AREA-START               Pic X(24)   Value                        
014800                                 'IN-AREA-START  '.                       
014900     SKIP2                                                                
015000                                                                          
015100*01  AREA -COPY WDD601     -PRE IN-.                                      
015200     EJECT                                                                
015300     03 FILLER  REDEFINES  IN-LPF-WDD601.                                 
015400        05 IN-LPF-WDD601KY   PIC X(14).                                   
015500        05 FILLER            PIC X(49).                                   
015600     EJECT                                                                
015700                                                                          
015710 01  IN-REC.                                                              
015730     03  IN-DATA                 PIC X(3000).                             
015800                                                                          
015900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016000*                                                                         
016100     EJECT                                                                
016200 01  Filler                      Pic X(16)   Value 'IMS-WS'.              
016300     SKIP3                                                                
016400 01  NYCKLAR-TILL-DLI.                                                    
016500     03  W-WDD601KY-MIN-X.                                                
016600         05  W-IDDC-MIN        Pic X(2)         Value Space.              
016700         05  W-IDLEVNR-MIN     Pic X(5)         Value Space.              
016800         05  W-IDARTNR-MIN     Pic S9(9) Comp-3 Value Zero.               
016900         05  W-IDANSK-MIN      Pic S9(3) Comp-3 Value Zero.               
017000     03  W-WDD601KY-MAX-X.                                                
017100         05  W-IDDC-MAX        Pic X(2)         Value Space.              
017200         05  W-IDLEVNR-MAX     Pic X(5)         Value Space.              
017300         05  W-IDARTNR-MAX     Pic S9(9) Comp-3 Value +999999999.         
017400         05  W-IDANSK-MAX      Pic S9(3) Comp-3 Value Zero.               
017500     03  W-IDARTNR-X.                                                     
017600         05  W-IDARTNR         PIC S9(9) Comp-3 Value Zero.               
017700     03  W-KDSEGKEY-X.                                                    
017800         05  W-KDSEGKEY        PIC X(1)         VALUE '1'.                
017900     03  W-WDD901KY-X.                                                    
018000         05 W-IDARTNR-D9       PIC S9(9) COMP-3 Value Zero.               
018100         05 W-IDDC-D9          PIC X(2)         Value Space.              
018200     03  W-IDLEVNR-X.                                                     
018300         05  W-IDLEVNR-D9      PIC X(5)         Value Space.              
018400     03  W-DAAVROP-MIN-X.                                                 
018500         05 W-DAAVROP-D9-MIN   PIC 9(6)         Value Zero.               
018600     03  W-DAAVROP-MAX-X.                                                 
018700         05 W-DAAVROP-D9-MAX   PIC 9(6)         Value Zero.               
018800     03  W-DAAVROP-X.                                                     
018900         05 W-DAAVROP-D9       PIC 9(6)         Value Zero.               
019000     03  W-KDAVROP-X.                                                     
019100         05  W-KDAVROP-D9      PIC S9(1) COMP-3 VALUE ZERO.               
019200     03  W-KDAVROP-MIN-X.                                                 
019300         05  W-KDAVROP-D9-MIN  PIC S9(1) COMP-3 VALUE 1.                  
019400     03  W-KDAVROP-MAX-X.                                                 
019500         05  W-KDAVROP-D9-MAX  PIC S9(1) COMP-3 VALUE 2.                  
019600     SKIP2                                                                
019700*    --- STATUS-KOD FRÅN IMS                                              
019800 01  STATUS-WS                   Pic XX.                                  
019900     88  SEGMENT-FINNS                       Value '  '.                  
020000     88  SEGMENT-FINNS-REDAN                 Value 'II'.                  
020100     88  SEGMENT-SAKNAS                      Value 'GE'.                  
020200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020300     88  IMS-EJ-OK                           VALUE 'XD'.                  
020400     SKIP2                                                                
020500 01  GODK-STATUSKODER.                                                    
020600     03  GODK-STATUS Occurs 5 Indexed By STATUS-IX Pic XX.                
020700     SKIP3                                                                
020800 01  ALL-SSA.                                                             
020900     03 SSA1                     Pic X(128).                              
021000     03 SSA2                     Pic X(128).                              
021100     03 SSA3                     Pic X(256).                              
021200     EJECT                                                                
021300*    --- IMS FUNKTIONSKODER                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600*    ---  DLI INPUT-OUTPUT AREA                                           
021700 01  Filler         Pic X(16) Value 'DLI-IO-WDD601'.                      
021800 01  DLI-IO-WDD601.                                                       
021900*    03  -COPY WDD601                                                     
022000     EJECT                                                                
022100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022200 01  DLI-IO-WDK601.                                                       
022300*    03  -COPY WDK601                                                     
022400     EJECT                                                                
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
022600 01  DLI-IO-WDK611.                                                       
022700*    03  -COPY WDK611                                                     
022800     EJECT                                                                
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
023000 01  DLI-IO-WDD901.                                                       
023100*    03  -COPY WDD901 -PRE D901-                                          
023200     EJECT                                                                
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
023400 01  DLI-IO-WDD902.                                                       
023500*    03  -COPY WDD902 -PRE D902-                                          
023600     EJECT                                                                
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
023800 01  DLI-IO-WDD904.                                                       
023900*    03  -COPY WDD904 -PRE D904-                                          
024000     EJECT                                                                
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
024200 01  DLI-IO-WDD905.                                                       
024300*    03  -COPY WDD905 -PRE D905-                                          
024400     EJECT                                                                
024500                                                                          
024600 LINKAGE Section.                                                         
024700                                                                          
024800*01  -COPY W0009  -PRE MSG-                                               
024900                                                                          
025000*01  -COPY W0008  -PRE WDD6-                                              
025100     05  Filler                  Pic X.                                   
025200*01  -COPY W0008  -PRE WDK6-                                              
025300     05  Filler                  Pic X.                                   
025400*01  -COPY W0008  -PRE WDD9-                                              
025500     05  Filler                  Pic X.                                   
025510*01  -COPY W0008  -PRE IN-GSAM-                                           
025520     05  KEYFB-RSA               PIC X(12).                               
025600     EJECT                                                                
025700                                                                          
025800 PROCEDURE DIVISION  Using                                                
025900                           MSG-PCB                                        
026000                           WDD6-PCB                                       
026100                           WDK6-PCB                                       
026200                           WDD9-PCB                                       
026210                           IN-GSAM-PCB.                                   
026300 MAIN Section.                                                            
026400     Entry 'DLITCBL' Using                                                
026500                           MSG-PCB                                        
026600                           WDD6-PCB                                       
026700                           WDK6-PCB                                       
026800                           WDD9-PCB                                       
026810                           IN-GSAM-PCB.                                   
026900     PERFORM A-INIT                                                       
027000                                                                          
027110     IF NORMAL-RUN                                                        
027120       PERFORM IMS-GN-INDATA                                              
027130     ELSE                                                                 
027140       PERFORM IMS-GU-INDATA                                              
027150     END-IF                                                               
                                                                                
027200     PERFORM UNTIL END-OF-W22159                                          
029800       IF CHKP-ANT = CHKP-MAX                                             
029900         PERFORM X-TAG-CHECKPOINT                                         
030000       END-IF                                                             
                                                                                
027400       SET VALID-PROPOSAL    To TRUE                                      
027500       MOVE ZERO             TO WS-ANT                                    
027600                                                                          
027700       Move IN-LPF-WDD601KY  To W-WDD601KY-MIN-X                          
027800                                W-WDD601KY-MAX-X                          
027900       Move Zero             To W-IDANSK-MIN                              
028000       Move +999             To W-IDANSK-MAX                              
028100       MOVE IN-LPF-IDDC      To WS-IDDC                                   
028200*      --- Man skall inte bry sig om värdet på anskaffare                 
028300*      --- Endast IDLEVNR och IDARTNR är relevant.                        
028400                                                                          
028500       display 'Key1> ' W-IDLEVNR-MIN ' ' W-IDARTNR-MIN                   
028600       ' ' W-IDANSK-MIN                                                   
028700       display 'Key2< ' W-IDLEVNR-MAX ' ' W-IDARTNR-MAX                   
028800       ' ' W-IDANSK-MAX                                                   
028900                                                                          
029000       PERFORM B-CHECK-PROPOSAL-EXIST                                     
029100                                                                          
029200       IF  VALID-PROPOSAL                                                 
029300       AND CDC-SE                                                         
029400           PERFORM C-VALIDATE-PROPOSAL                                    
029500       END-IF                                                             
029600       PERFORM D-UPD-PROPOSAL-WDD6                                        
029700                                                                          
             ADD +1            TO CHKP-ANT                                      
030110       PERFORM IMS-GN-INDATA                                              
030200     End-Perform                                                          
030300                                                                          
030400                                                                          
030500     Perform Z-FINIT                                                      
030600                                                                          
030700     Move Zero To RETURN-CODE                                             
030800     Goback                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 A-INIT Section.                                                          
031200     SKIP2                                                                
031300     Perform IMS-RESTART                                                  
031600                                                                          
031700     Accept DAGENS-DATUM  From DATE                                       
031800                                                                          
031900     MOVE 'IDAG'            TO DAT-KDDATFORM                              
032000     CALL WDATKONV USING       DAT-KDDATFORM                              
032100                               DAT-I-TIDATUM                              
032200                               DAT-O-TIDATUM                              
032300                               DAT-KDSVAR                                 
032400                                                                          
032500     MOVE DAT-TIAAVV-GRP    TO W-DAGENS-VECKA                             
032600                                                                          
032700     Move IDPGM To POSTSUM-PROGNAMN                                       
032800     .                                                                    
032900     EJECT                                                                
033000 B-CHECK-PROPOSAL-EXIST SECTION.                                          
033100                                                                          
033200     MOVE IN-LPF-IDARTNR       TO W-IDARTNR-D9                            
033300     MOVE IN-LPF-IDDC          TO W-IDDC-D9                               
033400     MOVE IN-LPF-IDLEVNR       TO W-IDLEVNR-D9                            
033500                                                                          
033600*    CHECK IF PROPOSAL OR DELIVERY SCHEDULE EXISTS                        
033700     PERFORM IMS-GU-WDD905-KDAVROP                                        
033800     IF SEGMENT-SAKNAS                                                    
033900        PERFORM IMS-GU-WDD902                                             
034000        IF SEGMENT-FINNS                                                  
034100           PERFORM IMS-GNP-WDD904                                         
034200           IF SEGMENT-SAKNAS                                              
034300              SET DELETE-PROPOSAL  TO TRUE                                
034400              display 'omspec saknas'                                     
034500           END-IF                                                         
034600        END-IF                                                            
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 C-VALIDATE-PROPOSAL SECTION.                                             
035100                                                                          
035200     IF IN-LPF-KDLEVPLF = 'J'                                             
035300        PERFORM CA-CALC-FIRST-SHIP-WEEK                                   
035400        PERFORM CB-CHECK-AVROP                                            
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 CA-CALC-FIRST-SHIP-WEEK SECTION.                                         
035900                                                                          
036000*    GET LEADTIME TO SHIP                                                 
036100     MOVE IN-LPF-IDARTNR       TO W-IDARTNR                               
036200                                                                          
036300     PERFORM IMS-GU-WDK611                                                
036400     IF SEGMENT-FINNS                                                     
036500                                                                          
036600        MOVE CLAG-KVVECKOR-LT  TO W-KVVECKOR-LT                           
036700                                                                          
036800*    First shipping week = current week + leadtime                        
036900        MOVE W-DAGENS-VECKA    TO W009VADD-AAVV                           
037000        MOVE W-KVVECKOR-LT     TO W009VADD-ANTAL                          
037100        CALL W009VADD       USING W009VADD-AAVV                           
037200                                  W009VADD-ANTAL                          
037300        MOVE W009VADD-AAVV     TO W-DAAVROP-SHP-AAVV                      
037400                                                                          
037500*    Add 10 Weeks to shipping week                                        
037600        MOVE W-DAAVROP-SHP-AAVV TO W009VADD-AAVV                          
037700        MOVE 10                 TO W009VADD-ANTAL                         
037800        CALL W009VADD       USING W009VADD-AAVV                           
037900                                  W009VADD-ANTAL                          
038000        MOVE W009VADD-AAVV     TO W-DAAVROP-LIM-AAVV                      
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 CB-CHECK-AVROP SECTION.                                                  
038500                                                                          
038600     MOVE W-IDARTNR            TO W-IDARTNR-D9                            
038700     MOVE IN-LPF-IDLEVNR       TO W-IDLEVNR-D9                            
038800     MOVE WC-CDC-SE            TO W-IDDC-D9                               
038900     MOVE W-DAAVROP-SHP        TO W-DAAVROP-D9-MIN                        
039000     MOVE W-DAAVROP-LIM        TO W-DAAVROP-D9-MAX                        
039100                                  W-DAAVROP-D9                            
039200     MOVE 1                    TO W-KDAVROP-D9                            
039300                                                                          
039400     PERFORM IMS-GU-WDD905-IN-LT                                          
039500     IF SEGMENT-FINNS                                                     
039600        CONTINUE                                                          
039700     ELSE                                                                 
039800        PERFORM IMS-GU-WDD905-QUAL                                        
039900        IF SEGMENT-FINNS                                                  
040000           CONTINUE                                                       
040100        ELSE                                                              
040200           PERFORM IMS-GU-WDD905-GT-LIM-DAT                               
040300           IF SEGMENT-FINNS                                               
040400              SET DELETE-PROPOSAL TO TRUE                                 
040500           END-IF                                                         
040600        END-IF                                                            
040700     END-IF                                                               
040800                                                                          
040900     IF DELETE-PROPOSAL                                                   
041000        PERFORM IMS-GU-WDD902                                             
041100        IF SEGMENT-FINNS                                                  
041200           PERFORM IMS-GHNP-WDD904                                        
041300           IF SEGMENT-FINNS                                               
041400              PERFORM IMS-DELETE-OMSPEC                                   
041500              PERFORM CBA-UPD-ARTIKELREG                                  
041600              display 'delete omspec ok'                                  
041700           END-IF                                                         
041800           PERFORM IMS-GHNP-WDD905                                        
041900           PERFORM UNTIL SEGMENT-SAKNAS                                   
042000             PERFORM IMS-DELETE-AVROP                                     
042100             PERFORM IMS-GHNP-WDD905                                      
042200             ADD +1           TO WS-ANT                                   
042300           END-PERFORM                                                    
042400***  BELOW COUNTER  DISPLAY NUMBER OF LINES DELETED FROM                  
042500***  AVROP SEGMENT WDD905. CAN BE USED FOR ANALYZING ISSUES               
042600           IF WS-ANT > ZERO                                               
042700              DISPLAY 'LINES DELETED :' WS-ANT                            
042800           END-IF                                                         
042900        END-IF                                                            
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 CBA-UPD-ARTIKELREG SECTION.                                              
043400                                                                          
043500     PERFORM IMS-GHU-WDK611                                               
043600     IF SEGMENT-FINNS                                                     
043700        MOVE ZERO      TO CLAG-KDLPSP                                     
043800        PERFORM IMS-REPL-WDK611                                           
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 D-UPD-PROPOSAL-WDD6  SECTION.                                            
044300                                                                          
044400     IF DELETE-PROPOSAL                                                   
044500        PERFORM IMS-GHU-WDD601                                            
044600                                                                          
044700        DISPLAY 'GHU for DLET STATUS= ' STATUS-WS                         
044800                                                                          
044900        IF SEGMENT-FINNS                                                  
045000           PERFORM IMS-DLET-WDD601                                        
045300           MOVE 'W22159' TO POSTSUM-FDNAMN                                
045400           MOVE 'WDD601' TO POSTSUM-DDNAMN2                               
045500           MOVE 'DLET' TO POSTSUM-TRANSTYP                                
045600           CALL POSTSUM USING POSTSUM-PARM                                
045700        END-IF                                                            
045800     ELSE                                                                 
045900        PERFORM IMS-GHU-WDD601                                            
046000                                                                          
046100        DISPLAY 'GHU for REPL STATUS= ' STATUS-WS                         
046200                                                                          
046300        IF SEGMENT-FINNS                                                  
046400           PERFORM IMS-DLET-WDD601                                        
046600           MOVE IN-LPF-WDD601  TO DLI-IO-WDD601                           
046700           PERFORM IMS-ISRT-WDD601                                        
047000           MOVE 'W22159' TO POSTSUM-FDNAMN                                
047100           MOVE 'WDD601' TO POSTSUM-DDNAMN2                               
047200           MOVE 'REPL' TO POSTSUM-TRANSTYP                                
047300           CALL POSTSUM USING POSTSUM-PARM                                
047400        ELSE                                                              
047500           MOVE IN-LPF-WDD601  TO DLI-IO-WDD601                           
047600           PERFORM IMS-ISRT-WDD601                                        
047900           MOVE 'W22159' TO POSTSUM-FDNAMN                                
048000           MOVE 'WDD601' TO POSTSUM-DDNAMN2                               
048100           MOVE 'ISRT' TO POSTSUM-TRANSTYP                                
048200           CALL POSTSUM USING POSTSUM-PARM                                
048300        END-IF                                                            
048400     END-IF                                                               
048500     .                                                                    
048600     EJECT                                                                
048700 Z-FINIT Section.                                                         
048900     SKIP2                                                                
049000     Move 'S' To POSTSUM-OPKOD                                            
049100     Call POSTSUM Using POSTSUM-PARM                                      
049200     .                                                                    
049300     EJECT                                                                
051510 IMS-GN-INDATA SECTION.                                                   
051520                                                                          
051521     INITIALIZE IN-REC                                                    
051530     MOVE 'IMS-GN-INDATA'        TO SSA1                                  
051540     MOVE '  GEGB'               TO GODK-STATUSKODER                      
051550     CALL CBLTDLI             USING GN                                    
051560                                    IN-GSAM-PCB                           
051570                                    IN-REC                                
051580     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
051590     IF SEGMENT-FINNS                                                     
051594       MOVE IN-DATA              TO IN-AREA                               
             DISPLAY 'IN-AREA     ' IN-AREA                                     
051595       MOVE 'W22159' TO POSTSUM-FDNAMN                                    
051596       Move 'W22159D1' TO POSTSUM-DDNAMN2                                 
051597       MOVE 'IN-'     TO POSTSUM-TRANSTYP                                 
051598       CALL POSTSUM USING POSTSUM-PARM                                    
051599     ELSE                                                                 
051600       SET END-OF-W22159         TO TRUE                                  
051601       MOVE SPACES               TO IN-AREA                               
051602     END-IF                                                               
051603     PERFORM IMS-STATUSKONTROLL                                           
051604     .                                                                    
051610     EJECT                                                                
                                                                                
051640 IMS-GU-INDATA SECTION.                                                   
051650                                                                          
051660* EXECUTES DURING A RESTART TO READ THE FIRST RECORD AFTER THE            
051670* LAST CHECKPOINT                                                         
051680                                                                          
051690     MOVE 'IMS-GU-INDATA'        TO SSA1                                  
051691     MOVE '  GEGB'               TO GODK-STATUSKODER                      
051692     CALL CBLTDLI             USING GU                                    
051693                                    IN-GSAM-PCB                           
051694                                    IN-REC                                
051695                                    KEYFB-RSA                             
051696     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
051697     IF SEGMENT-FINNS                                                     
051701       MOVE IN-DATA              TO IN-AREA                               
051702       Move 'W22159' To POSTSUM-FDNAMN                                    
051703       Move 'W22159D1' To POSTSUM-DDNAMN2                                 
051704       Move 'IN-'     To POSTSUM-TRANSTYP                                 
051705       Call POSTSUM Using POSTSUM-PARM                                    
051706     ELSE                                                                 
051707       SET END-OF-W22159         TO TRUE                                  
051708       MOVE SPACES               TO IN-AREA                               
051709     END-IF                                                               
051710     PERFORM IMS-STATUSKONTROLL                                           
051711     .                                                                    
051720 X-TAG-CHECKPOINT   SECTION.                                              
051800                                                                          
051900     PERFORM IMS-CHECKPOINT                                               
052100     .                                                                    
052200     EJECT                                                                
052300*                                                                         
052400* --- IMS SEKTIONER ---                                                   
052500*                                                                         
052600 IMS-GHU-WDD601 Section.                                                  
052700                                                                          
052800     MOVE SPACE                TO ALL-SSA                                 
052900     String 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
053000                 OCH 'WDD601KY<=' W-WDD601KY-MAX-X ')'                    
053100          Delimited By Size Into SSA1                                     
053200     Move '  GE' To GODK-STATUSKODER                                      
053300     Call CBLTDLI Using GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
053400     Move WDD6-STATUS-CODE To STATUS-WS                                   
053500     Perform IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     SKIP3                                                                
053800 IMS-ISRT-WDD601 Section.                                                 
053900                                                                          
054000     MOVE SPACE     TO ALL-SSA                                            
054100     Move 'WDD601 ' To SSA1                                               
054200     Move '  ' To GODK-STATUSKODER                                        
054300     Call CBLTDLI Using ISRT WDD6-PCB DLI-IO-WDD601 SSA1                  
054400     Move WDD6-STATUS-CODE To STATUS-WS                                   
054500     Perform IMS-STATUSKONTROLL                                           
054600     .                                                                    
054700     SKIP3                                                                
054800 IMS-DLET-WDD601 Section.                                                 
054900                                                                          
055000     Move '  ' To GODK-STATUSKODER                                        
055100     Call CBLTDLI Using DLET WDD6-PCB DLI-IO-WDD601                       
055200     Move WDD6-STATUS-CODE To STATUS-WS                                   
055300     Perform IMS-STATUSKONTROLL                                           
055400     .                                                                    
055500     EJECT                                                                
055600 IMS-GU-WDK611 SECTION.                                                   
055700                                                                          
055800     MOVE SPACE                TO ALL-SSA                                 
055900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
056000          DELIMITED BY SIZE INTO SSA1                                     
056100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
056200          DELIMITED BY SIZE INTO SSA2                                     
056300     MOVE '  GE' TO GODK-STATUSKODER                                      
056400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
056500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
056600     PERFORM IMS-STATUSKONTROLL                                           
056700     .                                                                    
056800     EJECT                                                                
056900 IMS-GHU-WDK611 SECTION.                                                  
057000                                                                          
057100     MOVE SPACE                TO ALL-SSA                                 
057200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
057300          DELIMITED BY SIZE INTO SSA1                                     
057400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
057500          DELIMITED BY SIZE INTO SSA2                                     
057600     MOVE '  GE' TO GODK-STATUSKODER                                      
057700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
057800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057900     PERFORM IMS-STATUSKONTROLL                                           
058000     .                                                                    
058100     EJECT                                                                
058200 IMS-REPL-WDK611 SECTION.                                                 
058300                                                                          
058400     MOVE '  '  TO GODK-STATUSKODER                                       
058500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
058600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900     EJECT                                                                
059000 IMS-GU-WDD905-KDAVROP  SECTION.                                          
059100                                                                          
059200     MOVE SPACE                TO ALL-SSA                                 
059300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
059400            DELIMITED BY SIZE INTO SSA1                                   
059500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
059600            DELIMITED BY SIZE INTO SSA2                                   
059700     STRING 'WDD905  (KDAVROP >=' W-KDAVROP-MIN-X                         
059800                    '&KDAVROP =<' W-KDAVROP-MAX-X ')'                     
059900            DELIMITED BY SIZE INTO SSA3                                   
060000     MOVE '  GE' TO GODK-STATUSKODER                                      
060100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
060200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     EJECT                                                                
060600 IMS-GU-WDD905-IN-LT  SECTION.                                            
060700                                                                          
060800     MOVE SPACE                TO ALL-SSA                                 
060900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
061000            DELIMITED BY SIZE INTO SSA1                                   
061100     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
061200            DELIMITED BY SIZE INTO SSA2                                   
061300     STRING 'WDD905  (DAAVROP =<' W-DAAVROP-MIN-X                         
061400                    '&KDAVROP >=' W-KDAVROP-MIN-X                         
061500                    '&KDAVROP =<' W-KDAVROP-MAX-X ')'                     
061600            DELIMITED BY SIZE INTO SSA3                                   
061700     MOVE '  GE' TO GODK-STATUSKODER                                      
061800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
061900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     .                                                                    
062200     EJECT                                                                
062300 IMS-GU-WDD905-QUAL   SECTION.                                            
062400                                                                          
062500     MOVE SPACE                TO ALL-SSA                                 
062600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
062700            DELIMITED BY SIZE INTO SSA1                                   
062800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
062900            DELIMITED BY SIZE INTO SSA2                                   
063000     STRING 'WDD905  (DAAVROP  >' W-DAAVROP-MIN-X                         
063100                    '&DAAVROP =<' W-DAAVROP-MAX-X                         
063200                    '&KDAVROP  =' W-KDAVROP-X ')'                         
063300            DELIMITED BY SIZE INTO SSA3                                   
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
063600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     EJECT                                                                
064000 IMS-GU-WDD905-GT-LIM-DAT SECTION.                                        
064100                                                                          
064200     MOVE SPACE                TO ALL-SSA                                 
064300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
064400            DELIMITED BY SIZE INTO SSA1                                   
064500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
064600            DELIMITED BY SIZE INTO SSA2                                   
064700     STRING 'WDD905  (DAAVROP  >' W-DAAVROP-MAX-X                         
064800                    '&KDAVROP >=' W-KDAVROP-MIN-X                         
064900                    '&KDAVROP =<' W-KDAVROP-MAX-X ')'                     
065000            DELIMITED BY SIZE INTO SSA3                                   
065100     MOVE '  GE' TO GODK-STATUSKODER                                      
065200     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
065300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     EJECT                                                                
065700 IMS-GU-WDD902  SECTION.                                                  
065800                                                                          
065900     MOVE SPACE                TO ALL-SSA                                 
066000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
066100         DELIMITED BY SIZE INTO SSA1                                      
066200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
066300         DELIMITED BY SIZE INTO SSA2                                      
066400     MOVE '  GE' TO GODK-STATUSKODER                                      
066500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
066600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     EJECT                                                                
067000 IMS-GNP-WDD904 SECTION.                                                  
067100                                                                          
067200     MOVE SPACE          TO ALL-SSA                                       
067300     MOVE   'WDD904  '   TO SSA1                                          
067400     MOVE '  GE' TO GODK-STATUSKODER                                      
067500     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD904 SSA1                   
067600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
067700     PERFORM IMS-STATUSKONTROLL                                           
067800     .                                                                    
067900     EJECT                                                                
068000 IMS-GHNP-WDD904 SECTION.                                                 
068100                                                                          
068200     MOVE SPACE          TO ALL-SSA                                       
068300     MOVE   'WDD904  '   TO SSA1                                          
068400     MOVE '  GE' TO GODK-STATUSKODER                                      
068500     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD904 SSA1                  
068600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     EJECT                                                                
069000 IMS-GHNP-WDD905 SECTION.                                                 
069100                                                                          
069200     MOVE SPACE                TO ALL-SSA                                 
069300     STRING 'WDD905  (DAAVROP >=' W-DAAVROP-X                             
069400                    '&KDAVROP  =' W-KDAVROP-X ')'                         
069500            DELIMITED BY SIZE INTO SSA1                                   
069600     MOVE '  GE' TO GODK-STATUSKODER                                      
069700     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
069800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
069900     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
070100     EJECT                                                                
070200 IMS-DELETE-OMSPEC SECTION.                                               
070300                                                                          
070400     MOVE '  '   TO GODK-STATUSKODER                                      
070500     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
070600     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     EJECT                                                                
071000 IMS-DELETE-AVROP    SECTION.                                             
071100                                                                          
071200     MOVE '  '   TO GODK-STATUSKODER                                      
071300     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
071400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700     EJECT                                                                
071800 IMS-RESTART SECTION.                                                     
071900     SKIP2                                                                
072000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
072100     MOVE '  ' TO GODK-STATUSKODER                                        
072200     CALL CBLTDLI USING XRST MSG-PCB                                      
072300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
072400                        CHKP-AREA-LENGTH CHKP-AREA                        
072410                        GSAM-PCB-LENGTH                                   
072420                        IN-GSAM-PCB                                       
072430                                                                          
072440     IF CHKP-MSG-IO-AREA = SPACES                                         
072450       SET NORMAL-RUN                 TO TRUE                             
072460     ELSE                                                                 
072470       SET RESTART-RUN                TO TRUE                             
072480     END-IF                                                               
072500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     SKIP3                                                                
072900 IMS-CHECKPOINT SECTION.                                                  
073000     SKIP2                                                                
073100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
073200     MOVE '  XD' TO GODK-STATUSKODER                                      
073300     CALL CBLTDLI USING CHKP MSG-PCB                                      
073400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
073500                        CHKP-AREA-LENGTH CHKP-AREA                        
073510                        GSAM-PCB-LENGTH                                   
073520                        IN-GSAM-PCB                                       
073600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073700     PERFORM IMS-STATUSKONTROLL                                           
073800                                                                          
073900     IF IMS-EJ-OK                                                         
074000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
074100       DISPLAY FELTEXT                                                    
074200       CALL FELLOG                                                        
           ELSE                                                                 
             MOVE +1            TO CHKP-ANT                                     
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 IMS-STATUSKONTROLL Section.                                              
074700                                                                          
074800     Set STATUS-IX To 1                                                   
074900     Search GODK-STATUS                                                   
075000       At End                                                             
075100         String ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
075200           Delimited By Size Into FELTEXT                                 
075300         Display FELTEXT                                                  
075400         Call FELLOG                                                      
075500       When GODK-STATUS (STATUS-IX) = STATUS-WS                           
075600         Continue                                                         
075700     End-Search                                                           
075800     .                                                                    

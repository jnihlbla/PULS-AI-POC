000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2133800.                                                
000300*AUTHOR.         P-A HELGEGREN.                                           
000400*DATE-WRITTEN.   JAN   2011.                                              
000500*    FUNKTION.                                                            
000600*                BYTE AV SVENSK OCH ENGELSK BENÄMNING                     
000700*                (GCP LÄGGS TILL FÖRST I BENÄMNINGEN) FÖR                 
000800*                CLASSIC-ARTIKLAR.                                        
000900*                (FRITT EFTER   W1011200)                                 
001000     SKIP2                                                                
001100*    INDATA.                                                              
001200*        INFIL        W21334                                              
001300*                                                                         
001400*    UTDATA.                                                              
001500*        UPPDATERAR   WDD3                                                
001600*                     XXAI                                                
001700*        LÄSER        WDK6                                                
001800*                                                                         
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL MED ARTIKELNR FRÅN W213P032                          
002700     SELECT W21334                     ASSIGN TO W21338D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W21334                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W21334      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W2133800'.            
004300 77  JA                          PIC X(1)    VALUE 'J'.                   
004400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004500 77  REGISTRERAD                 PIC X(1)    VALUE '®'.                   
004600 77  SVENSKA                     PIC X(1)    VALUE 'S'.                   
004700 77  WS-FLRSBEART                PIC X(1)    VALUE SPACE.                 
004800 77  WS-NYTT-NUMMER              PIC S9(7)   VALUE +0   COMP-3.           
004900 77  MAX-RAD                     PIC S9(3)   VALUE +11  COMP-3.           
005000 77  MAX-HOM-PLUS-1              PIC S9(3)   VALUE +3   COMP-3.           
005100 77  MAX-IDSKYLT                 PIC S9(3)   VALUE +26  COMP-3.           
005200 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77  IDSKYLT-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005500                                                                          
005600 77  WS-FLAENDR-A                PIC X(1)    VALUE 'N'.                   
005700 77  WS-IDBENNR-A                PIC S9(7)   VALUE ZERO COMP-3.           
005800 77  WS-KDHOMONYM-A              PIC  9(1)   VALUE ZERO COMP-3.           
005900                                                                          
006000 77  WS-FLAENDR-B                PIC X(1)    VALUE 'N'.                   
006100 77  WS-IDBENNR-B                PIC S9(7)   VALUE ZERO COMP-3.           
006200 77  WS-KDHOMONYM-B              PIC  9(1)   VALUE ZERO COMP-3.           
006300                                                                          
006400 77  NEVIS-SW                 PIC X       VALUE 'N'.                      
006500     88 NEVIS-ARTIKEL   VALUE 'J'.                                        
006600     88 ANDRA-ARTIKLAR  VALUE 'N'.                                        
006700     SKIP3                                                                
006800 01  WS-BEART                    PIC X(25)   VALUE SPACE.                 
006900 01  WS-BEART-NY                 PIC X(25)   VALUE SPACE.                 
007000 01  FILLER  REDEFINES WS-BEART-NY.                                       
007100     03  WS-BEART-GCP            PIC X(4).                                
007200     03  WS-BEART-21             PIC X(21).                               
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700                                                                          
007800 77  W21334-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W21334                       VALUE 'J'.                   
008000     EJECT                                                                
008100 01  CHKP-VAR.                                                            
008200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
008300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
008400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
008500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
008600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
008700     03 CHKP-MAX                 PIC S9(3)   VALUE +300 COMP-3.           
008800*    03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
008900*                                                                         
009000*01    -COPY WWPRODSL                                                     
009100                                                                          
009200*      --- VALID IDDC CODES                                               
009300*                                                                         
009400*01    -COPY WWDC99                                                       
009500       EJECT                                                              
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
009800   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
009900   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
010000   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
010100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700     EJECT                                                                
010800 01  IN-AREA-START               PIC X(24)   VALUE                        
010900                                             'IN-AREA-START'.             
011000     SKIP2                                                                
011100                                                                          
011200*01  AREA -COPY W21334     -PRE IN-                                       
011300*                                                                         
011400     EJECT                                                                
011500 01  WS-IDARTNR                             PIC X(9).                     
011600 01  IDARTNR-WS REDEFINES WS-IDARTNR        PIC 9(9).                     
011700     SKIP2                                                                
011800 01  WS-KDHOMONYM                           PIC X.                        
011900 01  KDHOMONYM-WS REDEFINES WS-KDHOMONYM    PIC 9.                        
012000     SKIP2                                                                
012100 01  INPUT-RETT                        PIC X    VALUE 'J'.                
012200     SKIP2                                                                
012300 01  SUBPROGRAM.                                                          
012400     03 WREVERSE                       PIC X(8) VALUE 'WREVERSE'.         
012500     EJECT                                                                
012600                                                                          
012700 01  IDSKYLT-TABELL.                                                      
012800     03 FILLER   VALUE 'CZ '        PIC X(3).                             
012900     03 FILLER   VALUE 'D  '        PIC X(3).                             
013000     03 FILLER   VALUE 'DK '        PIC X(3).                             
013100     03 FILLER   VALUE 'E  '        PIC X(3).                             
013200     03 FILLER   VALUE 'F  '        PIC X(3).                             
013300     03 FILLER   VALUE 'GB '        PIC X(3).                             
013400     03 FILLER   VALUE 'GR '        PIC X(3).                             
013500     03 FILLER   VALUE 'H  '        PIC X(3).                             
013600     03 FILLER   VALUE 'I  '        PIC X(3).                             
013700     03 FILLER   VALUE 'IR '        PIC X(3).                             
013800     03 FILLER   VALUE 'J  '        PIC X(3).                             
013900     03 FILLER   VALUE 'KOR'        PIC X(3).                             
014000     03 FILLER   VALUE 'MAL'        PIC X(3).                             
014100     03 FILLER   VALUE 'NL '        PIC X(3).                             
014200     03 FILLER   VALUE 'P  '        PIC X(3).                             
014300     03 FILLER   VALUE 'PL '        PIC X(3).                             
014400     03 FILLER   VALUE 'RC '        PIC X(3).                             
014500     03 FILLER   VALUE 'RCN'        PIC X(3).                             
014600     03 FILLER   VALUE 'RO '        PIC X(3).                             
014700     03 FILLER   VALUE 'RUS'        PIC X(3).                             
014800     03 FILLER   VALUE 'S  '        PIC X(3).                             
014900     03 FILLER   VALUE 'SF '        PIC X(3).                             
015000     03 FILLER   VALUE 'T  '        PIC X(3).                             
015100     03 FILLER   VALUE 'TR '        PIC X(3).                             
015200     03 FILLER   VALUE 'USA'        PIC X(3).                             
015300     03 FILLER   VALUE 'YU '        PIC X(3).                             
015400 01  IDSKYLT-TAB REDEFINES IDSKYLT-TABELL.                                
015500     03  FILLER OCCURS 26.                                                
015600      05  WS-IDSKYLT                PIC X(3).                             
015700     SKIP3                                                                
015800 01  DAGENS-DATUM                   PIC 9(6)   VALUE ZERO.                
015900     SKIP2                                                                
016000 01  WS-TEHOMONYM.                                                        
016100     03  BM-RS-NAMN                 PIC X(7).                             
016200     03  FILLER                     PIC X(53).                            
016300     EJECT                                                                
016400*01  -COPY WREVAREA                                                       
016500     EJECT                                                                
016600 01  NYCKLAR-TILL-DLI.                                                    
016700     03  W-IDARTNR-X.                                                     
016800         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
016900     03  W-IDBENNR-X.                                                     
017000         05  W-IDBENNR            PIC S9(7) COMP-3 VALUE ZERO.            
017100     03  W-IDBENNR-D-X.                                                   
017200         05  W-IDBENNR-D          PIC S9(7) COMP-3 VALUE ZERO.            
017300     03  W-IDSKYLT-X.                                                     
017400         05  W-IDSKYLT            PIC X(3)  VALUE SPACE.                  
017500     03  W-BEART-X.                                                       
017600         05  W-BEART              PIC X(25) VALUE SPACE.                  
017700     03  W-1207-KEY-X.                                                    
017800         05  FILLER               PIC X(4)  VALUE '1207'.                 
017900         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
018000     EJECT                                                                
018100                                                                          
018200******************************************************************        
018300*****                                                                     
018400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018500*****                                                                     
018600 01  IMS-WS.                                                              
018700     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
018800     SKIP3                                                                
018900*****                    **** STATUS-KOD FRÅN IMS                         
019000     03  STATUS-WS               PIC X(2).                                
019100         88  SEGMENT-FINNS                   VALUE '  '.                  
019200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
019300         88  IMS-EJ-OK                       VALUE 'XD'.                  
019400     SKIP3                                                                
019500     03  GODK-STATUSKODER.                                                
019600         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
019700     SKIP3                                                                
019800 01  SSA1                        PIC X(128).                              
019900 01  SSA2                        PIC X(128).                              
020000     EJECT                                                                
020100*                            IMS FUNKTIONSKODER                           
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400*                            DLI INPUT-OUTPUT AREA                        
020500 01  DLI-IO-AREA.                                                         
020600     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
020700     SKIP3                                                                
020800*    03  WLBENA  -COPY WDD301   -PRE BENA-  -RED IO-AREA.                 
020900     EJECT                                                                
021000*    03  WLBENA  -COPY WDD311   -PRE BENA-  -RED IO-AREA.                 
021100     EJECT                                                                
021200*    03  WLBENA  -COPY WDD312   -PRE BENA-  -RED IO-AREA.                 
021300*    03  WLBENA  -COPY WDD313   -PRE BENA-  -RED IO-AREA.                 
021400     EJECT                                                                
021500*    03  WLXXAI  -COPY WDGX1208 -PRE XXAI-  -RED IO-AREA.                 
021600     EJECT                                                                
021700*                            DLI INPUT-OUTPUT AREA-2                      
021800 01  DLI-IO-AREA-2.                                                       
021900     03  IO-AREA-2               PIC X(200)  VALUE SPACE.                 
022000     SKIP3                                                                
022100*    03  WLARTC  -COPY WDK601               -RED IO-AREA-2.               
022200     EJECT                                                                
022300*                            DLI INPUT-OUTPUT AREA-3                      
022400 01  DLI-IO-AREA-3.                                                       
022500     03  IO-AREA-3               PIC X(200)  VALUE SPACE.                 
022600     SKIP3                                                                
022700*    03  WLBEND  -COPY WDD301   -PRE BEND-  -RED IO-AREA-3.               
022800     EJECT                                                                
022900*    03  WLBEND  -COPY WDD311   -PRE BEND-  -RED IO-AREA-3.               
023000     EJECT                                                                
023100*    03  WLBEND  -COPY WDD312   -PRE BEND-  -RED IO-AREA-3.               
023200     EJECT                                                                
023300*    03  WLBEND  -COPY WDD313   -PRE BEND-  -RED IO-AREA-3.               
023400     EJECT                                                                
023500 LINKAGE SECTION.                                                         
023600     SKIP2                                                                
023700*01  -COPY W0009     -PRE MSG-                                            
023800     EJECT                                                                
023900*01  -COPY W0008     -PRE BENA-                                           
024000         05  FILLER              PIC X.                                   
024100     EJECT                                                                
024200*01  -COPY W0008     -PRE BENB-                                           
024300         05  FILLER              PIC X.                                   
024400     EJECT                                                                
024500*01  -COPY W0008     -PRE BENC-                                           
024600         05  FILLER              PIC X.                                   
024700     EJECT                                                                
024800*01  -COPY W0008     -PRE XXAI-                                           
024900         05  FILLER              PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008     -PRE ARTC-                                           
025200         05  FILLER              PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008     -PRE BEND-                                           
025500         05  FILLER              PIC X.                                   
025600     EJECT                                                                
025700 PROCEDURE DIVISION USING MSG-PCB                                         
025800                          BENA-PCB BENB-PCB BENC-PCB                      
025900                          XXAI-PCB ARTC-PCB BEND-PCB.                     
026000 MAIN SECTION.                                                            
026100     ENTRY 'DLITCBL' USING MSG-PCB                                        
026200                           BENA-PCB BENB-PCB BENC-PCB                     
026300                           XXAI-PCB ARTC-PCB BEND-PCB.                    
026400     PERFORM A-INIT                                                       
026500     PERFORM S01-LAES-W21334                                              
026600     PERFORM UNTIL END-OF-W21334                                          
026700       IF CHKP-ANT > CHKP-MAX                                             
026800         PERFORM X-TAG-CHECKPOINT                                         
026900       END-IF                                                             
027000                                                                          
027100       IF IN-ATGARD  = 'REPL'   AND                                       
027200          IN-IDSEGM  = 'WDK601' AND                                       
027300          IN-IDLEVNR = 'BQ8VA'                                            
027400          MOVE IDARTNR-WS TO W-IDARTNR                                    
027500          MOVE NEJ        TO NEVIS-SW                                     
027600          MOVE NEJ        TO WS-FLAENDR-A                                 
027700          MOVE NEJ        TO WS-FLAENDR-B                                 
027800          PERFORM IMS-GET-ARTC01                                          
027900          IF SEGMENT-FINNS                                                
028000             MOVE ART-KDPRODSL   TO TEST-KDPRODSL                         
028100             IF KDPRODSL-VOLVO-ALL                                        
028200               SET NEVIS-ARTIKEL TO TRUE                                  
028300             END-IF                                                       
028400          END-IF                                                          
028500                                                                          
028600          PERFORM B-UPPDATERA                                             
028700       ELSE                                                               
028800          IF IN-ATGARD  = 'REPL'   AND                                    
028900             IN-IDSEGM  = 'WDK601'                                        
029000***         *vi får kolla om det är en classic-artikel som ändras         
029100             PERFORM C-BACKA-CLASSIC                                      
029200          END-IF                                                          
029300       END-IF                                                             
029400                                                                          
029500       PERFORM S01-LAES-W21334                                            
029600     END-PERFORM                                                          
029700                                                                          
029800     PERFORM Z-FINIT                                                      
029900     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 A-INIT             SECTION.                                              
030400     SKIP2                                                                
030500                                                                          
030600     PERFORM IMS-RESTART                                                  
030700                                                                          
030800     OPEN INPUT W21334                                                    
030900                                                                          
031000                                                                          
031100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
031200                                                                          
031300     ACCEPT DAGENS-DATUM FROM DATE                                        
031400                                                                          
031500     MOVE 'GCP '         TO WS-BEART-GCP                                  
031600                                                                          
031700     .                                                                    
031800     EJECT                                                                
031900 B-UPPDATERA SECTION.                                                     
032000     SKIP2                                                                
032100     MOVE IDARTNR-WS       TO W-IDARTNR                                   
032200*    --- LÄSER ARTIKELNS BENA01 MED BENC-PCB                              
032300*                                         BENC   / IDARTNR                
032400     PERFORM IMS-GU-BENA01-BSEQ                                           
032500     IF SEGMENT-FINNS                                                     
032600        PERFORM BB-UPPDATERA-BEART                                        
032700                                                                          
032800*      --- KOLLA WS-FLAENDR FÖR UPPDATERING AV BENA-BEN-FLAENDR           
032900*      --- Denna sätts enbart om Switchen NEVIS-SW har värdet             
033000*      --- för NEVIS-ARTIKEL (D.v.s.=JA)                                  
033100*                                                                         
033200       IF WS-FLAENDR-A = JA                                               
033300         MOVE WS-IDBENNR-A TO W-IDBENNR                                   
033400*                                         BENA   / IDBENNR                
033500         PERFORM IMS-GHU-BENA01                                           
033600         MOVE JA           TO BENA-BEN-FLAENDR                            
033700         PERFORM IMS-REPL-BENA01                                          
033800       END-IF                                                             
033900       IF WS-FLAENDR-B = JA                                               
034000         MOVE WS-IDBENNR-B TO W-IDBENNR                                   
034100         PERFORM IMS-GHU-BENA01                                           
034200         MOVE JA           TO BENA-BEN-FLAENDR                            
034300         PERFORM IMS-REPL-BENA01                                          
034400       END-IF                                                             
034500                                                                          
034600     ELSE                                                                 
034700       DISPLAY 'ARTIKEL SAKNAS ' W-IDARTNR                                
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 BB-UPPDATERA-BEART SECTION.                                              
035200     SKIP2                                                                
035300*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
035400     MOVE BENA-BEN-IDBENNR   TO WS-IDBENNR-A                              
035500                                W-IDBENNR                                 
035600                                W-IDBENNR-D                               
035700     MOVE BENA-BEN-KDHOMONYM TO WS-KDHOMONYM-A                            
035800*TEST                                                                     
035900*    DISPLAY 'OLD IDBENNR '  WS-IDBENNR-A                                 
036000*TEST                                                                     
036100*    -- LÄS GAMLA S-BENÄMNING -> WS-BEART                                 
036200     MOVE SVENSKA            TO W-IDSKYLT                                 
036300*                                    GU   BENA   /IDBENNR                 
036400     PERFORM IMS-GET-BENA01                                               
036500*                                                /IDSKYLT                 
036600     PERFORM IMS-GNP-BENA11                                               
036700     MOVE BENA-TEXT-BEART    TO WS-BEART                                  
036800                                                                          
036900*    -- KOLLA OM DEN BÖRJAR PÅ GCP, I SÅ FALL REDAN KLART                 
037000     IF WS-BEART (1:3) = 'GCP'                                            
037100        CONTINUE                                                          
037200     ELSE                                                                 
037300*       -- LÄS FRAM DEN NYA WDD301 MED DEN NYA BENÄMNINGEN                
037400*       -- SKAPA NY BENÄMNING -> WS-BEART  =  GCP + BENÄMNING             
037500*test                                                                     
037600*       DISPLAY 'ARTNR ' IDARTNR-WS                                       
037700*test                                                                     
037800        MOVE WS-BEART        TO WS-BEART-21                               
037900        MOVE WS-BEART-NY     TO WS-BEART                                  
038000        MOVE WS-BEART        TO W-BEART                                   
038100        MOVE SVENSKA         TO W-IDSKYLT                                 
038200*                                          BENB  / IDSKYLT + BEART        
038300        PERFORM IMS-GU-BENA01-ASEQ                                        
038400        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
038500                      BENA-BEN-KDHOMONYM = WS-KDHOMONYM-A                 
038600          PERFORM IMS-GET-BENA01-ASEQ                                     
038700        END-PERFORM                                                       
038800        IF SEGMENT-FINNS                                                  
038900*         -- SPARA NYA BENÄMNINGSNUMRET                                   
039000          MOVE BENA-BEN-IDBENNR TO W-IDBENNR                              
039100                                   WS-IDBENNR-B                           
039200          PERFORM BBA-FINNS-REDAN                                         
039300        ELSE                                                              
039400          PERFORM BBE-REG-RSUNIK                                          
039500        END-IF                                                            
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 BBA-FINNS-REDAN SECTION.                                                 
040000     SKIP2                                                                
040100*    -- TAG BORT ARTIKELN FRÅN DEN GAMLA BENÄMNINGEN                      
040200*                                    GU  BENC  / IDARTNR                  
040300     PERFORM IMS-GU-BENA01-BSEQ                                           
040400     PERFORM IMS-GET-BENA12-BSEQ                                          
040500     PERFORM IMS-DLET-BENA12-BSEQ                                         
040600     IF NEVIS-ARTIKEL                                                     
040700       MOVE JA       TO WS-FLAENDR-A                                      
040800     END-IF                                                               
040900*TEST                                                                     
041000*    DISPLAY 'BBA IDBENNR '  WS-IDBENNR-B                                 
041100*TEST                                                                     
041200                                                                          
041300*    -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                        
041400     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
041500     MOVE NEJ        TO BENA-ART-FLFELHOMO                                
041600*                                     BENA   / IDBENNR                    
041700     PERFORM IMS-ISRT-BENA12                                              
041800     IF NEVIS-ARTIKEL                                                     
041900       MOVE JA       TO WS-FLAENDR-B                                      
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 BBE-REG-RSUNIK SECTION.                                                  
042400     SKIP2                                                                
042500*    -- SKAPAR NYTT BENÄMNINGSNUMMER IDBENNR                              
042600     PERFORM IMS-GET-XXAI01                                               
042700     PERFORM IMS-GET-XXAI11                                               
042800     ADD +1                 TO XXAI-1208-IDBENNR                          
042900     IF XXAI-1208-IDBENNR  > XXAI-1208-IDBENNR-MAX                        
043000       MOVE +1              TO XXAI-1208-IDBENNR                          
043100     END-IF                                                               
043200     MOVE XXAI-1208-IDBENNR TO W-IDBENNR                                  
043300                               WS-IDBENNR-B                               
043400     PERFORM IMS-REPL-XXAI11                                              
043500*TEST                                                                     
043600*    DISPLAY 'BBE IDBENNR '  WS-IDBENNR-B                                 
043700*TEST                                                                     
043800                                                                          
043900*    -- LÄGG UPP NY ROT (IDBENNR)                                         
044000     MOVE W-IDBENNR         TO BENA-BEN-IDBENNR                           
044100     MOVE WS-KDHOMONYM-A    TO BENA-BEN-KDHOMONYM                         
044200     MOVE ZERO              TO BENA-BEN-TIUPPDAT-STOP                     
044300     MOVE ZERO              TO BENA-BEN-KDBENSTAT                         
044400     IF NEVIS-ARTIKEL                                                     
044500       MOVE JA              TO BENA-BEN-FLAENDR                           
044600     ELSE                                                                 
044700       MOVE NEJ             TO BENA-BEN-FLAENDR                           
044800     END-IF                                                               
044900*                                      BENA   / IDBENNR                   
045000     PERFORM IMS-ISRT-BENA01                                              
045100                                                                          
045200*    -- LÄS DE  GAMLA BENÄMNINGARNA IDBENNR-D                             
045300*                                      BEND   / IDBENNR                   
045400     PERFORM IMS-GET-BENA01-D                                             
045500     PERFORM IMS-GET-BENA11-D                                             
045600     PERFORM UNTIL SEGMENT-SAKNAS                                         
045700*      -- KOPIERA TILL NYA BENÄMNINGSNUMRET                               
045800                                                                          
045900       PERFORM BBEA-KOPIERA-BENA11                                        
046000                                                                          
046100       PERFORM IMS-GET-BENA11-D                                           
046200     END-PERFORM                                                          
046300                                                                          
046400                                                                          
046500*    -- LÄS DE  GAMLA HOMONYMKODERNA   IDBENNR-D                          
046600*                                      BEND   / IDBENNR                   
046700     PERFORM IMS-GET-BENA13-D                                             
046800     PERFORM UNTIL SEGMENT-SAKNAS                                         
046900*      -- KOPIERA TILL NYA BENÄMNINGSNUMRET                               
047000       MOVE BEND-HOM-WDD313 TO BENA-HOM-WDD313                            
047100*                                      BENA   / IDBENNR                   
047200       PERFORM IMS-ISRT-BENA13                                            
047300                                                                          
047400       PERFORM IMS-GET-BENA13-D                                           
047500     END-PERFORM                                                          
047600                                                                          
047700*    -- TAG BORT ARTIKEL PÅ GAMLA IDBENNR (IDARTNR)                       
047800*                                           BENC   / IDARTNR              
047900     PERFORM IMS-GU-BENA01-BSEQ                                           
048000     PERFORM IMS-GET-BENA12-BSEQ                                          
048100     PERFORM IMS-DLET-BENA12-BSEQ                                         
048200                                                                          
048300*    -- LÄGG TILL ARTIKEL PÅ NYA IDBENNR                                  
048400     MOVE IDARTNR-WS        TO BENA-ART-IDARTNR                           
048500     MOVE NEJ               TO BENA-ART-FLFELHOMO                         
048600*                                          BENA   / IDBENNR               
048700     PERFORM IMS-ISRT-BENA12                                              
048800                                                                          
048900     IF NEVIS-ARTIKEL                                                     
049000       MOVE JA              TO WS-FLAENDR-A                               
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 BBEA-KOPIERA-BENA11 SECTION.                                             
049500                                                                          
049600     IF BEND-TEXT-IDSKYLT = 'S  ' OR 'GB '                                
049700        MOVE BEND-TEXT-BEART TO WS-BEART-21                               
049800        MOVE WS-BEART-NY     TO BEND-TEXT-BEART                           
049900     END-IF                                                               
050000                                                                          
050100     MOVE BEND-TEXT-WDD311   TO BENA-TEXT-WDD311                          
050200                                                                          
050300*                                          BENA   / IDBENNR               
050400     PERFORM IMS-ISRT-BENA11                                              
050500     .                                                                    
050600     EJECT                                                                
050700 C-BACKA-CLASSIC SECTION.                                                 
050800                                                                          
050900*   *KOLLA OM VI BYTER FRÅN CLASSIC TILL ICKE-CLASSIC LEVERANTÖR          
051000*   *(HUVUDLEVERANTÖR ÄNNU EJ BYTT PÅ WDK601)                             
051100     MOVE IDARTNR-WS               TO W-IDARTNR                           
051200     PERFORM IMS-GET-ARTC01                                               
051300     IF SEGMENT-FINNS AND ART-IDLEVNR = 'BQ8VA'                           
051400        MOVE IDARTNR-WS            TO W-IDARTNR                           
051500        MOVE NEJ                   TO NEVIS-SW                            
051600        MOVE NEJ                   TO WS-FLAENDR-A                        
051700        MOVE NEJ                   TO WS-FLAENDR-B                        
051800        MOVE ART-KDPRODSL        TO TEST-KDPRODSL                         
051900        IF KDPRODSL-VOLVO-ALL                                             
052000           SET NEVIS-ARTIKEL       TO TRUE                                
052100        END-IF                                                            
052200*       --- LÄSER ARTIKELNS BENA01 MED BENC-PCB                           
052300*                                         BENC   / IDARTNR                
052400        PERFORM IMS-GU-BENA01-BSEQ                                        
052500        IF SEGMENT-FINNS                                                  
052600*       -- WDD301 ÄR LÄST FÖR ARTIKELN                                    
052700           MOVE BENA-BEN-IDBENNR   TO WS-IDBENNR-A                        
052800                                      W-IDBENNR                           
052900                                      W-IDBENNR-D                         
053000           MOVE BENA-BEN-KDHOMONYM TO WS-KDHOMONYM-A                      
053100*TEST                                                                     
053200*    DISPLAY 'GCP IDBENNR '  WS-IDBENNR-A                                 
053300*TEST                                                                     
053400*          -- LÄS S-BENÄMNING -> WS-BEART                                 
053500           MOVE SVENSKA            TO W-IDSKYLT                           
053600*                                    GU   BENA   /IDBENNR                 
053700           PERFORM IMS-GET-BENA01                                         
053800*                                                /IDSKYLT                 
053900           PERFORM IMS-GNP-BENA11                                         
054000           MOVE BENA-TEXT-BEART    TO WS-BEART                            
054100                                                                          
054200*          -- KOLLA OM DEN BÖRJAR PÅ GCP                                  
054300           IF WS-BEART (1:3) = 'GCP'                                      
054400              PERFORM CA-KOLLA-OLD-BEN                                    
054500           END-IF                                                         
054600        END-IF                                                            
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
055000 CA-KOLLA-OLD-BEN   SECTION.                                              
055100                                                                          
055200*****SKAPA NY (=DEN 'TIDIGARE' UTAN GCP) BENÄMNING                        
055300*****SÖK BENÄMNINGSNUMMER (SAMMA HOMONYMKOD) TILL SAMMA                   
055400*****    BENÄMNING (DEN UTAN GCP)                                         
055500*****UPPDATERA ARTIKELN PÅ 'NYTT' BENÄMNINGSNUMMER                        
055600*****    OCH TAG BORT DEN PÅ DET GAMLA                                    
055700                                                                          
055800*       -- SKAPA NY BENÄMNING -> WS-BEART  = - GCP + BENÄMNING            
055900*       -- LÄS WDD301 FÖR BENÄMNING UTAN GCP                              
056000*test                                                                     
056100*       DISPLAY 'CA- ARTNR ' IDARTNR-WS                                   
056200*test                                                                     
056300        MOVE WS-BEART TO WS-BEART-NY                                      
056400        MOVE WS-BEART-21 TO WS-BEART                                      
056500        MOVE WS-BEART    TO W-BEART                                       
056600        MOVE SVENSKA     TO W-IDSKYLT                                     
056700*                                          BENB  / IDSKYLT + BEART        
056800        PERFORM IMS-GU-BENA01-ASEQ                                        
056900        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
057000                      BENA-BEN-KDHOMONYM = WS-KDHOMONYM-A                 
057100          PERFORM IMS-GET-BENA01-ASEQ                                     
057200        END-PERFORM                                                       
057300        IF SEGMENT-FINNS                                                  
057400*         -- SPARA NYA BENÄMNINGSNUMRET                                   
057500          MOVE BENA-BEN-IDBENNR TO W-IDBENNR                              
057600                                   WS-IDBENNR-B                           
057700          PERFORM CAA-FINNS                                               
057800        ELSE                                                              
057900***       ---DETTA BORDE INTE HÄNDA, MEN ...                              
058000***       ---VI HITTAR INTE DEN GAMLA BENÄMNINGEN, SÅ VI FÅR              
058100***       ---LÄGGA UPP BENÄMNINGEN UTAN GCP PÅ NYTT                       
058200***       ---(KANSKE FÖR ATT BENÄMNINGEN ÄR > 21 TECKEN?)                 
058300                                                                          
058400          PERFORM CAB-REG-RSUNIK                                          
058500        END-IF                                                            
058600                                                                          
058700*      --- KOLLA WS-FLAENDR FÖR UPPDATERING AV BENA-BEN-FLAENDR           
058800*      --- DENNA SÄTTS ENBART OM SWITCHEN NEVIS-SW HAR VÄRDET             
058900*      --- FÖR NEVIS-ARTIKEL (D.V.S.=JA)                                  
059000*                                                                         
059100       IF WS-FLAENDR-A = JA                                               
059200         MOVE WS-IDBENNR-A TO W-IDBENNR                                   
059300*                                         BENA   / IDBENNR                
059400         PERFORM IMS-GHU-BENA01                                           
059500         MOVE JA           TO BENA-BEN-FLAENDR                            
059600         PERFORM IMS-REPL-BENA01                                          
059700       END-IF                                                             
059800       IF WS-FLAENDR-B = JA                                               
059900         MOVE WS-IDBENNR-B TO W-IDBENNR                                   
060000         PERFORM IMS-GHU-BENA01                                           
060100         MOVE JA           TO BENA-BEN-FLAENDR                            
060200         PERFORM IMS-REPL-BENA01                                          
060300       END-IF                                                             
060400                                                                          
060500     .                                                                    
060600     EJECT                                                                
060700 CAA-FINNS SECTION.                                                       
060800                                                                          
060900*    -- TAG BORT ARTIKELN FRÅN DEN GAMLA BENÄMNINGEN                      
061000*                                    GU  BENC  / IDARTNR                  
061100     PERFORM IMS-GU-BENA01-BSEQ                                           
061200     PERFORM IMS-GET-BENA12-BSEQ                                          
061300     PERFORM IMS-DLET-BENA12-BSEQ                                         
061400     IF NEVIS-ARTIKEL                                                     
061500       MOVE JA TO WS-FLAENDR-A                                            
061600     END-IF                                                               
061700*TEST                                                                     
061800*    DISPLAY 'CAA IDBENNR '  WS-IDBENNR-B                                 
061900*TEST                                                                     
062000                                                                          
062100*    -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                        
062200     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
062300     MOVE NEJ        TO BENA-ART-FLFELHOMO                                
062400*                                     BENA   / IDBENNR                    
062500     PERFORM IMS-ISRT-BENA12                                              
062600     IF NEVIS-ARTIKEL                                                     
062700       MOVE JA TO WS-FLAENDR-B                                            
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 CAB-REG-RSUNIK SECTION.                                                  
063200     SKIP2                                                                
063300*    -- SKAPAR NYTT BENÄMNINGSNUMMER IDBENNR                              
063400     PERFORM IMS-GET-XXAI01                                               
063500     PERFORM IMS-GET-XXAI11                                               
063600     ADD +1                 TO XXAI-1208-IDBENNR                          
063700     IF XXAI-1208-IDBENNR  > XXAI-1208-IDBENNR-MAX                        
063800       MOVE +1              TO XXAI-1208-IDBENNR                          
063900     END-IF                                                               
064000     MOVE XXAI-1208-IDBENNR TO W-IDBENNR                                  
064100                               WS-IDBENNR-B                               
064200     PERFORM IMS-REPL-XXAI11                                              
064300*TEST                                                                     
064400*    DISPLAY 'CAB IDBENNR '  WS-IDBENNR-B                                 
064500*TEST                                                                     
064600                                                                          
064700*    -- LÄGG UPP NY ROT (IDBENNR)                                         
064800     MOVE W-IDBENNR         TO BENA-BEN-IDBENNR                           
064900     MOVE WS-KDHOMONYM-A    TO BENA-BEN-KDHOMONYM                         
065000     MOVE ZERO              TO BENA-BEN-TIUPPDAT-STOP                     
065100     MOVE ZERO              TO BENA-BEN-KDBENSTAT                         
065200     IF NEVIS-ARTIKEL                                                     
065300       MOVE JA              TO BENA-BEN-FLAENDR                           
065400     ELSE                                                                 
065500       MOVE NEJ             TO BENA-BEN-FLAENDR                           
065600     END-IF                                                               
065700*                                      BENA   / IDBENNR                   
065800     PERFORM IMS-ISRT-BENA01                                              
065900                                                                          
066000*    -- LÄS DE  GAMLA BENÄMNINGARNA IDBENNR-D                             
066100*                                      BEND   / IDBENNR                   
066200     PERFORM IMS-GET-BENA01-D                                             
066300     PERFORM IMS-GET-BENA11-D                                             
066400     PERFORM UNTIL SEGMENT-SAKNAS                                         
066500*      -- KOPIERA TILL NYA BENÄMNINGSNUMRET                               
066600                                                                          
066700       PERFORM CABA-KOPIERA-BENA11                                        
066800                                                                          
066900       PERFORM IMS-GET-BENA11-D                                           
067000     END-PERFORM                                                          
067100                                                                          
067200                                                                          
067300*    -- LÄS DE  GAMLA HOMONYMKODERNA   IDBENNR-D                          
067400*                                      BEND   / IDBENNR                   
067500     PERFORM IMS-GET-BENA13-D                                             
067600     PERFORM UNTIL SEGMENT-SAKNAS                                         
067700*      -- KOPIERA TILL NYA BENÄMNINGSNUMRET                               
067800       MOVE BEND-HOM-WDD313  TO BENA-HOM-WDD313                           
067900*                                      BENA   / IDBENNR                   
068000       PERFORM IMS-ISRT-BENA13                                            
068100                                                                          
068200       PERFORM IMS-GET-BENA13-D                                           
068300     END-PERFORM                                                          
068400                                                                          
068500*    -- TAG BORT ARTIKEL PÅ GAMLA IDBENNR (IDARTNR)                       
068600*                                           BENC   / IDARTNR              
068700     PERFORM IMS-GU-BENA01-BSEQ                                           
068800     PERFORM IMS-GET-BENA12-BSEQ                                          
068900     PERFORM IMS-DLET-BENA12-BSEQ                                         
069000                                                                          
069100*    -- LÄGG TILL ARTIKEL PÅ NYA IDBENNR                                  
069200     MOVE IDARTNR-WS   TO BENA-ART-IDARTNR                                
069300     MOVE NEJ          TO BENA-ART-FLFELHOMO                              
069400*                                          BENA   / IDBENNR               
069500     PERFORM IMS-ISRT-BENA12                                              
069600                                                                          
069700     IF NEVIS-ARTIKEL                                                     
069800       MOVE JA         TO WS-FLAENDR-A                                    
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 CABA-KOPIERA-BENA11 SECTION.                                             
070300                                                                          
070400     IF BEND-TEXT-IDSKYLT = 'S  ' OR 'GB '                                
070500        IF BEND-TEXT-BEART (1:3) = 'GCP'                                  
070600           MOVE BEND-TEXT-BEART TO WS-BEART-NY                            
070700           MOVE WS-BEART-21     TO BEND-TEXT-BEART                        
070800        END-IF                                                            
070900     END-IF                                                               
071000                                                                          
071100     MOVE BEND-TEXT-WDD311 TO BENA-TEXT-WDD311                            
071200                                                                          
071300*                                          BENA   / IDBENNR               
071400     PERFORM IMS-ISRT-BENA11                                              
071500     .                                                                    
071600     EJECT                                                                
071700 Z-FINIT SECTION.                                                         
071800                                                                          
071900                                                                          
072000     CLOSE W21334                                                         
072100     SKIP2                                                                
072200     MOVE 'S' TO POSTSUM-OPKOD                                            
072300     CALL POSTSUM USING POSTSUM-PARM                                      
072400     .                                                                    
072500     EJECT                                                                
072600 S01-LAES-W21334  SECTION.                                                
072700     SKIP2                                                                
072800     READ W21334 INTO IN-AREA                                             
072900     AT END                                                               
073000        SET END-OF-W21334 TO TRUE                                         
073100                                                                          
073200     NOT AT END                                                           
073300        MOVE 'W21334'  TO POSTSUM-FDNAMN                                  
073400        MOVE 'D1'      TO POSTSUM-DDNAMN2                                 
073500        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
073600        CALL POSTSUM USING POSTSUM-PARM                                   
073700                                                                          
073800        MOVE IN-IDARTNR TO IDARTNR-WS                                     
073900***     ADD 1 TO W-W21334-KVPOST-IN                                       
074000     END-READ                                                             
074100     .                                                                    
074200     EJECT                                                                
074300* IMS SEKTIONER                                                           
074400     SKIP3                                                                
074500 X-TAG-CHECKPOINT   SECTION.                                              
074600                                                                          
074700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
074800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
074900     PERFORM IMS-CHECKPOINT                                               
075000     MOVE ZERO TO CHKP-ANT                                                
075100* --- LÄS OM DATABAS OM DET BEHÖVS                                        
075200     .                                                                    
075300     EJECT                                                                
075400 IMS-GET-XXAI01 SECTION.                                                  
075500     STRING 'WLXXAI01(WDGXKEY  =' W-1207-KEY-X ')'                        
075600             DELIMITED BY SIZE INTO SSA1                                  
075700     MOVE '  GE' TO GODK-STATUSKODER                                      
075800     CALL CBLTDLI USING GU XXAI-PCB DLI-IO-AREA SSA1                      
075900     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
076000     PERFORM IMS-STATUS-KONTROLL                                          
076100     .                                                                    
076200     SKIP3                                                                
076300 IMS-GET-XXAI11 SECTION.                                                  
076400     MOVE 'WLXXAI11 ' TO SSA1                                             
076500     MOVE '  ' TO GODK-STATUSKODER                                        
076600     CALL CBLTDLI USING GHNP XXAI-PCB DLI-IO-AREA SSA1                    
076700     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
076800     PERFORM IMS-STATUS-KONTROLL                                          
076900     .                                                                    
077000     SKIP3                                                                
077100 IMS-REPL-XXAI11 SECTION.                                                 
077200     MOVE '  ' TO GODK-STATUSKODER                                        
077300     CALL CBLTDLI USING REPL XXAI-PCB DLI-IO-AREA                         
077400     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
077500     PERFORM IMS-STATUS-KONTROLL                                          
077600     ADD +1 TO CHKP-ANT                                                   
077700     .                                                                    
077800     EJECT                                                                
077900 IMS-GU-BENA01-BSEQ SECTION.                                              
078000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
078100             DELIMITED BY SIZE INTO SSA1                                  
078200     MOVE '  GE' TO GODK-STATUSKODER                                      
078300     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
078400     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
078500     PERFORM IMS-STATUS-KONTROLL                                          
078600     .                                                                    
078700     SKIP3                                                                
078800 IMS-GU-BENA11-BSEQ SECTION.                                              
078900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
079000             DELIMITED BY SIZE INTO SSA1                                  
079100     MOVE '  GE' TO GODK-STATUSKODER                                      
079200     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
079300     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
079400     PERFORM IMS-STATUS-KONTROLL                                          
079500     .                                                                    
079600     SKIP3                                                                
079700 IMS-GET-BENA12-BSEQ SECTION.                                             
079800     STRING 'WLBENA12(IDARTNR  =' W-IDARTNR-X ')'                         
079900             DELIMITED BY SIZE INTO SSA1                                  
080000     MOVE '  GE' TO GODK-STATUSKODER                                      
080100     CALL CBLTDLI USING GHU BENC-PCB DLI-IO-AREA SSA1                     
080200     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
080300     PERFORM IMS-STATUS-KONTROLL                                          
080400     .                                                                    
080500     EJECT                                                                
080600 IMS-DLET-BENA12-BSEQ SECTION.                                            
080700     MOVE '  ' TO GODK-STATUSKODER                                        
080800     CALL CBLTDLI USING DLET BENC-PCB DLI-IO-AREA                         
080900     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
081000     PERFORM IMS-STATUS-KONTROLL                                          
081100     ADD +1 TO CHKP-ANT                                                   
081200     .                                                                    
081300     SKIP3                                                                
081400 IMS-GU-BENA01-ASEQ SECTION.                                              
081500     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
081600             W-BEART-X ')'                                                
081700             DELIMITED BY SIZE INTO SSA1                                  
081800     MOVE '  GE' TO GODK-STATUSKODER                                      
081900     CALL CBLTDLI USING GU BENB-PCB DLI-IO-AREA SSA1                      
082000     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
082100     PERFORM IMS-STATUS-KONTROLL                                          
082200     .                                                                    
082300     SKIP3                                                                
082400 IMS-GET-BENA01-ASEQ SECTION.                                             
082500     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
082600             W-BEART-X ')'                                                
082700             DELIMITED BY SIZE INTO SSA1                                  
082800     MOVE '  GE' TO GODK-STATUSKODER                                      
082900     CALL CBLTDLI USING GN BENB-PCB DLI-IO-AREA SSA1                      
083000     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
083100     PERFORM IMS-STATUS-KONTROLL                                          
083200     .                                                                    
083300     SKIP3                                                                
083400 IMS-GET-BENA13-ASEQ SECTION.                                             
083500     MOVE 'WLBENA13 ' TO SSA1                                             
083600     MOVE '  GE' TO GODK-STATUSKODER                                      
083700     CALL CBLTDLI USING GNP BENB-PCB DLI-IO-AREA                          
083800     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
083900     PERFORM IMS-STATUS-KONTROLL                                          
084000     .                                                                    
084100     SKIP3                                                                
084200 IMS-GET-BENA01 SECTION.                                                  
084300     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
084400             DELIMITED BY SIZE INTO SSA1                                  
084500     MOVE '  GE' TO GODK-STATUSKODER                                      
084600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
084700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
084800     PERFORM IMS-STATUS-KONTROLL                                          
084900     .                                                                    
085000     EJECT                                                                
085100 IMS-GHU-BENA01 SECTION.                                                  
085200     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
085300             DELIMITED BY SIZE INTO SSA1                                  
085400     MOVE '  GE' TO GODK-STATUSKODER                                      
085500     CALL CBLTDLI USING GHU BENA-PCB DLI-IO-AREA SSA1                     
085600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUS-KONTROLL                                          
085800     .                                                                    
085900     EJECT                                                                
086000 IMS-REPL-BENA01 SECTION.                                                 
086100     MOVE '  ' TO GODK-STATUSKODER                                        
086200     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA                         
086300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
086400     PERFORM IMS-STATUS-KONTROLL                                          
086500     ADD +1 TO CHKP-ANT                                                   
086600     .                                                                    
086700     EJECT                                                                
086800 IMS-GET-BENA11 SECTION.                                                  
086900     MOVE 'WLBENA11 ' TO SSA1                                             
087000     MOVE '  GE' TO GODK-STATUSKODER                                      
087100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
087200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
087300     PERFORM IMS-STATUS-KONTROLL                                          
087400     .                                                                    
087500     SKIP3                                                                
087600 IMS-GNP-BENA11 SECTION.                                                  
087700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
087800             DELIMITED BY SIZE INTO SSA1                                  
087900     MOVE '  GE' TO GODK-STATUSKODER                                      
088000     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
088100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
088200     PERFORM IMS-STATUS-KONTROLL                                          
088300     .                                                                    
088400     SKIP3                                                                
088500 IMS-ISRT-BENA01 SECTION.                                                 
088600     MOVE 'WLBENA01 ' TO SSA1                                             
088700     MOVE '  ' TO GODK-STATUSKODER                                        
088800     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1                    
088900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
089000     PERFORM IMS-STATUS-KONTROLL                                          
089100     ADD +1 TO CHKP-ANT                                                   
089200     .                                                                    
089300     EJECT                                                                
089400 IMS-ISRT-BENA11 SECTION.                                                 
089500     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
089600              DELIMITED BY SIZE INTO SSA1                                 
089700     MOVE 'WLBENA11 ' TO SSA2                                             
089800     MOVE '  ' TO GODK-STATUSKODER                                        
089900     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
090000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
090100     PERFORM IMS-STATUS-KONTROLL                                          
090200     ADD +1 TO CHKP-ANT                                                   
090300     .                                                                    
090400     SKIP3                                                                
090500 IMS-ISRT-BENA12 SECTION.                                                 
090600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X  ')'                        
090700              DELIMITED BY SIZE INTO SSA1                                 
090800     MOVE 'WLBENA12  ' TO SSA2                                            
090900     MOVE '  ' TO GODK-STATUSKODER                                        
091000     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
091100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
091200     PERFORM IMS-STATUS-KONTROLL                                          
091300     ADD +1 TO CHKP-ANT                                                   
091400     .                                                                    
091500     SKIP3                                                                
091600 IMS-ISRT-BENA13 SECTION.                                                 
091700     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X  ')'                        
091800              DELIMITED BY SIZE INTO SSA1                                 
091900     MOVE 'WLBENA13  ' TO SSA2                                            
092000     MOVE '  ' TO GODK-STATUSKODER                                        
092100     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
092200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
092300     PERFORM IMS-STATUS-KONTROLL                                          
092400     ADD +1 TO CHKP-ANT                                                   
092500     .                                                                    
092600     SKIP3                                                                
092700 IMS-GET-BENA13 SECTION.                                                  
092800     MOVE 'WLBENA13 ' TO SSA1                                             
092900     MOVE '  GE' TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
093100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUS-KONTROLL                                          
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-GET-BENA01-D SECTION.                                                
093600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-D-X ')'                       
093700             DELIMITED BY SIZE INTO SSA1                                  
093800     MOVE '  GE' TO GODK-STATUSKODER                                      
093900     CALL CBLTDLI USING GU BEND-PCB DLI-IO-AREA-3 SSA1                    
094000     MOVE BEND-STATUS-CODE TO STATUS-WS                                   
094100     PERFORM IMS-STATUS-KONTROLL                                          
094200     .                                                                    
094300     EJECT                                                                
094400 IMS-GET-BENA11-D SECTION.                                                
094500     MOVE 'WLBENA11 ' TO SSA1                                             
094600     MOVE '  GE' TO GODK-STATUSKODER                                      
094700     CALL CBLTDLI USING GNP BEND-PCB DLI-IO-AREA-3 SSA1                   
094800     MOVE BEND-STATUS-CODE TO STATUS-WS                                   
094900     PERFORM IMS-STATUS-KONTROLL                                          
095000     .                                                                    
095100     SKIP3                                                                
095200 IMS-GET-BENA13-D SECTION.                                                
095300     MOVE 'WLBENA13 ' TO SSA1                                             
095400     MOVE '  GE' TO GODK-STATUSKODER                                      
095500     CALL CBLTDLI USING GNP BEND-PCB DLI-IO-AREA-3 SSA1                   
095600     MOVE BEND-STATUS-CODE TO STATUS-WS                                   
095700     PERFORM IMS-STATUS-KONTROLL                                          
095800     .                                                                    
095900     EJECT                                                                
096000 IMS-RESTART SECTION.                                                     
096100     SKIP2                                                                
096200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
096300     MOVE '  ' TO GODK-STATUSKODER                                        
096400     CALL CBLTDLI USING XRST MSG-PCB                                      
096500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
096600                        CHKP-AREA-LENGTH CHKP-AREA                        
096700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096800     PERFORM IMS-STATUS-KONTROLL                                          
096900     .                                                                    
097000     EJECT                                                                
097100 IMS-CHECKPOINT SECTION.                                                  
097200     SKIP2                                                                
097300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
097400     MOVE '  XD' TO GODK-STATUSKODER                                      
097500     CALL CBLTDLI USING CHKP MSG-PCB                                      
097600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
097700                        CHKP-AREA-LENGTH CHKP-AREA                        
097800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097900     PERFORM IMS-STATUS-KONTROLL                                          
098000                                                                          
098100     IF IMS-EJ-OK                                                         
098200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
098300       DISPLAY FELTEXT                                                    
098400       CALL FELLOG                                                        
098500     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800 IMS-STATUS-KONTROLL SECTION.                                             
098900     SET STATUS-IX TO 1                                                   
099000     SEARCH GODK-STATUS                                                   
099100       AT END                                                             
099200         CALL FELLOG                                                      
099300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
099400       CONTINUE                                                           
099500     END-SEARCH                                                           
099600     .                                                                    
099700     SKIP3                                                                
099800 IMS-GET-ARTC01 SECTION.                                                  
099900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
100000            DELIMITED BY SIZE INTO SSA1                                   
100100     MOVE '  GE' TO GODK-STATUSKODER                                      
100200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
100300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
100400     PERFORM IMS-STATUS-KONTROLL                                          
100500     .                                                                    

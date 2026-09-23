000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6128600.                                                
000400 AUTHOR.         KJELL (JOHAN L).                                         
000500 DATE-WRITTEN.   2011-10-14 (1997-06-23).                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*FUNKTION                                                                 
001000*   SKRIVER UT REFILL/AK FOLLOW-UP-RAPPORTER PER DC                       
001100*   FÖR DC:N SOM KÖR CLASSIC PULS OCH DÄRFÖR FÅR RAPPORTEN                
001200*   VIA EXPRESS DELIVERY ELLER D&P-MAIL/PRINTER                           
001300*   DESSUTOM FÅR LDC:ER OCH ANDRA SOM KÖR WEBB-PULS                       
001400*   MOTSVARANDE RAPPORT TILL FOLLOW-UP-MENYN                              
001500*                                                                         
001600*   PROGRAMMET ÄR EN DEL AV ETT TIDIGARE´W6128600 SOM                     
001700*   DELATS UPP I FLERA DELAR.                                             
001800*   ANDRA DELAR SKAPAR INDATAT (8F) OCH HANTERAR UTSKRIFT                 
001900*   TILL MANAGEMENT-FOLLOW-UP (8G)..                                      
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- INFIL SORTERAD PER DC                                      
003000     SELECT W6128F                     ASSIGN TO W61286D1.                
003100                                                                          
003200*          --- UTLISTA VIA EXPRESS-DELIVERY                               
003300     SELECT W61286-001                 ASSIGN TO W61286D2.                
003400                                                                          
003500*          --- LISTA TILL D&P SKICAS VIA WZ01SEND                         
003600                                                                          
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W6128F                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W6128F      -L.                                                
004700     SKIP3                                                                
004800 FD  W61286-001                                                           
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200 01  W61286-001-RAD              PIC X(121).                              
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W6128600'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900                                                                          
006000 77  SUMMA-POST                  PIC X       VALUE '9'.                   
006100                                                                          
006200 77  KDRC-DISPLAY                PIC Z(5).                                
006300 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006400                                                                          
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006800                                                                          
006900 01  SPAR-IDDC                   PIC XX      VALUE SPACE.                 
007000 01  SPAR-FLWEBDC                PIC X       VALUE SPACE.                 
007100                                                                          
007200 01  SW-RAPPORT-VIA-DAP          PIC X.                                   
007300 01  SW-RAPPORT-SKRIVEN          PIC X       VALUE 'N'.                   
007400                                                                          
007500 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
007600                                                                          
007700 01  W6128F-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W6128F                       VALUE 'J'.                   
007900                                                                          
008000     EJECT                                                                
008100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES DAGENS-DATUM.                                       
008300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008600                                                                          
008700 01  WS-YYMMDDHHMM.                                                       
008800     03 WS-YYMMDD                PIC  9(6).                               
008900     03 WS-TIME                  PIC  9(4).                               
009000                                                                          
009100 01  WS-HHMMSSTH.                                                         
009200     03 WS-HHMM                  PIC  9(4).                               
009300     03 WS-SSTH                  PIC  9(4).                               
009400                                                                          
009500     EJECT                                                                
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010200                                                                          
010300*    --- PARAMETRAR TILL ABEND                                            
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
011100*01  -COPY WZ01SEND                                                       
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL DATKORT                                          
011400*                                                                         
011500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61286'.              
011600     SKIP2                                                                
011700 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
011800     SKIP2                                                                
011900*01  -COPY WDATKORT                                                       
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL POSTSUM                                          
012200*                                                                         
012300*01  -COPY W0005   -PRE  POSTSUM-                                         
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL W612TIME                                         
012600*                                                                         
012700*01  -COPY W612TID     -PRE TIME-                                         
012800     EJECT                                                                
012900*01  -COPY WWDC99                                                         
013000                                                                          
013100     EJECT                                                                
013200 01  IN-AREA-START               PIC X(24)   VALUE                        
013300                                 'IN-AREA-START   '.                      
013400                                                                          
013500*01  AREA -COPY W6128F     -PRE IN-                                       
013600                                                                          
013700     EJECT                                                                
013800                                                                          
013900 01  HDR-AREA-START              PIC X(24)   VALUE                        
014000                                 'HDR-AREA-START  '.                      
014100                                                                          
014200 01  HDR-AREA.                                                            
014300*   03  -COPY WZ01REQU -PRE HDR-                                          
014400*   03  -COPY WZ04HDR                                                     
014500*                                                                         
014600                                                                          
014700 01  WEB-POST-AREA-START         PIC X(24)   VALUE                        
014800                                 'WEB-POST-AREA-START '.                  
014900                                                                          
015000 01  WEB-POST-AREA.                                                       
015100*    03 -COPY W612861 -PRE WEB-                                           
015200                                                                          
015300     EJECT                                                                
015400 01  W001-AREA-START             PIC X(24)   VALUE                        
015500                                 'W001-AREA-START  '.                     
015600                                                                          
015700 01  W001-HJALPAREOR.                                                     
015800*                                                                         
015900     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 3.                
016000     03  W001-CC                 PIC X       VALUE SPACE.                 
016100     03  W001-ANTAL-RADER                                                 
016200                                 PIC 9(3)    VALUE 999.                   
016300     03  W001-MAX-RADER-PER-SIDA                                          
016400                                 PIC 9(3)    VALUE 42.                    
016500     03  W001-MAX-POSITIONER-PER-RAD                                      
016600                                 PIC 9(3)    VALUE 120.                   
016700     03  W001-LISTNR             PIC X(11)   VALUE 'W61286-001'.          
016800     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
016900                                                                          
017000                                                                          
017100 01  W001-RAD.                                                            
017200     03  FILLER                  PIC X(121)  VALUE SPACE.                 
017300                                                                          
017400 01  W001-DAP-POST.                                                       
017500     03  FILLER                  PIC X(121)  VALUE SPACE.                 
017600                                                                          
017700     EJECT                                                                
017800 01  W001-RUBRIK1.                                                        
017900*                                                                         
018000     03  FILLER                  PIC X(3) VALUE SPACE.                    
018100     03  FILLER                  PIC X(23)                                
018200                                VALUE 'VOLVO CAR CUST. SERVICE'.          
018300     03  FILLER                  PIC X(2) VALUE SPACE.                    
018400     03  FILLER                  PIC X(10)                                
018500                                VALUE 'W61286-001'.                       
018600     03  FILLER                  PIC X(4) VALUE SPACE.                    
018700     03  FILLER                  PIC X(20)                                
018800                                VALUE 'PERIOD AK FOLLOW-UP.'.             
018900     03  FILLER                  PIC X(10) VALUE SPACE.                   
019000     03  FILLER                  PIC X(3) VALUE 'DC '.                    
019100     03  RUBRIK-DC-1             PIC XX.                                  
019200     03  FILLER                  PIC X(3) VALUE SPACE.                    
019300     03  FILLER                  PIC X(8) VALUE 'PERIOD: '.               
019400     03  RUBRIK-PERIOD           PIC 9(4).                                
019500     03  FILLER                  PIC X(7) VALUE SPACE.                    
019600     03  RUBRIK-DATUM            PIC XXBXXBXX.                            
019700     03  FILLER                  PIC X(3) VALUE SPACE.                    
019800     03  FILLER                  PIC X(4)                                 
019900                                 VALUE 'PAGE'.                            
020000     03  W001-SID                PIC Z(2)9.                               
020100     EJECT                                                                
020200 01  W001-RUBRIK2.                                                        
020300*                                                                         
020400     03  FILLER                  PIC X(20) VALUE SPACE.                   
020500     03  FILLER                  PIC X(16)                                
020600                                 VALUE 'TIME   IN   DAYS'.                
020700     03  FILLER                  PIC X(7)  VALUE SPACE.                   
020800     03  FILLER                  PIC X(6)  VALUE 'BINNED'.                
020900     03  FILLER                  PIC X(9)  VALUE SPACE.                   
021000     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
021100     03  FILLER                  PIC X(7)  VALUE SPACE.                   
021200     03  FILLER                  PIC X(7)  VALUE 'BINNED'.                
021300     EJECT                                                                
021400 01  W001-RUBRIK3.                                                        
021500*                                                                         
021600     03  FILLER                  PIC X(20) VALUE SPACE.                   
021700     03  FILLER                  PIC X(3)  VALUE 'TOT'.                   
021800     03  FILLER                  PIC X(9)  VALUE SPACE.                   
021900     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
022000     03  FILLER                  PIC X(8)   VALUE SPACE.                  
022100     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
022200     03  FILLER                  PIC X(8)  VALUE SPACE.                   
022300     03  FILLER                  PIC X(5)  VALUE 'LINES'.                 
022400     03  FILLER                  PIC X(8)  VALUE SPACE.                   
022500     03  FILLER                  PIC X(5)  VALUE 'VALUE'.                 
022600     EJECT                                                                
022700 01  W001-DETALJ.                                                         
022800     03 FILLER                     PIC X(3)    VALUE SPACE.               
022900     03 W001-KDREFTYP-TEXT         PIC X(9).                              
023000     03 FILLER                     PIC X       VALUE SPACE.               
023100     03 W001-KVDAGDEC-DAYS-BINNED  PIC Z(7)9.9.                           
023200     03 W001-DAYS-BINNED-X REDEFINES W001-KVDAGDEC-DAYS-BINNED            
023300                                   PIC X(10).                             
023400     03 FILLER                     PIC XXX     VALUE SPACE.               
023500     03 W001-KVDAGDEC-DAYS-PRIO    PIC Z(7)9.9.                           
023600     03 W001-DAYS-PRIO-X REDEFINES W001-KVDAGDEC-DAYS-PRIO                
023700                                   PIC X(10).                             
023800     03 FILLER                     PIC XXX     VALUE SPACE.               
023900     03 W001-KVANTAL-LINES-BINNED  PIC Z(9)9.                             
024000     03 FILLER                     PIC XXX     VALUE SPACE.               
024100     03 W001-KVANTAL-LINES-PRIO    PIC Z(9)9.                             
024200     03 FILLER                     PIC XXX     VALUE SPACE.               
024300     03 W001-SUARTNTO-BINNED       PIC Z(6)9.99.                          
024400                                                                          
024500     EJECT                                                                
024600 PROCEDURE DIVISION.                                                      
024700 MAIN SECTION.                                                            
024800                                                                          
024900     PERFORM A-INIT                                                       
025000     PERFORM S01-LAES-W6128F                                              
025100     PERFORM UNTIL END-OF-W6128F                                          
025200                                                                          
025300       IF IN-IDDC NOT = SPAR-IDDC                                         
025400         IF SW-RAPPORT-SKRIVEN = JA                                       
025500           PERFORM B-SLUT-DC                                              
025600         END-IF                                                           
025700         PERFORM B-NYTT-DC                                                
025800         PERFORM C-SKRIV-RUBRIKER                                         
025900       END-IF                                                             
026000                                                                          
026100*      -- OM INTE ALLT ÄR NOLLOR                                          
026200       IF IN-KVANTAL-LINES-AK > 0                                         
026300       OR IN-KVANTAL-LINES-BINNED > 0                                     
026400       OR IN-KVANTAL-LINES-PRIO > 0                                       
026500       OR IN-SUARTNTO-BINNED > 0                                          
026600         PERFORM D-SKRIV-DATA                                             
026700       END-IF                                                             
026800                                                                          
026900       PERFORM S01-LAES-W6128F                                            
027000     END-PERFORM                                                          
027100                                                                          
027200     IF SW-RAPPORT-SKRIVEN = JA                                           
027300       PERFORM B-SLUT-DC                                                  
027400     END-IF                                                               
027500                                                                          
027600     PERFORM Z-FINIT                                                      
027700     MOVE ZERO TO RETURN-CODE                                             
027800     GOBACK                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT SECTION.                                                          
028200                                                                          
028300     OPEN INPUT  W6128F                                                   
028400     OPEN OUTPUT W61286-001                                               
028500                                                                          
028600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028700                                                                          
028800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
028900     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
029000     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
029100     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
029200                                                                          
029300*    TIME-STAMP FOR D&P HDR-IDLIST                                        
029400     ACCEPT WS-YYMMDD      FROM DATE                                      
029500     ACCEPT WS-HHMMSSTH    FROM TIME                                      
029600     MOVE   WS-HHMM        TO WS-TIME                                     
029700     .                                                                    
029800                                                                          
029900     EJECT                                                                
030000 B-SLUT-DC  SECTION.                                                      
030100                                                                          
030200     IF SW-RAPPORT-VIA-DAP = JA                                           
030300       PERFORM S50-SEND-CLOSE                                             
030400     END-IF                                                               
030500     .                                                                    
030600                                                                          
030700     EJECT                                                                
030800 B-NYTT-DC  SECTION.                                                      
030900                                                                          
031000     MOVE IN-IDDC    TO SPAR-IDDC                                         
031100     MOVE IN-FLWEBDC TO SPAR-FLWEBDC                                      
031200                                                                          
031300*    -- CDC REPORT VIA D&P MAIL.                                          
031400*    -- LAGER SOM KÖR CLASSIC PULS FÅR RAPPORTEN                          
031500*    -- VIA SYSOUT TILL EXPRESS-DELIVERY.                                 
031600*    -- ALLA LAGER SOM KÖR WEBB-PULS (LDC/NDC) FÅR                        
031700*    -- RAPPORTEN VIA D&P TILL "FOLLOW-UP" MENYN.                         
031800     MOVE SPAR-IDDC TO WS-IDDC                                            
031900     IF SPAR-FLWEBDC = JA OR CDC-SE                                       
032000       MOVE JA  TO SW-RAPPORT-VIA-DAP                                     
032100     ELSE                                                                 
032200       MOVE NEJ TO SW-RAPPORT-VIA-DAP                                     
032300     END-IF                                                               
032400                                                                          
032500     IF SW-RAPPORT-VIA-DAP = JA                                           
032600       PERFORM S50-SEND-OPEN                                              
032700       PERFORM S15-SKAPA-DAP-HEADER                                       
032800     END-IF                                                               
032900                                                                          
033000*    -- RÄKNAREN ÖKAS MED 1 VID UTSKRIFT AV SIDRUBRIK                     
033100     MOVE 0 TO W001-SIDRAKNARE                                            
033200     .                                                                    
033300                                                                          
033400     EJECT                                                                
033500 C-SKRIV-RUBRIKER SECTION.                                                
033600                                                                          
033700     IF SPAR-FLWEBDC = NEJ                                                
033800       MOVE DAGENS-DATUM  TO RUBRIK-DATUM                                 
033900       MOVE IN-TIAARP     TO RUBRIK-PERIOD                                
034000                                                                          
034100       ADD 1 TO W001-SIDRAKNARE                                           
034200       MOVE W001-SIDRAKNARE TO W001-SID                                   
034300       MOVE SPAR-IDDC TO RUBRIK-DC-1                                      
034400                                                                          
034500       MOVE 99 TO W001-SKIP                                               
034600       MOVE W001-RUBRIK1 TO W001-RAD                                      
034700       PERFORM S20-SKRIV-SKICKA-RAD                                       
034800                                                                          
034900       MOVE 2 TO W001-SKIP                                                
035000       MOVE W001-RUBRIK2 TO W001-RAD                                      
035100       PERFORM S20-SKRIV-SKICKA-RAD                                       
035200                                                                          
035300       MOVE 1 TO W001-SKIP                                                
035400       MOVE W001-RUBRIK3 TO W001-RAD                                      
035500       PERFORM S20-SKRIV-SKICKA-RAD                                       
035600                                                                          
035700       MOVE +4 TO W001-ANTAL-RADER                                        
035800       MOVE 2 TO W001-SKIP                                                
035900     END-IF                                                               
036000                                                                          
036100     MOVE JA TO SW-RAPPORT-SKRIVEN                                        
036200     .                                                                    
036300                                                                          
036400     EJECT                                                                
036500 D-SKRIV-DATA          SECTION.                                           
036600                                                                          
036700     IF IN-KDREFTYP = SUMMA-POST                                          
036800       IF SPAR-FLWEBDC = JA                                               
036900         PERFORM DC-SKRIV-SUMMAPOST                                       
037000       ELSE                                                               
037100         PERFORM DD-SKRIV-SUMMARAD                                        
037200       END-IF                                                             
037300     ELSE                                                                 
037400       IF SPAR-FLWEBDC = JA                                               
037500         PERFORM DA-SKRIV-DETALJPOST                                      
037600       ELSE                                                               
037700         PERFORM DB-SKRIV-DETALJRAD                                       
037800       END-IF                                                             
037900     END-IF                                                               
038000                                                                          
038100     .                                                                    
038200                                                                          
038300     EJECT                                                                
038400 DA-SKRIV-DETALJPOST    SECTION.                                          
038500                                                                          
038600     MOVE '1         '              TO WEB-IDAFPRCD                       
038700     MOVE SPAR-IDDC                 TO WEB-IDDC                           
038800     MOVE IN-KVDAGDEC-DAYS-BINNED   TO WEB-TIME-TOT                       
038900     MOVE IN-KVDAGDEC-DAYS-PRIO     TO WEB-DAYS-PRIO                      
039000     MOVE IN-KVANTAL-LINES-AK       TO WEB-AK-LINES                       
039100     MOVE IN-KVANTAL-LINES-BINNED   TO WEB-BINNED-LINES                   
039200     MOVE IN-KVANTAL-LINES-PRIO     TO WEB-BINNED-PRIO                    
039300     MOVE IN-TIAARP                 TO WEB-TIAARP                         
039400     MOVE IN-KDREFTYP               TO WEB-KDREFTYP                       
039500*--- MOVE IN-SUARTNTO-BINNED        TO WEB-SUARTNTO-BINNED                
039600                                                                          
039700     PERFORM S50-PUT-WEB-POST                                             
039800     .                                                                    
039900                                                                          
040000     EJECT                                                                
040100 DB-SKRIV-DETALJRAD     SECTION.                                          
040200                                                                          
040300     EVALUATE IN-KDREFTYP                                                 
040400       WHEN 'A'   MOVE 'AIR'        TO W001-KDREFTYP-TEXT                 
040500       WHEN 'B'   MOVE 'BOAT'       TO W001-KDREFTYP-TEXT                 
040600       WHEN 'T'   MOVE 'TRANSFERS'  TO W001-KDREFTYP-TEXT                 
040700       WHEN 'Z'   MOVE 'OTHERS'     TO W001-KDREFTYP-TEXT                 
040800     END-EVALUATE                                                         
040900                                                                          
041000*--- MOVE IN-KVANTAL-LINES-AK       TO W001-KVANTAL-LINES-AK              
041100     MOVE IN-KVDAGDEC-DAYS-BINNED   TO W001-KVDAGDEC-DAYS-BINNED          
041200     MOVE IN-KVDAGDEC-DAYS-PRIO     TO W001-KVDAGDEC-DAYS-PRIO            
041300     MOVE IN-KVANTAL-LINES-BINNED   TO W001-KVANTAL-LINES-BINNED          
041400     MOVE IN-KVANTAL-LINES-PRIO     TO W001-KVANTAL-LINES-PRIO            
041500     MOVE IN-SUARTNTO-BINNED        TO W001-SUARTNTO-BINNED               
041600                                                                          
041700     MOVE W001-DETALJ  TO  W001-RAD                                       
041800     PERFORM S20-SKRIV-SKICKA-RAD                                         
041900                                                                          
042000     MOVE 1 TO W001-SKIP                                                  
042100     .                                                                    
042200                                                                          
042300     EJECT                                                                
042400 DC-SKRIV-SUMMAPOST  SECTION.                                             
042500                                                                          
042600     MOVE '2         '              TO WEB-IDAFPRCD                       
042700     MOVE SPAR-IDDC                 TO WEB-IDDC                           
042800     MOVE IN-KVDAGDEC-DAYS-BINNED   TO WEB-TIME-TOT                       
042900     MOVE IN-KVDAGDEC-DAYS-PRIO     TO WEB-DAYS-PRIO                      
043000     MOVE IN-KVANTAL-LINES-AK       TO WEB-AK-LINES                       
043100     MOVE IN-KVANTAL-LINES-BINNED   TO WEB-BINNED-LINES                   
043200     MOVE IN-KVANTAL-LINES-PRIO     TO WEB-BINNED-PRIO                    
043300     MOVE IN-TIAARP                 TO WEB-TIAARP                         
043400     MOVE SUMMA-POST                TO WEB-KDREFTYP                       
043500*--- MOVE IN-SUARTNTO-BINNED        TO WEB-SUARTNTO-BINNED                
043600                                                                          
043700     PERFORM S50-PUT-WEB-POST                                             
043800     .                                                                    
043900                                                                          
044000     EJECT                                                                
044100 DD-SKRIV-SUMMARAD  SECTION.                                              
044200                                                                          
044300     MOVE 'SUMMARY'                 TO W001-KDREFTYP-TEXT                 
044400                                                                          
044500*    MOVE SPACE                     TO W001-DAYS-BINNED-X                 
044600     MOVE IN-KVDAGDEC-DAYS-BINNED   TO W001-KVDAGDEC-DAYS-BINNED          
044700*    MOVE SPACE                     TO W001-DAYS-PRIO-X                   
044800     MOVE IN-KVDAGDEC-DAYS-PRIO     TO W001-KVDAGDEC-DAYS-PRIO            
044900                                                                          
045000     MOVE IN-KVANTAL-LINES-BINNED   TO W001-KVANTAL-LINES-BINNED          
045100     MOVE IN-KVANTAL-LINES-PRIO     TO W001-KVANTAL-LINES-PRIO            
045200     MOVE IN-SUARTNTO-BINNED        TO W001-SUARTNTO-BINNED               
045300                                                                          
045400     MOVE 2 TO W001-SKIP                                                  
045500     MOVE W001-DETALJ  TO  W001-RAD                                       
045600     PERFORM S20-SKRIV-SKICKA-RAD                                         
045700     .                                                                    
045800                                                                          
045900                                                                          
046000     EJECT                                                                
046100 Z-FINIT SECTION.                                                         
046200                                                                          
046300     CLOSE W6128F                                                         
046400     CLOSE W61286-001                                                     
046500     SKIP2                                                                
046600     MOVE 'S' TO POSTSUM-OPKOD                                            
046700     CALL POSTSUM USING POSTSUM-PARM                                      
046800     .                                                                    
046900     EJECT                                                                
047000 S01-LAES-W6128F  SECTION.                                                
047100     READ W6128F INTO IN-AREA                                             
047200     AT END                                                               
047300        MOVE HIGH-VALUE TO IN-AREA                                        
047400        SET END-OF-W6128F TO TRUE                                         
047500                                                                          
047600     NOT AT END                                                           
047700        MOVE 'W6128F'   TO POSTSUM-FDNAMN                                 
047800        MOVE 'W61286D1' TO POSTSUM-DDNAMN2                                
047900        MOVE IN-IDDC    TO POSTSUM-TRANSTYP                               
048000        CALL POSTSUM USING POSTSUM-PARM                                   
048100     END-READ                                                             
048200                                                                          
048300     .                                                                    
048400                                                                          
048500     EJECT                                                                
048600 S15-SKAPA-DAP-HEADER      SECTION.                                       
048700                                                                          
048800     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
048900     MOVE 1                          TO HDR-REQU-IDMSGVER                 
049000     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
049100     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
049200                                                                          
049300     MOVE SPACE                      TO HDR-IDOUTREC                      
049400                                                                          
049500*    -- OUTTYPE OCH OUTREC ÄR OLIKA FÖR WEBB-DC OCH NDC-JP/AU             
049600     IF NDC-PACIFIC OR CDC-SE                                             
049700       MOVE SPACE                    TO HDR-IDOUTTYPE                     
049800       MOVE 'W61286-0'               TO HDR-IDOUTTYPE(1:8)                
049900       MOVE SPAR-IDDC                TO HDR-IDOUTTYPE(9:2)                
050000                                                                          
050100       MOVE 'W61286'                 TO HDR-IDOUTREC                      
050200     END-IF                                                               
050300     IF SPAR-FLWEBDC = JA                                                 
050400       MOVE 'REFILL-AK-PER'          TO HDR-IDOUTTYPE                     
050500                                                                          
050600       MOVE SPACE                    TO HDR-IDOUTREC                      
050700       MOVE SPAR-IDDC                TO HDR-IDOUTREC(1:2)                 
050800       MOVE 'W61286'                 TO HDR-IDOUTREC(3:8)                 
050900     END-IF                                                               
051000                                                                          
051100     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
051200                                                                          
051300     PERFORM S50-PUT-HEADER                                               
051400     .                                                                    
051500                                                                          
051600     EJECT                                                                
051700 S20-SKRIV-SKICKA-RAD   SECTION.                                          
051800                                                                          
051900*    -- TRADITIONELL RAPPORT VIA PRINTER ELLER D&P                        
052000     IF SW-RAPPORT-VIA-DAP = JA                                           
052100       IF W001-SKIP = 99                                                  
052200*        -- PAGE SKIP - BYT TILL ASA STYRTECKEN                           
052300         MOVE '1'        TO W001-RAD(1:1)                                 
052400         MOVE 1 TO W001-SKIP                                              
052500       END-IF                                                             
052600*      -- EMULERA ÖVRIGA SKIPS MED BLANKA RADER                           
052700       IF W001-SKIP = 3                                                   
052800         MOVE SPACE      TO W001-DAP-POST                                 
052900         PERFORM S50-PUT-REPORT-LINE                                      
053000       END-IF                                                             
053100       IF W001-SKIP = 2 OR 3                                              
053200         MOVE SPACE      TO W001-DAP-POST                                 
053300         PERFORM S50-PUT-REPORT-LINE                                      
053400       END-IF                                                             
053500       MOVE W001-RAD TO W001-DAP-POST                                     
053600       PERFORM S50-PUT-REPORT-LINE                                        
053700                                                                          
053800     ELSE                                                                 
053900       IF W001-SKIP = 99                                                  
054000         WRITE W61286-001-RAD FROM W001-RAD AFTER PAGE                    
054100         MOVE 1 TO W001-SKIP                                              
054200       ELSE                                                               
054300         WRITE W61286-001-RAD FROM W001-RAD AFTER W001-SKIP               
054400       END-IF                                                             
054500     END-IF                                                               
054600     .                                                                    
054700                                                                          
054800     EJECT                                                                
054900 S50-SEND-OPEN SECTION.                                                   
055000                                                                          
055100     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
055200     MOVE 'OPEN'                          TO SEND-KDFUNC                  
055300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
055400                         SEND-OPEN-AREA                                   
055500     IF SEND-KDRC > ZERO                                                  
055600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
055700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
055800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
055900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056000     END-IF                                                               
056100     .                                                                    
056200     SKIP3                                                                
056300 S50-PUT-HEADER SECTION.                                                  
056400     MOVE 'PUT'                           TO SEND-KDFUNC                  
056500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
056600     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
056700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
056800                         SEND-KVDLEN                                      
056900                         HDR-AREA                                         
057000     IF SEND-KDRC > ZERO                                                  
057100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
057200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
057300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057500     END-IF                                                               
057600     .                                                                    
057700                                                                          
057800     EJECT                                                                
057900 S50-PUT-REPORT-LINE       SECTION.                                       
058000                                                                          
058100     MOVE 'PUT'                           TO SEND-KDFUNC                  
058200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
058300     MOVE LENGTH OF W001-DAP-POST         TO SEND-KVDLEN                  
058400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
058500                         SEND-KVDLEN                                      
058600                         W001-DAP-POST                                    
058700     IF SEND-KDRC > ZERO                                                  
058800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
058900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
059000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
059100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
059200     END-IF                                                               
059300                                                                          
059400     MOVE 'DAP'      TO POSTSUM-FDNAMN                                    
059500     MOVE 'DAP'      TO POSTSUM-DDNAMN2                                   
059600     MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                  
059700     CALL POSTSUM USING POSTSUM-PARM                                      
059800     .                                                                    
059900                                                                          
060000 S50-PUT-WEB-POST          SECTION.                                       
060100                                                                          
060200     MOVE 'PUT'                           TO SEND-KDFUNC                  
060300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
060400     MOVE LENGTH OF WEB-POST-AREA         TO SEND-KVDLEN                  
060500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
060600                         SEND-KVDLEN                                      
060700                         WEB-POST-AREA                                    
060800     IF SEND-KDRC > ZERO                                                  
060900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
061000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
061100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
061200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
061300     END-IF                                                               
061400                                                                          
061500     MOVE 'WEB'      TO POSTSUM-FDNAMN                                    
061600     MOVE 'WEB'      TO POSTSUM-DDNAMN2                                   
061700     MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                  
061800     CALL POSTSUM USING POSTSUM-PARM                                      
061900     .                                                                    
062000                                                                          
062100 S50-SEND-CLOSE SECTION.                                                  
062200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
062300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
062400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
062500     .                                                                    
062600                                                                          
062700 S99-ABEND SECTION.                                                       
062800                                                                          
062900     SKIP2                                                                
063000     MOVE 'S' TO POSTSUM-OPKOD                                            
063100     CALL POSTSUM USING POSTSUM-PARM                                      
063200     CALL ABEND USING RKOD-ABEND                                          
063300     .                                                                    

000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6128G00.                                                
000400 AUTHOR.         KJELL (JOHAN L).                                         
000500 DATE-WRITTEN.   2011-10-18 (1997-06-23).                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*FUNKTION                                                                 
001000*   SKRIVER UT REFILL/AK MANAGEMENT-FOLLOW-UP-RAPPORTER                   
001100*   PER FÖRETAG OCH VECKA (57=LDC-EU, 60=KINA)                            
001200*                                                                         
001300*   PROGRAMMET ÄR EN DEL AV ETT TIDIGARE´W6128600 SOM                     
001400*   DELATS UPP I FLERA DELAR.                                             
001500*   ANDRA DELAR SKAPAR INDATAT (8F) OCH HANTERAR UTSKRIFT                 
001600*   TILL VANLIG VECKO-FOLLOW-UP (86)..                                    
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL SORTERAD PER FÖRETAG                                 
002700     SELECT W6128F                     ASSIGN TO W6128GD1.                
002800                                                                          
002900*          --- RAPPORDATA TILL WEB SKICKAS VIA WZ01SEND                   
003000                                                                          
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W6128F                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W6128F      -L.                                                
004100                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W6128G00'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  SUMMA-POST                  PIC X       VALUE '9'.                   
005000                                                                          
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005300                                                                          
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 01  SPAR-KDMFUP                 PIC XX      VALUE SPACE.                 
005900 01  SPAR-IDDC                   PIC XX      VALUE ZERO.                  
006000 01  SPAR-IDLANDX2               PIC XX      VALUE SPACE.                 
006100 01  SPAR-FLWEBDC                PIC X       VALUE SPACE.                 
006200                                                                          
006300 01  SW-SEND-IS-OPEN             PIC X       VALUE 'N'.                   
006400                                                                          
006500 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
006600                                                                          
006700 01  W6128F-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W6128F                       VALUE 'J'.                   
006900                                                                          
007000     EJECT                                                                
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600                                                                          
007700 01  WS-YYMMDDHHMM.                                                       
007800     03 WS-YYMMDD                PIC  9(6).                               
007900     03 WS-TIME                  PIC  9(4).                               
008000                                                                          
008100 01  WS-HHMMSSTH.                                                         
008200     03 WS-HHMM                  PIC  9(4).                               
008300     03 WS-SSTH                  PIC  9(4).                               
008400                                                                          
008500     EJECT                                                                
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009200                                                                          
009300*    --- PARAMETRAR TILL ABEND                                            
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
010100*01  -COPY WZ01SEND                                                       
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL DATKORT                                          
010400*                                                                         
010500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6128G'.              
010600     SKIP2                                                                
010700 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
010800     SKIP2                                                                
010900*01  -COPY WDATKORT                                                       
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL POSTSUM                                          
011200*                                                                         
011300*01  -COPY W0005   -PRE  POSTSUM-                                         
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL W612TIME                                         
011600*                                                                         
011700*01  -COPY W612TID     -PRE TIME-                                         
011800     EJECT                                                                
011900*01  -COPY WDATAREA                                                       
012000     EJECT                                                                
012400 01  IN-AREA-START               PIC X(24)   VALUE                        
012500                                 'IN-AREA-START   '.                      
012600                                                                          
012700*01  AREA -COPY W6128F     -PRE IN-                                       
012800                                                                          
012900     EJECT                                                                
013000                                                                          
013100 01  HDR-AREA-START              PIC X(24)   VALUE                        
013200                                 'HDR-AREA-START  '.                      
013300                                                                          
013400 01  HDR-AREA.                                                            
013500*   03  -COPY WZ01REQU -PRE HDR-                                          
013600*   03  -COPY WZ04HDR                                                     
013700*                                                                         
013800                                                                          
013900 01  WEB-POST-AREA-START         PIC X(24)   VALUE                        
014000                                 'WEB-POST-AREA-START '.                  
014100                                                                          
014200 01  WEB-POST-AREA.                                                       
014300*    03 -COPY W612862 -PRE WEB-                                           
014400                                                                          
014500     EJECT                                                                
014600 PROCEDURE DIVISION.                                                      
014700 MAIN SECTION.                                                            
014800                                                                          
014900     PERFORM A-INIT                                                       
015000     PERFORM S01-LAES-W6128F                                              
015100     PERFORM UNTIL END-OF-W6128F                                          
015200                                                                          
015300       IF IN-IDLANDX2 NOT = SPAR-IDLANDX2                                 
015400         PERFORM C2-SLUT-IDLANDX2                                         
015500         PERFORM B2-NYTT-IDLANDX2                                         
015600       END-IF                                                             
015700                                                                          
015800       IF IN-KDMFUP NOT = SPAR-KDMFUP                                     
015900         PERFORM C1-SLUT-KONTINENT                                        
016000         PERFORM B1-NY-KONTINENT                                          
016100       END-IF                                                             
016200                                                                          
016300       IF IN-IDDC NOT = SPAR-IDDC                                         
016400         PERFORM B3-NYTT-DC                                               
016500       END-IF                                                             
016600                                                                          
016700*      -- OM INTE ALLT ÄR NOLLOR                                          
016800       IF IN-KVANTAL-LINES-AK > 0                                         
016900       OR IN-KVANTAL-LINES-BINNED > 0                                     
017000       OR IN-KVANTAL-LINES-PRIO > 0                                       
017100       OR IN-SUARTNTO-BINNED > 0                                          
017200         PERFORM D-SKRIV-DATA                                             
017300       END-IF                                                             
017400                                                                          
017500       PERFORM S01-LAES-W6128F                                            
017600     END-PERFORM                                                          
017700     IF SW-SEND-IS-OPEN = JA                                              
017800       PERFORM C1-SLUT-KONTINENT                                          
017900     END-IF                                                               
018000                                                                          
018100     PERFORM Z-FINIT                                                      
018200     MOVE ZERO TO RETURN-CODE                                             
018300     GOBACK                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 A-INIT SECTION.                                                          
018700                                                                          
018800     OPEN INPUT  W6128F                                                   
018900                                                                          
019000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019100                                                                          
019200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019300     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
019400     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
019500     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
019600                                                                          
019700*    TIME-STAMP FOR D&P HDR-IDLIST                                        
019800     ACCEPT WS-YYMMDD      FROM DATE                                      
019900     ACCEPT WS-HHMMSSTH    FROM TIME                                      
020000     MOVE   WS-HHMM        TO WS-TIME                                     
020100     .                                                                    
020200                                                                          
020300     EJECT                                                                
020400 B1-NY-KONTINENT SECTION.                                                 
020500                                                                          
020600     MOVE IN-KDMFUP TO SPAR-KDMFUP                                        
020700     .                                                                    
020800                                                                          
020900     EJECT                                                                
021000 B2-NYTT-IDLANDX2 SECTION.                                                
021100                                                                          
021200     MOVE IN-IDLANDX2 TO SPAR-IDLANDX2                                    
021300     .                                                                    
021400                                                                          
021500     EJECT                                                                
021600 B3-NYTT-DC       SECTION.                                                
021700                                                                          
021800     MOVE IN-IDDC     TO SPAR-IDDC                                        
021900     MOVE IN-FLWEBDC  TO SPAR-FLWEBDC                                     
022000                                                                          
022100*    -- MANAGEMENT-RAPPORTER SKAPAS BARA FÖR DC:N SOM KÖR                 
022200*    -- WEB-PULS                                                          
022300     IF SPAR-FLWEBDC = JA                                                 
022400     AND SW-SEND-IS-OPEN = NEJ                                            
022500       PERFORM S50-SEND-OPEN                                              
022600       PERFORM S15-SKAPA-DAP-HEADER                                       
022700     END-IF                                                               
022800     .                                                                    
022900                                                                          
023000     EJECT                                                                
023100 C1-SLUT-KONTINENT SECTION.                                               
023200                                                                          
023300     IF SW-SEND-IS-OPEN = JA                                              
023400       PERFORM S50-SEND-CLOSE                                             
023500     END-IF                                                               
023600     .                                                                    
023700                                                                          
023800 C2-SLUT-IDLANDX2   SECTION.                                              
023900                                                                          
024000     IF SPAR-FLWEBDC = JA                                                 
024100     AND SW-SEND-IS-OPEN = JA                                             
024200*      -- GENERATE A BLANK LINE BETWEEN COUNTRIES                         
024300       MOVE SPACE         TO WEB-POST-AREA                                
024400       MOVE '1         '  TO WEB-IDAFPRCD                                 
024500                                                                          
024600       PERFORM S50-PUT-WEB-POST                                           
024700     END-IF                                                               
024800     .                                                                    
024900                                                                          
025000 D-SKRIV-DATA    SECTION.                                                 
025100                                                                          
025200*    -- MANAGEMENT-RAPPORTER SKAPAS BARA FÖR DC:N SOM KÖR                 
025300*    -- WEB-PULS                                                          
025400     IF SPAR-FLWEBDC = JA                                                 
025500       IF IN-KDREFTYP = SUMMA-POST                                        
025600         PERFORM DC-SKRIV-SUMMAPOST                                       
025700       ELSE                                                               
025800         PERFORM DA-SKRIV-DETALJPOST                                      
025900       END-IF                                                             
026000     END-IF                                                               
026100     .                                                                    
026200                                                                          
026300     EJECT                                                                
026400 DA-SKRIV-DETALJPOST    SECTION.                                          
026500                                                                          
026600     MOVE '1         '            TO WEB-IDAFPRCD                         
026700     MOVE IN-IDDC                 TO WEB-IDDC                             
026800     MOVE IN-KVANTAL-LINES-AK     TO WEB-AK-LINES                         
026900     MOVE IN-KVANTAL-LINES-BINNED TO WEB-BINNED-LINES                     
027000     MOVE IN-KVANTAL-LINES-PRIO   TO WEB-BINNED-PRIO                      
027100     MOVE IN-KVDAGDEC-DAYS-BINNED TO WEB-TIME-TOT                         
027200     MOVE IN-KVDAGDEC-DAYS-PRIO   TO WEB-DAYS-PRIO                        
027300     MOVE IN-TIAARP               TO WEB-TIAARP                           
027400     MOVE IN-IDLANDX2             TO WEB-IDLANDX2                         
027500     MOVE IN-ADCITY               TO WEB-ADCITY                           
027600     MOVE IN-SUARTNTO-BINNED      TO WEB-SUARTNTO-BINNED                  
027700     MOVE IN-KDREFTYP             TO WEB-KDREFTYP                         
027800                                                                          
027900     PERFORM S50-PUT-WEB-POST                                             
028000     .                                                                    
028100                                                                          
028200     EJECT                                                                
028300 DC-SKRIV-SUMMAPOST     SECTION.                                          
028400                                                                          
028500     MOVE '2         '            TO WEB-IDAFPRCD                         
028600     MOVE IN-IDDC                 TO WEB-IDDC                             
028700     MOVE IN-KVANTAL-LINES-AK     TO WEB-AK-LINES                         
028800     MOVE IN-KVANTAL-LINES-BINNED TO WEB-BINNED-LINES                     
028900     MOVE IN-KVANTAL-LINES-PRIO   TO WEB-BINNED-PRIO                      
029000     MOVE IN-KVDAGDEC-DAYS-BINNED TO WEB-TIME-TOT                         
029100     MOVE IN-KVDAGDEC-DAYS-PRIO   TO WEB-DAYS-PRIO                        
029200     MOVE IN-TIAARP               TO WEB-TIAARP                           
029300     MOVE IN-IDLANDX2             TO WEB-IDLANDX2                         
029400     MOVE IN-ADCITY               TO WEB-ADCITY                           
029500     MOVE IN-SUARTNTO-BINNED      TO WEB-SUARTNTO-BINNED                  
029600     MOVE IN-KDREFTYP             TO WEB-KDREFTYP                         
029700                                                                          
029800     PERFORM S50-PUT-WEB-POST                                             
029900     .                                                                    
030000                                                                          
030100     EJECT                                                                
030200 Z-FINIT SECTION.                                                         
030300                                                                          
030400     CLOSE W6128F                                                         
030500                                                                          
030600     MOVE 'S' TO POSTSUM-OPKOD                                            
030700     CALL POSTSUM USING POSTSUM-PARM                                      
030800     .                                                                    
030900     EJECT                                                                
031000 S01-LAES-W6128F  SECTION.                                                
031100     READ W6128F INTO IN-AREA                                             
031200     AT END                                                               
031300        MOVE HIGH-VALUE TO IN-AREA                                        
031400        SET END-OF-W6128F TO TRUE                                         
031500                                                                          
031600     NOT AT END                                                           
031700        MOVE 'W6128F'   TO POSTSUM-FDNAMN                                 
031800        MOVE 'W6128GD1' TO POSTSUM-DDNAMN2                                
031900        MOVE IN-IDDC    TO POSTSUM-TRANSTYP                               
032000        CALL POSTSUM USING POSTSUM-PARM                                   
032100     END-READ                                                             
032200                                                                          
032300     .                                                                    
032400                                                                          
032500     EJECT                                                                
032600 S15-SKAPA-DAP-HEADER      SECTION.                                       
032700                                                                          
032800     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
032900     MOVE 1                          TO HDR-REQU-IDMSGVER                 
033000     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
033100     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
033200                                                                          
033300     MOVE 'MAN-REF-AK-PER'           TO HDR-IDOUTTYPE                     
033400                                                                          
033500     MOVE SPACE                      TO HDR-IDOUTREC                      
033600     MOVE SPAR-KDMFUP                TO HDR-IDOUTREC(1:2)                 
033700     MOVE 'W6128G'                   TO HDR-IDOUTREC(3:8)                 
033800                                                                          
033900     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
034000                                                                          
034100     PERFORM S50-PUT-HEADER                                               
034200     .                                                                    
034300                                                                          
034400     EJECT                                                                
034500 S50-SEND-OPEN SECTION.                                                   
034600                                                                          
034700     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
034800     MOVE 'OPEN'                          TO SEND-KDFUNC                  
034900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035000                         SEND-OPEN-AREA                                   
035100     IF SEND-KDRC > ZERO                                                  
035200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
035400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035600     END-IF                                                               
035700                                                                          
035800     MOVE JA TO SW-SEND-IS-OPEN                                           
035900     .                                                                    
036000     SKIP3                                                                
036100 S50-PUT-HEADER SECTION.                                                  
036200     MOVE 'PUT'                           TO SEND-KDFUNC                  
036300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
036400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
036500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036600                         SEND-KVDLEN                                      
036700                         HDR-AREA                                         
036800     IF SEND-KDRC > ZERO                                                  
036900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
037100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
037200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037300     END-IF                                                               
037400     .                                                                    
037500                                                                          
037600     EJECT                                                                
037700 S50-PUT-WEB-POST          SECTION.                                       
037800                                                                          
037900     MOVE 'PUT'                           TO SEND-KDFUNC                  
038000     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
038100     MOVE LENGTH OF WEB-POST-AREA         TO SEND-KVDLEN                  
038200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038300                         SEND-KVDLEN                                      
038400                         WEB-POST-AREA                                    
038500     IF SEND-KDRC > ZERO                                                  
038600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
038700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
038800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
038900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039000     END-IF                                                               
039100                                                                          
039200     MOVE 'WEB'      TO POSTSUM-FDNAMN                                    
039300     MOVE 'WEB'      TO POSTSUM-DDNAMN2                                   
039400     MOVE IN-IDDC    TO POSTSUM-TRANSTYP                                  
039500     CALL POSTSUM USING POSTSUM-PARM                                      
039600     .                                                                    
039700                                                                          
039800 S50-SEND-CLOSE SECTION.                                                  
039900                                                                          
040000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
040100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
040200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
040300                                                                          
040400     MOVE NEJ TO SW-SEND-IS-OPEN                                          
040500     .                                                                    

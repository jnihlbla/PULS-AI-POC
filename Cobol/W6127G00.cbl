000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6127G00.                                                
000400 AUTHOR.         KJELL (JOHAN L).                                         
000500 DATE-WRITTEN.   2011-10-18 (1997-06-23).                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*FUNKTION                                                                 
001000*   SKRIVER UT REFILL/AK MANAGEMENT-FOLLOW-UP-RAPPORTER                   
001100*   PER FÖRETAG OCH VECKA (57=LDC-EU, 60=KINA)                            
001200*                                                                         
001300*   PROGRAMMET ÄR EN DEL AV ETT TIDIGARE´W6127600 SOM                     
001400*   DELATS UPP I FLERA DELAR.                                             
001500*   ANDRA DELAR SKAPAR INDATAT (7F) OCH HANTERAR UTSKRIFT                 
001600*   TILL VANLIG VECKO-FOLLOW-UP (76)..                                    
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
002700     SELECT W6127F                     ASSIGN TO W6127GD1.                
002800                                                                          
002900*          --- RAPPORDATA TILL WEB SKICKAS VIA WZ01SEND                   
003000                                                                          
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W6127F                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W6127F      -L.                                                
004100                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W6127G00'.            
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
005800 01  SPAR-IDDC                   PIC XX      VALUE ZERO.                  
005900 01  SPAR-IDLANDX2               PIC XX      VALUE SPACE.                 
006000 01  SPAR-FLWEBDC                PIC X       VALUE SPACE.                 
006100                                                                          
006200*    AREA IS MA CN OR PF                                                  
006300 01  SPAR-KDMFUP                 PIC XX      VALUE SPACE.                 
006400                                                                          
006500 01  SW-SEND-IS-OPEN             PIC X       VALUE 'N'.                   
006600                                                                          
006700 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
006800                                                                          
006900 01  W6127F-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-W6127F                       VALUE 'J'.                   
007100                                                                          
007200     EJECT                                                                
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800                                                                          
007900 01  WS-YYMMDDHHMM.                                                       
008000     03 WS-YYMMDD                PIC  9(6).                               
008100     03 WS-TIME                  PIC  9(4).                               
008200                                                                          
008300 01  WS-HHMMSSTH.                                                         
008400     03 WS-HHMM                  PIC  9(4).                               
008500     03 WS-SSTH                  PIC  9(4).                               
008600                                                                          
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
009200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009400                                                                          
009500*    --- PARAMETRAR TILL ABEND                                            
009600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
010300*01  -COPY WZ01SEND                                                       
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL DATKORT                                          
010600*                                                                         
010700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6127G'.              
010800     SKIP2                                                                
010900 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
011000     SKIP2                                                                
011100*01  -COPY WDATKORT                                                       
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL POSTSUM                                          
011400*                                                                         
011500*01  -COPY W0005   -PRE  POSTSUM-                                         
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL W612TIME                                         
011800*                                                                         
011900*01  -COPY W612TID     -PRE TIME-                                         
012000     EJECT                                                                
012100*01  -COPY WDATAREA                                                       
012200     EJECT                                                                
012500 01  IN-AREA-START               PIC X(24)   VALUE                        
012600                                 'IN-AREA-START   '.                      
012700                                                                          
012800*01  AREA -COPY W6127F     -PRE IN-                                       
012900                                                                          
013000     EJECT                                                                
013100                                                                          
013200 01  HDR-AREA-START              PIC X(24)   VALUE                        
013300                                 'HDR-AREA-START  '.                      
013400                                                                          
013500 01  HDR-AREA.                                                            
013600*   03  -COPY WZ01REQU -PRE HDR-                                          
013700*   03  -COPY WZ04HDR                                                     
013800*                                                                         
013900                                                                          
014000 01  WEB-POST-AREA-START         PIC X(24)   VALUE                        
014100                                 'WEB-POST-AREA-START '.                  
014200                                                                          
014300 01  WEB-POST-AREA.                                                       
014400*    03 -COPY W612762 -PRE WEB-                                           
014500                                                                          
014600     EJECT                                                                
014700 PROCEDURE DIVISION.                                                      
014800 MAIN SECTION.                                                            
014900                                                                          
015000     PERFORM A-INIT                                                       
015100     PERFORM S01-LAES-W6127F                                              
015200     PERFORM UNTIL END-OF-W6127F                                          
015300                                                                          
015400       IF IN-IDLANDX2 NOT = SPAR-IDLANDX2                                 
015500         PERFORM C2-SLUT-IDLANDX2                                         
015600         PERFORM B2-NYTT-IDLANDX2                                         
015700       END-IF                                                             
015800                                                                          
015900       IF IN-KDMFUP NOT = SPAR-KDMFUP                                     
016000         PERFORM C1-SLUT-KONTINENT                                        
016100         PERFORM B1-NY-KONTINENT                                          
016200       END-IF                                                             
016300                                                                          
016400       IF IN-IDDC NOT = SPAR-IDDC                                         
016500         PERFORM B3-NYTT-DC                                               
016600       END-IF                                                             
016700                                                                          
016800*      -- OM INTE ALLT ÄR NOLLOR                                          
016900       IF IN-KVANTAL-LINES-AK > 0                                         
017000       OR IN-KVANTAL-LINES-BINNED > 0                                     
017100       OR IN-KVANTAL-LINES-PRIO > 0                                       
017200       OR IN-SUARTNTO-BINNED > 0                                          
017300         PERFORM D-SKRIV-DATA                                             
017400       END-IF                                                             
017500                                                                          
017600       PERFORM S01-LAES-W6127F                                            
017700     END-PERFORM                                                          
017800     IF SW-SEND-IS-OPEN = JA                                              
017900       PERFORM C1-SLUT-KONTINENT                                          
018000     END-IF                                                               
018100                                                                          
018200     PERFORM Z-FINIT                                                      
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     OPEN INPUT  W6127F                                                   
019000                                                                          
019100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019200                                                                          
019300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019400     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
019500     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
019600     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
019700                                                                          
019800*    TIME-STAMP FOR D&P HDR-IDLIST                                        
019900     ACCEPT WS-YYMMDD      FROM DATE                                      
020000     ACCEPT WS-HHMMSSTH    FROM TIME                                      
020100     MOVE   WS-HHMM        TO WS-TIME                                     
020200     .                                                                    
020300                                                                          
020400     EJECT                                                                
020500 B1-NY-KONTINENT SECTION.                                                 
020600                                                                          
020700     MOVE IN-KDMFUP TO SPAR-KDMFUP                                        
020800     .                                                                    
020900                                                                          
021000     EJECT                                                                
021100 B2-NYTT-IDLANDX2 SECTION.                                                
021200                                                                          
021300     MOVE IN-IDLANDX2 TO SPAR-IDLANDX2                                    
021400     .                                                                    
021500                                                                          
021600     EJECT                                                                
021700 B3-NYTT-DC       SECTION.                                                
021800                                                                          
021900     MOVE IN-IDDC     TO SPAR-IDDC                                        
022000     MOVE IN-FLWEBDC  TO SPAR-FLWEBDC                                     
022100                                                                          
022200*    -- MANAGEMENT-RAPPORTER SKAPAS BARA FÖR DC:N SOM KÖR                 
022300*    -- WEB-PULS                                                          
022400     IF SPAR-FLWEBDC = JA                                                 
022500     AND SW-SEND-IS-OPEN = NEJ                                            
022600       PERFORM S50-SEND-OPEN                                              
022700       PERFORM S15-SKAPA-DAP-HEADER                                       
022800     END-IF                                                               
022900     .                                                                    
023000                                                                          
023100     EJECT                                                                
023200 C1-SLUT-KONTINENT SECTION.                                               
023300                                                                          
023400     IF SW-SEND-IS-OPEN = JA                                              
023500       PERFORM S50-SEND-CLOSE                                             
023600     END-IF                                                               
023700     .                                                                    
023800                                                                          
023900 C2-SLUT-IDLANDX2   SECTION.                                              
024000                                                                          
024100     IF SPAR-FLWEBDC = JA                                                 
024200     AND SW-SEND-IS-OPEN = JA                                             
024300*      -- GENERATE A BLANK LINE BETWEEN COUNTRIES                         
024400       MOVE SPACE         TO WEB-POST-AREA                                
024500       MOVE '1         '  TO WEB-IDAFPRCD                                 
024600                                                                          
024700       PERFORM S50-PUT-WEB-POST                                           
024800     END-IF                                                               
024900     .                                                                    
025000                                                                          
025100 D-SKRIV-DATA    SECTION.                                                 
025200                                                                          
025300*    -- MANAGEMENT-RAPPORTER SKAPAS BARA FÖR DC:N SOM KÖR                 
025400*    -- WEB-PULS                                                          
025500     IF SPAR-FLWEBDC = JA                                                 
025600       IF IN-KDREFTYP = SUMMA-POST                                        
025700         PERFORM DC-SKRIV-SUMMAPOST                                       
025800       ELSE                                                               
025900         PERFORM DA-SKRIV-DETALJPOST                                      
026000       END-IF                                                             
026100     END-IF                                                               
026200     .                                                                    
026300                                                                          
026400     EJECT                                                                
026500 DA-SKRIV-DETALJPOST    SECTION.                                          
026600                                                                          
026700     MOVE '1         '            TO WEB-IDAFPRCD                         
026800     MOVE IN-IDDC                 TO WEB-IDDC                             
026900     MOVE IN-KVANTAL-LINES-AK     TO WEB-AK-LINES                         
027000     MOVE IN-KVANTAL-LINES-BINNED TO WEB-BINNED-LINES                     
027100     MOVE IN-KVANTAL-LINES-PRIO   TO WEB-BINNED-PRIO                      
027200     MOVE IN-KVDAGDEC-DAYS-BINNED TO WEB-TIME-TOT                         
027300     MOVE IN-KVDAGDEC-DAYS-PRIO   TO WEB-DAYS-PRIO                        
027400     MOVE IN-TIAAVV               TO WEB-TIAAVV                           
027500     MOVE IN-IDLANDX2             TO WEB-IDLANDX2                         
027600     MOVE IN-ADCITY               TO WEB-ADCITY                           
027700     MOVE IN-SUARTNTO-BINNED      TO WEB-SUARTNTO-BINNED                  
027800     MOVE IN-KDREFTYP             TO WEB-KDREFTYP                         
027900                                                                          
028000     PERFORM S50-PUT-WEB-POST                                             
028100     .                                                                    
028200                                                                          
028300     EJECT                                                                
028400 DC-SKRIV-SUMMAPOST     SECTION.                                          
028500                                                                          
028600     MOVE '2         '            TO WEB-IDAFPRCD                         
028700     MOVE IN-IDDC                 TO WEB-IDDC                             
028800     MOVE IN-KVANTAL-LINES-AK     TO WEB-AK-LINES                         
028900     MOVE IN-KVANTAL-LINES-BINNED TO WEB-BINNED-LINES                     
029000     MOVE IN-KVANTAL-LINES-PRIO   TO WEB-BINNED-PRIO                      
029100     MOVE IN-KVDAGDEC-DAYS-BINNED TO WEB-TIME-TOT                         
029200     MOVE IN-KVDAGDEC-DAYS-PRIO   TO WEB-DAYS-PRIO                        
029300     MOVE IN-TIAAVV               TO WEB-TIAAVV                           
029400     MOVE IN-IDLANDX2             TO WEB-IDLANDX2                         
029500     MOVE IN-ADCITY               TO WEB-ADCITY                           
029600     MOVE IN-SUARTNTO-BINNED      TO WEB-SUARTNTO-BINNED                  
029700     MOVE IN-KDREFTYP             TO WEB-KDREFTYP                         
029800                                                                          
029900     PERFORM S50-PUT-WEB-POST                                             
030000     .                                                                    
030100                                                                          
030200     EJECT                                                                
030300 Z-FINIT SECTION.                                                         
030400                                                                          
030500     CLOSE W6127F                                                         
030600                                                                          
030700     MOVE 'S' TO POSTSUM-OPKOD                                            
030800     CALL POSTSUM USING POSTSUM-PARM                                      
030900     .                                                                    
031000     EJECT                                                                
031100 S01-LAES-W6127F  SECTION.                                                
031200     READ W6127F INTO IN-AREA                                             
031300     AT END                                                               
031400        MOVE HIGH-VALUE TO IN-AREA                                        
031500        SET END-OF-W6127F TO TRUE                                         
031600                                                                          
031700     NOT AT END                                                           
031800        MOVE 'W6127F'   TO POSTSUM-FDNAMN                                 
031900        MOVE 'W6127GD1' TO POSTSUM-DDNAMN2                                
032000        MOVE IN-IDDC    TO POSTSUM-TRANSTYP                               
032100        CALL POSTSUM USING POSTSUM-PARM                                   
032200     END-READ                                                             
032300                                                                          
032400     .                                                                    
032500                                                                          
032600     EJECT                                                                
032700 S15-SKAPA-DAP-HEADER      SECTION.                                       
032800                                                                          
032900     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
033000     MOVE 1                          TO HDR-REQU-IDMSGVER                 
033100     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
033200     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
033300                                                                          
033400     MOVE 'MAN-REF-AK-WEEK'          TO HDR-IDOUTTYPE                     
033500                                                                          
033600     MOVE SPACE                      TO HDR-IDOUTREC                      
033700     MOVE SPAR-KDMFUP                TO HDR-IDOUTREC(1:2)                 
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

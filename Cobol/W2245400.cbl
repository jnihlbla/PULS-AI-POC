000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2245400.                                                
000400 AUTHOR.         G KJELLSON    (KOPIA W22154)                             
000500 DATE-WRITTEN.   2013 FEBRUARI                                            
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        LÄSER FIL FÖR GODKÄNNANDE AV LEVPLANFÖRSLAG                      
001000*        - SKAPAR TRANS W2T403X TILL DISPATCHEN                           
001100*                                                                         
001200*    INDATA .                                                             
001300*                                                                         
001400*        FIL FRÅN PGM W22412 (W224P012)                                   
001500*                                                                         
001600*    UTDATA .                                                             
001700*                                                                         
001800*        W2I40301 + WMSGKOM                                               
001810*                                                                         
001900                                                                          
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*                         ARTNR SOM HAR LEVERANSPLANFÖRSLAG               
002800*                               SOM SKALL GODKÄNNAS AUTOMATISKT           
002901     SELECT W22414                     ASSIGN TO W22454D1.                
003000                                                                          
003100 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W22414                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W22454        -L.                                              
004000                                                                          
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004601 77  IDPGM                       PIC X(8)    VALUE 'W2245400'.            
004700                                                                          
004800 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004900 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005000                                                                          
005100 77  W-CHKP-MAX                  PIC S9(3)   VALUE +100  COMP-3.          
005200 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
005300 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
005400 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
005500 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
005600 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
005700 77  JA                          PIC X(1)    VALUE 'J'.                   
005800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005900                                                                          
006000*01  -COPY WWDCKONS                                                       
006100                                                                          
006202 77  W22414-EOF-SW               PIC X       VALUE 'N'.                   
006302     88  END-OF-W22414                       VALUE 'J'.                   
006400                                                                          
006500 77  SW-FORSLAG                  PIC X       VALUE 'N'.                   
006600     88  FORSLAG-FINNS                       VALUE 'J'.                   
006700                                                                          
006800 77  WS-IDLEVNR                  PIC X(5)  VALUE SPACE.                   
006900 77  WS-IDARTNR                  PIC 9(9)  VALUE ZERO.                    
007000 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
007100 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
007200                                                                          
007300                                                                          
007400                                                                          
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800                                                                          
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
008500                                                                          
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005 -PRE  POSTSUM-                                           
008900                                                                          
009000                                                                          
009100 01  IN-AREA-START               PIC X(24)   VALUE                        
009200                                             'IN-AREA-START'.             
009402*01  AREA -COPY W22454       -PRE IN-.                                    
009500*                                                                         
009600                                                                          
009700                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900                                                                          
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-WDD901KY-X.                                                    
010200         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
010300         05  W-IDDC              PIC X(2)  VALUE SPACE.                   
010400     03  W-IDLEVNR-X.                                                     
010500         05  W-IDLEVNR           PIC X(5)  VALUE SPACE.                   
010600                                                                          
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011200     88  IMS-EJ-OK                           VALUE 'XD'.                  
011300                                                                          
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600                                                                          
011700                                                                          
011800 01  ALL-SSA.                                                             
011900     03 SSA1                     PIC X(64).                               
012000     03 SSA2                     PIC X(64).                               
012100     03 SSA3                     PIC X(64).                               
012200                                                                          
012300*    --- IMS FUNKTIONSKODER                                               
012400*01  -COPY W0003                                                          
012500                                                                          
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
012700 01  DLI-IO-WDD901.                                                       
012800*    03  -COPY WDD901  -PRE D901-                                         
013400                                                                          
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
013600 01  DLI-IO-WDD902.                                                       
013700*    03  -COPY WDD902  -PRE D902-                                         
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
014000 01  DLI-IO-WDD905.                                                       
014100*    03  -COPY WDD905  -PRE D905-                                         
014200                                                                          
014400                                                                          
014800*    ---  MSG INPUT-OUTPUT AREA                                           
014900 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
015000                                                                          
015100*01  -COPY WMSGAREA                                                       
015300     05  FILLER REDEFINES MSG-MID-OUT.                                    
015400        07  -COPY W2I40301  -PRE MID-.                                    
015500                                                                          
015600*    ---  AREA FÖR W006KOM SUBMODUL                                       
015700 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
015800                                                                          
015900 01  KOM-IO-AREA.                                                         
016000*    03  -COPY WMSGKOM                                                    
016100                                                                          
016200 LINKAGE SECTION.                                                         
016300                                                                          
016400*01  -COPY W0009 -PRE MSG-                                                
016500                                                                          
016600*01  -COPY W0009 -PRE ALT-                                                
016700                                                                          
016800*01  -COPY W0009 -PRE KOMA-                                               
016900                                                                          
017000*01  -COPY W0008 -PRE WDD9-                                               
017100     05  FILLER      PIC X.                                               
017200                                                                          
017300                                                                          
017400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB WDD9-PCB.             
017500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB WDD9-PCB.             
017600                                                                          
017700     PERFORM A-INIT                                                       
017800     PERFORM S01-LAES-W22414                                              
017900                                                                          
018001     PERFORM UNTIL END-OF-W22414                                          
018100        MOVE IN-IDARTNR TO W-IDARTNR                                      
018110        MOVE IN-IDDC    TO W-IDDC                                         
018200        MOVE IN-IDLEVNR TO W-IDLEVNR                                      
018300        PERFORM IMS-GU-WDD901                                             
018400        IF SEGMENT-FINNS                                                  
018500           MOVE NEJ TO SW-FORSLAG                                         
018600           PERFORM IMS-GNP-WDD905                                         
018700           PERFORM UNTIL SEGMENT-SAKNAS OR FORSLAG-FINNS                  
018800              IF D905-KDAVROP = 1                                         
018900                 MOVE JA TO SW-FORSLAG                                    
019000              ELSE                                                        
019100                 PERFORM IMS-GNP-WDD905                                   
019200              END-IF                                                      
019300           END-PERFORM                                                    
019310                                                                          
019330*** APRIL 2017: VI GODKÄNNER, ÄVEN OM FÖRSLAG/AVROP EJ FINNS,             
019340***             SAMMA SOM FÖR CDC, W2215400.                              
019400           IF SW-FORSLAG = NEJ                                            
019600              MOVE JA TO SW-FORSLAG                                       
019700              DISPLAY '* FÖRSLAG SAKNAS : ' IN-IDARTNR                    
019800                      ' * '                 IN-IDLEVNR                    
019900           END-IF                                                         
019910                                                                          
020000           IF FORSLAG-FINNS                                               
020100              PERFORM C-SKAPA-TRANS                                       
020200              PERFORM D-SKICKA-TRANS                                      
020300           ELSE                                                           
020400              DISPLAY '* FÖRSLAG SAKNAS : ' IN-IDARTNR                    
020500                      ' * '                 IN-IDLEVNR                    
020600           END-IF                                                         
020700        ELSE                                                              
020800           DISPLAY '* ARTNR SAKNAS : ' IN-IDARTNR                         
020900        END-IF                                                            
021000        PERFORM S01-LAES-W22414                                           
021100     END-PERFORM                                                          
021200                                                                          
021300     PERFORM Z-FINIT                                                      
021400                                                                          
021500     MOVE ZERO TO RETURN-CODE                                             
021600     GOBACK                                                               
021700     .                                                                    
021800                                                                          
021900                                                                          
022000 A-INIT SECTION.                                                          
022103     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
022200                                                                          
022301     OPEN INPUT W22414                                                    
022400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022500     MOVE ZERO  TO W-CHKP-RAEKNARE                                        
022600                                                                          
022700     PERFORM IMS-RESTART                                                  
022800     PERFORM AA-SKAPA-HEADER                                              
023100     .                                                                    
023200                                                                          
023300                                                                          
023400 AA-SKAPA-HEADER SECTION.                                                 
023503     MOVE 'AA-SKAPA-HEADER ' TO CURRENT-SECTION                           
023600                                                                          
023700     MOVE +54                   TO MSG-KOM-KVLL                           
023800     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
023900                                   MSG-KOM-KDZ2                           
024000     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
024100     MOVE 'W2I40301'            TO MSG-KOM-IDCPYTXT                       
024200     MOVE 'LEVPLAN'             TO MSG-KOM-IDSNDNOD                       
024300     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
024400                                                                          
024500     ACCEPT WS-TIUPPDAT FROM DATE                                         
024600     ACCEPT WS-TIUPPTID FROM TIME                                         
024700     MOVE WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                       
024800     MOVE WS-TIUPPTID           TO MSG-KOM-TIKLOCK                        
024900                                                                          
025000     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
025100                                   MSG-KOM-KDSVAR                         
025200     .                                                                    
025300                                                                          
025400                                                                          
025500 C-SKAPA-TRANS SECTION.                                                   
025603     MOVE 'C-SKAPA-TRANS   ' TO CURRENT-SECTION                           
025700                                                                          
025800                                                                          
025900     MOVE +135                  TO MSG-KVLL                               
026000     MOVE LOW-VALUE             TO MSG-KDZ1                               
026100                                   MSG-KDZ2                               
026200     MOVE 'W2T403X'             TO MSG-KDTRANS-1                          
026300     MOVE '2403'                TO MSG-IDTRANS-1                          
026400     MOVE '2'                   TO MSG-KDMFSFOR-1                         
026500                                                                          
026600     MOVE SPACE                 TO MID-W2I40301                           
026700     MOVE IN-IDARTNR            TO MID-IDARTNR-IN                         
026800     MOVE IN-IDDC               TO MID-IDDC-IN                            
026900     MOVE IN-IDLEVNR            TO MID-IDLEVNR-IN                         
027000     MOVE 'P'                   TO MID-KDAVROP-IN                         
027504     MOVE '2'                   TO MID-KDKOM                              
027505                                                                          
027800     .                                                                    
027900     EJECT                                                                
028000 D-SKICKA-TRANS SECTION.                                                  
028103     MOVE 'D-SKICKA-TRANS  ' TO CURRENT-SECTION                           
028200                                                                          
028300     CALL W006KOM USING MSG-PCB                                           
028400                        ALT-PCB                                           
028500                        KOMA-PCB                                          
028600                        MSG-KOM-WMSGKOM                                   
028700                        MSG-IO-AREA                                       
028800                                                                          
028900     ADD +1 TO W-CHKP-RAEKNARE                                            
029000                                                                          
029100     MOVE 'W22454'       TO POSTSUM-FDNAMN                                
029200     MOVE 'DISPATCH'     TO POSTSUM-DDNAMN2                               
029300     MOVE 'ANT'          TO POSTSUM-TRANSTYP                              
029400     CALL POSTSUM USING POSTSUM-PARM                                      
029500                                                                          
029600     IF W-CHKP-RAEKNARE > W-CHKP-MAX                                      
029700        PERFORM IMS-CHECKPOINT                                            
029800        MOVE +0 TO W-CHKP-RAEKNARE                                        
029900        ADD +1 TO MSG-KOM-TIKLOCK                                         
030000     END-IF                                                               
030100     .                                                                    
030200                                                                          
030300                                                                          
030400 Z-FINIT SECTION.                                                         
030503     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
030600                                                                          
030701     CLOSE W22414                                                         
030800                                                                          
030900     MOVE 'S' TO POSTSUM-OPKOD                                            
031000     CALL POSTSUM USING POSTSUM-PARM                                      
031100     .                                                                    
031200     EJECT                                                                
031300 S01-LAES-W22414  SECTION.                                                
031400                                                                          
031500     READ W22414 INTO IN-AREA                                             
031600     AT END                                                               
031700        SET END-OF-W22414 TO TRUE                                         
031800                                                                          
031900     NOT AT END                                                           
032000        MOVE 'W22414'       TO POSTSUM-FDNAMN                             
032100        MOVE 'W22454D1'     TO POSTSUM-DDNAMN2                            
032200        MOVE 'IN'           TO POSTSUM-TRANSTYP                           
032300        CALL POSTSUM USING POSTSUM-PARM                                   
032400     END-READ                                                             
032500     .                                                                    
032600     EJECT                                                                
032610*-----------------------------------------------------------------        
032700* IMS SECTIONER                                                           
032710*-----------------------------------------------------------------        
032800                                                                          
032900 IMS-GU-WDD901  SECTION.                                                  
033000     MOVE 'IMS-GU-WDD901    ' TO CURRENT-IMS-SECTION                      
033100                                                                          
033200     MOVE SPACE               TO ALL-SSA                                  
033300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
033400          DELIMITED BY SIZE INTO SSA1                                     
033500     MOVE '  GE'              TO GODK-STATUSKODER                         
033600     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
033700     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
033800     PERFORM IMS-STATUSKONTROLL                                           
033900     .                                                                    
034000                                                                          
034100                                                                          
034200 IMS-GNP-WDD905 SECTION.                                                  
034300     MOVE 'IMS-GNP-WDD905   ' TO CURRENT-IMS-SECTION                      
034400                                                                          
034500     MOVE SPACE               TO ALL-SSA                                  
034603     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
034700          DELIMITED BY SIZE INTO SSA1                                     
034803     STRING 'WDD905 '                                                     
034900          DELIMITED BY SIZE INTO SSA2                                     
035000     MOVE '  GE'              TO GODK-STATUSKODER                         
035103     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
035203     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500                                                                          
035600                                                                          
035700 IMS-RESTART SECTION.                                                     
035800     MOVE 'IMS-RESTART      ' TO CURRENT-IMS-SECTION                      
035900                                                                          
036000     MOVE SPACE TO W-MSG-IO-AREA                                          
036100     MOVE '  ' TO GODK-STATUSKODER                                        
036200     CALL CBLTDLI USING XRST MSG-PCB                                      
036300                             W-MSG-IO-AREA-LENGTH                         
036400                             W-MSG-IO-AREA                                
036500                             W-CHKP-AREA-1-LENGTH                         
036600                             W-CHKP-AREA-1                                
036700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000                                                                          
037100                                                                          
037200 IMS-CHECKPOINT SECTION.                                                  
037300     MOVE 'IMS-CHECKPOINT   ' TO CURRENT-IMS-SECTION                      
037400                                                                          
037500     MOVE IDPGM TO W-MSG-IO-AREA                                          
037600     MOVE '  XD' TO GODK-STATUSKODER                                      
037700     CALL CBLTDLI USING CHKP MSG-PCB                                      
037800                             W-MSG-IO-AREA-LENGTH                         
037900                             W-MSG-IO-AREA                                
038000                             W-CHKP-AREA-1-LENGTH                         
038100                             W-CHKP-AREA-1                                
038200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     IF IMS-EJ-OK                                                         
038500       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
038600       CALL FELLOG                                                        
038700     END-IF                                                               
038800     .                                                                    
038900                                                                          
039000                                                                          
039100 IMS-STATUSKONTROLL SECTION.                                              
039200                                                                          
039300     SET STATUS-IX TO 1                                                   
039400     SEARCH GODK-STATUS                                                   
039500       AT END                                                             
039600         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
039700         DISPLAY FELTEXT STATUS-WS                                        
039800         CALL FELLOG                                                      
039900       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
040000         CONTINUE                                                         
041000     END-SEARCH                                                           
050000     .                                                                    

000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0153100.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   90/11/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DISPATCH-SYSTEMET.                                               
000900*        RENSAR MEDDELANDE-KOMMUNIKATIONS-DATABASEN.                      
001000*        BORTTAG AV ROT SKER OM:                                          
001100*        - TRANSAKTIONEN KOMMER FRÅN ANNAT SYSTEM (EJ PULS)               
001200*          OCH ÄR EN VECKA GAMMAL SAMT ATT STATUS = A (AVSLUTAD)          
001300*          ELLER STATUS = F (FEL) OCH HANTERINGSKOD = R (RENSAS).         
001400*        - TRANSAKTIONEN KOMMER FRÅN PULS                                 
001500*          OCH ÄR 1 DAG GAMMAL SAMT ATT STATUS = A (AVSLUTAD)             
001600*          ELLER STATUS = F (FEL) OCH HANTERINGSKOD = R (RENSAS).         
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200                                                                          
002300     SELECT STYRIN      ASSIGN TO W01531D1.                               
002400                                                                          
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700 FD  STYRIN                                                               
002800     LABEL RECORD STANDARD                                                
002900     RECORDING F                                                          
003000     BLOCK CONTAINS 0.                                                    
003100                                                                          
003200 01  STYRPOST                    PIC X(80).                               
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(08)   VALUE 'W0154100'.            
003900 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                   PIC X(80) VALUE SPACE.                     
004300                                                                          
004400 77  JA                        PIC X       VALUE 'J'.                     
004500 77  NEJ                       PIC X       VALUE 'N'.                     
004600 77  STYR-EOF                  PIC X       VALUE 'N'.                     
004700 77  WS-DATE                   PIC 9(6)    VALUE ZERO.                    
004800 77  WS-TIME                   PIC 9(8)    VALUE ZERO.                    
004900 77  WS-OLD                    PIC S9(7)   VALUE +0    COMP-3.            
005000 77  WS-FOM                    PIC S9(7)   VALUE +0    COMP-3.            
005100 77  WS-TOM                    PIC S9(7)   VALUE +0    COMP-3.            
005200 77  WS-ANT-RENS-FOM           PIC S9(7)   VALUE +0    COMP-3.            
005300 77  WS-ANT-RENS-FOM-F         PIC S9(7)   VALUE +0    COMP-3.            
005400 77  WS-ANT-RENS-TOM           PIC S9(7)   VALUE +0    COMP-3.            
005500 77  WS-ANT-RENS-TOM-F         PIC S9(7)   VALUE +0    COMP-3.            
005510 77  WS-ANT-RENS-OLD           PIC S9(7)   VALUE +0    COMP-3.            
005600 77  WS-CHKP-RAEKNARE          PIC S9(3)   VALUE +0    COMP-3.            
005700 77  WS-CHKP-MAX               PIC S9(3)   VALUE +99   COMP-3.            
005800 77  CHKP-ID                   PIC X(8)    VALUE 'W0153100'.              
005900 77  CHKP-IO-AREA-LENGTH       PIC S9(9)   VALUE +32   COMP SYNC.         
006000 77  CHKP-IO-AREA              PIC X(32)   VALUE SPACE.                   
006100 77  CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32   COMP SYNC.         
006200 77  CHKP-AREA-1               PIC X(32)   VALUE SPACE.                   
006300*    --- PARAMETRAR TILL ABEND                                            
006400 77  RKOD-ABEND                PIC S9(4)   VALUE +33   COMP SYNC.         
006500     EJECT                                                                
006600*      --- VALID IDDC CODES                                               
006700*                                                                         
006900*01    -COPY WWDCKONS                                                     
007000       EJECT                                                              
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
007300     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
007400     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
007500     03  WORKDAY                 PIC X(8)   VALUE 'WORKDAY '.             
007700                                                                          
007800 01  IN-STYRAREA.                                                         
007900     03  IN-KVARBDAG             PIC 9(3)   VALUE 005.                    
008000     03  FILLER                  PIC X(77)  VALUE SPACE.                  
008100                                                                          
008200*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
008300*01  -COPY WORKAREA                                                       
008400                                                                          
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000                                                                          
009100 01  NYCKLAR-TILL-DLI.                                                    
009200                                                                          
009300                                                                          
009400     03  W-WDP8A1KY-X.                                                    
009500         05  W-TIREGDAT-SEQ       PIC S9(7)    VALUE +0 COMP-3.           
009600         05  W-TIKLOCK-SEQ        PIC S9(9)    VALUE +0 COMP-3.           
009700         05  FILLER               PIC X(16)    VALUE LOW-VALUE.           
009800                                                                          
009900                                                                          
010000     03  W-WDP801KY-X.                                                    
010100         05  W-IDSNDNOD           PIC X(8)     VALUE SPACE.               
010200         05  W-IDSNDJOB           PIC X(8)     VALUE SPACE.               
010300         05  W-TIREGDAT           PIC S9(7)    VALUE +0 COMP-3.           
010400         05  W-TIKLOCK            PIC S9(9)    VALUE +0 COMP-3.           
010500                                                                          
010600                                                                          
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011100     88  BASEN-SLUT                          VALUE 'GB'.                  
011200     88  IMS-EJ-OK                           VALUE 'XD'.                  
011300                                                                          
011400                                                                          
011500 01  GODK-STATUSKODER.                                                    
011600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  SSA1                        PIC X(64).                               
011900                                                                          
012000     EJECT                                                                
012100*    --- IMS FUNKTIONSKODER                                               
012200*01  -COPY W0003                                                          
012300                                                                          
012400     EJECT                                                                
012500*    ---  DLI INPUT-OUTPUT AREA                                           
012600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-KOMA'.         
012700                                                                          
012800 01  DLI-IO-KOMA.                                                         
012900*  05  -COPY WDP801 -PRE KOMA-                                            
013000                                                                          
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-KOMB'.         
013300                                                                          
013400 01  DLI-IO-KOMB.                                                         
013500*  05  -COPY WDP8A1 -PRE KOMB-                                            
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800                                                                          
013900*01  -COPY W0009 -PRE MSG-                                                
014000     EJECT                                                                
014100*01  -COPY W0008 -PRE KOMA-                                               
014200     05  FILLER                  PIC X.                                   
014300                                                                          
014400*01  -COPY W0008 -PRE KOMB-                                               
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700 PROCEDURE DIVISION  USING MSG-PCB KOMA-PCB KOMB-PCB.                     
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB KOMA-PCB KOMB-PCB.                     
015000                                                                          
015100     PERFORM A-INIT                                                       
015200                                                                          
015300     PERFORM IMS-GET-KOMB                                                 
015400     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
015500       IF KOMB-SEQA-TIREGDAT < WS-OLD                                     
015501          ADD +1 TO WS-ANT-RENS-OLD                                       
015502          PERFORM B-DELETE                                                
015503       ELSE                                                               
015510          IF KOMB-SEQA-TIREGDAT < WS-FOM                                  
015600             IF KOMB-SEQA-KDKOMSTA = 'A'                                  
015700                ADD +1 TO WS-ANT-RENS-FOM                                 
015800                PERFORM B-DELETE                                          
015900             ELSE                                                         
016000                IF  KOMB-SEQA-KDKOMSTA = 'F'                              
016100                AND KOMB-SEQA-KDKOMBEH = 'R'                              
016200                   ADD +1 TO WS-ANT-RENS-FOM-F                            
016300                   PERFORM B-DELETE                                       
016400                END-IF                                                    
016500             END-IF                                                       
016600          ELSE                                                            
016700             IF KOMB-SEQA-IDSNDNOD = 'INLEV   ' OR 'KIP     '             
016800                                  OR 'STATNR  ' OR 'STATNR2 '             
016900                                  OR 'INLEVRET' OR 'RET-RET '             
017000                                  OR 'LEVPLAN ' OR 'KREDIT  '             
017100                                  OR 'NYART   ' OR 'NYANSK  '             
017200                                  OR 'R25     ' OR 'ORDERING'             
017300                                  OR 'W460RHN ' OR 'PIE P   '             
017400                                  OR 'W46385  ' OR 'TRANSFER'             
017500                                  OR 'DDGS P  ' OR 'W463VG3 '             
017600                                  OR 'SKROT   ' OR 'BYPASS  '             
017700                                  OR 'D-LEV P ' OR 'W41240  '             
017800                IF KOMB-SEQA-TIREGDAT < WS-TOM                            
017900                   IF KOMB-SEQA-KDKOMSTA = 'A'                            
018000                      ADD +1 TO WS-ANT-RENS-TOM                           
018100                      PERFORM B-DELETE                                    
018200                   ELSE                                                   
018300                      IF KOMB-SEQA-KDKOMSTA = 'F'                         
018400                      AND KOMB-SEQA-KDKOMBEH = 'R'                        
018500                         ADD +1 TO WS-ANT-RENS-TOM-F                      
018600                         PERFORM B-DELETE                                 
018700                      END-IF                                              
018800                   END-IF                                                 
018900                END-IF                                                    
019000             ELSE                                                         
019100                IF KOMB-SEQA-IDSNDJOB = 'W4120200' OR 'W4607500'          
019200                   IF KOMB-SEQA-TIREGDAT < WS-TOM                         
019300                      IF KOMB-SEQA-KDKOMSTA = 'A'                         
019400                         ADD +1 TO WS-ANT-RENS-TOM                        
019500                         PERFORM B-DELETE                                 
019600                      END-IF                                              
019700                   END-IF                                                 
019800                END-IF                                                    
019900             END-IF                                                       
020000          END-IF                                                          
020100                                                                          
020200*      FIX FÖR ATT TA BORT FELKÖRNINGAR (ÄNDRA VID FIXNING)               
020300          IF  KOMB-SEQA-TIREGDAT < 070101                                 
020400              ADD +1 TO WS-ANT-RENS-TOM-F                                 
020500              PERFORM B-DELETE                                            
020600          END-IF                                                          
020610       END-IF                                                             
020700                                                                          
020800       PERFORM IMS-GET-KOMB                                               
020900     END-PERFORM                                                          
021000                                                                          
021100     PERFORM Z-FINIT                                                      
021200                                                                          
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800     MOVE WHEN-COMPILED TO W-COMPILED                                     
021900     OPEN INPUT STYRIN                                                    
022000                                                                          
022100     READ STYRIN INTO IN-STYRAREA                                         
022200       AT END                                                             
022300         MOVE JA TO STYR-EOF                                              
022400         MOVE 005 TO IN-KVARBDAG                                          
022500         DISPLAY 'STYR INFO SAKNAS, 5 DAGAR ANTAGITS'                     
022600       NOT AT END                                                         
022700         DISPLAY ' INKORT = ' IN-STYRAREA                                 
022800     END-READ                                                             
022900                                                                          
023000     IF IN-KVARBDAG NOT NUMERIC                                           
023100       MOVE 005         TO IN-KVARBDAG                                    
023200       DISPLAY 'STYR EJ NUMERISK, 5 DAGAR ANTAGITS'                       
023300     END-IF                                                               
023400                                                                          
023500     IF IN-KVARBDAG = ZERO                                                
023600       MOVE +999999999 TO W-TIKLOCK-SEQ                                   
023700     END-IF                                                               
023800                                                                          
023900     ACCEPT WS-TIME FROM TIME                                             
024000     IF WS-TIME < 20000000                                                
024100*         KLOCKAN 20.00                                                   
024200       ADD +1 TO IN-KVARBDAG                                              
024300     END-IF                                                               
024400                                                                          
024500     ACCEPT WS-DATE FROM DATE                                             
024600     MOVE WC-CDC-SE   TO WORK-IDDC                                        
024700     MOVE WS-DATE     TO WORK-TIAAMMDD-TOM                                
024800     MOVE IN-KVARBDAG TO WORK-KVWORKD                                     
024900     MOVE 003         TO WORK-KDCALL                                      
025000     CALL WORKDAY USING  WORK-KDCALL                                      
025100                         WORK-DATE-AREA                                   
025200                         WORK-KDSVAR                                      
025300     IF WORK-KDSVAR-FEL                                                   
025400       MOVE 'FEL FRÅN WORKDAY I SECTION INIT:1' TO FELTEXT                
025500       CALL ABEND USING RKOD-ABEND                                        
025600     ELSE                                                                 
025700       MOVE WORK-TIAAMMDD-FOM TO WS-FOM                                   
025800       MOVE WORK-TIAAMMDD-TOM TO WS-TOM                                   
025900     END-IF                                                               
026100                                                                          
026101*    BERÄKNA DATUM FRÅN VILKET ALLA RADER SKALL RENSAS                    
026102*    VI RÄKNAR 125 ARBETSDAGAR BAKÅT VILKET GER UNGEFÄR ½ ÅR              
026103                                                                          
026104     MOVE WS-DATE     TO WORK-TIAAMMDD-TOM                                
026105     MOVE 125         TO WORK-KVWORKD                                     
026106     MOVE 003         TO WORK-KDCALL                                      
026107     CALL WORKDAY USING  WORK-KDCALL                                      
026108                         WORK-DATE-AREA                                   
026109                         WORK-KDSVAR                                      
026110     IF WORK-KDSVAR-FEL                                                   
026111       MOVE 'FEL FRÅN WORKDAY I SECTION INIT:2' TO FELTEXT                
026112       CALL ABEND USING RKOD-ABEND                                        
026120     ELSE                                                                 
026130       MOVE WORK-TIAAMMDD-FOM TO WS-OLD                                   
026150     END-IF                                                               
026160                                                                          
026200     PERFORM IMS-RESTART                                                  
026300     .                                                                    
026400     EJECT                                                                
026500 B-DELETE SECTION.                                                        
026600                                                                          
026700     MOVE KOMB-SEQA-IDSNDNOD TO W-IDSNDNOD                                
026800     MOVE KOMB-SEQA-IDSNDJOB TO W-IDSNDJOB                                
026900     MOVE KOMB-SEQA-TIREGDAT TO W-TIREGDAT                                
027000     MOVE KOMB-SEQA-TIKLOCK  TO W-TIKLOCK                                 
027100     PERFORM IMS-GET-KOMA                                                 
027200     IF SEGMENT-FINNS                                                     
027300       PERFORM IMS-DELETE                                                 
027400       ADD +1 TO WS-CHKP-RAEKNARE                                         
027500     END-IF                                                               
027600     IF WS-CHKP-RAEKNARE > WS-CHKP-MAX                                    
027700       PERFORM IMS-CHECKPOINT                                             
027800       MOVE +0 TO WS-CHKP-RAEKNARE                                        
027900       MOVE W-TIREGDAT TO W-TIREGDAT-SEQ                                  
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 Z-FINIT SECTION.                                                         
028400                                                                          
028500     CLOSE STYRIN                                                         
028600                                                                          
028700     DISPLAY ' ANTAL RÖTTER ÄLDRE ÄN: DAG = '                             
028800             WS-FOM ' STATUS = A RENSADE : ' WS-ANT-RENS-FOM              
028900     DISPLAY ' ANTAL RÖTTER ÄLDRE ÄN: DAG = '                             
029000             WS-FOM ' STATUS = F RENSADE : ' WS-ANT-RENS-FOM-F            
029100     DISPLAY ' ANTAL RÖTTER ÄLDRE ÄN: DAG = '                             
029200             WS-TOM ' STATUS = A RENSADE : ' WS-ANT-RENS-TOM              
029300     DISPLAY ' ANTAL RÖTTER ÄLDRE ÄN: DAG = '                             
029400             WS-TOM ' STATUS = F RENSADE : ' WS-ANT-RENS-TOM-F            
029410     DISPLAY ' ANTAL RÖTTER ÄLDRE ÄN: DAG = '                             
029420             WS-OLD ' GAMLA RENSADE      : ' WS-ANT-RENS-OLD              
029500     DISPLAY '                              '                             
029600     .                                                                    
029700     EJECT                                                                
029800* --- IMS SEKTIONER ---                                                   
029900                                                                          
030000 IMS-RESTART SECTION.                                                     
030100                                                                          
030200     MOVE SPACE TO CHKP-IO-AREA                                           
030300     MOVE '  ' TO GODK-STATUSKODER                                        
030400     CALL CBLTDLI USING XRST MSG-PCB                                      
030500                             CHKP-IO-AREA-LENGTH CHKP-IO-AREA             
030600                             CHKP-AREA-1-LENGTH CHKP-AREA-1               
030700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031000     SKIP3                                                                
031100 IMS-CHECKPOINT SECTION.                                                  
031200                                                                          
031300     MOVE CHKP-ID TO CHKP-IO-AREA                                         
031400     MOVE '  XD' TO GODK-STATUSKODER                                      
031500     CALL CBLTDLI USING CHKP MSG-PCB                                      
031600                             CHKP-IO-AREA-LENGTH CHKP-IO-AREA             
031700                             CHKP-AREA-1-LENGTH CHKP-AREA-1               
031800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     IF IMS-EJ-OK                                                         
032100       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
032200       CALL FELLOG                                                        
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 IMS-GET-KOMB SECTION.                                                    
032700                                                                          
032800     STRING 'WLKOMB01(WDP8A1KY=>' W-WDP8A1KY-X ')'                        
032900             DELIMITED BY SIZE INTO SSA1                                  
033000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
033100     CALL CBLTDLI USING GN KOMB-PCB DLI-IO-KOMB SSA1                      
033200     MOVE KOMB-STATUS-CODE TO STATUS-WS                                   
033300     PERFORM IMS-STATUSKONTROLL                                           
033400     .                                                                    
033500     SKIP3                                                                
033600 IMS-GET-KOMA SECTION.                                                    
033700                                                                          
033800     STRING 'WLKOMA01(WDP801KY =' W-WDP801KY-X ')'                        
033900             DELIMITED BY SIZE INTO SSA1                                  
034000     MOVE '  GE' TO GODK-STATUSKODER                                      
034100     CALL CBLTDLI USING GHU KOMA-PCB DLI-IO-KOMA SSA1                     
034200     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
034300     PERFORM IMS-STATUSKONTROLL                                           
034400     .                                                                    
034500     SKIP3                                                                
034600 IMS-DELETE       SECTION.                                                
034700                                                                          
034800     MOVE '    ' TO GODK-STATUSKODER                                      
034900     CALL CBLTDLI USING DLET KOMA-PCB DLI-IO-KOMA                         
035000     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSKONTROLL                                           
035200     .                                                                    
035300     EJECT                                                                
035400 IMS-STATUSKONTROLL SECTION.                                              
035500                                                                          
035600     SET STATUS-IX TO 1                                                   
035700     SEARCH GODK-STATUS                                                   
035800       AT END                                                             
035900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036000           DELIMITED BY SIZE INTO FELTEXT                                 
036100         CALL FELLOG                                                      
036200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036300         CONTINUE                                                         
036400     END-SEARCH                                                           
036500     .                                                                    

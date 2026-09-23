000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W612LABL.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   12/01/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SUPROGRAM TO GET DATA FROM MAIN PROGRAM AND CREATE               
000900*        CHINESE LABLES ON WEB THRU D&P                                   
001000*                                                                         
001100*        PROGRAMMET          READS      WDD3                              
001110*                            READS      WDT4                              
001200                                                                          
001300     SKIP3                                                                
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 DATA DIVISION.                                                           
001800     EJECT                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000                                                                          
002100*    -- CHECKED BY WY2000                                                 
002200 77  IDPGM                       PIC X(08)   VALUE 'W612LABL'.            
002300 77  YES                         PIC X       VALUE 'Y'.                   
002400 77  NOO                         PIC X       VALUE 'N'.                   
002500                                                                          
002600 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
002700 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
002810 01  UNICODE-SPACE.                                                       
002820     03 FILLER                   PIC X(180) VALUE ALL X'20'.              
002900                                                                          
003000*      --- VALID IDDC CODES                                               
003100*                                                                         
003200*01    -COPY WWDC99                                                       
003300       EJECT                                                              
003400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
003500 01  GENERELLA-SUBPROGRAM.                                                
003600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
003800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
004100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
004200     EJECT                                                                
004300 01  KDRC-DISPLAY                PIC Z(5).                                
004400 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700*                                                                         
004800*    --- PARAMETERS TO ABEND                                              
004900                                                                          
005000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005300*                                                                         
005400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005500     SKIP3                                                                
005600*01  -COPY WZ01SUB                                                        
005700     SKIP3                                                                
005800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
005900*01  -COPY WZ01SEND                                                       
006000*                                                                         
006100 01  HDR-AREA.                                                            
006200*    03  -COPY WZ01REQU  -PRE HDR-                                        
006300*    03  -COPY WZ04HDR                                                    
006400     SKIP3                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006600     SKIP3                                                                
006700 01  RESP-AREA.                                                           
006800*    03  -COPY WZ01RESP                                                   
006900*    03  -COPY W612LB                                                     
007000     EJECT                                                                
007100*01  -COPY WMSGAREA                                                       
007200     EJECT                                                                
007300 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
007400*01  -COPY WTRAUTF8                                                       
007500                                                                          
007600 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
007700 01  WS-IDSKYLT-CN           PIC X(3) VALUE 'RCN'.                        
007800                                                                          
007900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008400                                                                          
008500     03  W-IDARTNR-X.                                                     
008600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008700                                                                          
008800     03  W-IDSKYLT-X.                                                     
008900         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
009000     SKIP2                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FOUND                       VALUE '  '.                  
009400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(128).                              
010100 01  SSA2                        PIC X(64).                               
010200 01  SSA3                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
010900     SKIP3                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDD311'.        
011100 01  DLI-IO-WDD311.                                                       
011200*    03  -COPY WDD311                                                     
011210 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDT401'.        
011220 01  DLI-IO-WDT401.                                                       
011230*    03  -COPY WDT401                                                     
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
011300                                                                          
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W612LABL PRE LABL-                                             
011800     EJECT                                                                
011900*01  -COPY WZ01REQU                                                       
012000     EJECT                                                                
012100 01  DISTRWEB-PCB                PIC X.                                   
012200     EJECT                                                                
012300*01  -COPY W0008  -PRE WDD3-                                              
012400     05  FILLER                  PIC X.                                   
012410*01  -COPY W0008  -PRE WDT4-                                              
012420     05  FILLER                  PIC X.                                   
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
012500                                                                          
012600     EJECT                                                                
012700 PROCEDURE DIVISION  USING LABL-W612LABL REQU-WZ01REQU                    
012800                           DISTRWEB-PCB WDD3-PCB WDT4-PCB                 
012900                           WDB6-PCB.                                      
013000 MAIN SECTION.                                                            
013100     ENTRY 'DLITCBL' USING LABL-W612LABL REQU-WZ01REQU                    
013200                           DISTRWEB-PCB WDD3-PCB WDT4-PCB                 
                                 WDB6-PCB.                                      
013300                                                                          
013400     PERFORM A-INIT                                                       
013500     PERFORM S90-OPEN-DAP-SEND-WEB                                        
013600     PERFORM S90-PUT-DAP-HEADER                                           
013700     PERFORM B-PROCESS-DATA                                               
013800     VARYING INDX FROM +1 BY +1                                           
013900       UNTIL INDX > MAX-INDX                                              
014000          OR LABL-IDARTNR (INDX) = 0                                      
014100     PERFORM S90-CLOSE-DAP-SEND                                           
014200                                                                          
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700                                                                          
014800     INITIALIZE LB-W612LB                                                 
014900     MOVE LABL-IDDC            TO WS-IDDC                                 
                                        W-IDDC-B6                               
015000     .                                                                    
015100     EJECT                                                                
015200 B-PROCESS-DATA SECTION.                                                  
015300                                                                          
015400     MOVE LABL-IDARTNR (INDX)  TO LB-IDARTNR                              
015500                                  W-IDARTNR                               
015600     MOVE LABL-KVANTAL (INDX)  TO LB-KVANTAL                              
015700     MOVE LABL-KDARTURS (INDX) TO LB-KDARTURS                             
015800                                                                          
015900     PERFORM BA-GET-BEART-CHINESE                                         
015910     PERFORM BB-GET-TECH-DESCR                                            
016000     PERFORM S90-PUT-LINE                                                 
016300     .                                                                    
016400     EJECT                                                                
016600 BA-GET-BEART-CHINESE.                                                    
016700*    -- READ CHINESE OR ENGLISH BEART                                     
016800*    -- ENGLISH WILL BE TRANSLATED TO UNICODE                             
           PERFORM IMS-GU-WDB601                                                
049301     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
049302     IF DCS-UNICODE-IDSKYLT                                               
049303       MOVE 'UTF8'             TO TRAUTF8-KDCP                            
049304     ELSE                                                                 
049305       MOVE '278 '             TO TRAUTF8-KDCP                            
049306     END-IF                                                               
017600                                                                          
017700     PERFORM IMS-GU-WDD311                                                
017800     IF SEGMENT-FOUND                                                     
017900       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
018000     ELSE                                                                 
018100       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
018200     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
018300                                                                          
018400*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
018500     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
018600                                                                          
018700     MOVE TRAUTF8-TECONV-TO   TO LB-BEART                                 
018800     .                                                                    
018900     EJECT                                                                
018910 BB-GET-TECH-DESCR.                                                       
018920*                                                                         
018921     PERFORM IMS-GU-WDT401                                                
018922     IF SEGMENT-FOUND                                                     
018923       MOVE TEKI-IDTEKINF TO LB-IDTEKINF                                  
018924     ELSE                                                                 
018925       MOVE UNICODE-SPACE TO LB-IDTEKINF                                  
018927     END-IF                                                               
018930     .                                                                    
018940     EJECT                                                                
019000 S90-OPEN-DAP-SEND-WEB SECTION.                                           
019100     MOVE 'OPEN'                  TO SEND-KDFUNC                          
019200*    -- WEB RESPONSE SHOULD HAVE LOWER PRIO TO FINISH LAST                
019300*    -- DISTRDOC = WZ0420X HAS LOWER PRIO THAN WZ0420U                    
019400     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
019500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019600                         SEND-OPEN-AREA                                   
019700     IF SEND-KDRC > ZERO                                                  
019800       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
019900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
020000       DELIMITED BY SIZE INTO FELTEXT                                     
020100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020200     END-IF                                                               
020300     .                                                                    
020400     SKIP3                                                                
020500 S90-PUT-DAP-HEADER SECTION.                                              
020600     MOVE 1                       TO HDR-REQU-IDMSGVER                    
020700     MOVE REQU-KDPGMACT           TO HDR-REQU-KDPGMACT                    
020800     MOVE REQU-IDUSER             TO HDR-REQU-IDUSER                      
020900     MOVE 'CN-LABEL'              TO HDR-IDOUTTYPE                        
021000     MOVE LABL-IDDC               TO HDR-IDOUTREC (1:2)                   
021100     MOVE REQU-IDUSER             TO HDR-IDOUTREC(3:)                     
021200     MOVE SPACE                   TO HDR-IDLIST                           
021300     MOVE 'PUT'                   TO SEND-KDFUNC                          
021400     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
021500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
021600                                     SEND-KVDLEN                          
021700                                     HDR-AREA                             
021800     IF SEND-KDRC > ZERO                                                  
021900       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
022000       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
022100       DELIMITED BY SIZE       INTO FELTEXT-STR                           
022200       DISPLAY FELTEXT                                                    
022300       CALL FELLOG                                                        
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 S90-PUT-LINE SECTION.                                                    
022800*    DISPLAY 'RRR LB-W612LB : ' LB-W612LB                                 
022900     MOVE 'PUT'                   TO SEND-KDFUNC                          
023000     MOVE LENGTH OF LB-W612LB     TO SEND-KVDLEN                          
023100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
023200                                     SEND-KVDLEN                          
023300                                     LB-W612LB                            
023400     IF SEND-KDRC > ZERO                                                  
023500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
023600       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
023700       DELIMITED BY SIZE       INTO FELTEXT-STR                           
023800       DISPLAY FELTEXT                                                    
023900       CALL FELLOG                                                        
024000     END-IF                                                               
024100     .                                                                    
024200     SKIP2                                                                
024300 S90-CLOSE-DAP-SEND SECTION.                                              
024400     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
024500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
024600                                                                          
024700     IF SEND-KDRC > 0                                                     
024800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
024900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
025000       DELIMITED BY SIZE INTO FELTEXT                                     
025100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025200     END-IF                                                               
025300     .                                                                    
025400     SKIP3                                                                
025500* --- IMS SEKTIONER ---                                                   
025600 IMS-GU-WDD311 SECTION.                                                   
025700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
025800             DELIMITED BY SIZE INTO SSA1                                  
025900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
026000              DELIMITED BY SIZE INTO SSA2                                 
026100     MOVE '  GE' TO GODK-STATUSKODER                                      
026200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
026300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
026400     PERFORM IMS-STATUSKONTROLL                                           
026500     .                                                                    
026600     EJECT                                                                
026610 IMS-GU-WDT401 SECTION.                                                   
026620     STRING 'WDT401  (IDARTNR  =' W-IDARTNR-X ')'                         
026630             DELIMITED BY SIZE INTO SSA1                                  
026660     MOVE '  GE' TO GODK-STATUSKODER                                      
026670     CALL CBLTDLI USING GU WDT4-PCB DLI-IO-WDT401 SSA1                    
026680     MOVE WDT4-STATUS-CODE TO STATUS-WS                                   
026690     PERFORM IMS-STATUSKONTROLL                                           
026691     .                                                                    
026692     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
026700 IMS-STATUSKONTROLL SECTION.                                              
026800                                                                          
026900     SET STATUS-IX TO 1                                                   
027000     SEARCH GODK-STATUS                                                   
027100       AT END                                                             
027200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027300         DELIMITED BY SIZE INTO FELTEXT                                   
027400         CALL FELLOG                                                      
027500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027600         CONTINUE                                                         
027700     END-SEARCH                                                           
027800     .                                                                    

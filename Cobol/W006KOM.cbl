000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W006KOM.                                                 
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   FEB   91.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        DISPATCH-SYSTEMET.                                               
000900*        GENERELLT SUBPROGRAM FÖR LÄSNING OCH SKRIVNING                   
001000*        MOT MEDDELANDE-KOMMUNIKATIONS-DATABASEN.                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001400*          MSG-PCB                                                        
001500*          ALT-PCB                                                        
001600*          WDP8-PCB                                                       
001700*          MSG-KOM-WMSGKOM                                                
001800*          MSG-IO-AREA                                                    
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MSG-KOM-WMSGKOM                                                  
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(8)    VALUE 'W006KOM '.            
003200 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  W-IDRADNR                   PIC S9(5)   VALUE ZERO  COMP-3.          
003700 77  W-KDKOMSTA                  PIC X       VALUE SPACE.                 
003800 77  W-KDTRANS                   PIC X(8)    VALUE SPACE.                 
003900 77  W-KVLL                      PIC S9(4)   VALUE ZERO  COMP.            
004000                                                                          
004100                                                                          
004200                                                                          
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004600                                                                          
004700     EJECT                                                                
004800*01  -COPY WMSGKOM                                                        
004900                                                                          
005000     EJECT                                                                
005100*                                                                         
005200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
005300*                                                                         
005400 01  IMS-WS.                                                              
005500   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
005600                                                                          
005700*                        **** STATUS-KOD FRÅN IMS                         
005800   03  STATUS-WS                 PIC XX.                                  
005900     88  SEGMENT-FINNS                       VALUE '  '.                  
006000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
006100     88  INSERTEN-OK                         VALUE '  '.                  
006200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
006300                                                                          
006400   03  GODK-STATUSKODER.                                                  
006500     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
006600                                                                          
006700                                                                          
006800 01  NYCKLAR-TILL-DLI.                                                    
006900   03  W-WDP801KY-X.                                                      
007000     05  W-IDSNDNOD              PIC X(8).                                
007100     05  W-IDSNDJOB              PIC X(8).                                
007200     05  W-TIREGDAT              PIC S9(7)   VALUE ZERO  COMP-3.          
007300     05  W-TIKLOCK               PIC S9(9)   VALUE ZERO  COMP-3.          
007400                                                                          
007500 01  SSA1                        PIC X(64).                               
007600 01  SSA2                        PIC X(64).                               
007700                                                                          
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP801'.        
008000                                                                          
008100 01  DLI-IO-WDP801.                                                       
008200*  03  -COPY WDP801  -PRE WDP8-                                           
008300                                                                          
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDP811'.        
008600                                                                          
008700 01  DLI-IO-WDP811.                                                       
008800*  03  -COPY WDP811  -PRE WDP8-                                           
008900                                                                          
009000     EJECT                                                                
009100*                            IMS FUNKTIONSKODER                           
009200*01    -COPY W0003                                                        
009300                                                                          
009400     EJECT                                                                
009500 LINKAGE SECTION.                                                         
009600*01  -COPY W0009 -PRE MSG-                                                
009700                                                                          
009800*01  -COPY W0009 -PRE ALT-                                                
009900     EJECT                                                                
010000*01  -COPY W0008 -PRE WDP8-                                               
010100     05  FILLER                  PIC X.                                   
010200                                                                          
010300 01  LINK-WMSGKOM                PIC X(54).                               
010400                                                                          
010500 01  LINK-WMSGAREA               PIC X(1000).                             
010600                                                                          
010700     EJECT                                                                
010800 PROCEDURE DIVISION USING  MSG-PCB                                        
010900                           ALT-PCB                                        
011000                           WDP8-PCB                                       
011100                           LINK-WMSGKOM                                   
011200                           LINK-WMSGAREA.                                 
011300 STYR SECTION.                                                            
011400                                                                          
011500     MOVE WHEN-COMPILED TO W-COMPILED                                     
011600     MOVE LINK-WMSGKOM TO MSG-KOM-WMSGKOM                                 
011700     IF MSG-KOM-IDMFSMED = '000'                                          
011800       MOVE MSG-KOM-IDSNDNOD TO W-IDSNDNOD                                
011900       MOVE MSG-KOM-IDSNDJOB TO W-IDSNDJOB                                
012000       MOVE MSG-KOM-TIREGDAT TO W-TIREGDAT                                
012100       MOVE MSG-KOM-TIKLOCK  TO W-TIKLOCK                                 
012200       PERFORM IMS-GET-WDP8-ROT                                           
012300       IF SEGMENT-FINNS                                                   
012400         IF WDP8-KOM-IDMFSMED = SPACE                                     
012500           IF WDP8-KOM-KDKOMSTA = 'S'                                     
012600             MOVE '130' TO MSG-KOM-IDMFSMED                               
012700           ELSE                                                           
012800             MOVE '034' TO MSG-KOM-IDMFSMED                               
012900           END-IF                                                         
013000         ELSE                                                             
013100           MOVE WDP8-KOM-IDMFSMED TO MSG-KOM-IDMFSMED                     
013200         END-IF                                                           
013300       ELSE                                                               
013400         MOVE '078' TO MSG-KOM-IDMFSMED                                   
013500       END-IF                                                             
013600     ELSE                                                                 
013700       IF MSG-KOM-IDSNDNOD = W-IDSNDNOD                                   
013800         AND MSG-KOM-IDSNDJOB = W-IDSNDJOB                                
013900         AND MSG-KOM-TIREGDAT = W-TIREGDAT                                
014000         AND MSG-KOM-TIKLOCK = W-TIKLOCK                                  
014100           CONTINUE                                                       
014200       ELSE                                                               
014300         MOVE MSG-KOM-IDSNDNOD TO WDP8-KOM-IDSNDNOD                       
014400                                  W-IDSNDNOD                              
014500         MOVE MSG-KOM-IDSNDJOB TO WDP8-KOM-IDSNDJOB                       
014600                                  W-IDSNDJOB                              
014700         MOVE MSG-KOM-TIREGDAT TO WDP8-KOM-TIREGDAT                       
014800                                  W-TIREGDAT                              
014900         MOVE MSG-KOM-TIKLOCK  TO WDP8-KOM-TIKLOCK                        
015000                                  W-TIKLOCK                               
015100         MOVE MSG-KOM-IDCPYTXT TO WDP8-KOM-IDCPYTXT                       
015200         MOVE MSG-KOM-IDMFSMED TO WDP8-KOM-IDMFSMED                       
015300         MOVE MSG-LTERM-NAME   TO WDP8-KOM-IDLTERM                        
015400         MOVE MSG-SIGNON-USERID TO WDP8-KOM-IDUSER                        
015500         MOVE SPACE            TO WDP8-KOM-KDKOMBEH                       
015600         MOVE 'K'              TO WDP8-KOM-KDKOMSTA                       
015700                                  W-KDKOMSTA                              
015800         PERFORM IMS-ISRT-WDP8-ROT                                        
015900         IF SEGMENT-FINNS-REDAN                                           
016000           MOVE '120' TO MSG-KOM-IDMFSMED                                 
016100           PERFORM IMS-GET-WDP8-ROT                                       
016200           MOVE WDP8-KOM-KDKOMSTA TO W-KDKOMSTA                           
016300         ELSE                                                             
016400           MOVE MSG-KOM-KVLL TO W-KVLL                                    
016500           MOVE MSG-KOM-KDTRANS TO W-KDTRANS                              
016600           MOVE 'W0T693X ' TO MSG-KOM-KDTRANS                             
016700           MOVE +54 TO MSG-KOM-KVLL                                       
016800           PERFORM IMS-INSERT-ALTMSG                                      
016900           MOVE +0 TO W-IDRADNR                                           
017000           MOVE W-KDTRANS TO MSG-KOM-KDTRANS                              
017100           MOVE W-KVLL TO MSG-KOM-KVLL                                    
017200         END-IF                                                           
017300       END-IF                                                             
017400       IF MSG-KOM-IDMFSMED = '   '                                        
017500         IF W-KDKOMSTA = 'A'                                              
017600           MOVE '120' TO MSG-KOM-IDMFSMED                                 
017700         ELSE                                                             
017800           ADD +1 TO W-IDRADNR                                            
017900           MOVE W-IDRADNR TO WDP8-TRAN-IDRADNR                            
018000           MOVE SPACE TO WDP8-TRAN-IDUSER                                 
018100           MOVE LINK-WMSGAREA TO WDP8-TRAN-WMSGAREA                       
018200           PERFORM IMS-ISRT-WDP8-TRANS                                    
018300         END-IF                                                           
018400       END-IF                                                             
018500     END-IF                                                               
018600     MOVE MSG-KOM-WMSGKOM TO LINK-WMSGKOM                                 
018700                                                                          
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100                                                                          
019200     EJECT                                                                
019300* IMS SEKTIONER                                                           
019400                                                                          
019500 IMS-INSERT-ALTMSG SECTION.                                               
019600     MOVE SPACE TO GODK-STATUSKODER                                       
019700     CALL CBLTDLI USING ISRT ALT-PCB MSG-KOM-WMSGKOM                      
019800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
019900     PERFORM IMS-STATUSKONTROLL                                           
020000     CALL CBLTDLI USING PURG ALT-PCB                                      
020100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
020200     PERFORM IMS-STATUSKONTROLL                                           
020300     .                                                                    
020400                                                                          
020500     EJECT                                                                
020600 IMS-GET-WDP8-ROT SECTION.                                                
020700                                                                          
020800     IF WDP8-DBD-NAME = 'WDP8'                                            
020900       STRING 'WDP801  (WDP801KY =' W-WDP801KY-X ')'                      
021000              DELIMITED BY SIZE INTO SSA1                                 
021100     ELSE                                                                 
021200       STRING 'WLKOMA01(WDP801KY =' W-WDP801KY-X ')'                      
021300              DELIMITED BY SIZE INTO SSA1                                 
021400     END-IF                                                               
021500     MOVE '  GE' TO GODK-STATUSKODER                                      
021600     CALL CBLTDLI USING GU WDP8-PCB DLI-IO-WDP801 SSA1                    
021700     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
021800     PERFORM IMS-STATUSKONTROLL                                           
021900     .                                                                    
022000                                                                          
022100                                                                          
022200 IMS-ISRT-WDP8-ROT SECTION.                                               
022300     IF WDP8-DBD-NAME = 'WDP8'                                            
022400       MOVE 'WDP801   ' TO SSA1                                           
022500     ELSE                                                                 
022600       MOVE 'WLKOMA01 ' TO SSA1                                           
022700     END-IF                                                               
022800     MOVE '  II' TO GODK-STATUSKODER                                      
022900     CALL CBLTDLI USING ISRT WDP8-PCB DLI-IO-WDP801 SSA1                  
023000     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
023100     PERFORM IMS-STATUSKONTROLL                                           
023200     .                                                                    
023300                                                                          
023400                                                                          
023500 IMS-ISRT-WDP8-TRANS SECTION.                                             
023600     IF WDP8-DBD-NAME = 'WDP8'                                            
023700       STRING 'WDP801  (WDP801KY =' W-WDP801KY-X ')'                      
023800               DELIMITED BY SIZE INTO SSA1                                
023900        MOVE 'WDP811   ' TO SSA2                                          
024000     ELSE                                                                 
024100       STRING 'WLKOMA01(WDP801KY =' W-WDP801KY-X ')'                      
024200              DELIMITED BY SIZE INTO SSA1                                 
024300        MOVE 'WLKOMA11 ' TO SSA2                                          
024400     END-IF                                                               
024500     MOVE '  ' TO GODK-STATUSKODER                                        
024600     CALL CBLTDLI USING ISRT WDP8-PCB DLI-IO-WDP811 SSA1 SSA2             
024700     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
024800     PERFORM IMS-STATUSKONTROLL                                           
024900     .                                                                    
025000                                                                          
025100     EJECT                                                                
025200 IMS-STATUSKONTROLL SECTION.                                              
025300     SET STATUS-IX TO 1                                                   
025400     SEARCH GODK-STATUS                                                   
025500       AT END                                                             
025600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025700         DELIMITED BY SIZE INTO FELTEXT                                   
025800         CALL FELLOG                                                      
025900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026000         CONTINUE                                                         
026100     END-SEARCH                                                           
026200     .                                                                    

000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2150800.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   91/02/21.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BMP SOM HÄMTAR ORDERNUMMER PÅ WLXXCO (FRÅN SATS-                 
001100*        ORDER) OCHUPPDATERAR WDD9 MED DET. HÄNDELSEN PÅ                  
001200*        WLXXCO TAS SEDAN BORT.                                           
001300*                                                                         
001400*        PROGRAMMET UPPATERAR WLXXCO (WDR5)                               
001500*                             WLINLB (WDD9)                               
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W2150800'.            
003800 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
003900 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
004000 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004100 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004200 01  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004300 01  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004400 01  CHKP-SW                     PIC X(1)    VALUE 'N'.                   
004500     88 CHKP-TAGEN                           VALUE 'J'.                   
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004701                                                                          
004710*01  -COPY WWDCKONS                                                       
004720                                                                          
004800     SKIP2                                                                
004900 01  FILLER                      PIC X(8)    VALUE 'WS-FAELT'.            
005000 01  WS-FAELT.                                                            
005100     03  WS-IDORDNSB             PIC S9(5)   VALUE ZERO COMP-3.           
005200     03  WS-TIBEODAT-SATS        PIC S9(5)   VALUE ZERO COMP-3.           
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006200*                                                                         
006300*    --- PARAMETRAR TILL ABEND                                            
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006800     SKIP3                                                                
006900 01  NYCKLAR-TILL-DLI.                                                    
007000     03  W-WDGXKEY-2237-X.                                                
007100         05  W-IDHTYP            PIC X(4)    VALUE '2237'.                
007200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007300     03  W-WDGXKEY-2238-X.                                                
007400         05  W-IDARTNR-H         PIC S9(9)   VALUE ZERO COMP-3.           
007500         05  W-TIBEHOV-H         PIC S9(5)   VALUE ZERO COMP-3.           
007600         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
007610     03  W-IDARTNR-X.                                                     
007620         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007700     03  W-WDD901KY-X.                                                    
007800         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
007900         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
008000     03  W-IDLEVNR-X.                                                     
008100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
008200     03  W-DAAVROP-X.                                                     
008300         05  W-DAAVROP           PIC  9(6)   VALUE ZERO.                  
008400     03  W-KDAVROP-X.                                                     
008500         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
008600     03  W-IDORDNSB-X.                                                    
008700         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
008800     SKIP2                                                                
008900*    --- STATUS-KOD FRÅN IMS                                              
009000 01  STATUS-WS                   PIC XX.                                  
009100     88  SEGMENT-FINNS                       VALUE '  '.                  
009200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009500     88  IMS-EJ-OK                           VALUE 'XD'.                  
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200 01  SSA3                        PIC X(64).                               
010300 01  SSA4                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREOR                                          
010900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA1'.         
011000     SKIP3                                                                
011100 01  DLI-IO-AREA1.                                                        
011200     03  IO-AREA1                PIC X(150) VALUE SPACE.                  
011300     SKIP3                                                                
011400     03  WLXXCO11 REDEFINES IO-AREA1.                                     
011500*        05  -COPY WDGX2238   -PRE XXCO-                                  
011600     SKIP3                                                                
011700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
011800     SKIP3                                                                
011900 01  DLI-IO-AREA2.                                                        
012000     03  IO-AREA2                PIC X(150) VALUE SPACE.                  
012100     SKIP3                                                                
012200     03  WLINLB32 REDEFINES IO-AREA2.                                     
012300*        05  -COPY WDD907     -PRE INLB-                                  
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700*01  -COPY W0009      -PRE MSG-                                           
012800     EJECT                                                                
012900*01  -COPY W0008      -PRE XXCO-                                          
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200*01  -COPY W0008      -PRE INLB-                                          
013300     05  FILLER                  PIC X.                                   
013400     EJECT                                                                
013500 PROCEDURE DIVISION  USING MSG-PCB XXCO-PCB INLB-PCB.                     
013600     ENTRY 'DLITCBL' USING MSG-PCB XXCO-PCB INLB-PCB.                     
013700                                                                          
013800     SKIP2                                                                
013900     PERFORM A-INIT                                                       
014000     PERFORM IMS-GET-XXCO-2237                                            
014100     PERFORM IMS-GET-XXCO-2238                                            
014200     MOVE +0 TO CHKP-ANT                                                  
014300     PERFORM UNTIL SEGMENT-SAKNAS                                         
014400*      IF XXCO-2238-IDARTNR = 54922 AND XXCO-2238-TIBEHOV = 9638          
014500*         CONTINUE                                                        
014600*      ELSE                                                               
014700          IF CHKP-ANT > CHKP-MAX                                          
014800            PERFORM X-TAG-CHECKPOINT                                      
014900          END-IF                                                          
015000          PERFORM B-UPPDATERA-WDD9                                        
015100          PERFORM C-TA-BORT-2238                                          
015200*      END-IF                                                             
015300       PERFORM IMS-GET-XXCO-2238                                          
015400     END-PERFORM                                                          
015500                                                                          
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-INIT SECTION.                                                          
016100                                                                          
016200     PERFORM IMS-RESTART                                                  
016300     .                                                                    
016400     SKIP3                                                                
016500 B-UPPDATERA-WDD9 SECTION.                                                
016600                                                                          
016700     MOVE XXCO-2238-IDARTNR TO W-IDARTNR                                  
016800                               W-IDARTNR-H                                
016810                               W-IDARTNR-D9                               
016900     MOVE WC-CDC-SE         TO W-IDDC-D9                                  
017000     MOVE '1002'            TO W-IDLEVNR                                  
017100     MOVE XXCO-2238-TIBEHOV TO W-DAAVROP                                  
017200                               W-TIBEHOV-H                                
017300* FIX PGA DATKONVÄNDRING 2001-11-20 KJELL 2007-11-19 BODIL                
017400* ARTIKEL 31260658 BORTTAGEN UR FIX                                       
017500     IF W-DAAVROP = 0752                                                  
017600        IF W-IDARTNR = 31212536 OR 30796691 OR 31217620 OR                
017700*          8635452 OR 31212524 OR 31260659 OR                             
017800           8635452 OR 31212524 OR 31260659 OR 31260658 OR                 
017900           9162367                                                        
018000           MOVE 0801 TO W-DAAVROP                                         
018100        END-IF                                                            
018200     END-IF                                                               
018300* FIX SLUT                                                                
018400     MOVE +2                TO W-KDAVROP                                  
018500     MOVE XXCO-2238-IDORDNSB TO WS-IDORDNSB                               
018600     IF W-DAAVROP > 9000                                                  
018700        ADD 190000          TO W-DAAVROP                                  
018800     ELSE                                                                 
018900        ADD 200000          TO W-DAAVROP                                  
019000     END-IF                                                               
019100                                                                          
019200     PERFORM IMS-GET-INLB-BEORD                                           
019300     IF INLB-IDORDNSB > 0                                                 
019400       IF INLB-IDORDNSB = WS-IDORDNSB                                     
019500*        SEGMENTET HAR REDAN UPPDATERATS FÖRUT (OMKÖRNING)                
019600         CONTINUE                                                         
019700       ELSE                                                               
019800         MOVE 'DUBBLA BEORDRADE AVROP' TO FELTEXT-STR                     
019900         DISPLAY FELTEXT                                                  
020000         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
020100       END-IF                                                             
020200     ELSE                                                                 
020300                                                                          
020400*    EFTERSOM DET INTE GÅR ATT ÄNDRA ETT NYCKELVÄRDE MÅSTE                
020500*    SEGMENTET FÖRST TAS BORT OCH SEDAN INSERTAS MED NYTT                 
020600*    ORDERNUMMER                                                          
020700                                                                          
020800       MOVE INLB-TIBEODAT-SATS TO WS-TIBEODAT-SATS                        
020900       PERFORM IMS-DLET-INLB                                              
021000       ADD +1 TO CHKP-ANT                                                 
021100                                                                          
021200*    MÅSTE LÄSA OM AVROPET FÖR ATT POSITIONERA SIG INFÖR                  
021300*    INSERTEN AV BEORDRINGSSEGMENTET.                                     
021400*    LÄGG M.A.O INTE IN NÅGON ANNAN LÄSNING HÄR.                          
021500                                                                          
021600       PERFORM IMS-GET-INLB-AVROP                                         
021700       MOVE WS-IDORDNSB      TO INLB-IDORDNSB                             
021800       MOVE WS-TIBEODAT-SATS TO INLB-TIBEODAT-SATS                        
021900       PERFORM IMS-ISRT-INLB-BEORD                                        
022000       ADD +1 TO CHKP-ANT                                                 
022100     END-IF                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 C-TA-BORT-2238 SECTION.                                                  
022500                                                                          
022600     IF CHKP-TAGEN                                                        
022700       PERFORM IMS-GET-XXCO-2237                                          
022800       PERFORM IMS-GET-XXCO-2238-KVAL                                     
022900       MOVE NEJ TO CHKP-SW                                                
023000     END-IF                                                               
023100     PERFORM IMS-DLET-XXCO                                                
023200     ADD +1 TO CHKP-ANT                                                   
023300     .                                                                    
023400     EJECT                                                                
023500 X-TAG-CHECKPOINT   SECTION.                                              
023600                                                                          
023700     PERFORM IMS-CHECKPOINT                                               
023800     MOVE +0 TO CHKP-ANT                                                  
023900     MOVE JA TO CHKP-SW                                                   
024000     .                                                                    
024100     EJECT                                                                
024200* --- IMS SEKTIONER ---                                                   
024300     SKIP3                                                                
024400     EJECT                                                                
024500 IMS-GET-XXCO-2237 SECTION.                                               
024600     STRING 'WLXXCO01(WDGXKEY  =' W-WDGXKEY-2237-X ')'                    
024700          DELIMITED BY SIZE INTO SSA1                                     
024800     MOVE '  GE' TO GODK-STATUSKODER                                      
024900     CALL CBLTDLI USING GU   XXCO-PCB DLI-IO-AREA1 SSA1                   
025000     MOVE XXCO-STATUS-CODE TO STATUS-WS                                   
025100     PERFORM IMS-STATUSKONTROLL                                           
025200     .                                                                    
025300     SKIP3                                                                
025400 IMS-GET-XXCO-2238 SECTION.                                               
025500     MOVE 'WLXXCO11 ' TO SSA1                                             
025600     MOVE '  GE' TO GODK-STATUSKODER                                      
025700     CALL CBLTDLI USING GHNP XXCO-PCB DLI-IO-AREA1 SSA1                   
025800     MOVE XXCO-STATUS-CODE TO STATUS-WS                                   
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     .                                                                    
026100     SKIP3                                                                
026200 IMS-GET-XXCO-2238-KVAL SECTION.                                          
026300     STRING 'WLXXCO11(WDGXKEY  =' W-WDGXKEY-2238-X ')'                    
026400          DELIMITED BY SIZE INTO SSA1                                     
026500     MOVE '  GE' TO GODK-STATUSKODER                                      
026600     CALL CBLTDLI USING GHNP XXCO-PCB DLI-IO-AREA1 SSA1                   
026700     MOVE XXCO-STATUS-CODE TO STATUS-WS                                   
026800     PERFORM IMS-STATUSKONTROLL                                           
026900     .                                                                    
027000     SKIP3                                                                
027100 IMS-DLET-XXCO SECTION.                                                   
027200                                                                          
027300     MOVE '  ' TO GODK-STATUSKODER                                        
027400     CALL CBLTDLI USING DLET XXCO-PCB DLI-IO-AREA1                        
027500     MOVE XXCO-STATUS-CODE TO STATUS-WS                                   
027600     PERFORM IMS-STATUSKONTROLL                                           
027700     .                                                                    
027800     EJECT                                                                
027900 IMS-GET-INLB-BEORD SECTION.                                              
028000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
028100          DELIMITED BY SIZE INTO SSA1                                     
028200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
028300          DELIMITED BY SIZE INTO SSA2                                     
028400     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-X                             
028500           '&KDAVROP  =' W-KDAVROP-X ')'                                  
028600          DELIMITED BY SIZE INTO SSA3                                     
028700     MOVE 'WLINLB32 ' TO SSA4                                             
028800     MOVE '  ' TO GODK-STATUSKODER                                        
028900     CALL CBLTDLI USING GHU  INLB-PCB DLI-IO-AREA2                        
029000                                        SSA1 SSA2 SSA3 SSA4               
029100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     .                                                                    
029400     EJECT                                                                
029500 IMS-GET-INLB-AVROP SECTION.                                              
029600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
029700          DELIMITED BY SIZE INTO SSA1                                     
029800     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
029900          DELIMITED BY SIZE INTO SSA2                                     
030000     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-X                             
030100           '&KDAVROP  =' W-KDAVROP-X ')'                                  
030200          DELIMITED BY SIZE INTO SSA3                                     
030300     MOVE '  ' TO GODK-STATUSKODER                                        
030400     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3           
030500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800     EJECT                                                                
030900 IMS-ISRT-INLB-BEORD SECTION.                                             
031000     MOVE 'WLINLB32 ' TO SSA1                                             
031100     MOVE '  ' TO GODK-STATUSKODER                                        
031200     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA2 SSA1                   
031300     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031600     SKIP3                                                                
031700 IMS-DLET-INLB SECTION.                                                   
031800                                                                          
031900     MOVE '  ' TO GODK-STATUSKODER                                        
032000     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA2                        
032100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032400     EJECT                                                                
032500 IMS-RESTART SECTION.                                                     
032600     SKIP2                                                                
032700     MOVE SPACE TO MSG-IO-AREA                                            
032800     MOVE '  ' TO GODK-STATUSKODER                                        
032900     CALL CBLTDLI USING XRST MSG-PCB                                      
033000                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
033100                        CHKP-AREA-LENGTH CHKP-AREA                        
033200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033300     PERFORM IMS-STATUSKONTROLL                                           
033400     .                                                                    
033500     EJECT                                                                
033600 IMS-CHECKPOINT SECTION.                                                  
033700     SKIP2                                                                
033800     MOVE SPACE TO MSG-IO-AREA                                            
033900     MOVE '  XD' TO GODK-STATUSKODER                                      
034000     CALL CBLTDLI USING CHKP MSG-PCB                                      
034100                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
034200                        CHKP-AREA-LENGTH CHKP-AREA                        
034300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034400     PERFORM IMS-STATUSKONTROLL                                           
034500                                                                          
034600     IF IMS-EJ-OK                                                         
034700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
034800       DISPLAY FELTEXT                                                    
034900       CALL FELLOG                                                        
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 IMS-STATUSKONTROLL SECTION.                                              
035400     SKIP2                                                                
035500     SET STATUS-IX TO 1                                                   
035600     SEARCH GODK-STATUS                                                   
035700       AT END                                                             
035800         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
035900         DISPLAY FELTEXT                                                  
036000         CALL FELLOG                                                      
036100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
036200     END-SEARCH                                                           
036300     .                                                                    
